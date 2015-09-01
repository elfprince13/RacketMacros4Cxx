#lang racket

(require (for-syntax macro-debugger/emit
                     racket/format
                     racket/dict
                     racket/syntax
                     syntax/context
                     syntax/id-table
                     syntax/parse
                     syntax/stx))

(require (for-syntax "util.rkt"
                     "syntax-classes.rkt"))
(require (for-syntax "../LoopTiles.rkt"))
(require (for-syntax "Writer.rkt"))

(require "req-utils.rkt")
(provide (all-defined-out))

(define simple-external-params-table
  (hash 
   '(Loop1d (test-loop)) (list 
                          (list #'(blockIdx . x)) (list #'((/ (threadIdx . x) 32))) (list #'((& (threadIdx . x) 31))) (list #'k) (list #'j) 
                          (list #'(gridDim . x)) (list #'((/ (blockDim . x) 32))) (list #'32) (list #'1) (list #'4)) 
   '(I ()) '()
   '(N ()) '()))


(define lookup-skeleton
  (lambda (table-list key)
    (begin
      ;(print (list "looking up "  key " in " table-list)) (newline)
      (match table-list 
        [(cons h t) (hash-ref h key (lambda () (lookup-skeleton t)))]
        [(list ) #f]))))

(struct skeleton-expansion (body table))

; Grid > Block > Warp > Thread-Loop > Unroll
; 
(define-for-syntax InitSkelTable 
  (hash 'Loop1d 
        #'(lambda (skel) 
          (syntax-parse skel
            [(_ (name:id) args:skeleton-args child:cxx-stmt)
             #'"A wild skeleton appeared"]))))
(define-for-syntax InitSkelIds
  (make-hash))


#;(define expand-stmt 
    (lambda (stmt skels) 
      (syntax-case stmt (if else for while begin call def @) 
        [(@ SkelKind (name ...) (params ...) body) 
         (begin 
           #;(print "expanding skeleton")
           #;(newline)
           (let ((expansion (((lookup-skeleton skels (syntax->datum #'SkelKind)) stmt skels) 
                             (hash-ref simple-external-params-table (list (syntax->datum #'SkelKind) (syntax->datum #'(name ...)))))))
             (begin
               ;(print (list "re-expanding with " (skeleton-expansion-body expansion) (skeleton-expansion-table expansion))) (newline)
               (expand-stmt (skeleton-expansion-body expansion) (skeleton-expansion-table expansion)))))] ; The expansion process may have introduced new macros, so expand those too
        )))



(define-syntax @
  (lambda (stx)
    (syntax-parse stx 
      [skel:cxx-@
       (with-syntax
           ([skel-macro (syntax-local-introduce (hash-ref InitSkelIds 'Loop1d)) #;(syntax-local-value (syntax-local-introduce (format-id #'skel.kind "@~a" (syntax-e #'skel.kind) #:source #'skel.kind #:props #'skel.kind)))])
         (local-expand #'(skel-macro (skel.name) skel.args skel.child) 'expression #f))]
      )))

(define-syntax while
  (lambda (stx)
    (syntax-parse stx
      [stmt:cxx-while 
       (with-syntax 
           ([cond (local-expand #'stmt.cond 'expression #f)]
            [child (local-expand #'stmt.child 'expression #f)])
         (no-expand #'(while cond child)))])))

(define-syntax -if
  (lambda (stx)
    (syntax-parse stx
      [stmt:cxx-if 
       (with-syntax 
           ([cond (local-expand #'stmt.cond 'expression #f)]
            [child (local-expand #'stmt.child 'expression #f)]
            [else-clause 
             (if (stx-null? #'stmt.else-clause)
                 #'stmt.else-clause
                 (local-expand #'stmt.else-clause 'expression #f))])
         (no-expand (if (stx-null? #'else-clause) #'(if cond child) #'(if cond child else else-clause))))])))

(define-syntax block
  (lambda (stx)
    (syntax-case stx ()
      [(keyword stmts ... )
       (with-syntax 
           ([(stmts ...)
             (let loop 
               ([work (syntax->list #'(stmts ...))]
                [defs (syntax-local-make-definition-context)]
                [ctx (generate-expand-context)])
               (if (null? work)
                   null
                   (let-values ([(head rest) (values (car work) (cdr work))])
                     (let-values 
                         ([(head defs)
                           (syntax-parse head
                             [vars:decls 
                              (let-values ([(head defs) (expand-and-extend head ctx defs)])
                                (bind-and-seal defs (parse-def-names #'vars))
                                (values head defs))]
                             [impl
                              (values 
                               (local-expand head ctx #f defs)
                               defs)])])
                       (cons head (loop rest defs ctx))))))])
         (no-expand #'(keyword stmts ...)))])))

#;(((
     [(expr ...) 
      (begin
        #;(print "no match for ") 
        #;(newline)
        #;(print stmt)
        #;(newline)
        (let ((expr-components 
               (map (curryr expand-stmt skels) (syntax->list #'(expr ...)))))
          (datum->syntax stmt expr-components)))]
     [any #'any]))) 

(define-for-syntax init-var-to-context
  (lambda (stx ctx defs)
    (syntax-parse stx
      [(var:var-init)
       (let*-values ([(name) #'var.name]
                     [(name defs expr) 
                      (handle-init #'var.exp
                                   (lambda () (values name defs #'() ))
                                   (lambda (eq-expr) 
                                     (let-values 
                                         ([(eq-expr defs)
                                           (expand-and-extend eq-expr ctx defs)])
                                       (bind-and-seal defs (list name))
                                       (values (internal-definition-context-apply/loc defs name) 
                                               defs 
                                               (with-syntax 
                                                   ([eq-expr eq-expr])
                                                 #'(= . eq-expr)))))
                                   (lambda (paren-expr) 
                                     (let-values 
                                         ([(paren-expr defs)
                                           (expand-and-extend paren-expr ctx defs)])
                                       (bind-and-seal defs (list name))
                                       (values (internal-definition-context-apply/loc defs name) 
                                               defs 
                                               (with-syntax 
                                                   ([paren-expr paren-expr])
                                                 #'paren-expr)))))])
         (values defs
                 (with-syntax
                     ([name name]
                      [expr expr])
                   #'(name . expr))))])))

(define-for-syntax defun
  (lambda (stx ctx defs)
    ;(display stx) (newline)
    (syntax-parse stx
      [func:fun-decl 
       (let ([defs (syntax-local-make-definition-context defs)]
             [ctx (generate-expand-context)]
             [args (parse-arg-names #'func.args)]
             [kw-args (parse-arg-names #'func.kw-args)])
         (syntax-local-bind-syntaxes args #f defs)
         (syntax-local-bind-syntaxes kw-args #f defs)
         (internal-definition-context-seal defs)
         (no-expand
          (with-syntax*
              ([f-name (internal-definition-context-apply/loc defs #'func.name)]
               [body (local-expand #'func.body (build-expand-context 'expression) #f defs)]
               [(arg ...) 
                (subs-decl-ids 
                 (syntax->list #'func.args)
                 (contextualize-args args defs))]
               [(kw-arg ...) 
                (subs-decl-ids
                 (syntax->list #'func.kw-args)
                 (contextualize-args kw-args defs))]
               [((attribute-term ...) ...) ; This is a splicing class so jam all the terms together
                #'func.attributes])
            #'(defun func.storage-classes func.ret-type f-name (arg ... kw-arg ...) attribute-term ... ... body))))])))

(define-for-syntax def
  (lambda (stx ctx defs)
    (syntax-parse stx
      [vars:decls
       (values 
        defs 
        (no-expand 
         (with-syntax*
             ([(extra-types ...) #'vars.extra-type-infos]
              [(vars ...) 
               (map
                (lambda (stx)
                  (let-values 
                      ([(new-defs new-stx)
                        (syntax-parse stx
                          [decl:var-decl
                           (let-values 
                               ([(new-defs new-init) (init-var-to-context #'decl.init ctx defs)])
                             (values 
                              new-defs 
                              #`(decl.storage-classes decl.type-info #,@new-init #,@#'decl.attributes)))]
                          [(init:var-init) (init-var-to-context #'init ctx defs)])])
                    (set! defs new-defs)
                    new-stx))
                (cons
                 #'vars.var
                 (syntax->list #'vars.extra-vars)))] ; heavy-lifting goes here
              [var (stx-car #'(vars ...))]
              [(extra-vars ...) (stx-cdr #'(vars ...))]
              [(extras ...) 
               (stx-map 
                (lambda (t-stx v-stx)
                  (with-syntax
                      ([(type ...) t-stx]
                       [(init ...) v-stx])
                    #'((type ...) init ...)))
                #'(extra-types ...)
                #'(extra-vars ...))]) 
           #'(def var extras ...))))])))

(define-syntax translation-unit
  (lambda (stx)
    (syntax-parse stx
      [unit:tu-stx
       (let ([top-level-defs (syntax-local-make-definition-context)]
             [ctx (generate-expand-context)])
         
         (hash-set! InitSkelIds 'Loop1d #'@Loop1d)
         (syntax-local-bind-syntaxes 
          (list (hash-ref InitSkelIds 'Loop1d)) 
          (local-transformer-expand (hash-ref InitSkelTable 'Loop1d) 'expression null) top-level-defs)
         (internal-definition-context-seal top-level-defs)
         (hash-set! InitSkelIds 'Loop1d (internal-definition-context-apply/loc top-level-defs (hash-ref InitSkelIds 'Loop1d)))
         (with-syntax 
             ([(declaration ...)
               (stx-map 
                (lambda (declaration) 
                  (define out-form 
                    (syntax-parse declaration
                      [record:record-decl #'record]
                      [typedef:typedef-decl #'typedef]
                      [vdefun:fun-decl 
                       (let ([new-defs (syntax-local-make-definition-context top-level-defs)])
                         (bind-and-seal new-defs (list #'vdefun.name))
                         (set! top-level-defs new-defs)
                         (defun #'vdefun ctx top-level-defs))]
                      [vdefs:decls 
                       (let-values ([(new-defs expanded-stx) (def #'vdefs ctx top-level-defs)])
                         (set! top-level-defs new-defs)
                         expanded-stx)]))
                  (set! top-level-defs (syntax-local-make-definition-context top-level-defs))
                  out-form)
                #'unit.items)])
           (no-expand #'(translation-unit declaration ...))))])))

(define-for-syntax display-inert-body
  (lambda (tag contents) 
    (with-syntax* ([stx-tag tag] 
                   [contents 
                    (stx-map 
                     (lambda (stx) 
                       (let ([expanded (local-expand stx 'top-level #f)])
                         ;(display expanded) (newline)
                         (make-cpp-tu expanded)
                         ;((walk-expr-safe-ids (make-bound-id-table)) expanded)
                         )) 
                     contents)]
                   [unpacked #'(apply values 'contents)]) 
      (if tag 
          #'(stx-tag unpacked) 
          #'unpacked))))

(define-syntax top-interaction
  (lambda (stx)
    (syntax-case stx ()
      [(top-interaction . body)
       (display-inert-body #f #'(body))])))

(define-syntax module-begin
  (lambda (stx)
    (syntax-case stx ()
      [(module-begin bodies ...)
       (display-inert-body #'#%module-begin #'(bodies ...))])))



