#lang racket

(require (for-syntax macro-debugger/emit
                     racket/format
                     racket/dict
                     racket/syntax
                     syntax/context
                     syntax/id-table
                     syntax/parse
                     syntax/stx))

(require (for-syntax "../LoopTiles.rkt"
                     "syntax-classes.rkt"
                     "util.rkt"
                     "Writer.rkt"))

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

(define-for-syntax macroize-skel-kind
  (lambda (skel-kind)
    (format-id
     skel-kind 
     "@~a" 
     (syntax-e skel-kind) #:source skel-kind #:props skel-kind)))
(define-for-syntax InitSkelIds
  (make-hash))

; Grid > Block > Warp > Thread-Loop > Unroll
; 
(define-for-syntax InitSkelTable 
  (hash 'Loop1d 
        #'(lambda (skel) 
            (syntax-parse skel
              [(_ (name:id) args:skeleton-args child:cxx-stmt)
               (let*
                   ([defs (syntax-local-make-definition-context)]
                    [ctx (generate-expand-context)]
                    [itr-id #'j]
                    [itr-skel-kind (stx-car (stx-cdr (stx-car #'args)))]
                    [itr-macro (macroize-skel-kind itr-skel-kind)])
                 (syntax-local-bind-syntaxes
                  (list itr-macro)
                  (local-transformer-expand 
                   (with-syntax ([itr-id itr-id])
                   #'(lambda (stx)
                       (syntax-parse stx
                         [(_ (name:id) inner-args:skeleton-args child:cxx-stmt)
                          (with-syntax 
                              ([itr-id #'itr-id]
                               [local-id (stx-car (stx-cdr (stx-car #'inner-args)))])
                            #'(= local-id itr-id))])))
                   'expression null) 
                  defs)
                 (internal-definition-context-seal defs)
                 (with-syntax 
                     ([itr-id itr-id]
                      [child (local-expand #'child ctx #f defs)])
                   #'(for ((def (() (int (!)) itr-id = 0)) (< itr-id 5) (++ itr-id)) child)))]))))


(define-syntax @
  (lambda (stx)
    (syntax-parse stx 
      [skel:cxx-@
       (with-syntax
           ([skel-macro 
             (hash-ref
              InitSkelIds
              (syntax->datum #'skel.kind) ; If it's a top-level thingy, we shouldn't have any problem looking it up. 
              (lambda () (macroize-skel-kind #'skel.kind)))]) ; If not, we shouldn't have any problem with the marks
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

(define-syntax for
  (lambda (stx)
    (syntax-parse stx
      [stmt:cxx-for
       (no-expand 
        (syntax-parse #'stmt.init
          [init:decls stx]
          [init:cxx-expr stx]))])))

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
             [ctx (generate-expand-context)]
             [skel-ids (list #'Loop1d)])
         (syntax-local-bind-syntaxes 
          (map macroize-skel-kind skel-ids)
          (with-syntax
              ([(skel-defs ...) 
                (map
                 (lambda (skel-id)
                   (local-transformer-expand (hash-ref InitSkelTable (syntax->datum skel-id)) 'expression null)) skel-ids)])
            #'(values skel-defs ...))
          top-level-defs)
         (internal-definition-context-seal top-level-defs)
         (map 
          (lambda (skel-id)
            (hash-set! InitSkelIds (syntax->datum skel-id) (internal-definition-context-apply/loc top-level-defs (macroize-skel-kind skel-id)))) skel-ids)
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
                       (let* ([expanded (local-expand stx 'top-level #f)]
                              [expanded ((walk-expr-safe-ids (make-bound-id-table)) expanded)])
                         ;(display expanded) (newline)
                         
                         (make-cpp-tu expanded))) 
                     contents)]
                   [unpacked #'(let () (map display 'contents) (void))]) 
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



