#lang racket

(require 
  (for-syntax 
   macro-debugger/emit
   racket/format
   racket/dict
   racket/syntax
   syntax/context
   syntax/id-table
   syntax/parse
   ;syntax/parse/debug
   syntax/stx))

(require 
  (for-syntax 
   "../LoopTiles.rkt"
   "syntax-classes.rkt"
   "util.rkt"
   "Writer.rkt"))

(require "req-utils.rkt")
(provide (all-defined-out))

(define simple-external-params-table
  (hash 
   '(Loop1d (test_loop)) (list 
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

(define-values-for-syntax
  (init-clock tick)
  (let
      ([time 0])
    (values
     (lambda ()
       (set! time (current-inexact-milliseconds))
       " 0")
     (lambda ()
       (let ([pv-time time]
             [now (current-inexact-milliseconds)])
         (set! time now)
         (string-append " " (~r (- now pv-time) #:precision 2)))))))

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
                    [itr-init (stx-cdr (stx-car (stx-cdr #'args)))]
                    [itr-final (stx-cdr (stx-car (stx-cdr (stx-cdr #'args))))]
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
                                ([itr-id (syntax-local-introduce #'itr-id)]
                                 [local-id (stx-car (stx-cdr (stx-car #'inner-args)))])
                              #'(= local-id itr-id))])))
                   'expression null) 
                  defs)
                 (internal-definition-context-seal defs)
                 (with-syntax 
                     ([itr-id itr-id]
                      [itr-init itr-init]
                      [itr-final itr-final]
                      [child #'child])
                   #;(cuda-loop1d #'child)
                   (local-expand #'(for ((def (() (int (!)) itr-id = itr-init)) (< itr-id itr-final) (++< itr-id)) child) ctx #f defs)))]))))

;;;;;;;;;;;;;;;;;;;;;;;;
; Expression definitions
;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax make-n-op
  (lambda (stx)
    (syntax-parse stx
      #:literals (make-n-op)
      [(make-n-op op:id (~between n:exact-nonnegative-integer 1 2) ...)
       (let*
           ([op-counts (syntax->datum #'(n ...))]
            [min-count (car op-counts)]
            [max-count 
             (if (eq? 2 (length op-counts))
                 (cadr op-counts)
                 min-count)])
         (with-syntax
             ([min-count min-count]
              [max-count max-count])
           ;(display (~a (list "Meow:" #''op #'min-count #'max-count))) (newline)
           #'(define-syntax op
               (lambda (stx)
                 (syntax-parse stx
                     #:literals (op)
                     [(op (~between term:cxx-expr min-count max-count) (... ...))
                      ;#''(term (... ...))
                      (with-syntax
                          ([(term (... ...)) (handle-expr-list #'(term (... ...)))])
                          (no-expand #'(op term (... ...))))])))))])))

(make-n-op = 2)
(make-n-op == 2)
(make-n-op >= 2)
(make-n-op <= 2)
(make-n-op > 2)
(make-n-op < 2)
(make-n-op + 2)
(make-n-op - 2)
(make-n-op / 2)
(make-n-op * 1 2)
(make-n-op & 1 2)
(make-n-op && 2)
(make-n-op ^ 2)
(make-n-op ?: 3)

(make-n-op >-- 1)
(make-n-op >++ 1)
(make-n-op --< 1)
(make-n-op ++< 1)

(define-syntax call
  (lambda (stx)
    #;(begin
      (display "attempting to expand call ") (newline)
      (display stx) (newline)
      (display (syntax-class-parse cxx-expr stx)) (newline)
      (display (debug-parse stx (call callee:cxx-expr arg:cxx-expr ...))) (newline))
    (syntax-parse stx
        [(call callee:cxx-expr arg:cxx-expr ...)
         (with-syntax 
             ([callee (handle-expr #'callee)]
              [(arg ...) (handle-expr-list #'(arg ...))])
           (no-expand #'(call callee arg ...)))])))



;;;;;;;;;;;;;;;;;;;;;;;;
; Statement definitions
;;;;;;;;;;;;;;;;;;;;;;;;

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
         (local-expand #'(skel-macro (skel.name) skel.args skel.child) (generate-expand-context) #f))])))

(define-syntax while
  (lambda (stx)
    (syntax-parse stx
      [stmt:cxx-while 
       (with-syntax 
           ([cond (handle-expr #'stmt.cond)]
            [child (local-expand #'stmt.child (generate-expand-context) #f)])
         (no-expand #'(while cond child)))])))

(define-syntax for
  (lambda (stx)
    (let 
        ([defs (syntax-local-make-definition-context)]
         [context (generate-expand-context)])
      (syntax-parse stx
        [stmt:cxx-for
         (let-values
             ([(defs init)
               (syntax-parse #'stmt.init
                 [init:decls (def #'init context defs)]
                 [init:cxx-expr (values defs (handle-expr #'init context defs))])])
           (with-syntax* 
               ([init init]
                [cond (handle-expr #'stmt.cond context defs)]
                [update (handle-expr #'stmt.update context defs)]
                [child (local-expand #'stmt.child context #f defs)])
             (no-expand 
              #'(for (init cond update) child))))]))))

(define-syntax return
  (lambda (stx)
    (syntax-parse stx
      [stmt:cxx-return
       (with-syntax
           ([ret-val 
             (if (stx-null? #'stmt.ret-val) 
                 #'()
                 (handle-expr #'(stmt.ret-val)))])
         (no-expand #'(return . ret-val)))])))

(define-syntax -if
  (lambda (stx)
    (syntax-parse stx
      [stmt:cxx-if 
       (with-syntax 
           ([cond (handle-expr #'stmt.cond)]
            [child (local-expand #'stmt.child (generate-expand-context) #f)]
            [else-clause 
             (if (stx-null? #'stmt.else-clause)
                 #'stmt.else-clause
                 (local-expand #'stmt.else-clause (generate-expand-context) #f))])
         (no-expand (if (stx-null? #'else-clause) #'(if cond child) #'(if cond child else else-clause))))])))

(define-syntax block
  (let ([nest 0])
    (lambda (stx)
      (set! nest (+ nest 1))
      (syntax-parse stx
        [stmt:cxx-block
         (with-syntax 
             ([(stmts ...)
               (let loop 
                 ([work (syntax->list #'stmt.children)]
                  [defs (syntax-local-make-definition-context)]
                  [ctx (generate-expand-context)])
                 ;(display (make-string nest #\ ))
                 ;(display (length work)) #;(display (tick)) (newline)
                 (if (null? work)
                     null
                     (let-values ([(head rest) (values (car work) (cdr work))])
                       ;(display head) (display " ") (display rest) (newline)
                       (let-values 
                           ([(head defs)
                             (syntax-parse head
                               [vars:cxx-decls 
                                (let-values ([(defs head) (def head ctx defs)])
                                  (values head defs))]
                               [expr:cxx-expr
                                (values
                                 (handle-expr head ctx defs)
                                 defs)]
                               [impl:cxx-stmt
                                (values 
                                 (local-expand head ctx #f defs)
                                 defs)])])
                         (cons head (loop rest defs ctx))))))])
           (set! nest (- nest 1))
           (no-expand #'(block stmts ...)))]))))

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


;;;;;;;;;;;;;;;;;;;;;;;;
; Define definitions
;;;;;;;;;;;;;;;;;;;;;;;;

(define-for-syntax init-var-to-context
  (lambda (stx ctx defs)
    (syntax-parse stx
      [(var:var-init)
       (let*-values 
           ([(name) #'var.name]
            [(name defs expr) 
             (handle-init 
              #'var.exp
              (lambda ()
                (bind-and-seal defs (list name))
                (values 
                 (internal-definition-context-apply/loc defs name) 
                 defs 
                 #'()))
              (lambda (eq-expr) 
                (let-values 
                    ([(eq-expr defs)
                      (expand-and-extend eq-expr ctx defs)])
                  (bind-and-seal defs (list name))
                  (values 
                   (internal-definition-context-apply/loc defs name) 
                   defs 
                   (with-syntax 
                       ([eq-expr eq-expr])
                     #'(= . eq-expr)))))
              (lambda (paren-expr) 
                (let-values 
                    ([(paren-expr defs)
                      (expand-and-extend paren-expr ctx defs)])
                  (bind-and-seal defs (list name))
                  (values 
                   (internal-definition-context-apply/loc defs name) 
                   defs 
                   (with-syntax 
                       ([paren-expr paren-expr])
                     #'paren-expr)))))])
         (values 
          defs
          (with-syntax
              ([name name]
               [expr expr])
            #'(name . expr))))])))

(define-for-syntax defun
  (lambda (stx ctx defs)
    ;(display stx) (newline)
    ;(display "starting defun") (display (tick)) (newline)
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
          (if (stx-null? #'func.body) 
              stx
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
                ;(display "putting the defun back together") (display (tick)) (newline)
                #'(defun func.storage-classes func.ret-type f-name (arg ... kw-arg ...) attribute-term ... ... body)))))])))

(define-for-syntax def
  (lambda (stx ctx defs)
    (syntax-parse stx
      [vars:decls
       (let 
           ([def-stx 
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
                                ; This shouldn't happen in a (def ...)
                                #;[(init:var-init) (init-var-to-context #'init ctx defs)])])
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
                 #'(def var extras ...)))])
         (values 
          defs 
          def-stx))])))

;;;;;;;;;;;;;;;;;;;;;;;;
; Top-level definitions
;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax translation-unit
  (lambda (stx)
    ;(display "starting up") (display (init-clock)) (newline)
    (syntax-parse stx
      [unit:tu-stx
       (let ([top-level-defs (syntax-local-make-definition-context)]
             [ctx (generate-expand-context)]
             [skel-ids (list #'Loop1d)])
         ;(display "have a parse") (display (tick)) (newline)
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
         ;(display "skeletons bound") (display (tick)) (newline)
         stx
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
                              #;[expanded ((walk-expr-safe-ids (make-bound-id-table)) expanded)])
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



