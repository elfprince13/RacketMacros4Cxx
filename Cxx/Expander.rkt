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
(require "../LoopTiles.rkt")

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
(define init-skeletons-table 
  (list (hash 'Loop1d 
              (lambda (skel table-here) 
                (syntax-case skel (@ Loop1d)
                  [(@ Loop1d (name ...) (params ...) body)
                   (let* ((params (syntax->datum #'(params ...)))
                          (num-params (length params)))
                     (cond
                       ((> 2 num-params) (raise-argument-error 'params "at least 2 arguments required" params))
                       ((< 4 num-params) (raise-argument-error 'params "at least 4 arguments required" params))
                       (else 
                        (let ((itr-var (car (car params)))
                              (lower-bound (if (eq? 2 num-params) 0 (car (cadr params))))
                              (upper-bound (car (if (eq? 2 num-params) (cadr params) (caddr params))))
                              (stride (if (eq? 4 num-params) (car (cadddr params)) 1)))
                          (lambda (ext-params)
                            (let ((num-ext-params (length ext-params)))
                              (if (eq? 10 num-ext-params)
                                  (let*-values (((split-vars split-bounds) (split-at (drop ext-params (- num-ext-params 10)) 5))
                                                ((index-vars) (map car split-vars))
                                                ((index-bounds) (map car split-bounds)))
                                    (let-values (((counter-exp geom-exp loop-code) (cuda-loop1d #'body index-vars index-bounds (list 0 1 2 3 4))))
                                      (skeleton-expansion
                                       loop-code
                                       (cons (hash 'I (lambda (skel table-here) 
                                                        (syntax-case skel (@ I)
                                                          [(@ I () () ()) 
                                                           (lambda (ext-params) 
                                                             (skeleton-expansion
                                                              (with-syntax ((itr-var itr-var)) #'itr-var)
                                                              table-here))]))
                                                   'N (lambda (skel table-here) 
                                                        (syntax-case skel (@ I)
                                                          [(@ N () () ()) 
                                                           (lambda (ext-params) 
                                                             (skeleton-expansion 
                                                              (with-syntax ((upper-bound upper-bound)) #'upper-bound)
                                                              table-here))]))) 
                                             table-here))))
                                  (raise-argument-error 'ext-params "10 arguments required" ext-params))))))))])))))


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

(define-syntax while
  (lambda (stx)
    (syntax-case stx ()
      [(while (cond ...) body) 
       (with-syntax 
           ([(cond ...) (local-expand #'(cond ...) 'expression #f)]
            [body (local-expand #'body 'expression #f)])
         #'(while (cond ...) body))])))

#;(((
      [(for ((init ...) (cond ...) (update ...)) body) 
       (with-syntax
           ((init (expand-stmt #'(init ...) skels))
            (cond (expand-stmt #'(cond ...) skels))
            (update (expand-stmt #'(update ...) skels))
            (body (expand-stmt #'body skels)))
         #'(for (init cond update) body))]
      [(if (cond ...) body) 
       (with-syntax  
           ((cond (expand-stmt #'(cond ...) skels))
            (body (expand-stmt #'body skels))) 
         #'(if cond body))]
      [(if (cond ...) body else else-body) 
       (with-syntax 
           ((cond (expand-stmt #'(cond ...) skels))
            (body (expand-stmt #'body skels))
            (else-body (expand-stmt #'else-body skels))) 
         #'(if cond body else else-body))]
      [(begin stmts ... ) 
       (begin 
         #;(print "expanding begin") 
         #;(newline ) 
         (let ((stmts (map (curryr expand-stmt skels) (syntax->list #'(stmts ...)))))
           (datum->syntax stmt (cons 'begin stmts))))]
      [(call func args ... ) 
       (begin 
         #;(print "expanding call") 
         #;(newline ) 
         (let ((args (map (curryr expand-stmt skels) (syntax->list #'(args ...)))))
           (with-syntax ((func (expand-stmt #'func skels)))
             #`(call func #,@(datum->syntax stmt args)))))]
      [(def defs ...) 
       (expand-decl stmt skels)]
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

(define-syntax defun
  (lambda (stx)
    (syntax-parse stx
      [func:fun-decl 
       (let ([defs (syntax-local-make-definition-context)]
             [ctx (generate-expand-context)]
             [args (parse-arg-names #'func.args)]
             [kw-args (parse-arg-names #'func.kw-args)]
             [subs-decl-ids 
              (lambda (decls ids)
                (map 
                 (lambda (decl id)
                   (syntax-parse decl
                     [var:var-decl 
                      (with-syntax ([name id])
                        #`(var.storage-classes var.type name #,@#'var.init-exp))]))
                     decls ids))]
             [contextualize-args
              (lambda (args defs)
                (let ([tmp-args 
                       (map 
                        (lambda (id) 
                          (internal-definition-context-apply defs id)) args)])
                  (map 
                   (lambda (id tmp-id)
                     (with-syntax ([tmp-id tmp-id])
                       (syntax/loc id tmp-id))) 
                   args tmp-args)))])
         (syntax-local-bind-syntaxes args #f defs)
         (syntax-local-bind-syntaxes kw-args #f defs)
         (internal-definition-context-seal defs)
         (no-expand
            (with-syntax*
                ([body (local-expand #'func.body (build-expand-context 'expression) #f defs)]
                 [(arg ...) (subs-decl-ids 
                             (syntax->list #'func.args)
                             (contextualize-args args defs))]
                 [(kw-arg ...) (subs-decl-ids
                                (syntax->list #'func.kw-args)
                                (contextualize-args kw-args defs))])
              #'(defun func.storage-classes func.ret-type func.name (arg ... kw-arg ...) body))))])))
  #;((lambda (decl skels)
    (syntax-case decl (def defun)
      [(defun (storage ...) 
         ret-type name (args ...) body)
       (with-syntax ((body (expand-stmt #'body skels)))
         #'(defun (storage ...) ret-type name (args ...) body))]
      [(def ((storage ...) type name init ...) 
         (next-name next-init ...) ...) decl])))

;(define-for-syntax add-names-and-seal)

(define-syntax translation-unit
  (lambda (stx)
    (syntax-parse stx
        [(_ declaration ...+) 
         (let ([top-level-defs (syntax-local-make-definition-context)]
              [ctx (generate-expand-context)])
           (with-syntax 
               ([(declaration ...)
                 (stx-map 
                  (lambda (declaration) 
                    (define out-form 
                      (syntax-parse declaration
                        [defun:fun-decl #'()]
                        [def:decls #'()]))
                    (set! top-level-defs (syntax-local-make-definition-context top-level-defs))
                    out-form)
                  #'(declaration ...))])
             #'(declaration ...)))])))

(define-for-syntax display-inert-body
  (lambda (tag contents) 
    (with-syntax* ([stx-tag tag] 
                   [contents 
                    (stx-map 
                     (lambda (stx) 
                       (let ([expanded (local-expand stx 'top-level #f)])
                         ((walk-expr-safe-ids (make-bound-id-table)) expanded))) 
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



