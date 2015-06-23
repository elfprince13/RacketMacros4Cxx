#lang racket/base

(require racket/stxparam
         (for-syntax racket/base
                     racket/set
                     racket/syntax
                     syntax/parse
                     syntax/stx
                     syntax/context))

(define-syntax Loop
  (lambda (stx)
    (syntax-parse stx
      [(Loop accum-id:id up-to-expr:expr body:expr ...+)
       (let* ([defs (syntax-local-make-definition-context)]
              [ctx (generate-expand-context)])
         (syntax-local-bind-syntaxes 
          (list #'accum-id) 
          (local-transformer-expand 
           #'(let ([vars (mutable-set)])
               (lambda ([stx #f])
                 (if (syntax? stx)
                     (syntax-parse stx
                       [(accum-id x:id dx:expr)
                        (set-add! vars (syntax-local-introduce #'x))
                        #'(set! x (+ x dx))])
                     vars))) 'expression null) defs)
         (internal-definition-context-seal defs)
         (with-syntax* ([(body ...) 
                         (stx-map 
                          (lambda (body)
                            (with-syntax ([body body])
                              (local-expand #'body ctx null defs)))
                          #'(body ...))]
                        [(init ...) 
                         (map 
                          (lambda (var) 
                            (with-syntax ([var (syntax-local-introduce var)]) #'(set! var 0))) 
                          (set->list 
                           ((syntax-local-value 
                             (internal-definition-context-apply defs #'accum-id)
                             #f defs))))])
           #'(let ([up-to up-to-expr])
               (letrec
                   ([loop
                     (lambda (n)
                       body ...
                       (let ([n (add1 n)])
                         (if (< n up-to)
                             (loop n)
                             (void))))])
                 init ...
                 (loop 0)))))]
      [(Loop up-to-expr:expr body:expr ...+)
       (with-syntax 
           ([Accum (datum->syntax stx 'Accum)])
         #'(Loop Accum up-to-expr body ...))])))

(let ([x (void)] [y (void)] [z (void)])
  (Loop 2
        (let ([dx 1] [dy 2] [dz 3])
          (Accum x dx)
          (Loop AccumInner 2
                (printf "x = ~v, y = ~v, z = ~v\n" x y z)
                (Accum y dy)
                (AccumInner z dz)))))
