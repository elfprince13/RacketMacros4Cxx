#lang racket/base

(require racket/stxparam
         (for-syntax racket/base
                     racket/set
                     racket/syntax
                     syntax/parse
                     syntax/context
                     ))

;; (syntax-parameter-value vars) : (U #f (MutableSetof Identifier))
(define-syntax-parameter vars #f)

(define-syntax set-accums-zero!
  (syntax-parser
    [(set-accums-zero!)
     #:with [x ...]
     (map syntax-local-introduce (set->list (syntax-parameter-value #'vars)))
     #'(begin (set! x 0) ...)]))

(define-syntax Loop
  (lambda (stx)
  (syntax-parse stx
    [(Loop accum-id:id up-to-expr:expr body:expr ...+)
     #'(syntax-parameterize 
        ([vars (mutable-set)]) 
        (let-syntax 
            ([accum-id
              (lambda (stx)
                (syntax-parse stx
                  [(accum-id x:id dx:expr)
                   (define vs (syntax-parameter-value #'vars))
                   ; Note: 
                   ; this has bad effects, because each expansion shares a parameter value, 
                   ; rather than each Loop/Accum instantiation
                   ; See "my-def-stx" example below for a fix
                   (unless (set-mutable? vs) 
                     (raise-syntax-error #f "cannot be used outside Loop" stx))
                   (set-add! vs (syntax-local-introduce #'x))
                   #'(set! x (+ x dx))]))])
          (let ([up-to up-to-expr])
            (letrec
                ([loop
                  (lambda (n)
                    body
                    ...
                    (if (< n up-to)
                        (loop (add1 n))
                        (void)))])
              (set-accums-zero!)
              (loop 0)))))]
    [(Loop up-to-expr:expr body:expr ...+)
     (with-syntax 
         ([Accum (datum->syntax stx 'Accum)])
       #'(Loop Accum up-to-expr body ...))])))

(let ([w "w"] [x "x"] [y "y"] [z "z"])
  (Loop 5
        (begin
          (define-values
            [dw dx dy dz]
            (values 1 2 3 4))
          (Accum w dw)
          (Accum x dx)
          (Loop AccumInner 5
                (printf "w = ~v, x = ~v, y = ~v, z = ~v\n" w x y z)
                (Accum y dy)
                (AccumInner z dz)))))

(define-syntax my-def-stx
  (lambda (stx)
    (syntax-case stx (my-def-stx)
      [(my-def-stx (id ...) rhs expr)
       #;#'(letrec-syntaxes+values ([(id ...) rhs]) () expr)
       (let* ([intdef (syntax-local-make-definition-context)]
             [ctx (generate-expand-context)])
           (syntax-local-bind-syntaxes (syntax->list #'(id ...)) (local-transformer-expand #'rhs 'expression null) intdef)
           (internal-definition-context-seal intdef)
           (define ret (local-expand #'expr ctx null intdef))
         
         (display ((syntax-local-value 
                   (internal-definition-context-apply intdef (car (syntax->list #'(id ...)))) 
                   #f intdef) #'j)) (newline)
         ret
           
         
       )])))

(my-def-stx 
 (j i) (let ([secret 1]) 
         (values 
          (lambda (stx)
            (with-syntax ([secret secret]) #'secret))
          (lambda (stx) 
            (set! secret (+ secret 1))
            (with-syntax ([secret secret]) #'secret))))
 (values
 (+ i j)
 (+ j i)))
