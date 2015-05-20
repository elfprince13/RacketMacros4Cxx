#lang racket

#;(require macro-debugger/stepper)

#;(define swap-stx 
  (lambda (stx)
    (syntax-case stx (swap)
      [(swap a b) 
       #'(let ([tmp a])
           (set! a b)
           (set! b tmp))])))


#;(define for5-stx
  (lambda (x) 
    (syntax-case x (from to in)
      [(for5 from low to high in bodies ...)
       (with-syntax ([it #'it]);(datum->syntax (syntax for5) 'it)])
         (syntax
          (for5 it from low to high in bodies ...)))]
      [(for5 var from low to high in bodies ...) 
       (syntax
        (local ([define high-value high] 
                [define loop 
                  (lambda (var)
                    (if (> var high-value) 'done
                        (begin
                          bodies ...
                          (loop (+ var 1)))))])
          (loop low)))])))

(require "test-module.rkt")
(define-namespace-anchor test-anchor)
(parameterize ([current-namespace 
                (namespace-anchor->empty-namespace test-anchor)])
  (namespace-require "test-module.rkt")
  (print (namespace-mapped-symbols)) (newline)
  #;(print (expand/step (datum->syntax #f '(for5 from 1 to 10 in (let ((tmp it) (y 5)) (begin (swap tmp y) (print it) (print tmp) (print y))))))) (newline)
  (print (expand-to-top-form (datum->syntax #f '(for5 from 1 to 10 in (let ((tmp it) (y 5)) (begin (swap tmp y) (print it) (print tmp) (print y))))))) (newline))
  
(newline)



#;(let ((tmp 1) (y 2) (x 3))
  (begin
  (swap tmp y)
  (swap tmp x)
  (print tmp)
  (print y)
  (print x)))
(newline)
#;(for5-stx (for5-stx #'(for6 from 1 to 10 in (print it))))
#;(for5 from 1 to 10 in (let ((tmp it) (y 5)) (begin (swap tmp y) (print it) (print tmp) (print y))))
