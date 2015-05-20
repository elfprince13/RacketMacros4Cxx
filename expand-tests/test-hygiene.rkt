#lang racket

#;(require macro-debugger/stepper)
#;(require "test-module.rkt")
(define-namespace-anchor test-anchor)
(parameterize ([current-namespace 
                (namespace-anchor->empty-namespace test-anchor)])
  (namespace-require "test-module.rkt")
  (print (namespace-mapped-symbols)) (newline)
  ;(print (expand/step (datum->syntax #f '(for5 from 1 to 10 in (let ((tmp it) (y 5)) (begin (swap tmp y) (print it) (print tmp) (print y))))))) (newline)
  (print (expand-to-top-form (datum->syntax #f '(for5 from 1 to 10 in (let ((tmp it) (y 5)) (begin (swap tmp y) (print it) (print tmp) (print y))))))) (newline))