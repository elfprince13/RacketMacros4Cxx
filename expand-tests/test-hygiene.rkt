#lang racket

(require macro-debugger/stepper)
(define-namespace-anchor test-anchor)
(parameterize ([current-namespace 
                (namespace-anchor->empty-namespace test-anchor)])
  (namespace-require "test-module.rkt")
  (print (namespace-mapped-symbols)) (newline)
  (let ([expand-maybe-step-quoted-form 
         (lambda (quoted-form)
           (let ([stx (datum->syntax #f quoted-form)])
             (expand/step stx)
             (begin (print (expand-to-top-form stx)) (newline))))])
    #;(expand-maybe-step-quoted-form '(for5 from 1 to 10 in (let ((tmp it) (y 5)) (begin (swap tmp y) (print it) (print tmp) (print y)))))
    (expand-maybe-step-quoted-form 
     '(let* ((tmp 1) (j 2)) (swap tmp j) (print tmp) (print j)))))



#;(parameterize ([current-namespace 
                  (namespace-anchor->empty-namespace test-anchor)])
    (namespace-require "test-module.rkt")
    (print (namespace-mapped-symbols)) (newline)
    (print 
     (expand-to-top-form 
      (syntax-property
       (datum->syntax #f 
                      '(defun (__global__) void kernelTest ((() int argc) (() char **argv)) 
                         (begin
                           (@ Loop1d (test-loop) ((i) (0) (argc)) 
                              (call printf "%s\\n" (* ((+ argv (@ I () () ()))))))
                           (call printf "done\\n")))) 
       'init-skeletons-table init-skeletons-table))) 
    (newline))