#lang racket
(require 
  macro-debugger/emit
  racket/set
  racket/syntax
  syntax/context
  syntax/parse
  syntax/stx)
(require
  Cxx/define-forms
  (for-template Cxx/core-forms)
  Cxx/syntax-classes
  Cxx/util)

(provide (all-defined-out))

(define-values (Min Mod)
  (values 
   (skeleton-factory
    (lambda (params-table)
      (lambda (kind name args)
        (with-syntax
            ([val 
              (apply min (map (compose get-number handle-expr extract-expr-arg) args))])
          #'val))))
   (skeleton-factory
    (lambda (params-table)
      (lambda (kind name args)
        (let*
            ([arg-vs (map (compose get-number handle-expr extract-expr-arg) args)]
             [v (modulo (car arg-vs) (cadr arg-vs))])
          (with-syntax
            ([val v])
          #'val)))))))
