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

(provide Min)

(define Min
  (lambda (params-table)
    (syntax-parser
      [skel:macro-@expr
       (let
           ([args
             (stx-map
              (compose get-number handle-expr extract-expr-arg)
              #'skel.args)])
         (with-syntax
             ([val (apply min args)])
           #'val))])))