#lang racket
(require 
  syntax/context
  syntax/parse
  syntax/stx)
(require
  (for-template Cxx/core-forms)
  Cxx/syntax-classes
  Cxx/util)

(provide Reinclude)

(define Reinclude
  (lambda (params-table) ; This allows the requiring module to pass through important bits of configuration, should they be necessary
    (lambda (skel defs)
      (syntax-parse skel
        [skel:macro-@ 
         (values
          (thunk 
           (with-syntax
               ([blob 
                 (string-join 
                  (stx-map
                   (lambda (arg)
                     (string-append
                      "#include "
                      (syntax->datum (stx-car (stx-cdr arg)))
                      "\n"))
                   #'skel.args))])
             #'((verbatim blob))))
          defs
          null)]))))