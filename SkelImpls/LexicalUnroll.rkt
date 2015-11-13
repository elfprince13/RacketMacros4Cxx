#lang racket
(require 
  syntax/context
  syntax/parse
  syntax/stx)
(require
  (for-template Cxx/core-forms)
  Cxx/syntax-classes
  Cxx/util)

(provide Repeat)

(define Repeat
  (lambda (params-table) ; This allows the requiring module to pass through important bits of configuration, should they be necessary
    (lambda (skel) 
      (syntax-parse skel
        [(_ (name:id) args:skeleton-args child:cxx-stmt)
         (let*
             ([rep-skel-kind (extract-id-arg #'args 0)]
              [rep-macro (macroize-skel-kind rep-skel-kind)]
              [rep-ct (syntax->datum (extract-expr-arg #'args 1))])
           (no-expand 
            (with-syntax 
                ([(child ...)
                  (map
                   (lambda (rep-ct
                            [defs (syntax-local-make-definition-context)]
                            [ctx (generate-expand-context)])
                     (syntax-local-bind-syntaxes
                      (list rep-macro)
                      (local-transformer-expand 
                       (with-syntax ([itr-val rep-ct])
                         #'(lambda (stx)
                             (syntax-parse stx
                               [(_ (name:id) inner-args:skeleton-args child:cxx-stmt)
                                (with-syntax 
                                    ([itr-val itr-val]
                                     [local-id (extract-expr-arg #'inner-args 0)])
                                  #'(= local-id itr-val))])))
                       'expression null) 
                      defs)
                     (internal-definition-context-seal defs)
                     (local-expand #'child ctx #f defs))
                   (range rep-ct))])
               #'(block child ...))))]))))