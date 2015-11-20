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
    (syntax-parser
      [skel:macro-@
       (let*
           ([rep-skel-kind (extract-id-arg #'skel.args 0)]
            [rep-macro (macroize-skel-kind rep-skel-kind)]
            [rep-ct (syntax->datum (extract-expr-arg #'skel.args 1))])
         (no-expand 
          (with-syntax 
              ([(child ...)
                (map
                 (lambda (rep-ct
                          [defs (syntax-local-make-definition-context)]
                          [ctx (generate-expand-context)])
                   (syntax-local-bind-syntaxes
                    (list rep-macro)
                    (with-syntax ([itr-val rep-ct])
                      #'(syntax-parser
                          [rep-skel:macro-@
                           (with-syntax 
                               ([itr-val itr-val]
                                [local-id (extract-expr-arg #'rep-skel.args 0)])
                             #'(= local-id itr-val))]))
                    defs)
                   (internal-definition-context-seal defs)
                   (local-expand #'skel.child ctx #f defs))
                 (range rep-ct))])
            #'(block child ...))))])))
