#lang racket
(require 
  syntax/context
  syntax/parse
  syntax/stx)
(require
  (for-template Cxx/core-forms)
  Cxx/syntax-classes
  Cxx/util)

(provide (all-defined-out))

(define-values (Repeat Unroll)
  (let*
      ([parse-handle
        (skeleton-factory
         (lambda (params-table)
           (lambda (kind name args child)
             (let*-values
                 ([(rep-skel-kind) (extract-id-arg args 0)]
                  [(rep-macro) (macroize-skel-kind rep-skel-kind)]
                  [(from to by) (apply values (stx-map (compose get-number handle-expr extract-expr-arg) (cdr args)))])
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
                                [rep-skel:macro-@expr #'itr-val]))
                          defs)
                         (internal-definition-context-seal defs)
                         (local-expand child ctx #f defs))
                       (range from to by))])
                  #'(block child ...)))))))])
    (values parse-handle (void))))
