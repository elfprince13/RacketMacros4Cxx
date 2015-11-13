#lang racket
(require 
  syntax/context
  syntax/parse
  syntax/stx)
(require
  (for-template Cxx/core-forms)
  Cxx/syntax-classes
  Cxx/util)

(provide Loop1d)

(define Loop1d
  (lambda (params-table) ; This allows the requiring module to pass through important bits of configuration, should they be necessary
    (lambda (skel) 
      (syntax-parse skel
        [(_ (name:id) args:skeleton-args child:cxx-stmt)
         (let*
             ([defs (syntax-local-make-definition-context)]
              [ctx (generate-expand-context)]
              [itr-id #'j]
              [itr-skel-kind (extract-id-arg #'args 0)]
              [itr-macro (macroize-skel-kind itr-skel-kind)]
              [itr-init (extract-expr-arg #'args 1)]
              [itr-final (extract-expr-arg #'args 2)])
           (syntax-local-bind-syntaxes
            (list itr-macro)
            (local-transformer-expand 
             (with-syntax ([itr-id itr-id])
               #'(lambda (stx)
                   (syntax-parse stx
                     [(_ (name:id) inner-args:skeleton-args child:cxx-stmt)
                      (with-syntax 
                          ([itr-id (syntax-local-introduce #'itr-id)]
                           [local-id (extract-expr-arg #'inner-args 0)])
                        #'(= local-id itr-id))])))
             'expression null) 
            defs)
           (internal-definition-context-seal defs)
           (with-syntax 
               ([itr-id itr-id]
                [itr-init itr-init]
                [itr-final itr-final]
                [child #'child])
             #;(cuda-loop1d #'child)
             (local-expand 
              #'(for ((def (() (int (!)) itr-id = (itr-init))) (< itr-id (itr-final)) (++< itr-id)) 
                  child) 
              ctx #f defs)))]))))
