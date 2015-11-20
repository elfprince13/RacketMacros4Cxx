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
    (syntax-parser
      [skel:macro-@
       (let*
           ([defs (syntax-local-make-definition-context)]
            [ctx (generate-expand-context)]
            [itr-id #'j]
            [itr-skel-kind (extract-id-arg #'skel.args 0)]
            [itr-macro (macroize-skel-kind itr-skel-kind)]
            [itr-init (extract-expr-arg #'skel.args 1)]
            [itr-final (extract-expr-arg #'skel.args 2)])
         (syntax-local-bind-syntaxes
          (list itr-macro)
          (with-syntax ([itr-id itr-id])
            #'(syntax-parser
                [itr-skel:macro-@
                 (with-syntax 
                     ([itr-id (syntax-local-introduce #'itr-id)]
                      [local-id (extract-expr-arg #'itr-skel.args 0)])
                   #'(= local-id itr-id))]))
          defs)
         (internal-definition-context-seal defs)
         (with-syntax 
             ([itr-id itr-id]
              [itr-init itr-init]
              [itr-final itr-final]
              [child #'skel.child])
           #;(cuda-loop1d #'child)
           (local-expand 
            #'(for ((def (() (int (!)) itr-id = (itr-init))) (< itr-id (itr-final)) (++< itr-id)) 
                child) 
            ctx #f defs)))])))
