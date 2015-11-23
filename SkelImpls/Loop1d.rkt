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
  (skeleton-factory
   (lambda (params-table) ; This allows the requiring module to pass through important bits of configuration, should they be necessary
     (lambda (kind name args child)
       (let*
           ([defs (syntax-local-make-definition-context)]
            [ctx (generate-expand-context)]
            [itr-id #'j]
            [itr-skel-kind (extract-id-arg args 0)]
            [itr-macro (macroize-skel-kind itr-skel-kind)]
            [itr-init (extract-expr-arg args 1)]
            [itr-final (extract-expr-arg args 2)])
         (syntax-local-bind-syntaxes
          (list itr-macro)
          (with-syntax ([itr-id itr-id])
            #'(skeleton-factory
               (thunk*
                (with-syntax 
                    ([itr-id (syntax-local-introduce #'itr-id)])
                  #'itr-id))
               #:no-table #t))
          defs)
         (internal-definition-context-seal defs)
         (with-syntax 
             ([itr-id itr-id]
              [itr-init itr-init]
              [itr-final itr-final]
              [child child])
           (local-expand 
            #'(for ((def (() (int (!)) itr-id = (itr-init))) (< itr-id (itr-final)) (++< itr-id)) 
                child) 
            ctx #f defs)))))))
