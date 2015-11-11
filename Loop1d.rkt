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
  (lambda (skel) 
    (syntax-parse skel
      [(_ (name:id) args:skeleton-args child:cxx-stmt)
       (let*
           ([defs (syntax-local-make-definition-context)]
            [ctx (generate-expand-context)]
            [itr-id #'j]
            [itr-init (stx-cdr (stx-car (stx-cdr #'args)))]
            [itr-final (stx-cdr (stx-car (stx-cdr (stx-cdr #'args))))]
            [itr-skel-kind (stx-car (stx-cdr (stx-car #'args)))]
            [itr-macro (macroize-skel-kind itr-skel-kind)])
         (syntax-local-bind-syntaxes
          (list itr-macro)
          (local-transformer-expand 
           (with-syntax ([itr-id itr-id])
             #'(lambda (stx)
                 (syntax-parse stx
                   [(_ (name:id) inner-args:skeleton-args child:cxx-stmt)
                    (with-syntax 
                        ([itr-id (syntax-local-introduce #'itr-id)]
                         [local-id (stx-car (stx-cdr (stx-car #'inner-args)))])
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
            #'(for ((def (() (int (!)) itr-id = itr-init)) (< itr-id itr-final) (++< itr-id)) 
                child) 
            ctx #f defs)))])))
