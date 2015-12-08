#lang racket
(require
  (for-template Cxx/core-forms)
  Cxx/util)

(provide Loop1d)

(define Loop1d
  (skeleton-factory
   (lambda (params-table) ; This allows the requiring module to pass through important bits of configuration, should they be necessary
     (lambda (kind name args child)
       (let*-values ([(itr-id) #'j]
                     [(itr-macro) (macroize (extract-id-arg (car args)))]
                     [(itr-init itr-final) (as-values extract-expr-arg (cdr args))])
         (expand-with-macros
          (list itr-macro)
          (with-syntax ([itr-id itr-id])
            #'(skeleton-factory
               (thunk*
                (with-syntax ([itr-id (syntax-local-introduce #'itr-id)]) #'itr-id)) #:no-table #t))
          (with-syntax 
             ([itr-id itr-id][itr-init itr-init][itr-final itr-final][child child])
           #'(for ((def (() (int (!)) itr-id = (itr-init))) (< itr-id (itr-final)) (++< itr-id)) child))))))))
