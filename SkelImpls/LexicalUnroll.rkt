#lang racket
(require 
  racket/syntax
  syntax/parse)
(require
  (for-template Cxx/core-forms)
  Cxx/syntax-classes
  Cxx/util)

(provide (all-defined-out))

(define-values (Repeat ParamContext LetLexicalArray)
  (let*-values
      ([(macro-arg value-arg) 
                     (values
                      (compose macroize-skel-kind extract-id-arg)
                      (compose get-number handle-expr extract-expr-arg))]
       [(repeat-handle)
        (skeleton-factory
         (lambda (params-table)
           (lambda (kind name args child)
             (let*-values
                 ([(rep-macro) (macro-arg (car args))]
                  [(from to by) (as-values value-arg (cdr args))])
               (no-expand 
                (with-syntax 
                    ([(child ...)
                      (map
                       (lambda (rep-ct)
                         (expand-with-macros
                          (list rep-macro)
                          (with-syntax ([itr-val rep-ct])
                            #'(syntax-parser
                                [rep-skel:macro-@expr #'itr-val])) child))
                       (range from to by))])
                  #'(block child ...)))))))]
       [(context-handle)
        (skeleton-factory
         (lambda (params-table)
           (let ([pc-params (hash-ref params-table '@ParamContext (thunk (raise-user-error '@ParamContext "This params table missing required entry of @ParamContext")))])
             (lambda (kind name args child)
               (when (> (length args) 3)
                 (raise-user-error '@ParamContext (~a (list "This skeleton only takes 3 arguments, received" args))))
               (let*-values
                   ([(pm lb ub)
                     (as-values (list macro-arg value-arg value-arg) args)])
                 (expand-with-macros
                  (list pm)
                  (with-syntax ([lb lb] [ub ub] [pc-params pc-params])
                      #'(skeleton-factory
                         (lambda (kind name args [child #'()])
                           (let*
                               ([k-sym (syntax->datum kind)]
                                [n-sym (if name 
                                           (syntax->datum name)
                                           (raise-user-error k-sym (~a (list k-sym "requires a name"))))]
                                [p-val 
                                 (hash-ref 
                                  (hash-ref pc-params k-sym (thunk (raise-user-error k-sym (~a (list "This params table missing required entry @ParamContext / " k-sym)))))
                                  n-sym (thunk (raise-user-error k-sym (~a (list "This params table missing required entry @ParamContext / " k-sym "(" n-sym ")")))))])
                             (unless (and (< p-val ub) (> p-val lb))
                               (raise-user-error k-sym (~a (list "Param value" p-val "outside instantiated bounds" lb ub))))
                             (with-syntax ([p-val p-val]) #'p-val)))
                       #:no-table #t)) child))))))]
       [(array-handle)
         (skeleton-factory
          (lambda (params-table)
            (lambda (kind name args child)
              (let*-values 
                  ([(access-sk args) (values (extract-id-arg (car args)) (cdr args))]
                   [(access-m) (macroize-skel-kind access-sk)]
                   [(base-def count) (values (extract-stmt-arg (car args)) (value-arg (cadr args)))]
                   [(all-defs all-vars)
                    (syntax-parse base-def
                      [decl:decls 
                       (syntax-parse #'decl.var
                         [var:var-decl
                          (let ([paired
                          (map
                           (lambda (i)
                             (with-syntax*
                                 ([v-name (format-id #f "~a~a" #'var.name i #:source #'var.name #:props #'var.name)]
                                  [var (car (subs-decl-ids (list #'var) (list #'v-name)))])
                               (cons #'var #'v-name)))
                           (range count))])
                           (values (map car paired) (map cdr paired)))])]
                      [else (raise-user-error '@LetLexicalArray "This macro requires a def as the second argument")])])
                (expand-with-macros
                 (list access-m)
                 (with-syntax 
                     ([(v-name ...) all-vars]
                      [count count]) 
                   #'(skeleton-factory
                      (lambda (kind name args [child #'()])
                        (let*-values
                            ([(names) (syntax->list #'(v-name ...))]
                             [(idx args) (values 
                                          (get-number (handle-expr (extract-expr-arg (car args)))) 
                                          (cdr args))]
                             [(asgn) (if (null? args) #f (extract-expr-arg (car args)))])
                        (with-syntax
                            ([var (syntax-local-introduce (list-ref names idx))])
                          (if asgn
                              (with-syntax ([asgn asgn]) #'(= var asgn))
                              #'var))))
                      #:no-table #t)) 
                 (with-syntax ([(v-def ...) all-defs][child child])
                   #'(block 
                      (def v-def) ...
                      child)))))))])
    (values repeat-handle context-handle array-handle)))
