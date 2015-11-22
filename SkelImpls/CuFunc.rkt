#lang racket
(require 
  macro-debugger/emit
  racket/set
  racket/syntax
  syntax/context
  syntax/parse
  syntax/stx)
(require
  Cxx/define-forms
  (for-template Cxx/core-forms)
  Cxx/syntax-classes
  Cxx/util)

(provide CuFunc)

(define CuFunc
  (lambda (params-table) ; This allows the requiring module to pass through important bits of configuration, should they be necessary
   (lambda (skel defs) 
      (syntax-parse skel
        [skel:macro-@
         (let*-values
             ([(base-name) #'skel.name]
              [(args) (stx->list #'skel.args)]
              [(skel-kinds storage-id fun-args)
               (let*-values ([(id-args stmt-args) (split-at args 3)]
                             [(skel-args storage-args) (split-at (map extract-id-arg id-args) 2)])
                 (values
                  skel-args
                  (car storage-args)
                  (map
                   (compose 
                    (syntax-parser
                      [var:decls
                       #'var.var])
                    extract-stmt-arg)
                   stmt-args)))]
              [(invoke-skel-kind size-skel-kind) (apply values skel-kinds)]
              [(invoke-macro size-macro) 
               (apply values (map macroize-skel-kind skel-kinds))]
              [(invoke-macro) (syntax-local-introduce invoke-macro)]
              [(bind-list) (list invoke-macro)]
              [(ret-defs) (syntax-local-make-definition-context defs)])
           (syntax-local-bind-syntaxes
            bind-list
            (with-syntax ()
              #'(let ([instances (mutable-set)])
                  (lambda ([stx #f])
                    (if stx
                        (syntax-parse stx 
                          [invoke-skel:macro-@
                           (let 
                               ([val (get-number (handle-expr (extract-expr-arg #'invoke-skel.args 0)))])
                             (set-add! instances val)
                             (with-syntax 
                                 ([fname (format-id #'skel.name "~a~a" #'skel.name val #:source #'skel.name #:props #'skel.name)]
                                  [(arg (... ...)) 
                                   (map
                                    (lambda (i)
                                      (extract-expr-arg #'invoke-skel.args i))
                                    (range 1 (length (stx->list #'invoke-skel.args))))])
                               #'(call fname arg (... ...))))])
                        instances))))
            ret-defs)
           (internal-definition-context-seal ret-defs)
           (values
            (thunk
             (let
                 ([instances ((syntax-local-value (internal-definition-context-apply ret-defs invoke-macro) #f ret-defs))])
               ;(display "mapping over ") (display instances) (newline)
               (set-map 
                instances
                (lambda (val)
                  (let* 
                      ([ctx (syntax-local-context)])
                    (defun 
                      (with-syntax 
                          ([fname (format-id base-name "~a~a" base-name val #:source base-name #:props base-name)]
                           [(fun-arg ...) fun-args]
                           [cu-attr storage-id])
                        #'(defun () (void (!)) fname (fun-arg ...) __attribute__ ((cu-attr)) skel.child))
                      ctx
                      defs
                      (cons 
                       (list size-macro)
                       (with-syntax ([val val])
                         #'(syntax-parser
                             [value-skel:macro-@expr #'val])))))))))
            ret-defs
            (list
             (let
                 ([sym (syntax->datum invoke-skel-kind)]
                  [binding (internal-definition-context-apply/loc ret-defs invoke-macro)])
               (cons sym binding)))))]))))