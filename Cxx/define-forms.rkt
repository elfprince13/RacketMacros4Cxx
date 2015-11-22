#lang racket
(require 
  macro-debugger/emit
  racket/syntax
  syntax/context
  syntax/parse
  (for-syntax syntax/parse)
  syntax/stx
  "syntax-classes.rkt"
  "util.rkt")

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;
; Define definitions
;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax init-var-to-context
  (syntax-parser 
    [(ivtc stx ctx defs)
     #'(syntax-parse stx
      [(var:var-init)
       (let*-values 
           ([(name) #'var.name]
            [(name defs expr) 
             (handle-init 
              #'var.exp
              (thunk
               (let ([defs (syntax-local-make-definition-context defs)])
                 (bind-and-seal defs (list name))
                 (values 
                  (internal-definition-context-apply/loc defs name) 
                  defs 
                  #'())))
              (lambda (eq-expr) 
                (let-values 
                    ([(eq-expr defs)
                      (expand-and-extend eq-expr ctx defs)])
                  (bind-and-seal defs (list name))
                  (values 
                   (internal-definition-context-apply/loc defs name) 
                   defs 
                   (with-syntax 
                       ([eq-expr eq-expr])
                     #'(= . eq-expr)))))
              (lambda (paren-expr) 
                (let-values 
                    ([(paren-expr defs)
                      (expand-and-extend paren-expr ctx defs)])
                  (bind-and-seal defs (list name))
                  (values 
                   (internal-definition-context-apply/loc defs name) 
                   defs 
                   (with-syntax 
                       ([paren-expr paren-expr])
                     #'paren-expr)))))])
         (values 
          defs
          (with-syntax
              ([name name]
               [expr expr])
            #'(name . expr))))])]))

(define defun
  (lambda (stx ctx defs [sneaky-bindings #f])
    (syntax-parse stx
      [func:fun-decl
       (emit-local-step #'func.name 
                        (internal-definition-context-apply/loc defs #'func.name) #:id #'defun)
                                   
       #;(emit-local-step #'func.body (local-expand
                     #'func.body ctx #f defs) #:id #'defun)
       (let ([defs (syntax-local-make-definition-context defs)]
             [ctx (build-expand-context ctx)]
             [args (parse-arg-names #'func.args)]
             [kw-args (parse-arg-names #'func.kw-args)])
         (syntax-local-bind-syntaxes args #f defs)
         (syntax-local-bind-syntaxes kw-args #f defs)
         (when sneaky-bindings
           (syntax-local-bind-syntaxes (car sneaky-bindings) (cdr sneaky-bindings) defs))
         (internal-definition-context-seal defs)
         (no-expand
          (if (stx-null? #'func.body) 
              stx
              (with-syntax*
                  ([f-name (internal-definition-context-apply/loc defs #'func.name)]
                   [body (local-expand #'func.body ctx #f defs)]
                   [(arg ...) 
                    (subs-decl-ids 
                     (syntax->list #'func.args)
                     (contextualize-args args defs))]
                   [(kw-arg ...) 
                    (subs-decl-ids
                     (syntax->list #'func.kw-args)
                     (contextualize-args kw-args defs))]
                   [((attribute-term ...) ...) ; This is a splicing class so jam all the terms together
                    #'func.attributes])
                (emit-local-step #'func.name #'f-name #:id #'defun2)
                #;'die
                #'(func.defun func.storage-classes func.ret-type f-name (arg ... kw-arg ...) attribute-term ... ... body)))))])))

(define def
  (lambda (stx ctx defs)
    (syntax-parse stx
      [vars:decls
       (let*-values 
           ([(defs vars)
             (let loop
               ([work (cons
                       #'vars.var
                       (syntax->list #'vars.extra-vars))]
                [defs defs]
                [ctx ctx])
               (if (null? work)
                   (values defs null)
                   (let-values ([(head rest) (values (car work) (cdr work))])
                     ;(display head) (display " ") (display rest) (newline)
                     (let-values 
                         ([(defs head)
                           (syntax-parse head
                             [decl:var-decl
                              (let-values 
                                  ([(defs init) (init-var-to-context #'decl.init ctx defs)])
                                (values 
                                 defs 
                                 #`(decl.storage-classes decl.type-info #,@init #,@#'decl.attributes)))]
                             [(init:var-init) 
                              (let-values
                                  ([(defs init) (init-var-to-context #'init ctx defs)])
                                (values defs init))])])
                       (let-values ([(defs rest) (loop rest defs ctx)]) 
                         (values defs (cons head rest)))))))]
            [(def-stx) 
              (no-expand 
               (with-syntax*
                   ([(extra-types ...) #'vars.extra-type-infos]
                    [var (car vars)]
                    [(extra-vars ...) (cdr vars)]
                    [(extras ...) 
                     (stx-map 
                      (lambda (t-stx v-stx)
                        (with-syntax
                            ([(type ...) t-stx]
                             [(init ...) v-stx])
                          #'((type ...) init ...)))
                      #'(extra-types ...)
                      #'(extra-vars ...))]) 
                 #'(vars.def var extras ...)))])
         (values 
          defs 
          def-stx))])))
