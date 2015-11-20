#lang racket
(require 
  macro-debugger/emit
  racket/syntax
  syntax/context
  syntax/parse
  syntax/stx)
(require
  Cxx/define-forms
  (for-template Cxx/core-forms)
  Cxx/syntax-classes
  Cxx/util)

(provide SillyThunk)

(define SillyThunk
  (lambda (params-table) ; This allows the requiring module to pass through important bits of configuration, should they be necessary
   (lambda (skel defs decl-f) 
      (syntax-parse skel
        [skel:macro-@
         (let*
             ([invoke-skel-kind (extract-id-arg #'skel.args 0)]
              [invoke-macro (syntax-local-introduce (macroize-skel-kind invoke-skel-kind))]
              [bind-list (list invoke-macro)]
              [value-skel-kind (extract-id-arg #'skel.args 1)]
              [value-macro (macroize-skel-kind value-skel-kind)]
              [ret-defs (syntax-local-make-definition-context defs)])
           (syntax-local-bind-syntaxes
            bind-list
            #'(let 
                  ([count 0]
                   [instances (make-hasheq)])
                (lambda ([stx #f])
                  (if stx
                      (syntax-parse stx 
                        [invoke-skel:macro-@
                         (hash-set! instances (syntax->datum #'invoke-skel.name) count)
                         (display instances) (newline)
                         (set! count (+ count 1))
                         #'(call invoke-skel.name)])
                      instances)))
                  ret-defs)
           (internal-definition-context-seal ret-defs)
           (values
            (thunk
             (let
                 ([instances ((syntax-local-value (internal-definition-context-apply ret-defs invoke-macro) #f ret-defs))])
               (display "mapping over ") (display instances) (newline)
               (hash-map 
                instances
                (lambda (id ct)
                  (let* 
                      ([ctx (syntax-local-context)]
                       [int-defs (syntax-local-make-definition-context defs)])
                    (syntax-local-bind-syntaxes
                     (list value-macro)
                     (with-syntax ([ct ct]
                                   [decl-f decl-f])
                       #'(syntax-parser
                           [value-skel:macro-@
                            (with-syntax 
                                ([local-id (extract-expr-arg #'value-skel.args 0)])
                              #'(= local-id ct))]))
                     int-defs)
                    (internal-definition-context-seal int-defs)
                    #;(emit-local-step value-skel-kind (internal-definition-context-apply/loc int-defs value-macro) #:id #'macroize-skel-kind)
                    #;(emit-local-step #'skel.child (local-expand
                     #'skel.child ctx #f int-defs) #:id #'skel.name)
                    (decl-f 
                     (with-syntax 
                         ([f-name id])
                       #'(defun () (void (!)) f-name () skel.child))
                     int-defs))))))
            ret-defs
            (list
             (let
                 ([sym (syntax->datum invoke-skel-kind)]
                  [binding (internal-definition-context-apply/loc ret-defs invoke-macro)])
               (cons sym binding)))))]))))