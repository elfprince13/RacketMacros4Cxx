#lang racket
(require macro-debugger/emit
         racket/format
         racket/dict
         racket/syntax
         (for-syntax racket/syntax)
         syntax/context
         syntax/id-table
         syntax/parse
         "syntax-classes.rkt"
         syntax/stx)


(provide (all-defined-out))

(define internal-definition-context-apply/loc
  (lambda (def-ctx id) 
    (with-syntax
        ([tmp-id (internal-definition-context-apply def-ctx id)])
      (syntax/loc id tmp-id))))

(define expand-and-extend
  (lambda (stx ctx defs [stoplist #f])
    (values
     (local-expand stx ctx stoplist defs)
     (syntax-local-make-definition-context defs))))

(define bind-and-seal
  (lambda (defs id-list [bind-to #f])
    (syntax-local-bind-syntaxes id-list bind-to defs)
    (internal-definition-context-seal defs)))



#;(define loop-decls-and-bodies 
    (lambda (parser decl-handler body-handler)
      (let*-values 
          ([(decl-pat decl-extractor decl-responder) decl-handler]
           [(body-pat body-expander) body-handler]
           [loop
            (lambda (stx)
              (syntax-parse stx
                [decl-pat ]
                [body-pat ]))])
        )))

(define contextualize-args
  (lambda (args defs)
    (map 
     (lambda (id)
       (internal-definition-context-apply/loc defs id)) args)))

(define subs-decl-ids
  (lambda (decls ids)
    (map 
     (lambda (decl id)
       (syntax-parse decl
         [var:var-decl 
          (with-syntax ([name id])
            #`(var.storage-classes var.type-info name #,@#'var.init-exp))]))
     decls ids)))

(define parse-arg-names
  (lambda (stx) 
    (syntax-case stx () 
      [(args ...) 
       (stx-map 
        (lambda (arg)
          (syntax-parse arg
            [decl:var-decl 
             #'decl.name])) 
        #'(args ...) )])))

(define parse-def-names
  (lambda (stx) 
    (syntax-parse stx 
      [vars:decls 
       (map
        (lambda (stx)
          (syntax-parse stx
            [var:var-decl #'var.name]
            [(var:var-init) #'var.name]))
        (cons
         #'vars.var
         (syntax->list #'vars.extra-vars)))])))

#;(parse-arg-names #'((() int a (5)) (() float b = 6.0) (() char c)))


(define walk-expr-safe-ids
  (lambda (bind-table)
    (letrec 
        ([safe-print-id 
          (lambda (stx)
            (if (identifier? stx)
                (dict-ref bind-table stx stx)
                stx))]
         [walk-expr 
          (lambda (stx)
            (syntax-case stx () ; bizarre interaction with syntax-case and lambda: can't have lambda as a keyword argument, or the syntax-case breaks
              [(let* ((arg expr)) bodies ...)
               (begin
                 (if (syntax-source #'arg)
                     (void)
                     (let ([btv (generate-temporary #'arg)])
                       (dict-set! bind-table #'arg btv)))
                 
                 (with-syntax 
                     ([arg 
                       (safe-print-id #'arg)]
                      [expr
                       (walk-expr #'expr)]
                      [(bodies ...) 
                       (stx-map
                        (lambda (stx)
                          (walk-expr stx)) #'(bodies ...))])
                   #'(let* ((arg expr)) bodies ...)))]
              [(seq ...)
               (begin
                 (stx-map walk-expr #'(seq ...)))]
              [atom 
               (begin
                 (safe-print-id #'atom))]))]) 
      walk-expr)))

(define string-from-stx 
 (lambda (stx)
   (symbol->string (syntax->datum stx))))

(define no-expand
  (lambda (stx)
    (syntax-case stx ()
      [(macro args ...)
       (with-syntax 
           ([macro 
             (syntax-local-introduce ; cancel marks to make it clear it's a keyword and not a local binding
              (datum->syntax #f (syntax->datum #'macro) #'macro #'macro))]) ; strip the macro definition to stop expansion
         #'(macro args ...))])))

(define-syntax handle-init
  (lambda (stx)
    (syntax-case stx ()
      [(handle-init init-exp null-case eq-case paren-case)
       #'(if (stx-null? init-exp)
             (null-case)
             (let ([head (stx-car init-exp)]
                   [exp (stx-cdr init-exp)])
               (if (eq? '= (syntax->datum head))
                   (eq-case exp)
                   (paren-case init-exp))))])))

#;(define-syntax print
    (lambda (stx)
      (syntax-case stx (print)
        [(print id) (no-expand stx)])))

#;(define-syntax dummy
    (lambda (stx)
      (syntax-case stx ()
        [(dummy) (no-expand stx)])))

#;(define-syntax set!
    (lambda (stx) 
      (syntax-case stx (set!)
        [(set! id val) 
         (no-expand stx)])))

#;(define-syntax let*
    (lambda (stx)
      (syntax-case stx (let*)
        [(let* ((id expr)) bodies ...)
         (let ([intdef (syntax-local-make-definition-context)])
           (syntax-local-bind-syntaxes (list #'id) #f intdef)
           (internal-definition-context-seal intdef)
           (no-expand
            (with-syntax*
                ([(bodies ...) 
                  (stx-map 
                   (lambda (body)
                     (with-syntax ([body body])
                       (local-expand #'body (build-expand-context 'expression) #f intdef)))
                   #'(bodies ...))]
                 [id-tmp (internal-definition-context-apply intdef #'id)]
                 [id (syntax/loc #'id id-tmp)]
                 [expr (local-expand #'expr 'expression #f)]) ; Strip the original racket definitions!
              #'(let* ((id expr)) bodies ...))))]
        [(let* ((id expr) (more-ids more-exprs) ...) bodies ...)
         (with-syntax
             ([body #'(let* ((more-ids more-exprs) ...) bodies ...)])
           #'(let* ((id expr)) body))])))

#;(define-syntax swap 
    (lambda (stx)
      (syntax-case stx (swap)
        [(swap a b) 
         (with-syntax ()
           #'(let* ([tmp a])
               (set! a b)
               (set! b tmp)))])))

