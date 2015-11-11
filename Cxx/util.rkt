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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shortcut routines for various intdef-ctx patterns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Random selection of other helpers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define walk-expr-safe-ids
  (lambda (bind-table uniq-table)
    (letrec 
        ([safe-print-id 
          (lambda (stx)
            (if (identifier? stx)
                (dict-ref bind-table stx stx)
                stx))]
         [walk-expr 
          (lambda (stx)
            (syntax-parse stx
              #;[decl:cxx-decls
               (begin
                 (if (syntax-original? #'arg)
                     (void)
                     (let ([btv (generate-temporary #'arg)])
                       (dict-set! bind-table #'arg btv)))
                 
                 #;(with-syntax 
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
               (with-syntax
                 ([(seq ...) (stx-map walk-expr #'(seq ...))])
                 #'(seq ...))]
              [atom (safe-print-id #'atom)]))]) 
      walk-expr)))

(define string-from-stx 
 (lambda (stx)
   (symbol->string (syntax->datum stx))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Some reusable handlers for various expansion events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define no-expand
  (lambda (stx)
    (syntax-case stx ()
      [(macro args ...)
       (with-syntax 
           ([macro 
             (syntax-local-introduce ; cancel marks to make it clear it's a keyword and not a local binding
              (datum->syntax #f (syntax->datum #'macro) #'macro #'macro))]) ; strip the macro definition to stop expansion
         #'(macro args ...))])))

(define handle-expr-list
  (lambda (stx-l)
    (stx-map
     (lambda (stx)
       (handle-expr stx))
       stx-l)))

(define handle-expr
  (lambda (stx [ctxt 'expression] [defs #f])
    (let* ([pair? (stx-pair? stx)]
           [head (if pair? (stx-car stx) stx)]
           [id? (identifier? head)]
           [macro? (and pair? id? (procedure? (syntax-local-value head (lambda () #f) defs)))])
      (cond 
        [macro? 
         (begin
           ;(display (~a stx "is a macro")) (newline)
           (local-expand stx ctxt #f defs))]
        [(and (not pair?) id?) head #;(syntax-local-introduce head)]
        [pair? 
         (with-syntax
             ([(term ...) (stx-map (lambda (term) (handle-expr term ctxt defs)) stx)])
           #'(term ...))]
        [else stx]))))

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

