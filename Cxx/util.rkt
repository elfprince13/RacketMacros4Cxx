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
        #'(args ...))])))

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

;(parse-arg-names #'((() (unsigned long (!)) size)))
;(parse-def-names #'(def (() (int (!)) t1 = 0) ((* (!)) t2 = (& t1))))

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
         [ensure-unique
          (lambda (id-l)
            ;(emit-remark "outside loop:" (car id-l) (~a (syntax-original? (car id-l))))
            (for ([id id-l])
              ;(emit-remark "inside loop:" id (~a (syntax-original? id) (syntax-property id 'original?)))
              (let
                ([safe-id 
                  (if (syntax-original? (syntax-local-introduce id))
                      #;(let ()
                        (display "original: ") (display id) (newline)
                        id)
                      id
                      (let loop
                        ([btv (generate-temporary id)])
                        ;(display "not original: ") (display id) (display btv) (newline)
                        (if (set-member? uniq-table btv)
                            (loop)
                            (begin
                              (dict-set! bind-table id btv)
                              btv))))])
                (set-add! uniq-table safe-id))))]
         [walk-tree
          (lambda (stx-l)
            ;(display "walking ") (display stx-l) (newline)
            (with-syntax
                 ([(seq ...) (stx-map parse-node stx-l)])
                 #'(seq ...)))]
         [parse-node 
          (lambda (stx)
            ;(display "parsing ") (display stx) (newline) 
            (syntax-parse stx
              [func:fun-decl
               ;(emit-local-step (stx-car #'func.args) (car (parse-arg-names #'func.args)) #:id #'parse-node-func)
               ;(emit-remark "The local step shown can be used to test if original is preserved by parse-arg-names")
               (ensure-unique (parse-arg-names #'func.args))
               (walk-tree stx)]
              [decl:cxx-decls
               (ensure-unique (parse-def-names stx))
               (walk-tree stx)]
              [(seq ...)
               ;(display "seq ...") (newline)
               (let ([walked-tree (walk-tree stx)])
                 ;(emit-local-step stx walked-tree #:id #'parse-node-seq)
                 walked-tree)]
              [atom 
               ;(display "atom") (newline)
               (safe-print-id stx)]))]) 
      parse-node)))

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

