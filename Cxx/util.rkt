#lang racket
(require json
         racket/format
         racket/syntax
         syntax/context
         syntax/id-table
         syntax/parse
         (for-syntax syntax/parse)
         "syntax-classes.rkt"
         syntax/stx)


(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Shortcut routines for various intdef-ctx patterns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (internal-definition-context-apply/loc def-ctx id) 
    ;(display "idca/l ") (display id) (newline)
    (with-syntax
        ([tmp-id (internal-definition-context-apply def-ctx id)])
      (syntax/loc id tmp-id)))

(define-syntax expand-and-extend
  (syntax-parser
    [(eae stx ctx defs (~optional stoplist))
     (with-syntax
         ([stoplist 
           (if (attribute stoplist)
               #'stoplist
               #'#f)])
       #'(values
          (handle-expr stx ctx defs)
          #;(local-expand stx ctx stoplist defs)
          (syntax-local-make-definition-context defs)))]))

(define-syntax bind-and-seal
  (syntax-parser
    [(bas defs id-list (~optional bind-to))
     (with-syntax
         ([bind-to 
           (if (attribute bind-to)
               #'bind-to
               #'#f)])
       #'(begin
           (syntax-local-bind-syntaxes id-list bind-to defs)
           (internal-definition-context-seal defs)))]))

(define (contextualize-args args defs)
    (map 
     (lambda (id)
       (internal-definition-context-apply/loc defs id)) args))

(define (subs-decl-ids decls ids)
  (map 
   (lambda (decl id)
     (syntax-parse decl
       [var:var-decl 
        (with-syntax ([name id]
                      [(attr ...) #'var.attributes])
          #`(var.storage-classes var.type-info name #,@#'var.init-exp attr ...))]))
   decls ids))

(define parse-arg-names
  (syntax-parser
    [(args ...) 
     (stx-map 
      (syntax-parser [decl:var-decl #'decl.name]) 
      #'(args ...))]))

(define parse-def-names 
  (syntax-parser 
    [vars:decls 
     (map
      (syntax-parser [var:var-decl #'var.name][(var:var-init) #'var.name])
      (cons #'vars.var (syntax->list #'vars.extra-vars)))]))

;(parse-arg-names #'((() (unsigned long (!)) size)))
;(parse-def-names #'(def (() (int (!)) t1 = 0) ((* (!)) t2 = (& t1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Random selection of other helpers.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (walk-decls leaf-f decl-f iter-f)
  (letrec 
      ([parse-node 
        (lambda (stx)
          ;(display "parsing ") (display stx) (newline) 
          (syntax-parse stx
            [func:fun-decl
             ;(emit-local-step (stx-car #'func.args) (car (parse-arg-names #'func.args)) #:id #'parse-node-func)
             ;(emit-remark "The local step shown can be used to test if original is preserved by parse-arg-names")
             (decl-f (parse-arg-names #'func.args))
             (iter-f parse-node stx)]
            [decl:cxx-decls
             (decl-f (parse-def-names stx))
             (iter-f parse-node stx)]
            [(seq ...)
             ;(display "seq ...") (newline)
             (iter-f parse-node stx)]
            [atom 
             ;(display "atom") (newline)
             (leaf-f stx)]))]) 
    parse-node))

(define (string-from-stx stx) (symbol->string (syntax->datum stx)))

(define (jsonpath-to-table path)
  (let ([table (call-with-input-file path read-json)])
    (if (eof-object? table)
        (raise-user-error 'jsonpath-to-table (~a (list path "was not a json file, or could not be read")))
        table)))

(define-values
  (init-clock tick)
  (let
      ([time 0])
    (values
     (lambda ()
       (set! time (current-inexact-milliseconds))
       " 0")
     (lambda ()
       (let ([pv-time time]
             [now (current-inexact-milliseconds)])
         (set! time now)
         (string-append " " (~r (- now pv-time) #:precision 2)))))))


(define ~a/shape
  (syntax-parser
    [(~and lst (term ...))
     (let-values
         ([(l r)
             (cond
               [(syntax-property #'lst 'paren-shape) 
                => (lambda (shape)
                     (values (string shape) 
                             (string (integer->char (+ 2 (char->integer shape))))))]
               [else (values "(" ")")])]
          [(contents) (string-join (stx-map ~a/shape #'lst) " ")])
       (string-append
        l
        contents
        r))]
    [else (~a (syntax->datum #'else))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Skeleton arg parsing helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (skeleton-factory
         expand-handler
         #:stmt [allow-stmt #t]
         #:expr [allow-expr #t]
         #:no-table [allow-skip? #f])
  (let 
      ([parser 
        (syntax-parser
          [skel:macro-@
           (if allow-stmt
               (values #'skel.@kind (attribute skel.name) (syntax->list #'skel.args) #'skel.child)
               (raise-user-error 'skeleton-factory "Parsed a skeleton statement, but this skeleton does not take a child"))]
          [skel:macro-@expr
           (if allow-expr
               (values #'skel.@kind (attribute skel.name) (syntax->list #'skel.args))
               (raise-user-error 'skeleton-factory "Parsed a skeleton expression, but this skeleton requires a child"))])])
    (if allow-skip?
        (lambda (stx) (call-with-values (thunk (parser stx)) expand-handler))
        (lambda (params-table) ; This allows the requiring module to pass through important bits of configuration, should they be necessary
          (lambda (stx)
            (call-with-values
             (thunk (parser stx))
             (expand-handler params-table)))))))

(define-values (extract-id-arg extract-expr-arg extract-stmt-arg as-values)
  (let 
      ([make-extractor
        (lambda (extract-f)
          (lambda (args [pos #f])
            (let 
                ([arg-stx 
                  (if pos
                      (list-ref args pos)
                      args)])
              (extract-f arg-stx))))])
    (values 
     (make-extractor
      (syntax-parser [((~datum @) x:id) #'x]))
     (make-extractor
      (syntax-parser [((~datum =) e:cxx-expr) #'e]))
     (make-extractor
      (syntax-parser [(s:cxx-stmt) #'s]))
     (lambda (extr-f args)
       (apply values (if (list? extr-f)
                         (map (lambda (f a) (f a)) extr-f args)
                         (map extr-f args)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Some reusable handlers for various expansion events
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (expand-with-macros 
         ids macro-stx body [defs (syntax-local-make-definition-context)] [ctx (generate-expand-context)])
  ;(display "Expanding: ") (display body) (display " with macros ") (display ids) (display macro-stx) (newline)
  (syntax-local-bind-syntaxes
   ids
   macro-stx
   defs)
  (internal-definition-context-seal defs)
  (local-expand body ctx #f defs))

(define no-expand
  (syntax-parser
    [(macro args ...)
     (with-syntax 
         ([macro 
           (syntax-local-introduce ; cancel marks to make it clear it's a keyword and not a local binding
            (datum->syntax #f (syntax->datum #'macro) #'macro #'macro))]) ; strip the macro definition to stop expansion
       #'(macro args ...))]))

(define (handle-expr-list stx-l)
  (stx-map
   (lambda (stx)
     (handle-expr stx))
   stx-l))

(define get-number
  (syntax-parser
    [n:number (syntax->datum #'n)]
    [(maybe) (get-number #'maybe)]
    [else (raise-user-error 'get-number (~a (list "Not a number @ " #'else)))]))

(define (handle-expr stx [ctxt 'expression] [defs #f])
  (let* ([pair? (stx-pair? stx)]
         [head (if pair? (stx-car stx) stx)]
         [id? (identifier? head)]
         [macro? (and pair? id? (procedure? (syntax-local-value head (lambda () #f) defs)))])
    (cond 
      [macro? 
       (begin
         ;(display (~a stx "is a macro")) (newline)
         (local-expand stx ctxt #f defs))]
      [(and (not pair?) id?) (if defs (internal-definition-context-apply/loc defs head) head) #;(syntax-local-introduce head)]
      [pair? 
       (with-syntax
           ([(term ...) (stx-map (lambda (term) (handle-expr term ctxt defs)) stx)])
         #'(term ...))]
      [else stx])))

(define-syntax handle-init
  (syntax-parser
      [(handle-init init-exp null-case eq-case paren-case)
       #'(if (stx-null? init-exp)
             (null-case)
             (let ([head (stx-car init-exp)]
                   [exp (stx-cdr init-exp)])
               (if (eq? '= (syntax->datum head))
                   (eq-case exp)
                   (paren-case init-exp))))]))

(define (macroize skel-kind)
    (format-id skel-kind "@~a" skel-kind #:source skel-kind #:props skel-kind))
