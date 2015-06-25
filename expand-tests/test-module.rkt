#lang racket
  (require (for-syntax racket/dict))
  (require (for-syntax syntax/id-table))
  (require (for-syntax syntax/stx))
  (require (for-syntax syntax/context))
  (require (for-syntax racket/syntax))
  (require (for-syntax macro-debugger/emit))
  (require (for-syntax racket/format))
  
  (provide (except-out (all-defined-out) module-begin top-interaction)
           (rename-out [module-begin #%module-begin] [top-interaction #%top-interaction]))
  
  (define-for-syntax walk-expr-safe-ids
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
  
  
  (define-for-syntax display-inert-body
    (lambda (tag contents) 
      (with-syntax* ([stx-tag tag] 
                     [contents 
                      (stx-map 
                       (lambda (stx) 
                         (let ([expanded (local-expand stx 'top-level #f)])
                           ((walk-expr-safe-ids (make-bound-id-table)) expanded))) 
                       contents)]
                     [unpacked #'(apply values 'contents)]) 
        (if tag 
            #'(stx-tag unpacked) 
            #'unpacked))))
  
  (define-for-syntax no-expand
    (lambda (stx)
      (syntax-case stx ()
        [(macro args ...)
         (with-syntax ([macro (datum->syntax #f (syntax->datum #'macro) #'macro #'macro)])
           #'(macro args ...))])))
  
  
  (define-syntax top-interaction
    (lambda (stx)
      (syntax-case stx ()
        [(top-interaction . body)
         (display-inert-body #f #'(body))])))
  
  (define-syntax module-begin
    (lambda (stx)
      (syntax-case stx ()
        [(module-begin bodies ...)
         (display-inert-body #'#%module-begin #'(bodies ...))])))
  
  (define-syntax print
    (lambda (stx)
      (syntax-case stx (print)
        [(print id) (no-expand stx)])))
  
  (define-syntax dummy
    (lambda (stx)
      (syntax-case stx ()
        [(dummy) (no-expand stx)])))
  
  (define-syntax set!
    (lambda (stx) 
      (syntax-case stx (set!)
        [(set! id val) 
         (no-expand stx)])))
  
  (define-syntax let*
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
  
  (define-syntax swap 
    (lambda (stx)
      (syntax-case stx (swap)
        [(swap a b) 
         (with-syntax ()
           #'(let* ([tmp a])
               (set! a b)
               (set! b tmp)))])))
