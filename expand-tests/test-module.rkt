(module test racket
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
    (lambda (shadow-table bind-table) ; we need two tables: one for replace bound-ids and one to test shadowing with free-ids
      (letrec 
          ([safe-print-id 
            (lambda (stx)
              (if (identifier? stx)
                  (dict-ref bind-table stx stx)
                  stx))]
           [walk-expr 
            (lambda (stx)
              (syntax-case stx () ; bizarre interaction with syntax-case and lambda: can't have lambda as a keyword argument, or the syntax-case breaks
                [(f (arg) bodies ...)
                 (begin
                   (display (~a (list "lambda: " #'arg " has " (syntax-source #'arg) (syntax-source stx) (identifier? #'arg))))
                   (newline)
                   (display stx) (newline)
                   (display #'arg) (newline)
                   (if (syntax-source #'arg)
                       (begin
                         (display (~a (list "setting shadow-table for " #'arg))) (newline) ; this is too primitive we need a stack
                         (dict-set! shadow-table #'arg #'arg))
                       (if (dict-has-key? shadow-table #'arg) ; Would this shadow?
                           (let ([btv (generate-temporary #'arg)])
                             (display (~a (list "setting bind-table " #'arg " to " btv))) (newline)
                             (dict-set! bind-table #'arg btv))
                           (void)))
                   
                   (with-syntax 
                       ([arg 
                         (safe-print-id #'arg)]
                        [(bodies ...) 
                         (stx-map
                          (lambda (stx)
                            (walk-expr stx)) #'(bodies ...))])
                     #'(f (arg) bodies ...)))]
                [(seq ...)
                 (begin
                   ;(display (~a (list "seq: " #'(seq ...)))) (newline)
                   (stx-map walk-expr #'(seq ...)))]
                [atom 
                 (begin
                   ;(display (~a (list "atom: " #'atom))) (newline)
                   (safe-print-id #'atom))]))]) 
        walk-expr)))
  
  
  (define-for-syntax display-inert-body
    (lambda (tag contents) 
      (with-syntax* ([stx-tag tag] 
                     [contents 
                      (stx-map 
                       (lambda (stx) 
                         (let ([expanded (local-expand stx 'top-level #f)])
                           ((walk-expr-safe-ids (make-free-id-table) (make-bound-id-table)) expanded))) 
                       contents)]
                     [unpacked #'(apply values 'contents)]) 
        (if tag 
            #'(stx-tag unpacked) 
            #'unpacked))))
  
  
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
  
  (define-syntax set!
    (lambda (stx) 
      (syntax-case stx (set!)
        [(set! id val) 
         (begin
           #;(emit-remark "Beginning set! inspection")
           #;(emit-remark #'id (~a (identifier-binding #'id)) (~a (syntax-property-symbol-keys #'id)) (~a (syntax-property #'id 'mark)))
           #;(if (identifier? #'val) 
                 (emit-remark #'val (~a (identifier-binding #'val)) (~a (syntax-property-symbol-keys #'val)) (~a (syntax-property #'val 'mark)))
                 (emit-remark "No second identifier"))
           #;(emit-remark "Ending set! inspection")
           #;(begin 
               (display (syntax-local-context)) (newline)
               (display (syntax-local-submodules)) (newline)
               (display (map syntax-local-module-exports (syntax-local-submodules))) (newline)
               )
           #'(set!~ id val))])))
  
  (define-syntax let*
    (lambda (stx)
      (syntax-case stx (let*)
        [(let* ((id expr)) bodies ...)
         (let ([intdef (syntax-local-make-definition-context)])
           (begin
             ;(display (~a (list "let*" #'id (syntax-source #'id)))) (newline)
             (syntax-local-bind-syntaxes (list #'id) #f intdef)
             (internal-definition-context-seal intdef)
             (with-syntax*
                 ([(bodies ...) 
                   (stx-map 
                    (lambda (body)
                      (with-syntax ([body body])
                        (local-expand #'body (build-expand-context 'expression) #f intdef)))
                    #'(bodies ...))]
                  [id-tmp (internal-definition-context-apply intdef #'id)]
                  [id (syntax/loc #'id id-tmp)]
                  [lambda (identifier-prune-to-source-module #'lambda)]) ; Strip the original racket definitions!
               
               ;(display (~a (list "let*-inner" #'id (syntax-source #'id)))) (newline)
               #'((lambda (id) bodies ...) expr))))]
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
  
  (define-syntax for5 
    (lambda (stx) 
      (syntax-case stx (from to in)
        [(for5 from low to high in bodies ...)
         (with-syntax ([it (datum->syntax (syntax for5) 'it)])
           (syntax
            (for5 it from low to high in bodies ...)))]
        [(for5 var from low to high in bodies ...)
         (with-syntax 
             ([local (identifier-prune-to-source-module #'local)] ; Strip the original racket definitions!
              [lambda (identifier-prune-to-source-module #'lambda)]
              [define (identifier-prune-to-source-module #'define)])
           (syntax #;(this is called 2 loops)
                   (local ([define high-value high] 
                           [define loop 
                             (lambda (var)
                               (if (> var high-value) 'done
                                   (begin
                                     bodies ...
                                     (loop (+ var 1)))))])
                     (loop low))))]))))