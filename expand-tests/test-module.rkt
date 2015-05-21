(module test racket
  ;this file is primarily necessary because I can't figure out how the search path for an inline-module works
  (provide (all-defined-out))
  
  (require (for-syntax syntax/stx))
  (require (for-syntax syntax/context))
  (require (for-syntax racket/syntax))
  
  (define-syntax set!
    (lambda (stx) 
      (syntax-case stx (set!)
        [(set! id val) 
         (begin
           (print "Beginning set! inspection") (newline)
           (print #'id) (newline)
           (print (identifier-binding #'id)) (newline)
           (if (identifier? #'val)
               (begin
                 (print #'val) (newline)
                 (print (identifier-binding #'val)) (newline))
               (begin
                 (print "No second identifier") (newline)))
           (print "Ending set! inspection") (newline)
           #'(set!~ id val))])))
  
  (define-syntax let*
    (lambda (stx)
      (syntax-case stx (let*)
        [(let* ((id expr)) bodies ...)
         (let ([intdef (syntax-local-make-definition-context)])
           (begin
             (syntax-local-bind-syntaxes (list #'id) #f intdef)
             (internal-definition-context-seal intdef)
             (with-syntax
                 ([(bodies ...) 
                   (stx-map 
                    (lambda (body)
                      (with-syntax ([body body])
                        (local-expand #'body (build-expand-context 'expression) #f intdef)))
                    #'(bodies ...))]
                  [id (internal-definition-context-apply intdef #'id)]
                  [lambda (identifier-prune-to-source-module #'lambda)]) ; Strip the original racket definitions!
               #'((lambda (id) bodies ...) expr))))]
        [(let* ((id expr) (more-ids more-exprs) ...) bodies ...)
         (with-syntax
             ([body #'(let* ((more-ids more-exprs) ...) bodies ...)])
           #'(let* ((id expr)) body))])))
  
  (define-syntax swap 
    (lambda (stx)
      (syntax-case stx (swap)
        [(swap a b) 
         (with-syntax
             ([let* (identifier-prune-to-source-module #'let*)] ; Strip the original racket definitions!
              [set! (identifier-prune-to-source-module #'set!)])
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