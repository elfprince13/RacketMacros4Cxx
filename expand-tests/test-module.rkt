(module test racket
  ;this file is primarily necessary because I can't figure out how the search path for an inline-module works
  (provide (all-defined-out))
  
  (define-syntax swap 
    (lambda (stx)
      (syntax-case stx (swap)
        [(swap a b) 
         #'(let ([tmp a])
             (set! a b)
             (set! b tmp))])))
  
  (define-syntax for5 
    (lambda (stx) 
      (syntax-case stx (from to in)
        [(for5 from low to high in bodies ...)
         (with-syntax ([it (datum->syntax (syntax for5) 'it)])
           (syntax
            (for5 it from low to high in bodies ...)))]
        [(for5 var from low to high in bodies ...)
         (with-syntax 
             ([local (identifier-prune-to-source-module #'local)] ; These must be rendered inert!
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