#lang racket
(require racket/require-syntax)

(provide (all-defined-out))

(define-for-syntax skeleton-modules
  (let ([modules-list null]) 
    (lambda ([update-list #f])
      (if update-list
          (set! modules-list update-list)
          modules-list))))

(define-syntax show-modules
  (lambda (stx)
    (with-syntax ([modules (skeleton-modules)])
      #''modules)))

;(display (show-modules)) (newline)

(define-require-syntax skels
  (lambda (stx) 
    (syntax-case stx ()
      [(skels)
       (begin
         (display (skeleton-modules))
         (with-syntax ([(path ...) (skeleton-modules)])
           #'(path ...)))])))
