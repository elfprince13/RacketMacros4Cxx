#lang racket
(require 
  (for-syntax
   racket/syntax
   syntax/context
   syntax/parse
   syntax/stx
   "define-forms.rkt"
   "syntax-classes.rkt"
   "util.rkt"))

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;
; Expression definitions
;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax make-n-op
  (lambda (stx)
    (syntax-parse stx
      #:literals (make-n-op)
      [(make-n-op op:id (~between n:exact-nonnegative-integer 1 2) ...)
       (let*
           ([op-counts (syntax->datum #'(n ...))]
            [min-count (car op-counts)]
            [max-count 
             (if (eq? 2 (length op-counts))
                 (cadr op-counts)
                 min-count)])
         (with-syntax
             ([min-count min-count]
              [max-count max-count])
           ;(display (~a (list "Meow:" #''op #'min-count #'max-count))) (newline)
           #'(define-syntax op
               (lambda (stx)
                 (syntax-parse stx
                     [((~bdatum opn op) (~between term:cxx-expr min-count max-count) (... ...))
                      ;#''(term (... ...))
                      (with-syntax
                          ([(term (... ...)) (handle-expr-list #'(term (... ...)))])
                          (no-expand #'(opn term (... ...))))])))))])))

; Assignment operators
(make-n-op = 2)
(make-n-op += 2)
(make-n-op -= 2)
(make-n-op *= 2)
(make-n-op /= 2)
(make-n-op %= 2)
(make-n-op &= 2)
(make-n-op \|= 2)
(make-n-op ^= 2)
(make-n-op <<= 2)
(make-n-op >>= 2)

; Comparison operators
(make-n-op != 2)
(make-n-op == 2)
(make-n-op >= 2)
(make-n-op <= 2)
(make-n-op > 2)
(make-n-op < 2)

; Arithmetic, pointers, boolean
(make-n-op + 1 2)
(make-n-op - 1 2)
(make-n-op % 2)
(make-n-op / 2)
(make-n-op * 1 2)
(make-n-op && 2)
(make-n-op \|\| 2)
(make-n-op ! 1)

; bits, pointers
(make-n-op & 1 2)
(make-n-op \| 2)
(make-n-op ~ 1)
(make-n-op ^ 2)
(make-n-op << 2)
(make-n-op >> 2)

; conditional
(make-n-op ?: 3)

; increment/decrement
(make-n-op >-- 1)
(make-n-op >++ 1)
(make-n-op --< 1)
(make-n-op ++< 1)

(define-syntax call
  (lambda (stx)
    #;(begin
      (display "attempting to expand call ") (newline)
      (display stx) (newline)
      (display (syntax-class-parse cxx-expr stx)) (newline)
      (display (debug-parse stx (call callee:cxx-expr arg:cxx-expr ...))) (newline))
    (syntax-parse stx
        [(call callee:cxx-expr arg:cxx-expr ...)
         (with-syntax 
             ([callee (handle-expr #'callee)]
              [(arg ...) (handle-expr-list #'(arg ...))])
           (no-expand #'(call callee arg ...)))])))



;;;;;;;;;;;;;;;;;;;;;;;;
; Statement definitions
;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax while
  (lambda (stx)
    (syntax-parse stx
      [stmt:cxx-while 
       (with-syntax 
           ([cond (handle-expr #'stmt.cond)]
            [child (local-expand #'stmt.child (generate-expand-context) #f)])
         (no-expand #'(while cond child)))])))

(define-syntax for
  (lambda (stx)
    (let 
        ([defs (syntax-local-make-definition-context)]
         [context (generate-expand-context)])
      (syntax-parse stx
        [stmt:cxx-for
         (let-values
             ([(defs init)
               (syntax-parse #'stmt.init
                 [init:decls (def #'init context defs)]
                 [init:cxx-expr (values defs (handle-expr #'init context defs))])])
           (with-syntax* 
               ([init init]
                [cond (handle-expr #'stmt.cond context defs)]
                [update (handle-expr #'stmt.update context defs)]
                [child (local-expand #'stmt.child context #f defs)])
             (no-expand 
              #'(stmt.for (init cond update) child))))]))))

(define-syntax return
  (lambda (stx)
    (syntax-parse stx
      [stmt:cxx-return
       (with-syntax
           ([ret-val 
             (if (stx-null? #'stmt.ret-val) 
                 #'()
                 (handle-expr #'(stmt.ret-val)))])
         (no-expand #'(stmt.return . ret-val)))])))

(define-syntax -if
  (lambda (stx)
    (syntax-parse stx
      [stmt:cxx-if 
       (with-syntax 
           ([cond (handle-expr #'stmt.cond)]
            [child (local-expand #'stmt.child (generate-expand-context) #f)]
            [else-clause 
             (if (stx-null? #'stmt.else-clause)
                 #'stmt.else-clause
                 (local-expand #'stmt.else-clause (generate-expand-context) #f))])
         (no-expand (if (stx-null? #'else-clause) #'(stmt.if cond child) #'(stmt.if cond child stmt.else else-clause))))])))

(define-syntax block
  (let ([nest 0])
    (lambda (stx)
      (set! nest (+ nest 1))
      (syntax-parse stx
        [stmt:cxx-block
         (with-syntax 
             ([(stmts ...)
               (let loop 
                 ([work (syntax->list #'stmt.children)]
                  [defs (syntax-local-make-definition-context)]
                  [ctx (generate-expand-context)])
                 ;(display (make-string nest #\ ))
                 ;(display (length work)) #;(display (tick)) (newline)
                 (if (null? work)
                     null
                     (let-values ([(head rest) (values (car work) (cdr work))])
                       ;(display head) (display " ") (display rest) (newline)
                       (let-values 
                           ([(head defs)
                             (syntax-parse head
                               [vars:cxx-decls 
                                (let-values ([(defs head) (def head ctx defs)])
                                  (values head defs))]
                               [expr:cxx-expr
                                (values
                                 (handle-expr head ctx defs)
                                 defs)]
                               [impl:cxx-stmt
                                (values 
                                 (local-expand head ctx #f defs)
                                 defs)])])
                         (cons head (loop rest defs ctx))))))])
           (set! nest (- nest 1))
           (no-expand #'(stmt.block stmts ...)))]))))

