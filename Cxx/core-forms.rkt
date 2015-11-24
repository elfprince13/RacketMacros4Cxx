#lang racket
(require 
  (for-syntax
   racket/format
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
  (syntax-parser
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
             (syntax-parser
               [((~bdatum opn op) (~between term:cxx-expr min-count max-count) (... ...))
                ;#''(term (... ...))
                ;(display (~a (list "Meow: " #'opn #'op #'(term (... ...)) )))
                (with-syntax
                    ([(term (... ...)) (handle-expr-list #'(term (... ...)))])
                  (no-expand #'(opn term (... ...))))]))))]))

(define-syntax make-cast
  (syntax-parser
    #:literals (make-cast)
    [(make-cast cast-name)
     #'(define-syntax cast-name
         (syntax-parser
             [((~bdatum cast-name) target:cxx-type term:cxx-expr)
              (with-syntax
                  ([term (handle-expr #'term)])
                (no-expand #'(cast-name target term)))]))]))

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

; Other weird stuff
(make-n-op |,| 2) 
(make-n-op |.| 2) 
(make-n-op |->| 2) 
(make-n-op |[]| 2)

; conditional
(make-n-op ?: 3)

; increment/decrement
(make-n-op >-- 1)
(make-n-op >++ 1)
(make-n-op --< 1)
(make-n-op ++< 1)

(make-cast c-cast)
(make-cast reinterpret_cast)
(make-cast static_cast)
(make-cast dynamic_cast)
(make-cast const_cast)

(define-syntaxes (call <<<>>>)
  (let
      ([call-op-expander
        (syntax-parser
          [(call-op callee:cxx-expr arg:cxx-expr ...)
           (with-syntax 
               ([callee (handle-expr #'callee)]
                [(arg ...) (handle-expr-list #'(arg ...))])
             (no-expand #'(call-op callee arg ...)))])])
    (values
     call-op-expander
     call-op-expander)))



;;;;;;;;;;;;;;;;;;;;;;;;
; Statement definitions
;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntaxes (while switch)
  (let*
      ([gen 
        (lambda (kw cond child)
          (with-syntax 
               ([kw kw]
                [cond (handle-expr cond)]
                [child (local-expand child (generate-expand-context) #f)])
             (no-expand #'(kw cond child))))]
       [while
        (syntax-parser
          [stmt:cxx-while (gen #'stmt.keyword #'stmt.cond #'stmt.child)])]
       [switch
        (syntax-parser
          [stmt:cxx-switch (gen #'stmt.keyword #'stmt.cond #'stmt.child)])])
    (values while switch)))


(define-syntax for
  (syntax-parser
        [stmt:cxx-for
         (let*-values
             ([(defs context) 
               (values (syntax-local-make-definition-context)
                       (generate-expand-context))]
              [(defs init)
               (syntax-parse #'stmt.init
                 [init:decls (def #'init context defs)]
                 [init:cxx-expr (values defs (handle-expr #'init context defs))])])
           (with-syntax* 
               ([init init]
                [cond (handle-expr #'stmt.cond context defs)]
                [update (handle-expr #'stmt.update context defs)]
                [child (local-expand #'stmt.child context #f defs)])
             (no-expand 
              #'(stmt.for (init cond update) child))))]))

(define-syntax return
  (syntax-parser
    [stmt:cxx-return
     (with-syntax
         ([ret-val 
           (if (stx-null? #'stmt.ret-val) 
               #'()
               (handle-expr #'(stmt.ret-val)))])
       (no-expand #'(stmt.return . ret-val)))]))

(define-syntax -if
  (syntax-parser
    [stmt:cxx-if 
     (with-syntax 
         ([cond (handle-expr #'stmt.cond)]
          [child (local-expand #'stmt.child (generate-expand-context) #f)]
          [else-clause 
           (if (stx-null? #'stmt.else-clause)
               #'stmt.else-clause
               (local-expand #'stmt.else-clause (generate-expand-context) #f))])
       (no-expand (if (stx-null? #'else-clause) #'(stmt.if cond child) #'(stmt.if cond child stmt.else else-clause))))]))


(define-syntaxes (block default case)
  (let* 
      ([child-loop
        (lambda (children)
          (let loop 
            ([work (syntax->list children)]
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
                    (cons head (loop rest defs ctx)))))))]
       [block
        (syntax-parser
        [stmt:cxx-block
         (with-syntax 
             ([(stmts ...)
               (child-loop #'stmt.children)])
           (no-expand #'(stmt.keyword stmts ...)))])]
       [default
        (syntax-parser
        [stmt:cxx-default
         (with-syntax 
             ([(stmts ...)
               (child-loop #'stmt.children)])
           (no-expand #'(stmt.keyword stmts ...)))])]
       [case
        (syntax-parser
        [stmt:cxx-case
         (with-syntax 
             ([(stmts ...)
               (child-loop #'stmt.children)]
              [when (handle-expr #'stmt.when)])
           (no-expand #'(stmt.keyword when stmts ...)))])])
  (values
   block
   default
   case)))

(define-syntaxes (continue break)
  (values no-expand no-expand))

