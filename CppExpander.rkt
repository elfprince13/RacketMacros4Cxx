#lang racket

(require (for-template racket))
(require "LoopTiles.rkt")

#;(define-syntax (shadow-expand stx)
    (syntax-case stx ()
      [(shadow-expand (names ...) body)
       #'(let ((names (expand-stmt names)) ...) body)]))

(define simple-external-params-table
  (hash 
   '(Loop1d (test-loop)) (list (list #'(blockIdx . x)) (list #'((/ (threadIdx . x) 32))) (list #'((& (threadIdx . x) 32))) (list #'i) (list #'j) (list #'(gridDim . x)) (list #'((/ (blockDim . x) 32))) (list #'32) (list #'1) (list #'4)) 
   '(I ()) '()))

#;(define-syntax (define-skeleton stx)
    (syntax-case stx ()
      [(define-skeleton SkName SrcPHandler ExtPHandler Impl) 
       (lambda (skel) 
         (syntax-case skel (@ SkName)
           [(@ SkName (name ...) (params ...) body)
            #'(SrcPHandler (ExtPHandler Impl))
            ]))]
      [(define-skeleton SkName ((Params ...) (OptParams ...)) ((EParams ...) (OptEParams ...)) Impl)
       (let* ((SrcParams (syntax->datum #'(Params ...)))
              (OptSrcParams (syntax->datum #'(OptParams ...)))
              (MinPCt (length SrcParams))
              (MaxPCt (+ MinPCt (length OptSrcParams)))
              (SrcPHandler (lambda (ExtPHandler) 
                             ))
              
              (EParams (syntax->datum #'(EParams ...)))
              (OptEparams (syntax->datum #'(OptEparams ...)))
              (MinEPCt (length EParams))
              (MaxEPCt (+ MinEPCt (length OptEParams)))
              (ExtPHandler ))
         (define-skeleton SkName SrcPHandler ExtPHandler Impl))]))


(define lookup-skeleton
  (lambda (table-list key)
    (begin
      ;(print (list "looking up "  key " in " table-list)) (newline)
      (match table-list 
        [(cons h t) (hash-ref h key (lambda () (lookup-skeleton t)))]
        [(list ) #f]))))

(struct skeleton-expansion (body table))

; Grid > Block > Warp > Thread-Loop > Unroll
; 
(define init-skeletons-table 
  (list (hash 'Loop1d 
              (lambda (skel table-here) 
                (syntax-case skel (@ Loop1d)
                  [(@ Loop1d (name ...) (params ...) body)
                   (let* ((params (syntax->datum #'(params ...)))
                          (num-params (length params)))
                     (cond
                       ((> 2 num-params) (raise-argument-error 'params "at least 2 arguments required" params))
                       ((< 4 num-params) (raise-argument-error 'params "at least 4 arguments required" params))
                       (else 
                        (let ((itr-var (car (car params)))
                              (lower-bound (if (eq? 2 num-params) 0 (car (cadr params))))
                              (upper-bound (car (if (eq? 2 num-params) (cadr params) (caddr params))))
                              (stride (if (eq? 4 num-params) (car (cadddr params)) 1)))
                          (lambda (ext-params)
                            (let ((num-ext-params (length ext-params)))
                              (if (eq? 10 num-ext-params)
                                  (let*-values (((split-vars split-bounds) (split-at (drop ext-params (- num-ext-params 10)) 5))
                                                ((index-vars) (map car split-vars))
                                                ((index-bounds) (map car split-bounds)))
                                    (let-values (((counter-exp geom-exp loop-code) (cuda-loop1d #'body index-vars index-bounds (list 0 1 2 3 4))))
                                      (skeleton-expansion
                                       loop-code
                                       (cons (hash 'I (lambda (skel table-here) 
                                                        (syntax-case skel (@ I)
                                                          [(@ I () () ()) 
                                                           (lambda (ext-params) 
                                                             (skeleton-expansion
                                                              (with-syntax ((itr-var counter-exp)) #'itr-var)
                                                              table-here))]))
                                                   'N (lambda (skel table-here) 
                                                        (syntax-case skel (@ I)
                                                          [(@ N () () ()) 
                                                           (lambda (ext-params) 
                                                             (skeleton-expansion 
                                                              (with-syntax ((upper-bound upper-bound)) #'upper-bound)
                                                              table-here))]))) 
                                             table-here))))
                                  (raise-argument-error 'ext-params "10 arguments required" ext-params))))))))])))))


(define expand-stmt 
  (lambda (stmt skels) 
    (syntax-case stmt (if else for while begin call def @) 
      [(@ SkelKind (name ...) (params ...) body) 
       (begin 
         #;(print "expanding skeleton")
         #;(newline)
         (let ((expansion (((lookup-skeleton skels (syntax->datum #'SkelKind)) stmt skels) 
                           (hash-ref simple-external-params-table (list (syntax->datum #'SkelKind) (syntax->datum #'(name ...)))))))
           (begin
             ;(print (list "re-expanding with " (skeleton-expansion-body expansion) (skeleton-expansion-table expansion))) (newline)
             (expand-stmt (skeleton-expansion-body expansion) (skeleton-expansion-table expansion)))))] ; The expansion process may have introduced new macros, so expand those too
      [(while (cond ...) body) 
       (with-syntax 
           ((body (expand-stmt #'body skels)))
         #'(while (cond ...) body))]
      [(for ((init ...) (cond ...) (update ...)) body) 
       (with-syntax
           ((body (expand-stmt #'body skels)))
         #'(for ((init ...) (cond ...) (update ...)) body))]
      [(if (cond ...) body) 
       (with-syntax  
           ((body (expand-stmt #'body skels))) 
         #'(if (cond ...) body))]
      [(if (cond ...) body else else-body) 
       (with-syntax 
           ((body (expand-stmt #'body skels))
            (else-body (expand-stmt #'else-body skels))) 
         #'(if (cond ...) body else else-body))]
      [(begin stmts ... ) 
       (begin 
         #;(print "expanding begin") 
         #;(newline ) 
         (let ((stmts (map (curryr expand-stmt skels) (syntax->list #'(stmts ...)))))
           (datum->syntax stmt (cons 'begin stmts))))]
      [(call func args ... ) 
       (begin 
         #;(print "expanding call") 
         #;(newline ) 
         (let ((args (map (curryr expand-stmt skels) (syntax->list #'(args ...)))))
           (with-syntax ((func (expand-stmt #'func skels)))
             #`(call func #,@(datum->syntax stmt args)))))]
      [(def defs ...) 
       (expand-decl stmt skels)]
      [(expr ...) 
       (begin
         #;(print "no match for ") 
         #;(newline)
         #;(print stmt)
         #;(newline)
         (let ((expr-components 
                (map (curryr expand-stmt skels) (syntax->list #'(expr ...)))))
           (datum->syntax stmt expr-components)))]
      [any #'any]))) 

(define expand-decl 
  (lambda (decl skels)
    (syntax-case decl (def defun)
      [(defun (storage ...) 
         ret-type name (args ...) body)
       (with-syntax ((body (expand-stmt #'body skels)))
         #'(defun (storage ...) ret-type name (args ...) body))]
      [(def ((storage ...) type name init ...) 
         (next-name next-init ...) ...) decl])))

(define make-cpp-expr 
  (lambda (expr) 
    (syntax-case expr (call) 
      [(call callee args ...) 
       (string-append (make-cpp-expr #'callee) "("
                      (string-join 
                       (map make-cpp-expr (syntax->list #'(args ...))) ", ")
                      ")")]
      [((paren-expr ...)) 
       (string-append "(" (make-cpp-expr #'(paren-expr ...)) ")")]
      [(operator arg1 arg2)
       (string-append (make-cpp-expr #'arg1) " " (make-cpp-expr #'operator) " " (make-cpp-expr #'arg2))]
      [(operator arg1)
       (string-append (make-cpp-expr #'operator) (make-cpp-expr #'arg1))]
      [() ""]
      [(name-or-literal) 
       (make-cpp-expr #'name-or-literal)]
      [(struct . member) 
       (string-append "(" (make-cpp-expr #'struct) "." (make-cpp-expr #'member) ")")]
      [name-or-literal 
       (let ((name-or-literal (syntax->datum #'name-or-literal)))
         (cond [(symbol? name-or-literal)
                (symbol->string name-or-literal)]
               [(number? name-or-literal)
                (number->string name-or-literal)]
               [(string? name-or-literal)
                [string-append "\"" name-or-literal "\""]]
               [else (raise-argument-error 'name-or-literal "expected name, number, or string" name-or-literal)]))])))

(define make-cpp-stmt 
  (lambda (stmt) 
    (syntax-case stmt (if else for while begin call def @) 
      [(for ((init ...) (cond ...) (update ...)) body) 
       (string-append
        "for (" (make-cpp-decl #'(init ...)) ";" (make-cpp-expr #'(cond ...)) ";" (make-cpp-expr #'(update ...)) ")"
        (make-cpp-stmt #'body))]
      [(while (cond ...) body) 
       (string-append
        "while (" (make-cpp-expr #'(cond ...)) ") " (make-cpp-stmt #'body))]
      [(if (cond ...) body) 
       (string-append
        "if (" (make-cpp-expr #'(cond ...)) ") " (make-cpp-stmt #'body))]
      [(if (cond ...) body else else-body) 
       (string-append
        "if (" (make-cpp-expr #'(cond ...)) ") " (make-cpp-stmt #'body) " else " (make-cpp-stmt #'else-body))]
      [(begin stmts ... ) 
       (begin
         #;(print "printing begin") 
         #;(newline) 
         #;(print #'(begin stmts ...)) 
         #;(newline)
         (string-append 
          "{\n" (string-join (map make-cpp-stmt (syntax->list #'(stmts ...))) "") "}\n"))]
      [(def defs ...) 
       (string-append (make-cpp-decl stmt) ";\n")]
      [(expr ...) 
       (begin 
         #;(print "no match found for ") 
         #;(newline) 
         #;(print stmt) 
         #;(newline)
         (string-append (make-cpp-expr stmt) ";\n"))])))

(define make-cpp-init 
  (lambda (init)
    (syntax-case init ()
      [(name) (symbol->string (syntax->datum #'name))]
      [(name = expr ...) (string-append (symbol->string (syntax->datum #'name)) " = " (make-cpp-expr #'(expr ...)))]
      [(name (expr ...)) (string-append (symbol->string (syntax->datum #'name)) "(" (make-cpp-expr #'(expr ...)) ")")])))

(define make-cpp-decl 
  (lambda (decl)
    (syntax-case decl (def defun)
      [(defun (storage ...) ret-type name (args ...) body)
       (string-append 
        (string-join 
         (map symbol->string 
              (syntax->datum #'(storage ...))) " " #:after-last " ")
        (symbol->string (syntax->datum #'ret-type)) " "
        (symbol->string (syntax->datum #'name)) "(" (string-join (map make-cpp-decl (syntax->list #'(args ...))) ", ") ")" 
        (make-cpp-stmt #'body))]
      [((storage ...) type name init ...) 
       (string-append
        (string-join
         (map symbol->string
              (syntax->datum #'(storage ...))) " " #:after-last " ")
        (symbol->string (syntax->datum #'type)) " " (make-cpp-init #'(name init ...)))]
      [(def ((storage ...) type name init ...) (next-name next-init ...) ...) 
       (string-append
        (string-join
         (map symbol->string
              (syntax->datum #'(storage ...))) " " #:after-last " ")
        (symbol->string (syntax->datum #'type)) " " (make-cpp-init #'(name init ...)) 
        (let ((extra-decls (syntax->list #'((next-name next-init ...) ...))))
          (string-join
           (map make-cpp-init extra-decls) ", "
           #:before-first (if (eq? 0 (length extra-decls)) "" ", "))))])))


; Note: we don't handle [] array syntax yet, because [] are ()
(let ((expanded-code 
       (expand-decl
        #'(defun () int main ((() int argc) (() char **argv)) 
            (begin
              (if (== 0 (% argc 4))
                  (@ Loop1d (test-loop) ((i) (0) (argc)) 
                     (call printf "%s\\n" (* ((+ argv (@ I () () ()))))))
                  else
                  (call printf "args not a multiple of 4: %d\\n" argc))
              (call printf "done\\n"))) init-skeletons-table)))
  (begin
    (print expanded-code)
    (newline)
    (display (make-cpp-decl expanded-code))))

