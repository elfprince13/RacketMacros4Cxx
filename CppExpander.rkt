#lang racket

(require (for-template racket))

#;(define-syntax (shadow-expand stx)
    (syntax-case stx ()
      [(shadow-expand (names ...) body)
       #'(let ((names (expand-stmt names)) ...) body)]))

(define simple-external-params-table
  (hash '(Loop1d (test-loop)) '(4)))

#;(define-syntax (define-skeleton stx)
  (syntax-case stx ()
    [(define-skeleton SkName SrcPHandler ExtPHandler Impl) 
     (lambda (skel) (syntax-case skel (@ SkName)
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
            
       

(define skeletons-table 
  (hash 'Loop1d 
        (lambda (skel) (syntax-case skel (@ Loop1d)
                         [(@ Loop1d (name ...) (params ...) body)
                          (let* ((params (syntax->datum #'(params ...)))
                                 (num-params (length params)))
                            (cond
                              ((> 2 num-params) (raise-argument-error 'params "at least 2 values required" params))
                              ((< 4 num-params) (raise-argument-error 'params "at most 4 values allowed" params))
                              (else (let ((itr-var (car (car params)))
                                 (lower-bound (if (eq? 2 num-params) 0 (car (cadr params))))
                                 (upper-bound (car (if (eq? 2 num-params) (cadr params) (caddr params))))
                                 (stride (if (eq? 4 num-params) (car (cadddr params)) 1)))
                            (lambda (ext-params)
                              (if (eq? 1 (length ext-params))
                            (let ((unroll-factor (car ext-params)))
                              (letrec ((unroller (lambda (unroll-factor itr-var stride body) 
                                                    (if (eq? 1 unroll-factor) 
                                                        (with-syntax ((itr-var itr-var) 
                                                                      (stride stride) 
                                                                      (body body)) 
                                                          #'(begin body (+= itr-var stride)))
                                                        (with-syntax ((itr-var itr-var) 
                                                                      (stride stride) 
                                                                      (body body) 
                                                                      (more-bodies (unroller (- unroll-factor 1) itr-var stride body))) 
                                                          #'(begin body (+= itr-var stride) more-bodies))))))
                              (with-syntax ((itr-var itr-var)
                                            (lower-bound lower-bound)
                                            (upper-bound upper-bound)
                                            (stride stride)
                                            (body (unroller unroll-factor itr-var stride #'body))) #'(for ((def (() int itr-var = lower-bound)) (< itr-var upper-bound) ()) body))))
                            (raise-argument-error 'ext-params "1 value required" ext-params)))))))]))))


(define expand-stmt (lambda (stmt) 
                      (syntax-case stmt (if else for while begin call def @) 
                        [(@ SkelKind (name ...) (params ...) body) 
                         (begin 
                           #;(print "expanding skeleton")
                           #;(newline)
                           (((hash-ref skeletons-table (syntax->datum #'SkelKind)) stmt)
                          (hash-ref simple-external-params-table (list (syntax->datum #'SkelKind) (syntax->datum #'(name ...))))))]
                        [(while (cond ...) body) (with-syntax 
                                                 ((body (expand-stmt #'body)))
                                               #'(while (cond ...) body))]
                        [(if (cond ...) body) (with-syntax  
                                              ((body (expand-stmt #'body))) 
                                            #'(if (cond ...) body))]
                        [(if (cond ...) body else else-body) (with-syntax 
                                              ((body (expand-stmt #'body))
                                               (else-body (expand-stmt #'else-body))) 
                                            #'(if (cond ...) body else else-body))]
                        [(begin stmts ... ) 
                         (begin 
                           #;(print "expanding begin") 
                           #;(newline ) 
                           (let ((stmts (map expand-stmt (syntax->list #'(stmts ...)))))
                             (datum->syntax stmt (cons 'begin stmts))))]
                        [(def defs ...) (expand-decl stmt)]
                        [(expr ...) (begin
                                      #;(print "no match for ") 
                                      #;(newline)
                                      #;(print stmt)
                                      #;(newline)
                                      stmt)]))) 

(define expand-decl (lambda (decl)
                      (syntax-case decl (def defun)
                        [(defun (storage ...) ret-type name (args ...) body)
                         (with-syntax ((body (expand-stmt #'body)))
                           #'(defun (storage ...) ret-type name (args ...) body))]
                        [(def ((storage ...) type name init ...) (next-name next-init ...) ...) decl])))

(define make-cpp-expr (lambda (expr) (syntax-case expr (call) 
                                       [(call callee args ...) 
                                        (string-append (make-cpp-expr #'callee) "("
                                                       (string-join 
                                                        (map make-cpp-expr (syntax->list #'(args ...))) ", ")
                                                       ")")]
                                       [((paren-expr ...)) (string-append "(" (make-cpp-expr #'(paren-expr ...)) ")")]
                                       [(operator arg1 arg2)
                                        (string-append (make-cpp-expr #'arg1) " " (make-cpp-expr #'operator) " " (make-cpp-expr #'arg2))]
                                       [(operator arg1)
                                        (string-append (make-cpp-expr #'operator) (make-cpp-expr #'arg1))]
                                       [() ""]
                                       [(name-or-literal) (make-cpp-expr #'name-or-literal)]
                                       [name-or-literal 
                                        (let ((name-or-literal (syntax->datum #'name-or-literal)))
                                          (cond [(symbol? name-or-literal)
                                              (symbol->string name-or-literal)]
                                              [(number? name-or-literal)
                                              (number->string name-or-literal)]
                                              [(string? name-or-literal)
                                               [string-append "\"" name-or-literal "\""]]
                                              [else (raise-argument-error 'name-or-literal "expected name, number, or string" name-or-literal)]))])))
(define make-cpp-stmt (lambda (stmt) (syntax-case stmt (if else for while begin call def @) 
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
                        [(def defs ...) (make-cpp-decl stmt)]
                        [(expr ...) (begin 
                                      #;(print "no match found for ") 
                                      #;(newline) 
                                      #;(print stmt) 
                                      #;(newline)
                                      (string-append (make-cpp-expr stmt) ";\n"))])))

(define make-cpp-init (lambda (init)
                        (syntax-case init ()
                          [(name) (symbol->string (syntax->datum #'name))]
                          [(name = expr ...) (string-append (symbol->string (syntax->datum #'name)) " = " (make-cpp-expr #'(expr ...)))]
                          [(name (expr ...)) (string-append (symbol->string (syntax->datum #'name)) "(" (make-cpp-expr #'(expr ...)) ")")])))

(define make-cpp-decl (lambda (decl)
                      (syntax-case decl (def defun)
                        [(defun (storage ...) ret-type name (args ...) body)
                         (string-append 
                          (string-join 
                           (map symbol->string 
                                (syntax->datum #'(storage ...))) " " #:after-last " ")
                          (symbol->string (syntax->datum #'ret-type)) " "
                          (symbol->string (syntax->datum #'name)) "(" (string-join (map make-cpp-decl (syntax->list #'(args ...))) ", ") ")" 
                          (make-cpp-stmt #'body))]
                        [((storage ...) type name init ...) (string-append
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


#;(expand-stmt #'(while 
                (true) (@ Loop1d (test-loop) ((i) (0) (8)) (+= a 6))))
#;(newline)

; Note: we don't handle [] array syntax yet, because [] are ()
(let ((expanded-code 
       (expand-decl
        #'(defun () int main ((() int argc) (() char **argv)) 
            (begin
            (if (== 0 (% argc 4))
                (@ Loop1d (test-loop) ((i) (0) (argc)) 
                   (call printf "%s\\n" (* ((+ argv i)))))
                else
                (call printf "args not a multiple of 4: %d\\n" argc))
            (call printf "done\\n"))))))
  (begin
    (print expanded-code)
    (newline)
    (display (make-cpp-decl expanded-code))))

