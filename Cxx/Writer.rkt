#lang racket

(require syntax/parse
         syntax/stx
         "syntax-classes.rkt")

(provide (all-defined-out))

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
  (lambda (stx)
    (syntax-parse stx
      [(init:var-init) 
       (let ([name (symbol->string (syntax->datum #'init.name))])
         (if (stx-null? #'init.exp) 
             name
             (let ([head (stx-car #'init.exp)]
                   [expr (stx-cdr #'init.exp)])
               (if (eq? '= (syntax->datum head))
                   (string-append name " = " (make-cpp-expr expr))
                   (string-append name (make-cpp-expr #'init.exp))))))])))

(define make-storage-classes
  (lambda (storage-syntax)
    (string-join 
         (map symbol->string 
              (syntax->datum storage-syntax)) " " #:after-last " ")))
    

(define make-cpp-decl 
  (lambda (stx)
    (syntax-parse stx
      [defun:fun-decl
       (string-append 
        (make-storage-classes #'defun.storage-classes)
        (symbol->string (syntax->datum #'defun.ret-type)) " "
        (symbol->string (syntax->datum #'defun.name)) "(" (string-join (map make-cpp-decl (syntax->list #'defun.args)) ", ") ")" 
        (make-cpp-stmt #'defun.body))]
      [var:var-decl 
       (string-append
        (make-storage-classes #'var.storage-classes)
        (symbol->string (syntax->datum #'var.type)) " " (make-cpp-init #'var.init))]
      [def:decls 
       (string-append
        (make-cpp-decl #'def.var) 
        (let ((extra-decls (syntax->list #'def.extra-vars)))
          (string-join
           (map make-cpp-init extra-decls) ", "
           #:before-first (if (eq? 0 (length extra-decls)) "" ", "))))])))