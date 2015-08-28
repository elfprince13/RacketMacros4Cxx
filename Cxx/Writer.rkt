#lang racket

(require syntax/parse
         syntax/stx
         "syntax-classes.rkt"
         "util.rkt")

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
    (syntax-case stmt (if else for while block call def @) 
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
      [(block stmts ... ) 
       (begin
         (print "printing block") 
         (newline) 
         (print #'(block stmts ...)) 
         (newline)
         (string-append 
          "{\n" (string-join (map make-cpp-stmt (syntax->list #'(stmts ...))) "") "}\n"))]
      [(def defs ...) 
       (string-append (make-cpp-decl stmt) ";\n")]
      [(expr ...) 
       (begin 
         (print "no match found for ") 
         (newline) 
         (print stmt) 
         (newline)
         (string-append (make-cpp-expr stmt) ";\n"))])))

(define make-cpp-init 
  (lambda (stx)
    (syntax-parse stx
      [(init:var-init) 
       (let ([name (string-from-stx #'init.name)])
         (handle-init #'init.exp 
                      (lambda () name)
                      (lambda (eq-expr)
                        (string-append name " = " (make-cpp-expr eq-expr)))
                      (lambda (paren-expr)
                        (string-append name (make-cpp-expr paren-expr)))))])))

(define make-storage-classes
  (lambda (storage-syntax)
    (string-join 
         (stx-map string-from-stx storage-syntax) " " #:after-last (if (stx-null? storage-syntax ) "" " "))))

(define make-attributes make-storage-classes)
(define make-qualifiers make-storage-classes)
(define make-type make-storage-classes)
    

(define make-cpp-decl 
  (lambda (stx)
    (syntax-parse stx
      [typedef:typedef-decl
       (string-append
        "typedef "
        (make-type #'typedef.type-terms)
        ";\n")]
      [record:record-decl
       (string-append
        (make-qualifiers #'record.qualifiers)
        (string-from-stx #'record.kind) " "
        (string-from-stx #'record.name) " "
        (make-attributes #'record.attributes)
        (if (stx-null? #'record.decls)
            ""
            (string-append
             "{\n"
             (string-join (stx-map make-cpp-decl #'record.decls))
             "}"))
        
       ";\n")]
      [defun:fun-decl
       (string-append 
        (make-storage-classes #'defun.storage-classes)
        (string-from-stx #'defun.ret-type) " "
        (string-from-stx #'defun.name) "(" (string-join (map make-cpp-decl (syntax->list #'defun.args)) ", ") ")" 
        (make-cpp-stmt #'defun.body))]
      [var:var-decl 
       (string-append
        (make-storage-classes #'var.storage-classes)
        (string-from-stx #'var.type) " " (make-cpp-init #'var.init))]
      [def:decls 
       (string-append
        (make-cpp-decl #'def.var) 
        (let ((extra-decls (syntax->list #'def.extra-vars)))
          (string-join
           (map make-cpp-init extra-decls) ", "
           #:before-first (if (eq? 0 (length extra-decls)) "" ", "))))])))

(define make-cpp-tu
  (lambda (stx)
    (display "emitting C++-proper\n")
    (syntax-parse stx
      [unit:tu-stx
       (display (string-join (stx-map make-cpp-decl #'unit.items)))])))