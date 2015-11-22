#lang racket

(require syntax/parse
         syntax/stx
         "syntax-classes.rkt"
         "util.rkt")

(provide (all-defined-out))

(define make-cpp-expr 
  (syntax-parser
    [skel:cxx-@expr (raise-user-error 'make-cpp-expr (~a (list "Unexpanded skeleton: " #'skel)))]
    [((~and unop
            (~or 
             (~datum >++)
             (~datum >--)
             (~datum ++<)
             (~datum --<))) arg)
     (let
         ([op-str (make-cpp-expr #'unop)]
          [arg-str (make-cpp-expr #'arg)])
       (if (char=? (string-ref op-str 0) #\>)
           (string-append arg-str " " (substring op-str 1 3))
           (string-append (substring op-str 0 2)  " "  arg-str)))]
    [((~datum call) callee args ...) 
     (string-append (make-cpp-expr #'callee) "("
                    (string-join 
                     (map make-cpp-expr (syntax->list #'(args ...))) ", ")
                    ")")]
    [((~and cast-name
            (~or
             (~datum static_cast)
             (~datum dynamic_cast)
             (~datum reinterpret_cast)
             (~datum const_cast))) cast-type:cxx-type cast-expr)
     (string-append
      (make-cpp-expr #'cast-name) "<" (synth-type-text #'cast-type "") ">(" (make-cpp-expr #'cast-expr) ")" )]
    [((~datum c-cast) cast-type:cxx-type cast-expr) 
     (string-append 
      "(" (synth-type-text #'cast-type "") ")" (make-cpp-expr #'cast-expr))]
    [((paren-expr ...)) 
     (begin
       ;(display "this is a paren-expr: ") (display expr) (newline)
       ; Need to figure out how to preserve []
       #;(when (not (eq? (syntax-property expr 'paren-shape) #\())
           (begin (display expr) (newline) (syntax-property expr 'paren-shape) (newline)))
       (string-append "(" (make-cpp-expr #'(paren-expr ...)) ")"))]
    [(operator arg1 arg2)
     (string-append (make-cpp-expr #'arg1) " " (make-cpp-expr #'operator) " " (make-cpp-expr #'arg2))]
    [(operator arg1)
     (string-append (make-cpp-expr #'operator) " " (make-cpp-expr #'arg1))]
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
              (string-append "\"" name-or-literal "\"")]
             [(char? name-or-literal)
              (string-append 
               "'"
               (let ([hex-v 
                      (lambda (n)
                        (~a (number->string (char->integer name-or-literal) 16)
                            #:width n
                            #:align 'right
                            #:pad-string "0"))])
                 (cond
                   [(and (char<=? name-or-literal #\uff) (char-alphabetic? name-or-literal) (char-numeric? name-or-literal) (char-punctuation? name-or-literal))
                    (~a name-or-literal)]
                   [(char<=? name-or-literal #\uff)
                    (string-append 
                     "\\x" (hex-v 2))]
                   [(char<=? name-or-literal #\uffff)
                    (string-append 
                     "\\u" (hex-v 4))]
                   [else 
                    (string-append 
                     "\\U" (hex-v 8))]))
               "'")]
             [else (raise-argument-error 'name-or-literal "expected name, number, character, or string" name-or-literal)]))]))

(define make-cpp-stmt 
  (syntax-parser
    [skel:cxx-@ (raise-user-error 'make-cpp-stmt (~a (list "Unexpanded skeleton: " #'skel)))]
    [for:cxx-for
     (string-append
      "for (" 
      (syntax-parse #'for.init
        [init:decls (make-cpp-decl #'init #f)]
        [(init:cxx-expr) (make-cpp-expr #'(init))]) 
      ";" 
      (make-cpp-expr #'for.cond) ";" (make-cpp-expr #'for.update) ")"
      (make-cpp-stmt #'for.child))]
    [switch:cxx-switch
     (string-append
      "switch (" (make-cpp-expr #'switch.cond) ") " (make-cpp-stmt #'switch.child))]
    [while:cxx-while 
     (string-append
      "while (" (make-cpp-expr #'while.cond) ") " (make-cpp-stmt #'while.child))]
    [if-stmt:cxx-if 
     (string-append
      "if (" (make-cpp-expr #'if-stmt.cond) ") " (make-cpp-stmt #'if-stmt.child) 
      (if (stx-null? #'if-stmt.else-clause)
          ""
          (string-append " else " (make-cpp-stmt #'if-stmt.else-clause))))]
    [block:cxx-block 
     (string-append 
      "{\n" (string-join (stx-map make-cpp-stmt #'block.children) "") "}\n")]
    [default:cxx-default 
     (string-append 
      "default:\n" (string-join (stx-map make-cpp-stmt #'default.children) "") "\n")]
    [case:cxx-case 
     (string-append 
      "case " (make-cpp-expr #'case.when) ":\n" (string-join (stx-map make-cpp-stmt #'case.children) "") "\n")]
    [decl:cxx-decls
     (make-cpp-decl #'decl)]
    [skel:cxx-@
     (raise-argument-error 'make-cpp-stmt "Received statement contained unexpanded skeleton: " #'skel)]
    [empty:cxx-empty ";\n"]
    [expr:cxx-expr 
     (string-append (make-cpp-expr #'expr) ";\n")]))

(define make-cpp-init 
  (syntax-parser
    [(init:var-init)
     (handle-init 
      #'init.exp 
      (thunk "")
      (lambda (eq-expr) (string-append "= " (make-cpp-expr (stx-car eq-expr))))
      (lambda (paren-expr) (string-append "(" (make-cpp-expr paren-expr) ")")))]))

(define make-cpp-single-decl
  (syntax-parser
    [var:var-decl 
     (string-append
      (make-storage-classes #'var.storage-classes)
      (synth-type-text #'var.type-info (string-from-stx #'var.name)) (make-cpp-init #'var.init)
      (let ([attr-text (make-attributes #'var.attributes)])
        (if (equal? attr-text "")
            ""
            (string-append " " attr-text))))]))

(define make-storage-classes
  (lambda (storage-syntax [string-f string-from-stx])
    (string-join 
     (stx-map string-f storage-syntax) " " #:after-last (if (stx-null? storage-syntax ) "" " "))))

(define make-attributes 
  (lambda (attrs)
    (string-join 
     (stx-map 
      (lambda (attr)
        (string-join 
         (stx-map
          (lambda (stx)
            (~a (syntax->datum stx))) attr)
         "")) attrs) " ")))
(define make-qualifiers make-storage-classes)


(define synth-type-text
  (lambda (type-stx placeholder-text)
    (let* ([synth-pref
           (lambda (pref-stx)
             (string-join 
              (stx-map 
               (lambda (stx) 
                 (~a (syntax->datum stx))) pref-stx)))]
          [synth-place 
           (lambda (place-stx)
             (if (stx-null? place-stx)
                 placeholder-text
                 (string-append
                  "(" (string-join (map ~a (syntax->datum (stx-car place-stx))) " ")
                  " "
                  placeholder-text " "
                  (string-join (map ~a (syntax->datum (stx-cdr place-stx)))) ")")))]
          [synth-suf 
           (lambda (suf-stx)
             (string-join 
              (stx-map 
               (lambda (stx) 
                 (~a (syntax->datum stx))) suf-stx)))]
          [synth-all
           (lambda (pref-stx place-stx suf-stx)
             (string-append
              (synth-pref pref-stx) " "
              (synth-place place-stx) " "
              (synth-suf suf-stx)))])
      (syntax-parse type-stx
        [type:cxx-type 
         (synth-all #'type.pre-terms #'type.placeholder-op #'type.post-terms)]
        [(type:cxx-type-suffix) 
         (synth-all #'type.pre-terms #'type.placeholder-op #'type.post-terms)]
        [(type:cxx-type-simple) 
         (synth-all #'type.pre-terms #'type.placeholder-op #'type.post-terms)]))))

(define make-cpp-decl 
  (lambda (stx [assume-decls-stmt #t])
    (syntax-parse stx
      [vb:cxx-verbatim (syntax->datum #'vb.blob)]
      [skel:cxx-@ (raise-user-error 'make-cpp-decl (~a (list "Unexpanded skeleton: " #'skel)))]
      [typedef:typedef-decl
       (string-append
        "typedef "
        (make-qualifiers #'typedef.qualifiers)
        (synth-type-text #'typedef.type-info  (string-from-stx #'typedef.new-name))
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
             (string-join (stx-map make-cpp-single-decl #'record.decls))
             "}"))
        
        ";\n")]
      [defun:fun-decl
        (string-append 
         (make-storage-classes #'defun.storage-classes)
         (synth-type-text 
          #'defun.ret-type
          (string-append
           (string-from-stx #'defun.name) "(" (string-join (map make-cpp-single-decl (syntax->list #'defun.args)) ", ") ")")) 
         " "
         (make-attributes #'defun.attributes)
         " "                                                                                                                     
         (make-cpp-stmt #'defun.body))]
      [def:decls 
        (string-append
         (make-cpp-single-decl #'def.var) 
         (let ([extra-inits (syntax->list #'def.extra-vars)]
               [extra-types (syntax->list #'def.extra-type-infos)])
           (string-join
            (map
             (lambda (type-stx init-stx)
               (string-append
                (syntax-parse init-stx
                  [(init-stx:var-init)
                   (synth-type-text type-stx (string-from-stx #'init-stx.name))])
                (make-cpp-init init-stx) ))
             extra-types
             extra-inits) 
            ", " 
            #:before-first (if (eq? 0 (length extra-inits)) "" ", ")))
         (if assume-decls-stmt
             ";\n"
             ""))])))

(define make-cpp-tu
  (lambda (stx)
    (string-append 
     "// emitting C++-proper via Racket's #Cxx\n"
     (syntax-parse stx
      [unit:tu-stx
       (string-join (stx-map make-cpp-decl #'unit.items))]))))