#lang racket
(require syntax/parse)

(provide (all-defined-out))

(define-splicing-syntax-class var-init
  (pattern (~seq (~var name id) (~bind [exp #'()])))
  (pattern (~seq (~var name id) = init-exp:expr ...+ (~bind [exp #'(= init-exp ...)])))
  (pattern (~seq (~var name id) ( init-exp:expr ...+) (~bind [exp #'((init-exp ...))]))))

(define-syntax-class var-decl
  (pattern ((storage ...) (~var type expr) (~var init var-init) (~bind [storage-classes #'(storage ...)] [name #'init.name] [init-exp #'init.exp]))))

(define-syntax-class decls
  (pattern (def (~var var var-decl) extra-var:var-init ... (~bind [extra-vars #'(extra-var ...)]))))

; we should set a fail-when on arg
(define-syntax-class fun-decl
  (pattern (defun (storage ...) (~var ret-type expr) (~var name id) (arg:var-decl ... kw-arg:var-decl ...) (~var body expr) 
             (~bind [storage-classes #'(storage ...)] [args #'(arg ...)] [kw-args #'(kw-arg ...)]))))