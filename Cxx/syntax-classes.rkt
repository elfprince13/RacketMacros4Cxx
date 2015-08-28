#lang racket
(require syntax/parse)

(provide (all-defined-out))

(define-splicing-syntax-class c-attribute
  (pattern (~seq (~literal __attribute__) ((attr-exp:expr)) )))

(define-splicing-syntax-class var-init
  (pattern (~seq (~var name id) (~bind [exp #'()])))
  (pattern (~seq (~var name id) = init-exp:expr ...+ (~bind [exp #'(= init-exp ...)])))
  (pattern (~seq (~var name id) ( init-exp:expr ...+) (~bind [exp #'((init-exp ...))]))))

(define-syntax-class typedef-decl
  (pattern ((~literal typedef) type-info ...+ (~bind [type-terms #'(type-info ...)])))) ; We don't actually care what's in it for now. We'll try to steer clear of type-space metaprogramming.


(define-syntax-class record-decl
  (pattern ((qualifier ...) kind:id name:id attr:c-attribute ... (decl ...) (~bind [qualifiers #'(qualifier ...)] [attributes #'(attr ...)] [decls #'(decl ...)])))
  (pattern ((qualifier ...) kind:id name:id attr:c-attribute ... (~bind [qualifiers #'(qualifier ...)] [attributes #'(attr ...)] [decls #'()])))) ; forward declaration

(define-syntax-class var-decl
  (pattern ((storage ...) (~var type expr) (~var init var-init) attr:c-attribute ... (~bind [storage-classes #'(storage ...)] [name #'init.name] [init-exp #'init.exp] [attributes #'(attr ...)]))))

(define-syntax-class decls
  (pattern (def (~var var var-decl) (extra-var:var-init) ... (~bind [extra-vars #'(extra-var ...)]))))

; we should set a fail-when on arg
(define-syntax-class fun-decl
  (pattern (defun (storage ...) (~var ret-type expr) (~var name id) (arg:var-decl ... kw-arg:var-decl ...) attr:c-attribute ...  (~var body expr) 
             (~bind [storage-classes #'(storage ...)] [args #'(arg ...)] [kw-args #'(kw-arg ...)] [attributes #'(attr ...)])))
  (pattern (defun (storage ...) (~var ret-type expr) (~var name id) (arg:var-decl ... kw-arg:var-decl ...) attr:c-attribute ... 
             (~bind [storage-classes #'(storage ...)] [args #'(arg ...)] [kw-args #'(kw-arg ...)] [attributes #'(attr ...)] [body #'()])))) ; forward declaration


(define-syntax-class tu-item
  (pattern item:typedef-decl)
  (pattern item:record-decl)
  (pattern item:decls)
  (pattern item:fun-decl))

(define-syntax-class tu-stx
  (pattern (translation-unit item:tu-item ... (~bind [items #'(item ...)]))))