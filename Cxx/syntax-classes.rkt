#lang racket
(require syntax/parse)

(provide (all-defined-out))



;;;;;;;;;;;;;;;;;;;;;;
; Types
;;;;;;;;;;;;;;;;;;;;;;

(define-splicing-syntax-class cxx-type-placeholder
  (pattern (~seq (~datum !) (~bind [placeholder-op #'()])))
  (pattern (~seq placeholders-pref ... ((~datum !)) placeholders-suf ... (~bind [placeholder-op #'((placeholders-pref ...) . (placeholders-suf ...))]))))

(define-splicing-syntax-class cxx-type-simple
  (pattern (~seq type-terms ... (~peek (place:cxx-type-placeholder)) (~bind [pre-terms #'(type-terms ...)] [post-terms #'()] [placeholder-op #'()]))))

(define-splicing-syntax-class cxx-type-suffix
  (pattern (~seq ((~var placeholder cxx-type-placeholder)) (type-terms-2  ...) (~bind [pre-terms #'()] [post-terms #'(type-terms-2 ...)] [placeholder-op #'placeholder.placeholder-op])))
  (pattern (~seq ((~var placeholder cxx-type-placeholder)) (~bind [pre-terms #'()] [post-terms #'()] [placeholder-op #'placeholder.placeholder-op]))))

(define-syntax-class cxx-type  
  (pattern (simple-type:cxx-type-simple suffix-type:cxx-type-suffix (~bind [pre-terms #'simple-type.pre-terms] [post-terms #'suffix-type.post-terms] [placeholder-op #'suffix-type.placeholder-op]))))


;;;;;;;;;;;;;;;;;;;;;;
; Statements
;;;;;;;;;;;;;;;;;;;;;;




;;;;;;;;;;;;;;;;;;;;;;
; Declaration stuff
;;;;;;;;;;;;;;;;;;;;;;

(define-splicing-syntax-class c-attribute
  (pattern (~seq (~datum __attribute__) ((attr-exp:expr)) )))

(define-splicing-syntax-class var-init
  (pattern (~seq (~var name id) (~bind [exp #'()])))
  (pattern (~seq (~var name id) (~datum =) init-exp:expr ...+ (~bind [exp #'(= init-exp ...)])))
  (pattern (~seq (~var name id) ( init-exp:expr ...+) (~bind [exp #'((init-exp ...))]))))

(define-syntax-class typedef-decl
  (pattern ((~datum typedef) (qualifier ...) type-info:cxx-type (~var new-name id) (~bind [qualifiers #'(qualifier ...)]))))

(define-syntax-class var-decl
  (pattern ((storage ...) type-info:cxx-type (~var init var-init) attr:c-attribute ... (~bind [storage-classes #'(storage ...)] [name #'init.name] [init-exp #'init.exp] [attributes #'(attr ...)]))))

(define-syntax-class record-decl
  (pattern ((qualifier ...) kind:id name:id attr:c-attribute ... (decl:var-decl ...) (~bind [qualifiers #'(qualifier ...)] [attributes #'(attr ...)] [decls #'(decl ...)])))
  (pattern ((qualifier ...) kind:id name:id attr:c-attribute ... (~bind [qualifiers #'(qualifier ...)] [attributes #'(attr ...)] [decls #'()])))) ; forward declaration

(define-syntax-class decls
  (pattern ((~datum def) (~var var var-decl) (extra-type:cxx-type extra-var:var-init) ... (~bind [extra-vars #'(extra-var ...)] [extra-type-infos #'(extra-type ...)]))))

; we should set a fail-when on arg
(define-syntax-class fun-decl
  (pattern ((~datum defun) (storage ...) (~var ret-type cxx-type) (~var name id) (arg:var-decl ... kw-arg:var-decl ...) attr:c-attribute ...  (~var body expr) 
             (~bind [storage-classes #'(storage ...)] [args #'(arg ...)] [kw-args #'(kw-arg ...)] [attributes #'(attr ...)])))
  (pattern ((~datum defun) (storage ...) (~var ret-type cxx-type) (~var name id) (arg:var-decl ... kw-arg:var-decl ...) attr:c-attribute ... 
             (~bind [storage-classes #'(storage ...)] [args #'(arg ...)] [kw-args #'(kw-arg ...)] [attributes #'(attr ...)] [body #'()])))) ; forward declaration

;;;;;;;;;;;;;;;;;;;;;;
; Some high-level stuff
;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-class tu-item
  (pattern item:typedef-decl)
  (pattern item:record-decl)
  (pattern item:decls)
  (pattern item:fun-decl))

(define-syntax-class tu-stx
  (pattern ((~datum translation-unit) item:tu-item ... (~bind [items #'(item ...)]))))