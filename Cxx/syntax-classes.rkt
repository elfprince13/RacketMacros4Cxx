#lang racket
(require syntax/parse
         (for-syntax syntax/parse))

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;
; Util
;;;;;;;;;;;;;;;;;;;;;;
(define-syntax ~bdatum
  (pattern-expander
   (lambda (stx)
     (syntax-case stx ()
       [(~bdatum id)
        #'(~and id (~datum id))]
       [(~bdatum bid id)
        #'(~and bid (~datum id))]))))

;;;;;;;;;;;;;;;;;;;;;;
; Types
;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-class record-kind-kw
  (pattern (~bdatum class))
  (pattern (~bdatum struct))
  (pattern (~bdatum __interface)) ;; hopefully shouldn't see this ever
  (pattern (~bdatum union))
  (pattern (~bdatum enum)))

(define-splicing-syntax-class cxx-type-placeholder
  (pattern (~seq (~bdatum !) (~bind [placeholder-op #'()])))
  (pattern (~seq placeholders-pref ... ((~bdatum !)) placeholders-suf ... (~bind [placeholder-op #'((placeholders-pref ...) . (placeholders-suf ...))]))))

(define-splicing-syntax-class cxx-type-simple
  (pattern (~seq type-terms ... (~peek (place:cxx-type-placeholder)) (~bind [pre-terms #'(type-terms ...)] [post-terms #'()] [placeholder-op #'()]))))

(define-splicing-syntax-class cxx-type-suffix
  (pattern (~seq ((~var placeholder cxx-type-placeholder)) type-terms-2  ... (~bind [pre-terms #'()] [post-terms #'(type-terms-2 ...)] [placeholder-op #'placeholder.placeholder-op]))))

(define-syntax-class cxx-type  
  (pattern (simple-type:cxx-type-simple suffix-type:cxx-type-suffix (~bind [pre-terms #'simple-type.pre-terms] [post-terms #'suffix-type.post-terms] [placeholder-op #'suffix-type.placeholder-op]))))


;;;;;;;;;;;;;;;;;;;;;;
; Expressions
;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-class cxx-expr
  (pattern (term ...+ (~bind [terms #'(term ...)])))
  (pattern atom))

;;;;;;;;;;;;;;;;;;;;;;
; Skeleton arguments
;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-class skel-id-arg
  (pattern (@ arg-id:id)))
(define-syntax-class skel-expr-arg
  (pattern (= arg-expr:cxx-expr)))
(define-syntax-class skel-stmt-arg
  (pattern (arg-stmt:cxx-stmt)))

(define-syntax-class skeleton-arg
  (pattern arg:skel-id-arg)
  (pattern arg:skel-expr-arg)
  (pattern arg:skel-stmt-arg))

(define-syntax-class skeleton-args
  (pattern (arg:skeleton-arg ...)))

;;;;;;;;;;;;;;;;;;;;;;
; Statements
;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-class cxx-empty
  (pattern ()))
(define-syntax-class cxx-decls
  (pattern decl:decls))
(define-syntax-class cxx-block
  (pattern ((~bdatum block) child ... (~bind [children #'(child ...)]))))
(define-syntax-class cxx-return
  (pattern ((~bdatum return) (~bind [ret-val #'()])))
  (pattern ((~bdatum return) ret-val:cxx-expr)))
(define-syntax-class cxx-for
  (pattern ((~bdatum for) ((~or init:decls init:expr) cond:cxx-expr update:cxx-expr) child:cxx-stmt)))
(define-syntax-class cxx-while
  (pattern ((~bdatum while) cond:cxx-expr child:cxx-stmt)))
(define-syntax-class cxx-if
  (pattern ((~bdatum if) cond:cxx-expr child:cxx-stmt (~bind [else #'()] [else-clause #'()])))
  (pattern ((~bdatum if) cond:cxx-expr child:cxx-stmt (~bdatum else) else-clause:cxx-stmt)))
(define-syntax-class cxx-@
  (pattern ((~bdatum @) kind:id (name:id) args:skeleton-args child:cxx-stmt)))

(define-syntax-class cxx-stmt
  (pattern item:cxx-empty)
  (pattern item:cxx-decls)
  (pattern item:cxx-block)
  (pattern item:cxx-for)
  (pattern item:cxx-while)
  (pattern item:cxx-if)
  (pattern item:cxx-@)
  (pattern item:cxx-expr))

;;;;;;;;;;;;;;;;;;;;;;
; Declaration stuff
;;;;;;;;;;;;;;;;;;;;;;
(define-splicing-syntax-class c-attribute
  (pattern (~seq (~bdatum __attribute__) ((attr-exp:expr)) )))

(define-splicing-syntax-class var-init
  (pattern (~seq (~var name id) (~bind [exp #'()])))
  (pattern (~seq (~var name id) (~bdatum =) init-exp:expr ...+ (~bind [exp #'(= init-exp ...)])))
  (pattern (~seq (~var name id) ( init-exp:expr ...+) (~bind [exp #'((init-exp ...))]))))

(define-syntax-class typedef-decl
  (pattern ((~bdatum typedef) (qualifier ...) type-info:cxx-type (~var new-name id) (~bind [qualifiers #'(qualifier ...)]))))

(define-syntax-class var-decl
  (pattern ((storage ...) type-info:cxx-type (~var init var-init) attr:c-attribute ... (~bind [storage-classes #'(storage ...)] [name #'init.name] [init-exp #'init.exp] [attributes #'(attr ...)]))))

(define-syntax-class record-decl
  (pattern ((qualifier ...) kind:record-kind-kw name:id attr:c-attribute ... (decl:var-decl ...) (~bind [qualifiers #'(qualifier ...)] [attributes #'(attr ...)] [decls #'(decl ...)])))
  (pattern ((qualifier ...) kind:record-kind-kw name:id attr:c-attribute ... (~bind [qualifiers #'(qualifier ...)] [attributes #'(attr ...)] [decls #'()])))) ; forward declaration

(define-syntax-class decls
  (pattern ((~bdatum def) (~var var var-decl) (extra-type:cxx-type extra-var:var-init) ... (~bind [extra-vars #'(extra-var ...)] [extra-type-infos #'(extra-type ...)]))))

(define-syntax-class inline-record-typedef
  (pattern ((~bdatum def) record:record-decl typedef:typedef-decl)))

; we should set a fail-when on arg
(define-syntax-class fun-decl
  (pattern ((~bdatum defun) (storage ...) (~var ret-type cxx-type) (~var name id) (arg:var-decl ... kw-arg:var-decl ...) attr:c-attribute ...  (~var body cxx-block) 
             (~bind [storage-classes #'(storage ...)] [args #'(arg ...)] [kw-args #'(kw-arg ...)] [attributes #'(attr ...)])))
  (pattern ((~bdatum defun) (storage ...) (~var ret-type cxx-type) (~var name id) (arg:var-decl ... kw-arg:var-decl ...) attr:c-attribute ... 
             (~bind [storage-classes #'(storage ...)] [args #'(arg ...)] [kw-args #'(kw-arg ...)] [attributes #'(attr ...)] [body #'()])))) ; forward declaration

;;;;;;;;;;;;;;;;;;;;;;
; Some high-level stuff
;;;;;;;;;;;;;;;;;;;;;;
(define-syntax-class tu-item
  (pattern item:cxx-@)
  (pattern item:typedef-decl)
  (pattern item:record-decl)
  (pattern item:inline-record-typedef)
  (pattern item:decls)
  (pattern item:fun-decl))

(define-syntax-class tu-stx
  (pattern ((~bdatum translation-unit) 
            (~optional ((~bdatum skeletons:) (skelId:id skelPath:str) ... configPath:str)) 
            item:tu-item ... 
            (~bind [items #'(item ...)] 
                   [skelIds (if (attribute skeletons:) #'(skelId ...) #'())] 
                   [skelPaths (if (attribute skeletons:) #'(skelPath ...) #'())]))))