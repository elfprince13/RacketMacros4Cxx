#lang racket

(require 
  (for-syntax 
   macro-debugger/emit
   racket/function
   racket/dict
   racket/set
   racket/syntax
   syntax/context
   syntax/id-table
   syntax/parse
   ;syntax/parse/debug
   syntax/stx
   (for-syntax
    racket
    syntax/parse)))

(require 
  (for-syntax
  "define-forms.rkt"
  "syntax-classes.rkt"
  "util.rkt"
  "Writer.rkt"))

(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;
; Skeleton handler
;;;;;;;;;;;;;;;;;;;;;;;;
(define-for-syntax InitSkelIds
  (make-hash))

(define-for-syntax @-handler
  (lambda (stx [in-defs #f])
    (syntax-parse stx 
      [(~or skel:cxx-@ skel:cxx-@expr)
       ;(display (~a (list "Looking up" (syntax->datum #'skel.kind) "in" InitSkelIds))) (newline)
       (with-syntax
           ([skel-macro
             (hash-ref
              InitSkelIds
              (syntax->datum #'skel.kind) ; If it's a top-level thingy, we shouldn't have any problem looking it up. 
              (lambda () (macroize #'skel.kind)))]) ; If not, we shouldn't have any problem with the marks
         (emit-local-step #'skel.kind #'skel-macro #:id #'macroize-skel-kind)
         (if (syntax-local-value #'skel-macro (thunk #f) in-defs)
             (let
                 ([expandable
                   (with-syntax
                       ([(child-term ...) 
                         (if (attribute skel.child)
                             #'(skel.child)
                             #'())]
                        [name-term
                         (if (attribute skel.name)
                             #'(skel.name)
                             #'())])
                     #'(skel-macro name-term skel.args child-term ...))]
                  [macro (syntax-local-value #'skel-macro #f in-defs)])
               (if in-defs
                   (macro expandable in-defs)
                   (local-expand expandable (generate-expand-context) #f)))
             (raise-user-error (syntax->datum #'skel-macro) "Not bound as a skeleton in this context")))])))

(define-syntax @ @-handler)

;;;;;;;;;;;;;;;;;;;;;;;;
; Top-level definitions
;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax translation-unit
  (lambda (stx)
    (set! InitSkelIds (make-hash)) ; Clean slate, if for some reason we get multiple TUs in a row (e.g. our demo)
    (syntax-parse stx
      [unit:tu-stx
       (letrec-syntaxes+values
           ([(defer-l)
             (syntax-parser
               [(defer-l stx)
                #'(thunk 
                   (with-syntax ([v stx])
                     #'(v)))])])
           ([(top-level-defs) (syntax-local-make-definition-context)]
            [(ctx) (generate-expand-context)]
            [(skel-ids skel-paths) 
             (apply values 
                    (stx-map syntax->list #'(unit.skelIds unit.skelPaths)))]
            [(params-table) 
              (if (attribute unit.configPath)
                  (jsonpath-to-table (string->path (syntax->datum #'unit.configPath)))
                  #hasheq())])
         (syntax-local-bind-syntaxes 
          (map macroize skel-ids)
          (with-syntax
              ([(skel-defs ...) 
                (map
                 (lambda (skel-id skel-path)
                   ((dynamic-require (string->path (syntax->datum skel-path)) (syntax->datum skel-id)) 
                    params-table)) 
                 skel-ids skel-paths)])
            #'(values skel-defs ...))
          top-level-defs)
         (internal-definition-context-seal top-level-defs)
         (for ([skel-id skel-ids])
            (hash-set! InitSkelIds (syntax->datum skel-id) (internal-definition-context-apply/loc top-level-defs (macroize skel-id))))
         stx
         (with-syntax 
             ([((declaration ...) ...)
               (reverse 
                (map
                 (lambda (f)
                   ;(display "un-thunking") (display f) (newline)
                   (f))
                 
                 (reverse 
                  (let loop
                    ([work (syntax->list #'unit.items)]
                     [defs top-level-defs]
                     [ctx ctx])
                    (if (null? work)
                        null
                        (let-values ([(head rest) (values (car work) (cdr work))])
                          ;(display head) (display " ") (display rest) (newline)
                          (let-values 
                              ([(head defs)
                                (syntax-parse head
                                  [record:record-decl (values (defer-l #'record) defs)]
                                  [typedef:typedef-decl (values (defer-l #'typedef) defs)]
                                  [tls:cxx-@
                                   (let-values ([(deferred-syntax defs new-skels) (@-handler #'tls defs)])
                                     (for ([bind-pair new-skels])
                                       ;(emit-remark "introducing" (cdr bind-pair))
                                       (hash-set! InitSkelIds (car bind-pair) (cdr bind-pair)))
                                     (values deferred-syntax defs))]
                                  [vdefun:fun-decl 
                                   #;(emit-local-step #'vdefun.body (local-expand
                                                                     #'vdefun.body ctx #f top-level-defs) #:id #'decl-f)
                                   (let ([defs (syntax-local-make-definition-context defs)])
                                     (bind-and-seal defs (list #'vdefun.name))
                                     (emit-local-step #'vdefun.name (internal-definition-context-apply/loc defs #'vdefun.name) #:id #'bas)
                                     (let ([defun-form (defun #'vdefun ctx defs)])
                                       (values (defer-l defun-form) defs)))]
                                  [vdefs:decls 
                                   (let-values ([(defs head) (def head ctx defs)])
                                     (values (defer-l head) defs))])])
                            (cons head (loop rest defs ctx)))))))))])
           (no-expand #'(unit.translation-unit declaration ... ...))))])))

(define-for-syntax display-inert-body
  (lambda (tag contents) 
    (with-syntax* ([stx-tag tag] 
                   [contents 
                    (stx-map 
                     (lambda (stx)
                       (let* ([expanded (local-expand stx 'top-level #f)]
                              [expanded-safe 
                               (let*
                                   ([uniq-table (mutable-set)]
                                    [leaf-f (thunk* (void))]
                                    [decl-f
                                     (lambda (id-l)
                                       (for ([id id-l])
                                         (when (syntax-original? (syntax-local-introduce id))
                                           (set-add! uniq-table (syntax->datum id))
                                           )))]
                                    [iter-f
                                     (lambda (parse-node stx-l)
                                       (for ([child (syntax->list stx-l)])
                                         (parse-node child)))])
                                 ((walk-decls leaf-f decl-f iter-f) expanded) ; walk once to catalog all the original ids
                                 (let*
                                     ([bind-table (make-bound-id-table)]
                                      [leaf-f 
                                       (lambda (stx)
                                         (if (identifier? stx)
                                             (dict-ref bind-table stx stx)
                                             stx))]
                                      [decl-f
                                       (lambda (id-l)
                                         (for ([id id-l])
                                           (if (syntax-original? (syntax-local-introduce id))
                                               id
                                               (let loop
                                                 ([btv (generate-temporary id)])
                                                 (if (set-member? uniq-table (syntax->datum btv))
                                                     (loop (generate-temporary btv))
                                                     (begin
                                                       (dict-set! bind-table id btv)
                                                       (set-add! uniq-table (syntax->datum btv))
                                                       btv))))))]
                                      [iter-f
                                       (lambda (parse-node stx-l)
                                         (with-syntax
                                             ([(seq ...) (stx-map parse-node stx-l)])
                                           (syntax-property #'(seq ...) 'paren-shape (syntax-property stx-l 'paren-shape))))])
                                   ((walk-decls leaf-f decl-f iter-f) expanded)))]) ; walk again to replace all the introduced ids
                         (emit-local-step expanded expanded-safe #:id #'walk-expr-safe-ids)
                         (make-cpp-tu expanded-safe))) 
                     contents)]
                   [unpacked #'(let () (map display 'contents) (void))]) 
      (if tag 
          #'(stx-tag unpacked) 
          #'unpacked))))

(define-syntax top-interaction
  (lambda (stx)
    (syntax-case stx ()
      [(top-interaction . body)
       (display-inert-body #f #'(body))])))

(define-syntax module-begin
  (lambda (stx)
    (syntax-case stx ()
      [(module-begin bodies ...)
       (display-inert-body #'#%module-begin #'(bodies ...))])))



