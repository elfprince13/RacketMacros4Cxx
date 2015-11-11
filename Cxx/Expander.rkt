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
   syntax/stx))

(require 
  (for-syntax
  "define-forms.rkt"
  "syntax-classes.rkt"
  "util.rkt"
  "Writer.rkt"))

(provide (all-defined-out))

(define simple-external-params-table
  (hash 
   '(Loop1d (test_loop)) (list 
                          (list #'(blockIdx . x)) (list #'((/ (threadIdx . x) 32))) (list #'((& (threadIdx . x) 31))) (list #'k) (list #'j) 
                          (list #'(gridDim . x)) (list #'((/ (blockDim . x) 32))) (list #'32) (list #'1) (list #'4)) 
   '(I ()) '()
   '(N ()) '()))

;;;;;;;;;;;;;;;;;;;;;;;;
; Skeleton handler
;;;;;;;;;;;;;;;;;;;;;;;;

(define-for-syntax InitSkelTable 
  (hash 'Loop1d 
        (dynamic-require (string->path "/Users/thomas/Documents/Brown/Proteins/racket-tests/Loop1d.rkt") 'Loop1d)))

(define-for-syntax InitSkelIds
  (make-hash))

(define-syntax @
  (lambda (stx)
    (syntax-parse stx 
      [skel:cxx-@
       (with-syntax
           ([skel-macro 
             (hash-ref
              InitSkelIds
              (syntax->datum #'skel.kind) ; If it's a top-level thingy, we shouldn't have any problem looking it up. 
              (lambda () (macroize-skel-kind #'skel.kind)))]) ; If not, we shouldn't have any problem with the marks
         (local-expand #'(skel-macro (skel.name) skel.args skel.child) (generate-expand-context) #f))])))

;;;;;;;;;;;;;;;;;;;;;;;;
; Top-level definitions
;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax translation-unit
  (lambda (stx)
    ;(display "starting up") (display (init-clock)) (newline)
    (syntax-parse stx
      [unit:tu-stx
       (let ([top-level-defs (syntax-local-make-definition-context)]
             [ctx (generate-expand-context)]
             [skel-ids (list #'Loop1d)])
         ;(display "have a parse") (display (tick)) (newline)
         (syntax-local-bind-syntaxes 
          (map macroize-skel-kind skel-ids)
          (with-syntax
              ([(skel-defs ...) 
                (map
                 (lambda (skel-id)
                   (hash-ref InitSkelTable (syntax->datum skel-id))) skel-ids)])
            #'(values skel-defs ...))
          top-level-defs)
         (internal-definition-context-seal top-level-defs)
         (map 
          (lambda (skel-id)
            (hash-set! InitSkelIds (syntax->datum skel-id) (internal-definition-context-apply/loc top-level-defs (macroize-skel-kind skel-id)))) skel-ids)
         ;(display "skeletons bound") (display (tick)) (newline)
         stx
         (with-syntax 
             ([(declaration ...)
               (stx-map 
                (lambda (declaration)
                  (define out-form 
                    (syntax-parse declaration
                      [record:record-decl #'record]
                      [typedef:typedef-decl #'typedef]
                      [vdefun:fun-decl 
                       (let ([new-defs (syntax-local-make-definition-context top-level-defs)])
                         (bind-and-seal new-defs (list #'vdefun.name))
                         (set! top-level-defs new-defs)
                         (defun #'vdefun ctx top-level-defs))]
                      [vdefs:decls 
                       (let-values ([(new-defs expanded-stx) (def #'vdefs ctx (syntax-local-make-definition-context top-level-defs))])
                         (set! top-level-defs new-defs)
                         expanded-stx)]))
                  (set! top-level-defs (syntax-local-make-definition-context top-level-defs))
                  out-form)
                #'unit.items)])
           (no-expand #'(unit.translation-unit declaration ...))))])))

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
                                           #'(seq ...)))])
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



