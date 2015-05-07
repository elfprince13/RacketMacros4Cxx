#lang racket

(require (for-template racket))
(require "LoopTiles.rkt")
(require "CppWriter.rkt")

(define simple-external-params-table
  (hash 
   '(Loop1d (test-loop)) (list 
                          (list #'(blockIdx . x)) (list #'((/ (threadIdx . x) 32))) (list #'((& (threadIdx . x) 31))) (list #'k) (list #'j) 
                          (list #'(gridDim . x)) (list #'((/ (blockDim . x) 32))) (list #'32) (list #'1) (list #'4)) 
   '(I ()) '()
   '(N ()) '()))


(define lookup-skeleton
  (lambda (table-list key)
    (begin
      ;(print (list "looking up "  key " in " table-list)) (newline)
      (match table-list 
        [(cons h t) (hash-ref h key (lambda () (lookup-skeleton t)))]
        [(list ) #f]))))

(struct skeleton-expansion (body table))

; Grid > Block > Warp > Thread-Loop > Unroll
; 
(define init-skeletons-table 
  (list (hash 'Loop1d 
              (lambda (skel table-here) 
                (syntax-case skel (@ Loop1d)
                  [(@ Loop1d (name ...) (params ...) body)
                   (let* ((params (syntax->datum #'(params ...)))
                          (num-params (length params)))
                     (cond
                       ((> 2 num-params) (raise-argument-error 'params "at least 2 arguments required" params))
                       ((< 4 num-params) (raise-argument-error 'params "at least 4 arguments required" params))
                       (else 
                        (let ((itr-var (car (car params)))
                              (lower-bound (if (eq? 2 num-params) 0 (car (cadr params))))
                              (upper-bound (car (if (eq? 2 num-params) (cadr params) (caddr params))))
                              (stride (if (eq? 4 num-params) (car (cadddr params)) 1)))
                          (lambda (ext-params)
                            (let ((num-ext-params (length ext-params)))
                              (if (eq? 10 num-ext-params)
                                  (let*-values (((split-vars split-bounds) (split-at (drop ext-params (- num-ext-params 10)) 5))
                                                ((index-vars) (map car split-vars))
                                                ((index-bounds) (map car split-bounds)))
                                    (let-values (((counter-exp geom-exp loop-code) (cuda-loop1d #'body index-vars index-bounds (list 0 1 2 3 4))))
                                      (skeleton-expansion
                                       loop-code
                                       (cons (hash 'I (lambda (skel table-here) 
                                                        (syntax-case skel (@ I)
                                                          [(@ I () () ()) 
                                                           (lambda (ext-params) 
                                                             (skeleton-expansion
                                                              (with-syntax ((itr-var itr-var)) #'itr-var)
                                                              table-here))]))
                                                   'N (lambda (skel table-here) 
                                                        (syntax-case skel (@ I)
                                                          [(@ N () () ()) 
                                                           (lambda (ext-params) 
                                                             (skeleton-expansion 
                                                              (with-syntax ((upper-bound upper-bound)) #'upper-bound)
                                                              table-here))]))) 
                                             table-here))))
                                  (raise-argument-error 'ext-params "10 arguments required" ext-params))))))))])))))


(define expand-stmt 
  (lambda (stmt skels) 
    (syntax-case stmt (if else for while begin call def @) 
      [(@ SkelKind (name ...) (params ...) body) 
       (begin 
         #;(print "expanding skeleton")
         #;(newline)
         (let ((expansion (((lookup-skeleton skels (syntax->datum #'SkelKind)) stmt skels) 
                           (hash-ref simple-external-params-table (list (syntax->datum #'SkelKind) (syntax->datum #'(name ...)))))))
           (begin
             ;(print (list "re-expanding with " (skeleton-expansion-body expansion) (skeleton-expansion-table expansion))) (newline)
             (expand-stmt (skeleton-expansion-body expansion) (skeleton-expansion-table expansion)))))] ; The expansion process may have introduced new macros, so expand those too
      [(while (cond ...) body) 
       (with-syntax 
           ((body (expand-stmt #'body skels)))
         #'(while (cond ...) body))]
      [(for ((init ...) (cond ...) (update ...)) body) 
       (with-syntax
           ((init (expand-stmt #'(init ...) skels))
            (cond (expand-stmt #'(cond ...) skels))
            (update (expand-stmt #'(update ...) skels))
            (body (expand-stmt #'body skels)))
         #'(for (init cond update) body))]
      [(if (cond ...) body) 
       (with-syntax  
           ((cond (expand-stmt #'(cond ...) skels))
            (body (expand-stmt #'body skels))) 
         #'(if cond body))]
      [(if (cond ...) body else else-body) 
       (with-syntax 
           ((cond (expand-stmt #'(cond ...) skels))
            (body (expand-stmt #'body skels))
            (else-body (expand-stmt #'else-body skels))) 
         #'(if cond body else else-body))]
      [(begin stmts ... ) 
       (begin 
         #;(print "expanding begin") 
         #;(newline ) 
         (let ((stmts (map (curryr expand-stmt skels) (syntax->list #'(stmts ...)))))
           (datum->syntax stmt (cons 'begin stmts))))]
      [(call func args ... ) 
       (begin 
         #;(print "expanding call") 
         #;(newline ) 
         (let ((args (map (curryr expand-stmt skels) (syntax->list #'(args ...)))))
           (with-syntax ((func (expand-stmt #'func skels)))
             #`(call func #,@(datum->syntax stmt args)))))]
      [(def defs ...) 
       (expand-decl stmt skels)]
      [(expr ...) 
       (begin
         #;(print "no match for ") 
         #;(newline)
         #;(print stmt)
         #;(newline)
         (let ((expr-components 
                (map (curryr expand-stmt skels) (syntax->list #'(expr ...)))))
           (datum->syntax stmt expr-components)))]
      [any #'any]))) 

(define expand-decl 
  (lambda (decl skels)
    (syntax-case decl (def defun)
      [(defun (storage ...) 
         ret-type name (args ...) body)
       (with-syntax ((body (expand-stmt #'body skels)))
         #'(defun (storage ...) ret-type name (args ...) body))]
      [(def ((storage ...) type name init ...) 
         (next-name next-init ...) ...) decl])))


; Note: we don't handle [] array syntax yet, because [] are ()
(let ((expanded-code 
       (expand-decl
        #'(defun (__global__) void kernelTest ((() int argc) (() char **argv)) 
            (begin
              (@ Loop1d (test-loop) ((i) (0) (argc)) 
                 (call printf "%s\\n" (* ((+ argv (@ I () () ()))))))
              (call printf "done\\n"))) init-skeletons-table)))
  (begin
    (print expanded-code)
    (newline)
    (display (make-cpp-decl expanded-code))))

