#lang racket

(require math/array)

(provide cuda-loop1d)

(struct tile (counter upper-bound body-gen))

(define make-default-tile
  (lambda (index ub)
    (tile index ub (lambda (body) body))))

(define make-for-tile
  (lambda (index ub)
    (tile
     index
     ub
     (lambda (body)
       (with-syntax ((index index)
                     (ub ub)
                     (body body))
         #'(for ((def (() int index = 0)) (< index ub) (++ index)) body))))))

(define make-unroll-tile (lambda (itr-var unroll-factor)                 
                   (tile
                    itr-var
                    unroll-factor
                    (lambda (body)
                      (letrec ((unroller (lambda (body factor-here tail)
                        (if (eq? 0 factor-here)
                            #`(begin #,@tail)
                            (unroller body (- factor-here 1)
                            (with-syntax ((itr-var itr-var) 
                                          (itr-val (- factor-here 1)) 
                                          (body body)) 
                              #`(#,@#'((begin (def ((const) int itr-var = itr-val)) body)) #,@tail))))))) 
                        (unroller body (syntax->datum unroll-factor) #'()))))))


(define wrap-in-bounds-check
  (lambda (body)
    (with-syntax ((body body)) #'(if (< (@ I () () ()) (@ N () () ())) (begin body)))))

(define make-boilerplate
  (lambda (body itr-exp geom-exp)
    (with-syntax ((body (wrap-in-bounds-check body))
                  (itr-exp itr-exp)) #;(@ I () () ())
      #'(begin (def ((const) int i = itr-exp)) body))))

(define compose-tiles 
  (lambda (outer inner)
    (tile
     (with-syntax 
         ((outer-counter (tile-counter outer))
          (inner-counter (tile-counter inner))
          (inner-bound (tile-upper-bound inner)))
       #'((+ (* outer-counter inner-bound) inner-counter)))
     (with-syntax 
         ((outer-bound (tile-upper-bound outer))
          (inner-bound (tile-upper-bound inner)))
       #'((* outer-bound inner-bound)))
     (lambda (body) 
       ((tile-body-gen outer) ((tile-body-gen inner) body))))))

(define compose-tile-permutation ; accepts in order: outer -> inner
  (lambda (tile-list tile-permutation)
    (letrec 
        ((tile-array (list->array tile-list))
         (permuted-array 
          (array-slice-ref 
           tile-array 
           (list (reverse tile-permutation))))
         (permuted-list (array->list permuted-array)))
      (foldl compose-tiles 
             (car permuted-list) 
             (cdr permuted-list)))))

(define make-permutation-macro-from-tile-constructors
  (lambda (tile-constructors)
    (lambda (body idxs bounds permutation)
    (let* ((tiles (map (lambda (tf i b) (tf i b)) tile-constructors idxs bounds))
           (loop-structure (compose-tile-permutation tiles permutation)))
      (values (tile-counter loop-structure) (tile-upper-bound loop-structure) ((tile-body-gen loop-structure) (make-boilerplate body (tile-counter loop-structure) (tile-upper-bound loop-structure))))))))

(define cuda-loop1d (make-permutation-macro-from-tile-constructors (list make-default-tile make-default-tile make-default-tile make-for-tile make-unroll-tile)))
(define cuda-loop1d-nowarp (make-permutation-macro-from-tile-constructors (list make-default-tile make-default-tile make-for-tile make-unroll-tile)))
           
; Uncomment for a simple test that everything is working as expected
#;(let ((unroll-tile (make-unroll-tile #'j #'4))
      (for-tile (make-for-tile #'i #'4))
      (thread-tile (tile #'(threadIdx . x) #'(blockDim . x) (lambda (body) body)))
      (block-tile (tile #'(blockIdx . x) #'(gridDim . x) (lambda (body) body))))
    (let-values (((new-tile) (compose-tiles block-tile (compose-tiles thread-tile (compose-tiles for-tile unroll-tile))))
          ((new-tile2) (compose-tile-permutation (list block-tile thread-tile for-tile unroll-tile) '(0 1 2 3)))
          ((counter3 bounds3 body3) (cuda-loop1d-nowarp 
                                     #'(= (* ((+ testA @I))) (* ((+ testB @I)))) 
                                     (list #'(blockIdx . x) #'(threadIdx . x) #'i #'j) (list #'(gridDim . x) #'(blockDim . x) #'4 #'4) 
                                     '(0 1 2 3)))) 
      (begin 
        (print (tile-counter new-tile)) 
        (newline) 
        (print (tile-counter new-tile2)) 
        (newline) 
        (print counter3) 
        (newline) 
        (print (tile-upper-bound new-tile)) 
        (newline) 
        (print (tile-upper-bound new-tile2)) 
        (newline) 
        (print bounds3) 
        (newline) 
        (print ((tile-body-gen new-tile) (wrap-in-bounds-check #'(= (* ((+ testA @I))) (* ((+ testB @I)))))))
        (newline)
        (print ((tile-body-gen new-tile2) (wrap-in-bounds-check #'(= (* ((+ testA @I))) (* ((+ testB @I)))))))
        (newline)
        (print body3)
        (newline))))
