#lang racket

(struct tile (counter upper-bound body-gen))

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

(define wrap-in-bounds-check
  (lambda (body)
    (with-syntax ((body body)) #'(if (@I < @N) body))))

(define compose-tiles 
  (lambda (outer inner)
    (tile
     (with-syntax ((outer-counter (tile-counter outer))
                  (inner-counter (tile-counter inner))
                  (inner-bound (tile-upper-bound inner)))
                 #'(+ (* outer-counter inner-bound) inner-counter))
     (with-syntax ((outer-bound (tile-upper-bound outer))
                  (inner-bound (tile-upper-bound inner)))
                 #'(* outer-bound inner-bound))
     (lambda (body) 
       ((tile-body-gen outer) ((tile-body-gen inner) body))))))

(let ((for-tile (make-for-tile #'i #'4))
      (thread-tile (tile #'(threadIdx . x) #'(blockDim . x) (lambda (body) body)))
      (block-tile (tile #'(blockIdx . x) #'(gridDim . x) (lambda (body) body))))
    (let ((new-tile (compose-tiles block-tile (compose-tiles thread-tile for-tile)))) 
      (begin 
        (print (tile-counter new-tile)) 
        (newline) 
        (print (tile-upper-bound new-tile)) 
        (newline) 
        (print ((tile-body-gen new-tile) (wrap-in-bounds-check #'(= (* ((+ testA @I))) (* ((+ testB @I)))))))
        (newline))))
