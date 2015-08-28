#lang Cxx
; Idea: keep our own identifier dictionary when walking for Writer
; and use Source Location information to check which identifiers are original and which are introduced
; Writer probably wants to use pretty print

(translation-unit

#;(let* ((tmp 1) (j 2)) 
  (swap tmp j) 
  (print tmp)
  (dummy)
  (print j))

#;(let* ((tmp 1) (j tmp)) 
  (swap tmp j) 
  (print tmp) 
  (print j))

#;(let* ((i 1) (j 1) (tmp3 (swap i j)))
  (swap i j)
  (let* ((tmp 1)) 
    (print tmp)
    (print tmp3))
  (swap i j))

(typedef int foobar)

(() struct fun 
    {  
     (() int a)
     (() int b)
     (() int c)})

(def (() int t1 = 0) (t2 = t1))

(defun () void kernelTest ((() int argc) (() char** argv)) __attribute__((__global__))
  (block
   (def (() int j) (k = 0))
   (while (!= j k)
     (call printf "done\\n"))
   (for ((() int i = 0) (0) (argc))
     (call printf "done\\n")))))
