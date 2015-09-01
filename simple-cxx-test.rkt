#lang Cxx
; Idea: keep our own identifier dictionary when walking for Writer
; and use Source Location information to check which identifiers are original and which are introduced
; Writer probably wants to use pretty print

#;(translation-unit
 
 #;(def (() union {
    (() (char (!) [128]) __mbstate8)
    (() (long long (!)) _mbstateL)
    }) (typedef () ((!)) __mbstate_t))
 
 (typedef () (const int * (* (!))([5])) foobar)
 
 (() struct fun 
     {  
      (() (int (!)) a)
      (() (int (!)) b)
      (() (int (!)) c)})
 
 (def (() (int (!)) t1 = 0) ((* (!)) t2 = (& t1)))
 (def (() (int (!)) t3(t2)))
 (def (() (int (!)) t4 = t2))
 (def (() (int (!)) t5))
 
 (defun () (void (!)) kernelTest ((() (int (!)) argc) (() (char * * (!)) argv)) __attribute__((__global__))
   (block
    (def (() (int (!)) j) (((!)) k = 0))
    (while (!= j k)
           (call printf "done\\n"))
    (for ((def (() (int (!)) i = 0)) (< i argc) (i ++))
      (call printf "done\\n")))))

(translation-unit

(defun () (int (!)) main () (block
    (def (() (int (!)) argc = 100))
    (def (() (char * (!)) data = (call malloc argc)))
    (if (== ((% argc 4)) 0) (block
        (@ Loop1d (test_loop) ([@ I][= 0][= argc])
            (block
                (def (() (int (!)) i))
                (@ I (test_itr) ([= i])())
                ((* ((+ data i))) ++)
            )
        )
        (return 0)    ) else (block
        (return (% argc 4))    )))))