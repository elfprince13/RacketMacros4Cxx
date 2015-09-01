#lang Cxx
; Idea: keep our own identifier dictionary when walking for Writer
; and use Source Location information to check which identifiers are original and which are introduced
; Writer probably wants to use pretty print

(translation-unit
 
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
    (def (() (char (!) [argc]) data))
    (if (== ((% argc 4)) 0) (block
        (@ Loop2dAccumulate (test_loop) (
            [@ I]
            [@ J]
            [= 0]
            [= argc]
            [= 0]
            [= 100]
          )
            (block
                (def (() int i))
                (@ I () (
                    [= i]
                  )
                    ()
                )
                (data[i] ++)
            )
        )
        (return 0)    ) else (block
        (return (% argc 4))    ))
)

)

)