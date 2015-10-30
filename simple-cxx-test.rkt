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

#;(translation-unit

(defun (extern ) (void * (!)) malloc ((() (unsigned long (!)) size)))
(defun (extern ) (void (!)) free ((() (void * (!)) data)))
(defun (extern ) (int (!)) puts ((() (const char * (!)) str)))
(defun (extern ) (unsigned long (!)) strlen ((() (const char * (!)) str)))
(defun () (int (!)) main ((() (int (!)) argc) (() (char * * (!)) argv)) (block
    (def (() (int (!)) ret))
    (if (== ((% argc 4)) 0) (block
        (@ Loop1d (test_loop) (
            [@ I]
            [= 0]
            [= argc]
          )
            (block
                (def (() (int (!)) i))
                (@ I (test_iterator) (
                    [= i]
                  )
                    ()
                )
                (if (> (call strlen (* ((+ argv i)))) 0)
                    (((* ((+ argv i)))) ++)
)
                (call puts (* ((+ argv i))))
            )
        )
        (def (() (const char (!)) nl = #\u0a))
        (call puts (& nl))
        (= ret 0)
    ) else (block
        (= ret (% argc 4))
    ))
    (return ret))

)

)

(translation-unit

(defun (extern ) (void * (!)) malloc ((() (unsigned long (!)) size)))
(defun (extern ) (void (!)) free ((() (void * (!)) data)))
(defun (extern ) (int (!)) puts ((() (const char * (!)) str)))
(defun (extern ) (unsigned long (!)) strlen ((() (const char * (!)) str)))
(defun () (int (!)) main ((() (int (!)) argc) (() (char * * (!)) argv)) (block
    (def (() (int (!)) ret))
    (if (== ((% argc 4)) 0) (block
        (for ((def (() (int (!)) j = 0)) (< j argc) (j ++))
            (block
                (def (() (int (!)) i))
                (= i j)
                (if (> (call strlen (* ((+ argv i)))) 0)
                    (((* ((+ argv i)))) ++)
)
                (call puts (* ((+ argv i))))
                
                #;(block
        (for ((def (() (int (!)) j = 0)) (< j argc) (j ++))
            (block
                (def (() (int (!)) i))
                (= i j)
                (if (> (call strlen (* ((+ argv i)))) 0)
                    (((* ((+ argv i)))) ++)
)
                (call puts (* ((+ argv i))))
                
                
            )
        )
        (def (() (const char (!)) nl = #\u0a))
        (call puts (& nl))
        (= ret 0)
    )
                
            )
        )
        (def (() (const char (!)) nl = #\u0a))
        (call puts (& nl))
        (= ret 0)
    ) else (block
        (= ret (% argc 4))
    ))
    (return ret))

)
)