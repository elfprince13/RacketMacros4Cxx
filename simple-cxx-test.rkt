#lang Cxx
; Idea: keep our own identifier dictionary when walking for Writer
; and use Source Location information to check which identifiers are original and which are introduced
; Writer probably wants to use pretty print

;;;;;;;;;;;;;;;;;;
; Cuda here
;;;;;;;;;;;;;;;;;;

#;(translation-unit
 
 #;(def 
     (() union 
         {
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
    (for ((def (() (int (!)) i = 0)) (< i argc) (>++ i))
      (call printf "done\\n")))))

;;;;;;;;;;;;;;;;;;;;;;;;;
; Simple skeletons here
;;;;;;;;;;;;;;;;;;;;;;;;;

(translation-unit
 (defun (extern ) (void * (!)) malloc ((() (unsigned long (!)) size)))
 (defun (extern ) (void (!)) free ((() (void * (!)) data)))
 (defun (extern ) (int (!)) puts ((() (const char * (!)) str)))
 (defun (extern ) (unsigned long (!)) strlen ((() (const char * (!)) str)))
 (defun () (int (!)) main ((() (int (!)) argc) (() (char * * (!)) argv)) 
   (block
    (def (() (int (!)) ret))
    (if (== ((% argc 4)) 0) 
        (block
         (@ Loop1d (test_loop) ([@ I] [= 0] [= argc])
            (block
             (def (() (int (!)) j1))
             (@ I (test_iterator) ([= j1]) ())
             (if (> (call strlen (* ((+ argv j1)))) 0)
                 (>++ ((* ((+ argv j1))))))
             (call puts (* ((+ argv j))))
             (@ Loop1d (test_loop) ([@ J] [= 0] [= argc])
                (block
                 (def (() (int (!)) j))
                 (@ J (test_iterator) ([= j]) ())
                 (if (> (call strlen (* ((+ argv j)))) 0)
                     (>++ ((* ((+ argv j))))))
                 (call puts (* ((+ argv j))))))))
         (def (() (const char (!)) nl = #\u0a))
         (call puts (& nl))
         (= ret 0)) 
        else 
        (block
         (= ret (% argc 4))))
    (return ret))))

(translation-unit
 (defun (extern ) (void * (!)) malloc ((() (unsigned long (!)) size)))
 (defun (extern ) (void (!)) free ((() (void * (!)) data)))
 (defun (extern ) (int (!)) puts ((() (const char * (!)) str)))
 (defun (extern ) (unsigned long (!)) strlen ((() (const char * (!)) str)))
 (defun () (int (!)) main ((() (int (!)) argc) (() (char * * (!)) argv)) 
   (block
    ; These two should generate equivalent loop bodies for now
    (for ((def (() (int (!)) j = 0)) (< j argc) (>++ j))
      (block
       (def (() (int (!)) i))
       (= i j)
       (if (> (call strlen (* ((+ argv i)))) 0)
           (>++ ((* ((+ argv i))))))
       (call puts (* ((+ argv i))))))
    (@ Loop1d (test_loop) ([@ I] [= 0] [= argc])
       (block
        (def (() (int (!)) j1))
        (@ I (test_iterator) ([= j1]) ())
        (if (> (call strlen (* ((+ argv j1)))) 0)
            (>++ ((* ((+ argv j1))))))
        (call puts (* ((+ argv j1))))))
    
    (return 0))))


;;;;;;;;;;;;;;;;;;;;;
; No skeletons here
;;;;;;;;;;;;;;;;;;;;;

#;(translation-unit
 (def (() (int (!)) i))
 (def (() (int (!)) j = i)))

#;(translation-unit

 (defun (extern ) (void * (!)) malloc ((() (unsigned long (!)) size)))
 (defun (extern ) (void (!)) free ((() (void * (!)) data)))
 (defun (extern ) (int (!)) puts ((() (const char * (!)) str)))
 (defun (extern ) (unsigned long (!)) strlen ((() (const char * (!)) str)))
 (defun () (int (!)) main ((() (int (!)) argc) (() (char * * (!)) argv)) 
   (block
    (def (() (int (!)) ret))
    (if (== ((% argc 4)) 0) 
        (block
         (for ((def (() (int (!)) j = 0)) (< j argc) (>++ j))
           (block
            (def (() (int (!)) i))
            (= i j)
            (if (> (call strlen (* ((+ argv i)))) 0)
                (>++ ((* ((+ argv i))))))
            (call puts (* ((+ argv i))))
            
            (block
             (for ((def (() (int (!)) j = 0)) (< j argc) (>++ j))
               (block
                (def (() (int (!)) i))
                (= i j)
                (if (> (call strlen (* ((+ argv i)))) 0)
                    (>++ ((* ((+ argv i))))))
                (call puts (* ((+ argv i))))))
             (def (() (const char (!)) nl = #\u0a))
             (call puts (& nl))
             (= ret 0))))
         (def (() (const char (!)) nl = #\u0a))
         (call puts (& nl))
         (= ret 0)) 
        else 
        (block
         (= ret (% argc 4))))
    (return ret))))

#;(translation-unit
 (defun () (void (!)) voidrettest ()
   (block 
    (return)))
 (defun () (int (!)) main ((() (int (!)) argc) (() (char * * (!)) argv))
   (block 
    (def (() (int (!)) ret = 0))
    (def (() (int (!)) ret2 = 0))
    (block 
     (block
      (def (() (int (!)) ret = 0))
      (def (() (int (!)) ret2 = 0))
      (block 
       (block 
        (block 
         (def (() (int (!)) ret = 0))
         (def (() (int (!)) ret2 = 0))
         (block
          (def (() (int (!)) ret = 0))
          (def (() (int (!)) ret2 = 0))
          (block (return 0)))))))))))
