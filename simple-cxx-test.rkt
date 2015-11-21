#lang Cxx
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
 (skeletons:
  (SillyThunk "./SkelImpls/SillyThunk.rkt")
  "./test-params.json")
 (defun (extern ) (int (!)) puts ((() (const char * (!)) str)))
 (@ SillyThunk (silly) ([@ CallSilly] [@ I])
    (block 
     (def (() (char (!)) oc))
     (@ I (get_char) ([= oc]) ())
     (call puts (& oc))))
 (defun () (int (!)) main ((() (int (!)) argc) (() (char * * (!)) argv))
   (block 
    (@ CallSilly (silly1) () ())
    (@ CallSilly (silly2) () ()))))

(translation-unit
 (skeletons:
  (Repeat "./SkelImpls/LexicalUnroll.rkt")
  "./test-params.json")
 (defun () (void (!)) reduce_warp (((volatile) (int * (!)) sdata) (() (int (!)) tid))  __attribute__((__device__))
   (block 
    (def (() (int (!)) selm = (+ sdata tid)))
    (@ Repeat (reduce_warp) ([@ I] [= 6])
       (block
        (def (() (int (!)) i))
        (@ I (reduce_idx) ([= i]) ())
        (= i (<< 1 ((- 5 i))))
        (= (* selm) (* ((+ selm i)))))))))

(translation-unit
 (skeletons:
  (Loop1d "./SkelImpls/Loop1d.rkt")
  "./test-params.json")
 (defun (extern ) (void * (!)) malloc ((() (unsigned long (!)) size)))
 (defun (extern ) (void (!)) free ((() (void * (!)) data)))
 (defun (extern ) (int (!)) puts ((() (const char * (!)) str)))
 (defun (extern ) (unsigned long (!)) strlen ((() (const char * (!)) str)))
 (defun () (int (!)) main ((() (int (!)) argc) (() (char * * (!)) argv)) 
   (block
    (def (() (int (!)) ret))
    (if (== ((% argc 4)) 0) 
        (block
         (@ Loop1d (test_loop) ([@ I] [= (+ 0 0)] [= (+ 0 argc)])
            (block
             (def (() (int (!)) j1))
             (@ Loop1d (test_loop) ([@ J] [= 0] [= argc])
                (block
                 (def (() (int (!)) j))
                 (@ J (test_iterator) ([= j]) ())
                 (@ I (test_iterator) ([= j1]) ())
                 (if (> (call strlen (* ((+ argv j1)))) 0)
                     (>++ ((* ((+ argv j1))))))
                 (call puts (* ((+ argv j1))))
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
 (skeletons:
  (Loop1d "./SkelImpls/Loop1d.rkt")
  "./test-params.json")
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

(translation-unit
 (def (() (int (!)) i))
 (def (() (int (!)) j = i) (((!)) k = j))
 (defun () (int (!)) main ((() (int (!)) argc) (() (char * * (!)) argv)) 
   (block
    (def (() (int (!)) m))
    (>++ m)
    (>++ j)
    (= j (c-cast (int (!)) argc))
    (= i (reinterpret_cast (int (!)) argc))
    (call main))))

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
