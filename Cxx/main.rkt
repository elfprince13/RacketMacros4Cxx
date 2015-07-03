#lang racket

(require "Expander.rkt" 
         "Writer.rkt")

(provide (except-out  (all-from-out "Expander.rkt") module-begin top-interaction)
         (rename-out [module-begin #%module-begin] [top-interaction #%top-interaction]))

; Note: we don't handle [] array syntax yet, because [] are ()
; Follow-up: check 'paren-shape :)
#;(let ((expanded-code 
       (expand-decl
        #'(defun (__global__) void kernelTest ((() int argc) (() char** argv)) 
            (begin
              (@ Loop1d (test-loop) ((i) (0) (argc)) 
                 (call printf "%s\\n" (* ((+ argv (@ I () () ()))))))
              (call printf "done\\n"))) init-skeletons-table)))
  (begin
    (print expanded-code)
    (newline)
    (display (make-cpp-decl expanded-code))))

(display (make-cpp-decl #'(defun (__global__) void kernelTest ((() int argc) (() char** argv)) 
            (block
              (for ((() int i = 0) (0) (argc))
                (call printf "done\\n"))))))