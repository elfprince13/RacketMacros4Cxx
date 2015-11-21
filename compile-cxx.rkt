#!/usr/bin/env racket
#lang racket


(define verbose-mode (make-parameter #f))
(define bindir (make-parameter #f))
(define clang-flags (make-parameter null))
(define skeleton-search-paths (make-parameter null))
(define skeleton-pairs (make-parameter null))
(define config-file (make-parameter #f))


(define file-to-compile
  (command-line
   #:once-each
   [("-v" "--verbose") "Compile with verbose messages"
                       (verbose-mode #t)]
   [("-b" "--bin-dir") dir
                       "The clang bin directory"
                       (bindir dir)]
   #:multi
   [("-S" "--skeleton-search-dir") dir
                                   "Add a directory for skeleton search"
                                   (skeleton-search-paths (cons dir (skeleton-search-paths)))]
   [("-@" "--skeleton-pair") sym file
                             "Bind a skeleton lookup to a filename"
                             (skeleton-pairs (cons (cons sym file) (skeleton-pairs)))]
   [("-c" "--clang-flag") cf
                          "Add a flag for clang to use at compile time"
                          (skeleton-pairs)]
   #:args (cfg src)
   (config-file cfg)
   src))



(define-values (clang-path clang-fmt-path)
  (let*
      ([env (current-environment-variables)]
       [PATH (string->bytes/locale "PATH")]
       [old-path (environment-variables-ref env PATH)]
       [new-path 
        (if (bindir)
            (string->bytes/locale (bindir))
            old-path)])
    (environment-variables-set! env PATH new-path)
    (let
        ([clang-path (find-executable-path "clang")]
         [clang-fmt-path (find-executable-path "clang-format")])
      (environment-variables-set! env PATH old-path)
      (values clang-path clang-fmt-path))))

(display "launching") (display clang-path) (newline)

(define sexp-src-raw
  (let*-values
      ([(stdout) #f #;(open-output-string "stdout-clang-expand")]
       [(stdin) (open-input-file file-to-compile)]
       [(stderr) #f #;(open-output-string "stderr-clang-expand")]
       [(command-line) (list "-cc1" "-ast-print" "-fdiagnostics-show-option" "-fcolor-diagnostics" "-x" "skeletons")] ; (string-join (reverse ())) need you later
       [(sexp-process
         stdout stdin stderr) ; safe to discard results because we supplied our own ports
        (apply subprocess stdout stdin stderr clang-path command-line)]
       [(exit-status)
        (begin 
          (subprocess-wait sexp-process)
          (subprocess-status sexp-process))]) 
    
    (if (eq? 0 exit-status)
        (display (port->string stdout))
        (display (port->string stderr) (current-error-port)))
    (port->string stdout)))
(display "////////////////") (newline)
(display sexp-src-raw) (newline)