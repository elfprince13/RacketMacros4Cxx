#!/usr/bin/env racket
#lang racket


(define verbose-mode (make-parameter #f))
(define bindir (make-parameter #f))
(define clang-flags (make-parameter null))
(define skeleton-search-paths (make-parameter null))
(define header-search-paths (make-parameter null))
(define skeleton-pairs (make-parameter null))
(define config-file (make-parameter #f))
(define delete-temps (make-parameter #t))
(define lang-ext (make-parameter "c"))

(define make-absolute
  (lambda (path)
    (if (relative-path? path)
        (normalize-path (build-path (current-directory) path))
        path)))

(define file-to-compile
  (command-line
   #:once-each
   [("-v" "--verbose") "Compile with verbose messages"
                       (verbose-mode #t)]
   [("-b" "--bin-dir") dir
                       "The clang bin directory"
                       (bindir dir)]
   [("-t" "--preserve-temps")
                       "Temporary files are not deleted"
                       (delete-temps #f)]
   
   #:once-any
   [("-c" "--dot-c") "Sets the file extension of expanded source to .c"
                     (lang-ext "c")]
   [("-u" "--dot-cuda") "Sets the file extension of expanded source to .cu"
                        (display "cu") (newline)
                     (lang-ext "cu")]
   [("-p" "--dot-cplusplus") "Sets the file extension of expanded source to .cpp"
                     (lang-ext "cpp")]
   [("-x" "--dot-x") x
                     "Sets the file extension of expanded source to a user supplied value"
                     (lang-ext x)]
   
   #:multi
   [("-S" "--skeleton-search-dir") dir
                                   "Add a directory for skeleton search"
                                   (skeleton-search-paths (cons dir (skeleton-search-paths)))]
   [("-@" "--skeleton-pair") sym file
                             "Bind a skeleton lookup to a filename"
                             (skeleton-pairs (cons (cons sym file) (skeleton-pairs)))]
   [("-f" "--clang-flag") cf
                          "Add a flag for clang to use at compile time"
                          (clang-flags (cons cf (clang-flags)))]
   [("-I" "--include-search-dir") isd
                          "Add an include search dir. Also added to the compile time flags"
                          (let 
                              ([flag (string-append "-I" isd)])
                            (header-search-paths (cons flag (header-search-paths)))
                            (clang-flags (cons flag (clang-flags))))]
   #:args (cfg src)
   (config-file (path->string (make-absolute cfg)))
   (path->string
    (make-absolute src))))




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

(define sexp-src-raw-port
  (let*-values
      ([(stdout stdin stderr) 
        (values #f (open-input-file file-to-compile) #f)]
       [(command-line) 
        (list* "-cc1" "-ast-sexp" "-fdiagnostics-show-option" "-fcolor-diagnostics" "-x" "skeletons" (reverse (header-search-paths)))] ; (string-join (reverse ())) need you later
       [(sexp-process
         stdout _ stderr) ; safe to discard results of stdin because we supplied our own ports
        (apply subprocess stdout stdin stderr clang-path command-line)]
       [(exit-status error-text)
        (begin 
          (subprocess-wait sexp-process)
          (values (subprocess-status sexp-process) (string-trim (port->string stderr))))])
    (close-input-port stderr)
    (close-input-port stdin)
    
    (unless (equal? error-text "")
      (display error-text (current-error-port))
      (newline (current-error-port)))
    (unless (eq? 0 exit-status)
      (exit exit-status))
    stdout))


(define full-pairs
  (map 
   (lambda (sk-pair)
     (let*-values 
         ([(sym file) (values (car sk-pair) (cdr sk-pair))]
          [(full-pair)
           (let loop
             ([work (skeleton-search-paths)])
             (if (null? work)
                 #f
                 (let*-values 
                     ([(head rest) (values (car work) (cdr work))]
                      [(full-path) (build-path head file)])
                   (if (file-exists? full-path)
                       (cons sym 
                             (if (relative-path? full-path)
                                 (build-path (current-directory) full-path)
                                 full-path))
                       (loop rest)))))])
       (if full-pair
           full-pair
           (begin
             (display (~a (list "Unable to find requested file" file "in paths:\n" (skeleton-search-paths))) (current-error-port))
             (newline (current-error-port))
             (exit 1)))))
   (skeleton-pairs)))

(define-values (template-header-port template-footer-port)
  (apply 
   values
   (map open-input-string
        (list
         (string-append 
          "#lang Cxx\n" 
          "(translation-unit\n(skeletons:\n"
          (string-join 
           (map
            (lambda (pair)
              (string-append 
               "("
               (car pair)
               " \""
               (path->string (cdr pair))
               "\")\n"))
            full-pairs))
          "\"" (config-file) "\")\n")
         ")"))))

(display "Constructed S-Exp") (newline)
(let* ([syntax-port (input-port-append #t template-header-port sexp-src-raw-port template-footer-port)]
       [tmp-file (make-temporary-file (string-append file-to-compile "~a.rkt"))]
       [sexp-contents
        (call-with-output-file tmp-file
          (lambda (out)
            (let ([copy-string (open-output-string "SExp Source")])
              (copy-port syntax-port out copy-string)
              (get-output-string copy-string)))
          #:exists 'replace)])
  (define expanded-src-port
    (let*-values 
        ([(stdout stdin stderr) (values #f #f #f)]
         [(racket-path command-line) 
          (values (find-executable-path "racket");(apply build-path (map find-system-path '(orig-dir exec-file)))
                  (list tmp-file))]
         [(sexp-process
           stdout stdin stderr)
          (apply subprocess stdout stdin stderr racket-path command-line)]
         [(exit-status error-text)
          (begin 
            (subprocess-wait sexp-process)
            (values (subprocess-status sexp-process) (string-trim (port->string stderr))))])
      (close-input-port stderr)
      (close-output-port stdin)
      (when (delete-temps)
        (delete-file tmp-file))
      
      (unless (equal? error-text "")
        (display error-text (current-error-port))
        (newline (current-error-port)))
      (unless (eq? 0 exit-status)
        (display "
Failed to expand the following program:
=======================================\n" 
                 (current-error-port))
        (display sexp-contents (current-error-port))
        (newline (current-error-port))
        (exit exit-status))
      stdout))
  
  (define pretty-src-port
      (let*-values 
          ([(stdout stdin stderr) (values #f expanded-src-port #f)]
           [(command-line) null]
           [(sexp-process
             stdout _ stderr)
            (apply subprocess stdout stdin stderr clang-fmt-path command-line)]
           [(exit-status error-text)
            (begin 
              (subprocess-wait sexp-process)
              (values (subprocess-status sexp-process) (string-trim (port->string stderr))))])
        (close-input-port stderr)
        (close-input-port stdin)
        
        (unless (equal? error-text "")
          (display error-text (current-error-port))
          (newline (current-error-port)))
        (unless (eq? 0 exit-status)
          (exit exit-status))
        stdout))
  (define clang-finished-port
      (let*-values 
          ([(tmp-file) (make-temporary-file (string-append file-to-compile "~a." (lang-ext)))]
           [(pretty-contents) 
            (call-with-output-file tmp-file
              (lambda (out)
                (let ([copy-string (open-output-string "Pretty Source")])
                  (copy-port pretty-src-port out copy-string)
                  (get-output-string copy-string)))
              #:exists 'replace)]
           [(stdout stdin stderr) (values #f #f #f)]
           [(command-line) (reverse (cons tmp-file (clang-flags)))]
           [(sexp-process
             stdout stdin stderr)
            (begin
              (display clang-path) (display " ") (display (~a command-line)) (newline)
            (apply subprocess stdout stdin stderr clang-path command-line))]
           [(exit-status error-text)
            (begin 
              (subprocess-wait sexp-process)
              (values (subprocess-status sexp-process) (string-trim (port->string stderr))))])
        (close-input-port stderr)
        (close-output-port stdin)
        (when (delete-temps)
          (delete-file tmp-file))
        
        (unless (equal? error-text "")
          (display error-text (current-error-port))
          (newline (current-error-port)))
        (unless (eq? 0 exit-status)
          (display "
Failed to compile the following program:
=======================================\n" 
                 (current-error-port))
        (display pretty-contents (current-error-port))
        (newline (current-error-port))
          (exit exit-status))
        stdout))
  (display (port->string clang-finished-port))
  (newline)
  (close-input-port clang-finished-port))

(display "Done")
(newline)

