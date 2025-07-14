;; Check if build-standalone works.
;;
;; This test should be run against an _installed_ Gauche.  So we don't
;; include this in 'make -s check', but run in 'DIST self-host-test'.

(use gauche.test)
(use gauche.process)
(use gauche.config)
(use file.util)

(test-start "build-standalone (installed)")

(test-section "build-standalone")

(call-with-temporary-directory
 (^[dir]
   (define source (build-path dir "hello.scm"))
   (define executable
     (string-append (path-sans-extension source)
                    (gauche-config "--executable-suffix")))
   (with-output-to-file source
     (^[] (write '(define (main args)
                    (format #t "Hello, ~a\n" (cadr args))
                    0))))
   (test* "Build" #t
          (and (do-process `(gosh tools/build-standalone -o ,executable ,source))
               (file-is-executable? executable)))
   (test* "Run" "Hello, Gauche"
          (process-output->string `(,executable "Gauche")))

   ))

(test-section "compile-r7rs")

(call-with-temporary-directory
 (^[dir]
   (define source (build-path dir "hello.scm"))
   (define executable
     (string-append (path-sans-extension source)
                    (gauche-config "--executable-suffix")))
   (with-output-to-file source
     (^[]
       (write '(import (scheme base) (scheme write) (scheme process-context)))
       (write '(display "Hello, "))
       (write '(display (cadr (command-line))))
       (write '(newline))))
   (test* "Build" #t
          (and (do-process `(gauche-compile-r7rs -o ,executable ,source))
               (file-is-executable? executable)))
   (test* "Run" "Hello, Droit"
          (process-output->string `(,executable "Droit")))
   ))

(test-end)
