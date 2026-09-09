;;
;; testing the gauche.ffi :aot subsystem
;;

(use gauche.test)
(use gauche.process)
(use file.util)

(test-start "FFI :aot")

(use gauche.ffi)
(use gauche.native-type)

(test-section "FFI :aot subsystem")

(test* "ffi-subsystem-available? :aot" #t
       (ffi-subsystem-available? :aot))

(define-module ffi-aot-sandbox
  (use gauche.ffi)
  (use gauche.native-type))

(test* ":aot outside precomp is rejected"
       (test-error <error> #/only be used in a source that is precompiled/)
       (eval '(with-ffi #f (:subsystem :aot)
                (define-c-function F-i '() 'int))
             (find-module 'ffi-aot-sandbox)))

;; precompile a module that uses :aot, compile and link it, then
;; load it and call in.  The child that loads the result runs in this
;; directory, so that the (dlopen "./f") in the generated module finds the
;; test library; everything generated goes under *aot-workdir*.
;;
;; The generated DSO is loaded into a child gosh, to avoid loaded DSO
;; stays in the test process.

(define *aot-workdir* "test-aot.o")

(define *top-srcdir*
  (sys-normalize-pathname (or (sys-getenv "top_srcdir") "../..")
                          :absolute #t :canonicalize #t))

(define *top-builddir*
  (sys-normalize-pathname (or (sys-getenv "top_builddir") "../..")
                          :absolute #t :canonicalize #t))

(define (fix-path path)
  (cond-expand
   [gauche.os.windows (regexp-replace-all #/\\/ path "/")]
   [else              path]))

(define *aot-gosh*    (fix-path (build-path *top-builddir* "src/gosh")))
(define *aot-precomp* (fix-path (build-path *top-srcdir* "lib/tools/precomp")))
(define *aot-package* (fix-path (build-path *top-srcdir*
                                            "src/gauche-package.in")))

;; Run CMD in the work directory.  Returns #t on success, or the combined
;; output on failure, so a failing test shows what the toolchain said.
(define (aot-run-in-workdir cmd)
  (let* ([p (run-process cmd :directory *aot-workdir*
                         :redirects '((>& 2 1) (> 1 out)))]
         [out (port->string (process-output p 'out))])
    (process-wait p)
    (or (zero? (process-exit-status p)) out)))

(define (aot-write-source! modname forms)
  (make-directory* *aot-workdir*)
  (with-output-to-file (build-path *aot-workdir* #"~|modname|.scm")
    (^[] (for-each (^f (write f) (newline)) forms))))

(define (aot-precompile! modname)
  (aot-run-in-workdir `(,*aot-gosh* "-ftest" ,*aot-precomp*
                        "-e" "-P" "-o" ,#"~|modname|_c" ,#"~|modname|.scm")))

(define (aot-compile! modname)
  (aot-run-in-workdir `(,*aot-gosh* "-ftest" ,*aot-package* "compile"
                        ,#"~|modname|_c" ,#"~|modname|_c.c")))

;; Load the built module in a child gosh and read back the value of EXPR.
(define (aot-load-and-eval modname expr)
  (with-output-to-file (build-path *aot-workdir* "t.scm")
    (^[]
      (write `(add-load-path ,*aot-workdir*))
      (write `(load ,#"./~|*aot-workdir*|/~|modname|.sci"))
      (write `(write ,expr))
      (write '(exit 0))))
  ;; Read everything the child says---stderr merged in, so a chatty child
  ;; can't fill a pipe nobody reads.  We decide whether it worked by the exit
  ;; status, not by whether the output parses: an error message happens to
  ;; start with a readable token, and reporting that instead of the message
  ;; would make a failure here very hard to read.
  (let* ([p (run-process `(,*aot-gosh* "-ftest"
                           ,(build-path *aot-workdir* "t.scm"))
                         :redirects '((>& 2 1) (> 1 out)))]
         [out (port->string (process-output p 'out))])
    (process-wait p)
    (if (zero? (process-exit-status p))
      (guard (e [else out]) (read-from-string out))
      out)))

(define (test-aot-module name forms expected expr)
  (aot-write-source! name forms)
  (test* #"precompiling ~name with :aot" #t (aot-precompile! name))
  (test* #"compiling ~name" #t (aot-compile! name))
  (test* #"running ~name" expected (aot-load-and-eval name expr)))

;; The basics, including a typespec that goes through a define-type of the
;; same file---precomp has to resolve it for the macro expander.
(test-aot-module
 "aotbasic"
 '((define-module aotbasic
     (use gauche.ffi)
     (use gauche.native-type)
     (export probe))
   (select-module aotbasic)
   (define-type myint (native-type 'int))
   (with-ffi (dlopen "./f") (:subsystem :aot)
     (define-c-function F-i '() 'int)
     (define-c-function Fi-i `(,myint) 'int)
     (define-c-function Fd-d '(double) 'double))
   (define (probe) (list (F-i) (Fi-i 41) (Fd-d 2.5))))
 '(42 42 5.0)
 '((with-module aotbasic probe)))

;; Callbacks, a variadic call with float arguments (which builds a sub-stub at
;; call time), constants and an enum---and two with-ffi forms in one file that
;; declare the same C function, which the generated names have to keep apart.
(test-aot-module
 "aotfull"
 '((define-module aotfull
     (use gauche.ffi)
     (use gauche.native-type)
     (export probe))
   (select-module aotfull)
   (with-ffi (dlopen "./f") (:subsystem :aot)
     (define-c-function F-i '() 'int)
     (define-c-function Fdvar '(int ...) 'double)
     (define-c-function Fcb2-i '(void* int int) 'int)
     (define-c-callback cb-add ((x 'int) (y 'int)) 'int
       (+ x y)))
   (with-ffi (dlopen "./f") (:subsystem :aot
                             :c-headers ("limits.h" "stdio.h"))
     (define-c-function F-i '() 'int)    ;same C name as above
     (define-c-constant CHAR-BIT)
     (define-c-enum seek-whence (SEEK-SET SEEK-CUR SEEK-END)))
   (define (probe)
     (list (F-i)
           (Fdvar 3 1.5 2.5 3.5)
           (Fcb2-i cb-add 20 22)
           CHAR-BIT
           SEEK-END
           (c-enum-symbol seek-whence SEEK-END))))
 '(42 7.5 42 8 2 SEEK-END)
 '((with-module aotfull probe)))

(test-end)
