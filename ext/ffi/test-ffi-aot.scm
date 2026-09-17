;;
;; testing the gauche.ffi :aot subsystem
;;

(use gauche.test)
(use gauche.process)
(use gauche.config)
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

;; The headers the :aot sources include live next to this file.  :aot
;; ignores :c-include-paths by design, so the include path has to reach the
;; child compiler through its flags.
(define *aot-c-dir*
  (fix-path (sys-normalize-pathname
             (build-path (sys-dirname (current-load-path)) "c")
             :absolute #t :canonicalize #t)))

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
                        ,#"--cppflags=-I~|*aot-c-dir*|"
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
;; The tail of the result is foreign-function-info: the tags are handed to
;; the setup procedure at load time, not baked into the generated C, so the
;; :dlobj entry has to name the library this run dlopen'd.
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
   (define (probe)
     (let1 info (foreign-function-info Fi-i)
       (list (F-i) (Fi-i 41) (Fd-d 2.5)
             (get-keyword :subsystem info #f)
             (get-keyword :dlobj info #f)
             (get-keyword :argtypes info #f)
             (get-keyword :rettype info #f)))))
 `(42 42 5.0 :aot ,#"./f.~(gauche-config \"--so-suffix\")" (int) int)
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

;; An enum declared in one with-ffi form, used as a type in a later one.
;; The name has to resolve at macro-expansion time, when the C code for the
;; second form is generated---long before the enumerator values exist.
(test-aot-module
 "aotacross"
 `((define-module aotacross
     (use gauche.ffi)
     (use gauche.native-type)
     (export probe))
   (select-module aotacross)
   (with-ffi #f (:subsystem :aot :c-headers ("ffi-const.h"))
     (define-c-enum (ffi_test_color_t ffi_test_color)
       (FFI_TEST_RED FFI_TEST_GREEN FFI_TEST_BLUE)))
   (with-ffi (dlopen "./f") (:subsystem :aot)
     (define-c-function Fi_i `(,ffi_test_color_t) ffi_test_color_t))
   (define (probe)
     (list (Fi_i FFI_TEST_RED)
           (c-enum-type-tag ffi_test_color_t)
           (c-enum-value ffi_test_color_t 'FFI_TEST_BLUE))))
 '(1 ffi_test_color 2)
 '((with-module aotacross probe)))

;; The same, within a single with-ffi form: the enum and the function that
;; uses it are declared side by side.  The type has to exist before either
;; the C code or the runtime cdef instances are built.
(test-aot-module
 "aotintra"
 `((define-module aotintra
     (use gauche.ffi)
     (use gauche.native-type)
     (export probe))
   (select-module aotintra)
   (with-ffi (dlopen "./f") (:subsystem :aot :c-headers ("ffi-const.h"))
     (define-c-enum (ffi_test_color_t ffi_test_color)
       (FFI_TEST_RED FFI_TEST_GREEN FFI_TEST_BLUE))
     (define-c-function Fi_i `(,ffi_test_color_t) ffi_test_color_t))
   (define (probe)
     (list (Fi_i FFI_TEST_GREEN)
           (c-enum-symbol ffi_test_color_t 2)
           ;; The tag info is a snapshot taken when the cdef instances are
           ;; built, which for an enum declared in this same form is before
           ;; ffisetup has told us the enumerators---so the signature names
           ;; the enum and its representation, but lists no enumerators.
           ;; (In aotacross above, where the enum comes from an earlier
           ;; form, it is complete by then and they are all listed.)
           (get-keyword :rettype (foreign-function-info Fi_i) #f))))
 '(2 FFI_TEST_BLUE (.enum ffi_test_color : uint32_t ()))
 '((with-module aotintra probe)))

;; An enum with negative enumerators needs an explicit base type, since the
;; representation is settled before the values are known.
(test-aot-module
 "aotsigned"
 `((define-module aotsigned
     (use gauche.ffi)
     (use gauche.native-type)
     (export probe))
   (select-module aotsigned)
   (with-ffi (dlopen "./f") (:subsystem :aot :c-headers ("ffi-const.h"))
     (define-c-enum (ffi_test_signed_t ffi_test_signed)
       (FFI_TEST_S_LO FFI_TEST_S_HI) 'int16_t)
     (define-c-function Fi_i `(,ffi_test_signed_t) ffi_test_signed_t))
   (define (probe)
     (list (Fi_i FFI_TEST_S_LO) FFI_TEST_S_LO
           (~ ffi_test_signed_t'size))))
 '(-4 -5 2)
 '((with-module aotsigned probe)))

(test-end)
