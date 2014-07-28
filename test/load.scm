;;
;; Tests for subtle effects of loading and autoloading
;;

(use gauche.test)

(test-start "load")

(add-load-path ".")

;; Some abstraction for win32... The file.util module provides
;; higher abstraction, but we're not at the stage of using it yet.
(define (P path) (sys-normalize-pathname path))

(define (rmrf . files)
  (dolist [f files]
    (cond-expand
     [gauche.os.windows
      (sys-system #"rmdir /q /s ~(P f) > NUL 2>&1")
      (sys-system #"del /q ~(P f) > NUL 2>&1")]
     [else
      (sys-system #"rm -rf ~f > /dev/null")])))

;;----------------------------------------------------------------
(test-section "require and provide")

(rmrf "test.o")
(sys-mkdir "test.o" #o777)
(with-output-to-file "test.o/a.scm"
  (^[]
    (write '(provide "test.o/a"))
    (newline)))

(test* "double require"
       #t
       (begin
         (eval '(require "test.o/a") (interaction-environment))
         (sys-unlink "test.o/a.scm")
         (eval '(require "test.o/a") (interaction-environment))
         #t))

(rmrf "test.o")
(sys-mkdir "test.o" #o777)
(with-output-to-file "test.o/b.scm"
  (^[]
    (write '(require "test.o/c"))
    (write '(provide "test.o/b"))
    (newline)))
(with-output-to-file "test.o/c.scm"
  (^[]
    (write '(require "test.o/b"))
    (write '(provide "test.o/c"))
    (newline)))

(test* "detecting loop of require"
       (test-error)
       (eval '(require "test.o/b") (interaction-environment)))

(rmrf "test.o")
(sys-mkdir "test.o" #o777)
(with-output-to-file "test.o/d.scm"
  (^[]
    (display "(define z 0)(")
    (newline)))

(test "reload after error"
      1
      (^[]
        (with-error-handler
         (^e #t)
         (^[] (eval '(require "test.o/d") (interaction-environment))))
        (with-output-to-file "test.o/d.scm"
          (^[]
            (write '(define z 1))
            (write '(provide "tset.o/d"))))
        (eval '(require "test.o/d") (interaction-environment))
        (eval 'z (interaction-environment))))

;; :environment arg -------------------------------------
(test-section "load environment")

(with-output-to-file "test.o/d.scm"
  (^[] (display "(define foo 3)")))
(define-module load.test )
(define foo 8)

(test* ":environment argument"
      3
      (begin
        (load "test.o/d" :environment (find-module 'load.test))
        (with-module load.test foo)))

;; a compicated case involving eval, load and restoration of environment.
;; this is actually testing code in Scm_VMEval, but I put it here
;; since the 'eval' test is done before i/o.
(with-output-to-file "test.o/d.scm"
  (^[] (display "(define foo 6)")))

(test* "eval & load & environment" 6
       (begin
         (eval '(load "test.o/d") (find-module 'load.test))
         (with-module load.test foo)))

;; current-load-* --------------------------------------
(test-section "current-load-* info")

(with-output-to-file "test.o/c1.scm"
  (^[]
    (write '(print (current-load-history)))
    (write '(print (current-load-next)))
    (write '(print (current-load-port)))
    (write '(print (current-load-path)))
    (write '(load "./test.o/c2.scm"))
    (write '(print (current-load-history)))
    (write '(print (current-load-next)))
    (write '(print (current-load-port)))
    (write '(print (current-load-path)))))

(with-output-to-file "test.o/c2.scm"
  (^[]
    (write '(print (current-load-history)))
    (write '(print (current-load-next)))
    (write '(print (current-load-port)))
    (write '(print (current-load-path)))))

(with-output-to-file "test.o/c.out"
  (^[] (load "./test.o/c1.scm")))

(with-input-from-file "test.o/c.out"
  (^[]
    (test* "current-load-history (1)" #/^\(\(#<iport [^>]*test\/load\.scm/
           (read-line) rxmatch)
    (test* "current-load-next (1)" "()" (read-line))
    (test* "current-load-port (1)" #/^#<iport [^>]*\/test\.o\/c1\.scm/
           (read-line) rxmatch)
    (test* "current-load-path (1)" "./test.o/c1.scm"
           (read-line) rxmatch)

    (test* "current-load-history (2)"
           #/^\(\(#<iport [^>]*test\.o\/c1\.scm [^\)]*\) \(#<iport [^>]*test\/load\.scm [^\)]*\)/
           (read-line) rxmatch)
    (test* "current-load-next (2)" "()" (read-line))
    (test* "current-load-port (2)" #/^#<iport [^>]*\/test\.o\/c2\.scm/
           (read-line) rxmatch)
    (test* "current-load-path (2)" "./test.o/c2.scm"
           (read-line) rxmatch)

    (test* "current-load-history (3)" #/^\(\(#<iport [^>]*test\/load\.scm/
           (read-line) rxmatch)
    (test* "current-load-next (3)" "()" (read-line))
    (test* "current-load-port (3)" #/^#<iport [^>]*\/test\.o\/c1\.scm/
           (read-line) rxmatch)
    (test* "current-load-path (3)" "./test.o/c1.scm"
           (read-line) rxmatch)
    ))

;; include --------------------------------------
(test-section "include")

(with-output-to-file "test.o/inc0.scm"
  (^[]
    (write '(define inc-var 4))
    (write 'inc-var)))
(with-output-to-file "test.o/inc1.scm"
  (^[]
    (write '(define inc-var 10))
    (write '(let ((x inc-var))
              (include "inc0.scm")
              (+ x inc-var)))))
(with-output-to-file "test.o/inc2.scm"
  (^[]
    (write '(define inc-var2 (let () (include "inc0"))))))

(define (include-into-toplevel filename)
  (eval `(begin (define inc-var 0)
                (include ,filename))
        (current-module)))

(test* "include (relative to current, expands into toplevel)" 4
       (include-into-toplevel "test.o/inc0"))
(test* "include (relative to current, expands inside let)" 4
       (let ((inc-var 2))
         (include "test.o/inc0")
         inc-var))
(test* "include (absolute, expands into toplevel)" 4
       (include-into-toplevel
        (sys-normalize-pathname "test.o/inc0" :absolute #t)))
(test* "include (nested)" 14
       (include-into-toplevel "test.o/inc1"))
(test* "include (postcondition)" 10
       (eval 'inc-var (current-module)))
(test* "include (within init part)" 4
       (let () (include "test.o/inc2.scm") inc-var2))

;; autoloading -----------------------------------------
(test-section "autoload")

(with-output-to-file "test.o/l0.scm"
  (^[] (write '(define foo 0))))
(autoload "test.o/l0" foo)
(test* "autoload (file)" 0 foo)

(with-output-to-file "test.o/l1.scm"
  (^[] (write '(define foo 0))))
(autoload "test.o/l1" foo1)
(test* "autoload (file/error)" (test-error) foo1)

(with-output-to-file "test.o/l0.scm"
  (^[]
    (write '(define-module foo (extend scheme)))
    (write '(load "./test.o/l1.scm" :environment (find-module 'foo)))))
(with-output-to-file "test.o/l1.scm"
  (^[] (write '(expt 2 3))))

(test* "autoload environment" #t
       (load "./test.o/l0.scm"))

(rmrf "test.o")

;; library utilities -----------------------------------

(test-section "libutil")

(sys-system "mkdir test.o")
(sys-system #"mkdir ~(P \"test.o/_test\")")
(sys-system #"mkdir ~(P \"test.o/_tset\")")

(with-output-to-file "test.o/_test.scm"
  (^[]
    (write '(define-module _test ))
    (write '(provide "_test"))))

(with-output-to-file "test.o/_test/_test.scm"
  (^[]
    (write '(define-module _test._test ))
    (write '(provide "_test/_test"))))

(with-output-to-file "test.o/_test/_test1.scm"
  (^[]
    (write '(define-module _test._test1 ))
    (write '(provide "_test/_test2"))))

(with-output-to-file "test.o/_tset/_test.scm"
  (^[]
    (write '(define-module _tset._test ))
    (write '(provide "_tset/_test"))))

(with-output-to-file "test.o/_tset/_test1"
  (^[]
    (write '(define-module dummy ))))

(with-output-to-file "test.o/_tset/_test2.scm"
  (^[]
    (write '(provide "_tset/_test2"))))

(test* "library-fold _test" `((_test . ,(P "test.o/_test.scm")))
       (library-fold '_test acons '() :paths '("./test.o")))

(test* "library-fold _test" `(("_test" . ,(P "test.o/_test.scm")))
       (library-fold "_test" acons '() :paths '("./test.o")))

(define paths-a (map P '("./test.o" "./test.o/_test" "./test.o/_tset")))
(define paths-b (map P '("./test.o/_test" "./test.o" "./test.o/_tset")))

(test* "library-fold _test (multi)" `((_test . ,(P "test.o/_test.scm")))
       (library-fold '_test acons '() :paths paths-a))
(test* "library-fold _test (multi)" `((_test . ,(P "test.o/_test.scm")))
       (library-fold '_test acons '() :paths paths-b))
(test* "library-fold _test (multi)"
       `(("_test" . ,(P "test.o/_test/_test.scm")))
       (library-fold "_test" acons '() :paths paths-b))
(test* "library-fold _test (multi)"
       `(("_test" . ,(P "test.o/_tset/_test.scm"))
         ("_test" . ,(P "test.o/_test.scm"))
         ("_test" . ,(P "test.o/_test/_test.scm")))
       (library-fold "_test" acons '() :paths paths-b
                     :allow-duplicates? #t))
(test* "library-fold _test (non-strict)"
       `((_test . ,(P "test.o/_tset/_test.scm"))
         (_test . ,(P "test.o/_test.scm"))
         (_test . ,(P "test.o/_test/_test.scm")))
       (library-fold '_test acons '() :paths paths-b
                     :strict? #f :allow-duplicates? #t))

(test* "library-fold _test._test"
       `((_test._test . ,(P "test.o/_test/_test.scm")))
       (library-fold '_test._test acons '() :paths paths-b))
(test* "library-fold _test/_test"
       `(("_test/_test" . ,(P "test.o/_test/_test.scm")))
       (library-fold "_test/_test" acons '() :paths paths-b))

;; needs sort the result, for the order library-fold returns depends on
;; readdir(), which may be system dependent.
(test* "library-fold _test.*"
       `((_test._test . ,(P "test.o/_test/_test.scm"))
         (_test._test1 . ,(P "test.o/_test/_test1.scm")))
       (sort (library-fold '_test.* acons '() :paths paths-b)
             (lambda (a b) (string<? (cdr a) (cdr b)))))
(test* "library-fold _tset.*"
       `((_tset._test . ,(P "test.o/_tset/_test.scm")))
       (sort (library-fold '_tset.* acons '() :paths paths-b)
             (lambda (a b) (string<? (cdr a) (cdr b)))))
(test* "library-fold _tset/*"
       `(("_tset/_test" . ,(P "test.o/_tset/_test.scm"))
         ("_tset/_test2" . ,(P "test.o/_tset/_test2.scm")))
       (sort (library-fold "_tset/*" acons '() :paths paths-b)
             (lambda (a b) (string<? (cdr a) (cdr b)))))

(test* "library-fold _test.*1"
       `((_test._test1 . ,(P "test.o/_test/_test1.scm")))
       (sort (library-fold '_test.*1 acons '() :paths paths-b)
             (lambda (a b) (string<? (cdr a) (cdr b)))))
(test* "library-fold _*t._te*"
       `((_test._test .  ,(P "test.o/_test/_test.scm"))
         (_test._test1 . ,(P "test.o/_test/_test1.scm"))
         (_tset._test .  ,(P "test.o/_tset/_test.scm")))
       (sort (library-fold '_*t._te* acons '() :paths paths-b)
             (lambda (a b) (string<? (cdr a) (cdr b)))))
(test* "library-fold */*"
       `(("_test/_test" .  ,(P "test.o/_test/_test.scm"))
         ("_test/_test1" . ,(P "test.o/_test/_test1.scm"))
         ("_tset/_test" .  ,(P "test.o/_tset/_test.scm"))
         ("_tset/_test2" . ,(P "test.o/_tset/_test2.scm")))
       (sort (library-fold "*/*" acons '() :paths paths-b)
             (lambda (a b) (string<? (cdr a) (cdr b)))))

(test* "library-fold _t??t._test?"
       `((_test._test1 . ,(P "test.o/_test/_test1.scm")))
       (sort (library-fold '_t??t._test? acons '() :paths paths-b)
             (lambda (a b) (string<? (cdr a) (cdr b)))))
(test* "library-fold ?test.?test"
       `((_test._test . ,(P "test.o/_test/_test.scm")))
       (sort (library-fold '?test.?test acons '() :paths paths-b)
             (lambda (a b) (string<? (cdr a) (cdr b)))))
(test* "library-fold _t??t._test?"
       `((_test._test1 . ,(P "test.o/_test/_test1.scm"))
         (_tset._test2 . ,(P "test.o/_tset/_test2.scm")))
       (sort (library-fold '_t??t._test? acons '() :paths paths-b :strict? #f)
             (lambda (a b) (string<? (cdr a) (cdr b)))))
(test* "library-fold _t??t/_test?"
       `(("_test/_test1" . ,(P "test.o/_test/_test1.scm"))
         ("_tset/_test2" . ,(P "test.o/_tset/_test2.scm")))
       (sort (library-fold "_t??t/_test?" acons '() :paths paths-b)
             (lambda (a b) (string<? (cdr a) (cdr b)))))
(test* "library-fold _t??t?/_test?"
       '()
       (sort (library-fold "_t??t?/_test?" acons '() :paths paths-b)
             (lambda (a b) (string<? (cdr a) (cdr b)))))

(test* "library-map" `((_test._test . ,(P "test.o/_test/_test.scm"))
                       (_test._test1 . ,(P "test.o/_test/_test1.scm")))
       (sort (library-map '_test.* cons :paths paths-b)
             (lambda (a b) (string<? (cdr a) (cdr b)))))
(test* "library-for-each" `((_test._test .  ,(P "test.o/_test/_test.scm"))
                            (_test._test1 . ,(P "test.o/_test/_test1.scm")))
       (let ((p '()))
         (library-for-each '_test.*
                           (lambda (x y) (push! p (cons x y)))
                           :paths paths-b)
         (sort p (lambda (a b) (string<? (cdr a) (cdr b))))))

(test* "library-exists? _test" #t
       (not (not (library-exists? '_test :paths paths-b))))
(test* "library-exists? _test1" #f
       (not (not (library-exists? '_test1 :paths paths-b))))
(test* "library-exists? _test1, non-strict" #t
       (not (not (library-exists? '_test1 :paths paths-b :strict? #f))))
(test* "library-exists? _tset._test" #t
       (not (not (library-exists? '_tset._test :paths paths-b :strict? #f))))
(test* "library-exists? \"_test1\"" #t
       (not (not (library-exists? "_test1" :paths paths-b))))
(test* "library-exists? \"_tset/_test2\"" #t
       (not (not (library-exists? "_tset/_test2" :paths paths-b))))
(test* "library-exists? \"_test9\"" #f
       (not (not (library-exists? "_test9" :paths paths-b))))

(test* "library-exists? gauche" #t
       (not (not (library-exists? 'gauche :paths paths-b))))
(test* "library-exists? gauche, force-search" #f
       (not (not (library-exists? 'gauche :paths paths-b :force-search? #t))))
(test* "library-exists? gauche" #t
       (not (not (library-exists? "gauche" :paths paths-b))))
;;NB: this no longer work since gauche.object is compiled in.
;(test* "library-exists? gauche/object" #t
;       (not (not (library-exists? "gauche/object" :paths paths-b))))

;; we check module here, since gauche.libutil is autoloaded.
(test-module 'gauche.libutil)

(rmrf "test.o")

;; Load-path hook -----------------------------------

(test-section "load-path hook")

(define (dummy-load-path-hook archive relpath suffixes)
  (and (equal? (sys-basename archive) "test.o")
       (member ".scm" suffixes)
       (cons #"~|archive|/~|relpath|.scm"
             (^[path]
               (open-input-string
                (format "(define *path* ~s)" path))))))

(test* "dummy-load-path-hook" #"~(sys-getcwd)/test.o/non/existent/file.scm"
       (unwind-protect
           (begin
             ((with-module gauche.internal %add-load-path-hook!)
              dummy-load-path-hook)
             ;; just create a dummy file - content doesn't matter.
             (with-output-to-file "test.o"
               (^[] (print)))
             (load "non/existent/file" :paths `(,#"~(sys-getcwd)/test.o"))
             (global-variable-ref (current-module) '*path*))
         ((with-module gauche.internal %delete-load-path-hook!)
          dummy-load-path-hook)))

(test-end)
