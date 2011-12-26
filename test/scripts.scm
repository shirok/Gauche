;; Test utility scripts

(use gauche.test)
(use gauche.process)
(use srfi-13)
(use file.util)
(use file.filter)
(use gauche.config)

(test-start "utility scripts")

(define *nulldev*
  (cond-expand
   [gauche.os.windows "NUL"]
   [else "/dev/null"]))

;;=======================================================================
(test-section "gosh")

(test* "'main' in an alternative module" "foo"
         (unwind-protect
             (begin
               (delete-files "test.o")
               (with-output-to-file "test.o"
                 (^[]
                   (write
                    '(define-module foo (define (main args) (print "foo") 0)))
                   (write
                    '(define (main args) (print "bar") 0))))
               (process-output->string '("./gosh" "-ftest" "-mfoo" "test.o")))
           (delete-files "test.o")))

;;=======================================================================
(test-section "gauche-config")

(define (run-gauche-config . opts)
  (let1 p (run-process `("./gauche-config" ,@opts) :output :pipe)
    (begin0 (read-line (process-output p))
      (process-wait p))))

(define *config-options*
  '("-V" "-I" "-L" "-l" "--cc" "--ac" "--reconfigure" "--arch"
    "--syslibdir" "--sysarchdir" "--sysincdir"
    "--sitelibdir" "--sitearchdir" "--siteincdir"
    "--pkglibdir" "--pkgarchdir" "--pkgincdir"
    "--mandir" "--infodir"
    "--object-suffix" "--executable-suffix" "--so-suffix" "--so-ldflags"
    "--so-libs" "--dylib-suffix" "--dylib-ldflags" "--rpath-flag"))

(dolist [opt *config-options*]
  (test* #`"gauhce-config ,opt" (gauche-config opt) (run-gauche-config opt)))

;;=======================================================================
(test-section "gauche-install")

(define (run-install . args)
  (run-process `("./gosh" "-ftest" "gauche-install.in" ,@args)
               :output *nulldev* :wait #t))

(remove-files "test.o" "test1.o")

(test* "-d" #t
       (begin (run-install "-d" "test1.o/dest")
              (file-is-directory? "test1.o/dest")))

(create-directory-tree "."
                       `(test.o ((bin ((command1 ,(make-string 20000))
                                       (command2 ,(make-string 20000))))
                                 (lib ((lib1 ,(make-string 1000000))
                                       (lib2 ,(make-string 500000))))
                                 (etc ((conf1 ,(make-string 1000)))))))

(test* "file -> file" #t
       (begin (run-install "-m" "444" "test.o/lib/lib1" "test1.o/dest/lib1")
              (and (file-is-regular? "test1.o/dest/lib1")
                   (= (file-perm "test1.o/dest/lib1") #o444)
                   (file-equal? "test.o/lib/lib1" "test1.o/dest/lib1"))))

(remove-files "test1.o/dest/lib1")

(test* "files -> dir" #t
       (begin (run-install "-m" "444" "test.o/lib/lib1" "test.o/lib/lib2"
                           "test1.o/dest")
              (and (file-is-regular? "test1.o/dest/lib1")
                   (file-is-regular? "test1.o/dest/lib2")
                   (= (file-perm "test1.o/dest/lib1")
                      (file-perm "test1.o/dest/lib1")
                      #o444)
                   (file-equal? "test.o/lib/lib1" "test1.o/dest/lib1")
                   (file-equal? "test.o/lib/lib2" "test1.o/dest/lib2"))))

(remove-files "test1.o/dest/lib1" "test1.o/dest/lib2")

(test* "-T" #t
       (begin (run-install "-T" "test1.o/dest" "-m" "555"
                           "test.o/bin/command1"
                           "test.o/bin/command2")
              (and (= (file-perm "test1.o/dest/test.o/bin/command1")
                      (file-perm "test1.o/dest/test.o/bin/command2")
		      (cond-expand
		       [gauche.os.windows #o444]
		       [else #o555]))
                   (file-equal? "test.o/bin/command1"
                                "test1.o/dest/test.o/bin/command1")
                   (file-equal? "test.o/bin/command2"
                                "test1.o/dest/test.o/bin/command2"))))

(test* "-U" #t
       (begin (run-install "-U" "test1.o/dest" "-m" "555"
                           "test.o/bin/command1"
                           "test.o/bin/command2")
              (and (not (file-exists? "test1.o/dest/test.o/bin/command1"))
                   (not (file-exists? "test1.o/dest/test.o/bin/command2")))))

(test* "-T -p" #t
       (begin (run-install "-T" "test1.o/dest" "-m" "555" "-p" "test.o"
                           "test.o/bin/command1"
                           "test.o/bin/command2")
              (and (= (file-perm "test1.o/dest/bin/command1")
                      (file-perm "test1.o/dest/bin/command2")
		      (cond-expand
		       [gauche.os.windows #o444]
		       [else #o555]))
                   (file-equal? "test.o/bin/command1"
                                "test1.o/dest/bin/command1")
                   (file-equal? "test.o/bin/command2"
                                "test1.o/dest/bin/command2"))))

(test* "-U -p" #t
       (begin (run-install "-U" "test1.o/dest" "-m" "555" "-p" "test.o"
                           "test.o/bin/command1"
                           "test.o/bin/command2")
              (and (not (file-exists? "test1.o/dest/bin/command1"))
                   (not (file-exists? "test1.o/dest/bin/command2")))))

(remove-files "test.o" "test1.o")

;;=======================================================================
(test-section "gauche-package")

(remove-files "test.o")
(make-directory* "test.o")

(define (package-generate-tests)
  (define (file-check name)
    (test* #`"checking existence of ,name" #t
           (file-exists? #`"test.o/Test/,name")))
  (define pwd (sys-getcwd))

  ;; We need to fake some directories in order to make this
  ;; work without installing.
  (with-output-to-file "test.o/run"
    (cut for-each write
         '((use gauche.config)
           (define prefix (if (equal? (sys-basename (sys-getcwd)) "Test")
                            "../" ""))
           (define sep (cond-expand [gauche.os.windows ";"][else ":"]))
           ;; This is used to copy templates from.
           (define (gauche-library-directory) #`",|prefix|../../ext/x")
           ;; Intercept gauche-config to override compiler flags
           (define gauche-config-orig
             (with-module gauche.config gauche-config))
           (define-in-module gauche.config (gauche-config opt)
             (cond [(equal? opt "--incdirs")
                    #`",|prefix|../../src,|sep|,|prefix|../../gc/include"]
                   [(equal? opt "--archdirs")
                    #`",|prefix|../../src"]
                   [else (gauche-config-orig opt)]))
           (load #`",|prefix|../gauche-package.in"))))

  (run-process
   `(../gosh -ftest ./run generate Test test.module)
   :output :null :error :null :wait #t :directory "test.o/")

  (for-each file-check '("DIST" "configure.ac" "Makefile.in"
                         "test.c" "test.h" "test.scm" "testlib.stub"
                         "test/module.scm"))

  (test* "gauche-package compile" #t
         (let* ([p (run-process
                    `(../../gosh -q -I../../../src -I../../../lib
                                 -lgauche-init.scm
                                 ../run compile
                                 --verbose test test.c testlib.stub)
                    :redirects '((>& 2 1) (> 1 out)) :directory "test.o/Test")]
                [o (port->string (process-output p 'out))])
           (process-wait p)
           ;; if compilation fails, returns the output for better diagnostics.
           (or (zero? (process-exit-status p)) o)))
  )

(unwind-protect (package-generate-tests)
  (remove-directory* "test.o"))

(test-end)
