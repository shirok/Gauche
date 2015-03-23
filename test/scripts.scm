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

(test* "r7rs script support" "bar"
       (unwind-protect
           (begin
             (delete-files "test.o")
             (with-output-to-file "test.o"
               (^[]
                 (write
                  '(import (scheme base) (scheme write)))
                 (write
                  '(display "bar\n"))))
             (process-output->string '("./gosh" "-ftest" "test.o")))
         (delete-files "test.o")))

(test* "r7rs script support (don't call main automatically)" "baz"
       (unwind-protect
           (begin
             (delete-files "test.o")
             (with-output-to-file "test.o"
               (^[]
                 (write
                  '(import (scheme base) (scheme write)))
                 (write
                  '(define (main args) (display "bar\n")))
                 (write
                  '(display "baz\n"))))
             (process-output->string '("./gosh" "-ftest" "test.o")))
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
  (test* #"gauhce-config ~opt" (gauche-config opt) (run-gauche-config opt)))

;;=======================================================================
(test-section "configure")

(use gauche.configure)
(test-module 'gauche.configure)

;; NB: At this moment, running env.exe via run-process doesn't work,
;; if gosh is runnung under MSYS shell.  (It is ok if gosh is running
;; under cmd.)
;; So we skip this test on MinGW.
(cond-expand
 [gauche.os.windows]
 [else

(remove-files "test.o" "test2.o")
(make-directory* "test.o/src")
(make-directory* "test2.o")
(let ([extdir (build-path (or (sys-getenv "top_srcdir") "..") "ext")])
  (define (filter-copy infile outfile)
    (file-filter (^[in out]
                   (dolist [line (port->string-list in)]
                     (display ($ regexp-replace-all* line #/@@/ ""
                                 #/\(cf-output \"Makefile\"\)/
                                 "(cf-define 'HAVE_STDIO_H \"1\")\n\
                                  (cf-config-headers \"config.h\")\n\
                                  (cf-output \"Makefile\" \"src/Makefile\")")
                              out)
                     (newline out)))
                 :input infile
                 :output outfile))
  (filter-copy (build-path extdir "template.configure") "test.o/configure")
  (filter-copy (build-path extdir "template.Makefile.in") "test.o/Makefile.in")

  (with-output-to-file "test.o/src/Makefile.in"
    (^[]
      (print "srcdir = @srcdir@")
      (print "top_srcdir = @top_srcdir@")
      (print "builddir = @builddir@")
      (print "top_builddir = @top_builddir@")))

  (with-output-to-file "test.o/config.h.in"
    (^[]
      (print "/* Define to 1 if you have the <foo.h> header file */")
      (print "#undef HAVE_FOO_H")
      (print)
      (print "this is literal line")
      (print "/* Define to 1 if you have the <stdio.h> header file */")
      (print "#undef HAVE_STDIO_H")
      ))
  )

;; When we run configure, we need to include directories of gosh and
;; other scripts in PATH.  Before installing Gauche, where we find them
;; is "..".
(define (run-with-parent-directory-in-paths cmd . args)
  (let* ([separ (cond-expand [gauche.os.windows ";"] [else ":"])]
         [paths #"..~|separ|~(sys-getenv \"PATH\")"])
    (apply run-process `("env" ,#"PATH=~paths" ,@cmd) args)))

(test* "running `configure' script" 0
       (process-exit-status
        (run-with-parent-directory-in-paths
         `("../gosh" "-ftest" "./configure")
         :output *nulldev* :wait #t :directory "test.o")))
(test* "Makefile substitution" '()
       (and (file-exists? "test.o/Makefile")
            (filter #/@\w+@/ (file->string-list "test.o/Makefile"))))
(test* "VERSION generation" "1.0\n"
       (and (file-exists? "test.o/VERSION")
            (file->string "test.o/VERSION")))
(test* "srcdir etc."
       '("srcdir = ." "top_srcdir = .." "builddir = ." "top_builddir = ..")
       (file->string-list "test.o/src/Makefile"))

(test* "configure --version" "package configure 1.0"
       (read-line
        (process-output
         (run-with-parent-directory-in-paths
          `("../gosh" "-ftest" "./configure" "--version")
          :output :pipe :directory "test.o"))))

(test* "running `configure' script in different directory" 0
       (process-exit-status
        (run-with-parent-directory-in-paths
         `("../gosh" "-ftest" "../test.o/configure")
         :output *nulldev* :wait #t :directory "test2.o")))

(test* "Makefiles in proper builddir" '(#t #t)
       (list (file-exists? "test2.o/Makefile")
             (file-exists? "test2.o/src/Makefile")))

(test* "srcdir etc."
       '("srcdir = ../test.o/src" "top_srcdir = ../../test.o" "builddir = ." "top_builddir = ..")
       (file->string-list "test2.o/src/Makefile"))

(dolist [d '("test.o" "test2.o")]
  (let1 config.h (build-path d "config.h")
    (test* config.h
           '("/* Define to 1 if you have the <foo.h> header file */"
             "/* #undef HAVE_FOO_H */"
             ""
             "this is literal line"
             "/* Define to 1 if you have the <stdio.h> header file */"
             "#define HAVE_STDIO_H 1")
           (and (file-exists? config.h)
                (file->string-list config.h)))))

(remove-files "test2.o/package.gpd")

(test* "gpd"
       "./configure --with-local=/a/b:/c/d"
       (and (zero?
             (process-exit-status
              (run-with-parent-directory-in-paths
               `("../gosh" "-ftest" "../test.o/configure"
                 "--with-local=/a/b:/c/d")
               :output *nulldev* :wait #t :directory "test2.o")))
            (cadr (memv :configure
                        (car (file->sexp-list "test2.o/package.gpd"))))))

(remove-files "test.o" "test2.o")

]) ; cond-expand except windows

;;=======================================================================
(test-section "gauche-install")

(define (run-install . args)
  (let1 gauche-install
      (build-path (or (sys-getenv "top_srcdir") "..") "src" "gauche-install.in")
    (run-process `("./gosh" "-ftest" ,gauche-install ,@args)
                 :output *nulldev* :wait #t)))

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
    (test* #"checking existence of ~name" #t
           (file-exists? #"test.o/Test/~name")))
  (define pwd (sys-getcwd))
  (define top-srcdir (sys-normalize-pathname
                      (or (sys-getenv "top_srcdir") "..")
                      :absolute #t :canonicalize #t))

  ;; When we run the test before installation, we need to tweak
  ;; some directories.  We can distinguish it, for install-check
  ;; passes an argument "install-check" to the test script.
  (define in-place? (not (equal? (cdr (command-line)) '("install-check"))))

  (define gauche-package
    (build-path (gauche-architecture-directory) "gauche-package"))
  (define generate-command
    (if in-place?
      `(../gosh -ftest ./run generate Test test.module)
      `(,gauche-package generate Test test.module)))
  (define compile-command
    (if in-place?
      `(../../gosh -q -I../../../src -I../../../lib
                   ../run compile
                   --verbose test test.c testlib.stub)
      `(,gauche-package compile --verbose test test.c testlib.stub)))    

  (when in-place?
    (with-output-to-file "test.o/run"
      (cut for-each write
           `((use gauche.config)
             (define prefix (if (equal? (sys-basename (sys-getcwd)) "Test")
                              "../" ""))
             (define sep (cond-expand [gauche.os.windows ";"][else ":"]))
             (define top-srcdir ,top-srcdir)
             ;; This is used to copy templates from.
             (define (gauche-library-directory) #"~|top-srcdir|/ext/x")
             ;; Intercept gauche-config to override compiler flags
             (define gauche-config-orig
               (with-module gauche.config gauche-config))
             (define-in-module gauche.config (gauche-config opt)
               (cond [(equal? opt "--incdirs")
                      #"~|top-srcdir|/src~|sep|~|prefix|../../src/~|sep|~|top-srcdir|/gc/include"]
                     [(equal? opt "--archdirs")
                      #"~|prefix|../../src"]
                     [else (gauche-config-orig opt)]))
             (load #"~|top-srcdir|/src/gauche-package.in")))))

  (test-log "Running ~a" generate-command)
  
  ($ run-process generate-command
     :output :null :error :null :wait #t :directory "test.o/")

  (for-each file-check '("configure" "Makefile.in"
                         "test.c" "test.h" "test.scm" "testlib.stub"
                         "test/module.scm"))

  (test-log "Running ~a" compile-command)
  
  (test* "gauche-package compile" #t
         (let* ([p ($ run-process
                      compile-command
                      :redirects '((>& 2 1) (> 1 out)) :directory "test.o/Test")]
                [o (port->string (process-output p 'out))])
           (process-wait p)
           ;; if compilation fails, returns the output for better diagnostics.
           (or (zero? (process-exit-status p)) o)))
  )

(unwind-protect (package-generate-tests)
  (remove-directory* "test.o"))

(test-end)
