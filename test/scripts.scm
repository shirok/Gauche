;; Test utility scripts

(use gauche.test)
(use gauche.process)
(use srfi.13)
(use file.util)
(use file.filter)
(use gauche.config)
(use gauche.threads)

(test-start "utility scripts")

(define *top-srcdir*
  (sys-normalize-pathname (or (sys-getenv "top_srcdir") "..")
                          :absolute #t :canonicalize #t))
(define *top-builddir*
  (sys-normalize-pathname (or (sys-getenv "top_builddir") "..")
                          :absolute #t :canonicalize #t))

(define *executable-suffix* (gauche-config "--executable-suffix"))

(define (fix-path path)
  (cond-expand
   [gauche.os.windows (regexp-replace-all #/\\/ path "/")]
   [else              path]))

(define (wrap-with-test-directory thunk dirs :optional (mkdir? #t))
  ;; cleanup may fail due to the timing (exp. on mingw), so we retry
  ;; with delay.
  (define (remove-dirs dirs)
    (let loop ([retry 0])
      (if (< retry 3)
        (or (guard (e [<system-error> #f])
              (apply remove-files dirs)
              #t)
            (begin (sys-sleep 1) (loop (+ retry 1))))
        (apply remove-files dirs))))
  (remove-dirs dirs)
  (when mkdir? (for-each make-directory* dirs))
  (unwind-protect (thunk) (remove-dirs dirs)))

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

(test* "r7rs + SRFI-22 by interpreter name" "baz bar"
       (unwind-protect
           (begin
             (delete-files "test.o" "scheme-r7rs")
             (copy-file #"./gosh~*executable-suffix*" "./scheme-r7rs")
             (sys-chmod "./scheme-r7rs" #o755)
             (with-output-to-file "test.o"
               (^[]
                 (write
                  '(import (scheme base) (scheme write)))
                 (write
                  '(define (main args) (display "bar\n") 0))
                 (write
                  '(display "baz\n"))))
             (process-output->string '("./scheme-r7rs" "-ftest" "test.o")
                                     :on-abnormal-exit :ignore))
         (delete-files "test.o" "scheme-r7rs")))

;; This caused assertion failure in 0.9.5, because 'main' was called
;; via Scm_ApplyRec without base VM running.
;; See https://github.com/shirok/Gauche/issues/244
(test* "proper error handling of 'main'" "ok"
       (unwind-protect
           (begin
             (delete-files "test.o")
             (with-output-to-file "test.o"
               (^[]
                 (write
                  '(use gauche.partcont))
                 (write
                  `(define (main args)
                     (reset (shift k (call-with-input-file "test.o" k)))
                     (print 'ok)
                     0))))
             (process-output->string '("./gosh" "-ftest" "test.o")))
         (delete-files "test.o")))

;;=======================================================================
(test-section "gauche-config")

(define (run-gauche-config . opts)
  (let1 p (run-process `("./gauche-config" ,@opts)
                       :redirects '((>& 2 1) (> 1 out)))
    (begin0 (port->string (process-output p 'out))
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
  (test* #"gauche-config ~opt" (gauche-config opt)
         (car (string-split (run-gauche-config opt) #[\r\n]))))

(test*/diff
 "gauche-config --help"
 '("Usage: gauche-config [option ...]"
   "Multiple options can be given."
   ""
   "General parameter"
   "  -V"
   "        The current Gauche version."
   ""
   "Parameters to compile applications using Gauche"
   "  -I"
   "        Include path options required to compile programs using Gauche"
   "        (Note: This doesn't work if Gauche installation directory"
   "        path contains whitespaces.  See --incdirs below.)"
   "  -L"
   "        Library path options required to link programs using Gauche"
   "        (Note: This doesn't work if Gauche installation directory"
   "        path contains whitespaces.  See --archdirs below.)"
   "  -l"
   "        Link library options required to link programs using Gauche."
   "  --cc"
   "        The name of the C compiler used to compile this Gauche."
   "  --cpp"
   "        The command to run the C preprocessor."
   "  --ac"
   "        The directory that contains Gauche-specific autoconf macros."
   "  --reconfigure"
   "        The command line used to configure the current installation."
   "  --arch"
   "        The autoconf-style architecture signature (cpu-vendor-kernel-os)."
   "  --incdirs"
   "  --archdirs"
   "        The list of directory names to be looked for include files and"
   "        libraries, respectively.  Each directory name may be quoted if"
   "        it contains whitespaces, and separated by ':' on Unix platforms, or"
   "        by ';' on Windows platforms."
   "  --local-incdir"
   "  --local-libdir"
   "        These are '-I' and '-L' flags for additional local headers/libraries"
   "        to search, given by '--with-local' configure flags.  Note that those"
   "        are also included in '-I', '-L', '--incdirs', and '--archdirs'."
   ""
   "Parameters to install files"
   "  --prefix"
   "        The directory prefix set by configure."
   "  --sysincdir"
   "  --siteincdir"
   "  --pkgincdir"
   "        Directories where system|site|package header files of extensions go."
   "  --syslibdir"
   "  --sitelibdir"
   "  --pkglibdir"
   "        Directories where system|site|package scheme files go."
   "  --sysarchdir"
   "  --sitearchdir"
   "  --pkgarchdir"
   "        Directories where system|site|package DSO files go."
   "  --mandir"
   "  --infodir"
   "        Directories where gauche manpage and info docs are installed."
   ""
   "Parameters to help building extensions"
   "  --object-suffix"
   "        The extension of the compiled objects (e.g. 'o' or 'obj')."
   "  --executable-suffix"
   "        The extension of the executable including a period (empty on Unix systems,"
   "        '.exe' on Windows."
   "  --so-suffix"
   "        The extension for dynamically loadable (dlopen-able) modules (e.g. 'so')."
   "  --so-cflags"
   "        Additional CFLAGS to create dynamically loadable modules."
   "  --so-ldflags"
   "        Additional LDFLAGS to create dynamically loadable modules."
   "  --so-libs"
   "        Additional libraries required to create dynamically loadable modules."
   "  --dylib-suffix"
   "        The extension for dynamically linked libraries (as opposed to dlopen()ed)"
   "        Usually the same as --so-suffix, but OSX wants 'dylib'."
   "  --dylib-ldflags"
   "        LDFLAGS to create dynamically linked libraries."
   "  --rpath-flag"
   "        Compiler flag(s) to embed RPATH"
   "  --default-cflags"
   "        This is the CFLAGS used to compile Gauche, and to be used to compile"
   "        extensions as well."
   "  --cppflags"
   "        Compiler flag(s) passed to C preprocessor"
   "  --static-libs"
   "        List of library link flags ('-llib') required to link Gauche statically."
   "        Similar to '-l', but this includes the libraries that are used"
   "        for extension modules, and also the static library itself,"
   "        that is -lgauche-static-X.X."
   "  --libgauche-so"
   "        The base name of dynamically linked libgauche."
   "")
 (string-split (run-gauche-config "--help")
               (cond-expand
                [gauche.os.windows "\r\n"]
                [else "\n"])))

;;=======================================================================
(test-section "configure")

(use gauche.configure)
(test-module 'gauche.configure)
(test-module 'gauche.configure.base)
(test-module 'gauche.configure.lang)
(test-module 'gauche.configure.prog)
(test-module 'gauche.configure.output)
(test-module 'gauche.configure.compile)
(test-module 'gauche.configure.init)

;; cf-help-string test
(test* "cf-help-string (run-in)"
       "  -C                      ignored for the compatibility to autoconf-generated\
      \n                          configure\n"
       ($ cf-help-string "-C"
          "ignored for the compatibility to autoconf-generated configure"))
(test* "cf-help-string (description next line)"
       "  -C, --config-cache,--cache-file=FILE\
      \n                          ignored for the compatibility to autoconf-generated\
      \n                          configure\n"
       ($ cf-help-string "-C, --config-cache,--cache-file=FILE"
          "ignored for the compatibility to autoconf-generated configure"))

;; When we run configure, we need to include directories of gosh and
;; other scripts in PATH.  Before installing Gauche, where we find them
;; is "..".
(define (run-with-parent-directory-in-paths cmd . args)
  (let* ([separ (cond-expand [gauche.os.windows ";"] [else ":"])]
         [paths-old (sys-getenv "PATH")]
         [paths-new #"..~|separ|~|paths-old|"])
    (cond-expand
     [gauche.os.windows
      (sys-setenv "PATH" paths-new #t)
      (unwind-protect (apply run-process cmd args)
        (sys-setenv "PATH" paths-old #t))]
     [else
      (apply run-process `("env" ,#"PATH=~|paths-new|" ,@cmd) args)])))

(define (with-cf-subst-test)
  (make-directory* "test.o")
  (with-output-to-file "test.o/configure"
    (^[]
      (write '(use gauche.configure))
      (write '(cf-init "foo" "1.0"))
      (write '(cf-subst 'A "1"))
      (write '(cf-subst 'B "2"))
      (write '(cf-subst 'C "3"))
      (write '(define (dump)
                (for-each (^v (cf-msg-notice "~a=~a" v (cf$ v)))
                          '(A B C D E F))))
      (write '(with-cf-subst
               ((A "11") (B + "zz") (C "nn" +) (D "44") (E + "55") (F "66" +))
               (dump)))
      (write '(dump))))

  (test* "running `configure' script for with-cf-subst" 0
         (process-exit-status
          (run-with-parent-directory-in-paths
           `("../gosh" "-ftest" "./configure")
           :output :null :wait #t :directory "test.o")))
  (test*/diff "check result of with-cf-subst"
              '("Configuring foo 1.0"
                "A=11"
                "B=2 zz"
                "C=nn 3"
                "D=44"
                "E=55"
                "F=66"
                "A=1"
                "B=2"
                "C=3"
                "D="
                "E="
                "F=")
              (file->string "test.o/config.log")))

(define (configure-test-1)
  (make-directory* "test.o/src")
  (make-directory* "test2.o")
  (let ([extdir (build-path (or (sys-getenv "top_srcdir") "..") "ext")])
    (define (filter-copy infile outfile)
      (file-filter (^[in out]
                     (dolist [line (port->string-list in)]
                       (display ($ regexp-replace-all* line
                                   #/@@author@@/ ""
                                   #/@@license@@/ ""
                                   #/@@/ ""
                                   #/\(cf-output-default\)/
                                   "(cf-define 'HAVE_STDIO_H \"1\")\n\
                                    (cf-config-headers \"config.h\")\n\
                                    (cf-output-default)")
                                out)
                       (newline out)))
                   :input infile
                   :output outfile))
    (filter-copy (build-path extdir "package-templates" "configure")
                 "test.o/configure")
    (filter-copy (build-path extdir "package-templates" "package.scm")
                 "test.o/package.scm")
    (filter-copy (build-path extdir "package-templates" "Makefile.in")
                 "test.o/Makefile.in")

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

  (test* "running `configure' script" 0
         (process-exit-status
          (run-with-parent-directory-in-paths
           `("../gosh" "-ftest" "./configure")
           :output :null :wait #t :directory "test.o")))
  (test* "Makefile substitution" '()
         (and (file-exists? "test.o/Makefile")
              (filter #/@\w+@/ (file->string-list "test.o/Makefile"))))
  (test* "VERSION generation" "0.1\n"
         (and (file-exists? "test.o/VERSION")
              (file->string "test.o/VERSION")))
  (test* "srcdir etc."
         '("srcdir = ." "top_srcdir = ../" "builddir = ." "top_builddir = ../")
         (file->string-list "test.o/src/Makefile"))

  (test* "configure --version" "package configure 0.1"
         (read-line
          (process-output
           (run-with-parent-directory-in-paths
            `("../gosh" "-ftest" "./configure" "--version")
            :output :pipe :directory "test.o"))))

  (test* "running `configure' script in different directory" 0
         (process-exit-status
          (run-with-parent-directory-in-paths
           `("../gosh" "-ftest" "../test.o/configure")
           :output :null :wait #t :directory "test2.o")))

  (test* "Makefiles in proper builddir" '(#t #t)
         (list (file-exists? "test2.o/Makefile")
               (file-exists? "test2.o/src/Makefile")))

  (test* "srcdir etc."
         '("srcdir = ../../test.o/src" "top_srcdir = ../../test.o" "builddir = ." "top_builddir = ../")
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
         (cond-expand
          [gauche.os.windows "./configure \"--with-local=/a/b:/c/d\""]
          [else              "./configure --with-local=/a/b:/c/d"])
         (and (zero?
               (process-exit-status
                (run-with-parent-directory-in-paths
                 `("../gosh" "-ftest" "../test.o/configure"
                   "--with-local=/a/b:/c/d")
                 :output :null :wait #t :directory "test2.o")))
              (cadr (memv :configure
                          (car (file->sexp-list "test2.o/package.gpd"))))))
  )

(define (configure-test-2)
  (make-directory* "test.o/src")
  (make-directory* "test2.o")
  (let ([extdir (build-path (or (sys-getenv "top_srcdir") "..") "ext")])
    (define (filter-copy infile outfile)
      (file-filter (^[in out]
                     (dolist [line (port->string-list in)]
                       (display (regexp-replace-all* line
                                                     #/@@author@@/ ""
                                                     #/@@license@@/ ""
                                                     #/@@/ "")
                                out)
                       (newline out)))
                   :input infile
                   :output outfile))
    (filter-copy (build-path extdir "package-templates" "package.scm")
                 "test.o/package.scm")
    (filter-copy (build-path extdir "package-templates" "Makefile.in")
                 "test.o/Makefile.in")

    (with-output-to-file "test.o/configure"
      (^[]
        ($ for-each write
           '((use gauche.configure)
             (cf-init-gauche-extension)
             (cf-check-headers '("stdio.h" "stdlib.h"
                                 "no-such-header-should-exist.h"))
             (cf-check-types '("a_t" "b_t" "struct c_t" "d_t")
                             :includes '("typedef int a_t;"
                                         "int b_t;"
                                         "struct c_t { int i; };"))
             (cf-check-decls '("a" "b" "c" "d" "e" "f")
                             :includes '("#define a 1"
                                         "int b;"
                                         "const int c = 1;"
                                         "extern int d();"
                                         "struct foo { int i; } e;"))
             (cf-check-members '("struct foo.a" "struct foo.b")
                               :includes '("struct foo { int b; };"))
             (cf-check-funcs '("printf" "nonexistent_weird_function"))
             (unless (#/darwin/ (gauche-architecture))
               ;; this test fails on OSX since 'sin' is recognized as built-in
               (cf-check-lib "m" "sin"))
             (cf-check-lib "no-such-library-should-exist" "sin")
             (cf-config-headers "config.h")
             (cf-output-default)))))

    (with-output-to-file "test.o/src/Makefile.in"
      (^[]
        (print "LIBS = @LIBS@")))

    (with-output-to-file "test.o/config.h.in"
      (^[]
        (print "#undef HAVE_STDIO_H")
        (print "#undef HAVE_STDLIB_H")
        (print "#undef HAVE_NO_SUCH_HEADER_SHOILD_EXIST_H")
        (print "#undef HAVE_A_T")
        (print "#undef HAVE_B_T")
        (print "#undef HAVE_STRUCT_C_T")
        (print "#undef HAVE_D_T")
        (print "#undef HAVE_DECL_A")
        (print "#undef HAVE_DECL_B")
        (print "#undef HAVE_DECL_C")
        (print "#undef HAVE_DECL_D")
        (print "#undef HAVE_DECL_E")
        (print "#undef HAVE_DECL_F")
        (print "#undef HAVE_STRUCT_FOO_A")
        (print "#undef HAVE_STRUCT_FOO_B")
        (print "#undef HAVE_PRINTF")
        (print "#undef HAVE_NONEXISTENT_WEIRD_FUNCTION")
        (print "#undef HAVE_LIBM")
        (print "#undef HAVE_LIBNO_SUCH_LIBRARY_SHOULD_EXIST")
        ))
    )

  (test* "running `configure' script with actual checks" 0
         (process-exit-status
          (run-with-parent-directory-in-paths
           `("../gosh" "-ftest" "./configure")
           :output :null :wait #t :directory "test.o")))

  (test* "cf-check-lib set LIBS"
         (if (#/darwin/ (gauche-architecture))
           '("LIBS = ")
           '("LIBS = -lm"))
         (file->string-list "test.o/src/Makefile"))

  (test*/diff "cf-check-headers and cf-check-lib to set defines"
              `("#define HAVE_STDIO_H 1"
                "#define HAVE_STDLIB_H 1"
                "/* #undef HAVE_NO_SUCH_HEADER_SHOILD_EXIST_H */"
                "#define HAVE_A_T 1"
                "/* #undef HAVE_B_T */"
                "#define HAVE_STRUCT_C_T 1"
                "/* #undef HAVE_D_T */"
                "#define HAVE_DECL_A 1"
                "#define HAVE_DECL_B 1"
                "#define HAVE_DECL_C 1"
                "#define HAVE_DECL_D 1"
                "#define HAVE_DECL_E 1"
                "#define HAVE_DECL_F 0"
                "/* #undef HAVE_STRUCT_FOO_A */"
                "#define HAVE_STRUCT_FOO_B 1"
                "#define HAVE_PRINTF 1"
                "/* #undef HAVE_NONEXISTENT_WEIRD_FUNCTION */"
                ,(if (#/darwin/ (gauche-architecture))
                   "/* #undef HAVE_LIBM */"
                   "#define HAVE_LIBM 1")
                "/* #undef HAVE_LIBNO_SUCH_LIBRARY_SHOULD_EXIST */")
              ($ filter #/HAVE_/
                 $ file->string-list "test.o/config.h"))
  )

(wrap-with-test-directory with-cf-subst-test '("test.o") #f)
(wrap-with-test-directory configure-test-1 '("test.o" "test2.o") #f)
(wrap-with-test-directory configure-test-2 '("test.o" "test2.o") #f)

;;=======================================================================
(test-section "gauche-install")

(define *gauche-install-script*
  (build-path (or (sys-getenv "top_srcdir") "..") "src" "gauche-install.in"))

(test-script *gauche-install-script*)

(define (run-install . args)
  (run-process `("./gosh" "-ftest" ,*gauche-install-script* ,@args)
               :output :null :wait #t))

(define (test-install)
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
  )

(wrap-with-test-directory test-install '("test.o" "test1.o") #f)

;;=======================================================================
(test-section "gauche-package")

(test-script
 (build-path (or (sys-getenv "top_srcdir") "..") "src" "gauche-package.in"))

(define (package-generate-tests scheme-only?)
  (define (file-check name)
    (test* #"checking existence of ~name" #t
           (file-exists? #"test.o/Test/~name")))
  (define pwd (sys-getcwd))

  ;; When we run the test before installation, we need to tweak
  ;; some directories.  We can distinguish it, for install-check
  ;; passes an argument "install-check" to the test script.
  (define in-place? (not (equal? (cdr (command-line)) '("install-check"))))

  (define gauche-package
    (build-path (gauche-architecture-directory) "gauche-package"))
  (define generate-command
    (if in-place?
      `(../gosh -ftest
                ,(build-path *top-srcdir* "src" "gauche-package.in")
                generate
                ,@(cond-list [scheme-only? "--scheme-only"])
                --template-dir ,(build-path *top-srcdir* "ext"
                                            "package-templates")
                Test test.tester)
      `(,gauche-package generate
                        ,@(cond-list [scheme-only? "--scheme-only"])
                        Test test.tester)))
  (define compile-command
    (if in-place?
      `(../../gosh -q -I../../../src -I../../../lib
                   ../run compile
                   --verbose test test.c testlib.scm)
      `(,gauche-package compile --verbose test test.c testlib.scm)))
  (define test-command
    (if in-place?
      `(../../gosh -ftest -I. ./test.scm)
      `(,(build-path (gauche-architecture-directory) "gosh") -I. ./test.scm)))

  (when in-place?
    (with-output-to-file "test.o/run"
      (cut for-each write
           `((use gauche.config)
             (define prefix (if (equal? (sys-basename (sys-getcwd)) "Test")
                              "../" ""))
             (define sep (cond-expand [gauche.os.windows ";"][else ":"]))
             (define top-srcdir ,*top-srcdir*)
             ;; This is used to copy templates from.
             (define (gauche-library-directory) #"~|top-srcdir|/ext/templates/x")
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

  ($ run-process generate-command :wait #t :directory "test.o/")

  (for-each file-check
            (if scheme-only?
              '("configure" "Makefile.in" "package.scm"
                "test.scm" "test/tester.scm")
              '("configure" "Makefile.in" "package.scm"
                "test.c" "test.h" "test.scm" "testlib.scm"
                "test/tester.scm")))

  (test-log "Running ~a" compile-command)

  (unless scheme-only?
    (test* "gauche-package compile" #t
           (let* ([p ($ run-process
                        compile-command
                        :redirects '((>& 2 1) (> 1 out))
                        :directory "test.o/Test")]
                  [o (port->string (process-output p 'out))])
             (process-wait p)
             ;; if compilation fails, returns the output for better diagnostics.
             (or (zero? (process-exit-status p)) o))))

  (test* "running test" #t
         (let* ([p ($ run-process
                      test-command
                      :redirects  '((>& 2 1) (> 1 out))
                      :directory "test.o/Test")]
                [o (port->string (process-output p 'out))])
           (process-wait p)
           (or (zero? (process-exit-status p)) o)))
  )

(wrap-with-test-directory (cut package-generate-tests #f) '("test.o"))
(wrap-with-test-directory (cut package-generate-tests #t) '("test.o"))

;;=======================================================================
(test-section "precomp")

(define (do-precomp! files extra-options)
  (do-process! `("../../src/gosh" "-ftest"
                 ,#"-I~|*top-srcdir*|/test/test-precomp"
                 ,(build-path *top-srcdir* "lib/tools/precomp")
                 "--strip-prefix"
                 ,(fix-path (build-path *top-srcdir* "test/test-precomp"))
                 ,@extra-options
                 ,@(map (^[file] (fix-path (build-path *top-srcdir* "test/test-precomp" file)))
                        files))
               :directory "test.o"))

(define (do-compile! output files)
  (do-process!
   `("../../src/gosh" "-ftest"
     ,(build-path *top-srcdir* "src/gauche-package.in")
     "compile"
     ,#"--cppflags=-I~(fix-path (build-path *top-srcdir* \"src\")) \
                   -I~(fix-path (build-path *top-srcdir* \"gc/include\")) \
                   -I~(fix-path (build-path *top-builddir* \"src\")) \
                   -I~(fix-path (build-path *top-builddir* \"gc/include\"))"
     ,#"--ldflags=-L~(fix-path (build-path *top-srcdir* \"src\")) \
                  -L~(fix-path (build-path *top-builddir* \"src\"))"
     ,output
     ,@files)
   :directory "test.o"))

;; Removing a DSO file while it is opened tend to cause issues, so we invoke
;; a child gosh to load and test it.
(define-syntax dynload-and-eval
  (syntax-rules ()
    [(_ libname expr)
     (begin
       (with-output-to-file "test.o/t.scm"
         (^[]
           (write '(add-load-path "."))
           (write '(use gauche.threads)) ; some tests need this
           (write '(use file.util))
           (write `(define *top-srcdir*
                     ,(if (absolute-path? *top-srcdir*)
                        *top-srcdir*
                        (build-path ".." *top-srcdir*))))
           (write '(load libname))
           (write '(write expr))
           (write '(exit 0))))
       (let* ([p (run-process '("../../src/gosh" "-ftest" "./t.scm")
                              :output :pipe :directory "test.o")]
              [result (read (process-output p))])
         (process-wait p)
         result))]))

(define (precomp-test-1)
  (test* "running precomp 1" #t
         (do-precomp! '("foo.scm" "foo/bar1.scm" "foo/bar2.scm" "foo/bar3.scm")
                      '()))
  (test* "precomp generated files"
         '("test.o/foo--bar1.c"
           "test.o/foo--bar2.c"
           "test.o/foo--bar3.c"
           "test.o/foo.c"
           "test.o/foo.sci"
           "test.o/foo/bar1.sci"
           "test.o/foo/bar2.sci"
           "test.o/foo/bar3.sci")
         (sort (map fix-path (directory-fold "test.o" cons '()))))
  )

(add-load-path "test.o")
(use srfi.42)
(use scheme.vector :only (vector-every))

(define (precomp-test-2)
  (define (literal=? x y)
    (cond [(pair? x) (and (pair? y)
                          (literal=? (car x) (car y))
                          (literal=? (cdr x) (cdr y)))]
          [(vector? x) (and (vector? y)
                            (= (vector-length x) (vector-length y))
                            (vector-every literal=? x y))]
          [(uvector? x) (and (eqv? (class-of x) (class-of y))
                             (= (uvector-length x) (uvector-length y))
                             (every?-ec (:parallel (: ex x) (: ey y))
                                        (literal=? ex ey)))]
          [(number? x)
           (and (number? y)
                (cond [(nan? x) (nan? y)]
                      [(negative-zero? x) (negative-zero? y)]
                      [else (= x y)]))]
          [else (equal? x y)]))

  (test* "running precomp 2" #t
         (do-precomp! '("foo.scm" "foo/bar1.scm" "foo/bar2.scm" "foo/bar3.scm")
                      '("--single-interface")))
  (test* "precomp generated files with --single-interface"
         '("test.o/foo--bar1.c"
           "test.o/foo--bar2.c"
           "test.o/foo--bar3.c"
           "test.o/foo.c"
           "test.o/foo.sci")
         (sort (map fix-path (directory-fold "test.o" cons '()))))

  (test*/diff "consolidated sci file"
              '(";; generated automatically.  DO NOT EDIT"
                "#!no-fold-case"
                "(define-module foo.bar2 (use util.match) (export bar2))"
                "(select-module foo.bar2)"
                "(dynamic-load \"foo\" :init-function \"Scm_Init_foo__bar2\")"
                "(provide \"foo/bar2\")"
                "(define-module foo.bar3 (use foo.bar2) (export bar3))"
                "(select-module foo.bar3)"
                "(dynamic-load \"foo\" :init-function \"Scm_Init_foo__bar3\")"
                "(provide \"foo/bar3\")"
                "(define-module foo.bar1 (use foo.bar2) (export bar1))"
                "(select-module foo.bar1)"
                "(dynamic-load \"foo\" :init-function \"Scm_Init_foo__bar1\")"
                "(provide \"foo/bar1\")"
                "(define-module foo (use gauche.uvector) (use foo.bar1) (use foo.bar3) (export foo-master foo-literals foo-shared-literals foo-begin1 foo-begin2) (export foo-include1 foo-include2))"
                "(select-module foo)"
                "(dynamic-load \"foo\" :init-function \"Scm_Init_foo\")")
              (file->string-list "test.o/foo.sci"))

  (test* "compile 2" #t
         (do-compile! "foo"
                      '("foo.c" "foo--bar1.c" "foo--bar2.c" "foo--bar3.c")))
  (test* "dynload and literals 1"
         (list (include "test-precomp/literals.scm")
               'begin1
               'begin2
               'include1
               'include2)
         (dynload-and-eval
          "foo"
          (list ((module-binding-ref 'foo 'foo-literals))
                ((module-binding-ref 'foo 'foo-begin1))
                ((module-binding-ref 'foo 'foo-begin2))
                ((module-binding-ref 'foo 'foo-include1))
                ((module-binding-ref 'foo 'foo-include2))))
         literal=?)

  (test* "literal sharing" '(#t #t #t #t #t #t)
         (dynload-and-eval
          "foo"
          (let1 vs ((module-binding-ref 'foo 'foo-shared-literals))
            (list
             ;; See if partial lists are shared
             (eq? (cdr (assq-ref vs 'list1))
                  (assq-ref vs 'list2))
             (eq? (cdr (assq-ref vs 'list2))
                  (assq-ref vs 'list3))
             ;; See if vectors are shared
             (eq? (assq-ref vs 'vec1)
                  (assq-ref vs 'vec2))
             (eq? (assq-ref vs 'uvec1)
                  (assq-ref vs 'uvec2))
             ;; See if strings are shared
             (eq? (assq-ref vs 'str1)
                  (assq-ref vs 'str2))
             ;; Descriptive types
             (eq? (assq-ref vs 'dtype1)
                  (assq-ref vs 'dtype2))
             ))))
  )

(define (precomp-test-3)
  (test* "running precomp 3" #t (do-precomp! '("types-test.scm") '("-e")))
  (test* "compile 3" #t (do-compile! "types-test" '("types-test.c")))

  (test* "type reconstruction" '(#t #t #f #t)
         (dynload-and-eval
          "types-test"
          (list
           (eq? (module-binding-ref 'types-test '<A>)
                (</> (<Tuple> (<?> <int8>) <string> <integer>)
                     (<List> <integer> 3 10)))
           (eq? (module-binding-ref 'types-test '<A>)
                (module-binding-ref 'types-test '<B>))
           (eq? (module-binding-ref 'types-test '<A>)
                (module-binding-ref 'types-test '<C>))
           ;; NB: The following may not be eq?, since (<List> <integer>) is
           ;; serialized as (<List> <integer> #f #f).  The memoization is
           ;; based on the argument list of the type constructor, so it
           ;; doesn't match with (<List> <integer>) at runtime.
           (equal? (module-binding-ref 'types-test '<C>)
                   (</> (<Tuple> (<?> <int8>) <string> <integer>)
                        (<List> <integer>)))
           )))

  (test* "assertion with reconstructed type" '((3 "ok" 8483958394)
                                               (#f "ok" -8483958394)
                                               (1 2 3 4 5))
         (dynload-and-eval
          "types-test"
          (list
           ((module-binding-ref 'types-test 'foo) '(3 "ok" 8483958394))
           ((module-binding-ref 'types-test 'foo) '(#f "ok" -8483958394))
           ((module-binding-ref 'types-test 'foo) '(1 2 3 4 5)))))

  (test* "assertion with reconstructed type"
         #t
         (dynload-and-eval
          "types-test"
          (guard (e ((<error> e)
                     (boolean (#/supposed to be of type/ (~ e'message)))))
            ((module-binding-ref 'types-test 'foo) "ng"))))
  )

(define (precomp-test-4)
  (test* "running precomp 4" #t (do-precomp! '("literal-mutex.scm") '("-e")))
  (test* "compile 4" #t (do-compile! "literal-mutex" '("literal-mutex.c")))

  (test* "run-once" '(1 1 1 1 1)
         (dynload-and-eval
          "literal-mutex"
          (let* ([foo (module-binding-ref 'literal-mutex 'foo)]
                 [thrs (map (^_ (make-thread foo)) (iota 5))])
            (for-each thread-start! thrs)
            (map thread-join! thrs))))
  )

(define (precomp-test-5)
  (test* "running precomp 5" #t (do-precomp! '("macros.scm") '("-e")))
  (test* "compile 5" #t (do-compile! "macros" '("macros.c")))

  (test* "compiled macro"
         '(("compiled macro (er)" ((apple) (banana) bonk))
           ("compiled macro (id-macro)" (#f 1 2)))
         (dynload-and-eval
          "macros"
          (begin
            (load (build-path *top-srcdir* "test" "test-precomp" "macros-user"))
            `(("compiled macro (er)"
               ,(map er-aif-test '(apple banana dragonfruit)))
              ("compiled macro (id-macro)"
               ,(im-state-test))))))
  )

(wrap-with-test-directory precomp-test-1 '("test.o"))
(wrap-with-test-directory precomp-test-2 '("test.o"))
(wrap-with-test-directory precomp-test-3 '("test.o"))
(wrap-with-test-directory precomp-test-4 '("test.o"))
(wrap-with-test-directory precomp-test-5 '("test.o"))

;;=======================================================================
(test-section "build-standalone")

(test-script
 (build-path (or (sys-getenv "top_srcdir") "..") "lib" "tools" "build-standalone"))

(define (run-build-static opts args)
  (do-process `("../../src/gosh" "-ftest"
                ,(build-path *top-srcdir* "lib/tools/build-standalone")
                ,#"-I=~|*top-srcdir*|/test/test-static"
                ,#"--header-dir=~|*top-srcdir*|/src"
                ,#"--header-dir=~|*top-builddir*|/src"
                ,#"--header-dir=~|*top-srcdir*|/gc/include"
                ,#"--library-dir=~|*top-builddir*|/src"
                ,@opts
                ,@args)
              :directory "test.o"))

(define (static-test-1)
  (test* "static link test" #t
         (run-build-static `("-o" "staticmain"
                             "-I" ,(build-path *top-srcdir* "test/test-static"))
                           `(,(fix-path (build-path *top-srcdir*
                                                    "test/test-static/main.scm"))
                             "foo/bar.scm"
                             "foo/bar-impl.scm"
                             "foo/baz.scm")))
  (test* "static link executable" "ARGS: #(A b CdE)"
         (process-output->string '(test.o/staticmain A b CdE)))
  )

(wrap-with-test-directory static-test-1 '("test.o"))

;;=======================================================================
(test-section "test-script")

;; Test compile-only option

(define (test-script-test-1)
  (with-output-to-file "test.o/script"
    (^[]
      (write `(use gauche.uvector))
      (write `(print "bar"))
      (write `(define (bar) (foo) (foo)))
      (write `(print "foo"))
      (write `(define (foo)
                (with-output-to-file "test.o/out.o"
                  (cut print (u8vector 1 2 3 4 5)))))
      (write `(bar))))
  (test-script "./test.o/script" :compile-only #t)
  (test* "not executed" #f (file-exists? "test.o/out.o"))

  (test-script "./test.o/script")
  (test* "executed" #t (file-exists? "test.o/out.o")))

(wrap-with-test-directory test-script-test-1 '("test.o") #t)

(test-end)
