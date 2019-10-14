;;;
;;; configure.scm - configuring Gauche extensions
;;;
;;;   Copyright (c) 2013-2019  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;; This is a utility library to write 'configure' script, a replacement
;; of autotool-generated 'configure' shell script.  See ext/template.configure
;; for an example.
;;
;; The biggest advantage of using autotool's 'configure' is that it runs
;; on most vanilla unix, for it only uses minimal shell features and
;; basic unix commands.   However, when you configure Gauche extension,
;; you sure have Gauche already.  Why not use full power of Gauche
;; to do the configuration work?
;;
;; If we use Gauche to write 'configure', we no longer need an extra step
;; to generate configure from configure.ac, for the interpreter (gosh)
;; is powerful enough to allow extension writers to do any abstraction
;; he needs.   So the author can check in 'configure' script itself
;; to the source tree, and anyone who checks it out can directly run
;; ./configure, without worrying running autoconf (and free from headache
;; of autoconf version mismatch)
;;
;; The core feature of gauche.configure is the ability to generate files
;; (e.g. Makefile) from templates (e.g. Makefile.in) with replacing
;; parameters.  We follow autoconf convention, so the replacement variables
;; in a template is written like @VAR@.
;;
;; The API is roughly corresponds to autoconf's AC_* macros, while we use
;; 'cf-' suffix instead.
;;
;; The simplest configure scripts can be just the following 3 expressions:
;;
;;  (use gauche.configure)
;;  (cf-init-gauche-extension)
;;  (cf-output-default)
;;
;; It takes package name and version from package.scm file, sets several
;; substitution variables, and creates Makefile from Makefile.in along
;; the gpd (Gauche package description) file.

;; TODO: Caching test results

(define-module gauche.configure
  (use gauche.parameter)
  (use gauche.generator)
  (use gauche.dictionary)
  (use gauche.parseopt)
  (use gauche.logger)
  (use gauche.cgen)
  (use gauche.package)
  (use gauche.process)
  (use gauche.version)
  (use gauche.mop.singleton)
  (use util.match)
  (use file.filter)
  (use file.util)
  (use text.tr)
  (use text.tree)
  (use srfi-13)
  (use srfi-113) ; sets & bags
  (extend gauche.config)
  (export cf-init cf-init-gauche-extension
          cf-arg-enable cf-arg-with cf-feature-ref cf-package-ref
          cf-help-string
          cf-msg-checking cf-msg-result cf-msg-warn cf-msg-error cf-msg-notice
          cf-echo
          cf-make-gpd
          cf-define cf-subst cf-subst-append cf-subst-prepend
          cf-arg-var cf-have-subst? cf-ref cf$
          with-cf-subst
          cf-config-headers cf-output cf-output-default cf-show-substs
          cf-check-prog cf-path-prog cf-check-tool
          cf-prog-cxx

          cf-lang <c-language>
          cf-lang-program cf-lang-io-program cf-lang-call
          cf-try-compile cf-try-compile-and-link

          cf-check-header cf-check-headers cf-includes-default
          cf-check-type cf-check-types
          cf-check-decl cf-check-decls
          cf-check-member cf-check-members
          
          cf-check-func cf-check-funcs
          cf-check-lib cf-search-libs
          ))
(select-module gauche.configure)

;; A package
(define-class <package> ()
  ((name       :init-keyword :name)
   (version    :init-keyword :version)
   (bug-report :init-keyword :bug-report :init-value #f) ; email addr
   (url        :init-keyword :url :init-value #f)
   (gpd        :init-keyword :gpd)       ; <gauche-package-description>
   (string     :init-keyword :string)    ; package_string
   (tarname    :init-keyword :tarname)
   (tool-prefix :init-form #f)           ; cross compilation tool prefix
   (config.h   :init-form '())           ; list of (config-header . source)
   (defs       :init-form (make-hash-table 'eq?)) ;cf-define'd thingy
   (substs     :init-form (make-hash-table 'eq?)) ;cf-subst'ed thingy
   (precious   :init-form (set eq-comparator))    ;vars overridable by env
   (features   :init-form (make-hash-table 'eq?)) ;enabled features by cmdarg
   (packages   :init-form (make-hash-table 'eq?)) ;optional packages
   ))

(define current-package (make-parameter #f))

(define run-quietly (make-parameter #f)) ;-silent or -quiet option turn this on

;; Alist of arg processors and help strings given to cf-arg-with and
;; cf-arg-enable.
;; Each element is
;; (<name> <kind> <help-string> <proc-if-given> <proc-if-not-given>)
(define arg-processors (make-parameter '()))

;; some internal utilities

(define (listify x) (if (list? x) x (list x)))

(define (ensure-package)
  (or (current-package)
      (error "No current package - cf-init hasn't been called")))

(define (tee-msg console-fmt log-fmt args)
  (apply format #t console-fmt args)
  (apply log-format log-fmt args))

(define (safe-variable-name s)
  (string-tr (string-upcase s) "A-Z0-9" "_*" :complement #t))

;; constants for formatting help strings.  see cf-help-string.
(define-constant *usage-description-indent* 26)
(define-constant *usage-item-indent* 2)
(define-constant *usage-fill-column* 79)

;;;
;;; Basic APIs
;;; 

;; API
;; Like AC_MSG_*
(define (cf-msg-checking fmt . args)
  (tee-msg #"checking ~|fmt|... " #"checking: ~fmt" args))
(define (cf-msg-result fmt . args)
  (tee-msg #"~|fmt|\n" #"result: ~fmt" args))
(define (cf-msg-warn fmt . args)
  (tee-msg #"Warning: ~|fmt|\n" #"Warning: ~fmt" args))
(define (cf-msg-error fmt . args)
  (tee-msg #"Error: ~|fmt|\n" #"Error: ~fmt" args)
  (exit 1))
(define (cf-msg-notice fmt . args)
  (tee-msg #"~|fmt|\n" #"~|fmt|\n" args))

;; API
;; Convenience routine for substitute of shell's echo
;; e.g.  (cf-echo "something" > "FILE")
;; or    (cf-echo "something" >> "FILE")
;; The destination, '>' or '>>' followed by a filename, must be at the end
;; of arglist if any.  If no destination is given, output goes to the current
;; output port.
(define-macro (cf-echo . args)
  (match (take-right* args 2)
    [('> name)
     `(with-output-to-file ,name
        (cut print ,@(intersperse " " (drop-right* args 2)))
        :if-exists :supersede)]
    [('>> name)
     `(with-output-to-file ,name
        (cut print ,@(intersperse " " (drop-right* args 2)))
        :if-exists :append)]
    [_ `(,tee-msg "~a\n" "Message: ~a" (list (string-join (list ,@args))))]))

;; API
;; Like AC_INIT; package-name and version can be omitted if the package
;; has "package.scm".
(define (cf-init :optional (package-name #f) (version #f)
                           (bug-report #f) (url #f))
  (when (current-package)
    (exit 1 "Incorrect configure script: cf-init is called more than once."))
  (let* ([gpd (and-let* ([srcdir (current-load-path)]
                         [pfile (build-path (sys-dirname srcdir) "package.scm")]
                         [ (file-exists? pfile) ])
                (path->gauche-package-description pfile))]
         ;; We need to support old way (package info given to cf-init) and
         ;; new way (package info taken from package.scm).  If both are
         ;; given, they must much, for the inconsistency would likely be
         ;; an overlook during transition.  bug-report and url is needed
         ;; for the compatibility with autoconf (PACKAGE_BUGREPORT and
         ;; PACKAGE_URL substitution variable).
         [package-name (if gpd
                         (if package-name
                           (if (equal? package-name (~ gpd'name))
                             package-name
                             (errorf "Package name in package.scm (~s) and \
                                      cf-init (~s) don't match"
                                     (~ gpd'name) package-name))
                           (~ gpd'name))
                         (or package-name
                             (error "Missing package name in cf-init \
                                     (required when package.scm is not present)")))]
         [version (if gpd
                    (if version
                      (if (equal? version (~ gpd'version))
                        version
                        (errorf "Version in package.scm (~s) and \
                                 cf-init (~s) don't match"
                                (~ gpd'version) version))
                      (~ gpd'version))
                    (or version
                        (error "Missing version in cf-init \
                               (required when package.scm is not present)")))]
         [bug-report (if gpd
                       (let1 ms (~ gpd'maintainers)
                         (if bug-report
                           (begin
                             (unless (or (null? ms)
                                         (and (null? (cdr ms))
                                              (equal? (car ms) bug-report)))
                               (errorf "Maintainer in package.scm ~s and \
                                        cf-init bug-report argument ~s don't \
                                        match" ms bug-report))
                             bug-report)
                           (if (null? ms) #f (car ms))))
                       bug-report)]
         [url (if gpd
                (let1 x (~ gpd'homepage)
                  (if url
                    (begin (unless (equal? url x)
                             (errorf "Homepage in package.scm ~s and \
                                      cf-init url argument ~s don't match"
                                     x url))
                           url)
                    x))
                url)]
         )
    (assume-type package-name <string>)
    (assume-type version <string>)
    (sys-unlink "config.log")
    (log-open "config.log" :prefix "")
    (log-format "Configuring ~a ~a" package-name version)
    (current-package
     (make <package>
       :name package-name
       :version version
       :bug-report bug-report
       :url url
       :gpd (or gpd
                ($ make-gauche-package-description package-name
                   :version version :homepage url
                   :maintainers (if bug-report (list bug-report) '())))
       :string (format "~a ~a" package-name version)
       :tarname (cgen-safe-name-friendly (string-downcase package-name))))
    (cf-lang (instance-of <c-language>))
    (initialize-default-definitions)
    (parse-command-line-arguments)
    (check-directory-names)
    (process-args)
    (check-requirements)))

(define (initialize-default-definitions)
  (define p (current-package))
  (define (S s) (and s (write-to-string s)))
  (cf-subst 'PACKAGE_NAME    (~ p'name))
  (cf-subst 'PACKAGE_TARNAME (string-tr (string-downcase (~ p'name))
                                        "a-z0-9_-" "_*" :complement #t))
  (cf-subst 'PACKAGE_VERSION (~ p'version))
  (cf-subst 'PACKAGE_STRING (~ p'string))
  (cf-subst 'PACKAGE_BUGREPORT (~ p'bug-report))
  (cf-subst 'PACKAGE_URL (~ p'url))
  (cf-define 'PACKAGE_NAME    (S (~ p'name)))
  (cf-define 'PACKAGE_TARNAME (S (string-tr (string-downcase (~ p'name))
                                            "a-z0-9_-" "_*" :complement #t)))
  (cf-define 'PACKAGE_VERSION (S (~ p'version)))
  (cf-define 'PACKAGE_STRING (S (~ p'string)))
  (cf-define 'PACKAGE_BUGREPORT (S (~ p'bug-report)))
  (cf-define 'PACKAGE_URL (S (~ p'url)))

  (cf-subst 'SHELL (or (sys-getenv "CONFIG_SHELL") "/bin/sh"))
  (cf-subst 'LIBOBJS "")
  (cf-subst 'MFLAGS "")
  (cf-subst 'MAKEFLAGS "")

  (cf-subst 'default_prefix "/usr/local")
  (cf-subst 'prefix "NONE")       ;will be replaced by cf-output
  (cf-subst 'exec_prefix "NONE")  ;will be replaced by cf-output
  (cf-subst 'bindir "${exec_prefix}/bin")
  (cf-subst 'sbindir "${exec_prefix}/sbin")
  (cf-subst 'libexecdir "${exec_prefix}/libexec")
  (cf-subst 'datarootdir "${prefix}/share")
  (cf-subst 'datadir "${datarootdir}")
  (cf-subst 'sysconfdir "${prefix}/etc")
  (cf-subst 'sharedstatedir "${prefix}/com")
  (cf-subst 'localstatedir "${prefix}/var")
  (cf-subst 'includedir "${prefix}/include")
  (cf-subst 'oldincludedir "/usr/include")
  (cf-subst 'docdir "${datarootdir}/doc/${PACKAGE_TARNAME}")
  (cf-subst 'infodir "${datarootdir}/info")
  (cf-subst 'htmldir "${docdir}")
  (cf-subst 'dvidir "${docdir}")
  (cf-subst 'pdfdir "${docdir}")
  (cf-subst 'psdir "${docdir}")
  (cf-subst 'libdir "${exec_prefix}/lib")
  (cf-subst 'localedir "${datarootdir}/locale")
  (cf-subst 'mandir "${datarootdir}/man")

  (cf-subst 'cross_compiling "no")
  (cf-subst 'subdirs "")

  ;; NB: Autoconf uses AC_PROG_CC to set up CC.  However, we need
  ;; to use the same C compiler with which Gauche was compiled, so
  ;; so we set it as the default.  We also allow env overrides for
  ;; some common variables.  (In autoconf, AC_PROG_CC issues AC_ARG_VAR
  ;; for them).
  (cf-subst 'CC       (gauche-config "--cc"))
  (cf-arg-var 'CPP)
  (cf-arg-var 'CPPFALGS)
  (cf-arg-var 'CC)
  (cf-arg-var 'CFLAGS)
  (cf-arg-var 'LDFLAGS)
  (cf-arg-var 'LIBS)
  ;; NB: Autoconf detemines these through tests, but we already
  ;; know them at the time Gauche is configured.
  (cf-subst 'SOEXT  (gauche-config "--so-suffix"))
  (cf-subst 'OBJEXT (gauche-config "--object-suffix"))
  (cf-subst 'EXEEXT (gauche-config "--executable-suffix"))
  )

(define (parse-command-line-arguments)
  (let1 rest
      (parse-options (cdr (command-line))
        (["bindir=s" (dir) (cf-subst 'bindir dir)]
         ["build=s" (build) (cf-subst 'build_alias build)]
         ["c|cache-file=s" (_)
          (values)] ;; support for the compatibility.  we don't do anything.
         ["C|config-cache" (_) (values)]
         ["datadir=s" (dir) (cf-subst 'datadir dir)]
         ["datarootdir=s" (dir) (cf-subst 'datarootdir dir)]
         ["docdir=s" (dir) (cf-subst 'docdir dir)]
         ["dvidir=s" (dir) (cf-subst 'dvidir dir)]
         ;; --disable-option-checking
         ["exec_prefix=s" (pre) (cf-subst 'exec_prefix pre)]
         ["help" () (usage)]
         ["host=s" (host) (cf-subst 'host_alias host)]
         ["htmldir=s" (dir) (cf-subst 'htmldir dir)]
         ["includedir=s" (dir) (cf-subst 'includedir dir)]
         ["infodir=s" (dir) (cf-subst 'infodir dir)]
         ["libdir=s" (dir) (cf-subst 'libdir dir)]
         ["libexecdir=s" (dir) (cf-subst 'libexecdir dir)]
         ["localedir=s" (dir) (cf-subst 'localedir dir)]
         ["localstatedir=s" (dir) (cf-subst 'localstatedir dir)]
         ["mandir=s" (dir) (cf-subst 'mandir dir)]
         ;; -no-create
         ;; -no-recursion
         ;; -oldincludedir
         ["prefix=s" (dir) (cf-subst 'prefix dir)]
         ["program-suffix=s" (arg) (cf-subst 'program_suffix arg)]
         ["program-transform-name=s" (arg)
          (cf-subst 'program_transform_name arg)]
         ["pdfdir=s" (arg) (cf-subst 'pdfdir arg)]
         ["psdir=s" (arg) (cf-subst 'psdir arg)]
         ["q|quiet|silent" () (run-quietly #t)]
         ["sbindir=s" (arg) (cf-subst 'sbindir arg)]
         ["sharedstatedir=s" (arg) (cf-subst 'sharedstatedir arg)]
         ["site=s" (arg) (cf-subst 'site arg)]
         ["srcdir=s" (arg) (cf-subst 'srcdir arg)]
         ["sysconfdir=s" (arg) (cf-subst 'sysconfdir arg)]
         ["target=s" (arg) (cf-subst 'target_alias arg)]
         ["v|verbose" () (run-quietly #f)]
         ["V|version" ()
          (format #t "~a configure ~a\n"
                  (~ (current-package)'name)
                  (~ (current-package)'version))
          (exit 0)]
         ;; -x-includes
         ;; -x-libraries
         
         ;; Deal with --enable|--disable|--with|--without options.
         ;; Note that parse-options split '=arg' part of --enable-*=arg or
         ;; --with-*=arg; if it is given, it will be in (car rest).
         ;; maybe-get-arg gets it.
         [else (opt rest cont)
               (let ([maybe-get-arg (^[] (and (pair? rest)
                                              (not (#/^-/ (car rest)))
                                              (pop! rest)))])
                 (rxmatch-case opt
                   [#/^enable-([-\w]+)$/ (_ feature)
                    (set! (cf-feature-ref (string->symbol feature))
                          (or (maybe-get-arg) "yes"))
                    (cont rest)]
                   [#/^disable-([-\w]+)$/ (_ feature)
                    (set! (cf-feature-ref (string->symbol feature)) "no")
                    (cont rest)]
                   [#/^with-([-\w]+)?$/ (_ package)
                    (set! (cf-package-ref (string->symbol package))
                          (or (maybe-get-arg) "yes"))
                    (cont rest)]
                   [#/^without-([-\w]+)$/ (_ package)
                    (set! (cf-package-ref (string->symbol package)) "no")
                    (cont rest)]
                   [else
                    (print "Unrecognized option: " opt)
                    (print "Type `./configure --help' for valid options.")
                    (exit 1)]
                   ))]))
    ;; process VAR=VAL
    (dolist [arg rest]
      (rxmatch-case arg
        [#/^([-\w]+)=(.*)$/ (_ var val)
         (cf-subst (string->symbol var) val)]
        [else (print "Invalid argument: " arg)
              (print "Type `./configure --help' for usage.")
              (exit 1)]))
    ))

(define (check-directory-names)
  ;; --bindir etc. must have absolute pathnames, and no trailing slash
  (dolist [var '(exec_prefix prefix bindir sbindir libexecdir datarootdir
                 datadir sysconfdir sharedstatedir localstatedir includedir
                 oldincludedir docdir infodir htmldir dvidir pdfdir psdir
                 libdir localedir mandir)]
    (let1 val (cf$ var)
      (when (string-suffix? "/" val)
        (cf-subst var (string-trim-right val #[/])))
      (unless (or (absolute-path? val)
                  (string-prefix? "$" val)
                  (and (memq var '(prefix exec_prefix))
                       (member val '("NONE" ""))))
        (exit 1 "absolute directory name required for --~a but got: ~a"
              var val))))
  ;; setup srcdir and builddir
  (unless (cf-have-subst? 'srcdir)
    (cf-subst 'srcdir (sys-dirname (car (command-line)))))
  (cf-subst 'top_srcdir (cf$ 'srcdir))
  (unless (cf-have-subst? 'builddir)
    (cf-subst 'builddir "."))
  (cf-subst 'top_builddir (cf$ 'builddir))
  )

(define (process-args)
  (dolist [entry (arg-processors)]
    (match entry
      [(name kind help-string arg-if-given arg-if-not-given)
       (let1 val (ecase kind
                   [(enable) (cf-feature-ref name)]
                   [(with) (cf-package-ref name)])
         (if val
           (arg-if-given val)
           (arg-if-not-given)))]
      [_ #f])))

(define (check-requirements)
  ;; Returns (<ok> <found-version>)
  (define (check-1 package spec)
    (if-let1 vers (if (equal? package "Gauche")
                    (gauche-version)
                    (and-let1 gpd (find-gauche-package-description package)
                      (~ gpd'version)))
      (if (version-satisfy? spec vers)
        `(#t ,vers)
        `(#f ,vers))
      '(#f #f)))
  (define (check-all reqs)
    (dolist [req reqs]
      (match (apply check-1 req)
        [(#t vers)
         (log-format "package ~a, required to be ~s... found ~s, ok."
                     (car req) (cadr req) vers)]
        [(#f #f)
         (cf-msg-error "Unfulfilled dependency of package ~a: \
                        required to be ~s, but not found."
                       (car req) (cadr req))]
        [(#f vers)
         (cf-msg-error "Unfulfilled dependency of package ~a: \
                        required to be ~s, but only found ~s."
                       (car req) (cadr req) vers)])))
  
  (and-let* ([reqs (~ (current-package)'gpd'require)]
             [ (not (null? reqs)) ])
    (cf-msg-checking "package dependencies")
    (check-all reqs)
    (cf-msg-result "ok")))

(define (usage)
  (define p print)
  (define (opt a b) (display (cf-help-string a b)))
  (p "Usage: "(car (command-line))" args ... [var=value ...]")
  (p "  Generate Makefiles and other files suitable for your system.")
  (p)
  (p "Configuration options:")
  (opt "-h, --help" "display this help and exit")
  (opt "-V, --version" "display version information and exit")
  (opt "-q, --quiet, --silent" "suppress messages")
  (opt "-C, --config-cache, --cache-file=FILE"
      "ignored for the compatibility to autoconf-generated configure")
  (opt "--srcdir=DIR" "find the sources in DIR")
  (p)
  (p "Installation directories:")
  (opt "--prefix=PREFIX"
      "install architecture-independent files in PREFIX")
  (opt "--exec-prefix=EPREFIX"
      "install architecture-dependent files in PREFIX")
  (p)
  (p "Most of the time, giving --prefix and/or --exec-prefix is enough")
  (p "to install stuff in one location.  You can fine tune installation")
  (p "directories for different kind of files with the options below:")
  (p)
  (opt "--bindir=DIR" "user executables [EPREFIX/bin]")
  (opt "--sbindir=DIR" "system admin executables [EPREFIX/bin]")
  (opt "--libexecdir=DIR" "program executables [EPREFIX/libexec]")
  (opt "--sysconfdir=DIR" "read-only single-machine data [PREFIX/etc]")
  (opt "--sharedstatedir=DIR"
       "modifiable architecture-independed data [PREFIX/com]")
  (opt "--localstatedir=DIR"
       "modifiable single-machine data [PREFIX/var]")
  (opt "--libdir=DIR" "object code libraries [EPREFIX/lib]")
  (opt "--includedir=DIR" "C header files [PREFIX/include]")
  (opt "--datarootdir=DIR"
       "read-only architecture-independent data root [PREFIX/share]")
  (opt "--datadir=DIR"
       "read-only architecture-independent data root [DATAROOTDIR]")
  (opt "--infodir=DIR" "info documentation [DATAROOTDIR/info]")
  (opt "--localedir=DIR" "locale-dependent data [DATAROOTDIR/locale]")
  (opt "--mandir=DIR" "manual pages [DATAROOTDIR/man]")
  (opt "--docdir=DIR"
       #"documentation root [DATAROOTDIR/doc/~(cf$ 'PACKAGE_TARNAME)]")
  (opt "--htmldir=DIR" "html documentation [DOCDIR]")
  (opt "--dvidir=DIR" "dvi documentation [DOCDIR]")
  (opt "--pdfdir=DIR" "pdf documentation [DOCDIR]")
  (opt "--sdir=DIR" "ps documentation [DOCDIR]")
  (p)
  ;; Add --build, --host, --target after checking cross compilation and
  ;; default setting (should be taken from GOSH settings)
  (p "Optional Features:")
  (opt "--disable-FEATURE"
       "do not include FEATURE (same as --enable-FEATURE=no)")
  (opt "--enable-FEATURE[=ARG]" "include FEATURE [ARG=yes]")
  (dolist [e (arg-processors)]
    (match-let1 (name kind help-string _ _) e
      (when (eq? kind 'enable)
        (display help-string))))
  (p)
  (p "Optional Packages:")
  (opt "--with-PACKAGE[=ARG]" "use PACKAGE [ARG=yes]")
  (opt "--without-PACKAGE" "do not use PACKAGE (same as --with-PACKAGE=no)")
  (dolist [e (arg-processors)]
    (match-let1 (name kind help-string _ _) e
      (when (eq? kind 'with)
        (display help-string))))
  ;; TODO: explanation of VAR=VAL
  (exit 1))

;; API
;; cf-init-gauche-extension packages common stuff around cf-init.
;;
(define (cf-init-gauche-extension)
  (cf-arg-with 'local
               (cf-help-string
                "--with-local=PATH:PATH..."
                "For each PATH, add PATH/include to the include search
  paths and PATH/lib to the library search paths.  Useful if you have some
  libraries installed in non-standard places. ")
               (^[with-local]
                 (unless (member with-local '("yes" "no" ""))
                   (cf-subst 'LOCAL_PATHS with-local)))
               (^[] (cf-subst 'LOCAL_PATHS "")))
  (cf-init)
  (cf-path-prog 'GOSH            "gosh")
  (cf-path-prog 'GAUCHE_CONFIG   "gauche-config")
  (cf-path-prog 'GAUCHE_PACKAGE  "gauche-package")
  (cf-path-prog 'GAUCHE_INSTALL  "gauche-install")
  (cf-path-prog 'GAUCHE_CESCONV  "gauche-cesconv")

  (cf-subst 'default_prefix (gauche-config "--prefix"))
 
  (cf-subst 'GAUCHE_PKGINCDIR  (gauche-config "--pkgincdir"))
  (cf-subst 'GAUCHE_PKGLIBDIR  (gauche-config "--pkglibdir"))
  (cf-subst 'GAUCHE_PKGARCHDIR (gauche-config "--pkgarchdir"))
  )

;;;
;;; Variables and substitutions
;;;

;; API
(define (cf-define symbol :optional (value 1))
  (assume-type symbol <symbol>)
  (dict-put! (~ (ensure-package)'defs) symbol value))

;; API
;; Like AC_SUBST, but we require value (instead of implicitly referencing
;; a global variable.
(define (cf-subst symbol value)
  (assume-type symbol <symbol>)
  (dict-put! (~ (ensure-package)'substs) symbol value))

;; API
(define (cf-subst-prepend symbol value :optional (delim " ") (default ""))
  (let1 v (cf-ref symbol default)
    (if (equal? v "")
      (cf-subst symbol value)
      (cf-subst symbol #"~|value|~|delim|~|v|"))))

;; API
(define (cf-subst-append symbol value :optional (delim " ") (default ""))
  (let1 v (cf-ref symbol default)
    (if (equal? v "")
      (cf-subst symbol value)
      (cf-subst symbol #"~|v|~|delim|~|value|"))))

;; API
(define (cf-have-subst? symbol)
  (assume-type symbol <symbol>)
  (dict-exists? (~ (ensure-package)'substs) symbol))

;; API
;; Make the named variable overridable by the environment variable.
;; The term "precious" comes from autoconf implementation.
;; At this moment, we don't save the help string.  cf-arg-var is
;; called after cf-init and we can't include help message (the limitation
;; of one-pass processing.)
(define (cf-arg-var symbol)
  (assume-type symbol <symbol>)
  (update! (~ (ensure-package)'precious) (cut set-adjoin! <> symbol))
  (and-let1 v (sys-getenv (x->string symbol))
    (cf-subst symbol v)))

(define (var-precious? symbol)
  (assume-type symbol <symbol>)
  (set-contains? (~ (ensure-package)'precious) symbol))

;; API
;; Lookup the current value of the given variable.
(define (cf-ref symbol :optional (default (undefined)))
  (assume-type symbol <symbol>)
  (rlet1 v (dict-get (~ (ensure-package)'substs) symbol default)
    (when (undefined? v)
      (errorf "Configure variable ~s is not defined." symbol))))

;; API
;; Like cf-ref, but returns empty string if undefined.
(define (cf$ symbol) (cf-ref symbol ""))

;; API
;; Temporarily replace cf subst value
(define-syntax with-cf-subst 
  (syntax-rules ()
    [(_ ((var val) ...) body ...)
     (let ([saves (map (cut cf$ <>) '(var ...))])
       (for-each (cut cf-subst <> <>)
                 '(var ...)
                 (list val ...))
       (unwind-protect (begin body ...)
         (for-each (cut cf-subst <> <>)
                   '(var ...)
                   saves)))]))

;;;
;;; Argument processing
;;;

;; --with-PACKAGE and --enable-FEATURE processing
;; They're recoginzed by cf-init.
;;
;;  --enable-FEATURE=VAL
;;  --enable-FEATURE    == --enable-FEATURE=yes
;;  --disable-FEATURE   == --enable-FEATURE=no
;;
;;  --with-PACKAGE=VAL
;;  --with-PACKAGE      == --with-PACKAGE=yes
;;  --without-PACKAGE   == --with-PACKAGE=no
;;
;; In order to generate help strings, calls of cf-arg-enable and cf-arg-with
;; must precede cf-init.
;;
;; After cf-init, you can check the feature/package specified to the command
;; line by cf-feature-ref and cf-package-ref.

;; API
(define cf-feature-ref
  (getter-with-setter
   (^[name] (hash-table-get (~ (ensure-package)'features) name #f))
   (^[name val] (hash-table-put! (~ (ensure-package)'features) name val))))

;; API
(define cf-package-ref
  (getter-with-setter
   (^[name] (hash-table-get (~ (ensure-package)'packages) name #f))
   (^[name val] (hash-table-put! (~ (ensure-package)'packages) name val))))

;; API
(define (cf-arg-enable feature help-string
                       :optional (action-if-given (^[val] #f))
                                 (action-if-not-given (^[] #f)))
  (unless (symbol? feature)
    (errorf "cf-arg-enable: feature must be a symbol, but got ~s" feature))
  (push! (arg-processors)
         (list feature 'enable help-string
               action-if-given action-if-not-given)))
  
;; API
(define (cf-arg-with package help-string
                     :optional (action-if-given (^[val] #f))
                               (action-if-not-given (^[] #f)))
  (unless (symbol? package)
    (errorf "cf-arg-with: package must be a symbol, but got ~s" package))
  (push! (arg-processors)
         (list package 'with help-string
               action-if-given action-if-not-given)))

;; API
(define (cf-help-string item description)
  (define descr-indent (make-string *usage-description-indent* #\space))
  (define (fill paragraph)
    (let loop ([words (string-split description #[\s])]
               [column *usage-description-indent*]
               [r '()])
      (if (null? words)
        (string-concatenate (reverse r))
        (let* ([word (car words)]
               [word-len (string-length word)]
               [nextcol (+ column 1 word-len)])
          (if (< nextcol *usage-fill-column*)
            (loop (cdr words) nextcol (cons* word " " r))
            (loop (cdr words) (+ *usage-description-indent* word-len)
                  (cons* word descr-indent "\n" r)))))))
  ;; NB: 'fill' adds one space before the paragraph, hence -1
  (let1 item-len (string-length item)
    (if (< (+ item-len *usage-item-indent*) (- *usage-description-indent* 1))
      (format "~va~va~a\n" *usage-item-indent* " "
              (- *usage-description-indent* *usage-item-indent* 1) item
              (fill description))
      (format "~va~a\n~va~a\n" *usage-item-indent* " " item
              (- *usage-description-indent* 1) " " (fill description)))))

;;;
;;; Output
;;;

;; API
(define (cf-config-headers header-or-headers)
  (dolist [h (listify header-or-headers)]
    (match (string-split h #\:)
      [(out src) (push! (~(ensure-package)'config.h) (cons out src))]
      [(out) (push! (~(ensure-package)'config.h) (cons out #"~|out|.in"))]
      [_ (error "Invalid header name in cf-config-headers" h)])))

;; API
;; Like AC_OUTPUT
(define (cf-output . files)
  (define pa (ensure-package))
  (define base-substs (~ pa'substs))
  (define (make-defs)
    (if (null? (~ pa'config.h))
      (string-join (dict-map (~ pa'defs) (^[k v] #"-D~|k|=~|v|")) " ")
      "-DHAVE_CONFIG_H"))
  (define (make-subst path-prefix)
    (receive (srcdir top_srcdir builddir top_builddir)
        (adjust-srcdirs path-prefix)
      (let1 substs (make-stacked-map (alist->hash-table
                                      `((srcdir       . ,srcdir)
                                        (top_srcdir   . ,top_srcdir)
                                        (builddir     . ,builddir)
                                        (top_builddir . ,top_builddir)
                                        (DEFS . ,(make-defs)))
                                      'eq?)
                                     base-substs)
        (^[m]
          (let1 name (string->symbol (m 1))
            (or (dict-get substs name #f)
                (begin (warn "@~a@ isn't substituted." name)
                       #"@~|name|@")))))))
  ;; We use '/' in the replaced pathname even on Windows; that's what
  ;; autoconf-generated configure does, and it's less likely to confuse
  ;; Unix-originated tools.
  (define (simplify-path+ path)
    (cond-expand
     [gauche.os.windows (string-tr (simplify-path path) "\\\\" "/")]
     [else (simplify-path path)]))
  (define (adjust-srcdirs path-prefix)
    (let ([srcdir    (~ base-substs'srcdir)]
          [tsrcdir   (~ base-substs'top_srcdir)]
          [builddir  (~ base-substs'builddir)]
          [tbuilddir (~ base-substs'top_builddir)])
      (if (equal? path-prefix ".")
        (values srcdir tsrcdir builddir tbuilddir)
        (let1 revpath ($ apply build-path
                         $ map (^_ "..") (string-split path-prefix #[\\/]))
          (values (if (equal? srcdir ".")
                    srcdir
                    (simplify-path+ (build-path srcdir path-prefix)))
                  (simplify-path+ (build-path revpath tsrcdir))
                  (if (equal? builddir ".")
                    builddir
                    (simplify-path+ (build-path builddir path-prefix)))
                  (simplify-path+ (build-path revpath tbuilddir)))))))
                  
  (define (make-replace-1 output-file)
    (let1 subst (make-subst (sys-dirname (simplify-path+ output-file)))
      (^[line outp]
        (display (regexp-replace-all #/@(\w+)@/ line subst) outp)
        (newline outp))))

  (define (make-config.h)
    (^[line outp]
      (rxmatch-case line
        [#/^#undef\s+([A-Za-z_]+)/ (_ name)
         (if-let1 defval (dict-get (~ pa'defs) (string->symbol name) #f)
           (display #"#define ~name ~defval" outp)
           (display #"/* #undef ~name */" outp))]
        [else (display line outp)])
      (newline outp)))

  ;; Realize prefix and exec_prefix if they're not set.
  (when (equal? (cf$ 'prefix) "NONE")
    (cf-subst 'prefix (cf$ 'default_prefix)))
  (when (equal? (cf$ 'exec_prefix) "NONE")
    (cf-subst 'exec_prefix "${prefix}"))
  
  (dolist [f files]
    (let1 inf (build-path (cf$'srcdir) #"~|f|.in")
      (unless (file-is-readable? inf)
        (error "Cannot read input file ~s" inf))
      (unless (file-is-directory? (sys-dirname f))
        (make-directory* (sys-dirname f)))
      (cf-msg-notice "configure: creating ~a" f)
      (file-filter-for-each (make-replace-1 f) :input inf :output f
                            :temporary-file #t :leave-unchanged #t)))
  (dolist [h (~ pa'config.h)]
    (let1 inf (build-path (cf$'srcdir) (cdr h))
      (unless (file-is-readable? inf)
        (error "Cannot read input file ~s" inf))
      (unless (file-is-directory? (sys-dirname (car h)))
        (make-directory* (sys-dirname (car h))))
      (cf-msg-notice "configure: creating ~a" (car h))
      (file-filter-for-each (make-config.h) :input inf :output (car h)
                            :temporary-file #t :leave-unchanged #t)))
  )

;; API
;; Show definitions.
(define (cf-show-substs :key (formatter (^[k v] (format #t "~16s ~s" k v))))
  (let1 dict (~ (ensure-package)'substs)
    (dolist [k (sort (dict-keys dict)
                     (^[a b] (string<? (x->string a) (x->string b))))]
      (formatter k (dict-get dict k))
      (newline))))

;; API
;; Create .gpd file.  This is Gauche-specific.
(define (cf-make-gpd)
  (let ([gpd-file #"~(cf$ 'PACKAGE_NAME).gpd"]
        [gpd (~ (ensure-package)'gpd)])
    (cf-echo #"creating ~gpd-file")
    (set! (~ gpd'configure)
          ($ string-join $ cons "./configure"
             $ map shell-escape-string $ cdr $ command-line))
    (with-output-to-file gpd-file
      (cut write-gauche-package-description gpd))))

;; API
;; Packages common output
(define (cf-output-default . output-files)
  (cf-make-gpd)
  (cf-echo (cf$ 'PACKAGE_VERSION) > "VERSION")
  (let* ([pfx (cf$'srcdir)]
         [outfiles (append
                    ($ map (^f (string-drop (string-drop-right f 3)
                                            (+ (string-length pfx) 1)))
                       $ glob #"~|pfx|/**/Makefile.in")
                    output-files)])
    (apply cf-output outfiles)))

;;;
;;; Target languages
;;;

;; Various language-specific operations are defined as methods on
;; the language singleton object inheriting <cf-language>.
;; Methods a named with -m suffix to distinguish from cf- API, which
;; is a usual procedure dispatches according to the current
;; language.
(define-class <cf-language> ()
  ((name :init-keyword :name :init-value "(none)"))
  :metaclass <singleton-meta>)

;; API
;; Parameter cf-lang holds a singleton instance of the current language.
(define cf-lang (make-parameter (make <cf-language>)))

(define (cf-lang-ext) (cf-lang-ext-m (cf-lang)))

;; API
;; cf-lang-program <prologue> <body>
;; Returns a string tree that consists a stand-alone program for the
;; current language.  <prologue> and <body> both are string tree.
(define (cf-lang-program prologue body)
  (cf-lang-program-m (cf-lang) prologue body))
(define-method cf-lang-program-m ((lang <cf-language>) prologue body)
  (error "No language is selected."))

;; API
;; cf-lang-io-program
;; Returns a string tree of a program that creates "conftest.out"
;; in the current language.
(define (cf-lang-io-program)
  (cf-lang-io-program-m (cf-lang)))
(define-method cf-lang-io-program-m ((lang <cf-language>))
  (error "No language is selected."))

;; API
;; cf-lang-call
(define (cf-lang-call prologue func-name)
  (cf-lang-call-m (cf-lang) prologue func-name))
(define-method cf-lang-call-m ((lang <cf-language>) prologue func-name)
  (error "No language is selected."))

;;
;; C
;;
(define-class <c-language> (<cf-language>) ((name :init-value "C")))

(define-method cf-lang-ext-m ((lang <c-language>)) "c")
(define-method cf-lang-cpp-m ((lang <c-language>))
  #"~(cf$'CPP) ~(cf$'CPPFLAGS)")
(define-method cf-lang-compile-m ((lang <c-language>))
  #"~(cf$'CC) -c ~(cf$'CFLAGS) ~(cf$'CPPFLAGS) conftest.~(cf-lang-ext-m lang)")
(define-method cf-lang-link-m ((lang <c-language>))
  #"~(cf$'CC) -o conftest~(cf$'EXEEXT) ~(cf$'CFLAGS) ~(cf$'CPPFLAGS) ~(cf$'LDFLAGS) conftest.~(cf-lang-ext-m lang) ~(cf$'LIBS)")
(define-method cf-lang-null-program-m ((lang <c-language>))
  (cf-lang-program-m lang "" ""))

(define-method cf-lang-program-m ((lang <c-language>) prologue body)
  `(,prologue
    "\nint main(){\n"
    ,body
    "\n; return 0;\n}\n"))

(define-method cf-lang-io-program-m ((lang <c-language>))
  (cf-lang-program-m lang "#include <stdio.h>\n"
                     '("FILE *f = fopen(\"conftest.out\", \"w\");\n"
                       "return ferror(f) || fclose(f) != 0;")))

(define-method cf-lang-call-m ((lang <c-language>) prologue body)
  ($ cf-lang-program-m lang
     (if (equal? func-name "main")
       `(,prologue
         "\n#ifdef __cplusplus\nextern \"C\"\n#endif\nchar main();\n")
       prologue)
     `("return" ,func-name "();\n")))

;; C++
(define-class <c++-language> (<c-language>) ((name :init-value "C++")))
(define-method cf-lang-ext-m ((lang <c++-language>)) "c")
(define-method cf-lang-cpp-m ((lang <c++-language>))
  #"~(cf$'CXXCPP) ~(cf$'CPPFLAGS)")
(define-method cf-lang-compile-m ((lang <c++-language>))
  #"~(cf$'CXX) -c ~(cf$'CXXFLAGS) ~(cf$'CPPFLAGS) conftest.~(cf-lang-ext-m lang)")
(define-method cf-lang-link-m ((lang <c++-language>))
  #"~(cf$'CXX) -o conftest~(cf$'EXEEXT) ~(cf$'CXXFLAGS) ~(cf$'CPPFLAGS) ~(cf$'LDFLAGS) conftest.~(cf-lang-ext-m lang) ~(cf$'LIBS)")


;;;
;;; Tests - compilation
;;;

;; Dump CONTENT to a file conftext.$(cf-lang-ext) and run COMMAND.
;; The output and error goes to config.log.  Returns #t on success,
;; #f on failure.  Make sure to clean temporary files.
(define (run-compiler-with-content command content)
  (define (clean)
    (remove-files (glob "conftest.err*")
                  #"conftest.~(cf-lang-ext)"
                  #"conftest.~(cf$'OBJEXT)"
                  #"conftest~(cf$'EXEEXT)"))
  (define cmd
    (if (string? command)
      (shell-tokenize-string command)
      command))
  (unwind-protect
      (receive (errout erroutfile) (sys-mkstemp "conftest.err.")
        (log-format "configure: ~s" cmd)
        (with-output-to-file #"conftest.~(cf-lang-ext)"
          (^[] (write-tree content)))
        (let1 st ($ process-exit-status
                    (run-process cmd :wait #t
                                 :redirects `((> 1 ,errout) (> 2 ,errout))))
          (close-port errout)
          ($ generator-for-each (cut log-format "~a" <>)
             $ file->line-generator erroutfile)
          (log-format "configure: $? = ~s" (sys-wait-exit-status st))
          (unless (zero? st)
            (log-format "configure: failed program was:")
            ($ generator-for-each (cut log-format "| ~a" <>)
               $ file->line-generator #"conftest.~(cf-lang-ext)"))
          (zero? st)))
    (clean)))

;; API
;; Try compile BODY as the current language.
;; Returns #t on success, #f on failure.
(define (cf-try-compile prologue body)
  ($ run-compiler-with-content
     (cf-lang-compile-m (cf-lang))
     (cf-lang-program prologue body)))

;; API
;; Try compile and link BODY as the current language.
;; Returns #t on success, #f on failure.
(define (cf-try-compile-and-link prologue body)
  ($ run-compiler-with-content
     (cf-lang-link-m (cf-lang))
     (cf-lang-program prologue body)))

;; Try to produce executable from
;; This emits message---must be called in feature test api
(define (compiler-can-produce-executable?)
  (cf-msg-checking "whether the ~a compiler works" (~ (cf-lang)'name))
  (rlet1 result ($ run-compiler-with-content
                   (cf-lang-link-m (cf-lang))
                   (cf-lang-null-program-m (cf-lang)))
    (cf-msg-result (if result "yes" "no"))))

;; Feature Test API
;; Find c++ compiler.  Actually, we need the one that generates compatible
;; binary with which Gauche was compiled, but there's no reliable way
;; (except building an extension and trying to load into Gauche, but that's
;; a bit too much.)
(define (cf-prog-cxx :optional (compilers '("g++" "c++" "gpp" "aCC" "CC"
                                            "cxx" "cc++" "cl.exe" "FCC"
                                            "KCC" "RCC" "xlC_r" "xlC")))
  (cf-arg-var 'CXX)
  (cf-arg-var 'CXXFLAGS)
  (cf-arg-var 'CCC)
  (or (cf-ref 'CXX #f)
      (and-let1 ccc (cf-ref 'CCC #f)
        (cf-subst 'CXX ccc)
        #t)
      (cf-check-tool 'CXX compilers :default "g++"))
  (parameterize ([cf-lang (instance-of <c++-language>)])
    (compiler-can-produce-executable?)))

;;;
;;; Tests - programs
;;;

;; API
;; Common feature for AC_CHECK_PROG, AC_PATH_PROG etc.
;; Search one of programs listed in PROGS within PATHS.
;; PATHS can be #f - then we use PATH environment variable.
;; For each found program, FILTER is called with full path of the program;
;; it may return #f if the caller wants to exclude the program for some reason.
;; If a program found, and FILTER returns true, then this procedure returns
;; the full path of the program.  If no program found, #f is returned.
(define (check-for-program progs :key ((:paths ps) #f) (filter #f))
  (define paths (or ps
                    (string-split (or (sys-getenv "PATH") '())
                                  (cond-expand [gauche.os.windows #\;]
                                               [else #\:]))))
  (define (finder prog)
    (find-file-in-paths prog :paths paths
                        :pred (if filter
                                (^p (and (filter p)
                                         (file-is-executable? p)))
                                file-is-executable?)))
  (any (^[prog]
         (cond-expand [gauche.os.windows
                       (if (equal? (path-extension prog) "exe")

                         (finder prog)
                         (or (finder prog) (finder #"~|prog|.exe")))]
                      [else (finder prog)]))
       progs))

;; Feature Test API
;; cf-check-prog works like AC_CHECK_PROG and AC_CHECK_PROGS.
;; If SYM is already cf-subst'd, we don't do anything.
(define (cf-check-prog sym prog-or-progs
                       :key (value #f) (default #f) (paths #f) (filter #f))
  (unless (cf-have-subst? sym)
    (cf-msg-checking "for ~a" prog-or-progs)
    (if-let1 found (check-for-program (listify prog-or-progs)
                                      :paths paths :filter filter)
      (let1 result (or value (sys-basename found))
        (cf-msg-result "~a" result)
        (cf-subst sym result))
      (begin (cf-msg-result "no")
             (and default (cf-subst sym default))))))

;; Feature Test API
;; cf-path-prog works like AC_PATH_PROG and AC_PATH_PROGS.
(define (cf-path-prog sym prog-or-progs
                      :key (value #f) (default #f) (paths #f) (filter #f))
  (unless (cf-have-subst? sym)
    (cf-msg-checking "for ~a" prog-or-progs)
    (if-let1 found (check-for-program (listify prog-or-progs)
                                      :paths paths :filter filter)
      (let1 result (or value found)
        (cf-msg-result "~a" result)
        (cf-subst sym result))
      (begin (cf-msg-result "no")
             (and default (cf-subst sym default))))))

;; Feature Test API *COMPATIBILITY*
;; This is used in place of cf-check-prog when cross compilation needs to
;; taken care of.  At this moment we don't support cross compilation yet,
;; so we just call cf-check-prog.
(define (cf-check-tool sym prog-or-progs
                       :key (default #f) (paths #f) (filter #f) :rest keys)
  (apply cf-check-prog sym prog-or-progs keys))

;;;
;;; Tests - headers
;;;

;; API
;; Returns a string tree
;; Unlike AC_INCLUDES_DEFAULT, we don't accept argument.  The
;; behavior of AC_INCLUDES_DEFAULT is convenient for m4 macros,
;; but makes little sense for Scheme.
(define cf-includes-default
  (let* ([defaults '("#include <stdio.h>\n"
                     "#ifdef HAVE_SYS_TYPES_H\n"
                     "# include <sys/types.h>\n"
                     "#endif\n"
                     "#ifdef HAVE_SYS_STAT_H\n"
                     "# include <sys/stat.h>\n"
                     "#endif\n"
                     "#ifdef STDC_HEADERS\n"
                     "# include <stdlib.h>\n"
                     "# include <stddef.h>\n"
                     "#else\n"
                     "# ifdef HAVE_STDLIB_H\n"
                     "#  include <stdlib.h>\n"
                     "# endif\n"
                     "#endif\n"
                     "#ifdef HAVE_STRING_H\n"
                     "# if !defined STDC_HEADERS && defined HAVE_MEMORY_H\n"
                     "#  include <memory.h>\n"
                     "# endif\n"
                     "# include <string.h>\n"
                     "#endif\n"
                     "#ifdef HAVE_STRINGS_H\n"
                     "# include <strings.h>\n"
                     "#endif\n"
                     "#ifdef HAVE_INTTYPES_H\n"
                     "# include <inttypes.h>\n"
                     "#endif\n"
                     "#ifdef HAVE_STDINT_H\n"
                     "# include <stdint.h>\n"
                     "#endif\n"
                     "#ifdef HAVE_UNISTD_H\n"
                     "# include <unistd.h>\n"
                     "#endif\n")]
         [requires (delay
                     (begin (cf-check-headers '("sys/types.h" "sys/stat.h"
                                                "stdlib.h" "string.h" "memory.h"
                                                "strings.h" "inttypes.h"
                                                "stdint.h" "unistd.h")
                                              :includes defaults)
                            defaults))])
    (^[] (force requires))))

;; Feature Test API
;; Like AC_CHECK_HEADER.
;; Returns #t on success, #f on failure.
(define (cf-check-header header-file :key (includes #f))
  (let1 includes (or includes (cf-includes-default))
    (cf-msg-checking "~a usability" header-file)
    (rlet1 result (cf-try-compile (list includes
                                        "/* Testing compilability */"
                                        #"#include <~|header-file|>\n")
                                  "")
      (cf-msg-result (if result "yes" "no")))))

;; Feature Test API
;; Like AC_CHECK_HEADERS.  Besides the check, it defines HAVE_<header-file>
;; definition.
(define (cf-check-headers header-files
                          :key (includes #f) (if-found #f) (if-not-found #f))
  (dolist [h header-files]
    (if (cf-check-header h :includes includes)
      (begin (cf-define (string->symbol #"HAVE_~(safe-variable-name h)"))
             (when if-found (if-found h)))
      (when if-not-found (if-not-found h)))))

;; Feature Test API
;; Like AC_CHECK_TYPE.
;; Returns #t on success, #f on failure.
;; If TYPE is a valid type, sizeof(TYPE) compiles and sizeof((TYPE)) fails.
;; The second test is needed in case TYPE happens to be a variable.
(define (cf-check-type type :key (includes #f))
  (let1 includes (or includes (cf-includes-default))
    (cf-msg-checking "for ~a" type)
    (rlet1 result
        (and (cf-try-compile (list includes)
                             #"if (sizeof (~|type|)) return 0;")
             (not (cf-try-compile (list includes)
                                  #"if (sizeof ((~|type|))) return 0;")))
      (cf-msg-result (if result "yes" "no")))))

;; Feature Test API
;; Like AC_CHECK_TYPES.
;; For each type in types, run cf-check-type and define HAVE_type if found.
(define (cf-check-types types :key (includes #f)
                                   (if-found identity)
                                   (if-not-found identity))
  (dolist [type types]
    (if (cf-check-type type :includes includes)
      (begin (cf-define (string->symbol #"HAVE_~(safe-variable-name type)"))
             (if-found type))
      (if-not-found type))))

;; Feature Test API
;; Like AC_CHECK_DECL
;; Returns #t on success, #f on failure.
;; Check SYMBOL is declared as a macro, a constant, a variable or a function.
(define (cf-check-decl symbol :key (includes #f))
  (let1 includes (or includes (cf-includes-default))
    (cf-msg-checking "whether ~a is declared" symbol)
    (rlet1 result
        (cf-try-compile (list includes)
                        (list #"#ifndef ~|symbol|\n"
                              #" (void)~|symbol|;\n"
                              #"#endif\n"
                              "return 0;"))
      (cf-msg-result (if result "yes" "no")))))

;; Feature Test API
;; Like AC_CHECK_DECLS
;; For each symbol in symbols, run cf-check-decl and define HAVE_DECL_symbol
;; to 1 (found) or 0 (not found).
(define (cf-check-decls symbols :key (includes #f)
                                     (if-found identity)
                                     (if-not-found identity))
  (dolist [symbol symbols]
    (let1 nam (string->symbol #"HAVE_DECL_~(safe-variable-name symbol)")
      (if (cf-check-decl symbol :includes includes)
        (begin (cf-define nam 1)
               (if-found symbol))
        (begin (cf-define nam 0)
               (if-not-found symbol))))))

;; Feature Test API
;; Like AC_CHECK_MEMBER
;; Works as a predicate
(define (cf-check-member aggregate.member :key (includes #f))
  (receive (aggr memb) (string-scan aggregate.member #\. 'both)
    (unless (and aggr memb)
      (error "cf-check-member: argument doesn't contain a dot:"
             aggregate.member))
    (cf-msg-checking "`~a' is a member of `~a'" memb aggr)
    (let1 includes (or includes (cf-includes-default))
      (rlet1 result
          (or (cf-try-compile (list includes)
                              (list #"static ~aggr ac_aggr;\n"
                                    #"if (ac_aggr.~|memb|) return 0;"))
              (cf-try-compile (list includes)
                              (list #"static ~aggr ac_aggr;\n"
                                    #"if (sizeof ac_aggr.~|memb|) return 0;")))
        (cf-msg-result (if result "yes" "no"))))))

;; Feature Test API
;; Like AC_CHECK_MEMBERS
(define (cf-check-members members :key (includes #f)
                                       (if-found identity)
                                       (if-not-found identity))
  (dolist [mem members]
    (if (cf-check-member mem :includes includes)
      (begin (cf-define (string->symbol #"HAVE_~(safe-variable-name mem)"))
             (if-found mem))
      (if-not-found mem))))

;; Feature Test API
;; Like AC_CHECK_FUNC
;; NB: autoconf has language-dependent methods (AC_LANG_FUNC_LINK_TRY)
;; For now, we hardcode C.
(define (cf-check-func func)
  (let1 includes (cf-includes-default)
    (cf-msg-checking #"for ~func")
    (rlet1 result ($ cf-try-compile-and-link
                     `(,#"#define ~func innocuous_~func\n"
                       "#ifdef __STDC__\n"
                       "# include <limits.h>\n"
                       "#else\n"
                       "# include <assert.h>\n"
                       "#endif\n"
                       ,#"#undef ~func\n"
                       "#ifdef __cplusplus\n"
                       "extern \"C\"\n"
                       "#endif\n"
                       ,#"char ~func ();\n")
                     `(,#"return ~func ();"))
      (cf-msg-result (if result "yes" "no")))))

;; Feature Test API
;; Like AC_CHECK_FUNCS
(define (cf-check-funcs funcs :key (if-found identity) 
                                   (if-not-found identity))
  (dolist [f funcs]
    (if (cf-check-func f)
      (begin (cf-define (string->symbol #"HAVE_~(safe-variable-name f)"))
             (if-found f))
      (if-not-found f))))


(define (default-lib-found libname)
  (when libname
    (cf-subst-prepend 'LIBS #"-l~|libname|")
    (cf-define (string->symbol #"HAVE_LIB~(safe-variable-name libname)")))
  #t)

(define (default-lib-not-found libname) #f)

;; Feature Test API
;; Like AC_CHECK_LIB
(define (cf-check-lib lib fn
                      :key (other-libs '()) 
                           (if-found default-lib-found)
                           (if-not-found default-lib-not-found))
  (let1 includes (cf-includes-default)
    (cf-msg-checking "for ~a in -l~a" fn lib)
    (if (with-cf-subst
         ([LIBS #"-l~|lib| ~(string-join other-libs \" \") ~(cf$'LIBS)"])
         (cf-try-compile-and-link includes
                                  (format "extern void ~a(); ~a();" fn fn)))
      (begin
        (cf-msg-result "yes")
        (if-found lib))
      (begin
        (cf-msg-result "no")
        (if-not-found lib)))))

(define (default-lib-search-found libname)
  (when libname
    (cf-subst-prepend 'LIBS #"-l~|libname|"))
  #t)
   
;; Feature test API
;; Like AC_CHECK_LIBS
(define (cf-search-libs fn libs
                        :key (other-libs '())
                             (if-found default-lib-search-found) 
                             (if-not-found default-lib-not-found))
  (let ([includes (cf-includes-default)]
        [xlibs #"~(string-join other-libs \" \") ~(cf$'LIBS)"])
    (define (try lib)
      (with-cf-subst 
       ([LIBS (if (eq? lib 'none) xlibs #"-l~|lib| ~xlibs")])
       (cf-try-compile-and-link includes
                                (format "extern void ~a(); ~a();"
                                        fn fn))))
    (cf-msg-checking "for ~a" fn)
    (if-let1 lib (find try (cons 'none libs))
      (begin
        (cf-msg-result (if (eq? lib 'none) "found" #"found in -l~|lib|"))
        (if-found (if (eq? lib 'none) #f lib)))
      (begin
        (cf-msg-result "no")
        (if-not-found #f)))))
