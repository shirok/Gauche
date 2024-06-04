;;;
;;; gauche.configure.init - initialization & cmdline processing
;;;
;;;   Copyright (c) 2013-2024  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.configure.init
  (use gauche.config)
  (use gauche.configure.base)
  (use gauche.configure.lang)
  (use gauche.configure.prog)
  (use gauche.cgen)
  (use gauche.package)
  (use gauche.parseopt)
  (use gauche.logger)
  (use file.util)
  (use srfi.13)
  (use text.fill)
  (use text.tr)
  (use text.tree)
  (use util.match)
  (export cf-init cf-init-gauche-extension
          cf-arg-enable cf-arg-with cf-feature-ref cf-package-ref
          cf-help-string)
  )
(select-module gauche.configure.init)

;; constants for formatting help strings.  see cf-help-string.
(define-constant *usage-description-indent* 26)
(define-constant *usage-item-indent* 2)
(define-constant *usage-fill-column* 79)

;;;
;;; Initialization
;;;

;; API
;; Like AC_INIT; package-name and version can be omitted if the package
;; has "package.scm".
(define (cf-init :optional (package-name #f) (version #f)
                           (bug-report #f) (url #f))
  (when (current-package)
    (exit 1 "Incorrect configure script: cf-init is called more than once."))
  (let* ([gpd (and-let* ([pfile (build-path 'cld "package.scm")]
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
    (cf-lang (cf-lang-C))
    (initialize-default-definitions)
    (parse-command-line-arguments)
    (check-directory-names)
    (process-args)
    (check-package-superseded-by)
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

  ;; For cross compilation.  These can be overridden by --build, --host and
  ;; --target.
  (cf-subst 'build  (gauche-architecture))
  (cf-subst 'host   (gauche-architecture))
  (cf-subst 'target (gauche-architecture))

  ;; NB: Autoconf uses AC_PROG_CC to set up CC.  However, we need
  ;; to use the same C compiler with which Gauche was compiled, so
  ;; we set it as the default.  We also allow env overrides for some
  ;; common variables.  (In autoconf, AC_PROG_CC issues AC_ARG_VAR
  ;; for them).
  (cf-subst 'CC (gauche-config "--cc"))
  (cf-subst 'CFLAGS (gauche-config "--default-cflags"))
  (cf-subst 'CPP (gauche-config "--cpp"))
  (cf-subst 'CPPFLAGS (gauche-config "--cppflags"))
  (cf-arg-var 'CPP)
  (cf-arg-var 'CPPFLAGS)
  (cf-arg-var 'CC)
  (cf-arg-var 'CFLAGS)
  (cf-arg-var 'LDFLAGS)
  (cf-arg-var 'LIBS)
  ;; NB: Autoconf determines these through tests, but we already
  ;; know them at the time Gauche is configured.
  (cf-subst 'SOEXT  (gauche-config "--so-suffix"))
  (cf-subst 'OBJEXT (gauche-config "--object-suffix"))
  (cf-subst 'EXEEXT (gauche-config "--executable-suffix"))

  ;; '-DUNICODE' on windows.
  (cf-subst 'WINDOWS_UNICODE_FLAG
            (cond-expand [gauche.os.windows "-DUNICODE"]
                         [else ""]))
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
    ;; build, host and target override
    (when (cf-have-subst? 'build_alias)  (cf-subst 'build (cf$ 'build_alias)))
    (when (cf-have-subst? 'host_alias)   (cf-subst 'host (cf$ 'host_alias)))
    (when (cf-have-subst? 'target_alias) (cf-subst 'target (cf$ 'target_alias)))
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

(define (check-package-superseded-by)
  ;; If package.scm has superseded-by clause, tell so.
  (and-let1 sup (~ (current-package)'gpd'superseded-by)
    (cf-msg-error "This package is superseded by ~a and no longer maintained. \
                   Use the successor module."
                  sup))
  )

;; Process the arg-if-given and arg-if-not-given callbacks of cf-arg-*.
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
  (tree->string
   (list ($ text->filled-stree description
            :lead-in (format "~va~a" *usage-item-indent* "" item)
            :indent *usage-description-indent*
            :width *usage-fill-column*)
         "\n")))
