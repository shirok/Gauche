;;;
;;; configure.scm - configuring Gauche extensions
;;;
;;;   Copyright (c) 2013-2015  Shiro Kawai  <shiro@acm.org>
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

;; The core feature of gauche.configure is the ability to generate files
;; (e.g. Makefile) from templates (e.g. Makefile.in) with replacing
;; parameters.  We follow autoconf convention, so the replacement variables
;; in a template is written like @VAR@.

;; The API is roughly corresponds to autoconf's AC_* macros, while we use
;; 'cf-' suffix instead.

;; NB: cf-define currently only suppors substituting DEFS; it doesn't
;; handle config.h.

(define-module gauche.configure
  (use gauche.parameter)
  (use gauche.generator)
  (use gauche.dictionary)
  (use gauche.parseopt)
  (use gauche.logger)
  (use gauche.cgen)
  (use gauche.package)
  (use gauche.process)
  (use util.match)
  (use file.filter)
  (use file.util)
  (use text.tr)
  (use srfi-13)
  (extend gauche.config)
  (export cf-init
          cf-arg-enable cf-arg-with cf-feature-ref cf-package-ref
          cf-help-string
          cf-msg-checking cf-msg-result cf-msg-warn cf-msg-error
          cf-echo
          cf-make-gpd
          cf-define cf-subst cf-have-subst? cf-ref cf$
          cf-config-headers cf-output cf-show-substs
          cf-check-prog cf-path-prog))
(select-module gauche.configure)

;; A package
(define-class <package> ()
  ((name       :init-keyword :name)
   (version    :init-keyword :version)
   (bug-report :init-keyword :bug-report :init-value #f) ; email addr
   (url        :init-keyword :url :init-value #f)
   (string     :init-keyword :string)    ; package_string
   (tarname    :init-keyword :tarname)
   (config.h   :init-form '())           ; list of (config-header . source)
   (defs       :init-form (make-hash-table 'eq?)) ;cf-define'd thingy
   (substs     :init-form (make-hash-table 'eq?)) ;cf-subst'ed thingy
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

;; constants for formatting help strings.  see cf-help-string.
(define-constant *usage-description-indent* 26)
(define-constant *usage-item-indent* 2)
(define-constant *usage-fill-column* 79)

;;
;; Basic APIs
;; 

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

;; API
;; Convenience routie for substitute of shell's echo
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
;; Like AC_INIT
(define (cf-init package-name version :optional (bug-report "") (url ""))
  (check-arg string? package-name)
  (check-arg string? version)
  (sys-unlink "config.log")
  (log-open "config.log" :prefix "")
  (log-format "Configuring ~a ~a" package-name version)
  (current-package
   (make <package>
     :name package-name
     :version version
     :bug-report bug-report
     :url url
     :string (format "~a ~a" package-name version)
     :tarname (cgen-safe-name-friendly (string-downcase package-name))))
  (initialize-default-definitions)
  (parse-command-line-arguments)
  (check-directory-names)
  (process-args))

(define (initialize-default-definitions)
  (define p (current-package))
  (cf-subst 'PACKAGE_NAME    (~ p'name))
  (cf-subst 'PACKAGE_TARNAME (string-tr (string-downcase (~ p'name))
                                         "a-z0-9_-" "_*" :complement #t))
  (cf-subst 'PACKAGE_VERSION (~ p'version))
  (cf-subst 'PACKAGE_STRING (~ p'string))
  (cf-subst 'PACKAGE_BUGREPORT (~ p'bug-report))
  (cf-subst 'PACKAGE_URL (~ p'url))

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
  )

(define (parse-command-line-arguments)
  (let1 rest
      (parse-options (cdr (command-line))
        (["bindir=s" (dir) (cf-subst 'bindir dir)]
         ["build=s" (build) (cf-subst 'build-alias build)]
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
         ["host=s" (host) (cf-subst 'host-alias host)]
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
(define (cf-define symbol :optional (value 1))
  (dict-put! (~ (ensure-package)'defs) symbol value))

;; API
;; Like AC_SUBST, but we require value (instead of implicitly referencing
;; a global variable.
(define (cf-subst symbol value)
  (dict-put! (~ (ensure-package)'substs) symbol value))

;; API
(define (cf-have-subst? symbol)
  (dict-exists? (~ (ensure-package)'substs) symbol))

;; API
(define (cf-ref symbol :optional (default (undefined)))
  (rlet1 v (dict-get (~ (ensure-package)'substs) symbol default)
    (when (undefined? v)
      (errorf "Configure variable ~s is not defined." symbol))))

;; API
;; Like cf-ref, but returns empty string if undefined.
(define (cf$ symbol) (cf-ref symbol ""))

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
      (file-filter-for-each (make-replace-1 f) :input inf :output f
                            :temporary-file #t :leave-unchanged #t)))
  (dolist [h (~ pa'config.h)]
    (let1 inf (build-path (cf$'srcdir) (cdr h))
      (unless (file-is-readable? inf)
        (error "Cannot read input file ~s" inf))
      (unless (file-is-directory? (sys-dirname (car h)))
        (make-directory* (sys-dirname (car h))))
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
  (let1 gpd-file #"~(cf$ 'PACKAGE_NAME).gpd"
    (cf-echo #"creating ~gpd-file")
    (with-output-to-file gpd-file
      (cut write-gauche-package-description
           (make <gauche-package-description>
             :name (cf$ 'PACKAGE_NAME)
             :version (cf$ 'PACKAGE_VERSION)
             :configure ($ string-join $ cons "./configure"
                           $ map shell-escape-string $ cdr $ command-line))))))

;;
;; Tests - programs
;;

;; API
;; Common feature for AC_CHECK_PROG, AC_PATH_PROG etc.
;; Search one of programs listed in PROGS within PATHS.
;; PATHS can be #f - then we use PATH enviornment variable.
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

;; API
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

;; API
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

