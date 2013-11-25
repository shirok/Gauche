;;;
;;; configure.scm - configuring Gauche extensions
;;;
;;;   Copyright (c) 2013  Shiro Kawai  <shiro@acm.org>
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
;; parameters.  We follow autoconf convension, so the replacement variables
;; in a template is written like @VAR@.  

;; The API is roughly corresponds to autoconf's AC_* macros, while we use
;; 'cf-' suffix instead.

(define-module gauche.configure
  (use gauche.parameter)
  (use gauche.generator)
  (use gauche.dictionary)
  (use gauche.parseopt)
  (use gauche.logger)
  (use gauche.cgen)
  (use util.match)
  (use file.filter)
  (use file.util)
  (use text.tr)
  (use srfi-13)
  (extend gauche.config)
  (export cf-init
          cf-msg-checking cf-msg-result cf-msg-warn cf-msg-error
          cf-echo
          cf-define cf-ref cf$ cf-output cf-show-variables
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
   (defs       :init-form (make-hash-table 'eq?))
   (features   :init-form (make-hash-table 'eq?)) ;enabled features by cmdarg
   (packages   :init-form (make-hash-table 'eq?)) ;optional packages
   ))

(define current-package (make-parameter #f))

(define run-quietly (make-parameter #f)) ;-silent or -quiet option turn this on

;; some internal utilities

(define (listify x) (if (list? x) x (list x)))

(define (ensure-package)
  (or (current-package)
      (error "No current package - cf-init hasn't been called")))

(define (tee-msg console-fmt log-fmt args)
  (apply format #t console-fmt args)
  (apply log-format log-fmt args))

;;
;; Basic APIs
;; 

;; API
;; Like AC_MSG_*
(define (cf-msg-checking fmt . args)
  (tee-msg #`"checking ,|fmt|... " #`"checking: ,fmt" args))
(define (cf-msg-result fmt . args)
  (tee-msg #`",|fmt|\n" #`"result: ,fmt" args))
(define (cf-msg-warn fmt . args)
  (tee-msg #`"Warning: ,|fmt|\n" #`"Warning: ,fmt" args))
(define (cf-msg-error fmt . args)
  (tee-msg #`"Error: ,|fmt|\n" #`"Error: ,fmt" args)
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
    [_ `(print ,@(intersperse " " (drop-right* args 2)))]))

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
  )

(define (initialize-default-definitions)
  (define p (current-package))
  (cf-define 'PACKAGE_NAME    (~ p'name))
  (cf-define 'PACKAGE_TARNAME (string-tr (string-downcase (~ p'name))
                                         "a-z0-9_-" "_*" :complement #t))
  (cf-define 'PACKAGE_VERSION (~ p'version))
  (cf-define 'PACKAGE_STRING (~ p'string))
  (cf-define 'PACKAGE_BUGREPORT (~ p'bug-report))
  (cf-define 'PACKAGE_URL (~ p'url))

  (cf-define 'SHELL (or (sys-getenv "CONFIG_SHELL") "/bin/sh"))
  (cf-define 'LIBOBJS "")
  (cf-define 'MFLAGS "")
  (cf-define 'MAKEFLAGS "")

  (cf-define 'default_prefix "/usr/local")
  (cf-define 'prefix "NONE")       ;will be replaced by cf-output
  (cf-define 'exec_prefix "NONE")  ;will be replaced by cf-output
  (cf-define 'bindir "${exec_prefix}/bin")
  (cf-define 'sbindir "${exec_prefix}/sbin")
  (cf-define 'libexecdir "${exec_prefix}/libexec")
  (cf-define 'datarootdir "${prefix}/share")
  (cf-define 'datadir "${datarootdir}")
  (cf-define 'sysconfdir "${prefix}/etc")
  (cf-define 'sharedstatedir "${prefix}/com")
  (cf-define 'localstatedir "${prefix}/var")
  (cf-define 'includedir "${prefix}/include")
  (cf-define 'oldincludedir "/usr/include")
  (cf-define 'docdir "${datarootdir}/doc/${PACKAGE_TARNAME}")
  (cf-define 'infodir "${datarootdir}/info")
  (cf-define 'htmldir "${docdir}")
  (cf-define 'dvidir "${docdir}")
  (cf-define 'pdfdir "${docdir}")
  (cf-define 'psdir "${docdir}")
  (cf-define 'libdir "${exec_prefix}/lib")
  (cf-define 'localedir "${datarootdir}/locale")
  (cf-define 'mandir "${datarootdir}/man")

  (cf-define 'cross_compiling "no")
  (cf-define 'subdirs "")
  )

(define (parse-command-line-arguments)
  (let1 rest
      (parse-options (cdr (command-line))
        (["bindir=s" (dir) (cf-define 'bindir dir)]
         ["build=s" (build) (cf-define 'build-alias build)]
         ["c|cache-file=s" (_)
          (values)] ;; support for the compatibility.  we don't do anything.
         ["C|config-cache=s" (_) (values)]
         ["datadir=s" (dir) (cf-define 'datadir dir)]
         ["datarootdir=s" (dir) (cf-define 'datarootdir dir)]
         ["docdir=s" (dir) (cf-define 'docdir dir)]
         ["dvidir=s" (dir) (cf-define 'dvidir dir)]
         ["exec_prefix=s" (pre) (cf-define 'exec_prefix pre)]
         ["help" () (usage)]
         ["host=s" (host) (cf-define 'host-alias host)]
         ["htmldir=s" (dir) (cf-define 'htmldir dir)]
         ["includedir=s" (dir) (cf-define 'includedir dir)]
         ["infodir=s" (dir) (cf-define 'infodir dir)]
         ["libdir=s" (dir) (cf-define 'libdir dir)]
         ["libexecdir=s" (dir) (cf-define 'libexecdir dir)]
         ["localedir=s" (dir) (cf-define 'localedir dir)]
         ["localstatedir=s" (dir) (cf-define 'localstatedir dir)]
         ["mandir=s" (dir) (cf-define 'mandir dir)]
         ;; -no-create
         ;; -no-recursion
         ;; -oldincludedir
         ["prefix=s" (dir) (cf-define 'prefix dir)]
         ["program-suffix=s" (arg) (cf-define 'program_suffix arg)]
         ["program-transform-name=s" (arg)
          (cf-define 'program_transform_name arg)]
         ["pdfdir=s" (arg) (cf-define 'pdfdir arg)]
         ["psdir=s" (arg) (cf-define 'psdir arg)]
         ["q|quiet|silent" () (run-quietly #t)]
         ["sbindir=s" (arg) (cf-define 'sbindir arg)]
         ["sharedstatedir=s" (arg) (cf-define 'sharedstatedir arg)]
         ["site=s" (arg) (cf-define 'site arg)]
         ["srcdir=s" (arg) (cf-define 'srcdir arg)]
         ["sysconfidr=s" (arg) (cf-define 'sysconfdir arg)]
         ["target=s" (arg) (cf-define 'target_alias arg)]
         ["v|verbose" () (run-quietly #f)]
         ["V|version" () (exit 0 "gauche.configure ~a" (gauche-version))]
         ;; -x-includes
         ;; -x-libraries
         [else (opt rest cont)
               (rxmatch-case opt
                 [#/^--?enable-([-\w]+)(?:=([-\w]+))$/ (_ feature arg)
                               (set! (cf-feature-ref (string->symbol feature))
                                     (or arg "yes"))
                               (cont rest)]
                 [#/^--?disable-([-\w]+)$/ (_ feature)
                                (set! (cf-feature-ref (string->symbol feature)) "no")
                                (cont rest)]
                 [#/^--?with-([-\w]+)(?:=([-\w]+))$/ (_ package arg)
                             (set! (cf-package-ref (string->symbol package))
                                   (or arg "yes"))
                             (cont rest)]
                 [#/^--?without-([-\w]+)$/ (_ package)
                                (set! (cf-package-ref (string->symbol package)) "no")
                                (cont rest)]
                 [else
                  (print "Unrecognized option: " opt)
                  (print "Type `./configure --help' for valid options.")
                  (exit 1)]
                 )]))
    ;; process VAR=VAL
    (dolist [arg rest]
      (rxmatch-case arg
        [#/^([-\w]+)=([-\w]*)$/ (_ var val)
         (cf-define (string->symbol var) val)]
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
        (cf-define var (string-trim-right val #[/])))
      (unless (or (absolute-path? val)
                  (string-prefix? "$" val)
                  (and (memq var '(prefix exec_prefix))
                       (member val '("NONE" ""))))
        (exit 1 "absolute directory name required for --~a but got: ~a"
              var val))))
  ;; setup srcdir and builddir
  (unless (cf-defined? 'srcdir)
    (cf-define 'srcdir (sys-dirname (car (command-line)))))
  (cf-define 'top_srcdir (cf$ 'srcdir))
  (unless (cf-defined? 'builddir)
    (cf-define 'builddir "."))
  (cf-define 'top_builddir (cf$ 'builddir))
  )

(define (usage)
  (print "Usage: "(car (command-line))" args ...")
  (print "  Generate Makefiles and other files suitable for your system.")
  ;; WRITEME - option descriptions
  (exit 1))

;; API
;; Like AC_DEFINE
(define (cf-define symbol :optional (value 1))
  (dict-put! (~ (ensure-package)'defs) symbol value))

;; API
(define (cf-defined? symbol)
  (dict-exists? (~ (ensure-package)'defs) symbol))

;; API
(define (cf-ref symbol :optional (default (undefined)))
  (rlet1 v (dict-get (~ (ensure-package)'defs) symbol default)
    (when (undefined? v)
      (errorf "Configure variable ~s is not defined." symbol))))

;; API
;; Like cf-ref, but returns empty string if undefined.
(define (cf$ symbol) (cf-ref symbol ""))

;; API
;; --enable-FEATURE=VAL
;; --enable-FEATURE    == --enable-FEATURE=yes
;; --disable-FEATURE   == --enable-FEATURE=no
(define cf-feature-ref
  (getter-with-setter
   (^[name] (hash-table-get (ensure-package)'features name #f))
   (^[name val] (hash-table-put! (ensure-package)'features name val))))

;; API
;; --with-PACKAGE=VAL
;; --with-PACKAGE      == --with-PACKAGE=yes
;; --without-PACKAGE   == --with-PACKAGE=no
(define cf-package-ref
  (getter-with-setter
   (^[name] (hash-table-get (ensure-package)'packages name #f))
   (^[name val] (hash-table-put! (ensure-package)'packages name val))))

;; API
;; This is a macro, so that we can register helpstr to the global registry.
;; Hence arguments must be literal.
;(define-macro (cf-help-string cmdarg description)
  

;; API
;; Like AC_OUTPUT
(define (cf-output . files)
  (define base-defs (~ (ensure-package)'defs))
  (define (make-subst path-prefix)
    (receive (srcdir top_srcdir builddir top_builddir)
        (adjust-srcdirs path-prefix)
      (let1 defs (make-stacked-map (alist->hash-table
                                    `((srcdir       . ,srcdir)
                                      (top_srcdir   . ,top_srcdir)
                                      (builddir     . ,builddir)
                                      (top_builddir . ,top_builddir))
                                    'eq?)
                                   base-defs)
        (^[m]
          (let1 name (string->symbol (m 1))
            (or (dict-get defs name #f)
                (begin (warn "@~a@ isn't substituted." name)
                       #`"@,|name|@")))))))
  ;; We use '/' in the replaced pathname even on Windows; that's what
  ;; autoconf-generated configure does, and it's less likely to confuse
  ;; Unix-originated tools.
  (define (simplify-path+ path)
    (cond-expand
     [gauche.os.windows (string-tr (simplify-path path) "\\\\" "/")]
     [else (simplify-path path)]))
  (define (adjust-srcdirs path-prefix)
    (let ([srcdir    (~ base-defs'srcdir)]
          [tsrcdir   (~ base-defs'top_srcdir)]
          [builddir  (~ base-defs'builddir)]
          [tbuilddir (~ base-defs'top_builddir)])
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

  ;; Realize prefix and exec_prefix if they're not set.
  (when (equal? (cf$ 'prefix) "NONE")
    (cf-define 'prefix (cf$ 'default_prefix)))
  (when (equal? (cf$ 'exec_prefix) "NONE")
    (cf-define 'exec_prefix "${prefix}"))
  
  (dolist [f files]
    (let1 inf (build-path (cf$'srcdir) #`",|f|.in")
      (unless (file-is-readable? inf)
        (error "Cannot read input file ~s" inf))
      (unless (file-is-directory? (sys-dirname f))
        (make-directory* (sys-dirname f)))
      (file-filter-for-each (make-replace-1 f) :input inf :output f
                            :temporary-file #t :leave-unchanged #t)))
  0)

;; API
;; Show definitions.
(define (cf-show-variables :key (formatter (^[k v] (format #t "~16s ~s" k v))))
  (let1 dict (~ (ensure-package)'defs)
    (dolist [k (sort (dict-keys dict)
                     (^[a b] (string<? (x->string a) (x->string b))))]
      (formatter k (dict-get dict k))
      (newline))))

;;
;; Tests - programs
;;

;; API
;; Common featuer for AC_CHECK_PROG, AC_PATH_PROG etc.
;; Search one of programs listed in PROGS within PATHS.
;; PATHS can be #f - then we use PATH enviornment variable.
;; For each found program, FILTER is called with full path of the program;
;; it may return #f if the caller wants to exclude the program for some reason.
;; If a program found, and FILTER returns true, then this procedure returns
;; the full path of the program.  If no program found, #f is returned.
(define (check-for-program progs :key (paths #f) (filter #f))
  (define paths (or paths
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
;; If SYM is already cf-define'd, we don't do anything.
(define (cf-check-prog sym prog-or-progs
                       :key (value #f) (default #f) (paths #f) (filter #f))
  (unless (cf-defined? sym)
    (cf-msg-checking "for ~a" prog-or-progs)
    (if-let1 found (check-for-program (listify prog-or-progs)
                                      :paths paths :filter filter)
      (let1 result (or value (sys-basename found))
        (cf-msg-result "~a" result)
        (cf-define sym result))
      (begin (cf-msg-result "no")
             (and default (cf-define sym default))))))

;; API
;; cf-path-prog works like AC_PATH_PROG and AC_PATH_PROGS.
(define (cf-path-prog sym prog-or-progs
                      :key (value #f) (default #f) (paths #f) (filter #f))
  (unless (cf-defined? sym)
    (cf-msg-checking "for ~a" prog-or-progs)
    (if-let1 found (check-for-program (listify prog-or-progs)
                                      :paths paths :filter filter)
      (let1 result (or value found)
        (cf-msg-result "~a" result)
        (cf-define sym result))
      (begin (cf-msg-result "no")
             (and default (cf-define sym default))))))

