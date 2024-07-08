;;;
;;; gauche.package.commands - commands for gauche-packgae
;;;
;;;   Copyright (c) 2004-2024  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.package.commands
  (use gauche.collection)
  (use gauche.config)
  (use gauche.package)
  (use gauche.package.util :only (copy-templates))
  (use gauche.package.build :only (gauche-package-build
                                   gauche-package-tarball))
  (use gauche.package.compile)
  (use gauche.parseopt)
  (use gauche.process)
  (use gauche.version)
  (use file.filter)
  (use file.util)
  (use scheme.list)
  (use srfi.13)
  (use text.fill)
  (use text.tr)
  (use text.multicolumn :only (display-multicolumn))
  (use util.levenshtein :only (re-distances))
  (use util.match)
  (export gauche-package-config
          gauche-package-command
          gauche-package-usage))
(select-module gauche.package.commands)

(define *commands* '())

;; API
;;  Show usage of gauche-package, or one of the subcommands.
(define (gauche-package-usage :optional (cmd #f))
  (if cmd
    (cond [(assoc-ref *commands* cmd) => (cut <> 'help)]
          [else
           (print "Unknown command name: " cmd)
           (print "Valid commands are: " (map car (reverse *commands*)))])
    (begin
      (print "Usage: gauche-package <command> [options] <args> ...")
      (print "Commands:")
      (dolist [cmd (reverse *commands*)]
        (format #t "  ~15a - ~a\n" (car cmd) ((cdr cmd) 'summary)))
      (print "Type 'gauche-package help <command>' for detailed help of each command.")))
  (exit 0))

(define *config* '())

;; API
;;  Read ~/.gauche-package and set up *config*
(define (gauche-package-config)
  (let ((config-file (build-path (home-directory) ".gauche-package")))
    (when (file-is-readable? config-file)
      (set! *config* (with-input-from-file config-file read)))
    (dolist (p *config*)
      (when (eq? (car p) 'build-dir)
        (set! (cdr p) (expand-path (cdr p))))))
  )

;; API
;;  Returns a closure to execute a subcommand COMMAND.
;;  Returned closure takes a list of arguments (excluding the subcommand name)
(define (gauche-package-command command)
  (if-let1 handler (assoc-ref *commands* command)
    (^[args] (apply handler 'run args))
    (error "Unknown gauche-package command:" command)))

(define (search-similar package-name)
  (let* ([names (map ($ (cut ~ <> 'name) $ path->gauche-package-description $)
                     (gauche-package-description-paths :all-versions #t))]
         [distances (re-distances package-name names :cutoff 5)])
    (filter-map (^[name distance] (and distance name)) names distances)))

(define (no-such-package package-name)
  (let1 candidates (search-similar package-name)
    (if (null? candidates)
      (print ";; I don't know about package " package-name)
      (begin
        (print ";; I don't know about package " package-name ".  You meant the following?")
        (display-multicolumn candidates :indent 4)))))

;;======================================================
;; Command definitions
;;

;; Define-cmd associates command name strings to handler procedures.
;; The handler procedure does a few different things, based on the
;; first arguments.
;;
;;   (handler 'run arg ...)    Run the command functionarity.
;;   (handler 'help)           Print detailed help message and exit.
;;   (handler 'summary)        Returns one-line summary string.
;;
;; This awkward interface is because gauche.parseopt#option-parser-help-string
;; needs to be called in the dynamic scope of let-args.

(define-syntax define-cmd
  (er-macro-transformer
   (^[f r c]
     (match-let1 (name                      ;string command name
                  (synopsis summary detail) ;help info
                  (cmdargspec ...)          ;argspec, as in let-args
                  . body)                   ;body expressions
         (cdr f)
       ;; Within body, the identifier 'args' is bound to the cmdline
       ;; arguments except options, and 'usage-self' is bound to a
       ;; thunk to display help string.
       `(push! *commands*
               (cons ,name
                     (^[msg . args]
                       (let-args args ,(append cmdargspec 'args)
                         (let ((usage-self
                                (^[] (print-cmd-usage ,name ,synopsis
                                                      ,summary ,detail))))
                           (case msg
                             [(run) (let () ,@body)]
                             [(help) (usage-self)]
                             [(summary) ,summary]))))))))))

(define (print-cmd-usage name synposis summary detail)
  (print "Usage: " synposis)
  (print "  " summary)
  (print detail)
  (let1 opts (option-parser-help-string)
    (unless (#/^\s*$/ opts)
      (print)
      (print "Options:")
      (print opts)))
  (exit 0))

;;------------------------------------------------------
;; install
;;
(define-cmd "install"
  ("install [options] <tarball-path/url>"
   "Fetch, extract, configure, make & install"
   "Argument:
  a path to a tarball (uncompressed, gzipped or bzipped), or URL (http or ftp)
  of a tarball.")
  ([dry-run "n|dry-run" ? "shows commands to be executed, without running them."]
   [copts   "C|configure-options=s{OPTIONS}" #f
            ? "passes {OPTIONS} to ./configure.  overrides -r."]
   [reconf  "r|reconfigure" ? "uses the same configure options as before"]
   [clean   "clean" ? "clean up the build directory after installation"]
   [sudo    "S|install-as=s{USER}" #f ? "sudo to {USER} when installing"])
  (unless (length=? args 1) (usage-self))
  (gauche-package-build (car args)
                        :config *config*
                        :dry-run dry-run :install #t :clean clean
                        :sudo-install sudo
                        :reconfigure reconf
                        :configure-options copts))

;;------------------------------------------------------
;; build
;;
(define-cmd "build"
  ("build [options] <tarball-path/url>"
   "Fetch, extract, configure & make"
   "Argument:
  a path to a tarball (uncompressed, gzipped or bzipped), or URL (http or ftp)
  of a tarball.")
  ([dry-run "n|dry-run" ? "shows commands to be executed, without running them."]
   [copts   "C|configure-options=s{OPTIONS}" #f
            ? "passes {OPTIONS} to ./configure.  overrides -r."]
   [reconf  "r|reconfigure" ? "uses the same configure options as before."])
  (unless (length=? args 1) (usage-self))
  (gauche-package-build (car args)
                        :config *config*
                        :dry-run dry-run
                        :reconfigure reconf
                        :configure-options copts))

;;------------------------------------------------------
;; reconfigure
;;
(define-cmd "reconfigure"
  ("reconfigure <package>"
   "Show configure options of <package>"
   "Argument: a package name.
  If the package has installed .gpd (Gauche package description) file, show
  the options to the configure script when the package is built.")
  ()
  (match args
    [(package)
     (if-let1 gpd (find-gauche-package-description package :all-versions #t)
       (print (ref gpd 'configure))
       (no-such-package package))]
    [_ (usage-self)]))

;;------------------------------------------------------
;; info
;;
(define-cmd "info"
  ("info <package>"
   "Show information of installed package <package>"
   "Argument: a package name.")
  ()
  (match args
    [(package)
     (if-let1 gpd (find-gauche-package-description package :all-versions #t)
       (let1 desc (string-split (or (~ gpd'description)
                                   "(No description available)")
                               "\n")
         (format #t "Package ~a\n" (~ gpd'name))
         (format #t "  ~a\n" (car desc))
         (unless (null? (cdr desc))
           (display-filled-text (string-join (cdr desc) " ")
                                :indent 2 :width 75)
           (newline))
         (format #t "   repository: ~a\n" (~ gpd'repository))
         (format #t "      version: ~a\n" (~ gpd'version))
         (format #t "      require: ~:w\n" (~ gpd'require))
         (format #t "      authors: ~:w\n" (~ gpd'authors))
         (format #t "  maintainers: ~:w\n" (~ gpd'maintainers))
         (format #t "     licenses: ~:w\n" (~ gpd'licenses))
         (format #t "     provides: ~:w\n" (~ gpd'providing-modules))
         (format #t "     homepage: ~a\n" (~ gpd'homepage))
         (format #t "superseded-by: ~a\n" (~ gpd'superseded-by))
         (format #t "    configure: ~a\n" (~ gpd'configure)))
       (no-such-package (car args)))]
    [_ (usage-self)]))

;;------------------------------------------------------
;; list
;;
(define-cmd "list"
  ("list"
   "List known installed packages"
   "  Only packages that have .gpd file are listed.")
  ([all?  "a|all" ? "shows all packages, even the ones that are installed for
                     other versions of Gauche."])
  (let1 gpds (map path->gauche-package-description
                  (gauche-package-description-paths :all-versions all?))
    (dolist (gpd (sort gpds
                       (lambda (a b)
                         (string<= (ref a 'name) (ref b 'name)))))
      (if (version=? (gauche-version) (ref gpd 'gauche-version))
        (format #t " ~19a ~8a~%" (ref gpd 'name) (ref gpd 'version))
        (when (or all?
                  (abi-version=? (gauche-version) (ref gpd 'gauche-version)))
          (format #t " ~19a ~8a (@ Gauche ~a)~%"
                  (ref gpd 'name) (ref gpd 'version)
                  (ref gpd 'gauche-version))))
      )))

;; Since 0.9, we keep ABI compatibility across all micro versions in
;; principle, so we only need to compare major and minor versions.
;; Eventually it would be better to have an interface to obatin abi-version
;; of running Gauche, and also to record abi version in .gpd file.
(define (abi-version=? v0 v1)
  (equal? (take* (string-split v0 #[.]) 2 #f)
          (take* (string-split v1 #[.]) 2 #f)))

;;------------------------------------------------------
;; make-gpd
;;
(define-cmd "make-gpd"
  ("make-gpd <name> <param> ..."
   "Make gpd file (called from the configure script)"
   "
  This command is to create a gpd (Gauche package description) file.
  Usually the user doens't invoke this command.  It is intended to be
  called within the configure script, like the following:

    gauche-package make-gpd Foo \\
       -version $PACKAGE_VERSION \\
       -configure \"./configure $GAUCHE_PACKAGE_CONFIGURE_ARGS\"

  If you generate template configure.ac by 'gauche-package generate',
  the make-gpd stuff is included in it.")
  ()
  (match args
    [(package . params)
     (let loop ([p params]
                [r '()])
       (cond [(null? p)
              (let1 gpd (apply make <gauche-package-description>
                               :name package
                               (reverse! r))
                (with-output-to-file #"~|package|.gpd"
                  (cut write-gauche-package-description gpd)))]
             [(null? (cdr p))
              (exit 1 "gauche-package: make-gpd: parameter list not even")]
             [else
              (loop (cddr p)
                    (list* (cadr p)
                           (make-keyword (string-trim (car p) #[-:]))
                           r))]))]
    [_ (usage-self)]))

;;------------------------------------------------------
;; compile
;;
(define-cmd "compile"
  ("compile [options] [<extension-name>] <file> ..."
   "Compile and link an extension module from sources"
   "
  <file> can be a Gauche source (*.scm), a stub file (*.stub), or
  any types the system's C compiler accepts.

  By default, this command compiles given files with the options appropriate
  to compile Gauche extensions, then links a dynamically loadable object
  <extension-name>.so (the suffix may differ among systems).
  If '-c' option is given, only compilation of a single file is done.
  You can give extra flags for the compiler/linker via options.

  <extension-name> must match the name passed to SCM_INIT_EXTENSION,
  and must be a valid C identifier.  (NB: <extension-name> is used only
  as the filename and the argument of SCM_INIT_EXTENSION, and has nothing
  to do with the package name or the module name.

  Note: This command looks at the environment variables CC, CPPFLAGS, CFLAGS,
  LDFLAGS and LIBS as the default value of the --cc, --cppflags, --cflags,
  --ldflags and --libs, respectively.")
  ([dry-run      "n|dry-run" ? "just display commands to be executed."]
   [verbose      "v|verbose" ? "reports commands being executed."]
   [compile-only "c|compile" ? "compile only.  with this option, <module>
                                shouldn't be given and only one <file> is
                                allowed."]
   [output       "o|output=s{NAME}" ? "alternative output file name"]
   [clean        "clean" ? "instead of compile and link, removes the
                            intermediate and output file(s) that would be
                            generated otherwise.  useful for 'make clean'."]
   [keep-c       "k|keep-c-files" ? "do not remove intermediate generated C
                                     files."]
   [no-line      "no-line" ? "do not emit #line directives in generated C
                              files."]
   [srcdir       "S|srcdir=s{DIR}" ? "specify the source directory when
                                      building out-of-tree."]
   [gauche-builddir "gauche-builddir=s{DIR}"
                    ? "specify the top builddir of the Gauche when the
                      extensions should be compiled for /uninstalled/ Gauche."]
   [local        "l|local=s{PATH:PATH:...}"
                 ? "adds {PATH}/include to inlcude paths and {PATH}/lib to
                    library search paths for compiling."]
   [cc           "cc=s{CC}" ? "alternative C compiler.  Note that the compiler
                               should have compatible ABI with the one that
                               compiled Gauche.
                               To compile C++ code, use --c++ below."]
   [c++          "c++=s{CXX}" ? "compile the source with c++, with the C++
                                 compiler. The precompiler generate *.cpp
                                 instead of *.c. This and --cc option are
                                 mutually exclusive."]
   [cppflags     "cppflags=s{CPPFLAGS}"
                 ? "extra cpp flags for compile, such as -I/usr/local"]
   [cflags       "cflags=s{CFLAGS}" ? "extra cc flags for compile"]
   [ldflags      "ldflags=s{LDFLAGS}" ? "extra ld flags"]
   [libs         "libs=s{LIBS}" ? "extra libraries"])
  ;; process 'local' option
  ;; e.g. "/usr/local:/pkg:/Program Files"
  ;;   => "-I /usr/local/include -I /pkg/include -I '/Program Files/include'"
  ;; etc.
  (define (local-paths prefix subdir)
    (and local
         (not (string-null? local))
         (not (string-null? prefix))
         (string-join (map (^[path]
                             (shell-escape-string (build-path path subdir)))
                           (string-split local #\:))
                      #" ~prefix" 'prefix)))
  ;; preprocess parameters.
  ;; if parameter is not given, look at the named environment variable.
  (define (param given envname . additionals)
    (let1 v
        (filter values (cons (or given (sys-getenv envname)) additionals))
      (and (not (null? v)) (string-join v))))

  (when (and cc c++)
    (exit 1 "Options --cc and --c++ are mutually exclusive."))
  (let ([use-c++  (boolean c++)]
        [compiler (or c++ (param cc "CC"))]
        [cppflags (param cppflags "CPPFLAGS" (local-paths "-I" "include"))]
        [cflags   (param cflags   "CFLAGS")]
        [ldflags  (param ldflags  "LDFLAGS"
                         (local-paths "-L" "lib")
                         (local-paths (gauche-config "--rpath-flag") "lib"))]
        [libs     (param libs     "LIBS")])
    (cond
     [clean
      (unless (null? args)
        (gauche-package-clean (if compile-only #f (car args))
                              (if compile-only args (cdr args))
                              :output output))]
     [compile-only
      (unless (length=? args 1) (usage-self))
      (gauche-package-compile (car args)
                              :dry-run dry-run :verbose verbose
                              :srcdir srcdir
                              :gauche-builddir gauche-builddir
                              :keep-c keep-c :no-line no-line
                              :output output :c++-mode use-c++
                              :cc compiler
                              :cppflags cppflags :cflags cflags)]
     [else
      (when (length<=? args 1) (usage-self))
      (gauche-package-compile-and-link (car args) (cdr args)
                                       :dry-run dry-run :verbose verbose
                                       :srcdir srcdir
                                       :gauche-builddir gauche-builddir
                                       :keep-c keep-c :no-line no-line
                                       :output output :c++-mode use-c++
                                       :cc compiler :ld compiler
                                       :cppflags cppflags :cflags cflags
                                       :ldflags ldflags :libs libs)]))
    )

;;------------------------------------------------------
;; generate
;;
(define-cmd "generate"
  ("generate [options] package-name [module-name]"
   "Generate template source tree for a new Gauche extension"
   "
  This command creates a directory <package-name> under the current
  directory, and populates it with the template files.  It is an easy
  way to start writing Gauche extension.

  <package-name> is the one you'll see as a part of the name of tarball,
  for example, \"Gauche-gl\".  It is the name of the unit of distribution
  and installation of your package.

  <module-name>, if given, is used as the name of the module
  instead of <package-name>.  It may affect the generated directory
  structure.")
  ([autoconf "autoconf" ? "generates configure.ac to be processed with GNU
                           autoconf, instead of a Scheme configure script."]
   [verbose  "v|verbose" ? "operates verbosely."]
   [scheme-only "S|scheme-only" ? "generates a package that is implemented in
                                   pure Scheme (i.e. no C files)."]
   [tmpl-dir "template-dir=s{DIR}" ? "specifies the directory where template
                                      files are stored.  By default, they are
                                      stored along installed Gauche libraries."]
   [pkg-dir  "package-dir=s{DIR}" ? "specifies the directory where the files
                                     are generated.  By default, the package
                                     name is used as the directory name."]
   [license "license=s{TYPE}" ? "causes to include license info in package.scm
                                 and COPYING file.  Currently, 'BSD' and 'MIT'
                                 are supported as {TYPE}.  (Both 'BSD' and
                                 'BSD3' are for 3-clause BSD)."])
  (let-optionals* args ([package-name #f]
                        [module-name #f]
                        . more)
    (unless (and package-name (null? more)) (usage-self))
    (unless (#/^[\w-]+$/ package-name)
      (exit 1 "Invalid character in package-name ~s: You can only use alphanumeric chars, underscore, and minus sign." package-name))
    (unless (or (not module-name) (#/^[\w.-]+$/ module-name))
      (exit 1 "Invalid character in module-name ~s" module-name))
    (unless (member license '(#f "BSD" "BSD3" "MIT"))
      (exit 1 "License ~s is unsupported.  You need to add license description manually after the project is generated."))
    (let* ([package-name*  (rxmatch-case package-name
                             (#/^Gauche-(.*)/ (#f rest) rest)
                             (else package-name))]
           [srcdir (or tmpl-dir
                       (build-path (sys-dirname (gauche-library-directory))
                                   "package-templates"))]
           [dstdir (or pkg-dir package-name)]
           [lic (and license (string->symbol (string-downcase license)))])
      (copy-templates srcdir dstdir package-name
                      :module-name (and module-name
                                        (string->symbol module-name))
                      :scheme-only scheme-only
                      :use-autoconf autoconf
                      :license lic
                      :verbose verbose))))

;;------------------------------------------------------
;; populate
;;
(define-cmd "populate"
  ("populate [options]"
   "Add missing files of the package"
   "
  Run this command in the toplevel directory of an extention package.
  It checks the content of 'package.scm' and the existing files, and
  prompts missing files and package entries.  It is useful to turn
  existing source tree to an extension package, or update the package
  for the newer versions of Gauche.")
  ([verbose  "v|verbose" ? "operates verbosely."]
   [tmpl-dir "template-dir=s{DIR}" ? "specifies the directory where template
                                      files are stored.  By default, they are
                                      stored along installed Gauche libraries."]
   [pkg-dir  "package-dir=s{DIR}" ? "specifies the directory where the files
                                     are generated.  By default, the current
                                     directory is assumed.  The directory must
                                     contain 'package.scm'."]
   [license "license=s{TYPE}" ? "Include license info in package.scm and
                                 COPYING file.  Currently, 'BSD' and 'MIT' are
                                 supported as {TYPE}.  (Both 'BSD' and 'BSD3'
                                 are for 3-clause BSD)."])
  (define dstdir (or pkg-dir "."))
  (define (P file) (build-path dstdir file))
  (define (populate-with-package) ;when we have package.scm
    (let* ([package
            (guard (e [(<package-description-error> e)
                       (exit 1 "Can't read package.scm: ~a" (~ e'message))])
              (path->gauche-package-description (P "package.scm")))]
           [lic (cond [license (string->symbol (string-downcase license))]
                      [(~ package'licenses) pair?
                       => ($ string->symbol $ string-downcase $ car $)]
                      [else #f])]
           [mod (if (pair? (~ package'providing-modules))
                  (car (~ package'providing-modules))
                  (exit 1 "Missing providing-modules in package.scm."))])
      (do-copy! (~ package'name) mod (~ package'authors) lic)))
  (define (do-copy! package-name module-name authors license)
    (let ([srcdir (or tmpl-dir
                      (build-path (sys-dirname (gauche-library-directory))
                                  "package-templates"))])
      (receive [installed preserved]
          (copy-templates srcdir dstdir package-name
                          :module-name module-name
                          :scheme-only #t ;for now
                          :license license
                          :licensor (string-join authors ", ")
                          :preserve #t
                          :verbose verbose)
        (format #t "File~p installed:\n" (length installed))
        (display-multicolumn installed :indent 4)
        (format #t "File~p preserved:\n" (length preserved))
        (display-multicolumn preserved :indent 4))))
  (define (guess-package-name)        ;when we don't have package.scm
    (cond [(file-exists? (P "configure.ac"))
           (cond [(any #/AC_INIT\(\s*([\w-]+)/
                       (file->string-list (P "configure.ac")))
                  => (^m (m 1))]
                 [else (exit 1 "Failed to obtain package name from 'configure.ac'")])]
          [(file-exists? (P "configure"))
           (let1 bad
               (^[] (exit 1 "Failed to obtain package name from 'configure'"))
             (guard (e [(<read-error> e) (bad)])
               (or (any (^x (match x
                              [('cf-init pkg-name . _) pkg-name]
                              [_ #f]))
                        (file->sexp-list (P "configure")))
                   (bad))))]
          [else #f]))
  (define (populate-without-package package-name)
    (do-copy! package-name #f '() #f)
    (print "We created a template 'package.scm'.  Make sure to edit it for \
              your package.  You might also want to change 'configure'."))

  (cond [(file-exists? (P "package.scm")) (populate-with-package)]
        [(guess-package-name) => populate-without-package]
        [else  (exit 1 "Cannot find any of 'package.scm', 'configure.ac', \
                        or 'configure'")]))

;;------------------------------------------------------
;; make-tarball
;;

(define-cmd "make-tarball"
  ("make-tarball [options]"
   "Create tarball of the package for distribution."
   "
  This command must be run at the top source directory of the package.  It
  first cleans the directory for distribution (by make maintainer-clean,
  run configure, then make disclean), then call tar to create
  ../$PACKAGE_NAME-$VERSION.tgz.  Certain predefined files (such as .git)
  are excluded; if you want to exclude other files, list them in DIST_EXCLUDE.")
  ([dry-run "n|dry-run" ? "just display commands to be executed."]
   [verbose "v|verbose" ? "reports package contents."])
  (unless (or (file-exists? "configure")
              (file-exists? "configure.ac"))
    (exit 1 "`gauche-package make-tarball' should be run in the top source directory."))
  (gauche-package-tarball :config *config* :dry-run dry-run :verbose verbose))

;;------------------------------------------------------
;; help
;;

(define-cmd "help"
  ("help <command>"
   "Show detailed help of <command>"
   "")
  ()
  (apply gauche-package-usage args))
