;;;
;;; gauche.cgen.precomp - Precompile Scheme into C data
;;;
;;;   Copyright (c) 2004-2019  Shiro Kawai  <shiro@acm.org>
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

;;; Precompiler takes compiled VM instruction array and dumps it
;;; as C code.

(define-module gauche.cgen.precomp
  (use srfi-1)
  (use srfi-13)
  (use gauche.cgen)
  (use gauche.cgen.stub)
  (use gauche.cgen.tmodule)
  (use gauche.vm.insn)
  (use gauche.parameter)
  (use gauche.sequence)
  (use gauche.experimental.lamb)
  (use file.util)
  (use util.match)
  (use util.toposort)
  (use text.tr)
  (export cgen-precompile cgen-precompile-multi
          cgen-scm-path->c-file cgen-c-file->initfn))
(select-module gauche.cgen.precomp)

(autoload gauche.cgen.optimizer optimize-compiled-code)

;;================================================================
;; Main Entry point
;;
;;  The cgen-precompile function reads a scheme file "<foo>.scm",
;;  compiles it and dumps the result as a C source "<foo>.c".
;;  It may also generates "<foo>.sci", an interface definition file
;;  which contains forms like define-module, use, export, etc.
;;
;;  A generated C file contains an initialization function, named
;;  Scm_Init_<foo> by default.   Typically it is called by "extension
;;  initializer", which is invoked when the DSO file is loaded by
;;  dynamic-load.
;;
;;    Example1:
;;
;;     *  An extension extension.so is built from extension.c,
;;        foo.scm and bar.scm.
;;
;;     *  extension.c must contain a funciton Scm_Init_extension(),
;;        which is the extension initializer.  It is called from
;;        (dynamic-load "extension")
;;
;;     *  By processing foo.scm and bar.scm, you'll get foo.c and
;;        bar.c, each contain Scm_Init_foo() and Scm_Init_bar(),
;;        respectively.
;;
;;     *  Scm_Init_extension() is responsible to call Scm_Init_foo()
;;        and Scm_Init_bar().
;;
;;  Sometimes sources consist of Scheme files only.  In which case,
;;  giving true value to ext-initializer keyword argument makes
;;  the inialization function work as an extension initializer.
;;
;;    Example2:
;;
;;     * An extension extension.so is built from extension.scm.
;;
;;     * Processing extension.scm with :ext-initializer #t
;;       makes generated Scm_Init_extension() work as an extension
;;       initializer.
;;
;;  If there are more than one Scheme files and you want to make
;;  one of its initializer funtion as an extension initializer,
;;  give :sub-initializers argument to the 'main' source whose
;;  initialization function becomes an extension initializer.
;;
;;    Example3:
;;
;;     * An extension extension.so is built from extension.scm,
;;       foo.scm and bar.scm
;;
;;     * foo.c and bar.c are to be created normally.  Each has
;;       Scm_Init_foo() and Scm_Init_bar(), respectively.
;;
;;     * extension.c are to be created with :ext-initializer #t
;;       and :sub-initializers '(Scm_Init_foo Scm_Init_bar).
;;       The generated Scm_Init_extension() works as an extension
;;       initializer, _and_ it calls Scm_Init_foo() and Scm_Init_bar().
;;

;; Keyword arguments:
;;
;; ext-initializer : See above.
;; sub-initializers : See above.
;;
;; out.c :   Alternative name for C output  #f to use the default
;;           (path-swap-extension (sys-basename src) "c").
;; out.sci : Alternative name for SCI output.  If #f, take the default
;;           behavior which is:
;;           - If the source's first form is define-module, use
;;             (strip-prefix prefix (path-swap-extension src "sci"))
;;           - Otherwise, do not produce SCI output.
;;           If the source has define-module form and you don't want
;;           to create SCI output, pass "/dev/null" to this argument.
;;
;; strip-prefix : Used to derive sci file path from the source file name.
;;           This argument is ignored when out.sci is specified.
;;           - If #f, the sci file is the same as src except its suffix is
;;             substituted for ".sci".
;;           - If #t, the sci file is the basename of src, with its suffx
;;             substituted for ".sci".
;;           - Otherwise, this argument must be a string.  First src's
;;             prefix is checked to match this argument; if they match,
;;             the matching prefix is removed from SRC, then its extension
;;             is substituted for ".sci".  If SRC's suffix does not match,
;;             it works just like #f is given to this argument.
;;           This feature is useful to generate *.sci files mirroring
;;           the source directory hierarchy.
;;
;; predef-syms : A list of strings, to insert #defines at the top of
;;      generated C source.
;;
;; macros-to-keep : List of names of private macros that should be included
;;      in the output.  Usually private macros (macros bound to a variable
;;      which isn't exported) are not included in the output.  But sometimes
;;      hygienic public macros expands to a call of private macros, and
;;      gauche.cgen.precomp cannot detect such dependencies yet.

(define (cgen-precompile src . keys)
  (with-tmodule-recording
   <ptmodule>
   (apply %cgen-precompile src keys)))

;; Precompile multiple Scheme sources that are to be linked into
;; single DSO.  Need to check dependency.  The name of the first
;; source is used to derive DSO name.
(define (cgen-precompile-multi srcs
                               :key (ext-initializer #f)
                                    ((:strip-prefix prefix) #f)
                                    ((:dso-name dso) #f)
                                    (predef-syms '())
                                    (macros-to-keep '())
                                    (single-sci-file #f)
                                    (extra-optimization #f))
  (define (precomp-1 main src)
    (let* ([out.c (cgen-scm-path->c-file (strip-prefix src prefix))]
           [initname (cgen-c-file->initfn out.c)])

      (%cgen-precompile src
                        :out.c out.c
                        :dso-name (or dso (basename-sans-extension main))
                        :predef-syms predef-syms
                        :strip-prefix prefix
                        :macros-to-keep macros-to-keep
                        :extra-optimization extra-optimization
                        :ext-initializer (and (equal? src main)
                                              ext-initializer)
                        :initializer-name initname)))
  (define (tweak-sci sci) ; for consolidating sci files. returns sexp list
    (if (not sci)
      '()
      (let* ([sexps (file->sexp-list sci)]
             [mod (any (^p (match p [('define-module m . _) m] [_ #f])) sexps)])
        (if mod
          (append sexps `((provide ,(module-name->path mod))))
          (begin
            (warn "Couldn't find define-module form in ~a." sci)
            sexps)))))
  (match srcs
    [() #f]
    [(main . subs)
     (clean-output-files srcs prefix)
     (let* ([srcs (order-files-by-dependency srcs)]
            [scis ($ with-tmodule-recording <ptmodule>
                     $ map (cut precomp-1 main <>) srcs)])
       (when single-sci-file
         (and-let* ([main-sci (any (^[s i] (and (equal? s main) i)) srcs scis)]
                    [other-scis (reverse (delete main-sci scis))]
                    [sexps (append (append-map tweak-sci other-scis)
                                   (file->sexp-list main-sci))])
           (with-output-to-file main-sci
             (^[]
               (print ";; generated automatically.  DO NOT EDIT")
               (print "#!no-fold-case")
               (dolist [x sexps] (write x) (newline))))
           (for-each remove-files other-scis))))]
    ))

;; Common stuff -- process single source
;; Returns sci file name if it's created.
(define (%cgen-precompile src
                          :key (out.c #f)
                               (out.sci #f)
                               ((:strip-prefix prefix) #f)
                               (ext-initializer #f)
                               ((:dso-name dso) #f)
                               (initializer-name #f)
                               (sub-initializers '())
                               (predef-syms '())
                               (macros-to-keep '())
                               (extra-optimization #f))
  (define (do-it)
    (parameterize ([omitted-code '()])
      (setup ext-initializer sub-initializers)
      (with-input-from-file src
        (cut emit-toplevel-executor
             (reverse (generator-fold compile-toplevel-form '() read))))
      (finalize sub-initializers)
      (cgen-emit-c (cgen-current-unit))))
  (let ([out.c   (or out.c (path-swap-extension (sys-basename src) "c"))]
        [out.sci (or out.sci
                     (and (check-first-form-is-define-module src)
                          (strip-prefix (path-swap-extension src "sci")
                                        prefix)))])
    ;; see PARAMETERS section below
    (parameterize ([cgen-current-unit (get-unit src out.c predef-syms
                                                ext-initializer)]
                   [dso-name (cons
                              (or dso
                                  (and ext-initializer
                                       (basename-sans-extension out.c)))
                              initializer-name)]
                   [vm-eval-situation SCM_VM_COMPILING]
                   [private-macros-to-keep macros-to-keep]
                   [run-extra-optimization-passes extra-optimization])
      (select-tmodule 'gauche)
      (cond [out.sci
             (make-directory* (sys-dirname out.sci))
             (call-with-output-file out.sci
               (^p (display ";; generated automatically.  DO NOT EDIT\n" p)
                   (display "#!no-fold-case\n" p)
                   (parameterize ([ext-module-file p]
                                  [ext-module-forms '()])
                     (do-it)
                     (flush-ext-module))))]
            [else
             (parameterize ([ext-module-file #f])
               (do-it))]))
    out.sci))

;; Utility API to derive C filename and initfn name from
;; Scheme path.
;; e.g. "binary/io.scm" -> "binary--io.c"
;;      "binary--io.c" -> "Scm_Init_binary__io"
;; We expose these so that other tools will use consistent naming.
(define (cgen-scm-path->c-file path)
  ($ regexp-replace-all #/[\/\\]/
     ($ path-swap-extension
        (sys-normalize-pathname path :canonicalize #t)
        "c")
     "--"))

(define (cgen-c-file->initfn c-name)
  (let1 initfn (string-tr (path-sans-extension c-name) "-+." "___")
    #"Scm_Init_~initfn"))

;;================================================================
;; Transient modules
;;

;; To avoid interference between the host compiler and the target code,
;; the compiler creates a temporary anonymous module and works in it.
;; The basic mechanism is provided by gauche.cgen.tmodule.  We augument
;; it with adding special bindings.

(define-class <ptmodule> (<tmodule>) ())

(define-method initialize ((m <ptmodule>) initargs)
  (next-method)
  ;; redefine several toplevel syntaxes precompiler needs to recognize
  (for-each (^f (eval f (~ m'module))) *special-handlers*))

(define (eval-in-current-tmodule expr)
  (eval expr (~ (current-tmodule)'module)))

;; Expr -> CompiledCode
(define (compile-in-current-tmodule expr)
  (compile expr (~ (current-tmodule)'module)))

;;================================================================
;; Parameters
;;

;; A pair of the name of the generated DSO (w/o extension) and the name
;; of the initializer function.
(define dso-name (make-parameter #f))

;; keep the list of exported bindings (or #t if export-all)
(define compile-module-exports (make-parameter '()))

;; when we're compiling extension module (--ext-module=file), this parameter
;; keeps a port to the specified file.  the file becomes a module definition
;; file, containing define-module and dynamic-load forms, as well as the
;; exported macros.
;; NB: we insert (dynamic-load ...) just after select-module in the ext-module
;; file, assuming the source file has standard layout.
(define ext-module-file (make-parameter #f))

;; The forms to be written out to ext-module file (sci) are pushed
;; on this parameter.
(define ext-module-forms (make-parameter '()))

;; list of private macros that should be included in the output.
;; (--keep-private-macro=name,name,...)
;; usually private macros (macros bound to a variable which isn't exported)
;; are discarded, but sometimes hygienic public macros expands to a call
;; of private macros.  precomp cannot detect such dependency yet, and
;; so they need to be explicitly listed for the time being.
(define private-macros-to-keep (make-parameter '()))

;; Experimental: Run extra optimization during AOT compilation.
(define run-extra-optimization-passes (make-parameter #f))

;;================================================================
;; Bridge to the internal stuff
;;

;; compatibility kludge
(define compile       (with-module gauche.internal compile))
(define %procedure-inliner
  (with-module gauche.internal %procedure-inliner))
(define vm-code->list (with-module gauche.internal vm-code->list))
(define vm-eval-situation
  (with-module gauche.internal vm-eval-situation))
(define global-eq?? (with-module gauche.internal global-eq??))
(define make-identifier (with-module gauche.internal make-identifier))
(define pair-attributes (with-module gauche.internal pair-attributes))
(define pair-attribute-get (with-module gauche.internal pair-attribute-get))

(define-constant SCM_VM_COMPILING 2) ;; must match with vm.h

;;================================================================
;; Utilities
;;

(define (get-unit src out.c predef-syms ext-init?)
  (let ([base (basename-sans-extension out.c)]
        [initfn (cgen-c-file->initfn out.c)])
    (rlet1 u (make <cgen-stub-unit>
               :name base :c-name-prefix (cgen-safe-name base)
               :preamble `(,#"/* Generated automatically from ~|src|.  DO NOT EDIT */")
               :init-prologue (format "~avoid ~a() {"
                                      (if ext-init? "SCM_EXTENSION_ENTRY " "")
                                      initfn)
               )
      (parameterize ([cgen-current-unit u])
        (for-each cgen-define predef-syms)
        (cgen-include "<gauche.h>")))))

(define (strip-prefix path prefix)
  (cond
   [(not prefix) path]
   [(eq? prefix #t) (sys-basename path)]
   [else
    (let1 pre (if (#/[\/\\]$/ prefix) prefix (string-append prefix "/"))
      (if (string-prefix? pre path)
        (string-drop path (string-length pre))
        path))]))

(define (basename-sans-extension path)
  (path-sans-extension (sys-basename path)))

;; Read the first form.
;; We don't read the entire content of the file, since it may contain
;; srfi-10 read-time constructor that we don't know about yet.
(define (first-form src) (with-input-from-file src read))

;; Check if the first form is define-module.
(define (check-first-form-is-define-module src)
  (match (first-form src)
    [('define-module . _) #t]
    [else #f]))

;; Returns (<module> <srcname> (<depended> ...))
(define (get-module-dependency src)
  (match (first-form src)
    [('define-module modname . forms)
     (list modname src
           (filter-map (^x(match x [('use mod . _) mod] [_ #f])) forms))]
    [_ #f]))

;; Sort the given list of source files so that each file only depends on
;; the files that appear later.
(define (order-files-by-dependency srcs)
  (let* ([deps (filter-map get-module-dependency srcs)]
         [sorted (topological-sort (map (^.[(n _ ns) (cons n ns)]) deps))]
         [sorted-srcs (filter-map (^s (cond [(assq s deps) => cadr]
                                            [else #f]))
                                  sorted)]
         [unsorted-srcs (lset-difference string=? srcs sorted-srcs)])
    (append sorted-srcs unsorted-srcs)))

;; Removes *.sci files before start compiling so that the old file
;; won't interfere with compilation.
(define (clean-output-files scms prefix)
  (dolist [s scms]
    (when (equal? (path-extension s) "sci")
      (error "source file list contains *.sci file:" s))
    (sys-unlink (strip-prefix (path-swap-extension s "sci") prefix))))

;; Writers out FORM to *.sci file if we're generating one.  No-op if we aren't
;; generating *.sci file.
;; TODO: Since *.sci file is read back as ordinary Scheme file, we can't
;; include identifiers in it.  We strip identifier info for now, but it can
;; break hygiene.  Solution: We should think the feature of writing arbitrary
;; code to *.sci as a transient measure and eventually we should have a way
;; to serialize Scheme code, including indentifier info, to the file.
(define (write-ext-module form)
  (push! (ext-module-forms) (unwrap-syntax form)))

(define (flush-ext-module)
  (cond [(ext-module-file) =>
         (^p (dolist [f (reverse (ext-module-forms))]
               (write f p) (newline p)))]))

(define (setup ext-init? subinits)
  (cgen-decl "#include <gauche/code.h>")
  (cond [(and ext-init? (ext-module-file))
         => (^[extm]
              (cgen-decl "#include <gauche/extend.h>")
              (let* ([extname ($ path-sans-extension
                                 $ sys-basename $ port-name extm)]
                     [safe-extname (regexp-replace-all #/\W/ extname "_")])
                (cgen-init #"SCM_INIT_EXTENSION(~safe-extname);")))])
  (dolist [init subinits]
    (cgen-decl #"extern void Scm_Init_~init(void);"))
  )

(define (finalize subinits)
  (dolist [init subinits]
    (cgen-init #"  Scm_Init_~init();")))

;;================================================================
;; Compiler stuff
;;

;; NOTE:
;;   The code is compiled in the version of the compiler currently
;;   running precomp (host compiler).  It may differ from the version
;;   of the compiler we're compiling (target compiler), and it becomes
;;   a problem if the two versions of compilers are using different
;;   mappings between mnemonics and codes.
;;
;;   When precomp generates the C literals for the compiled code, it
;;   uses the following mapping scheme.
;;
;;    1. use vm-code->list to extract mnemonics from the code
;;       compiled by the host compiler.
;;    2. use vm-find-insn-info (in gauche.vm.insn module) to map
;;       mnemonics to the target compiler's code.
;;
;;   For this scheme to work, the following conditions should be satisfied.
;;
;;    a. gauche.vm.insn should be the one generated from the same
;;       vminsn.scm of the target compiler.
;;    b. all the mnemonics that consists of the code generated by
;;       the host compiler must exists in the target compiler's ISA.
;;
;;   The condition b. implies that if you want to rename an instruction,
;;   you have to take three steps:
;;    (1) add a new instruction of the desired name, compile the
;;        target compiler #1.  (This version of the compiled target
;;        compiler still uses old instruction).
;;    (2) compile the target compiler again, using the target compiler #1,
;;        to generate the target compiler #2.  (This version of
;;        the target compiler uses the new instruction).
;;    (3) remove the old instruction.
;;

;; predicates to match the global identifiers that are not redefined
(define-syntax define-global-pred
  (syntax-rules ()
    [(_ name sym)
     (define name
       (global-eq?? 'sym 'gauche (^[](~(current-tmodule)'module))))]))
(define-global-pred =define-module?   define-module)
(define-global-pred =select-module?   select-module)
(define-global-pred =use?             use)
(define-global-pred =export?          export)
(define-global-pred =export-all?      export-all)
(define-global-pred =export-if-defined? export-if-defined)
(define-global-pred =extend?          extend)
(define-global-pred =provide?         provide)
(define-global-pred =lambda?          lambda)
(define-global-pred =include?         include)
(define-global-pred =begin?           begin)

;; A parameter that holds the list of 'omitted' #<compiled-code> - for
;; example, the cliche of (CLOSURE #<compiled-code> DEFINE #<identifier> RET)
;; will be generated as Scm_Define() in the initialization, not as a code
;; vector.  We need to suppress emitting code vector for those, since they
;; can be reachable via parent link in the inner closure.
(define omitted-code (make-parameter '()))

;; compile FORM, and conses the toplevel code (something to be
;; executed at toplevel).
(define (compile-toplevel-form form seed)
  (guard (e
          [(<error> e)
           (format (current-error-port) "Error in compiling ~s\n" form)
           (raise e)])
    (match form
      ;; Module related stuff
      [((? =define-module?) mod . body)
       (receive (seed extforms)
           (parameterize ((ext-module-forms '()))
             (let1 seed (with-tmodule mod
                          (fold compile-toplevel-form seed body))
               (values seed (ext-module-forms))))
         (write-ext-module `(define-module ,mod ,@(reverse extforms)))
         seed)]
      [((? =select-module?) mod)
       (write-ext-module form)
       (match (dso-name)
         [(name . #f)
          (write-ext-module `(dynamic-load ,name))]
         [(name . initfn)
          (write-ext-module `(dynamic-load ,name :init-function ,initfn))]
         [_ #f])
       (select-tmodule mod)
       seed]
      [((? =use?) . _)
       (write-ext-module form)
       (eval-in-current-tmodule form)
       seed]
      [((? =export?) . syms)
       (when (list? (compile-module-exports))
         (compile-module-exports
          (lset-union eq? syms (compile-module-exports))))
       (eval-in-current-tmodule `(export ,@syms))
       ;; If generate .sci file, 'export' form will be in it.
       ;; Otherwise we need to do export during initialization.
       (if (ext-module-file)
         (write-ext-module form)
         (let1 exp-specs (cgen-literal syms)
           (cgen-init
            (format "  (void)Scm_ExportSymbols(Scm_CurrentModule(), ~a);"
                    (cgen-cexpr exp-specs)))))
       seed]
      [((? =export-all?)) (write-ext-module form) (compile-module-exports #t)]
      [((? =export-if-defined?) . _) (write-ext-module form) seed]
      [((? =provide?) arg) (write-ext-module form) seed]
      [((? =extend?) . _) 
       (write-ext-module form)
       (eval-in-current-tmodule form)
       seed]
      [((? =begin?) . forms) (fold compile-toplevel-form seed forms)]
      [((? =include?) . filenames) (compile-includes filenames #f seed)]
      ;; Finally, ordinary expressions.
      [else
       (let* ([compiled-code (compile-in-current-tmodule form)]
              [toplevel-code (and (eq? (~ compiled-code 'name) '%toplevel)
                                  (vm-code->list compiled-code))])
         ;; We exclude a compiled code with only CONSTU-RET, which appears
         ;; as the result of macro expansion sometimes.
         (cond [(toplevel-constu-ret-code? toplevel-code) seed]
               [(toplevel-definition-code? toplevel-code)
                => (match-lambda
                     ([inner-code id flags]
                      (push! (omitted-code) compiled-code)
                      (emit-toplevel-definition inner-code id flags)
                      seed))]
               [else (cons (cgen-literal compiled-code) seed)]))]
      )))

;; check to see the compiled code only contains CONSTU-RET insn.
(define (toplevel-constu-ret-code? toplevel-code)
  (and (null? (cdr toplevel-code))
       (eq? (caar toplevel-code) 'CONSTU-RET)))

;; check to see if the compiled code has the cliche of toplevel
;; definition (CLOSURE #<code> DEFINE #<id> RET).  If we find it, returns
;; the internal closure code, identifier, and define flags.
(define (toplevel-definition-code? toplevel-code)
  (and (eq? (car (~ toplevel-code 0)) 'CLOSURE)
       (eq? (car (~ toplevel-code 2)) 'DEFINE)
       (eq? (car (~ toplevel-code 4)) 'RET)
       (list (~ toplevel-code 1)           ; #<compiled-code> of CLOSURE
             (~ toplevel-code 3)           ; identifier
             (cadr (~ toplevel-code 2))))) ; define flags

(define (emit-toplevel-definition inner-code id flags)
  (let ([sym  (cgen-literal (unwrap-syntax id))]
        ;; NB: Currently, the main tmodule has no name.  It's better to give
        ;; it a proper name.  Then the following Scm_CurrentModule hack
        ;; will be unnecessary.
        [mod  (and-let* ([n (module-name (~ id'module))])
                (find-tmodule n))]
        [code (cgen-literal inner-code)]
        [tmp (gensym)])
    (cgen-init (format "  ScmObj ~a = Scm_MakeClosure(~a, NULL);\n"
                       tmp (cgen-cexpr code)))
    (cgen-init (format "  Scm_MakeBinding(SCM_MODULE(~a) /* ~a */, \
                                          SCM_SYMBOL(~a) /* ~a */, \
                                          ~a,\
                                          ~a);\n"
                       (if mod (tmodule-cname mod) "Scm_CurrentModule()")
                       (if mod (cgen-safe-comment (~ mod'name)) "")
                       (cgen-cexpr sym)
                       (cgen-safe-comment (unwrap-syntax id))
                       tmp
                       (case flags
                         [(2) 'SCM_BINDING_CONST]
                         [(4) 'SCM_BINDING_INLINABLE]
                         [else 0])))))

;; given list of toplevel compiled codes, generate code in init
;; that calls them.  This is assumed to be the last procedure before
;; calling cgen-emit.
(define (emit-toplevel-executor topcodes)
  (cgen-body "static ScmCompiledCode *toplevels[] = {")
  (dolist [t topcodes]
    (cgen-body (format "  SCM_COMPILED_CODE(~a)," (cgen-cexpr t))))
  (cgen-body " NULL /*termination*/" "};")

  (cgen-init (format "  Scm_VMExecuteToplevels(toplevels);"))
  )

;; Handle include.  Ideally this whole routine should be shared with
;; src/compile-1.scm.
(define (compile-includes filenames case-fold? seed)
  (define (do-include filename seed)
    (unless (string? filename)
      (error "include requires literal string, but got:" filename))
    (with-input-from-port
        ($ (with-module gauche.internal pass1/open-include-file)
           filename 
           (sys-dirname (port-name (current-input-port))))
      (^[]
        (port-case-fold-set! (current-input-port) case-fold?)
        (unwind-protect
            (generator-fold compile-toplevel-form seed read)
          (close-port (current-input-port))))))
  (fold do-include seed filenames))

;;================================================================
;; Special form handlers
;;

;; Some special forms must be handled differently from the ordinary
;; compilation.  We implement it by replacing those special forms
;; for tailored handlers within the compiler environment.
;;
;; NB: We used to recognize those forms literally within
;; compile-toplevel-form.  It failed to work, however, when these
;; forms are generated as the result of macro expansion.
;; The current approach still has an issue when the compiled source
;; overrides these special forms.  Such sources should be very unusual,
;; so we don't support them for the time being.
;;
;; TODO: Reconstructing define-cproc etc. would lose the source information
;; attached to the original form.  We can't fix it right now, for define-macro
;; doesn't provide access to the original form.  Maybe after we replace
;; it with er-macro-expander...

(define *special-handlers*
  '((define-macro (current-module)
      `(find-module ',(with-module gauche.cgen.precomp
                        (~(current-tmodule)'name))))
    (define-macro (inline-stub . forms)
      (dolist [s forms]
        ((with-module gauche.cgen.stub cgen-stub-parse-form)
         (unwrap-syntax s)))
      (undefined))
    (define-macro (define-cproc . args)
      ((with-module gauche.cgen.stub cgen-stub-parse-form)
       (unwrap-syntax (cons 'define-cproc args)))
      (undefined))
    (define-macro (define-cfn . args)
      ((with-module gauche.cgen.stub cgen-stub-parse-form)
       (unwrap-syntax (cons 'define-cfn args)))
      (undefined))
    (define-macro (define-enum . args)
      ((with-module gauche.cgen.stub cgen-stub-parse-form)
       (unwrap-syntax (cons 'define-enum args)))
      (undefined))
    (define-macro (define-enum-conditionally . args)
      ((with-module gauche.cgen.stub cgen-stub-parse-form)
       (unwrap-syntax (cons 'define-enum-conditionally args)))
      (undefined))
    (define-macro (define-constant . f)
      ((with-module gauche.cgen.precomp handle-define-constant) f))
    (define-macro (define-syntax . f)
      ((with-module gauche.cgen.precomp handle-define-syntax) f))
    (define-macro (define-macro . f)
      ((with-module gauche.cgen.precomp handle-define-macro) f))
    (define-macro (define-inline/syntax . f)
      ((with-module gauche.cgen.precomp handle-define-inline/syntax) f))
    ;; TODO - we need more general framework supporting various declarations.
    ;; for the time being, this ad-hoc solution suffice our needs.
    (define-macro (declare . f)
      ((with-module gauche.cgen.precomp handle-declare) f))
    ;; A special directive not to precompile; the given forms are
    ;; not precompiled, but the code that evaluates the forms at
    ;; the loading time is generated.  It is useful if you want to delay
    ;; macro-expansion until the load time (e.g. cond-expand).  The
    ;; forms are not evaluated at all in the compiling environment,
    ;; so they are not avaialble for macro expansion of the forms
    ;; to be precompiled.  (The case can be handled more generally by
    ;; 'eval-when' mechanism, but properly support eval-when needs more
    ;; work.)
    ;; NB: When we quote the forms, identifier information in them will be
    ;; lost, since it becomes a quoted literal.
    (define-macro (without-precompiling . forms)
      ((with-module gauche.cgen.precomp handle-without-compiling) forms))
    ))

;; Macros are "consumed" by the Gauche's compiler---that is, it is
;; executed inside the compiler and won't appear in the compiled output.
;; For exported macros, we should include the macro itself in the compiled
;; file, so we intercept define-macro and define-syntax.

(define (handle-define-macro form)
  (define %define (make-identifier 'define (find-module 'gauche) '()))
  (define %define-syntax (make-identifier 'define-syntax (find-module 'gauche) '()))
  (define %lambda (make-identifier 'lambda (find-module 'gauche) '()))
  (define %begin (make-identifier 'begin (find-module 'gauche) '()))
  (define %macro (make-identifier '%make-macro-transformer
                                  (find-module 'gauche.internal) '()))
  (define %apply (make-identifier 'apply (find-module 'gauche) '()))
  (define %cdr (make-identifier 'cdr (find-module 'gauche) '()))
  (define (do-handle name expr)
    (if (or (symbol-exported? name)
            (memq name (private-macros-to-keep)))
      (let ([form (gensym)]
            [env (gensym)])
        `(,%begin
          (,%define ,name (,%macro ',name
                                   (,%lambda (,form ,env)
                                             (,%apply ,expr (,%cdr ,form)))
                                   ',expr))
          ((with-module gauche define-macro) ,name ,expr)))
      `((with-module gauche define-macro) ,name ,expr)))

  (match form
    [((name . formals) . body)
     (handle-define-macro `(,name (,%lambda ,formals ,@body)))]
    [(name expr) (do-handle name expr)]
    [_ (error "Malformed define-macro" form)]))

;; At this moment, we can only precompile macros that have closure
;; in its transformer.
(define (handle-define-syntax form)
  (match form
    [(name xformer-spec)
     ;; This inserts sets-up compile-time environment.
     (eval-in-current-tmodule
      `((with-module gauche define-syntax) ,@form))
     ;; If the macro needs to exported, check if we can put it in
     ;; precompiled file (it is, if the transformer is a closure).
     ;; Otherwise, we emit the form to *.sci file.
     (when (or (symbol-exported? name)
               (memq name (private-macros-to-keep)))
       (let1 val (global-variable-ref (~ (current-tmodule)'module) name #f)
         (or (and-let* ([ ((with-module gauche.internal macro?) val) ]
                        [tx ((with-module gauche.internal macro-transformer) val)]
                        [ (closure? tx) ])
               `((with-module gauche.internal %insert-syntax-binding)
                 (current-module) ',name
                 (let ((,name ,xformer-spec)) ,name))) ; attach name to closure
             (begin (write-ext-module `(define-syntax . ,form))
                    #f))))]
    [_ (error "Malformed define-syntax" form)]))

(define (handle-define-inline/syntax form)
  (match form
    [(name expr xformer-spec)
     ;; KLUDGE: We treat NAME as a macro during compiling this unit, since
     ;; we don't have the actual value of EXPR at compile-time.
     (eval-in-current-tmodule
      `((with-module gauche define-syntax) ,name ,xformer-spec))
     ;; Now generate code.  NB: Once we make macro serializable, we won't
     ;; need all these manual wiring.
     (let1 val (global-variable-ref (~ (current-tmodule)'module) name #f)
       (or (and-let* ([ ((with-module gauche.internal macro?) val) ]
                      [tx ((with-module gauche.internal macro-transformer) val)]
                      [ (closure? tx) ])
             ;; TODO: Hygiene
             `((with-module gauche define-inline) ,name
               ((with-module gauche.internal %with-inline-transformer)
                ,expr ,xformer-spec)))
           (error "Currently, you can't use define-inline/syntax with \
                   other than er-macro-transformer in precompiled file"
                  form)))]
    [_ (error "Malformed define-inline/syntax" form)]))

(define (handle-define-constant form)
  (match form
    [((? symbol? name) expr)
     (eval-in-current-tmodule
      `((with-module gauche define-constant) ,@form))]
    [_ #f])
  (cons '(with-module gauche define-constant) form))

(define (handle-without-compiling forms)
  ;; We eval the toplevel form at the initialization time.
  `(begin ,@(map (^f `(eval ',f (current-module))) forms)))

;; Handle declaration.
;; At this moment, we only recognize (keep-private-macro name ...) form
;; Caveat:
;;  - The decalation must be seen before define-macro
;;  - The name is treated unhygienic and global, no matter where it appears.
;; It should really be a metadata of macro binding and must be folded
;; into define-macro form.  The keep-private-macro trick should strictly
;; be considered temporary hack.
(define (handle-declare decls)
  (dolist [x decls]
    (match x
      [('keep-private-macro . macros)
       (private-macros-to-keep (append (private-macros-to-keep) macros))]
      [other (error "Unknown declaration:" other)]))
  (undefined))

;; check to see if the symbol is exported
(define (symbol-exported? sym)
  (or (eq? (compile-module-exports) #t)
      (memq sym (compile-module-exports))))

;;================================================================
;; Compiler-specific literal handling definitions
;;

;;----------------------------------------------------------------
;; <compiled-code>
;;

(define-cgen-literal <cgen-scheme-code> <compiled-code>
  ([code-name          :init-keyword :code-name]
   [code-vector-c-name :init-keyword :code-vector-c-name]
   [literals           :init-keyword :literals]
   [signature-info     :init-keyword :signature-info]
   )
  (make (value)
    (let* ([code (if (run-extra-optimization-passes)
                   (optimize-compiled-code value)
                   value)]
           [cv  (vm-code->list code)]
           [lv  (extract-literals cv)]
           [cvn (allocate-code-vector cv lv (~ code'full-name))]
           [code-name (cgen-literal (~ code'name))]
           [signature-info (cgen-literal (serializable-signature-info code))]
           [inliner (check-packed-inliner code)])
      (define (init-thunk)
        (format #t "    SCM_COMPILED_CODE_CONST_INITIALIZER(  /* ~a */\n"
                (cgen-safe-comment (~ code'name)))
        (format #t "            (ScmWord*)(~a), ~a,\n"
                cvn (length cv))
        (format #t "            ~a, ~a, ~a, ~a, SCM_NIL, ~a,\n"
                (~ code'max-stack)
                (~ code'required-args)
                (~ code'optional-args)
                (if (cgen-literal-static? code-name)
                  (cgen-cexpr code-name)
                  "SCM_FALSE")
                (if (cgen-literal-static? signature-info)
                  (cgen-cexpr signature-info)
                  "SCM_FALSE"))
        (format #t "            ~a, ~a)"
                (let1 parent-code (~ code'parent)
                  (if (memq parent-code (omitted-code))
                    "SCM_FALSE"
                    (cgen-cexpr (cgen-literal (~ code'parent)))))
                (if inliner
                  (cgen-cexpr inliner)
                  "SCM_FALSE")))
      (make <cgen-scheme-code> :value code
            :c-name (cgen-allocate-static-datum 'runtime 'ScmCompiledCode
                                                init-thunk)
            :code-vector-c-name cvn
            :code-name code-name
            :signature-info signature-info
            :literals lv)))
  (init (self)
    (unless (cgen-literal-static? (~ self'code-name))
      (print "  SCM_COMPILED_CODE("(~ self'c-name)")->name = "
             (cgen-cexpr (~ self'code-name))";"
             "/* "(cgen-safe-comment (~ self'value'full-name))" */"))
    (unless (cgen-literal-static? (~ self'signature-info))
      (print "  SCM_COMPILED_CODE("(~ self'c-name)")->signatureInfo = "
             (cgen-cexpr (~ self'signature-info)) ";"))
    (fill-code self))
  (static (self) #t)
  )

;; Construct serializable signature info.  cgen-literal doesn't emit
;; ExtendedPair as it is, and we need to use <serialiable-extended-pair>.
;; See literal.scm.
(define (serializable-signature-info code)
  (and-let* ([sig (~ code'signature-info)])
    (if (and (pair? sig)
             (pair? (car sig))
             (not (null? (pair-attributes (car sig)))))
      (cons ($ make-serializable-extended-pair
               (unwrap-syntax (caar sig)) (unwrap-syntax (cdar sig))
               (cond-list [(pair-attribute-get (car sig) 'source-info #f)
                           => (^x `(source-info . ,x))]
                          [(pair-attribute-get (car sig) 'unused-args #f)
                           => (^x `(unused-args . ,x))]))
            (unwrap-syntax (cdr sig)))
      (unwrap-syntax sig))))

;; Returns a list of the same length of CODE, which includes the
;; <cgen-literal>s corresponding to the literal values in CODE.
;; #f is filled in the places that don't have corresponding litaral value.
(define (extract-literals code)
  (let loop ([code code]
             [lits '()])
    (if (null? code)
      (reverse lits)
      (let* ([insn (car code)]
             [info (vm-find-insn-info (car insn))])
        (case (~ info'operand-type)
          [(none) (loop (cdr code)  (cons #f lits))]
          [(addr) (loop (cddr code) (list* #f #f lits))]
          [(code codes) (loop (cddr code)
                              (list* (cgen-literal (cadr code)) #f lits))]
          [(obj) (loop (cddr code)
                        (list* (cgen-literal (cadr code)) #f lits))]
          [(obj+addr)
           (loop (cdddr code)
                 (list* #f (cgen-literal (cadr code)) #f lits))]
          )))))

(define (allocate-code-vector cv lv full-name)

  (define (alloc-word initval)
    (cgen-allocate-static-datum 'runtime 'ScmWord initval))

  (define (loop cv lv count first-cexpr)
    (if (null? cv)
      first-cexpr
      (let* ([insn (car cv)]
             [info (vm-find-insn-info (car insn))]
             [insnval (vm-build-insn insn)]
             [name-info (if first-cexpr
                          ""
                          (format "/* ~a */\n    " (cgen-safe-comment full-name)))]
             [insn-cexpr
              (alloc-word
               ;; We emit it as signed integer so that 64bit machine
               ;; correctly handles negative parameter value.
               (if (> insnval #x80000000)
                 (format "~a-0x~8,'0x   /* ~3d ~a */"
                         name-info (- #x100000000 insnval) count
                         (cgen-safe-comment insn))
                 (format "~a0x~8,'0x    /* ~3d ~a */"
                         name-info insnval count
                         (cgen-safe-comment insn))))]
             [first-cexpr (or first-cexpr insn-cexpr)])
        (case (~ info'operand-type)
          [(none)
           (loop (cdr cv) (cdr lv) (+ count 1) first-cexpr)]
          [(addr)
           (alloc-word
            (format "SCM_WORD((ScmWord*)~a + ~d)"
                    first-cexpr (cadr cv)))
           (loop (cddr cv) (cddr lv) (+ count 2) first-cexpr)]
          [(obj code codes)
           (alloc-word
            (if (cgen-literal-static? (cadr lv))
              (format "SCM_WORD(~a) /* ~a */"
                      (cgen-cexpr (cadr lv))
                      (cgen-safe-comment (write-to-string (cadr cv))))
              (format "SCM_WORD(SCM_UNDEFINED) /* ~a */"
                      (cgen-safe-comment (write-to-string (cadr cv))))))
           (loop (cddr cv) (cddr lv) (+ count 2) first-cexpr)]
          [(obj+addr)
           (alloc-word
            (if (cgen-literal-static? (cadr lv))
              (format "SCM_WORD(~a) /* ~a */"
                      (cgen-cexpr (cadr lv))
                      (cgen-safe-comment (write-to-string (cadr cv))))
              (format "SCM_WORD(SCM_UNDEFINED) /* ~a */"
                      (cgen-safe-comment (write-to-string (cadr cv))))))
           (alloc-word
            (format "SCM_WORD((ScmWord*)~a + ~d)  /*    ~3d */"
                    first-cexpr (caddr cv) (caddr cv)))
           (loop (cdddr cv) (cdddr lv) (+ count 3) first-cexpr)]
          ))))

  (loop cv lv 0 #f))

(define (fill-code code-literal)
  (let ([cvn  (~ code-literal'code-vector-c-name)]
        [lv   (~ code-literal'literals)]
        [si   (~ code-literal'signature-info)])
    (for-each-with-index
     (^[index lit] (when (and lit (not (cgen-literal-static? lit)))
                     (format #t "  ((ScmWord*)~a)[~a] = SCM_WORD(~a);\n"
                             cvn index (cgen-cexpr lit))))
     lv)
    (unless (cgen-literal-static? si)
      (format #t "  SCM_COMPILED_CODE(~a)->signatureInfo = SCM_OBJ(~a);\n"
              (cgen-cexpr code-literal) (cgen-cexpr si)))))

;; If the compiled-code has packed IForm for inliner, translate it for
;; the target VM insns and returns the packed IForm.
(define (check-packed-inliner compiled-code)
  (let1 il (~ compiled-code'intermediate-form)
    (and (vector? il)
         (let* ([insns (class-slot-ref <vm-insn-info> 'all-insns)]
                [packed ((with-module gauche.internal translate-packed-iform)
                         il insns)])
           (cgen-literal packed)))))

;;----------------------------------------------------------------
;; <module>
;;

;; We recover modules at runtime.  Anonymous modules can be recovered
;; if they are used as a proxy of a real module (via <tmodule>).
(define-cgen-literal <cgen-module> <module>
  ((name  :init-keyword :name)
   (name-literal :init-keyword :name-literal))
  (make (value)
    (if-let1 n (predefined-module value)
      (make <cgen-module> :value value
            :c-name n
            :name (~ value'name))
      (let1 name (or (~ value'name)
                     (any (^[tm] (and (eq? (~ tm'module) value) (~ tm'name)))
                          (all-tmodules))
                     (error "Cannot emit literal for anonymous module:" value))
        (make <cgen-module> :value value
              :c-name (cgen-allocate-static-datum)
              :name name :name-literal (cgen-literal name)))))
  (init (self)
    (unless (predefined-module (~ self'value))
      (format #t "  ~a = SCM_OBJ(Scm_FindModule(SCM_SYMBOL(~a), \
                                     SCM_FIND_MODULE_CREATE)); \
                  /* module ~a */\n"
              (~ self'c-name)
              (cgen-cexpr (~ self'name-literal))
              (~ self'name))))
  (static (self) (boolean (predefined-module (~ self'value))))
  )

(define (predefined-module module)
  (assq-ref `((,(find-module 'null)   . "Scm_NullModule()")
              (,(find-module 'scheme) . "Scm_SchemeModule()")
              (,(find-module 'gauche) . "Scm_GaucheModule()")
              (,(find-module 'gauche.internal) . "Scm_GaucheInternalModule()")
              )
            module))

;;----------------------------------------------------------------
;; <identifier>
;;

(define-cgen-literal <cgen-scheme-identifier> <identifier>
  ((id-name        :init-keyword :id-name)
   (module-literal :init-keyword :module-literal))
  (make (value)
    (unless ((with-module gauche.internal identifier-toplevel?) value)
      (error "identifier with compiler environment can't be compiled" value))
    (make <cgen-scheme-identifier> :value value
          :c-name (cgen-allocate-static-datum)
          :id-name (cgen-literal (unwrap-syntax value))
          :module-literal (cgen-literal (~ value'module))))
  (init (self)
    (let ([name (cgen-cexpr (~ self'id-name))]
          [cname (~ self'c-name)])
      (format #t "  ~a = Scm_MakeIdentifier(~a, SCM_MODULE(~a), \
                                            SCM_NIL); /* ~a#~a */\n"
              cname name (cgen-cexpr (~ self'module-literal))
              (cgen-safe-comment (~ self'module-literal'name))
              (cgen-safe-comment (~ self'id-name'value)))))
  (static (self) #f)
  )

;;----------------------------------------------------------------
;; <macro>
;;

;; Macro transformer is usually not a toplevel closure, for it is
;; likely to close over macro definition environment for hygiene.
;; We need to figure out better way to serialize macros.

;; (define-cgen-literal <cgen-scheme-macro> <macro>
;;   ([transformer :init-keyword :transformer] ; <cgen-closure>
;;    [name :init-keyword :name])              ; maybe <cgen-symbol>
;;   (make (value)
;;     (let ([xf   ((with-module gauche.internal macro-transformer) value)]
;;           [name ((with-module gauche.internal macro-name) value)])
;;       (unless (toplevel-closure? xf)
;;         (errorf "A macro ~s cannot be a compile-time constant since its \
;;                  transformer is not a top-level closure: ~s" value xf))
;;       (make <cgen-scheme-macro>
;;         :value value :c-name (cgen-allocate-static-datum)
;;         :transformer (cgen-literal xf)
;;         :name (and name (cgen-literal name)))))
;;   (init (self)
;;     (format #t "  ~a = Scm_MakeMacroTransformer(~a, ~a); /* ~a */\n"
;;             (cgen-cexpr self)
;;             (if-let1 n (~ self'name) (cgen-cexpr n) "NULL")
;;             (cgen-cexpr (~ self'transformer))
;;             (cgen-safe-comment (write-to-string (~ self'value)))))
;;   (static (self) #f))

;;----------------------------------------------------------------
;; <generic>
;;

;; For generic functions, we initialize it at runtime.
(define-cgen-literal <cgen-scheme-generic> <generic>
  ((gf-name :init-keyword :gf-name))
  (make (value)
    (make <cgen-scheme-generic>
      :value value
      :c-name  (cgen-allocate-static-datum)
      :gf-name (cgen-literal (ref value 'name))))
  (init (self)
    (format #t "  ~a = Scm_GlobalVariableRef(SCM_MODULE(~a), SCM_SYMBOL(~a), FALSE);\n"
            (~ self'c-name)
            (current-tmodule-cname)
            (~ self'gf-name'c-name)))
  (static (self) #f)
  )

;;----------------------------------------------------------------
;; <procedure>
;;

;; We allow literal closures if it doens't close environment.
;; Closures do not have its own class, so we define cgen-literal class
;; for <procedure>.
(define-cgen-literal <cgen-closure> <procedure>
  ([code :init-keyword :code])  ; <cgen-scheme-code>
  (make (value)
    (unless (toplevel-closure? value)
      (error "a procedure (except top-level closure) cannot be \
              a compile-time constant:" value))
    (make <cgen-closure>
      :value value :c-name (cgen-allocate-static-datum)
      :code (cgen-literal (closure-code value))))
  (init (self)
    (format #t "  ~a = Scm_MakeClosure(~a, NULL); /* ~a */\n"
            (cgen-cexpr self) (cgen-cexpr (~ self'code))
            (cgen-safe-comment (write-to-string (~ self'value)))))
  (static (self) #f))
