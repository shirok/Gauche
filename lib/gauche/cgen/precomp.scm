;;;
;;; gauche.cgen.precomp - Precompile Scheme into C data
;;;  
;;;   Copyright (c) 2004-2008  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: cise.scm,v 1.9 2008-05-10 13:35:57 shirok Exp $
;;;

;;; Precompiler takes compiled VM instruction array and dumps it
;;; as C code.

(define-module gauche.cgen.precomp
  (use srfi-1)
  (use srfi-13)
  (use gauche.cgen)
  (use gauche.cgen.stub)
  (use gauche.vm.insn)
  (use gauche.parameter)
  (use gauche.sequence)
  (use gauche.experimental.app)
  (use gauche.experimental.ref)
  (use gauche.experimental.lamb)
  (use file.util)
  (use util.match)
  (use util.list)
  (use text.tr)
  (export cgen-precompile
          cgen-with-ext-module))
(select-module gauche.cgen.precomp)

;;================================================================
;; Main Entry point
;;
;;  The cgen-precompile function reads a scheme file SRC, compiles it
;;  and dumps the result as a C source.  VM instructions and Scheme
;;  constants are generated as C static data whenever possible.  One
;;  C function is created, which should be called to initialize the
;;  rest of the data which should be created at runtime.
;;
;;  The initialization function is named Scm_Init_xxx, where xxx is
;;  the basename sans extension of the source Scheme file, unless
;;  otherwise specified.   It takes no arguments, and must be called
;;  when the extension is initialized (typically it is called from
;;  extension's initialization routine, which is invoked when the
;;  extension is dynamically loaded.)
;;
;;  The initialization function itself can be an extention
;;  initialization routine, by giving ext-initializer keyword argument.
;;  It is convenient to make it so, when there's no auxiliary C files
;;  in the sources of the extension.
;;
;; SRC: a Scheme source file name.
;; OUT: a output C file name.  If #f, the name is calculate from SRC by
;;      swapping its suffix to .c and stripping directory part.
;;
;; Keyword arguments:
;;
;; EXT-INITIALIZER: If true, generate extra code in the initialization
;;      routine so that it works as an extention initializer.
;;      Default is #f.
;;
;; SUB-INITIALIZERS: Give a list of C initializer names that should be
;;      called from the initializer of this code.  Mainly intended to be
;;      used with EXT-INITIALIZER, when multiple Scheme sources are to
;;      be compiled into single extension file.
;;
;; PREDEF-SYMS: A list of strings, to insert #defines at the top of
;;      generated C source.
;;
;; MACROS-TO-KEEP: List of names of private macros that should be included
;;      in the output.  Usually private macros (macros bound to a variable
;;      which isn't exported) are not included in the output.  But sometimes
;;      hygienic public macros expands to a call of private macros, and
;;      gauche.cgen.precomp cannot detect such dependencies yet.
;; 
;;
;; If this procedure is called in the dynamic extent of cgen-with-ext-module,
;; several forms such as define-module, use, export, and dynamic-load are
;; written to the "external module file".
;;
(define (cgen-precompile src out
                         :key (ext-initializer #f)
                              (sub-initializers '())
                              (predef-syms '())
                              (macros-to-keep '()))
  (let1 base (or out (sys-basename (path-sans-extension src)))
    ;; see PARAMETERS section below
    (parameterize ([cgen-current-unit (get-unit src base predef-syms
                                                ext-initializer
                                                sub-initializers)]
                   [compile-module    (make-module #f)]
                   [compile-file-basename base]
                   [vm-eval-situation SCM_VM_COMPILING]
                   [private-macros-to-keep macros-to-keep])
      ;; Set up initial environment
      (eval '(define-macro (current-module)
               `(find-module ',(with-module gauche.cgen.precomp
                                 (compile-module-name))))
            (compile-module))
      (eval '(define-macro (inline-stub . forms)
               (for-each (lambda (s)
                           ((with-module gauche.cgen.stub cgen-stub-parse-form)
                            (unwrap-syntax s)))
                         forms)
               (undefined))
            (compile-module))
      ;; Static stuff
      (static-setup sub-initializers)
      ;; Main processing
      (with-input-from-file src
        (lambda ()
          (emit-toplevel-executor
           (reverse (port-fold compile-toplevel-form '() read)))))
      ;; Emitting
      (cgen-emit-c (cgen-current-unit))))
  )

;; An utility function to set up external module file.
;; EXT-MODULE-FILE-NAME is a filename or #f.

(define (cgen-with-ext-module ext-module-file-name thunk)
  (cond [ext-module-file-name
         (make-directory* (sys-dirname ext-module-file-name))
         (call-with-output-file ext-module-file-name
           (lambda (p)
             (display ";; generated automatically.  DO NOT EDIT\n" p)
             (display "#!no-fold-case\n" p)
             (parameterize ([ext-module-file p]) (thunk))))]
        [else
         (parameterize ([ext-module-file #f]) (thunk))]))

;;================================================================
;; Parameters
;;

;; we compile things within an anonymous module to avoid interference.
(define compile-module (make-parameter #f))

;; keep the (supposed) name of the current module.  (current-module) syntax
;; is compiled into (find-module ...) expression to lookup this name at
;; runtime.
(define compile-module-name (make-parameter #f))

;; keep the basename sans extension of the compiling file.
(define compile-file-basename (make-parameter #f))

;; keep the list of exported bindings (or #t if export-all)
(define compile-module-exports (make-parameter '()))

;; when we're compiling extension module (--ext-module=file), this parameter
;; keeps a port to the specified file.  the file becomes a module definition
;; file, containing define-module and dynamic-load forms, as well as the
;; exported macros.
;; NB: we insert (dynamic-load ...) just after select-module in the ext-module
;; file, assuming the source file has standard layout.
(define ext-module-file (make-parameter #f))

;; list of private macros that should be included in the output.
;; (--keep-private-macro=name,name,...)
;; usually private macros (macros bound to a variable which isn't exported)
;; are discarded, but sometimes hygienic public macros expands to a call
;; of private macros.  gencomp cannot detect such dependency yet, and
;; so they need to be explicitly listed for the time being.
(define private-macros-to-keep (make-parameter '()))

;;================================================================
;; Bridge to the internal stuff
;;
  
;; compatibility kludge
(define compile       (with-module gauche.internal compile))
(define compile-toplevel-lambda
  (with-module gauche.internal compile-toplevel-lambda))
(define %procedure-inliner
  (with-module gauche.internal %procedure-inliner))
(define vm-code->list (with-module gauche.internal vm-code->list))
(define vm-eval-situation
  (with-module gauche.internal vm-eval-situation))
(define global-eq?? (with-module gauche.internal global-eq??))

(define-constant SCM_VM_COMPILING 2) ;; must match with vm.h

;;================================================================
;; Utilities
;;

(define (get-unit src base predef-syms ext-init? sub-inits)
  (define safe-name (string-tr (sys-basename base) "-+" "__"))
  (make <cgen-stub-unit>
    :name base :c-name-prefix safe-name
    :preamble `(,(format "/* Generated automatically from ~a.  DO NOT EDIT */"
                         src))
    :pre-decl (map (lambda (s) #`"#define ,s") predef-syms)
    :init-prologue (format "~avoid Scm_Init_~a() { ScmModule *mod;"
                           (if ext-init? "SCM_EXTENSION_ENTRY " "")
                           safe-name)
    ))

(define (write-ext-module form)
  (cond [(ext-module-file) => (^_ (write form _) (newline _))]))

(define (static-setup subinits)
  (cgen-decl "#include <gauche/code.h>")
  (cgen-decl "#include <gauche/macro.h>") ; for MakeMacroTransformerOld. temporary.
  (cond [(ext-module-file)
         => (lambda (extm)
              (cgen-decl "#include <gauche/extend.h>")
              (let* ([extname (path-sans-extension (port-name extm))]
                     [safe-extname (regexp-replace-all #/\W/ extname "_")])
                (cgen-init #`"SCM_INIT_EXTENSION(,safe-extname);")))])
  (dolist [init subinits]
    (cgen-decl #`"extern void ,init(void);")))

;;================================================================
;; Compiler stuff
;;

;; NOTE:
;;   The code is compiled in the version of the compiler currently
;;   running gencomp (host compiler).  It may differ from the version
;;   of the compiler we're compiling (target compiler), and it becomes
;;   a problem if the two versions of compilers are using different
;;   mappings between mnemonics and codes.
;;
;;   When gencomp generates the C literals for the compiled code, it
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
     (define name (global-eq?? 'sym 'gauche compile-module))]))
(define-global-pred =define-module?   define-module)
(define-global-pred =select-module?   select-module)
(define-global-pred =use?             use)
(define-global-pred =export?          export)
(define-global-pred =export-all?      export-all)
(define-global-pred =provide?         provide)
(define-global-pred =define-macro?    define-macro)
(define-global-pred =define-syntax?   define-syntax)
(define-global-pred =define?          define)
(define-global-pred =lambda?          lambda)
(define-global-pred =define-constant? define-constant)
(define-global-pred =declare?         declare)

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
       (write-ext-module form)
       (parameterize ([compile-module-name mod])
         (fold compile-toplevel-form seed body))]
      [((? =select-module?) mod)
       (write-ext-module form)
       (write-ext-module
        `(dynamic-load ,(compile-file-basename)))
       (let1 sym (cgen-literal mod)
         (cgen-init
          (format "  mod = Scm_FindModule(SCM_SYMBOL(~a),\
                                          SCM_FIND_MODULE_CREATE);"
                  (cgen-cexpr sym))
          ;; force the current module to be mod
          "  Scm_SelectModule(mod);"))
       (compile-module-name mod)
       seed]
      [((? =use?) mod)
       (eval `(use ,mod) (compile-module)) seed]
      [((? =export?) . syms)
       (when (list? (compile-module-exports))
         (compile-module-exports
          (lset-union eq? syms (compile-module-exports))))
       (eval `(export ,@syms) (compile-module)) seed]
      [((? =export-all?)) (compile-module-exports #t)]
      [((? =provide?) arg) (write-ext-module form) seed]
      ;; For the time being, we only compile the legacy macros into C file.
      ;; R5RS macros are put in ext-module file as is.
      [((? =define-macro?) (name . formals) . body)
       (eval form (compile-module))
       (when (or (symbol-exported? name)
                 (memq name (private-macros-to-keep)))
         (let* ([body-closure (compile-toplevel-lambda form name formals
                                                       body (compile-module))]
                [code (cgen-literal (closure-code body-closure))]
                [var  (cgen-literal name)])
           (cgen-init
            (format "  Scm_Define(mod, SCM_SYMBOL(~a), \
                             Scm_MakeMacroTransformerOld(SCM_SYMBOL(~a),\
                                 SCM_PROCEDURE(Scm_MakeClosure(~a, NULL)))); /* ~s */"
                    (cgen-cexpr var) (cgen-cexpr var)
                    (cgen-cexpr code) name))))
       seed]
      [((? =define-macro?) name . _)
       (when (symbol-exported? name)
         (write-ext-module form))
       (eval form (compile-module)) seed]
      [((? =define-syntax?) name . _)
       (when (or (symbol-exported? name)
                 (memq name (private-macros-to-keep)))
         (write-ext-module form))
       (eval form (compile-module)) seed]
      ;; TODO - we need more general framework supporting various declarations.
      ;; for the time being, this ad-hoc solution suffice our needs.
      [((? =declare?) decls ...)
       (for-each (^.[('keep-private-macro . macros)
                     (private-macros-to-keep (append (private-macros-to-keep)
                                                     macros))]
                    [other (error "Unknown declaration:" other)])
                 decls)
       seed]
      ;; Finally, ordinary expressions.
      [((? =define?) (name . args) . body)
       (compile-toplevel-form `(define ,name (lambda ,args ,@body)) seed)]
      [((? =define?) (? symbol? name) ((? =lambda?) args . body))
       (let* ([closure
               (compile-toplevel-lambda form name args body (compile-module))]
              [code (cgen-literal (closure-code closure))]
              [var  (cgen-literal name)])
         (cgen-init
          (format "  Scm_Define(mod, SCM_SYMBOL(~a), Scm_MakeClosure(~a, NULL)); /* ~s */"
                  (cgen-cexpr var) (cgen-cexpr code) name)))
       seed]
      [((? =define-constant?) (? symbol?) expr)
       (eval form (compile-module))
       (cons (cgen-literal (compile form (compile-module))) seed)]
      [else
       (let1 compiled-code (compile form (compile-module))
         ;; We exclude a compiled code with only CONSTU-RET, which appears
         ;; as the result of macro expansion sometimes.
         (if (toplevel-constu-ret-code? compiled-code)
           seed
           (cons (cgen-literal compiled-code) seed)))]
      )))

;; check to see the compiled code only contains CONSTU-RET insn.
(define (toplevel-constu-ret-code? compiled-code)
  (and (eq? (~ compiled-code'name) '%toplevel)
       (= (~ compiled-code'size) 1)
       (let1 code (vm-code->list compiled-code)
         (null? (cdr code))
         (eq? (caar code) 'CONSTU-RET))))

;; given list of toplevel compiled codes, generate code in init
;; that calls them.  This is assumed to be the last procedure before
;; calling cgen-emit.
(define (emit-toplevel-executor topcodes)
  (cgen-body "static ScmCompiledCode *toplevels[] = {")
  (dolist (t topcodes)
    (cgen-body (format "  SCM_COMPILED_CODE(~a)," (cgen-cexpr t))))
  (cgen-body " NULL /*termination*/" "};")

  (cgen-init (format "  Scm_VMExecuteToplevels(toplevels);"))
  )

;; check to see if the symbol is exported
(define (symbol-exported? sym)
  (or (eq? (compile-module-exports) #t)
      (memq sym (compile-module-exports))))

;;================================================================
;; Compiler-specific literal handling definitions
;;       
(define-cgen-literal <cgen-scheme-code> <compiled-code>
  ((code-name   :init-keyword :code-name)
   (code-vector-c-name :init-keyword :code-vector-c-name)
   (literals    :init-keyword :literals)
   )
  (make (value)
    (let* ((cv  (vm-code->list value))
           (lv  (extract-literals cv))
           (cvn (allocate-code-vector cv lv (~ value'full-name)))
           (code-name (cgen-literal (~ value'name)))
           (arg-info (cgen-literal (~ value'arg-info)))
           (inliner (check-packed-inliner value))
           )
      (define (init-thunk)
        (format #t "    SCM_COMPILED_CODE_CONST_INITIALIZER(  /* ~a */\n"
                (cgen-safe-comment (~ value'name)))
        (format #t "            (ScmWord*)(~a), ~a,\n"
                cvn (length cv))
        (format #t "            ~a, ~a, ~a, ~a, SCM_NIL, ~a,\n"
                (~ value'max-stack)
                (~ value'required-args)
                (~ value'optional-args)
                (if (cgen-literal-static? code-name)
                  (cgen-cexpr code-name)
                  "SCM_FALSE")
                (cgen-cexpr arg-info))
        (format #t "            ~a, ~a)"
                (cgen-cexpr (cgen-literal (~ value'parent)))
                (if inliner
                  (cgen-cexpr inliner)
                  "SCM_FALSE")))
      (make <cgen-scheme-code> :value value
            :c-name (cgen-allocate-static-datum 'runtime 'ScmCompiledCode
                                                init-thunk)
            :code-vector-c-name cvn
            :code-name code-name
            :literals lv)))
  (init (self)
    (unless (cgen-literal-static? [~ self'code-name])
      (print "  SCM_COMPILED_CODE("[~ self'c-name]")->name = "
             (cgen-cexpr [~ self'code-name])";"
             "/* "(cgen-safe-comment [~ self'value'full-name])" */"))
    (fill-code self))
  (static (self) #t)
  )

;; Returns a list of the same length of CODE, which includes the
;; <cgen-literal>s corresponding to the literal values in CODE.
;; #f is filled in the places that don't have corresponding litaral value.
(define (extract-literals code)
  (let loop ((code code)
             (lits '()))
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

(define (fill-code code)
  (let ([cvn  (~ code'code-vector-c-name)]
        [lv   (~ code'literals)])
    (for-each-with-index
     (lambda (index lit)
       (when (and lit (not (cgen-literal-static? lit)))
         (format #t "  ((ScmWord*)~a)[~a] = SCM_WORD(~a);\n"
                 cvn index (cgen-cexpr lit))))
     lv)
    ))

;; If the compiled-code has packed IForm for inliner, translate it for
;; the target VM insns and returns the packed IForm.
(define (check-packed-inliner compiled-code)
  (let1 il (~ compiled-code'intermediate-form)
    (and (vector? il)
         (let* ([insns (class-slot-ref <vm-insn-info> 'all-insns)]
                [packed ((with-module gauche.internal translate-packed-iform)
                         il insns)])
           (cgen-literal packed)))))

;; NB: this doesn't yet handle identifiers that are inserted by hygienic
;; macro (so that they have different module than the current one).
(define-cgen-literal <cgen-scheme-identifier> <identifier>
  ((id-name   :init-keyword :id-name)
   (mod-name  :init-keyword :mod-name))
  (make (value)
    (let ((name (ref value 'name))
          (mod  (ref value 'module))
          (env  (ref value 'env)))
      (unless (null? env)
        (error "identifier with compiler environment can't be compiled" value))
      (make <cgen-scheme-identifier> :value value
            :c-name (cgen-allocate-static-datum)
            :id-name (cgen-literal name)
            :mod-name (and-let* ((modnam (module-name-fix mod)))
                        (cgen-literal modnam)))))
  (init (self)
    (let ((name (cgen-cexpr (ref self 'id-name)))
          (cname (ref self 'c-name)))
      (or (and-let* ((modnam (ref self 'mod-name)))
            (print "  "cname" = Scm_MakeIdentifier(SCM_SYMBOL("name"), "
                   "Scm_FindModule(SCM_SYMBOL("(cgen-cexpr modnam)"), SCM_FIND_MODULE_CREATE),"
                   "SCM_NIL);"))
          (print "  "cname" = Scm_MakeIdentifier(SCM_SYMBOL("name"), mod, SCM_NIL);"))))
  (static (self) #f)
  )

;; NB: for compatibility, we check modnam vs '# to find out anonymous
;; modules.  (By 0.8.14 anonymous modules are named as |#|.)
(define (module-name-fix module)
  (and-let* ([nam (module-name module)]
             [ (not (eq? nam '|#|)) ]) ;|# <- to fool emacs
    nam))

;; NB: for now, we ignore macros (we assume they are only used within
;; the source file).
(define-cgen-literal <cgen-scheme-macro> <macro>
  ()
  (make (value)
    (make <cgen-scheme-macro> :value value :c-name #f))
  )

;; For generic functions, we initialize it at runtime.
(define-cgen-literal <cgen-scheme-generic> <generic>
  ((gf-name :init-keyword :gf-name))
  (make (value)
    (make <cgen-scheme-generic>
      :value value
      :c-name  (cgen-allocate-static-datum)
      :gf-name (cgen-literal (ref value 'name))))
  (init (self)
    (format #t "  ~a = Scm_SymbolValue(mod, SCM_SYMBOL(~a));\n"
            (ref self 'c-name)
            (ref (ref self 'gf-name) 'c-name)))
  (static (self) #f)
  )

(provide "gauche.cgen.precomp")
