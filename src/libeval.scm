;;;
;;; libeval.scm - eval, load and related stuff
;;;
;;;   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

(select-module gauche.internal)

(inline-stub
 (declcode (.include <gauche/vminsn.h>
                     <gauche/class.h>
                     <gauche/priv/macroP.h>
                     <gauche/priv/readerP.h>)))

(declare (keep-private-macro autoload add-load-path
                             define-compiler-macro))

;;;
;;; Eval
;;;

(select-module scheme)

;; API
(define-cproc eval (expr env) Scm_VMEval)

;; API
;; for now, just return a module.
(define-cproc null-environment (version::<fixnum>)
  (when (!= version 5) (Scm_Error "unknown rNrs version: %d" version))
  (return (SCM_OBJ (Scm_NullModule))))
(define-cproc scheme-report-environment (version::<fixnum>)
  (when (!= version 5) (Scm_Error "unknown rNrs version: %d" version))
  (return (SCM_OBJ (Scm_SchemeModule))))
(define-cproc interaction-environment () (return (SCM_OBJ (Scm_UserModule))))

;;;
;;; Load
;;;

(select-module gauche.internal)

;; API: Main entry of `load'
(define-in-module scheme (load file :key (paths *load-path*)
                                         (suffixes *load-suffixes*)
                                         (error-if-not-found #t)
                                         (environment #f)
                                         (ignore-coding #f))
  ;; NB: 'File not found' error is handled in find-load-file.
  (and-let* ([r (find-load-file file paths suffixes error-if-not-found #t)])
    (let* ([path (car r)]
           [remaining-paths (cadr r)]
           [hooked? (pair? (cddr r))]
           [opener (if hooked? (caddr r) open-input-file)]
           [port (guard (e [else e]) (opener path))])
      (when (%load-verbose?)
        (format (current-error-port) ";;~aLoading ~a~a...\n"
                (make-string (* (length (current-load-history)) 2) #\space)
                path
                (if hooked? " (hooked) " "")))
      (if (not (input-port? port))
        (and error-if-not-found (raise port))
        (load-from-port (if ignore-coding
                          port
                          (open-coding-aware-port port))
                        :environment environment
                        :paths remaining-paths)))))


(select-module gauche.internal)

;; API: The actual load operation is done here.
(define-in-module gauche (load-from-port port
                                         :key (paths #f)
                                              (environment #f))
  (unless (input-port? port)
    (error "input port required, but got:" port))
  (unless (or (module? environment) (not environment))
    (error "module or #f required, but got:" environment))
  (let ([prev-module  (vm-current-module)]
        [prev-port    (current-load-port)]
        [prev-history (current-load-history)]
        [prev-next    (current-load-next)]
        [prev-reader-lexical-mode (reader-lexical-mode)]
        [prev-eval-situation (vm-eval-situation)]
        [prev-read-context (current-read-context)])

    (define (setup-load-context)
      (when (port-closed? port) (error "port alrady closed:" port))
      (%port-lock! port)
      (when environment (vm-set-current-module environment))
      (current-load-port port)
      (current-load-next paths)
      (current-load-history
       (cons (if (port? prev-port)
               (list prev-port (port-current-line prev-port))
               (list #f))
             prev-history))
      (vm-eval-situation SCM_VM_LOADING)
      (current-read-context (%new-read-context-for-load))
      (%record-load-stat (or (current-load-path) "(unnamed source)")))

    (define (restore-load-context)
      (vm-set-current-module prev-module)
      (current-load-port prev-port)
      (current-load-history prev-history)
      (current-load-next prev-next)
      (reader-lexical-mode prev-reader-lexical-mode)
      (vm-eval-situation prev-eval-situation)
      (current-read-context prev-read-context)
      (close-port port)
      (%record-load-stat #f)
      (%port-unlock! port))

    (guard (e [else (let1 e2 (if (condition? e)
                               ($ make-compound-condition e
                                  $ make <load-condition-mixin>
                                  :history (current-load-history)
                                  :port (current-load-port)
                                  :expr #f)
                               e)
                      (restore-load-context)
                      (raise e2))])
      (setup-load-context)
      (do ([s (read port) (read port)])
          [(eof-object? s)]
        (eval s #f)))
    (restore-load-context)
    #t))

;; A few helper procedures
(define-cproc %record-load-stat (path) ::<void>
  (.if "defined(HAVE_GETTIMEOFDAY)"
       (let* ([vm::ScmVM* (Scm_VM)])
         (when (SCM_VM_RUNTIME_FLAG_IS_SET vm SCM_COLLECT_LOAD_STATS)
           (let* ([t0::(struct timeval)])
             (gettimeofday (& t0) NULL)
             (let* ([t (Scm_MakeIntegerU (+ (* (ref t0 tv_sec) 1000000)
                                            (ref t0 tv_usec)))])
               (set! (ref (-> vm stat) loadStat)
                     (Scm_Cons
                      (?: (SCM_FALSEP path) t (Scm_Cons path t))
                      (ref (-> vm stat) loadStat)))))))))

(define-cproc %new-read-context-for-load ()
  (let* ([ctx::ScmReadContext* (Scm_MakeReadContext NULL)])
    (set! (-> ctx flags)
          (logior (-> ctx flags)
                  (logior RCTX_LITERAL_IMMUTABLE
                          RCTX_SOURCE_INFO)))
    (return (SCM_OBJ ctx))))

(define-cproc %load-verbose? () ::<boolean>
  (return (SCM_VM_RUNTIME_FLAG_IS_SET (Scm_VM) SCM_LOAD_VERBOSE)))


(select-module gauche)

;; API
(define-cproc dynamic-load (file::<string>
                            :key (init-function #f)
                            (export-symbols #f)); for backward compatibility
  (return (Scm_DynLoad file init_function 0)))

;; API
(define-cproc provide (feature)   Scm_Provide)
;; API
(define-cproc provided? (feature) ::<boolean> Scm_ProvidedP)

(select-module gauche.internal)
(define-cproc %loaded-dlobjs () Scm_DLObjs) ; for internal use; name may change

(select-module gauche.internal)
;; NB: 'require' is recognized by the compiler, which calls
;; this one directly.
(define-cproc %require (feature) ::<boolean>
  (return (not (Scm_Require feature SCM_LOAD_PROPAGATE_ERROR NULL))))

(define-cproc %add-load-path (path::<const-cstring> :optional afterp)
  (return (Scm_AddLoadPath path (not (SCM_FALSEP afterp)))))

(define-cproc %autoload (mod::<module> file-or-module entries)
  ::<void> Scm_DefineAutoload)

;; API
;; Get the file path currently loading from.
;; We may swap the implementation with more reliable way in future.
(define-in-module gauche (current-load-path)
  (and-let* ([p (current-load-port)]
             [info (port-name p)]
             [ (string? info) ]
             [ (not (#/^\(.*\)$/ info)) ])
    info))

;; API
(select-module gauche)
(define-macro (autoload file . vars)
  `((with-module gauche.internal %autoload) (current-module) ',file ',vars))

;; API
;; Load path needs to be dealt with at the compile time.  this is a
;; hack to do so.   Don't modify *load-path* directly, since it causes
;; weird compiler-evaluator problem.
;; I don't like the current name "add-load-path", though---looks like
;; more a procedure than a compiler syntax---any ideas?
(select-module gauche)
(define-macro (add-load-path path . args)
  (let ([afterp (or (memq #t args) (memq :after args))]
        ;; If :relative is given, we trust the programmer to give a relative
        ;; pathname to PATH.
        [path (or (and-let* ([ (memq :relative args) ]
                             [cur (current-load-path) ])
                    (string-append (sys-dirname cur) "/" path))
                  path)])
    `',((with-module gauche.internal %add-load-path) path afterp)))

;; Load path hooks
(select-module gauche.internal)
(define-cproc %add-load-path-hook! (proc :optional (after?::<boolean> #f))
  ::<void> Scm_AddLoadPathHook)
(define-cproc %delete-load-path-hook! (proc)
  ::<void> Scm_DeleteLoadPathHook)

;; API: find-load-file
;;
;;   Core function to search specified file from the search path *PATH.
;;   Search rules are:
;;
;;    (1) If given filename begins with "/", "./" or "../", the file is
;;        searched.
;;    (2) If given filename begins with "~", unix-style username
;;        expansion is done, then the resulting file is searched.
;;    (3) Otherwise, the file is searched for each directory in PATHs.
;;
;;   If the named file is found, a list of the actual filename, and the
;;   remaining paths is returned.  The remaining paths can be used again
;;   to find next matching filename.
;;   (The returned list may have the third element, if load-path-hooks is
;;   used.  See below).
;;
;;   If SUFFIXES is given, after the filename is tested, names with
;;   each element in SUFFIXES list appended are tried.
;;   The element in SUFFIXES is directly appended to the FILENAME;
;;   so usually it begins with dot.
;;
;;   PATHs may contain a regular file, or an empty string "".
;;   In which case, procedures chained to *load-path-hooks* are called
;;   in turn.  It receives three arguments; the regular filename in PATHs
;;   or "", the (partial) filename given to the find-load-file, and the
;;   list of suffixes.  The hook is mainly intended to allow loading
;;   from archive files or from special location (e.g. prelinked in the
;;   executing binary).   If the hook procedure "finds"
;;   the searched file in the archive file, it should return a pair of
;;   the canonical filename (given filename plus suffix if applicable), and
;;   a thunk that opens and returns a port to read the file.  If the hook
;;   procedure doesn't find the searched file, it should return #f.
;;
;;   NB: find-file-in-paths in file.util is similar to this, but this one
;;   captures the exact behavior of `load'.
(select-module gauche.internal)
(define (find-load-file filename paths suffixes
                        :optional (error-if-not-found #f)
                                  (allow-archive #f))
  (define (file-ok? file)
    (and (file-exists? file)
         (not (file-is-directory? file))))
  (define (try-suffixes stem)
    (cond [(file-ok? stem) stem]
          [else (any (^s (let1 file (string-append stem s)
                           (and (file-ok? file) file)))
                     suffixes)]))
  (define (do-absolute stem)
    (if-let1 found (try-suffixes stem)
      (list found '())
      (and error-if-not-found
           (errorf "cannot find ~s to load" stem))))
  (define (do-relative ps)
    (cond
     [(null? ps)
      (and error-if-not-found
           (errorf "cannot find ~s in ~s" filename paths))]
     [(file-is-directory? (car ps))
      (if-let1 found (try-suffixes (string-append (car ps) "/" filename))
        (list found (cdr ps))
        (do-relative (cdr ps)))]
     [(and allow-archive
           (or (equal? (car ps) "") (file-is-regular? (car ps))))
      (if-let1 r (any (^p (p (car ps) filename suffixes)) *load-path-hooks*)
        (list (car r) (cdr ps) (cdr r))
        (do-relative (cdr ps)))]
     [else (do-relative (cdr ps))]))

  (when (equal? filename "")
    (error "bad filename to load" filename))
  (cond [(char=? (string-ref filename 0) #\~)
         (do-absolute (sys-normalize-pathname filename :expand #t))]
        [(rxmatch #/^\.{0,2}\// filename) (do-absolute filename)]
        ;; we can't use cond-expand here, for this file is precompiled
        ;; on a system different from the final target.
        [(and (or (assq 'gauche.os.windows (cond-features))
                  (assq 'gauche.os.cygwin (cond-features)))
              (rxmatch #/^[a-zA-Z]:/ filename)) ; the wicked drive-letter
         (do-absolute filename)]
        [else (do-relative paths)]))

;;;
;;; Repl
;;;

(select-module gauche.internal)

(define (%repl-print . vals) (for-each (^e (write e) (newline)) vals))
(define (%repl-prompt) (display "gosh> ") (flush))

;; API
(define-in-module gauche (read-eval-print-loop :optional (reader #f)
                                                         (evaluator #f)
                                                         (printer #f)
                                                         (prompter #f))
  (let ([reader    (or reader read)]
        [evaluator (or evaluator eval)]
        [printer   (or printer %repl-print)]
        [prompter  (or prompter %repl-prompt)])
    (let loop1 ()
      (and
       (with-error-handler
           (^e (report-error e) #t)
         (^[]
           (let loop2 ()
             (prompter)
             (let1 exp (reader)
               (and (not (eof-object? exp))
                    (receive results (evaluator exp (vm-current-module))
                      (apply printer results)
                      (loop2)))))))
       (loop1)))))

;;;
;;; Macros
;;;

(select-module gauche.internal)
;; API
(define-in-module gauche (macroexpand form)
  (%internal-macro-expand form (make-cenv (vm-current-module)) #f))
;; API
(define-in-module gauche (macroexpand-1 form)
  (%internal-macro-expand form (make-cenv (vm-current-module)) #t))

(select-module gauche)
;; API
(define-cproc unwrap-syntax (form) Scm_UnwrapSyntax)

(select-module gauche.internal)
(inline-stub
 (define-type <macro> "ScmMacro*" "macro"
   "SCM_MACROP" "SCM_MACRO" "SCM_OBJ")
 )

;; These are used in the compiler, and hidden inside gauche.internal.
(define-cproc macro? (obj) ::<boolean> SCM_MACROP)
(define-cproc syntax? (obj) ::<boolean> SCM_SYNTAXP)

(define-cproc make-macro-transformer (name::<symbol> proc::<procedure>)
  Scm_MakeMacroTransformerOld)

(define-cproc %make-macro-transformer (name::<symbol>? proc)
  Scm_MakeMacro)

(define-cproc compile-syntax-rules (name ellipsis literals rules mod env)
  Scm_CompileSyntaxRules)

(define-cproc macro-transformer (mac::<macro>) Scm_MacroTransformer)

(define (call-macro-expander mac expr cenv)
  ((macro-transformer mac) expr cenv))

(define-cproc make-syntax (name::<symbol> proc)
  Scm_MakeSyntax)

(define-cproc make-syntactic-closure (env literals expr)
  Scm_MakeSyntacticClosure)

(define-cproc call-syntax-handler (syn program cenv)
  (SCM_ASSERT (SCM_SYNTAXP syn))
  (return (Scm_VMApply2 (-> (SCM_SYNTAX syn) handler) program cenv)))

(define-cproc syntax-handler (syn)
  (SCM_ASSERT (SCM_SYNTAXP syn))
  (return (-> (SCM_SYNTAX syn) handler)))

;;;
;;; System termination
;;;

(select-module gauche)
(define-cproc %exit (:optional (code 0)) ::<void>
  (let* ([status::int 0])
    (cond
     [(SCM_EQ code SCM_TRUE)]  ; status == 0
     [(SCM_INTP code) (set! status (SCM_INT_VALUE code))]
     [else (set! status 70)])  ; EX_SOFTWARE
    (Scm_Exit status)))

;; API
;; exit handler.  we don't want to import the fluff with gauche.parameter,
;; so we manually allocate parameter slot.
(select-module gauche.internal)
(define-in-module gauche exit-handler
  (let1 index (%vm-make-parameter-slot)
    ;; set default exit handler
    (%vm-parameter-set! index #f
                        (^[code fmt args]
                          (when fmt
                            (apply format (standard-error-port) fmt args)
                            (newline (standard-error-port)))))
    (^ maybe-arg
      (rlet1 old (%vm-parameter-ref index #f)
        (when (pair? maybe-arg)
          (%vm-parameter-set! index #f (car maybe-arg)))))))

;; API
(define-in-module gauche (exit :optional (code 0) (fmt #f) :rest args)
  (cond [(exit-handler)
         => (^h (guard (e [(<error> e) #f]) (h code fmt args)))])
    (%exit code))

;;;
;;; GC control
;;;

(select-module gauche)
;; API
(define-cproc gc () (call <void> GC_gcollect))

;; API
(define-cproc gc-stat ()
  (return
   (list
    (list ':total-heap-size
          (Scm_MakeIntegerFromUI (cast u_long (GC_get_heap_size))))
    (list ':free-bytes
          (Scm_MakeIntegerFromUI (cast u_long (GC_get_free_bytes))))
    (list ':bytes-since-gc
          (Scm_MakeIntegerFromUI (cast u_long (GC_get_bytes_since_gc))))
    (list ':total-bytes
          (Scm_MakeIntegerFromUI (cast u_long (GC_get_total_bytes)))))))

(select-module gauche.internal)
;; for diagnostics
(define-cproc gc-print-static-roots () ::<void> Scm_PrintStaticRoots)

;;;
;;; Some system introspection
;;;

(select-module gauche)
;; API
;; Obtain info about gauche itself
(define-cproc gauche-version () ::<const-cstring> (return GAUCHE_VERSION))
(define-cproc gauche-architecture () ::<const-cstring> Scm_HostArchitecture)
(define-cproc gauche-library-directory () Scm_LibraryDirectory)
(define-cproc gauche-architecture-directory () Scm_ArchitectureDirectory)
(define-cproc gauche-site-library-directory () Scm_SiteLibraryDirectory)
(define-cproc gauche-site-architecture-directory () Scm_SiteArchitectureDirectory)
(define-cproc gauche-dso-suffix () ::<const-cstring> (return SHLIB_SO_SUFFIX))

;; Temporary - only usable on Windows(mingw) and Darwin; used to replace
;; '@' in the paths embedded in gauche.config.
(define-cproc %gauche-runtime-directory () Scm__RuntimeDirectory)

;; API
;; Command line - R7RS adds 'command-line' procedure.  We provide it as
;; a predefined parameter.  Like exit-handler, we manually allocate a
;; parametre slot to avoid importing gauche.parameter.
(select-module gauche.internal)
(define-in-module gauche command-line
  (let1 index (%vm-make-parameter-slot)
    (^ maybe-arg
      (rlet1 old (%vm-parameter-ref index '())
        (when (pair? maybe-arg)
          (%vm-parameter-set! index #f (car maybe-arg)))))))
;;
;; External view of VM.
;;

(select-module gauche)
(inline-stub
 (define-cclass <thread> "ScmVM" "Scm_VMClass"
   ()
   ((name)
    (specific)
    )
   (printer
    (let* ([vm::ScmVM* (SCM_VM obj)]
           [state::(const char *)])
      (case (-> vm state)
        [(SCM_VM_NEW)        (set! state "new")]
        [(SCM_VM_RUNNABLE)   (set! state "runnable")]
        [(SCM_VM_STOPPED)    (set! state "stopped")]
        [(SCM_VM_TERMINATED) (set! state "terminated")]
        [else                (set! state "(unknown state")])
      (Scm_Printf port "#<thread %S %s %p>" (-> vm name) state vm)))
   )
 )
;; API
;; Other thread stuff is in ext/threads/thrlib.stub
(define-cproc current-thread () (return (SCM_OBJ (Scm_VM))))

;; API
(define-cproc vm-dump
  (:optional (vm::<thread> (c "SCM_OBJ(Scm_VM())"))) ::<void>
  (Scm_VMDump vm))

;; API
(define-cproc vm-get-stack-trace
  (:optional (vm::<thread> (c "SCM_OBJ(Scm_VM())")))
  (return (Scm_VMGetStack vm)))

;; API
(define-cproc vm-get-stack-trace-lite
  (:optional (vm::<thread> (c "SCM_OBJ(Scm_VM())")))
  (return (Scm_VMGetStackLite vm)))

(define-cproc %vm-show-stack-trace (trace :key
                                          (port::<port> (current-output-port))
                                          (maxdepth::<int> 0)
                                          (skip::<int> 0)
                                          (offset::<int> 0))
  ::<void>
  (Scm_ShowStackTrace port trace maxdepth skip offset
                      SCM_STACK_TRACE_FORMAT_ORIGINAL))

;; API
(select-module gauche.internal)
(define-cproc %vm-custom-error-reporter-set! (vm::<thread> handler) ::<void>
  (unless (or (SCM_FALSEP handler) (SCM_PROCEDUREP handler))
    (SCM_TYPE_ERROR handler "a procedure or #f"))
  (set! (-> vm customErrorReporter) handler))

;; parameter internal API
;; These will be called by the public API in gauche.parameter.  The protocol
;; is a bit weird, for the Scheme-level parameter has its own instance
;; definition distinct from C-level ScmParameterLoc.  Eventually it would
;; be nicer if we could merge two.
(select-module gauche.internal)

(define-cproc %vm-make-parameter-slot () ::<int>
  (let* ([loc::ScmParameterLoc])
    (Scm_InitParameterLoc (Scm_VM) (& loc) SCM_FALSE)
    (return (ref loc index))))

(define-cproc %vm-parameter-ref (index::<int> init-value)
  (let* ([loc::ScmParameterLoc])
    (set! (ref loc index) index
          (ref loc initialValue) init-value)
    (return (Scm_ParameterRef (Scm_VM) (& loc)))))

(define-cproc %vm-parameter-set! (index::<int> init-value new-value)
  (let* ([loc::ScmParameterLoc])
    (set! (ref loc index) index
          (ref loc initialValue) init-value)
    (return (Scm_ParameterSet (Scm_VM) (& loc) new-value))))

;; TRANSIENT
;; For the backward compatibility---files precompiled by 0.9.2 or before
;; can contain reference to the old API (as the result of expansion of
;; parameterize).  These definition converts them to the new API.
;; Will be removed on 1.0 release.
(select-module gauche)
(define (%vm-make-parameter-slot)
  (values ((with-module gauche.internal %vm-make-parameter-slot)) 0))
(define (%vm-parameter-ref index id)
  ((with-module gauche.internal %vm-parameter-ref) index #f))
(define (%vm-parameter-set! index id value)
  ((with-module gauche.internal %vm-parameter-set!) index #f value))

;;;
;;;Tentative compiler macro
;;;

;;
;;  (define-compiler-macro <name> <transformer>)
;;
;;  The <transformer> is the same as macro transformers, except that
;;  <transformer> can return the given form untouched if it gives up
;;  expansion.
;;
;;  For the backward compatilibity, the following form is also recognized
;;  as <transfomer>:
;;
;;    (er-transformer
;;     (lambda (form rename compare) ...)))
;;
;;  This resembles er-macro-transformer, but in fact we dispatch to a
;;  half-baked temporary implementation of macro transformer; it is
;;  for internal optimization experiment until the 'real' macro system
;;  is in place.   It'll be dropped once we have revised macro system,
;;  and user code shouldn't use it.

(select-module gauche)

;; API
(define-macro (define-compiler-macro name xformer-spec)
  (if (and (= (length xformer-spec) 2)
           (eq? (unwrap-syntax (car xformer-spec)) 'er-transformer))
    `((with-module gauche.internal %bind-inline-er-transformer)
      (current-module) ',name ,(cadr xformer-spec))
    `((with-module gauche.internal %attach-inline-transformer)
      (current-module) ',name
      (^[form cenv]
        ((with-module gauche.internal call-macro-expander)
         ,xformer-spec form cenv)))))
