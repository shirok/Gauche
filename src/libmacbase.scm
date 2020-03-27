;;;
;;; libmacbase.scm - macro basic staff
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
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
(use util.match)
(inline-stub
 (declcode (.include "gauche/class.h"
                     "gauche/priv/macroP.h")))

;; This file defines lower level layer of macro system.
;; Initialization of compile.scm depends on this file.

;;;
;;; Macro expansion utilities
;;;

;; macroexpand FORM [ENV/FLAG]
;; macroexpand-1 FORM [ENV/FLAG]
;;   ENV/FLAG can be:
;;     #f - we expand it in the current runtime env, run unravel-syntax on output
;;     #t - we expand it in the current runtime env
;;     #<module> - use it as the toplevel env.

;; API
(define-in-module gauche (macroexpand form :optional (env/flag #f))
  (%do-macroexpand form env/flag #f))
;; API
(define-in-module gauche (macroexpand-1 form :optional (env/flag #f))
  (%do-macroexpand form env/flag #t))

(define (%do-macroexpand form env/flag once?)
  (let* ([env (cond
               [(boolean? env/flag) (vm-current-module)]
               [(module? env/flag) env/flag]
               [else (error "argument must be a boolean or a module, but got:"
                            env/flag)])]
         [r (%internal-macro-expand form (make-cenv env) once?)])
    (if (eq? env/flag #f)
      (unravel-syntax r)
      r)))

(select-module gauche)
;; API
;; This strips all syntactic information (lossy)
(define-cproc unwrap-syntax (form :optional (to_immutable::<boolean> #f))
  (if to_immutable
    (return (Scm_UnwrapSyntaxImmutable form))
    (return (Scm_UnwrapSyntax form))))

;; API
;; This preserves identity of local identifiers by suffixing it.
;; The identity of toplevel identifiers are still not preserved across modules.
(select-module gauche.internal)
(define-in-module gauche (unravel-syntax form)
  (define id->sym
    (let1 tab (make-hash-table 'eq?)  ; identifier -> symbol
      (^[id]
        (or (hash-table-get tab id #f)
            (let1 nam (unwrap-syntax id)
              (if (global-identifier=? id (make-identifier nam
                                                           (vm-current-module)
                                                           '()))
                (begin (hash-table-put! tab id nam) nam)
                (let1 num ($ hash-table-fold tab
                             (^[i _ c]
                               (if (eq? (unwrap-syntax i) nam) (+ c 1) c))
                             0)
                  (rlet1 sym (symbol-append nam "." num)
                    (hash-table-put! tab id sym)))))))))
  (define (rec form)
    (cond [(wrapped-identifier? form) (id->sym form)]
          [(pair? form) (cons (rec (car form)) (rec (cdr form)))]
          [(vector? form) (vector-map rec form)]
          [else form]))
  (rec form))

(select-module gauche.internal)
(inline-stub
 (define-type <macro> "ScmMacro*" "macro"
   "SCM_MACROP" "SCM_MACRO" "SCM_OBJ")
 )

;; These are used in the compiler, and hidden inside gauche.internal.
(define-cproc macro? (obj) ::<boolean> SCM_MACROP)
(define-cproc syntax? (obj) ::<boolean> SCM_SYNTAXP)

;;;
;;; Macro object
;;;

(select-module gauche)
(inline-stub
 (define-cclass <macro>
   "ScmMacro*" "Scm_MacroClass"
   (c "SCM_CLASS_DEFAULT_CPL")
   ((name         :setter #f)
    (transformer  :setter #f)
    (source       :c-name "src" :setter #f)
    (describer    :setter #f))
   (printer (Scm_Printf port "#<macro %A>" (-> (SCM_MACRO obj) name)))))

;;;
;;; Transformer interface
;;;

;; NB: %make-macro-transformer is in libalpha.scm

(select-module gauche.internal)

(define-cproc compile-syntax-rules (name src ellipsis literals rules mod env)
  Scm_CompileSyntaxRules)

(define-cproc macro-transformer (mac::<macro>) Scm_MacroTransformer)
(define-cproc macro-name (mac::<macro>) Scm_MacroName)

;; Macro expand tracer
;; *trace-macro* can be #f (default - no trace), #t (trace all macros),
;; or a list of symbols/regexp (trace macros whose name that matches)
(define *trace-macro* #f)

(define (%macro-traced? mac)
  (or (eq? *trace-macro* #t)
      (and-let1 macname (unwrap-syntax (macro-name mac))
        (find (^z (cond [(symbol? z) (eq? z macname)]
                        [(regexp? z) (z (symbol->string macname))]
                        [else #f]))
              *trace-macro*))))

(define (call-macro-expander mac expr cenv)
  (let* ([r ((macro-transformer mac) expr cenv)]
         [out (if (and (pair? r) (not (eq? expr r)))
                (rlet1 p (if (extended-pair? r)
                           r
                           (extended-cons (car r) (cdr r)))
                  (pair-attribute-set! p 'original expr))
                r)])
    (when (and *trace-macro* (%macro-traced? mac))
      ;; NB: We need to apply unravel-syntax on expr and out at once,
      ;; so that we can correspond the identifiers from input and output.
      (let1 unraveled (unravel-syntax (cons expr out))
        (display "Macro input>>>\n" (current-trace-port))
        (pprint (car unraveled) :port (current-trace-port) :level #f :length #f)
        (display "\nMacro output<<<\n" (current-trace-port))
        (pprint (cdr unraveled) :port (current-trace-port) :level #f :length #f)
        (display "\n" (current-trace-port))
        (flush (current-trace-port))))
    out))

(define-cproc make-syntax (name::<symbol> module::<module> proc)
  Scm_MakeSyntax)

(define-cproc make-syntactic-closure (env literals expr)
  Scm_MakeSyntacticClosure)

(define-cproc call-syntax-handler (syn program cenv)
  (SCM_ASSERT (SCM_SYNTAXP syn))
  (return (Scm_VMApply2 (-> (SCM_SYNTAX syn) handler) program cenv)))

(define-cproc syntax-handler (syn)
  (SCM_ASSERT (SCM_SYNTAXP syn))
  (return (-> (SCM_SYNTAX syn) handler)))

(define-in-module gauche (trace-macro . args)
  ;; force resolving autoload of pprint before changing *trace-macro*; otherwise,
  ;; autoloading pprint can trigger macro tracing which needs pprint.
  (procedure? pprint)
  (match args
    [() #f]                             ;just show current traces
    [(#t) (set! *trace-macro* #t)]      ;trace all macros
    [(#f) (set! *trace-macro* #f)]      ;untrace all macros
    [(objs ...)
     (unless (every (^x (or (symbol? x) (regexp? x))) objs)
       (error "Argument(s) of trace-macro should be a \
               boolean, or symbols or regexps, but got:" args))
     (and-let1 ms (cond [(not *trace-macro*) '()]
                        [(list? *trace-macro*) *trace-macro*]
                        [else #f]) ;; if we're tracing all macros, no need to
                                   ;; add symbols.
       (set! *trace-macro* (delete-duplicates (append objs ms))))])
  *trace-macro*)

(define-in-module gauche (untrace-macro . args)
  (match args
    [() (set! *trace-macro* #f)]        ;untrace all
    [(sym ...) (when (list? *trace-macro*)
                 (let1 ms (remove (cute member <> sym) *trace-macro*)
                   (set! *trace-macro* (if (null? ms) #f ms))))])
  *trace-macro*)
