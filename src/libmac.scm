;;;
;;; libmac.scm - macro-related stuff
;;;
;;;   Copyright (c) 2000-2018  Shiro Kawai  <shiro@acm.org>
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

(declare (keep-private-macro define-compiler-macro))

;;;
;;; Macro expansion utilities
;;;

;; API
(define-in-module gauche (macroexpand form)
  (%internal-macro-expand form (make-cenv (vm-current-module)) #f))
;; API
(define-in-module gauche (macroexpand-1 form)
  (%internal-macro-expand form (make-cenv (vm-current-module)) #t))

(select-module gauche)
;; API
;; This strips all syntactic information (lossy)
(define-cproc unwrap-syntax (form) Scm_UnwrapSyntax)

;; API
;; EXPERIMENTAL
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
    (cond [(identifier? form) (id->sym form)]
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

;; Macro expand tracer (temporary)
;; *trace-macro* can be #f (default - no trace), #t (trace all macros),
;; or a list of symbols (trace macros whose name that matches one of the
;; symbols).
(define *trace-macro* #f)

(define (call-macro-expander mac expr cenv)
  (let* ([r ((macro-transformer mac) expr cenv)]
         [out (if (and (pair? r) (not (eq? expr r)))
                (rlet1 p (if (extended-pair? r)
                           r
                           (extended-cons (car r) (cdr r)))
                  (pair-attribute-set! p 'original expr))
                r)])
    (when (and *trace-macro*
               (or (eq? *trace-macro* #t)
                   (memq (macro-name mac) *trace-macro*)))
      ;; NB: We need to apply unravel-syntax on expr and out at once,
      ;; so that we can correspond the identifiers from input and output.
      (let1 unraveled (unravel-syntax (cons expr out))
        (display "Macro input>>>\n")
        (pprint (car unraveled) :port (current-error-port) :level #f :length #f)
        (display "\nMacro output<<<\n")
        (pprint (cdr unraveled) :port (current-error-port) :level #f :length #f)
        (display "\n")))
    out))

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

;; EXPERIMENTAL - API may change
(define-in-module gauche (trace-macro . args)
  (match args
    [() #f]                             ;just show current traces
    [(#t) (set! *trace-macro* #t)]      ;trace all macros
    [(sym ...) (and-let1 ms (cond [(not *trace-macro*) '()]
                                 [(list? *trace-macro*) *trace-macro*]
                                 [else #f])
                 (set! *trace-macro* (delete-duplicates (append sym ms))))])
  *trace-macro*)

(define-in-module gauche (untrace-macro . args)
  (match args
    [() (set! *trace-macro* #f)]        ;untrace all
    [(sym ...) (when (list? *trace-macro*)
                 (let1 ms (remove (cute memq <> sym) *trace-macro*)
                   (set! *trace-macro* (if (null? ms) #f ms))))])
  *trace-macro*)

;;;
;;; OBSOLETED - Tentative compiler macro 
;;;

;;  Use define-inline/syntax instead.
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

(select-module gauche)

;; TRANSIENT:
;; API
(define-macro (define-compiler-macro name xformer-spec)
  (warn "define-compiler-macro is obsoleted.  Use define-inline/syntax.")
  (if (and (= (length xformer-spec) 2)
           (eq? (unwrap-syntax (car xformer-spec)) 'er-transformer))
    `((with-module gauche.internal %bind-inline-er-transformer)
      (current-module) ',name ,(cadr xformer-spec))
    `((with-module gauche.internal %attach-inline-transformer)
      (current-module) ',name
      (^[form cenv]
        ((with-module gauche.internal call-macro-expander)
         ,xformer-spec form cenv)))))

(select-module gauche.internal)

;; TRANSIENT: This is obsoleted, but may be referred from files precompiled
;; by 0.9.5 and before.  Remove this after 1.0 release.
;; proc :: Arg, ... -> Sexpr
(define (make-macro-transformer name proc)
  (%make-macro-transformer name (^[form env] (apply proc (cdr form))) #f #f))

;; TRANSIENT: This is only used via obsoleted define-compiler-macro.
;; Remove this when we remove define-compiler-macro.
(define (%bind-inline-er-transformer module name er-xformer)
  (define macro-def-cenv (%make-cenv module '()))
  ($ %attach-inline-transformer module name
     (^[form cenv]
       ;; Call the transformer with rename and compare procedure,
       ;; just like explicit renaming macro.  However, THE CURRENT
       ;; CODE DOES NOT IMPLEMENT PROPER SEMANTICS.  They're just
       ;; placeholders for experiment.
       (er-xformer form
                   (cut ensure-identifier <> macro-def-cenv)
                   (^[a b] (free-identifier=? (ensure-identifier a cenv)
                                              (ensure-identifier b cenv)))))))

;; TRANSIENT: This is only used via obsoleted define-compiler-macro.
;; Remove this when we remove define-compiler-macro.
(define (%attach-inline-transformer module name xformer)
  (define proc (global-variable-ref module name #f))
  (unless proc
    (errorf "define-compiler-macro: procedure `~s' not defined in ~s"
            name module))
  ;; If PROC is defined by define-inline (thus have a packed IForm in
  ;; %procedure-inliner), we keep it and applies expand-inline-procedure
  ;; after the compiler macro finishes its job.
  (let1 orig-inliner (%procedure-inliner proc)
    (when (procedure? orig-inliner)
      (error "Attaching a compiler macro to ~a clobbers previously attached \
              inline transformers." proc))
    (set! (%procedure-inliner proc) (%make-macro-transformer name xformer)))
  (%mark-binding-inlinable! module name)
  name)

