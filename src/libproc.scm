;;;
;;; libproc.scm - procedure call & return, and other control stuff
;;;
;;;   Copyright (c) 2000-2022  Shiro Kawai  <shiro@acm.org>
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
 (declcode (.include "gauche/vminsn.h"
                     "gauche/prof.h"
                     "gauche/priv/vmP.h"
                     "gauche/priv/procP.h")))

;;;
;;; R5RS
;;;

(select-module scheme)
(define-cproc procedure? (obj) ::<boolean> :fast-flonum :constant
  SCM_PROCEDUREP)

(define-cproc apply (proc arg1 :rest args)
  (inliner TAIL-APPLY)
  (let* ([head::ScmObj] [tail::ScmObj])
    (cond [(SCM_NULLP args) (return (Scm_VMApply proc arg1))]
          [else (set! head (Scm_Cons arg1 SCM_NIL)
                      tail head)
                (dopairs [cp args]
                  (when (SCM_NULLP (SCM_CDR cp))
                    (when (< (Scm_Length (SCM_CAR cp)) 0)
                      (Scm_Error "improper list not allowed: %S" (SCM_CAR cp)))
                    (SCM_APPEND head tail (SCM_CAR cp))
                    (break))
                  (unless (SCM_PAIRP (SCM_CDR cp))
                    (Scm_Error "improper list not allowed: %S" (SCM_CDR cp)))
                  (SCM_APPEND1 head tail (SCM_CAR cp)))
                (return (Scm_VMApply proc head))])))

(define-cproc call-with-current-continuation (proc) Scm_VMCallCC)
(define-cproc values (:rest args) :constant (inliner VALUES) Scm_Values)
(define-cproc dynamic-wind (pre body post) Scm_VMDynamicWind)

(define-in-module scheme (call-with-values producer consumer)
  (receive vals (producer) (apply consumer vals)))

(define-in-module scheme call/cc call-with-current-continuation)

;; srfi-226
(define-cproc continuation? (obj) ::<boolean> Scm_ContinuationP)

(select-module gauche.internal)
;; for partial continuation.  See lib/gauche/partcont.scm
(define-cproc %call/pc (proc) (return (Scm_VMCallPC proc)))
(define-cproc %reset (proc) (return (Scm_VMReset proc)))

;; Continuaton prompts
(select-module gauche)
(define-cproc make-continuation-prompt-tag (:optional (name #f))
  Scm_MakePromptTag)
(define-cproc default-continuation-prompt-tag ()
  Scm_DefaultPromptTag)
(define-cproc continuation-prompt-tag? (obj) ::<boolean>
  SCM_PROMPT_TAG_P)

(define-cproc call-with-continuation-prompt (thunk
                                             :optional (prompt-tag #f)
                                                       (abort-handler #f))
  Scm_VMCallWithContinuationPrompt)
(define-cproc abort-current-continuation (prompt-tag :rest objs)
  Scm_VMAbortCurrentContinuation)

;; Continuation marks
(select-module gauche)
(define-cproc continuation-marks (cont :optional (prompt-tag #f))
  (return (Scm_ContinuationMarks cont prompt-tag)))
(define-cproc current-continuation-marks (:optional (prompt-tag #f))
  (return (Scm_CurrentContinuationMarks prompt-tag)))

(define-cproc continuation-marks? (obj) ::<boolean>
  (return (SCM_CONTINUATION_MARK_SET_P obj)))

(define-cproc call-with-immediate-continuation-mark (key proc
                                                     :optional (fallback #f))
  (let* ([vm::ScmVM* (Scm_VM)]
         [p (-> vm denv)])
    (for [() (SCM_PAIRP p) (set! p (SCM_CDR p))]
      (cond [(and (-> vm cont) (SCM_EQ p (-> vm cont denv)))
             (return (Scm_VMApply1 proc fallback))]
            [(SCM_EQ (SCM_CAAR p) key)
             (return (Scm_VMApply1 proc (SCM_CDAR p)))]))
    (return (Scm_VMApply1 proc fallback))))

;; TRANSIENT: To compile 0.9.13 with 0.9.12
(inline-stub
 (declare-stub-type <continuation-mark-set> "ScmContinuationMarkSet*")
 )

(define-cproc continuation-mark-set->list (cmset::<continuation-mark-set> key)
  (return (Scm_ContinuationMarkSetToList cmset key)))

(define-cproc continuation-mark-set-first (cmset::<continuation-mark-set>?
                                           key :optional (fallback #f))
  (let* ([cms::ScmContinuationMarkSet*
          (?: cmset cmset
              (SCM_CONTINUATION_MARK_SET (Scm_CurrentContinuationMarks SCM_UNDEFINED)))]
         [p (-> cms denv)])
    (for [() (SCM_PAIRP p) (set! p (SCM_CDR p))]
      (when (SCM_EQ (SCM_CAAR p) key)
        (return (SCM_CDAR p))))
    (return fallback)))

;; srfi-226
;; We use uninterned symbols for unique continuation mark key.
;; Since we don't have specialized procedures for the keys specifically
;; created with make-continuation-mark-key, our continuation-mark-key?
;; returns #t for all objects.
(define (make-continuation-mark-key :optional (name 'continuation-mark-key-))
  (gensym (x->string name)))
(define (continuation-mark-key? obj) #t)

;;;
;;; Useful gadgets
;;;

(select-module gauche)
;; 'values' could be used, but sometimes handy to be explicit that
;; we're dealing with a single value.
(define-inline (identity x) x)

;; more verbose than its definition, but it shows the intention clearly.
(define-inline (constantly val) (^ _ val))

;;;
;;; Generator primitives
;;;

;; Generators are just an ordinary procedure, so having these in core
;; comes handy.  We used to have these as port-*.

(select-module gauche.internal)
;; Returns a list of new values generated by each of GENS, having tail
;; as the tail of the returned list.  It returns #<eof> when any one of
;; generator returns eof (generation is
(define (%generate-values gens . tail)
  (fold-right (^[g tail]
                (let1 v (g)
                  (if (or (eof-object? v) (eof-object? tail))
                    (eof-object)
                    (cons v tail))))
              tail gens))

(define-in-module gauche (generator-fold fn knil gen . more)
  (if (null? more)
    (let loop ([item (gen)]
               [r    knil])
      (if (eof-object? item)
        r
        (let1 r (fn item r)
          (loop (gen) r))))
    (let1 gens (cons gen more)
      (let loop ([knil knil])
        (let1 items (%generate-values gens knil)
          (if (eof-object? items)
            knil
            (loop (apply fn items))))))))

;; This will consume large stack if input file is large.
(define-in-module gauche (generator-fold-right fn knil gen . more)
  (if (null? more)
    (let loop ([item (gen)])
      (if (eof-object? item)
        knil
        (fn item (loop (gen)))))
    (let1 gens (cons gen more)
      (let loop ()
        (let1 items (%generate-values gens)
          (if (eof-object? items)
            knil
            (apply fn (append items `(,(loop))))))))))

(define-in-module gauche (generator-for-each fn gen . more)
  (if (null? more)
    (let loop ([item (gen)])
      (unless (eof-object? item)
        (fn item) (loop (gen))))
    (let1 gens (cons gen more)
      (let loop ()
        (let1 items (%generate-values gens)
          (unless (eof-object? items)
            (apply fn items)
            (loop)))))))

(define-in-module gauche (generator-map fn gen . more)
  (if (null? more)
    (let loop ([item (gen)] [r '()])
      (if (eof-object? item)
        (reverse r)
        (let1 r (cons (fn item) r)
          (loop (gen) r))))
    (let1 gens (cons gen more)
      (let loop ([r '()])
        (let1 items (%generate-values gens)
          (if (eof-object? items)
            (reverse r)
            (loop (cons (apply fn items) r))))))))

(define-in-module gauche (generator-find pred gen)
  (let loop ([item (gen)])
    (cond [(eof-object? item) #f]
          [(pred item) item]
          [else (loop (gen))])))

;;;
;;; Profiler
;;;

(select-module gauche)
(define-cproc profiler-start () ::<void> Scm_ProfilerStart)
(define-cproc profiler-stop  () ::<int>  Scm_ProfilerStop)
(define-cproc profiler-reset () ::<void> Scm_ProfilerReset)

(select-module gauche.internal)
;; Autoloaded profiler-get-result will use this.
;; See lib/gauche/vm/profiler.scm
(define-cproc profiler-raw-result () Scm_ProfilerRawResult)

;;;
;;; Introspection
;;;

(select-module gauche)
;; Misc.  WARNING: API may change
(define-cproc subr? (obj)    ::<boolean> SCM_SUBRP)
(define-cproc closure? (obj) ::<boolean> SCM_CLOSUREP)
(define-cproc toplevel-closure? (obj) ::<boolean>
  (return (and (SCM_CLOSUREP obj) (== (-> (SCM_CLOSURE obj) env) NULL))))

(define-cproc closure-code (clo::<closure>) (return (SCM_CLOSURE_CODE clo)))
(define-cproc method-code (m::<method>)
  (if (-> m func)
    ;; code is not available for C-defined method
    (return SCM_FALSE)
    (return (SCM_OBJ (-> m data)))))

(define-cproc procedure-info (proc::<procedure>)
  (return (SCM_PROCEDURE_INFO proc)))

;; procedure-type returns a descriptive type of the given procedure.
;; It is an instance of <^> type constructor (see libtype.scm).
;; procedure-type is computed lazily, and cached to proc->typeHint.
;; If the procedure is precompiled, the type info is serialized to a
;; vector; it is reconstructed to #<^ ...> type when accessed for the
;; first time.
(define-cproc procedure-type (proc::<procedure>)
  (let* ([typehint (-> proc typeHint)])
    (cond [(SCM_VECTORP typehint)
           (let* ([reconstruct-proc::(static ScmObj) SCM_UNDEFINED])
             (SCM_BIND_PROC reconstruct-proc "reconstruct-procedure-type"
                            (Scm_GaucheInternalModule))
             (let1/cps type (Scm_VMApply2 reconstruct-proc (SCM_OBJ proc) typehint)
               (proc)
               (set! (-> (cast ScmProcedure* proc) typeHint) type)
               (return type)))]
          [(SCM_FALSEP typehint)
           (let* ([compute-proc::(static ScmObj) SCM_UNDEFINED])
             (SCM_BIND_PROC compute-proc "compute-procedure-type"
                            (Scm_GaucheInternalModule))
             (let1/cps type (Scm_VMApply1 compute-proc (SCM_OBJ proc))
               (proc)
               (set! (-> (cast ScmProcedure* proc) typeHint) type)
               (return type)))]
          [else (return typehint)])))

(define-cproc method-leaf? (m::<method>) ::<boolean> SCM_METHOD_LEAF_P)

;; NB: This takes a list of classes.  But what if we support eqv-specilizer?
;; One idea is to let the caller wrap a concrete instance.  We'll see...
(define (applicable? proc . arg-types)
  (define method-applicable?
    (with-module gauche.object method-applicable-for-classes?))
  (let1 c (class-of proc)
    (cond [(eq? c <procedure>)
           (let1 nargs (length arg-types)
             (if-let1 infos (case-lambda-decompose proc)
               (any (^[info] (apply [^(reqargs optarg proc)
                                      ((if optarg >= =) nargs reqargs)]
                                    info))
                    infos)
               ((if (slot-ref proc 'optional) >= =)
                nargs (slot-ref proc 'required))))]
          [(eq? c <generic>)
           (any (^m (apply method-applicable? m arg-types)) (~ proc'methods))]
          [else (apply applicable? object-apply c arg-types)])))

(select-module gauche.internal)
;; If procedure has a setter and it's locked, return it.  Otherwise
;; return #f
(define-cproc procedure-locked-setter (proc::<procedure>)
  (if (and (-> proc locked)
           (SCM_PROCEDUREP (-> proc setter)))
    (return (-> proc setter))
    (return SCM_FALSE)))
;; Returns #t iff obj is a procedure AND constant
;; Note: This also returns #t if obj is generic and sealed.  The behavior
;; may change in future, though, so do not rely on it.
(define-cproc procedure-constant? (obj) ::<boolean>
  (return (and (SCM_PROCEDUREP obj)
               (SCM_PROCEDURE_CONSTANT obj))))

(define-cproc %procedure-copy (p::<procedure> :optional (tags-alist #f))
  Scm__CopyProcedure)

(define-cproc %procedure-tags-alist (p::<procedure>)
  (return (-> p tagsAlist)))

(select-module gauche.internal)
;; Make the environment the closure closes into a list and return it.
;; Used from gauche.test.
(define-cproc %closure-env->list (clo::<closure>)
  (let* ([env::ScmEnvFrame* (SCM_CLOSURE_ENV clo)]
         [h SCM_NIL]
         [t SCM_NIL])
    (when (== env NULL)
      (return SCM_NIL))
    (dotimes [i (-> env size)]
      (SCM_APPEND1 h t (ENV_DATA env i)))
    (return h)))

(select-module gauche.internal)
;; Tester procedures
;;   These are not meant to be used in the actual Scheme code.  They're
;;   here to test particular C APIs which wouldn't be called from normal
;;   Scheme programs.
(define-cproc %apply-rec0 (p)            Scm_ApplyRec0)
(define-cproc %apply-rec1 (p a)          Scm_ApplyRec1)
(define-cproc %apply-rec2 (p a b)        Scm_ApplyRec2)
(define-cproc %apply-rec3 (p a b c)      Scm_ApplyRec3)
(define-cproc %apply-rec4 (p a b c d)    Scm_ApplyRec4)
(define-cproc %apply-rec  (p :rest args) Scm_ApplyRec)

(select-module gauche.internal)
;; To add autocurrying flag (experimental)
(define-cproc %procedure-currying-set! (p::<procedure> f::<boolean>) ::<void>
  (set! (SCM_PROCEDURE_CURRYING p) f))
