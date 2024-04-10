;;;
;;; libparam.scm - Parameters
;;;
;;;   Copyright (c) 2020-2024  Shiro Kawai  <shiro@acm.org>
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

;; NB: This must be initialized _after_ the object and marco system fully
;; booted.

(select-module gauche)
(use util.match)

(inline-stub
 (.include "gauche/priv/configP.h"
           "gauche/priv/parameterP.h"
           "gauche/priv/vmP.h"))

(declare (keep-private-macro parameterize parameterize/dynwind temporarily))

;;;
;;; Primitive parameters
;;;

;; Parameter internal API
(select-module gauche.internal)

(define-cproc %primitive-parameter-ref (p::<primitive-parameter>)
  (return (Scm_PrimitiveParameterRef (Scm_VM) p)))
(define-cproc %primitive-parameter-set! (p::<primitive-parameter> val)
  (return (Scm_PrimitiveParameterSet (Scm_VM) p val)))
(define-cproc %make-parameter-subr (p::<primitive-parameter>)
  Scm_MakePrimitiveParameterSubr)
(define-cproc %with-parameterization (params vals thunk)
  (Scm_PushParameterization params vals)
  (return (Scm_VMApply0 thunk)))

;;;
;;; Generic parameters
;;;

(select-module gauche)

;; Must not be instantiated directly.  Always use make-parameter.
(define-class <parameter> (<primitive-parameter>)
  (;; all slots should be private.
   (filter :init-keyword :filter :init-value #f)
   (setter)
   (restorer)                          ;used to restore previous value
   ;; These are for the backward compatibility.  gauche.parameter module
   ;; provides the feature that uses them.  New code shouldn't use these.
   (pre-observers)
   (post-observers)
   ))

(define (%make-parameter value :optional (filter #f) (type 'legacy))
  (call-with-current-expression-name
   (^[name]
     (let* ([v (if filter (filter value) value)]
            [p (make <parameter>
                 :filter filter
                 :initial-value v
                 :type type
                 :name name)])
       (getter-with-setter
        ((with-module gauche.internal %make-parameter-subr) p)
        (^[val] ((slot-ref p 'setter) val)))))))

(define (make-thread-parameter value :optional (filter #f))
  (%make-parameter value filter 'thread))

;; this is SRFI-226 make-parameter
(define (make-shared-parameter value :optional (filter #f))
  (%make-parameter value filter 'shared))

(define (make-legacy-parameter value :optional (filter #f))
  (%make-parameter value filter 'legacy))

;; In 0.9.13, make-parameter returns a legacy parameter so that it
;; won't break existing code.  We'll switch it to make-shared-parameter
;; in futuer release.
;; Note that you also need to use parameterize/dynwind to get full
;; backward compatibility.  Using gauche.parameter guarantees it.
(define make-parameter make-legacy-parameter)

(define (parameter? obj)
  (boolean (procedure-parameter obj)))

(define-cproc procedure-parameter (proc)
  (if (and (SCM_SUBRP proc)
           (SCM_PAIRP (SCM_PROCEDURE_INFO proc))
           (SCM_EQ (SCM_CAR (SCM_PROCEDURE_INFO proc))
                   (Scm__GetParameterSymbol)))
    (return (SCM_OBJ (SCM_SUBR_DATA proc)))
    (return SCM_FALSE)))

(select-module gauche.internal)

(define-cproc %parameter-name (param)
  (unless (SCM_PRIMITIVE_PARAMETER_P param)
    (SCM_TYPE_ERROR param "<parameter>"))
  (return (-> (SCM_PRIMITIVE_PARAMETER param) name)))

(define (%parameter-set! param val)     ;called from general_param_proc
  ((slot-ref param 'setter) val))

(define-method initialize ((self <parameter>) initargs)
  (next-method)
  (let ([filter (slot-ref self 'filter)]
        [get (^[] (%primitive-parameter-ref self))]
        [set (^v (%primitive-parameter-set! self v))])
    ;; Those callback may be overwritten if hooks are set.
    (slot-set! self 'setter
               (if filter
                 (^v (rlet1 old (get)
                       (set (filter v))))
                 (^v (rlet1 old (get)
                       (set v)))))
    (slot-set! self 'restorer          ;bypass filter proc
               (^v (rlet1 old (get)
                     (set v))))))

;; restore parameter value after parameterize body.  we need to bypass
;; the filter procedure (fix for the bug reported by Joo ChurlSoo.
;; NB: For historical reasons, PARAMETERIZE may be used with paremeter-like
;; procedures.
(define (%restore-parameter param prev-val)
  (cond
   [(procedure-parameter param) =>
    (^p (if (is-a? p <parameter>)
          ((slot-ref p'restorer) prev-val)
          ((with-module gauche.internal %primitive-parameter-set!) p prev-val)))]
   ;; <parameter> and <primitive-parameter> should never be used directly,
   ;; but for the backward compatibility:
   [(is-a? param <parameter>) ((slot-ref param'restorer) prev-val)]
   [(is-a? param <primitive-parameter>)
    ((with-module gauche.internal %primitive-parameter-set!) param prev-val)]
   [else (param prev-val)]))

;; Actual processing of the parameterization.
;;  gauche.internal#%parameterize is embedded in the compiled code, so do not
;;  change the API across releases.
(define (%parameterize params orig-vals thunk compatible?)
  ;; Returns list of filters if all params are parameter procedures.
  ;; If not, we fallback to the old parameterize.
  (define (gather-filters params)
    (cond [(null? params) '()]
          [(compatible-parameter (car params))
           => (^p (and-let1 tail (gather-filters (cdr params))
                    (if (is-a? p <parameter>)
                      (cons (or (slot-ref p 'filter) identity) tail)
                      (cons identity tail))))]
          [else #f]))
  (define (compatible-parameter param)
    (and-let* ([p (procedure-parameter param)]
               [ (if (is-a? p <parameter>)
                   (and (not (slot-bound? p 'pre-observers))
                        (not (slot-bound? p 'post-observers)))
                   (is-a? p <primitive-parameter>)) ])
      p))
  (define (apply-filters filters vals)
    (if (null? filters)
      '()
      (cons ((car filters) (car vals))
            (apply-filters (cdr filters) (cdr vals)))))

  (cond [(gather-filters params)
         ;; SRFI-226 conformant parameters
         => (^[filters]
              (%with-parameterization (map procedure-parameter params)
                                      (apply-filters filters orig-vals)
                                      thunk))]
        [compatible?
         ;; Backward-compatibiltiy mode
         (%parameterize/dynwind params orig-vals thunk)]
        [else
         (let1 p (find (complement compatible-parameter) params)
           (if (procedure-parameter p)
             (error "Incompatible parameter for SRFI-226 parameterize:" p)
             (error "Non-parameter can't be used with SRFI-226 parameterize:" p)))]))

(define (%parameterize/dynwind params orig-vals thunk)
  (let ([restarted #f]
        [saved '()])
    (dynamic-wind
      (^[] (set! saved
                 (if restarted
                   (map (^[p v] (%restore-parameter p v)) params saved)
                   (map (^[p] (p)) params))))
      (^[] (unless restarted
             (set! saved (map (^[p v] (p v)) params orig-vals)))
        (thunk))
      (^[] (set! restarted #t)
        (set! saved (map (^[p v] (%restore-parameter p v)) params saved))))))


(select-module gauche)

(define-syntax parameterize
  (er-macro-transformer
   (^[f r c]
     (define %parameterize
       ((with-module gauche.internal make-identifier)
        '%parameterize
        (find-module 'gauche.internal)
        '()))
     (match f
       [(_ () . body) (quasirename r `(let () ,@body))]
       ;; TODO: shortcut for single-parameter case
       [(_ ((param val) ...) . body)
        (quasirename r
          `(,%parameterize (list ,@param) (list ,@val) (^[] ,@body) #t))]
       [(_ . x) (error "Invalid parameterize form:" f)]))))

(define-syntax parameterize/dynwind
  (er-macro-transformer
   (^[f r c]
     (define %parameterize/dynwind
       ((with-module gauche.internal make-identifier)
        '%parameterize/dynwind
        (find-module 'gauche.internal)
        '()))
     (match f
       [(_ () . body) (quasirename r `(let () ,@body))]
       ;; TODO: shortcut for single-parameter case
       [(_ ((param val) ...) . body)
        (quasirename r
          `(,%parameterize/dynwind (list ,@param) (list ,@val) (^[] ,@body)))]
       [(_ . x) (error "Invalid parameterize form:" f)]))))

(define-cproc current-parameterization ()
  Scm_CurrentParameterization)

(define-cproc parameterization? (obj) ::<boolean>
  SCM_PARAMETERIZATIONP)

;; SRFI-226
(define-syntax temporarily
  (er-macro-transformer
   (^[f r c]
     (match f
       [( () . body) (quasirename r `(let () ,@body))]
       [(_ ((param-like val) ...) . body)
        (let ([Ps (map (^_ (gensym)) param-like)]
              [Vs (map (^_ (gensym)) val)])
          (quasirename r
            `(let (,@(map list Ps param-like)
                   ,@(map list Vs val))
               (dynamic-wind
                 (^[] ,@(map (^[p v]
                               (quasirename r
                                 `(let ((tmp (,p))) (,p ,v) (set! ,v tmp))))
                             Ps Vs))
                 (^[] ,@body)
                 (^[] ,@(map (^[p v]
                               (quasirename r
                                 `(let ((tmp (,p))) (,p ,v) (set! ,v tmp))))
                             Ps Vs))))))]
       [_ (error "Invalid temporarily form:" f)]))))


(define-cproc call-with-parameterization (parameterization::<parameterization>
                                          thunk)
  (Scm_InstallParameterization parameterization)
  (return (Scm_VMApply0 thunk)))
