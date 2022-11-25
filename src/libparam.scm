;;;
;;; libparam.scm - Parameters
;;;
;;;   Copyright (c) 2020-2022  Shiro Kawai  <shiro@acm.org>
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
 (declcode (.include <gauche/priv/parameterP.h>
                     <gauche/priv/vmP.h>)))

(declare (keep-private-macro parameterize parameterize/dynwind with))

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
  (;; all slots should be private.  'observers' slots are only initialized
   ;; when observers are set.
   (filter :init-keyword :filter :init-value #f)
   (setter)
   (restorer)                          ;used to restore previous value
   (pre-observers)
   (post-observers)
   ))

(define-method write-object ((p <parameter>) port)
  (format port "#<parameter ~a>" 
          ((with-module gauche.internal %parameter-name) p)))

;; We're switching to the srfi-226  model that parameter bindings are shared 
;; among threads by default.  The legacy code that requires thread-local
;; parameters need to move to make-thread-parameter.  To ease transition,
;; we also provide make-shared-parameter.
(define (make-parameter value :optional (filter #f) (shared? #f))
  (call-with-current-expression-name
   (^[name]
     (let* ([v (if filter (filter value) value)]
            [p (make <parameter>
                 :filter filter
                 :initial-value v
                 :shared shared?
                 :name name)])
       (getter-with-setter
        ((with-module gauche.internal %make-parameter-subr) p)
        (^[val] ((slot-ref p 'setter) val)))))))

(define (make-thread-parameter value :optional (filter #f))
  (make-parameter value filter #f))

;; this is srfi-226 make-parameter
(define (make-shared-parameter value :optional (filter #f))
  (make-parameter value filter #t))

(define (parameter? obj)
  (boolean (procedure-parameter obj)))

(define-cproc procedure-parameter (proc)
  (if (and (SCM_SUBRP proc)
           (SCM_EQ (SCM_PROCEDURE_INFO proc) (Scm__GetParameterSymbol)))
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
   [(procedure-parameter param) => (^p ((slot-ref p'restorer) prev-val))]
   ;; <parameter> and <primitive-parameter> should never be used directly,
   ;; but for the backward compatibility:
   [(is-a? param <parameter>) ((slot-ref param'restorer) prev-val)]
   [(is-a? param <primitive-parameter>)
    ((with-module gauche.internal %primitive-parameter-set!) param prev-val)]
   [else (param prev-val)]))

;; Actual processing of the parameterization.
;;  gauche.internal#%parameterize is embedded in the compiled code, so do not
;;  change the API across releases.
(define (%parameterize params orig-vals thunk)
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

  (if-let1 filters (gather-filters params)
    (%with-parameterization (map procedure-parameter params)
                            (apply-filters filters orig-vals)
                            thunk)
    (%parameterize/dynwind params orig-vals thunk)))

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
          `(,%parameterize (list ,@param) (list ,@val) (^[] ,@body)))]
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

;; srfi-226
(define-syntax with
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
       [_ (error "Invalid with form:" f)]))))


;; TRANSIENT: To compile 0.9.13 with 0.9.12
(inline-stub
 (declare-stub-type <parameterization> "ScmParameterization*")
 )

(define-cproc call-with-parameterization (parameterization::<parameterization>
                                          thunk)
  (let* ([k (Scm__GetDenvKey SCM_DENV_KEY_PARAMETERIZATION)])
    (Scm_VMPushDynamicEnv k (-> parameterization parameterization))
    (return (Scm_VMApply0 thunk))))
