;;;
;;; libvec.scm - builtin vector procedures
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
 (declcode (.include <gauche/vminsn.h>)))

;;;
;;; Standard Vector
;;;

;; NB: make-vector is defined in libalpha.scm

(select-module scheme)

(define-cproc vector (:rest args) (inliner VEC)
  (return (Scm_ListToVector args 0 -1)))

(define-cproc vector? (obj) ::<boolean> :fast-flonum :constant
  (inliner VECTORP) SCM_VECTORP)

(define-cproc vector-length (vec::<vector>) ::<fixnum> :constant
  (inliner VEC-LEN) SCM_VECTOR_SIZE)

(define-cproc vector-ref (vec::<vector> k::<integer> :optional fallback)
  :constant
  (setter vector-set!)
  (cond [(or (SCM_BIGNUMP k)
             (< (SCM_INT_VALUE k) 0)
             (>= (SCM_INT_VALUE k) (SCM_VECTOR_SIZE vec)))
         (when (SCM_UNBOUNDP fallback)
           (Scm_Error "vector-ref index out of range: %S" k))
         (return fallback)]
        [else (return (SCM_VECTOR_ELEMENT vec (SCM_INT_VALUE k)))]))

(define-cproc vector-set! (vec::<vector> k::<integer> obj) ::<void>
  (if (or (SCM_BIGNUMP k)
          (< (SCM_INT_VALUE k) 0)
          (>= (SCM_INT_VALUE k) (SCM_VECTOR_SIZE vec)))
    (Scm_Error "vector-set! index out of range: %S" k)
    (set! (SCM_VECTOR_ELEMENT vec (SCM_INT_VALUE k)) obj)))

(define-cproc vector->list
  (vec::<vector> :optional (start::<fixnum> 0) (end::<fixnum> -1))
  Scm_VectorToList)

(define-cproc list->vector
  (list::<list> :optional (start::<fixnum> 0) (end::<fixnum> -1))
  Scm_ListToVector)

(define-cproc vector-fill!
  (vec::<vector> fill :optional (start::<fixnum> 0) (end::<fixnum> -1))
  ::<void> Scm_VectorFill)

(define-cproc vector-copy
  (v::<vector> :optional (start::<fixnum> 0) (end::<fixnum> -1) fill)
  Scm_VectorCopy)

(define (vector->string v :optional (start 0) (end -1)) ;;R7RS
  (list->string (vector->list v start end))) ; TODO: can be more efficient
(define (string->vector s :optional (start 0) (end -1)) ;;R7RS
  (list->vector (string->list s start end))) ; TOOD: can be more efficient

;;;
;;; Weak vectors
;;;

(select-module gauche)

(define-cproc make-weak-vector (size::<fixnum>) Scm_MakeWeakVector)

(define-cproc weak-vector-length (wv::<weak-vector>) ::<int>
  (return (-> wv size)))

(define-cproc weak-vector-ref
  (wv::<weak-vector> index::<fixnum> :optional fallback)
  Scm_WeakVectorRef)

(define-cproc weak-vector-set! (wv::<weak-vector> index::<fixnum> val)
  Scm_WeakVectorSet)

;;;
;;; Uniform vectors
;;;

;; (public uniform vector APIs are defined in gauche.uvector, which calls
;; this one internally).
(select-module gauche.internal)

(define-cproc %uvector-ref (v::<uvector> t::<int> k::<fixnum>
                                         :optional fallback)
  :constant
  (unless (== (Scm_UVectorType (SCM_CLASS_OF v)) t)
    (Scm_TypeError "vec" (Scm_UVectorTypeName t) (SCM_OBJ v)))
  (return (Scm_VMUVectorRef v t (SCM_INT_VALUE k) fallback)))

(select-module gauche)
(inline-stub
 (define-enum SCM_UVECTOR_S8)
 (define-enum SCM_UVECTOR_U8)
 (define-enum SCM_UVECTOR_S16)
 (define-enum SCM_UVECTOR_U16)
 (define-enum SCM_UVECTOR_S32)
 (define-enum SCM_UVECTOR_U32)
 (define-enum SCM_UVECTOR_S64)
 (define-enum SCM_UVECTOR_U64)
 (define-enum SCM_UVECTOR_F16)
 (define-enum SCM_UVECTOR_F32)
 (define-enum SCM_UVECTOR_F64)
 )

(define-cproc uvector-length (v::<uvector>) ::<int> :constant
  SCM_UVECTOR_SIZE)
(define-cproc uvector-immutable? (v::<uvector>) ::<boolean>
  SCM_UVECTOR_IMMUTABLE_P)
(define-cproc uvector? (obj) ::<boolean> :constant
  SCM_UVECTORP)


