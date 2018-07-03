;;;
;;; libvec.scm - builtin vector procedures
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

(inline-stub
 (declcode (.include <gauche/vminsn.h>)
           (.include <gauche/priv/vectorP.h>)))

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

(define-cproc vector-copy!
  (t::<vector> tstart::<fixnum> s::<vector>
               :optional (sstart::<fixnum> 0) (send::<fixnum> -1)) ::<void>
  (let* ([tsize::long (SCM_VECTOR_SIZE t)])
    (when (< send 0) (set! send (SCM_VECTOR_SIZE s)))
    (unless (and (<= 0 tstart) (<= tstart tsize))
      (Scm_Error "tstart out of range: %ld" tstart))
    (unless (and (<= 0 sstart) (<= sstart (SCM_VECTOR_SIZE s)))
      (Scm_Error "sstart out of range: %ld" sstart))
    (unless (and (<= 0 send) (<= send (SCM_VECTOR_SIZE s)))
      (Scm_Error "send out of range: %ld" send))
    (unless (<= (+ tstart (- send sstart)) tsize)
      (Scm_Error "source vector overruns the target vector: %20.0S [%ld,%ld]"
                 s sstart send))
    (unless (<= sstart send)
      (Scm_Error "send (%ld) must be greater than or equal to sstart (%ld)"
                 send sstart))
    (if (<= tstart sstart)
      (let* ([i::ScmSmallInt sstart] [j::ScmSmallInt tstart])
        (for [() (and (< i send) (< j tsize)) (begin (post++ i) (post++ j))]
             (set! (SCM_VECTOR_ELEMENT t j) (SCM_VECTOR_ELEMENT s i))))
      (let* ([i::ScmSmallInt send]
             [j::ScmSmallInt (+ tstart (- send sstart))])
        (when (>= j tsize)
          (set! i (- i (- j tsize)))
          (set! j tsize))
        (for [(begin (post-- i) (post-- j))
              (and (>= i sstart) (>= j tstart))
              (begin (post-- i) (post-- j))]
             (set! (SCM_VECTOR_ELEMENT t j) (SCM_VECTOR_ELEMENT s i)))))))

(define-cproc vector-append (:rest vecs)
  (let* ([len::long 0])
    (dolist [v vecs]
      (unless (SCM_VECTORP v)
        (Scm_Error "vector required, but got: %S" v))
      (set! len (+ len (SCM_VECTOR_SIZE v))))
    (let* ([dst (Scm_MakeVector len SCM_UNDEFINED)]
           [j::long 0])
      (dolist [v vecs]
        (let* ([k::long (SCM_VECTOR_SIZE v)])
          (memcpy (+ (SCM_VECTOR_ELEMENTS dst) j) (SCM_VECTOR_ELEMENTS v)
                  (* k (sizeof ScmWord)))
          (set! j (+ j k))))
      (return dst))))

;; TRANSIENT: :optional thing will be expanded unhygienically, inserting
;; reference to 'error'.  If we define these within #<module scheme> it
;; would cause 'error' to be unbound.  Better fix would be to make
;; expansion hygienic, but this is a quick remedy.
(select-module gauche)
(define-in-module scheme (vector->string v :optional (start 0) (end -1)) ;;R7RS
  (list->string (vector->list v start end))) ; TODO: can be more efficient
(define-in-module scheme (string->vector s :optional (start 0) (end -1)) ;;R7RS
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

;; Most public uniform vector APIs are defined in gauche.uvector.
;; We provide a handful basic APIs here for the performance.

(select-module gauche.internal)
;; TRANSIENT: This is inserted by old compiler macro, expecting to be
;; futher expanded into VM instruction UVEC-REF by the compiler.
;; Now we directly expands TAGvector-ref into VM insn so this is no longer
;; called.  We keep this only for the modules precompiled by older versions.
;; Will remove on 1.0.
(define-cproc %uvector-ref (v::<uvector> t::<int> k::<fixnum>
                                         :optional fallback)
  :constant
  (unless (== (Scm_UVectorType (SCM_CLASS_OF v)) t)
    (Scm_TypeError "vec" (Scm_UVectorTypeName t) (SCM_OBJ v)))
  (return (Scm_VMUVectorRef v t k fallback)))

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

(define-cproc s8vector-set! (v::<s8vector> i::<fixnum> val :optional clamp)
  (return (Scm_UVectorSet v SCM_UVECTOR_S8 i val (Scm_ClampMode clamp))))
(define-cproc s8vector-ref (v::<s8vector> i::<fixnum> :optional fallback)
  :fast-flonum
  (setter s8vector-set!)
  (return (Scm_VMUVectorRef v SCM_UVECTOR_S8 i fallback)))

(define-cproc u8vector-set! (v::<u8vector> i::<fixnum> val :optional clamp)
  (return (Scm_UVectorSet v SCM_UVECTOR_U8 i val (Scm_ClampMode clamp))))
(define-cproc u8vector-ref (v::<u8vector> i::<fixnum> :optional fallback)
  :fast-flonum
  (setter u8vector-set!)
  (return (Scm_VMUVectorRef v SCM_UVECTOR_U8 i fallback)))

(define-cproc s16vector-set! (v::<s16vector> i::<fixnum> val :optional clamp)
  (return (Scm_UVectorSet v SCM_UVECTOR_S16 i val (Scm_ClampMode clamp))))
(define-cproc s16vector-ref (v::<s16vector> i::<fixnum> :optional fallback)
  :fast-flonum
  (setter s16vector-set!)
  (return (Scm_VMUVectorRef v SCM_UVECTOR_S16 i fallback)))

(define-cproc u16vector-set! (v::<u16vector> i::<fixnum> val :optional clamp)
  (return (Scm_UVectorSet v SCM_UVECTOR_U16 i val (Scm_ClampMode clamp))))
(define-cproc u16vector-ref (v::<u16vector> i::<fixnum> :optional fallback)
  :fast-flonum
  (setter u16vector-set!)
  (return (Scm_VMUVectorRef v SCM_UVECTOR_U16 i fallback)))

(define-cproc s32vector-set! (v::<s32vector> i::<fixnum> val :optional clamp)
  (return (Scm_UVectorSet v SCM_UVECTOR_S32 i val (Scm_ClampMode clamp))))
(define-cproc s32vector-ref (v::<s32vector> i::<fixnum> :optional fallback)
  :fast-flonum
  (setter s32vector-set!)
  (return (Scm_VMUVectorRef v SCM_UVECTOR_S32 i fallback)))

(define-cproc u32vector-set! (v::<u32vector> i::<fixnum> val :optional clamp)
  (return (Scm_UVectorSet v SCM_UVECTOR_U32 i val (Scm_ClampMode clamp))))
(define-cproc u32vector-ref (v::<u32vector> i::<fixnum> :optional fallback)
  :fast-flonum
  (setter u32vector-set!)
  (return (Scm_VMUVectorRef v SCM_UVECTOR_U32 i fallback)))

(define-cproc s64vector-set! (v::<s64vector> i::<fixnum> val :optional clamp)
  (return (Scm_UVectorSet v SCM_UVECTOR_S64 i val (Scm_ClampMode clamp))))
(define-cproc s64vector-ref (v::<s64vector> i::<fixnum> :optional fallback)
  :fast-flonum
  (setter s64vector-set!)
  (return (Scm_VMUVectorRef v SCM_UVECTOR_S64 i fallback)))

(define-cproc u64vector-set! (v::<u64vector> i::<fixnum> val :optional clamp)
  (return (Scm_UVectorSet v SCM_UVECTOR_U64 i val (Scm_ClampMode clamp))))
(define-cproc u64vector-ref (v::<u64vector> i::<fixnum> :optional fallback)
  :fast-flonum
  (setter u64vector-set!)
  (return (Scm_VMUVectorRef v SCM_UVECTOR_U64 i fallback)))

(define-cproc f16vector-set! (v::<f16vector> i::<fixnum> val :optional clamp)
  (return (Scm_UVectorSet v SCM_UVECTOR_F16 i val (Scm_ClampMode clamp))))
(define-cproc f16vector-ref (v::<f16vector> i::<fixnum> :optional fallback)
  :fast-flonum
  (setter f16vector-set!)
  (return (Scm_VMUVectorRef v SCM_UVECTOR_F16 i fallback)))

(define-cproc f32vector-set! (v::<f32vector> i::<fixnum> val :optional clamp)
  (return (Scm_UVectorSet v SCM_UVECTOR_F32 i val (Scm_ClampMode clamp))))
(define-cproc f32vector-ref (v::<f32vector> i::<fixnum> :optional fallback)
  :fast-flonum
  (setter f32vector-set!)
  (return (Scm_VMUVectorRef v SCM_UVECTOR_F32 i fallback)))

(define-cproc f64vector-set! (v::<f64vector> i::<fixnum> val :optional clamp)
  (return (Scm_UVectorSet v SCM_UVECTOR_F64 i val (Scm_ClampMode clamp))))
(define-cproc f64vector-ref (v::<f64vector> i::<fixnum> :optional fallback)
  :fast-flonum
  (setter f64vector-set!)
  (return (Scm_VMUVectorRef v SCM_UVECTOR_F64 i fallback)))

(define-cproc uvector-set! (v::<uvector> i::<fixnum> val :optional clamp)
  ::<void> :fast-flonum 
  (Scm_UVectorSet v (Scm_UVectorType (Scm_ClassOf (SCM_OBJ v))) i val
                  (Scm_ClampMode clamp)))
(define-cproc uvector-ref (v::<uvector> i::<fixnum> :optional fallback)
  :fast-flonum
  (setter uvector-set!)
  (return (Scm_VMUVectorRef v (Scm_UVectorType (Scm_ClassOf (SCM_OBJ v)))
                            i fallback)))
(define-cproc uvector-length (v::<uvector>) ::<ulong> :constant
  SCM_UVECTOR_SIZE)
(define-cproc uvector-immutable? (v::<uvector>) ::<boolean>
  SCM_UVECTOR_IMMUTABLE_P)
(define-cproc uvector? (obj) ::<boolean> :constant
  SCM_UVECTORP)

;;;
;;; Flat vector API (interact with underlying C array)
;;;

(inline-stub
 (define-cise-stmt %binary-search
   [(_ elttype)
    `(let* ([esize::u_int  (+ skip 1)]
            [nume::size_t (/ len esize)]
            [k::size_t (/ nume 2)]
            [hi::size_t nume]
            [lo::size_t 0])
       (while (< lo hi)
         (let* ([v:: ,elttype (aref vec (* k esize))])
           (cond [(== v key) (return (* k esize))]
                 [(< v key)
                  (set! lo k) (set! k (+ lo (/ (- hi lo) 2)))
                  (when (== lo k) (break))]
                 [else
                  (set! hi k) (set! k (+ lo (/ (- hi lo) 2)))])))
       (when floor
         (if (== lo hi)
           (set! (* floor) (cast (size_t) -1))
           (set! (* floor) (* lo esize))))
       (when ceil
         (if (== hi nume)
           (set! (* ceil) (cast (size_t) -1))
           (set! (* ceil) (* hi esize))))
       (return (cast (size_t) -1)))])
 
 (define-cfn Scm_BinarySearchS8 (vec::(const int8_t*)
                                 len::size_t
                                 key::int8_t
                                 skip::u_int
                                 floor::size_t*
                                 ceil::size_t*)
   ::size_t (%binary-search int8_t))

 (define-cfn Scm_BinarySearchU8 (vec::(const uint8_t*)
                                 len::size_t
                                 key::uint8_t
                                 skip::u_int
                                 floor::size_t*
                                 ceil::size_t*)
   ::size_t (%binary-search uint8_t))

 (define-cfn Scm_BinarySearchS16 (vec::(const int16_t*)
                                  len::size_t
                                  key::int16_t
                                  skip::u_int
                                  floor::size_t*
                                  ceil::size_t*)
   ::size_t (%binary-search int16_t))

 (define-cfn Scm_BinarySearchU16 (vec::(const uint16_t*)
                                  len::size_t
                                  key::uint16_t
                                  skip::u_int
                                  floor::size_t*
                                  ceil::size_t*)
   ::size_t (%binary-search uint16_t))

 (define-cfn Scm_BinarySearchS32 (vec::(const int32_t*)
                                  len::size_t
                                  key::int32_t
                                  skip::u_int
                                  floor::size_t*
                                  ceil::size_t*)
   ::size_t (%binary-search int32_t))

 (define-cfn Scm_BinarySearchU32 (vec::(const uint32_t*)
                                  len::size_t
                                  key::uint32_t
                                  skip::u_int
                                  floor::size_t*
                                  ceil::size_t*)
   ::size_t (%binary-search uint32_t))

 (define-cfn Scm_BinarySearchS64 (vec::(const ScmInt64*)
                                  len::size_t
                                  key::ScmInt64
                                  skip::u_int
                                  floor::size_t*
                                  ceil::size_t*)
   ::size_t (%binary-search ScmInt64))

 (define-cfn Scm_BinarySearchU64 (vec::(const ScmUInt64*)
                                  len::size_t
                                  key::ScmUInt64
                                  skip::u_int
                                  floor::size_t*
                                  ceil::size_t*)
   ::size_t (%binary-search ScmUInt64))
 
 (define-cfn Scm_BinarySearchF16 (vec::(const ScmHalfFloat*)
                                  len::size_t
                                  key::ScmHalfFloat
                                  skip::u_int
                                  floor::size_t*
                                  ceil::size_t*)
   ::size_t (%binary-search ScmHalfFloat))

 (define-cfn Scm_BinarySearchF32 (vec::(const float*)
                                  len::size_t
                                  key::float
                                  skip::u_int
                                  floor::size_t*
                                  ceil::size_t*)
   ::size_t (%binary-search float))

 (define-cfn Scm_BinarySearchF64 (vec::(const double*)
                                  len::size_t
                                  key::double
                                  skip::u_int
                                  floor::size_t*
                                  ceil::size_t*)
   ::size_t (%binary-search double))
 )
