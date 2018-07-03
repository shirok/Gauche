;;;
;;; gauche.uvector - uniform vectors
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

;; This module defines the superset of SRFI-4, homogeneous numeric vector
;; types.   Most of basic operations are defined in the DSO module libuvector.
;; Besides defining functions, the DSO sets up a reader hook to enable
;; extended syntax such as #s8(1 2 3).
;; This module also defines methods for collection and sequence frameworks.

;; NB: The following identifiers are defined in core now:
;;   uvector? ${TAG}vector-ref  ${TAG}vector-set!  uvector-ref  uvector-set!
;;   uvector-length    uvector-immutable?

(define-module gauche.uvector
  (use gauche.collection)
  (use gauche.sequence)
  (use data.queue)
  (export f16vector f16vector->list f16vector->vector
          f16vector-add f16vector-add! f16vector-append
          f16vector-clamp f16vector-clamp! f16vector-compare
          f16vector-copy f16vector-copy! f16vector-div f16vector-div!
          f16vector-dot f16vector-fill! f16vector-length
          f16vector-mul f16vector-mul!
          f16vector-multi-copy! f16vector-range-check
          f16vector-ref f16vector-set! f16vector-sub f16vector-sub!
          f16vector-swap-bytes f16vector-swap-bytes!
          f16vector=? f16vector?

          f32vector f32vector->list f32vector->vector
          f32vector-add f32vector-add! f32vector-append
          f32vector-clamp f32vector-clamp! f32vector-compare
          f32vector-copy f32vector-copy! f32vector-div f32vector-div!
          f32vector-dot f32vector-fill! f32vector-length
          f32vector-mul f32vector-mul!
          f32vector-multi-copy! f32vector-range-check
          f32vector-ref f32vector-set! f32vector-sub f32vector-sub!
          f32vector-swap-bytes f32vector-swap-bytes!
          f32vector=? f32vector?

          f64vector f64vector->list f64vector->vector f64vector-add
          f64vector-add! f64vector-append f64vector-clamp f64vector-clamp!
          f64vector-compare f64vector-copy f64vector-copy! f64vector-div
          f64vector-div! f64vector-dot f64vector-fill! f64vector-length
          f64vector-mul f64vector-mul! f64vector-multi-copy!
          f64vector-range-check f64vector-ref f64vector-set! f64vector-sub
          f64vector-sub! f64vector-swap-bytes f64vector-swap-bytes! f64vector=?
          f64vector?

          get-output-uvector

          list->f16vector list->f32vector
          list->f64vector list->s16vector list->s32vector list->s64vector
          list->s8vector list->u16vector list->u32vector list->u64vector
          list->u8vector

          make-f16vector make-f32vector make-f64vector
          make-s16vector make-s32vector make-s64vector make-s8vector
          make-u16vector make-u32vector make-u64vector make-u8vector
          make-uvector

          open-output-uvector port->uvector read-block!
          read-uvector read-uvector! referencer

          s16vector s16vector->list
          s16vector->vector s16vector-add s16vector-add! s16vector-and
          s16vector-and! s16vector-append s16vector-clamp s16vector-clamp!
          s16vector-compare s16vector-copy s16vector-copy! s16vector-dot
          s16vector-fill! s16vector-ior s16vector-ior! s16vector-length
          s16vector-mul s16vector-mul! s16vector-multi-copy!
          s16vector-range-check s16vector-ref s16vector-set! s16vector-sub
          s16vector-sub! s16vector-swap-bytes s16vector-swap-bytes!
          s16vector-xor s16vector-xor! s16vector=? s16vector?

          s32vector s32vector->list s32vector->string
          s32vector->vector s32vector-add s32vector-add! s32vector-and
          s32vector-and! s32vector-append s32vector-clamp s32vector-clamp!
          s32vector-compare s32vector-copy s32vector-copy! s32vector-dot
          s32vector-fill! s32vector-ior s32vector-ior! s32vector-length
          s32vector-mul s32vector-mul! s32vector-multi-copy!
          s32vector-range-check s32vector-ref s32vector-set! s32vector-sub
          s32vector-sub! s32vector-swap-bytes s32vector-swap-bytes!
          s32vector-xor s32vector-xor! s32vector=? s32vector?

          s64vector s64vector->list
          s64vector->vector s64vector-add s64vector-add! s64vector-and
          s64vector-and! s64vector-append s64vector-clamp s64vector-clamp!
          s64vector-compare s64vector-copy s64vector-copy! s64vector-dot
          s64vector-fill! s64vector-ior s64vector-ior! s64vector-length
          s64vector-mul s64vector-mul! s64vector-multi-copy!
          s64vector-range-check s64vector-ref s64vector-set! s64vector-sub
          s64vector-sub! s64vector-swap-bytes s64vector-swap-bytes!
          s64vector-xor s64vector-xor! s64vector=? s64vector?

          s8vector s8vector->list s8vector->string s8vector->vector
          s8vector-add s8vector-add! s8vector-and s8vector-and! s8vector-append
          s8vector-clamp s8vector-clamp! s8vector-compare s8vector-copy
          s8vector-copy! s8vector-dot s8vector-fill! s8vector-ior s8vector-ior!
          s8vector-length s8vector-mul s8vector-mul! s8vector-multi-copy!
          s8vector-range-check s8vector-ref s8vector-set! s8vector-sub
          s8vector-sub! s8vector-xor s8vector-xor! s8vector=? s8vector?

          string->s32vector string->s32vector! string->s8vector
          string->s8vector! string->u32vector string->u32vector!
          string->u8vector string->u8vector!

          u16vector u16vector->list
          u16vector->vector u16vector-add u16vector-add! u16vector-and
          u16vector-and! u16vector-append u16vector-clamp u16vector-clamp!
          u16vector-compare u16vector-copy u16vector-copy! u16vector-dot
          u16vector-fill! u16vector-ior u16vector-ior! u16vector-length
          u16vector-mul u16vector-mul! u16vector-multi-copy!
          u16vector-range-check u16vector-ref u16vector-set! u16vector-sub
          u16vector-sub! u16vector-swap-bytes u16vector-swap-bytes!
          u16vector-xor u16vector-xor! u16vector=? u16vector?

          u32vector u32vector->list u32vector->string
          u32vector->vector u32vector-add u32vector-add! u32vector-and
          u32vector-and! u32vector-append u32vector-clamp u32vector-clamp!
          u32vector-compare u32vector-copy u32vector-copy! u32vector-dot
          u32vector-fill! u32vector-ior u32vector-ior! u32vector-length
          u32vector-mul u32vector-mul! u32vector-multi-copy!
          u32vector-range-check u32vector-ref u32vector-set! u32vector-sub
          u32vector-sub! u32vector-swap-bytes u32vector-swap-bytes!
          u32vector-xor u32vector-xor! u32vector=? u32vector?

          u64vector u64vector->list u64vector->vector u64vector-add
          u64vector-add! u64vector-and u64vector-and! u64vector-append
          u64vector-clamp u64vector-clamp! u64vector-compare u64vector-copy
          u64vector-copy! u64vector-dot u64vector-fill! u64vector-ior
          u64vector-ior! u64vector-length u64vector-mul u64vector-mul!
          u64vector-multi-copy! u64vector-range-check u64vector-ref
          u64vector-set! u64vector-sub u64vector-sub! u64vector-swap-bytes
          u64vector-swap-bytes! u64vector-xor u64vector-xor! u64vector=?
          u64vector?

          u8vector u8vector->list u8vector->string u8vector->vector
          u8vector-add u8vector-add! u8vector-and u8vector-and! u8vector-append
          u8vector-clamp u8vector-clamp! u8vector-compare u8vector-copy
          u8vector-copy! u8vector-dot u8vector-fill! u8vector-ior u8vector-ior!
          u8vector-length u8vector-mul u8vector-mul! u8vector-multi-copy!
          u8vector-range-check u8vector-ref u8vector-set! u8vector-sub
          u8vector-sub! u8vector-xor u8vector-xor! u8vector=? u8vector?

          uvector-alias uvector-binary-search uvector-class-element-size
          uvector-copy uvector-copy! uvector-ref uvector-set! uvector-size
          uvector-swap-bytes uvector-swap-bytes!

          vector->f16vector vector->f32vector vector->f64vector
          vector->s16vector vector->s32vector vector->s64vector
          vector->s8vector vector->u16vector vector->u32vector
          vector->u64vector vector->u8vector

          write-block write-uvector))
(select-module gauche.uvector)

;; gauche.vport is used by port->uvector.  Technically it's on top
;; of uniform vector ports provided by gauche.vport, but logically
;; it is expected to belong gauche.uvector.
(autoload gauche.vport open-output-uvector get-output-uvector)

(inline-stub
 "#include <math.h>"
 "#define EXTUVECTOR_EXPORTS"
 "#include \"gauche/uvector.h\""
 "#include \"gauche/priv/vectorP.h\""
 "#include \"uvectorP.h\""
 )

;; uvlib.scm is generated by uvlib.scm.tmpl
(inline-stub
 (include "./uvlib.scm")
 )


;;;
;;; Generic procedures
;;;

;; uvector-alias
(inline-stub
 (define-cproc uvector-alias
   (klass::<class> v::<uvector> :optional (start::<int> 0) (end::<int> -1))
   Scm_UVectorAlias)
 )

;; byte swapping
(inline-stub
 (define-cise-stmt swap-bytes-common
   [(_ c-fn v type)
    `(let* ([opt::int SWAPB_STD])
       (cond [(== ,type NULL)]
             [(SCM_EQ (SCM_OBJ ,type) 'le:arm-le) (= opt SWAPB_ARM_LE)]
             [(SCM_EQ (SCM_OBJ ,type) 'be:arm-le) (= opt SWAPB_ARM_BE)]
             [else (Scm_TypeError "type" "#f or a symbol le:arm-le or be:arm-le"
                                  (SCM_OBJ ,type))])
       (,c-fn ,v opt))])

 (define-cproc uvector-swap-bytes (v::<uvector> :optional (type::<symbol>? #f)) ::<void>
   (swap-bytes-common Scm_UVectorSwapBytes v type))

 (define-cproc uvector-swap-bytes! (v::<uvector> :optional (type::<symbol>? #f)) ::<void>
   (swap-bytes-common Scm_UVectorSwapBytesX v type))
 )

;; uvector-size
(inline-stub
 (define-cproc uvector-size (v::<uvector>
                             :optional (start::<int> 0) (end::<int> -1))
   ::<int>
   (let* ([len::int (SCM_UVECTOR_SIZE v)])
     (SCM_CHECK_START_END start end len)
     (return (* (- end start)
                (Scm_UVectorElementSize (Scm_ClassOf (SCM_OBJ v)))))))

 (define-cproc uvector-class-element-size (c::<class>) ::<fixnum>
   (let* ([r::int (Scm_UVectorElementSize c)])
     (when (< r 0)
       (Scm_Error "A class of uvector is required, but got: %S" c))
     (return r)))
 )

;; allocation by class
(inline-stub
 (define-cproc make-uvector (klass::<class> size::<fixnum>
                             :optional (init 0))
   (unless (>= size 0) (Scm_Error "invalid uvector size: %d" size))
   (let* ([v (Scm_MakeUVector klass size NULL)])
     (case (Scm_UVectorType klass)
       [(SCM_UVECTOR_S8)
        (Scm_S8VectorFill (SCM_S8VECTOR v) 
                          (Scm_GetInteger8Clamp init SCM_CLAMP_ERROR NULL)
                          0 -1)]
       [(SCM_UVECTOR_U8)
        (Scm_U8VectorFill (SCM_U8VECTOR v) 
                          (Scm_GetIntegerU8Clamp init SCM_CLAMP_ERROR NULL)
                          0 -1)]
       [(SCM_UVECTOR_S16)
        (Scm_S16VectorFill (SCM_S16VECTOR v) 
                           (Scm_GetInteger16Clamp init SCM_CLAMP_ERROR NULL)
                           0 -1)]
       [(SCM_UVECTOR_U16)
        (Scm_U16VectorFill (SCM_U16VECTOR v) 
                           (Scm_GetIntegerU16Clamp init SCM_CLAMP_ERROR NULL)
                           0 -1)]
       [(SCM_UVECTOR_S32)
        (Scm_S32VectorFill (SCM_S32VECTOR v) 
                           (Scm_GetInteger32Clamp init SCM_CLAMP_ERROR NULL)
                           0 -1)]
       [(SCM_UVECTOR_U32)
        (Scm_U32VectorFill (SCM_U32VECTOR v) 
                           (Scm_GetIntegerU32Clamp init SCM_CLAMP_ERROR NULL)
                           0 -1)]
       [(SCM_UVECTOR_S64)
        (Scm_S64VectorFill (SCM_S64VECTOR v) 
                           (Scm_GetInteger64Clamp init SCM_CLAMP_ERROR NULL)
                           0 -1)]
       [(SCM_UVECTOR_U64)
        (Scm_U64VectorFill (SCM_U64VECTOR v) 
                           (Scm_GetIntegerU64Clamp init SCM_CLAMP_ERROR NULL)
                           0 -1)]
       [(SCM_UVECTOR_F16)
        (Scm_F16VectorFill (SCM_F16VECTOR v)
                           (Scm_DoubleToHalf (Scm_GetDouble init))
                           0 -1)]
       [(SCM_UVECTOR_F32)
        (Scm_F32VectorFill (SCM_F32VECTOR v)
                           (cast float (Scm_GetDouble init))
                           0 -1)]
       [(SCM_UVECTOR_F64)
        (Scm_F64VectorFill (SCM_F64VECTOR v)
                           (Scm_GetDouble init)
                           0 -1)]
       [else SCM_UNDEFINED]) ; can't happen
     (return v))))

;; generic copy
(inline-stub
 (define-cproc uvector-copy (v::<uvector>
                             :optional (start::<fixnum> 0)
                                       (end::<fixnum> -1))
   (let* ([len::int (SCM_UVECTOR_SIZE v)]
          [klass::ScmClass* (Scm_ClassOf (SCM_OBJ v))]
          [eltsize::int (Scm_UVectorElementSize klass)]
          [src::(const char *) (cast (const char *) (SCM_UVECTOR_ELEMENTS v))])
     (SCM_CHECK_START_END start end len)
     (let* ([newsize::int (* (- end start) eltsize)]
            [dst::char* (SCM_NEW_ATOMIC_ARRAY (char) newsize)])
       (memcpy dst (+ src (* start eltsize)) newsize)
       (return (Scm_MakeUVector klass (- end start) dst)))))
 )

;; search
;; rounding can be #f, 'floor or 'ceiling  (srfi-114 also uses symbols
;; for rounding.  we don't use 'round and 'truncate, though, for
;; it doesn't make much sense.)
(inline-stub
 ;; aux fn to deal with optional fixnum arg.  we should make genstub handle
 ;; this in future.
 (define-cfn get-fixnum-arg (arg fallback::ScmSmallInt name::(const char*))
   ::ScmSmallInt :static
   (cond [(SCM_INTP arg) (return (SCM_INT_VALUE arg))]
         [(SCM_FALSEP arg) (return fallback)]
         [else (Scm_Error "%s expects fixnum or #f, but got: %S" name arg)
               (return 0)])) ; dummy
 
 (define-cproc uvector-binary-search (v::<uvector> key::<number>
                                                   :optional
                                                   (start #f)
                                                   (end   #f)
                                                   (skip  #f)
                                                   (rounding #f))
   (let* ([len::size_t (SCM_UVECTOR_SIZE v)]
          [s::ScmSmallInt (get-fixnum-arg start 0 "start")]
          [e::ScmSmallInt (get-fixnum-arg end -1 "end")]
          [p::ScmSmallInt (get-fixnum-arg skip 0 "skip")])
     (SCM_CHECK_START_END s e len)
     (unless (== (% (- e s) (+ p 1)) 0)
       (Scm_Error "uvector size (%d) isn't multiple of record size (%d)"
                  (- e s) (+ p 1)))
     (let* ([r::size_t (cast (size_t) -1)]
            [lb::size_t]
            [ub::size_t])
       (case (Scm_UVectorType (Scm_ClassOf (SCM_OBJ v)))
         [(SCM_UVECTOR_S8)
          (let* ([k::int8_t
                  (Scm_GetInteger8Clamp key SCM_CLAMP_ERROR NULL)])
            (set! r (Scm_BinarySearchS8 (+ (SCM_S8VECTOR_ELEMENTS v) s)
                                        (- e s) k p (& lb) (& ub))))]
         [(SCM_UVECTOR_U8)
          (let* ([k::uint8_t
                  (Scm_GetIntegerU8Clamp key SCM_CLAMP_ERROR NULL)])
            (set! r (Scm_BinarySearchU8 (+ (SCM_U8VECTOR_ELEMENTS v) s)
                                        (- e s) k p (& lb) (& ub))))]
         [(SCM_UVECTOR_S16)
          (let* ([k::int16_t
                  (Scm_GetInteger16Clamp key SCM_CLAMP_ERROR NULL)])
            (set! r (Scm_BinarySearchS16 (+ (SCM_S16VECTOR_ELEMENTS v) s)
                                         (- e s) k p (& lb) (& ub))))]
         [(SCM_UVECTOR_U16)
          (let* ([k::uint16_t
                  (Scm_GetIntegerU16Clamp key SCM_CLAMP_ERROR NULL)])
            (set! r (Scm_BinarySearchU16 (+ (SCM_U16VECTOR_ELEMENTS v) s)
                                         (- e s) k p (& lb) (& ub))))]
         [(SCM_UVECTOR_S32)
          (let* ([k::int32_t
                  (Scm_GetInteger32Clamp key SCM_CLAMP_ERROR NULL)])
            (set! r (Scm_BinarySearchS32 (+ (SCM_S32VECTOR_ELEMENTS v) s)
                                         (- e s) k p (& lb) (& ub))))]
         [(SCM_UVECTOR_U32)
          (let* ([k::uint32_t
                  (Scm_GetIntegerU32Clamp key SCM_CLAMP_ERROR NULL)])
            (set! r (Scm_BinarySearchU32 (+ (SCM_U32VECTOR_ELEMENTS v) s)
                                         (- e s) k p (& lb) (& ub))))]
         [(SCM_UVECTOR_S64)
          (let* ([k::ScmInt64
                  (Scm_GetInteger64Clamp key SCM_CLAMP_ERROR NULL)])
            (set! r (Scm_BinarySearchS64 (+ (SCM_S64VECTOR_ELEMENTS v) s)
                                         (- e s) k p (& lb) (& ub))))]
         [(SCM_UVECTOR_U64)
          (let* ([k::ScmUInt64
                  (Scm_GetIntegerU64Clamp key SCM_CLAMP_ERROR NULL)])
            (set! r (Scm_BinarySearchU64 (+ (SCM_U64VECTOR_ELEMENTS v) s)
                                         (- e s) k p (& lb) (& ub))))]
         [(SCM_UVECTOR_F16)
          (let* ([k::ScmHalfFloat (Scm_DoubleToHalf (Scm_GetDouble key))])
            (set! r (Scm_BinarySearchF16 (+ (SCM_F16VECTOR_ELEMENTS v) s)
                                         (- e s) k p (& lb) (& ub))))]         
         [(SCM_UVECTOR_F32)
          (let* ([k::float (Scm_GetDouble key)])
            (set! r (Scm_BinarySearchF32 (+ (SCM_F32VECTOR_ELEMENTS v) s)
                                         (- e s) k p (& lb) (& ub))))]         
         [(SCM_UVECTOR_F64)
          (let* ([k::double (Scm_GetDouble key)])
            (set! r (Scm_BinarySearchF64 (+ (SCM_F64VECTOR_ELEMENTS v) s)
                                         (- e s) k p (& lb) (& ub))))]
         [else (SCM_ASSERT "Invalid uvector type")]
         )
       (when (== r (cast (size_t) -1))
         (cond
          [(SCM_EQ rounding 'floor)   (set! r lb)]
          [(SCM_EQ rounding 'ceiling) (set! r ub)]
          [(not (SCM_FALSEP rounding))
           (Scm_Error "Rounding argument must be either #f, floor \
                       or ceiling, but got: %S" rounding)]))
       (if (== r (cast (size_t) -1))
         (return SCM_FALSE)
         (return (Scm_MakeIntegerU (+ r s)))))))
 )

;; block i/o
(inline-stub
 (define-cproc read-uvector! (v::<uvector>
                              :optional (port::<input-port> (current-input-port))
                                        (start::<fixnum> 0)
                                        (end::<fixnum> -1)
                                        (endian::<symbol>? #f))
   Scm_ReadBlockX)

 (define-cproc read-uvector (klass::<class> size::<fixnum> 
                             :optional (port::<input-port> (current-input-port))
                                       (endian::<symbol>? #f))
   (unless (Scm_SubtypeP klass SCM_CLASS_UVECTOR)
     (Scm_TypeError "class" "uniform vector class" (SCM_OBJ klass)))
   (let* ([v::ScmUVector* (cast ScmUVector* (Scm_MakeUVector klass size NULL))]
          [r (Scm_ReadBlockX v port 0 size endian)])
     (if (SCM_EOFP r)
       (return r)
       (begin
         (SCM_ASSERT (SCM_INTP r))
         (let* ([n::long (SCM_INT_VALUE r)])
           (SCM_ASSERT (and (<= n size) (<= 0 n)))
           ;; NB: If read size is a lot shorter than requested size, we may
           ;; want to copy it instead of just keeping the rest of vector
           ;; unused.
           (if (< n size)
             (return (Scm_UVectorAlias klass v 0 n))
             (return (SCM_OBJ v))))))))

 (define-cproc write-uvector (v::<uvector>
                              :optional (port::<output-port> (current-output-port))
                                        (start::<fixnum> 0)
                                        (end::<fixnum> -1)
                                        (endian::<symbol>? #f))
   Scm_WriteBlock)
 )

;; copy
(inline-stub
 (define-cproc uvector-copy! (dest::<uvector> dstart::<int> src::<uvector>
                              :optional (sstart::<int> 0)
                                        (send::<int> -1))
   ::<void>
   (SCM_UVECTOR_CHECK_MUTABLE dest)
   (SCM_CHECK_START_END sstart send (SCM_UVECTOR_SIZE src))
   (let* ([deltsize::int (Scm_UVectorElementSize (Scm_ClassOf (SCM_OBJ dest)))]
          [doff::int (* dstart deltsize)]
          [seltsize::int (Scm_UVectorElementSize (Scm_ClassOf (SCM_OBJ src)))]
          [soff::int (* sstart seltsize)]
          [size::int (- (* send seltsize) soff)])
     (memmove (+ (cast char* (SCM_UVECTOR_ELEMENTS dest)) doff)
              (+ (cast (const char*) (SCM_UVECTOR_ELEMENTS src)) soff)
              size)))
 )

;; String operations
(inline-stub
 ;; A common operation to extract range of char* from the input string S.
 ;; START and END may be adjusted.
 ;; SP and EP are const char* variable that gets start and end pointers.
 (define-cise-stmt with-input-string-pointers
   [(_ (s start end sp ep) . body)
    (let ([sb (gensym)] [size (gensym)] [len (gensym)] [ss (gensym)])
      `(let* ([,sb :: (const ScmStringBody*) (SCM_STRING_BODY ,s)]
              [,size :: u_int (SCM_STRING_BODY_SIZE ,sb)]
              [,len :: u_int (SCM_STRING_BODY_LENGTH ,sb)]
              [,ss :: (const char*) (SCM_STRING_BODY_START ,sb)])
         (SCM_CHECK_START_END ,start ,end (cast int ,len))
         (let* ([,sp :: (const char*)
                     (?: (== ,start 0)
                         ,ss
                         (Scm_StringBodyPosition ,sb ,start))]
                [,ep :: (const char*)
                     (?: (== ,end ,len)
                         (+ ,ss ,size)
                         (Scm_StringBodyPosition ,sb ,end))])
           ,@body)))])
 
 (define-cfn string->bytevector
   (klass::ScmClass* s::ScmString* start::int end::int immutable::int) :static
   (with-input-string-pointers (s start end sp ep)
     (let* ([buf::char* NULL])
       (if immutable
         (set! buf (cast char* sp))  ; Eek! drop const qualifier
         (begin
           (set! buf (SCM_NEW_ATOMIC2 (char*) (- ep sp)))
           (memcpy buf sp (- ep sp))))
       (return (Scm_MakeUVectorFull klass (cast int (- ep sp)) buf
                                    immutable NULL)))))

 (define-cproc string->s8vector
   (s::<string>
    :optional (start::<fixnum> 0) (end::<fixnum> -1) (immutable?::<boolean> #f))
   (return (string->bytevector SCM_CLASS_S8VECTOR s start end immutable?)))

 (define-cproc string->u8vector
   (s::<string>
    :optional (start::<fixnum> 0) (end::<fixnum> -1) (immutable?::<boolean> #f))
   (return (string->bytevector SCM_CLASS_U8VECTOR s start end immutable?)))

 (define-cfn string->bytevector!
   (v::ScmUVector* tstart::int s::ScmString* start::int end::int) :static
   (let* ([tlen::int (SCM_UVECTOR_SIZE v)])
     (when (and (>= tstart 0) (< tstart tlen))
       (SCM_UVECTOR_CHECK_MUTABLE v)
       (with-input-string-pointers (s start end sp ep)
         (let* ([buf::(char*) (+ (cast char* (SCM_UVECTOR_ELEMENTS v)) tstart)])
           (if (> (- tlen tstart) (- ep sp))
             (memcpy buf sp (- ep sp))
             (memcpy buf sp (- tlen tstart))))))
     (return (SCM_OBJ v))))

 (define-cproc string->s8vector! (v::<s8vector>
                                  tstart::<int>
                                  s::<string>
                                  :optional (start::<fixnum> 0)
                                  (end::<fixnum> -1))
   (return (string->bytevector! (SCM_UVECTOR v) tstart s start end)))

 (define-cproc string->u8vector! (v::<u8vector>
                                  tstart::<int>
                                  s::<string>
                                  :optional (start::<fixnum> 0)
                                  (end::<fixnum> -1))
   (return (string->bytevector! (SCM_UVECTOR v) tstart s start end)))

 (define-cfn bytevector->string (v::ScmUVector* start::int end::int term)
   :static
   (let* ([len::int (SCM_UVECTOR_SIZE v)])
     ;; We automatically avoid copying the string contents when the
     ;; following conditions are met:
     ;; * The source vector is immutable
     ;; * The owner of source vector is NULL (If there's an owner such as
     ;;   mmap handle, it isn't desirable if a string points to the memory
     ;;   without keeping ownership info.)
     ;; * The resulting string is not a small fraction of a large vector.
     ;;   If so, we may waste space by retaining large chunk of memory
     ;;   most of which won't be ever used.  Here we use some heuristics:
     ;;   - If the source vector is not small (>= 256)
     ;;   - and the string covers only a fraction (1/5) or less,
     ;;   - then we copy the content.
     ;; NB: We may add a flag that force the content to be shared, for
     ;; the programs that really want to avoid allocation.
     (SCM_CHECK_START_END start end len)
     (let* ([flags::int (?: (and (SCM_UVECTOR_IMMUTABLE_P v)
                                 (== (-> v owner) NULL)
                                 (not (and (>= len 256)
                                           (<= (- end start) (/ len 5)))))
                            0
                            SCM_STRING_COPYING)])
       (when (SCM_INTP term)
         (let* ([terminator::u_int (logand #xff (SCM_INT_VALUE term))]
                [i::int])
           (for [(set! i start) (< i end) (post++ i)]
             (when (== terminator
                       (aref (cast u_char* (SCM_UVECTOR_ELEMENTS v)) i))
               (set! end i)
               (break)))))
       (return (Scm_MakeString (+ (cast char* (SCM_UVECTOR_ELEMENTS v)) start)
                               (- end start) -1 flags)))))

 (define-cproc s8vector->string (v::<s8vector>
                                 :optional (start::<fixnum> 0)
                                           (end::<fixnum> -1)
                                           (terminator #f))
   (return (bytevector->string (SCM_UVECTOR v) start end terminator)))

 (define-cproc u8vector->string (v::<u8vector>
                                 :optional (start::<fixnum> 0)
                                           (end::<fixnum> -1)
                                           (terminator #f))
   (return (bytevector->string (SCM_UVECTOR v) start end terminator)))

 (define-cfn string->wordvector
   (klass::ScmClass* s::ScmString* start::int end::int) :static
   (with-input-string-pointers (s start end sp ep)
     (let* ([v (Scm_MakeUVector klass (- end start) NULL)]
            [eltp::int32_t* (cast int32_t* (SCM_UVECTOR_ELEMENTS v))]
            [i::int 0])
       (for [() (< sp ep) (post++ i)]
         (let* ([ch::ScmChar])
           (SCM_CHAR_GET sp ch)
           (set! (aref eltp i) ch)
           (+= sp (SCM_CHAR_NBYTES ch))))
       (return v))))

 (define-cproc string->s32vector (s::<string>
                                  :optional (start::<fixnum> 0)
                                  (end::<fixnum> -1))
   (return (string->wordvector SCM_CLASS_S32VECTOR s start end)))

 (define-cproc string->u32vector (s::<string>
                                  :optional (start::<fixnum> 0)
                                  (end::<fixnum> -1))
   (return (string->wordvector SCM_CLASS_U32VECTOR s start end)))

 (define-cfn string->wordvector!
   (v::ScmUVector* tstart::int s::ScmString* start::int end::int) :static
   (let* ([tlen::int (SCM_UVECTOR_SIZE v)])
     (when (and (>= tstart 0) (< tstart tlen))
       (SCM_UVECTOR_CHECK_MUTABLE v)
       (with-input-string-pointers (s start end sp ep)
         (let* ([buf::int32_t* (cast int32_t* (SCM_UVECTOR_ELEMENTS v))]
                [i::int tstart])
           (for [() (and (< sp ep) (< i tlen)) (post++ i)]
             (let* ([ch::ScmChar])
               (SCM_CHAR_GET sp ch)
               (set! (aref buf i) ch)
               (+= sp (SCM_CHAR_NBYTES ch)))))))
     (return (SCM_OBJ v))))

 (define-cproc string->s32vector! (v::<s32vector>
                                   tstart::<fixnum>
                                   s::<string>
                                   :optional (start::<fixnum> 0)
                                             (end::<fixnum> -1))
   (return (string->wordvector! (SCM_UVECTOR v) tstart s start end)))

 (define-cproc string->u32vector! (v::<u32vector>
                                   tstart::<fixnum>
                                   s::<string>
                                   :optional (start::<fixnum> 0)
                                             (end::<fixnum> -1))
   (return (string->wordvector! (SCM_UVECTOR v) tstart s start end)))

 (define-cfn wordvector->string (v::ScmUVector* start::int end::int term)
   :static
   (let* ([len::int (SCM_UVECTOR_SIZE v)]
          [s (Scm_MakeOutputStringPort FALSE)])
     (SCM_CHECK_START_END start end len)
     (let* ([eltp::int32_t* (cast int32_t* (SCM_UVECTOR_ELEMENTS v))])
       (while (< start end)
         (let* ([ch::ScmChar (cast ScmChar (aref eltp (post++ start)))])
           (when (and (SCM_INTP term)
                      (== (SCM_INT_VALUE term) ch))
             (break))
           (Scm_PutcUnsafe ch (SCM_PORT s)))))
     (return (Scm_GetOutputStringUnsafe (SCM_PORT s) 0))))

 (define-cproc s32vector->string (v::<s32vector>
                                  :optional (start::<fixnum> 0)
                                            (end::<fixnum> -1)
                                            (terminator #f))
   (return (wordvector->string (SCM_UVECTOR v) start end terminator)))

 (define-cproc u32vector->string (v::<u32vector>
                                  :optional (start::<fixnum> 0)
                                            (end::<fixnum> -1)
                                            (terminator #f))
   (return (wordvector->string (SCM_UVECTOR v) start end terminator)))
 )

;; for the bakcward compatibility
(define read-block! read-uvector!)
(define write-block write-uvector)

;; for symmetry of port->string, etc.
;; This actually uses uvector port in gauche.vport, but that's inner
;; details users shouldn't need to care.
;; TODO: Optional endian argument
(define (port->uvector iport :optional (class <u8vector>))
  (let1 p (open-output-uvector (make-uvector class 0) :extendable #t)
    (copy-port iport p)
    (get-output-uvector p :shared #t)))

;;-------------------------------------------------------------
;; Appending vectors
;;

(define-macro (define-appender tag)
  (let ([app   (string->symbol #"~|tag|vector-append")]
        [len   (string->symbol #"~|tag|vector-length")]
        [make  (string->symbol #"make-~|tag|vector")]
        [copy! (string->symbol #"~|tag|vector-copy!")])
    `(define (,app . vs)
       (let* ([size (apply + (map ,len vs))]
              [dest (,make size)])
         (do ([vs vs (cdr vs)]
              [k  0  (+ k (,len (car vs)))])
             [(null? vs) dest]
           (,copy! dest k (car vs)))))
    ))

(define-appender s8)
(define-appender u8)
(define-appender s16)
(define-appender u16)
(define-appender s32)
(define-appender u32)
(define-appender s64)
(define-appender u64)
(define-appender f16)
(define-appender f32)
(define-appender f64)

;;-------------------------------------------------------------
;; Sequence protocol implementation
;;

(define-macro (%define-srfi-4-collection-interface tag)
  (let* ([tagvector (string->symbol #"~|tag|vector")]
         [class     (string->symbol #"<~|tagvector|>")]
         [meta      (string->symbol #"<~|tagvector|-meta>")]
         [len       (string->symbol #"~|tagvector|-length")]
         [ref       (string->symbol #"~|tagvector|-ref")]
         [set       (string->symbol #"~|tagvector|-set!")]
         [copy      (string->symbol #"~|tagvector|-copy")]
         [->list    (string->symbol #"~|tagvector|->list")]
         [list->    (string->symbol #"list->~|tagvector|")]
         [->vec     (string->symbol #"~|tagvector|->vector")]
         [vec->     (string->symbol #"vector->~|tagvector|")]
         [make      (string->symbol #"make-~|tagvector|")])
    `(begin
       (define-method call-with-iterator ((v ,class) proc :key (start #f))
         (let* ([len (,len v)] [i (or start 0)])
           (proc (^[] (>= i len))
                 (^[] (rlet1 r (,ref v i) (inc! i))))))
       (define-method call-with-builder ((c ,meta) proc :key (size #f))
         (if size
           (let ([v (,make size)] [i 0])
             (proc (^[item] (,set v i item) (inc! i))
                   (^[] v)))
           (let1 q (make-queue)
             (proc (^[item] (enqueue! q item))
                   (^[] (,list-> (dequeue-all! q)))))
           ))
       (define-method referencer ((v ,class)) ,ref)
       (define-method modifier   ((v ,class)) ,set)
       (define-method size-of ((v ,class)) (,len v))
       (define-method coerce-to ((c <list-meta>) (v ,class)) (,->list v))
       (define-method coerce-to ((c ,meta) (v <list>)) (,list-> v))
       (define-method coerce-to ((c <vector-meta>) (v ,class)) (,->vec v))
       (define-method coerce-to ((c ,meta) (v <vector>)) (,vec-> v))
       (define-method coerce-to ((c ,meta) (v ,class)) (,copy v))
       (define-method subseq ((v ,class) . args) (apply ,copy v args))
       )))

(%define-srfi-4-collection-interface s8)
(%define-srfi-4-collection-interface u8)
(%define-srfi-4-collection-interface s16)
(%define-srfi-4-collection-interface u16)
(%define-srfi-4-collection-interface s32)
(%define-srfi-4-collection-interface u32)
(%define-srfi-4-collection-interface s64)
(%define-srfi-4-collection-interface u64)
(%define-srfi-4-collection-interface f16)
(%define-srfi-4-collection-interface f32)
(%define-srfi-4-collection-interface f64)

;; some special cases
(define-method coerce-to ((dst <string-meta>) (src <u8vector>))
  (u8vector->string src))
(define-method coerce-to ((dst <string-meta>) (src <s8vector>))
  (s8vector->string src))
(define-method coerce-to ((dst <u8vector-meta>) (src <string>))
  (string->u8vector src))
(define-method coerce-to ((dst <s8vector-meta>) (src <string>))
  (string->s8vector src))
(define-method coerce-to ((dst <string-meta>) (src <u32vector>))
  (u32vector->string src))
(define-method coerce-to ((dst <string-meta>) (src <s32vector>))
  (s32vector->string src))
(define-method coerce-to ((dst <u32vector-meta>) (src <string>))
  (string->u32vector src))
(define-method coerce-to ((dst <s32vector-meta>) (src <string>))
  (string->s32vector src))

