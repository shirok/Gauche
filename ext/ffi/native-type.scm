;;;
;;; gauche.native-type - Native type handling
;;;
;;;   Copyright (c) 2011-2025  Shiro Kawai  <shiro@acm.org>
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

;; This module provides the means to define and manipulate
;; compound native types and native handles.

(define-module gauche.native-type
  (use util.match)
  (use gauche.cgen.type.parse)
  (use gauche.sequence)
  (use gauche.threads)
  (use gauche.uvector)
  (extend gauche.typeutil)              ;access internal routines
  (export make-c-pointer-type
          make-c-function-type
          make-c-array-type
          make-c-struct-type
          make-c-union-type

          c-pointer-type-pointee
          c-array-type-dimensions
          c-function-type-return-type
          c-function-type-argument-types
          c-function-type-variadic?
          c-struct/union-type-tag
          c-struct/union-type-field-names
          c-struct/union-type-field-type
          c-struct/union-type-field-offset

          c-pointer-type?
          c-array-type?
          c-function-type?
          c-struct-type?
          c-union-type?
          c-struct/union-type?
          c-aggregate-type?
          c-pointer-like-type?

          native-handle-type
          native*
          native-aref
          native.
          native->
          native&

          uvector->native-handle
          null-pointer-handle
          null-pointer-handle?

          c-pointer-handle?
          c-function-handle?
          c-array-handle?
          c-struct-handle?
          c-union-handle?
          c-aggregate-handle?
          c-pointer-like-handle?

          cast-handle

          c-pointer-compare
          c-pointer=?
          c-pointer<?
          c-pointer<=?
          c-pointer>?
          c-pointer>=?
          c-memwise-compare

          native-type
          native-type->signature

          <int16-le> <int16-be>
          <uint16-le> <uint16-be>
          <int32-le> <int32-be>
          <uint32-le> <uint32-be>
          <int64-le> <int64-be>
          <uint64-le> <uint64-be>
          <float-le> <float-be>
          <double-le> <double-be>

          <native-wrapper-meta>
          <native-wrapper-mixin>
          <wrapped-c-pointer>
          <wrapped-c-array>
          <wrapped-c-struct>
          <wrapped-c-union>
          make-native-wrapper-class
          wrap-native-handle
          ))
(select-module gauche.native-type)

(inline-stub
 (.include "gauche/priv/bytesP.h"
           "gauche/priv/typeP.h")

 (declare-stub-type <native-handle> ScmNativeHandle*)
 )

;;;
;;; Additonal native type predicates/accessors
;;;

(define-inline (c-pointer-type? type) (is-a? type <c-pointer>))
(define-inline (c-array-type? type) (is-a? type <c-array>))
(define-inline (c-function-type? type) (is-a? type <c-function>))
(define-inline (c-struct-type? type) (is-a? type <c-struct>))
(define-inline (c-union-type? type) (is-a? type <c-union>))

(define-inline (c-struct/union-type? type)
  (or (is-a? type <c-struct>)
      (is-a? type <c-union>)))

;; C types that has some structure
(define (c-aggregate-type? type)
  (or (is-a? type <c-array>)
      (is-a? type <c-struct>)
      (is-a? type <c-union>)))

;; C types that can be 'casted' to a poinetr
(define (c-pointer-like-type? type)
  (or (is-a? type <c-pointer>)
      (is-a? type <c-array>)
      (is-a? type <c-function>)))

(define-inline (c-pointer-type-pointee type)
  (assume-type type <c-pointer>)
  (~ type'pointee-type))

(define-inline (c-array-type-dimensions type)
  (assume-type type <c-array>)
  (~ type'dimensions))

(define-inline (c-function-type-return-type type)
  (assume-type type <c-function>)
  (~ type'return-type))

(define-inline (c-function-type-argument-types type)
  (assume-type type <c-function>)
  (~ type'argument-types))

(define-inline (c-function-type-variadic? type)
  (assume-type type <c-function>)
  (~ type'variadic?))

(define-inline (c-struct/union-type-tag type)
  (assume-type type (</> <c-struct> <c-union>))
  (~ type'tag))

(define-inline (c-struct/union-type-field-names type)
  (assume-type type (</> <c-struct> <c-union>))
  (map car (~ type'fields)))

(define-inline (c-struct/union-type-field-type type field-name)
  (assume-type type (</> <c-struct> <c-union>))
  (if-let1 e (assq field-name (~ type'fields))
    (cadr e)
    (errorf "~s does not have a field named ~a" type field-name)))

(define-inline (c-struct/union-type-field-offset type field-name)
  (assume-type type (</> <c-struct> <c-union>))
  (if-let1 e (assq field-name (~ type'fields))
    (caddr e)
    (errorf "~s does not have a field named ~a" type field-name)))

;;;
;;; Endian-specified types
;;;

;; These can be used to access binary data.

(inline-stub
 (define-cfn int16swap_ref (_::ScmNativeType* ptr::void*) :static
   (let* ([v::swap_s16_t])
     (set! (ref v val) (* (cast (int16_t*) ptr)))
     (SWAP_2 v)
     (return (SCM_MAKE_INT (ref v val)))))

 (define-cfn int16swap_set (_::ScmNativeType* ptr::void* obj) ::void :static
   (let* ([v::swap_s16_t])
     (set! (ref v val) (cast int16_t (SCM_INT_VALUE obj)))
     (SWAP_2 v)
     (set! (* (cast (int16_t*) ptr)) (ref v val))))

 (define-cfn uint16swap_ref (_::ScmNativeType* ptr::void*) :static
   (let* ([v::swap_u16_t])
     (set! (ref v val) (* (cast (uint16_t*) ptr)))
     (SWAP_2 v)
     (return (SCM_MAKE_INT (ref v val)))))

 (define-cfn uint16swap_set (_::ScmNativeType* ptr::void* obj) ::void :static
   (let* ([v::swap_u16_t])
     (set! (ref v val) (cast uint16_t (SCM_INT_VALUE obj)))
     (SWAP_2 v)
     (set! (* (cast (uint16_t*) ptr)) (ref v val))))

 (define-cfn int32swap_ref (_::ScmNativeType* ptr::void*) :static
   (let* ([v::swap_s32_t])
     (set! (ref v val) (* (cast (int32_t*) ptr)))
     (SWAP_4 v)
     (return (Scm_MakeInteger (ref v val)))))

 (define-cfn int32swap_set (_::ScmNativeType* ptr::void* obj) ::void :static
   (let* ([v::swap_s32_t])
     (set! (ref v val) (cast int32_t (Scm_GetInteger32 obj)))
     (SWAP_4 v)
     (set! (* (cast (int32_t*) ptr)) (ref v val))))

 (define-cfn uint32swap_ref (_::ScmNativeType* ptr::void*) :static
   (let* ([v::swap_u32_t])
     (set! (ref v val) (* (cast (uint32_t*) ptr)))
     (SWAP_4 v)
     (return (Scm_MakeIntegerU (ref v val)))))

 (define-cfn uint32swap_set (_::ScmNativeType* ptr::void* obj) ::void :static
   (let* ([v::swap_u32_t])
     (set! (ref v val) (cast uint32_t (Scm_GetIntegerU32 obj)))
     (SWAP_4 v)
     (set! (* (cast (uint32_t*) ptr)) (ref v val))))

 (define-cfn int64swap_ref (_::ScmNativeType* ptr::void*) :static
   (let* ([v::swap_s64_t])
     (set! (ref v val) (* (cast (int64_t*) ptr)))
     (SWAP_8 v)
     (return (Scm_MakeInteger64 (ref v val)))))

 (define-cfn int64swap_set (_::ScmNativeType* ptr::void* obj) ::void :static
   (let* ([v::swap_s64_t])
     (set! (ref v val) (cast int64_t (Scm_GetInteger64 obj)))
     (SWAP_8 v)
     (set! (* (cast (int64_t*) ptr)) (ref v val))))

 (define-cfn uint64swap_ref (_::ScmNativeType* ptr::void*) :static
   (let* ([v::swap_u64_t])
     (set! (ref v val) (* (cast (uint64_t*) ptr)))
     (SWAP_8 v)
     (return (Scm_MakeIntegerU64 (ref v val)))))

 (define-cfn uint64swap_set (_::ScmNativeType* ptr::void* obj) ::void :static
   (let* ([v::swap_u64_t])
     (set! (ref v val) (cast uint64_t (Scm_GetIntegerU64 obj)))
     (SWAP_8 v)
     (set! (* (cast (uint64_t*) ptr)) (ref v val))))

 (define-cfn floatswap_ref (_::ScmNativeType* ptr::void*) :static
   (let* ([v::swap_f32_t])
     (set! (ref v val) (* (cast (float*) ptr)))
     (SWAP_4 v)
     (return (Scm_MakeFlonum (ref v val)))))

 (define-cfn floatswap_set (_::ScmNativeType* ptr::void* obj) ::void :static
   (let* ([v::swap_f32_t])
     (set! (ref v val) (cast (float) (Scm_GetDouble obj)))
     (SWAP_4 v)
     (set! (* (cast (float*) ptr)) (ref v val))))

  (define-cfn doubleswap_ref (_::ScmNativeType* ptr::void*) :static
   (let* ([v::swap_f64_t])
     (set! (ref v val) (* (cast (double*) ptr)))
     (SWAP_8 v)
     (return (Scm_MakeFlonum (ref v val)))))

 (define-cfn doubleswap_set (_::ScmNativeType* ptr::void* obj) ::void :static
   (let* ([v::swap_f64_t])
     (set! (ref v val) (Scm_GetDouble obj))
     (SWAP_8 v)
     (set! (* (cast (double*) ptr)) (ref v val))))

 (initcode
  (.if "WORDS_BIGENDIAN"
    ;; Big endian platform
    (let* ([m::ScmModule* (SCM_CURRENT_MODULE)])
      (SCM_DEFINE m "<int16-be>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeInt16Type))
                   "<int16-be>" 0 NULL NULL))
      (SCM_DEFINE m "<uint16-be>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeUint16Type))
                   "<uint16-be>" 0 NULL NULL))
      (SCM_DEFINE m "<int32-be>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeInt32Type))
                   "<int32-be>" 0 NULL NULL))
      (SCM_DEFINE m "<uint32-be>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeUint32Type))
                   "<uint32-be>" 0 NULL NULL))
      (SCM_DEFINE m "<int64-be>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeInt64Type))
                   "<int64-be>" 0 NULL NULL))
      (SCM_DEFINE m "<uint64-be>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeUint64Type))
                   "<uint64-be>" 0 NULL NULL))
      (SCM_DEFINE m "<float-be>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeFloatType))
                   "<float-be>" 0 NULL NULL))
      (SCM_DEFINE m "<double-be>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeDoubleType))
                   "<double-be>" 0 NULL NULL))

      (SCM_DEFINE m "<int16-le>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeInt16Type))
                   "<int16-le>" 0 int16swap_ref int16swap_set))
      (SCM_DEFINE m "<uint16-le>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeUint16Type))
                   "<uint16-le>" 0 uint16swap_ref uint16swap_set))
      (SCM_DEFINE m "<int32-le>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeInt32Type))
                   "<int32-le>" 0 int32swap_ref int32swap_set))
      (SCM_DEFINE m "<uint32-le>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeUint32Type))
                   "<uint32-le>" 0 uint32swap_ref uint32swap_set))
      (SCM_DEFINE m "<int64-le>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeInt64Type))
                   "<int64-le>" 0 int64swap_ref int64swap_set))
      (SCM_DEFINE m "<uint64-le>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeUint64Type))
                   "<uint64-le>" 0 uint64swap_ref uint64swap_set))
      (SCM_DEFINE m "<float-le>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeFloatType))
                   "<float-le>" 0 floatswap_ref floatswap_set))
      (SCM_DEFINE m "<double-le>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeDoubleType))
                   "<double-le>" 0 doubleswap_ref doubleswap_set))
      )
    ;; Little endian platform
    (let* ([m::ScmModule* (SCM_CURRENT_MODULE)])
      (SCM_DEFINE m "<int16-le>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeInt16Type))
                   "<int16-le>" 0 NULL NULL))
      (SCM_DEFINE m "<uint16-le>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeUint16Type))
                   "<uint16-le>" 0 NULL NULL))
      (SCM_DEFINE m "<int32-le>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeInt32Type))
                   "<int32-le>" 0 NULL NULL))
      (SCM_DEFINE m "<uint32-le>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeUint32Type))
                   "<uint32-le>" 0 NULL NULL))
      (SCM_DEFINE m "<int64-le>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeInt64Type))
                   "<int64-le>" 0 NULL NULL))
      (SCM_DEFINE m "<uint64-le>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeUint64Type))
                   "<uint64-le>" 0 NULL NULL))
      (SCM_DEFINE m "<float-le>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeFloatType))
                   "<float-le>" 0 NULL NULL))
      (SCM_DEFINE m "<double-le>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeDoubleType))
                   "<double-le>" 0 NULL NULL))

      (SCM_DEFINE m "<int16-be>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeInt16Type))
                   "<int16-be>" 0 int16swap_ref int16swap_set))
      (SCM_DEFINE m "<uint16-be>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeUint16Type))
                   "<uint16-be>" 0 uint16swap_ref uint16swap_set))
      (SCM_DEFINE m "<int32-be>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeInt32Type))
                   "<int32-be>" 0 int32swap_ref int32swap_set))
      (SCM_DEFINE m "<uint32-be>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeUint32Type))
                   "<uint32-be>" 0 uint32swap_ref uint32swap_set))
      (SCM_DEFINE m "<int64-be>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeInt64Type))
                   "<int64-be>" 0 int64swap_ref int64swap_set))
      (SCM_DEFINE m "<uint64-be>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeUint64Type))
                   "<uint64-be>" 0 uint64swap_ref uint64swap_set))
      (SCM_DEFINE m "<float-be>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeFloatType))
                   "<float-be>" 0 floatswap_ref floatswap_set))
      (SCM_DEFINE m "<double-be>"
                  (Scm__MakeNativeTypeVariant
                   (SCM_NATIVE_TYPE (Scm_NativeDoubleType))
                   "<double-be>" 0 doubleswap_ref doubleswap_set))
      )
    )))

(define %extended-native-type-table
  (let1 tab (make-hash-table 'eq?)
    (define (tname tsym)
      (string->symbol
       (regexp-replace-all* (symbol->string tsym)
                            #/[<>]/ ""
                            #/-/ "_")))
    (let-syntax ([! (syntax-rules ()
                      [(_ t ...)
                       (begin (hash-table-put! tab (tname 't) t) ...)])])
      (! <int16-le> <int16-be> <uint16-le> <uint16-be>
         <int32-le> <int32-be> <uint32-le> <uint32-be>
         <int64-le> <int64-be> <uint64-le> <uint64-be>
         <float-le> <float-be> <double-le> <double-be>))
    (^[] tab)))

;;;
;;; Native handles
;;;

(define-inline (native-handle-type handle) (~ handle'type))

(define-cproc %uvector->native-handle (uv::<uvector>
                                       handle-type::<native-type>
                                       offset::<fixnum>)
  (let* ([p::void* (SCM_UVECTOR_ELEMENTS uv)]
         [vecsize::ScmSmallInt (Scm_UVectorSizeInBytes uv)]
         [data-type::ScmNativeType* (?: (SCM_C_POINTER_P handle-type)
                                        (-> (SCM_C_POINTER handle-type)
                                            pointee_type)
                                        handle-type)]
         [datasize::ScmSmallInt (-> data-type size)]
         [max::void* (+ p (Scm_UVectorSizeInBytes uv))])
    (unless (and (<= 0 offset) (<= (+ offset datasize) vecsize))
      (Scm_Error "Offset %ld out of range, or type size %ld too big."
                 offset datasize))
    (return
     (Scm__MakeNativeHandle (+ p offset)
                            handle-type
                            (-> handle-type name) ; temporary
                            p
                            max
                            (SCM_OBJ uv)
                            SCM_NIL
                            0))))

(define (uvector->native-handle uv type :optional (offset 0))
  (assume-type uv (<?> <uvector>))
  (assume (or (is-a? type <c-pointer>)
              (c-aggregate-type? type))
    "Type must be native pointer or aggregate type, but got:" type)
  (%uvector->native-handle (or uv (make-u8vector (~ type'size)))
                           type offset))

;; The handle pointing into a region originally pointed
(define-cproc make-internal-handle (base::<native-handle>
                                    offset::<fixnum>
                                    :optional (type::<native-type>? NULL))
  (let* ([p::void* (+ (-> base ptr) offset)]
         [t::ScmNativeType* (?: type type (-> base type))])
    (unless (and (!= (-> base region-min) NULL)
                 (!= (-> base region-max) NULL)
                 (<= (-> base region-min) p)
                 (<   p (-> base region-max)))
      (Scm_Error "Offset out of range: %S" offset))
    (return
     (Scm__MakeNativeHandle p
                            t
                            (-> t name) ; temporary
                            (-> base region-min)
                            (-> base region-max)
                            (-> base owner)
                            SCM_NIL
                            0))))

(define-cproc null-pointer-handle (:optional (type::<native-type>? #f))
  (let* ([t::ScmNativeType*
          (?: type type (SCM_NATIVE_TYPE (Scm_NativeVoidPointerType)))])
    (unless (or (SCM_C_POINTER_P t)
                (SCM_C_ARRAY_P t)
                (SCM_C_FUNCTION_P t))
      (Scm_Error "Invalid type for a null pointer: %S" t))
    (return
     (Scm__MakeNativeHandle NULL
                            t
                            (-> t name)
                            NULL
                            NULL
                            SCM_FALSE
                            SCM_NIL
                            0))))

(define-cproc null-pointer-handle? (h::<native-handle>) ::<boolean>
  (return (and (or (SCM_C_POINTER_P (-> h type))
                   (SCM_C_ARRAY_P (-> h type)))
               (== (-> h ptr) NULL))))

(define (c-pointer-handle? obj)
  (and (is-a? obj <native-handle>)
       (is-a? (native-handle-type obj) <c-pointer>)))

(define (c-array-handle? obj)
  (and (is-a? obj <native-handle>)
       (is-a? (native-handle-type obj) <c-array>)))

(define (c-function-handle? obj)
  (and (is-a? obj <native-handle>)
       (is-a? (native-handle-type obj) <c-function>)))

(define (c-struct-handle? obj)
  (and (is-a? obj <native-handle>)
       (is-a? (native-handle-type obj) <c-struct>)))

(define (c-union-handle? obj)
  (and (is-a? obj <native-handle>)
       (is-a? (native-handle-type obj) <c-union>)))

(define (c-pointer-like-handle? obj)
  (and (is-a? obj <native-handle>)
       (c-pointer-like-type? (native-handle-type obj))))

(define (c-aggregate-handle? obj)
  (and (is-a? obj <native-handle>)
       (c-aggregate-type? (native-handle-type obj))))

;;
;; Casting
;;

;; We can 'cast' pointer-like handle to different pointer-like type.
;; Offset is a bypte offset in relative to the original pointer---only to
;; be used by those who know what they're doing.

(define (cast-handle type handle :optional (offset 0))
  (assume-type type <native-type>)
  (assume-type handle <native-handle>)
  (assume-type offset <fixnum>)
  (unless (c-pointer-like-type? type)
    (error "You can only cast to pointer-like type, but got:" handle))
  (unless (c-pointer-like-handle? handle)
    (error "You can only cast pointer-like handle, but got:" handle))
  (%cast-handle type handle offset))

(define-cproc %cast-handle (type::<native-type>
                            handle::<native-handle>
                            offset::<fixnum>)
  (return (Scm__MakeNativeHandle (+ (-> handle ptr) offset)
                                 type
                                 (-> handle name)
                                 (-> handle region-min)
                                 (-> handle region-max)
                                 (-> handle owner)
                                 (-> handle attrs)
                                 (-> handle flags))))

;;
;; Comparison
;;

(define-cproc c-pointer-compare (a::<native-handle> b::<native-handle>)
  ::<fixnum>
  (unless (or (SCM_C_POINTER_P (-> a type))
              (SCM_C_ARRAY_P (-> a type))
              (SCM_C_FUNCTION_P (-> a type)))
    (Scm_Error "c-pointer, c-array, or c-function handle expeced, but got: %S"
               a))
  (unless (or (SCM_C_POINTER_P (-> b type))
              (SCM_C_ARRAY_P (-> b type))
              (SCM_C_FUNCTION_P (-> b type)))
    (Scm_Error "c-pointer, c-array, or c-function handle expeced, but got: %S"
               b))
  (cond [(== (-> a ptr) (-> b ptr)) (return 0)]
        [(<  (-> a ptr) (-> b ptr)) (return -1)]
        [else                       (return 1)]))

(define (c-pointer=? a b)  (zero? (c-pointer-compare a b)))
(define (c-pointer<? a b)  (< (c-pointer-compare a b) 0))
(define (c-pointer<=? a b) (<= (c-pointer-compare a b) 0))
(define (c-pointer>? a b)  (> (c-pointer-compare a b) 0))
(define (c-pointer>=? a b) (>= (c-pointer-compare a b) 0))

;; This perform memcmp on aggregate types like u8vector-compare.  That is,
;;   - First the sizes are compared. If they differ, that's the result.
;;   - If the sizes are the same, contents are compared bytewise.
;; Two types may differ.
(define-cproc c-memwise-compare (a::<native-handle> b::<native-handle>)
  ::<fixnum>
  (unless (or (SCM_C_ARRAY_P (-> a type))
              (SCM_C_STRUCT_P (-> a type))
              (SCM_C_UNION_P (-> a type)))
    (Scm_Error "c-array, c-struct, or c-union handle expected, but got: %S" a))
  (unless (or (SCM_C_ARRAY_P (-> b type))
              (SCM_C_STRUCT_P (-> b type))
              (SCM_C_UNION_P (-> b type)))
    (Scm_Error "c-array, c-struct, or c-union handle expected, but got: %S" b))
  (let* ([a-size::ptrdiff_t (- (-> a region-max) (-> a region-min))]
         [b-size::ptrdiff_t (- (-> b region-max) (-> b region-min))])
    (cond [(< a-size b-size) (return -1)]
          [(> a-size b-size) (return 1)]
          [else (let* ([r::int (memcmp (-> a ptr) (-> b ptr) a-size)])
                  (cond [(< r 0) (return -1)]
                        [(> r 0) (return 1)]
                        [else (return 0)]))])))

(define-method object-equal? ((a <native-handle>) (b <native-handle>))
  (cond
   [(c-pointer-handle? a)
    (and (c-pointer-handle? b) (zero? (c-pointer-compare a b)))]
   [(c-function-handle? a)
    (and (c-function-handle? b) (zero? (c-pointer-compare a b)))]
   [(c-array-handle? a)
    (and (c-array-handle? b) (zero? (c-memwise-compare a b)))]
   [(c-struct-handle? a)
    (and (c-struct-handle? b) (zero? (c-memwise-compare a b)))]
   [(c-union-handle? a)
    (and (c-union-handle? b) (zero? (c-memwise-compare a b)))]
   [else #f]))

;;;
;;; Low-level accessor/modifier
;;;

;; Access handle's ptr + offset
(define-cproc %pref (element-type::<native-type>
                     handle::<native-handle>
                     offset::<fixnum>)
  (let* ([p::void* (+ (-> handle ptr) offset)]
         [c-ref::(.function (t::ScmNativeType* p::void*)::ScmObj *)
                 (-> element-type c-ref)])
    (when (== p NULL)
      (Scm_Error "Attempt to derefernce NULL pointer: %S" handle))
    (when (== c-ref NULL)
      (Scm_Error "Cannot dereference type %S" element-type))
    (when (and (!= (-> handle region-max) NULL)
               (!= (-> handle region-min) NULL)
               (not (and (<= (-> handle region-min) p)
                         (<  p (-> handle region-max)))))
      (Scm_Error "Offset out of range: %ld" offset))
    (return (c-ref element-type p))))

;; Unified set: set handle's ptr + offset = val
(define-cproc %pset! (element-type::<native-type>
                      handle::<native-handle>
                      offset::<fixnum>
                      val)
  ::<void>
  (let* ([p::void* (+ (-> handle ptr) offset)]
         [c-of-type::(.function (t::ScmNativeType* v::ScmObj)::int *)
                     (-> element-type c-of-type)]
         [c-set::(.function (t::ScmNativeType*  p::void* v::ScmObj)::void *)
                 (-> element-type c-set)])
    (unless (c-of-type element-type val)
      (Scm_Error "Invalid object to set to %S (%S): %S"
                 handle element-type val))
    (when (== c-set NULL)
      (Scm_Error "Cannot set value of type %S" element-type))
    (when (and (!= (-> handle region-max) NULL)
               (!= (-> handle region-min) NULL)
               (not (and (<= (-> handle region-min) p)
                         (<  p (-> handle region-max)))))
      (Scm_Error "Offset out of range: %ld" offset))
    (c-set element-type p val)))

(define (%handle-ref type handle offset)
  (if (c-aggregate-type? type)
    (make-internal-handle handle offset type)
    (%pref type handle offset)))

(define (%handle-set! type handle offset val)
  (when (c-aggregate-type? type)
    ;; For now, we reject it.  Technically we can copy aggregate type
    ;; content into the target aggregate.
    (errorf "Can't set a value of type ~s into ~s" type handle))
  (%pset! type handle offset val))

(define (c-struct-field type selector)
  (assume-type type (</> <c-struct> <c-union>))
  (assume-type selector <symbol>)
  (if-let1 field (assq selector (~ type'fields))
    field
    (errorf "Unknown native struct field ~s for ~s" selector type)))

;; Returns a byte offset
(define (native-type-offset type selector)
  (assume-type type <native-type>)
  (typecase type
    [<c-pointer>
     ;; Selector is an element index
     (assume-type selector <fixnum>)
     (* selector (~ type'pointee-type'size))]
    [<c-array>
     (unless (every (every-pred fixnum? (complement negative?)) selector)
       (error "Invalid native array selector:" selector))
     (let* ([etype (~ type'element-type)]
            [dims (~ type'dimensions)]
            [sels (cond
                   [(length<? selector dims)
                    ;; Arrays can be dereferenced with partial index, e.g.
                    ;; array int a[2][3][4] can be dereferenced as
                    ;; a[2][3], returning int [4].  Offset is computed
                    ;; by setting unspecified index to zero.
                    (append selector
                            (make-list (- (length dims) (length selector)) 0))]
                   [(length=? selector dims) selector]
                   [else
                    (errorf "Native array selector ~s doesn't match the \
                            dimensions ~s"
                            selector dims)])])
       (let loop ([ds (reverse dims)]
                  [ss (reverse sels)]
                  [step 1]
                  [i 0])
         (cond [(null? ds) (* i (~ etype'size))]
               [(eq? (car ds) '*) ; this can only appear in the 1st dim
                (* (+ (* (car ss) step) i) (~ etype'size))]
               [(< (car ss) (car ds))
                (loop (cdr ds) (cdr ss) (* step (car ds))
                      (+ (* (car ss) step) i))]
               [else
                (error "Native array selector is out of range:" selector)])))]
    [(</> <c-struct> <c-union>)
     (caddr (c-struct-field type selector))]
    [else
     (error "Unsupported native aggregate type:" type)]))

;; Returns an array type if selector does not specify a single element
;; in the array.
(define (%array-dereference-type atype selector)
  (let1 etype (~ atype'element-type)
    (let loop ([dims (~ atype'dimensions)]
               [sels selector])
      (if (null? sels)
        (if (not (null? dims))
          (make-c-array-type etype dims)
          etype)
        (loop (cdr dims) (cdr sels))))))

;; Common routine to allow type override.
(define (%handle-type handle type-override)
  (or type-override
      (~ handle'type)
      (error "Can't dereference a pointer with unknown type:" handle)))

;;;
;;;  Public accessor/modifier
;;;

;; Pointer dereference
(define (native* handle :optional (type #f))
  (assume-type handle <native-handle>)
  (let1 t (%handle-type handle type)
    (assume-type t <c-pointer>
      "Attempt to dereferencing non-pointer type:" (cons handle t))
    (let1 pt (~ t'pointee-type)
      ;; NB: We 'shortcut' pointer-to-aggregate handles; both
      ;; pointer-to-aggregaete and reference-to-aggregate handles have
      ;; ptr point to the aggregate, and only the type differs.
      ;; So, dereferencing pointer-to-aggregaete handle returns
      ;; the same ptr handle with reference-to-aggregate type.
      (if (c-aggregate-type? pt)
        (%cast-handle pt handle 0)
        (%pref (~ t'pointee-type) handle 0)))))

(define (%native*-set! handle type val)
  (assume-type handle <native-handle>)
  (let* ([t (%handle-type handle type)])
    (assume-type t <c-pointer>)
    (%handle-set! (~ t'pointee-type) handle 0 val)))
(set! (setter native*)
      (case-lambda
        [(handle type val) (%native*-set! handle type val)]
        [(handle val) (%native*-set! handle #f val)]))

;; Array reference
;; (Pointer reference with offset, e.g. *(p+i), can be handled with this, too)
(define (native-aref handle indices :optional (type #f))
  (assume-type handle <native-handle>)
  (assume-type indices (</> <fixnum> (<List> <fixnum>)))
  (let* ([t (%handle-type handle type)]
         [offset (native-type-offset t indices)])
    (etypecase t
      [<c-array>
       (%handle-ref (%array-dereference-type t indices) handle offset)]
      [<c-pointer>
       (%handle-ref (~ t 'pointee-type) handle offset)])))

(define (%native-aref-set! handle indices type val)
  (assume-type handle <native-handle>)
  (assume-type indices (</> <fixnum> (<List> <fixnum>)))
  (let* ([t (%handle-type handle type)]
         [offset (native-type-offset t indices)])
    (etypecase t
      [<c-array>
       (%handle-set! (%array-dereference-type t indices) handle offset val)]
      [<c-pointer>
       (%handle-set! (~ t'pointee-type) handle offset val)])))
(set! (setter native-aref)
      (case-lambda
        [(handle indices type val) (%native-aref-set! handle indices type val)]
        [(handle indices val) (%native-aref-set! handle indices #f val)]))


;; Struct/union direct field access
(define (native. handle slot :optional (type #f))
  (assume-type handle <native-handle>)
  (assume-type slot <symbol>)
  (let* ([t (%handle-type handle type)])
    (assume-type t (</> <c-struct> <c-union>))
    (let* ([field (c-struct-field t slot)]
           [ctype (cadr field)]
           [offset (caddr field)])
      (%handle-ref ctype handle offset))))

(define (%native.-set! handle slot type val)
  (assume-type handle <native-handle>)
  (assume-type slot <symbol>)
  (let* ([t (%handle-type handle type)])
    (assume-type t (</> <c-struct> <c-union>))
    (let* ([field (c-struct-field t slot)]
           [ctype (cadr field)]
           [offset (caddr field)])
      (%handle-set! ctype handle offset val))))
(set! (setter native.)
      (case-lambda
        [(handle slot type val) (%native.-set! handle slot type val)]
        [(handle slot val) (%native.-set! handle slot #f val)]))

;; Struct/union indirect field access
(define (native-> handle slot :optional (type #f))
  (assume-type handle <native-handle>)
  (assume-type slot <symbol>)
  (let* ([t (%handle-type handle type)])
    (assume-type t <c-pointer>)
    (let* ([pt (~ t'pointee-type)])
      (assume-type pt (</> <c-struct> <c-union>))
      (let* ([field (c-struct-field pt slot)]
             [ctype (cadr field)]
             [offset (caddr field)])
        (%handle-ref ctype handle offset)))))

(define (%native->-set! handle slot type val)
  (assume-type handle <native-handle>)
  (assume-type slot <symbol>)
  (let* ([t (%handle-type handle type)])
    (assume-type t <c-pointer>)
    (let* ([pt (~ t'pointee-type)])
      (assume-type pt (</> <c-struct> <c-union>))
      (let* ([field (c-struct-field pt slot)]
             [ctype (cadr field)]
             [offset (caddr field)])
        (%handle-set! ctype handle offset val)))))

(set! (setter native->)
      (case-lambda
        [(handle slot type val) (%native->-set! handle slot type val)]
        [(handle slot val) (%native->-set! handle slot #f val)]))

;; Take an address of struct/union member or array element
(define (native& handle selector)
  (assume-type handle <native-handle>)
  (let1 type (~ handle'type)
    (typecase type
      [(</> <c-struct> <c-union>)
       (assume-type selector <symbol>
         "native& on c-struct or c-union requires a symbol selector, but got;"
         selector)
       (let ([off (native-type-offset type selector)]
             [etype (c-struct/union-type-field-type type selector)])
         (make-internal-handle handle off (make-c-pointer-type etype)))]
      [<c-array>
       (assume-type selector (</> <fixnum> (<List> <fixnum>))
         "native& on c-struct or c-union requires a symbol selector, but got;"
         selector)
       (let ([off (native-type-offset type (if (list? selector)
                                             selector
                                             (list selector)))])
         (make-internal-handle handle off
                               (make-c-pointer-type (~ type'element-type))))]
      [else
       (error "native& requires c-array, c-struct or c-union handle, but got:"
              handle)])))

;;;
;;;  Convert type signatures to native-type instance
;;;

;; Check if a symbol name ends with '*', indicating a pointer type.
;; Returns (base-symbol . depth) or #f.
(define (%pointer-type-decompose sym)
  (and-let1 m (#/^([^*]*)(\*+)$/ (symbol->string sym))
    `(,(string->symbol (m 1)) . ,(string-length (m 2)))))

;; Normalize a list of symbols representing a C type, e.g. (char *),
;; (char const*), (int **), (int* *).  Strip 'const' and concatenate
;; the remaining parts into a single symbol.
(define (%normalize-c-type-list syms)
  ($ string->symbol $ apply string-append
     (filter-map (^s (rxmatch-case (symbol->string s)
                       [#/^const(\**)$/ (#f ptrs) ptrs]
                       [else => values]))
                 syms)))

;; Wrap a native type in N layers of pointer type.
(define (%wrap-pointer-type base-type depth)
  (let loop ([t base-type] [d depth])
    (if (= d 0)
      t
      (loop (make-c-pointer-type t) (- d 1)))))

;; Parse a struct/union field spec.
(define (%parse-field-specs specs)
  (map (^e (match-let1 (member ':: type) e
             (if type
               (list member (native-type type))
               (error "missing type for field:" member))))
       (cgen-canonical-typed-var-list specs #f)))

;; (native-type signature) => <native-type> instance
;;
;; Parse a type signature (using cise conventions) and return the
;; corresponding native type.
;;
;; Examples:
;;   (native-type int)                   => <int>
;;   (native-type int*)                  => (make-c-pointer-type <int>)
;;   (native-type char**)               => (make-c-pointer-type (make-c-pointer-type <int8>))
;;   (native-type (.array int (3)))     => (make-c-array-type <int> '(3))
;;   (native-type (.array char (2 3)))  => (make-c-array-type <int8> '(2 3))
;;   (native-type (.struct foo (a::int b::double)))
;;     => (make-c-struct-type 'foo `((a ,<c-int>) (b ,<double>)))
;;   (native-type (.struct foo ((a::(.array char (8))))))
;;     => (make-c-struct-type 'foo `((a ,(make-c-array-type <int8> '(8)))))
;;   (native-type (.union u1 (a::int b::float)))
;;     => (make-c-union-type 'u1 `((a ,<c-int>) (b ,<float>)))
;;   (native-type (.function (int int) double))
;;     => (make-c-function-type <c-double> `(,<int> ,<int>))
;;   (native-type (.function (int char* ...) void))
;;     => (make-c-function-type <void> `(,<int> ,(make-c-pointer-type <int8>) ...))
;;
(define (native-type signature)
  (match signature
    ;; Pass-through if already a native type instance
    [(? (cut is-a? <> <native-type>)) signature]

    ;; Compound types

    ;; (.array element-type (dim ...))
    [('.array etype (dims ...))
     (make-c-array-type (native-type etype) dims)]

    ;; (.struct tag (field-specs ...))
    [('.struct (? symbol? tag) (field-specs ...))
     (make-c-struct-type tag (%parse-field-specs field-specs))]
    ;; (.struct (field-specs ...))  -- anonymous
    [('.struct (field-specs ...))
     (make-c-struct-type #f (%parse-field-specs field-specs))]

    ;; (.union tag (field-specs ...))
    [('.union (? symbol? tag) (field-specs ...))
     (make-c-union-type tag (%parse-field-specs field-specs))]
    ;; (.union (field-specs ...))  -- anonymous
    [('.union (field-specs ...))
     (make-c-union-type #f (%parse-field-specs field-specs))]

    ;; (.function (arg-types ...) return-type)
    [('.function (arg-types ...) ret-type)
     (make-c-function-type
      (native-type ret-type)
      (map (^[a] (if (eq? a '...) '... (native-type a)))
           arg-types))]

    ;; We ignore 'const' for now
    [('const type)
     (native-type type)]

    ;; List with an inserted <native-type> instance, e.g. (,foo *),
    ;; (const ,foo *), (,foo **).  The remaining elements must be
    ;; modifier symbols (const, *, **, etc.).
    [(? (^l (and (list? l)
                 (any (cut is-a? <> <native-type>) l)
                 (every (^x (or (symbol? x) (is-a? x <native-type>))) l))))
     (let ([instances (filter (cut is-a? <> <native-type>) signature)]
           [modifiers (filter symbol? signature)])
       (unless (null? (cdr instances))
         (error "Native type signature can only contain one type instance:"
                signature))
       (let1 normalized (symbol->string (%normalize-c-type-list modifiers))
         (unless (#/^\**$/ normalized)
           (error "Invalid native type signature:" signature))
         (%wrap-pointer-type (car instances) (string-length normalized))))]

    ;; C-style list type form, e.g. (char*), (char *), (char const*),
    ;; (int **), (int* *).  Strip const and concatenate into a single
    ;; symbol, then re-parse.
    [((? symbol?) . (? (^r (every symbol? r))))
     (native-type (%normalize-c-type-list signature))]

    ;; Simple symbol types
    [(? symbol?)
     (or
      ;; Primitive types
      (hash-table-get (%builtin-native-type-table) signature #f)
      (hash-table-get (%extended-native-type-table) signature #f)
      ;; Pointer type (trailing *)
      (and-let* ([decomp (%pointer-type-decompose signature)])
        (%wrap-pointer-type (native-type (car decomp)) (cdr decomp)))
      ;; c-string is a pseudo type name for <c-string> (NUL-terminated
      ;; C string).  It's not registered in the builtin table because
      ;; its C type name is "const char*".
      (and (eq? signature 'c-string) <c-string>)
      ;; Error
      (error "Unknown native type:" signature))]

    [_ (error "Invalid native type signature:" signature)]))

;; Reverse table: native-type instance -> C type name symbol
(define %native-type->cname
  (let1 tab (make-hash-table 'eq?)
    (hash-table-for-each
     (%builtin-native-type-table)
     (^[k v]
       (unless (memq v (list <c-string>))
         (hash-table-put! tab v k))))
    ;; c-string is registered with a string key, not a symbol
    (hash-table-put! tab <c-string> 'c-string)
    (hash-table-for-each
     (%extended-native-type-table)
     (^[k v]
       (hash-table-put! tab v k)))
    (^[type] (hash-table-get tab type #f))))

;; Reverse field specs: list of (name type offset) -> typed-var-list
(define (%unparse-field-specs fields)
  (append-map
   (^[f]
     (let ([name (car f)]
           [sig (native-type->signature (cadr f))])
       (if (symbol? sig)
         (list (string->symbol
                (string-append (symbol->string name) "::"
                               (symbol->string sig))))
         (list (string->symbol
                (string-append (symbol->string name) "::"))
               sig))))
   fields))

;; (native-type->signature native-type) => S-expression signature
;;
;; Reverse of native-type: given a <native-type> instance, produce the
;; S-expression signature that native-type would accept.
;; Pointer types use shortest representation (e.g. char**).
(define (native-type->signature type)
  (assume-type type <native-type>)
  (cond
    ;; Primitive types (including void and c-string)
    [(%native-type->cname type)]
    ;; Pointer: collect depth, build name*...*
    [(is-a? type <c-pointer>)
     (let loop ([t type] [depth 0])
       (if (is-a? t <c-pointer>)
         (loop (~ t'pointee-type) (+ depth 1))
         (let1 base (native-type->signature t)
           (string->symbol
            (string-append (x->string base)
                           (make-string depth #\*))))))]
    ;; Array
    [(is-a? type <c-array>)
     `(.array ,(native-type->signature (~ type'element-type))
              ,(~ type'dimensions))]
    ;; Struct
    [(is-a? type <c-struct>)
     `(.struct ,@(cond-list [(~ type'tag)])
               ,(%unparse-field-specs (~ type'fields)))]
    ;; Union
    [(is-a? type <c-union>)
     `(.union ,@(cond-list [(~ type'tag)])
              ,(%unparse-field-specs (~ type'fields)))]
    ;; Function
    [(is-a? type <c-function>)
     (let ([args (map native-type->signature (~ type'argument-types))]
           [ret (native-type->signature (~ type'return-type))])
       `(.function ,(if (~ type'variadic?)
                      (append args '(...))
                      args)
                   ,ret))]
    [else (error "Cannot produce signature for native type:" type)]))

;;;
;;; Native Wrapper
;;;

;; Native wrappers allow to access native aggregate objects as if they're
;; Gauche's objects.  For example, a native handle of (.struct (a::int b::int))
;; can beecome an instance with slot 'a and 'b.
;;
;; C-struct and c-union behave like <object>, whose fields can be accessed
;; like slots.  C-array behaves like a sequence.
;;
;; Note that there's no definite mappings between native types and Gauche
;; object system.  For example. the size of unbounded array in C type
;; can't be automatically determined.  What we provide is mainly for
;; the convenience to cover typical patterns, but you may need to use
;; underlying handles occasionally.
;;
;; One caveat is the difference of an aggregate object itself and a pointer
;; to an aggregate object.  In the native type system, these two are
;; distinguished.  Wrapped handle doesn't distinguish them.  At Gauche level,
;; they're mostly interchangeable, except when mutation is involved.

(define-class <native-wrapper-meta> (<class>)
  ((native-type :init-keyword :native-type)
   ;; Global table to keep class vs type correspondence
   (type->class :allocation :class
                :init-form (atom (make-hash-table 'eq?)))))

(define-class <native-wrapper-mixin> ()
  ((%handle :init-keyword :%handle))
  :metaclass <native-wrapper-meta>)

(define-inline %wh
  (getter-with-setter
   (^[wrapper] (slot-ref wrapper '%handle))
   (^[wrapper handle] (slot-set! wrapper '%handle handle))))

(define-class <wrapped-c-pointer> (<native-wrapper-mixin>)
  ())

(define-class <wrapped-c-array> (<sequence> <native-wrapper-mixin>)
  ())

(define-class <wrapped-c-struct> (<native-wrapper-mixin>)
  ())

(define-class <wrapped-c-union> (<native-wrapper-mixin>)
  ())

;; API
;;  Creates a class that wraps a native type TYPE.
;;  We keep 1-to-1 correspondence between the class and the type.
;; TODO: We may be able to do a better job to synthesize the default name.
(define (make-native-wrapper-class type :optional (name (gensym)))
  (or (atomic (class-slot-ref <native-wrapper-meta> 'type->class)
              (^t (hash-table-get t type #f)))
      (let1 c (%make-native-wrapper-class type name)
        (atomic (class-slot-ref <native-wrapper-meta> 'type->class)
                (^t (or (hash-table-get t type #f)
                        (begin (hash-table-put! t type c)
                               c)))))))

(define (%make-native-wrapper-class type name)
  (etypecase type
    [<c-struct>
     (make <native-wrapper-meta>
       :name name :supers (list <wrapped-c-struct>)
       :native-type type
       :slots (map (cut %build-slot type <>) (~ type'fields)))]
    [<c-union>
     (make <native-wrapper-meta>
       :name name :supers (list <wrapped-c-union>)
       :native-type type
       :slots (map (cut %build-slot type <>) (~ type'fields)))]
    [<c-array>
     (make <native-wrapper-meta>
       :name name :supers (list <wrapped-c-array>)
       :native-type type)]
    ))

(define (%build-slot type slot)
  (let ([slot-name (car slot)]
        [slot-type (cadr slot)])
    `(,slot-name :init-keyword ,(make-keyword slot-name)
                 :allocation :virtual
                 :slot-ref ,(^o (%wrap (native. (%wh o) slot-name)))
                 :slot-set! ,(^[o v] (set! (native. (%wh o) slot-name)
                                           (%unwrap v)))
                 :type ,slot-type)))

(define (%wrap value)
  (if (is-a? value <native-handle>)
    (cond [(null-pointer-handle? value) #f]
          [(and-let* ([ (c-pointer-handle? value) ]
                      [pt (c-pointer-type-pointee (native-handle-type value))]
                      [ (c-aggregate-type? pt) ])
             (wrap-native-handle (native* value)))]
          [else (wrap-native-handle value)])
    value))

(define (%unwrap value)
  (if (is-a? value <native-wrapper-mixin>)
    (%wh value)
    value))

(define-method make ((class <native-wrapper-meta>) . initargs)
  (let* ([storage (make-u8vector (~ class'native-type'size))]
         [handle (uvector->native-handle storage (~ class'native-type))]
         [instance (%wrap-native-handle class handle)])
    (initialize instance initargs)
    instance))

(define (wrap-native-handle handle)
  (let* ([type (~ handle'type)])
    (%wrap-native-handle (make-native-wrapper-class type) handle)))

(define (%wrap-native-handle class handle)
  (rlet1 instance (allocate-instance class '())
    (set! (%wh instance) handle)))

;; wrapped-c-array implements sequence protocol.
;; As a sequence, we only consider the first dimension.
;; However, ref generic function accepts multi-dimensional index
(define-method size-of ((wrapped <wrapped-c-array>))
  (let* ([h (%wh wrapped)]
         [dims (c-array-type-dimensions (native-handle-type h))])
    (if (integer? (car dims))
      (car dims)
      0)))

(define-method call-with-iterator ((coll <wrapped-c-array>) proc
                                   :allow-other-keys)
  (let ([i 0] [len (size-of coll)])
    (proc (^[] (>= i len)) (^[] (begin0 (ref coll i) (inc! i))))))

(define-method ref ((coll <wrapped-c-array>) (index <integer>))
  (%wrap (native-aref (%wh coll) (list index))))

(define-method (setter ref) ((coll <wrapped-c-array>) (index <integer>) val)
  (set! (native-aref (%wh coll) (list index)) (%unwrap val)))

(define-method ref ((coll <wrapped-c-array>) (indices <list>))
  (%wrap (native-aref (%wh coll) indices)))

(define-method (setter ref) ((coll <wrapped-c-array>) (indices <list>) val)
  (set! (native-aref (%wh coll) indices) (%unwrap val)))

(define-method referencer ((coll <wrapped-c-array>))
  (^[index] (ref coll index)))
(define-method modifier ((coll <wrapped-c-array>))
  (^[index val] (set! (ref coll index) val)))
