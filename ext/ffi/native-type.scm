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
  (use gauche.dictionary)
  (use gauche.threads)
  (use gauche.uvector)
  (extend gauche.typeutil)              ;access internal routines
  (export make-c-pointer-type
          make-c-function-type
          make-c-array-type
          make-c-struct-type
          make-c-union-type
          make-c-enum-type

          c-pointer-type-pointee
          c-array-type-dimensions
          c-function-type-return-type
          c-function-type-argument-types
          c-function-type-variadic?
          c-struct/union-type-tag
          c-struct/union-type-field-names
          c-struct/union-type-field-type
          c-struct/union-type-field-offset
          c-enum-type-tag
          c-enum-type-enumerator-alist
          c-enum-value
          c-enum-symbol

          c-pointer-type?
          c-array-type?
          c-function-type?
          c-struct-type?
          c-union-type?
          c-struct/union-type?
          c-enum-type?
          c-aggregate-type?
          c-pointer-like-type?

          make-native-tag-namespace
          current-native-tag-namespace
          tag->native-type

          native-handle-type
          native-handle-owner
          native-handle-belongs?
          native-pointer+
          native*
          native-aref
          native.
          native->
          native&

          make-native-handle
          make-c-array-handle
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
          copy-handle-memory!

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
;;; Tag namespace registry
;;;

;; Tag namespace is an opaque dictionary to map struct/union/enum tags
;; (symbol) to native-type.

(define (make-native-tag-namespace :optional (parent #f))
  (if parent
    (make-stacked-map (make-hash-table 'eq?) parent)
    (make-stacked-map (make-hash-table 'eq?))))

;; Current namespace - by default it's #f, making the native
;; aggregates not registered.
(define current-native-tag-namespace
  (make-parameter #f))

(define (tag->native-type tag :optional (ns #f))
  (and-let1 registry (or ns (current-native-tag-namespace))
    (dict-get registry tag #f)))

(define (%register-native-tag! tag type)
  ;; TODO: We need to allow registering incomplete types.  Then it should
  ;; be allowed to replace or augument it with a complete type.
  (and-let* ([ tag ]
             [registry (current-native-tag-namespace)])
    (if-let1 t (dict-get registry tag #f)
      (unless (equal? t type)
        (errorf "struct/union/enum tag ~s is already used by ~s" tag type))
      (set! (dict-get registry tag) type)))
  type)

;;;
;;; Aggregate type constructors
;;;

(define (make-c-pointer-type pointee-type)
  (assume-type pointee-type <native-type>)
  (let* ([bare-name (regexp-replace* (symbol->string (~ pointee-type'name))
                                     #/^</ ""
                                     #/>$/ "")]
         [pointer-name #"<~|bare-name|*>"])
    (%make-c-pointer-type pointer-name pointee-type)))

;; Argument-types are list of native types, optionally end with
;; a symbol ... for varargs.
(define (make-c-function-type return-type argument-types)
  (assume-type return-type <native-type>)
  (receive (arg-types variadic?)
      (if (and (pair? argument-types) (eq? (last argument-types) '...))
        (values (drop-right argument-types 1) #t)
        (values argument-types #f))
    (dolist [arg-type arg-types]
      (assume-type arg-type <native-type>))
    (%make-c-function-type
     (format "~{ ~a~}~:[~; ...~] -> ~a"
             (map (cut ~ <>'name) arg-types) variadic?
             (~ return-type'name))
     return-type arg-types variadic?)))

(define (make-c-array-type element-type dimensions)
  ;; allow single integer or '* for 1-dim array
  (define dims (if (list? dimensions) dimensions (list dimensions)))
  (assume-type element-type <native-type>)
  (let loop ([dims dims])
    (cond [(null? dims)]
          [(not (pair? dims))
           (error "Bad native array dimensions; must be a proper list, but got:"
                  dimensions)]
          [(and (eq? dims dimensions)
                (eq? (car dims) '*))
           (loop (cdr dims))]   ;first dimension can be *
          [(and (exact-integer? (car dims))
                (>= (car dims) 0))
           (loop (cdr dims))]
          [else
           (error "Bad native array dimensions; must be a list of nonnegative \
                   integers, or '* at the last position, but got:"
                  dimensions)]))
  (let ([name (format "~a~a" (~ element-type 'name) dims)]
        [num-elts (if (eq? (car dims) '*)
                    0                   ; unknown sized array
                    (fold * 1 dims))]
        [elt-size (~ element-type'size)])
    (%make-c-array-type name element-type
                             (* elt-size num-elts)
                             (~ element-type'alignment)
                             dims)))

(define (struct-size-roundup size alignment)
  (* alignment (quotient (+ size alignment -1) alignment)))

(define (make-c-struct/union-type tag fields struct?)
  (let loop ([fs fields] [offset 0] [alignment 1] [descs '()] [bounded #t])
    (match fs
      [()
       (let* ([size (struct-size-roundup offset alignment)]
              [tname (if struct? "struct" "union")]
              [name (if tag
                      #"~tname ~tag"
                      #"~tname anonymous")]) ;TODO: Give better name
         ($ %register-native-tag! tag
            (%make-c-struct/union-type
             (if struct? <c-struct> <c-union>)
             name size alignment (if bounded 1 0) tag (reverse descs))))]
      [(((? symbol? fname) ftype) . rest)
       (assume-type ftype <native-type>)
       (when (and (not bounded) struct?)
         (error "Struct type can have unbounded field only at the end:" fs))
       (let* ([falign (~ ftype'alignment)]
              [foffset (if struct?
                         (struct-size-roundup offset falign)
                         0)]
              [next (if struct?
                      (+ foffset (~ ftype'size))
                      (max offset (~ ftype'size)))]
              [new-align (max alignment falign)])
         (loop rest
               next
               new-align
               (cons (list fname ftype foffset) descs)
               (and bounded (~ ftype'bounded?))))]
      [_
       (error "Bad native struct fields; must be a proper list, but got:"
              fields)])))

(define (make-c-struct-type tag fields)
  (make-c-struct/union-type tag fields #t))

(define (make-c-union-type tag fields)
  (make-c-struct/union-type tag fields #f))

;; make-c-enum-type tag typespec (enumerator ...)
;;   enumerator : symbol | (symbol integer-value)
(define (make-c-enum-type tag typespec enumerators)
  ;; NB: We can't use <?> type constructor yet
  (assume (or (not tag) (symbol? tag))
    "c-enum tag must be a symbol or #f, but got:" tag)
  (assume (or (not typespec) (is-a? typespec <native-type>))
    "c-enum typespec must be a native-type or #f, but got:" typespec)
  (let* ([alist (%build-enum-alist enumerators)]
         [vals (map cdr alist)])
    (receive (underlying size alignment)
        (cond
         [typespec
          (unless (%native-type-integral? typespec)
            (error "c-enum typespec must be an integral native type, but got:"
                   typespec))
          (dolist [v vals]
            (unless (of-type? v typespec)
              (error "enumerator value out of range for the enum type:" v)))
          (values typespec (~ typespec'size) (~ typespec'alignment))]
         [(null? vals)
          ;; TODO: We should support incomplete declaration.
          (error "cannot determine c-enum size: give a typespec or \
                  at least one enumerator")]
         [else
          (receive (lo hi) (apply min&max vals)
            (let1 size (implicit-enum-size (%enum-bit-width lo hi))
              (values (%native-int-type-of-size size (not (negative? lo)))
                      size size)))])
      (let ([cname (cond [(and tag typespec)
                          #"enum ~tag : ~(~ typespec'c-type-name)"]
                         [tag #"enum ~tag"]
                         [else (~ underlying'c-type-name)])]
            [pname (if tag #"enum ~tag" "enum anonymous")])
        ($ %register-native-tag! tag
           (%make-c-enum-type pname cname underlying size alignment
                              tag typespec alist))))))

;;;
;;; Additonal native type predicates/accessors
;;;

(define-inline (c-pointer-type? type) (is-a? type <c-pointer>))
(define-inline (c-array-type? type) (is-a? type <c-array>))
(define-inline (c-function-type? type) (is-a? type <c-function>))
(define-inline (c-struct-type? type) (is-a? type <c-struct>))
(define-inline (c-union-type? type) (is-a? type <c-union>))
(define-inline (c-enum-type? type) (is-a? type <c-enum>))

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

(define-inline (c-enum-type-tag type)
  (assume-type type <c-enum>)
  (~ type'tag))

(define-inline (c-enum-type-enumerator-alist type)
  (assume-type type <c-enum>)
  (~ type'enumerator-alist))

(define-inline (c-enum-value type symbol)
  (assume-type type <c-enum>)
  (assume-type symbol <symbol>)
  (or (alist-ref (~ type'enumerator-alist) symbol)
      (errorf "Invalid enum symbol ~s for ~s" symbol type)))

(define-inline (c-enum-symbol type value)
  (assume-type type <c-enum>)
  (assume-type value <integer>)
  (or (alist-key (~ type'enumerator-alist) value)
      (errorf "Invalid enum value ~s for ~s" value type)))

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

;; for debugging
(define-cproc native-handle-dump (handle::<native-handle>) ::<void>
  (Scm_Printf SCM_CUROUT "Native handle @%p\n" (-> handle ptr))
  (Scm_Printf SCM_CUROUT "    type: %S\n" (-> handle type))
  (Scm_Printf SCM_CUROUT "  region: %p-%p\n"
              (-> handle region-min) (-> handle region-max))
  (Scm_Printf SCM_CUROUT "    name: %S\n" (-> handle name))
  (Scm_Printf SCM_CUROUT "   atrts: %S\n" (-> handle attrs))
  (Scm_Printf SCM_CUROUT "   flags: %16x\n" (-> handle flags))
  (Scm_Printf SCM_CUROUT "   owner: %#65.1S\n" (-> handle owner)))

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

(define (make-native-handle type :optional (init #f) (offset 0))
  (assume (or (c-pointer-type? type)
              (c-aggregate-type? type))
    "Type must be native pointer or aggregate type, but got:" type)
  (assume-type init (<?> (</> <uvector> <string>))
    "Init argument must be a string or a uvector, but got:" init)
  (assume (or init (zero? offset))
    "Non-zero offset is not allowed with uvector auto allocation:" offset)
  (let1 buf (cond [(string? init) (string->u8vector init)]
                  [(uvector? init) init]
                  [else (make-u8vector
                         (if (c-pointer-type? type)
                           (~ (c-pointer-type-pointee type)'size)
                           (~ type'size)))])
    (%uvector->native-handle buf type offset)))

;; To be removed
(define (uvector->native-handle uv type :optional (offset 0))
  (make-native-handle type uv offset))

(define (make-c-array-handle element-type dims
                             :optional (init #f) (offset 0))
  (assume element-type <native-type>)
  (make-native-handle (make-c-array-type element-type dims) init offset))

;; The handle pointing into a region originally pointed
(define-cproc make-internal-handle (base::<native-handle>
                                    offset::<fixnum>
                                    :optional (type::<native-type>? #f))
  (let* ([p::void* (+ (-> base ptr) offset)]
         [t::ScmNativeType* (?: type type (-> base type))])
    (when (and (!= (-> base region-min) NULL)
               (!= (-> base region-max) NULL)
               (not (and
                     (<= (-> base region-min) p)
                     (<=  p (-> base region-max))))) ;allows "past-end" pointer
      (Scm_Error "Offset out of range: %S %ld" base offset))
    (return
     (Scm__MakeNativeHandle p
                            t
                            (-> t name) ; temporary
                            (-> base region-min)
                            (-> base region-max)
                            (-> base owner)
                            SCM_NIL
                            0))))

;; If this handle is created by uvector->native-handle, the original uvector
;; is returned.  Even if the handle is created with offset, the rturned
;; object is the "whole" one.  Returns #f if the pointer came from
;; elsewhere.  Note that it may return other than uvector or #f,
;; if we ever allow handles points into other Gauche objects.
(define-cproc native-handle-owner (handle::<native-handle>)
  (let* ([r (-> handle owner)])
    (if (SCM_UNDEFINEDP r)
      (return SCM_FALSE)
      (return r))))

(define-cfn %native-handle-has-region? (h::ScmNativeHandle*) ::int
  (return (not (or (== (-> h region-min) NULL)
                   (== (-> h region-max) NULL)))))

(define-cfn %native-handle-belongs? (pointer::ScmNativeHandle*
                                     body::ScmNativeHandle*)
  ::int
  (cond
   [(not (%native-handle-has-region? body)) (return FALSE)]
   [(and (<= (-> body region-min) (-> pointer ptr))
         (<= (-> pointer ptr) (-> body region-max))) ;include one-past-end pointer
    (return TRUE)]
   [else (return FALSE)]))

(define-cproc native-handle-belongs? (pointer::<native-handle>
                                      body::<native-handle>)
  ::<boolean>
  (return (%native-handle-belongs? pointer body)))

(define-cproc native-handle-difference (a::<native-handle>
                                        b::<native-handle>)
  (cond
   [(== (-> a ptr) NULL)
    (return (?: (==  (-> b ptr) NULL) (SCM_MAKE_INT 0) SCM_FALSE))]
   [(== (-> b ptr) NULL) (return SCM_FALSE)]
   [(%native-handle-has-region? a)
    (if (%native-handle-belongs? b a)
      (return (Scm_PtrdiffToInteger (- (-> a ptr) (-> b ptr))))
      (return SCM_FALSE))]
   [(%native-handle-has-region? b)
    (if (%native-handle-belongs? a b)
      (return (Scm_PtrdiffToInteger (- (-> a ptr) (-> b ptr))))
      (return SCM_FALSE))]
   [else
    ;; If both handle don't have region info, we compare bare pointer value.
    (return (Scm_PtrdiffToInteger (- (-> a ptr) (-> b ptr))))]))

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

;; We can 'cast' a pointer-like handle to a different pointer-like type,
;; or aggregate handle to a different aggregate type.
;; Arrays are both pointer-like and aggregate, so arrays can be cast
;; either to a pointer-like type or to an aggregate type.
;; Offset is a byte offset in relative to the original pointer---only to
;; be used by those who know what they're doing.
;;
;; If the destination type is not a c-pointer, and the source handle has
;; a known memory region, the region must cover the destination type's
;; size (starting at ptr+offset).

(define-syntax cast-handle
  (er-macro-transformer
   (^[f r c]
     (define (quote? x) (c (r'quote) x))
     (match f
       [(_ ((? quote?) signature) handle . opts)
        (quasirename r
          `(cast-handle-proc (native-type ',signature) ,handle ,@opts))]
       [(_ type handle . opts)
        (quasirename r
          `(cast-handle-proc ,type ,handle ,@opts))]))))

(define (cast-handle-proc type handle :optional (offset 0))
  (assume-type type <native-type>)
  (assume-type handle <native-handle>)
  (assume-type offset <fixnum>)
  (let1 htype (native-handle-type handle)
    (unless (or (and (c-pointer-like-type? htype)
                     (c-pointer-like-type? type))
                (and (c-aggregate-type? htype)
                     (c-aggregate-type? type)))
      (errorf "cast must be between pointer-like types or between aggregate \
               types, but got source of type ~s and destination of type ~s"
              htype type)))
  (%cast-handle type handle offset))

(define-cproc %cast-handle (type::<native-type>
                            handle::<native-handle>
                            offset::<fixnum>)
  (let* ([p::void* (+ (-> handle ptr) offset)])
    ;; If destination type is not a c-pointer, ensure the source handle's
    ;; known memory region (if any) covers the destination type's size.
    (when (and (not (SCM_C_POINTER_P type))
               (!= (-> handle region-min) NULL)
               (!= (-> handle region-max) NULL))
      (let* ([dsize::ScmSmallInt (-> type size)])
        (unless (and (<= (-> handle region-min) p)
                     (<= (+ p dsize) (-> handle region-max)))
          (Scm_Error "cast destination type %S (size %ld) at offset %ld \
                      does not fit within source handle's memory region: %S"
                     (SCM_OBJ type) dsize offset (SCM_OBJ handle)))))
    (return (Scm__MakeNativeHandle p
                                   type
                                   (-> handle name)
                                   (-> handle region-min)
                                   (-> handle region-max)
                                   (-> handle owner)
                                   (-> handle attrs)
                                   (-> handle flags)))))

;; Copy memory pointed by src-handle into the memory pointed by dst-handle.
;; The source region begins at src.ptr; if size is #f, the entire region
;; from src.ptr+offset up to src's region-max is copied (src must have
;; known region bounds in that case).  If dst has known region bounds, the
;; copy must fit within them.
(define (copy-handle-memory! dst-handle src-handle
                             :optional (size #f) (offset 0))
  (assume-type dst-handle <native-handle>)
  (assume-type src-handle <native-handle>)
  (assume (or (not size)
              (and (fixnum? size) (>= size 0)))
    "size must be a nonnegative integer or #f, but got:" size)
  (assume (and (fixnum? offset) (>= offset 0))
    "offset must be a nonnegative integer, but got:" offset)
  (%pcopy! dst-handle 0 src-handle offset (or size -1)))

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
               (== offset 0)
               (== (-> handle ptr) (-> handle region-max)))
      ;; We allow a pointer points right past the end of array.
      ;; They are valid for pointer arithmetic, but cannot be dereferenced.
      ;; We distinguish it from offset-out-of-bound error.
      (Scm_Error "Past-end pointer cannot be dereferenced: %S" handle))
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
               (== offset 0)
               (== (-> handle ptr) (-> handle region-max)))
      ;; We allow a pointer points right past the end of array.
      ;; They are valid for pointer arithmetic, but cannot be dereferenced.
      ;; We distinguish it from offset-out-of-bound error.
      (Scm_Error "Past-end pointer cannot be used for setting: %S" handle))
    (when (and (!= (-> handle region-max) NULL)
               (!= (-> handle region-min) NULL)
               (not (and (<= (-> handle region-min) p)
                         (<  p (-> handle region-max)))))
      (Scm_Error "Offset out of range: %ld" offset))
    (c-set element-type p val)))

;; Verbatim memcpy of `size` bytes from src.ptr+src-offset into
;; dst.ptr+dst-offset, with bounds validation against both handles' regions
;; when known.  If size is -1, the size is derived from size of src's type,
;; minus offset.
(define-cproc %pcopy! (dst::<native-handle>
                       dst-offset::<fixnum>
                       src::<native-handle>
                       src-offset::<fixnum>
                       size::<fixnum>)
  ::<void>
  (when (== (-> dst ptr) NULL)
    (Scm_Error "Attempt to copy into NULL pointer: %S" dst))
  (when (== (-> src ptr) NULL)
    (Scm_Error "Attempt to copy from NULL pointer: %S" src))
  (when (< size -1)
    (Scm_Error "Invalid size: %ld" size))
  (let* ([dst-ptr::void* (+ (-> dst ptr) dst-offset)]
         [src-ptr::void* (+ (-> src ptr) src-offset)]
         [src-region-known?::int
          (and (!= (-> src region-min) NULL)
               (!= (-> src region-max) NULL))]
         [dst-region-known?::int
          (and (!= (-> dst region-min) NULL)
               (!= (-> dst region-max) NULL))]
         [actual-size::ScmSmallInt size])
    ;; Derive size from src region when -1 is given.
    (when (< actual-size 0)
      (set! actual-size (- (-> (-> src type) size) src-offset))
      (when (< actual-size 0)
        (Scm_Error "src-offset %ld is too large for source type size: %S"
                   src-offset src)))
    ;; Validate source region when known.
    (when (and src-region-known?
               (not (and (<= (-> src region-min) src-ptr)
                         (<= (+ src-ptr actual-size)
                             (-> src region-max)))))
      (Scm_Error "src-offset %ld + size %ld out of range for %S"
                 src-offset actual-size src))
    ;; Validate destination region when known.
    (when (and dst-region-known?
               (not (and (<= (-> dst region-min) dst-ptr)
                         (<= (+ dst-ptr actual-size)
                             (-> dst region-max)))))
      (Scm_Error "dst-offset %ld + size %ld out of range for %S"
                 dst-offset actual-size dst))
    (memcpy dst-ptr src-ptr actual-size)))

(define (%handle-ref type handle offset)
  (if (c-aggregate-type? type)
    (make-internal-handle handle offset type)
    (%pref type handle offset)))

(define (%handle-set! type handle offset val)
  (cond
   [(c-aggregate-type? type)
    ;; Verbatim copy of the aggregate value.  val must be a native handle
    ;; whose type matches the destination type.
    (unless (is-a? val <native-handle>)
      (errorf "A native handle of type ~s is required to set into ~s, \
               but got: ~s"
              type handle val))
    (unless (equal? type (native-handle-type val))
      (errorf "Type mismatch: can't set a value of type ~s into a field \
               of type ~s"
              (native-handle-type val) type))
    (%pcopy! handle offset val 0 (~ type'size))]
   [else
    (%pset! type handle offset val)]))

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
     ;; Selector is a single element index, passed as a one-element list.
     (match selector
       [((? fixnum? i)) (* i (~ type'pointee-type'size))]
       [_
        (error "Pointer dereference requires a single integer index, but got:"
               selector)])]
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
(define (native-aref handle . indices)
  (assume-type handle <native-handle>)
  (assume-type indices (<List> <fixnum> 1))
  (let* ([t (%handle-type handle #f)]
         [offset (native-type-offset t indices)])
    (etypecase t
      [<c-array>
       (%handle-ref (%array-dereference-type t indices) handle offset)]
      [<c-pointer>
       (%handle-ref (~ t 'pointee-type) handle offset)])))

(define (%native-aref-set! handle indices val)
  (assume-type handle <native-handle>)
  (assume-type indices (<List> <fixnum> 1))
  (let* ([t (%handle-type handle #f)]
         [offset (native-type-offset t indices)])
    (etypecase t
      [<c-array>
       (%handle-set! (%array-dereference-type t indices) handle offset val)]
      [<c-pointer>
       (%handle-set! (~ t'pointee-type) handle offset val)])))
(set! (setter native-aref)
      (^[handle . args]
        (when (null? args)
          (error "native-aref setter requires at least one index and a value"))
        (%native-aref-set! handle (drop-right args 1) (last args))))

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
(define (native& handle :optional (selector #f))
  (define (struct-field& type)
    (assume-type selector <symbol>
      "native& on c-struct or c-union requires a symbol selector, but got;"
      selector)
    (let ([off (native-type-offset type selector)]
          [etype (c-struct/union-type-field-type type selector)])
      (make-internal-handle handle off (make-c-pointer-type etype))))
  (define (array& type)
    (assume-type selector (</> <fixnum> (<List> <fixnum>))
      "native& on c-struct or c-union requires a symbol selector, but got;"
      selector)
    (let ([off (native-type-offset type (if (list? selector)
                                          selector
                                          (list selector)))])
      (make-internal-handle handle off
                            (make-c-pointer-type (~ type'element-type)))))

  (define (bad)
    (error "native& requires aggregate type ahndle, or \
            a pointer handle to aggregate type, but got:"
           handle))
  (assume-type handle <native-handle>)
  (let1 type (~ handle'type)
    (typecase type
      [(</> <c-struct> <c-union>)
       (if selector
         (struct-field& type)
         (make-internal-handle handle 0 (make-c-pointer-type type)))]
      [<c-array>
       (if selector
         (array& type)
         (make-internal-handle handle 0 (make-c-pointer-type type)))]
      [<c-pointer>
       (if selector
         ;; We let native& handle both &(foo.field) and &(foo->field)
         (typecase (c-pointer-type-pointee type)
           [(</> <c-struct> <c-union>) => struct-field&]
           [<c-array> => array&]
           [else (bad)])
         (error "native& on c-pointer type needs a selector:" handle))]
      [else (bad)])))

;; handle can be c-pointer handle or c-array handle.  For the latter, it is
;; cast to the pointer handle, like C.
(define (native-pointer+ handle element-offset)
  (assume-type handle <native-handle>)
  (let1 t (~ handle'type)
    (etypecase t
      [<c-pointer>
       (let1 pt (~ t'pointee-type)
         (make-internal-handle handle
                               (* element-offset (~ pt'size))
                               (~ handle'type)))]
      [<c-array>
       (let* ([et (~ t'element-type)]
              [dims (~ t'dimensions)]
              [pt (if (length=? dims 1)
                    (make-c-pointer-type et)
                    (make-c-pointer-type (make-c-array-type et (cdr dims))))])
         (native-pointer+ (cast-handle pt handle) element-offset))])))

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
  (match syms
    [((or 'signed 'unsigned) . rest)
     `(,(car syms) ,(%normalize-c-type-list rest))]
    [('const . rest)
     (%normalize-c-type-list rest)]
    [_
     ($ string->symbol $ apply string-append
        (filter-map (^s (rxmatch-case (symbol->string s)
                          [#/^const(\**)$/ (#f ptrs) ptrs]
                          [else => values]))
                    syms))]))

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

;; Handle signed/unsigned qualifier
(define (%attach-signedness natype signedness)
  (define (%s orig-type signed-type unsigned-type)
    (and (eqv? natype orig-type)
         (ecase signedness
           [(signed) signed-type]
           [(unsigned) unsigned-type])))
  (cond
   [(not signedness) natype]
   [(and (%native-type-integral? natype)
         (or (%s <c-char> <int8> <uint8>) ; map signed/unsigned char to [u]int8
             (%s <short> <short> <ushort>)
             (%s <ushort> <short> <ushort>)
             (%s <int> <int> <uint>)
             (%s <uint> <int> <uint>)
             (%s <long> <long> <ulong>)
             (%s <ulong> <long> <ulong>)
             (%s <int8> <int8> <uint8>)
             (%s <uint8> <int8> <uint8>)
             (%s <int16> <int16> <uint16>)
             (%s <uint16> <int16> <uint16>)
             (%s <int32> <int32> <uint32>)
             (%s <uint32> <int32> <uint32>)
             (%s <int64> <int64> <uint64>)
             (%s <uint64> <int64> <uint64>)
             (%s <size_t> <ssize_t> <size_t>)
             (%s <ssize_t> <ssize_t> <size_t>)
             (%s <int16-be> <int16-be> <uint16-be>)
             (%s <uint16-be> <int16-be> <uint16-be>)
             (%s <int32-be> <int32-be> <uint32-be>)
             (%s <uint32-be> <int32-be> <uint32-be>)
             (%s <int64-be> <int64-be> <uint64-be>)
             (%s <uint64-be> <int64-be> <uint64-be>)
             (%s <int16-le> <int16-le> <uint16-le>)
             (%s <uint16-le> <int16-le> <uint16-le>)
             (%s <int32-le> <int32-le> <uint32-le>)
             (%s <uint32-le> <int32-le> <uint32-le>)
             (%s <int64-le> <int64-le> <uint64-le>)
             (%s <uint64-le> <int64-le> <uint64-le>)))
    ;; let unrecognized base type case handled by else clause
    ]
   [(c-pointer-type? natype)
    (make-c-pointer-type (%attach-signedness (c-pointer-type-pointee natype)
                                             signedness))]
   [else (errorf "~a can't be attached to ~s" signedness natype)]))

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
;;   (native-type (.enum color (red green blue)))
;;     => (make-c-enum-type 'color #f '(red green blue))
;;   (native-type (.enum flag : uint8 (a (b 4) c)))
;;     => (make-c-enum-type 'flag <uint8> '(a (b 4) c))
;;
;; Parse the part following '.enum: [tag] [: signature] (enumerator ...)
(define (%parse-enum-spec rest)
  (match rest
    [(enumerators)
     (values #f #f enumerators)]
    [(': sig enumerators)
     (values #f (native-type sig) enumerators)]
    [((? symbol? tag) enumerators)
     (values tag #f enumerators)]
    [((? symbol? tag) ': sig enumerators)
     (values tag (native-type sig) enumerators)]
    [_ (error "Invalid .enum signature:" (cons '.enum rest))]))

(define (native-type signature)
  (let rec ((signature signature)
            (signedness #f))
    (define (no-signedness)
      (when signedness
        (errorf "~a can't be used with ~s" signature signedness)))

    (match signature
      ;; Pass-through if already a native type instance
      [(? (cut is-a? <> <native-type>))
       (%attach-signedness signature signedness)]

      ;; Compound types

      ;; (.array element-type (dim ...))
      [('.array etype (dims ...))
       (no-signedness)
       (make-c-array-type (native-type etype) dims)]

      ;; (.struct tag (field-specs ...))
      [('.struct (? symbol? tag) (field-specs ...))
       (no-signedness)
       (make-c-struct-type tag (%parse-field-specs field-specs))]
      ;; (.struct (field-specs ...))  -- anonymous
      [('.struct (field-specs ...))
       (no-signedness)
       (make-c-struct-type #f (%parse-field-specs field-specs))]

      ;; (.union tag (field-specs ...))
      [('.union (? symbol? tag) (field-specs ...))
       (no-signedness)
       (make-c-union-type tag (%parse-field-specs field-specs))]
      ;; (.union (field-specs ...))  -- anonymous
      [('.union (field-specs ...))
       (no-signedness)
       (make-c-union-type #f (%parse-field-specs field-specs))]

      ;; (.enum [tag] [: signature] (enumerator ...))
      [('.enum . rest)
       (no-signedness)
       (receive (tag typespec enumerators) (%parse-enum-spec rest)
         (make-c-enum-type tag typespec enumerators))]

      ;; (.function (arg-types ...) return-type)
      [('.function (arg-types ...) ret-type)
       (no-signedness)
       (make-c-function-type
        (native-type ret-type)
        (map (^[a] (if (eq? a '...) '... (native-type a)))
             arg-types))]

      ;; We ignore 'const' for now
      [('const type)
       (rec type signedness)]

      ;; Signedness
      [((and (or 'signed 'unsigned) s) type)
       (no-signedness)
       (rec type s)]

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

      [((? list? main-t) suffix ...) (=> fail)
       (no-signedness)
       (let* ([suff (apply string-append (map x->string suffix))]
              [ptrs (regexp-replace-all* suff #/const/ "")]
              [m (#/^\**$/ ptrs)])
         (if m
           (let add-pointer ([t (rec main-t #f)]
                             [n (string-length (m 0))])
             (if (zero? n) t (add-pointer (make-c-pointer-type t) (- n 1))))
           (fail)))]

      ;; C-style list type form, e.g. (char*), (char *), (char const*),
      ;; (int **), (int* *).  Strip const and concatenate into a single
      ;; symbol, then re-parse.
      [((? symbol?) . (? (^r (every symbol? r))))
       (native-type (%normalize-c-type-list signature))]

      ;; Simple symbol types
      [(? symbol?)
       (%attach-signedness
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
         (error "Unknown native type:" signature))
        signedness)]

      [_ (error "Invalid native type signature:" signature)])))

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

;; Reverse enumerator alist ((id . value) ...) -> (enumerator ...), using the
;; bare-symbol form when the value matches the running auto-increment (0, then
;; previous+1), and (id value) otherwise.  Round-trips through native-type.
(define (%unparse-enumerators alist)
  (let loop ([as alist] [next 0] [acc '()])
    (if (null? as)
      (reverse acc)
      (let ([id (caar as)] [val (cdar as)])
        (loop (cdr as) (+ val 1)
              (cons (if (= val next) id (list id val)) acc))))))

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
    ;; Enum
    [(is-a? type <c-enum>)
     `(.enum ,@(cond-list [(~ type'tag)])
             ,@(cond-list [(~ type'type-spec)
                           @ `(: ,(native-type->signature (~ type'type-spec)))])
             ,(%unparse-enumerators (~ type'enumerator-alist)))]
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
  (define (make-class type)
    (typecase type
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
      [else #f]))
  (or (cond
       [(c-pointer-type? type)
        (make-class (c-pointer-type-pointee type))]
       [(c-aggregate-type? type)
        (make-class type)]
       [else #f])
      (error "Cannot wrap a native handle with type" type)))

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
    (wrap-native-handle value)
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

;; API
(define (wrap-native-handle handle)
  (assume-type handle <native-handle>)
  (cond [(null-pointer-handle? handle) #f]
        [(and-let* ([ (c-pointer-handle? handle) ]
                    [pt (c-pointer-type-pointee (native-handle-type handle))]
                    [ (c-aggregate-type? pt) ])
           (%wrap-native-handle (make-native-wrapper-class pt)
                                (native* handle)))]
        [else (%wrap-native-handle (make-native-wrapper-class
                                    (native-handle-type handle))
                                   handle)]))

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
  (%wrap (native-aref (%wh coll) index)))

(define-method (setter ref) ((coll <wrapped-c-array>) (index <integer>) val)
  (set! (native-aref (%wh coll) index) (%unwrap val)))

(define-method ref ((coll <wrapped-c-array>) (indices <list>))
  (%wrap (apply native-aref (%wh coll) indices)))

(define-method (setter ref) ((coll <wrapped-c-array>) (indices <list>) val)
  (apply (setter native-aref) (%wh coll)
         (append indices (list (%unwrap val)))))

(define-method referencer ((coll <wrapped-c-array>))
  (^[index] (ref coll index)))
(define-method modifier ((coll <wrapped-c-array>))
  (^[index val] (set! (ref coll index) val)))
