;;;
;;; binary.ftype - foreign types and foreign objects
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
;; compound binary types.
;;
;; NB: This module has never been documented, and will likely to be
;; subsumed by FFI module.  Any code here is for experiment to explore
;; effective interface.

(define-module binary.ftype
  (use util.match)
  (use gauche.record)
  (extend gauche.typeutil)              ;access internal routines
  (export native-ref
          native-set!
          native-bytevector-ref
          native-bytevector-set!

          make-bytevector-cursor
          bytevector-cursor-storage
          bytevector-cursor-pos
          bytevector-cursor-type

          ;; TRANSIENT: We want better (more concise, but distinct) names
          ;; for these.  At the moment, we just reexport them from
          ;; gauche.typeutil
          make-pointer-type
          ;make-native-functon-type
          make-native-array-type))
(select-module binary.ftype)

;; Used to point to a specific location within the given bytevector.
(define-record-type <bytevector-cursor>
    (%make-bytevector-cursor storage pos type)
    bytevector-cursor?
  (storage bytevector-cursor-storage)
  (pos bytevector-cursor-pos)
  (type bytevector-cursor-type))

(define (make-bytevector-cursor bytevector pos :optional (type #f))
  (assume-type bytevector <u8vector>)
  (assume (and (fixnum? pos) (>= pos 0)))
  (assume-type type (<?> <native-type>))
  (%make-bytevector-cursor bytevector pos type))

(inline-stub
 (.include "gauche/priv/typeP.h")

 ;; Access p[offset], where
 ;;   etype is the type of element
 ;;   fp is a foreign pointer for p
 ;;   offset is the element index
 (define-cproc %aref (element-type::<native-type>
                      fp::<foreign-pointer>
                      offset::<fixnum>)
   (let* ([p::void* (Scm_ForeignPointerRef fp)]
          [c-ref::(.function (p::void*)::ScmObj *)
                  (-> element-type c-ref)])
     (when (== c-ref NULL)
       (Scm_Error "Cannot dereference type %S" element-type))
     (unless (== offset 0)
       (set! p (+ p (* offset (-> element-type size)))))
     (return (c-ref p))))

 ;; Set p[offset] = val
 (define-cproc %aset! (element-type::<native-type>
                       fp::<foreign-pointer>
                       offset::<fixnum>
                       val)
   ::<void>
   (let* ([p::void* (Scm_ForeignPointerRef fp)]
          [c-of-type::(.function (v::ScmObj)::int *) (-> element-type c-of-type)]
          [c-set::(.function (p::void* v::ScmObj)::void *) (-> element-type c-set)])
    (unless (c-of-type val)
      (Scm_Error "Invalid object to set to %S: %S" fp val))
    (when (== c-set NULL)
      (Scm_Error "Cannot set value of type %S" element-type))
    (unless (== offset 0)
      (set! p (+ p (* offset (-> element-type size)))))
    (c-set p val)))

 ;; Similar, for bytevector access
 (define-cproc %bvref (element-type::<native-type>
                       bv::<u8vector>
                       start::<fixnum>
                       offset::<fixnum>)
   (let* ([p::uint8_t* (SCM_U8VECTOR_ELEMENTS bv)]
          [c-ref::(.function (p::void*)::ScmObj *)
                  (-> element-type c-ref)])
     (when (== c-ref NULL)
       (Scm_Error "Cannot dereference type %S" element-type))
     (return (c-ref (+ p (+ start (* offset (-> element-type size))))))))

 (define-cproc %bvset! (element-type::<native-type>
                        bv::<u8vector>
                        start::<fixnum>
                        offset::<fixnum>
                        val)
   ::<void>
   (let* ([p::uint8_t* (SCM_U8VECTOR_ELEMENTS bv)]
          [c-of-type::(.function (v::ScmObj)::int *) (-> element-type c-of-type)]
          [c-set::(.function (p::void* v::ScmObj)::void *) (-> element-type c-set)])
    (unless (c-of-type val)
      (Scm_Error "Invalid object to set to %S: %S" bv val))
    (when (== c-set NULL)
      (Scm_Error "Cannot set value of type %S" element-type))
    (c-set (+ p (+ start (* offset (-> element-type size)))) val)))
 )

(define-cproc %native-pointer-pointee-type (type::<native-type>)
  (return (SCM_OBJ (Scm_NativePointerPointeeType type))))

(define-cproc %native-array-element-type (type::<native-type>)
  (return (SCM_OBJ (Scm_NativeArrayElementType type))))

(define-cproc %native-array-dimensions (type::<native-type>)
  (return (Scm_NativeArrayDimensions type)))

(define (native-type-offset type selector)
  (assume-type type <native-type>)
  (cond
   [(subtype? type <native-pointer>)
    (assume-type selector <fixnum>)
    selector]
   [(subtype? type <native-array>)
    (unless (every (every-pred fixnum? (complement negative?)) selector)
      (error "Invalid native array selector:" selector))
    (let* ([etype (%native-array-element-type type)]
           [dims (%native-array-dimensions type)]
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
        (cond [(null? ds) i]
              [(eq? (car ds) '*) ; this can only appear in the 1st dim
               (+ (* (car ss) step) i)]
              [(< (car ss) (car ds))
               (loop (cdr ds) (cdr ss) (* step (car ds))
                     (+ (* (car ss) step) i))]
              [else
               (error "Native array selector is out of range:" selector)])))]
   [else
    (error "Unsupported native aggregate type:" type)]))

;; Returns an array type if selector does not specify a single element
;; in the array.
(define (%partial-array-dereference-type atype selector)
  (let1 etype (%native-array-element-type atype)
    (let loop ([dims (%native-array-dimensions atype)]
               [sels selector])
      (if (null? sels)
        (and (not (null? dims))
             (make-native-array-type etype dims))
        (loop (cdr dims) (cdr sels))))))

(define (native-ref fp selector :optional (type #f))
  (assume-type fp <foreign-pointer>)
  (let1 t (or ((with-module gauche.internal foreign-pointer-type fp) fp)
              type)
    (unless t
      (error "Can't dereference a foreign pointer: type unknown:" fp))
    (let1 offset (native-type-offset t selector)
      (cond
       [(subtype? t <native-pointer>)
        (%aref (%native-pointer-pointee-type t) fp offset)]
       [(subtype? t <native-array>)
        (if-let1 partial (%partial-array-dereference-type t selector)
          (error "[Internal] Partial array reference is not yet supported \
                  for foreign pointer:" selector)
          (%aref (%native-array-element-type t) fp offset))]
       [else (error "Unsupported native aggregate type:" t)]))))

(define (native-set! fp selector val :optional (type #f))
  (assume-type fp <foreign-pointer>)
  (let1 t (or ((with-module gauche.internal foreign-pointer-type fp) fp)
              type)
    (unless t
      (error "Can't set a foreign pointer: type unknown:" fp))
    (let1 offset (native-type-offset t selector)
      (cond
       [(subtype? t <native-pointer>)
        (%aset! (%native-pointer-pointee-type t) fp offset val)]
       [(subtype? t <native-array>)
        (if-let1 partial (%partial-array-dereference-type t selector)
          (error "Setting an array element needs to specify exactly one element:"
                 selector)
          (%aset! (%native-array-element-type t) fp offset val))]
       [else (error "Unsupported native aggregate type:" t)]))))

(define (native-bytevector-ref bvcursor type selector)
  (assume-type bvcursor <bytevector-cursor>)
  (assume-type type (<?> <native-type>))
  (let1 t (or type (bytevector-cursor-type bvcursor))
    (unless t
      (error "Unknown type to dereference:" bvcursor))
    (let1 offset (native-type-offset t selector)
      (cond
       [(subtype? t <native-pointer>)
        (%bvref (%native-pointer-pointee-type t)
                (bytevector-cursor-storage bvcursor)
                (bytevector-cursor-pos bvcursor)
                offset)]
       [(subtype? t <native-array>)
        (if-let1 partial (%partial-array-dereference-type t selector)
          (make-bytevector-cursor (bytevector-cursor-storage bvcursor)
                                  offset
                                  partial)
          (%bvref (%native-array-element-type t)
                  (bytevector-cursor-storage bvcursor)
                  (bytevector-cursor-pos bvcursor)
                  offset))]
       [else (error "Unsupported native aggregate type:" t)]))))

(define (native-bytevector-set! bvcursor type selector val)
  (assume-type bvcursor <bytevector-cursor>)
  (assume-type type (<?> <native-type>))
  (let1 t (or type (bytevector-cursor-type bvcursor))
    (unless t
      (error "Unknown type to dereference:" bvcursor))
    (let1 offset (native-type-offset t selector)
      (cond
       [(subtype? t <native-pointer>)
        (%bvset! (%native-pointer-pointee-type t)
                 (bytevector-cursor-storage bvcursor)
                 (bytevector-cursor-pos bvcursor)
                 offset val)]
       [(subtype? t <native-array>)
        (if-let1 partial (%partial-array-dereference-type t selector)
          (error "Setting an array element needs to specify exactly one element:"
                 selector)
          (%bvset! (%native-array-element-type t)
                   (bytevector-cursor-storage bvcursor)
                   (bytevector-cursor-pos bvcursor)
                   offset val))]
       [else (error "Unsupported native aggregate type:" t)]))))
