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
  (extend gauche.typeutil)              ;access internal routines
  (export native-ref
          native-set!)
  )
(select-module binary.ftype)

(inline-stub
 (.include "gauche/priv/typeP.h")

 ;; Access p[offset], where
 ;;   etype is the type of element
 ;;   fp is a foreign pointer for p
 ;;   offset is the element index
 (define-cfn %aref (element-type::ScmNativeType*
                    fp::ScmForeignPointer*
                    offset::ScmSmallInt)
   :static
   (let* ([p::void* (Scm_ForeignPointerRef fp)]
          [c-ref::(.function (p::void*)::ScmObj *)
                  (-> element-type c-ref)])
     (when (== c-ref NULL)
       (Scm_Error "Cannot dereference foreign pointer: %S" fp))
    (unless (== offset 0)
      (set! p (+ p (* offset (-> element-type size)))))
    (return (c-ref p))))

 ;; Set p[offset] = val
 (define-cfn %aset! (element-type::ScmNativeType*
                     fp::ScmForeignPointer*
                     offset::ScmSmallInt
                     val)
   ::void :static
   (let* ([p::void* (Scm_ForeignPointerRef fp)]
          [c-of-type::(.function (v::ScmObj)::int *) (-> element-type c-of-type)]
          [c-set::(.function (p::void* v::ScmObj)::void *) (-> element-type c-set)])
    (unless (c-of-type val)
      (Scm_Error "Invalid object to set to %S: %S" fp val))
    (when (== c-set NULL)
      (Scm_Error "Cannot set foreign pointer: %S" fp))
    (unless (== offset 0)
      (set! p (+ p (* offset (-> element-type size)))))
    (c-set p val)))
 )

;; native pointer type
;; type must be a subtype of <native-pointer>
(define-cproc %native-pointer-ref (type::<native-type>
                                   fp::<foreign-pointer>
                                   offset::<fixnum>)
  (let* ([inner::ScmNativeType* (Scm_NativePointerPointeeType type)])
    (%aref inner fp offset)))

(define-cproc %native-pointer-set! (type::<native-type>
                                    fp::<foreign-pointer>
                                    offset::<fixnum>
                                    val)
  ::<void>
  (let* ([inner::ScmNativeType* (Scm_NativePointerPointeeType type)])
    (%aset! inner fp offset val)))

(define-cproc %native-array-element-type (type::<native-type>)
  (return (SCM_OBJ (Scm_NativeArrayElementType type))))

(define-cproc %native-array-dimensions (type::<native-type>)
  (return (Scm_NativeArrayDimensions type)))

(define-cproc %native-array-ref (type::<native-type>
                                 fp::<foreign-pointer>
                                 offset::<fixnum>)
  (let* ([etype::ScmNativeType* (Scm_NativeArrayElementType type)])
    (%aref etype fp offset)))

(define-cproc %native-array-set! (type::<native-type>
                                  fp::<foreign-pointer>
                                  offset::<fixnum>
                                  val)
  (let* ([etype::ScmNativeType* (Scm_NativeArrayElementType type)])
    (%aset! etype fp offset val)))

(define (native-type-offset type selector)
  (assume-type type <native-type>)
  (cond
   [(subtype? type <native-pointer>)
    (assume-type selector <fixnum>)
    selector]
   [(subtype? type <native-array>)
    (let ([etype (%native-array-element-type type)]
          [dims (%native-array-dimensions type)])
      (unless (length=? selector dims)
        (errorf "Native array selector ~s doesn't match the dimensions ~s"
                selector dims))
      (unless (every (every-pred fixnum? positive?) selector)
        (error "Invalid native array selector:" selector))
      (let loop ([ds (reverse dims)]
                 [ss (reverse selector)]
                 [step 1]
                 [i 0])
        (cond [(null? ds) i]
              [(eq? (car ds) '*) ; this can only appear in the 1st dim
               (+ (car ss) i)]
              [(< (car ss) (car ds))
               (loop (cdr ds) (cdr ss) (* step (car ds))
                     (+ (* (car ss) step) i))]
              [else
               (error "Native array selector is out of range:" selector)])))]
   [else
    (error "Unsupported native aggregate type:" type)]))

(define (native-ref fp selector :optional (type #f))
  (assume-type fp <foreign-pointer>)
  (let1 t (or ((with-module gauche.internal foreign-pointer-type fp) fp)
              type)
    (unless t
      (error "Can't dereference a foreign pointer: type unknown:" fp))
    (let1 offset (native-type-offset t selector)
      (cond
       [(subtype? t <native-pointer>) (%native-pointer-ref t fp offset)]
       [(subtype? t <native-array>) (%native-array-ref t fp offset)]
       [else (error "Unsupported native aggregate type:" t)]))))

(define (native-set! fp selector val :optional (type #f))
  (assume-type fp <foreign-pointer>)
  (let1 t (or ((with-module gauche.internal foreign-pointer-type fp) fp)
              type)
    (unless t
      (error "Can't set a foreign pointer: type unknown:" fp))
    (let1 offset (native-type-offset t selector)
      (cond
       [(subtype? t <native-pointer>) (%native-pointer-set! t fp offset val)]
       [(subtype? t <native-array>) (%native-array-set! t fp offset val)]
       [else (error "Unsupported native aggregate type:" t)]))))
