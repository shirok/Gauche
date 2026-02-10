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
  (use gauche.uvector)
  (extend gauche.typeutil)              ;access internal routines
  (export native-ref
          native-set!
          native-pointer+

          make-domestic-pointer
          domestic-pointer-storage
          domestic-pointer-pos
          domestic-pointer-type

          ;; TRANSIENT: We want better (more concise, but distinct) names
          ;; for these.  At the moment, we just reexport them from
          ;; gauche.typeutil
          make-pointer-type
          ;make-native-functon-type
          make-native-array-type
          make-native-struct-type
          make-native-union-type))
(select-module binary.ftype)

(inline-stub
 (.include "gauche/priv/typeP.h")
 )

;;;
;;; Internal pointers
;;;

;; The pointers pointing into a structure which is originally pointed by
;; foreign pointers obtained form outside (e.g. dlsym()).

(inline-stub
 (define-cvar internal-pointer-class::ScmClass* :static)

 (initcode
  (set! internal-pointer-class
        (Scm_MakeForeignPointerClass (Scm_CurrentModule)
                                     "<internal-pointer>"
                                     NULL NULL 0))
  ))

(define-cproc %make-internal-pointer (base::<foreign-pointer>
                                       offset::<fixnum>)
  (let* ([p::void* (Scm_ForeignPointerRef base)])
    (return (Scm_MakeForeignPointer internal-pointer-class
                                    (+ p offset)))))

;; Returns a new foreign pointer derived from BASE, with byte offset
;; OFFSET and type TYPE.
(define (make-internal-pointer base offset type)
  (assume-type base <foreign-pointer>)
  (assume-type offset <fixnum>)
  (assume-type type <native-type>)
  (rlet1 sndptr (%make-internal-pointer base offset)
    ((with-module gauche.internal foreign-pointer-type-set!)
     sndptr type)))

;;;
;;; Pointer equivalent into bytevector storage
;;;

;; Used to point to a specific location within the given bytevector.
(define-record-type <domestic-pointer>
    (%make-domestic-pointer storage pos type)
    domestic-pointer?
  (storage domestic-pointer-storage)
  (pos domestic-pointer-pos)
  (type domestic-pointer-type))

;; Returns a new domestic pointer points to the byte offset POS
;; into a bytevector BYTEVECTOR, with type TYPE.
(define (make-domestic-pointer bytevector pos :optional (type #f))
  (assume-type bytevector <u8vector>)
  (assume (and (fixnum? pos) (>= pos 0)))
  (assume-type type (<?> <native-type>))
  (unless (ineq 0 <= pos < (u8vector-length bytevector))
    (error "Domestic pointer position out of range:" pos))
  (%make-domestic-pointer bytevector pos type))

(define-method write-object ((obj <domestic-pointer>) port)
  (format port "#<domestic-pointer ~a[~a]>"
          (domestic-pointer-type obj)
          (domestic-pointer-pos obj)))

;;;
;;; Low-level accessor/modifier
;;;

(define (aggregate-type? type)
  (or (is-a? type <native-array>)
      (is-a? type <native-struct>)
      (is-a? type <native-union>)))

;; Access p+offset, where
;;   etype is the type of element
;;   fp is a foreign pointer for p
;;   offset is the byte offset
(define-cproc %fpref (element-type::<native-type>
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

;; Set p+offset = val
(define-cproc %fpset! (element-type::<native-type>
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
      (set! p (+ p offset)))
    (c-set p val)))

;; Similar, for bytevector access
(define-cproc %dpref (element-type::<native-type>
                      bv::<u8vector>
                      start::<fixnum>
                      offset::<fixnum>)
  (let* ([p::uint8_t* (SCM_U8VECTOR_ELEMENTS bv)]
         [c-ref::(.function (p::void*)::ScmObj *)
                 (-> element-type c-ref)])
    (when (== c-ref NULL)
      (Scm_Error "Cannot dereference type %S" element-type))
    (return (c-ref (+ p (+ start offset))))))

(define-cproc %dpset! (element-type::<native-type>
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
    (c-set (+ p start offset) val)))

(define (%pref type ptr offset)
  (etypecase ptr
    [<foreign-pointer>
     (if (aggregate-type? type)
       (make-internal-pointer ptr offset type)
       (%fpref type ptr offset))]
    [<domestic-pointer>
     (if (aggregate-type? type)
       (make-domestic-pointer (domestic-pointer-storage ptr) offset type)
       (%dpref type
               (domestic-pointer-storage ptr)
               (domestic-pointer-pos ptr)
               offset))]))

(define (%pset! type ptr offset val)
  (when (aggregate-type? type)
    ;; For now, we reject it.  Technically we can copy aggregate type
    ;; content into the target aggregate.
    (errorf "Can't set a value of type ~s into ~s" type ptr))
  (etypecase ptr
    [<foreign-pointer> (%fpset! type ptr offset val)]
    [<domestic-pointer> (%dpset! type
                                 (domestic-pointer-storage ptr)
                                 (domestic-pointer-pos ptr)
                                 offset val)]))

(define (native-struct-field type selector)
  (assume-type type (</> <native-struct> <native-union>))
  (assume-type selector <symbol>)
  (if-let1 field (assq selector (~ type'fields))
    field
    (errorf "Unknown native struct field ~s for ~s" selector type)))

;; Returns a byte offset
(define (native-type-offset type selector)
  (assume-type type <native-type>)
  (typecase type
    [<native-pointer>
     ;; Selector is an element index
     (assume-type selector <fixnum>)
     (* selector (~ type'pointee-type'size))]
    [<native-array>
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
    [(</> <native-struct> <native-union>)
     (caddr (native-struct-field type selector))]
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
          (make-native-array-type etype dims)
          etype)
        (loop (cdr dims) (cdr sels))))))

;; Common routine to allow type override.
;; ptr can be <foreign-pointer> or <domestic-pointer>.
(define (%ptr-type ptr type-override)
  (or type-override
      (etypecase ptr
        [<foreign-pointer>
         ((with-module gauche.internal foreign-pointer-type) ptr)]
        [<domestic-pointer>
         (domestic-pointer-type ptr)])
      (error "Can't dereference a pointer with unknown type:" ptr)))

;;;
;;;  Public accessor/modifier
;;;

;; NB: We refrain from using (</> <foreign-pointer> <domestic-pointer>))
;; for now, because of the issue of precompiler
;; https://github.com/shirok/Gauche/issues/1209

(define (native-ref ptr selector :optional (type #f))
  ;;(assume-type ptr (</> <foreign-pointer> <domestic-pointer>))
  (assume (or (is-a? ptr <foreign-pointer>)
              (is-a? ptr <domestic-pointer>))
    "Foreign pointer of domestic pointer expected, but got:" ptr)
  (let* ([t (%ptr-type ptr type)]
         [offset (native-type-offset t selector)])
    (typecase t
      [<native-pointer>
       (%pref (~ t'pointee-type) ptr offset)]
      [<native-array>
       (%pref (%array-dereference-type t selector) ptr offset)]
      [(</> <native-struct> <native-union>)
       (let1 ftype (cadr (native-struct-field t selector))
         (%pref ftype ptr offset))]
      [else (error "Unsupported native aggregate type:" ptr)])))

(define (native-set! ptr selector val :optional (type #f))
  ;;(assume-type ptr (</> <foreign-pointer> <domestic-pointer>))
  (assume (or (is-a? ptr <foreign-pointer>)
              (is-a? ptr <domestic-pointer>))
    "Foreign pointer of domestic pointer expected, but got:" ptr)
  (let* ([t (%ptr-type ptr type)]
         [offset (native-type-offset t selector)])
    (typecase t
      [<native-pointer>
       (%pset! (~ t'pointee-type) ptr offset val)]
      [<native-array>
       (%pset! (%array-dereference-type t selector) ptr offset val)]
      [(</> <native-struct> <native-union>)
       (let1 ftype (cadr (native-struct-field t selector))
         (%pset! ftype ptr offset val))]
      [else (error "Unsupported native aggregate type:" t)])))

(define (native-pointer+ ptr delta :optional (type #f))
  ;;(assume-type ptr (</> <foreign-pointer> <domestic-pointer>))
  (assume (or (is-a? ptr <foreign-pointer>)
              (is-a? ptr <domestic-pointer>))
    "Foreign pointer of domestic pointer expected, but got:" ptr)
  (assume-type delta <fixnum>)
  (let* ([t (%ptr-type ptr type)]
         [offset (* delta (~ t'pointee-type'size))])
    (typecase ptr
      [<foreign-pointer>
       (make-internal-pointer ptr offset t)]
      [<domestic-pointer>
       (make-domestic-pointer (domestic-pointer-storage ptr)
                              (+ (domestic-pointer-pos ptr) offset)
                              t)])))
