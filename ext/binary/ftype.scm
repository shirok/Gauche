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
  (use gauche.cgen)
  (use gauche.record)
  (use gauche.uvector)
  (extend gauche.typeutil)              ;access internal routines
  (export native-ref
          native-set!

          uvector->native-handle

          ;; TRANSIENT: We want better (more concise, but distinct) names
          ;; for these.  At the moment, we just reexport them from
          ;; gauche.typeutil
          make-pointer-type
          make-native-function-type
          make-native-array-type
          make-native-struct-type
          make-native-union-type

          native-type

          native-alloc
          native-free))
(select-module binary.ftype)

(inline-stub
 (.include "gauche/priv/typeP.h")

 (declare-stub-type <native-handle> ScmNativeHandle*)
 )

(define (aggregate-type? type)
  (or (is-a? type <native-array>)
      (is-a? type <native-struct>)
      (is-a? type <native-union>)))

;;;
;;; Native handles
;;;

;; Native handles can be created with several ways.

;;
(define-cproc %uvector->native-handle (uv::<uvector>
                                       type::<native-type>
                                       offset::<fixnum>)
  (let* ([p::void* (SCM_UVECTOR_ELEMENTS uv)]
         [vecsize::ScmSmallInt (Scm_UVectorSizeInBytes uv)]
         [datasize::ScmSmallInt (-> type size)]
         [max::void* (+ p (Scm_UVectorSizeInBytes uv))])
    (unless (and (<= 0 offset) (<= (+ offset datasize) vecsize))
      (Scm_Error "Offset %ld out of range, or type size %ld too big."
                 offset datasize))
    (return
     (Scm__MakeNativeHandle (+ p offset)
                            type
                            (-> type name) ; temporary
                            p
                            max
                            (SCM_OBJ uv)
                            SCM_NIL
                            0))))

(define (uvector->native-handle uv type :optional (offset 0))
  (assume-type uv <uvector>)
  (assume (or (is-a? type <native-pointer>)
              (aggregate-type? type))
    "Type must be native pointer or aggregate type, but got:" type)
  (%uvector->native-handle uv type offset))

;; The handle pointing into a region originally pointed
(define-cproc make-internal-handle (base::<native-handle>
                                    offset::<fixnum>
                                    :optional (type::<native-type>? NULL))
  (let* ([p::void* (+ (-> base ptr) offset)]
         [t::ScmNativeType* (?: type type (-> base type))])
    (unless (and (<= (-> base region-min) p)
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

;;;
;;; Low-level accessor/modifier
;;;

;; Access handle's ptr + offset
(define-cproc %pref (element-type::<native-type>
                     handle::<native-handle>
                     offset::<fixnum>)
  (let* ([p::void* (+ (-> handle ptr) offset)]
         [c-ref::(.function (p::void*)::ScmObj *)
                 (-> element-type c-ref)])
    (when (== c-ref NULL)
      (Scm_Error "Cannot dereference type %S" element-type))
    (unless (and (<= (-> handle region-min) p)
                 (<  p (-> handle region-max)))
      (Scm_Error "Offset out of range: %ld" offset))
    (return (c-ref p))))

;; Unified set: set handle's ptr + offset = val
(define-cproc %pset! (element-type::<native-type>
                      handle::<native-handle>
                      offset::<fixnum>
                      val)
  ::<void>
  (let* ([p::void* (+ (-> handle ptr) offset)]
         [c-of-type::(.function (v::ScmObj)::int *) (-> element-type c-of-type)]
         [c-set::(.function (p::void* v::ScmObj)::void *) (-> element-type c-set)])
    (unless (c-of-type val)
      (Scm_Error "Invalid object to set to %S: %S" handle val))
    (when (== c-set NULL)
      (Scm_Error "Cannot set value of type %S" element-type))
    (unless (and (<= (-> handle region-min) p)
                 (<  p (-> handle region-max)))
      (Scm_Error "Offset out of range: %ld" offset))
    (c-set p val)))

(define (%handle-ref type handle offset)
  (if (aggregate-type? type)
    (make-internal-handle handle offset type)
    (%pref type handle offset)))

(define (%handle-set! type handle offset val)
  (when (aggregate-type? type)
    ;; For now, we reject it.  Technically we can copy aggregate type
    ;; content into the target aggregate.
    (errorf "Can't set a value of type ~s into ~s" type handle))
  (%pset! type handle offset val))

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
(define (%handle-type handle type-override)
  (or type-override
      (~ handle'type)
      (error "Can't dereference a pointer with unknown type:" handle)))

;;;
;;;  Public accessor/modifier
;;;

(define (native-ref handle selector :optional (type #f))
  (assume-type handle <native-handle>)
  (let* ([t (%handle-type handle type)]
         [offset (native-type-offset t selector)])
    (typecase t
      [<native-pointer>
       (%handle-ref (~ t'pointee-type) handle offset)]
      [<native-array>
       (%handle-ref (%array-dereference-type t selector) handle offset)]
      [(</> <native-struct> <native-union>)
       (let1 ftype (cadr (native-struct-field t selector))
         (%handle-ref ftype handle offset))]
      [else (error "Unsupported native aggregate type:" handle)])))

(define (native-set! handle selector val :optional (type #f))
  (assume-type handle <native-handle>)
  (let* ([t (%handle-type handle type)]
         [offset (native-type-offset t selector)])
    (typecase t
      [<native-pointer>
       (%handle-set! (~ t'pointee-type) handle offset val)]
      [<native-array>
       (%handle-set! (%array-dereference-type t selector) handle offset val)]
      [(</> <native-struct> <native-union>)
       (let1 ftype (cadr (native-struct-field t selector))
         (%handle-set! ftype handle offset val))]
      [else (error "Unsupported native aggregate type:" t)])))

;; Compare native types
(define-method object-equal? ((s <native-pointer>) (t <native-pointer>))
  (equal? (~ s'pointee-type) (~ t'pointee-type)))

(define-method object-equal? ((s <native-function>) (t <native-function>))
  (and (equal? (~ s'return-type) (~ t'return-type))
       (equal? (~ s'arg-types) (~ t'arg-types))
       (equal? (~ s'varargs) (~ t'varargs))))

(define-method object-equal? ((s <native-array>) (t <native-array>))
  (and (equal? (~ s'element-type) (~ t'element-type))
       (equal? (~ s'dimensions) (~ t'dimensions))))

(define-method object-equal? ((s <native-struct>) (t <native-struct>))
  (and (equal? (~ s'tag) (~ t'tag))
       (equal? (~ s'fields) (~ t'fields))))

(define-method object-equal? ((s <native-union>) (t <native-union>))
  (and (equal? (~ s'tag) (~ t'tag))
       (equal? (~ s'fields) (~ t'fields))))

;;;
;;;  Convert type signatures to native-type instance
;;;

;; Check if a symbol name ends with '*', indicating a pointer type.
;; Returns (base-symbol . depth) or #f.
(define (%pointer-type-decompose sym)
  (and-let1 m (#/^([^*]*)(\*+)$/ (symbol->string sym))
    `(,(string->symbol (m 1)) . ,(string-length (m 2)))))

;; Wrap a native type in N layers of pointer type.
(define (%wrap-pointer-type base-type depth)
  (let loop ([t base-type] [d depth])
    (if (= d 0)
      t
      (loop (make-pointer-type t) (- d 1)))))

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
;;   (native-type int*)                  => (make-pointer-type <int>)
;;   (native-type char**)               => (make-pointer-type (make-pointer-type <int8>))
;;   (native-type (.array int (3)))     => (make-native-array-type <int> '(3))
;;   (native-type (.array char (2 3)))  => (make-native-array-type <int8> '(2 3))
;;   (native-type (.struct foo (a::int b::double)))
;;     => (make-native-struct-type 'foo `((a ,<int>) (b ,<double>)))
;;   (native-type (.struct foo ((a::(.array char (8))))))
;;     => (make-native-struct-type 'foo `((a ,(make-native-array-type <int8> '(8)))))
;;   (native-type (.union u1 (a::int b::float)))
;;     => (make-native-union-type 'u1 `((a ,<int>) (b ,<float>)))
;;   (native-type (.function (int int) double))
;;     => (make-native-function-type <double> `(,<int> ,<int>))
;;   (native-type (.function (int char* ...) void))
;;     => (make-native-function-type <void> `(,<int> ,(make-pointer-type <int8>) ...))
;;
(define (native-type signature)
  (match signature
    ;; Pass-through if already a native type instance
    [(? (cut is-a? <> <native-type>)) signature]

    ;; Compound types

    ;; (.array element-type (dim ...))
    [('.array etype (dims ...))
     (make-native-array-type (native-type etype) dims)]

    ;; (.struct tag (field-specs ...))
    [('.struct (? symbol? tag) (field-specs ...))
     (make-native-struct-type tag (%parse-field-specs field-specs))]
    ;; (.struct (field-specs ...))  -- anonymous
    [('.struct ((? (^x (or (symbol? x) (pair? x))) field-specs) ...))
     (make-native-struct-type #f (%parse-field-specs field-specs))]

    ;; (.union tag (field-specs ...))
    [('.union (? symbol? tag) (field-specs ...))
     (make-native-union-type tag (%parse-field-specs field-specs))]
    ;; (.union (field-specs ...))  -- anonymous
    [('.union ((? (^x (or (symbol? x) (pair? x))) field-specs) ...))
     (make-native-union-type #f (%parse-field-specs field-specs))]

    ;; (.function (arg-types ...) return-type)
    [('.function (arg-types ...) ret-type)
     (make-native-function-type
      (native-type ret-type)
      (map (^[a] (if (eq? a '...) '... (native-type a)))
           arg-types))]

    ;; We ignore 'const' for now
    [('const type)
     (native-type type)]

    ;; Simple symbol types
    [(? symbol?)
     (or
      ;; Direct table lookup
      (%builtin-native-type-lookup signature)
      ;; Pointer type (trailing *)
      (and-let* ([decomp (%pointer-type-decompose signature)])
        (%wrap-pointer-type (native-type (car decomp)) (cdr decomp)))
      ;; Error
      (error "Unknown native type:" signature))]

    [_ (error "Invalid native type signature:" signature)]))

;;;
;;;  Raw memory
;;;

;; This should actually belong to FFI module.  For now, however,
;; since this depends heavily on native compound types, we put it
;; here for experimenting.

(define-cproc %native-alloc (size::<fixnum> type::<native-type>)
  (let* ([p::void* (malloc size)]
         [attrs (SCM_LIST1 (Scm_Cons 'malloc '#t))])
    (when (== p NULL)
      (Scm_Error "malloc failed (size %ld\n)" size))
    (return
     (Scm__MakeNativeHandle p
                            type
                            (-> type name)
                            p
                            (+ p size)
                            SCM_UNDEFINED
                            attrs
                            0))))

(define (native-alloc size-or-type)
  (assume-type size-or-type (</> <fixnum> <native-type>))
  (let ([realsize (typecase size-or-type
                    [<fixnum> size-or-type]
                    [<native-type> (~ size-or-type'size)])]
        [ptype (cond
                [(aggregate-type? size-or-type) size-or-type]
                [(is-a? size-or-type <native-type>)
                 (make-pointer-type size-or-type)]
                [else (make-pointer-type <void>)])])
    (%native-alloc realsize ptype)))

(define-cproc native-free (handle::<native-handle>) ::<void>
  (when (SCM_FALSEP (Scm_Assq 'malloc (-> handle attrs)))
    (Scm_Error "Attempt to free a handle which is not malloc'ed: %S"
               handle))
  (unless (SCM_FALSEP (Scm_Assq 'freed (-> handle attrs)))
    (Scm_Error "Attempt to free an already-freed handle: %S"
               handle))
  (set! (-> handle attrs)
        (Scm_Acons 'freed '#t (-> handle attrs)))
  (free (-> handle ptr)))
