;;;
;;; gauche.ctype - C types and native handles
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

(define-module gauche.ctype
  (use util.match)
  (use gauche.cgen.type.parse)
  (use gauche.uvector)
  (extend gauche.typeutil)              ;access internal routines
  (export native-ref
          native-set!

          uvector->native-handle

          ;; TRANSIENT: We want better (more concise, but distinct) names
          ;; for these.  At the moment, we just reexport them from
          ;; gauche.typeutil
          make-c-pointer-type
          make-c-function-type
          make-c-array-type
          make-c-struct-type
          make-c-union-type

          native-type
          native-type->signature

          native-alloc
          native-free))
(select-module gauche.ctype)

(inline-stub
 (.include "gauche/priv/typeP.h")

 (declare-stub-type <native-handle> ScmNativeHandle*)
 )

(define (aggregate-type? type)
  (or (is-a? type <c-array>)
      (is-a? type <c-struct>)
      (is-a? type <c-union>)))

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
  (assume (or (is-a? type <c-pointer>)
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
         [c-ref::(.function (t::ScmNativeType* p::void*)::ScmObj *)
                 (-> element-type c-ref)])
    (when (== c-ref NULL)
      (Scm_Error "Cannot dereference type %S" element-type))
    (unless (and (<= (-> handle region-min) p)
                 (<  p (-> handle region-max)))
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
      (Scm_Error "Invalid object to set to %S: %S" handle val))
    (when (== c-set NULL)
      (Scm_Error "Cannot set value of type %S" element-type))
    (unless (and (<= (-> handle region-min) p)
                 (<  p (-> handle region-max)))
      (Scm_Error "Offset out of range: %ld" offset))
    (c-set element-type p val)))

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
     (assume-type selector <c-fixnum>)
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

(define (native-ref handle selector :optional (type #f))
  (assume-type handle <native-handle>)
  (let* ([t (%handle-type handle type)]
         [offset (native-type-offset t selector)])
    (typecase t
      [<c-pointer>
       (%handle-ref (~ t'pointee-type) handle offset)]
      [<c-array>
       (%handle-ref (%array-dereference-type t selector) handle offset)]
      [(</> <c-struct> <c-union>)
       (let1 ctype (cadr (c-struct-field t selector))
         (%handle-ref ctype handle offset))]
      [else (error "Unsupported native aggregate type:" handle)])))

(define (native-set! handle selector val :optional (type #f))
  (assume-type handle <native-handle>)
  (let* ([t (%handle-type handle type)]
         [offset (native-type-offset t selector)])
    (typecase t
      [<c-pointer>
       (%handle-set! (~ t'pointee-type) handle offset val)]
      [<c-array>
       (%handle-set! (%array-dereference-type t selector) handle offset val)]
      [(</> <c-struct> <c-union>)
       (let1 ctype (cadr (c-struct-field t selector))
         (%handle-set! ctype handle offset val))]
      [else (error "Unsupported native aggregate type:" t)])))

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
;;   (native-type int)                   => <c-int>
;;   (native-type int*)                  => (make-c-pointer-type <c-int>)
;;   (native-type char**)               => (make-c-pointer-type (make-c-pointer-type <c-int8>))
;;   (native-type (.array int (3)))     => (make-c-array-type <c-int> '(3))
;;   (native-type (.array char (2 3)))  => (make-c-array-type <c-int8> '(2 3))
;;   (native-type (.struct foo (a::int b::double)))
;;     => (make-c-struct-type 'foo `((a ,<c-int>) (b ,<c-double>)))
;;   (native-type (.struct foo ((a::(.array char (8))))))
;;     => (make-c-struct-type 'foo `((a ,(make-c-array-type <c-int8> '(8)))))
;;   (native-type (.union u1 (a::int b::float)))
;;     => (make-c-union-type 'u1 `((a ,<c-int>) (b ,<c-float>)))
;;   (native-type (.function (int int) double))
;;     => (make-c-function-type <c-double> `(,<c-int> ,<c-int>))
;;   (native-type (.function (int char* ...) void))
;;     => (make-c-function-type <void> `(,<c-int> ,(make-c-pointer-type <c-int8>) ...))
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
    [('.struct ((? (^x (or (symbol? x) (pair? x))) field-specs) ...))
     (make-c-struct-type #f (%parse-field-specs field-specs))]

    ;; (.union tag (field-specs ...))
    [('.union (? symbol? tag) (field-specs ...))
     (make-c-union-type tag (%parse-field-specs field-specs))]
    ;; (.union (field-specs ...))  -- anonymous
    [('.union ((? (^x (or (symbol? x) (pair? x))) field-specs) ...))
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

    ;; C-style list type form, e.g. (char*), (char *), (char const*),
    ;; (int **), (int* *).  Strip const and concatenate into a single
    ;; symbol, then re-parse.
    [((? symbol?) . (? (^r (every symbol? r))))
     (native-type (%normalize-c-type-list signature))]

    ;; Simple symbol types
    [(? symbol?)
     (or
      ;; Direct table lookup
      (%builtin-native-type-lookup signature)
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
  (rlet1 ht (make-hash-table 'eq?)
    (for-each (^[cname]
                (and-let1 t (%builtin-native-type-lookup cname)
                  (hash-table-put! ht t cname)))
              '(short u_short int u_int long u_long
                int8_t uint8_t int16_t uint16_t
                int32_t uint32_t int64_t uint64_t
                char size_t ssize_t ptrdiff_t
                off_t intptr_t uintptr_t
                float double void))
    ;; c-string is registered with a string key, not a symbol
    (hash-table-put! ht <c-string> 'c-string)))

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
    [(hash-table-get %native-type->cname type #f)]
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
     (let ([args (map native-type->signature (~ type'arg-types))]
           [ret (native-type->signature (~ type'return-type))])
       `(.function ,(if (~ type'varargs)
                      (append args '(...))
                      args)
                   ,ret))]
    [else (error "Cannot produce signature for native type:" type)]))

;;;
;;;  Raw memory
;;;

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
  (assume-type size-or-type (</> <c-fixnum> <native-type>))
  (let ([realsize (typecase size-or-type
                    [<c-fixnum> size-or-type]
                    [<native-type> (~ size-or-type'size)])]
        [ptype (cond
                [(aggregate-type? size-or-type) size-or-type]
                [(is-a? size-or-type <native-type>)
                 (make-c-pointer-type size-or-type)]
                [else (make-c-pointer-type <void>)])])
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
