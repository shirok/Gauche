;;;
;;; libtype.scm - type-related stuff
;;;
;;;   Copyright (c) 2021-2025  Shiro Kawai  <shiro@acm.org>
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

;; This must be the first form to prevent generation of *.sci file
(declare)

;; In Gauche, types are a data structure that appear in both compile-time
;; and run-time, and describe metalevel properties of run-time data.
;;
;; Gauche has two kinds of types--prescriptive types and descriptive types.
;; Prescriptive types are the types that are used to generate
;; actual data---we also call it classes.  Descriptive types are, otoh,
;; used only to describe the nature of data at the certain point of program
;; execution---for example, you may say the argument must be either <integer>
;; or <boolean>.  The descriptive type can't be used to generate an instance,
;; only to be used to validate and/or infer the actual (generative) type of
;; data.
;;
;; Descriptive types can be constructed by type constructors.  Since the
;; compiler needs to know about types, type constructor expressions
;; are evaluated at the compile time, much like macros.
;;
;; We could've implemented types with macros, but it would be tricky.  Unlike
;; macros, type expression needs to evaluate from inside to outside, just
;; like ordinary expressions.  It's more like compile-time constant folding.
;;
;; Since type handing is deeply intertwined with compiler, we let the compiler
;; recognize type expression specifically, rather than reusing existing
;; evaluation mechanism.  When the compile sees (C x ...) and C has an
;; inlineable binding to an instance of <type-constructor-meta>, it recognizes
;; type expression.
;;
;; We have several kinds of descriptive types.
;;
;;   Constraint types
;;      - We don't have a separate class for this, but this is a kind of
;;        descriptive type that constraints the type the concerned
;;        value can take, e.g. (</> <integer> <string>).   Created by
;;        a type constructor.   Currently, the main use is in `assert-type`.
;;
;;   <native-type>
;;      - A subset of Scheme primitive types that can map onto the native
;;        types.  E.g. <long> is a subset of <integer> but can map onto
;;        the underlying `long` integer value.   Main use is for FFI.
;;
;;   <proxy-type>
;;      - This is a wrapper of an prescriptive types, and as far as type
;;        checking is concerened, it behaves just like the wrapped type.
;;        We need this because classes can be redefined.  When a class
;;        is redefined, a new class instance is created and the class name
;;        is rebound to it.  We want that other aggregate descriptive types
;;        also start referring to the new class, so a descriptive type can't
;;        hold a direct reference to a prescriptive type; instead, we refer
;;        to it through a proxy type.  A proxy type always refer to the
;;        the redefined class.
;;        Proxy types are automatically created and handled under the hood;
;;        users doesn't need to deal with them explicity.


;; This module is not meant to be `use'd.   It is just to hide
;; auxiliary procedures from the rest of the system.  The necessary
;; bindings are injected into 'gauche' module at the initialization time.
(define-module gauche.typeutil)
(select-module gauche.typeutil)
(use util.match)


(inline-stub
 (.include "gauche/priv/configP.h"
           "gauche/priv/classP.h"
           "gauche/priv/nativeP.h"
           "gauche/priv/memoP.h"
           "gauche/priv/typeP.h")

 ;; Metaclass: <type-constructor-meta>
 ;;   Its instance is ScmTypeConstructor.  Provides the following slots.
 ;;   (We don't use generic functions, for they are also called from C runtime).
 ;;
 ;;   initializer :: <descriptive-type> args -> <descriptive-type>
 ;;     Called from a type constructor that fills the descriptive type.
 ;;     It is called from either a compiler or an initcode of precompiled code.
 ;;     Note that a raw class object should never passed in the args---the
 ;;     compiler wraps class objects with a proxy type.  The initializer
 ;;     should raise an error if a class object is given.
 ;;     This procedure must be pure.  We may memoize the constructor result.
 ;;   deconstructor :: <descriptive-type> -> (arg ...)
 ;;     Returns a list of arguments such that when they are passed to the
 ;;     constructor, an equivalent descriptive type is constructed again.
 ;;     This is called from the precompiler to serialize the descriptive type.
 ;;   validator :: <descriptive-type> obj -> <boolean>
 ;;     Returns true iff obj is of type <descriptive-type>.  Called from
 ;;     `of-type?`.
 ;;   subtype? :: <descriptive-type> type -> <boolean>|'super
 ;;     Returns one of three values:  #t if the descriptive type is a subtype
 ;;     of TYPE, which may be a class or another descriptive type.  #f if
 ;;     the descriptive type is definitely not a subtype.  A symbol 'super
 ;;     if you need to ask TYPE.
 ;;     Note that proxy types and native types are already excluded, as well
 ;;     as the reflective case (subtype? x x) and the base cases
 ;;     (subtype? x <top>).
 ;;   supertype? :: <descriptive-type> type -> <boolean>
 ;;     Returns true iff the descriptive type is a supertype of TYPE.
 ;;     Trivial cases are already excluded, esp., TYPE won't be the
 ;;     same kind of <descriptive-type>.

 (define-cclass <type-constructor-meta> :base :private :no-meta
   "ScmTypeConstructor*" "Scm_TypeConstructorMetaClass"
   (c "SCM_CLASS_METACLASS_CPL")
   ((initializer)
    (deconstructor)
    (validator)
    (subtype? :c-name "subtypeP")
    (supertype? :c-name "supertypeP"))
   )

 (define-cfn Scm_TypeConstructorP (klass) ::int
   (return (SCM_ISA klass (& Scm_TypeConstructorMetaClass))))

 ;; The 'name' slot is computed by the initializer.
 ;; The 'constructorArgs' slot is #f by default.  If the instance is
 ;; reconstructed from the precompiled form, however, we delay the actual
 ;; initialization until the type is used.
 (define-ctype ScmDescriptiveType
   ::(.struct ScmDescriptiveTypeRec
              (SCM_INSTANCE_HEADER::||
               name::ScmObj
               constructorArgs::ScmObj)))

 (define-cclass <descriptive-type> :base :private :no-meta
   "ScmDescriptiveType*" "Scm_DescriptiveTypeClass"
   (c "SCM_CLASS_METACLASS_CPL+1")
   ((name))
   (metaclass <type-constructor-meta>)
   (allocator (let* ([z::ScmDescriptiveType*
                      (SCM_NEW_INSTANCE ScmDescriptiveType klass)])
                (cast void initargs)    ;suppress unused warning
                (set! (-> z name) SCM_FALSE)
                (set! (-> z constructorArgs) SCM_FALSE)
                (return (SCM_OBJ z)))))

 ;; We memoize constructed types.  The type is keyed by the constructor class
 ;; and a list of contructor arguments.
 (define-cvar type-table::ScmMemoTable* :static)
 (initcode
  (set! type-table (SCM_MEMO_TABLE (Scm_MakeMemoTable 256 -1 0))))

 (define-cfn lookup-constructed-type (type-ctor::ScmTypeConstructor*
                                      args)
   :static
   (let* ([keys::(.array ScmObj (2))] [r])
     (set! (aref keys 0) (SCM_OBJ type-ctor))
     (set! (aref keys 1) args)
     (set! r (Scm_MemoTableGetv type-table keys 2))
     (if (SCM_UNBOUNDP r)
       (return SCM_FALSE)
       (return r))))

 (define-cfn register-constructed-type (type-ctor::ScmTypeConstructor*
                                        args
                                        type)
   :static
   (let* ([keys::(.array ScmObj (2))])
     (set! (aref keys 0) (SCM_OBJ type-ctor))
     (set! (aref keys 1) args)
     (Scm_MemoTablePutv type-table keys 2 type)
     (return type)))

 (define-cproc %dump-type-table () ::<void> ; for debug
   (Scm__MemoTableDump type-table SCM_CURERR))

 (define-cclass <native-type> :base :no-meta
   "ScmNativeType*" "Scm_NativeTypeClass"
   (c "SCM_CLASS_METACLASS_CPL+1")
   ((name)
    (super)
    (c-type-name :type <const-cstring>)
    (size :type <size_t>)
    (alignment :type <size_t>)
    (unsigned? :type <boolean> :c-name "unsigned_p")
    (bounded? :type <boolean> :c-name "bounded_p")
    (c-typecheck-name :type <const-cstring>)
    (c-boxer-name :type <const-cstring>)
    (c-unboxer-name :type <const-cstring>))
   (printer (Scm_Printf port "#<native-type %A>" (-> (SCM_NATIVE_TYPE obj) name))))

 ;; CPA for native type subclasses
 (define-cvar native-type-cpa::(.array ScmClass* (*)) :static
   #((SCM_CLASS_STATIC_PTR Scm_NativeTypeClass)
     (SCM_CLASS_STATIC_PTR Scm_TopClass)
     NULL))

 (define-cfn Scm_NativeHandlePtr (h::ScmNativeHandle*) ::void*
   (return (-> h ptr)))
 (define-cfn Scm_NativeHandleType (h::ScmNativeHandle*) ::ScmNativeType*
   (return (-> h type)))

 (define-cclass <c-pointer> :base :no-meta
   "ScmCPointer*" "Scm_CPointerClass"
   (c "native_type_cpa")
   ((pointee-type :type <native-type> :c-name "pointee_type"))
   (printer (Scm_Printf port "#<c-pointer %A>" (-> (& (-> (SCM_C_POINTER obj) common)) name)))
   (comparer (c Scm_ObjectCompare)))

 (define-cclass <c-function> :base :no-meta
   "ScmCFunction*" "Scm_CFunctionClass"
   (c "native_type_cpa")
   ((return-type :type <native-type> :c-name "return_type")
    (argument-types :c-name "argument_types")
    (variadic? :c-name "variadic" :type <boolean>))
   (printer (Scm_Printf port "#<c-function%A>" (-> (& (-> (SCM_C_FUNCTION obj) common)) name)))
   (comparer (c Scm_ObjectCompare)))

 (define-cclass <c-array> :base :no-meta
   "ScmCArray*" "Scm_CArrayClass"
   (c "native_type_cpa")
   ((element-type :type <native-type> :c-name "element_type")
    (dimensions))
   (printer (Scm_Printf port "#<c-array %A>" (-> (& (-> (SCM_C_ARRAY obj) common)) name)))
   (comparer (c Scm_ObjectCompare)))

 (define-cclass <c-struct> :base :no-meta
   "ScmCStruct*" "Scm_CStructClass"
   (c "native_type_cpa")
   ((tag)
    (fields))
   (printer (Scm_Printf port "#<c-struct %A>" (-> (& (-> (SCM_C_STRUCT obj) common)) name)))
   (comparer (c Scm_ObjectCompare)))

 (define-cclass <c-union> :base :no-meta
   "ScmCUnion*" "Scm_CUnionClass"
   (c "native_type_cpa")
   ((tag)
    (fields))
   (printer (Scm_Printf port "#<c-union %A>" (-> (& (-> (SCM_C_UNION obj) common)) name)))
   (comparer (c Scm_ObjectCompare)))

 (define-cclass <c-enum> :base :no-meta
   "ScmCEnum*" "Scm_CEnumClass"
   (c "native_type_cpa")
   ((tag)
    (type-spec)
    (enumerator-alist))
   (printer (Scm_Printf port "#<c-enum %A>" (-> (& (-> (SCM_C_ENUM obj) common)) name)))
   (comparer (c Scm_ObjectCompare)))
 )

(define-method initialize ((c <type-constructor-meta>) initargs)
  (next-method)
  (unless (slot-bound? c 'subtype?)
    (slot-set! c 'subtype? (^[type super] #f)))
  (unless (slot-bound? c 'supertype?)
    (slot-set! c 'supertype? (^[type sub] #f))))

;; Returns true if the native type's super is SCM_CLASS_INTEGER
(inline-stub
 (define-cfn Scm_NativeTypeIntegralP (np::ScmNativeType*) ::int
   (return (SCM_EQ (-> np super) (SCM_OBJ SCM_CLASS_INTEGER))))

 ;; Returns true if the native type is an unsigned integral type
 (define-cfn Scm_NativeTypeUnsignedP (np::ScmNativeType*) ::int
   (return (-> np unsigned-p)))
 )

;; Returns a class that's the basis of the native type
(inline-stub
 (define-cfn Scm_NativeTypeBaseClass (np::ScmNativeType*)
   (let* ([t::ScmNativeType* np])
     (loop
      (let* ([sup (-> t super)])
        (cond [(SCM_CLASSP sup) (return sup)]
              [(SCM_NATIVE_TYPE_P sup) (set! t (SCM_NATIVE_TYPE sup))]
              [else (Scm_Error "[internal] Bad inheritance setup: %S" np)])))))
 )

;;;
;;; type? obj
;;;

;; Returns obj if it's either a class metaobject, a descriptive type,
;; a native type, or a proxy type.  It's the same as (is-a? obj <type>)
;; but this is more concise and clear.
(define-cproc type? (obj) ::<boolean> :constant
  (return (SCM_ISA obj SCM_CLASS_TYPE)))

;;;
;;; of-type? obj type
;;;

(inline-stub
 ;;  This may push C continuations on VM, so must be called on VM.
 (define-cfn Scm_VMOfType (obj type)
   (cond [(SCM_PROXY_TYPE_P type)
          (return (Scm_VMIsA obj (Scm_ProxyTypeRef (SCM_PROXY_TYPE type))))]
         [(SCM_DESCRIPTIVE_TYPE_P type)
          (let* ([k::ScmClass* (SCM_CLASS_OF type)])
            (SCM_ASSERT (SCM_TYPE_CONSTRUCTOR_META_P k))
            (return (Scm_VMApply2 (-> (SCM_TYPE_CONSTRUCTOR_META k) validator)
                                  type obj)))]
         [(SCM_NATIVE_TYPE_P type)
          (return (SCM_MAKE_BOOL
                   (funcall (-> (SCM_NATIVE_TYPE type) c-of-type)
                            (SCM_NATIVE_TYPE type)
                            obj)))]
         [(SCM_CLASSP type)
          (return (Scm_VMIsA obj (SCM_CLASS type)))]
         [else
          (Scm_Error "Second argument of of-type? must be a type, but got: %S"
                     type)]))

 (define-cproc of-type? (obj type) :constant Scm_VMOfType)
 )

;;;
;;; subtype? type-or-class type-or-class
;;;

(define-cfn delegate-to-super (sub super) :static
  (if (SCM_DESCRIPTIVE_TYPE_P super)
    (let* ([k::ScmClass* (Scm_ClassOf super)])
      (SCM_ASSERT (SCM_TYPE_CONSTRUCTOR_META_P k))
      (return (Scm_VMApply2 (-> (SCM_TYPE_CONSTRUCTOR_META k) supertypeP)
                            super sub)))
    (return SCM_FALSE)))

(define-cproc subtype? (sub super)
  (loop
   (cond
    ;; Strip proxy types first
    [(SCM_PROXY_TYPE_P sub)
     (set! sub (SCM_OBJ (Scm_ProxyTypeRef (SCM_PROXY_TYPE sub))))]
    [(SCM_PROXY_TYPE_P super)
     (set! super (SCM_OBJ (Scm_ProxyTypeRef (SCM_PROXY_TYPE super))))]
    ;; Filter out the trivial cases
    [(SCM_EQ super (SCM_OBJ SCM_CLASS_TOP)) (return SCM_TRUE)]
    [(SCM_EQ sub (SCM_OBJ SCM_CLASS_BOTTOM)) (return SCM_TRUE)]
    [(SCM_EQ super sub) (return SCM_TRUE)]
    ;; Native types can be a subtype of a class.  Classes never be a subtype
    ;; of a native type (except <bottom>).
    ;; Native types can form single inheritance chain.
    [(and (SCM_NATIVE_TYPE_P sub)
          (SCM_CLASSP super))
     ;; we fall back to class vs class comparison
     (let* ([klass (Scm_NativeTypeBaseClass (SCM_NATIVE_TYPE sub))])
       (set! sub klass))]        ; retry
    [(and (SCM_NATIVE_TYPE_P sub)
          (SCM_NATIVE_TYPE_P super))
     ;; we have single inheritance between native types
     (set! sub (-> (SCM_NATIVE_TYPE sub) super))
     (loop
      (cond [(SCM_EQ sub super) (return SCM_TRUE)]
            [(SCM_NATIVE_TYPE_P sub)
             (set! sub (-> (SCM_NATIVE_TYPE sub) super))] ;retry
            [else (return SCM_FALSE)]))]
    ;; the case of (subtype? <native-type>  <descriptive-type>) is handled
    ;; later
    [(SCM_NATIVE_TYPE_P super) (return SCM_FALSE)]
    ;; Both are classes, we can use subclass?
    [(and (SCM_CLASSP sub) (SCM_CLASSP super))
     (return (SCM_MAKE_BOOL (Scm_SubclassP (SCM_CLASS sub) (SCM_CLASS super))))]
    ;; Delegate descriptive types to its handlers.
    [(SCM_DESCRIPTIVE_TYPE_P sub)
     (let* ([k::ScmClass* (Scm_ClassOf sub)])
       (SCM_ASSERT (SCM_TYPE_CONSTRUCTOR_META_P k))
       (let1/cps r (Scm_VMApply2 (-> (SCM_TYPE_CONSTRUCTOR_META k) subtypeP)
                                 sub super)
         [sub super]
         (cond [(or (SCM_FALSEP r) (SCM_EQ r SCM_TRUE)) (return r)]
               [(SCM_EQ r 'super) (return (delegate-to-super sub super))]
               [else
                (Scm_Error "subtype? handler of %S returned invalid value: %S"
                           r)])))]
    [else (return (delegate-to-super sub super))])))

;;;
;;; Descriptive type infrastructure
;;;

(define-method allocate-instance ((t <descriptive-type>) initargs)
  (error "Abstract type instance cannot instantiate a concrete object:" t))

(define-method write-object ((t <descriptive-type>) port)
  (format port "#~a" (~ t'name)))

;; Equality is used when consolidate literals.  It's not lightweight
;; (it calls deconstructor, which allocates).
(define-method object-equal? ((x <descriptive-type>) (y <descriptive-type>))
  (and (equal? (class-of x) (class-of y))
       (equal? (deconstruct-type x) (deconstruct-type y))))

;; This can also be called from initialization of precompiled code to recover
;; descripitve type instance.
(inline-stub
 (define-cfn Scm_ConstructType (ctor args)
   (SCM_ASSERT (!= ctor NULL))
   (unless (Scm_TypeConstructorP ctor)
     (SCM_TYPE_ERROR ctor "<type-constructor-meta>"))
   (let* ([ct::ScmTypeConstructor* (cast ScmTypeConstructor* ctor)]
          [type (lookup-constructed-type ct args)])
     (unless (SCM_FALSEP type) (return type))
     (set! type (Scm_NewInstance (SCM_CLASS ct) (sizeof ScmDescriptiveType)))
     (Scm_ApplyRec2 (-> ct initializer) type args)
     (return (register-constructed-type ct args type))))
 )
;; Public interface to construct a descriptive type.
(define-cproc construct-type (meta args) Scm_ConstructType)

;; Internal API, required to precompile descriptive type constant
(define-method deconstruct-type ((t <descriptive-type>))
  ((~ (class-of t)'deconstructor) t))


;;;
;;; Proxy type API.
;;;

;; These are available in gauche.internal

(define-cproc wrap-with-proxy-type (id gloc)
  (unless (SCM_IDENTIFIERP id)
    (SCM_TYPE_ERROR id "identifier"))
  (unless (SCM_GLOCP gloc)
    (SCM_TYPE_ERROR gloc "gloc"))
  (return (Scm_MakeProxyType (SCM_IDENTIFIER id) (SCM_GLOC gloc))))

(define-cproc proxy-type-ref (type)
  (unless (SCM_PROXY_TYPE_P type)
    (SCM_TYPE_ERROR type "proxy-type"))
  (return (SCM_OBJ (Scm_ProxyTypeRef (SCM_PROXY_TYPE type)))))

(define-cproc proxy-type-id (type)
  (unless (SCM_PROXY_TYPE_P type)
    (SCM_TYPE_ERROR type "proxy-type"))
  (return (Scm_ProxyTypeId (SCM_PROXY_TYPE type))))

;; Internal.  Recover type from the serialized type name for precompiled subr.
;; KLUDGE: 'module' here is the module where the subr is defined, but the type
;; itself isn't necessarily visible from that module.  A typical case is the
;; subr defined in 'scheme' module---the types are obviously not visible from
;; vanilla scheme.  For now, we try the given module, then try #<module gauche>.
(define-cproc %lookup-type (mod::<module> type-name::<symbol>)
  (let* ([g::ScmGloc* (Scm_FindBinding mod type-name 0)])
    (when (== g NULL)
      (set! g (Scm_FindBinding (Scm_GaucheModule) type-name 0)))
    (when (== g NULL)
      (return SCM_FALSE))
    (let* ([val (Scm_GlocGetValue g)])
      (cond [(SCM_CLASSP val)
             (let* ([id (Scm_MakeIdentifier (SCM_OBJ type-name) mod SCM_NIL)])
               (return (Scm_MakeProxyType (SCM_IDENTIFIER id) g)))]
            [(SCM_ISA val SCM_CLASS_TYPE) (return val)]
            [else (return SCM_FALSE)]))))

(define (%type-name->type module type-name)
  (define (maybe-type y)
    (let1 s (symbol->string y)
      (and (eqv? (string-ref s (- (string-length s) 1)) #\?)
           (string->symbol (substring s 0 (- (string-length s) 1))))))
  (if-let1 m (maybe-type type-name)
    (and-let1 mt (%lookup-type module m)
      (construct-type <?> (list mt)))
    (%lookup-type module type-name)))

;;;
;;; Utilities
;;;

(define (join-class-names classes)
  (string-join (map (^k (x->string
                         (cond [(is-a? k <class>) (class-name k)]
                               [(is-a? k <descriptive-type>) (~ k'name)]
                               [(is-a? k <native-type>) (~ k'name)]
                               [(is-a? k <proxy-type>)
                                (~ (proxy-type-id k) 'name)]
                               [else k])))
                    classes)
               " " 'prefix))

(define (make-compound-type-name op-name classes)
  ($ string->symbol
     $ string-append "<" (x->string op-name) (join-class-names classes) ">"))

(define (make-min-max-len-type-name op-name classes min max)
  ($ string->symbol
     $ string-append "<" (x->string op-name) (join-class-names classes)
     (if min
       (if max
         (if (= min max)
           (format " ~d" min)
           (format " ~d..~d" min max))
         (format " ~d.." min))
       (if max
         (format " ..~d" max)
         ""))
     ">"))

;;;
;;; Class: <^>  (maybe we want to name it <λ>)
;;;   Creates a procedure type.
;;;   The signature can be specified as
;;;
;;;       <argtype1> <argtype2> ... -> <rettype1> <rettype2> ...
;;;
;;;   Argument types and/or return types can be also a single symbol *,
;;;   indicating arbitrary number of args/values.   That is, any procedure
;;;   can be of type * -> *.
;;;
;;;   TODO: How to type optional and keyword arguments?
;;;


(define (init-^ type init-args)
  (define (scan-args xs as)
    ;; NB: We want to allow a right arrow (U+2192) in place of ->.
    ;; The code would be messy though, if we want to support 'none' encoding as
    ;; well.  Think about it after we drop 'none' support.
    (match xs
      [() (error "Missing '->' in the procedure type constructor arguments:"
                 init-args)]
      [('-> . xs) (scan-results xs (reverse as) '())]
      [('* '-> . xs) (scan-results xs (reverse as '(*)) '())]
      [_
       (if (is-a? (car xs) <type>)
         (scan-args (cdr xs) (cons (car xs) as))
         (error "Non-type argument in the procedure type constructor:"
                (car xs)))]))
  (define (scan-results xs args rs)
    (match xs
      [() (values args (reverse rs))]
      [('*) (values args (reverse rs '(*)))]
      [(x . xs)
       (if (is-a? x <type>)
         (scan-results xs args (cons x rs))
         (error "Non-class argument in the procedure type constructor:" x))]
      [_ (error "Invalid arguments:" xs)]))

  (receive (args results) (scan-args init-args '())
    (slot-set! type 'name      (make-compound-type-name '^ init-args))
    (slot-set! type 'arguments (construct-type <Tuple> args))
    (slot-set! type 'results   (construct-type <Tuple> results))))

(define (deconstruct-^ type)
  (append (~ type'arguments'elements)
          (if (~ type'arguments'allow-rest?) '(*) '())
          '(->)
          (~ type'results'elements)
          (if (~ type'results'allow-rest?) '(*) '())))

(define (validate-^ type obj)
  (and-let1 otype (compute-procedure-type obj)
    (subtype? type otype)))

(define (subtype-^ type super)
  (if (is-a? super <^>)
    (and (subtype? (~ type'arguments) (~ super'arguments))
         (subtype? (~ super'results) (~ type'results)))
    'super))

;; Internal API - called from procedure-type (libproc)
;; Reconstruct #<^ ...> type from a serialized type info encoded in a vector.
(define (reconstruct-procedure-type proc encoded-type)
  (if (and (vector? encoded-type)
           (>= (vector-length encoded-type) 3)
           (= (vector-ref encoded-type 0) 1))
    (let* ([module-name (vector-ref encoded-type 1)]
           [module (find-module module-name)])
      (if (not module)
        (begin
          (warn "unknown module during reconstructing procedure type: ~a\n"
                module-name)
          (compute-procedure-type proc)) ;; fallback
        ($ construct-type <^>
           $ map (^e (if (or (memq e '(* ->)))
                       e
                       (or (%type-name->type module e)
                           (error "unknown type in procedure type info:" e))))
           (vector->list encoded-type 2))))
    (compute-procedure-type proc)))

;; Internal API - called from procedure-type (libproc)
;; Compute #<^ ...> type from the information available in the procedure.
;; Once this info is computed, it is cached in PROC.
(define (compute-procedure-type proc)
  (define (%procedure-type proc)
    (if-let1 clinfo (case-lambda-decompose proc)
      (construct-type </> (map (^v (%procedure-type (caddr v))) clinfo))
      (or (and-let* ([ (closure? proc) ]
                     [code (closure-code proc)])
            ((with-module gauche.internal compiled-code-type) code))
          ;; Fallback - if we don't have detailed type info, just use
          ;; # of arguments.
          (let1 top (%class->proxy <top>)
            (construct-procedure-type (make-list (~ proc'required) top)
                                      (~ proc'optional)
                                      '*)))))
  (define (%method-type meth)
    (construct-procedure-type (map %class->proxy (~ meth'specializers))
                              (~ meth'optional)
                              '*))
  (define (%generic-type gf)
    (construct-type </> (map %method-type (~ gf'methods))))

  (cond [(is-a? proc <procedure>) (%procedure-type proc)]
        [(is-a? proc <generic>)   (%generic-type proc)]
        [(is-a? proc <method>)    (%method-type proc)]
        ;; Dealing with applicable objects are debatable.  Any object can
        ;; become applicable, which makes its type
        ;; (</> <original-type> <type-when-applied>).  That makes type
        ;; operations extremely complicated.
        [else #f]))

(define-cproc %class->proxy (klass::<class>)
  (let* ([ms (-> klass modules)]
         [n (-> klass name)])
    (dolist [m ms]
      (let* ([g::ScmGloc* (Scm_FindBinding (SCM_MODULE m) (SCM_SYMBOL n) 0)])
        (when (!= g NULL)
          (let* ([id (Scm_MakeIdentifier n (SCM_MODULE m) SCM_NIL)])
            (return (Scm_MakeProxyType (SCM_IDENTIFIER id) g))))))
    ;; If we're here, the class doesn't have a known global binding.
    ;; It is possible---a class can be created procedurally at runtime---
    ;; but to be used in a type expession, it must be recognized by the
    ;; compiler, which requires the class is statically bound to a global
    ;; identifier.  We raise an error if that's not the case.
    (Scm_Error "Class %S doesn't have a known global binding and can't be used \
                in a type expression." klass)))

;; Utility.  Called from compiler, too.
(define (construct-procedure-type argtypes ; (<List> <type>)
                                  has-optional? ; <boolean>
                                  rettypes) ; (<List> <type>) or '*
  (let1 rts (if (eq? rettypes '*) '(*) rettypes)
    ($ construct-type <^>
       (if has-optional?
         `(,@argtypes * -> ,@rts)
         `(,@argtypes -> ,@rts)))))

(define-class <^> (<descriptive-type>)
  ((arguments :init-keyword :arguments)    ; <Tuple>
   (results   :init-keyword :results))     ; <Tuple>
  :metaclass <type-constructor-meta>
  :initializer init-^
  :deconstructor deconstruct-^
  :validator validate-^
  :subtype? subtype-^)

;;;
;;; Class: </>
;;;   Creates a union type.
;;;

(define-class </> (<descriptive-type>)
  ((members :init-keyword :members))
  :metaclass <type-constructor-meta>
  :initializer (^[type args]
                 (assume (every (cut is-a? <> <type>) args))
                 (slot-set! type 'name (make-compound-type-name '/ args))
                 (slot-set! type 'members args))
  :deconstructor (^[type] (~ type'members))
  :validator (^[type obj] (any (cut of-type? obj <>) (~ type'members)))
  :subtype? (^[type super] (every (cut subtype? <> super) (~ type'members)))
  :supertype? (^[type sub] (any (cut subtype? sub <>) (~ type'members))))

;;;
;;; Class: <?>
;;;   Creates a boolean-optional type, that is, <type> or #f.
;;;

(define-class <?> (<descriptive-type>)
  ((primary-type :init-keyword :primary-type))
  :metaclass <type-constructor-meta>
  :initializer (^[type args]
                 (let1 ptype (car args)
                   (assume (is-a? ptype <type>))
                   (slot-set! type 'name (make-compound-type-name '? `(,ptype)))
                   (slot-set! type 'primary-type ptype)))
  :deconstructor (^[type] (list (~ type'primary-type)))
  :validator (^[type obj]
               (or (eqv? obj #f) (of-type? obj (~ type'primary-type))))
  :subtype? (^[type super]
              (if (is-a? super <?>)
                (subtype? (~ type'primary-type) (~ super'primary-type))
                (and (of-type? #f super)
                     (subtype? (~ type'primary-type) super))))
  :supertype? (^[type sub] (subtype? sub (~ type'primary-type))))

;;;
;;; Class: <Tuple>
;;;   Fixed-length list, each element having its own type constraints.
;;;

;; (<Tuple> type ... [*])

(define (init-Tuple type args)
  (receive (types rest?) (if (and (pair? args) (eqv? (last args) '*))
                           (values (drop-right args 1) #t)
                           (values args #f))
    (dolist [t types]
      (unless (is-a? t <type>)
        (error "Non-type parameter in <Tuple> constructor:" t)))
    (slot-set! type 'name (make-compound-type-name 'Tuple args))
    (slot-set! type 'elements types)
    (slot-set! type 'allow-rest? rest?)))

(define (deconstruct-Tuple type)
  (if (~ type'allow-rest?)
    (append (~ type'elements) '(*))
    (~ type'elements)))

(define (validate-Tuple type obj)
  (let loop ([obj obj] [elts (~ type'elements)])
    (cond [(null? obj) (null? elts)]
          [(not (pair? obj)) #f]
          [(null? elts) (~ type'allow-rest?)]
          [else (and (of-type? (car obj) (car elts))
                     (loop (cdr obj) (cdr elts)))])))

(define (subtype-Tuple type super)
  (or (eqv? super <list>)
      (and (is-a? super <Tuple>)
           (if (~ type'allow-rest?)
             (<= (length (~ type'elements)) (length (~ super'elements)))
             (= (length (~ type'elements)) (length (~ super'elements))))
           (every (cut subtype? <> <>) (~ type'elements) (~ super'elements)))
      (and (is-a? super <List>)
           (every (cute subtype? <> (~ super'element-type)) (~ type'elements))
           (<= (or (~ super'min-length) 0)
               (length (~ type'elements))
               (or (~ super'max-length) +inf.0)))
      'super))

(define-class <Tuple> (<descriptive-type>)
  ((elements    :init-keyword :elements)
   (allow-rest? :init-keyword :allow-rest?))
  :metaclass <type-constructor-meta>
  :initializer init-Tuple
  :deconstructor deconstruct-Tuple
  :validator  validate-Tuple
  :subtype? subtype-Tuple)

;;;
;;; Class: <List>
;;; Class: <Vector>
;;;   A list or vector of specified types.
;;;

(define (make-init-Seq name)
  (^[type args]
    (apply (^[etype :optional (min #f) (max #f)]
             (unless (or (not min) (real? min))
               (error "min argument must be a real number or #f, but got:" min))
             (unless (or (not max) (real? max))
               (error "max argument must be a real number or #f, but got:" max))
             (slot-set! type 'name
                        (make-min-max-len-type-name name (list etype) min max))
             (slot-set! type 'element-type etype)
             (slot-set! type 'min-length min)
             (slot-set! type 'max-length max))
           args)))

(define (deconstruct-Seq type)
  (list (~ type'element-type) (~ type'min-length) (~ type'max-length)))

(define (validate-List type obj)
  (let ([et (~ type'element-type)]
        [mi (~ type'min-length)]
        [ma (~ type'max-length)])
    (if (not (or mi ma))
      ;; simple case
      (let loop ([obj obj])
        (cond [(null? obj) #t]
              [(not (pair? obj)) #f]
              [(of-type? (car obj) et) (loop (cdr obj))]
              [else #f]))
      ;; general case
      (let loop ([obj obj] [n 0])
        (cond [(null? obj) (or (not mi) (<= mi n))]
              [(and ma (<= ma n)) #f]
              [(not (pair? obj)) #f]
              [(of-type? (car obj) et) (loop (cdr obj) (+ n 1))]
              [else #f])))))

(define (validate-Vector type obj)
  (and (vector? obj)
       (let ([et (~ type'element-type)]
             [mi (~ type'min-length)]
             [ma (~ type'max-length)]
             [len (vector-length obj)])
         (and (or (not mi) (<= mi len))
              (or (not ma) (<= len ma))
              (let loop ([i 0])
                (cond [(= i len) #t]
                      [(of-type? (vector-ref obj i) et) (loop (+ i 1))]
                      [else #f]))))))

(define (make-subtype-Seq base-type)
  (^[type super]
    (or (eqv? super base-type)
        (and (is-a? super (class-of type))
             (subtype? (~ type'element-type) (~ super'element-type))
             (>= (or (~ type'min-length) 0)
                 (or (~ super'min-length) 0))
             (<= (or (~ type'max-length) +inf.0)
                 (or (~ super'max-length) +inf.0)))
        'super)))

(define-class <List> (<descriptive-type>)
  ((element-type :init-keyword :element-type)
   (min-length :init-keyword :min-length :init-value #f)
   (max-length :init-keyword :max-length :init-value #f))
  :metaclass <type-constructor-meta>
  :initializer (make-init-Seq 'List)
  :deconstructor deconstruct-Seq
  :validator validate-List
  :subtype? (make-subtype-Seq <list>))

(define-class <Vector> (<descriptive-type>)
  ((element-type :init-keyword :element-type)
   (min-length :init-keyword :min-length :init-value #f)
   (max-length :init-keyword :max-length :init-value #f))
  :metaclass <type-constructor-meta>
  :initializer (make-init-Seq 'Vector)
  :deconstructor deconstruct-Seq
  :validator validate-Vector
  :subtype? (make-subtype-Seq <vector>))

;;;
;;; Class: <Assortment>
;;;   A type consists of a set of concrete objects.
;;;   If it has only one member, you can think of it as a singleton type.
;;;   If it has multiple members, it's a union of singleton types.
;;;   The members (instances) are sorted by default-comparator so that
;;;   equivalence is easily tested.
;;;

(define-class <Assortment> (<descriptive-type>)
  ((instances :init-keyword :instance))
  :metaclass <type-constructor-meta>
  :initializer (^[type args]
                 (define objs (map unwrap-syntax args))
                 (slot-set! type 'name
                            (string->symbol
                             (string-append
                              "<Assortment " (x->string objs) ">")))
                 (slot-set! type 'instances (sort objs)))
  :deconstructor (^[type] (list (~ type'instances)))
  :validator (^[type obj] (boolean (memv obj (~ type'instances))))
  :subtype? (^[type super] (every (^[obj] (of-type? obj super))
                                  (~ type'instances)))
  :supertype? (^[type sub] #f))

;;;
;;; <native-type> : Types for bridging Scheme and C
;;;

;; Each of these types has a corresponding cgen-type that maintains
;; the knowledge how it is represented in C.

(inline-stub
 (define-cfn make_native_type
   (name::(const char*)
    super
    c-type-name::(const char*)
    size::size_t
    alignment::size_t
    c-of-type::(.function (type::ScmNativeType* obj) ::int *)
    c-ref::(.function (type::ScmNativeType* ptr::void*)::ScmObj *)
    c-set::(.function (type::ScmNativeType* ptr::void* obj)::void *)
    c-typecheck-name::(const char *)
    c-boxer-name::(const char*)
    c-unboxer-name::(const char*)
    unsigned-p::int
    bounded-p::int)
   :static
   (let* ([z::ScmNativeType*
           (SCM_NEW_INSTANCE ScmNativeType (& Scm_NativeTypeClass))])
     (set! (-> z name) (SCM_INTERN name))
     (set! (-> z super) super)
     (set! (-> z c-type-name) c-type-name)
     (set! (-> z c-of-type) c-of-type)
     (set! (-> z c-ref) c-ref)
     (set! (-> z c-set) c-set)
     (set! (-> z size) size)
     (set! (-> z alignment) alignment)
     (set! (-> z c-typecheck-name) c-typecheck-name)
     (set! (-> z c-boxer-name) c-boxer-name)
     (set! (-> z c-unboxer-name) c-unboxer-name)
     (set! (-> z unsigned-p) unsigned-p)
     (set! (-> z bounded-p) bounded-p)
     (return (SCM_OBJ z))))

 ;; Internal API for gauche.native-type to create a 'variant' of
 ;; existing native type (e.g. <int16be>)
 ;; The variants can't be used FFI argtype/rettype.  Only meaningful for
 ;; binary data access via native handles.
 ;; alignment, c-ref, c-set : if its 0 or NULL, inherit original value.
 (define-cfn Scm__MakeNativeTypeVariant
   (orig::(const ScmNativeType*)
    name::(const char*)
    alignment::size_t
    c-ref::(.function (type::ScmNativeType* ptr::void*)::ScmObj *)
    c-set::(.function (type::ScmNativeType* ptr::void* obj)::void *))
   ::ScmObj
   (let* ([z::ScmNativeType*
           (SCM_NEW_INSTANCE ScmNativeType (& Scm_NativeTypeClass))])
     (set! (* z) (* orig))
     (set! (-> z name) (SCM_INTERN name))
     (when (> alignment 0) (set! (-> z alignment) alignment))
     (when c-ref (set! (-> z c-ref) c-ref))
     (when c-set (set! (-> z c-set) c-set))
     (return (SCM_OBJ z))))

 ;; Primitive native type name -> native type instance
 ;; Used by gauche.native-type.  Can be used for other purposes, but
 ;; it's not official and the structure/interface may change.
 (define-cvar builtin-native-types :static)

 (initcode
  (set! builtin-native-types (Scm_MakeHashTableSimple SCM_HASH_EQ 16)))

 (define-cproc %builtin-native-type-table ()
   (return builtin-native-types))

 ;; define-native-type NAME CVAR SUPER CTYPE PRED BOX UNBOX
 ;;   NAME - Symbol for Scheme name
 ;;   CVAR - C variable name to store the ScmObj (e.g., Scm_NativeFixnumType)
 ;;   SUPER - C macro name for superclass
 ;;   CTYPE - C type name
 ;;   PRED - single-argument C macro or function: ScmObj -> int
 ;;          (auto-wrapped into (ScmNativeType*, ScmObj) -> int)
 ;;   BOX - C function ctype -> ScmObj for boxing
 ;;   UNBOX - C function ScmObj -> ctype for unboxing

 (define-cise-stmt define-native-type
   [(_ name cvar super ctype pred box unbox unsignedp)
    (define c-of-type-name (symbol-append name '-c-of-type))
    (define c-ref-name (symbol-append name '-ptr-ref))
    (define c-set-name (symbol-append name '-ptr-set))
    ;; TRANSIENT: We need to compile 0.9.16 with 0.9.15.  To make cgen.stub
    ;; work with both versions, we define aliases of native-types without
    ;; 'c-' prefix.  This will go after 0.9.16 release.
    (define name-sans-c ($ string->symbol
                           $ regexp-replace #/^<c-/ (symbol->string name) "<"))
    (define cvar-str (symbol->string cvar))
    (define priv-str (string-append cvar-str "_"))
    (define priv-sym (string->symbol priv-str))

    (cgen-decl #"static ScmObj ~priv-str;")
    (cgen-decl #"ScmObj ~cvar-str(void) { return ~priv-str; }")
    (cgen-decl
     (cise-render-to-string
      `(define-cfn ,c-of-type-name (_::ScmNativeType* obj) ::int :static
         (return (,pred obj)))
      'toplevel))
    (cgen-decl
     (cise-render-to-string
      `(define-cfn ,c-ref-name (_::ScmNativeType* ptr::void*) :static
         (let* ([pp :: (,ctype *) (cast (,ctype *) ptr)])
           (return (,box (* pp)))))
      'toplevel))
    (cgen-decl
     (cise-render-to-string
      `(define-cfn ,c-set-name (t::ScmNativeType* ptr::void* obj) ::void :static
         (let* ([pp :: (,ctype *) (cast (,ctype *) ptr)])
           (unless (,c-of-type-name t obj)
             (SCM_TYPE_ERROR obj ,(x->string name)))
           (set! (* pp) (,unbox obj))))
      'toplevel))
    `(let* ([z (make_native_type ,(symbol->string name)
                                 (SCM_OBJ ,super) ,(x->string ctype)
                                 (sizeof (.type ,ctype))
                                 (SCM_ALIGNOF (.type ,ctype))
                                 ,c-of-type-name
                                 ,c-ref-name
                                 ,c-set-name
                                 ,(x->string pred)
                                 ,(x->string box)
                                 ,(x->string unbox)
                                 ,unsignedp
                                 TRUE)])
       (set! ,priv-sym z)
       (Scm_HashTableSet (SCM_HASH_TABLE builtin-native-types)
                         ',ctype (SCM_OBJ z) 0)
       (Scm_MakeBinding (Scm_GaucheModule)
                        (SCM_SYMBOL (-> (SCM_NATIVE_TYPE z) name)) z
                        SCM_BINDING_INLINABLE)
       ;; TRANSIENT: See above about this alias.
       ;; We avoid defining <char>, which is used as Scheme type.
       ,@(if (memq name-sans-c '(<char> <string>))
           '()
           `((Scm_MakeBinding (Scm_GaucheModule)
                              (SCM_SYMBOL ',name-sans-c) z
                              SCM_BINDING_INLINABLE))))])

 ;; subrs returning <void> actually return #<undef>
 (define-cfn native_voidP (_::ScmNativeType* obj) ::int :static
   (return (SCM_UNDEFINEDP obj)))

 ;; Singleton of those types (static private vars + public accessor functions)
 (define-cvar Scm_NativeVoidType_ ::ScmObj :static)
 (define-cfn Scm_NativeVoidType () ::ScmObj (return Scm_NativeVoidType_))
 (define-cvar Scm_NativeVoidPointerType_ ::ScmObj :static)
 (define-cfn Scm_NativeVoidPointerType () ::ScmObj (return Scm_NativeVoidPointerType_))

 (initcode
  (define-native-type <fixnum>  Scm_NativeFixnumType  SCM_CLASS_INTEGER ScmSmallInt
    SCM_INTP SCM_MAKE_INT SCM_INT_VALUE FALSE)
  (define-native-type <ufixnum>  Scm_NativeUfixnumType  SCM_CLASS_INTEGER ScmSmallInt
    SCM_UINTP SCM_MAKE_INT SCM_INT_VALUE TRUE)
  (define-native-type <short>   Scm_NativeShortType   SCM_CLASS_INTEGER short
    SCM_INTEGER_FITS_SHORT_P SCM_MAKE_INT SCM_INT_VALUE FALSE)
  (define-native-type <ushort>  Scm_NativeUshortType  SCM_CLASS_INTEGER u_short
    SCM_INTEGER_FITS_USHORT_P SCM_MAKE_INT SCM_INT_VALUE TRUE)
  (define-native-type <int>     Scm_NativeIntType     SCM_CLASS_INTEGER int
    SCM_INTEGER_FITS_INT_P Scm_MakeInteger Scm_GetInteger FALSE)
  (define-native-type <uint>    Scm_NativeUintType    SCM_CLASS_INTEGER u_int
    SCM_INTEGER_FITS_UINT_P Scm_MakeIntegerU Scm_GetIntegerU TRUE)
  (define-native-type <long>    Scm_NativeLongType    SCM_CLASS_INTEGER long
    SCM_INTEGER_FITS_LONG_P Scm_MakeInteger Scm_GetInteger FALSE)
  (define-native-type <ulong>   Scm_NativeUlongType   SCM_CLASS_INTEGER u_long
    SCM_INTEGER_FITS_ULONG_P Scm_MakeIntegerU Scm_GetIntegerU TRUE)
  (define-native-type <int8>    Scm_NativeInt8Type    SCM_CLASS_INTEGER int8_t
    SCM_INTEGER_FITS_INT8_P SCM_MAKE_INT SCM_INT_VALUE FALSE)
  (define-native-type <uint8>   Scm_NativeUint8Type   SCM_CLASS_INTEGER uint8_t
    SCM_INTEGER_FITS_UINT8_P SCM_MAKE_INT SCM_INT_VALUE TRUE)
  (define-native-type <int16>   Scm_NativeInt16Type   SCM_CLASS_INTEGER int16_t
    SCM_INTEGER_FITS_INT16_P SCM_MAKE_INT SCM_INT_VALUE FALSE)
  (define-native-type <uint16>  Scm_NativeUint16Type  SCM_CLASS_INTEGER uint16_t
    SCM_INTEGER_FITS_UINT16_P SCM_MAKE_INT SCM_INT_VALUE TRUE)
  (define-native-type <int32>   Scm_NativeInt32Type   SCM_CLASS_INTEGER int32_t
    SCM_INTEGER_FITS_INT32_P Scm_MakeInteger Scm_GetInteger FALSE)
  (define-native-type <uint32>  Scm_NativeUint32Type  SCM_CLASS_INTEGER uint32_t
    SCM_INTEGER_FITS_UINT32_P Scm_MakeIntegerU Scm_GetIntegerU TRUE)
  (define-native-type <int64>   Scm_NativeInt64Type   SCM_CLASS_INTEGER int64_t
    SCM_INTEGER_FITS_INT64_P Scm_MakeInteger64 Scm_GetInteger64 FALSE)
  (define-native-type <uint64>  Scm_NativeUint64Type  SCM_CLASS_INTEGER uint64_t
    SCM_INTEGER_FITS_UINT64_P Scm_MakeIntegerU64 Scm_GetIntegerU64 TRUE)

  (define-native-type <size_t>    Scm_NativeSizetType    SCM_CLASS_INTEGER size_t
    Scm_IntegerFitsSizeP Scm_SizeToInteger Scm_IntegerToSize TRUE)
  (define-native-type <ssize_t>   Scm_NativeSsizetType   SCM_CLASS_INTEGER ssize_t
    Scm_IntegerFitsSsizeP Scm_SsizeToInteger Scm_IntegerToSsize FALSE)
  (define-native-type <ptrdiff_t> Scm_NativePtrdifftType SCM_CLASS_INTEGER ptrdiff_t
    Scm_IntegerFitsPtrdiffP Scm_PtrdiffToInteger Scm_IntegerToPtrdiff FALSE)
  (define-native-type <off_t>     Scm_NativeOfftType     SCM_CLASS_INTEGER off_t
    Scm_IntegerFitsOffsetP Scm_OffsetToInteger Scm_IntegerToOffset FALSE)
  (define-native-type <intptr_t>  Scm_NativeIntptrtType  SCM_CLASS_INTEGER intptr_t
    Scm_IntegerFitsIntptrP Scm_IntptrToInteger Scm_IntegerToIntptr FALSE)
  (define-native-type <uintptr_t> Scm_NativeUintptrtType SCM_CLASS_INTEGER uintptr_t
    Scm_IntegerFitsUintptrP Scm_UintptrToInteger Scm_IntegerToUintptr TRUE)

  (define-native-type <float>   Scm_NativeFloatType   SCM_CLASS_REAL float
    SCM_REALP Scm_MakeFlonum Scm_GetDouble FALSE)
  (define-native-type <double>  Scm_NativeDoubleType  SCM_CLASS_REAL double
    SCM_REALP Scm_MakeFlonum Scm_GetDouble FALSE)

  ;; Technically, time_t can be a real numebr.
  (define-native-type <time_t>  Scm_UnixTimeType      SCM_CLASS_NUMBER time_t
    SCM_REALP Scm_MakeSysTime Scm_GetSysTime FALSE)

  ;; We map C char to our character in 8-bit range.  If you want to use
  ;; char as a one-byte integer, use <c-int8> or <c-uint8>.
  (define-native-type <c-char>   Scm_NativeCCharType   SCM_CLASS_INTEGER char
    SCM_CHAR_FITS_LATIN1_P SCM_MAKE_CHAR SCM_CHAR_VALUE FALSE)
  ;; A special case of NUL-terminated string.
  (define-native-type <c-string> Scm_NativeCStringType SCM_CLASS_STRING "const char*"
    SCM_STRINGP SCM_MAKE_STR_COPYING SCM_STRING_CONST_CSTRING FALSE)

  ;; <void> needs special care, as it doesn't have a real C type.
  (let* ([z (make_native_type "<void>" (SCM_OBJ SCM_CLASS_TOP) "void"
                              1 1 native_voidP NULL NULL
                              "" "SCM_VOID_RETURN_VALUE" "" FALSE TRUE)])
    (set! Scm_NativeVoidType_ z)
    (Scm_HashTableSet (SCM_HASH_TABLE builtin-native-types)
                      'void z 0)
    (Scm_MakeBinding (Scm_GaucheModule) (SCM_SYMBOL '<void>) z
                     SCM_BINDING_INLINABLE))
  )

 (define-cfn native_type_list_equalP (as::ScmObj bs::ScmObj) ::int
   (for (()
         (and (SCM_PAIRP as) (SCM_PAIRP bs))
         (set! as (SCM_CDR as) bs (SCM_CDR bs)))
     (let* ([a (SCM_CAR as)]
            [b (SCM_CAR bs)])
       (cond
        [(SCM_NATIVE_TYPE_P a)
         (unless (and (SCM_NATIVE_TYPE_P b)
                      (Scm_NativeTypeEqualP (SCM_NATIVE_TYPE a)
                                            (SCM_NATIVE_TYPE b)))
           (return FALSE))]
        [(SCM_PAIRP a)
         (unless (and (SCM_PAIRP b)
                      (native_type_list_equalP a b))
           (return FALSE))]
        [else
         (unless (Scm_EqvP a b)
           (return FALSE))])))
   (unless (and (SCM_NULLP as) (SCM_NULLP bs))
     (return FALSE))
   (return TRUE))

 ;; An enum's type-spec is either #f or an integral native type.
 (define-cfn enum_type_spec_equalP (a b) ::int :static
   (cond [(and (SCM_NATIVE_TYPE_P a) (SCM_NATIVE_TYPE_P b))
          (return (Scm_NativeTypeEqualP (SCM_NATIVE_TYPE a) (SCM_NATIVE_TYPE b)))]
         [else (return (SCM_EQ a b))])) ;both #f

 (define-cfn Scm_NativeTypeEqualP (a::ScmNativeType* b::ScmNativeType*) ::int
   (cond
    [(SCM_EQ a b) (return TRUE)]
    [(SCM_C_POINTER_P a)
     (return (and (SCM_C_POINTER_P b)
                  (Scm_NativeTypeEqualP (-> (SCM_C_POINTER a) pointee-type)
                                        (-> (SCM_C_POINTER b) pointee-type))))]
    [(SCM_C_FUNCTION_P a)
     (return
      (and (SCM_C_FUNCTION_P b)
           (Scm_NativeTypeEqualP (-> (SCM_C_FUNCTION a) return-type)
                                 (-> (SCM_C_FUNCTION b) return-type))
           (native_type_list_equalP (-> (SCM_C_FUNCTION a) argument-types)
                                    (-> (SCM_C_FUNCTION b) argument-types))
           (== (-> (SCM_C_FUNCTION a) variadic)
               (-> (SCM_C_FUNCTION b) variadic))))]
     [(SCM_C_ARRAY_P a)
      (return
       (and (SCM_C_ARRAY_P b)
            (Scm_NativeTypeEqualP (-> (SCM_C_ARRAY a) element-type)
                                  (-> (SCM_C_ARRAY b) element-type))
            (native_type_list_equalP (-> (SCM_C_ARRAY a) dimensions)
                                     (-> (SCM_C_ARRAY b) dimensions))))]
     [(or (SCM_C_STRUCT_P a) (SCM_C_UNION_P a))
      (return
       (and (SCM_EQ (Scm_ClassOf (SCM_OBJ a)) (Scm_ClassOf (SCM_OBJ b)))
            (Scm_EqvP (-> (SCM_C_STRUCT a) tag)
                      (-> (SCM_C_STRUCT b) tag))
            (native_type_list_equalP (-> (SCM_C_ARRAY a) dimensions)
                                     (-> (SCM_C_ARRAY b) dimensions))))]
     [(SCM_C_ENUM_P a)
      (return
       (and (SCM_C_ENUM_P b)
            (Scm_EqvP (-> (SCM_C_ENUM a) tag)
                      (-> (SCM_C_ENUM b) tag))
            (enum_type_spec_equalP (-> (SCM_C_ENUM a) type-spec)
                                   (-> (SCM_C_ENUM b) type-spec))
            ;; enumerator-alist holds only symbols and integers
            (Scm_EqualP (-> (SCM_C_ENUM a) enumerator-alist)
                        (-> (SCM_C_ENUM b) enumerator-alist))))]
     [else (return FALSE)]))

 ) ;inline-stub

(define-cproc native-type=? (a::<native-type> b::<native-type>) ::<boolean>
  (return (Scm_NativeTypeEqualP a b)))

;; Compare native types
(define-method object-equal? ((s <native-type>) (t <native-type>))
  (native-type=? s t))

;;
;; Native handle
;;
;;  The struct definition is in priv/typeP.h

(inline-stub
 (define-cclass <native-handle> :base :no-meta
   "ScmNativeHandle*" "Scm_NativeHandleClass"
   (c "SCM_CLASS_DEFAULT_CPL")
   ((name)
    (type :type <native-type>))
   (printer (Scm_Printf port "#<native-handle %A@%p>"
                        (-> (SCM_NATIVE_HANDLE obj) name)
                        (-> (SCM_NATIVE_HANDLE obj) ptr)))
   (comparer (c "Scm_ObjectCompare")))

 (define-cfn Scm__MakeNativeHandle (ptr::void*
                                    type::ScmNativeType*
                                    name::ScmObj
                                    region-min::void*
                                    region-max::void*
                                    owner::ScmObj
                                    attrs::ScmObj
                                    flags::u_long)
   (let* ([h::ScmNativeHandle* (SCM_NEW ScmNativeHandle)])
     (SCM_SET_CLASS h SCM_CLASS_NATIVE_HANDLE)
     (set! (-> h ptr) ptr
           (-> h type) type
           (-> h name) name
           (-> h region-min) region-min
           (-> h region-max) region-max
           (-> h owner) owner
           (-> h attrs) attrs
           (-> h flags) flags)
     (return (SCM_OBJ h))))

 ;; Public API for stubgen FFI
 (define-cfn Scm_MakeNativeHandleSimple (ptr::void* type::ScmObj) ::ScmObj
   (SCM_ASSERT (SCM_NATIVE_TYPE_P type))
   (return (Scm__MakeNativeHandle ptr
                                  (SCM_NATIVE_TYPE type)
                                  (-> (SCM_NATIVE_TYPE type) name)
                                  NULL NULL
                                  SCM_UNDEFINED
                                  SCM_NIL
                                  0)))
 )

;;
;; Native composite types
;;

(inline-stub
 (define-cfn native_handle_typeP (t::ScmNativeType* obj) ::int :static
   (unless (SCM_NATIVE_HANDLE_P obj) (return FALSE))
   (when (Scm_NativeTypeEqualP (-> (SCM_NATIVE_HANDLE obj) type)
                               (SCM_NATIVE_TYPE (Scm_NativeVoidPointerType)))
     ;; void* pointer can be used in any pointer context; it's caller's
     ;; responsibility.
     (return TRUE))                     ;
   (return (Scm_NativeTypeEqualP (-> (SCM_NATIVE_HANDLE obj) type) t)))

 ;; Helper function to initialize common fields of composite native types
 (define-cfn init-native-type-common
   (nt::ScmNativeType*
    name::(const char*)
    super::ScmObj
    c-type-name::(const char*)
    size::size_t
    alignment::size_t
    bounded-p::int
    c-of-type::(.function (type::ScmNativeType* obj)::int *)
    c-ref::(.function (type::ScmNativeType* ptr::void*)::ScmObj *)
    c-set::(.function (type::ScmNativeType* ptr::void* obj)::void *))
   ::void :static
   (set! (-> nt name) (SCM_INTERN name))
   (set! (-> nt super) super)
   (set! (-> nt c-type-name) c-type-name)
   (set! (-> nt c-of-type) c-of-type)
   (set! (-> nt c-ref) c-ref)
   (set! (-> nt c-set) c-set)
   (set! (-> nt size) size)
   (set! (-> nt alignment) alignment)
   (set! (-> nt c-typecheck-name) "SCM_NATIVE_HANDLE_P")
   (set! (-> nt c-boxer-name) "SCM_OBJ")
   (set! (-> nt c-unboxer-name) "SCM_NATIVE_HANDLE")
   (set! (-> nt unsigned-p) FALSE)      ;irrelevant
   (set! (-> nt bounded-p) bounded-p))

 (define-cfn c-pointer-ref (type::ScmNativeType* ptr::void*)
   :static
   (return (Scm_MakeNativeHandleSimple (* (cast char** ptr)) (SCM_OBJ type))))

 (define-cfn c-pointer-set (_::ScmNativeType* ptr::void* obj)
   ::void :static
   (unless (and (SCM_NATIVE_HANDLE_P obj)
                (SCM_C_POINTER_P (-> (SCM_NATIVE_HANDLE obj) type)))
     (Scm_Error "c-pointer handle required, bot got: %S" obj))
   (set! (* (cast void** ptr)) (-> (SCM_NATIVE_HANDLE obj) ptr)))

 (define-cfn %make-c-pointer-type-fn (pointer-type-name::(const char *)
                                      pointee-type)
   :static
   (let* ([z::ScmCPointer*
           (SCM_NEW_INSTANCE ScmCPointer (& Scm_CPointerClass))])
     ;; Fill in common fields
     (init-native-type-common (& (-> z common))
                              pointer-type-name
                              (SCM_OBJ SCM_CLASS_TOP)
                              "ScmNativeHandle*"
                              (sizeof (.type void*))
                              (SCM_ALIGNOF (.type void*))
                              TRUE
                              native_handle_typeP
                              c-pointer-ref
                              c-pointer-set)
     ;; Fill in type-specific fields
     (SCM_ASSERT (SCM_NATIVE_TYPE_P pointee-type))
     (set! (-> z pointee_type) (SCM_NATIVE_TYPE pointee-type))
     (return (SCM_OBJ z))))
 )

(define-cproc %make-c-pointer-type (pointer-type-name::<const-cstring>
                                    pointee-type)
  ;; Special treatment of void*; it's convenient to have it as a singleton.
  ;; TODO: We can make single-level indiration all singletons by caching it,
  ;; for thye appeare frequently.
  (if (SCM_EQ pointee-type (Scm_NativeVoidType))
    (return (Scm_NativeVoidPointerType))
    (return (%make-c-pointer-type-fn pointer-type-name pointee-type))))

(define-cproc %make-c-function-type (type-name::<const-cstring>
                                     return-type
                                     argument-types
                                     variadic?::<boolean>)
  (let* ([z::ScmCFunction*
          (SCM_NEW_INSTANCE ScmCFunction (& Scm_CFunctionClass))])
    ;; Fill in common fields
    (init-native-type-common (& (-> z common))
                             type-name
                             (SCM_OBJ SCM_CLASS_TOP)
                             "ScmNativeHandle*"
                             (sizeof (.type void*))
                             (SCM_ALIGNOF (.type void*))
                             TRUE
                             native_handle_typeP
                             NULL
                             NULL)
    ;; Fill in type-specific fields
    (SCM_ASSERT (SCM_NATIVE_TYPE_P return-type))
    (set! (-> z return_type) (SCM_NATIVE_TYPE return-type))
    (set! (-> z argument_types) argument-types)
    (set! (-> z variadic) (?: variadic? TRUE FALSE))
    (return (SCM_OBJ z))))

(inline-stub
 (initcode
  (set! Scm_NativeVoidPointerType_
        (%make-c-pointer-type-fn "void*" Scm_NativeVoidType_))
  (Scm_MakeBinding (Scm_GaucheModule) (SCM_SYMBOL '<void*>)
                   (Scm_NativeVoidPointerType)
                   SCM_BINDING_INLINABLE)))

;; For array, we keep element-type and dimensions in dedicated fields.
;; Each <dim> is a nonnegative fixnum.  The first <dim> can be '*,
;; indicating it is not specified (C allows it).
(define-cproc %make-c-array-type (type-name::<const-cstring>
                                  element-type
                                  size::<fixnum>
                                  alignment::<fixnum>
                                  dimensions)
  (let* ([z::ScmCArray*
          (SCM_NEW_INSTANCE ScmCArray (& Scm_CArrayClass))])
    ;; Fill in common fields
    ;; If the first element of dimensions is no a fixnum
    ;; (then it must be '* --- checked by the caller), the array
    ;; is "unbounded".
    (init-native-type-common (& (-> z common))
                             type-name
                             (SCM_OBJ SCM_CLASS_TOP)
                             "ScmNativeHandle*"
                             size
                             alignment
                             (?: (SCM_INTP (SCM_CAR dimensions)) TRUE FALSE) ;bounded?
                             native_handle_typeP
                             NULL
                             NULL)
    ;; Fill in type-specific fields
    (SCM_ASSERT (SCM_NATIVE_TYPE_P element-type))
    (set! (-> z element_type) (SCM_NATIVE_TYPE element-type))
    (set! (-> z dimensions) dimensions)
    (return (SCM_OBJ z))))

;; For struct/union, we keep tag and field-list in dedicated fields.
(define-cproc %make-c-struct/union-type (klass::<class>
                                         type-name::<const-cstring>
                                         size::<fixnum>
                                         alignment::<fixnum>
                                         bounded-p::<int>
                                         tag-name::<symbol>?
                                         field-list)
  (let* ([z::ScmCStruct* (SCM_NEW_INSTANCE ScmCStruct klass)])
    ;; Fill in common fields
    (init-native-type-common (& (-> z common))
                             type-name
                             (SCM_OBJ SCM_CLASS_TOP)
                             "ScmNativeHandle*"
                             size
                             alignment
                             bounded-p
                             native_handle_typeP
                             c-pointer-ref
                             c-pointer-set)
    ;; Fill in type-specific fields
    (set! (-> z tag) (?: tag-name (SCM_OBJ tag-name) SCM_FALSE))
    (set! (-> z fields) field-list)
    (return (SCM_OBJ z))))

(inline-stub
 ;; Size of enum is implementation-dependent.  For our purpose, the external
 ;; code will be compiled with the same compiler & options as Gauche core,
 ;; so we compute them at initialization time.
 ;; TODO: C++ enums may use different size.  How shall we check them?
 (define-cvar enum_size_8::int :static)
 (define-cvar enum_size_16::int :static)
 (define-cvar enum_size_32::int :static)
 (define-cvar enum_size_64::int :static)

 (define-ctype enum_size_8_t ::(.enum ((enum_8 255))))
 (define-ctype enum_size_16_t ::(.enum ((enum_16 65535))))
 (define-ctype enum_size_32_t ::(.enum ((enum_32 UINT32_MAX))))
 (define-ctype enum_size_64_t ::(.enum ((enum_64 UINT64_MAX))))

 (initcode
  (set! enum_size_8 (sizeof enum_size_8_t))
  (set! enum_size_16 (sizeof enum_size_16_t))
  (set! enum_size_32 (sizeof enum_size_32_t))
  (set! enum_size_64 (sizeof enum_size_64_t)))

 (define-cproc implicit-enum-size (width::<fixnum>) ::<fixnum>
   (case width
     [(8) (return enum_size_8)]
     [(16) (return enum_size_16)]
     [(32) (return enum_size_32)]
     [else (return enum_size_64)]))
 )

(inline-stub
 (define-cproc %native-type-integral? (nt::<native-type>) ::<boolean>
   (return (Scm_NativeTypeIntegralP nt)))

 ;; Pick the builtin native integer type of the given byte size and signedness.
 (define-cproc %native-int-type-of-size (size::<fixnum> unsigned?::<boolean>)
   (case size
     [(1) (return (?: unsigned? (Scm_NativeUint8Type) (Scm_NativeInt8Type)))]
     [(2) (return (?: unsigned? (Scm_NativeUint16Type) (Scm_NativeInt16Type)))]
     [(4) (return (?: unsigned? (Scm_NativeUint32Type) (Scm_NativeInt32Type)))]
     [(8) (return (?: unsigned? (Scm_NativeUint64Type) (Scm_NativeInt64Type)))]
     [else
      (Scm_Error "enum underlying integer size must be 1, 2, 4 or 8 bytes, \
                  but got %ld" (cast long size))
      (return SCM_UNDEFINED)]))

 (define-cproc %make-c-enum-type (type-name::<const-cstring>
                                  c-type-name::<const-cstring>
                                  underlying
                                  size::<fixnum>
                                  alignment::<fixnum>
                                  tag-name::<symbol>?
                                  type-spec
                                  enumerator-alist)
   (SCM_ASSERT (SCM_NATIVE_TYPE_P underlying))
   (let* ([u::ScmNativeType* (SCM_NATIVE_TYPE underlying)]
          [z::ScmCEnum* (SCM_NEW_INSTANCE ScmCEnum (& Scm_CEnumClass))]
          [nt::ScmNativeType* (& (-> z common))])
     ;; Borrow integer accessors from the underlying integral native type.
     ;; (We copy field-by-field rather than the whole struct, to keep z's
     ;; instance header pointing at Scm_CEnumClass.)
     (set! (-> nt name) (SCM_INTERN type-name))
     (set! (-> nt super) (SCM_OBJ SCM_CLASS_INTEGER))
     (set! (-> nt c-type-name) c-type-name)
     (set! (-> nt c-of-type) (-> u c-of-type))
     (set! (-> nt c-ref) (-> u c-ref))
     (set! (-> nt c-set) (-> u c-set))
     (set! (-> nt c-typecheck-name) (-> u c-typecheck-name))
     (set! (-> nt c-boxer-name) (-> u c-boxer-name))
     (set! (-> nt c-unboxer-name) (-> u c-unboxer-name))
     (set! (-> nt size) (cast size_t size))
     (set! (-> nt alignment) (cast size_t alignment))
     (set! (-> nt unsigned-p) (-> u unsigned-p))
     (set! (-> nt bounded-p) TRUE)
     ;; Enum-specific fields
     (set! (-> z tag) (?: tag-name (SCM_OBJ tag-name) SCM_FALSE))
     (set! (-> z type-spec) type-spec)
     (set! (-> z enumerator-alist) enumerator-alist)
     (return (SCM_OBJ z))))
 )

;; Validate the enumerator list and turn it into an alist ((id . value) ...).
;; Each enumerator is a symbol or (symbol integer-value).  A symbol without an
;; explicit value takes the previous enumerator's value plus one; the first
;; such symbol takes 0.  As in C, duplicate ids are rejected, but duplicate
;; values (aliases) are allowed.
(define (%build-enum-alist enumerators)
  (let loop ([es enumerators] [next 0] [seen '()] [acc '()])
    (match es
      [() (reverse acc)]
      [(e . rest)
       (receive (id val)
           (match e
             [(? symbol? id) (values id next)]
             [((? symbol? id) (? exact-integer? val)) (values id val)]
             [_ (error "bad c-enum enumerator; must be a symbol or \
                        (symbol integer), but got:" e)])
         (when (memq id seen)
           (error "duplicate enumerator id in c-enum:" id))
         (loop rest (+ val 1) (cons id seen) (cons (cons id val) acc)))]
      [_ (error "bad c-enum enumerator list; must be a proper list, but got:"
                enumerators)])))

;; Smallest of {8,16,32,64} that can represent [lo, hi].  If lo is negative
;; the range is signed and needs one extra bit for the sign.  integer-length
;; gives the number of significant (sign-excluded) bits of each end.
(define (%enum-bit-width lo hi)
  (let1 bits (if (negative? lo)
               (+ 1 (max (integer-length lo) (integer-length hi)))
               (integer-length hi))
    (cond [(<= bits 8) 8]
          [(<= bits 16) 16]
          [(<= bits 32) 32]
          [(<= bits 64) 64]
          [else (error "c-enum value range too large to fit in 64 bits:"
                       (list lo hi))])))

;;;
;;; Pointer fill gate
;;;

;; A parameter to restrict extent of native-ptr-fill! availability.  We
;; don't want it to be called arbitrarily.
(define native-ptr-fill-enabled? (make-parameter #f))

;; native-ptr-fill! TARGET TSTART SIZE TYPE OBJ
;;   Fill region [TSTART, TSTART+SIZE) of TARGET (a u8vector) with the
;;   native pointer representation of OBJ according to TYPE.
;;   TYPE must be <c-string>, a pointer/array/function native type, or <top>.
;;   Only callable while native-ptr-fill-enabled? is #t.
(define native-ptr-fill!
  (let1 impl (module-binding-ref 'gauche.bootstrap '%%native-ptr-fill!)
    (^[target tstart size type obj]
      (unless (native-ptr-fill-enabled?)
        (error "native-ptr-fill! called outside native dispatch context; \
                set native-ptr-fill-enabled? to #t via parameterize"))
      (impl target tstart size type obj))))

;;;
;;; Make exported symbol visible from outside
;;;

;; We export those only to be used by selected library modules to
;; workaround dependency issues. User programs should use gauche.native-type
;; for these identifiers.
(export <native-handle> <c-pointer> <c-array> <c-struct> <c-union> <c-function>
        <c-enum>)

(let ((xfer (with-module gauche.internal %transfer-bindings)))
  (xfer (current-module)
        (find-module 'gauche)
        '(<type-constructor-meta>
          <descriptive-type>
          <native-type>
          <^> </> <?> <Tuple> <List> <Vector> <Assortment>
          type? subtype? of-type?))
  (xfer (current-module)
        (find-module 'gauche.internal)
        '(construct-type
          deconstruct-type
          wrap-with-proxy-type
          proxy-type-ref
          proxy-type-id
          ;; these are used by native-supp.scm, so make them visible from
          ;; gauche.internal
          <c-pointer> <c-array> <c-struct> <c-union> <c-function>
          ;; followings are called from procedure-type (libproc)
          reconstruct-procedure-type
          compute-procedure-type
          construct-procedure-type)))
