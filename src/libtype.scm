;;;
;;; libtype.scm - type-related stuff
;;;
;;;   Copyright (c) 2021  Shiro Kawai  <shiro@acm.org>
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

;; This must be the first form to prevents generation of *.sci file
(declare (keep-private-macro define-type-constructor))

;; In Gauche, types are a data structure that appears in both compile-time
;; and run-time, describes metalevel properties of run-time data.
;;
;; Gauche has two kinds of types--prescriptive types and descriptive types.
;; Prescriptive types are the types that are actually used to generate the
;; actual data---we also call it classes.  Descriptive types are, otoh,
;; used only to descrive the nature of data at the certain point of program
;; execution---for example, you may say the argument must be either <integer>
;; or <boolean>.  The descriptive type can't be used to generate an instance,
;; only to be used to validate and/or infer the actual (generative) type of
;; data.
;;
;; Descriptive types can be constructed by type constructors.  Since the
;; compiler needs to know about types, type constructor expressions
;; are evaluated at the compile time, much like macros.
;;
;; We could implemented types with macros, but it would be tricky.  Unlike
;; macros, type expression needs to evaluate from inside to outside, just
;; like the ordinary expression.  It's more like compile-time constant folding.
;;
;; Since type handing is deeply intertwined with compiler, we let the compiler
;; recognize type expression specifically, rather than reusing existing
;; evaluation mechanism.  When the compile sees (C x ...) and C has an
;; inlineable binding to an instance of <type-constructor-meta>, it recognizes
;; type expression.

;; This module is not meant to be `use'd.   It is just to hide
;; auxiliary procedures from the rest of the system.  The necessary
;; bindings are injected into 'gauche' module at the initialization time.
(define-module gauche.typeutil)
(select-module gauche.typeutil)
(use util.match)


(inline-stub
 (.include "gauche/priv/classP.h")
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
 ;;   subtype? :: <descriptive-type> type -> <boolean>
 ;;     Returns true iff the descriptive type is a subtype of TYPE, which may
 ;;     be a class or another descriptive type.  Note that proxy types and
 ;;     native types are already excluded, as well as the case where reflective
 ;;     case (subtype? x x) and the base cases (subtype? x <top>).
 ;;     NB: if TYPE is a descriptive type different kind of <descriptive-type>,
 ;;     you may want to call (~ type'supertype?) to determine if it thinks
 ;;     <descriptive-type> its subtype.
 ;;   supertype? :: <descriptive-type> type -> <boolean>
 ;;     Returns true iff the descriptive type is a supertype of TYPE.
 ;;     Trivial cases are already excluded, esp., TYPE won't be the
 ;;     same kind of <descriptive-type>.

 (define-ctype ScmTypeConstructor
   ::(.struct ScmTypeConstructorRec
              (common::ScmClass
               initializer::ScmObj
               deconstructor::ScmObj
               validator::ScmObj
               subtypeP::ScmObj
               supertypeP::ScmObj)))

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
              (hdr::ScmInstance
               name::ScmObj
               constructorArgs::ScmObj)))

 (define-cclass <descriptive-type> :base :private :no-meta
   "ScmDescriptiveType*" "Scm_DescriptiveTypeClass"
   (c "SCM_CLASS_METACLASS_CPL+1")
   ((name))
   (allocator (let* ([z::ScmDescriptiveType*
                      (SCM_NEW_INSTANCE ScmDescriptiveType klass)])
                (cast void initargs)    ;suppress unused warning
                (set! (-> z name) SCM_FALSE)
                (set! (-> z constructorArgs) SCM_FALSE)
                (return (SCM_OBJ z)))))

 (define-ctype ScmNativeType
   ::(.struct ScmNativeTypeRec
              (hdr::ScmHeader
               name::ScmObj
               c-of-type::(.function (obj) ::int *)
               super::ScmObj
               of-type::ScmObj)))       ; obj -> bool

 (define-cclass <native-type> :built-in :private :no-meta
   "ScmNativeType*" "Scm_NativeTypeClass"
   (c "SCM_CLASS_METACLASS_CPL+1")
   ((name)
    (super)
    (of-type))
   (printer (Scm_Printf port "#<native-type %S>" (-> (SCM_NATIVE_TYPE obj) name))))
 )

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
          (if (-> (SCM_NATIVE_TYPE type) c-of-type)
            (return (SCM_MAKE_BOOL
                     (funcall (-> (SCM_NATIVE_TYPE type) c-of-type) obj)))
            (return (Scm_VMApply1 (-> (SCM_NATIVE_TYPE type) of-type) obj)))]
         [(SCM_CLASSP type)
          (return (Scm_VMIsA obj (SCM_CLASS type)))]
         [else
          (Scm_Error "Second argument of of-type? must be a type, but got: %S"
                     type)]))

 (define-cproc of-type? (obj type) Scm_VMOfType)
 )

;;;
;;; subtype? type-or-class type-or-class
;;;

(define-cproc subtype? (sub super)
  (loop
   (cond
    ;; Strip proxy types first
    [(SCM_PROXY_TYPE_P sub)
     (set! sub (SCM_OBJ (Scm_ProxyTypeRef (SCM_PROXY_TYPE sub))))]
    [(SCM_PROXY_TYPE_P super)
     (set! super (SCM_OBJ (Scm_ProxyTypeRef (SCM_PROXY_TYPE super))))]
    ;; Both are classes, we can use subclass?
    [(and (SCM_CLASSP sub) (SCM_CLASSP super))
     (return (SCM_MAKE_BOOL (Scm_SubclassP (SCM_CLASS sub) (SCM_CLASS super))))]
    ;; Filter out the trivial cases
    [(SCM_EQ super (SCM_OBJ SCM_CLASS_TOP)) (return SCM_TRUE)]
    [(SCM_EQ sub (SCM_OBJ SCM_CLASS_BOTTOM)) (return SCM_TRUE)]
    [(SCM_EQ super sub) (return SCM_TRUE)]
    ;; Native types can be a subtype of a class.  No type (except BOTTOM) can
    ;; be a subtype of a native type.
    [(SCM_NATIVE_TYPE_P sub)
     (let* ([klass (-> (SCM_NATIVE_TYPE sub) super)])
       (SCM_ASSERT (SCM_CLASSP klass))
       (set! sub klass))]
    [(SCM_NATIVE_TYPE_P super) (return FALSE)]
    ;; Delegate descriptive types to its handlers.
    [(SCM_DESCRIPTIVE_TYPE_P sub)
     (let* ([k::ScmClass* (Scm_ClassOf sub)])
       (SCM_ASSERT (SCM_TYPE_CONSTRUCTOR_META_P k))
       (return (Scm_VMApply2 (-> (SCM_TYPE_CONSTRUCTOR_META k) subtypeP)
                             sub super)))]
    [(SCM_DESCRIPTIVE_TYPE_P super)
     (let* ([k::ScmClass* (Scm_ClassOf super)])
       (SCM_ASSERT (SCM_TYPE_CONSTRUCTOR_META_P k))
       (return (Scm_VMApply2 (-> (SCM_TYPE_CONSTRUCTOR_META k) supertypeP)
                             super sub)))]
    [else (return SCM_FALSE)])))

;; Call this in subtype? after dealing with typical cases.
(define (%delegate-to-super type super)
  (and (is-a? super <descriptive-type>)
       ((~ (class-of super)'supertype?) super type)))

;;;
;;; Descriptive type constructors
;;;

;; define-type-constructor name supers
;;   (slot ...)
;;   of-type

(define-syntax define-type-constructor
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ name supers slots initializer deconstructor validator sub? sup?)
        (let ([meta-name (rxmatch-if (#/^<(.*)>$/ (symbol->string name))
                             [_ trimmed]
                           (string->symbol #"<~|trimmed|-meta>")
                           (string->symbol #"~|name|-meta"))]
              [supers (if (null? supers)
                        (list (r'<descriptive-type>))
                        supers)])
          (quasirename r
            `(begin
               (define-class ,meta-name (<type-constructor-meta>) ())
               (define-class ,name ,supers ,slots
                 :metaclass ,meta-name
                 :initializer ,initializer
                 :deconstructor ,deconstructor
                 :validator ,validator
                 :subtype? ,sub?
                 :supertype? ,sup?))))]))))

(define-method allocate-instance ((t <descriptive-type>) initargs)
  (error "Abstract type instance cannot instantiate a concrete object:" t))

(define-method write-object ((t <descriptive-type>) port)
  (format port "#~a" (~ t'name)))

;; Equality is used when consolidate literals.  It's not lightweight
;; (it calls deconstructor, which allocates).
(define-method object-equal? ((x <descriptive-type>) (y <descriptive-type>))
  (and (equal? (class-of x) (class-of y))
       (equal? (deconstruct-type x) (deconstruct-type y))))

;; Public interface to construct a descriptive type.
(define (construct-type meta args)
  (rlet1 type (make meta)
    ((~ meta'initializer) type args)))

(define (lazy-construct-type meta args)
  (rlet1 type (make meta)
    (slot-set! type 'constructor-args args)))

;; Internal API, required to precompile descriptive type constant
(define-method deconstruct-type ((t <descriptive-type>))
  ((~ (class-of t)'deconstructor) t))

;; This is called from initialization of precompiled code to recover
;; descripitve type instance.
(inline-stub
 (define-cfn Scm_ConstructType (ctor args)
   (unless (Scm_TypeConstructorP ctor)
     (SCM_TYPE_ERROR ctor "<type-constructor-meta>"))
   (let* ([type (Scm_NewInstance (SCM_CLASS ctor) (sizeof ScmDescriptiveType))])
     (Scm_ApplyRec2 (-> (cast ScmTypeConstructor* ctor) initializer) type args)
     (return type)))
 )

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
    (and (subtype-Tuple (~ type'arguments) (~ super'arguments))
         (subtype-Tuple (~ super'results) (~ type'results)))
    (%delegate-to-super type super)))

;; No types other than <^ ...> can be a subtype of <^>, but that case is
;; already handled.
(define (supertype-^ type sub) #f)

;; Internal API - called from procedure-type (libproc)
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
(define (compute-procedure-type proc)
  (define (%procedure-type proc)
    (if-let1 clinfo (case-lambda-info proc)
      (construct-type </> (map (^v (%procedure-type (caddr v))) clinfo))
      (let1 top (%class->proxy <top>)
        (construct-type <^>
                        `(,@(make-list (~ proc'required) top)
                          ,@(if (~ proc'optional) '(*) '())
                          -> *)))))
  (define (%method-type meth)
    (construct-type <^>
                    `(,@(map %class->proxy (~ meth'specializers))
                      ,@(if (~ meth'optional) '(*) '())
                      -> *)))
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

(define-type-constructor <^> ()
  ((arguments :init-keyword :arguments)    ; <Tuple>
   (results   :init-keyword :results))     ; <Tuple>
  init-^
  deconstruct-^
  validate-^
  subtype-^
  supertype-^)

;;;
;;; Class: </>
;;;   Creates a union type.
;;;

(define (init-/ type args)
  (assume (every (cut is-a? <> <type>) args))
  (slot-set! type 'name (make-compound-type-name '/ args))
  (slot-set! type 'members args))

(define (deconstruct-/ type)
  (~ type'members))

(define (validate-/ type obj)
  (any (cut of-type? obj <>) (~ type'members)))

(define (subtype-/ type super)
  (if (is-a? super </>)
    (every (cut subtype? <> super) (~ type'members))
    (%delegate-to-super type super)))

(define (supertype-/ type sub) (any (cut subtype? sub <>) (~ type'members)))

(define-type-constructor </> ()
  ((members :init-keyword :members))
  init-/
  deconstruct-/
  validate-/
  subtype-/
  supertype-/)

;;;
;;; Class: <?>
;;;   Creates a boolean-optional type, that is, <type> or #f.
;;;

(define (init-? type args)
  (let1 ptype (car args)
    (assume (is-a? ptype <type>))
    (slot-set! type 'name (make-compound-type-name '? `(,ptype)))
    (slot-set! type 'primary-type ptype)))

(define (deconstruct-? type)
  (list (~ type'primary-type)))

(define (validate-? type obj)
  (or (eqv? obj #f) (of-type? obj (~ type'primary-type))))

(define (subtype-? type super)
  (if (is-a? super <?>)
    (subtype? (~ type'primary-type) (~ super'primary-type))
    (%delegate-to-super type super)))

(define (supertype-? type sub)
  (subtype? sub (~ type'primary-type)))

(define-type-constructor <?> ()
  ((primary-type :init-keyword :primary-type))
  init-?
  deconstruct-?
  validate-?
  subtype-?
  supertype-?)

;;;
;;; Class: <Tuple>
;;;   Fixed-lenght list, each element having its own type constraints.
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
      (%delegate-to-super type super)))

(define (supertype-Tuple type sub) #f)

(define-type-constructor <Tuple> ()
  ((elements    :init-keyword :elements)
   (allow-rest? :init-keyword :allow-rest?))
  init-Tuple
  deconstruct-Tuple
  validate-Tuple
  subtype-Tuple
  supertype-Tuple)

;;;
;;; Class: <List>
;;;   A list of specified types.
;;;

(define (init-List type args)
  (apply (^[etype :optional (min #f) (max #f)]
           (slot-set! type 'name
                      (make-min-max-len-type-name 'List (list etype) min max))
           (slot-set! type 'element-type etype)
           (slot-set! type 'min-length min)
           (slot-set! type 'max-length max))
         args))

(define (deconstruct-List type)
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

(define (subtype-List type super)
  (or (eqv? super <list>)
      (and (is-a? super <List>)
           (subtype? (~ type'element-type) (~ super'element-type))
           (>= (or (~ type'min-length) 0)
               (or (~ super'min-length) 0))
           (<= (or (~ type'max-length) +inf.0)
               (or (~ super'max-length) +inf.0)))
      (%delegate-to-super type super)))

(define (supertype-List type sub) #f)

(define-type-constructor <List> ()
  ((element-type :init-keyword :element-type)
   (min-length :init-keyword :min-length :init-value #f)
   (max-length :init-keyword :max-length :init-value #f))
  init-List
  deconstruct-List
  validate-List
  subtype-List
  supertype-List)

;;;
;;; <Vector> element-type [min-length [max-length]]
;;;

(define (init-Vector type args)
  (apply (^[etype :optional (min #f) (max #f)]
           (slot-set! type 'name
                      (make-min-max-len-type-name 'Vector (list etype) min max))
           (slot-set! type 'element-type etype)
           (slot-set! type 'min-length min)
           (slot-set! type 'max-length max))
         args))

(define (deconstruct-Vector type)
  (list (~ type'element-type) (~ type'min-length) (~ type'max-length)))

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

(define (subtype-Vector type super)
  (or (eqv? super <vector>)
      (and (is-a? super <Vector>)
           (subtype? (~ type'element-type) (~ super'element-type))
           (>= (or (~ type'min-length) 0)
               (or (~ super'min-length) 0))
           (<= (or (~ type'max-length) +inf.0)
               (or (~ super'max-length) +inf.0)))
      (%delegate-to-super type super)))

(define (supertype-Vector type sub) #f)

(define-type-constructor <Vector> ()
  ((element-type :init-keyword :element-type)
   (min-length :init-keyword :min-length :init-value #f)
   (max-length :init-keyword :max-length :init-value #f))
  init-Vector
  deconstruct-Vector
  validate-Vector
  subtype-Vector
  supertype-Vector)

;;;
;;; Types for bridging Scheme and C
;;;

;; Each of these types has a corresponding cgen-type that maintains
;; the knowledge how it is represented in C.

(inline-stub
 (define-cfn Scm_MakeNativeType (name::(const char*)
                                 super
                                 c-of-type::(.function (obj)::int *))
   (let* ([z::ScmNativeType* (SCM_NEW ScmNativeType)])
     (SCM_SET_CLASS z (& Scm_NativeTypeClass))
     (set! (-> z name) (SCM_INTERN name))
     (set! (-> z super) super)
     (set! (-> z c-of-type) c-of-type)
     (set! (-> z of-type) SCM_FALSE)
     (return (SCM_OBJ z))))

 (define-cise-stmt define-native-type
   [(_ name super fn)
    `(let* ([z (Scm_MakeNativeType ,name ,super ,fn)])
       (Scm_MakeBinding (Scm_GaucheModule)
                        (SCM_SYMBOL (-> (SCM_NATIVE_TYPE z) name)) z
                        SCM_BINDING_INLINABLE))])

 (define-cfn native_fixnumP (obj) ::int :static
   (return (SCM_INTP obj)))

 ;; NB: Range check is also in ext/uvector/uvector.scm.  May be integrated.
 (define-cfn native_s8P (obj) ::int :static
   (return (and (SCM_INTP obj)
                (>= (SCM_INT_VALUE obj) -128)
                (<= (SCM_INT_VALUE obj) 127))))
 (define-cfn native_u8P (obj) ::int :static
   (return (and (SCM_INTP obj)
                (>= (SCM_INT_VALUE obj) 0)
                (<= (SCM_INT_VALUE obj) 255))))
 (define-cfn native_s16P (obj) ::int :static
   (return (and (SCM_INTP obj)
                (>= (SCM_INT_VALUE obj) -32768)
                (<= (SCM_INT_VALUE obj) 32767))))
 (define-cfn native_u16P (obj) ::int :static
   (return (and (SCM_INTP obj)
                (>= (SCM_INT_VALUE obj) 0)
                (<= (SCM_INT_VALUE obj) 65535))))
 (define-cfn native_s32P (obj) ::int :static
   (.if (== SIZEOF_LONG 4)
        (return (or (SCM_INTP obj)
                    (and (SCM_BIGNUMP obj)
                         (>= (Scm_NumCmp obj '#x-8000_0000) 0)
                         (<= (Scm_NumCmp obj '#x7fff_ffff) 0))))
        (return (and (SCM_INTP obj)
                     (>= (SCM_INT_VALUE obj) #x-8000_0000)
                     (<= (SCM_INT_VALUE obj) #x7fff_ffff)))))
 (define-cfn native_u32P (obj) ::int :static
   (.if (== SIZEOF_LONG 4)
        (return (or (and (SCM_INTP obj)
                         (>= (SCM_INT_VALUE obj) 0))
                    (and (SCM_BIGNUMP obj)
                         (>= (Scm_Sign obj) 0)
                         (<= (Scm_NumCmp obj '#xffff_ffff) 0))))
        (return (and (SCM_INTP obj)
                     (>= (SCM_INT_VALUE obj) 0)
                     (<= (SCM_INT_VALUE obj) #xffff_ffff)))))
 (define-cfn native_s64P (obj) ::int :static
   (return (or (SCM_INTP obj)
               (and (SCM_BIGNUMP obj)
                    (>= (Scm_NumCmp obj '#x-8000_0000_0000_0000) 0)
                    (<= (Scm_NumCmp obj '#x-7fff_ffff_ffff_ffff) 0)))))
 (define-cfn native_u64P (obj) ::int :static
   (return (or (and (SCM_INTP obj)
                    (>= (SCM_INT_VALUE obj) 0))
               (and (SCM_BIGNUMP obj)
                    (>= (Scm_Sign obj) 0)
                    (<= (Scm_NumCmp obj '#xffff_ffff_ffff_ffff) 0)))))

 (define-cfn native_shortP (obj) ::int :static
   (return (and (SCM_INTP obj)
                (>= (SCM_INT_VALUE obj) SHRT_MIN)
                (<= (SCM_INT_VALUE obj) SHRT_MAX))))
 (define-cfn native_ushortP (obj) ::int :static
   (return (and (SCM_INTP obj)
                (>= (SCM_INT_VALUE obj) 0)
                (<= (SCM_INT_VALUE obj) USHRT_MAX))))
 (define-cfn native_intP (obj) ::int :static
   (.if (== SIZEOF_LONG 4)
        (if (SCM_BIGNUMP obj)
          (let* ([oor::int FALSE]
                 [v::long (Scm_GetIntegerClamp obj SCM_CLAMP_BOTH (& oor))])
            (return (and (not oor)
                         (>= v INT_MIN)
                         (<= v INT_MAX))))
          (return (SCM_INTP obj)))
        (if (SCM_INTP obj)
          (let* ([v::long (SCM_INT_VALUE obj)])
            (return (and (>= v INT_MIN)
                         (<= v INT_MAX))))
          (return FALSE))))
 (define-cfn native_uintP (obj) ::int :static
   (.if (== SIZEOF_LONG 4)
        (if (SCM_BIGNUMP obj)
          (let* ([oor::int FALSE]
                 [v::u_long (Scm_GetIntegerUClamp obj SCM_CLAMP_BOTH (& oor))])
            (return (not oor)))
          (return (and (SCM_INTP obj) (>= (SCM_INT_VALUE obj) 0))))
        (return (and (SCM_INTP obj)
                     (>= (SCM_INT_VALUE obj) 0)
                     (<= (SCM_INT_VALUE obj) UINT_MAX)))))
 (define-cfn native_longP (obj) ::int :static
   (if (SCM_BIGNUMP obj)
     (let* ([oor::int FALSE])
       (cast void (Scm_GetIntegerClamp obj SCM_CLAMP_BOTH (& oor)))
       (return (not oor)))
     (return (SCM_INTP obj))))
 (define-cfn native_ulongP (obj) ::int :static
   (if (SCM_BIGNUMP obj)
     (let* ([oor::int FALSE])
       (cast void (Scm_GetIntegerUClamp obj SCM_CLAMP_BOTH (& oor)))
       (return (not oor)))
     (return (and (SCM_INTP obj) (>= (SCM_INT_VALUE obj) 0)))))

 ;; we don't range-check flonums
 (define-cfn native_realP (obj) ::int :static
   (return (SCM_REALP obj)))

 (define-cfn native_cstrP (obj) ::int :static
   (return (SCM_STRINGP obj)))

 ;; subrs returning <void> actually return #<undef>
 (define-cfn native_voidP (obj) ::int :static
   (return (SCM_UNDEFINEDP obj)))

 (define-cfn native_iportP (obj) ::int :static
   (return (SCM_IPORTP obj)))
 (define-cfn native_oportP (obj) ::int :static
   (return (SCM_OPORTP obj)))
 (define-cfn native_closureP (obj) ::int :static
   (return (SCM_CLOSUREP obj)))

 (define-cvar intclass :static)
 (define-cvar realclass :static)
 (define-cvar strclass :static)
 (initcode
  (set! intclass (Scm_GlobalVariableRef (Scm_GaucheModule)
                                        (SCM_SYMBOL (SCM_INTERN "<integer>"))
                                        0))
  (set! realclass (Scm_GlobalVariableRef (Scm_GaucheModule)
                                         (SCM_SYMBOL (SCM_INTERN "<real>"))
                                         0))
  (set! strclass (Scm_GlobalVariableRef (Scm_GaucheModule)
                                        (SCM_SYMBOL (SCM_INTERN "<string>"))
                                        0))
  (define-native-type "<fixnum>"  intclass native_fixnumP)
  (define-native-type "<short>"   intclass native_shortP)
  (define-native-type "<ushort>"  intclass native_ushortP)
  (define-native-type "<int>"     intclass native_intP)
  (define-native-type "<uint>"    intclass native_uintP)
  (define-native-type "<long>"    intclass native_longP)
  (define-native-type "<ulong>"   intclass native_ulongP)
  (define-native-type "<int8>"    intclass native_s8P)
  (define-native-type "<uint8>"   intclass native_u8P)
  (define-native-type "<int16>"   intclass native_s16P)
  (define-native-type "<uint16>"  intclass native_u16P)
  (define-native-type "<int32>"   intclass native_s32P)
  (define-native-type "<uint32>"  intclass native_u32P)
  (define-native-type "<int64>"   intclass native_s64P)
  (define-native-type "<uint64>"  intclass native_u64P)
  (define-native-type "<float>"   realclass native_realP)
  (define-native-type "<double>"  realclass native_realP)
  (define-native-type "<const-cstring>" strclass native_cstrP)
  (define-native-type "<input-port>"  (SCM_OBJ SCM_CLASS_PORT) native_iportP)
  (define-native-type "<output-port>" (SCM_OBJ SCM_CLASS_PORT) native_oportP)
  (define-native-type "<closure>" (SCM_OBJ SCM_CLASS_PROCEDURE) native_closureP)
  (define-native-type "<void>"    (SCM_OBJ SCM_CLASS_TOP) native_voidP)
  ))

;;;
;;; Make exported symbol visible from outside
;;;

;; TRANSIENT: the inlinable flag is only necessary in 0.9.10 -> 0.9.11
;; transition, for define-class doesn't create inlinable binding in 0.9.10 but
;; it does in 0.9.11.
(let ((xfer (with-module gauche.internal %transfer-bindings)))
  (xfer (current-module)
        (find-module 'gauche)
        '(<type-constructor-meta>
          <descriptive-type>
          <^> </> <?> <Tuple> <List> <Vector>
          subtype? of-type?)
        '(inlinable))
  (xfer (current-module)
        (find-module 'gauche.internal)
        '(construct-type
          deconstruct-type
          wrap-with-proxy-type
          proxy-type-ref
          proxy-type-id
          ;; followings are called from procedure-type (libproc)
          reconstruct-procedure-type
          compute-procedure-type)))
