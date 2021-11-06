;;;
;;; gauche.cgen.type - Stub type management
;;;
;;;   Copyright (c) 2004-2021  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.cgen.type
  (use srfi-13)
  (use text.tr)
  (use gauche.mop.instance-pool)
  (export <cgen-type> cgen-type-from-name make-cgen-type
          cgen-boxer-name cgen-unboxer-name cgen-pred-name
          cgen-box-expr cgen-box-tail-expr cgen-unbox-expr cgen-pred-expr
          cgen-type-maybe? cgen-return-stmt)
  )
(select-module gauche.cgen.type)

;; Stub types augment Gauche's runtime type system, adding information
;; on how to generate C code fragments to typecheck, box and unbox the
;; value of the given type.
;;
;; Each stub type has a "boxer" and an "unboxer".  A boxer is a C name
;; of a function or a macro that takes an object of C type of the stub
;; type and returns a Scheme object.  An unboxer is a C name of a function
;; or a macro that takes Scheme object and checks its vailidy, then
;; returns a C object of the C type or throws an error.
;;
;; We have a few categories of stub types.
;;
;;  - Native types.  Some native types can represent a subset of Scheme 
;;    types; e.g. <int16> native type corresponds to C's int16_t, 
;;    and covers a subset of Scheme <integer> type.  See src/libtype.scm
;;    for those native types.
;;
;;  - C-class types.  These are Scheme object whose structure is defined in C.
;;    They can be treated as ScmObj or can be casted to the specific C type;
;;    e.g. <symbol> can be casted to ScmSymbol*.
;;    Its unboxer is ScmObj -> C-TYPE*, and boxer is C-TYPE* -> ScmObj.
;;
;;  - Pass-through types.  These are Scheme object that are also handled
;;    as ScmObj in C-level.  Stub types only typecheck, and its boxer and
;;    unboxer are just identity.  It can be either purely-Scheme-defined
;;    objects, or an object that can take multiple representations
;;    (e.g. <integer> cna be a fixnum or ScmBignum*, so the stub generator
;;    passes through it, and the C routine handles the internals.)
;;
;;  - Maybe types.  (<?> TYPE).  In stub context, we only concern maybe type
;;    that can be unboxed into a C pointer type.  In addition to the objects
;;    of TYPE, it maps Scheme's #f to C's NULL and vice versa.
;;    For the convenicne, maybe type can be notated as TYPE? in the stub,
;;    e.g. <port>?

;; Each <cgen-type> has a corresponding Gauche type.  However, when
;; compiling an extension that introduces a new type, the compiling
;; Gauche may not know the type that's being defined.  So <cgen-types> are
;; catalogued with the 'name' of the corresponding Gauche type, rather than
;; the type object itself.

;; Stub type definition
(define-class <cgen-type> (<instance-pool-mixin>)
  ((name        :init-keyword :name)
   ;; ::<symbol> - name of the Gauche type.
   (scheme-type :init-keyword :scheme-type)
   ;; class or type.  can be #f if the type does not exist at runtime yet.
   (c-type      :init-keyword :c-type)
   ;; ::<string> - C type name this stub type represents
   (description :init-keyword :description)
   ;; ::<string> - used in the type error message

   ;; The following field should be private.  Use cgen-box-expr etc.
   (%c-predicate :init-keyword :c-predicate)
   ;; ::<string> - name of a C function (macro) to find out the given
   ;;              ScmObj has a valid type for this stub type.
   (%unboxer     :init-keyword :unboxer)
   ;; ::<string> - name of a C function (macro) that takes Scheme object
   ;;              and returns a C object.
   (%boxer       :init-keyword :boxer :init-value "SCM_OBJ_SAFE")
   ;; ::<string> - name of a C function (macro) that takes C object
   ;;              and returns a Scheme Object.
   (%maybe       :init-keyword :maybe       :init-value #f)
   ;; ::<type>? - base type, if this is 'maybe' qualified type.
   ))

;; Lookup/create a cgen type from Gauche type name
(define (cgen-type-from-name name)
  (or (find (lambda (type) (eq? (~ type'name) name))
            (instance-pool->list <cgen-type>))
      ;; when 'maybe' qualified type is used for the first time, we
      ;; create it from the base type.
      (and-let* ((m (#/\?$/ (symbol->string name)))
                 (basename (string->symbol (m 'before)))
                 (basetype (cgen-type-from-name basename)))
        (make <cgen-type> 
          :name name
          :scheme-type #f ;; cna be (<?> TYPE) after 0.9.11 release
          :c-type (~ basetype'c-type)
          :description #"~(~ basetype'description) or #f"
          :c-predicate (~ basetype'%c-predicate)
          :unboxer     (~ basetype'%unboxer)
          :boxer       (~ basetype'%boxer)
          :maybe       basetype))))

;; accessor
(define (cgen-type-maybe? type)
  (boolean (~ type'%maybe)))

;; These could be #f
(define (cgen-boxer-name type) (~ type'%boxer))
(define (cgen-unboxer-name type) (~ type'%unboxer))
(define (cgen-pred-name type) (~ type'%c-predicate))

;; Create a new cgen-type.
;; Many cgen-types follows a specific convention to name boxer/unboxer etc,
;; and make-cgen-type assumes the convention if they are not provided.

(define (make-cgen-type name scheme-type c-type :optional (desc #f) (c-pred #f)
                        (unbox #f) (box #f))
  (define (strip<> name) (string-trim-both name #[<>]))
  (define (default-cpred name)
    (if (#/-/ name)
      (string-append "SCM_"
                     (string-tr (strip<> name) "a-z-" "A-Z_")
                     "_P")
      #"SCM_~(string-upcase (strip<> name))P"))
  (define (default-unbox name)
    #"SCM_~(string-tr (strip<> name) \"a-z-\" \"A-Z_\")")
  (define (default-box name)
    #"SCM_MAKE_~(string-tr (strip<> name) \"a-z-\" \"A-Z_\")")
  (make <cgen-type>
    :name name :scheme-type scheme-type :c-type c-type
    :description (or desc (x->string name))
    :c-predicate (or c-pred (default-cpred (x->string name)))
    :unboxer     (or unbox (default-unbox (x->string name)))
    :boxer       (or box "SCM_OBJ_SAFE")))

;; TRANSIENT: Remove this after 0.9.11 release
;; <native-type> is introduced in 0.9.11, but we need 'em to compile 0.9.11
;; with 0.9.10, so we fake 'em.  See src/libtype.scm for the complete
;; definition.
(cond-expand
 [gauche-0.9.10
  (begin
    (define-class <native-type> ()
      ((name :init-keyword :name)
       (c-type-name :init-keyword :c-type-name)))
    (define-syntax define-native-type
      (syntax-rules ()
        [(_ name super c-type-name pred)
         (define name (make <native-type> 
                        :name 'name 
                        :c-type-name 'c-type-name))]))
    ;; Taken from src/libtype.scm
    (define-native-type <fixnum>  SCM_CLASS_INTEGER ScmSmallInt native_fixnumP)
    (define-native-type <short>   SCM_CLASS_INTEGER short native_shortP)
    (define-native-type <ushort>  SCM_CLASS_INTEGER u_short native_ushortP)
    (define-native-type <int>     SCM_CLASS_INTEGER int native_intP)
    (define-native-type <uint>    SCM_CLASS_INTEGER u_int native_uintP)
    (define-native-type <long>    SCM_CLASS_INTEGER long native_longP)
    (define-native-type <ulong>   SCM_CLASS_INTEGER u_long native_ulongP)
    (define-native-type <int8>    SCM_CLASS_INTEGER int8_t native_s8P)
    (define-native-type <uint8>   SCM_CLASS_INTEGER uint8_t native_u8P)
    (define-native-type <int16>   SCM_CLASS_INTEGER int16_t native_s16P)
    (define-native-type <uint16>  SCM_CLASS_INTEGER uint16_t native_u16P)
    (define-native-type <int32>   SCM_CLASS_INTEGER int32_t native_s32P)
    (define-native-type <uint32>  SCM_CLASS_INTEGER uint32_t native_u32P)
    (define-native-type <int64>   SCM_CLASS_INTEGER int64_t native_s64P)
    (define-native-type <uint64>  SCM_CLASS_INTEGER uint64_t native_u64P)
    (define-native-type <size_t>  SCM_CLASS_INTEGER size_t native_sizetP)
    (define-native-type <ssize_t> SCM_CLASS_INTEGER ssize_t native_ssizetP)
    (define-native-type <ptrdiff_t> SCM_CLASS_INTEGER ptrdiff_t native_ptrdifftP)
    (define-native-type <float>   SCM_CLASS_REAL float native_realP)
    (define-native-type <double>  SCM_CLASS_REAL double native_realP)
    (define-native-type <const-cstring> SCM_CLASS_STRING char* native_cstrP)
    (define-native-type <input-port>  SCM_CLASS_PORT ScmObj native_iportP)
    (define-native-type <output-port> SCM_CLASS_PORT ScmObj native_oportP)
    (define-native-type <closure> SCM_CLASS_PROCEDURE ScmObj native_closureP)
    (define-native-type <void>    SCM_CLASS_TOP ScmObj native_voidP))]
 [else])

;; Builtin types
(for-each
 (^[args]
   (apply (^[name c-type :optional (desc #f) (c-pred #f) (unbox #f) (box #f)]
            (make-cgen-type name #f c-type desc c-pred unbox box))
          args))
 '(;; Numeric types
   ;; NB: The boxer of <real> may be substituted when cgen-box-tail-expr
   ;; is used.
   (<fixnum>  "ScmSmallInt" "small integer" "SCM_INTP" "SCM_INT_VALUE" "SCM_MAKE_INT")
   (<integer> "ScmObj" "exact integer" "SCM_INTEGERP" "")
   (<real>    "double" "real number" "SCM_REALP" "Scm_GetDouble" "Scm_MakeFlonum")
   (<number>  "ScmObj" "number" "SCM_NUMBERP" "")
   (<int>     "int" "C integer" "SCM_INTEGERP" "Scm_GetInteger" "Scm_MakeInteger")
   (<long>    "long" "C long integer" "SCM_INTEGERP" "Scm_GetInteger" "Scm_MakeInteger")
   (<short>   "short" "C short integer" "SCM_INTP" "(short)SCM_INT_VALUE" "SCM_MAKE_INT")
   (<int8>    "int" "8bit signed integer" "SCM_INTEGERP" "Scm_GetInteger8" "Scm_MakeInteger")
   (<int16>   "int" "16bit signed integer" "SCM_INTEGERP" "Scm_GetInteger16" "Scm_MakeInteger")
   (<int32>   "int" "32bit signed integer" "SCM_INTEGERP" "Scm_GetInteger32" "Scm_MakeInteger")
   (<uint>    "u_int" "C integer" "SCM_UINTEGERP" "Scm_GetIntegerU" "Scm_MakeIntegerU")
   (<ulong>   "u_long" "C integer" "SCM_UINTEGERP" "Scm_GetIntegerU" "Scm_MakeIntegerU")
   (<ushort>  "u_short" "C short integer" "SCM_INTEGERP" "(unsigned short)Scm_GetIntegerU" "Scm_MakeIntegerU")
   (<uint8>   "u_int" "8bit unsigned integer" "SCM_UINTP" "Scm_GetIntegerU8" "Scm_MakeIntegerU")
   (<uint16>  "u_int" "16bit unsigned integer" "SCM_UINTP" "Scm_GetIntegerU16" "Scm_MakeIntegerU")
   (<uint32>  "u_int" "32bit unsigned integer" "SCM_UINTEGERP" "Scm_GetIntegerU32" "Scm_MakeIntegerU")
   (<float>   "float" "real number" "SCM_REALP" "(float)Scm_GetDouble" "Scm_MakeFlonum")
   (<double>  "double" "real number" "SCM_REALP" "Scm_GetDouble" "Scm_VMReturnFlonum")

   ;; Basic immediate types
   (<boolean> "int" "boolean" "SCM_BOOLP"   "SCM_BOOL_VALUE" "SCM_MAKE_BOOL")
   (<char>    "ScmChar" "character" "SCM_CHARP" "SCM_CHAR_VALUE" "SCM_MAKE_CHAR")
   (<void>    "void" "void" "" "" "SCM_VOID_RETURN_VALUE")
   (<top>     "ScmObj" "scheme object" "" "")
   ;; C string
   (<const-cstring> "const char *" "const C string"
                    "SCM_STRINGP" "SCM_STRING_CONST_CSTRING" "SCM_MAKE_STR_COPYING")

   ;; Aggregate types
   (<pair> "ScmPair*" "pair" "SCM_PAIRP" "SCM_PAIR" "SCM_OBJ")
   (<list> "ScmObj" "list" "SCM_LISTP" "")
   (<vector> "ScmVector*" "vector" "SCM_VECTORP" "SCM_VECTOR")
   (<uvector> "ScmUVector*" "uniform vector" "SCM_UVECTORP" "SCM_UVECTOR")
   (<s8vector> "ScmUVector*" "s8vector" "SCM_S8VECTORP" "SCM_S8VECTOR")
   (<u8vector> "ScmUVector*" "u8vector" "SCM_U8VECTORP" "SCM_U8VECTOR")
   (<s16vector> "ScmUVector*" "s16vector" "SCM_S16VECTORP" "SCM_S16VECTOR")
   (<u16vector> "ScmUVector*" "u16vector" "SCM_U16VECTORP" "SCM_U16VECTOR")
   (<s32vector> "ScmUVector*" "s32vector" "SCM_S32VECTORP" "SCM_S32VECTOR")
   (<u32vector> "ScmUVector*" "u32vector" "SCM_U32VECTORP" "SCM_U32VECTOR")
   (<s64vector> "ScmUVector*" "s64vector" "SCM_S64VECTORP" "SCM_S64VECTOR")
   (<u64vector> "ScmUVector*" "u64vector" "SCM_U64VECTORP" "SCM_U64VECTOR")
   (<f16vector> "ScmUVector*" "f16vector" "SCM_F16VECTORP" "SCM_F16VECTOR")
   (<f32vector> "ScmUVector*" "f32vector" "SCM_F32VECTORP" "SCM_F32VECTOR")
   (<f64vector> "ScmUVector*" "f64vector" "SCM_F64VECTORP" "SCM_F64VECTOR")
   (<c32vector> "ScmUVector*" "c32vector" "SCM_C32VECTORP" "SCM_C32VECTOR")
   (<c64vector> "ScmUVector*" "c64vector" "SCM_C64VECTORP" "SCM_C64VECTOR")
   (<c128vector> "ScmUVector*" "c128vector" "SCM_C128VECTORP" "SCM_C128VECTOR")
   (<bitvector> "ScmBitvector*" "bitvector" "SCM_BITVECTORP" "SCM_BITVECTOR")
   (<string> "ScmString*" "string" "SCM_STRINGP" "SCM_STRING")
   (<string-cursor> "ScmObj" "string cursor" "Scm_StringCursorP" "")
   (<symbol> "ScmSymbol*" "symbol" "SCM_SYMBOLP" "SCM_SYMBOL")
   (<keyword> "ScmKeyword*" "keyword" "SCM_KEYWORDP" "SCM_KEYWORD")
   (<identifier> "ScmIdentifier*" "identifier" "SCM_IDENTIFIERP" "SCM_IDENTIFIER")
   (<char-set> "ScmCharSet*" "char-set" "SCM_CHARSETP" "SCM_CHARSET")
   (<regexp> "ScmRegexp*" "regexp" "SCM_REGEXPP" "SCM_REGEXP")
   (<regmatch> "ScmRegMatch*" "regmatch" "SCM_REGMATCHP" "SCM_REGMATCH")
   (<port> "ScmPort*" "port" "SCM_PORTP" "SCM_PORT")
   (<input-port> "ScmPort*" "input port" "SCM_IPORTP" "SCM_PORT")
   (<output-port> "ScmPort*" "output port" "SCM_OPORTP" "SCM_PORT")
   (<procedure> "ScmProcedure*" "procedure" "SCM_PROCEDUREP" "SCM_PROCEDURE")
   (<closure> "ScmClosure*" "closure" "SCM_CLOSUREP" "SCM_CLOSURE")
   (<promise> "ScmPromise*" "promise" "SCM_PROMISEP" "SCM_PROMISE")
   (<comparator> "ScmComparator*" "comparator" "SCM_COMPARATORP" "SCM_COMPARATOR")
   (<hash-table> "ScmHashTable*" "hash table" "SCM_HASH_TABLE_P" "SCM_HASH_TABLE")
   (<tree-map> "ScmTreeMap*" "tree map" "SCM_TREE_MAP_P" "SCM_TREE_MAP")
   (<class> "ScmClass*" "class" "SCM_CLASSP" "SCM_CLASS")
   (<method> "ScmMethod*" "method" "SCM_METHODP" "SCM_METHOD")
   (<module> "ScmModule*" "module" "SCM_MODULEP" "SCM_MODULE")
   (<thread> "ScmVM*" "thread" "SCM_VMP" "SCM_VM")
   (<mutex> "ScmMutex*" "mutex" "SCM_MUTEXP" "SCM_MUTEX")
   (<condition-variable> "ScmConditionVariable*" "condition variable"
                         "SCM_CONDITION_VARIABLE_P" "SCM_CONDITION_VARIABLE")
   (<weak-vector> "ScmWeakVector*" "weak vector"
                  "SCM_WEAK_VECTOR_P" "SCM_WEAK_VECTOR")
   (<weak-hash-table> "ScmWeakHashTable*" "weak hash table"
                      "SCM_WEAK_HASH_TABLE_P" "SCM_WEAK_HASH_TABLE")
   (<compiled-code> "ScmCompiledCode*" "compiled code"
                    "SCM_COMPILED_CODE_P" "SCM_COMPILED_CODE")
   (<foreign-pointer> "ScmForeignPointer*" "foreign pointer"
                      "SCM_FOREIGN_POINTER_P" "SCM_FOREIGN_POINTER")
   (<box>  "ScmBox*" "box" "SCM_BOXP" "SCM_BOX")
   (<primitive-parameter> "ScmPrimitiveParameter*" "primitive parameter"
                          "SCM_PRIMITIVE_PARAMETER_P" "SCM_PRIMITIVE_PARAMETER")
   (<dlobj> "ScmDLObj*" "dlobj" "SCM_DLOBJP" "SCM_DLOBJ")
   (<dlptr> "ScmObj" "dlptr" "Scm_DLPtrP" "SCM_OBJ")
   ))

;;
;; Generating C expressions from type info
;;
;;   cgen-box-tail-expr can be used when the generated value will be
;;   immediately returned from SUBR.  The only difference from cgen-box-expr
;;   is the case for <real>, that can use register-allocated flonumbs
;;   in that case.
;;

(define (cgen-box-expr type c-expr)
  (let1 boxer (or (~ type'%boxer) "")
    (if (cgen-type-maybe? type)
      #"SCM_MAKE_MAYBE(~|boxer|, ~c-expr)"
      #"~|boxer|(~c-expr)")))

(define (cgen-box-tail-expr type c-expr)
  (let1 boxer (if (memq (~ type'name) '(<real> <float>))
                "Scm_VMReturnFlonum"
                (or (~ type'%boxer) ""))
    (if (cgen-type-maybe? type)
      #"SCM_MAKE_MAYBE(~|boxer|, ~c-expr)"
      #"~|boxer|(~c-expr)")))

(define (cgen-unbox-expr type c-expr)
  (let1 unboxer (or (~ type'%unboxer) "")
    (if (cgen-type-maybe? type)
      #"SCM_MAYBE(~|unboxer|, ~c-expr)"
      #"~|unboxer|(~c-expr)")))

(define (cgen-pred-expr type c-expr)
  (if-let1 pred (~ type'%c-predicate)
    (if (cgen-type-maybe? type)
      #"SCM_MAYBE_P(~|pred|, ~c-expr)"
      #"~|pred|(~c-expr)")
    "TRUE"))

(define (cgen-return-stmt expr)
  #"SCM_RETURN(~expr);")
