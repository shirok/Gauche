;;;
;;; gauche.cgen.type - Stub type management
;;;
;;;   Copyright (c) 2004-2022  Shiro Kawai  <shiro@acm.org>
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
;;    (e.g. <integer> can be a fixnum or ScmBignum*, so the stub generator
;;    passes through it, and the C routine handles the internals.)
;;
;;  - Maybe types.  (<?> TYPE).  In stub context, we only concern maybe type
;;    that can be unboxed into a C pointer type.  In addition to the objects
;;    of TYPE, it maps Scheme's #f to C's NULL and vice versa.
;;    For the convenicne, maybe type can be notated as TYPE? in the stub,
;;    e.g. <port>?
;;
;; In general, types defined in extensions can't be directly accesible from
;; other extensions at C level.  So we don't need to carry around stub types
;; across boundary of extensions.  Each extension is only accessible to
;; the stub types provided from Gauche core, or the stub types explicitly
;; defined within it.
;; The stub types provided from Gauche core are defined in this module.

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
   (cclass      :init-keyword :cclass :init-value #f)
   ;; If this cgen-type is for define-cclass'ed class, this slot holds
   ;; <cclass>.

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

(define-method write-object ((ct <cgen-type>) port)
  (format port "#<cten-type ~a>" (~ ct'name)))

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
          :scheme-type #f ;; can be (<?> TYPE) after 0.9.11 release
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

;; Stub types corresponding to native types.
(let ()
  (define (%native native-type pred unbox box)
    (make <cgen-type>
      :name (~ native-type'name)
      :scheme-type native-type
      :c-type (~ native-type'c-type-name)
      :description (~ native-type'c-type-name)
      :c-predicate pred
      :unboxer unbox
      :boxer box
      :maybe #f))

  (%native <fixnum>  "SCM_INTP" "SCM_INT_VALUE" "SCM_MAKE_INT")
  (%native <int>     "SCM_INTEGERP" "Scm_GetInteger" "Scm_MakeInteger")
  (%native <long>    "SCM_INTEGERP" "Scm_GetInteger" "Scm_MakeInteger")
  (%native <short>   "SCM_INTP" "(short)SCM_INT_VALUE" "SCM_MAKE_INT")
  (%native <int8>    "SCM_INTEGERP" "Scm_GetInteger8" "Scm_MakeInteger")
  (%native <int16>   "SCM_INTEGERP" "Scm_GetInteger16" "Scm_MakeInteger")
  (%native <int32>   "SCM_INTEGERP" "Scm_GetInteger32" "Scm_MakeInteger")
  (%native <int64>   "SCM_INTEGERP" "Scm_GetInteger64" "Scm_MakeInteger")
  (%native <uint>    "SCM_UINTEGERP" "Scm_GetIntegerU" "Scm_MakeIntegerU")
  (%native <ulong>   "SCM_UINTEGERP" "Scm_GetIntegerU" "Scm_MakeIntegerU")
  (%native <ushort>  "SCM_UINTEGERP" "(unsigned short)Scm_GetIntegerU" "Scm_MakeIntegerU")
  (%native <uint8>   "SCM_UINTP" "Scm_GetIntegerU8" "Scm_MakeIntegerU")
  (%native <uint16>  "SCM_UINTP" "Scm_GetIntegerU16" "Scm_MakeIntegerU")
  (%native <uint32>  "SCM_UINTEGERP" "Scm_GetIntegerU32" "Scm_MakeIntegerU")
  (%native <uint64>  "SCM_UINTEGERP" "Scm_GetIntegerU64" "Scm_MakeIntegerU")
  (%native <float>   "SCM_REALP" "(float)Scm_GetDouble" "Scm_MakeFlonum")
  (%native <double>  "SCM_REALP" "Scm_GetDouble" "Scm_VMReturnFlonum")

  (%native <size_t>  "Scm_IntegerFitsSizeP" "Scm_IntegerToSize" "Scm_SizeToInteger")
  (%native <ssize_t> "Scm_IntegerFitsSsizeP" "Scm_IntegerToSsize" "Scm_SsizeToInteger")
  (%native <ptrdiff_t> "Scm_IntegerFitsPtrdiffP"  "Scm_IntegerToPtrdiff" "Scm_PtrdiffToInteger")

  ;; TRANSIENT: The following needs to be in after 0.9.12 release
  ;;(%native <off_t>   "SCM_INTEGERP" "Scm_IntegerToOffset" "Scm_OffsetToInteger")
  ;;(%native <intptr_t>   "SCM_INTEGERP" "Scm_IntegerToIntptr" "Scm_IntptrToInteger")

  (%native <closure> "SCM_CLOSUREP" "SCM_CLOSURE" "SCM_OBJ")
  (%native <void>    "" "" "SCM_VOID_RETURN_VALUE")

  (%native <const-cstring> "SCM_STRINGP" "SCM_STRING_CONST_CSTRING" "SCM_MAKE_STR_COPYING")
  )

;; A few native types that has corresponding actual Scheme types.
;; NB: <real> should be a pass-through type, for coercing to double can lose
;; information.  To interface with C double or float, you can use <double>
;; or <float>.  We keep it so for the backward compatibility, but at some
;; point we'll change it to a pass-through type.
(make-cgen-type '<real> <real> "double" "real number"
                "SCM_REALP" "Scm_GetDouble" "Scm_MakeFlonum")
(make-cgen-type '<char> <char> "ScmChar" "character"
                "SCM_CHARP" "SCM_CHAR_VALUE" "SCM_MAKE_CHAR")
(make-cgen-type '<boolean> <boolean> "int" "boolean"
                "SCM_BOOLP" "SCM_BOOL_VALUE" "SCM_MAKE_BOOL")

;; Pass-through types
(let ()
  (define (%pass-through class desc pred)
    (make <cgen-type>
      :name (class-name class)
      :scheme-type class
      :c-type "ScmObj"
      :description desc
      :c-predicate pred
      :unboxer "" :maybe #f))

   (%pass-through <integer> "exact integer" "SCM_INTEGERP")
   (%pass-through <number>  "number" "SCM_NUMBERP")
   (%pass-through <top>     "scheme object" "")
   (%pass-through <list>    "list" "SCM_LISTP")

   (%pass-through <string-cursor> "string cursor" "Scm_StringCursorP")
   )

;; C-class types
(let ()
  (define (%cclass class c-type :optional (pred #f) (unbox #f))
    (make-cgen-type (class-name class) class c-type
                    (x->string (class-name class)) pred unbox))

   (%cclass <pair> "ScmPair*")
   (%cclass <vector> "ScmVector*")
   (%cclass <uvector> "ScmUVector*")
   (%cclass <s8vector> "ScmUVector*")
   (%cclass <u8vector> "ScmUVector*")
   (%cclass <s16vector> "ScmUVector*")
   (%cclass <u16vector> "ScmUVector*")
   (%cclass <s32vector> "ScmUVector*")
   (%cclass <u32vector> "ScmUVector*")
   (%cclass <s64vector> "ScmUVector*")
   (%cclass <u64vector> "ScmUVector*")
   (%cclass <f16vector> "ScmUVector*")
   (%cclass <f32vector> "ScmUVector*")
   (%cclass <f64vector> "ScmUVector*")
   (%cclass <c32vector> "ScmUVector*")
   (%cclass <c64vector> "ScmUVector*")
   (%cclass <c128vector> "ScmUVector*")
   (%cclass <bitvector> "ScmBitvector*")
   (%cclass <string> "ScmString*")
   (%cclass <symbol> "ScmSymbol*")
   (%cclass <keyword> "ScmKeyword*")
   (%cclass <identifier> "ScmIdentifier*")
   (%cclass <char-set> "ScmCharSet*")
   (%cclass <regexp> "ScmRegexp*")
   (%cclass <regmatch> "ScmRegMatch*")
   (%cclass <port> "ScmPort*")
   (%cclass <input-port> "ScmPort*" "SCM_IPORTP" "SCM_PORT")
   (%cclass <output-port> "ScmPort*" "SCM_OPORTP" "SCM_PORT")
   (%cclass <procedure> "ScmProcedure*")
   (%cclass <promise> "ScmPromise*")
   (%cclass <comparator> "ScmComparator*")
   (%cclass <hash-table> "ScmHashTable*")
   (%cclass <tree-map> "ScmTreeMap*")
   (%cclass <class> "ScmClass*")
   (%cclass <method> "ScmMethod*")
   (%cclass <module> "ScmModule*")
   (%cclass <thread> "ScmVM*" "SCM_VMP" "SCM_VM")
   (%cclass <weak-vector> "ScmWeakVector*")
   (%cclass <weak-hash-table> "ScmWeakHashTable*")
   (%cclass <compiled-code> "ScmCompiledCode*")
   (%cclass <foreign-pointer> "ScmForeignPointer*")
   (%cclass <box>  "ScmBox*")
   (%cclass <primitive-parameter> "ScmPrimitiveParameter*")
   (%cclass <dlobj> "ScmDLObj*")
   (%cclass <dlptr> "ScmObj" "Scm_DLPtrP" "SCM_OBJ")
   (%cclass <sys-group> "ScmSysGroup*")
   (%cclass <sys-passwd> "ScmSysPasswd*")
   (%cclass <sys-sigset> "ScmSysSigset*")
   (%cclass <sys-stat> "ScmSysStat*")
   (%cclass <time> "ScmTime*")
   ;(%cclass <sys-tm> "ScmSysTm*")
   (%cclass <sys-fdset> "ScmSysFdset*")

   ;; Exception - These classes are not available until we load gauche.threads,
   ;; but we need those stub types before compiling gauche.threads.
   ;; We hand-wire them, leaving scheme-type field #f.
   (make-cgen-type '<mutex> #f "ScmMutex*" "<mutex>")
   (make-cgen-type '<condition-variable> #f "ScmConditionVariable*"
                   "<condition-variable>")
   )

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
