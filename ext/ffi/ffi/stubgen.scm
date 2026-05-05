;;;
;;; gauche.ffi.stubgen - Foreign function interface via stub generation
;;;
;;;   Copyright (c) 2026  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.ffi.stubgen
  (use gauche.ffi)
  (use gauche.cgen)
  (use gauche.cgen.type)
  (use gauche.cgen.dyncomp)
  (use gauche.native-type)
  (use gauche.sequence)
  (use gauche.config)
  (use gauche.package.compile)
  (use file.util)
  (use util.match)
  (export with-stubgen-ffi))
(select-module gauche.ffi.stubgen)

(define-syntax with-stubgen-ffi
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ dlo-var dlo-expr options cdef-specs forms)
        (let1 cdef-list-expr
            (quasirename r
              `(list ,@(map cdr cdef-specs)))
          (quasirename r
            `(begin
               ,@forms
               ;; We insert dummy binding so that expansion contanis
               ;; only definitions.
               (define ,dlo-var ,dlo-expr)
               (define _dummy
                 (compile-and-link-ffi-stub ,dlo-var
                                            ,cdef-list-expr
                                            (current-module)))
               )))]))))

(define (compile-and-link-ffi-stub dlobj cdef-instances mod)
  (let ([unit (generate-ffi-c-code-unit cdef-instances)]
        ;; Collect return types for pointer-returning functions, in order.
        ;; These are passed as extra args to ffisetup so it can populate
        ;; the per-function static type variables used in boxing.
        [pointer-ret-types
         (filter-map (^[cdef] (and (is-a? cdef <foreign-c-function>)
                                   (pointer-type? (~ cdef'return-type))
                                   (~ cdef'return-type)))
                     cdef-instances)]
        ;; Collect (fixed-arg-types . ret-type) pairs for variadic functions,
        ;; in order.  ffisetup uses these to populate the sub-stub type
        ;; variables that %generate-float-substub needs at call time.
        [variadic-type-infos
         (filter-map (^[cdef] (and (is-a? cdef <foreign-c-function>)
                                   (~ cdef'variadic?)
                                   (cons (~ cdef'arg-types)
                                         (~ cdef'return-type))))
                     cdef-instances)])
    (cgen-dynamic-load unit (sys-tmpdir))
    ((module-binding-ref mod 'ffisetup) dlobj pointer-ret-types variadic-type-infos
     mod)))

;; Maximum number of variadic arguments supported by the switch-case kludge.
;; There is no portable way to convert a list of arguments to va_list, so
;; we generate an explicit switch-case that dispatches on the number of
;; extra args and calls the variadic function with that many literal args.
(define-constant *max-variadic-args* 12)

;; Utility.  (We have this in gauche.cgen.stub as well; maybe we can
;; export it, but needs better name if we do so.)
(define (join/comma strs) (string-join strs ", "))

;;;
;;; On-the-fly sub-stub generation for float variadic arguments
;;;

;; When a variadic FFI function is called with one or more floating-point
;; extra arguments, the simple intptr_t switch-case cannot represent them
;; correctly.
;;
;; We handle this by generating, compiling, and loading a tiny "sub-stub"
;; DSO on the first call with each new (nvargs, float-mask) combination.
;; Subsequent calls with the same pattern look up the cached SUBR in a
;; per-function hash table and dispatch directly.
;;
;; Key encoding: key = (nvargs << *max-variadic-args*) | float-mask
;;   float-mask is a bitmask, bit i set means variadic arg i is a double

;; Called from the generated outer SUBR's float dispatch path (via
;; Scm_ApplyRec).  Generates a sub-stub for the specific type pattern
;; encoded in KEY, loads it, and returns the SUBR.
;;
;; fixed-arg-types : list of <native-type> for the fixed parameters
;; ret-type        : <native-type> for the return value
;; fn-ptr-scm      : intptr_t-as-Scheme-integer encoding the C function ptr
;; key             : integer (nvargs << *max-variadic-args*) | float-mask
;; c-name          : string, C-safe name of the outer function
(define (%generate-float-substub fixed-arg-types ret-type fn-ptr-scm key c-name)
  (let* ([nvargs     (ash key (- *max-variadic-args*))]
         [float-mask (logand key (- (ash 1 *max-variadic-args*) 1))]
         [unit       (build-substub-unit fixed-arg-types ret-type
                                         nvargs float-mask key c-name)]
         [setup-sym  (string->symbol (format "substub-setup-~a-~x" c-name key))])
    (cgen-dynamic-load unit (sys-tmpdir))
    (let1 setup (module-binding-ref (find-module 'gauche.ffi.stubgen) setup-sym)
      (if (pointer-type? ret-type)
        (setup fn-ptr-scm ret-type)
        (setup fn-ptr-scm)))))

;; Build a <cgen-unit> such that, when compiled and loaded, registers in
;; gauche.ffi.stubgen a setup procedure named substub-setup-CNAME-KEY.
;; That procedure takes the C function pointer (as Scheme integer) and
;; optionally the return type (for pointer-returning functions), and
;; returns the configured sub-stub SUBR.
;;
;; The sub-stub takes a flat ScmObj array:
;;   args[0..nfixed-1]             : fixed arguments (unboxed by fixed-arg-types)
;;   args[nfixed..nfixed+nvargs-1] : variadic arguments
;;                                   (double if float-mask bit set, else intptr_t)
;; data: the C function pointer (stored by the setup function)
(define (build-substub-unit fixed-arg-types ret-type nvargs
                            float-mask key c-name)
  (let* ([nfixed     (length fixed-arg-types)]
         [ptr-ret?   (pointer-type? ret-type)]
         [ret-typevar (and ptr-ret? "ss_rettype_")]
         ;; C type of the variadic function pointer, used for the cast in the call
         [ret-c      (%type->c-type ret-type)]
         [fixed-c-str (if (null? fixed-arg-types)
                        "void"
                        (join/comma (map %type->c-type fixed-arg-types)))]
         [fn-cast    (format "(~a (*)(~a, ...))" ret-c fixed-c-str)]
         [unit-name  (format "ffi_substub_~x" key)]
         ;; Names for the setup function registered in gauche.ffi.stubgen
         [setup-c-name   (format "substub_setup_~a_~x_" c-name key)]
         [setup-scm-name (format "substub-setup-~a-~x" c-name key)]
         ;; setup takes fn-ptr-scm; pointer-return variant also takes ret-type
         [setup-nargs    (if ptr-ret? 2 1)])
    (define unit
      (make <cgen-unit>
        :name unit-name
        :preamble (list "/* Generated FFI float variadic sub-stub.  Do not edit. */")
        :init-name unit-name))
    (parameterize ([cgen-current-unit unit])
      (cgen-decl "#include <gauche.h>")
      ;; Static rettype variable for pointer return boxing (populated in init)
      (when ptr-ret?
        (cgen-decl "static ScmObj ss_rettype_ = SCM_FALSE;"))
      ;; The sub-stub function
      (cgen-body "static ScmObj ss_fn_(ScmObj *args, int nargs SCM_UNUSED, void *data)")
      (cgen-body "{")
      ;; Unbox fixed arguments
      (for-each-with-index
       (lambda (i type)
         (cgen-body (format "    ~a arg~a = ~a;"
                            (%type->c-type type)
                            i
                            (%type->unbox-expr type (format "args[~a]" i)))))
       fixed-arg-types)
      ;; Unbox variadic arguments: double if float-mask bit set, else intptr_t
      (let loop ([i 0])
        (when (< i nvargs)
          (let* ([is-float (logbit? i float-mask)]
                 [c-type   (if is-float "double" "intptr_t")]
                 [unbox    (if is-float
                             (format "Scm_GetDouble(args[~a])" (+ nfixed i))
                             (%type->unbox-expr <intptr_t>
                                                (format "args[~a]" (+ nfixed i))))])
            (cgen-body (format "    ~a va~a = ~a;" c-type i unbox)))
          (loop (+ i 1))))
      ;; Build the call: cast data to the variadic fn ptr type and call it
      (let* ([all-args (join/comma
                        (append (map (^i (format "arg~a" i)) (iota nfixed))
                                (map (^i (format "va~a"  i)) (iota nvargs))))]
             [call-expr (format "(~a data)(~a)" fn-cast all-args)])
        (cgen-body (format "    return ~a;"
                           (%type->box-expr ret-type call-expr ret-typevar))))
      (cgen-body "}")
      ;; Setup function: called by %generate-float-substub after dynamic loading.
      ;; args[0] = fn-ptr-scm (intptr_t encoded as Scheme integer)
      ;; args[1] = ret-type Scheme object (only for pointer-returning functions)
      ;; Sets ss_rettype_ (if needed) and returns the configured sub-stub SUBR.
      (cgen-body (format "static ScmObj ~a(ScmObj *args, int nargs SCM_UNUSED, void *data SCM_UNUSED)"
                         setup-c-name))
      (cgen-body "{")
      (cgen-body "    void *fn_ = (void*)(intptr_t)Scm_IntegerToIntptr(args[0]);")
      (when ptr-ret?
        (cgen-body "    ss_rettype_ = args[1];"))
      (cgen-body (format "    return Scm_MakeSubr(ss_fn_, fn_, ~a, 0, SCM_FALSE);" (+ nfixed nvargs)))
      (cgen-body "}")
      ;; Init: register the setup function in gauche.ffi.stubgen so Scheme can
      ;; retrieve and call it after cgen-dynamic-load returns.
      (cgen-init
       "    {"
       "    ScmModule *m_ = SCM_FIND_MODULE(\"gauche.ffi.stubgen\", 0);"
       (format "    Scm_Define(m_, SCM_SYMBOL(SCM_INTERN(~a))," (cgen-safe-string setup-scm-name))
       (format "               Scm_MakeSubr(~a, NULL, ~a, 0, SCM_FALSE));" setup-c-name setup-nargs)
       "    }"))
    unit))

;;;
;;; Type helpers operating on <native-type> instances
;;;

;; All type information has already been resolved to <native-type> by
;; parse-define-c-function in gauche.ffi.  These helpers extract the
;; C code fragments needed to emit the stub.

;; True if a native type is compound (pointer, array, struct, union,
;; or function pointer).  Compound types are passed as native handles.
(define (compound-native-type? type)
  (or (is-a? type <c-pointer>)
      (is-a? type <c-array>)
      (is-a? type <c-struct>)
      (is-a? type <c-union>)
      (is-a? type <c-function>)))

;; True if the type is a C pointer type (<c-pointer>).  These require
;; special boxing/unboxing via Scm_NativeHandlePtr / Scm_MakeNativeHandleSimple.
(define (pointer-type? type)
  (is-a? type <c-pointer>))

;; Name of the static ScmObj variable that holds the return type for boxing
;; for a function that returns a <c-pointer>.
(define (ffi-rettype-varname cfn)
  (format "ffi_rettype_~a" (~ cfn'c-name)))

;; Name of the static ScmObj variable that holds the fixed arg types list
;; for a variadic function (used by %generate-float-substub at call time).
(define (ffi-substub-argtypes-varname cfn)
  (format "ffi_substub_argtypes_~a" (~ cfn'c-name)))

;; Name of the static ScmObj variable that holds the return type Scheme object
;; for a variadic function (passed to %generate-float-substub).
(define (ffi-substub-rettype-varname cfn)
  (format "ffi_substub_rettype_~a" (~ cfn'c-name)))

;; Name of the static ScmObj hash table used to cache per-pattern sub-stubs
;; for a variadic function.
(define (ffi-substub-table-varname cfn)
  (format "ffi_substub_table_~a" (~ cfn'c-name)))

;; Name of the static ScmInternalMutex variable to lock substub table.
(define (ffi-substub-mutex-varname cfn)
  (format "ffi_substub_mutex_~a" (~ cfn'c-name)))

;; Convert a <native-type> to a C type string.
(define (%type->c-type type)
  (if (eq? type <top>)
    "ScmObj"
    (etypecase type
      [<c-pointer>
       (string-append (%type->c-type (~ type'pointee-type)) "*")]
      [<c-array>
       (string-append (%type->c-type (~ type'element-type)) "*")]
      [<c-struct>
       (if (~ type'tag) (format "struct ~a" (~ type'tag)) "struct /*anon*/")]
      [<c-union>
       (if (~ type'tag) (format "union ~a" (~ type'tag)) "union /*anon*/")]
      [<c-function>
       "void*"]                         ; function pointers used as opaque void*
      [<native-type> (~ type'c-type-name)])))

;; Generate a C expression that unboxes a Scheme value to a C value.
;; arg-expr: C string expression yielding ScmObj.
;; For <c-pointer> types, extracts the raw pointer from the native handle.
(define (%type->unbox-expr type arg-expr)
  (cond
   [(eq? type <top>) arg-expr]
   [(pointer-type? type)
    ;; Extract void* from the native handle and cast to the C pointer type.
    ;; Scm_NativeHandlePtr is the public accessor for the pointer field.
    (format "(~a)Scm_NativeHandlePtr(SCM_NATIVE_HANDLE(~a))"
            (%type->c-type type) arg-expr)]
   [else
    (let1 unboxer (~ type'c-unboxer-name)
      (if (or (not unboxer) (equal? unboxer ""))
        (errorf "No unboxer for type: ~s" type)
        (format "~a(~a)" unboxer arg-expr)))]))

;; Generate a C expression that boxes a C value back to a Scheme object.
;; val-expr: C string expression yielding the C value.
;; For <c-pointer> return types, type-var is the name of a static ScmObj
;; variable that holds the native type object (populated during ffisetup).
(define (%type->box-expr type val-expr :optional (type-var #f))
  (cond
   [(eq? type <top>) (format "SCM_OBJ(~a)" val-expr)]
   [(pointer-type? type)
    (unless type-var
      (errorf "type-var required for boxing <c-pointer> return type: ~s" type))
    ;; Wrap the raw C pointer in a native handle using the stored type object.
    (format "Scm_MakeNativeHandleSimple((void*)(~a), ~a)" val-expr type-var)]
   [else
    (let1 boxer (~ type'c-boxer-name)
      (if (or (not boxer) (equal? boxer ""))
        (errorf "No boxer for type: ~s" type)
        (format "~a(~a)" boxer val-expr)))]))

;;;
;;; C code emitters
;;; Each emitter takes a <foreign-c-function> instance.
;;;

;; Emit function pointer variable (decl section).
;; e.g.: static int (*ffi_fn_mylib_add)(int, int) = NULL;
;; Variadic: static int (*ffi_fn_printf)(const char*, ...) = NULL;
(define (emit-fn-ptr-decl cfn)
  (let* ([c-name    (~ cfn'c-name)]
         [arg-types (~ cfn'arg-types)]
         [ret-type  (~ cfn'return-type)]
         [variadic? (~ cfn'variadic?)]
         [ret-c     (%type->c-type ret-type)]
         [arg-c-str (if (null? arg-types)
                      "void"
                      (join/comma (map %type->c-type arg-types)))])
    (cgen-decl (format "static ~a (*ffi_fn_~a)(~a~a) = NULL;"
                       ret-c c-name arg-c-str
                       (if variadic? ", ..." "")))))

;; Emit the SUBR function for one FFI function (body section).
;; Dispatches to the variadic variant when cfn has variadic? set.
(define (emit-subr-body cfn)
  (if (~ cfn'variadic?)
    (emit-variadic-subr-body cfn)
    (emit-fixed-subr-body cfn)))

;; Emit the SUBR for a fixed-arity (non-variadic) FFI function.
(define (emit-fixed-subr-body cfn)
  (let* ([scm-name  (~ cfn'scheme-name)]
         [c-name    (~ cfn'c-name)]
         [arg-types (~ cfn'arg-types)]
         [ret-type  (~ cfn'return-type)]
         ;; For <c-pointer> return types we use a per-function static ScmObj
         ;; variable (populated during ffisetup) to hold the type for boxing.
         [ret-typevar (and (pointer-type? ret-type) (ffi-rettype-varname cfn))])
    (cgen-body
     (format "static ScmObj ffi_subr_~a(ScmObj *args, int nargs SCM_UNUSED, void *data SCM_UNUSED)"
             c-name))
    (cgen-body "{")
    ;; Guard: function pointer must have been set up
    (cgen-body (format "    if (ffi_fn_~a == NULL)" c-name))
    (cgen-body (format "        Scm_Error(\"FFI: ~a is not initialized; call with-ffi first\");"
                       (symbol->string scm-name)))
    ;; Unbox each argument into a typed C local variable
    (for-each-with-index
     (lambda (i arg-type)
       (let* ([c-type (%type->c-type arg-type)]
              [unbox  (%type->unbox-expr arg-type (format "args[~a]" i))])
         (cgen-body (format "    ~a arg~a = ~a;" c-type i unbox))))
     arg-types)
    ;; Build the call expression and box the result
    (let1 call-expr
        (format "ffi_fn_~a(~a)"
                c-name
                (join/comma (map (^i (format "arg~a" i))
                                 (iota (length arg-types)))))
      (cgen-body (format "    return ~a;"
                         (%type->box-expr ret-type call-expr ret-typevar))))
    (cgen-body "}")))

;; Emit the SUBR for a variadic FFI function.
;;
;; The SUBR is registered with required=nfixed, optional=1 so the VM
;; collects any extra arguments into a rest list passed as args[nfixed].
;;
;; On each call we scan that rest list to build a float_mask (bit i set when
;; variadic arg i is a flonum).  Two dispatch paths follow:
;;
;;   float_mask == 0 (all-integer path):
;;     Unbox every extra arg as intptr_t and use the pre-generated
;;     switch-case, which covers 0..SCM_STUBGEN_MAX_VARIADIC_ARGS args.
;;
;;   float_mask != 0 (float path):
;;     Encode the pattern as key = (nvargs << MAX) | float_mask.
;;     Look it up in a per-function hash table.
;;     On a miss, call %generate-float-substub which compiles a small DSO
;;     tailored to that exact (int/double) type pattern and returns a SUBR.
;;     The SUBR is cached in the table for all future calls.
;;     Then flatten args[0..nfixed-1] + the rest list into a temporary array
;;     and call the sub-stub's C function directly (no Scheme overhead).
(define (emit-variadic-subr-body cfn)
  (let* ([scm-name    (~ cfn'scheme-name)]
         [c-name      (~ cfn'c-name)]
         [arg-types   (~ cfn'arg-types)]
         [ret-type    (~ cfn'return-type)]
         [nfixed      (length arg-types)]
         [ret-typevar (and (pointer-type? ret-type) (ffi-rettype-varname cfn))]
         [va-unbox    (%type->unbox-expr <intptr_t> "SCM_CAR(lst_)")]
         [table-var   (ffi-substub-table-varname cfn)]
         [mutex-var   (ffi-substub-mutex-varname cfn)]
         [argtypes-var (ffi-substub-argtypes-varname cfn)]
         [rettype-var  (ffi-substub-rettype-varname cfn)])
    (cgen-body
     (format "static ScmObj ffi_subr_~a(ScmObj *args, int nargs SCM_UNUSED, void *data SCM_UNUSED)"
             c-name))
    (cgen-body "{")
    ;; Guard: function pointer must have been set up
    (cgen-body #"    if (ffi_fn_~|c-name| == NULL)"
               #"        Scm_Error(\"FFI: ~(symbol->string scm-name) \
                                   is not initialized; call with-ffi first\");")
    ;; Unbox fixed arguments
    (for-each-with-index
     (^[i arg-type]
       (let ([c-type (%type->c-type arg-type)]
             [unbox  (%type->unbox-expr arg-type (format "args[~a]" i))])
         (cgen-body (format "    ~a arg~a = ~a;" c-type i unbox))))
     arg-types)
    ;; args[nfixed] is the rest list of variadic Scheme arguments
    (cgen-body
     #"    ScmObj vargs_ = args[~nfixed];"
     #"    int nvargs_ = Scm_Length(vargs_);"
     #"    if (nvargs_ > SCM_STUBGEN_MAX_VARIADIC_ARGS)"
     #"        Scm_Error(\"FFI: ~(symbol->string scm-name): too many \
                                   variadic arguments (max %d, got %d)\","
     #"                  SCM_STUBGEN_MAX_VARIADIC_ARGS, nvargs_);"
     ;; Compute float mask by scanning the rest list
     #"    int float_mask_ = 0;"
     #"    {"
     #"        ScmObj lst_ = vargs_;"
     #"        for (int i_ = 0; i_ < nvargs_; i_++) {"
     #"            if (SCM_FLONUMP(SCM_CAR(lst_))) float_mask_ |= (1 << i_);"
     #"            lst_ = SCM_CDR(lst_);"
     #"        }"
     "    }")
    ;; ---- All-integer path ----
    (cgen-body
     #"    if (float_mask_ == 0) {"
     #"        intptr_t va_[SCM_STUBGEN_MAX_VARIADIC_ARGS];"
     #"        {"
     #"            ScmObj lst_ = vargs_;"
     #"            for (int i_ = 0; i_ < nvargs_; i_++) {"
     #"                va_[i_] = ~|va-unbox|;"
     #"                lst_ = SCM_CDR(lst_);"
     #"            }"
     #"        }"
     #"        switch (nvargs_) {")
    (dotimes [n (+ *max-variadic-args* 1)]
      (let* ([fixed-args (map (^i (format "arg~a" i)) (iota nfixed))]
             [var-args   (map (^i (format "va_[~a]" i)) (iota n))]
             [all-args   (join/comma (append fixed-args var-args))]
             [call-expr  (format "ffi_fn_~a(~a)" c-name all-args)])
        (cgen-body (format "        case ~a: return ~a; break;"
                           n (%type->box-expr ret-type call-expr
                                              ret-typevar)))))
    (cgen-body
     #"        default: Scm_Error(\"BUG: nvargs_ check above failed\"); return SCM_UNDEFINED;"
     #"        }")
    ;; ---- Float path ----
    (cgen-body
     #"    } else {"
     ;; Encode type pattern as integer key
     #"        int key_ = (nvargs_ << ~|*max-variadic-args*|) | float_mask_;"
     ;; Flatten fixed args + rest list into flat_ array.
     ;; We need to copy everything from args[], as calling
     ;; Scm_ApplyRec can change VM stack.
     #"        int total_ = ~|nfixed| + nvargs_;"
     #"        ScmObj flat_[~|nfixed| + SCM_STUBGEN_MAX_VARIADIC_ARGS];"
     #"        for (int i_ = 0; i_ < ~|nfixed|; i_++) flat_[i_] = args[i_];"
     #"        {"
     #"            ScmObj lst_ = vargs_;"
     #"            for (int i_ = 0; i_ < nvargs_; i_++) { flat_[~|nfixed| + i_] = SCM_CAR(lst_); lst_ = SCM_CDR(lst_); }"
     #"        }"
     ;; Lazy-initialize the per-function sub-stub table
     #"        SCM_INTERNAL_MUTEX_LOCK(~mutex-var);"
     #"        if (SCM_FALSEP(~table-var))"
     #"            ~table-var = Scm_MakeHashTableSimple(SCM_HASH_EQ, 4);"
     ;; Look up the sub-stub for this pattern
     #"        ScmObj sub_ = Scm_HashTableRef(SCM_HASH_TABLE(~table-var), SCM_MAKE_INT(key_), SCM_FALSE);"
     #"        SCM_INTERNAL_MUTEX_UNLOCK(~mutex-var);"
     #"        if (SCM_FALSEP(sub_)) {"
     ;; Call %generate-float-substub to compile the sub-stub on the fly
     ;; (We call it outside the lock.  Running it simultaneously wastes
     ;;  cycle but safe.)
     #"            static ScmObj generate_float_substub_proc = SCM_UNDEFINED;"
     #"            ScmModule *m_ = SCM_FIND_MODULE(\"gauche.ffi.stubgen\", 0);"
     #"            SCM_BIND_PROC(generate_float_substub_proc,"
     #"                          \"%generate-float-substub\","
     #"                          SCM_FIND_MODULE(\"gauche.ffi.stubgen\", 0));"
     #"            ScmObj fnptr_ = Scm_IntptrToInteger((intptr_t)(void*)ffi_fn_~c-name);"
     #"            sub_ = Scm_ApplyRec(generate_float_substub_proc,"
     #"                                Scm_List(~argtypes-var,"
     #"                                         ~rettype-var,"
     #"                                         fnptr_,"
     #"                                         SCM_MAKE_INT(key_),"
     #"                                         SCM_MAKE_STR(\"~c-name\"), NULL));"
     #"            SCM_INTERNAL_MUTEX_LOCK(~mutex-var);"
     #"            Scm_HashTableSet(SCM_HASH_TABLE(~table-var), SCM_MAKE_INT(key_), sub_, 0);"
     #"            SCM_INTERNAL_MUTEX_UNLOCK(~mutex-var);"
     #"        }"
     #"        return SCM_SUBR_FUNC(sub_)(flat_, total_, SCM_SUBR_DATA(sub_));"
     #"    }")
    (cgen-body "}")))

;; Return a string with the setup code for one FFI function.
;; This is emitted inside ffisetup which takes a ScmDLObj*.
;; Also emits the Scm_Define + Scm_MakeSubrWithTags call that creates the
;; tagged subr and binds it in the target module.  The tags value is consumed
;; from ffisetup's local variable tags_ (a list, one entry per function in
;; order), and target_mod_ is the module passed as argv[4].
(define (setup-code-for-fn cfn)
  (let* ([scm-name  (~ cfn'scheme-name)]
         [c-name    (~ cfn'c-name)]
         [nfixed    (length (~ cfn'arg-types))]
         [tags      (cgen-literal (~ cfn'tag-info))]
         [optional  (if (~ cfn'variadic?) 1 0)])
    (string-append
     (format "    fptr = Scm_DLOGetEntryAddress(dlo, SCM_STRING(SCM_MAKE_STR(~a)), SCM_FALSE);"
             (cgen-safe-string #"_~c-name"))
     "\n"
     (format "    if (!SCM_NATIVE_HANDLE_P(fptr))\
            \n        Scm_Error(\"FFI setup: symbol ~a not found in library\");"
             c-name)
     "\n"
     (format "    *(void**)&ffi_fn_~a = Scm_NativeHandlePtr(SCM_NATIVE_HANDLE(fptr));"
             c-name)
     "\n"
     ;; Pop the per-function tags alist from tags_ and define the tagged subr.
     (format "      Scm_Define(target_mod_, SCM_SYMBOL(SCM_INTERN(~a)),\
             \n      Scm_MakeSubrWithTags(ffi_subr_~a, NULL, ~a, ~a, SCM_INTERN(~a), ~a));"
             (cgen-safe-string (symbol->string scm-name))
             c-name
             nfixed
             optional
             (cgen-safe-string (symbol->string scm-name))
             (cgen-cexpr tags)))))

;;;
;;; Generate C code from a list of <foreign-c-function> instances.
;;;

(define (generate-ffi-c-code-unit cdef-instances)
  (define unit-name (symbol->string (gensym "ffi")))
  (define cfn-instances
    (filter (cut is-a? <> <foreign-c-function>) cdef-instances))
  (define unit
    (make <cgen-unit>
      :name unit-name
      :preamble
      (list "/* Generated by gauche.ffi.stubgen.  Do not edit. */")
      :init-name unit-name))
  ;; Collect functions that return a <c-pointer> — they need a static ScmObj
  ;; variable to hold the type object for use in boxing the return value.
  (define pointer-return-cfns
    (filter (^[cfn] (pointer-type? (~ cfn'return-type))) cfn-instances))
  ;; Collect variadic functions — they need extra static ScmObj variables for
  ;; the sub-stub generation machinery.
  (define variadic-cfns
    (filter (^[cfn] (~ cfn'variadic?)) cfn-instances))
  (parameterize ([cgen-current-unit unit])
    (cgen-decl "#include <gauche.h>")
    (cgen-decl (format "#define SCM_STUBGEN_MAX_VARIADIC_ARGS ~a" *max-variadic-args*))

    (for-each emit-fn-ptr-decl cfn-instances)

    ;; Static type variables for functions that return a <c-pointer>.
    ;; Populated during ffisetup; used in the boxer call.
    (dolist [cfn pointer-return-cfns]
      (cgen-decl (format "static ScmObj ~a = SCM_FALSE;" (ffi-rettype-varname cfn))))

    ;; Static variables for variadic functions: sub-stub cache table,
    ;; fixed arg types list, and return type — all populated during ffisetup
    ;; and used by the float dispatch path at call time.
    (dolist [cfn variadic-cfns]
      (cgen-decl
       #"static ScmObj ~(ffi-substub-table-varname cfn) = SCM_FALSE;"
       #"static ScmObj ~(ffi-substub-argtypes-varname cfn) = SCM_FALSE;"
       #"static ScmObj ~(ffi-substub-rettype-varname cfn) = SCM_FALSE;"
       #"static ScmInternalMutex ~(ffi-substub-mutex-varname cfn);")
      (cgen-init
       #"   SCM_INTERNAL_MUTEX_INIT(~(ffi-substub-mutex-varname cfn));"))

    (cgen-body "")
    (for-each emit-subr-body cfn-instances)

    ;; FFI setup function (body section) — called by compile-and-link-ffi-stub.
    ;;   argv[0] = DLObj
    ;;   argv[1] = list of return-type ScmObjs for pointer-returning functions
    ;;   argv[2] = list of (fixed-arg-types . ret-type) for variadic functions
    ;;   argv[3] = target module (where to Scm_Define each function)
    (cgen-body ""
               "static ScmObj ffisetup(ScmObj *argv, int argc, void *data)"
               "{"
               "    SCM_ASSERT(argc == 4);"
               "    ScmObj dlobj = argv[0];"
               "    SCM_ASSERT(SCM_FALSEP(dlobj) || SCM_DLOBJP(dlobj));"
               "    ScmDLObj *dlo = SCM_FALSEP(dlobj)? NULL : SCM_DLOBJ(dlobj);"
               "    ScmObj fptr;"
               "    ScmModule *target_mod_ = SCM_MODULE(argv[3]);")
    (when (pair? pointer-return-cfns)
      (cgen-body "    ScmObj types = argv[1];"))
    (when (pair? variadic-cfns)
      (cgen-body "    ScmObj var_infos_ = argv[2];"))
    (dolist [cfn cfn-instances]
      (cgen-body (setup-code-for-fn cfn)))
    ;; Populate per-function return-type variables from the types list.
    (dolist [cfn pointer-return-cfns]
      (cgen-body (format "    ~a = SCM_CAR(types); types = SCM_CDR(types);"
                         (ffi-rettype-varname cfn))))
    ;; Populate per-variadic-function argtypes/rettype variables.
    ;; var_infos_ is a list of (fixed-arg-types . ret-type) pairs.
    (dolist [cfn variadic-cfns]
      (cgen-body
       #"    { ScmObj vi_ ="
       #"         SCM_CAR(var_infos_); var_infos_ = SCM_CDR(var_infos_);"
       #"      ~(ffi-substub-argtypes-varname cfn) = SCM_CAR(vi_);"
       #"      ~(ffi-substub-rettype-varname cfn) = SCM_CDR(vi_); }"))
    (cgen-body "    return SCM_UNDEFINED;"
               "}")

    (cgen-init "    Scm_Define(SCM_CURRENT_MODULE(),"
               "               SCM_SYMBOL(SCM_INTERN(\"ffisetup\")),"
               "               Scm_MakeSubr(ffisetup, NULL, 4, 0, SCM_FALSE));")
    )
  ;; Return unit
  unit)
