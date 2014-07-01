;;;
;;; gauce.cgen.stub - stub forms parsing and code generation
;;;
;;;   Copyright (c) 2000-2014  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.cgen.stub
  (use srfi-1)
  (use srfi-13)
  (use srfi-42)
  (use util.match)
  (use text.tr)
  (use file.util)
  (use util.list)
  (use gauche.parameter)
  (use gauche.sequence)
  (use gauche.mop.instance-pool)
  (use gauche.cgen.type)
  (use gauche.cgen.unit)
  (use gauche.cgen.literal)
  (use gauche.cgen.cise)
  (use gauche.cgen.tmodule)
  (export <cgen-stub-unit> <cgen-stub-error>
          cgen-genstub
          cgen-stub-parser cgen-stub-parse-form)
  )
(select-module gauche.cgen.stub)

;; Summary of forms
;;
;;   define-type name c-type [desc c-predicate unboxer boxer]
;;
;;      Register a new type to be recognized.  This is rather a declaration
;;      than definition; no C code will be generated directly by this form.
;;
;;   define-cproc name (args ...) [rettype] [flag ...] [qual ...] body ...
;;
;;      Create a subr function.
;;
;;      <args> specifies arguments.
;;
;;       (<arg> ... [:rest <var>])
;;          Each <arg> is variable name or var::type, specifies required
;;          argument.  If :rest is given, list of excessive arguments are
;;          passed to <var>.
;;
;;       (<arg> ... :optional <spec> ... [:rest <var>])
;;          Optional arguments.  <spec> is <arg> or (<arg> <default>).
;;          If no default is given, <arg> receives SCM_UNBOUND---if <arg>
;;          isn't a type of ScmObj it will raise an error.
;;
;;       (<arg> ... :key <spec> ... [:allow-other-keys [:rest <var>]])
;;          Keyword arguments.  <spec> is <arg> or (<arg> <default>).
;;          If no default is given, <arg> receives SCM_UNBOUND---if <arg>
;;          isn't a type of ScmObj it will raise an error.
;;
;;       (<arg> ... :optarray (<var> <cnt> <max>) [:rest <var>])
;;          A special syntax to receive optional arguments as a C array.
;;          <var> is a C variable of type ScmObj*.  <cnt> is a C
;;          variable of type int, which receives the number of optional
;;          argument in the ScmObj array.  <max> specifies the maximum number
;;          of optional arguments that can be passed in the array form.
;;          If more than <max> args are given, a list of excessive arguments
;;          are passed to the rest <var> if it is specified, or
;;
;;      <Rettype> specifies the return type of SUBR.
;;
;;      <rettype>  = :: <typespec> | ::<typespec>
;;      <typespec> = <symbol>
;;                 | (<symbol> ...)     ; in case SUBR returns multiple values
;;          where <symbol> must name a valid stub type.
;;
;;      When omitted, SUBR is assumed to return <top>.
;;
;;      <Flag> is a keyword to modify some aspects of SUBR.  Supported
;;      flags are as follows:
;;
;;        :fast-flonum   - indicates that the SUBR accepts flonum arguments
;;               and it won't retain the reference to them.  The VM can pass
;;               flonums on VM registers to the SUBRs with this flag.
;;               (This improves floating-point number handling, but it's
;;               behavior is highly VM-specific; ordinary stub writers
;;               shouldn't need to care about this flag at all.)
;;        :constant      - indicates that this SUBR returns a constant value
;;               if all args are compile-time constants.  The compiler may
;;               replace the call to this proc with the value, if it determines
;;               all arguments are known at the compile time.  The resulting
;;               value should be serializable to the precompiled file.
;;               NB: Since this procedure may be called at compile time,
;;               a subr that may return a different value for batch/cross
;;               compilation shouldn't have this flag.
;;
;;      <Qual> (qualifier) is a list to adds auxiliary information to the
;;      SUBR.  Currently the following <quals> are officially supported.
;;
;;        (setter <setter-name>) : specfy setter.  <setter-name> should
;;             be a cproc name defined in the same stub file
;;        (setter (args ...) body ...) : specify setter anonymously.
;;
;;        (catch (<decl> <C-stmt> ...) ...) : when writing a stub
;;             for C++ function that may throw an exception, use this spec
;;             to ensure the exception will be caught and converted to
;;             Gauche error condition.
;;
;;        (inliner <insn-name>) : only used in Gauche core procedures
;;             that can be inlined into an VM insturuction.
;;
;;      <Body> is a cise expression.  Inside the expression, a cise macro
;;      (result expr ...) can be used to assign the value(s) to return
;;      from the cproc.   As a special case, if <body> is a single symbol,
;;      it names a C function to be called with the same argument (mod
;;      unboxing) as the cproc.
;;
;;      The following forms in <body> are DEPRECATED, retained for the
;;      backward compatibility.  The new code MUST NOT use them.
;;
;;        (code <C-code> ...)
;;             <C-code> is inserted at this position.  Useful to insert
;;             extra code before 'call' or 'expr' spec.
;;        (call [<rettype>] <C-function-name>)
;;             Calls C-function.  If <rettype> is omitted, C-function
;;             is assumed to return ScmObj.  Otherwise, a boxer of
;;             <rettype> is used.  As a special case, if <rettype> is
;;             <void>, the return value of C-function is ignored and
;;             the function returns #<undef>.
;;        (expr [<rettype>] <C-expr>) :
;;             <C-expr> must be a C expression of type <rettype>.  The
;;             value of C-expr is boxed and returned.  <void> isn't allowed
;;             as <rettype> (you can use 'body' directive).
;;        (body [<rettype>] <C-code> ...) :
;;             C-code becomes the body of the stub function.  In it,
;;             the code must assign to a variable SCM_RESULT.  The stub
;;             generator boxes the value and returns it.  (If <rettype>
;;             is <void>, though, C-code shouldn't assign SCM_RESULT.
;;             The generated function returns #<undef>.
;;        (body (<rettype> ...) <C-code> ...) :
;;             Procedure yields more than one value.  C variables
;;             SCM_RESULT0, SCM_RESULT1, ... are defined to receive the
;;             results.
;;        a string : becomes the body of C code.
;;
;;   define-cgeneric name c-name property-clause ...)
;;
;;      Defines generic function.   C-name specifies a C variable name
;;      that keeps the generic function structure.  One or more of
;;      the following clauses can appear in property-clause ...:
;;        (extern) : makes c-name visible from other file (i.e. do
;;             not define the structure as 'static').
;;        (fallback "fallback") : specifies the fallback function.
;;        (setter . setter-spec) : specifies the setter.
;;
;;   define-cmethod name (arg ...) body ...
;;
;;   define-cclass scheme-name [qualifiers] c-typename c-class-name cpa
;;      (slot-spec ...)
;;      property-clause ...
;;
;;   define-cptr scheme-class-name [qualifier] c-typename c-class-var
;;               c-pred c-boxer c-unboxer
;;               [(flags flag ...)]
;;               [(print print-proc)]
;;               [(cleanup cleanup-proc)]
;;
;;       qualifier :: :private
;;       flag :: :keep-identity | :mak-null
;;
;;   define-symbol scheme-name [c-name]
;;      Defines a Scheme symbol.  No Scheme binding is created.
;;      When c-name is given, the named C variable points to the
;;      created ScmSymbol.
;;
;;   define-variable scheme-name initializer
;;      Defines a Scheme variable.
;;
;;   define-constant scheme-name initializer
;;      Defines a Scheme constant.
;;
;;   define-enum name
;;      A define-constant Specialized for enum values.
;;
;;   define-enum-conditionally name
;;      Abbreviation of (if "defined(name)" (define-enum name))
;;
;;   define-cise-stmt name clause ...
;;   define-cise-expr name clause ...
;;      Cise macro definitions (see gauche.cgen.cise).
;;
;;   initcode <c-code>
;;      Insert <c-code> literally in the initialization function
;;
;;   begin <form> ...
;;      Treat each <form> as if they are toplevel stub forms.
;;
;;   <string>
;;      The string is inserted into the generated C body.
;;
;;   eval* <scheme-expr> ...
;;      Evaluates Scheme expressions inside a temporary anonymous
;;      module.  If the last expression returns a string or a list,
;;      it is processed again as a toplevel stub form.
;;      NB: This is highly experimental feature even the author is
;;      skeptical about.  Do not use this.

;;====================================================================
;; The do-it-all entry
;;

(define (cgen-genstub stubfile :key (predef-syms '()))
  (unless (file-is-readable? stubfile)
    (error <cgen-stub-error> "Couldn't read input file:" stubfile))
  (let1 prefix
      (cgen-safe-name-friendly (path-sans-extension (sys-basename stubfile)))
    (parameterize ([cgen-current-unit
                    (make <cgen-stub-unit>
                      :name (path-sans-extension (sys-basename stubfile))
                      :c-name-prefix (string-append prefix "_")
                      :preamble '("/* Generated by genstub.  Do not edit. */")
                      :init-prologue (format "void Scm_Init_~a(ScmModule *mod)\
                                              {\n" prefix))])
      (with-tmodule-recording
       <tmodule>
       (for-each cgen-define predef-syms)
       (cgen-include "<gauche.h>")
       (with-input-from-file stubfile
         (cut port-fold
              ;; We treat the initial raw strings specially---they will be
              ;; in decl part.  The flag decl-strings? tracks that.
              (^[form decl-strings?]
                (cond [(and decl-strings? (string? form)) (cgen-decl form) #t]
                      [else (cgen-stub-parse-form form) #f]))
              #t read))
       (cgen-emit-c (cgen-current-unit))))))

;; utilities
(define (f fmt . args) (apply format #t fmt args) (newline))
(define (p . args)     (apply print args))
(define (get-c-name prefix scheme-name)
  (cgen-safe-name-friendly (string-append prefix (x->string scheme-name))))

;; <cgen-stub-unit> is a specialized class to handle stub forms.
(define-class <cgen-stub-unit> (<cgen-unit>)
  ((c-name-prefix :init-keyword :c-name-prefix) ; prefix used for C identifiers
   (c++-exception-used? :init-value #f) ;#t if C++ exception has ever been used
   (temporary-module :init-form (make-module #f)) ; module used by eval*
   ))

(define-condition-type <cgen-stub-error> <error> #f)

;;===================================================================
;; Form parsers
;;

;; just a device to register handlers of syntax elements.
(define-class <form-parser> (<instance-pool-mixin>)
  ((name    :init-keyword :name)
   (args    :init-keyword :args)
   (handler :init-keyword :handler)))

(define-macro (define-form-parser name args . body)
  `(make <form-parser> :name ',name :args ',args :handler (^ ,args ,@body)))

(define-method invoke ((self <form-parser>) form)
  (define (badform)
    (errorf <cgen-stub-error> "stub: malformed ~a: ~s" (~ self'name) form))
  (apply (~ self'handler)
         ;; need to check if given form matches args
         (let loop ([llist (~ self'args)]
                    [form  (cdr form)])
           (cond [(null? llist) (if (null? form) '() (badform))]
                 [(pair? llist)
                  (if (null? form)
                    (badform)
                    (cons (car form) (loop (cdr llist) (cdr form))))]
                 [else form]))))

(define (cgen-stub-parser key)
  (cond [(find (^p (eq? key (~ p'name))) (instance-pool->list <form-parser>))
         => (^[parser] (cut invoke parser <>))]
        [else #f]))

(define (cgen-stub-parse-form form)
  (match form
    [(? string?) (cgen-body form)]
    [((= cgen-stub-parser (? values p)) . _) (p form)]
    [_ (error <cgen-stub-error> "invalid stub form:" form)]))

;; meta stuff
(define-form-parser define-cise-stmt args
  (eval `(define-cise-stmt ,@args) (current-module)))

(define-form-parser define-cise-expr args
  (eval `(define-cise-expr ,@args) (current-module)))

(define-form-parser define-cfn args
  (cgen-body (cise-render-to-string `(define-cfn ,@args) 'toplevel)))

;;===================================================================
;; Type handling
;;

(define (name->type name)
  (or (cgen-type-from-name name)
      (error <cgen-stub-error> "unknown stub-type: " name)))

;; define-type name c-type [desc c-predicate unboxer boxer]
;;
;;   Creates a new stub type for existing scheme type.
;;   This form itself doesn't generate any C code, but predicate, boxer
;;   and unboxer will be used by other stub forms to generate C code
;;   based on stub types.

(define-form-parser define-type args
  (unless (<= 2 (length args) 6)
    (error <cgen-stub-error> "malformed define-type: " `(define-type . ,args)))
  (apply make-cgen-type args))

;; default
(define *scm-type* (name->type '<top>))

;;===================================================================
;; Stub : base class of declarations
;;

(define-class <stub> (<cgen-node>)
  ((scheme-name :init-keyword :scheme-name :init-form #f)
      ;; Scheme name (symbol) of the object this stub represents.
      ;; Can be #f if the object is anonymous.
   (c-name      :init-keyword :c-name)
      ;; C expression of type ScmObj to refer to the object in C program.
   (tmodule     :init-form (current-tmodule))
      ;; The current transient module when this stub is parsed.
      ;; Can be #f.  See stub-tmodule-cname below
   ))

(define (get-stubs class)
  (filter (cut is-a? <> class) (cgen-unit-toplevel-nodes (cgen-current-unit))))

;; convenience routine.
(define (stub-tmodule-cname stub)
  (cond [(~ stub'tmodule) => (cut ~ <> 'cname)]
        [else (current-tmodule-cname)]))

;;===================================================================
;; Literals
;;

;; Most literals are handled by gauche.cgen.literal.  We need a special
;; handling to insert raw C code.

;; A special literal to include raw C expr in place of Scheme literal
(define-class <special-literal> () ()) ;; dummy class.  not really used.
(define-cgen-literal <raw-c-literal> <special-literal>
  ()
  (static (self) #t))

;; NB: (make <special-literal>) is just to fake the literal hash.
(define (make-literal obj . args)
  (match obj
    [('c _)
     (make <raw-c-literal> :value (make <special-literal>)
           :c-name (c-literal-expr obj))]
    [('current-input-port)
     (make <raw-c-literal>
       :value (make <special-literal>) :c-name "SCM_OBJ(SCM_CURIN)")]
    [('current-output-port)
     (make <raw-c-literal>
       :value (make <special-literal>) :c-name "SCM_OBJ(SCM_CUROUT)")]
    [('current-error-port)
     (make <raw-c-literal>
       :value (make <special-literal>) :c-name "SCM_OBJ(SCM_CURERR)")]
    [_ (cgen-literal obj)]))

;;===================================================================
;; Arg
;;

;; <arg> is used to keep procedure's argument information.
(define-class <arg> ()
  ((name     :init-keyword :name)
   ;; - <symbol>: the name as appears in the Scheme argument list.
   (c-name)
   ;; - <string>: C variable name for unboxed value
   (scm-name)
   ;; - <string>: C variable name to hold boxed ScmObj value
   (count    :init-keyword :count)
   ;; - <integer>: This arg is count-th in the procedure
   (type     :init-keyword :type)
   ;; - <cgen-type>: Stub type of this arg
   (default  :init-keyword :default :init-value #f)
   ;; - #f or <cgen-literal> : default value for optional/keyword arg
   ))

;; The opt-count is an integer, indicating the position of this optional
;; arg counted from the beginning of the optional arg
(define-class <required-arg> (<arg>) ())
(define-class <optional-arg> (<arg>) ((opt-count :init-keyword :opt-count)))
(define-class <keyword-arg>  (<arg>) (keyword))
(define-class <rest-arg>     (<arg>) ())
(define-class <optarray-arg> (<arg>) ((count-var :init-keyword :count-var)
                                      (max-count :init-keyword :max-count)))

(define-method write-object ((self <arg>) out)
  (format out "#<~a ~a>" (class-of self) (~ self'name)))

(define-method initialize ((self <arg>) initargs)
  (next-method)
  (set! (~ self'c-name)   (get-c-name "" (~ self'name)))
  (set! (~ self'scm-name) (string-append (~ self'c-name) "_scm")))

(define-method initialize ((self <keyword-arg>) initargs)
  (next-method)
  (set! (~ self'keyword) (cgen-literal (make-keyword (~ self'name)))))

;;===================================================================
;; Symbol and keyword definition
;;

;;-------------------------------------------------------------------
;; (define-symbol scheme-name c-name)

(define-form-parser define-symbol (name c-name . maybe-init)
  (check-arg symbol? name)
  (check-arg string? c-name)
  (let1 literal (make-literal name :c-name c-name)
    (cgen-decl #`"#define ,c-name (,(cgen-c-name literal))")
    (cgen-add! literal)))

;;-------------------------------------------------------------------
;; (define-variable scheme-name init &keyword c-name)
;; (define-constant scheme-name init &keyword c-name)
;; (define-enum name) - a special case of define-constant

(define (variable-parser-common const? name init opts)
  (let ([c-name  (get-keyword :c-name opts #f)]
        [symbol  (make-literal name)]
        [initval (make-literal init)])
    (when c-name
      (cgen-decl #`"#define ,c-name (,(cgen-cexpr symbol))"))
    (cgen-init (format "  Scm_MakeBinding(SCM_MODULE(~a), SCM_SYMBOL(~a), ~a, ~a);\n"
                       (current-tmodule-cname)
                       (cgen-cexpr symbol)
                       (cgen-cexpr initval)
                       (if const? "SCM_BINDING_CONST" "0")))
    (cgen-add! symbol)
    (cgen-add! initval)))

(define-form-parser define-variable (name init . opts)
  (check-arg symbol? name)
  (variable-parser-common #f name init opts))

(define-form-parser define-constant (name init . opts)
  (check-arg symbol? name)
  (variable-parser-common #t name init opts))

(define-form-parser define-enum (name)
  (check-arg symbol? name)
  (variable-parser-common #t name (list 'c #`"Scm_MakeInteger(,name)") '()))

(define-form-parser define-enum-conditionally (name)
  (check-arg symbol? name)
  (cgen-with-cpp-condition `(defined ,name)
    (variable-parser-common #t name (list 'c #`"Scm_MakeInteger(,name)") '())))

;;-------------------------------------------------------------------
;; (define-keyword scheme-name c-name)

(define-form-parser define-keyword (name c-name)
  (check-arg symbol? name)
  (check-arg string? c-name)
  (let1 literal (make-literal (make-keyword name) :c-name c-name)
    (cgen-decl #`"#define ,c-name (,(cgen-c-name literal))")
    (cgen-add! literal)))

;;===================================================================
;; Procedure
;;

;; Common stuff for cproc and cmethod

(define-class <setter-mixin> ()
  ((setter          :initform #f)
   ;; setter keeps the name of the setter, or a string of c-name of the
   ;; setter in case of anonymous setter.
   ))

(define-class <procstub> (<setter-mixin> <stub>)
  ((args              :initform '() :init-keyword :args)
   (keyword-args      :initform '() :init-keyword :keyword-args)
   (num-reqargs       :initform 0   :init-keyword :num-reqargs)
      ;; # of required arguments.
   (num-optargs       :initform 0   :init-keyword :num-optargs)
      ;; # of optional arguments.  including keyword-args.  rest-arg
      ;; is counted as 1 if the procedure takes it.
   (have-rest-arg?    :initform #f  :init-keyword :have-rest-arg?)
      ;; true if the procedure takes a rest-arg.
   (allow-other-keys? :initform '() :init-keyword :allow-other-keys?)
      ;; true if :allow-other-keys has veen given.
   (return-type       :initform #f :init-keyword :return-type)
      ;; return type given by ::<type>.
   (decls             :initform '())
   (stmts             :initform '())
      ;; reverse list of C stmt lines.
   (c++-handlers      :initform '())
      ;; ((<c++-exception-decl> <handler-stmt> ...) ...)
      ;; If not null, the entire procedure body is wrapped by 'try' and
      ;; an appropriate handlers are emitted.  Necessary to write a stub
      ;; for C++ functions that may throw an exception.
   (flags             :initform '() :init-keyword :flags)
      ;; list of keywords to modify code generation.  currently
      ;; :fast-flonum and :constant are supported.
   ))

(define (get-arg cproc arg) (find (^x (eq? arg (~ x'name))) (~ cproc'args)))
(define (push-stmt! cproc stmt) (push! (~ cproc'stmts) stmt))

(define-generic c-stub-name )

;;-----------------------------------------------------------------
;; (define-cproc scheme-name (argspec) body)
;;

(define-class <cproc> (<procstub>)
  ((inline-insn  :initform #f)
   (proc-name    :init-keyword :proc-name)  ; string literal
   ))

(define-form-parser define-cproc (scheme-name argspec . body)
  (define (check-fast-flonum cproc args)
    ;; if every arg has a type other than <top> or <number>,
    ;; we know that we can safely pass FLONUM_REGs, since the args
    ;; are immediately type-checked and/or unboxed.
    (let1 unsafe-types (list *scm-type* (name->type '<number>))
      (when (and (not (memq :fast-flonum (~ cproc'flags)))
                 (every (^a (or (is-a? a <rest-arg>)
                                (not (memq (~ a'type) unsafe-types))))
                        args))
        (push! (~ cproc'flags) :fast-flonum))))
  (define (extract-flags body)
    (receive (flags body) (span keyword? body)
      (unless (every (cut memq <> '(:fast-flonum :constant)) flags)
        (error "Invalid cproc flag in " flags))
      (values flags body)))

  (check-arg symbol? scheme-name)
  (check-arg list? argspec)
  (receive (args keyargs nreqs nopts rest? other-keys?)
      (process-cproc-args scheme-name argspec)
    (receive (body rettype) (extract-rettype body)
      (receive (flags body) (extract-flags body)
        (let1 cproc (make <cproc>
                      :scheme-name scheme-name
                      :c-name (get-c-name (~(cgen-current-unit)'c-name-prefix)
                                          scheme-name)
                      :proc-name (make-literal (x->string scheme-name))
                      :return-type rettype :flags flags
                      :args args
                      :keyword-args keyargs
                      :num-reqargs nreqs
                      :num-optargs nopts
                      :have-rest-arg? rest?
                      :allow-other-keys? other-keys?)
          (process-body cproc body)
          (check-fast-flonum cproc args)
          (cgen-add! cproc))))))

(define-method c-stub-name ((cproc <cproc>)) #`",(~ cproc'c-name)__STUB")

;; create arg object.  used in cproc and cmethod
(define (make-arg class argname count . rest)
  (define (grok-argname argname)
    (let1 namestr (symbol->string argname)
      (receive (realname typename) (string-scan namestr "::" 'both)
        (if realname
          (values (string->symbol realname)
                  (name->type (string->symbol typename)))
          (values argname *scm-type*)))))
  (receive (arg type) (grok-argname argname)
    (apply make class :name arg :type type :count count rest)))

;; returns a list of args, a list of keyword args, # of reqargs, # of
;; optargs, have-rest-arg?, and allow-other-keys?
;; each keyword arg and a rest arg counts 1 oprtarg for each.
(define (process-cproc-args name argspecs)
  (define (badarg arg)
    (errorf <cgen-stub-error> "bad argument in argspec: ~a in ~a" arg name))

  ;; support old &-notation.  will fade away.
  (define (xlate-old-lambda-keywords specs)
    (map (^s (cond [(assq s '((&optional . :optional) (&keyword . :key)
                              (&rest . :rest)
                              (&allow-other-keys . :allow-other-keys)))
                    => cdr]
                   [else s]))
         specs))

  (define (required specs args nreqs)
    (match specs
      [()                  (values (reverse args) '() nreqs 0 #f #f)]
      [(':optional . specs) (optional specs args nreqs 0)]
      [(':rest . specs)     (rest specs args '() nreqs 0 #f)]
      [(':key . specs)      (keyword specs args '() nreqs 0)]
      [(':allow-other-kyes . specs)
       (errorf <cgen-stub-error>
               "misplaced :allow-other-key parameter: ~s in ~a" argspecs name)]
      [(':optarray (var cnt max) . specs)
       (let1 args (cons (make-arg <optarray-arg> var nreqs
                                  :count-var cnt :max-count max)
                        args)
         (match specs
           [() (values (reverse args) '() nreqs max #f #f)]
           [(':rest . specs) (rest specs args '() nreqs max #f)]
           [_ (badarg specs)]))]
      [([? symbol? sym] . specs)
       (required specs
                 (cons (make-arg <required-arg> sym nreqs) args)
                 (+ nreqs 1))]
      [_ (badarg (car specs))]))

  (define (optional specs args nreqs nopts)
    (match specs
      [()   (values (reverse args) '() nreqs nopts #f #f)]
      [(':optional . specs)
       (error <cgen-stub-error> "extra :optional parameter in "name)]
      [(':key . specs)
       (error <cgen-stub-error>
              ":key and :optional can't be used together in "name)]
      [(':optarray . specs)
       (error <cgen-stub-error>
              ":optarray and :optional can't be used together in "name)]
      [(':rest . specs)     (rest specs args '() nreqs nopts #f)]
      [(':allow-other-keys . specs)
       (error <cgen-stub-error>
              "misplaced :allow-other-keys parameter in "name)]
      [([? symbol? sym] . specs)
       (optional specs
                 (cons (make-arg <optional-arg> sym (+ nreqs nopts)
                                 :opt-count nopts)
                       args)
                 nreqs
                 (+ nopts 1))]
      [(([? symbol? sym] default) . specs)
       (optional specs
                 (cons (make-arg <optional-arg> sym (+ nreqs nopts)
                                 :opt-count nopts
                                 :default (make-literal default))
                       args)
                 nreqs
                 (+ nopts 1))]
      [_ (badarg (car specs))]))

  (define (keyword specs args keyargs nreqs nopts)
    (match specs
      [() (values (reverse args) (reverse keyargs) nreqs nopts #f #f)]
      [(':allow-other-keys)
       (values (reverse args) (reverse keyargs) nreqs nopts #f #t)]
      [(':allow-other-keys ':rest . specs)
       (rest specs args keyargs nreqs nopts #t)]
      [(':allow-other-keys . specs)
       (error <cgen-stub-error> "misplaced :allow-other-keys parameter in "name)]
      [(':key . specs)
       (error <cgen-stub-error> "extra :key parameter in "name)]
      [(':optional . specs)
       (error <cgen-stub-error>
              ":key and :optional can't be used together in "name)]
      [(':optarray . specs)
       (error <cgen-stub-error>
              ":optarray and :optional can't be used together in "name)]
      [(':rest . specs)     (rest specs args keyargs nreqs nopts #f)]
      [([? symbol? sym] . specs)
       (keyword specs args
                (cons (make-arg <keyword-arg> sym (+ nreqs nopts))
                      keyargs)
                nreqs (+ nopts 1))]
      [(([? symbol? sym] default) . specs)
       (keyword specs args
                (cons (make-arg <keyword-arg> sym (+ nreqs nopts)
                                :default (make-literal default))
                      keyargs)
                nreqs (+ nopts 1))]
      [_ (badarg (car specs))]))

  (define (rest specs args keyargs nreqs nopts other-keys?)
    (match specs
      [() (values (reverse args) (reverse keyargs) nreqs nopts #t other-keys?)]
      [((? symbol? sym))
       (values (reverse (cons (make-arg <rest-arg> sym (+ nreqs nopts)) args))
               (reverse keyargs)
               nreqs (+ nopts 1) #t other-keys?)]
      [_ (badarg (car specs))]))

  (required (xlate-old-lambda-keywords argspecs) '() 0)
  )

;; returns two values, body stmts and return-type.  return-type can be #f
;; if unspecified.
(define (extract-rettype forms)

  (define (type-symbol? s)
    (and (keyword? s) (#/^:[^:]/ (keyword->string s))))
  (define (type-symbol-type s)
    (string->symbol (string-drop (keyword->string s) 1)))

  (match forms
    [(':: type . body) (values body type)]
    [([? type-symbol? ts] . body) (values body (type-symbol-type ts))]
    [_ (values forms #f)]))

(define-method process-body ((cproc <cproc>) body)
  (let loop ((body body))
    (match body
      [() #f]
      [([? string? s] . r) (push-stmt! cproc s) (loop r)]
      [(('inliner opcode) . r) (set! (~ cproc'inline-insn) opcode) (loop r)]
      [(('setter . spec) . r) (process-setter cproc spec) (loop r)]
      [(('call . spec) . r) (process-call-spec cproc spec) (loop r)]
      [(('body . spec) . r) (process-body-spec cproc spec) (loop r)]
      [(('expr . spec) . r) (process-expr-spec cproc spec) (loop r)]
      [(('catch . spec) . r) (process-catch-spec cproc spec) (loop r)]
      [(('code . stmts) . r) (dolist [s stmts] (push-stmt! cproc s)) (loop r)]
      [([? symbol? s]) ; 'call' convention
       ;; If the named C routine returns multiple values, we don't need to
       ;; call Scm_Values*() by ourselves.  We generate an appropriate cise
       ;; form (if return type is <void>, we just calls the named proc,
       ;; otherwise we insert result stmt) and pass it to process-body-inner.
       (let* ([args (map (cut ref <> 'name)
                         (append (~ cproc'args) (~ cproc'keyword-args)))]
              [form (if (eq? (~ cproc'return-type) '<void>)
                      `((,s ,@args))
                      `((result (,s ,@args))))]
              [rettype (if (pair? (~ cproc'return-type))
                         '<top>
                         (~ cproc'return-type))])
         (process-body-inner cproc rettype form))]
      ;; The following two patters are obsoleted forms.  We issue a warning
      ;; and process it as if call form is specified.
      [(('return [? symbol? s] [? string? p]) . r)
       (warn-obsoleted-return-form (car body))
       (loop `((call ,s ,p) ,@r))]
      [(('return [? string? p2]) . r)
       (warn-obsoleted-return-form (car body))
       (loop `((call <top> ,p2) ,@r))]
      [_ (process-body-inner cproc (~ cproc'return-type) body)])))

(define (warn-obsoleted-return-form form)
  (let1 sinfo (debug-source-info form)
    (warn "~s:~a: Stub file contains obsoleted cproc body form ~s. \
           It must be replaced by either 'call' form or CiSE expr."
          (if sinfo (car sinfo) "") (if sinfo (cadr sinfo) "") form)))

(define-method process-setter ((cproc <cproc>) decl)
  (cond
   [(symbol? (car decl)) (set! (~ cproc'setter) (car decl))]
   [(< (length decl) 2)
    (error <cgen-stub-error> "bad form of anonymous setter:" `(setter ,decl))]
   [else
    (receive (args keyargs nreqs nopts rest? other-keys?)
        (process-cproc-args (ref cproc'proc-name) (car decl))
      (receive (body rettype) (extract-rettype (cdr decl))
        (let1 setter (make <cproc>
                       :scheme-name `(setter ,(~ cproc'scheme-name))
                       :c-name #`",(~ cproc'c-name)_SETTER"
                       :proc-name (make-literal (x->string `(setter ,(~ cproc'scheme-name))))
                       :args args :return-type rettype
                       :keyword-args keyargs
                       :num-reqargs nreqs
                       :num-optargs nopts
                       :have-rest-arg? rest?
                       :allow-other-keys? other-keys?)
          (set! (~ cproc'setter) #`",(~ setter'c-name)__STUB")
          (process-body setter body)
          (cgen-add! setter))))]))

(define-method process-call-spec ((cproc <procstub>) form)
  (define (err) (error <cgen-stub-error> "malformed 'call' spec:" form))
  (define check-expr (any-pred string? symbol?))
  (define (args)
    (string-join (map (cut ref <> 'c-name)
                      (append (~ cproc'args) (~ cproc'keyword-args)))
                 ", "))
  (define (typed-result rettype c-func-name)
    (push-stmt! cproc "{")
    (push-stmt! cproc #`",(~ rettype'c-type) SCM_RESULT;")
    (push-stmt! cproc #`"SCM_RESULT = ,c-func-name(,(args));")
    (push-stmt! cproc (cgen-return-stmt (cgen-box-expr rettype "SCM_RESULT")))
    (push-stmt! cproc "}"))
  (match form
    [([? check-expr expr])
     (let1 rt (~ cproc'return-type)
       (if rt
         (process-call-spec cproc `(,rt ,expr)) ; for transition
         (typed-result *scm-type* expr)))]
    [('<void> [? check-expr expr])
     (push-stmt! cproc #`",expr(,(args));")
     (push-stmt! cproc "SCM_RETURN(SCM_UNDEFINED);")]
    [(typename [? check-expr expr])
     (unless (and (symbol? typename) (check-expr expr)) (err))
     (typed-result (name->type typename) expr)]
    [else (err)]))

(define-method process-body-spec ((cproc <procstub>) form)
  (define (err) (error <cgen-stub-error> "malformed 'body' spec:" form))
  (match form
    [((? symbol? rettype) . stmts)
     (process-body-inner cproc rettype stmts)]
    [((? list? rettypes) . stmts)
     (unless (every symbol? rettypes) (err))
     (process-body-inner cproc rettypes stmts)]
    [stmts (process-body-inner cproc #f stmts)]))

(define-method process-expr-spec ((cproc <procstub>) form)
  (define (typed-result rettype expr)
    (let1 expr (if (string? expr)
                 expr
                 (call-with-output-string (cut cise-render expr <> 'expr)))
      (push-stmt! cproc "{")
      (push-stmt! cproc #`",(~ rettype'c-type) SCM_RESULT;")
      (push-stmt! cproc #`" SCM_RESULT = (,expr);")
      (push-stmt! cproc (cgen-return-stmt (cgen-box-expr rettype "SCM_RESULT")))
      (push-stmt! cproc "}")))
  (match form
    [('<void> . stmts)
     (error <cgen-stub-error> "<void> type isn't allowed in 'expr' directive:" form)]
    [([? symbol? rettype] expr)
     (typed-result (name->type rettype) expr)]
    [(expr)
     (let1 rt (~ cproc'return-type)
       (if rt
         (process-expr-spec cproc `(,rt ,expr)) ; for transition
         (typed-result *scm-type* expr)))]
    [else (error <cgen-stub-error> "malformed 'expr' spec:" form)]))

(define-method process-catch-spec ((cproc <procstub>) form)
  (match form
    [((decl . handler-stmts) ...)
     ;; push default handlers
     (push! (~ cproc'c++-handlers)
            (list "..."
                  (format "Scm_Error(\"C++ exception is thrown in ~s\");"
                          (~ cproc'scheme-name))))
     (push! (~ cproc'c++-handlers)
            (list "std::exception& e"
                  (format "Scm_Error(\"~a: %s\", e.what());"
                          (~ cproc'scheme-name))))
     (for-each (^(d s) (push! (~ cproc'c++-handlers) (cons d s)))
               decl handler-stmts)
     ;; if this is the first time, make sure we include <stdexcept>.
     (unless (~ (cgen-current-unit)'c++-exception-used?)
       (cgen-decl "#include <stdexcept>")
       (set! (~ (cgen-current-unit)'c++-exception-used?) #t))
     ]
    [else (error <cgen-stub-error> "malformed 'catch' spec:" form)]))

(define-method process-body-inner ((cproc <procstub>) rettype body)
  (define (expand-stmt stmt)
    (push-stmt! cproc (if (string? stmt)
                        stmt
                        (cise-render-to-string stmt))))
  (define (typed-result rettype stmts)
    (push-stmt! cproc "{")
    (push-stmt! cproc #`",(~ rettype'c-type) SCM_RESULT;")
    (for-each expand-stmt stmts)
    (push-stmt! cproc (cgen-return-stmt (cgen-box-expr rettype "SCM_RESULT")))
    (push-stmt! cproc "}"))
  (define (typed-results rettypes stmts)
    (let1 nrets (length rettypes)
      (for-each-with-index
       (^[i rettype] (push-stmt! cproc #`",(~ rettype'c-type) SCM_RESULT,i;"))
       rettypes)
      (push-stmt! cproc "{")
      (for-each expand-stmt stmts)
      (push-stmt! cproc "}")
      (let1 results
          (string-join
           (map-with-index (^[i rettype]
                             (cgen-box-expr rettype #`"SCM_RESULT,i"))
                           rettypes)
           ",")
        (push-stmt! cproc
                    (case nrets
                      [(0) (cgen-return-stmt "Scm_Values(SCM_NIL)")]
                      [(1) (cgen-return-stmt results)]
                      [(2) (cgen-return-stmt #`"Scm_Values2(,results)")]
                      [(3) (cgen-return-stmt #`"Scm_Values3(,results)")]
                      [(4) (cgen-return-stmt #`"Scm_Values4(,results)")]
                      [(5) (cgen-return-stmt #`"Scm_Values5(,results)")]
                      [else (cgen-return-stmt
                             #`"Scm_Values(Scm_List(,results,, NULL))")]))
        )))
  (match rettype
    [#f             ; the default case; we assume it is <top>.
     (typed-result *scm-type* body)]
    ['<void>        ; no results
     (for-each expand-stmt body)
     (push-stmt! cproc "SCM_RETURN(SCM_UNDEFINED);")]
    [[? symbol? t]  ; single result
     (typed-result (name->type t) body)]
    [[? list? ts]   ; multiple values
     (typed-results (map name->type ts) body)]
    [_ (error <cgen-stub-error> "invalid cproc return type:" rettype)]))

;;;
;;; Emitting code
;;;

(define-method cgen-emit-body ((cproc <cproc>))
  (p "static ScmObj "(~ cproc'c-name)"(ScmObj *SCM_FP, int SCM_ARGCNT, void *data_)")
  (p "{")
  ;; argument decl
  (for-each emit-arg-decl (~ cproc'args))
  (for-each emit-arg-decl (~ cproc'keyword-args))
  (if (null? (~ cproc'keyword-args))
    (p "  ScmObj SCM_SUBRARGS["(+ (length (~ cproc'args))
                                  (~ cproc'num-optargs))"];")
    (begin
      (p "  ScmObj SCM_SUBRARGS["(+ (length (~ cproc'args))
                                    (~ cproc'num-optargs)
                                    -1)"];")
      (p "  ScmObj SCM_OPTARGS = SCM_ARGREF(SCM_ARGCNT-1);")))
  (p "  int SCM_i;")
  (p "  SCM_ENTER_SUBR(\""(~ cproc'scheme-name)"\");")
  ;; argument count check (for optargs)
  (when (and (> (~ cproc'num-optargs) 0)
             (null? (~ cproc'keyword-args))
             (not (~ cproc'have-rest-arg?)))
    (p "  if (SCM_ARGCNT >= "(+ (~ cproc'num-reqargs) (~ cproc'num-optargs) 1))
    (p "      && !SCM_NULLP(SCM_ARGREF(SCM_ARGCNT-1)))")
    (p "    Scm_Error(\"too many arguments: up to "
       (+ (~ cproc'num-reqargs) (~ cproc'num-optargs))
       " is expected, %d given.\", "
       "SCM_ARGCNT + Scm_Length(SCM_ARGREF(SCM_ARGCNT-1)) - 1);"))
  ;; argument assertions & unbox op.
  (let1 k (+ (length (~ cproc'args)) (~ cproc'num-optargs)
             (if (null? (~ cproc'keyword-args)) 0 -1))
    (p "  for (SCM_i=0; SCM_i<"k"; SCM_i++) {")
    (p "    SCM_SUBRARGS[SCM_i] = SCM_ARGREF(SCM_i);")
    (p "  }"))
  (for-each emit-arg-unbox (~ cproc'args))
  (unless (null? (~ cproc'keyword-args))
    (emit-keyword-args-unbox cproc))
  ;; body
  (unless (null? (~ cproc'c++-handlers))
    (p "try {"))
  (p "  {")
  (for-each p (reverse (~ cproc'stmts)))
  (p "  }")
  (unless (null? (~ cproc'c++-handlers))
    (p "}")
    (dolist [h (~ cproc'c++-handlers)]
      (f "catch (~a) {" (car h))
      (for-each p (cdr h))
      (p "}")))
  ;; closing the function
  (p "}")
  (p)
  ;; emit stub record
  (let1 flags (~ cproc'flags)
    (format #t "static SCM_DEFINE_SUBR~a(" (if (null? flags) "" "X"))
    (format #t "~a, ~a, ~a,"
            (c-stub-name cproc)                       ; cvar
            (~ cproc'num-reqargs)                     ; req
            (cond [(zero? (~ cproc'num-optargs)) 0]
                  [(not (null? (~ cproc'keyword-args))) 1]
                  [(~ cproc'have-rest-arg?) (~ cproc'num-optargs)]
                  [else (+ (~ cproc'num-optargs) 1)])); opt
    (unless (null? flags)                             ; cst
      (if (memq :constant flags) (display "1, ") (display "0, ")))
    (format #t "~a," (cgen-c-name (~ cproc'proc-name))); inf
    (unless (null? flags)                              ; flags
      (if (memq :fast-flonum flags)
        (display "SCM_SUBR_IMMEDIATE_ARG, ")
        (display "0, ")))
    (format #t "~a, ~a, NULL);\n"
            (~ cproc'c-name)                          ; func
            (cond [(~ cproc'inline-insn)
                   => (^i (format "SCM_MAKE_INT(SCM_VM_~a)"
                                  (string-tr (x->string i) "-" "_")))]
                  [else "NULL"])))                    ; inliner
  (p))

(define-method cgen-emit-init ((cproc <cproc>))
  (when (symbol? (~ cproc'scheme-name))
    (f "  Scm_MakeBinding(SCM_MODULE(~a), SCM_SYMBOL(SCM_INTERN(~s)), SCM_OBJ(&~a), ~a);"
       (stub-tmodule-cname cproc)
       (symbol->string (~ cproc'scheme-name))
       (c-stub-name cproc)
       (if (or (~ cproc'inline-insn) (memq :constant (~ cproc'flags)))
         "SCM_BINDING_INLINABLE" "0")))
  (next-method)
  )

(define-method cgen-emit-init ((cproc <setter-mixin>))
  (define (emit setter-name)
    (f "  Scm_SetterSet(SCM_PROCEDURE(&~a), SCM_PROCEDURE(&~a), TRUE);"
       (c-stub-name cproc) setter-name))
  (match (~ cproc'setter)
    [(? string? x) (emit x)]
    [(? symbol? x)
     (or (and-let* ([setter (find (^z (eq? (~ z'scheme-name) x))
                                  (get-stubs <stub>))])
           (emit (c-stub-name setter)))
         (errorf <cgen-stub-error>
                 "unknown setter name '~a' is used in the definition of '~a'"
                 x (~ cproc'scheme-name)))]
    [_ #f]))

(define-method emit-arg-decl ((arg <arg>))
  (p "  ScmObj "(~ arg'scm-name)";")
  (p "  "(~ arg'type'c-type)" "(~ arg'c-name)";"))

(define-method emit-arg-decl ((arg <keyword-arg>))
  (p "  ScmObj "(~ arg'scm-name)" = "(get-arg-default arg)";")
  (p "  "(~ arg'type'c-type)" "(~ arg'c-name)";"))

(define-method emit-arg-decl ((arg <optarray-arg>))
  (p "  ScmObj* "(~ arg'c-name)";")
  (p "  int "(get-c-name "" (~ arg'count-var))";"))

(define (emit-arg-unbox-rec arg)
  (let1 pred (~ arg'type'c-predicate)
    (when (and pred (not (string-null? pred)))
      (f "  if (!~a) Scm_Error(\"~a required, but got %S\", ~a);"
         (cgen-pred-expr (~ arg'type) (~ arg'scm-name))
         (~ arg'type'description) (~ arg'scm-name)))
    (if (~ arg'type'unboxer)
      (p "  "(~ arg'c-name)" = "(cgen-unbox-expr (~ arg'type) (~ arg'scm-name))";")
      (p "  "(~ arg'c-name)" = "(~ arg'scm-name)";"))))

(define-method emit-arg-unbox ((arg <required-arg>))
  (p "  "(~ arg'scm-name)" = SCM_SUBRARGS["(~ arg'count)"];")
  (emit-arg-unbox-rec arg))

(define-method emit-arg-unbox ((arg <optional-arg>))
  (p "  if (SCM_ARGCNT > "(~ arg'count)"+1) {")
  (p "    "(~ arg'scm-name)" = SCM_SUBRARGS["(~ arg'count)"];")
  (p "  } else {")
  (p "    "(~ arg'scm-name)" = "(get-arg-default arg)";")
  (p "  }")
  (emit-arg-unbox-rec arg))

(define-method emit-arg-unbox ((arg <rest-arg>))
  (p "  "(~ arg'scm-name)" = SCM_SUBRARGS[SCM_ARGCNT-1];")
  (emit-arg-unbox-rec arg))

(define-method emit-arg-unbox ((arg <optarray-arg>))
  (p "  "(~ arg'c-name)" = &SCM_SUBRARGS["(~ arg'count)"];")
  (p "  "(get-c-name "" (~ arg'count-var))" = SCM_ARGCNT-1-"(~ arg'count)";"))

(define (get-arg-default arg)
  (cond [(~ arg'default) => cgen-cexpr]
        [else "SCM_UNBOUND"]))

(define (emit-keyword-args-unbox cproc)
  (let ([args (~ cproc'keyword-args)]
        [other-keys? (~ cproc'allow-other-keys?)])
    (p "  if (Scm_Length(SCM_OPTARGS) % 2)")
    (p "    Scm_Error(\"keyword list not even: %S\", SCM_OPTARGS);")
    (p "  while (!SCM_NULLP(SCM_OPTARGS)) {")
    (pair-for-each
     (^[args] (let ([arg (car args)]
                    [tail? (null? (cdr args))])
                (f "    if (SCM_EQ(SCM_CAR(SCM_OPTARGS), ~a)) {"
                   (cgen-c-name (~ arg'keyword)))
                (f "      ~a = SCM_CADR(SCM_OPTARGS);" (~ arg'scm-name))
                (if tail?
                  (p "    }")
                  (p "    } else "))))
     args)
    (unless other-keys?
      (p "    else Scm_Warn(\"unknown keyword %S\", SCM_CAR(SCM_OPTARGS));"))
    (p "    SCM_OPTARGS = SCM_CDDR(SCM_OPTARGS);")
    (p "  }")
    (for-each emit-arg-unbox-rec args)))

;;-----------------------------------------------------------------
;; Generic function
;;

;; (define-cgeneric scheme-name c-name
;;    [(extern)]
;;    [(fallback "fallback")]
;;    [(setter setter-desc)])

(define-class <cgeneric> (<setter-mixin> <stub>)
  ((extern?  :initform #f :init-keyword :extern? :accessor extern?)
   (fallback :initform "NULL" :init-keyword :fallback)
   ))

(define-method c-stub-name ((self <cgeneric>))
  (~ self'c-name))

(define-method cgen-emit-body ((self <cgeneric>))
  (unless (extern? self) (p "static "))
  (p "SCM_DEFINE_GENERIC("(~ self'c-name)", "(~ self'fallback)", NULL);")
  (p))

(define-method cgen-emit-init ((self <cgeneric>))
  (f "  Scm_InitBuiltinGeneric(&~a, ~s, SCM_MODULE(~a));"
     (~ self'c-name) (symbol->string (~ self'scheme-name))
     (stub-tmodule-cname self))
  (next-method))

(define-form-parser define-cgeneric (scheme-name c-name . body)
  (check-arg symbol? scheme-name)
  (check-arg string? c-name)
  (let1 gf (make <cgeneric> :scheme-name scheme-name :c-name c-name)
    (for-each (match-lambda
                [('extern) (set! [~ gf'extern?] #t)]
                [('fallback (? string? fallback))
                 (set! (~ gf'fallback) (cadr form))]
                [('setter . spec) (process-setter gf spec)]
                [form (error <cgen-stub-error> "bad gf form:" form)])
              body)
    (cgen-add! gf)))

(define-method process-setter ((gf <cgeneric>) decl)
  (if (symbol? (car decl))
    (set! (~ gf'setter) (car decl))
    (error <cgen-stub-error> "bad form of anonymous setter:" `(setter ,@decl))))

(define (get-c-generic-name name)
  (cond [(find (^x (eq? (~ x'scheme-name) name)) (get-stubs <cgeneric>))
         => (cut ref <> 'c-name)]
        [else #f]))

;;-----------------------------------------------------------------
;; Methods
;;

;; (define-cmethod scheme-name (argspec ...)
;;    [ (c-generic-name "CGenericName") ]
;;    body ...)

(define-class <cmethod> (<procstub>)
  ((specializers :init-keyword :specializers)
   (c-generic    :initform #f)
   ))

(define-form-parser define-cmethod (scheme-name argspec . body)
  (check-arg symbol? scheme-name)
  (check-arg list? argspec)
  (receive (args specializers numargs have-optarg?)
      (parse-specialized-args argspec)
    (receive (body rettype) (extract-rettype body)
      (let1 method (make <cmethod>
                     :scheme-name scheme-name
                     :c-name (get-c-name (~(cgen-current-unit)'c-name-prefix)
                                         (gensym (x->string scheme-name)))
                     :return-type rettype
                     :specializers specializers
                     :num-reqargs numargs
                     :args args
                     :have-rest-arg? have-optarg?
                     )
        (let loop ([body body])
          (match body
            [() #f]
            [([? string? stmt] . r) (push-stmt! method stmt) (loop r)]
            [(('c-generic-name gen-name) . r)
             (unless (string? gen-name)
               (error <cgen-stub-error>
                      "c-generic-name requires a string:" gen-name))
             (set! (~ method'c-generic) gen-name)
             (loop r)]
            [(('body . spec) . r) (process-body-spec method spec) (loop r)]
            [(('call . spec) . r) (process-call-spec method spec) (loop r)]
            [(('expr . spec) . r) (process-expr-spec method spec) (loop r)]
            [(('code . stmts) . r)
             (for-each (cut push-stmt! method <>) stmts)
             (loop r)]
            [([? symbol? s]) ; 'call' convention
             ;; If the named C routine returns multiple values, we don't need to
             ;; call Scm_Values*() by ourselves.   Hence we intercept
             (let* ([args (map (cut ref <> 'name) (~ method'args))]
                    [form (if (eq? (~ method'return-type) '<void>)
                            `((,s ,@args))
                            `((result (,s ,@args))))]
                    [rettype (if (pair? (~ method'return-type))
                               '<top>
                               (~ method'return-type))])
               (process-body-inner method rettype form))]
            [_ (process-body-inner method (~ method'return-type) body)]))
        (unless (~ method'c-generic)
          (set! (~ method'c-generic)
                (or (get-c-generic-name scheme-name)
                    (error <cgen-stub-error>
                           "method can't find C name of the generic function:"
                           scheme-name))))
        (cgen-add! method)
        ))))

(define-method cgen-emit-body ((method <cmethod>))
  (f "static ScmObj ~a(ScmNextMethod *nm_, ScmObj *SCM_FP, int SCM_ARGCNT, void *d_)"
     (~ method'c-name))
  (p "{")
  (for-each emit-arg-decl (~ method'args))
  (if (~ method'have-rest-arg?)
    (begin (p "  ScmObj SCM_SUBRARGS["(+ (length (~ method'args)) -1)"];")
           (p "  ScmObj SCM_OPTARGS = SCM_ARGREF(SCM_ARGCNT-1);"))
    (p "  ScmObj SCM_SUBRARGS["(length (~ method'args))"];"))
  (p "  int SCM_i;")
  (let1 k (+ (length (~ method'args))
             (if (~ method'have-rest-arg?) -1 0))
    (p "  for (SCM_i=0; SCM_i<"k"; SCM_i++) {")
    (p "    SCM_SUBRARGS[SCM_i] = SCM_ARGREF(SCM_i);")
    (p "  }"))
  (for-each emit-arg-unbox (~ method'args))
  ;; body
  (p "  {")
  (for-each p (reverse (~ method'stmts)))
  (p "  }")
  (p "}")
  (p "")
  (p "static ScmClass *"(~ method'c-name)"__SPEC[] = { ")
  (for-each (^[spec] (p "SCM_CLASS_STATIC_PTR("spec"), "))
            (reverse (~ method'specializers)))
  (p "};")
  (f "static SCM_DEFINE_METHOD(~a__STUB, &~a, ~a, ~a, ~a__SPEC, ~:*~a, NULL);"
     (~ method'c-name) (~ method'c-generic)
     (~ method'num-reqargs) (if (~ method'have-rest-arg?) "1" "0")
     (~ method'c-name))
  (p "")
  )

(define-method cgen-emit-init ((method <cmethod>))
  (f "  Scm_InitBuiltinMethod(&~a__STUB);" (~ method'c-name)))

;; returns four values: args, specializers, numreqargs, have-optarg?
(define (parse-specialized-args arglist)
  (define (badlist) (error <cgen-stub-error> "malformed arglist:" arglist))
  (let loop ([arglist arglist]
             [args    '()]
             [specs   '()])
    (cond [(null? arglist)
           (values (reverse args) specs (length args) #f)]
          [(symbol? arglist)
           (values (cons (make-arg <rest-arg> arglist (length args))
                         args)
                   (cons "Scm_ListClass" specs)
                   (length args) #t)]
          [(not (pair? arglist)) (badlist)]
          [(symbol? (car arglist))
           (loop (cdr arglist)
                 (cons (make-arg <required-arg> (car arglist) (length args))
                       args)
                 (cons "Scm_TopClass" specs))]
          [(not (and (pair? (car arglist))
                     (= (length (car arglist)) 2)
                     (symbol? (caar arglist))
                     (string? (cadar arglist))))
           (badlist)]
          [else
           (loop (cdr arglist)
                 (cons (make-arg <required-arg> (caar arglist) (length args))
                       args)
                 (cons (cadar arglist) specs))]
          )))

;;===================================================================
;; Class
;;
;;  - Generates C stub for static class definition, slot accessors and
;;    initialization.   Corresponding C struct has to be defined elsewhere.
;;
;;  - <cclass> should be a <cgen-type> as well, but currently not.
;;    If a corresponding type is not defined at the time define-cclass is
;;    parsed, the type is created with the default parameters.

;; (define-cclass scheme-name [qualifiers] c-type-name c-class-name cpa
;;   (<slot-spec> ...)
;;   [(allocator <proc-spec>)]
;;   [(printer   <proc-spec>)]
;;   [(direct-supers <string> ...)]
;;   )
;;
;; <slot-spec> := slot-name
;;             |  (slot-name
;;                  [:type <cgen-type>]
;;                  [:c-name <c-name>]
;;                  [:c-spec <c-spec>]
;;                  [:getter <proc-spec>]
;;                  [:setter <proc-spec>])
;;
;; <proc-spec> := <c-code> | (c <c-name>) | #f | #t
;;
;; <cpa> := (<string> ...)
;;
;; qualifiers := :base | :built-in | :private

;; 'qualifiers' modifies the generated code slightly.  :base and :built-in
;; is exclusive.  :base generates a base class definition (inheritable
;; from Scheme code), while :built-in generates a built-in class definition
;; (not inheritable from Scheme code).  If neither one appears, built-in
;; class is generated.  :private is an optional qualifier, and when it is
;; given, the class declaration and standard macro definitions are
;; also generated (which needs to be in the separate header file if you want
;; the C-level structure to be used from other C code.  If the extension is
;; small enough to be contained in one C file, this option is convenient.)
;;
;; 'cpa' lists ancestor classes in precedence order.  They need to
;; be C identifiers of Scheme class (Scm_*Class), for the time being.
;; Scm_TopClass is added at the end automatically.
;;
;; 'direct-supers' specifies a list of direct superclass, if the
;; defined class does multiple inheritance.  When omitted, the first
;; element of 'cpa' is used as the only direct superclass.
;;
;; 'allocator' and 'printer' clause specifies custom allocator and/or
;; printer procedure.  You can either directly write C function body
;; in <c-code>, or specify a C function name by '(c <c-name>)' form.

(define-class <cclass> (<stub>)
  ((cpa        :init-keyword :cpa       :init-value '())
   (c-type     :init-keyword :c-type)
   (qualifiers :init-keyword :qualifiers)
   (allocator  :init-keyword :allocator :init-value #f)
   (printer    :init-keyword :printer   :init-value #f)
   (slot-spec  :init-keyword :slot-spec :init-value '())
   (direct-supers :init-keyword :direct-supers :init-value '())
   ))

(define-method initialize ((self <cclass>) initargs)
  (next-method)
  (unless (cgen-type-from-name (~ self'scheme-name))
    (cgen-stub-parse-form
     `(define-type ,(~ self'scheme-name) ,(~ self'c-type))))
  )

(define-class <cslot> ()
  ((cclass      :init-keyword :cclass)
   (scheme-name :init-keyword :scheme-name)
   (c-name      :init-keyword :c-name)
   (c-spec      :init-keyword :c-spec)
   (type        :init-keyword :type   :init-value '<top>)
   (getter      :init-keyword :getter :init-value #t)
   (setter      :init-keyword :setter :init-value #t)
   ))

(define-form-parser define-cclass (scm-name . args)
  (check-arg symbol? scm-name)
  (receive (quals rest) (span keyword? args)
    (cond
     [(lset-difference eqv? quals '(:built-in :base :private)) pair?
      => (cut error <cgen-stub-error> "unknown define-cclass qualifier(s)" <>)])
    (match rest
      [(c-type c-name cpa slot-spec . more)
       (check-arg string? c-name)
       (check-arg list? cpa)
       (check-arg list? slot-spec)
       (let* ([allocator (cond [(assq 'allocator more) => cadr] [else #f])]
              [printer   (cond [(assq 'printer more) => cadr] [else #f])]
              [dsupers   (cond [(assq 'direct-supers more) => cdr] [else '()])]
              [cclass (make <cclass>
                        :scheme-name scm-name :c-type c-type :c-name c-name
                        :qualifiers quals
                        :cpa cpa :direct-supers dsupers
                        :allocator allocator :printer printer)])
         (set! (~ cclass'slot-spec) (process-cclass-slots cclass slot-spec))
         (cgen-add! cclass))])))

(define-method c-printer-name ((self <cclass>))
  (let1 printer (~ self'printer)
    (cond [(c-literal-expr printer)]
          [(not printer) "NULL"]
          [else #`",(~ self'c-name)_PRINT"])))

(define-method c-allocator-name ((self <cclass>))
  (let1 allocator (~ self'allocator)
    (cond [(c-literal-expr allocator)]
          [(not allocator) "NULL"]
          [else #`",(~ self'c-name)_ALLOCATE"])))

(define-method c-slot-spec-name ((self <cclass>))
  (if (null? (~ self'slot-spec))
    "NULL"
    #`",(~ self'c-name)__SLOTS"))

(define (cclass-emit-standard-decls self)
  (let1 type (cgen-type-from-name (~ self'scheme-name))
    (p "SCM_CLASS_DECL("(~ self'c-name)");")
    (p "#define "(~ type'unboxer)"(obj) (("(~ self'c-type)")obj)")
    (p "#define "(~ type'c-predicate)"(obj) SCM_XTYPEP(obj, (&"(~ self'c-name)"))")
    ))

(define-method cgen-emit-body ((self <cclass>))
  (when (memv :private (~ self'qualifiers))
    (cclass-emit-standard-decls self))
  (unless ((any-pred not c-literal?) (~ self'allocator))
    (p "static ScmObj "(c-allocator-name self)"(ScmClass *klass, ScmObj initargs)")
    (p "{")
    (p (c-code (~ self'allocator)))
    (p "}")
    (p ""))
  (unless ((any-pred not c-literal?) (~ self'printer))
    (p "static void "(c-printer-name self)"(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)")
    (p "{")
    (p (c-code (~ self'printer)))
    (p "}")
    (p ""))
  (emit-cpa self)
  (if (memv :base (~ self'qualifiers))
    (let1 c-type (string-trim-right (~ self'c-type))
      (unless (string-suffix? "*" c-type)
        (errorf <cgen-stub-error> "can't use C-type ~s as a base class; C-type must be a pointer type" c-type))
      (let1 c-instance-type (string-drop-right c-type 1)
        (p "SCM_DEFINE_BASE_CLASS("(~ self'c-name)", "c-instance-type", "(c-printer-name self)", NULL, NULL, "(c-allocator-name self)", "(cpa-name self)");")))
    (p "SCM_DEFINE_BUILTIN_CLASS("(~ self'c-name)", "(c-printer-name self)", NULL, NULL, "(c-allocator-name self)", "(cpa-name self)");"))
  (p "")
  (when (pair? (~ self'slot-spec))
    (for-each emit-getter-n-setter (~ self'slot-spec))
    (p "static ScmClassStaticSlotSpec "(c-slot-spec-name self)"[] = {")
    (for-each emit-spec-definition (~ self'slot-spec))
    (p "  SCM_CLASS_SLOT_SPEC_END()")
    (p "};")
    (p))
  )

(define-method cgen-emit-init ((self <cclass>))
  (p "  Scm_InitBuiltinClass(&"(~ self'c-name)", \""(~ self'scheme-name)"\", "(c-slot-spec-name self)", TRUE, SCM_MODULE("(stub-tmodule-cname self)"));")
  ;; adjust direct-supers if necessary
  (let1 ds (~ self'direct-supers)
    (when (not (null? ds))
      (p "  "(~ self'c-name)".directSupers = Scm_List(")
      (for-each (^s (p "SCM_OBJ(&"s"), ")) ds)
      (p " NULL);")
      (p))))

;; cpa ----------
;;  For now, cpa should be a list of C class names, or c literal

(define-method cpa-name ((self <cclass>))
  (cond [(null? (~ self'cpa)) "SCM_CLASS_DEFAULT_CPL"]
        [(c-literal-expr (~ self'cpa))]
        [else #`",(~ self'c-name)_CPL"]))

(define-method emit-cpa ((self <cclass>))
  (let1 cpa (~ self'cpa)
    (unless (or (null? cpa) (c-literal? cpa))
      (p "static ScmClass *"(~ self'c-name)"_CPL[] = {")
      (for-each (^[class] (p "  SCM_CLASS_STATIC_PTR("class"),")) cpa)
      (unless (equal? (car (last-pair cpa)) "Scm_TopClass")
        (p "  SCM_CLASS_STATIC_PTR(Scm_TopClass),"))
      (p "  NULL")
      (p "};"))))

;; slot ---------

(define (process-cclass-slots cclass slot-spec)
  (map (^[spec]
         (unless (list? spec) (error <cgen-stub-error> "bad slot spec" spec))
         (let* ([name (car spec)]
                [type   (get-keyword :type (cdr spec) '<top>)]
                [c-name (get-keyword :c-name (cdr spec) (get-c-name "" name))]
                [c-spec (get-keyword :c-spec (cdr spec) #f)]
                [getter (get-keyword :getter (cdr spec) #t)]
                [setter (get-keyword :setter (cdr spec) #t)])
           (make <cslot>
             :cclass cclass :scheme-name name :c-name c-name
             :c-spec c-spec :type (name->type type)
             :getter getter :setter setter)))
       slot-spec))

(define-method slot-getter-name ((slot <cslot>))
  (let1 getter (~ slot'getter)
    (or
     (c-literal-expr getter)
     #`",(~ slot'cclass'c-name)_,(get-c-name \"\" (~ slot'scheme-name))_GET")))

(define-method slot-setter-name ((slot <cslot>))
  (let1 setter (~ slot'setter)
    (cond [(c-literal-expr setter)]
          [(not setter) "NULL"]
          [else #`",(~ slot'cclass'c-name)_,(get-c-name \"\" (~ slot'scheme-name))_SET"])))

(define-method emit-getter-n-setter ((slot <cslot>))
  (unless (c-literal? (~ slot'getter)) (emit-getter slot))
  (when (and (~ slot'setter) (not (c-literal? (~ slot'setter))))
    (emit-setter slot)))

(define-method emit-getter ((slot <cslot>))
  (let* ([type  (~ slot'type)]
         [class (~ slot'cclass)]
         [class-type (name->type (~ class'scheme-name))])
    (p "static ScmObj "(slot-getter-name slot)"(ScmObj OBJARG)")
    (p "{")
    (p "  "(~ class-type'c-type)" obj = "(cgen-unbox-expr class-type "OBJARG")";")
    (cond [(string? (~ slot'getter)) (p (~ slot'getter))]
          [(string? (~ slot'c-spec))
           (f "  return ~a;" (cgen-box-expr type (~ slot'c-spec)))]
          [else
           (f "  return ~a;" (cgen-box-expr type #`"obj->,(~ slot'c-name)"))])
    (p "}")
    (p "")))

(define-method emit-setter ((slot <cslot>))
  (let* ([type (~ slot'type)]
         [class (~ slot'cclass)]
         [class-type (name->type (~ class'scheme-name))])
    (p "static void "(slot-setter-name slot)"(ScmObj OBJARG, ScmObj value)")
    (p "{")
    (p "  "(~ class-type'c-type)" obj = "(cgen-unbox-expr class-type "OBJARG")";")
    (if (string? (~ slot'setter))
      (p (~ slot'setter))
      (begin
        (unless (eq? type *scm-type*)
          (f "  if (!~a(value)) Scm_Error(\"~a required, but got %S\", value);"
             (~ type'c-predicate) (~ type'c-type)))
        (if (~ slot'c-spec)
          (f "  ~a = ~a;" (~ slot'c-spec) (cgen-unbox-expr type "value"))
          (f "  obj->~a = ~a;" (~ slot'c-name) (cgen-unbox-expr type "value")))))
    (p "}")
    (p "")))

(define-method emit-spec-definition ((slot <cslot>))
  (p "  SCM_CLASS_SLOT_SPEC(\""(~ slot'scheme-name)"\", "(slot-getter-name slot)", "(slot-setter-name slot)"),"))

;;===================================================================
;; Foreign pointers
;;

;; DEFINE-CPTR generates glue code to use ScmForeignPointer easily.
;; It is suitable when the C structure is mostly passed around using
;; pointers; most typically, when the foreign library allocates the
;; structure and returns the pointer to Scheme world.  It is possible
;; to allocate the structure in Scheme side, but the gist is that
;; foreign library treats it as a thing in heap, pointed by other
;; structures etc.
;;
;; define-cptr scm-name [qualifier] c-type c-name c-pred c-boxer c-unboxer
;;    [(flags flag ...)]
;;    [(print print-proc)]
;;    [(cleanup cleanup-proc)]
;;
;;  qualifier :: :private
;;  flag :: :keep-identity | :map-nul

(define-class <c-ptr> (<stub>)
  ((c-type       :init-keyword :c-type)
   (private      :init-keyword :private)
   (flags        :init-keyword :flags)
   (print-proc   :init-keyword :print-proc)
   (cleanup-proc :init-keyword :cleanup-proc)))

(define-form-parser define-cptr (scm-name . args)
  (check-arg symbol? scm-name)
  (receive (quals args) (span keyword? args)
    (let1 z (lset-difference eqv? quals '(:private))
      (unless (null? z)
        (errorf <cgen-stub-error>
                "invalid qualifier(s) ~s in (define-cptr ~s ...)"
                 z scm-name)))
    (match args
      [(c-type c-varname c-pred c-boxer c-unboxer . clauses)
       (let ([flags (or (assq-ref clauses 'flags) '())]
             [print-proc (assq-ref clauses 'print)]
             [cleanup-proc (assq-ref clauses 'cleanup)])
         (cond
          [(lset-difference eqv? flags '(:keep-identity :map-null)) pair?
           => (cut error <cgen-stub-error>
                   "unknown define-cptr flag(s)" <>)])
         (let1 fptr (make <c-ptr>
                      :scheme-name scm-name :c-type c-type :c-name c-varname
                      :private (memq :private quals)
                      :flags flags
                      :print-proc (and print-proc (car print-proc))
                      :cleanup-proc (and cleanup-proc (car cleanup-proc)))
           (make-cgen-type scm-name c-type
                           (x->string scm-name) ; description
                           (x->string c-pred)
                           (x->string c-boxer)
                           (x->string c-unboxer))
           (cgen-add! fptr)))]
      [_ (error <cgen-stub-error> "invalid define-cptr clause for" scm-name)])))

(define-method cgen-emit-body ((self <c-ptr>))
  (when (~ self'private)
    (let1 type (cgen-type-from-name (~ self'scheme-name))
      (p "static ScmClass *" (~ self'c-name) ";")
      (p "#define "(~ type'unboxer)"(obj) "
         "SCM_FOREIGN_POINTER_REF("(~ self'c-type)", obj)")
      (p "#define "(~ type'c-predicate)"(obj) "
         "SCM_XTYPEP(obj, "(~ self'c-name)")")
      (p "#define "(~ type'boxer)"(ptr) "
         "Scm_MakeForeignPointer("(~ self'c-name)", ptr)")))
  )

(define-method cgen-emit-init ((self <c-ptr>))
  (p "  "(~ self'c-name)" = Scm_MakeForeignPointerClass(Scm_CurrentModule(),"
     "\""(~ self'scheme-name)"\", "
     (or (~ self'print-proc) "NULL")", "
     (or (~ self'cleanup-proc) "NULL")", "
     (let1 flags (cond-list
                  [(memq ':keep-identity (~ self'flags))
                   "SCM_FOREIGN_POINTER_KEEP_IDENTITY"]
                  [(memq ':map-null (~ self'flags))
                   "SCM_FOREIGN_POINTER_MAP_NULL"])
       (if (pair? flags)
         (string-join flags "|")
         "0"))
     ");"))

;;===================================================================
;; Miscellaneous utilities
;;

;; Check if item is in the form (c <c-expr>)
(define (c-literal? item)
  (match item
    [('c _) #t]
    [else #f]))

;; Exteract <c-expr> from (c <c-expr>) form as a string.
(define (c-literal-expr item)
  (match item
    [('c (? string? e)) e]
    [('c cise) (call-with-output-string (cut cise-render cise <> 'expr))]
    [else #f]))

;; Given a c-code fragment in string or cise, get a C code in string.
(define (c-code string-or-cise)
  (if (string? string-or-cise)
    string-or-cise
    (call-with-output-string (cut cise-render string-or-cise <> #f))))

;;===================================================================
;; Main parsers
;;

(define-form-parser if (test then . maybe-else)
  (cgen-with-cpp-condition test
    (cgen-stub-parse-form then))
  (unless (null? maybe-else)
    (cgen-with-cpp-condition `(not ,test)
      (cgen-stub-parse-form (car maybe-else)))))

(define-form-parser begin forms
  (for-each cgen-stub-parse-form forms))

(define-form-parser when (test . forms)
  (cgen-with-cpp-condition test
    (for-each cgen-stub-parse-form forms)))

(define-form-parser include (file)
  (unless (file-exists? file)
    ;; TODO: search path
    (error <cgen-stub-error> "couldn't find include file: " file))
  (with-input-from-file file
    (^[] (port-for-each cgen-stub-parse-form read)))
  )

(define-form-parser initcode codes
  (dolist [c codes]
    (if (string? c)
      (cgen-init c)
      (cgen-init (call-with-output-string (cut cise-render c <>))))))

(define-form-parser declcode codes
  (dolist [c codes]
    (if (string? c)
      (cgen-decl c)
      (cgen-decl (call-with-output-string (cut cise-render c <>))))))

(define-form-parser begin forms
  (for-each cgen-stub-parse-form forms))

(define-form-parser eval* exprs
  (let* ([m (~ (cgen-current-unit)'temporary-module)]
         [r (fold (^[f r] (eval f m)) #f exprs)])
    (when (or (pair? r) (string? r))
      (cgen-stub-parse-form r))))

