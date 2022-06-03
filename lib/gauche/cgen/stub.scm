;;;
;;; gauce.cgen.stub - stub forms parsing and code generation
;;;
;;;   Copyright (c) 2000-2022  Shiro Kawai  <shiro@acm.org>
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
  (use scheme.list)
  (use srfi-13)
  (use srfi-42)
  (use util.match)
  (use text.tr)
  (use text.tree)
  (use file.util)
  (use gauche.sequence)
  (use gauche.mop.instance-pool)
  (use gauche.cgen.type)
  (use gauche.cgen.unit)
  (use gauche.cgen.literal)
  (use gauche.cgen.cise)
  (use gauche.cgen.tmodule)
  (export <cgen-stub-unit> <cgen-stub-error>
          cgen-stub-cise-ambient
          cgen-genstub
          cgen-stub-parser cgen-stub-parse-form)
  )
(select-module gauche.cgen.stub)

;; Summary of forms
;;
;;   declare-stub-type name c-type [desc c-predicate unboxer boxer]
;;
;;      Register a new type to be recognized.  This is only to tell the
;;      stub generator about how C code should be generated dealing
;;      with the given type.  It is independent from the actual
;;      Scheme class/type---only effective during stub generation of
;;      the file.
;;
;;      This restriction seems too limiting, but actually, you can't refer
;;      to the external types introduced in the extension module anyway;
;;      so this form is only effective for the types introduced
;;      in the extension module itself.
;;
;;      This form is called define-type before, but it may be confusing
;;      with the actual 'types' in Gauche, so we renamed.  The old
;;      form will be supported for a while.
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
;;        (setter <setter-name>) : specify setter.  <setter-name> should
;;             be a cproc name defined in the same stub file
;;        (setter (args ...) body ...) : specify setter anonymously.
;;
;;        (catch (<decl> <C-stmt> ...) ...) : when writing a stub
;;             for C++ function that may throw an exception, use this spec
;;             to ensure the exception will be caught and converted to
;;             Gauche error condition.
;;
;;        (inliner <insn-name>) : only used in Gauche core procedures
;;             that can be inlined into an VM instruction.
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
;;   define-cgeneric name c-name property-clause ...
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
;;   define-cstruct scheme-name c-struct-name (slot-spec ...)
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
;;   define-cenum scm-name c-type-name (enum ...)
;;      Also register c enum type name as a stub type.
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

(define (cgen-genstub stubfile :key (predef-syms '())
                                    (output-directory #f))
  (unless (file-is-readable? stubfile)
    (error <cgen-stub-error> "Couldn't read input file:" stubfile))
  (let ([prefix ($ cgen-safe-name-friendly $ path-sans-extension
                   $ sys-basename stubfile)]
        [unit-name (path-sans-extension (sys-basename stubfile))])
    (parameterize ([cgen-current-unit
                    (make <cgen-stub-unit>
                      :name unit-name
                      :c-name-prefix (string-append prefix "_")
                      :c-file (and output-directory
                                   #"~(build-path output-directory unit-name).c")
                      :h-file (and output-directory
                                   #"~(build-path output-directory unit-name).h")
                      :preamble '("/* Generated by genstub.  Do not edit. */")
                      :init-prologue (format "void Scm_Init_~a(ScmModule *mod SCM_UNUSED)\
                                              {\n" prefix))])
      (with-tmodule-recording
       <tmodule>
       (for-each cgen-define predef-syms)
       (cgen-include "<gauche.h>")
       (with-input-from-file stubfile
         (cut generator-fold
              ;; We treat the initial raw strings specially---they will be
              ;; in decl part.  The flag decl-strings? tracks that.
              (^[form decl-strings?]
                (cond [(and decl-strings? (string? form)) (cgen-decl form) #t]
                      [else
                       (guard (e [else
                                  ($ raise $ make-compound-condition e
                                     $ make <compile-error-mixin> :expr form)])
                         (cgen-stub-parse-form form) #f)]))
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
  `(make <form-parser> :name ',name :args ',args
         :handler (^ (&whole ,@args) ,@body)))

(define-macro (define-form-parser-alias name orig)
  `(if-let1 p (instance-pool-find <form-parser> (^o (eq? (~ o'name) ',orig)))
     (make <form-parser> :name ',name :args (~ p'args) :handler (~ p'handler))
     (error "unknown form parser:" ',orig)))

(define-syntax export-toplevel-cise-form
  (syntax-rules ()
    [(_ name)
     (define-form-parser name args
       (parameterize ([cise-ambient (cise-ambient-copy (cise-ambient) '())])
         (cgen-body (cise-render-to-string `(name ,@args) 'toplevel))
         (cgen-decl (cise-ambient-decl-strings (cise-ambient)))))]))

(define-method invoke ((self <form-parser>) form)
  (define (badform)
    (errorf <cgen-stub-error> "stub: malformed ~a: ~s" (~ self'name) form))
  (apply (~ self'handler)
         ;; the first (implicit) argument is form itself, bound to &whole
         form
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
  (cond [(instance-pool-find <form-parser> (^p (eq? key (~ p'name))))
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

;; toplevel CiSE forms
(export-toplevel-cise-form declare-cfn)
(export-toplevel-cise-form declare-cvar)
(export-toplevel-cise-form define-cfn)
(export-toplevel-cise-form define-ctype)
(export-toplevel-cise-form define-cvar)

; CiSE forms .if, .cond, .when and .unless are not exported because
; there are (or will be) stub replacements of the same name.
(export-toplevel-cise-form .define)
(export-toplevel-cise-form .include)
(export-toplevel-cise-form .undef)

;; extra check of valid clauses
(define (check-clauses directive name clauses valid-keys)
  (and-let1 bad (find (^c (or (not (pair? c))
                              (not (memq (car c) valid-keys))))
                      clauses)
    (errorf <cgen-stub-error>
            "Invalid clause for ~s ~s: Expected a list starting with either one of ~s, but got: ~s"
            directive name valid-keys bad)))

;;===================================================================
;; Type handling
;;

(define (name->type name)
  (or (cgen-type-from-name name)
      (error <cgen-stub-error> "unknown stub-type: " name)))

;; declare-stub-type name c-type [desc c-predicate unboxer boxer]
;;
;;   Creates a new stub type for existing scheme type.
;;   This form itself doesn't generate any C code, but predicate, boxer
;;   and unboxer will be used by other stub forms to generate C code
;;   based on stub types.

(define-form-parser declare-stub-type args
  (unless (<= 2 (length args) 6)
    (error <cgen-stub-error> "malformed declare-stub-type: "
           `(declare-stub-type . ,args)))
  (apply (^[name c-type :optional (desc #f) (c-pred #f) (unbox #f) (box #f)]
           (make-cgen-type name #f c-type desc c-pred unbox box))
         args))

(define-form-parser-alias define-type declare-stub-type)

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
   (type     :init-keyword :type)
   ;; - <cgen-type>: Stub type of this arg
   (default  :init-keyword :default :init-value #f)
   ;; - #f or <cgen-literal> : default value for optional/keyword arg
   ))

;; required, optional and optarray arg uses "count" slot, that indicates
;; the arg is count-th argument in the procedure.
;; The opt-count is an integer, indicating the position of this optional
;; arg counted from the beginning of the optional arg
(define-class <required-arg> (<arg>)
  ((count     :init-keyword :count)))
(define-class <optional-arg> (<arg>)
  ((count     :init-keyword :count)
   (opt-count :init-keyword :opt-count)))
(define-class <keyword-arg>  (<arg>)
  (keyword))
(define-class <rest-arg>     (<arg>)
  ())
(define-class <optarray-arg> (<arg>)
  ((count     :init-keyword :count)
   (count-var :init-keyword :count-var)
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

;; Argument named '_' is just a placehodler and ignored.
(define (unused-arg? arg) (eq? (~ arg'name) '_))
(define (used-args args) (remove unused-arg? args))

;;===================================================================
;; Symbol and keyword definition
;;

;;-------------------------------------------------------------------
;; (define-symbol scheme-name c-name)

(define-form-parser define-symbol (name c-name . maybe-init)
  (check-arg symbol? name)
  (check-arg string? c-name)
  (let1 literal (make-literal name :c-name c-name)
    (cgen-decl #"#define ~c-name (~(cgen-c-name literal))")
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
      (cgen-decl #"#define ~c-name (~(cgen-cexpr symbol))"))
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
  (variable-parser-common #t name (list 'c #"Scm_MakeInteger(~name)") '()))

(define-form-parser define-enum-conditionally (name)
  (check-arg symbol? name)
  (cgen-with-cpp-condition `(defined ,name)
    (variable-parser-common #t name (list 'c #"Scm_MakeInteger(~name)") '())))

;; define-cenum scm-type c-type-name (enum ...)
;;   This combines define-type and declare-stub-type.
(define-form-parser define-cenum (scm-name c-type-name enums)
  (assume-type scm-name <symbol>)
  (assume-type c-type-name <string>)
  (assume-type enums <list>)
  (make-cgen-type scm-name #f c-type-name #f
                  "SCM_INTP" "SCM_INT_VALUE" "SCM_MAKE_INT")
  (dolist [e enums]
    (variable-parser-common #t e `(c ,#"Scm_MakeInteger(~e)") '())))

;;-------------------------------------------------------------------
;; (define-keyword scheme-name c-name)

(define-form-parser define-keyword (name c-name)
  (check-arg symbol? name)
  (check-arg string? c-name)
  (let1 literal (make-literal (make-keyword name) :c-name c-name)
    (cgen-decl #"#define ~c-name (~(cgen-c-name literal))")
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
      ;; # of optional arguments.  keyword args and rest-args
      ;; are counted as 1 if the procedure takes it.
   (have-rest-arg?    :initform #f  :init-keyword :have-rest-arg?)
      ;; true if the procedure takes a rest-arg.
   (allow-other-keys? :initform '() :init-keyword :allow-other-keys?)
      ;; true if :allow-other-keys has veen given.
   (return-type       :initform #f :init-keyword :return-type)
      ;; return type given by ::<type>.
   (decls             :initform '())
      ;; reverse list of C declaration that should come before body stmts.
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
   (forward-decls     :initform '())
      ;; list of strings for code that must be emitted before the
      ;; procedure definition
   (source-info       :initform #f :init-keyword :source-info)
      ;; (file::<string> line-no::<integer>), if known.
      ;; Will be saved in pair attributes of info.
   (bind-info         :initform #f :init-keyword :bind-info)
      ;; (module-name::<symbol> proc-name::<symbol>), if known.
      ;; Will be saved in pair attributes of info.
   (type-info         :initform #f :init-keyword :type-info)
      ;; cgen-literal of a vector-encoded type info; see compute-type-info below
   (info              :initform #f)
      ;; cgen-literal of ScmObj to be set in the 'info' slot of ScmProcedure.
   ))

(define (get-arg cproc arg) (find (^x (eq? arg (~ x'name))) (~ cproc'args)))
(define (push-stmt! cproc stmt) (push! (~ cproc'stmts) stmt))
(define (push-decl! cproc decl) (push! (~ cproc'decls) decl))

(define-generic c-stub-name )

;; API
;; This parameter holds <procstub> during processing the body.
(define current-procstub (make-parameter #f))

;; Contains a list of return type objects the current cise expression
;; supposed to generate.  This may differ from (~ (current-procstub)'rettype),
;; for the cise expression may call a C routine that generates multiple
;; values directly on VM.  In that case, this parameter contains
;; just (#<type top>).   () means no return value.  #f means no information.
(define cise-return-types (make-parameter #f))

;; Given rettype : #f | symbol | (symbol ...),
;; bind cise-return-types with (Type ...)
(define-syntax with-return-types
  (syntax-rules ()
    [(_ rettype body ...)
     (parameterize ([cise-return-types
                     (match rettype
                       [#f `(,*scm-type*)]
                       ['<void> '()]
                       [(? symbol?) `(,(name->type rettype))]
                       [(? list?) (map name->type rettype)]
                       [_ (error <cgen-stub-error>
                                 "invalid cproc return type:" rettype)])])
       body ...)]))

;; API
;; We use customized cise context while generating stubs
(define (cgen-stub-cise-ambient :optional (orig-ambient (cise-ambient)))
  (rlet1 ctx (cise-ambient-copy orig-ambient '())
    ;; Override "return"
    ($ cise-register-macro! 'return
       (^[form env]
         (when (and (cise-return-types)
                    (not (= (length (cdr form))
                            (length (cise-return-types)))))
           (errorf <cgen-stub-error>
                   "return macro got wrong number of return values (expecting ~s): ~s"
                   (length (cise-return-types)) form))
         (match form
           [(_)         `(goto SCM_STUB_RETURN)]
           [(_ e0)       `(begin (set! SCM_RESULT ,e0)
                                 (goto SCM_STUB_RETURN))]
           [(_ xs ...) `(begin
                          (set!
                           ,@(concatenate
                              (map-with-index
                               (^[i x] `(,(string->symbol #"SCM_RESULT~i") ,x))
                               xs)))
                          (goto SCM_STUB_RETURN))]))
       ctx)))

(define-syntax with-cise-ambient
  (syntax-rules ()
    [(_ procstub body ...)
     (parameterize ([cise-ambient (cgen-stub-cise-ambient (cise-ambient))]
                    [current-procstub procstub])
       body ...
       (push! (~ procstub'forward-decls)
              (cise-ambient-decl-strings (cise-ambient))))]))

;; Construct stub type info
;; We don't create <^>-type, for it'd be tricky to serialize, and we don't
;; want to spend initialization time to reconstruct it for something that
;; might not be used.
;; The current format is a vector, with the first element 1 indicates the
;; version.  Followed by the module name, and arguments for <^> but the
;; stub type is represented.
;; NB: The module name may be #f, when we're precompiling stub file (as opposed
;; to the scm file).  For, in case of the stub file, the module is passed
;; to the initialization routine.  In that case, we'll patch up the typehint
;; vector at the init code.  (See cgen-emit-init <cpproc> below).
(define (compute-type-info procstub)
  (define (arg-types args)
    (let loop ([args args] [types '()])
      (cond [(null? args) (reverse types)]
            [(is-a? (car args) <required-arg>)
             (loop (cdr args)
                   (cons (~ (car args)'type'name) types))]
            [else ;; all auxiliary arguments are rest arg as type
             (reverse types '(*))])))
  (define (ret-types types)
    (cond
     [(eq? types #f) '(<top>)]
     [(list? types) types]
     [else (ret-types (list types))]))
  (list->vector
   `(1 ,(and-let1 tm (current-tmodule) (~ tm'name))
       ,@(arg-types (~ procstub'args))
       ->
       ,@(ret-types (~ procstub'return-type)))))

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
                      ;; Heuristics - Ideally &whole has source info, but
                      ;; precomp may have reconstructed the form and the
                      ;; original source info may be lost.  We try to find
                      ;; relevant info from a few subforms.
                      ;; Once we fix precomp, we can just say
                      ;; (debug-source-info &whole).
                      :source-info (or (debug-source-info &whole)
                                       (debug-source-info (cdr &whole))
                                       (debug-source-info (caddr &whole)))
                      :bind-info (and-let* ([m (current-tmodule)]
                                            [mod-name (~ m'name)]
                                            [ (symbol? mod-name) ])
                                   (list mod-name scheme-name))
                      :keyword-args keyargs
                      :num-reqargs nreqs
                      :num-optargs nopts
                      :have-rest-arg? rest?
                      :allow-other-keys? other-keys?)
          (process-body cproc body)
          (check-fast-flonum cproc args)
          (assign-proc-info! cproc)
          (set! (~ cproc'type-info) (cgen-literal (compute-type-info cproc)))
          (cgen-add! cproc))))))

(define-method c-stub-name ((cproc <cproc>)) #"~(~ cproc'c-name)__STUB")

;; Get stub name of the named cproc in the current unit.
(define (cproc-stub-c-name scheme-name)
  (if-let1 cproc (find (^p (eq? (~ p'scheme-name) scheme-name))
                       (get-stubs <cproc>))
    (c-stub-name cproc)
    (error "cproc-stub-c-name: No such cproc: " scheme-name)))

;; In cise, you can use .cproc macro to get cproc stub as ScmObj.
(define-cise-expr .cproc
  [(_ name)
   (unless (symbol? name) (error ".cproc requires a symbol, but got:" name))
   `(SCM_OBJ (& (C: ,(cproc-stub-c-name name))))])

;; Allow to call Scheme procedure from <procstub> body.
;;
;;  (.funcall/rec scheme-proc-name arg ...)
;;  => static ScmObj tmp = SCM_UNDEFINED;
;;     SCM_BIND_PROC(tmp, "scheme-proc-name", current_module);
;;     Scm_ApplyRec(tmp, args);
;;
;;  (.funcall/cps scheme-proc-name arg ...)
;;  => static ScmObj tmp = SCM_UNDEFINED;
;;     SCM_BIND_PROC(tmp, "scheme-proc-name", current_module);
;;     Scm_VMApplyRec(tmp, args);

;; returns tmp-var name (symbol)
;; module can be #f (then the tmodule is used), or a symbol
(define (%insert-funcall-decls who scheme-proc-name module)
  (if-let1 cproc (current-procstub)
    (rlet1 tmpvar (gensym "proc")
      (let ([ctmpvar (cgen-safe-name (symbol->string tmpvar))]
            [modexp  (if module
                       #"Scm_FindModule(SCM_SYMBOL(SCM_INTERN(\"~module\")),\
                                        SCM_FIND_MODULE_CREATE)"
                       #"SCM_MODULE(~(tmodule-cname (current-tmodule)))")])
        (push-decl! cproc #"static ScmObj ~ctmpvar = SCM_UNDEFINED;")
        (push-decl! cproc #"SCM_BIND_PROC(~ctmpvar, \
                             \"~scheme-proc-name\", \
                             ~modexp);")))
    (errorf "~a form is not in define-cproc" who)))

(define (%expand-funcall-apply applyproc tmpvar args)
  (define (suffix n) (string->symbol #"~|applyproc|~|n|"))
  (match args
    [()           `(,(suffix 0) ,tmpvar)]
    [(a)          `(,(suffix 1) ,tmpvar ,a)]
    [(a b)        `(,(suffix 2) ,tmpvar ,a ,b)]
    [(a b c)      `(,(suffix 3) ,tmpvar ,a ,b ,c)]
    [(a b c d)    `(,(suffix 4) ,tmpvar ,a ,b ,c ,d)]
    [(a b c d e)  `(,(suffix 5) ,tmpvar ,a ,b ,c ,d ,e)]
    ;; TODO: for more than 5 args, emit list generation code
    ))

(define-cise-expr .funcall/rec
  [(_ (module-name scheme-proc-name) arg ...)
   (let1 tmpvar (%insert-funcall-decls '.funcall/rec
                                       scheme-proc-name module-name)
     (%expand-funcall-apply 'Scm_ApplyRec tmpvar arg))]
  [(_ scheme-proc-name arg ...)
   (let1 tmpvar (%insert-funcall-decls '.funcall/rec scheme-proc-name #f)
     (%expand-funcall-apply 'Scm_ApplyRec tmpvar arg))])

(define-cise-expr .funcall/cps
  [(_ (module-name scheme-proc-name) arg ...)
   (let1 tmpvar (%insert-funcall-decls '.funcall/cps
                                       scheme-proc-name module-name)
     (%expand-funcall-apply 'Scm_VMApply tmpvar arg))]
  [(_ scheme-proc-name arg ...)
   (let1 tmpvar (%insert-funcall-decls '.funcall/cps scheme-proc-name #f)
     (%expand-funcall-apply 'Scm_VMApply tmpvar arg))])

;; create arg object.  used in cproc and cmethod
;; NB: count arg is ignored if class is <keyword-arg> or <rest-arg>
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
      [(':rest . specs)     (rest specs args '() nreqs 1 #f)]
      [(':key . specs)      (keyword specs args '() nreqs 1)]
      [(':allow-other-kyes . specs)
       (errorf <cgen-stub-error>
               "misplaced :allow-other-key parameter: ~s in ~a" argspecs name)]
      [(':optarray (var cnt maxcnt) . specs)
       (let1 args (cons (make-arg <optarray-arg> var nreqs
                                  :count-var cnt :max-count maxcnt)
                        args)
         (match specs
           [() (values (reverse args) '() nreqs maxcnt #f #f)]
           [(':rest . specs) (rest specs args '() nreqs (+ maxcnt 1) #f)]
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
      [(':rest . specs)     (rest specs args '() nreqs (+ nopts 1) #f)]
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
                (cons (make-arg <keyword-arg> sym 0)
                      keyargs)
                nreqs nopts)]
      [(([? symbol? sym] default) . specs)
       (keyword specs args
                (cons (make-arg <keyword-arg> sym 0
                                :default (make-literal default))
                      keyargs)
                nreqs nopts)]
      [_ (badarg (car specs))]))

  (define (rest specs args keyargs nreqs nopts other-keys?)
    (match specs
      [() (values (reverse args) (reverse keyargs) nreqs nopts #t other-keys?)]
      [((? symbol? sym))
       (values (reverse (cons (make-arg <rest-arg> sym 0) args))
               (reverse keyargs)
               nreqs nopts #t other-keys?)]
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
                      `((return (,s ,@args))))] ; this is the overridden "return" - see cgen-stub-cise-ambient above
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
                       :c-name #"~(~ cproc'c-name)_SETTER"
                       :proc-name (make-literal (x->string `(setter ,(~ cproc'scheme-name))))
                       :args args :return-type rettype
                       :keyword-args keyargs
                       :num-reqargs nreqs
                       :num-optargs nopts
                       :have-rest-arg? rest?
                       :allow-other-keys? other-keys?)
          (set! (~ cproc'setter) #"~(~ setter'c-name)__STUB")
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
    (push-stmt! cproc #"~(~ rettype'c-type) SCM_RESULT;")
    (push-stmt! cproc #"SCM_RESULT = ~c-func-name(~(args));")
    (push-stmt! cproc "goto SCM_STUB_RETURN;") ; avoid 'label not used' error
    (push-stmt! cproc "SCM_STUB_RETURN:") ; label
    (push-stmt! cproc (cgen-return-stmt (cgen-box-tail-expr rettype "SCM_RESULT")))
    (push-stmt! cproc "}"))
  ($ with-cise-ambient cproc
     (match form
       [([? check-expr expr])
        (let1 rt (~ cproc'return-type)
          (if rt
            (process-call-spec cproc `(,rt ,expr)) ; for transition
            (typed-result *scm-type* expr)))]
       [('<void> [? check-expr expr])
        (push-stmt! cproc #"~expr(~(args));")
        (push-stmt! cproc "goto SCM_STUB_RETURN;") ; avoid 'label not used' error
        (push-stmt! cproc "SCM_STUB_RETURN:") ; label
        (push-stmt! cproc "SCM_RETURN(SCM_UNDEFINED);")]
       [(typename [? check-expr expr])
        (unless (and (symbol? typename) (check-expr expr)) (err))
        (typed-result (name->type typename) expr)]
       [else (err)])))

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
      (push-stmt! cproc #"~(~ rettype'c-type) SCM_RESULT;")
      (push-stmt! cproc #" SCM_RESULT = (~expr);")
      (push-stmt! cproc "goto SCM_STUB_RETURN;") ; avoid 'label not used' error
      (push-stmt! cproc "SCM_STUB_RETURN:") ; label
      (push-stmt! cproc (cgen-return-stmt (cgen-box-tail-expr rettype "SCM_RESULT")))
      (push-stmt! cproc "}")))
  ($ with-cise-ambient cproc
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
       [else (error <cgen-stub-error> "malformed 'expr' spec:" form)])))

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
    (push-stmt! cproc #"~(~ rettype'c-type) SCM_RESULT;")
    (for-each expand-stmt stmts)
    (push-stmt! cproc "goto SCM_STUB_RETURN;") ; avoid 'label not used' error
    (push-stmt! cproc "SCM_STUB_RETURN:") ; label
    (push-stmt! cproc (cgen-return-stmt (cgen-box-tail-expr rettype "SCM_RESULT")))
    (push-stmt! cproc "}"))
  (define (typed-results rettypes stmts)
    (let1 nrets (length rettypes)
      (for-each-with-index
       (^[i rettype] (push-stmt! cproc #"~(~ rettype'c-type) SCM_RESULT~i;"))
       rettypes)
      (push-stmt! cproc "{")
      (for-each expand-stmt stmts)
      (push-stmt! cproc "}")
      (let1 results
          (string-join
           (map-with-index (^[i rettype]
                             (cgen-box-tail-expr rettype #"SCM_RESULT~i"))
                           rettypes)
           ",")
        (push-stmt! cproc "goto SCM_STUB_RETURN;") ; avoid 'label not used' error
        (push-stmt! cproc "SCM_STUB_RETURN:") ; label
        (push-stmt! cproc
                    (case nrets
                      [(0) (cgen-return-stmt "Scm_Values(SCM_NIL)")]
                      [(1) (cgen-return-stmt results)]
                      [(2) (cgen-return-stmt #"Scm_Values2(~results)")]
                      [(3) (cgen-return-stmt #"Scm_Values3(~results)")]
                      [(4) (cgen-return-stmt #"Scm_Values4(~results)")]
                      [(5) (cgen-return-stmt #"Scm_Values5(~results)")]
                      [else (cgen-return-stmt
                             #"Scm_Values(Scm_List(~results, NULL))")]))
        )))
  ($ with-return-types rettype
     $ with-cise-ambient cproc
     (match (cise-return-types)
       [()
        (for-each expand-stmt body)
        (push-stmt! cproc "goto SCM_STUB_RETURN;") ; avoid 'label not used' error
        (push-stmt! cproc "SCM_STUB_RETURN:") ; label
        (push-stmt! cproc "SCM_RETURN(SCM_UNDEFINED);")]
       [(type) (typed-result type body)]
       [[types ...] (typed-results types body)])))

(define-method assign-proc-info! ((proc <cproc>))
  (define (arginfo arg) (~ arg'name))
  (let* ([qargs (filter (cut is-a? <> <required-arg>) (~ proc'args))]
         [oargs (filter (cut is-a? <> <optional-arg>) (~ proc'args))]
         [kargs (filter (cut is-a? <> <keyword-arg>) (~ proc'args))]
         [rarg  (filter (cut is-a? <> <rest-arg>) (~ proc'args))]
         [aarg  (filter (cut is-a? <> <optarray-arg>) (~ proc'args))]
         [all-args `(,@(map arginfo qargs)
                     ,@(cond-list
                        [(pair? oargs) @ `(:optional ,@(map arginfo oargs))]
                        [(pair? kargs) @ `(:key ,@(map arginfo kargs))]
                        [(pair? rarg) @ `(:rest ,(arginfo (car rarg)))]
                        [(pair? aarg) @ `(:optarray ,(arginfo (car aarg)))]))])
    (set! (~ proc'info)
          (make-literal
           (if (or (~ proc'source-info) (~ proc'bind-info))
             ($ make-serializable-extended-pair (~ proc'scheme-name) all-args
                (cond-list
                 [(~ proc'source-info) => (cut cons 'source-info <>)]
                 [(~ proc'bind-info)   => (cut cons 'bind-info <>)]))
             (cons (~ proc'scheme-name) all-args))))))

;;;
;;; Emitting code
;;;

(define-method cgen-emit-decl ((cproc <cproc>))
  (next-method)
  ;; We emit forward definitions of C function names and stub record definitions
  ;; first, so that the function body can refer to the stub record.
  (p "static ScmObj "(~ cproc'c-name)"(ScmObj*, int, void*);")
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
    (format #t "SCM_FALSE,")                          ; info - to be set in init
    (unless (null? flags)                             ; flags
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

(define-method cgen-emit-body ((proc <procstub>))
  (dolist [s (~ proc'forward-decls)] (p s)))

(define-method cgen-emit-body ((cproc <cproc>))
  (next-method)
  (p "static ScmObj "(~ cproc'c-name)"(ScmObj *SCM_FP SCM_UNUSED, int SCM_ARGCNT SCM_UNUSED, void *data_ SCM_UNUSED)")
  (p "{")
  ;; argument decl
  (for-each emit-arg-decl (used-args (~ cproc'args)))
  (for-each emit-arg-decl (used-args (~ cproc'keyword-args)))
  (let1 arg-array-size (+ (length (~ cproc'args))
                          (~ cproc'num-optargs)
                          (if (null? (~ cproc'keyword-args)) 0 -1))
    (unless (zero? arg-array-size)
      (p "  ScmObj SCM_SUBRARGS["arg-array-size"];")))
  (unless (null? (used-args (~ cproc'keyword-args)))
    (p "  ScmObj SCM_OPTARGS = SCM_ARGREF(SCM_ARGCNT-1);"))
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
    (unless (zero? k)
      (p "  for (int SCM_i=0; SCM_i<"k"; SCM_i++) {")
      (p "    SCM_SUBRARGS[SCM_i] = SCM_ARGREF(SCM_i);")
      (p "  }")))
  (for-each emit-arg-unbox (used-args (~ cproc'args)))
  (unless (null? (~ cproc'keyword-args))
    (emit-keyword-args-unbox cproc))
  ;; body
  (unless (null? (~ cproc'c++-handlers))
    (p "try {"))
  (p "  {")
  (for-each p (reverse (~ cproc'decls)))
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
  (p))

(define-method cgen-emit-init ((cproc <cproc>))
  (when (symbol? (~ cproc'scheme-name))
    (f "  Scm_MakeBinding(SCM_MODULE(~a), SCM_SYMBOL(SCM_INTERN(~s)), SCM_OBJ(&~a), ~a);"
       (stub-tmodule-cname cproc)
       (symbol->string (~ cproc'scheme-name))
       (c-stub-name cproc)
       (if (or (~ cproc'inline-insn) (memq :constant (~ cproc'flags)))
         "SCM_BINDING_INLINABLE" "0")))
  (when (~ cproc'info)
    (f "  ~a.common.info = ~a;"
       (c-stub-name cproc) (cgen-c-name (~ cproc'info))))
  (when (~ cproc'type-info)
    (cgen-with-cpp-condition `(>= GAUCHE_API_VERSION 98)
      (f "  ~a.common.typeHint = ~a;"
         (c-stub-name cproc) (cgen-c-name (~ cproc'type-info)))
      (unless (current-tmodule)
        ;; we're precompiling *.stub file (old style), instead of *.scm style.
        ;; patch up the module name.
        (f "  SCM_VECTOR_ELEMENT(~a, 1) = SCM_MODULE(mod)->name;"
           (cgen-c-name (~ cproc'type-info))))))
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
           (emit (c-stub-name setter))
           #t)
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
  (f "  if (!~a) Scm_Error(\"~a required, but got %S\", ~a);"
     (cgen-pred-expr (~ arg'type) (~ arg'scm-name))
     (~ arg'type'description) (~ arg'scm-name))
  (p "  "(~ arg'c-name)" = "(cgen-unbox-expr (~ arg'type) (~ arg'scm-name))";"))

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
  (let ([args (used-args (~ cproc'keyword-args))]
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
  (next-method)
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
                     ;; NB: See define-cproc about this.
                     :source-info (or (debug-source-info &whole)
                                      (debug-source-info (cdr &whole))
                                      (debug-source-info (caddr &whole)))
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
  (next-method)
  (f "static ScmObj ~a(ScmNextMethod *nm_ SCM_UNUSED, ScmObj *SCM_FP, int SCM_ARGCNT SCM_UNUSED, void *d_ SCM_UNUSED)"
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
  (for-each p (reverse (~ method'decls)))
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
;;  - Define-cclass automatically creates corresponding stub-type.
;;    By default, C predicate, unboxer and boxer names are inferred from
;;    the scheme name.  If you need different names, you can specify them
;;    in the options.

;; (define-cclass scheme-name [qualifiers] c-type-name c-class-name cpa
;;   (<slot-spec> ...)
;;   ;; optional fields - can appear in any order
;;   (allocator <proc-spec>)
;;   (printer   <proc-spec>)
;;   (comparer  <proc-spec>)
;;   (direct-supers <string> ...)
;;   (metaclass <class-name>)
;;   (c-predicate <string>)   ; for stub-type
;;   (unboxer <string>)       ; for stub-type
;;   (boxer <string>)         ; for stub-type
;;   )
;;
;; <slot-spec> := slot-name
;;             |  (slot-name
;;                  [:type <cgen-type>]
;;                  [:c-name <c-name>]
;;                  [:c-spec <c-spec>]
;;                  [:getter <proc-spec>]
;;                  [:setter <proc-spec>]
;;
;; <proc-spec> := <c-code> | (c <c-name>) | #f | #t
;;
;; <cpa> := (<string> ...)
;;
;; qualifiers := qualifier ...
;; qualifier := :base | :built-in | :private | :no-meta
;;
;; <class-name> := <string> | <symbol>
;;
;; 'qualifiers' modifies the generated code slightly.  :base and :built-in
;; are exclusive.  :base generates a base class definition (inheritable
;; from Scheme code), while :built-in generates a built-in class definition
;; (not inheritable from Scheme code).  If neither one appears, built-in
;; class is generated.  :private is an optional qualifier, and when it is
;; given, the class declaration and standard macro definitions are
;; also generated (which needs to be in the separate header file if you want
;; the C-level structure to be used from other C code.  If the extension is
;; small enough to be contained in one C file, this option is convenient.)
;; :no-meta is another optional qualifier and prevents a corresponding
;; metaclass to be generated (by default, when you define-cclass <foo>,
;; a metaclass <foo-meta> is automatically generated).
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
;;
;; 'metaclass' specifies the metaclass of this class; if specified, it msut
;; be either C class name (e.g. "SCM_CLASS_CLASS") or a symbol naming a
;; global variable bound to a class visible from the module.

(define-class <cclass> (<stub>)
  ((cpa        :init-keyword :cpa       :init-value '())
   (c-type     :init-keyword :c-type)
   (qualifiers :init-keyword :qualifiers)
   (metaclass  :init-keyword :metaclass :init-value #f)
   (allocator  :init-keyword :allocator :init-value #f)
   (printer    :init-keyword :printer   :init-value #f)
   (comparer   :init-keyword :comparer  :init-value #f)
   (slot-spec  :init-keyword :slot-spec :init-value '())
   (direct-supers :init-keyword :direct-supers :init-value '())
   ))

(define-class <cslot> ()
  ((cclass      :init-keyword :cclass)
   (scheme-name :init-keyword :scheme-name)
   (c-name      :init-keyword :c-name)
   (c-spec      :init-keyword :c-spec)
   (type        :init-keyword :type   :init-value '<top>)
   (getter      :init-keyword :getter :init-value #t)
   (setter      :init-keyword :setter :init-value #t)
   ;; init-cexpr is not currently used by <cclass>; it will eventually
   ;; be used to auto-generate allocator.
   (init-cexpr  :init-keyword :init-cexpr :init-value #f)
   ))

(define-form-parser define-cclass (scm-name . args)
  (check-arg symbol? scm-name)
  (receive (quals rest) (span keyword? args)
    (cond
     [(lset-difference eqv? quals '(:built-in :base :private :no-meta)) pair?
      => (cut error <cgen-stub-error> "unknown define-cclass qualifier(s)" <>)])
    (match rest
      [(c-type c-name cpa slot-spec . more)
       (define (get-opt opt init) (cond [(assq opt more) => cadr] [else init]))
       (define (listify x) (if (list? x) x (list x)))
       (check-arg string? c-name)
       (check-arg list? cpa)
       (check-arg list? slot-spec)
       (let* ([allocator (get-opt 'allocator #f)]
              [printer   (get-opt 'printer #f)]
              [comparer  (get-opt 'comparer #f)]
              [dsupers   (listify (get-opt 'direct-supers '()))]
              [metaclass (get-opt 'metaclass #f)]
              [c-pred    (get-opt 'c-predicate #f)]
              [unboxer   (get-opt 'unboxer #f)]
              [boxer     (get-opt 'boxer #f)]
              [cclass (make <cclass>
                        :scheme-name scm-name :c-type c-type :c-name c-name
                        :qualifiers quals
                        :cpa cpa :direct-supers dsupers
                        :allocator allocator :printer printer
                        :comparer comparer :metaclass metaclass)])
         (let1 stub-type
             (or (cgen-type-from-name scm-name)
                 (make-cgen-type scm-name #f c-type (x->string scm-name)
                                 c-pred unboxer boxer))
           (set! (~ stub-type'cclass) cclass))
         (set! (~ cclass'slot-spec) (process-cclass-slots cclass slot-spec))
         (cgen-add! cclass))])))

(define-method c-allocator-name ((self <cclass>))
  (let1 allocator (~ self'allocator)
    (cond [(c-literal-expr allocator)]
          [(not allocator) "NULL"]
          [else #"~(~ self'c-name)_ALLOCATE"])))

(define-method c-printer-name ((self <cclass>))
  (let1 printer (~ self'printer)
    (cond [(c-literal-expr printer)]
          [(not printer) "NULL"]
          [else #"~(~ self'c-name)_PRINT"])))

(define-method c-comparer-name ((self <cclass>))
  (let1 comparer (~ self'comparer)
    (cond [(c-literal-expr comparer)]
          [(not comparer) "NULL"]
          [else #"~(~ self'c-name)_COMPARE"])))

(define-method c-slot-spec-name ((self <cclass>))
  (if (null? (~ self'slot-spec))
    "NULL"
    #"~(~ self'c-name)__SLOTS"))

(define (cclass-emit-standard-decls self)
  (let1 type (cgen-type-from-name (~ self'scheme-name))
    (p "SCM_CLASS_DECL("(~ self'c-name)");")
    (p "#define "(cgen-unboxer-name type)"(obj) (("(~ self'c-type)")obj)")
    (p "#define "(cgen-pred-name type)"(obj) SCM_ISA(obj, (&"(~ self'c-name)"))")
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
    (p "static void "(c-printer-name self)"(ScmObj obj, ScmPort *port, ScmWriteContext *ctx SCM_UNUSED)")
    (p "{")
    (p (c-code (~ self'printer)))
    (p "}")
    (p ""))
  (unless ((any-pred not c-literal?) (~ self'comparer))
    (p "static int "(c-comparer-name self)"(ScmObj x, ScmObj y, int equalp)")
    (p "{")
    (p (c-code (~ self'comparer)))
    (p "}")
    (p ""))
  (emit-cpa self)
  (if (memv :base (~ self'qualifiers))
    (let1 c-type (string-trim-right (~ self'c-type))
      (unless (string-suffix? "*" c-type)
        (errorf <cgen-stub-error> "can't use C-type ~s as a base class; C-type must be a pointer type" c-type))
      (let1 c-instance-type (string-drop-right c-type 1)
        (p "SCM_DEFINE_BASE_CLASS("(~ self'c-name)", "c-instance-type", "(c-printer-name self)", "(c-comparer-name self)", NULL, "(c-allocator-name self)", "(cpa-name self)");")))
    (p "SCM_DEFINE_BUILTIN_CLASS("(~ self'c-name)", "(c-printer-name self)", "(c-comparer-name self)", NULL, "(c-allocator-name self)", "(cpa-name self)");"))
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
  (let ([class-addr #"&~(~ self'c-name)"]
        [class-name (cgen-safe-string (x->string (~ self'scheme-name)))]
        [specs  (c-slot-spec-name self)]
        [mod (stub-tmodule-cname self)])
    (define (gen-init-class/meta meta-cname)
      (p "  Scm_InitStaticClassWithMeta("class-addr", "class-name","
         " SCM_MODULE("mod"), "meta-cname", SCM_FALSE, "specs", 0);"))

    (cond [(memq :no-meta (~ self'qualifiers))
           (p "  Scm_InitStaticClass("class-addr", "class-name","
              " SCM_MODULE("mod"), "specs", 0);")]
          [(~ self'metaclass)
           => (^m (cond [(string? m) (gen-init-class/meta m)]
                        [(symbol? m)
                         (p "{ ScmObj meta = Scm_GlobalVariableRef("mod",\
                                  SCM_SYMBOL(SCM_INTERN(\"" m "\")), 0);")
                         (p "  if (SCM_UNBOUNDP(meta)) Scm_Error(\"Uknown metaclass: "m"\");")
                         (p "  if (!SCM_CLASSP(meta)) Scm_Error(\"Metaclass is not a class: %S\", meta);")
                         (gen-init-class/meta "SCM_CLASS(meta)")
                         (p "}")]
                        [else
                         (error "Invalid metaclass spec:" m)]))]
          [else (gen-init-class/meta "NULL")]))
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
        [else #"~(~ self'c-name)_CPL"]))

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
                [setter (get-keyword :setter (cdr spec) #t)]
                [init   (get-keyword :init   (cdr spec) #f)])
           (make <cslot>
             :cclass cclass :scheme-name name :c-name c-name
             :c-spec c-spec :type (name->type type)
             :getter getter :setter setter :init-cexpr init)))
       slot-spec))

(define-method slot-getter-name ((slot <cslot>))
  (let1 getter (~ slot'getter)
    (or
     (c-literal-expr getter)
     #"~(~ slot'cclass'c-name)_~(get-c-name \"\" (~ slot'scheme-name))_GET")))

(define-method slot-setter-name ((slot <cslot>))
  (let1 setter (~ slot'setter)
    (cond [(c-literal-expr setter)]
          [(not setter) "NULL"]
          [else #"~(~ slot'cclass'c-name)_~(get-c-name \"\" (~ slot'scheme-name))_SET"])))

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
           (f "  return ~a;" (cgen-box-tail-expr type (~ slot'c-spec)))]
          [else
           (f "  return ~a;" (cgen-box-tail-expr type #"obj->~(~ slot'c-name)"))])
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
          (f "  if (!~a) Scm_Error(\"~a required, but got %S\", value);"
             (cgen-pred-expr type "value") (~ type'c-type)))
        (if (~ slot'c-spec)
          (f "  ~a = ~a;" (~ slot'c-spec) (cgen-unbox-expr type "value"))
          (f "  obj->~a = ~a;" (~ slot'c-name) (cgen-unbox-expr type "value")))))
    (p "}")
    (p "")))

(define-method emit-spec-definition ((slot <cslot>))
  (p "  SCM_CLASS_SLOT_SPEC(\""(~ slot'scheme-name)"\", "(slot-getter-name slot)", "(slot-setter-name slot)"),"))

;;===================================================================
;; CStruct
;;

;; (define-cstruct scheme-name c-struct-name
;;   (<slot-spec> ...)
;;   )
;;
;;   This is to generate a stub to access plain old C struct, wrapped
;;   in a Scheme Object.  The C struct is embedded in the Scheme object,
;;   and its memory and lifetime are goverened by Gauche GC.
;;   This isn't a form for a C struct that is allocated and owned by
;;   the external library.
;;
;;   - The C struct is defined elsewhere (e.g. defined by the API).
;;     We call it c_struct_t here.
;;
;;   - The stub creates a Scheme object that embeds the structure:
;;
;;       struct {
;;          SCM_HEADER;
;;          c_struct_t data;
;;       };
;;
;;   - The Scheme class specified by scheme-name is defined to represent
;;     the above Scheme structure.
;;
;;   - When it is passed from Scheme world to C world, the pointer to
;;     the data field is passed.
;;
;;   - When C world returns it to the Scheme world, the content of the
;;     C struct is copied into a Scheme-owned structure.
;;
;;   <slot-spec> is the following format:
;;
;;       <slot-spec> := slot-name::<slot-type> ["<c-spec>"]
;;
;;   slot-name is what you see in Scheme world.
;;
;;   <slot-type> specifies the type of slot, and how to box/unbox the
;;   value from C struct.
;;
;;       <slot-type> := <stub-type>
;;                   |  (.array* <stub-type>)
;;                   |  (.array <stub-type>)
;;                   |  &<stub-type>
;;
;;   The '.array*' type denotes a pointer to an array of <stub-type>.
;;   The size of the array must e specified by another field of the struct.
;;   see <c-spec> description below.
;;
;;   <c-spec> is a string encodes C-related info.
;;
;;       <c-spec> := <c-name>? <c-length>? <c-init>?
;;       <c-name> ; C field name.  If omitted, slot-name is used.
;;       <c-length> := '[' <c-name> ']'
;;                  ; This is used for '.array*' slot, specifies the
;;                  ; C field that for the length of the array.
;;       <c-init> := '=' C-literal
;;                  ; Specifies the initial value.
;;
;;   For the '.array*' type, the pointed array is allocated by Gauche
;;   and subject to GC.

(define-form-parser define-cstruct (scm-name c-struct-name slots . opts)
  (define (get-opt opt init) (cond [(assq opt opts) => cadr] [else init]))
  (assume-type scm-name <symbol>)
  (assume-type c-struct-name <string>)
  (assume-type slots <list>)
  (let* ([TYPENAME ($ cgen-safe-name-friendly
                      $ (cut regexp-replace-all* <>  #/[<>]/ "")
                      $ symbol->string scm-name)]
         [ClassName #"Scm_~|TYPENAME|_Class"]
         [RecName #"Scm_~|TYPENAME|_Rec"]
         [BoxerName #"Scm_Make_~|TYPENAME|"]
         [type (make-cgen-type scm-name #f #"~|c-struct-name|*" #f
                               #f #f BoxerName)]
         [initializer (get-opt 'initializer #f)]
         [printer     (get-opt 'printer #f)]
         [comparer    (get-opt 'comparer #f)]
         [cclass (make <cclass>
                   :scheme-name scm-name
                   :c-type #"~|c-struct-name|*"
                   :c-name ClassName
                   :qualifiers '()
                   :cpa '()
                   :direct-supers '()
                   :allocator #f
                   :printer printer
                   :comparer comparer)]
         [slot-specs (cstruct-grok-slot-specs cclass slots)])
    (set! (~ cclass'slot-spec)
          (append-map (cut process-cstruct-slot cclass RecName <>) slot-specs))
    (cgen-decl "#include <gauche/class.h>")
    (cgen-decl #"typedef struct {"
               #"  SCM_HEADER;"
               #"  ~c-struct-name data;"
               #"} ~|RecName|;")
    (cgen-decl #"SCM_CLASS_DECL(~ClassName);")
    (cgen-decl #"#define ~(cgen-pred-name type)(obj) \
                         SCM_ISA(obj,&~|ClassName|)")
    (cgen-decl #"#define ~(cgen-unboxer-name type)(obj) \
                         &(((~RecName *)(obj))->data)")
    (cgen-decl #"SCM_EXTERN ScmObj ~(cgen-boxer-name type)(const ~|c-struct-name|*);")
    (cgen-body #"ScmObj ~(cgen-boxer-name type)(const ~|c-struct-name| *v)"
               #"{"
               #"  ~RecName *z = SCM_NEW(~RecName);"
               #"  SCM_SET_CLASS(z, &~ClassName);"
               #"  z->data = *v;")
    (dolist [slot (~ cclass'slot-spec)]
      (and-let1 init (~ slot'init-cexpr)
        (cgen-body #"  z->data.~(~ slot'c-name) = ~|init|;")))
    (cgen-body (if initializer
                 #"{ ~|c-struct-name| *obj = &z->data;\n~|initializer|\n}"
                 "")
               #"  return SCM_OBJ(z);"
               #"}")
    (cgen-add! cclass)))

;; returns (<cslot> ...)
(define (process-cstruct-slot cclass cclass-cname slot-spec)
  (match-let1 (slot-name type c-field c-length c-init) slot-spec
    (receive (type getter setter)
        (match type
          [('& rtype)
           (make-embedded-struct-getter-setter cclass cclass-cname
                                               slot-name rtype
                                               c-field c-length c-init)]
          [('.array etype)
           (make-array-getter-setter cclass cclass-cname
                                     slot-name etype #f '<vector>
                                     c-field c-length c-init)]
          [('.array etype ':as scm-vector)
           (make-array-getter-setter cclass cclass-cname
                                     slot-name etype #f scm-vector
                                     c-field c-length c-init)]
          [('.array* etype)
           (make-array-getter-setter cclass cclass-cname
                                     slot-name etype #t '<vector>
                                     c-field c-length c-init)]
          [('.array* etype ':as scm-vector)
           (make-array-getter-setter cclass cclass-cname
                                     slot-name etype #t scm-vector
                                     c-field c-length c-init)]
          [_ (values type #t #t)])
      `(,(make <cslot>
           :cclass cclass :scheme-name slot-name :type (name->type type)
           :c-name (get-c-name "" c-field)
           :c-spec #f
           :getter getter :setter setter
           :init-cexpr c-init)))))

;; Returns ((slot-name type c-field c-length c-init) ...)
;; type can be (& type) for embedded types
(define (cstruct-grok-slot-specs cclass slots)
  ;; parse slot-name::type.  Returns slot name symbol and type symbol.
  (define (parse-symbol::type sym)
    (rxmatch-case (x->string sym)
      [#/^(.*?)(::(&)?(.*))?$/ (_ name-s _ embed type-s)
       (let* ([etype (if type-s (string->symbol type-s) '<top>)]
              [type (if (equal? embed "&") `(& ,etype) etype)])
         (values (string->symbol name-s) type))]
      [_ (error <cgen-stub-error> "bad slot name::type:" sym)]))
  ;; parse c-spec.  Returns c-field, c-length and init
  ;; cclass and slot-name are only for error message.
  (define (parse-c-spec slot-name c-spec)
    (rxmatch-case c-spec
      [#/^(\w+)?(?:\[(\w+)\])?(?:=(.*))?$/ (_ field length init)
          (values (or field (x->string slot-name))
                  (and length (or (string->number length) length))
                  init)]
      [else
       (errorf "Bad c-spec ~s for a slot ~s of ~s" c-spec slot-name
               (~ cclass'scheme-name))]))
  (define (grok-1 slots)
    (match slots
      [((? symbol? y) . rest)
       (if (#/::$/ (symbol->string y))
         (match rest
           [((and ((or '.array* '.array) etype . opts) type) (? string? c-spec) . rest)
            (receive (slot-name _) (parse-symbol::type y)
              (receive (c-field c-length c-init)
                  (parse-c-spec slot-name c-spec)
                (values (list slot-name type c-field c-length c-init) rest)))]
           [_ (error <cgen-stub-error> "bad slot spec in define-cstruct:"
                     (take* slots 2))])
         (match rest
           [((? string? c-spec) . rest)
            (receive (slot-name type) (parse-symbol::type y)
              (receive (c-field c-length c-init)
                  (parse-c-spec slot-name c-spec)
                (values (list slot-name type
                              (or c-field (x->string slot-name))
                              c-length c-init)
                        rest)))]
           [_  (receive (slot-name type) (parse-symbol::type y)
                 (values (list slot-name type (x->string slot-name) #f #f)
                         rest))]))]
      [_ (error <cgen-stub-error> "bad slot spec in define-cstruct:" slots)]))
  (let loop ([slots slots] [r '()])
    (if (null? slots)
      (reverse r)
      (receive (slot rest) (grok-1 slots)
        (loop rest (cons slot r))))))

;; Handle slot::(.array[*] <elt-type> [:as <scheme-vector>]) "c-name"
;;   c-name : "c-field[c-length]"
;;   cclass-cname is the C typename of the wrapper.
;;   By default, array field is extracted as a Scheme vector. You can give
;;   a uniform vector type by :as <scheme-vector> to treat it as uvector.
;;   If uvector type is specified, its element type must be compatible with
;;   <elt-type>.
;; Returns stub-type, getter-name, setter-name
;; TODO: Make c-length field read-only.
(define (make-array-getter-setter cclass cclass-cname
                                  slot-name elt-type-name ptr? scm-vector
                                  c-field c-length c-init)
  (define etype (name->type elt-type-name))
  (define (gen-getter c-field c-length) ; returns getter name
    (rlet1 getter-name #"~(~ cclass 'c-name)_~|c-field|_GET"
      (cgen-decl   #"static ScmObj ~|getter-name|(ScmObj);")
      ($ cgen-body $ cise-render-to-string
         `(define-cfn ,getter-name (obj_s) :static
            (let* ([obj :: (,cclass-cname *) (cast (,cclass-cname *) obj_s)]
                   [len::ssize_t 0])
              ;; TODO: We might have a pointer to an array of fixed size given as
              ;; a C macro.  Need to distinguish from the case of separate size
              ;; field.
              ,(if (and ptr? (string? c-length))
                 `(if (<= (ref (-> obj data) (C: ,c-length)) 0)
                    (return SCM_FALSE)
                    (set! len (ref (-> obj data) (C: ,c-length))))
                 `(set! len (C: ,c-length)))
              ,(if (eq? scm-vector '<vector>)
                 `(let* ([v (Scm_MakeVector len SCM_FALSE)]
                         [i::ScmSmallInt 0])
                    (for (() (< i len) (post++ i))
                      (let* ([e :: ,(~ etype'c-type)
                                (aref (ref (-> obj data) ,(string->symbol c-field)) i)])
                        (set! (SCM_VECTOR_ELEMENT v i)
                              (C: ,(cgen-box-expr etype "e")))))
                    (return (SCM_OBJ v)))
                 ;; TODO: Check type consistency
                 `(return (Scm_MakeU8VectorFromArray len (ref (-> obj data) ,(string->symbol c-field)))))))
         'toplevel)))
  (define (gen-setter c-field c-length)
    (rlet1 setter-name #"~(~ cclass 'c-name)_~|c-field|_SET"
      (cgen-decl   #"static void ~|setter-name|(ScmObj, ScmObj);")
      ($ cgen-body $ cise-render-to-string
         `(define-cfn ,setter-name (obj_s val) ::void :static
            (let* ([obj :: (,cclass-cname *) (cast (,cclass-cname *) obj_s)]
                   [vlen::ScmSmallInt 0]
                   [vs :: (,(~ etype 'c-type) *)])
              ,(if ptr?
                 `(when (SCM_FALSEP val)
                    (set! (ref (-> obj data) (C: ,c-length)) 0)
                    (set! (ref (-> obj data) (C: ,c-field)) NULL)
                    (return))
                 '())
              ,(if (eq? scm-vector '<vector>)
                 `(unless (SCM_VECTORP val) (SCM_TYPE_ERROR val "vector"))
                 `(unless (SCM_U8VECTORP val) (SCM_TYPE_ERROR val "u8vector")))
              (set! vlen (SCM_VECTOR_SIZE val))
              ,(if (not (and ptr? (string? c-length)))
                 `(unless (== vlen (C: ,c-length))
                    (Scm_Error ,#"Invalid length for ~|cclass-cname|.~|c-field|: %ld (must be ~c-length)\n" vlen))
                 '())
              ,(if ptr?
                 `(set! vs (SCM_NEW_ARRAY (C: ,(~ etype 'c-type)) vlen))
                 `(set! vs (ref (-> obj data) (C: ,c-field))))
              ,(if (eq? scm-vector '<vector>)
                 `(let* ([i::ScmSmallInt 0])
                    (for (() (< i vlen) (post++ i))
                      (let* ([val_i (SCM_VECTOR_ELEMENT val i)])
                        (unless (C: ,(cgen-pred-expr etype "val_i"))
                          (SCM_TYPE_ERROR val_i ,(x->string (~ etype'name))))
                        (set! (aref vs i) (C: ,(cgen-unbox-expr etype "val_i"))))))
                 `(memcpy vs (SCM_UVECTOR_ELEMENTS val) vlen))
              ,(if (and ptr? (string? c-length))
                 `(set! (ref (-> obj data) (C: ,c-length)) vlen)
                 '())
              ,(if ptr?
                 `(set! (ref (-> obj data) (C: ,c-field)) vs)
                 '())))
         'toplevel)))

  (cond [(eq? scm-vector '<vector>)]
        [(eq? scm-vector '<u8vector>)
         (unless (memq elt-type-name '(<uint8> <int8>))
           (errorf "Slot %S: Array element type (%A) and Scheme type (<u8vector>) don't match"
                   slot-name elt-type-name))]
        [else
         (errorf "Slot %S: Unsupported array type mapping: %S"
                 slot-name scm-vector)])
  (values scm-vector
          `(c ,(gen-getter c-field c-length))
          `(c ,(gen-setter c-field c-length))))

;; Handle slot::(& etype) "c-name"
;;   c-name : "c-field"
;;   cclass-cname is the C typename of the wrapper.
;; Returns stub-type, getter-name, setter-name
;; Whether the content of the embedded struct to be copied or not is
;; up to the boxer of the embedded type.  If the embedded type
;; is also a cstruct, the content is copied.  (That poses a problem when
;; the user want to modify its field.  We'll provide setters for
;; each individual subfields later, but the user code may want to carry
;; around the embedded structure for modification).
(define (make-embedded-struct-getter-setter cclass cclass-cname
                                            slot-name embedded-type-name
                                            c-field c-length c-init)
  (define etype (name->type embedded-type-name))
  (define (gen-getter c-field c-length) ; returns getter name
    (rlet1 getter-name #"~(~ cclass 'c-name)_~|c-field|_GET"
      (cgen-decl #"static ScmObj ~|getter-name|(ScmObj);")
      (cgen-body #"static ScmObj ~|getter-name|(ScmObj obj_s)"
                 #"{"
                 #"  ~|cclass-cname|* obj = (~|cclass-cname|*)obj_s;"
                 #"  ~(~ etype'c-type) e = &(obj->data.~|c-field|);"
                 #"  return SCM_OBJ(~(cgen-box-expr etype \"e\"));"
                 #"}")))
  (define (gen-setter c-field c-length)
    (rlet1 setter-name #"~(~ cclass 'c-name)_~|c-field|_SET"
      (cgen-decl #"static void ~|setter-name|(ScmObj, ScmObj);")
      (cgen-body #"static void ~|setter-name|(ScmObj obj_s, ScmObj val)"
                 #"{"
                 #"  ~|cclass-cname|* obj = (~|cclass-cname|*)obj_s;"
                 #"  if (!~(cgen-pred-expr etype \"val\")) {"
                 #"    SCM_TYPE_ERROR(val, \"~(~ etype'name)\");"
                 #"  }"
                 #"  ~(~ etype'c-type) e = ~(cgen-unbox-expr etype \"val\");"
                 ;; We need customizable copyer, but for now...
                 #"  obj->data.~|c-field| = *e;"
                 #"}")))

  (unless etype
    (error <cgen-stub-error> "unknown type can't be embedded:"
           embedded-type-name))
  (values embedded-type-name
          `(c ,(gen-getter c-field c-length))
          `(c ,(gen-setter c-field c-length))))

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
;;  qualifier : :private
;;  flag      : :keep-identity | :map-null
;;
;;  scm-name  : Scheme variable name; this will be bound to a newly-created
;;              subclass of <foreign-pointer> to represent this C-ptr type.
;;  c-type    : Name of the actual C type we wrap.
;;  c-name    : A C variable name (must have type ScmClass *).  If :private
;;              qualifier is given, the static definition of the variable is
;;              generated; otherwise, the definition & declaration must
;;              be provided elsewhere.  In initialization code, an instance
;;              of a class (the same one bound to scm-name in the Scheme
;;              world) will be stored in this C variable.
;;  c-pred    : A macro name to determine if ScmObj is this type.
;;  c-boxer   : A macro name to wrap C pointer and return ScmObj
;;  c-unboxer : A macro name to extract C pointer from ScmObj
;;
;;  If :private is given, c-pred, c-boxer and c-unboxer definitions are
;;  generated automatically.  Otherwise those definitions must be provided
;;  elsewhere.

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
       (check-clauses 'define-cptr scm-name clauses '(flags print cleanup))
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
           (make-cgen-type scm-name #f c-type
                           (x->string scm-name) ; description
                           (x->string c-pred)
                           (x->string c-unboxer)
                           (x->string c-boxer))
           (cgen-add! fptr)))]
      [_ (error <cgen-stub-error> "invalid define-cptr clause for" scm-name)])))

(define-method cgen-emit-body ((self <c-ptr>))
  (when (~ self'private)
    (let1 type (cgen-type-from-name (~ self'scheme-name))
      (p "static ScmClass *" (~ self'c-name) ";")
      (p "#define "(cgen-unboxer-name type)"(obj)"
         " SCM_FOREIGN_POINTER_REF("(~ self'c-type)", obj)")
      (p "#define "(cgen-pred-name type)"(obj)"
         " SCM_XTYPEP(obj, "(~ self'c-name)")")
      (p "#define "(cgen-boxer-name type)"(ptr)"
         " Scm_MakeForeignPointer("(~ self'c-name)", ptr)")))
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

;; deprecated
(define-form-parser if (test then . maybe-else)
  (cgen-with-cpp-condition test
    (cgen-stub-parse-form then))
  (unless (null? maybe-else)
    (cgen-with-cpp-condition `(not ,test)
      (cgen-stub-parse-form (car maybe-else)))))

(define-form-parser .if (test then . maybe-else)
  (cgen-with-cpp-condition test
    (cgen-stub-parse-form then))
  (unless (null? maybe-else)
    (cgen-with-cpp-condition `(not ,test)
      (cgen-stub-parse-form (car maybe-else)))))

(define-form-parser begin forms
  (for-each cgen-stub-parse-form forms))

;; deprecated
(define-form-parser when (test . forms)
  (cgen-with-cpp-condition test
    (for-each cgen-stub-parse-form forms)))

(define-form-parser .when (test . forms)
  (cgen-with-cpp-condition test
    (for-each cgen-stub-parse-form forms)))

(define-form-parser .unless (test . forms)
  (cgen-with-cpp-condition `(not ,test)
    (for-each cgen-stub-parse-form forms)))

(define-form-parser include (file)
  (unless (file-exists? file)
    ;; TODO: search path
    (error <cgen-stub-error> "couldn't find include file: " file))
  (with-input-from-file file
    (cut generator-for-each cgen-stub-parse-form read))
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
