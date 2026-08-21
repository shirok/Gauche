;;;
;;; gauche.ffi - Foreign function interface
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

(define-module gauche.ffi
  (use util.match)
  (use gauche.native-type)
  (use gauche.cgen.unit :only (cgen-safe-name-friendly))
  (export with-ffi
          dlopen
          default-ffi-subsystem
          ffi-subsystem-available?
          define-c-function
          define-c-callback
          define-c-constant
          define-c-enum
          <foreign-c-function>
          <foreign-c-callback>
          <foreign-c-constant>
          <foreign-c-enum>
          foreign-function-info)
  )
(select-module gauche.ffi)

;; API
;;  dynamic-load provides necessary functionality, but this is
;;  convenient for FFI use.
(define (dlopen dsoname :key (paths (dynamic-load-paths))
                             (versions '()))
  (dynamic-load dsoname :paths paths :versions versions
                :error-if-not-found #t
                :init-function #f))

;; API
(define (foreign-function-info proc)
  (and (procedure? proc)
       (assq-ref ((with-module gauche.internal %procedure-tags-alist) proc)
                 'foreign-function-tag)))

;; FFI syntax
;;
;;  We'll have multiple FFI backends, but this high-level module
;;  hides the underlying implementation.
;;
;;  All FFI definitions must be enclosed by `with-ffi` form, which sets
;;  up the enviornment to define FFI functions
;;
;;   (with-ffi <dlobj> (<option> ...)
;;     <body> ...)
;;
;;  <dlobj> is an expression that yields #<dlobj>, e.g. call to 'dynamic-load'.
;;
;;  The list of <option>s is for future extension.  Currently it should be
;;  an empty list.  (:subsystem :native) selects the native FFI backend.
;;
;;  What follows are <body> just like let body.  In it, you can use
;;  define-c-function form at the toplevel.
;;
;;  Foreign function can be defined as follows:
;;
;;   (define-c-function <name>
;;     <arglist> <rettype>)
;;
;;  <name> is an identifier that must match the exported function name
;;  in the <dlobj>.
;;
;;  <arglist> and <rettype> are evaluated expressions.  <arglist> must yield
;;  a list of typespecs, and <rettype> must yield a typespec.  A typespec is
;;  either a native-type signature symbol/S-expr (resolved via native-type)
;;  or a <native-type> instance directly.
;;
;;   (define-c-function mylib-init '(int (.array c-string)) 'int)
;;
;;   (define-c-function mylib-init `(,<c-int> ,(make-c-array-type <c-string>)) <c-int>)
;;
;;  If <name> is prefixed with '%', it is removed to derive C function name.
;;  It allows to define a Scheme "wrapper" function on top of FFI C function
;;  with the sanem name (the wrapper function calls %-named FFI function).
;;
;;  You can also define a C function that can be called back from C
;;  program to evaluate Scheme expressions.
;;
;;    (define-c-callback <name> ((<var> <type>) ...) <rettype>
;;      <body> ...)
;;
;;  <arglist> must be a literal list of (<var> <type>) pairs.  <var> is
;;  an identifier visible in <body>; <type> is an expression evaluated
;;  at runtime that should yield a value such that (native-type <type>)
;;  yields a native type.
;;
;;  <rettype> is also evaluated, and should yield a value such that
;;  (native-type <rettype>) yields a native type.
;;
;;  This binds a native handle with a function type to <name>.  The
;;  handle can be passed to a foreign function expecting a function
;;  pointer.
;;
;;  A C constant---typically a #define'd value, but an enum member works
;;  as well---can be reified as a Scheme constant:
;;
;;    (define-c-constant <name> [<type>])
;;
;;  <name> is translated to the C identifier by the same rule as
;;  define-c-function.  The header that defines it must be listed in the
;;  :c-headers option of with-ffi.
;;
;;  <type> is an evaluated expression yielding a typespec, and tells how
;;  the C value is boxed.  It defaults to <fixnum>.
;;
;;  A C enum can be reified as a whole:
;;
;;    (define-c-enum <enum-set-name> (<name> ...) [<base-type>])
;;    (define-c-enum (<enum-set-name> <c-tag>) (<name> ...) [<base-type>])
;;
;;  Each <name> is bound as a Scheme constant, just as define-c-constant
;;  does, and <enum-set-name> is bound to the <c-enum> native type that
;;  collects them.  Use c-enum-value and c-enum-symbol to convert between
;;  the enumerator symbols and their values.
;;
;;  In the first form the C tag is derived from <enum-set-name> by the
;;  same name translation.  The second form gives the tag separately,
;;  either as a symbol, or as #f for an anonymous enum.
;;
;;  <base-type> is an evaluated expression yielding a typespec; it fixes
;;  the enum's size and alignment.  When omitted, they are derived from
;;  the enumerator values.
;;

;;;
;;; <foreign-c-function> - parsed representation of a define-c-function form
;;;

;; Created by parse-define-c-function at macro-expansion time.
;; Backend macros receive a list of its instances.
;; 'Type' in arg-types and return-type is an instance of <native-type>.

(define-class <foreign-c-function> ()
  ((scheme-name  :init-keyword :scheme-name)  ; symbol
   (c-name       :init-keyword :c-name)       ; string, C-safe function name
   (arg-types    :init-keyword :arg-types)    ; list of types (fixed args)
   (return-type  :init-keyword :return-type)  ; native-type
   (variadic?    :init-keyword :variadic?     ; #t when arg-types ends with '...
                 :init-value #f)
   (tag-info     :init-keyword :tag-info)     ; info to be tagged
   ))

(define-class <foreign-c-callback> ()
  ((scheme-name  :init-keyword :scheme-name)
   (c-name       :init-keyword :c-name)
   (body-name    :init-keyword :body-name)    ; symbol naming the Scheme proc
   (arg-vars     :init-keyword :arg-vars)     ; (symbol ...)
   (arg-types    :init-keyword :arg-types)    ; (<native-type> ...)
   (return-type  :init-keyword :return-type)  ; native-type
   ))

(define-class <foreign-c-constant> ()
  ((scheme-name  :init-keyword :scheme-name)  ; symbol
   (c-name       :init-keyword :c-name)       ; string, C-safe name
   (type         :init-keyword :type)         ; <native-type>
   ))

(define-class <foreign-c-enum> ()
  ((scheme-name  :init-keyword :scheme-name)  ; symbol, bound to the <c-enum>
   (tag          :init-keyword :tag)          ; symbol or #f (anonymous)
   (enumerators  :init-keyword :enumerators)  ; ((scheme-name . c-name) ...)
   (base-type    :init-keyword :base-type)    ; <native-type> or #f
   ))

;; Resolve a typespec to a <native-type> instance at runtime.
;; Reference to this procedure is inserted by macro expander.
;; A typespec is either a <native-type> instance (returned as-is), <top>
;;  (a synonym of <ScmObj>), or a native-type signature.
(define (%resolve-typespec spec)
  (cond [(is-a? spec <native-type>) spec]
        [(eq? spec <top>) <ScmObj>]
        [else (native-type spec)]))

;; Derive the C identifier from the Scheme name.  A leading '%' is
;; dropped, so that a Scheme wrapper can bear the unprefixed name.
(define (%ffi-c-name name)
  (cgen-safe-name-friendly (regexp-replace #/^%/ (x->string name) "")))

;; Parse a define-c-function arg-types list.  The list may end with the
;; symbol '... to designate a variadic C function (the same convention as
;; make-c-function-type).  Returns two values:
;;   1. list of fixed arg type specs (for map %resolve-typespec)
;;   2. boolean: #t if variadic
(define (%parse-ffi-arg-types specs)
  (if (and (pair? specs) (eq? (last specs) '...))
    (values (drop-right specs 1) #t)
    (values specs #f)))

;;;
;;; Susbsystem selection
;;;

;; API
(define (ffi-subsystem-available? kw)
  (case kw
    [(:stubgen) #t]
    [(:native) (boolean (#/^x86_64-.*/ (gauche-architecture)))]
    [else (error "Unrecognized FFI subsystem:" kw)]))

;; API
(define default-ffi-subsystem
  (make-parameter
   (if (ffi-subsystem-available? :native)
     :native
     :stubgen)))

;;;
;;; Syntax
;;;

(define-syntax define-c-function
  (syntax-rules ()
    [(_ . _)
     (syntax-error "define-c-function used outside with-ffi")]))

(define-syntax define-c-callback
  (syntax-rules ()
    [(_ . _)
     (syntax-error "define-c-callback used outside with-ffi")]))

(define-syntax define-c-constant
  (syntax-rules ()
    [(_ . _)
     (syntax-error "define-c-constant used outside with-ffi")]))

(define-syntax define-c-enum
  (syntax-rules ()
    [(_ . _)
     (syntax-error "define-c-enum used outside with-ffi")]))

(autoload gauche.ffi.stubgen (:macro with-stubgen-ffi))
(autoload gauche.ffi.native  (:macro with-native-ffi))
(autoload gauche.ffi.ffiaux  native-alloc native-free)

(define-syntax with-ffi
  (er-macro-transformer
   (^[f r c]
     (match f
       [(_ dlo-expr options . body)
        ;; Variable dlo-var is bound to the result of dlo-expr
        ;; in the expaneded with-*-ffi macros.
        (define dlo-var (gensym "dlo-"))
        ;; Chain define-c-function and define-c-callback
        (define cdefs '())
        ;; Chain scheme procedure definitions for c-callback body
        ;; procedure.
        ;;   (define <body-name> (lambda <vars> <body> ...))
        (define ccb-defines '())
        ;; (name body-name) per callback, in declaration order.  Threaded
        ;; through to with-native-ffi so it can pair each callback with
        ;; its body procedure when batching them into one codepad.
        (define ccb-info '())
        (define subsystem
          (get-keyword :subsystem (unwrap-syntax options)
                       (default-ffi-subsystem)))
        (define ids (list (r'define-c-function)
                          (r'define-c-callback)
                          (r'define-c-constant)
                          (r'define-c-enum)))
        ;; Forms other than C FFIs, callbacks, constants or enums
        (define extra-forms
          (filter-map
           (^[form]
             (if (and (pair? form)
                      (member (r (car form)) ids c)
                      (pair? (cdr form)))
               (begin
                 (push! cdefs (unwrap-syntax form))
                 #f)
               form))
           body))
        ;; For each define-c-function form, build a runtime
        ;; (make <foreign-c-function> ...) expression.
        ;; define-c-function arg-types may end with '... to mark a variadic
        ;; C function (same convention as make-c-function-type).
        ;; Example: (define-c-function printf '(c-string ...) 'int)
        (define (make-cfn-expr cfn-form)
          (match cfn-form
            [(_ name arg-types-expr rettype-expr)
             (define c-name (%ffi-c-name name))
             (quasirename r
               `(receive (arg-types* variadic?*)
                    (%parse-ffi-arg-types ,arg-types-expr)
                  (let ([atypes (map %resolve-typespec arg-types*)]
                        [rtype (%resolve-typespec ,rettype-expr)])
                    (make <foreign-c-function>
                      :scheme-name ',name
                      :c-name ',c-name
                      :arg-types atypes
                      :return-type rtype
                      :variadic? variadic?*
                      :tag-info `((foreign-function-tag
                                   :dlobj ,(~ ,dlo-var'path)
                                   :subsystem ,',subsystem
                                   :argtypes ,(map native-type->signature atypes)
                                   :rettype ,(native-type->signature rtype)))))))]))
        ;; For each define-c-callback form, build a runtime
        ;; (make <foreign-c-callback> ...) expression.
        ;; We also generate a definition of Scheme-side procedure
        ;; with the generated body-name.
        (define (make-ccb-expr ccb-form)
          (match ccb-form
            [(_ name ((vars type-exprs) ...) rettype-expr . body)
             ;; Use an interned symbol so the symbol the C stub interns via
             ;; SCM_INTERN matches the binding made by `define' below.
             (let ([body-name
                    (string->symbol
                     (symbol->string (gensym "%c-callback-body-")))])
               (unless (every symbol? vars)
                 (error "define-c-callback: arglist vars must be identifiers:"
                        vars))
               (push! ccb-defines
                      (quasirename r
                        `(define ,body-name (lambda ,vars ,@body))))
               (push! ccb-info (list name body-name))
               (quasirename r
                 `(let ([atypes (list ,@(map (^t (quasirename r
                                                   `(%resolve-typespec ,t)))
                                             type-exprs))]
                        [rtype (%resolve-typespec ,rettype-expr)])
                    (make <foreign-c-callback>
                      :scheme-name ',name
                      :c-name ,(cgen-safe-name-friendly (x->string name))
                      :body-name ',body-name
                      :arg-vars ',vars
                      :arg-types atypes
                      :return-type rtype))))]
            [_ (error "Malformed define-c-callback form:" ccb-form)]))

        ;; For each define-c-constant form, build a runtime
        ;; (make <foreign-c-constant> ...) expression.  The type is
        ;; optional and defaults to <fixnum>.
        ;; Example: (define-c-constant MAX-VALUE)
        (define (make-ccst-expr ccst-form)
          (match ccst-form
            [(_ name . type-expr?)
             (unless (symbol? name)
               (error "define-c-constant: name must be an identifier:" name))
             (let1 type-expr (match type-expr?
                               [() (quasirename r `<fixnum>)]
                               [(type-expr) type-expr]
                               [_ (error "Malformed define-c-constant form:"
                                         ccst-form)])
               (quasirename r
                 `(make <foreign-c-constant>
                    :scheme-name ',name
                    :c-name ',(%ffi-c-name name)
                    :type (%resolve-typespec ,type-expr))))]))

        ;; For each define-c-enum form, build a runtime
        ;; (make <foreign-c-enum> ...) expression.
        ;;   (define-c-enum <name> (<e> ...) [<base-type>])
        ;;   (define-c-enum (<name> <tag>) (<e> ...) [<base-type>])
        ;; When the tag isn't given, it is derived from <name> by the
        ;; same name translation as the enumerators.  An explicit tag is
        ;; used verbatim, and #f makes the enum anonymous.
        (define (make-cenum-expr cenum-form)
          (match cenum-form
            [(_ head (enumerators ...) . base-type?)
             (receive (name tag)
                 (match head
                   [(? symbol? name)
                    (values name (string->symbol (%ffi-c-name name)))]
                   [((? symbol? name) (? symbol? tag)) (values name tag)]
                   [((? symbol? name) #f) (values name #f)]
                   [_ (error "define-c-enum: malformed name and tag:" head)])
               (unless (every symbol? enumerators)
                 (error "define-c-enum: enumerators must be identifiers:"
                        enumerators))
               (let1 base-type-expr
                   (match base-type?
                     [() #f]
                     [(e) e]
                     [_ (error "Malformed define-c-enum form:" cenum-form)])
                 (quasirename r
                   `(make <foreign-c-enum>
                      :scheme-name ',name
                      :tag ',tag
                      :enumerators ',(map (^e (cons e (%ffi-c-name e)))
                                          enumerators)
                      :base-type ,(if base-type-expr
                                    (quasirename r
                                      `(%resolve-typespec ,base-type-expr))
                                    #f)))))]
            [_ (error "Malformed define-c-enum form:" cenum-form)]))

        (define (make-cdef-expr form)
          (ecase (car form) ; forms are already unwrapped
            [(define-c-function) (make-cfn-expr form)]
            [(define-c-callback) (make-ccb-expr form)]
            [(define-c-constant) (make-ccst-expr form)]
            [(define-c-enum)     (make-cenum-expr form)]))

        ;; The Scheme name a cdef form binds.  It is the second element
        ;; of the form, except that define-c-enum may carry the C tag
        ;; along with the name.
        (define (cdef-name form)
          (match form
            [('define-c-enum (name _) . _) name]
            [(_ name . _) name]))

        ;; cfn-specs is ((name . cfn-expr) ...), where name is a symbol
        ;; name of cfn, and cfn-expr is (make <foreivn-c-function> ...)
        ;; constructed above.  The subsystem macro should rearrange
        ;; cfn-specs so that cfn-expr is evaluated in proper context.
        (define cdef-specs
          (map (^[cdef]
                 (cons (cdef-name cdef)
                       (make-cdef-expr cdef))) ;expr
               (reverse cdefs)))

        ;; Names bound by define-c-enum forms, in declaration order.
        ;; with-stubgen-ffi needs them to bind each <c-enum> the stub
        ;; reifies.
        (define cenum-names
          (filter-map (^[cdef] (and (eq? (car cdef) 'define-c-enum)
                                    (cdef-name cdef)))
                      (reverse cdefs)))

        ;; Body forms with synthesized callback body definitions prepended.
        ;; The body lambdas need to be visible by the time the FFI binding
        ;; for <name> is invoked, but they don't need to precede the C stub
        ;; load (the stub looks them up lazily via SCM_BIND_PROC).
        (define final-forms (reverse ccb-defines extra-forms))

        ;; NB: with-*-ffi should expand into definitions, so that
        ;; defined C functions (and other definitions) are visible
        ;; from the following expressions.  Be careful not to wrap
        ;; the expansion with let etc.
        (ecase subsystem
          [(:native)
           ;; The native subsystem has no C compiler at hand, so it can't
           ;; reify C constants yet.
           (let1 cdef (find (^[cdef] (memq (car cdef)
                                           '(define-c-constant
                                             define-c-enum)))
                            cdefs)
             (when cdef
               (errorf "~a is not supported in the native ffi subsystem \
                        yet.  use stubgen ffi subsystem instead."
                       (car cdef))))
           (quasirename r
             `(with-native-ffi ,dlo-var ,dlo-expr ,options ,cdef-specs
                               ,(reverse ccb-info)
                               ,final-forms))]
          [(:stubgen)
           (quasirename r
             `(with-stubgen-ffi ,dlo-var ,dlo-expr ,options ,cdef-specs
                                ,cenum-names ,final-forms))]
          )]))))
