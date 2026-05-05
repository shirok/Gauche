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
          default-ffi-subsystem
          ffi-subsystem-available?
          define-c-function
          <foreign-c-function>
          <foreign-c-callback>
          foreign-function-info)
  )
(select-module gauche.ffi)

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
;;
;;  You can also define a C function that can be called back from C
;;  program to evaluate Scheme expressions.
;;
;;    (define-c-callback <name> <arglist>  <rettype>
;;      <body> ...)
;;
;;  <arglist> is an expression that should yield the following form:
;;
;;     ((<var> <type>) ...)
;;
;;  where <var> is an identifier and, <type> is a value such that
;;  (native-type <type>) yields a native type.
;;
;;  <rettype> is also evaluated, and should yield a value such that
;;  (native-type <rettype>) yields a native type.
;;
;;  This binds a native handle with a function type to the <nane>.  The
;;  handle can be passed to a foreign function excpeting a function pointer.
;;

;;;
;;; <foreign-c-function> - parsed representation of a define-c-function form
;;;

;; Created by parse-define-c-function at macro-expansion time.
;; Backend macros receive a list of its instances.
;; 'Type' in arg-types and return-type should be either an instance
;; of <native-type> or <top>.

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
   (arg-vars     :init-keyword :arg-vars)     ; (symbol ...)
   (arg-types    :init-keyword :arg-types)    ; (<antive-type> ...)
   (return-type  :init-keyword :return-type)  ; native-type
   (body         :init-keyword :body)
   ))

;; Resolve a typespec to a <native-type> instance or <top> at runtime.
;; Reference to this procedure is inserted by macro expander.
;; A typespec is either a <native-type> instance (returned as-is), <top>,
;;  or a native-type signature.
(define (%resolve-typespec spec)
  (cond [(is-a? spec <native-type>) spec]
        [(eq? spec <top>) spec]
        [else (native-type spec)]))

;; Parse a define-c-function arg-types list.  The list may end with the
;; symbol '... to designate a variadic C function (the same convention as
;; make-c-function-type).  Returns two values:
;;   1. list of fixed arg type specs (for map %resolve-typespec)
;;   2. boolean: #t if variadic
(define (%parse-ffi-arg-types specs)
  (if (and (pair? specs) (eq? (last specs) '...))
    (values (drop-right specs 1) #t)
    (values specs #f)))

;; This is to convert FFI type info to S-expr for documentation purpose
;; (attached to foreign-function-info).  We convert <top> to ScmObj
;; for the time being.  It is kludge, as it can't be converted back
;; to <native-type>.
(define (%signature-type type)
  (if (eq? type <top>)
    'ScmObj
    (native-type->signature type)))

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
        (define cdefs '())
        (define subsystem
          (get-keyword :subsystem (unwrap-syntax options)
                       (default-ffi-subsystem)))
        (define ids (list (r'define-c-function)
                          (r'define-c-callback)))
        (define forms
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
             (quasirename r
               `(receive (arg-types* variadic?*)
                    (%parse-ffi-arg-types ,arg-types-expr)
                  (let ([atypes (map %resolve-typespec arg-types*)]
                        [rtype (%resolve-typespec ,rettype-expr)])
                    (make <foreign-c-function>
                      :scheme-name ',name
                      :c-name ,(cgen-safe-name-friendly (x->string name))
                      :arg-types atypes
                      :return-type rtype
                      :variadic? variadic?*
                      :tag-info `((foreign-function-tag
                                   :dlobj ,(~ ,dlo-var'path)
                                   :subsystem ,',subsystem
                                   :argtypes ,(map %signature-type atypes)
                                   :rettype ,(%signature-type rtype)))))))]))

        (define (make-ccb-expr ccb-form)
          (match ccb-form
            [(_ name arg-list-expr rettype-expr . body)
             (define body-name (gensym "c-callback-"))
             (quasirename r
               `(let* ([arg&types ,arg-list-expr]
                       [args (map car arg&types)]
                       [atypes (map ($ %resolve-typespec $ cadr $) arg&types)]
                       [rtype (%resolve-typespec ,rettype-expr)])
                  (make <foreign-c-callback>
                    :scheme-name ',name
                    :c-name ,(cgen-safe-name-friendly (x->string name))
                    :arg-vars args
                    :arg-types atypes
                    :return-type rtype
                    :body `(lambda ,args . ,',body))))]))

        (define (make-cdef-expr form)
          (ecase (car form) ; forms are already unwrapped
            [(define-c-function) (make-cfn-expr form)]
            [(define-c-callback) (make-ccb-expr form)]))

        ;; cfn-specs is ((name . cfn-expr) ...), where name is a symbol
        ;; name of cfn, and cfn-expr is (make <foreivn-c-function> ...)
        ;; constructed above.  The subsystem macro should rearrange
        ;; cfn-specs so that cfn-expr is evaluated in proper context.
        (define cdef-specs
          (map (^[cdef]
                 (cons (cadr cdef) ; name
                       (make-cdef-expr cdef))) ;expr
               (reverse cdefs)))

        ;; NB: with-stubgen-ffi should expand into definitions, so that
        ;; defined C functions (and other definitions) are visible
        ;; from the following expressions.  Be careful not to wrap
        ;; the expansion with let etc.
        (ecase subsystem
          [(:native)
           (quasirename r
             `(with-native-ffi ,dlo-var ,dlo-expr ,options ,cdef-specs ,forms))]
          [(:stubgen)
           (quasirename r
             `(with-stubgen-ffi ,dlo-var ,dlo-expr ,options ,cdef-specs ,forms))]
          )]))))
