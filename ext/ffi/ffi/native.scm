;;;
;;; gauche.ffi.native - Native FFI interface
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

;; Experimental.  Only available on selected platforms (OS/CPU)

(define-module gauche.ffi.native
  (extend gauche.ffi)
  (use gauche.native-type)
  (use util.match)
  (export with-native-ffi))
(select-module gauche.ffi.native)

(define %procedure-copy (with-module gauche.internal %procedure-copy))

;;;
;;; Main macro
;;;

;; (with-native-ffi dlo-var dlo-expr options cdef-specs forms)
;;
;; cdef-specs is ((name . expr) ...) where expr evaluates to a
;; <foreign-c-function>, <foreign-c-callback> or <foreign-c-constant>.
;;
;; The provisional (define name) bindings and the body lambdas are
;; emitted as begin-level forms.  Each define-c-enum's set name is bound
;; to its provisional <c-enum-type> by define-type, as the :stub subsystem
;; does.  Then constants and enums are computed by a single call of
;; %native-ffi-setup!, placed right after those bindings and before the
;; body forms.  All callbacks are batched into a
;; single ScmFFICallbackContext which is anchored in a hidden
;; module-level binding so the foreign side's invisible references
;; cannot dangle.
(define-syntax with-native-ffi
  (er-macro-transformer
   (^[f r c]
     ;; Kludge.  We need to ensure gauche.ffi.native is loaded
     ;; when precompiled ffi code is run.
     ;; https://github.com/shirok/Gauche/issues/1293
     (define %require. ((with-module gauche.internal make-identifier)
                        '%require (find-module 'gauche.internal) '()))
     (match f
       [(_ dlo-var dlo-expr options cdef-specs forms)
        ;; TODO: Support define-c-variable.
        (and-let1 spec (find (cut of-kind? <> :variable) cdef-specs)
          (error "define-c-variable isn't supported by the :native FFI \
                  subsystem yet; use (:subsystem :stub):" (~ spec'name)))
        (let* ([cfn-specs (filter (cut of-kind? <> :function) cdef-specs)]
               [ccb-specs (filter (cut of-kind? <> :callback) cdef-specs)]
               [cst-specs (filter (cut of-kind? <> :constant) cdef-specs)]
               [cen-specs (filter (cut of-kind? <> :enum) cdef-specs)]
               [ccb-body-names (map (cut ~ <>'body-name) ccb-specs)]
               [ctx-var   (gensym "%ffi-native-ctx-")]
               [instances-var (gensym "%ccbs-")])
          (define (emit-cfn-set! spec)
            (quasirename r
              `(set! ,(~ spec'name)
                     (make-native-ffi-proc ,dlo-var ,(~ spec'expr)))))
          (define (emit-ccb-block)
            (if (null? ccb-specs)
              #f
              (quasirename r
                `(let* ([,instances-var (list ,@(map (cut ~ <> 'expr)  ccb-specs))]
                        [bodies         (list ,@ccb-body-names)]
                        [ctx (install-callback-context! ,instances-var bodies)])
                   ,@(map (^[i s]
                            (quasirename r
                              `(set! ,(~ s'name)
                                     (callback-context-handle
                                      ctx ,i (list-ref ,instances-var ,i)))))
                          (iota (length ccb-specs)) ccb-specs)
                   ctx))))
          ;; Provisional bindings of the enumerators, to be filled by
          ;; %native-ffi-setup!.
          (define (emit-enumerator-defines)
            (append-map (^s (map (^n (quasirename r `(define ,n)))
                                 (~ s'enumerators)))
                        cen-specs))
          ;; Bind each enum-set name to its provisional <c-enum-type>.
          ;; The type is defined without enumerators; %native-ffi-setup!
          ;; fills them in.
          (define (emit-enum-type-defines)
            (map (^s (quasirename r
                       `(define-type ,(~ s'name) ,(~ s'enum-type-expr))))
                 cen-specs))
          ;; Constants and enums are computed at load time, right after the
          ;; provisional defines, so that the body forms can use them.
          ;; They don't need the dlobj.  The result is bound to a dummy
          ;; variable to keep the expansion definitions-only.
          ;; NB: cdef-specs come in reverse order of the source; we restore
          ;; it so that errors are reported in order.
          (define (emit-setup-define)
            (if (and (null? cst-specs) (null? cen-specs))
              '()
              (list
               (quasirename r
                 `(define ,(gensym "%ffi-native-setup-")
                    (%native-ffi-setup!
                     (current-module)
                     (list ,@(map (cut ~ <>'expr) (reverse cst-specs)))
                     (list ,@(map (cut ~ <>'expr) (reverse cen-specs)))
                     ',(get-keyword :c-headers options '())
                     ',(get-keyword :c-include-paths options '())))))))
          (quasirename r
            `(begin
               (,%require. "gauche/ffi/native")
               ,@(filter-map (^s (and (not (of-kind? s :enum))
                                      (quasirename r `(define ,(~ s'name)))))
                             cdef-specs)
               ,@(emit-enumerator-defines)
               ,@(emit-enum-type-defines)
               ,@(emit-setup-define)
               ,@forms
               (define ,ctx-var
                 (let ([,dlo-var ,dlo-expr])
                   ,@(map emit-cfn-set! cfn-specs)
                   ,(emit-ccb-block))))))]))))

;;;
;;; Type canonicalization
;;;

;; Convert a <native-type> instance to the call-sysvx64 canonical type:
;;   <void>      - void (return only)
;;   <double>    - double (xmm register, 64-bit float)
;;   <float>     - float  (xmm register, 32-bit float)
;;   <c-string>  - C string (Scheme string passed as const char*)
;;   <void*>     - raw pointer (native-handle or foreign-pointer)
;;   <uintptr_t> - unsigned integer (unsigned integral types)
;;   <intptr_t>  - signed integer (all other integral types)
(define (native-type->call-canon type)
  (cond
    [(or (is-a? type <c-pointer-type>)
         (is-a? type <c-function-type>)) type]
    ;; An array argument is treated the same way as a pointer.
    [(is-a? type <c-array-type>)
     (make-c-pointer-type (~ type 'element-type))]
    [(or (is-a? type <c-struct-type>)
         (is-a? type <c-union-type>))
     (error "Directly passing struct or union isn't supported yet")]
    [(is-a? type <native-type>)
     (cond
      [(eq? type <void>)             <void>]
      [(eq? type <double>)           <double>]
      [(eq? type <float>)            <float>]
      [(eq? type <c-string>)         <c-string>]
      [(eq? type <ScmObj>)           <top>]
      [(~ type 'unsigned?)           <uintptr_t>]
      [else                          <intptr_t>])]
    [else (error "Invalid type for native call:" type)]))

;; <c-char> object needs to be passed as integer
(define (native-type->arg-coerce type)
  (if (eq? type <c-char>) char->integer values))

(define (native-type->return-coerce type)
  (cond [(eq? type <c-char>) integer->char]
        [(and (subtype? type <integer>)
              (not (~ type 'unsigned?)))
         (%sign-extender (~ type 'size))]
        [else values]))

(define (%sign-extender bytes)
  (ecase bytes
    [(1) (^v (if (< v 128)
               v
               (logior v (lognot #xff))))]
    [(2) (^v (if (< v 32768)
               v
               (logior v (lognot #xffff))))]
    [(4) (^v (if (< v #x8000_0000)
               v
               (logior v (lognot #xffff_ffff))))]
    [(8) (^v (if (< v #x8000_0000_0000_0000)
               v
               (logior v (lognot #xffff_ffff_ffff_ffff))))]))

;;;
;;; Runtime procedure builder
;;;

;; Build a Scheme procedure that calls the C function described by cfn via
;; call-sysvx64.  Called at load time from the set! forms emitted by
;; with-native-ffi.
(define-method make-native-ffi-proc (dlo (cfn <foreign-c-function>))
  (let* ([native-call (cond-expand
                       [x86_64
                        (use lang.asm.x86_64)
                        (cond-expand
                         [gauche.os.windows
                          (with-module gauche.internal call-winx64)]
                         [else
                          (with-module gauche.internal call-sysvx64)])]
                       [else
                        (error "Native FFI is not supported on this platform")])]
         [ptr        (begin
                       (unless (is-a? dlo <dlobj>)
                         (errorf "FFI: ~a needs a C symbol, but with-ffi is \
                                  given #f instead of a dynamically loaded \
                                  object" (~ cfn'scheme-name)))
                       (dlobj-get-entry-address dlo #"_~(~ cfn'c-name)"))]
         [ret-type   (~ cfn'return-type)]
         [arg-types  (~ cfn'arg-types)]
         [variadic?  (~ cfn'variadic?)]
         [tag-info   (~ cfn'tag-info)]
         [nfixed     (length arg-types)]
         [ret-canon  (native-type->call-canon ret-type)]
         [arg-canons (map native-type->call-canon arg-types)]
         [arg-coerce (map native-type->arg-coerce arg-types)]
         [ret-coerce (native-type->return-coerce ret-type)])
    (unless ptr
      (error "FFI (native): cannot find function in library:"
             (~ cfn'scheme-name)))
    (let1 raw-proc
        (if variadic?
          ;; For variadic functions, map fixed args with known types, then infer
          ;; types for the remaining variadic args from their runtime values.
          ;; NB: For now, we just distinguish flonum values form the rest in
          ;; the varargs list.  Eventually we want to check if all the varargs
          ;; can be convertable to C objects.
          (^ args
             (let* ([fixed-args (take args nfixed)]
                    [var-args   (drop args nfixed)]
                    [fixed-pairs (map (^[canon coerce val] (list canon (coerce val)))
                                      arg-canons arg-coerce fixed-args)]
                    [var-pairs   (map (^[val]
                                        `(,(if (flonum? val) <double> <intptr_t>)
                                          ,val))
                                      var-args)])
               (parameterize
                   ([(with-module gauche.typeutil native-ptr-fill-enabled?) #t])
                 (ret-coerce
                  (native-call
                   ptr
                   (append fixed-pairs var-pairs)
                   ret-canon)))))
          ;; Non-variadic: map all args with their declared types.
          (^ args
            (parameterize
                ([(with-module gauche.typeutil native-ptr-fill-enabled?) #t])
              (ret-coerce
               (native-call
                ptr
                (map (^[canon coerce val] (list canon (coerce val)))
                     arg-canons arg-coerce args)
                ret-canon)))))
      (%procedure-copy raw-proc tag-info))))

;;;
;;; Constants
;;;
;;; define-c-constant is handled at load time.  For all the constants
;;; in one with-ffi form, we synthesize a single translation unit
;;;
;;;    #include <hdr1>
;;;    ...
;;;    ScmSmallInt Scm__FFI_constant_0 = CHAR_BIT;
;;;    const char* Scm__FFI_constant_1 = SOME_STRING;
;;;    ...
;;;
;;; grok it with lang.c, take the initial value of each dummy variable,
;;; and store it into the provisional binding, then make the binding
;;; constant.  This requires C preprocessor at load time.
;;;
;;; define-c-enum shares the same grok.  The enum-set name is bound to a
;;; provisional <c-enum-type> at expansion time, with the explicit base
;;; type or the default one, as the :stub subsystem does; its enumerators
;;; are filled from the grokked headers.

(autoload lang.c.grokker c-grok-string c-variable-initial-value
          c-get-definition-or-declaration c-get-tag-type
          c-context-native-type <c-enumerator>
          <c-grok-error> <c-unevaluatable-initializer>)
(autoload lang.c.parser c-parser-gauche-typedefs <c-parser-error>)

(define %find-binding (with-module gauche.internal find-binding))
(define %gloc-set! (with-module gauche.internal gloc-set!))
(define %gloc-make-constant! (with-module gauche.internal gloc-make-constant!))

;; Returns the C type text to declare the dummy variable for the
;; constant CCST.  The typedefs given by c-parser-gauche-typedefs cover
;; the non-basic type names, e.g. ScmSmallInt or u_long.
(define (%constant-c-type ccst)
  (define (unsupported what)
    (errorf "define-c-constant: ~a: ~a not supported"
            (~ ccst'scheme-name) what))
  (let1 type (~ ccst'type)
    (cond
     [(c-pointer-like-type? type)
      (unsupported "pointer, array and function types are")]
     ;; An enum is declared as its base type, as stubgen does.
     [(is-a? type <c-enum-type>)
      (~ (c-enum-type-base-type type)'c-type-name)]
     [(or (memq type (list <fixnum> <c-char> <c-bool> <c-string>
                           <float> <double> <longdouble>))
          (and (not (eq? type <ScmObj>))
               (not (eq? type <void>))
               (not (is-a? type <c-struct-type>))
               (not (is-a? type <c-union-type>))
               (subtype? type <integer>)))
      (~ type'c-type-name)]
     [else (unsupported (format "type ~a is" (~ type'name)))])))

;; Convert the value VAL, as c-variable-initial-value returns, to the
;; Scheme value of the constant CCST.  Raises an error if it isn't
;; appropriate for the constant's type.
(define (%convert-constant-value ccst val)
  (define type (~ ccst'type))
  (define (bad msg)
    (errorf "~a (C name: ~a) ~a, but got: ~s"
            (~ ccst'scheme-name) (~ ccst'c-name) msg val))
  (cond
   [(eq? type <c-string>)
    (if (string? val) val (bad "is not a string constant"))]
   [(string? val) (bad "is declared as a non-string type")]
   [(eq? type <c-bool>) (not (eqv? val 0))]
   [(eq? type <c-char>) (integer->char val)]
   [(eq? type <fixnum>)
    (if (fixnum? val) val (bad "doesn't fit in a fixnum"))]
   [else val]))

;; Translate a <c-unevaluatable-initializer> condition E, raised while
;; evaluating the dummy variable for CCST, to a message for the user.
;; If the offending expression is the constant's C name itself, cpp
;; didn't replace it, so it isn't a macro.  It's either not declared at
;; all, or declared as a function or a variable.  (An enumerator would
;; have been evaluated.)
;; Returns the message and a flag whether to suggest :subsystem :stub.
(define (%unevaluatable-constant-message ctx ccst e)
  (define names #"~(~ ccst'scheme-name) (C name: ~(~ ccst'c-name))")
  (match (~ e'expression)
    [('ident (? (cut equal? <> (string->symbol (~ ccst'c-name)))))
     (if (c-get-definition-or-declaration ctx (string->symbol (~ ccst'c-name)))
       (values #"~names is not a constant" #f)
       (values #"~names is not found in the headers given by :c-headers"
               #f))]
    ;; A string value can't be converted to a non-string type.
    [(? string?)
     (values #"~names is a string constant; declare its type as c-string"
             #f)]
    [_ (values #"Can't evaluate the value of ~|names|: ~(condition-message e)"
               #t)]))

;; Set the value of the provisionally defined binding NAME in MOD, and
;; make it constant.  WHAT is used in error messages.
(define (%bind-native-constant! mod name val what)
  (let1 g (%find-binding mod name #f)
    (unless g
      (errorf "[internal] ~a: provisional binding of ~a isn't found in ~s"
              what name mod))
    (%gloc-set! g val)
    (%gloc-make-constant! g)))

;; Look up the enum of CEN (<foreign-c-enum>) in the grokked context CTX.
;; Returns the alist of (<scheme-name> <value>) of CEN's enumerators
;; in its order, suitable to pass to c-enum-type-complete!.
;; FAIL! is called with a message for each problem; returns #f if there's
;; any.  HINT! is called if we'd better suggest :subsystem :stub.
;;
;; Unlike the :stub subsystem, the enum is identified by its tag (or, for
;; an anonymous enum, by its first enumerator), and each name must be an
;; enumerator of that very enum; a macro or an enumerator of another enum
;; isn't accepted.
(define (%native-enum-values ctx cen fail! hint!)
  (define tag (~ cen'tag))
  (define enumerators (~ cen'enumerators)) ; ((scheme-name . c-name) ...)
  (define what (if tag #"enum ~tag" "the anonymous enum"))
  (define (names e) #"~(car e) (C name: ~(cdr e))")
  (define (lookup-global e)
    (c-get-definition-or-declaration ctx (string->symbol (cdr e))))
  ;; Returns the c-type of the enum, or #f
  (define (find-enum-type)
    (cond
     [tag
      (match (c-get-tag-type ctx tag)
        [#f (fail! #"~what is not found in the headers given by :c-headers")
            #f]
        [('.enum _ _ #f . _)
         (fail! #"~what is declared but not defined in the headers")
         #f]
        [(and ('.enum . _) ctype) ctype]
        [(kind . _)
         (fail! (format "~a is not found in the headers given by \
                         :c-headers, but the tag is used for ~a"
                        what (if (eq? kind '.struct) "a struct" "a union")))
         #f])]
     [(null? enumerators)
      (fail! "an anonymous enum needs at least one enumerator to be located")
      #f]
     [else
      (let1 g (lookup-global (car enumerators))
        (if (is-a? g <c-enumerator>)
          (~ g'type)
          (begin
            (fail! #"~(names (car enumerators)) is not an enumerator \
                     in the headers given by :c-headers")
            #f)))]))
  (define (enum-alist ctype)
    (guard (e [(<c-unevaluatable-initializer> e)
               (hint!)
               (fail! #"Can't evaluate the enumerators of ~|what|: \
                        ~(condition-message e)")
               #f])
      (and-let1 nt (c-context-native-type ctx ctype)
        (c-enum-type-enumerator-alist nt))))
  (and-let* ([ctype (find-enum-type)]
             [alist (enum-alist ctype)])
    (let1 vals
        (map (^e (if-let1 p (assq (string->symbol (cdr e)) alist)
                   (list (car e) (cdr p))
                   (let1 g (lookup-global e)
                     (fail! (cond
                             [(not g)
                              #"~(names e) is not found as an enumerator \
                                in the headers given by :c-headers"]
                             [(is-a? g <c-enumerator>)
                              #"~(names e) is not an enumerator of ~what, \
                                but of another enum"]
                             [else #"~(names e) is not an enumerator"]))
                     #f)))
             enumerators)
      (and (every identity vals) vals))))

;; Called at load time from the expansion of with-native-ffi.
;; MOD is the module where the provisional bindings live.  CCST-INSTANCES
;; is a list of <foreign-c-constant>, and CENUM-INSTANCES is a list of
;; <foreign-c-enum>.  The headers are grokked once for all of them.
;; Returns the list of the defined names.
(define (%native-ffi-setup! mod ccst-instances cenum-instances
                            c-headers c-include-paths)
  (define c-types (map %constant-c-type ccst-instances))
  (define dummy-names
    (map (^i (string->symbol #"Scm__FFI_constant_~i"))
         (iota (length ccst-instances))))
  (define src
    (with-output-to-string
      (^[]
        (dolist [hdr c-headers]
          (print #"#include <~|hdr|>"))
        (for-each (^[ccst ctype dname]
                    (print #"~|ctype| ~|dname| = ~(~ ccst'c-name);"))
                  ccst-instances c-types dummy-names))))
  (define stub-hint
    "If lang.c can't handle it, try :subsystem :stub in with-ffi.")
  ;; Prefix of error messages that aren't specific to a definition.
  (define form-name
    (cond [(null? cenum-instances) "define-c-constant"]
          [(null? ccst-instances) "define-c-enum"]
          [else "with-ffi"]))
  (define ctx
    (guard (e [(<c-grok-error> e)
               ;; If cpp fails, :stub won't help, so we suggest it only
               ;; when lang.c fails to parse the headers.
               (errorf "~a: Can't process the headers given \
                        by :c-headers ~s: ~a~a"
                       form-name c-headers (condition-message e)
                       (if (condition-has-type? e <c-parser-error>)
                         #"\n~stub-hint"
                         ""))])
      (c-grok-string src
                     :typedefs (c-parser-gauche-typedefs)
                     :cpp-include-paths c-include-paths)))
  ;; We collect all the failures and report them at once.
  (define failures '())                 ; list of (form-name . message)
  (define suggest-stub? #f)
  (define (hint!) (set! suggest-stub? #t))
  (define (unevaluatable! ccst e)
    (receive (msg hint?) (%unevaluatable-constant-message ctx ccst e)
      (when hint? (hint!))
      (push! failures (cons "define-c-constant" msg))
      #f))
  ;; NB: We don't use map, for the failures should be in order.
  (define cst-vals
    (let loop ([ccsts ccst-instances] [dnames dummy-names] [r '()])
      (if (null? ccsts)
        (reverse r)
        (let* ([ccst (car ccsts)]
               [v (c-variable-initial-value ctx (car dnames)
                                            :on-failure (cut unevaluatable!
                                                             ccst <>))]
               [cv (and v
                        (guard (e [(error? e)
                                   (push! failures
                                          (cons "define-c-constant"
                                                (condition-message e)))
                                   #f])
                          (list (%convert-constant-value ccst v))))])
          (loop (cdr ccsts) (cdr dnames) (cons cv r))))))
  ;; For each enum, the list of (scheme-name value), or #f on failure.
  ;; The provisional <c-enum-type> is completed here; it checks that the
  ;; values fit in the base type.
  (define enum-vals
    (let loop ([cens cenum-instances] [r '()])
      (if (null? cens)
        (reverse r)
        (let* ([cen (car cens)]
               [prefix #"define-c-enum ~(~ cen'scheme-name)"]
               [fail! (^[msg] (push! failures (cons prefix msg)))]
               [vals (%native-enum-values ctx cen fail! hint!)]
               [vals (and vals
                          (guard (e [(error? e)
                                     (fail! (condition-message e)) #f])
                            (c-enum-type-complete! (~ cen'type) vals
                                                   (~ cen'scheme-name))
                            vals))])
          (loop (cdr cens) (cons vals r))))))
  (unless (null? failures)
    (let* ([fs (reverse failures)]
           [prefixes (delete-duplicates (map car fs))])
      (errorf "~a: ~a~a"
              (if (length=? prefixes 1) (car prefixes) form-name)
              (cond [(length=? fs 1) (cdar fs)]
                    [(length=? prefixes 1)
                     (format "~a errors:~a" (length fs)
                             (string-join (map cdr fs) "\n  " 'prefix))]
                    [else
                     (format "~a errors:~a" (length fs)
                             (string-join (map (^f #"~(car f): ~(cdr f)") fs)
                                          "\n  " 'prefix))])
              (if suggest-stub? #"\n~stub-hint" ""))))
  (for-each (^[ccst v]
              (%bind-native-constant! mod (~ ccst'scheme-name) (car v)
                                      "define-c-constant"))
            ccst-instances cst-vals)
  (for-each (^[cen vals]
              (dolist [v vals]
                (%bind-native-constant! mod (car v) (cadr v)
                                        "define-c-enum")))
            cenum-instances enum-vals)
  (append (map (cut ~ <>'scheme-name) ccst-instances)
          (append-map (^[vals] (map car vals)) enum-vals)))

;; Callbacks no longer flow through make-native-ffi-proc; with-native-ffi
;; batches them into a single ScmFFICallbackContext.  See the helpers
;; below.

;;;
;;; Callback context construction
;;;
;;; with-native-ffi emits code that calls install-callback-context! once
;;; per with-ffi block to install all of that block's callbacks in a
;;; single mmapped region, then calls callback-context-handle per
;;; callback to wrap each entry as a <native-handle>.
;;;
;;; The context returned by install-callback-context! is held by a
;;; hidden module-level binding; this is what keeps the mapped pages
;;; alive for the program lifetime, even though foreign code may have
;;; recorded the trampoline addresses where the GC cannot see them.

(define %%install-ffi-callback-context
  (with-module gauche.internal %%install-ffi-callback-context))
(define %%ffi-callback-context-entry
  (with-module gauche.internal %%ffi-callback-context-entry))

;; Platform-dispatched entry into the trampoline assembler.  Takes the
;; body procedure plus canonical arg/ret types and returns four values:
;;   (values code-bytes entry-offset win-prolog-end win-frame-size)
(define assemble-callback
  (cond-expand
   [x86_64
    (cond-expand
     [gauche.os.windows
      (with-module gauche.internal assemble-callback-winx64)]
     [else
      (with-module gauche.internal assemble-callback-sysvx64)])]
   [else
    (^ _ (error "Native FFI callbacks not supported on this platform"))]))

(define (install-callback-context! ccb-instances body-procs)
  (let1 specs
      (map (^[ccb body]
             (let* ([arg-canons (map native-type->call-canon
                                     (~ ccb'arg-types))]
                    [ret-canon  (native-type->call-canon
                                 (~ ccb'return-type))])
               (receive (code entry win-pe win-fs)
                   (assemble-callback body arg-canons ret-canon)
                 (list code entry win-pe win-fs))))
           ccb-instances body-procs)
    (%%install-ffi-callback-context specs)))

(define (callback-context-handle ctx i ccb)
  (%%ffi-callback-context-entry
   ctx i (make-c-function-type (~ ccb'return-type) (~ ccb'arg-types))))
