;;;
;;; gauche.cgen.literal - static literal data
;;;
;;;   Copyright (c) 2004-2025  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.cgen.literal
  (use scheme.list)
  (use srfi.13)
  (use srfi.42)
  (use srfi.197)
  (use gauche.sequence)
  (use gauche.threads)
  (use gauche.cgen.unit)
  (use util.match)
  (export <cgen-literal> cgen-c-name cgen-cexpr cgen-make-literal
          cgen-literal-static?
          define-cgen-literal cgen-literal
          cgen-allocate-static-datum

          make-serializable-extended-pair

          ;; Deprecated - use cgen-safe-string
          c-safe-string-literal
          )
  )
(select-module gauche.cgen.literal)

;; <module> literal needs to access this
(autoload gauche.cgen.tmodule all-tmodules)

;;=============================================================
;; Static objects
;;

;; Many Scheme literals can be emitted as static C data.  So
;; we can delegate pointer adjustment task to system's ld.
;; Usually you don't need to touch this layer directly;
;; cgen-literal stuff (see "Scheme static values" below)
;; takes care of it.
;;
;; Static C data are categorized in two groups.  Constant structure,
;; which contains only statically determined pointers, and runtime
;; structure, which may contain slots that needs to be initialized
;; at runtime.  Each one is realized as static struct.  The constant
;; one is emitted first, so there's a constraint that the runtime one
;; can refer to the address of the member of the constant one, but not
;; vice versa.  (They're splitted so that each can be placed in
;; different sections of the compiled object).
;;
;; Each static data has to be 'allocated' before the code generation stage.
;; cgen-allocate-static-datum does the job.  It returns a string for C
;; expression representing a pointer to the C data.  Usually it may be
;; casted to SCM_OBJ.  You have to pass a thunk to generate initialization
;; code for the C variable.
;;
;; If the allocated data itself is a pointer type (e.g. ScmObj), there's
;; a possibility that the client uses its value, or the pointer to the
;; allocated data.  The latter only appears in the runtime structure,
;; since you can't use variable's value as a compile-time static constant.
;; You can tell cgen-allocate-static-datum that you're using value of
;; the allocated variable, by passing #f to init-thunk (the actual variable
;; is statically initialized by SCM_UNDEFINED (if c-type is ScmObj) or NULL
;; (otherwise).  It is the caller's responsibility to set appropriate value
;; within initialization stage.)
;;
;; Internal structure to register static data per type per category.
;; It is chained in <cgen-unit>'s static-data-list.
(define-class <cgen-static-data-list> ()
  ((category :init-keyword :category) ; 'constant or 'runtime
   (c-type :init-keyword :c-type)  ; symbol for C type, e.g. 'ScmObj
   (c-member-name :init-form (gensym "d")) ; member name in the C structure
   (count  :init-value 0)          ; # of allocated objs.
   (cpp-conditions :init-form (cgen-cpp-conditions)) ; cpp condition
   (init-thunks :init-value '())   ; thunks to generate initializers
                                   ;  constructed in rev order.
   ))

(define (static-data-c-struct-name category)
  (case category
    [(constant) "scm__sc"]
    [(runtime)  "scm__rc"]
    [else (error "[cgen internal] invalid category:" category)]))

(define (cgen-allocate-static-datum :optional (category 'runtime)
                                              (c-type   'ScmObj)
                                              (init-thunk #f))

  (define (ensure-static-data-list category c-type)
    (and-let* ([unit (cgen-current-unit)])
      (let* ([cppc (cgen-cpp-conditions)]
             [dl   (find (^[dl] (and (eq? (~ dl'c-type) c-type)
                                     (eq? (~ dl'category) category)
                                     (equal? (~ dl'cpp-conditions) cppc)))
                         (~ unit'static-data-list))])
        (or dl
            (rlet1 new (make <cgen-static-data-list>
                         :category category :c-type c-type)
              (when (eq? c-type 'ScmPair)
                ;; We need a sentinel at the beginning of ScmPair array
                ;; See ScmExtendedPair description in gauche/priv/pairP.h.
                (push! (~ new'init-thunks) "   { SCM_NIL, SCM_NIL }")
                (inc! (~ new'count)))
              (push! (~ unit'static-data-list) new))))))

  (let ([dl (ensure-static-data-list category c-type)]
        [value-type? (not init-thunk)]
        [ithunk (or init-thunk (if (eq? c-type 'ScmObj) "SCM_UNBOUND" "NULL"))])
    (let1 count (~ dl'count)
      (slot-push! dl'init-thunks ithunk)
      (inc! (~ dl'count))
      (if value-type?
        (format "~a.~a[~a]" ; no cast, for this'll be also used as lvalue.
                (static-data-c-struct-name category)
                (~ dl'c-member-name)
                count)
        (format "SCM_OBJ(&~a.~a[~a])"
                (static-data-c-struct-name category)
                (~ dl'c-member-name)
                count)))))

(define (cgen-allocate-static-array category c-type init-thunks)
  (fold (^[init-thunk seed]
          (let1 cexpr (cgen-allocate-static-datum category c-type init-thunk)
            (or seed cexpr)))
        #f init-thunks))

(define-method cgen-emit-static-data ((unit <cgen-unit>))

  (define (emit-one-category category dls)
    (let1 dls (filter (^[dl] (eq? (~ dl'category) category)) dls)
      (unless (null? dls)
        (emit-struct-def category dls)
        (print "{")
        (dolist [dl dls] (emit-initializers dl))
        (print "};"))))

  ;; NB: We add SCM_UNUSED in the static data, for it could be empty
  ;; depending on cpp conditions, and we don't want the compiler warn
  ;; about unused static var.
  (define (emit-struct-def category dls)
    (let1 name (static-data-c-struct-name category)
      (format #t "static ~astruct ~aRec {\n"
              (if (eq? category 'constant) "SCM_CGEN_CONST " "")
              name)
      (dolist [dl dls]
        (for-each (cut print "#if "<>) (reverse (~ dl'cpp-conditions)))
        (format #t "  ~a ~a[~a]~a;\n"
                (~ dl'c-type) (~ dl'c-member-name) (~ dl'count)
                (if (eq? (~ dl'c-type) 'ScmPair)
                  " SCM_ALIGN_PAIR"
                  ""))
        (for-each (cut print "#endif /*"<>"*/") (~ dl'cpp-conditions)))
      (format #t "} ~a SCM_UNUSED = " name)))

  (define (emit-initializers dl)
    (for-each (cut print "#if "<>) (reverse (~ dl'cpp-conditions)))
    (print "  {   /* "(~ dl'c-type)" "(~ dl'c-member-name)" */")
    (dolist [thunk (reverse (~ dl'init-thunks))]
      (if (string? thunk)
        (format #t "    ~a,\n" thunk)
        (begin (format #t "    ") (thunk) (print ","))))
    (print "  },")
    (for-each (cut print "#endif /*"<>"*/") (~ dl'cpp-conditions)))

  (and-let* ([dls (~ unit'static-data-list)])
    (unless (null? dls)
      ;; This piece of code is required, for Win32 DLL doesn't like
      ;; structures to be const if it contains SCM_CLASS_PTR.  Doh!
      (print "#if defined(__CYGWIN__) || defined(GAUCHE_WINDOWS)")
      (print "#define SCM_CGEN_CONST /*empty*/")
      (print "#else")
      (print "#define SCM_CGEN_CONST const")
      (print "#endif"))
    (emit-one-category 'constant dls)
    (emit-one-category 'runtime dls)
    ))

;;=============================================================
;; Scheme static values
;;
;;   The class family of <cgen-literal> is used to generate
;;   'static' Scheme values in C.  If possible, the Scheme object
;;   will be statically allocated.  Otherwise, a C variable is defined
;;   and the object is allocated in the initialization routine.
;;
;;   To arrange a Scheme object to be dumped in C, just pass it to
;;   cgen-literal.  It returns an instance of <cgen-literal>
;;   (or its subclass).   You can extract a C expression (ScmObj type)
;;   that refers to the Scheme object by cgen-cexpr.
;;
;;   Note that cgen-cexpr may return a C expression that is only
;;   available after initialization (e.g. for a Scheme symbol, cgen-cexpr
;;   returns a ScmObj variable which is set in initialize routine).
;;   You have to check the result of cgen-literal-static? if you use
;;   cgen-cexpr result in the static definition.
;;
;;   To define a scheme value, use define-cgen-literal macro.
;;
;;    (define-cgen-literal <literal-class> <scheme-class>
;;      (slot ...)
;;      method ...)
;;
;;   Methods:
;;      (make (arg) ...)   -> returns instance of <literal-class>
;;
;;      (cexpr (self) ...) -> returns C expression of the literal.
;;                            If omitted, the C variable name that
;;                            hols the object is returned.
;;
;;      (extern (self) ...)
;;      (decl (self) ...)
;;      (body (self) ...)
;;      (init (self) ...)  -> handle generation of each part.
;;                            If slot has <cgen-literal>, they
;;                            are traversed before these methods are
;;                            called.  Can be omitted.
;;
;;      (static (self) ...) -> used by cgen-literal-static?.
;;                            If omitted, #t is returned.
;;
;;

;; <cgen-literal> base class ----------------------------------

(define-class <cgen-literal> (<cgen-node>)
  ((scope  :init-keyword :scope  :init-value 'local)
   (c-name :init-keyword :c-name
           :init-form (format "scm__~a" (cgen-unique-name)))
   ;; C-NAME: the C expression that returns the pointer
   ;;  to this literal.  Hence it's #f for immediate literals.
   ;;  It may have a thunk that computes the expression.
   (value  :init-keyword :value :init-value #f)
   ;; VALUE: the Scheme value this literal represents.
   (comment :init-keyword :comment :init-value #f)
   ;; COMMENT: Auxiliary info (string) to be emitted into C source.
   ;; It is up to each literal class how to use this info.
   ;; #f if there's no comment.

   (inferred-literal-types :allocation :class :init-value '())
   ))

(define-method initialize ((node <cgen-literal>) initargs)
  (next-method)
  (when (~ node'c-name)
    (and-let* ([unit (cgen-current-unit)])
      (register-literal-value unit node)
      (slot-push! unit 'toplevels node))))

;; Fallback methods
;;
(define-method cgen-c-name ((node <cgen-literal>))
  (and-let* ([n (~ node'c-name)])
    (if (string? n) n (n))))

(define-method cgen-cexpr ((node <cgen-literal>))
  (cgen-c-name node))

(define-method cgen-make-literal (value)
  (or (infer-literal-handler value)
      (error "cannot make a static C data for Scheme value:" value)))

(define-method cgen-literal-static? (self) #t)

(define-method cgen-emit-xtrn ((node <cgen-literal>))
  (when (and (~ node'extern?) (cgen-c-name node))
    (print "extern ScmObj " (cgen-c-name node) ";")))

;; define-cgen-literal macro

(define-syntax define-cgen-literal
  (syntax-rules (make cexpr extern decl body init static)
    ;; loop for generating methods
    ((define-cgen-literal "methods" class scheme-class)
     #f)                                ;;end of loop
    ((define-cgen-literal "methods" class scheme-class
       (make (arg) . ?body) . rest)
     (begin
       (define-method cgen-make-literal ((arg scheme-class)) . ?body)
       (define-cgen-literal "methods" class scheme-class . rest)))
    ((define-cgen-literal "methods" class scheme-class
       (cexpr (self) . ?body) . rest)
     (begin
       (define-method cgen-cexpr ((self class)) . ?body)
       (define-cgen-literal "methods" class scheme-class . rest)))
    ((define-cgen-literal "methods" class scheme-class
       (extern (self) . ?body) . rest)
     (begin
       (define-method cgen-emit-xtrn ((self class)) . ?body)
       (define-cgen-literal "methods" class scheme-class . rest)))
    ((define-cgen-literal "methods" class scheme-class
       (decl (self) . ?body) . rest)
     (begin
       (define-method cgen-emit-decl ((self class)) . ?body)
       (define-cgen-literal "methods" class scheme-class . rest)))
    ((define-cgen-literal "methods" class scheme-class
       (body (self) . ?body) . rest)
     (begin
       (define-method cgen-emit-body ((self class)) . ?body)
       (define-cgen-literal "methods" class scheme-class . rest)))
    ((define-cgen-literal "methods" class scheme-class
       (init (self) . ?body) . rest)
     (begin
       (define-method cgen-emit-init ((self class)) . ?body)
       (define-cgen-literal "methods" class scheme-class . rest)))
    ((define-cgen-literal "methods" class scheme-class
       (static (self) . ?body) . rest)
     (begin
       (define-method cgen-literal-static? ((self class)) . ?body)
       (define-cgen-literal "methods" class scheme-class . rest)))
    ((define-cgen-literal "methods" class scheme-class
       _ . rest)
     (syntax-error "Unrecognized method clause in define-cgen-literal:" _))
    ;; Main entry
    ((define-cgen-literal class scheme-class slots . methods)
     (begin
       (define-class class (<cgen-literal>) slots)
       (define-cgen-literal "methods" class scheme-class . methods)))
    ;; Fallback
    ((define-cgen-literal . _)
     (syntax-error "malformed define-cgen-literal:" (define-cgen-literal . _)))
    ))

;; procedure cgen-literal returns a <cgen-literal> node for the
;; literal value of given Scheme value.  It first scans the current
;; unit's toplevel nodes with the same value, and returns it if found.
;; Otherwise, it creates a new node and register it to the toplevel if
;; necessary.
;;
;; The optional comment is attached to the created <cgen-literal>
;; (if comment is given, the literal instance won't be shared).
;; It is up to the literal class how to use the comment.

(define (cgen-literal value :optional (comment #f))
  (or (and-let* ([unit (cgen-current-unit)]
                 [ (not comment) ])
        (lookup-literal-value unit value))
      (rlet1 lit (cgen-make-literal value)
        (when comment (set! (~ lit 'comment) comment)))))

;; useful function to obtain initializer

(define (get-literal-initializer value)
  (if (cgen-literal-static? value)
    (cgen-cexpr value)
    "SCM_UNDEFINED"))

;; literal value management -----------------------------------

;; We want to share the same literals.  The criteria of this 'same' is
;; a bit complicated, and we need a customized comparator for thta.
;;
;; NOTE: We don't share literals when they have different cpp-conditions,
;; even if they are the same otherwise.  Theoretically we can share if one
;; has cpp-condition and the other doesn't, but tracking those dependencies
;; is just a headache.  We just assume such sharing rarely occurs.
;; This, the key of the hashtable consists of a pair of the literal value
;; and its cpp condition.

(define (%descriptive-type? obj)
  (is-a? obj <descriptive-type>))

(define (%deconstruct-type t)
  ((with-module gauche.internal deconstruct-type) t))

(define (literal-value-hash key)
  (define (rec val)
    (cond
     [(wrapped-identifier? val)
      (combine-hash-value (rec (~ val'name)) (rec (~ val'module)))]
     [(%descriptive-type? val)
      (fold combine-hash-value (rec (~ val'name))
            (map rec (%deconstruct-type val)))]
     [else (default-hash val)]))
  ;; for hashing, we ignore cpp condition
  (rec (car key)))

(define (literal-value=? x-key y-key)
  (and (equal? (cdr x-key) (cdr y-key)) ;cpp conditions
       (literal-value=?-rec (car x-key) (car y-key))))

;; NB: This should be a local function in literal-value=?, but 0.9.13 compiler
;; fails to lift it, adding overhead of closure allocation.  Since
;; literal-value=? is called many millions of times during compilation,
;; we hand-optmize it for now.  Put it back once we have a better compiler.
(define (literal-value=?-rec x y)
  (cond
   [(eqv? x y)]
   [(pair? x) (and (pair? y)
                   (literal-value=?-rec (car x) (car y))
                   (literal-value=?-rec (cdr x) (cdr y)))]
   [(vector? x)
    (and (vector? y)
         (let1 len (vector-length x)
           (and (= len (vector-length y))
                (let loop ([i 0])
                  (cond [(= i len) #t]
                        [(literal-value=?-rec (vector-ref x i) (vector-ref y i))
                         (loop (+ i 1))]
                        [else #f])))))]
   [(or (uvector? x) (string? x) (char-set? x) (regexp? x)
        (%descriptive-type? x))
    (equal? x y)]
   [(wrapped-identifier? x)
    (and (wrapped-identifier? y)
         (eq? (~ x'name) (~ y'name))
         (eq? (~ x'module) (~ y'module)))]
   [else #f]))


(define (ensure-literal-hash unit)
  (or (~ unit'literals)
      (rlet1 hash (make-hash-table (make-comparator (constantly #t)
                                                    literal-value=?
                                                    #f
                                                    literal-value-hash))
        (set! (~ unit'literals) hash))))

(define *uniq* (cons #f #f))

(define (register-literal-value unit literal-obj)
  (let ([h (ensure-literal-hash unit)]
        [key (cons (~ literal-obj'value)
                   (~ literal-obj'cpp-conditions))])
    (hash-table-update! h key
                        (^v (if (eq? v *uniq*)
                              literal-obj
                              v))
                        *uniq*)))

(define (lookup-literal-value unit val)
  (let ([h (ensure-literal-hash unit)]
        [key (cons val (cgen-cpp-conditions))])
    (hash-table-get h key #f)))

;; primitive values -------------------------------------------

;; boolean.  use predefined values.
(define-cgen-literal <cgen-scheme-boolean> <boolean>
  ()
  (make (value)
    (if value *cgen-scheme-true* *cgen-scheme-false*))
  (cexpr (self)
    (if (~ self'value) "SCM_TRUE" "SCM_FALSE")))

(define *cgen-scheme-true*
  (make <cgen-scheme-boolean> :c-name #f :value #t))
(define *cgen-scheme-false*
  (make <cgen-scheme-boolean> :c-name #f :value #f))

;; character.
(define-cgen-literal <cgen-scheme-char> <char>
  ()
  (make (value)
    (make <cgen-scheme-char> :c-name #f :value value))
  (cexpr (self)
    (format "SCM_MAKE_CHAR(~a)" (char->integer (~ self'value)))))

;; ()
(define-cgen-literal <cgen-scheme-null> <null>
  ()
  (make (value)
    (make <cgen-scheme-null> :c-name #f :value '()))
  (cexpr (self) "SCM_NIL"))

;; #<eof>
(define-cgen-literal <cgen-scheme-eof-object> <eof-object>
  ()
  (make (value)
    (make <cgen-scheme-eof-object> :c-name #f :value '()))
  (cexpr (self) "SCM_EOF"))

;; #<undef>
(define-cgen-literal <cgen-scheme-undefined-object> <undefined-object>
  ()
  (make (value)
    (make <cgen-scheme-undefined-object> :c-name #f :value '()))
  (cexpr (self) "SCM_UNDEFINED"))

;; string ------------------------------------------------------
;; (for now, we just deal with ASCII string w/o NUL.)

(define-cgen-literal <cgen-scheme-string> <string>
  ()
  (make (value)
    (make <cgen-scheme-string>
      :c-name (cgen-allocate-static-datum
               'constant 'ScmString
               (format "  SCM_STRING_CONST_INITIALIZER(~a, ~a, ~a)"
                       (cgen-safe-string value)
                       (string-length value) (string-size value)))
      :value value))
  )

;; For the backward compatibility
(define c-safe-string-literal cgen-safe-string)

;; symbol ------------------------------------------------------

(define-cgen-literal <cgen-scheme-symbol> <symbol>
  ((symbol-name :init-keyword :symbol-name)) ;; <cgen-scheme-string>
  (make (value)
    (make <cgen-scheme-symbol> :value value
          :c-name (cgen-allocate-static-datum)
          :symbol-name (cgen-literal (symbol->string value))))
  (init (self)
    (print "  " (cgen-c-name self)
           " = Scm_MakeSymbol(SCM_STRING("
           (cgen-cexpr (~ self'symbol-name))
           ")," (if (symbol-interned? (~ self'value)) "TRUE" "FALSE")");"
           " /* "(cgen-safe-comment (~ self'value))" */"))
  (static (self) #f)
  )

;; keyword ------------------------------------------------------

(define-cgen-literal <cgen-scheme-keyword> <keyword>
  ((keyword-name :init-keyword :keyword-name)) ;; <cgen-scheme-string>
  (make (value)
    (make <cgen-scheme-keyword> :value value
          :c-name (cgen-allocate-static-datum)
          :keyword-name (cgen-literal (keyword->string value))))
  (init (self)
    (print "  " (cgen-c-name self)
           " = Scm_MakeKeyword(SCM_STRING("
           (cgen-cexpr (~ self'keyword-name))
           ")); /* "(cgen-safe-comment (~ self'value))" */"))
  (static (self) #f)
  )

;; numbers -----------------------------------------------------

(define-cgen-literal <cgen-scheme-integer> <integer>
  ((string-rep :init-keyword :string-rep :init-value #f)
                                        ;; if value is too large to be C
                                        ;; literal, use string representation.
   )
  (make (value)
    (cond
     ;; We don't use fixnum?, since we may be cross-compiling on 64bit
     ;; machine for 32bit machine.  This is the range of 30bit fixnum.
     [(< (- (integer-expt 2 29)) value (- (integer-expt 2 29) 1))
      (make <cgen-scheme-integer> :value value :c-name #f)]
     ;; Integers that doesn't fit in 30bit fixnum but the literal number
     ;; can fit in C 32bit integers.  Note: We can use both signed and
     ;; unsigned literals, so the upper bound is 2^32-1, while
     ;; the lower bound is -2^31.
     [(< (- (integer-expt 2 31)) value (- (integer-expt 2 32) 1))
      (make <cgen-scheme-integer> :value value
            :c-name (cgen-allocate-static-datum))]
     [else
      (make <cgen-scheme-integer> :value value
            :c-name (cgen-allocate-static-datum)
            :string-rep (cgen-literal (number->string value 16)))]))
  (cexpr (self)
    (or (cgen-c-name self)
        (if (positive? (~ self'value))
          (format "SCM_MAKE_INT(~aU)" (~ self'value))
          (format "SCM_MAKE_INT(~a)" (~ self'value)))))
  (init (self)
    (when (cgen-c-name self)
      ;; Kludge: we just assume the machine's 'long' can hold at least
      ;; 32 bits.  The right thing may be to insert #ifdefs to check if
      ;; we can use 64bit literal, but we'll leave it for later revision.
      (let ([val   (~ self'value)]
            [cname (cgen-c-name self)])
        (cond [(< (- (integer-expt 2 31)) val 0)
               (print "  " cname " = Scm_MakeInteger("val");")]
              [(<= 0 val (- (integer-expt 2 32) 1))
               (print "  " cname " = Scm_MakeIntegerU("val"U);")]
              [else
               (print "  " cname " = Scm_StringToNumber(SCM_STRING("
                      (cgen-cexpr (~ self'string-rep))"), 16, 0);")]))))
  (static (self)
    (if (cgen-c-name self) #f #t))
  )

(define-cgen-literal <cgen-scheme-real> <real>
  ((numer :init-keyword :numer :init-value #f)
   (denom :init-keyword :denom :init-value #f))
  (make (value)
    (make <cgen-scheme-real> :value value
          :c-name (cgen-allocate-static-datum)
          :numer (and (exact? value) (cgen-make-literal (numerator value)))
          :denom (and (exact? value) (cgen-make-literal (denominator value)))))
  (cexpr (self) (cgen-c-name self))
  (init (self)
    (let1 v (~ self'value)
      (print "  "(cgen-c-name self)" = ")
      (cond
       [(exact? v)
        (print "Scm_MakeRational("(cgen-cexpr (~ self'numer))","(cgen-cexpr (~ self'denom))");")]
       [(finite? v)
        (print "Scm_MakeFlonum("v");")]
       [(nan? v)
        (print "SCM_NAN;")]
       [(infinite? v)
        (if (positive? v)
          (print "SCM_POSITIVE_INFINITY;")
          (print "SCM_NEGATIVE_INFINITY;"))]
       [else (error "don't know how to emit real value literal for " v)])))
  (static (self) #f))

(define-cgen-literal <cgen-scheme-complex> <complex>
  ()
  (make (value)
    (make <cgen-scheme-complex> :value value
          :c-name (cgen-allocate-static-datum)))
  (cexpr (self) (cgen-c-name self))
  (init (self)
    (let ([real (real-part (~ self'value))]
          [imag (imag-part (~ self'value))])
      (define (double-literal v)
        (cond [(finite? v) v]
              [(nan? v) "SCM_DBL_NAN"]
              [(positive? v) "SCM_DBL_POSITIVE_INFINITY"]
              [else "SCM_DBL_NEGATIVE_INFINITY"]))
      (print "  "(cgen-c-name self)" = "
             "Scm_MakeComplex("(double-literal real)", "(double-literal imag)");")))
  (static (self) #f))

;; pair ---------------------------------------------------------

;; NB: We don't serialize extended pairs automatically.  Pair attributes
;; can contain arbitrary information, some of which may be transient, and
;; serializing everything isn't usually what you want.

(define-cgen-literal <cgen-scheme-pair> <pair>
  ((car :init-keyword :car)
   (cdr :init-keyword :cdr))
  (make (value)
    (let* ([ca (cgen-literal (car value))]
           [cd (cgen-literal (cdr value))])
      (define (init-thunk)
        (format #t "   { ~a, ~a}"
                (get-literal-initializer ca)
                (get-literal-initializer cd)))
      (make <cgen-scheme-pair> :value value :car ca :cdr cd
            :c-name (cgen-allocate-static-datum 'runtime 'ScmPair
                                                init-thunk))))
  (init (self)
    (let1 cname (cgen-cexpr self)
      (unless (cgen-literal-static? (~ self'car))
        (format #t "  SCM_SET_CAR(~a, ~a);\n" cname (cgen-cexpr (~ self'car))))
      (unless (cgen-literal-static? (~ self'cdr))
        (format #t "  SCM_SET_CDR(~a, ~a);\n" cname (cgen-cexpr (~ self'cdr))))
      ))
  )

;; If you do need to serialize extended pair, replace it
;; with <serializable-extended-pair>, and set its attr slot
;; only with necessary info.

(define-class <serializable-extended-pair> ()
  ((car :init-keyword :car)
   (cdr :init-keyword :cdr)
   (attrs :init-keyword :attrs :init-value '())))

;; This always create serializable extended pair, even PAIR is just an
;; ordinary pair, so that you can call pair-attribute-set! afterwards.
(define (make-serializable-extended-pair ca cd attrs)
  (make <serializable-extended-pair> :car ca :cdr cd :attrs attrs))

(define-cgen-literal <cgen-scheme-extended-pair> <serializable-extended-pair>
  ((car :init-keyword :car)
   (cdr :init-keyword :cdr)
   (attrs :init-keyword :attrs))
  (make (value)
    (let* ([ca (cgen-literal (~ value'car))]
           [cd (cgen-literal (~ value'cdr))]
           [attrs (cgen-literal (~ value'attrs))])
      (make <cgen-scheme-extended-pair>
        :value value :car ca :cdr cd :attrs attrs
        :c-name (cgen-allocate-static-datum))))
  (static (self) #f)
  (init (self)
    (print "  " (cgen-c-name self)
           " = Scm_MakeExtendedPair("
           (cgen-cexpr (~ self'car)) ", "
           (cgen-cexpr (~ self'cdr)) ", "
           (cgen-cexpr (~ self'attrs)) ");"))
  )

;; vector -------------------------------------------------------

;; NB: ScmVector has variable part, so we can't directly initialize it.
;; We emit a vector as an array of ScmWord and cast it to ScmVector*
;; when using it.
(define-cgen-literal <cgen-scheme-vector> <vector>
  ((literals :init-keyword :literals) ;; list of cgen-literals for elements.
   )
  (make (value)
    (let* ([literals (map cgen-literal value)]
           [sobj (cgen-allocate-static-array
                  'runtime 'ScmObj
                  (list*
                   "SCM_OBJ(SCM_CLASS_STATIC_TAG(Scm_VectorClass)) /* <vector> */"
                   (format "SCM_VECTOR_SIZE_SLOT_INITIALIZER(~a, ~a)"
                           (length literals)
                           (if (vector-immutable? value) "TRUE" "FALSE"))
                   (map (^[lit] (if (cgen-literal-static? lit)
                                  (cgen-cexpr lit)
                                  "SCM_UNDEFINED"))
                        literals)))])
      (make <cgen-scheme-vector>
        :c-name sobj
        :value value
        :literals literals)))
  (init (self)
    (for-each-with-index
     (^[ind elt]
       (unless (cgen-literal-static? elt)
         (print "  ((ScmObj*)"(cgen-c-name self)")["(+ ind 2)"] = "(cgen-cexpr elt)";")))
     (~ self'literals)))
  )

;; NB: Because of building dependencies, we can't rely on gauche.uvector
;; module (since this file is used to precompile gauche.uvector).
;; The following code is carefully assembled to use only the built-in features.

(define-cgen-literal <cgen-scheme-uvector> <uvector>
  ((elements :init-keyword :elements))
  [make (value)
    (let1 elts (format "uvector__~a" (cgen-unique-name))
      (make <cgen-scheme-uvector>
        :value value :elements elts
        :c-name (cgen-allocate-static-datum
                 'runtime 'ScmUVector
                 (format "  SCM_UVECTOR_INITIALIZER(~a, ~a, ~a, ~a, NULL)"
                         (uvector-class->c-name (class-of value))
                         (uvector-length value)
                         elts
                         (if (uvector-immutable? value) 1 0)))))]
  [decl (self)
    (let* ([value (~ self'value)]
           [class (class-of value)])
      (and-let1 c (~ self'comment)
        (print "/* " (cgen-safe-comment c) " */"))
      (print "static "(uvector-class->c-type-name class)" "(~ self'elements)"[] = {")
      ;; Avoid using generic fold over uvector (value), for that would introduce
      ;; dependency to gauche.uvector.
      (do ([i 0 (+ i 1)]
           [column 0 (uvector-class-emit-elt class
                                             (uvector-ref value i)
                                             column)])
          [(= i (uvector-length value))]
        )
      (print "};"))]
  [static (self) #f]
  )

(define (uvector-class->tag-name class)
  (rxmatch->string #/<(.\d+)vector>/ (symbol->string (class-name class)) 1))

(define (uvector-class->c-name class)
  #"Scm_~(string-upcase (uvector-class->tag-name class))VectorClass")

(define (uvector-class->type-enum class)
  (module-binding-ref
   'gauche
   (string->symbol
    #"SCM_UVECTOR_~(string-upcase (uvector-class->tag-name class))")))

(define (uvector-class->c-type-name class)
  (cond
    [(eq? class <s8vector>) "signed char"]
    [(eq? class <u8vector>) "unsigned char"]
    [(eq? class <s16vector>) "short"]
    [(eq? class <u16vector>) "unsigned short"]
    [(eq? class <s32vector>) "int32_t"]
    [(eq? class <u32vector>) "uint32_t"]
    [(eq? class <s64vector>) "int64_t"]
    [(eq? class <u64vector>) "uint64_t"]
    [(eq? class <f16vector>) "ScmHalfFloat"]
    [(eq? class <f32vector>) "float"]
    [(eq? class <f64vector>) "double"]
    [(eq? class <c32vector>) "ScmHalfComplex"]
    [(eq? class <c64vector>) "ScmFloatComplex"]
    [(eq? class <c128vector>) "ScmDoubleComplex"]
    ))

;; Returns a new column number (0-based)
(define (uvector-class-emit-elt class v column)
  (define (fmt-half-float v)
    (format "0x~4,'0x" ((with-module gauche.internal flonum->f16bits) v)))
  (define (fmt-float v)
    (cond [(nan? v) "SCM_FLT_NAN"]
          [(infinite? v) (if (> v 0)
                           "SCM_FLT_POSITIVE_INFINITY"
                           "SCM_FLT_NEGATIVE_INFINITY")]
          [(negative-zero? v) "-0.0f"]
          [else (format "~sf" v)]))
  (define (fmt-double v)
    (cond [(nan? v) "SCM_DBL_NAN"]
          [(infinite? v) (if (> v 0)
                           "SCM_DBL_POSITIVE_INFINITY"
                           "SCM_DBL_NEGATIVE_INFINITY")]
          [(negative-zero? v) "-0.0"]
          [else (format "~s" v)]))
  (define (fmt column fmtstr . args)
    (let* ([s (apply format fmtstr args)]
           [len (string-length s)])
      (cond [(>= (+ column len) 70) (newline) (display s) len]
            [else (display " ") (display s) (+ column len 1)])))
  (define (prn column s)
    (unless (zero? column) (newline))
    (display s) (newline)
    0)
  (cond
   [(memq class `(,<s8vector> ,<s16vector> ,<s32vector>))
    (fmt column "~d," v)]
   [(memq class `(,<u8vector> ,<u16vector> ,<u32vector>))
    (fmt column "~du," v)]
   [(eq? class <s64vector>)
    (chain (prn column "#if SIZEOF_LONG == 8")
           (fmt _ " ~dl," v)
           (prn _ "#else  /*SIZEOF_LONG == 4*/")
           (fmt _ " (((int64_t)~dl << 32)|~dl),"
                (ash v -32) (logand v (- (integer-expt 2 32) 1)))
           (prn _ "#endif /*SIZEOF_LONG == 4*/"))]
   [(eq? class <u64vector>)
    (chain (prn column "#if SIZEOF_LONG == 8")
           (fmt _ " ~dlu," v)
           (prn _ "#else  /*SIZEOF_LONG == 4*/")
           (fmt _ " (((int64_t)~dlu << 32)|~dlu),"
                (ash v -32) (logand v (- (integer-expt 2 32) 1)))
           (prn _ "#endif /*SIZEOF_LONG == 4*/"))]
   [(eq? class <f16vector>) (fmt column "~a," (fmt-half-float v))]
   [(eq? class <f32vector>) (fmt column "~a," (fmt-float v))]
   [(eq? class <f64vector>) (fmt column "~a," (fmt-double v))]
   [(eq? class <c32vector>)
    (fmt column "{ ~a, ~a },"
         (fmt-half-float (real-part v))
         (fmt-half-float (imag-part v)))]
   [(eq? class <c64vector>)
    (fmt column "~a + ~a * _Complex_I,"
         (fmt-float (real-part v))
         (fmt-float (imag-part v)))]
   [(eq? class <c128vector>)
    (fmt column "~a + ~a * _Complex_I,"
         (fmt-double (real-part v))
         (fmt-double (imag-part v)))]
   ))

;; char-set -----------------------------------------------------

(define-cgen-literal <cgen-scheme-char-set> <char-set>
  ()
  (make (value)
    (make <cgen-scheme-char-set> :value value
          :c-name (cgen-allocate-static-datum)))
  (init (self)
    (print "  {")
    (print "     ScmCharSet *cs = SCM_CHARSET(Scm_MakeEmptyCharSet());")
    (dolist [range ((with-module gauche.internal %char-set-ranges)
                    (~ self'value))]
      (format #t "     Scm_CharSetAddRange(cs, SCM_CHAR(~a), SCM_CHAR(~a));\n"
              (car range) (cdr range)))
    (print "     "(cgen-c-name self)" = SCM_OBJ(cs);")
    (print "  }"))
  (static (self) #f))

;; regexp -------------------------------------------------------

(define-cgen-literal <cgen-scheme-regexp> <regexp>
  ((source-string :init-keyword :source-string)
   (case-fold?    :init-keyword :case-fold?))
  (make (value)
    (make <cgen-scheme-regexp> :value value
          :c-name (cgen-allocate-static-datum)
          :source-string (cgen-literal (regexp->string value))
          :case-fold? (regexp-case-fold? value)))
  (init (self)
    (format #t "  ~a = Scm_RegComp(SCM_STRING(~a), ~a);\n"
            (cgen-c-name self)
            (cgen-c-name (~ self'source-string))
            (if (~ self'case-fold?) "SCM_REGEXP_CASE_FOLD" "0")))
  (static (self) #f))

;; class --------------------------------------------------------

;; We can only dump classes that has globally bound name.
(define-cgen-literal <cgen-scheme-class> <class>
  ((defined-module :init-keyword :defined-module)
   (name :init-keyword :name))
  (make (value)
    (when (null? (~ value'defined-modules))
      (error "Can't precompile a class that doesn't have global binding:"
             value))
    (assume (module? (car (~ value'defined-modules))))
    (assume (symbol? (class-name value)))
    (make <cgen-scheme-class> :value value
          :c-name (cgen-allocate-static-datum)
          :name (cgen-literal (class-name value))
          :defined-module (cgen-literal (car (~ value'defined-modules)))))
  (init (self)
    (let ([m (gensym "mod")] [n (gensym "name")])
      (format #t "{\n  ScmObj ~a = SCM_OBJ(~a); ScmObj ~a = ~a;\n"
              m (cgen-c-name (~ self'defined-module))
              n (cgen-c-name (~ self'name)))
      (format #t "  SCM_ASSERT(SCM_MODULEP(~a));\n" m)
      (format #t "  SCM_ASSERT(SCM_SYMBOLP(~a));\n" n)
      (format #t "  ~a = Scm_GlobalVariableRef(SCM_MODULE(~a), SCM_SYMBOL(~a),0);\n"
              (cgen-c-name self) m n)
      (format #t "}\n")))
  (static (self) #f))

;; descriptive type ---------------------------------------------

(define-cgen-literal <cgen-scheme-proxy-type> <proxy-type>
  ((id :init-keyword :id))
  (make (value)
    (make <cgen-scheme-proxy-type> :value value
          :c-name (cgen-allocate-static-datum)
          :id ($ cgen-literal
                 $ (with-module gauche.internal proxy-type-id) value)))
  (init (self)
        (format #t "  ~a = Scm_MakeProxyType(SCM_IDENTIFIER(~a), NULL);"
                (cgen-c-name self)
                (cgen-c-name (~ self'id))))
  (static (self) #f))

(define-cgen-literal <cgen-scheme-descriptive-type> <descriptive-type>
  ((ctor :init-keyword :ctor)
   (args :init-keyword :args))
  (make (value)
    (make <cgen-scheme-descriptive-type> :value value
          :c-name (cgen-allocate-static-datum)
          :ctor (cgen-literal (class-of value))
          :args ($ cgen-literal
                   $ (with-module gauche.internal deconstruct-type) value)))
  (init (self)
        (format #t "  ~a = Scm_ConstructType(~a, ~a);"
                (cgen-c-name self)
                (cgen-c-name (~ self'ctor))
                (cgen-c-name (~ self'args))))
  (static (self) #f))

(define-cgen-literal <cgen-scheme-native-type> <native-type>
  ((name :init-keyword :name))
  (make (value)
    (make <cgen-scheme-native-type> :value value
          :c-name (cgen-allocate-static-datum)
          :name (cgen-literal (~ value'name))))
  (init (self)
        (format #t "  ~a = Scm_GlobalVariableRef(Scm_GaucheModule(), \
                                     SCM_SYMBOL(~a), 0);"
                (cgen-c-name self) (cgen-c-name (~ self'name)))
        (format #t "  if (SCM_UNBOUNDP(~a)) \
                             Scm_Error(\"Invalid native type: %S\", ~a);"
                (cgen-c-name self) (cgen-c-name (~ self'name))))
  (static (self) #f))

;; module -------------------------------------------------------

;; We recover modules at runtime.  Anonymous modules can be recovered
;; if they are used as a proxy of a real module (via <tmodule>).
(define-cgen-literal <cgen-module> <module>
  ((name  :init-keyword :name)
   (name-literal :init-keyword :name-literal))
  (make (value)
    (if-let1 n (predefined-module value)
      (make <cgen-module> :value value
            :c-name n
            :name (~ value'name))
      (let1 name (or (~ value'name)
                     (any (^[tm] (and (eq? (~ tm'module) value) (~ tm'name)))
                          (all-tmodules))
                     (error "Cannot emit literal for anonymous module:" value))
        (make <cgen-module> :value value
              :c-name (cgen-allocate-static-datum)
              :name name :name-literal (cgen-literal name)))))
  (init (self)
    (unless (predefined-module (~ self'value))
      ;; Module literals are emitted only related with macro hygiene.  In that
      ;; case, the module is created without loading its actual body, so
      ;; we mark it as 'placeholding'.  The placeholding module won't show
      ;; up in (all-modules).  Once (define-module ...) is evaluated by loading
      ;; the module, it becomes 'real' module.
      (format #t "  ~a = SCM_OBJ(Scm_FindModule(SCM_SYMBOL(~a), \
                         SCM_FIND_MODULE_CREATE|SCM_FIND_MODULE_PLACEHOLDING)); \
                  /* module ~a */\n"
              (~ self'c-name)
              (cgen-cexpr (~ self'name-literal))
              (~ self'name))))
  (static (self) (boolean (predefined-module (~ self'value))))
  )

(define (predefined-module module)
  (assq-ref `((,(find-module 'null)   . "Scm_NullModule()")
              (,(find-module 'scheme) . "Scm_SchemeModule()")
              (,(find-module 'gauche) . "Scm_GaucheModule()")
              (,(find-module 'gauche.internal) . "Scm_GaucheInternalModule()")
              )
            module))

;; identiifer ---------------------------------------------------

(define-cgen-literal <cgen-scheme-identifier> <identifier>
  ((id-name        :init-keyword :id-name)
   (module-literal :init-keyword :module-literal))
  (make (value)
    (unless ((with-module gauche.internal identifier-toplevel?) value)
      (error "identifier with compiler environment can't be compiled" value))
    (make <cgen-scheme-identifier> :value value
          :c-name (cgen-allocate-static-datum)
          :id-name (cgen-literal (unwrap-syntax value))
          :module-literal (cgen-literal (~ value'module))))
  (init (self)
    (let ([name (cgen-cexpr (~ self'id-name))]
          [cname (~ self'c-name)])
      (format #t "  ~a = Scm_MakeIdentifier(~a, SCM_MODULE(~a), \
                                            SCM_NIL); /* ~a#~a */\n"
              cname name (cgen-cexpr (~ self'module-literal))
              (cgen-safe-comment (~ self'module-literal'name))
              (cgen-safe-comment (~ self'id-name'value)))))
  (static (self) #f)
  )

;; box ----------------------------------------------------------

(define-cgen-literal <cgen-box> <box>
  ((vals           :init-keyword :vals))
  (make (value)
    (make <cgen-box> :value value
          :c-name (cgen-allocate-static-datum)
          :vals (map cgen-literal (values->list (unbox value)))))
  (init (self)
    (let ([c-vals (map cgen-cexpr (~ self'vals))]
          [cname (~ self'c-name)])
      (if (length=? c-vals 1)
        (format #t "  ~a = SCM_OBJ(Scm_MakeBox(~a));\n" cname (car c-vals))
        (format #T "  ~a = SCM_OBJ(Scm_ListToMVBox(~a));\n"
                cname (string-join c-vals ", ")))))
  (static (self) #f)
  )

;; mutex --------------------------------------------------------

;; Literal mutex is useful when a macro wants to insert a global lock.
(define-cgen-literal <cgen-mutex> <mutex>
  ((name           :init-keyword :name))
  (make (value)
    (make <cgen-mutex> :value value
          :c-name (cgen-allocate-static-datum)
          :name (cgen-literal (mutex-name value))))
  (init (self)
    (let ([name (cgen-cexpr (~ self'name))]
          [cname (~ self'c-name)])
      (format #t "  ~a = Scm_MakeMutex(~a);\n"
              cname name)))
  (static (self) #f)
  )

;;---------------------------------------------------------------
;; Inferring literal handlers.
;;  The code generator does not know how to generate literals for
;;  user-defined objects.  But we can do something if its read-time
;;  constructor is defined.

;;  NB: this routine writes value to a string and read it back (sans
;;  SRFI-10 prefix if any).   If the written out value contains *nested*
;;  SRFI-10 syntax, reader constructor of the inner srfi-10 is invoked
;;  in this process.  If the reader constructor have side effects, this
;;  routine may cause undesirable side effects as well.  There's no way
;;  we can know if it is safe.

(define-class <cgen-user-defined-type> (<cgen-literal>)
  ((ctor-name :init-keyword :ctor-name)
   (ctor-args :init-keyword :ctor-args)))

(define-method cgen-emit-init ((self <cgen-user-defined-type>))
  (print "{ ScmObj ctor = Scm_GetReaderCtor("(cgen-cexpr (~ self'ctor-name))","
         "SCM_FALSE);")
  (print "ScmObj args = "(cgen-cexpr (~ self'ctor-args))";")
  (print "if (SCM_FALSEP(ctor)) {")
  (print "  Scm_Error(\"Unknown reader constructor name: ~S\", "
         (cgen-cexpr (~ self'ctor-name))");")
  (print "}")
  (print "  "(cgen-c-name self)" = Scm_ApplyRec(SCM_CAR(ctor), args);")
  (print "}"))

(define-method cgen-literal-static? ((self <cgen-user-defined-type>)) #f)

(define (infer-literal-handler value)
  (and-let* ([s (guard (e [else #f]) (write-to-string value))]
             [m (#/#,\(/ s)]  ; SRFI-10 syntax
             [xpr (guard (e [else #f]) (read-from-string (string-drop s 2)))])
    (match xpr
      [(tag . args)
       (unless (%get-reader-ctor tag)
         (errorf "Reader constructor name '~s' is unknown \
                 to the code generator" tag))
       (make <cgen-user-defined-type>
         :value value
         :c-name (cgen-allocate-static-datum)
         :ctor-name (cgen-make-literal tag)
         :ctor-args (cgen-make-literal args))]
      [_ #f])))

;;=============================================================
;; Utilities
;;

(define cgen-unique-name
  (let1 counter 0
    (^[] (format "~5,'0d" (inc! counter)))))
