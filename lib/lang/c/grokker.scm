;;;
;;; C Semantic analysis
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

;;  Takes the output of c-parser and understands it.

(define-module lang.c.grokker
  (use util.match)
  (use lang.c.parser)
  (use lang.c.type)
  (export <c-context>
          c-grok-file
          c-grok-string

          c-variable-initial-value
          <c-unevaluatable-initializer>)
  )
(select-module lang.c.grokker)

(define-class <c-context> ()
  ((globals :init-form (make-hash-table 'eq?))
   ;; Global typedefs the parser has seen; symbol -> c-type.
   ;; Used to resolve type names in casts and sizeof.
   (typedefs :init-value #f)
   ;; Memo table for c-type->native-type, so that the same struct/union/enum
   ;; is converted to the same native type.
   (native-types :init-form (make-hash-table 'eq?))))

(define current-c-context (make-parameter (make <c-context>)))

(define (with-fresh-c-context thunk)
  (rlet1 ctx (make <c-context>)
    (parameterize ([current-c-context ctx]) (thunk))))

(define-class <c-global> ()
  ((name :init-keyword :name)               ;symbol
   (type :init-keyword :type)
   (storage-class :init-keyword :storage-class)
   (source-info :init-keyword :source-info)))

(define-method register-global! ((ctx <c-context>) (g <c-global>))
  (let* ([tab (~ ctx'globals)]
         [name (~ g'name)]
         [e (hash-table-get tab name #f)])
    (if e
      (begin
        ;; TODO: we also need to check type mistmatch
        (unless (or (eq? (~ e'storage-class) 'extern)
                    (eq? (~ g'storage-class) 'extern))
          (warn "~aC-context: Duplicate identifier: ~a (previously ~a)\n"
                (format-source-info (~ g'source-info))
                name
                (format-source-info (~ e'source-info))))
        ;; We keep non-"extern" version
        (unless (eq? (~ g'storage-class) 'extern)
          (hash-table-put! tab name g)))
      (hash-table-put! tab name g))))

(define-class <c-variable> (<c-global>)
  ((initializer :init-keyword :initializer)))

(define (add-variable name type storage-class initializer si)
  (rlet1 e (make <c-variable> :name name :type type :source-info si
                 :storage-class storage-class :initializer initializer)
    (register-global! (current-c-context) e)))

(define-class <c-function> (<c-global>)
  ((body :init-keyword :body)))

(define (add-function name type storage-class body si)
  (rlet1 e (make <c-function> :name name :type type :source-info si
                 :storage-class storage-class :body body)
    (register-global! (current-c-context) e)))

(define (format-source-info si)
  (match si
    [(file line) (format "~s:~d:" file line)]
    [_ ""]))

(define (c-grok-parser-output-1 form)
  (let1 si (c-source-info-of form)
    (match form
      [('decl identifier storage-class type init)
       (add-variable identifier type storage-class init si)]
      [('def identifier storage-class type body)
       (add-function identifier type storage-class body si)]
      [_ (error "[internal] Unknown parser output:" form)])))

;; TODO: Make parser returns lseq/cseq and run grokker concurrently

;; API
(define (c-grok-file file :key (cpp-include-paths '())
                               (cpp-definitions '()))
  (with-fresh-c-context
   (^[]
     (let1 parser (c-toplevel-parser :cpp-include-paths cpp-include-paths
                                     :cpp-definitions cpp-definitions)
       (dolist [form (c-parse-file parser file)]
         (c-grok-parser-output-1 form))
       (set! (~ (current-c-context)'typedefs) (~ parser'typedefs))))))

;; API
(define (c-grok-string string :key (cpp-include-paths '())
                                   (cpp-definitions '()))
  (with-fresh-c-context
   (^[]
     (let1 parser (c-toplevel-parser :cpp-include-paths cpp-include-paths
                                     :cpp-definitions cpp-definitions)
       (dolist [form (c-parse-string parser string)]
         (c-grok-parser-output-1 form))
       (set! (~ (current-c-context)'typedefs) (~ parser'typedefs))))))

;; API
;;   Look up definition or declaration
(define-method c-get-definition-or-declaration ((ctx <c-context>) name)
  (hash-table-get (~ ctx'globals) name #f))

;; Convert the string of C integer constant, as the lexer returns, to
;; an exact integer.  The suffix is already validated by the lexer.
;; Returns #f if the string isn't a valid integer constant.
(define (c-integer-literal->number s)
  (rxmatch-case s
    [#/^0[xX]([[:xdigit:]]+)[uUlL]*$/ (_ ds) (string->number ds 16)]
    [#/^0[bB]([01]+)[uUlL]*$/ (_ ds) (string->number ds 2)]
    [#/^(0[0-7]*)[uUlL]*$/ (_ ds) (string->number ds 8)]
    [#/^([1-9][0-9]*)[uUlL]*$/ (_ ds) (string->number ds 10)]
    [else #f]))

;; Convert the string of C floating constant to a flonum.
;; Returns #f if the string isn't a valid floating constant.
;; NB: Gauche's flonum is always double; we discard suffix information.
(define (c-floating-literal->number s)
  (rxmatch-case s
    [#/^0[xX]([[:xdigit:]]*\.?[[:xdigit:]]*[pP][-+]?[0-9]+)[fFlL]?$/ (_ ds)
     (string->number ds 16 'inexact)]
    [#/^([0-9]*\.?[0-9]*(?:[eE][-+]?[0-9]+)?)[fFlL]?$/ (_ ds)
     (string->number ds 10 'inexact)]
    [else #f]))

;;
;; Type information for evaluation
;;

;; Convert c-type to native type, using the memo table in CTX.
;; EVAL-CONST is to evaluate constant expressions in the type, e.g. array
;; dimensions; see c-type->native-type.
(define (c-context-native-type ctx type eval-const)
  (c-type->native-type type eval-const (~ ctx'native-types)))

;; Returns the size of c-type, or #f if we don't know it, or the type
;; is incomplete.
(define (c-type-size ctx type eval-const)
  (match (c-actual-type type)
    [('.function . _) #f]
    [('.array _ _ #f) #f]
    [('void _) #f]
    [((or '.struct '.union '.enum) _ _ #f) #f] ;incomplete
    [_ (and-let1 nt (c-context-native-type ctx type eval-const)
         (~ nt'size))]))

;; Wrap an exact integer V into BYTES-wide integer, as C conversion does.
(define (wrap-integer v bytes unsigned?)
  (let* ([bits (* bytes 8)]
         [m (logand v (- (ash 1 bits) 1))])
    (if (and (not unsigned?) (logbit? (- bits 1) m))
      (- m (ash 1 bits))
      m)))

;; Convert a value V to C TYPE, as cast does.  Returns #f if we can't.
;; NB: Conversion to float isn't rounded to single precision yet.
(define (c-convert-value ctx type v eval-const)
  (define (->integer bytes unsigned?)
    (cond [(exact-integer? v) (wrap-integer v bytes unsigned?)]
          [(and (real? v) (finite? v))
           (wrap-integer (exact (truncate v)) bytes unsigned?)]
          [else #f]))
  (define (->native-integer)
    (and-let1 nt (c-context-native-type ctx type eval-const)
      (->integer (~ nt'size) (~ nt'unsigned?))))
  (match (c-actual-type type)
    [('bool _) (if (zero? v) 0 1)]
    ;; We treat pointers as unsigned.
    [('.pointer . _) (and (exact-integer? v) (->integer (~ <void*>'size) #t))]
    [('.enum . _) (->native-integer)]
    [(? c-basic-type-integral?) (->native-integer)]
    [(? c-basic-type-flonum?) (and (real? v) (inexact v))]
    [((or 'float-complex 'double-complex 'long-double-complex) _) (inexact v)]
    [_ #f]))

;; Condition raised when an initializer can't be evaluated.
;;   initializer - the whole initializer expression being evaluated
;;   expression  - the offending part of the initializer
(define-condition-type <c-unevaluatable-initializer> <error> #f
  (initializer)
  (expression))

;; Evaluate initializer expression.  INITIALIZER is what the parser
;; returned in the initializer slot.
;; As the representation of parser output hasn't been finalized, the
;; implementation here is provisional.  The API remains same.
;; Raises <c-unevaluatable-initializer> if it can't be evaluated.
(define-method c-eval-initializer ((ctx <c-context>) initializer)
  ;; The operators return #f if they can't compute the value from
  ;; the operands; ev raises an error then.
  (define (unop op x) (op (ev x)))
  (define (binop op x y) (op (ev x) (ev y)))
  (define (nop op xs) (apply op (map ev xs)))
  ;; Wrap an operator that is only valid on integers.
  (define (int-op op) (^ vs (and (every exact-integer? vs) (apply op vs))))
  (define (rsh x y) (ash x (- y)))
  ;; C division truncates if both operands are integers.
  (define (cdiv x y)
    (if (and (exact? x) (exact? y))
      (and (not (zero? y)) (quotient x y))
      (/ (inexact x) (inexact y))))
  (define (cmod x y) (and (not (zero? y)) (remainder x y)))
  (define (cnot x) (if (zero? x) 1 0))
  (define (cmp op) (^[x y] (if (op x y) 1 0)))
  (define (cne x y) (if (= x y) 0 1))
  ;; Type name in cast and sizeof is (<specs> (#f <modifier> ...)).
  (define (type-name->c-type tn)
    (match tn
      [(specs (#f . mods))
       (guard (e [(<error> e) #f])
         (grok-c-type `(,@mods ,specs) (~ ctx'typedefs)))]
      [_ #f]))
  ;; Static type of an expression, used for sizeof.  We only handle
  ;; simple cases.
  (define (type-of expr)
    (match expr
      [('ident v) (and-let1 vv (c-get-definition-or-declaration ctx v)
                    (and (is-a? vv <c-variable>) (~ vv'type)))]
      [('paren x) (type-of x)]
      [('cast tn _) (type-name->c-type tn)]
      [('const 'char _) '(int ())]      ;character constant is int
      [('const (? symbol? t) _) `(,t ())]
      [_ #f]))
  (define (csizeof x)
    (and-let1 type (if (pair? (car x)) ; type name or expression
                     (type-name->c-type x)
                     (type-of x))
      (c-type-size ctx type ev)))
  ;; Character constant.  The lexer already decoded escape sequences.
  ;; Multi-character constants are implementation-defined, so we reject.
  (define (char-const s wide?)
    (and (= (string-length s) 1)
         (let1 code (char->integer (string-ref s 0))
           (cond [wide? code]
                 [(< code 256) (wrap-integer code 1 (~ <c-char>'unsigned?))]
                 [else #f]))))
  ;; && and || short-circuit, so the operands are evaluated in turn.
  (define (cand xs)
    (let loop ([xs xs])
      (cond [(null? xs) 1]
            [(zero? (ev (car xs))) 0]
            [else (loop (cdr xs))])))
  (define (cor xs)
    (let loop ([xs xs])
      (cond [(null? xs) 0]
            [(zero? (ev (car xs))) (loop (cdr xs))]
            [else 1])))
  (define (ev1 expr)
    (match expr
      [('const (or 'int 'u-int 'long 'u-long 'long-long 'u-long-long) s)
       (c-integer-literal->number s)]
      [('const (or 'float 'double 'long-double) s)
       (c-floating-literal->number s)]
      [('const 'char s) (char-const s #f)]
      [('const 'wchar s) (char-const s #t)]
      [('paren x) (ev x)]
      [('cast tn x) (and-let1 type (type-name->c-type tn)
                      (c-convert-value ctx type (ev x) ev))]
      [('sizeof x) (csizeof x)]
      [('+ a b) (binop + a b)]
      [('- a b) (binop - a b)]
      [('* a b) (binop * a b)]
      [('/ a b) (binop cdiv a b)]
      [('% a b) (binop (int-op cmod) a b)]
      [('<< a b) (binop (int-op ash) a b)]
      [('>> a b) (binop (int-op rsh) a b)]
      [('- a) (unop - a)]
      [('+ a) (ev a)]
      [('! a) (unop cnot a)]
      [('and . xs) (cand xs)]
      [('or . xs) (cor xs)]
      [('~ a) (unop (int-op lognot) a)]
      [('logand . xs) (nop (int-op logand) xs)]
      [('logior . xs) (nop (int-op logior) xs)]
      [('logxor . xs) (nop (int-op logxor) xs)]
      [('== a b) (binop (cmp =) a b)]
      [('!= a b) (binop cne a b)]
      [('< a b) (binop (cmp <) a b)]
      [('> a b) (binop (cmp >) a b)]
      [('<= a b) (binop (cmp <=) a b)]
      [('>= a b) (binop (cmp >=) a b)]
      [('?: c t e) (if (zero? (ev c)) (ev e) (ev t))]
      ;; Returns #f if V is undefined or has no initializer.
      [('ident v) (c-variable-initial-value ctx v)]
      [_ #f]))
  (define (ev expr)
    (or (ev1 expr)
        (error <c-unevaluatable-initializer>
               :initializer initializer :expression expr
               "Unevaluatable initializer:" expr)))

  (ev initializer))

;; API
;;   Lookup the initial value of the named variable.  Returns #f if
;;   the variable isn't defined or has no initializer.
;;   If the initializer can't be evaluated, the behavior depends on
;;   ON-FAILURE:
;;     :error    - Raises <c-unevaluatable-initializer>.
;;     #f        - Returns #f.
;;     procedure - Called with the condition, and its result is returned.
(define-method c-variable-initial-value ((ctx <c-context>) name
                                         :key (on-failure :error))
  (define (eval-init init)
    (if (eq? on-failure :error)
      (c-eval-initializer ctx init)
      (guard (e [(<c-unevaluatable-initializer> e)
                 (and on-failure (on-failure e))])
        (c-eval-initializer ctx init))))
  (unless (or (memv on-failure '(:error #f)) (procedure? on-failure))
    (error "on-failure must be :error, #f or a procedure, but got:"
           on-failure))
  (and-let* ([vv (c-get-definition-or-declaration ctx name)]
             [ (is-a? vv <c-variable>) ]
             [init (~ vv'initializer)])
    (eval-init init)))
