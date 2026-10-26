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
  (export <c-context>
          c-grok-file
          c-grok-string

          c-variable-initial-value
          <c-unevaluatable-initializer>)
  )
(select-module lang.c.grokker)

(define-class <c-context> ()
  ((globals :init-form (make-hash-table 'eq?))))

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
         (c-grok-parser-output-1 form))))))

;; API
(define (c-grok-string string :key (cpp-include-paths '())
                                   (cpp-definitions '()))
  (with-fresh-c-context
   (^[]
     (let1 parser (c-toplevel-parser :cpp-include-paths cpp-include-paths
                                     :cpp-definitions cpp-definitions)
       (dolist [form (c-parse-string parser string)]
         (c-grok-parser-output-1 form))))))

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
      [('paren x) (ev x)]
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
