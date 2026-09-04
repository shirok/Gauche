;;;
;;; compile-t.scm - Type handling during compilation
;;;
;;;   Copyright (c) 2021-2025  Shiro Kawai  <shiro@acm.org>
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

;; Descriptive types should be created during compilation.

;; These identifiers are recognized in type constructor expressions.
;; (We don't use the convention adding '.' after the name, for we have
;  a procedure named '*.'.
(define id:-> (global-id '->))
(define id:*  (global-id '*))

(define-in-module gauche -> (undefined)) ; need to be bound for identifier match

;; Ensure the result of pass1 is a type, or returns #f.
;; Called from pass1/type-expression to compile a type expression.
;; Always returns a ($const TYPE) if not #f.
;; Classes are wrapped with proxy-type.
(define (type/ensure iform cenv)
  (or (and-let* ([ ($const? iform) ]
                 [v ($const-value iform) ]
                 [ (is-a? v <type>) ])
        v)
      (and-let* ([ (has-tag? iform $GREF) ]
                 [gloc (gref-inlinable-gloc iform)]
                 [v (gloc-ref gloc)])
        (cond [(is-a? v <class>) (wrap-with-proxy-type ($gref-id iform) gloc)]
              [(is-a? v <type>) v]
              [else #f]))))

;; Called from pass1/body when it sees an internal `define-type'.
;; If EXPR denotes a type we can compute at the compile time, returns the
;; type; otherwise returns #f.
(define (type/compile-time-value expr cenv)
  (cond
   [(identifier? expr)
    ;; Like the $GREF case of type/ensure, but works on the source
    ;; identifier: EXPR may be bound to an identifier macro, and we don't want
    ;; to expand it here only to expand it again if we fall back.
    (let1 var (cenv-lookup cenv expr)
      (cond [(is-a? var <type>) var]    ;another internal type binding
            [(wrapped-identifier? var)
             (and-let* ([gloc (id->bound-gloc var)]
                        [ (gloc-inlinable? gloc) ]
                        [v (gloc-ref gloc)])
               (cond [(is-a? v <class>) (wrap-with-proxy-type var gloc)]
                     [(is-a? v <type>) v]
                     [else #f]))]
            [else #f]))]
   [(%type-constructor-call? expr cenv)
    (type/ensure (pass1 expr cenv) cenv)]
   [else #f]))

;; Is EXPR a call of a type constructor, such as (<?> <integer>)?
(define (%type-constructor-call? expr cenv)
  (and (pair? expr)
       (identifier? (car expr))
       (and-let* ([h (cenv-lookup cenv (car expr))]
                  [ (wrapped-identifier? h) ])
         (receive (gval type) (global-call-type h cenv)
           (eq? type 'type-ctor)))))

;; Returns the compile-time value of ARG, an IForm of an argument of a type
;; constructor expression.  SRC is the source of the expression, for error
;; messages.
(define (type/arg-value arg src)
  (cond [($const? arg) ($const-value arg)]
        [(has-tag? arg $GREF)
         ;; We recognize some "reserved keywords".
         ;; NB: Semantically we could use free-identifier=?, but
         ;; global-syntax=? is faster.
         (cond [(global-syntax=? ($gref-id arg) id:*) '*]
               [(global-syntax=? ($gref-id arg) id:->) '->]
               [(gref-inlinable-gloc arg)
                => (^[gloc]
                     (let1 v (gloc-ref gloc)
                       (if (is-a? v <class>)
                         (wrap-with-proxy-type ($gref-id arg) gloc)
                         v)))]
               [else
                (errorf "Can't use non-inlinable global variable `~s' in \
                         type constructor expression: ~s"
                        ($gref-id arg) src)])]
        [else
         ;; we can run constant folding here, but for the time being...
         (error "Arguments of type constructor expression must be \
                 a compile-time constant:" src)]))

;; For now, we restrict type ctor arguments to simple values
(define (type/check-arg-value val)
  (unless (or (is-a? val <type>)
              (number? val)
              (boolean? val)
              (string? val)
              (symbol? val))
    (error "Invalid value as type constructor argument:" val))
  val)

;; Called from pass1/global-call, when we detect (<type-ctor> arg ...).
;;
;; CTOR is the global value of the type constructor, ID the identifier it is
;; named by, and PROGRAM the source of the whole call.
;;
;; Returns IForm - either a $const of computed type, or $call of
;; construct-type to create a type at runtime.  The latter case is for
;; local type binding.
(define (type/construct ctor id program cenv)
  ;; Check if any of args refer to a local type binding.
  (define (type-args-have-local-type? args cenv)
    (any (^a (or (boolean (%local-type-ref a cenv))
                 (and (pair? a)
                      (%type-constructor-call? a cenv)
                      (type-args-have-local-type? (cdr a) cenv))))
         args))
  (if (type-args-have-local-type? (cdr program) cenv)
    (%construct-runtime ctor program cenv)
    (%construct-const ctor
                      (pass1/call program ($gref id) (cdr program) cenv))))

;; Construct the type now, and return it as a constant.
;; IFORM is the $CALL node representing the ctor invocation.  Since its
;; arguments have already gone through pass1, constant variable reference
;; and type constructor calls are already handled.
(define (%construct-const ctor iform)
  ;; Call type constructor
  (let1 type ($ construct-type ctor
                (map (^a (type/check-arg-value
                          (type/arg-value a ($*-src iform))))
                     ($call-args iform)))
    (unless (is-a? type <descriptive-type>)
      (errorf "Type costructor ~s returned an object other than a \
               type instance: ~s"
              ($*-src iform) type))
    ($const type)))

;; Returns the local type binding EXPR refers to, or #f if EXPR isn't a
;; reference to one.  See pass1/body-rec for local type bindings.
(define (%local-type-ref expr cenv)
  (and (identifier? expr)
       (let1 v (cenv-lookup cenv expr)
         (and (local-type? v) v))))

;; Emit code that constructs the type of PROGRAM on each evaluation, out of
;; the local proxy type the activation holds.  It is correct under re-entry
;; and across threads, for the type is built from the activation's own value.
(define (%construct-runtime ctor program cenv)
  (define (arg-iform arg)
    (cond [(%local-type-ref arg cenv)
           => (^[lt] (pass1 (local-type-proxy-name lt) cenv))]
          [(and (pair? arg) (%type-constructor-call? arg cenv))
           ;; A nested type constructor call.  If it mentions a local type
           ;; as well, pass1 brings it back here and it is built at runtime,
           ;; too; otherwise it is folded into a $const.
           (pass1 arg cenv)]
          [else
           ($const (type/check-arg-value
                    (type/arg-value (pass1 arg cenv) program)))]))
  ($call program ($gref construct-type.)
         (list ($const ctor)
               ($list program (map arg-iform (cdr program))))))
