;;;
;;; parseopt.scm - yet another command-line argument parser
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.parseopt
  (export make-option-parser parse-options let-args <parseopt-error>))
(select-module gauche.parseopt)

;; This error is thrown when the given argument doesn't follow the spec.
;; (An error in the spec itself is thrown as an ordinary error.)
(define-condition-type <parseopt-error> <error> #f
  (option-name))

;; Represents each option info
(define-class <option-spec> ()
  ((name :init-keyword :name)        ; option name
   (args :init-keyword :args)        ; option agrspecs (list of chars)
   (arg-optional? :init-keyword :arg-optional?) ; option's arg optional?
   (handler :init-keyword :handler)  ; handler closure
   (plural? :init-keyword :plural?)  ; accept multiple options?
   (optspec :init-keyword :optspec)  ; original <optspec> string
   (help    :init-keyword :help)     ; help string
   ))

;; Helper functions

;; Parse optspec clause, and returns
;;  (<option-spec> ...)
;; <a-spec> is (<optspec> <handler>) or
;; ((<optspec> <help-string>) <handler>)
(define (compose-entry a-spec)
  (let ([optspec (if (pair? (car a-spec)) (caar a-spec) (car a-spec))]
        [helpstr (and (not (length<=? (car a-spec) 1))
                      (cadar a-spec))]
        [handler (cadr a-spec)])
    (unless (string? optspec)
      (error "option spec must be a string, but got" optspec))
    (rxmatch-if (rxmatch #/^-*([-+\w|]+)(\*)?(?:([=:])(.+))?$/ optspec)
        (#f optnames plural? optional?  argspec)
      (map (cute make <option-spec>
                 :name <>
                 :args (if argspec (string->list argspec) '())
                 :arg-optional? (equal? optional? ":")
                 :handler handler
                 :plural? plural?
                 :optspec optspec
                 :help helpstr)
           (string-split optnames #\|))
      (error "unrecognized option spec:" optspec))))

;; From the args given at the command line, get a next option.
;; Returns option string and rest args.
(define (next-option args)
  (cond
   [(null? args) (values #f '())]
   [(string=? (car args) "--") (values #f (cdr args))]
   [(#/^--?(\w[-+\w]*)(=)?/ (car args))
    => (^m (values (m 1) (if (m 2) (cons (m'after) (cdr args)) (cdr args))))]
   [else (values #f args)]))

;; From the list of optarg spec and given command line arguments,
;; get a list of optargs.  Returns optargs and unconsumed args.
(define (get-optargs optspec args)
  (define (get-number arg)
    (or (string->number arg)
        (errorf <parseopt-error> :option-name (~ optspec 'name)
                "a number is required for option ~a, but got ~a"
                (~ optspec 'name) arg)))
  (define (get-real arg)
    (or (and-let* ([num (string->number arg)]
                   [ (real? num) ])
          (exact->inexact num))
        (errorf <parseopt-error> :option-name (~ optspec'name)
                "a real number is required for option ~a, but got ~a"
                (~ optspec 'name) arg)))
  (define (get-integer arg)
    (or (and-let* ([num (string->number arg)]
                   [ (exact? num) ])
          num)
        (errorf <parseopt-error> :option-name (~ optspec'name)
                "an integer is required for option ~a, but got ~a"
                (~ optspec 'name) arg)))
  (define (get-sexp arg)
    (guard (e [(<read-error> e)
               (errorf <parseopt-error> :option-name (~ optspec'name)
                       "the argument for option ~a is not valid sexp: ~s"
                       (~ optspec 'name) arg)])
      (read-from-string arg)))
  (define (process-args)
    (let loop ([spec (~ optspec 'args)]
               [args args]
               [optargs '()])
      (cond [(null? spec) (values (reverse! optargs) args)]
            [(null? args) (error <parseopt-error> :option-name (~ optspec'name)
                                 "running out the arguments for option"
                                 (~ optspec 'name))]
            [else
             (case (car spec)
               [(#\s) (loop (cdr spec) (cdr args) (cons (car args) optargs))]
               [(#\n) (loop (cdr spec) (cdr args)
                            (cons (get-number (car args)) optargs))]
               [(#\f) (loop (cdr spec) (cdr args)
                            (cons (get-real (car args)) optargs))]
               [(#\i) (loop (cdr spec) (cdr args)
                            (cons (get-integer (car args)) optargs))]
               [(#\e) (loop (cdr spec) (cdr args)
                            (cons (get-sexp (car args)) optargs))]
               [(#\y) (loop (cdr spec) (cdr args)
                            (cons (string->symbol (car args)) optargs))]
               [else (error "unknown option argument spec:" (car spec))])])
      ))

  (if (~ optspec 'arg-optional?)
    (if (or (null? args)
            (#/^-/ (car args)))
      (values (make-list (length (~ optspec 'args)) #f) args)
      (process-args))
    (process-args))
  )

;; Now, this is the argument parser body.
(define (parse-cmdargs args speclist fallback)
  (let loop ([args args])
    (receive (option nextargs) (next-option args)
      (if option
        (cond [(find (^e (equal? option (~ e'name))) speclist)
               => (^[entry] (receive (optargs nextargs)
                                (get-optargs entry nextargs)
                              (apply (~ entry'handler) optargs)
                              (loop nextargs)))]
              [else (fallback option nextargs loop)])
        nextargs))))

;; Build
(define (build-option-parser spec fallback)
  (let1 speclist (append-map compose-entry spec)
    (^[args :optional (fb fallback)] (parse-cmdargs args speclist fb))))

;;;
;;; The main body of the macros
;;;

(define-syntax make-option-parser
  (syntax-rules ()
    [(_ clauses)
     (make-option-parser-int clauses ())]))

(define-syntax make-option-parser-int
  (syntax-rules (else =>)
    [(_ () specs)
     (build-option-parser (list . specs)
                          (^[option args looper]
                            (error <parseopt-error> :option-name #f
                                   "unrecognized option:" option)))]
    [(_ ((else args . body)) specs)
     (build-option-parser (list . specs) (^ args . body))]
    [(_ ((else => proc)) specs)
     (build-option-parser (list . specs) proc)]
    [(_ ((optspec => proc) . clause) (spec ...))
     (make-option-parser-int clause (spec ... (list optspec proc)))]
    [(_ ((optspec vars . body) . clause) (spec ...))
     (make-option-parser-int clause (spec ... (list optspec (^ vars . body))))]
    [(_ (other . clause) specs)
     (syntax-error "make-option-parser: malformed clause:" other)]
    ))

(define-syntax parse-options
  (syntax-rules ()
    [(_ args clauses)
     ((make-option-parser clauses) args)]))

;;;
;;; The alternative way : let-args
;;;   Based on Alex Shinn's implementation.
;;;

;; (let-args args (varspec ...) body ...)
;;  where varspec can be
;;   (var spec [default])
;;  or
;;   (var spec [default] => callback)
;;
;;  varspec can be an improper list, as
;;
;; (let-args args (varspec ... . rest) body ...)
;;
;;  then, rest is bound to the rest of the args.


;; Auxiliary macro.
;; Collects parse-options optspec (opts) and variable bindings (binds).
(define-syntax let-args-internal
  (syntax-rules (else =>)
    ;;
    ;; Handle var == #f case first.  This is only useful for side-effects
    ;; or recognizing option (and then discard).
    ;;
    [(_ args binds (opts ...) ((#f spec1 def => callback) . varspecs) body)
     (let-args-internal args
        binds
        (opts ... (spec1 => callback))
        varspecs
        body)]
    [(_ args binds (opts ...) ((#f spec1 => callback) . varspecs) body)
     (let-args-internal args
        binds
        (opts ... (spec1 => callback))
        varspecs
        body)]
    [(_ args binds opts ((#f spec1 ignore) . varspecs) body)
     (let-args-internal args
        binds
        opts
        ((#f spec1 => (^ _ #f)) . varspecs)
        body)]
    [(_ args binds opts ((#f spec1) . varspecs) body)
     (let-args-internal args
        binds
        opts
        ((#f spec1 => (^ _ #f)) . varspecs)
        body)]
    ;;
    ;; Handle else clause
    ;; The contents of clause must evaluated outside of binding scope.
    ;;
    [(_ args binds (opts ...) ((else => else-cb) . varspecs) body)
     (let-args-internal args
         ((e else-cb) . binds)
         (opts ... (else f (apply e f)))
         varspecs
         body)]
    [(_ args binds (opts ...) ((else formals . forms) . varspecs) body)
     (let-args-internal args
         ((e (^ formals . forms)) . binds)
         (opts ... (else f (apply e f)))
         varspecs
         body)]
    ;;
    ;; Handle explicit callbacks.
    ;;
    [(_ args binds (opts ...) ((var1 spec1 default1 => callback) . varspecs) body)
     (let-args-internal args
         ((var1 default1) (cb1 callback) . binds)
         (opts ... (spec1 => (^ x (set! var1 (apply cb1 x)))))
         varspecs
         body)]
    [(_ args binds (opts ...) ((var1 spec1 => callback) . varspecs) body)
     (let-args-internal args
         binds
         (opts ...)
         ((var1 spec1 #f => callback) . varspecs)
         body)]
    ;; Normal case.
    ;; Transform base form into a let w/ a callback to set its value
    ;; we don't know # of values to receive unless we parse the optspec,
    ;; so the callback needs extra trick.  (Ugly... needs rework).
    ;;
    [(_ args binds (opts ...) ((var1 spec1 default1) . varspecs) body)
     (let-args-internal args
         ((var1 default1) . binds)
         (opts ... (spec1 => (^ x
                               (set! var1
                                     (cond [(null? x) #t] ;; no arg
                                           [(null? (cdr x)) (car x)]
                                           [else x])))))
         varspecs
         body)]
    ;;
    ;; No default means #f
    ;;
    [(_ args binds (opts ...) ((var1 spec1) . varspecs) body)
     (let-args-internal args
         binds
         (opts ...)
         ((var1 spec1 #f) . varspecs)
         body)]
    ;;
    ;; Capture invalid clause
    ;;
    [(_ args binds (opts ...) (other . varspecs) body)
     (syntax-error "let-args: invalid clause:" other)]
    ;;
    ;; Finish
    ;; Extra let() allows body contains internal defines.
    ;;
    [(_ args binds opts () body)
     (let binds (parse-options args opts) (let () . body))]
    [(_ args binds opts rest body)
     (let binds (let ((rest (parse-options args opts))) (let () . body)))]
    ))

(define-syntax let-args
  (syntax-rules ()
    ;; transfer to let-args-internal which collects the parse-options
    ;; form
    [(_ args varspecs . body)
     (let-args-internal args () () varspecs body)]
    [(_ . otherwise)
     (syntax-error "malformed let-args:" (let-args . otherwise))]
    ))

