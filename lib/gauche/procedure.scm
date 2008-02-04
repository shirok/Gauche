;;;
;;; procedure.scm - auxiliary procedure utilities.  to be autoloaded.
;;;  
;;;   Copyright (c) 2000-2007  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: procedure.scm,v 1.22 2008-02-04 09:17:47 shirok Exp $
;;;

(define-module gauche.procedure
  (use srfi-1)
  (export compose complement $ $* pa$ map$ for-each$ apply$
          count$ fold$ fold-right$ reduce$ reduce-right$
          filter$ partition$ remove$ find$ find-tail$
          any$ every$ delete$ member$ assoc$
          any-pred every-pred
          arity procedure-arity-includes?
          <arity-at-least> arity-at-least? arity-at-least-value
          case-lambda disasm
          ))

(select-module gauche.procedure)

;; *HIGHLY EXPERIMENTAL*
;; Haskell-ish application.  May change later.
;;
;;  ($ f a b c)         => (f a b c)
;;  ($ f a b c $)       => (lambda args (apply f a b c args))
;;  ($ f $ g a b c)     => (f (g a b c))
;;  ($ f $ g a b c $)   => (lambda args (f (apply g a b c args)))
;;  ($ f $ g $ h a b c) => (f (g (h a b c)))
;;  ($ f a $ g b $ h c) => (f a (g b (h c)))
;;  ($ f a $ g b $ h $) => (lambda args (f a (g b (apply h args))))
;;
;;  ($* f a b c)         => (apply f a b c)
;;  ($* f a $ g b c)     => (apply f a (g b c))
;;  ($* f $* g $ h a b)  => (apply f (apply g (h a b)))
;;  ($* f $* g $* h a b) => (apply f (apply g (apply h a b)))


(define-syntax $
  (syntax-rules ()
    [(_ f . rest) (%$-rec () () f . rest)]
    [(_)          (syntax-error "Invalid $-form: ($)")]))

(define-syntax $*
  (syntax-rules ()
    [(_ f . rest) (%$-rec (apply) () f . rest)]
    [(_)          (syntax-error "Invalid $*-form: ($*)")]))

(define-syntax %$-rec
  (syntax-rules ($ $*)
    ;[(_ (app ...) es $)              (app ... pa$ . es)]
    ;[(_ (app ...) es $*)             (app ... pa$ apply . es)]
    [(_ (app ...) (e ...) $ . rest)  (app ... e ... ($ . rest))]
    [(_ (app ...) (e ...) $* . rest) (app ... e ... ($* . rest))]
    [(_ apps      (e ...) x . rest)  (%$-rec apps (e ... x) . rest)]
    [(_ (app ...) es)                (app ... . es)]))

;; Combinator utilities -----------------------------------------

(define (pa$ fn . args)                  ;partial apply
  (lambda more-args (apply fn (append args more-args))))

(define (compose . fns)
  (cond ((null? fns) values)
        ((null? (cdr fns)) (car fns))
        ((null? (cddr fns))
         (lambda args
           (call-with-values (lambda () (apply (cadr fns) args)) (car fns))))
        (else
         (compose (car fns) (apply compose (cdr fns))))))

(define (compose$ f) (pa$ compose f))

(define (complement fn)
  (case (arity fn) ;; some optimization
    ((0) (lambda () (not (fn))))
    ((1) (lambda (x) (not (fn x))))
    ((2) (lambda (x y) (not (fn x y))))
    (else (lambda args (not (apply fn args))))))

(define (map$ proc)      (pa$ map proc))
(define (for-each$ proc) (pa$ for-each proc))
(define (apply$ proc)    (pa$ apply proc))

;; partial evaluation version of srfi-1 procedures
(define (count$ pred) (pa$ count pred))
(define (fold$ kons . maybe-knil)
  (lambda lists (apply fold kons (append maybe-knil lists))))
(define (fold-right$ kons . maybe-knil)
  (lambda lists (apply fold-right kons (append maybe-knil lists))))
(define (reduce$ f . maybe-ridentity)
  (lambda args (apply reduce f (append maybe-ridentity args))))
(define (reduce-right$ f . maybe-ridentity)
  (lambda args (apply reduce-right f (append maybe-ridentity args))))
(define (filter$ pred) (pa$ filter pred))
(define (partition$ pred) (pa$ partition pred))
(define (remove$ pred) (pa$ remove pred))
(define (find$ pred) (pa$ find pred))
(define (find-tail$ pred) (pa$ find-tail pred))
(define (any$ pred) (pa$ any pred))
(define (every$ pred) (pa$ every pred))
(define (delete$ x) (pa$ delete x))
(define (member$ x) (pa$ member x))
(define (assoc$ x) (pa$ assoc x))


(define (any-pred . preds)
  (lambda args
    (let loop ((preds preds))
      (cond ((null? preds) #f)
            ((apply (car preds) args))
            (else (loop (cdr preds)))))))

(define (every-pred . preds)
  (if (null? preds)
      (lambda args #t)
      (lambda args
        (let loop ((preds preds))
          (cond ((null? (cdr preds))
                 (apply (car preds) args))
                ((apply (car preds) args)
                 (loop (cdr preds)))
                (else #f))))))

;; Procedure arity -----------------------------------------

(define-class <arity-at-least> ()
  ((value :init-keyword :value :init-value 0)))

(define-method write-object ((obj <arity-at-least>) port)
  (format port "#<arity-at-least ~a>" (ref obj 'value)))

(define (arity-at-least? x) (is-a? x <arity-at-least>))

(define (arity-at-least-value x)
  (check-arg arity-at-least? x)
  (ref x 'value))

(define (arity proc)
  (cond
   ((or (is-a? proc <procedure>) (is-a? proc <method>))
    (if (ref proc 'optional)
        (make <arity-at-least> :value (ref proc 'required))
        (ref proc 'required)))
   ((is-a? proc <generic>)
    (map arity (ref proc 'methods)))
   (else
    (errorf "cannot get arity of ~s" proc))))

(define (procedure-arity-includes? proc k)
  (let1 a (arity proc)
    (define (check a)
      (cond ((integer? a) (= a k))
            ((arity-at-least? a) (>= k (arity-at-least-value a)))
            (else (errorf "implementation error in (procedure-arity-includes? ~s ~s)" proc k))))
    (if (list? a)
        (any check a)
        (check a))))

;; case-lambda (srfi-16) ---------------------------------------

;; This is a temporary implementation.  There's a plan to replace it
;; for more efficient dispatching mechanism.  (But I'm not sure when).

(define-syntax case-lambda
  (syntax-rules ()
    ((case-lambda (arg . body) ...)
     (make-dispatcher (list (lambda arg . body) ...)))
    ((case-lambda . _)
     (syntax-error "malformed case-lambda" (case-lambda . _)))))

;; support procedure
(define (make-dispatcher closures)
  (lambda args
    (let ((len (length args)))
      (cond ((find (lambda (p) (procedure-arity-includes? p len)) closures)
             => (cut apply <> args))
            (else
             (error "wrong number of arguments to case-lambda:" args))))))

;; disassembler.
;; I'm not sure whether this should be here or not, but fot the time being...

(define (disasm proc)
  ;; kludge
  (let ((dumper (if (find-module 'gauche.internal)
                  (eval '(with-module gauche.internal vm-dump-code)
                        (find-module 'gauche))
                  vm-dump-code)))
    (dumper (closure-code proc))))

(provide "gauche/procedure")
