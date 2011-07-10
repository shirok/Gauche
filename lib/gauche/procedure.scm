;;;
;;; procedure.scm - auxiliary procedure utilities.  to be autoloaded.
;;;  
;;;   Copyright (c) 2000-2010  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.procedure
  (use srfi-1)
  (export compose .$ complement pa$ map$ for-each$ apply$
          count$ fold$ fold-right$ reduce$ reduce-right$
          filter$ partition$ remove$ find$ find-tail$
          any$ every$ delete$ member$ assoc$
          any-pred every-pred
          arity procedure-arity-includes?
          <arity-at-least> arity-at-least? arity-at-least-value
          case-lambda ~ ref* disasm
          ))

(select-module gauche.procedure)

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

(define .$ compose)                     ;experimental

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

;; Curryable procedures ------------------------------------

#|  disabled for now; see proc.c for the details.

(define %procedure-currying-set!        ;hidden
  (with-module gauche.internal %procedure-currying-set!))

(define-syntax curry-lambda
  (syntax-rules ()
    [(_ formals . body)
     (let1 var (lambda formals . body)
       (%procedure-currying-set! var #t)
       var)]))

(define-syntax define-curry
  (syntax-rules ()
    [(_ (name . formals) . body)
     (define name (curry-lambda formals . body))]))
|#

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
  (cond [(or (is-a? proc <procedure>) (is-a? proc <method>))
         (if (ref proc 'optional)
           (make <arity-at-least> :value (ref proc 'required))
           (ref proc 'required))]
        [(is-a? proc <generic>) (map arity (ref proc 'methods))]
        [else (errorf "cannot get arity of ~s" proc)]))

(define (procedure-arity-includes? proc k)
  (let1 a (arity proc)
    (define (check a)
      (cond [(integer? a) (= a k)]
            [(arity-at-least? a) (>= k (arity-at-least-value a))]
            [else (errorf "implementation error in (procedure-arity-includes? ~s ~s)" proc k)]))
    (if (list? a)
      (any check a)
      (check a))))

;; ~, ref*  ----------------------------------------------------
;;  (~ a b c d) => (ref (ref (ref a b) c) d)
;;  NB: TEMPORARY BEING HERE: This is better to be in src/objlib.scm,
;;  but we need to depend on case-lambda, so this should be defined
;;  after case-lambda (& related helper procedures) to be loaded.
;;  The way to go is to integrate case-lambda in the core first, then
;;  move this to objlib.scm.
(define ~
  (getter-with-setter
   (case-lambda
     [(obj selector) (ref obj selector)]
     [(obj selector . more) (apply ~ (ref obj selector) more)])
   (case-lambda
     [(obj selector val) ((setter ref) obj selector val)]
     [(obj selector selector2 . rest)
      (apply (setter ~) (ref obj selector) selector2 rest)])))

(define ref* ~)                         ;for the backward compatibility

;; disassembler ------------------------------------------------
;; I'm not sure whether this should be here or not, but fot the time being...

(define (disasm proc)
  (define dump (with-module gauche.internal vm-dump-code))
  (define (dump-case-lambda min-reqargs proc-array)
    (print "CASE-LAMBDA")
    (let1 len (vector-length proc-array)
      (do ([i 0 (+ i 1)])
          ((= i len) #t)
        (when (closure? (vector-ref proc-array i))
          (if (= i (- len 1))
            (print ";;; numargs >= " (+ i min-reqargs))
            (print ";;; numargs = " (+ i min-reqargs)))
          (dump (closure-code (vector-ref proc-array i)))))))
    
  (cond
   [(closure? proc) (print "CLOSURE " proc) (dump (closure-code proc))]
   [(is-a? proc <method>)
    (print "METHOD " proc)
    (cond [(method-code proc) => dump]
          [else (print "(defined in C)")])]
   [(is-a? proc <generic>)
    (print "GENERIC FUNCTION " proc)
    (dolist [m (~ proc'methods)]
      (print ">> method " m)
    (cond [(method-code m) => dump]
          [else (print "(defined in C)")]))]
   [(and (subr? proc)
         (let1 info (procedure-info proc)
           ;; NB: Detect case-lamdba.  The format of procedure-info of
           ;; case-lambda is tentative, and may be changed in future.
           ;; See the comment of make-case-lambda-dispatcher in intlib.stub.
           (and (pair? info)
                (eq? (car info) 'case-lambda-dispatcher)
                (pair? (cdr info))
                (integer? (cadr info))
                (pair? (cddr info))
                (vector? (caddr info))
                (apply dump-case-lambda (cdr info)))))]
   [else (print "Disassemble not applicable for " proc)])
  (values))
