;;;
;;; procedure.scm - auxiliary procedure utilities.  to be autoloaded.
;;;
;;;  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: procedure.scm,v 1.3 2002-05-07 20:17:19 shirok Exp $
;;;

(define-module gauche.procedure
  (use srfi-1)
  (export arity procedure-arity-includes?
          <arity-at-least> arity-at-least? arity-at-least-value
          compose pa
          ))

(select-module gauche.procedure)

;; Procedure arity -----------------------------------------

(define-class <arity-at-least> ()
  ((value :init-keyword :value :init-value 0)))

(define-method write-object ((obj <arity-at-least>) port)
  (format port "#<arity-at-least ~a>" (ref obj 'value)))

(define (arity-at-least? x) (is-a? x <arity-at-least>))

(define (arity-at-least-value x)
  (check-arg? arity-at-least? x)
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

;; Combinator utilities -----------------------------------------

(define (compose f g . more)
  (if (null? more)
      (lambda args
        (call-with-values (lambda () (apply g args)) f))
      (compose f (apply compose g more))))

(define (pa fn . args)                  ;partial apply
  (lambda more-args (apply fn (append args more-args))))

(provide "gauche/procedure")
