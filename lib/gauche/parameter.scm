;;;
;;; parameter.scm - simple implementation of parameters.
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
;;;  $Id: parameter.scm,v 1.1 2002-04-20 01:08:05 shirok Exp $
;;;

;; NB: This is a "quick patch" to realize parameters in Gauche.
;; This won't work well in the MT env.

;; The API is taken from ChezScheme and Chicken.

(define-module gauche.parameter
  (use gauche.let-opt)
  (export make-parameter parameterize)
  )
(select-module gauche.parameter)

(define (make-parameter value . maybe-filter)
  (let-optionals* maybe-filter ((filter identity))
    (lambda newval
      (cond ((null? newval) value)
            ((null? (cdr newval))
             (let ((oldval value))
               (set! value (car newval))
               oldval))
            (else
             (error "parameter accepts zero or one argument, but got" newval))
            ))))

(define-syntax parameterize
  (syntax-rules ()
    ((_ (binds ...) . body)
     (%parameterize () () () (binds ...) body))))

(define-syntax %parameterize
  (syntax-rules ()
    ((_ (param ...) (val ...) (var ...) () body)
     (let ((var #f) ...)
       (dynamic-wind
        (lambda () (set! var (param val)) ...)
        (lambda () . body)
        (lambda () (param var) ...))))
    ((_ (param ...) (val ...) (var ...) ((p v) . more) body)
     (%parameterize (param ... p) (val ... v) (var ... tmp) more body))
    ((_ params vals vars other body)
     (syntax-error "malformed binding list for parameterize" other))
    ))

(provide "gauche/parameter")

     
     