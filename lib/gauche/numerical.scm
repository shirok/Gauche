;;;
;;; numerical.scm - auxiliary numerical functions - to be autoloaded
;;;
;;;  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: numerical.scm,v 1.2 2001-04-23 07:10:09 shiro Exp $
;;;

(select-module gauche)

;; Gauche is not designed for number crunching programs.  Here I define
;; some R5RS functions in Scheme, just for completeness.

;;
;; GCD and LCM
;;

;; These are the simplest ones.  If you need efficiency, consult
;;  Knuth: "The Art of Computer Programming" Chap. 4.5.2

(define (gcd . args)
  (define (rec u v)
    (if (zero? v) u (rec v (remainder u v))))
  (define (recn arg args)
    (if (null? args)
        arg
        (recn (rec arg (car args)) (cdr args))))
  (let ((args (map (lambda (arg)
                     (unless (integer? arg)
                       (error "integer required, but got %S" arg))
                     (abs arg))
                   args)))
    (cond ((null? args) 0)
          ((null? (cdr args)) (car args))
          (else (recn (car args) (cdr args))))))

(define (lcm . args)
  (define (gcd u v)
    (if (zero? v) u (gcd v (remainder u v))))
  (define (lcm2 u v)
    (let ((g (gcd u v)))
      (if (zero? u) 0 (* (quotient u g) v))))
  (define (recn arg args)
    (if (null? args)
        arg
        (recn (lcm2 arg (car args)) (cdr args))))
  (let ((args (map (lambda (arg)
                     (unless (integer? arg)
                       (error "integer required, but got %S" arg))
                     (abs arg))
                   args)))
    (cond ((null? args) 1)
          ((null? (cdr args)) (car args))
          (else (recn (car args) (cdr args))))))

;;
;; Numerator and denominator
;;

;; in Gauche, rational number is just the same as integer, i.e. N/1

(define (numerator q)
  (unless (integer? q) (error "integer required, but got: %S" q))
  q)

(define (denominator q)
  (unless (integer? q) (error "integer required, but got: %S" q))
  (if (exact? q) 1 1.0))

;;
;; Complex numbers
;;

(define (make-polar r t)
  (check-arg real? r)
  (check-arg real? t)
  (make-rectangular (* r (cos t)) (* r (sin t))))

(define (real-part z)
  (cond ((real? z) z)
        ((number? z) (car (%complex->real/imag z)))
        (else (error "number required, but got: %S" z))))

(define (imag-part z)
  (cond ((real? z) (if (exact? z) 0 0.0))
        ((number? z) (cdr (%complex->real/imag z)))
        (else (error "number required, but got: %S" z))))

;; Insert R5RS functions into scheme module

(with-module scheme
  (define gcd (with-module gauche gcd))
  (define lcm (with-module gauche lcm))
  (define numerator (with-module gauche numerator))
  (define denominator (with-module gauche denominator))
  (define make-polar (with-module gauche make-polar))
  (define real-part (with-module gauche real-part))
  (define imag-part (with-module gauche imag-part))
  )

(provide "gauche/numerical")
