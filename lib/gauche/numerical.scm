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
;;;  $Id: numerical.scm,v 1.1 2001-04-23 06:30:46 shiro Exp $
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


;; Insert R5RS functions into scheme module

(with-module scheme
  (define gcd (with-module gauche gcd))
  (define lcm (with-module gauche lcm)))

(provide "gauche/numerical")
