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
;;;  $Id: numerical.scm,v 1.8 2001-05-11 20:25:53 shirok Exp $
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
  (make-rectangular (* r (%cos t)) (* r (%sin t))))

(define (real-part z)
  (cond ((real? z) z)
        ((number? z) (car (%complex->real/imag z)))
        (else (error "number required, but got: %S" z))))

(define (imag-part z)
  (cond ((real? z) (if (exact? z) 0 0.0))
        ((number? z) (cdr (%complex->real/imag z)))
        (else (error "number required, but got: %S" z))))

;; Complex transcedental functions.
;; These are called by the main transcedental functions.  assumes
;; z are complex.
;;  Cf. Teiji Takagi: "Kaiseki Gairon" pp.193--198

(define (%complex-exp z)
  (let ((xy (%complex->real/imag z)))
    (make-polar (%exp (car xy)) (cdr xy))))

(define (%complex-log z)
  (make-rectangular (%log (magnitude z)) (angle z)))

(define (%complex-sqrt z)
  (make-polar (%sqrt (magnitude z)) (/ (angle z) 2.0)))

(define (%complex-expt x y)
  (if (real? x)
      (let ((ri (%complex->real/imag y)))
        (* (%expt x (car ri)) (exp (* +i (cdr ri) (%log x)))))
      (exp (* y (log x)))))

(define (%complex-cos z)
  (let ((xy (%complex->real/imag z)))
    (make-rectangular (* (%cos (car xy)) (%cosh (cdr xy)))
                      (- (* (%sin (car xy)) (%sinh (cdr xy)))))))

(define (%complex-cosh z)
  (let ((xy (%complex->real/imag z)))
    (make-rectangular (* (%cosh (car xy)) (%cos (cdr xy)))
                      (* (%sinh (car xy)) (%sin (cdr xy))))))

(define (%complex-sin z)
  (let ((xy (%complex->real/imag z)))
    (make-rectangular (* (%sin (car xy)) (%cosh (cdr xy)))
                      (* (%cos (car xy)) (%sinh (cdr xy))))))

(define (%complex-sinh z)
  (let ((xy (%complex->real/imag z)))
    (make-rectangular (* (%sinh (car xy)) (%cos (cdr xy)))
                      (* (%cosh (car xy)) (%sin (cdr xy))))))

(define (%complex-tan z)
  (let ((iz (* +i z)))
    (* -i
       (/ (- (exp iz) (exp (- iz)))
          (+ (exp iz) (exp (- iz)))))))

(define (%complex-tanh z)
  (/ (- (exp z) (exp (- z)))
     (+ (exp z) (exp (- z)))))

(define (%complex-asin z)
  ;; The definition of asin is
  ;;   (* -i (log (+ (* +i z) (sqrt (- 1 (* z z))))))
  ;; This becomes unstable when the term in the log is reaching
  ;; toward 0.0.  The term, k = (+ (* +i z) (sqrt (- 1 (* z z)))),
  ;; gets closer to zero when |z| gets bigger, but for large |z|, k is prone
  ;; to lose precision and starts drifting around the point zero.
  ;; For now, I let asin to return NaN for large z's.
  (if (> (magnitude z) 1.0e5)
      (make-rectangular (log 0.0) (log 0.0)) ;NaN+NaNi
      (* -i (log (+ (* +i z) (sqrt (- 1 (* z z)))))))
  )

(define (%complex-asinh z)
  (if (> (magnitude z) 1.0e5)
      (make-rectangular (log 0.0) (log 0.0)) ;NaN+NaNi
      (log (+ z (sqrt (+ (* z z) 1))))))

(define (%complex-acos z)
  ;; The definition of acos is
  ;;  (* -i (log (+ z (* +i (sqrt (- 1 (* z z)))))))))
  ;; This also falls in the victim of numerical unstability; worse than
  ;; asin, sometimes the real part of marginal value "hops" between
  ;; +pi and -pi.  It's rather stable to use asin.
  (- 1.5707963267948966 (asin z)))

(define (%complex-acosh z)
  ;; See the discussion of CLtL2, pp. 313-314
  (* 2 (log (+ (sqrt (/ (+ z 1) 2))
               (sqrt (/ (- z 1) 2))))))

(define (%complex-atan z)
  (let ((iz (* z +i)))
    (/ (- (log (+ 1 iz))
          (log (- 1 iz)))
       +2i)))

(define (%complex-atanh z)
  (/ (- (log (+ 1 z)) (log (- 1 z))) 2))

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
