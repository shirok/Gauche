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
;;;  $Id: numerical.scm,v 1.13 2002-03-07 09:59:24 shirok Exp $
;;;

(select-module gauche)

;; Gauche is not designed for number crunching programs.  Here I define
;; some R5RS functions in Scheme, just for completeness.

;;
;; GCD and LCM
;;

;; These are the simplest ones.  If you need efficiency, consult
;;  Knuth: "The Art of Computer Programming" Chap. 4.5.2

(define-in-module scheme (gcd . args)
  (define (rec u v)
    (if (zero? v) u (rec v (remainder u v))))
  (define (recn arg args)
    (if (null? args)
        arg
        (recn (rec arg (car args)) (cdr args))))
  (let ((args (map (lambda (arg)
                     (unless (integer? arg)
                       (error "integer required, but got" arg))
                     (abs arg))
                   args)))
    (cond ((null? args) 0)
          ((null? (cdr args)) (car args))
          (else (recn (car args) (cdr args))))))

(define-in-module scheme (lcm . args)
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
                       (error "integer required, but got" arg))
                     (abs arg))
                   args)))
    (cond ((null? args) 1)
          ((null? (cdr args)) (car args))
          (else (recn (car args) (cdr args))))))

;;
;; Numerator and denominator
;;

;; in Gauche, rational number is just the same as integer, i.e. N/1

(define-in-module scheme (numerator q)
  (unless (integer? q) (error "integer required, but got:" q))
  q)

(define-in-module scheme (denominator q)
  (unless (integer? q) (error "integer required, but got:" q))
  (if (exact? q) 1 1.0))

;;
;; Complex numbers
;;

(define-in-module scheme (make-polar r t)
  (check-arg real? r)
  (check-arg real? t)
  (make-rectangular (* r (%cos t)) (* r (%sin t))))

(define-in-module scheme (real-part z)
  (receive (x y) (%complex->real/imag z) x))

(define-in-module scheme (imag-part z)
  (receive (x y) (%complex->real/imag z) y))

;; Transcedental functions.
;; The real version of these functions are built-in.
;;  Cf. Teiji Takagi: "Kaiseki Gairon" pp.193--198

(define-in-module scheme (exp z)
  (cond ((real? z) (%exp z))
        ((complex? z)
         (receive (x y) (%complex->real/imag z)
           (make-polar (%exp x) y)))
        (else (error "number required, but got" z))))

(define-in-module scheme (log z)
  (cond ((real? z) (%log z))
        ((complex? z)
         (make-rectangular (%log (magnitude z)) (angle z)))
        (else (error "number required, but got" z))))

(define-in-module scheme (sqrt z)
  (cond ((real? z) (%sqrt z))
        ((complex? z) 
         (make-polar (%sqrt (magnitude z)) (/ (angle z) 2.0)))
        (else (error "number required, but got" z))))

(define-in-module scheme (expt x y)
  (cond ((real? x)
         (cond ((real? y) (%expt x y))
               ((number? y)
                (receive (real imag) (%complex->real/imag y)
                  (* (%expt x real) (exp (* +i imag (%log x))))))
               (else (error "number required, but got" y))))
        ((number? x) (exp (* y (log x))))
        (else (error "number required, but got" x))))

(define-in-module scheme (cos z)
  (cond ((real? z) (%cos z))
        ((number? z)
         (receive (x y) (%complex->real/imag z)
           (make-rectangular (* (%cos x) (%cosh y))
                             (- (* (%sin x) (%sinh y))))))
        (else (error "number required, but got" z))))

(define (cosh z)
  (cond ((real? z) (%cosh z))
        ((number? z)
         (receive (x y) (%complex->real/imag z)
           (make-rectangular (* (%cosh x) (%cos y))
                             (* (%sinh x) (%sin y)))))
        (else (error "number required, but got" z))))

(define-in-module scheme (sin z)
  (cond ((real? z) (%sin z))
        ((number? z)
         (receive (x y) (%complex->real/imag z)
           (make-rectangular (* (%sin x) (%cosh y))
                             (* (%cos x) (%sinh y)))))
        (else (error "number required, but got" z))))

(define (sinh z)
  (cond ((real? z) (%sinh z))
        ((number? z)
         (receive (x y) (%complex->real/imag z)
           (make-rectangular (* (%sinh x) (%cos y))
                             (* (%cosh x) (%sin y)))))
        (else (error "number required, but got" z))))

(define-in-module scheme (tan z)
  (cond ((real? z) (%tan z))
        ((number? z)
         (let ((iz (* +i z)))
           (* -i
              (/ (- (exp iz) (exp (- iz)))
                 (+ (exp iz) (exp (- iz)))))))
        (else (error "number required, but got" z))))

(define (tanh z)
  (cond ((real? z) (%tanh z))
        ((number? z)
         (/ (- (exp z) (exp (- z)))
            (+ (exp z) (exp (- z)))))
        (else (error "number required, but got" z))))

(define-in-module scheme (asin z)
  (cond ((real? z) (%asin z))
        ((number? z)
         ;; The definition of asin is
         ;;   (* -i (log (+ (* +i z) (sqrt (- 1 (* z z))))))
         ;; This becomes unstable when the term in the log is reaching
         ;; toward 0.0.  The term, k = (+ (* +i z) (sqrt (- 1 (* z z)))),
         ;; gets closer to zero when |z| gets bigger, but for large |z|,
         ;; k is prone to lose precision and starts drifting around
         ;; the point zero.
         ;; For now, I let asin to return NaN for large z's.
         (if (> (magnitude z) 1.0e5)
             (make-rectangular (log 0.0) (log 0.0)) ;NaN+NaNi
             (* -i (log (+ (* +i z) (sqrt (- 1 (* z z))))))))
        (else (error "number required, but got" z))))

(define (asinh z)
  (if (> (magnitude z) 1.0e5)
      (make-rectangular (log 0.0) (log 0.0)) ;NaN+NaNi
      (log (+ z (sqrt (+ (* z z) 1))))))

(define-in-module scheme (acos z)
  (cond ((real? z) (%acos z))
        ((number? z)
         ;; The definition of acos is
         ;;  (* -i (log (+ z (* +i (sqrt (- 1 (* z z)))))))))
         ;; This also falls in the victim of numerical unstability; worse than
         ;; asin, sometimes the real part of marginal value "hops" between
         ;; +pi and -pi.  It's rather stable to use asin.
         (- 1.5707963267948966 (asin z)))
        (else (error "number required, but got" z))))

(define (acosh z)
  ;; See the discussion of CLtL2, pp. 313-314
  (* 2 (log (+ (sqrt (/ (+ z 1) 2))
               (sqrt (/ (- z 1) 2))))))

(define-in-module scheme (atan z . x)
  (if (null? x)
      (cond ((real? z) (%atan z))
            ((number? z)
             (let ((iz (* z +i)))
               (/ (- (log (+ 1 iz))
                     (log (- 1 iz)))
                  +2i)))
            (else (error "number required, but got" z)))
      (%atan z (car x))))

(define (atanh z)
  (/ (- (log (+ 1 z)) (log (- 1 z))) 2))

(provide "gauche/numerical")
