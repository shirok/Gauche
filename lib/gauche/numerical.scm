;;;
;;; numerical.scm - auxiliary numerical functions - to be autoloaded
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

(select-module gauche)

;; Gauche is not designed for number crunching programs.  Here I define
;; some R5RS functions in Scheme, just for completeness.

;;
;; GCD and LCM
;;

;; These are the simplest ones.  If you need efficiency, consult
;;  Knuth: "The Art of Computer Programming" Chap. 4.5.2

(define-in-module scheme (gcd . args)
  (define (recn arg args)
    (if (null? args)
        arg
        (recn (%gcd arg (car args)) (cdr args))))
  (let ((args (map (lambda (arg)
                     (unless (integer? arg)
                       (error "integer required, but got" arg))
                     (abs arg))
                   args)))
    (cond ((null? args) 0)
          ((null? (cdr args)) (car args))
          (else (recn (car args) (cdr args))))))

(define-in-module scheme (lcm . args)
  (define (lcm2 u v)
    (let ((g (%gcd u v)))
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
;; Complex numbers
;;

;; I put make-polar back to the C subr.  Because of Intel's 80-bit
;; floating-point number arithmetic, the calculation done in C (in the
;; number reading routine) doesn't exactly meet the calculation done
;; in Scheme (via Scm_Multiply), and it made the following expression fail:
;;
;;   (eqv? (make-polar 7.0 -1.5) 7.0@-1.5)
;;
;(define-in-module scheme (make-polar r t)
;  (check-arg real? r)
;  (check-arg real? t)
;  (make-rectangular (* r (%cos t)) (* r (%sin t))))

;; Transcedental functions.
;; The real version of these functions are built-in.
;;  Cf. Teiji Takagi: "Kaiseki Gairon" pp.193--198

(define-in-module scheme (exp z)
  (cond [(real? z) (%exp z)]
        [(complex? z) (make-polar (%exp (real-part z)) (imag-part z))]
        [else (error "number required, but got" z)]))

(define-in-module scheme (log z . base)
  (if (null? base)
    (cond [(real? z) (%log z)]
          [(complex? z) (make-rectangular (%log (magnitude z)) (angle z))]
          [else (error "number required, but got" z)])
    (/ (log z) (log (car base)))))  ; R6RS addition

(define-in-module scheme (sqrt z)
  (cond [(and (exact? z) (>= z 0))
         ;; Gauche doesn't have exact complex, so we have real z.
         (if (integer? z)
           (receive (s r) (%exact-integer-sqrt z)
             (if (= r 0) s (%sqrt z)))
           ;; we have ratnum.  take expensive path.
           (let ([n (numerator z)]
                 [d (denominator z)])
             (receive (ns nr) (%exact-integer-sqrt n)
               (if (= nr 0)
                 (receive (ds dr) (%exact-integer-sqrt d)
                   (if (= dr 0)
                     (/ ns ds)
                     (%sqrt z)))
                 (%sqrt z)))))]
        [(real? z) (%sqrt z)]
        [(complex? z) (make-polar (%sqrt (magnitude z)) (/ (angle z) 2.0))]
        [else (error "number required, but got" z)]))

;; a la R6RS
(define (exact-integer-sqrt k)
  (unless (and (exact? k) (integer? k) (>= k 0))
    (error "exact nonnegative integer required, but got" k))
  (%exact-integer-sqrt k))

(define (%exact-integer-sqrt k)
  (if (< k 9007199254740992)            ;2^53
    ;; k can be converted to a double without loss.
    (let1 s (floor->exact (%sqrt k))
      (values s (- k (* s s))))
    ;; use Newton-Rhapson
    (let loop ([s (floor->exact (%sqrt k))])
      (let1 s2 (* s s)
        (if (< k s2)
          (loop (quotient (+ s2 k) (* 2 s)))
          (let1 s2+ (+ s2 (* 2 s) 1)
            (if (< k s2+)
              (values s (- k s2))
              (loop (quotient (+ s2 k) (* 2 s))))))))))

(define-in-module scheme (expt x y)
  (cond [(real? x)
         (cond [(real? y) (%expt x y)]
               [(number? y)
                (* (%expt x (real-part y))
                   (exp (* +i (imag-part y) (%log x))))]
               [else (error "number required, but got" y)])]
        [(number? x) (exp (* y (log x)))]
        [else (error "number required, but got" x)]))

(define-in-module scheme (cos z)
  (cond [(real? z) (%cos z)]
        [(number? z)
         (let ((x (real-part z))
               (y (imag-part z)))
           (make-rectangular (* (%cos x) (%cosh y))
                             (- (* (%sin x) (%sinh y)))))]
        [else (error "number required, but got" z)]))

(define (cosh z)
  (cond [(real? z) (%cosh z)]
        [(number? z)
         (let ((x (real-part z))
               (y (imag-part z)))
           (make-rectangular (* (%cosh x) (%cos y))
                             (* (%sinh x) (%sin y))))]
        [else (error "number required, but got" z)]))

(define-in-module scheme (sin z)
  (cond [(real? z) (%sin z)]
        [(number? z)
         (let ((x (real-part z))
               (y (imag-part z)))
           (make-rectangular (* (%sin x) (%cosh y))
                             (* (%cos x) (%sinh y))))]
        [else (error "number required, but got" z)]))

(define (sinh z)
  (cond [(real? z) (%sinh z)]
        [(number? z)
         (let ((x (real-part z))
               (y (imag-part z)))
           (make-rectangular (* (%sinh x) (%cos y))
                             (* (%cosh x) (%sin y))))]
        [else (error "number required, but got" z)]))

(define-in-module scheme (tan z)
  (cond [(real? z) (%tan z)]
        [(number? z)
         (let ((iz (* +i z)))
           (* -i
              (/ (- (exp iz) (exp (- iz)))
                 (+ (exp iz) (exp (- iz))))))]
        [else (error "number required, but got" z)]))

(define (tanh z)
  (cond [(real? z) (%tanh z)]
        [(number? z)
         (/ (- (exp z) (exp (- z)))
            (+ (exp z) (exp (- z))))]
        [else (error "number required, but got" z)]))

(define-in-module scheme (asin z)
  (cond [(real? z) (%asin z)]
        [(number? z)
         ;; The definition of asin is
         ;;   (* -i (log (+ (* +i z) (sqrt (- 1 (* z z))))))
         ;; This becomes unstable when the term in the log is reaching
         ;; toward 0.0.  The term, k = (+ (* +i z) (sqrt (- 1 (* z z)))),
         ;; gets closer to zero when |z| gets bigger, but for large |z|,
         ;; k is prone to lose precision and starts drifting around
         ;; the point zero.
         ;; For now, I let asin to return NaN in such cases.
         (let1 zz (+ (* +i z) (sqrt (- 1 (* z z))))
           (if (< (/. (magnitude zz) (magnitude z)) 1.0e-8)
             (make-rectangular +nan.0 +nan.0)
             (* -i (log zz))))]
        [else (error "number required, but got" z)]))

(define (asinh z)
  (let1 zz (+ z (sqrt (+ (* z z) 1)))
    (if (< (/. (magnitude zz) (magnitude z)) 1.0e-8)
      (make-rectangular +nan.0 +nan.0)
      (log (+ z (sqrt (+ (* z z) 1)))))))

(define-in-module scheme (acos z)
  (cond [(real? z) (%acos z)]
        [(number? z)
         ;; The definition of acos is
         ;;  (* -i (log (+ z (* +i (sqrt (- 1 (* z z)))))))))
         ;; This also falls in the victim of numerical unstability; worse than
         ;; asin, sometimes the real part of marginal value "hops" between
         ;; +pi and -pi.  It's rather stable to use asin.
         (- 1.5707963267948966 (asin z))]
        [else (error "number required, but got" z)]))

(define (acosh z)
  ;; See the discussion of CLtL2, pp. 313-314
  (* 2 (log (+ (sqrt (/ (+ z 1) 2))
               (sqrt (/ (- z 1) 2))))))

(define-in-module scheme (atan z . x)
  (if (null? x)
    (cond [(real? z) (%atan z)]
          [(number? z)
           (let1 iz (* z +i)
             (/ (- (log (+ 1 iz))
                   (log (- 1 iz)))
                +2i))]
          [else (error "number required, but got" z)])
    (%atan z (car x))))

(define (atanh z)
  (/ (- (log (+ 1 z)) (log (- 1 z))) 2))

;;
;; R6RS compatibiliby
;;
(define inexact exact->inexact)
(define exact   inexact->exact)

;; Since Gauche coerces complex number whose imag part is 0.0i into
;; real, these procedures has no difference from real?, rational? and
;; integer?
(define real-valued?     real?)
(define rational-valued? rational?)
(define integer-valued?  integer?)

;;
;; Nearly equal comparison
;;  (Unofficial yet; see how it works)

(define (nearly=? tolerance x y)
  (< (abs (- x y))
     (/ (max (abs x) (abs y)) tolerance)))

