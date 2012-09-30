;;;
;;; numerical.scm - auxiliary numerical functions - to be autoloaded
;;;
;;;   Copyright (c) 2000-2012  Shiro Kawai  <shiro@acm.org>
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
;; Rational approximation
;;

;; Aux. function to calculate (regular) continued fraction representation
;; of rational Q.
(define (continued-fraction Q)
  (let ([a (numerator Q)] [b (denominator Q)])
    (generator->lseq
     (^[] (if (zero? b)
            (eof-object)
            (receive (q r) (quotient&remainder a b)
              (set! a b) (set! b r)
              q))))))

;; More general version of rationalize.  Given real number x,
;; returns an exact rational number q such that
;;
;;   x - dx- <= q <= x + dx+   ;if open? is #f
;;   x - dx- <  q <  x + dx+   ;if open? is #t
;;
;; If you omit all of optional args, you can get the exact rational
;; representation of x.
(define (real->rational x :optional (dx+ #f) (dx- #f) (open? #f))

  ;; refine/* returns reverse list of continued fraction of
  ;; approximated rational.  /c is for closed interval, /o for open.
  (define (refine/c xn yn an)
    (cond [(null? (cdr xn)) `(,(if (null? (cdr yn))
                                 (min (car xn) (car yn))
                                 (car xn)))]
          [(null? (cdr yn)) `(,(car yn))]
          [(= (car xn) (car yn))
           (refine/c (cdr xn) (cdr yn) (cons (car xn) an))]
          [else (cons (+ 1 (min (car xn) (car yn))) an)]))

  (define (refine/o xn yn an)
    (cond [(null? (cdr xn))
           ;; There can be the following cases.
           ;; (1) X and Y are the same length
           ;;    X = [..., xn], Y = [..., yn], xn != yn
           ;;    If |xn-yn| > 1, Z = [..., min(xn,yn)+1]
           ;;    If |xn-yn| = 1, Z = [..., min(xn,yn), 2] *
           ;; (2) otherwise, Y = [..., yn, ym, ...].
           ;;    If yn > (xn)+1, Z = [..., (xn)+1]
           ;;    If yn = (xn)+1, Z = [..., xn, 2] *
           ;;    If yn = xn,     Z = [..., (xn)-1, 2] *
           ;;    If yn = (xn)-1, Z = [..., yn, (ym)+1] *
           ;;    If yn < (xn)-1, Z = [..., (yn)+1]
           ;; The * cases uses the fact that [..., an] (n>1) can also be
           ;; represented as [..., (an)-1, 1].
           (if (null? (cdr yn))
             (case (- (car xn) (car yn))
               [(-1 1) (cons* 2 (min (car xn) (car yn)) an)]
               [else   (cons (+ 1 (min (car xn) (car yn))) an)])
             (let ([x (car xn)] [y (car yn)])
               (cond [(> y (+ x 1)) (cons (+ x 1) an)]
                     [(= y (+ x 1)) (cons* 2 x an)]
                     [(= y x)       (cons* 2 (- x 1) an)]
                     [(= y (- x 1)) (cons* (+ (cadr yn) 1) y an)]
                     [else          (cons (+ y 1) an)])))]
          [(null? (cdr yn)) (refine/o yn xn an)]
          [(= (car xn) (car yn))
           (refine/o (cdr xn) (cdr yn) (cons (car xn) an))]
          [else (cons (+ 1 (min (car xn) (car yn))) an)]))

  (define (realize rcf) ; reverse continued fraction -> rational
    (let loop ([r (car rcf)] [as (cdr rcf)])
      (if (null? as) r (loop (+ (car as) (/ r)) (cdr as)))))

  (define (ensure-exact n) ;; dumb exact rationalize
    (if (exact? n)
      n
      (let1 v (decode-float n)
        (* (vector-ref v 2)              ; sign
           (vector-ref v 0)              ; mantissa
           (expt 2 (vector-ref v 1)))))) ; exponent

  ;; Error bounds: Most of the time, the number d represents
  ;; numbers between d + epsion/2 and d - epsilon/2, where epsilon
  ;; is the least quantum representable with the exponent of d.
  ;; The range is closed if mantissa is even, open if mantissa is odd.
  ;; There are one special case: If mantissa is exactly 2^52, then the
  ;; lower bound is d - epsilon/4.
  ;; NB: Denormalized numbers are considered in decode-float; that is,
  ;; exponent gets fixed at -1074 for all denomarlized numbers and just
  ;; mantissa decreases, so we can safely say the quantum is (expt 2 expo).
  (define (float-error-bounds d); returns upper, lower bounds and open?
    (let* ([v (decode-float d)]
           [mant (vector-ref v 0)]
           [expo (vector-ref v 1)]
           [sign (vector-ref v 2)]
           [open? (odd? mant)])
      (if (= mant 4503599627370496)     ;2^52
        (values (expt 2 (- expo 1)) (expt 2 (- expo 2)) open?)
        (values (expt 2 (- expo 1)) (expt 2 (- expo 1)) open?))))

  (define (find-rational-1 x dx+ dx- open?) ; x >= 0
    (let ([ub (+ (ensure-exact x) (ensure-exact dx+))]
          [lb (- (ensure-exact x) (ensure-exact dx-))])
      (cond [(< lb 0) 0]
            [(= ub lb)
             (if open?
               (error "real->rational: range can't be open when error boundary is zero")
               ub)]
            [else (realize ((if open? refine/o refine/c)
                            (continued-fraction ub)
                            (continued-fraction lb)
                            '()))])))

  (define (find-rational x dx+ dx-)     ; x >= 0
    (if (and (inexact? x) (or (not dx+) (not dx-)))
      (receive (dx+_ dx-_ open?) (float-error-bounds x)
        (find-rational-1 x (or dx+ dx+_) (or dx- dx-_) open?))
      (find-rational-1 x (or dx+ 0) (or dx- 0) open?)))

  (if (< x 0)
    (- (find-rational (- x) dx- dx+))
    (find-rational x dx+ dx-)))

;;
;; Some R6RS stuff
;;

(define (exact-integer-sqrt k)
  (unless (and (exact? k) (integer? k) (>= k 0))
    (error "exact nonnegative integer required, but got" k))
  ((with-module gauche.internal %exact-integer-sqrt) k))

;; Since Gauche coerces complex number whose imag part is 0.0i into
;; real, these procedures has no difference from real?, rational? and
;; integer?
(define real-valued?     real?)
(define rational-valued? rational?)
(define integer-valued?  integer?)

;; An extension of quotient&remainder to the real domain.  Their results
;; match with div/mod when divisor is positive.
(define (%div&mod x y)
  (define (edom what)
      (error "real number required for integer division, but got" what))
  (cond [(= y 0) (error "zero divisor is not allowed in integer division")]
        [(and (integer? x) (integer? y)) (quotient&remainder x y)]
        [(and (exact? x) (exact? y))    ;at least one of arg is rational
         (let1 q (truncate (/ x y)) (values q (- x (* q y))))]
        [(not (real? x)) (edom x)]
        [(not (real? y)) (edom y)]
        [else (receive (r q) (modf (/. x y)) (values q (- x (* q y))))]))

(define (div-and-mod x y)
  (if (>= x 0)
    (%div&mod x y)
    (receive (q r) (%div&mod x y)
      (if (= r 0)
        (values q r)
        (if (> y 0)
          (values (- q 1) (+ r y))
          (values (+ q 1) (- r y)))))))

(define (div0-and-mod0 x y)
  (receive (q r) (%div&mod x y)
    (if (>= x 0)
      (if (> y 0)
        (if (>= (* r 2) y)
          (values (+ q 1) (- r y))
          (values q r))
        (if (>= (* r 2) (- y))
          (values (- q 1) (+ r y))
          (values q r)))
      (if (> y 0)
        (if (< (* r 2) (- y))
          (values (- q 1) (+ r y))
          (values q r))
        (if (< (* r 2) y)
          (values (+ q 1) (- r y))
          (values q r))))))

(define (div x y)  (values-ref (div-and-mod x y) 0))
(define (mod x y)  (values-ref (div-and-mod x y) 1))
(define (div0 x y) (values-ref (div0-and-mod0 x y) 0))
(define (mod0 x y) (values-ref (div0-and-mod0 x y) 1))

;; Nearly equal comparison
;;  (Unofficial yet; see how it works)

(define (nearly=? tolerance x y)
  (< (abs (- x y))
     (/ (max (abs x) (abs y)) tolerance)))

