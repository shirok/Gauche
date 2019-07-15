;;;
;;; numerical.scm - auxiliary numerical functions - to be autoloaded
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
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
                                 (min (car xn) (+ 1 (car yn))))
                              ,@an)]
          [(null? (cdr yn)) `(,(min (car yn) (+ (car xn) 1))
                              ,@an)]
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
    (fold (^[a r] (+ a (/ r))) (car rcf) (cdr rcf)))

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

;; modulus exponent
(define (expt-mod n e m)  ; (modulo (expt n e) m)
  (if (and (exact-integer? n) (exact-integer? e) (exact-integer? m) (>= e 0))
    (if (< (* (integer-length n) e) (fixnum-width))
      (modulo (expt n e) m) ; in fixnum range, it's fast enough
      (let loop ([b (ash 1 (- (integer-length e) 1))]
                 [r 1])
        (cond [(zero? b) r]
              [(zero? (logand b e)) (loop (ash b -1) (modulo (* r r) m))]
              [else (loop (ash b -1) (modulo (* (modulo (* r r) m) n) m))])))
    (modulo (expt (inexact n) e) m))) ; inexact fallback

;; Gamma functions.  If the system provides tgamma/lgamma, we use it.
;; Otherwise we use an alternative implementation ported from John D Cook's
;; public domain C++ code (http://www.johndcook.com/stand_alone_code.html)
;; The test of this implementation is in test/number.scm, although it is
;; excluded by default.
(with-module gauche.internal
  (define (%alt-gamma x)
    (cond [(<= x 0.0)
           (cond [(zero? x) +inf.0] ;; Gamma(+0)
                 [(integer? x) +nan.0]
                 [else (/ (%alt-gamma (+ x 1)) x)])] ; slow. just for the completeness.
          [(< x 0.001)
           ;; for small x, Gamma(x) = x + g*x^2 + O(x^3) where g is Euler's
           ;; gamma constant.
           (/ (* x (+ 1.0 (* 0.577215664901532860606512090 x))))]
          [(< x 12)
           ;; we map the input into [1,2] and use polynomial approximation
           (let ([y (+ (fmod x 1.0) 1)]
                 [P '#(-1.71618513886549492533811e+0
                       +2.47656508055759199108314e+1
                       -3.79804256470945635097577e+2
                       +6.29331155312818442661052e+2
                       +8.66966202790413211295064e+2
                       -3.14512729688483675254357e+4
                       -3.61444134186911729807069e+4
                       +6.64561438202405440627855e+4)]
                 [Q '#(-3.08402300119738975254353e+1
                       +3.15350626979604161529144e+2
                       -1.01515636749021914166146e+3
                       -3.10777167157231109440444e+3
                       +2.25381184209801510330112e+4
                       +4.75584627752788110767815e+3
                       -1.34659959864969306392456e+5
                       -1.15132259675553483497211e+5)])
             (do ([i 0 (+ i 1)]
                  [z (- y 1)]
                  [numer 0.0 (* (+ numer (vector-ref P i)) z)]
                  [denom 1.0 (+ (* denom z) (vector-ref Q i))])
                 [(= i 8)
                  (let1 res (+ (/ numer denom) 1)
                    ;; remap the result to the original range
                    ;; using Gamma(z+1) = z*Gamma(z)
                    (cond [(< x 1) (/ res x)]
                          [(> x 2) (do ([i (- (floor x) 1) (- i 1)]
                                        [y y (+ y 1)]
                                        [res res (* y res)])
                                       [(= i 0) res])]
                          [else res]))]))]
          [else (exp (%alt-lgamma x))]))

  (define (%alt-lgamma x)
    (cond [(<= x 0.0)
           (if (or (zero? x) (integer? x))
             +inf.0
             (log (abs (%alt-gamma x))))] ; not accurate, just for completeness
          [(< x 12.0) (log (abs (%alt-gamma x)))]
          [else
           (let ([C '#(0.08333333333333333
                       -0.002777777777777778
                       7.936507936507937e-4
                       -5.952380952380953e-4
                       8.417508417508417e-4
                       -0.0019175269175269176                     
                       0.00641025641025641
                       -0.029550653594771242)]
                 [z (/ (*. x x))])
             (do ([i   7 (- i 1)]
                  [sum 0 (+ (* sum z) (vector-ref C i))])
                 [(< i 0)
                  (+ (* (- x 0.5) (log x))
                     (- 0.9189385332046728 x) ; 1/2*log(2*pi)
                     (/ sum x))]))]))
  )

(define gamma
  (global-variable-ref (find-module 'gauche.internal) '%gamma
                       (with-module gauche.internal %alt-gamma)))
(define lgamma
  (global-variable-ref (find-module 'gauche.internal) '%lgamma
                       (with-module gauche.internal %alt-lgamma)))

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

;; R7RS division operators
(define (floor/ x y)
  (receive (q r) (quotient&remainder x y)
    (if (>= x 0)
      (if (or (> y 0) (zero? r))
        (values q r)
        (values (- q 1) (+ r y)))
      (if (and (> y 0) (not (zero? r)))
        (values (- q 1) (+ r y))
        (values q r)))))
(define (floor-quotient x y)     (values-ref (floor/ x y) 0))
(define (floor-remainder x y)    (modulo x y))
(define (truncate/ x y)          (quotient&remainder x y))
(define (truncate-quotient x y)  (quotient x y))
(define (truncate-remainder x y) (remainder x y))

;; R7RS
(define (square x) (* x x))

;; reverse of decode-float, for the convenience
(define (encode-float vec)
  (define max-mantissa (- (%expt 2 53) 1))
  (define max-exponent (- 1024 53))
  (define min-exponent (- -1023 51))
  (unless (and (vector? vec)
               (= (vector-length vec) 3))
    (error "Vector of length 3 required, but got:" vec))
  (let ([mantissa (vector-ref vec 0)]
        [exponent (vector-ref vec 1)]
        [sign     (vector-ref vec 2)])
    
    (unless (<= min-exponent exponent max-exponent)
      (errorf "Exponent is out of range (must be between ~a and ~a: ~s"
              min-exponent max-exponent exponent))
    (case mantissa
      [(#f) +nan.0]
      [(#t) (if (< sign 0) -inf.0 +inf.0)]
      [else
       (unless (<= 0 mantissa max-mantissa)
         (error "Mantissa is out of range (must be between 0 and 2^53-1):"
                mantissa))
       (* sign (ldexp mantissa exponent))])))

;; Nearly equal comparison
;;  (Unofficial yet; see how it works)

(define (approx=? x y :optional (relative-tolerance (flonum-epsilon))
                                (absolute-tolerance (flonum-min-denormalized)))
  (cond [(eqv? x y) #t]
        [(or (not (finite? x)) (not (finite? y))) #f]
        [else
         (<= (abs (- x y))
             (max (* (max (abs x) (abs y)) relative-tolerance)
                  absolute-tolerance))]))

(define (nearly=? tolerance x y)
  (warn "nearly=? is deprecated; use approx=?")
  (< (abs (- x y))
     (/ (max (abs x) (abs y)) tolerance)))

