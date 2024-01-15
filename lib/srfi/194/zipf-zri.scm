;;;
;;; Grafted from srfi-194 reference implementation
;;; Original code by Linas Vepstas
;;;

(define-module srfi.194.zipf-zri
  (use srfi.194)
  (export make-zipf-generator)
  )
(select-module srfi.194.zipf-zri)

;
; zipf-zri.scm
; Create a Zipf random distribution.
;
; Created by Linas Vepstas 10 July 2020
; Nominated for inclusion in srfi-194
;
; Not optimized for speed!
;
; Implementation from ZRI algorithm presented in the paper:
; "Rejection-inversion to generate variates from monotone discrete
; distributions", Wolfgang Hörmann and Gerhard Derflinger
; ACM TOMACS 6.3 (1996): 169-184
;
; Hörmann and Derflinger use "q" everywhere, when they really mean "s".
; Thier "q" is not the standard q-series deformation. Its just "s".
; The notation in the code below differs from the article to reflect
; conventional usage.
;
;------------------------------------------------------------------
;
; The Hurwicz zeta distribution 1 / (k+q)^s for 1 <= k <= n integer
; The Zipf distribution is recovered by setting q=0.
;
; The exponent `s` must be a real number not equal to 1.
; Accuracy is diminished for |1-s|< 1e-6. The accuracy is roughly
; equal to 1e-15 / |1-s| where 1e-15 == 64-bit double-precision ULP.
;
(define (make-zipf-generator/zri n s q)

  ; The hat function h(x) = 1 / (x+q)^s
  (define (hat x)
    (expt (+ x q) (- s)))

  (define _1-s (- 1 s))
  (define oms (/ 1 _1-s))

  ; The integral of hat(x)
  ; H(x) = (x+q)^{1-s} / (1-s)
  ; Note that H(x) is always negative.
  (define (big-h x)
    (/ (expt (+ q x) _1-s) _1-s))

  ; The inverse function of H(x)
  ; H^{-1}(y) = -q + (y(1-s))^{1/(1-s)}
  (define (big-h-inv y)
    (- (expt (* y _1-s) oms) q))

  ; Lower and upper bounds for the uniform random generator.
  (define big-h-half (- (big-h 1.5) (hat 1)))
  (define big-h-n (big-h (+ n 0.5)))

  ; Rejection cut
  (define cut (- 1 (big-h-inv (- (big-h 1.5) (hat 1)))))

  ; Uniform distribution
  (define dist (make-random-real-generator big-h-half big-h-n))

  ; Attempt to hit the dartboard. Return #f if we fail,
  ; otherwise return an integer between 1 and n.
  (define (try)
    (define u (dist))
    (define x (big-h-inv u))
    (define kflt (floor (+ x 0.5)))
    (define k (exact kflt))
    (if (and (< 0 k)
             (or
               (<= (- k x) cut)
               (>= u (- (big-h (+ k 0.5)) (hat k))))) k #f))

  ; Did we hit the dartboard? If not, try again.
  (define (loop-until)
    (define k (try))
    (if k k (loop-until)))

  ; Return the generator.
  loop-until)

;------------------------------------------------------------------
;
; The Hurwicz zeta distribution 1 / (k+q)^s for 1 <= k <= n integer
; The Zipf distribution is recovered by setting q=0.
;
; The exponent `s` must be a real number close to 1.
; Accuracy is diminished for |1-s|> 2e-4. The accuracy is roughly
; equal to 0.05 * |1-s|^4 due to exp(1-s) being expanded to 4 terms.
;
; This handles the special case of s==1 perfectly.
(define (make-zipf-generator/one n s q)

  (define _1-s (- 1 s))

  ; The hat function h(x) = 1 / (x+q)^s
  ; Written for s->1 i.e. 1/(x+q)(x+q)^{s-1}
  (define (hat x)
    (define xpq (+ x q))
    (/ (expt xpq _1-s) xpq))

  ; Expansion of exn(y) = [exp(y(1-s))-1]/(1-s) for s->1
  ; Expanded to 4th order.
  ; Should equal this:
  ;;; (define (exn lg) (/ (- (exp (* _1-s lg)) 1) _1-s))
  ; but more accurate for s near 1.0
  (define (exn lg)
    (define (trm n u lg) (* lg (+ 1 (/ (* _1-s u) n))))
    (trm 2 (trm 3 (trm 4 1 lg) lg) lg))

  ; Expansion of lg(y) = [log(1 + y(1-s))] / (1-s) for s->1
  ; Expanded to 4th order.
  ; Should equal this:
  ;;; (define (lg y) (/ (log (+ 1 (* y _1-s))) _1-s))
  ; but more accurate for s near 1.0
  (define (lg y)
    (define yms (* y _1-s))
    (define (trm n u r) (- (/ 1 n) (* u r)))
    (* y (trm 1 yms (trm 2 yms (trm 3 yms (trm 4 yms 0))))))

  ; The integral of hat(x) defined at s==1
  ; H(x) = [exp{(1-s) log(x+q)} - 1]/(1-s)
  ; Should equal this:
  ;;;  (define (big-h x) (/ (- (exp (* _1-s (log (+ q x)))) 1)  _1-s))
  ; but expanded so that it's more accurate for s near 1.0
  (define (big-h x)
    (exn (log (+ q x))))

  ; The inverse function of H(x)
  ; H^{-1}(y) = -q + (1 + y(1-s))^{1/(1-s)}
  ; Should equal this:
  ;;; (define (big-h-inv y) (- (expt (+ 1 (* y _1-s)) (/ 1 _1-s)) q ))
  ; but expanded so that it's more accurate for s near 1.0
  (define (big-h-inv y)
    (- (exp (lg y)) q))

  ; Lower and upper bounds for the uniform random generator.
  (define big-h-half (- (big-h 1.5) (hat 1)))

  (define big-h-n (big-h (+ n 0.5)))

  ; Rejection cut
  (define cut (- 1 (big-h-inv (- (big-h 1.5) (/ 1 (+ 1 q))))))

  ; Uniform distribution
  (define dist (make-random-real-generator big-h-half big-h-n))

  ; Attempt to hit the dartboard. Return #f if we fail,
  ; otherwise return an integer between 1 and n.
  (define (try)
    (define u (dist))
    (define x (big-h-inv u))
    (define kflt (floor (+ x 0.5)))
    (define k (exact kflt))
    (if (and (< 0 k)
             (or
               (<= (- k x) cut)
               (>= u (- (big-h (+ k 0.5)) (hat k))))) k #f))

  ; Did we hit the dartboard? If not, try again.
  (define (loop-until)
    (define k (try))
    (if k k (loop-until)))

  ; Return the generator.
  loop-until)

;------------------------------------------------------------------
;
; (make-zipf-generator n [s [q]])
;
; The Hurwicz zeta distribution 1 / (k+q)^s for 1 <= k <= n integer
; The Zipf distribution is recovered by setting q=0.
; If `q` is not specified, 0 is assumed.
; If `s` is not specified, 1 is assumed.
;
; Valid for real -10 < s < 100 (otherwise overflows likely)
; Valid for real -0.5 < q < 2e8 (otherwise overflows likely)
; Valid for integer 1 <= k < int-max
;
; Example usage:
;    (define zgen (make-zipf-generator 50 1.01 0))
;    (generator->list zgen 10)
;
(define make-zipf-generator
  (case-lambda
    ((n)
     (make-zipf-generator n 1.0 0.0))
    ((n s)
     (make-zipf-generator n s 0.0))
    ((n s q)
     (if (< 1e-5 (abs (- 1 s)))
         (make-zipf-generator/zri n s q)
         (make-zipf-generator/one n s q)))))
