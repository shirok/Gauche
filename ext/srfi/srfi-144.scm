;;;
;;; srfi-144 - flonums
;;;
;;;   Copyright (c) 2019  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi-144
  (use math.const :prefix const:)
  (export fl-e fl-1/e fl-e-2 fl-e-pi/4 fl-log2-e fl-log10-e
          fl-log-2 fl-1/log-2 fl-log-3 fl-log-pi fl-log-10 fl-1/log-10
          fl-pi fl-1/pi fl-2pi fl-pi/2 fl-pi/4 fl-pi-squared
          fl-degree fl-2/pi fl-2/sqrt-pi
          fl-sqrt-2 fl-sqrt-3 fl-sqrt-5 fl-sqrt-10 fl-1/sqrt-2
          fl-cbrt-2 fl-cbrt-3 fl-4thrt-2 fl-phi fl-log-phi
          fl-1/log-phi fl-euler fl-e-euler fl-sin-1 fl-cos-1
          fl-gamma-1/2 fl-gamma-1/3 fl-gamma-2/3
          
          fl-greatest fl-least fl-epsilon fl-fast-fl+*
          fl-integer-exponent-zero fl-integer-exponent-nan

          flonum fladjacent flcopysign make-flonum

          flinteger-fraction flexponent flinteger-exponent
          flnormalized-fraction-exponent flsign-bit

          flonum? fl=? fl<? fl>? fl<=? fl>=?
          flunordered? flinteger? flzero? flpositive? flnegative?
          flodd? fleven? flfinite? flinfinite? flnan?
          flnormalized? fldenormalized?

          flmax flmin fl+ fl* fl+* fl- fl/ flabs flabsdiff
          flposdiff flsgn flnumerator fldenominator 
          flfloor flceiling flround fltruncate

          flexp flexp2 flexp-1 flsquare flsqrt flcbrt flhypot flexpt fllog
          fllog1+ fllog2 fllog10 make-fllog-base

          flsin flcos fltan flasin flacos flatan
          flsinh flcosh fltanh flasinh flacosh flatanh

          flquotient flremainder flremquo 

          flgamma flloggamma flfirst-bessel flsecond-bessel
          flerf flerfc
          ))
(select-module srfi-144)

(inline-stub
 (.include <math.h>))

;;;
;;; Constants
;;;
(define-constant fl-e const:e)
(define-constant fl-1/e (/ const:e))
(define-constant fl-e-2 7.38905609893065) ; e*e yields 1ulp error
(define-constant fl-e-pi/4 (%exp (/ const:pi 4)))
(define-constant fl-log2-e (/ (%log 2)))
(define-constant fl-log10-e 0.4342944819032518); (/ (%log 10)) yiels 1ulp error
(define-constant fl-log-2 (%log 2))
(define-constant fl-1/log-2 (/ (%log 2)))
(define-constant fl-log-3 (%log 3))
(define-constant fl-log-pi (%log const:pi))
(define-constant fl-log-10 (%log 10))
(define-constant fl-1/log-10 0.4342944819032518); (/ (%log 10)) yiels 1ulp error
(define-constant fl-pi const:pi)
(define-constant fl-1/pi (/ const:pi))
(define-constant fl-2pi (* const:pi 2))
(define-constant fl-pi/2 const:pi/2)
(define-constant fl-pi/4 const:pi/4)
(define-constant fl-pi-squared (* const:pi const:pi))
(define-constant fl-degree const:pi/180)
(define-constant fl-2/pi (/ const:pi/2))
(define-constant fl-2/sqrt-pi (/ 2 (%sqrt const:pi)))
(define-constant fl-sqrt-2 (%sqrt 2))
(define-constant fl-sqrt-3 (%sqrt 3))
(define-constant fl-sqrt-5 (%sqrt 5))
(define-constant fl-sqrt-10 (%sqrt 10))
(define-constant fl-1/sqrt-2 (/ (%sqrt 2)))
(define-constant fl-cbrt-2 (%expt 2 1/3))
(define-constant fl-cbrt-3 (%expt 3 1/3))
(define-constant fl-4thrt-2 (%expt 2 1/4))
(define-constant fl-phi (/ (+ 1 (%sqrt 5)) 2))
(define-constant fl-log-phi (%log fl-phi))
(define-constant fl-1/log-phi 2.07808692123502753); (/ fl-log-phi) yields 1ulp error
(define-constant fl-euler 0.5772156649015329)
(define-constant fl-e-euler (exp fl-euler))
(define-constant fl-sin-1 (%sin 1))
(define-constant fl-cos-1 (%cos 1))
(define-constant fl-gamma-1/2 1.77245385090551603); (gamma 1/2) yields 1ulp error on MinGW
(define-constant fl-gamma-1/3 2.67893853470774763); (gamma 1/3) yields 1ulp error
(define-constant fl-gamma-2/3 (gamma 2/3))

(define-constant fl-greatest  (encode-float `#(,(- (expt 2 53) 1) 971 1)))
(define-constant fl-least     (encode-float '#(1 -1074 1)))
(define-constant fl-epsilon   (flonum-epsilon))
(define-constant fl-fast-fl+* #t)

;; these two needs to be 'define-inline', for ilogb can't be used
;; before loading this module.
(define-inline fl-integer-exponent-zero (ilogb 0))
(define-inline fl-integer-exponent-nan (ilogb +nan.0))

;;;
;;; constructors
;;;

;; See post-finalization note in srfi about returning +nan.0
(define-inline (flonum n) (if (real? n) (inexact n) +nan.0))

(define-cproc fladjacent (x::<real> y::<real>)
  ::<real> :constant :fast-flonum 
  (return (nextafter x y)))

(define-cproc flcopysign (x::<real> y::<real>)
  ::<real> :constant :fast-flonum
  (return (copysign x y)))
  
(define-inline (make-flonum x n) (ldexp x n))

;;;
;;; accessors
;;;

(define-cproc logb (x::<real>) ::<real> :constant :fast-flonum logb)
(define-cproc ilogb (x::<real>) ::<int> :constant :fast-flonum ilogb)

(define-inline (flinteger-fraction x) 
  (receive (r q) (modf x) (values q r)))
(define-inline (flexponent x) (logb x))
(define-inline (flinteger-exponent x) (ilogb x))
(define-inline (flnormalized-fraction-exponent x) (frexp x))
(define-cproc flsign-bit (x::<real>) ::<int> :constant :fast-flonum
  (return (?: (signbit x) 1 0)))

;;;
;;; predicates
;;;

;; flonum? - built-in

;; we don't check types of arguments in these; the point of flonum-specific
;; ops is speed, so it's less useful if these ones are slower than the
;; generic version.
(define-inline (fl=? . args)  (apply = args))
(define-inline (fl<? . args)  (apply < args))
(define-inline (fl<=? . args) (apply <= args))
(define-inline (fl>? . args)  (apply > args))
(define-inline (fl>=? . args) (apply >= args))

(define-inline (flunordered? x y) (or (nan? x) (nan? y)))
(define-inline (flinteger? x) (and (flonum? x) (integer? x)))
(define-inline (flzero? x) (and (flonum? x) (zero? x)))
(define-inline (flpositive? x) (and (flonum? x) (positive? x)))
(define-inline (flnegative? x) (and (flonum? x) (negative? x)))
(define-inline (flodd? x) (and (flonum? x) (odd? x)))
(define-inline (fleven? x) (and (flonum? x) (even? x)))
(define-inline (flfinite? x) (finite? x))
(define-inline (flinfinite? x) (infinite? x))
(define-inline (flnan? x) (nan? x))

(inline-stub
 ;; On MinGW 32bit, fpclassify is broken.
 ;; cf. https://github.com/shirok/Gauche/pull/465
 (define-cfn flonum_classify (d::double) ::int :static 
   (.if (and (defined __MINGW32__) (not (defined __MIGW64__)))
        (return (__builtin_fpclassify FP_INFINITE FP_NAN FP_NORMAL FP_SUBNORMAL
                                      FP_ZERO d))
        (return (fpclassify d)))))

(define-cproc flnormalized? (x::<real>) ::<boolean> :constant :fast-flonum
  (return (== (flonum_classify x) FP_NORMAL)))
(define-cproc fldenormalized? (x::<real>) ::<boolean> :constant :fast-flonum
  (return (== (flonum_classify x) FP_SUBNORMAL)))

;;;
;;; Arithmetic
;;;

;; Again, we don't check the argument types for the sake of the speed.

(define-inline (flmax . args) (if (null? args) -inf.0 (apply max args)))
(define-inline (flmin . args) (if (null? args) +inf.0 (apply min args)))

(define-inline (fl+ . args) (apply +. args))
(define-inline (fl* . args) (apply *. args))
(define-cproc fl+* (x::<real> y::<real> z::<real>) 
  ::<real> :fast-flonum :constant
  (return (fma x y z)))

(define-inline (fl- x . args) (apply -. x args))
(define-inline (fl/ x . args) (apply /. x args))

(define-cproc flabs (x::<real>) ::<real> :fast-flonum :constant fabs)
(define-cproc flabsdiff (x::<real> y::<real>) ::<real> :fast-flonum :constant
  (return (fabs (- x y))))
(define-cproc flposdiff (x::<real> y::<real>) ::<real> :fast-flonum :constant
  (return (?: (> x y) (- x y) 0.0)))
(define-cproc flsgn  (x::<real>) ::<real> :fast-flonum :constant
  (return (?: (signbit x) -1.0 1.0)))

(define (flnumerator x)
  (if (or (infinite? x) (nan? x) (zero? x))
    x ; -0.0 is handled here
    (inexact (numerator (exact x)))))

(define (fldenominator x)
  (cond [(or (infinite? x) (zero? x)) 1.0]
        [(nan? x) x]
        [else (inexact (denominator (exact x)))]))
  
(define-cproc flfloor (x::<real>) ::<real> :fast-flonum :constant floor)
(define-cproc flceiling (x::<real>) ::<real> :fast-flonum :constant ceil)
(define (flround x) (assume (flonum? x)) (round x))
(define-cproc fltruncate (x::<real>) ::<real> :fast-flonum :constant trunc)

(define-inline (flexp x) (%exp x))
(define-inline (flexp2 x) (%expt 2.0 x))
(define-cproc flexp-1 (x::<real>) ::<real> :fast-flonum :constant expm1)
(define-inline (flsquare x) (*. x x))
(define-inline (flsqrt x) (%sqrt x))
(define (flcbrt x)
  (assume (real? x))
  ;; (%expt x 1/3) may give us a complex root, so we roll our own.
  (if (or (nan? x) (infinite? x))
    x
    (let loop ([r (flcopysign (magnitude (%expt x 1/3)) x)])
      (if (zero? r)
        r
        (let1 r+ (- r (/ (- (* r r r) x)
                         (* 3 r r)))
          (if (= r r+)
            r
            (loop r+)))))))
(define-cproc flhypot (x::<real> y::<real>) ::<real> :fast-flonum :constant hypot)
(define-inline (flexpt x y) (%expt x y))
(define-inline (fllog x) (%log x))
(define-cproc fllog1+ (x::<real>) ::<real> :fast-flonum :constant log1p)
(define-cproc fllog2 (x::<real>) ::<real> :fast-flonum :constant log2)
(define-cproc fllog10 (x::<real>) ::<real> :fast-flonum :constant log10)
(define (make-fllog-base base) (^x (/. (%log x) (%log base))))

(define-inline (flsin x) (%sin x))
(define-inline (flcos x) (%cos x))
(define-inline (fltan x) (%tan x))
(define-inline (flasin x) (%asin x))
(define-inline (flacos x) (%acos x))
(define-inline (flatan y . x) (apply %atan y x))
(define-inline (flsinh x) (%sinh x))
(define-inline (flcosh x) (%cosh x))
(define-inline (fltanh x) (%tanh x))
(define-cproc flasinh (x::<real>) ::<real> :fast-flonum :constant asinh)
(define-cproc flacosh (x::<real>) ::<real> :fast-flonum :constant acosh)
(define-cproc flatanh (x::<real>) ::<real> :fast-flonum :constant atanh)

(define (flquotient x y) (fltruncate (/. x y)))
(define (flremainder x y) (-. x (*. y (flquotient x y))))
(define-cproc flremquo (x::<real> y::<real>) ::(<real> <int>)
  (let* ([quo::int]
         [rem::double (remquo x y (& quo))])
    (result rem quo)))

(define-inline (flgamma x) (gamma x))
(define (flloggamma x)
  (values (lgamma x)
          (cond [(<= 0 x) 1.0]
                [(infinite? x) +nan.0] ; sign of gamma(-inf.0) undecidable
                [(nan? x) +nan.0]
                [(odd? (floor x)) -1.0]
                [else 1.0])))
                  
(define-cproc flfirst-bessel (n::<int> x::<real>)
  ::<real> :fast-flonum :constant
  jn)
(define-cproc flsecond-bessel (n::<int> x::<real>) 
  ::<real> :fast-flonum :constant
  ;; As of 2019, NB: MinGW's yn returns NaN if x = 0, as opposed to the 
  ;; POSIX definition that says -HUGE_VAL.
  (.if (defined GAUCHE_WINDOWS)
       (if (== x 0.0)
         (return SCM_DBL_NEGATIVE_INFINITY)
         (return (yn n x)))
       (return (yn n x))))

(define-cproc flerf (x::<real>)
  ::<real> :fast-flonum :constant
  erf)
(define-cproc flerfc (x::<real>)
  ::<real> :fast-flonum :constant
  erfc)
    
