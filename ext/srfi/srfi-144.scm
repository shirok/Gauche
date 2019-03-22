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

          flonum #;fladjacent flcopysign make-flonum

          flinteger-fraction flexponent flinteger-exponent
          flnormalized-fraction-exponent flsign-bit

          ;; flonum? fl=? fl<? fl>? fl<=? fl>=?
          ;; flunordered? flinteger? flzero? flpositive? flnegative?
          ;; flodd? fleven? flfinite? flinfinite? flnan?
          ;; flnormalized? fldenormalized?

          ;; flmax flmin fl+ fl* fl+* fl- fl/ flabs flabsdiff
          ;; flposdiff flsign flnumerator fldenominator 
          ;; flfloor flceiling flround fltruncate

          ;; flexp flexp2 flexp-1 flsquare flcbrt flhypot flexpt fllog
          ;; fllog1+ fllog2 fllog10 make-fllog-base

          ;; flsin flcos fltan flasin flacos flatan
          ;; flsinh flcosh fltanh flasinh flacosh flatanh

          ;; flquotient flremainder flremquo 

          ;; flgamma flloggamma flfirst-bessel flsecond-bessel
          ;; flerf flerfc
          ))
(select-module srfi-144)

(inline-stub
 (.include <math.h>))

(define-cproc logb (x::<real>) ::<real> logb)
(define-cproc ilogb (x::<real>) ::<int> ilogb)

;;;
;;; Constants
;;;
(define-constant fl-e const:e)
(define-constant fl-1/e (/ const:e))
(define-constant fl-e-2 (* const:e  const:e))
(define-constant fl-e-pi/4 (%exp (/ const:pi 4)))
(define-constant fl-log2-e (/ (%log const:e) (%log 2)))
(define-constant fl-log10-e (/ (%log const:e) (%log 10)))
(define-constant fl-log-2 (%log 2))
(define-constant fl-1/log-2 (/ (%log 2)))
(define-constant fl-log-3 (%log 3))
(define-constant fl-log-pi (%log const:pi))
(define-constant fl-log-10 (%log 10))
(define-constant fl-1/log-10 (/ (%log 10)))
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
(define-constant fl-1/log-phi (/ fl-log-phi))
(define-constant fl-euler 0.5772156649015329)
(define-constant fl-e-euler (exp fl-euler))
(define-constant fl-sin-1 (%sin 1))
(define-constant fl-cos-1 (%cos 1))
(define-constant fl-gamma-1/2 (gamma 1/2))
(define-constant fl-gamma-1/3 (gamma 1/3))
(define-constant fl-gamma-2/3 (gamma 2/3))

(define-constant fl-greatest  (encode-float `#(,(- (expt 2 53) 1) 971 1)))
(define-constant fl-least     (encode-float '#(1 -1074 1)))
(define-constant fl-epsilon   (flonum-epsilon))
(define-constant fl-fast-fl+* #t)
(define-inline fl-integer-exponent-zero (ilogb 0))
(define-inline fl-integer-exponent-nan (ilogb +nan.0))

;;;
;;; constructors
;;;
(define-inline (flonum n) 
  (assume (real? n))
  (inexact n))

;; (fladjacent x y)

(define (flcopysign x y)
  (assume (flonum? x))
  (assume (flonum? y))
  (let ([vx (decode-float x)]
        [vy (decode-float y)])
    (encode-float `#(,(~ vx 0) ,(~ vx 1) ,(~ vy 2)))))

(define-inline (make-flonum x n) (ldexp x n))

;;;
;;; accessors
;;;

(define-inline (flinteger-fraction x) (modf x))
(define-inline (flexponent x) (logb x))
(define-inline (flinteger-exponent x) (ilogb 0))
(define-inline (flnormalized-fraction-exponent x) (frexp x))
(define-cproc flsign-bit (x::<real>) ::<int> signbit)
