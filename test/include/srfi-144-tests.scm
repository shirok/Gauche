;;; Copyright (C) William D Clinger (2017).
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE. 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Tests these (srfi 144) constants and procedures:
;;;
;;;     fl-e
;;;     fl-1/e
;;;     fl-e-2
;;;     fl-e-pi/4
;;;     fl-log2-e
;;;     fl-log10-e
;;;     fl-log-2
;;;     fl-1/log-2
;;;     fl-log-3
;;;     fl-log-pi
;;;     fl-log-10
;;;     fl-1/log-10
;;;     fl-pi
;;;     fl-1/pi
;;;     fl-2pi
;;;     fl-pi/2
;;;     fl-pi/4
;;;     fl-2/sqrt-pi
;;;     fl-pi-squared
;;;     fl-degree
;;;     fl-2/pi
;;;     fl-sqrt-2
;;;     fl-sqrt-3
;;;     fl-sqrt-5
;;;     fl-sqrt-10
;;;     fl-1/sqrt-2
;;;     fl-cbrt-2
;;;     fl-cbrt-3
;;;     fl-4thrt-2
;;;     fl-phi
;;;     fl-log-phi
;;;     fl-1/log-phi
;;;     fl-euler
;;;     fl-e-euler
;;;     fl-sin-1
;;;     fl-cos-1
;;;     fl-gamma-1/2
;;;     fl-gamma-1/3
;;;     fl-gamma-2/3
;;;
;;;     fl-greatest
;;;     fl-least
;;;     fl-epsilon
;;;     fl-fast-fl+*
;;;     fl-integer-exponent-zero
;;;     fl-integer-exponent-nan
;;;
;;;     flonum
;;;     fladjacent
;;;     flcopysign
;;;     make-flonum
;;;
;;;     flinteger-fraction
;;;     flexponent
;;;     flinteger-exponent
;;;     flnormalized-fraction-exponent
;;;     flsign-bit
;;;
;;;     flonum?
;;;     fl=?
;;;     fl<?
;;;     fl>?
;;;     fl<=?
;;;     fl>=?
;;;     flunordered?
;;;     flmax
;;;     flmin
;;;     flinteger?
;;;     flzero?
;;;     flpositive?
;;;     flnegative?
;;;     flodd?
;;;     fleven?
;;;     flfinite?
;;;     flinfinite?
;;;     flnan?
;;;     flnormalized?
;;;     fldenormalized?
;;;
;;;     fl+
;;;     fl*
;;;     fl+*
;;;     fl-
;;;     fl/
;;;     flabs
;;;     flabsdiff
;;;     flposdiff
;;;     flsgn
;;;     flnumerator
;;;     fldenominator
;;;     flfloor
;;;     flceiling
;;;     flround
;;;     fltruncate
;;;
;;;     flexp
;;;     flexp2
;;;     flexp-1
;;;     flsquare
;;;     flsqrt
;;;     flcbrt
;;;     flhypot
;;;     flexpt
;;;     fllog
;;;     fllog1+
;;;     fllog2
;;;     fllog10
;;;     make-fllog-base
;;;
;;;     flsin
;;;     flcos
;;;     fltan
;;;     flasin
;;;     flacos
;;;     flatan
;;;     flsinh
;;;     flcosh
;;;     fltanh
;;;     flasinh
;;;     flacosh
;;;     flatanh
;;;
;;;     flquotient
;;;     flremainder
;;;     flremquo
;;;
;;;     flgamma
;;;     flloggamma
;;;     flfirst-bessel
;;;     flsecond-bessel
;;;     flerf
;;;     flerfc


(define-library (tests scheme flonum)
  (export run-flonum-tests)
  (import (scheme base)
          (srfi 144)
          (tests scheme test)
          (scheme inexact))

  (cond-expand
   ((library (scheme list))
    (import (only (scheme list) filter iota)))
   ((library (srfi 1))
    (import (only (srfi 1) filter iota)))
   (else
    (begin
     (define (filter p? x)
       (cond ((null? x)    x)
             ((p? (car x)) (cons (car x) (filter p? (cdr x))))
             (else         (filter p? (cdr x)))))
     (define (iota n)
       (do ((n (- n 1) (- n 1))
            (x '() (cons n x)))
           ((< n 0) x))))))

  (begin

   (define-syntax test-assert
     (syntax-rules ()
       ((test-assert expr)
        (test expr #t))))

   (define-syntax test-deny
     (syntax-rules ()
       ((test-assert expr)
        (test expr #f))))

   (define-syntax test-error
     (syntax-rules ()
       ((test-error expr)
        (test/unspec-or-exn expr &error))))

   (define-syntax test/=
     (syntax-rules ()
      ((test/= expr1 expr2)
       (test expr2 expr1))))

   ;; convenient values for test cases

   (define posints (map flonum '(1 2 3 4 5 10 65536 1e23)))
   (define nats (cons (flonum 0) posints))
   (define ints (append (map flonum '(-20 -8 -2 -1)) nats))
   (define posfracs (map flonum '(1/1000 1/10 1/3 1/2)))
   (define extremes
     (list (fl- fl-greatest) (fl- fl-least) fl-least fl-greatest))
   (define infinities (map flonum (list -inf.0 +inf.0)))
   (define weird (append infinities (list (flonum +nan.0))))

   (define somereals (append (map flonum
                                  (list (fl- fl-greatest)
                                        -10
                                        (fl- fl-least)
                                        0))
                             posfracs
                             posints))
   (define somereals+weird
     (append somereals weird))

   (define negzero (flonum -0.0))
   (define zero (flonum 0))
   (define one (flonum 1))
   (define two (flonum 2))

   (define neginf (flonum -inf.0))
   (define posinf (flonum +inf.0))
   (define nan (flonum +nan.0))


   (define (run-flonum-tests)

     ;; Mathematical constants

     (test/= 2.718281828459045235360287                    fl-e)
     (test/= .3678794411714423215955238                    fl-1/e)
     (test/= 7.389056098930650227230427                    fl-e-2)
     (test/= 2.1932800507380154566                         fl-e-pi/4)
     (test/= 1.4426950408889634073599246810018921374266    fl-log2-e)
     (test/= .4342944819032518276511289                    fl-log10-e)
     (test/= .6931471805599453094172321                    fl-log-2)
     (test/= 1.4426950408889634073599246810018921374266    fl-1/log-2)
     (test/= 1.0986122886681096913952452                   fl-log-3)
     (test/= 1.144729885849400174143427                    fl-log-pi)
     (test/= 2.3025850929940456840179915                   fl-log-10)
     (test/= 0.4342944819032518276511289189166050822944    fl-1/log-10)

     (test/= 3.1415926535897932384626433832795028841972    fl-pi)
     (test/= 0.3183098861837906715377675267450287240689    fl-1/pi)
     (test/= 6.283185307179586476925287                    fl-2pi)
     (test/= 1.570796326794896619231322                    fl-pi/2)
     (test/= .7853981633974483096156608                    fl-pi/4)
     (test/= .5641895835477562869480795                    (/ fl-2/sqrt-pi 2))
     (test/= 9.869604401089358618834491                    fl-pi-squared)
     (test/= 0.0174532925199432957692369076848861271344    fl-degree)
     (test/= .3183098861837906715377675                    (/ fl-2/pi 2))

     (test/= 1.4142135623730950488016887242096980785697    fl-sqrt-2)
     (test/= 1.7320508075688772935274463415058723669428    fl-sqrt-3)
     (test/= 2.2360679774997896964091736687311762354406    fl-sqrt-5)
     (test/= 3.1622776601683793319988935444327185337196    fl-sqrt-10)
     (test/= 1.4142135623730950488016887242096980785697    (* 2 fl-1/sqrt-2))
     (test/= 1.2599210498948731647672106072782283505703    fl-cbrt-2)
     (test/= 1.4422495703074083823216383107801095883919    fl-cbrt-3)
     (test/= 1.1892071150027210667174999705604759152930    fl-4thrt-2)

     (test/= 1.6180339887498948482045868343656381177203    fl-phi)
     (test/= 0.4812118250596034474977589134243684231352    fl-log-phi)
     (test/= 2.0780869212350275376013226061177957677422    fl-1/log-phi)
     (test/= 0.5772156649015328606065120900824024310422    fl-euler)
     (test/= 1.7810724179901979852365041031071795491696    fl-e-euler)

     (test/= 0.8414709848078965066525023216302989996226    fl-sin-1)
     (test/= 0.5403023058681397174009366074420766037323    fl-cos-1)

     (test/= 1.7724538509055160272981674833411451827975    fl-gamma-1/2)
     (test/= 2.6789385347077476336556929409746776441287    fl-gamma-1/3)
     (test/= 1.3541179394264004169452880281545137855193    fl-gamma-2/3)

     ;; Implementation Constants

     (test-assert (inexact? fl-greatest))
     (test-assert (inexact? fl-least))
     (test-assert (inexact? fl-epsilon))

     (test-assert (real? fl-greatest))
     (test-assert (real? fl-least))
     (test-assert (real? fl-epsilon))

     (test-assert (flonum? fl-greatest))
     (test-assert (flonum? fl-least))
     (test-assert (flonum? fl-epsilon))

     (test-assert (< 0.0
                     fl-least
                     fl-epsilon
                     1.0
                     (+ 1.0 fl-epsilon)
                     fl-greatest
                     posinf))
     (test-assert (= (* 2 fl-greatest) posinf))
     (test-assert (= 1 (/ (+ 1 (+ 1.0 fl-epsilon)) 2)))
     (test-assert (= 0.0 (/ fl-least 2)))

     (test-assert (boolean? fl-fast-fl+*))
     (test-assert (exact-integer? fl-integer-exponent-zero))
     (test-assert (exact-integer? fl-integer-exponent-nan))

     ;; Constructors

     (test (flonum 3) (flonum 3.0))
     (test (map flonum somereals) somereals)
     (test (map flonum weird) weird)

     (test (map fladjacent somereals somereals) somereals)
     (test (map fladjacent weird weird) weird)

     (test (fladjacent zero posinf) fl-least)
     (test (fladjacent zero neginf) (fl- fl-least))
     (test (fladjacent fl-least posinf) (fl+ fl-least fl-least))
     (test (fladjacent fl-least neginf) zero)
     (test (fladjacent (fl- fl-least) posinf) negzero)
     (test (fladjacent (fl- fl-least) neginf) (fl* -2.0 fl-least))

     (test (fladjacent zero one) fl-least)
     (test (fladjacent zero (fl- one)) (fl- fl-least))
     (test (fladjacent fl-least one) (fl+ fl-least fl-least))
     (test (fladjacent fl-least (fl- one)) zero)
     (test (fladjacent (fl- fl-least) one) negzero)
     (test (fladjacent (fl- fl-least) (fl- one)) (fl* -2.0 fl-least))

     (test (fl- (fladjacent one fl-greatest) one) fl-epsilon)
     (test (fl- one (fladjacent one zero)) (fl/ fl-epsilon 2.0))

     (test (fladjacent posinf zero) fl-greatest)
     (test (fladjacent neginf zero) (fl- fl-greatest))

     (test (flcopysign zero posinf) zero)
     (test (flcopysign zero neginf) negzero)
     (test (flcopysign zero one) zero)
     (test (flcopysign zero (fl- one)) negzero)
     (test (flcopysign one fl-least) one)
     (test (flcopysign one (fl- fl-greatest)) (fl- one))
     (test (flcopysign (fl- one) zero) one)
     (test (map flcopysign somereals somereals) somereals)
     (test (map flcopysign somereals (map fl- somereals))
           (map fl- somereals))
     (test (map flcopysign infinities infinities) infinities)
     (test (map flcopysign infinities (reverse infinities))
           (reverse infinities))

     (test (make-flonum zero 12) zero)
     (test (make-flonum zero -24) zero)
     (test (make-flonum zero 0) zero)
     (test (map make-flonum somereals (map (lambda (x) 0) somereals))
           somereals)
     (test (map make-flonum somereals (map (lambda (x) 2) somereals))
           (map (lambda (x) (fl* (flonum 4) x)) somereals))
     (test (map make-flonum somereals (map (lambda (x) -4) somereals))
           (map (lambda (x) (fl/ x (flonum 16))) somereals))
     (test (make-flonum fl-greatest 1) posinf)
     (test (make-flonum (fl- fl-greatest) 1) neginf)
     (test (make-flonum fl-greatest -1) (fl/ fl-greatest two))
     (test (make-flonum (fl- fl-greatest) -1) (fl- (fl/ fl-greatest two)))
     (test (make-flonum fl-least 1) (fl* two fl-least))
     (test (make-flonum (fl- fl-least) 1) (fl- (fl* two fl-least)))
     (test (make-flonum fl-least -1) zero)
     (test (make-flonum (fl- fl-least) -1) negzero)

     ;; Accessors

     (call-with-values
      (lambda () (flinteger-fraction 3.75))
      (lambda (q r)
        (test q (flonum 3))
        (test r (flonum .75))))

     (call-with-values
      (lambda () (flinteger-fraction -3.75))
      (lambda (q r)
        (test q (flonum -3))
        (test r (flonum -.75))))

     (test/= (flonum 12.0)
             (flexponent (flexpt two (flonum 12))))
     (test/approx (flexponent (flexpt two (flonum 12.5)))
                  (flonum 12))
     (test/= (flonum -5.0)
             (flexponent (flexpt two (flonum -5))))
     (test/approx (flexponent (flexpt two (flonum +4.5)))
                  (flonum +4))
     (test/approx (flexponent (flexpt two (flonum -4.5)))
                  (flonum -5))

     (test (flinteger-exponent (flexpt two (flonum 12)))   12)
     (test (flinteger-exponent (flexpt two (flonum 12.5))) 12)
     (test (flinteger-exponent (flexpt two (flonum -5)))   -5)
     (test (flinteger-exponent (flexpt two (flonum -4.5))) -5)

     (let* ((correct?
             (lambda (x y n)
               (or (fl=? x (* y (expt two n)))
                   (fl=? x (* 4.00 y (expt two (- n 2))))
                   (fl=? x (* 0.25 y (expt two (+ n 2)))))))
            (test-flnormalized-fraction-exponent
             (lambda (x)
               (call-with-values
                (lambda () (flnormalized-fraction-exponent x))
                (lambda (y n)
                  (list (flonum? y)
                        (exact-integer? n)
                        (fl<=? (flonum 0.5) (flabs y))
                        (fl<? (flabs y) one)
                        (correct? x y n)))))))
       (test (test-flnormalized-fraction-exponent zero)
             '(#t #t #f #t #t))
       (test (test-flnormalized-fraction-exponent negzero)
             '(#t #t #f #t #t))
       (test (test-flnormalized-fraction-exponent one)
             '(#t #t #t #t #t))
       (test (test-flnormalized-fraction-exponent two)
             '(#t #t #t #t #t))
       (test (test-flnormalized-fraction-exponent fl-least)
             '(#t #t #t #t #t))
       (test (test-flnormalized-fraction-exponent fl-greatest)
             '(#t #t #t #t #t))
       (test (test-flnormalized-fraction-exponent (fl- fl-least))
             '(#t #t #t #t #t))
       (test (test-flnormalized-fraction-exponent (fl- fl-greatest))
             '(#t #t #t #t #t))
       (test (test-flnormalized-fraction-exponent posinf)
             '(#t #t #t #t #t))
       (test (test-flnormalized-fraction-exponent neginf)
             '(#t #t #t #t #t))
       (test (test-flnormalized-fraction-exponent nan)
             '(#t #t #f #f #f))

       )

     (test (flsign-bit one) 0)
     (test (flsign-bit zero) 0)
     (test (flsign-bit negzero) 1)
     (test (flsign-bit (flonum -2)) 1)
     (test (flsign-bit posinf) 0)
     (test (flsign-bit neginf) 1)

     ;; Predicates

     (let ((alltrue  (map (lambda (x) #t) somereals))
           (allfalse (map (lambda (x) #f) somereals)))

       (test (map flonum? somereals) alltrue)
       (test (map flonum? weird) '(#t #t #t))

       (test-deny   (fl=? zero fl-least))
       (test-assert (fl=? fl-least fl-least))
       (test-deny   (fl=? one fl-least))
       (test-assert (fl=? neginf neginf))
       (test-deny   (fl=? neginf posinf))
       (test-deny   (fl=? posinf neginf))
       (test-assert (fl=? posinf posinf))
       (test-deny   (fl=? zero nan))
       (test-deny   (fl=? nan one))
       (test (map fl=? somereals somereals) alltrue)
       (test (map fl=? somereals (cdr somereals)) (cdr allfalse))
       (test (map fl=? (cdr somereals) somereals) (cdr allfalse))

       (test-assert (fl<? zero fl-least))
       (test-deny   (fl<? fl-least fl-least))
       (test-deny   (fl<? one fl-least))
       (test-deny   (fl<? neginf neginf))
       (test-assert (fl<? neginf posinf))
       (test-deny   (fl<? posinf neginf))
       (test-deny   (fl<? posinf posinf))
       (test-deny   (fl<? zero nan))
       (test-deny   (fl<? nan one))
       (test (map fl<? somereals somereals) allfalse)
       (test (map fl<? somereals (cdr somereals)) (cdr alltrue))
       (test (map fl<? (cdr somereals) somereals) (cdr allfalse))

       (test-deny   (fl>? zero fl-least))
       (test-deny   (fl>? fl-least fl-least))
       (test-assert (fl>? one fl-least))
       (test-deny   (fl>? neginf neginf))
       (test-deny   (fl>? neginf posinf))
       (test-assert (fl>? posinf neginf))
       (test-deny   (fl>? posinf posinf))
       (test-deny   (fl>? zero nan))
       (test-deny   (fl>? nan one))
       (test (map fl>? somereals somereals) allfalse)
       (test (map fl>? somereals (cdr somereals)) (cdr allfalse))
       (test (map fl>? (cdr somereals) somereals) (cdr alltrue))

       (test-assert (fl<=? zero fl-least))
       (test-assert (fl<=? fl-least fl-least))
       (test-deny   (fl<=? one fl-least))
       (test-assert (fl<=? neginf neginf))
       (test-assert (fl<=? neginf posinf))
       (test-deny   (fl<=? posinf neginf))
       (test-assert (fl<=? posinf posinf))
       (test-deny   (fl<=? zero nan))
       (test-deny   (fl<=? nan one))
       (test (map fl<=? somereals somereals) alltrue)
       (test (map fl<=? somereals (cdr somereals)) (cdr alltrue))
       (test (map fl<=? (cdr somereals) somereals) (cdr allfalse))

       (test-deny   (fl>=? zero fl-least))
       (test-assert (fl>=? fl-least fl-least))
       (test-assert (fl>=? one fl-least))
       (test-assert (fl>=? neginf neginf))
       (test-deny   (fl>=? neginf posinf))
       (test-assert (fl>=? posinf neginf))
       (test-assert (fl>=? posinf posinf))
       (test-deny   (fl>=? zero nan))
       (test-deny   (fl>=? nan one))
       (test (map fl>=? somereals somereals) alltrue)
       (test (map fl>=? somereals (cdr somereals)) (cdr allfalse))
       (test (map fl>=? (cdr somereals) somereals) (cdr alltrue))

       (test-deny   (flunordered? zero fl-least))
       (test-deny   (flunordered? fl-least fl-least))
       (test-deny   (flunordered? one fl-least))
       (test-deny   (flunordered? neginf neginf))
       (test-deny   (flunordered? neginf posinf))
       (test-deny   (flunordered? posinf neginf))
       (test-deny   (flunordered? posinf posinf))
       (test-assert (flunordered? zero nan))
       (test-assert (flunordered? nan one))
       (test (map flunordered? somereals somereals) allfalse)
       (test (map flunordered? somereals (cdr somereals)) (cdr allfalse))
       (test (map flunordered? (cdr somereals) somereals) (cdr allfalse))

       )

     (test (flmax) neginf)
     (test (flmax zero) zero)
     (test (flmax zero one) one)
     (test (flmax one zero) one)
     (test (apply flmax somereals) (car (reverse somereals)))

     (test (flmin) posinf)
     (test (flmin one) one)
     (test (flmin zero one) zero)
     (test (flmin one zero) zero)
     (test (apply flmin somereals) (car somereals))

     (test (map flinteger? somereals)
           (map fl=?
                somereals
                (map flround somereals)))

     (test-deny   (flzero? neginf))
     (test-deny   (flzero? (fl- fl-least)))
     (test-assert (flzero? negzero))
     (test-assert (flzero? zero))
     (test-deny   (flzero? fl-least))
     (test-deny   (flzero? posinf))

     (test-deny   (flpositive? neginf))
     (test-deny   (flpositive? (fl- fl-least)))
     (test-deny   (flpositive? negzero))
     (test-deny   (flpositive? zero))
     (test-assert (flpositive? fl-least))
     (test-assert (flpositive? posinf))

     (test-assert (flnegative? neginf))
     (test-assert (flnegative? (fl- fl-least)))
     (test-deny   (flnegative? negzero))    ; explicit in SRFI 144
     (test-deny   (flnegative? zero))
     (test-deny   (flnegative? fl-least))
     (test-deny   (flnegative? posinf))

     (test-deny   (flodd? zero))
     (test-assert (flodd? one))

     (test-assert (fleven? zero))
     (test-deny   (fleven? one))

     (test (map flfinite? somereals)
           (map (lambda (x) #t) somereals))
     (test (map flfinite? weird)
           (map (lambda (x) #f) weird))

     (test-assert (flinfinite? neginf))
     (test-assert (flinfinite? posinf))
     (test-deny   (flinfinite? nan))
     (test (map flinfinite? somereals)
           (map (lambda (x) #f) somereals))

     (test-deny   (flnan? neginf))
     (test-deny   (flnan? posinf))
     (test-assert (flnan? nan))
     (test (map flnan? somereals)
           (map (lambda (x) #f) somereals))

     (test-assert (flnormalized? fl-greatest))
     (test-deny   (flnormalized? fl-least))
     (test-deny   (fldenormalized? fl-greatest))
     (test-assert (fldenormalized? fl-least))

     ;; Arithmetic

     (test (fl+) zero)
     (test (fl+ zero) zero)
     (test (flzero? (fl+ negzero)) #t)
     (test (fl+ one) one)
     (test (fl+ one one) two)
     (test (fl+ nan one) nan)
     (test (fl+ one nan) nan)
     (test (map fl+ somereals somereals somereals)
           (map (lambda (x) (fl* (flonum 3) x))
                somereals))
     (test (map fl+ infinities infinities) infinities)
     (test (map flnan?
                (map fl+ infinities (reverse infinities)))
           (map (lambda (x) #t) infinities))
     
     (test (fl*) one)
     (test (fl* zero) zero)
     (test (flzero? (fl* negzero)) #t)
     (test (fl* one) one)
     (test (fl* one one) one)
     (test (fl* nan one) nan)
     (test (fl* one nan) nan)
     (test (map fl* somereals somereals somereals)
           (map (lambda (x) (flonum (expt x 3)))
                somereals))
     (test (map fl* infinities infinities)
           (map (lambda (x) posinf) infinities))
     (test (map fl* infinities (reverse infinities))
           (map (lambda (x) neginf) infinities))
     
     (let ((three (flonum 3))
           (four  (flonum 4))
           (five  (flonum 5))
           (x23   (flonum 23))
           (ten11 (flonum (expt 10 11)))
           (ten12 (flonum (expt 10 12))))
       (test (fl+* four five three) x23)
       (test (fl+* ten11 ten12 one)
             (flonum (+ (* (exact ten11) (exact ten12)) (exact one))))
       (test (fl+* ten11 ten12 (fl- one))
             (flonum (+ (* (exact ten11) (exact ten12)) (exact (fl- one)))))

       ;; FIXME: the following test assumes IEEE double precision,
       ;; in which (expt 10 23) lies exactly halfway between the
       ;; two nearest flonums.

       (test-deny (fl=? (fl+* ten11 ten12 one)
                        (fl+* ten11 ten12 (fl- one)))))

     (test-assert (flnan? (fl+* zero posinf one)))
     (test-assert (flnan? (fl+* zero neginf one)))
     (test-assert (flnan? (fl+* posinf zero one)))
     (test-assert (flnan? (fl+* neginf zero one)))
     (test-assert (flnan? (fl+* zero posinf nan)))
     (test-assert (flnan? (fl+* zero neginf nan)))
     (test-assert (flnan? (fl+* posinf zero nan)))
     (test-assert (flnan? (fl+* neginf zero nan)))
     (test (fl+* fl-greatest fl-greatest neginf) neginf)
     (test (fl+* fl-greatest (fl- fl-greatest) posinf) posinf)
     (test-assert (flnan? (fl+* nan one one)))
     (test-assert (flnan? (fl+* one nan one)))
     (test-assert (flnan? (fl+* one one nan)))

     (test (fl- zero) negzero)
     (test (fl- negzero) zero)
     (test (fl- one) (flonum -1))
     (test (fl- one one) zero)
     (test (fl- nan one) nan)
     (test (fl- one nan) nan)
     (test (map fl- somereals somereals somereals)
           (map (lambda (x) (if (eqv? x zero) zero (fl- x)))
                somereals))
     (test (map flnan? (map fl- infinities infinities))
           '(#t #t))
     (test (map fl- infinities (reverse infinities))
           infinities)
     
     (test (fl/ zero) posinf)
     (test (fl/ negzero) neginf)
     (test (fl/ one) one)
     (test (fl/ one one) one)
     (test (fl/ nan one) nan)
     (test (fl/ one nan) nan)
     (test (map fl/ somereals somereals somereals)
           (map (lambda (x) (if (flzero? x) (fl/ zero zero) (fl/ x)))
                somereals))
     (test (map flnan? (map fl/ infinities infinities))
           '(#t #t))
     (test (map flnan? (map fl/ infinities (reverse infinities)))
           '(#t #t))
     
     (test (flabs zero) zero)
     (test (flabs negzero) zero)
     (test (flabs one) one)
     (test (flabs (flonum -5.25)) (flonum 5.25))
     
     (test (flabsdiff zero one) one)
     (test (flabsdiff one zero) one)
     (test (flabsdiff one one) zero)
     (test (flabsdiff posinf neginf) posinf)
     (test (flabsdiff neginf posinf) posinf)
#|
     (test (flposdiff zero one) zero)
     (test (flposdiff one zero) one)
     (test (flposdiff one one) zero)
     (test (flposdiff posinf neginf) posinf)
     (test (flposdiff neginf posinf) posinf)
|#
     (test (flsgn posinf) one)
     (test (flsgn neginf) (fl- one))
     (test (flsgn zero) one)
     (test (flsgn negzero) (fl- one))
     (test (flsgn two) one)
     (test (flsgn (fl- two)) (fl- one))

     (test (flnumerator (flonum 2.25)) (flonum 9))
     (test (fldenominator (flonum 2.25)) (flonum 4))
     (test (flnumerator (flonum -2.25)) (flonum -9))
     (test (fldenominator (flonum -2.25)) (flonum 4))
     (test (map flnumerator ints) ints)
     (test (map fldenominator ints)
           (map (lambda (x) one) ints))
     (test (map flnumerator weird) weird)
     (test (map fldenominator infinities) (list one one))
     (test-assert (flnan? (flnumerator nan)))
     (test-assert (flnan? (fldenominator nan)))

     (test (flfloor    (flonum -3.125)) (flonum -4))
     (test (flceiling  (flonum -3.125)) (flonum -3))
     (test (flround    (flonum -3.125)) (flonum -3))
     (test (fltruncate (flonum -3.125)) (flonum -3))

     (test (flfloor    (flonum -3.75)) (flonum -4))
     (test (flceiling  (flonum -3.75)) (flonum -3))
     (test (flround    (flonum -3.75)) (flonum -4))
     (test (fltruncate (flonum -3.75)) (flonum -3))

     (test (flfloor    (flonum -3.5)) (flonum -4))
     (test (flceiling  (flonum -3.5)) (flonum -3))
     (test (flround    (flonum -3.5)) (flonum -4))
     (test (fltruncate (flonum -3.5)) (flonum -3))

     (test (map flfloor    ints) ints)
     (test (map flceiling  ints) ints)
     (test (map flround    ints) ints)
     (test (map fltruncate ints) ints)

     (test (map flfloor    posfracs) (map (lambda (x) zero) posfracs))
     (test (map flceiling  posfracs) (map (lambda (x) one) posfracs))
     (test (map flround    posfracs) (map (lambda (x) zero) posfracs))
     (test (map fltruncate posfracs) (map (lambda (x) zero) posfracs))

     (test (map flfloor    weird) weird)
     (test (map flceiling  weird) weird)
     (test (map flround    weird) weird)
     (test (map fltruncate weird) weird)

     ;; Exponents and logarithms

     (test (flexp negzero) one)
     (test (flexp zero) one)
     (test (flexp one) fl-e)
     (test/approx (flexp (fl- one)) fl-1/e)
     (test/approx (flexp two) fl-e-2)
     (test/approx (flexp fl-pi/4) fl-e-pi/4)
     (test (flexp posinf) posinf)
     (test (flexp fl-greatest) posinf)
     (test/approx (flexp fl-least) one)
     (test/approx (flexp (fl- fl-greatest)) zero)
     (test/approx (flexp neginf) zero)

     (test (fl+ one (flexp-1 negzero)) one)
     (test (fl+ one (flexp-1 zero)) one)
     (test (fl+ one (flexp-1 one)) fl-e)
     (test/approx (fl+ one (flexp-1 (fl- one))) fl-1/e)
     (test/approx (fl+ one (flexp-1 two)) fl-e-2)
     (test/approx (fl+ one (flexp-1 fl-pi/4)) fl-e-pi/4)
     (test (fl+ one (flexp-1 posinf)) posinf)
     (test (fl+ one (flexp-1 fl-greatest)) posinf)
     (test/approx (fl+ one (flexp-1 fl-least)) one)
     (test/approx (fl+ one (flexp-1 (fl- fl-greatest))) zero)
     (test/approx (fl+ one (flexp-1 neginf)) zero)

     (test (flexp2 negzero) one)
     (test (flexp2 zero) one)
     (test (flexp2 one) two)
     (test (flexp2 (fl- one)) (fl/ two))
     (test (flexp2 two) (fl* two two))
     (test/approx (flexp2 fl-log2-e) fl-e)
     (test/approx (flexp2 fl-log2-e) fl-e)
     (test (flexp2 posinf) posinf)
     (test (flexp2 fl-greatest) posinf)
     (test/approx (flexp2 fl-least) one)
     (test/approx (flexp2 (fl- fl-greatest)) zero)
     (test/approx (flexp2 neginf) zero)

     (test (flsquare zero) zero)
     (test (flsquare one) one)
     (test (flsquare two) (fl+ two two))
     (test/approx (flsquare fl-sqrt-2) two)
     (test/approx (flsquare fl-sqrt-3) (flonum 3))
     (test/approx (flsquare fl-sqrt-5) (flonum 5))
     (test/approx (flsquare fl-sqrt-10) (flonum 10))
     (test (flsquare (flonum -5)) (flonum 25))
     (test (flsquare neginf) posinf)
     (test (flsquare posinf) posinf)

     (test (flsqrt zero) zero)
     (test (flsqrt one) one)
     (test/approx (flsqrt two) fl-sqrt-2)
     (test/approx (flsqrt (flonum 3)) fl-sqrt-3)
     (test/approx (flsqrt (flonum 5)) fl-sqrt-5)
     (test/approx (flsqrt (flonum 10)) fl-sqrt-10)
     (test/approx (flsqrt (flonum 698)) (flonum 26.419689627245813))
     (test (flsqrt posinf) posinf)

     (test (flcbrt zero) zero)
     (test (flcbrt one) one)
     (test/approx (flcbrt two) fl-cbrt-2)
     (test/approx (flcbrt (flonum 3)) fl-cbrt-3)
     (test/approx (flcbrt (flonum 698)) (flonum 8.8705757224791313))
     (test/approx (flcbrt (flonum 11.390625)) (flonum 2.25))
     (test/approx (flcbrt (flonum -11.390625)) (flonum -2.25))
     (test (flcbrt posinf) posinf)
     (test (flcbrt neginf) neginf)

     (test (flhypot zero zero) zero)
     (test (flhypot zero one) one)
     (test/approx (flhypot two one) fl-sqrt-5)
     (test/approx (flhypot (fl- two) one) fl-sqrt-5)
     (test/approx (flhypot two (fl- one)) fl-sqrt-5)
     (test/approx (flhypot (fl- two) (fl- one)) fl-sqrt-5)
     (test/approx (flhypot (fl/ fl-greatest two) (fl/ fl-greatest two))
                  (fl/ fl-greatest fl-sqrt-2))
     (test (flhypot zero posinf) posinf)
     (test (flhypot neginf zero) posinf)

     (test (flexpt two zero) one)
     (test (flexpt two one) two)
     (test (flexpt two two) (flonum 4))
     (test/approx (flexpt two (fl/ two)) fl-sqrt-2)
     (test/approx (flexpt (flonum 441) (flonum 10))
                  (flonum 2.7821842944695155e26))
     (test/approx (flexpt (flonum 441) (fl/ (flonum 5)))
                  (flonum 3.37977444523542851))
     (for-each (lambda (x)
                 (for-each (lambda (frac)
                             (test/approx (flexpt (flexpt x frac) (fl/ frac))
                                          x))
                           posfracs))
               (filter flpositive? somereals))

     (test (fllog zero) neginf)
     (test (fllog one) zero)
     (test/approx (fllog two) fl-log-2)
     (test/approx (fllog (flonum 3)) fl-log-3)
     (test/approx (fllog fl-pi) fl-log-pi)
     (test/approx (fllog (flonum 10)) fl-log-10)
     (test (fllog posinf) posinf)
     (for-each (lambda (x)
                 (test/approx (flexp (fllog x)) x))
               (filter flpositive? somereals))

     (test (fllog2 zero) neginf)
     (test (fllog2 one) zero)
     (test (fllog2 two) one)
     (test/approx (fllog2 fl-e) fl-log2-e)
     (test (fllog2 posinf) posinf)
     (for-each (lambda (x)
                 (test/approx (flexpt two (fllog2 x)) x))
               (filter flpositive? somereals))

     (test (fllog10 zero) neginf)
     (test (fllog10 one) zero)
     (test/approx (fllog10 fl-e) fl-log10-e)
     (test (fllog10 (flonum 10)) one)
     (test (fllog10 posinf) posinf)
     (for-each (lambda (x)
                 (test/approx (flexpt (flonum 10) (fllog10 x)) x))
               (filter flpositive? somereals))

     (test-assert (flpositive? (fllog1+ fl-least)))
     (test (fllog1+ (fl- zero one)) neginf)
     (test (fllog1+ (fl- one one)) zero)
     (test/approx (fllog1+ (fl- two one)) fl-log-2)
     (test/approx (fllog1+ (fl- (flonum 3) one)) fl-log-3)
     (test/approx (fllog1+ (fl- fl-pi one)) fl-log-pi)
     (test/approx (fllog1+ (fl- (flonum 10) one)) fl-log-10)
     (test (fllog1+ (fl- posinf one)) posinf)
     (for-each (lambda (x)
                 (test/approx (flexp (fllog1+ (fl- x one))) x))
               (filter flpositive? somereals))

     (test/approx ((make-fllog-base two) fl-e) fl-log2-e)
     (test/approx ((make-fllog-base (flonum 10)) fl-e) fl-log10-e)
     (test/approx ((make-fllog-base fl-e) two) fl-log-2)
     (for-each (lambda (base)
                 (let ((f (make-fllog-base base)))
                   (for-each (lambda (x)
                               (test/approx (flexpt (flonum base) (f x)) x))
                             (filter flpositive? somereals))))
               (map flonum '(3 7 19)))

     ;; Trigonometric functions

     (test/approx (flsin zero)           zero)
     (test/approx (flcos zero)           one)
     (test/approx (fltan zero)           zero)
     (test/approx (flsin (flonum 0.2))   0.19866933079506121545941)
     (test/approx (flcos (flonum 0.2))   0.98006657784124163112420)
     (test/approx (flsin (flonum 0.5))   0.47942553860420300027329)
     (test/approx (flcos (flonum 0.5))   0.87758256189037271611628)
     (test/approx (flsin (flonum 0.7))   0.64421768723769105367261)
     (test/approx (flcos (flonum 0.7))   0.76484218728448842625586)
     (test/approx (flsin fl-pi/4)        fl-1/sqrt-2)
     (test/approx (flcos fl-pi/4)        fl-1/sqrt-2)
     (test/approx (flsin one)            0.84147098480789651665250)
     (test/approx (flcos one)            0.54030230586813971740094)
     (test/approx (flsin fl-pi/2)        one)
     (test/approx (flcos fl-pi/2)        zero)
     (test/approx (flsin two)            0.90929742682568169539602)
     (test/approx (flcos two)            -0.41614683654714238699757)
     (test/approx (flsin (flonum 3))     0.14112000805986722210074)
     (test/approx (flcos (flonum 3))     -0.98999249660044545727157)
     (test/approx (flsin fl-pi)          zero)
     (test/approx (flcos fl-pi)          (fl- one))
     (test/approx (flsin fl-2pi)         zero)
     (test/approx (flcos fl-2pi)         one)
     (test/approx (flsin (flonum 35))    -0.42818266949615100440675)
     (test/approx (flcos (flonum 35))    -0.90369220509150675984730)

     (for-each (lambda (x)
                 (test/approx (flsin x) (fl- (flsin (fl- x))))
                 (test/approx (flcos x) (flcos (fl- fl-2pi x)))
                 (test/approx (fltan x) (fl/ (flsin x) (flcos x)))
                 (test/approx (flhypot (flsin x) (flcos x)) one))
               (filter (lambda (x)
                         (and (flnormalized? x)
                              (fl<? (flabs x) (flonum 10000))))
                       (append somereals posfracs)))

     (for-each (lambda (x)
                 (test/approx (flsin (flasin x))     x)
                 (test/approx (flcos (flacos x))     x))
               (filter (lambda (x) (fl<=? (fl- one) x one))
                       (append somereals posfracs)))


     (let ((xs (filter (lambda (x)
                         (and (flnormalized? x)
                              (fl<? (flabs x) (flonum 10000))))
                       (append somereals posfracs))))
       (for-each (lambda (x)
                   (let ((theta (flatan x)))
                     (test/approx (fl/ (flsin theta) (flcos theta)) x))
                   (for-each (lambda (y)
                               (let ((theta (flatan x)))
                                 (test/approx (flatan (flsin theta)
                                                      (flcos theta))
                                              theta)))
                             xs))
                 xs))

     (test/approx (flsinh zero)           zero)
     (test/approx (flcosh zero)           one)
     (test/approx (flsinh (flonum 0.2))   0.201336002541094)
     (test/approx (flcosh (flonum 0.2))   1.020066755619076)
     (test/approx (flsinh (flonum 0.5))   0.521095305493747)
     (test/approx (flcosh (flonum 0.5))   1.127625965206381)
     (test/approx (flsinh (flonum 0.7))   0.758583701839534)
     (test/approx (flcosh (flonum 0.7))   1.255169005630943)
     (test/approx (flsinh one)            1.175201193643801)
     (test/approx (flcosh one)            1.543080634815244)
     (test/approx (flsinh two)            3.626860407847019)
     (test/approx (flcosh two)            3.762195691083631)
     (test/approx (flsinh (flonum 3))     10.01787492740990)
     (test/approx (flcosh (flonum 3))     10.06766199577777)
     (test/approx (flsinh (flonum 10))    11013.23287470339)
     (test/approx (flcosh (flonum 10))    11013.23292010332)

     (for-each (lambda (x)
                 (test/approx (flasinh (flsinh x)) x)
                 (test/approx (flacosh (flcosh x)) (flabs x))
                 (test/approx (flatanh (fltanh x)) x))
               (filter (lambda (x)
                         (fl<? (flabs x) (flonum 100)))
                       (append somereals posfracs (map fl- posfracs))))

     ;; Integer division

     (test (flquotient  (flonum 9.75) (flonum 0.5))     (flonum 19))
     (test (flremainder (flonum 9.75) (flonum 0.5))     (flonum 0.25))
     (test (flquotient  (flonum -9.75) (flonum 0.5))    (flonum -19))
     (test (flremainder (flonum -9.75) (flonum 0.5))    (flonum -0.25))
     (test (flquotient  (flonum 9.75) (flonum -0.5))    (flonum -19))
     (test (flremainder (flonum 9.75) (flonum -0.5))    (flonum 0.25))
     (test (flquotient  (flonum -9.75) (flonum -0.5))   (flonum 19))
     (test (flremainder (flonum -9.75) (flonum -0.5))   (flonum -0.25))

     (let ((f (lambda (x y)
                (call-with-values
                 (lambda () (flremquo x y))
                 (lambda (r q)
                   (list r (modulo q 8)))))))

       (test (f (flonum 15.875) (flonum 0.5))
             (list (flonum -0.125) 0))
       (test (f (flonum 16.000) (flonum 0.5))
             (list (flonum +0.000) 0))
       (test (f (flonum 16.125) (flonum 0.5))
             (list (flonum +0.125) 0))
       (test (f (flonum 16.250) (flonum 0.5))
             (list (flonum +0.250) 0))

       (test (f (flonum 16.875) (flonum 0.5))
             (list (flonum -0.125) 2))
       (test (f (flonum 17.000) (flonum 0.5))
             (list (flonum +0.000) 2))
       (test (f (flonum 17.125) (flonum 0.5))
             (list (flonum +0.125) 2))
       (test (f (flonum 17.250) (flonum 0.5))
             (list (flonum +0.250) 2))

       (test (f (flonum 17.875) (flonum 0.5))
             (list (flonum -0.125) 4))
       (test (f (flonum 18.000) (flonum 0.5))
             (list (flonum +0.000) 4))
       (test (f (flonum 18.125) (flonum 0.5))
             (list (flonum +0.125) 4))
       (test (f (flonum 18.250) (flonum 0.5))
             (list (flonum +0.250) 4))

       (test (f (flonum 18.875) (flonum 0.5))
             (list (flonum -0.125) 6))
       (test (f (flonum 19.000) (flonum 0.5))
             (list (flonum +0.000) 6))
       (test (f (flonum 19.125) (flonum 0.5))
             (list (flonum +0.125) 6))
       (test (f (flonum 19.250) (flonum 0.5))
             (list (flonum +0.250) 6))

       (test (f (flonum 15.375) (flonum 0.5))
             (list (flonum -0.125) 7))
       (test (f (flonum 16.500) (flonum 0.5))
             (list (flonum +0.000) 1))
       (test (f (flonum 16.625) (flonum 0.5))
             (list (flonum +0.125) 1))
       (test (f (flonum 16.750) (flonum 0.5))
             (list (flonum -0.250) 2))

       (test (f (flonum 16.375) (flonum 0.5))
             (list (flonum -0.125) 1))
       (test (f (flonum 17.500) (flonum 0.5))
             (list (flonum +0.000) 3))
       (test (f (flonum 17.625) (flonum 0.5))
             (list (flonum +0.125) 3))
       (test (f (flonum 17.750) (flonum 0.5))
             (list (flonum -0.250) 4))

       (test (f (flonum 17.375) (flonum 0.5))
             (list (flonum -0.125) 3))
       (test (f (flonum 18.500) (flonum 0.5))
             (list (flonum +0.000) 5))
       (test (f (flonum 18.625) (flonum 0.5))
             (list (flonum +0.125) 5))
       (test (f (flonum 38.750) (flonum 0.5))
             (list (flonum -0.250) 6))

       (test (f (flonum 18.375) (flonum 0.5))
             (list (flonum -0.125) 5))
       (test (f (flonum 19.500) (flonum 0.5))
             (list (flonum +0.000) 7))
       (test (f (flonum 19.625) (flonum 0.5))
             (list (flonum +0.125) 7))
       (test (f (flonum 19.750) (flonum 0.5))
             (list (flonum -0.250) 0))

       )

     ;; Special functions

     (test/approx (flgamma (flonum 0.5)) fl-gamma-1/2)
     (test/approx (flgamma (flonum #i1/3)) fl-gamma-1/3)
     (test/approx (flgamma (flonum #i2/3)) fl-gamma-2/3)
     (test/approx (flgamma (flonum 0.75)) (flonum 1.225416702465178))
     (test (flgamma one) one)
     (test/approx (flgamma (flonum 1.25)) (flonum 0.906402477055477))
     (test/approx (flgamma (flonum 1.38)) (flonum 0.888537149431357))
     (test/approx (flgamma (flonum 1.50)) (fl/ (flsqrt fl-pi) two))
     (test (flgamma two) one)
     (test/approx (flgamma (flonum 11)) (flonum 3628800))
     (test/approx (flgamma (flonum 11.5))
                  (* 10.5 9.5 8.5 7.5 6.5 5.5 4.5 3.5 2.5 1.5 0.5
                     fl-gamma-1/2))
     (test/approx (flgamma (flonum 1.3))
                  (* 0.3 -0.7 -1.7 -2.7 -3.7 -4.7 -5.7 -6.7
                     (flgamma (flonum -6.7))))
     (test/approx (flgamma (flonum 1.3))
                  (* 0.3 -0.7 -1.7 -2.7 -3.7 -4.7 -5.7 -6.7 -7.7
                     (flgamma (flonum -7.7))))
     (test/approx (flgamma (flonum 76.5))
                  (flonum 2.159256448318447e110))
     (test/approx (flgamma (flonum 100))
                  (flonum 9.332621544439442e155))

     (let* ((f (lambda (x)
                 (call-with-values
                  (lambda () (flloggamma x))
                  list)))
            (g (lambda (x) (car (f x))))
            (s (lambda (x) (cadr (f x)))))
       (test/approx (g (flonum 0.5)) (log fl-gamma-1/2))
       (test/approx (g (flonum #i1/3)) (log fl-gamma-1/3))
       (test/approx (g (flonum #i2/3)) (log fl-gamma-2/3))
       (test/approx (g (flonum 0.75)) (log (flonum 1.225416702465178)))
       (test/approx (g one) (log one))
       (test/approx (g (flonum 1.25)) (log (flonum 0.906402477055477)))
       (test/approx (g (flonum 1.38)) (log (flonum 0.888537149431357)))
       (test/approx (g (flonum 1.50)) (log (fl/ (flsqrt fl-pi) two)))
       (test/approx (g two) (log one))
       (test/approx (g (flonum 11)) (log (flonum 3628800)))
       (test/approx (g (flonum 1000))
                    (apply + (map log (cdr (iota 1000)))))
       (test/approx (g (flonum -0.5))
                    (log (flabs (flgamma (flonum -0.5)))))
       (test/approx (g (flonum -49.2))
                    (log (flabs (flgamma (flonum -49.2)))))
       (test/approx (g (flonum -50.3))
                    (log (flabs (flgamma (flonum -50.3)))))
       (test/approx (g (flonum -100.5))
                    (log (flabs (flgamma (flonum -100.5)))))
       (test/approx (g posinf) posinf)
       (test/unspec-or-exn (g neginf) &error)
       (test/unspec-or-exn (g zero) &error)
       (test/unspec-or-exn (g nan) &error)

       (test (s (flonum 0.5))   one)
       (test (s (flonum #i1/3)) one)
       (test (s (flonum #i2/3)) one)
       (test (s (flonum 0.75))  one)
       (test (s one)            one)
       (test (s (flonum 1.25))  one)
       (test (s (flonum 1.38))  one)
       (test (s (flonum 1.50))  one)
       (test (s two)            one)
       (test (s (flonum 11))    one)
       (test (s (flonum 1000))  one)
       (test (s (flonum -0.5))  (fl- one))
       (test (s (flonum -49.2)) one)
       (test (s (flonum -50.3)) (fl- one))
       (test (s (flonum -999.5)) one)
       (test (s (flonum -1000.5)) (fl- one))
       )

     (test (flfirst-bessel 0 zero) one)
     (test (flfirst-bessel 1 zero) zero)
     (test (flfirst-bessel 2 zero) zero)
     (test (flfirst-bessel 3 zero) zero)
     (test (flfirst-bessel 4 zero) zero)
     (test (flfirst-bessel 5 zero) zero)
     (test (flfirst-bessel 6 zero) zero)
     (test (flfirst-bessel 7 zero) zero)
     (test (flfirst-bessel 8 zero) zero)
     (test (flfirst-bessel 9 zero) zero)

     (test/approx (flfirst-bessel 0 (flonum 0.4))
                  (flonum 0.960398226659563))
     (test/approx (flfirst-bessel 1 (flonum 0.4))
                  (flonum 0.196026577955319))
     (test/approx (flfirst-bessel 2 (flonum 0.4))
                  (flonum 0.01973466311703027))
     (test/approx (flfirst-bessel 3 (flonum 0.4))
                  (flonum 1.320053214983958e-3))
     (test/approx (flfirst-bessel 4 (flonum 0.4))
                  (flonum 6.613510772909677e-5))
     (test/approx (flfirst-bessel 5 (flonum 0.4))
                  (flonum 2.648939597977585e-6))
     (test/approx (flfirst-bessel 6 (flonum 0.4))
                  (flonum 8.838222034285578e-8))
     (test/approx (flfirst-bessel 7 (flonum 0.4))
                  (flonum 2.527012308088268e-9))
     (test/approx (flfirst-bessel 8 (flonum 0.4))
                  (flonum 6.321044023358773e-11))
     (test/approx (flfirst-bessel 9 (flonum 0.4))
                  (flonum 1.405301255241872e-12))

     (test/approx (flfirst-bessel 0 one)
                  (flonum 0.765197686557967))
     (test/approx (flfirst-bessel 1 one)
                  (flonum 0.440050585744934))
     (test/approx (flfirst-bessel 2 one)
                  (flonum 0.114903484931900))
     (test/approx (flfirst-bessel 3 one)
                  (flonum 1.956335398266841e-2))
     (test/approx (flfirst-bessel 4 one)
                  (flonum 2.476638964109955e-3))
     (test/approx (flfirst-bessel 5 one)
                  (flonum 2.497577302112344e-4))
     (test/approx (flfirst-bessel 6 one)
                  (flonum 2.093833800238927e-5))
     (test/approx (flfirst-bessel 7 one)
                  (flonum 1.502325817436808e-6))
     (test/approx (flfirst-bessel 8 one)
                  (flonum 9.422344172604501e-8))
     (test/approx (flfirst-bessel 9 one)
                  (flonum 5.249250179911875e-9))

     (test/approx (flfirst-bessel 0 (flonum 17))
                  (flonum -0.1698542521511835))
     (test/approx (flfirst-bessel 1 (flonum 17))
                  (flonum -0.09766849275778065))
     (test/approx (flfirst-bessel 2 (flonum 17))
                  (flonum 0.1583638412385035))
     (test/approx (flfirst-bessel 3 (flonum 17))
                  (flonum 0.1349305730591932))
     (test/approx (flfirst-bessel 4 (flonum 17))
                  (flonum -0.1107412860446706))
     (test/approx (flfirst-bessel 5 (flonum 17))
                  (flonum -0.1870441194231559))
     (test/approx (flfirst-bessel 6 (flonum 17))
                  (flonum 0.0007153334428141831))
     (test/approx (flfirst-bessel 7 (flonum 17))
                  (flonum 0.1875490606769070))
     (test/approx (flfirst-bessel 8 (flonum 17))
                  (flonum 0.1537368341734622))
     (test/approx (flfirst-bessel 9 (flonum 17))
                  (flonum -0.04285556969011908))

     (test/approx (flfirst-bessel 0 (flonum 50))
                  (flonum +5.581232766925182e-2))
     (test/approx (flfirst-bessel 1 (flonum 50))
                  (flonum -9.751182812517514e-2))
     (test/approx (flfirst-bessel 2 (flonum 50))
                  (flonum -5.971280079425882e-2))
     (test/approx (flfirst-bessel 3 (flonum 50))
                  (flonum +9.273480406163443e-2))
     (test/approx (flfirst-bessel 4 (flonum 50))
                  (flonum +7.084097728165495e-2))
     (test/approx (flfirst-bessel 5 (flonum 50))
                  (flonum -8.140024769656964e-2))
     (test/approx (flfirst-bessel 6 (flonum 50))
                  (flonum -8.712102682096888e-2))
     (test/approx (flfirst-bessel 7 (flonum 50))
                  (flonum +6.049120125953711e-2))
     (test/approx (flfirst-bessel 8 (flonum 50))
                  (flonum +1.040585631736393e-1))
     (test/approx (flfirst-bessel 9 (flonum 50))
                  (flonum -2.719246104397254e-2))

     (let ((f (lambda (n x y)
                (test/approx (fl/ (flfirst-bessel n x) y)
                             (flexpt x (flonum n))))))
       (f 10 (flonum 8) 5.659370382712330e-11)
       (f 11 (flonum 8) 2.979844833403859e-12)
       (f 20 (flonum 8) 1.804618055659560e-25)
       (f 21 (flonum 8) 4.456237531615702e-27))

     (test/approx (flfirst-bessel 10 (flonum 10))  2.074861066333589e-1)
     (test/approx (flfirst-bessel 15 (flonum 10))  4.507973143721253e-3)
     (test/approx (flfirst-bessel 20 (flonum 10))  1.151336924781340e-5)
     (test/approx (flfirst-bessel 50 (flonum 10))  1.784513607871595e-30)
     (test/approx (flfirst-bessel 100 (flonum 10)) 6.597316064155381e-89)

     (test/approx (flfirst-bessel 10 (flonum 100))  -5.473217693547201e-2)
     (test/approx (flfirst-bessel 15 (flonum 100))  +1.519812122392732e-2)
     (test/approx (flfirst-bessel 20 (flonum 100))  +6.221745849833875e-2)
     (test/approx (flfirst-bessel 50 (flonum 100))  -3.869833972852538e-2)
     (test/approx (flfirst-bessel 100 (flonum 100)) +9.636667329586156e-2)


     (test (flsecond-bessel 0 zero) neginf)
     (test (flsecond-bessel 1 zero) neginf)
     (test (flsecond-bessel 2 zero) neginf)
     (test (flsecond-bessel 3 zero) neginf)
     (test (flsecond-bessel 4 zero) neginf)
     (test (flsecond-bessel 5 zero) neginf)
     (test (flsecond-bessel 6 zero) neginf)
     (test (flsecond-bessel 7 zero) neginf)
     (test (flsecond-bessel 8 zero) neginf)
     (test (flsecond-bessel 9 zero) neginf)
     (test (flsecond-bessel 10 zero) neginf)
     (test (flsecond-bessel 20 zero) neginf)

     (test/approx (flsecond-bessel 0 (flonum 0.4))
                  (flonum -0.6060245684270096))
     (test/approx (flsecond-bessel 1 (flonum 0.4))
                  (flonum -1.780872044270051))
     (test/approx (flsecond-bessel 2 (flonum 0.4))
                  (flonum -8.298335652923247))
     (test/approx (flsecond-bessel 3 (flonum 0.4))
                  (flonum -8.120248448496242e1))
     (test/approx (flsecond-bessel 4 (flonum 0.4))
                  (flonum -1.209738931621513e3))
     (test/approx (flsecond-bessel 5 (flonum 0.4))
                  (flonum -2.411357614794530e4))
     (test/approx (flsecond-bessel 6 (flonum 0.4))
                  (flonum -6.016296647670110e5))
     (test/approx (flsecond-bessel 7 (flonum 0.4))
                  (flonum -1.802477636686238e7))
     (test/approx (flsecond-bessel 8 (flonum 0.4))
                  (flonum -6.302655431754164e8))
     (test/approx (flsecond-bessel 9 (flonum 0.4))
                  (flonum -2.519259695064979e10))

     (test/approx (flsecond-bessel 0 one)
                  (flonum +0.08825696421567696))
     (test/approx (flsecond-bessel 1 one)
                  (flonum -0.7812128213002887))
     (test/approx (flsecond-bessel 2 one)
                  (flonum -1.650682606816254))
     (test/approx (flsecond-bessel 3 one)
                  (flonum -5.821517605964729))
     (test/approx (flsecond-bessel 4 one)
                  (flonum -3.327842302897212e1))
     (test/approx (flsecond-bessel 5 one)
                  (flonum -2.604058666258122e2))
     (test/approx (flsecond-bessel 6 one)
                  (flonum -2.570780243229150e3))
     (test/approx (flsecond-bessel 7 one)
                  (flonum -3.058895705212399e4))
     (test/approx (flsecond-bessel 8 one)
                  (flonum -4.256746184865067e5))
     (test/approx (flsecond-bessel 9 one)
                  (flonum -6.780204938731983e6))

     (test/approx (flsecond-bessel 0 (flonum 17))
                  (flonum -0.09263719844232369))
     (test/approx (flsecond-bessel 1 (flonum 17))
                  (flonum +0.1672050360772337))
     (test/approx (flsecond-bessel 2 (flonum 17))
                  (flonum +0.1123083791572924))
     (test/approx (flsecond-bessel 3 (flonum 17))
                  (flonum -0.1407795350990472))
     (test/approx (flsecond-bessel 4 (flonum 17))
                  (flonum -0.1619952738981326))
     (test/approx (flsecond-bessel 5 (flonum 17))
                  (flonum +0.06454646502933781))
     (test/approx (flsecond-bessel 6 (flonum 17))
                  (flonum +0.1999637827389195))
     (test/approx (flsecond-bessel 7 (flonum 17))
                  (flonum +0.07660444043342891))
     (test/approx (flsecond-bessel 8 (flonum 17))
                  (flonum -0.1368777729702134))
     (test/approx (flsecond-bessel 9 (flonum 17))
                  (flonum -0.2054305796995121))

     (test/approx (flsecond-bessel 0 (flonum 50))
                  (flonum -9.806499547007708e-2))
     (test/approx (flsecond-bessel 1 (flonum 50))
                  (flonum -5.679566856201477e-2))
     (test/approx (flsecond-bessel 2 (flonum 50))
                  (flonum +9.579316872759649e-2))
     (test/approx (flsecond-bessel 3 (flonum 50))
                  (flonum +6.445912206022249e-2))
     (test/approx (flsecond-bessel 4 (flonum 50))
                  (flonum -8.805807408036979e-2))
     (test/approx (flsecond-bessel 5 (flonum 50))
                  (flonum -7.854841391308165e-2))
     (test/approx (flsecond-bessel 6 (flonum 50))
                  (flonum +7.234839129775346e-2))
     (test/approx (flsecond-bessel 7 (flonum 50))
                  (flonum +9.591202782454248e-2))
     (test/approx (flsecond-bessel 8 (flonum 50))
                  (flonum -4.549302350688156e-2))
     (test/approx (flsecond-bessel 9 (flonum 50))
                  (flonum -1.104697953467446e-1))

     (let ((f (lambda (n x y)
                (test/approx (fl/ (flsecond-bessel n x) y)
                             (flexpt x (flonum (- n)))))))
       (f 10 (flonum 8) -0.97356279e9)
       (f 20 (flonum 8) -0.962608e23)
       )

     (test/approx (flsecond-bessel 10 (flonum 10))   -3.598141521834027e-1)
     (test/approx (flsecond-bessel 15 (flonum 10))   -6.364745876939129e0)
     (test/approx (flsecond-bessel 20 (flonum 10))   -1.597483848269626e3)
     (test/approx (flsecond-bessel 50 (flonum 10))   -3.641066501800740e27)
     (test/approx (flsecond-bessel 100 (flonum 10))  -4.849148271180607e85)

     (test/approx (flsecond-bessel 10 (flonum 100))  +5.833157423641493e-2)
     (test/approx (flsecond-bessel 15 (flonum 100))  +7.879068694670284e-2)
     (test/approx (flsecond-bessel 20 (flonum 100))  +5.124797307618842e-2)
     (test/approx (flsecond-bessel 50 (flonum 100))  +7.650526394480304e-2)
     (test/approx (flsecond-bessel 100 (flonum 100)) -1.669214114175765e-1)


     (test (flerf zero) zero)
     (test (flerf posinf) one)
     (test (flerf neginf) (fl- one))

     (test/approx (flerf (flonum -0.5)) -0.5204998778130465)
     (test/approx (flerf (flonum -1.0)) -0.8427007929497149)
     (test/approx (flerf (flonum -1.5)) -0.9661051464753107)
     (test/approx (flerf (flonum -2.0)) -0.9953222650189527)

     (test/approx (flerf (flonum 0.25))  0.2763263901682369)
     (test/approx (flerf (flonum 0.50))  0.5204998778130465)
     (test/approx (flerf (flonum 0.75))  0.7111556336535151)
     (test/approx (flerf (flonum 1.00))  0.8427007929497149)
     (test/approx (flerf (flonum 1.25))  0.9229001282564582)
     (test/approx (flerf (flonum 1.50))  0.9661051464753107)
     (test/approx (flerf (flonum 1.75))  0.9866716712191824)
     (test/approx (flerf (flonum 2.00))  0.9953222650189527)

     (test (flerfc zero) one)
     (test (flerfc posinf) zero)
     (test (flerfc neginf) two)

     (test/approx (flerfc (flonum -0.5)) 1.520499877813047)
     (test/approx (flerfc (flonum -1.0)) 1.842700792949715)
     (test/approx (flerfc (flonum -1.5)) 1.966105146475310)
     (test/approx (flerfc (flonum -2.0)) 1.995322265018953)

     (test/approx (flerfc (flonum 0.25)) (- 1.0 0.2763263901682369))
     (test/approx (flerfc (flonum 0.50)) (- 1.0 0.5204998778130465))
     (test/approx (flerfc (flonum 0.75)) (- 1.0 0.7111556336535151))
     (test/approx (flerfc (flonum 1.00)) (- 1.0 0.8427007929497149))
     (test/approx (flerfc (flonum 1.25)) (- 1.0 0.9229001282564582))
     (test/approx (flerfc (flonum 1.50)) (- 1.0 0.9661051464753107))
     (test/approx (flerfc (flonum 1.75)) (- 1.0 0.9866716712191824))
     (test/approx (flerfc (flonum 2.00)) (- 1.0 0.9953222650189527))

     (let* ((f (lambda (x y)
                 (test/approx (fl* x (flexp (flsquare x)) (flerfc x))
                              y)))
            (g (lambda (x^-2 y)
                 (f (flsqrt (fl/ x^-2)) y))))
       (g (flonum 0.250) 0.5107913526210115)
       (g (flonum 0.125) 0.5340672374463438)
       (g (flonum 0.100) 0.5394141079847177)
       (g (flonum 0.075) 0.5450551142044333)
       (g (flonum 0.050) 0.5510294916242269)
       (g (flonum 0.025) 0.5573864515094235)
       (g (flonum 0.020) 0.5587090260270524)
       (g (flonum 0.015) 0.5600499741400112)
       (g (flonum 0.010) 0.5614099274382259)
       (g (flonum 0.005) 0.5627895581750243))

     )))
