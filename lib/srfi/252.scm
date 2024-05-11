;;;
;;; SRFI-252 - Property testing
;;;
;;;   Copyright (c) 2024  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi.252
  (use gauche.test)
  (use srfi.64 :prefix srfi-64:)
  (use gauche.generator)
  (use data.random)
  (export boolean-generator bytevector-generator
          char-generator string-generator symbol-generator

          complex-generator integer-generator number-generator
          rational-generator real-generator

          exact-complex-generator exact-integer-generator
          exact-integer-complex-generator exact-number-generator
          exact-rational-generator exact-real-generator

          inexact-complex-generator inexact-integer-generator
          inexact-number-generator inexact-rational-generator
          inexact-real-generator

          list-generator-of pair-generator-of
          procedure-generator-of vector-generator-of

          test-property test-property-expect-fail test-property-skip
          test-property-error test-property-error-type
          property-test-runner
          )
  )
(select-module srfi.252)

;;
;; Some constants
;;

(define-constant default-runs 30)


;;
;; Test runner a la srfi-64
;;

(define (property-test-runner)
  (srfi-64:test-runner-simple))

;;
;; Test procedures
;;

(define (%prop-test pred generators expr runs expected hook)
  (dotimes [n runs]
    (let1 args (map (^g (g)) generators)
      (test* (format "Property test ~s [~d] with args ~s" expr n args)
             expected
             (boolean (apply pred (map (^g (g)) generators)))
             test-check
             (^[name expected actual]
               (format #t "Failed (~s) with arguments: ~s" actual args))
             (^[pass/fail name expected actual]
               (and-let* ([ hook ]
                          [runner (srfi-64:test-runner-current)])
                 (hook runner pass/fail)))))))

(define (%test-property pred generators expr runs)
  (%prop-test pred generators expr runs #t
              (^[runner pass/fail]
                (ecase pass/fail
                  [(pass) (inc! (srfi-64:test-runner-pass-count runner))]
                  [(fail) (inc! (srfi-64:test-runner-fail-count runner))]))))

(define-syntax test-property
  (syntax-rules ()
    [(_ pred generators)
     (%test-property pred generators 'pred default-runs)]
    [(_ pred generators runs)
     (%test-property pred generators 'pred runs)]))

(define (%test-property-expect-fail pred generators expr runs)
  (%prop-test pred generators expr runs #f
              (^[runner pass/fail]
                (ecase pass/fail
                  [(pass) (inc! (srfi-64:test-runner-xfail-count runner))]
                  [(fail) (inc! (srfi-64:test-runner-xpass-count runner))]))))

(define-syntax test-property-expect-fail
  (syntax-rules ()
    [(_ pred generators)
     (%test-property-expect-fail pred generators 'pred default-runs)]
    [(_ pred generators runs)
     (%test-property-expect-fail pred generators 'pred runs)]))

(define (%test-property-skip pred generators expr runs)
  (and-let1 runner (srfi-64:test-runner-current)
    (inc! (srfi-64:test-runner-skip-count runner)))
  #f)

(define-syntax test-property-skip
  (syntax-rules ()
    [(_ pred generators)
     (%test-property-skip pred generators 'pred default-runs)]
    [(_ pred generators runs)
     (%test-property-skip pred generators 'pred runs)]))

(define (%test-property-error pred generators expr runs)
  (%prop-test pred generators expr runs (test-error)
              (^[runner pass/fail]
                (ecase pass/fail
                  [(pass) (inc! (srfi-64:test-runner-pass-count runner))]
                  [(fail) (inc! (srfi-64:test-runner-fail-count runner))]))))

(define-syntax test-property-error
  (syntax-rules ()
    [(_ pred generators)
     (%test-property-error pred generators 'pred default-runs)]
    [(_ pred generators runs)
     (%test-property-error pred generators 'pred runs)]))

(define (%test-property-error-type pred generators expr etype runs)
  (%prop-test pred generators expr runs (test-error etype)
              (^[runner pass/fail]
                (ecase pass/fail
                  [(pass) (inc! (srfi-64:test-runner-pass-count runner))]
                  [(fail) (inc! (srfi-64:test-runner-fail-count runner))]))))

(define-syntax test-property-error-type
  (syntax-rules ()
    [(_ etype pred generators)
     (%test-property-error-type pred generators 'pred etype default-runs)]
    [(_ etype pred generators runs)
     (%test-property-error-type pred generators 'pred etype runs)]))

;; Generators

(define-constant sequence-max-size 33)

(define (boolean-generator) (gcons* #t #f (booleans$)))

(define (bytevector-generator)
  (gcons* '#u8()
          (sequences-of <u8vector>
                        (integers$ sequence-max-size)
                        (integers$ 256))))

(define (char-generator) (gcons* #\null (chars$ char-set:full)))

(define (string-generator)
  ($ gcons* ""
     $ strings-of (integers$ sequence-max-size) (chars$ char-set:full)))

(define (symbol-generator)
  ($ gcons* '||
     $ gmap string->symbol
     $ strings-of (integers$ sequence-max-size) (chars$ char-set:full)))

(define (complex-generator) (inexact-complex-generator))

(define (integer-generator)
  (samples-from (list (exact-integer-generator) (inexact-integer-generator))))

(define (number-generator)
  (samples-from (list (exact-number-generator) (inexact-number-generator))))

(define (rational-generator)
  (samples-from (list (exact-rational-generator) (inexact-rational-generator))))

(define (real-generator)
  (samples-from (list (exact-real-generator) (inexact-real-generator))))

(define (exact-complex-generator) (error "Exact complex isn't supported."))

;; Since our integers are unbounded, we can't uniformly sample from the
;; entire space.  Besides, real programs tend to meet a lots of short
;; integers but relatively few bigger ones.
;;
;; So, heuristically, we prepare three generators - samples from 16bit integers,
;; samples from entire fixnum range, and a long-tail distribution of bignums.
(define (%heuristic-uints)
  (let ([smalls (uint16s$)]
        [mids (gmap abs (fixnums$))]
        [bigs (reals-power-law$ (+ (greatest-fixnum) 1) 1.1)])
    (samples-from (list smalls mids (^[] (round->exact (bigs)))))))

(define (%heuristic-sints)
  (let ([sign (samples$ '(-1 1))]
        [uints (%heuristic-uints)])
    (^[] (* (sign) (uints)))))

(define (exact-integer-generator)
  (gcons* 0 1 -1 (%heuristic-sints)))

(define (exact-integer-complex-generator)
  (error "Exact complex isn't supported."))

(define (exact-number-generator)
  ;; we don't have exact complex, so this is the same as...
  (exact-rational-generator))

(define (%nonzero-uints) (gdelete 0 (%heuristic-uints)))

(define (%ratios)
  (gmap / (%heuristic-sints) (%nonzero-uints)))

(define (exact-rational-generator)
  (gcons* 0 1 -1 1/2 -1/2 (%ratios)))

(define (exact-real-generator) (exact-rational-generator))

(define (inexact-complex-generator)
  (let ([re (finite-flonums$)]
        [im (finite-flonums$)])
    (gcons* 0.0 -0.0 0.5 -0.5 1.0 -1.0
            0.0+1.0i 0.0-1.0i -0.0+1.0i -0.0-1.0i
            0.5+0.5i 0.5-0.5i -0.5+0.5i -0.5-0.5i
            1.0+1.0i 1.0-1.0i -1.0+1.0i -1.0-1.0i
            +inf.0+inf.0i +inf.0-inf.0i -inf.0+inf.0i -inf.0-inf.0i
            +nan.0+nan.0i
            +inf.0 -inf.0 +nan.0
            (^[] (make-rectangular (re) (im))))))

(define (inexact-integer-generator)
  (gcons* 0.0 -0.0 1.0 -1.0
          (gfilter finite? (gmap inexact (%heuristic-sints)))))

(define (inexact-number-generator) (inexact-complex-generator))

(define (inexact-rational-generator)
  (gfilter finite? (inexact-real-generator)))

(define (inexact-real-generator)
  (gcons* 0.0 -0.0 0.5 -0.5 1.0 -1.0 +inf.0 -inf.0 +nan.0
          (finite-flonums$)))

;; Generator combiners

(define (list-generator-of gen :optional (max-length #f))
  (if max-length
    (lists-of (integers$ max-length) gen)
    (lists-of gen)))

(define (pair-generator-of car-gen :optional (cdr-gen #f))
  (pairs-of car-gen (or cdr-gen car-gen)))

(define (procedure-generator-of gen)
  (gmap (^v (^ _ v)) gen))

(define (vector-generator-of gen :optional (max-length #f))
  (if max-length
    (vectors-of (integers$ max-length) gen)
    (vectors-of gen)))
