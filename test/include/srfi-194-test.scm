;; NOTE: for zipf tests data can be exported, this can be enabled by uncommenting appropriate lines.

(import
  (scheme base)
  (scheme inexact)
  (scheme complex)
  (scheme cxr)
  (scheme file)
  (scheme write)
  (srfi 1)
  (srfi 27)
  (srfi 133)
  (srfi 194))

(cond-expand
  ((library (srfi 158)) (import (srfi 158)))
  ((library (srfi 121)) (import (srfi 121))))

(cond-expand
  (chibi (begin
           (import (except (chibi test) test-equal))
           (define-syntax test-equal
             (syntax-rules ()
               ((_ args ...) (test args ...))))
           (define-syntax test-approximate
             (syntax-rules ()
               ((_ target value max-delta)
                (test-assert (and (<= value (+ target max-delta))
                                  (>= value (- target max-delta)))))))))
  (else (import (srfi 64))))

;; syntax just we can plop it at top and still allow internal `define`s
(define-syntax reset-source!
  (syntax-rules ()
    ((_)
     (define _ (random-source-pseudo-randomize! (current-random-source) 0 0)))))

(define (reset-source!*)
  (random-source-pseudo-randomize! (current-random-source) 0 0))

(define (assert-number-generator/all-in-range gen from to)
  (test-assert
    (generator-every
      (lambda (num)
        (and (>= num from)
             (< num to)))
      (gtake gen 1000))))

(define (assert-number-generator gen from to)
  (define range (- to from))
  (define lower-quarter (+ from (* 0.25 range)))
  (define upper-quarter (- to (* 0.25 range)))
  (assert-number-generator/all-in-range gen from to)

  (test-assert
    (generator-any
      (lambda (num)
        (and (>= num from)
             (< num lower-quarter)))
      (gtake gen 1000)))

  (test-assert
    (generator-any
      (lambda (num)
        (and (>= num lower-quarter)
             (< num upper-quarter)))
      (gtake gen 1000)))

  (test-assert
    (generator-any
      (lambda (num)
        (and (>= num upper-quarter)
             (< num to)))
      (gtake gen 1000))))

(define (assert-int-generator gen byte-size signed?)
  (define from (if signed?
                   (- (expt 2 (- byte-size 1)))
                   0))
  (define to (if signed?
                 (expt 2 (- byte-size 1))
                 (expt 2 byte-size)))
  (assert-number-generator gen from to))

(test-begin "srfi-194")

(test-group "Test clamp real number"
            (reset-source!)
            (test-equal 10.0 (clamp-real-number 5.0 10.0 11))
            (test-equal 5.0 (clamp-real-number 5.0 10.0 2.0))
            (test-equal 7.5 (clamp-real-number 5.0 10.0 7.5)))

(test-group "Test with-random-source basic syntax"
            (reset-source!)
            (with-random-source default-random-source
                                (lambda () (make-random-integer-generator 0 10))))

;; testing random source, which is implementation specific
(cond-expand
  (gauche
    (import
      (gauche base)
      (math mt-random))
    (test-group "Test with-random-source"
                (reset-source!)
                ;;create and consume generators that are made with different source
                ;;with various order, and check that order doesn't change the outcome
                (define (test-multiple-sources gen1-maker gen1-expect
                                               gen2-maker gen2-expect)

                  ;;create gen1, consume gen1, create gen2, consume gen2
                  (let ((gen1 (gen1-maker)))
                   (test-equal (generator->list gen1) gen1-expect)
                   (let ((gen2 (gen2-maker)))
                    (test-equal (generator->list gen2) gen2-expect)))

                  ;;create gen1, create gen2, consume gen1, consume gen2
                  (let ((gen1 (gen1-maker))
                        (gen2 (gen2-maker)))
                    (test-equal (generator->list gen1) gen1-expect)
                    (test-equal (generator->list gen2) gen2-expect)))

                (define multiple-sources-testcase
                  (list (lambda ()
                          (gtake (with-random-source
                                   (make <mersenne-twister> :seed 0)
                                   (lambda () (make-random-integer-generator 0 10)))
                                 5))
                        '(5 5 7 8 6)
                        (lambda ()
                          (gtake (with-random-source
                                   (make <mersenne-twister> :seed 1)
                                   (lambda () (make-random-integer-generator 0 10)))
                                 5))
                        '(4 9 7 9 0)))
                (apply test-multiple-sources multiple-sources-testcase))))

(test-group "Test make-random-source-generator"
            (reset-source!)
            (define (make-numbers src-gen)
              (define gen1 (with-random-source (src-gen) (lambda () (make-random-integer-generator 0 100))))
              (define gen2 (with-random-source (src-gen) (lambda () (make-random-real-generator 0. 100.))))
              (generator->list
                (gappend
                  (gtake gen1 10)
                  (gtake gen2 10))))

            (test-equal
              (make-numbers (make-random-source-generator 0))
              (make-numbers (make-random-source-generator 0)))
            (test-assert
              (not (equal? (make-numbers (make-random-source-generator 0))
                           (make-numbers (make-random-source-generator 1))))))
(test-group "Test random int"
            (reset-source!)
            (assert-number-generator
              (make-random-integer-generator 1 100)
              1 100)

            (for-each
              (lambda (testcase)
                (define make-gen (car testcase))
                (define byte-size (cadr testcase))
                (define signed? (caddr testcase))
                (assert-int-generator (make-gen) byte-size signed?))
              (list
                (list make-random-u8-generator 8 #f)
                (list make-random-s8-generator 8 #t)
                (list make-random-u16-generator 16 #f)
                (list make-random-s16-generator 16 #t)
                (list make-random-u32-generator 32 #f)
                (list make-random-s32-generator 32 #t)
                (list make-random-u64-generator 64 #f)
                (list make-random-s64-generator 64 #t)))

            ;;test u1 separately, since it will fail quarter checks due to small range
            (assert-number-generator/all-in-range (make-random-u1-generator) 0 2)
            (test-assert
              (generator-any
                (lambda (v) (= v 0))
                (gtake (make-random-u1-generator) 100)))
            (test-assert
              (generator-any
                (lambda (v) (= v 1))
                (gtake (make-random-u1-generator) 100))))

(test-group "Test random real"
            (reset-source!)
            (assert-number-generator
              (make-random-real-generator 1.0 5.0)
              1.0 5.0)

            (test-assert
              (generator-any
                (lambda (v)
                  (not (= v (floor v))))
                (make-random-real-generator 1.0 5.0))))

(test-group "Test complex rectangular"
            (reset-source!)

            (assert-number-generator
              (gmap
                real-part
                (make-random-rectangular-generator -10.0 10.0 -100.0 100.0))
              -10 10)

            (assert-number-generator
              (gmap
                imag-part
                (make-random-rectangular-generator -100.0 100.0 -10.0 10.0))
              -10 10)

            (test-assert
              (generator-any
                (lambda (num)
                  (and (not (= 0 (real-part num)))
                       (not (= 0 (imag-part num)))))
                (make-random-rectangular-generator -10.0 10.0 -10.0 10.0))))
(test-group "Test complex polar"
            (reset-source!)
            (define PI (* 4 (atan 1.0)))

            (define (test-polar g origin mag-from mag-to angle-from angle-to test-converge-origin)
              (assert-number-generator
                (gmap
                  (lambda (num)
                    (angle (- num origin)))
                  g)
                angle-from angle-to)

              (assert-number-generator
                (gmap
                  (lambda (num)
                    (magnitude (- num origin)))
                  g)
                mag-from mag-to)

              ;; split generated area through circle at 0.5*(mag-from + mag-to)
              ;; and validate generated points in them proportional to their area
              (let* ((outter-count 0)
                     (inner-count 0)
                     (donut-area (lambda (r1 r2) (- (* r1 r1) (* r2 r2))))
                     (mag-mid (/ (+ mag-from mag-to) 2.))
                     (expected-fraction (/ (donut-area mag-to mag-mid)
                                           (donut-area mag-mid mag-from))))
                (generator-for-each
                  (lambda (point)
                    (if (< (magnitude (- point origin)) mag-mid)
                        (set! inner-count (+ 1 inner-count))
                        (set! outter-count (+ 1 outter-count))))
                  (gtake g 10000))
                (test-approximate expected-fraction (/ outter-count inner-count) 0.2))

              ;; test points converge to center
              (when test-converge-origin
                (let ((sum 0+0i))
                  (generator-for-each
                    (lambda (point) (set! sum (+ point sum)))
                    (gtake g 1000))
                  (test-approximate (real-part origin) (real-part (/ sum 1000.)) 0.1)
                  (test-approximate (imag-part origin) (imag-part (/ sum 1000.)) 0.1))))


            (test-polar (make-random-polar-generator 0. 1.)
                        0+0i 0. 1. (- PI) PI #t)

            (test-polar (make-random-polar-generator 2+5i 1. 2.)
                        2+5i 1. 2. (- PI) PI #t)

            (test-polar (make-random-polar-generator 1. 2. -1. 1.)
                        0+0i 1. 2. -1. 1. #f)

            (test-polar (make-random-polar-generator -1+3i 0. 2. (- PI) PI)
                        -1+3i 0. 2. (- PI) PI #t))

(test-group "Test random bool"
            (reset-source!)
            (test-assert
              (generator-every
                (lambda (v)
                  (or (eq? v #t)
                      (eq? v #f)))
                (gtake (make-random-boolean-generator) 10000)))

            (test-assert
              (generator-any
                (lambda (v)
                  (eq? #t v))
                (make-random-boolean-generator)))

            (test-assert
              (generator-any
                (lambda (v)
                  (eq? #f v))
                (make-random-boolean-generator))))

(test-group "Test random char"
            (reset-source!)
            (test-assert
              (generator-every
                (lambda (v)
                  (or (equal? v #\a)
                      (equal? v #\b)))
                (gtake (make-random-char-generator "ab")
                       10000)))

            (test-assert
              (generator-any
                (lambda (v)
                  (equal? v #\a))
                (make-random-char-generator "ab")))

            (test-assert
              (generator-any
                (lambda (v)
                  (equal? v #\b))
                (make-random-char-generator "ab"))))

(test-group "Test random string"
            (reset-source!)
            (test-assert
              (generator-every
                (lambda (str)
                  (and (< (string-length str) 5)
                       (every (lambda (c)
                                (or (equal? c #\a)
                                    (equal? c #\b)))
                              (string->list str))))
                (gtake (make-random-string-generator 5 "ab")
                       10000)))

            (test-assert
              (generator-any
                (lambda (str)
                  (equal? "abb" str))
                (make-random-string-generator 4 "ab"))))

(test-group "Test Bernoulli"
            (reset-source!)
            (define g (make-bernoulli-generator 0.7))
            (define expect 7000)
            (define actual (generator-count
                             (lambda (i) (= i 1))
                             (gtake g 10000)))
            (define ratio (inexact (/ actual expect)))
            (test-assert (> ratio 0.9))
            (test-assert (< ratio 1.1)))

(test-group "Test categorical"
            (reset-source!)
            (define result-vec (vector 0 0 0))
            (define expect-vec (vector 2000 5000 3000))
            (define wvec (vector 20 50 30))
            (define g (make-categorical-generator wvec))
            (generator-for-each
              (lambda (i)
                (vector-set! result-vec i (+ 1 (vector-ref result-vec i))))
              (gtake g 10000))
            (vector-for-each
              (lambda (result expect)
                (define ratio (inexact (/ result expect)))
                (test-approximate 1.0 ratio 0.1))
              result-vec
              expect-vec))

(test-group "Test poisson"
            (reset-source!)
            ;;TODO import from somewhere?
            (define (fact k)
              (cond
                ((<= k 1) 1)
                (else (* k (fact (- k 1))))))
            (define (expected-fraction L k)
              (/ (* (exact (expt L k)) (exact (exp (- L))))
                 (fact k)))

            (define (test-poisson L poisson-gen test-points)
              (generator-every
                (lambda (k)
                  (define expect (expected-fraction L k))
                  (define actual (/ (generator-count
                                      (lambda (i) (= i k))
                                      (gtake poisson-gen 10000))
                                    10000))
                  (define ratio (/ actual expect))
                  (test-assert (> ratio 8/10))
                  (test-assert (< ratio 12/10))
                  #t)
                (list->generator test-points)))

            (test-poisson 2 (make-poisson-generator 2) '(1 2 3))
            (test-poisson 40 (make-poisson-generator 40) '(30 40 50))
            (test-poisson 280 (make-poisson-generator 280) '(260 280 300)))

(test-group "Test normal"
            (reset-source!)
            (define frac-at-1dev 0.34134)
            (define frac-at-2dev 0.47725)
            (define frac-at-3dev 0.49865)

            (define (test-normal-at-point gen count-from count-to expected-fraction)
              (define actual (/ (generator-count
                                  (lambda (n)
                                    (and (>= n count-from)
                                         (< n count-to)))
                                  (gtake gen 10000))
                                10000.0))
              (test-assert (and (> actual (* 0.9 expected-fraction))
                                (< actual (* 1.1 expected-fraction)))))

            (define (test-normal gen mean deviation)
              (test-normal-at-point gen mean (+ mean deviation) frac-at-1dev)
              (test-normal-at-point gen mean (+ mean (* 2 deviation)) frac-at-2dev)
              (test-normal-at-point gen mean (+ mean (* 3 deviation)) frac-at-3dev))

            (test-normal (make-normal-generator) 0.0 1.0)
            (test-normal (make-normal-generator 1.0) 1.0 1.0)
            (test-normal (make-normal-generator 1.0 2.0) 1.0 2.0))

(test-group "Test exponential"
            (reset-source!)
            (define (expected-fraction mean x)
              (- 1 (exp (* (- (/ 1.0 mean)) x))))

            (define (test-exp-at-point gen count-to expected)
              (define actual (/ (generator-count
                                  (lambda (n)
                                    (< n count-to))
                                  (gtake gen 10000))
                                10000.0))
              (test-assert (> actual (* 0.9 expected)))
              (test-assert (< actual (* 1.1 expected))))

            (define (test-exp gen mean)
              (test-exp-at-point gen 1 (expected-fraction mean 1))
              (test-exp-at-point gen 2 (expected-fraction mean 2))
              (test-exp-at-point gen 3 (expected-fraction mean 3)))

            (test-exp (make-exponential-generator 1) 1)
            (test-exp (make-exponential-generator 1.5) 1.5))

(test-group "Test geometric"
            (reset-source!)
            (define (expected-fraction p x)
              (* (expt (- 1 p) (- x 1)) p))

            (define (test-geom-at-point gen p x)
              (define expected (expected-fraction p x))
              (define actual (/ (generator-count
                                  (lambda (n)
                                    (= n x))
                                  (gtake gen 100000))
                                100000))
              (define ratio (/ actual expected))
              (test-assert (> ratio 0.9))
              (test-assert (< ratio 1.1)))

            (define (test-geom gen p)
              (test-geom-at-point gen p 1)
              (test-geom-at-point gen p 3)
              (test-geom-at-point gen p 5))

            (test-geom (make-geometric-generator 0.5) 0.5))

(test-group "Test uniform sampling"
            (reset-source!)
            (test-equal
              '()
              (generator->list (gsampling)))
            (test-equal
              '()
              (generator->list (gsampling (generator) (generator))))
            (test-equal
              '(1 1 1)
              (generator->list (gsampling (generator) (generator 1 1 1))))
            (test-assert
              (generator-any
                (lambda (el)
                  (= el 1))
                (gsampling (circular-generator 1) (circular-generator 2))))
            (test-assert
              (generator-any
                (lambda (el)
                  (= el 2))
                (gsampling (circular-generator 1) (circular-generator 2)))))

; See zipf-test.scm
(test-group "Test Zipf sampling"
            (reset-source!)
            (include "srfi-194-zipf-test.scm")
            (zipf-test-group))

(test-group "Test sphere"
            (include "srfi-194-sphere-test.scm")
            (reset-source!*)
            (test-sphere (make-sphere-generator 1) (vector 1.0 1.0) 200 #t)
            (test-sphere (make-sphere-generator 2) (vector 1.0 1.0 1.0) 200 #t)
            (test-sphere (make-sphere-generator 3) (vector 1.0 1.0 1.0 1.0) 200 #t)

            (reset-source!*)
            (test-sphere (make-ellipsoid-generator (vector 1.0 1.0)) (vector 1.0 1.0) 200 #t)
            (test-sphere (make-ellipsoid-generator (vector 1.0 1.0 1.0)) (vector 1.0 1.0 1.0) 200 #t)
            (test-sphere (make-ellipsoid-generator (vector 1.0 1.0 1.0 1.0)) (vector 1.0 1.0 1.0 1.0) 200 #t)

            (reset-source!*)
            (test-sphere (make-ellipsoid-generator (vector 1.0 3.0)) (vector 1.0 3.0) 200 #f)
            (test-sphere (make-ellipsoid-generator (vector 1.0 3.0 5.0)) (vector 1.0 3.0 5.0) 200 #f)
            (test-sphere (make-ellipsoid-generator (vector 1.0 3.0 5.0 7.0)) (vector 1.0 3.0 5.0 7.0) 200 #f)

            (reset-source!*)
            (test-ball (make-ball-generator 2) (vector 1.0 1.0))
            (test-ball (make-ball-generator 3) (vector 1.0 1.0 1.0))
            (test-ball (make-ball-generator (vector 1.0 3.0)) (vector 1.0 3.0))
            (test-ball (make-ball-generator (vector 1.0 3.0 5.0)) (vector 1.0 3.0 5.0)))

(test-group "Test binomial"
            (reset-source!)
            (define (factorial n)
              (if (<= n 1)
                  1
                  (* n (factorial (- n 1)))))
            (define (C n k)
              (/ (factorial n)
                 (* (factorial k) (factorial (- n k)))))
            (define (expected-frac n p k)
              (* (C n k) (expt p k) (expt (- 1 p) (- n k))))

            (define (test-binomial n p count)
              (define g (make-binomial-generator n p))
              (define counts (make-vector (+ n 1) 0))
              (generator-for-each
                (lambda (x)
                  (vector-set! counts x (+ 1 (vector-ref counts x))))
                (gtake g count))
              (for-each
                (lambda (k)
                  (define expected (* count (expected-frac n p k) ))
                  (define actual (vector-ref counts k))
                  (cond
                    ((= expected 0)
                     (test-equal 0 actual))
                    ;;hacky.. testing values with very low probability fails
                    ((> expected (* 1/1000 count))
                     (test-approximate 1.0 (/ actual expected) 0.2))))
                (iota (+ n 1))))

            (test-binomial 1 0 100)
            (test-binomial 1 1 100)
            (test-binomial 1 0. 100)
            (test-binomial 1 1. 100)
            (test-binomial 10 0 100)
            (test-binomial 10 1 100)
            (test-binomial 10 0. 100)
            (test-binomial 10 1. 100)
            (test-binomial 10 0.25 100000)
            (test-binomial 40 0.375 1000000))


(test-end "srfi-194")
