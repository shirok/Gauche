;; Testing math.* that depends on other extension modules.

(use gauche.test)
(use srfi.27)
(use srfi.42)

(test-start "math.* modules")

(test-section "math.prime")
(use math.prime)
(test-module 'math.prime)

(define *nth-primes*
  '((1 . 3) (3 . 7) (5 . 13) (10 . 31) (100 . 547)
    (1000 . 7927) (10000 . 104743) (100000 . 1299721) (200000 . 2750161)))

(test* "selected prime numbers" *nth-primes*
       (map (^[i] (cons (car i) (list-ref *primes* (car i)))) *nth-primes*))

(let ([source (make-random-source)])
  (random-source-pseudo-randomize! source 10 20)
  (let1 samples (list-ec (: n 20)
                         ((random-source-make-integers source) (expt 10 10)))
    (test* "naive-factorize" samples
           (map (^[n] (let1 factors (naive-factorize n)
                        (if (every small-prime? factors)
                          (apply * factors)
                          `(composites in ,factors))))
                samples))))

;; we need more systematic approach to test this
(define *prime-test-samples*
  '((#f . 736775510329)
    (#t . 516119616549881)
    (#f . 516119616549887)
    ))

(test* "miller-rabin test" *prime-test-samples*
       (map (^p (cons (miller-rabin-prime? (cdr p)) (cdr p)))
            *prime-test-samples*))

(define *random-prime-test-count* 500)
;(define *random-prime-test-count* 1000000) ;takes 5min on 2.4GHz Core2 machine

(let ([source (make-random-source)])
  (random-source-pseudo-randomize! source 11 20)
  (let ([rand (random-source-make-integers source)]
        [r `(,*random-prime-test-count* samples tested)])
    (test* "bpsw test vs miller-rabin test" r
           (let loop ([n 0])
             (if (= n *random-prime-test-count*)
               r
               ;; Avoid testing even numbers (which is waste of time), plus
               ;; this makes density of smaller numbers slighly higher.
               (let* ([x (rand (expt 2 48))]
                      [sample (ash x (- (twos-exponent-factor x)))])
                 (if (eq? (bpsw-prime? sample) (miller-rabin-prime? sample))
                   (loop (+ n 1))
                   `(disagreement at ,n with sample ,sample))))))))

(let1 results
    ;;(a n jacobi)
    '((0    1    1)
      (0    3    0)
      (4    5    1)
      (333  1    1)
      (78   13   0)
      (1001 9907 -1))
  (test* "jacobi symbol" results
         (map (^[r] `(,(car r) ,(cadr r) ,(jacobi (car r) (cadr r))))
              results))
  )

(let1 ps (take *primes* 100)
  (test* "totient (small primes)" (map (cut - <> 1) ps)
         (map totient ps))
  (let1 p2s (slices ps 3)
    (test* "totient (some primes)"
           (map (^[ps] (apply * (map (cut - <> 1) ps))) p2s)
           (map (^[ps] (totient (apply * ps))) p2s))))

;;;
;;;  math.simplex
;;;

(test-section "math.simplex")
(use math.simplex)
(test-module 'math.simplex)

(define (test-simplex cnt A b goal c result)
  (test* #"simplex test ~cnt"
         result
         (simplex-solve A b goal c)
         (^[a b]
           (or (eqv? a b)
               (and (uvector? a) (uvector? b)
                    (every (^i (approx=? (~ a i) (~ b i) 1e-5))
                           (iota (uvector-length a))))))))

(test-simplex 1
              #,(<f64array> (0 2 0 2) 1 0.5 3 2)
              #f64(2 12)
              :maximize #f64(1 1)
              #f64(0 4))

(test-simplex 2
              #,(<array> (0 3 0 3) 8 6 1 4 2 1.5 2 1.5 0.5)
              #(48 20 8)
              :maximize #(60 30 20)
              #f64(2 0 8))

(test-simplex 3
              #,(<array> (0 3 0 4) 3 2 1 2  1 1 1 1  4 3 3 4)
              #u32(225 117 420)
              :maximize #u32(19 13 12 17)
              #f64(39 0 48 30))

(test-simplex 4
              #,(<array> (0 3 0 4) 2 3 1 1 1 2 2 3 2 1 0 1)
              #f64(18 20 16)
              :maximize #f64(3 5 4 2)
              #f64(5.333333 0 7.33333 0))

(test-simplex 5
              #,(<array> (0 4 0 2) 6 4 1 2 -1 1 0 1)
              #f64(24 6 1 2)
              :minimize #f64(-5 -4)
              #f64(3 1.5))

(test-simplex 6
              #,(<array> (0 3 0 2) 0.25 0.5 0.4 0.2 0 0.8)
              #f64(40 40 40)
              :minimize #f64(-2 -3)
              #f64(80 40))

(test-simplex 7
              #,(<array> (0 4 0 3) 1 2 4 2 1 1 1 2 3 1 0 0)
              #f64(2000 3600 2400 30)
              :minimize #f64(-16 -17 -10)
              #f64(30 985 0))

(test-simplex 8
              #,(<array> (0 3 0 3) 2 3 0 2 5 0 3 2 4)
              #f64(8 10 15)
              :minimize #f64(-3 -5 -4)
              #f64(0 2 2.75))

;; Degenerative case
(test-simplex 9
              #,(<array> (0 3 0 4) 1/2 -11/2 -5/2 9 1/2 -3/2 -1/2 1 1 1 1 1)
              #f64(0 0 1)
              :minimize #f64(-10 57 9 24)
              #f64(0.5 0 0.5 0))

;; Negative rhs
(test-simplex 10
              #,(<array> (0 2 0 2) -2 -1 -1 -7)
              #f64(-4 -7)
              :minimize #f64(1 1)
              #f64(21/13 10/13))

(test-simplex 11
              #,(<array> (0 2 0 2) 2 3 -3 -1)
              #f64(6 -3)
              :maximize #f64(4 5)
              #f64(3 0))

;; Negative rhs, fails in phase 1
(test-simplex 12
              #,(<array> (0 4 0 2) 1/2 1/4 -1 -3 1 1 -1 -1)
              #f64(4 -36 10 -10)
              :minimize #f64(2 3)
              #f)

(test-simplex 13
              #,(<array> (0 3 0 2)
                         -3 -2
                         -1 -4
                         1 1)
              #f64(-3 -4 5)
              :maximize #f64(5 8)
              #f64(0 5))

(test-end)
