;; Testing math.* that depends on other extension modules.

(use gauche.test)
(use srfi-27)
(use srfi-42)

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

(test-end)

