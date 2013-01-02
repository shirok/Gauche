;; Testing math.* that depends on other extension modules.

(use gauche.test)

(test-start "math.* modules")

(test-section "math.prime")
(use math.prime)
(test-module 'math.prime)

(define *nth-primes*
  '((1 . 3) (3 . 7) (5 . 13) (10 . 31) (100 . 547)
    (1000 . 7927) (10000 . 104743) (100000 . 1299721) (200000 . 2750161)))

(test* "selected prime numbers" *nth-primes*
       (map (^[i] (cons (car i) (list-ref *primes* (car i)))) *nth-primes*))

;; we need more systematic approach to test this
(define *prime-test-samples*
  '((#f . 736775510329)
    (#t . 516119616549881)
    (#f . 516119616549887)
    ))

(test* "miller-rabin test" *prime-test-samples*
       (map (^p (cons (miller-rabin-prime? (cdr p)) (cdr p)))
            *prime-test-samples*))

(test-end)

