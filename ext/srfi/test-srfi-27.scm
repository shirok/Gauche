;;;
;;; SRFI-27 depends on mt-random, so we test it here.
;;;

(use gauche.test)
(test-start "SRFI-27")
(test-section "SRFI-27")

(use srfi.27)
(test-module 'srfi.27)

(let ([s0 (make-random-source)]
      [s1 (make-random-source)]
      [saved #f])
  (define (get-random-series source)
    (let* ([random0 (random-source-make-integers source)]
           [random1 (random-source-make-reals source)])
      (append-map (^_ (let* ([r0 (random0 10)]
                             [r1 (random1)])
                        (list r0 r1)))
                  (iota 10))))
  (define (unit-random source unit)
    (let1 random (random-source-make-reals source unit)
      (test* "real random unit exactness" (exact? unit)
             (exact? (random)))
      (test* "real random unit range" #t
             (every (^_ (< 0 (random) 1)) (iota 100)))))

  (test* "random-source?" #t (random-source? s0))

  (random-source-randomize! s0)
  (random-source-randomize! s1)

  (set! saved (random-source-state-ref s0))

  (let1 result (get-random-series s0)
    (test* "random-state" result
           (begin
             (random-source-state-set! s1 saved)
             (get-random-series s1))))

  (unit-random s0 0.1)
  (unit-random s0 1/3)
  )

(test-end)
