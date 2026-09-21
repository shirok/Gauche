;;
;; testing math.random.*
;;

(use gauche.test)
(use gauche.collection)
(use scheme.list)
(use srfi.13)
(use gauche.uvector)
(use gauche.threads)

(test-start "random")

(test-section "math.random.mt")

(use math.random.mt)
(test-module 'math.random.mt)


(define (value-in-range? range)
  (^n (<= 0 n (- range 1))))
(define (make-random-sequence class size generator)
  (with-builder (class add! get :size size)
    (dotimes [i size (get)]
      (add! (generator)))))

(define m (make <mersenne-twister>))

(test "mt-random-integer" #t
      (^[] (every (value-in-range? 7)
                  (make-random-sequence <list> 1000
                                        (^[] (mt-random-integer m 7))))))

(test "mt-random-integer" #t
      (^[] (every (value-in-range? 113)
                  (make-random-sequence <list> 1000
                                        (^[] (mt-random-integer m 113))))))

(test "mt-random-integer" #t
      (^[] (every (value-in-range? 78356385638456)
                  (make-random-sequence <list> 1000
                                        (^[] (mt-random-integer m 78356385638456))))))

(test "mt-random-real" #t
      (^[] (every (^n (< 0 n 1))
                  (make-random-sequence <list> 1000
                                        (^[] (mt-random-real m))))))

(test "seed" #t
      (^[] (let ([m0 (make <mersenne-twister> :seed 1)]
                 [m1 (make <mersenne-twister> :seed 1)])
             (equal? (make-random-sequence <list> 100
                                           (^[] (mt-random-real m0)))
                     (make-random-sequence <list> 100
                                           (^[] (mt-random-real m1)))))))

(test "seed" #f
      (^[] (let ([m0 (make <mersenne-twister> :seed 1)]
                 [m1 (make <mersenne-twister> :seed 2)])
             (equal? (make-random-sequence <list> 100
                                           (^[] (mt-random-real m0)))
                     (make-random-sequence <list> 100
                                           (^[] (mt-random-real m1)))))))

(test "seed" #t
      (^[] (let ([m0 (make <mersenne-twister>)]
                 [m1 (make <mersenne-twister>)])
             (mt-random-set-seed! m0 '#u32(472346 37429385 72))
             (mt-random-set-seed! m1 '#u32(472346 37429385 72))
             (equal? (make-random-sequence <list> 100
                                           (^[] (mt-random-real m0)))
                     (make-random-sequence <list> 100
                                           (^[] (mt-random-real m1)))))))

(test "seed" #f
      (^[] (let ([m0 (make <mersenne-twister>)]
                 [m1 (make <mersenne-twister>)])
             (mt-random-set-seed! m0 '#u32(472346 37429385 72))
             (mt-random-set-seed! m1 '#u32(472346 37429385 71))
             (equal? (make-random-sequence <list> 100
                                           (^[] (mt-random-real m0)))
                     (make-random-sequence <list> 100
                                           (^[] (mt-random-real m1)))))))

(test "seed transplant" #t
      (^[] (let ([m0 (make <mersenne-twister>)]
                 [m1 (make <mersenne-twister>)])
             (mt-random-set-seed! m0 '#u32(472346 37429385 72))
             (let1 s0 (make-random-sequence <list> 100
                                            (^[] (mt-random-real m0)))
               (mt-random-set-seed! m1 (mt-random-get-seed m0))
               (equal? s0
                       (make-random-sequence <list> 100
                                             (^[] (mt-random-real m1))))))))

(test "u32vector" #t
      (^[] (let ([m0 (make <mersenne-twister> :seed 1)]
                 [m1 (make <mersenne-twister> :seed 1)])
             (equal? (make-random-sequence <u32vector> 100
                                           (^[] (mt-random-integer m0 (expt 2 32))))
                     (rlet1 v (make-u32vector 100 0)
                       (mt-random-fill-u32vector! m1 v))))))

(test "f64vector" #t
      (^[] (let ([m0 (make <mersenne-twister> :seed 1)]
                 [m1 (make <mersenne-twister> :seed 1)])
             (equal? (make-random-sequence <f64vector> 100
                                           (^[] (mt-random-real m0)))
                     (rlet1 v (make-f64vector 100 0)
                       (mt-random-fill-f64vector! m1 v))))))

(test "state" #t
      (^[] (let ([s  (mt-random-get-state m)]
                 [m2 (make <mersenne-twister> :seed 9324)])
             (mt-random-set-state! m2 s)
             (equal? (make-random-sequence <list> 100 (^[] (mt-random-real m)))
                     (make-random-sequence <list> 100 (^[] (mt-random-real m2)))
                     ))))


(test-section "math.random.xos")

(use math.random.xos)
(test-module 'math.random.xos)

(define (xos-random-sequence g n)
  (map (^_ (xos-random-u64 g)) (iota n)))

(test* "seed" 12345 (xos-random-get-seed (make-xoshiro256 :seed 12345)))

(test* "set-seed!" #t
       (let1 g (make-xoshiro256 :seed 1)
         (xos-random-set-seed! g 7)
         (equal? (xos-random-sequence g 10)
                 (xos-random-sequence (make-xoshiro256 :seed 7) 10))))

;; :private? only turns off the mutex; it must not affect the sequence.
(test* "private? generates the same sequence" #t
       (equal? (xos-random-sequence (make-xoshiro256 :seed 7) 20)
               (xos-random-sequence (make-xoshiro256 :seed 7 :private? #t) 20)))

(test* "private? initarg" #t
       (equal? (xos-random-sequence (make <xoshiro256> :seed 7) 20)
               (xos-random-sequence (make <xoshiro256> :seed 7 :private? #t) 20)))

;; State saving/restoring with copy-xoshiro256 and copy-xoshiro256!

(test* "copy-xoshiro256 copies the seed" 12345
       (let1 g (make-xoshiro256 :seed 12345)
         (xos-random-sequence g 5)      ;advance the state
         (xos-random-get-seed (copy-xoshiro256 g))))

(test* "copy-xoshiro256 snapshots the current state" #t
       (let* ([g (make-xoshiro256 :seed 314159)]
              [_ (xos-random-sequence g 10)] ;advance the state
              [snapshot (copy-xoshiro256 g)])
         (equal? (xos-random-sequence g 20)
                 (xos-random-sequence snapshot 20))))

(test* "copy-xoshiro256 snapshot is independent of the original" #t
       (let* ([g (make-xoshiro256 :seed 2718)]
              [snapshot (copy-xoshiro256 g)]
              [expect (xos-random-sequence g 20)]) ;advances g, not snapshot
         (equal? expect (xos-random-sequence snapshot 20))))

(test* "copy-xoshiro256! restores the saved state" #t
       (let* ([g (make-xoshiro256 :seed 8888)]
              [_ (xos-random-sequence g 7)]
              [snapshot (copy-xoshiro256 g)]
              [expect (xos-random-sequence g 20)])
         (copy-xoshiro256! g snapshot)
         (equal? expect (xos-random-sequence g 20))))

(test* "copy-xoshiro256! restores repeatedly" '(#t #t #t)
       (let* ([g (make-xoshiro256 :seed 8888)]
              [snapshot (copy-xoshiro256 g)]
              [expect (xos-random-sequence g 20)])
         (map (^_ (copy-xoshiro256! g snapshot)
                  (equal? expect (xos-random-sequence g 20)))
              (iota 3))))

(test* "copy-xoshiro256! leaves the source intact" #t
       (let* ([g (make-xoshiro256 :seed 4649)]
              [snapshot (copy-xoshiro256 g)]
              [h (make-xoshiro256 :seed 1)])
         (copy-xoshiro256! h snapshot)
         (xos-random-sequence h 20)     ;advances h, not snapshot
         (equal? (xos-random-sequence g 20)
                 (xos-random-sequence snapshot 20))))

(test* "copy-xoshiro256! overwrites the destination" '(#t 8888)
       (let* ([g (make-xoshiro256 :seed 8888)]
              [_ (xos-random-sequence g 3)]
              [snapshot (copy-xoshiro256 g)]
              [expect (xos-random-sequence g 20)]
              [h (make-xoshiro256 :seed 999)])
         (xos-random-sequence h 5)
         (copy-xoshiro256! h snapshot)
         (list (equal? expect (xos-random-sequence h 20))
               (xos-random-get-seed h))))

(test-end)
