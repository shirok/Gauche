;;
;; Testing data.heap
;; The test uses math.mt-random, so we test it here (the impmentation
;; doesn't depend on it, though).
;;

(use gauche.test)
(test-start "data.heap")
(test-section "data.heap")
(use data.heap)
(test-module 'data.heap)

(use scheme.list)
(use srfi.27)
(use gauche.sequence)
(use util.match)

(let ((rs (make-random-source)))
  (define (do-test data comparator) ; data must be sorted
    (define len (length data))
    (define heap (make-binary-heap :storage (make-vector len)
                                   :comparator comparator))
    (let1 input (shuffle data rs)
      (test* (format "heap(~s) insertion ~s" len input)
             (map-with-index (^[i e] (list i e #t #t)) input)
             (let ([zmin #f]
                   [zmax #f])
               (map-with-index
                (^[i e]
                  (begin
                    (binary-heap-push! heap e)
                    (binary-heap-check heap)
                    (when (or (not zmin)
                              (< (comparator-compare comparator e zmin) 0))
                      (set! zmin e))
                    (when (or (not zmax)
                              (> (comparator-compare comparator e zmax) 0))
                      (set! zmax e))
                    (list i e
                          (equal? (binary-heap-find-min heap) zmin)
                          (equal? (binary-heap-find-max heap) zmax))))
                input))))
    (let1 hp1 (binary-heap-copy heap)
      (test* (format "heap(~s) clear" len) #t
             (begin
               (binary-heap-clear! hp1)
               (binary-heap-empty? hp1))))
    (let1 hp1 (binary-heap-copy heap)
      (test* (format "heap(~s) deletion from min" len)
             data
             (map-in-order (^_ (begin0 (binary-heap-pop-min! hp1)
                                 (binary-heap-check hp1)))
                           (iota len))))
    (let1 hp1 (binary-heap-copy heap)
      (test* (format "heap(~s) deletion from max" len)
             (reverse data)
             (map-in-order (^_ (begin0 (binary-heap-pop-max! hp1)
                                 (binary-heap-check hp1)))
                           (iota len))))
    (let1 hp1 (binary-heap-copy heap)
      (test* (format "heap(~s) deletion mixed" len)
             (map list
                  (take data (quotient len 2))
                  (take (reverse data) (quotient len 2)))
             (map-in-order (^_ (let* ([x (binary-heap-pop-min! hp1)]
                                      [y (binary-heap-pop-max! hp1)])
                                 (binary-heap-check hp1)
                                 (list x y)))
                           (iota (quotient len 2)))))
    )

  (do-test (iota 15) default-comparator)
  (do-test (iota 253) default-comparator)
  (do-test (reverse (iota 33))
           (make-comparator/compare number? #t (^[a b] (- (compare a b))) #f))
  (do-test '(#\a #\b #\c #\d #\e) default-comparator)
  )

(let ((rs (make-random-source)))
  (define (suck-all heap)
    (do ([r '() (cons (binary-heap-pop-min! heap) r)])
        [(binary-heap-empty? heap) (reverse r)]
      ))
  (define (do-heapify lis builder comparator)
    (test* (format "heapify ~s" lis)
           lis
           (let* ([src  (builder (shuffle lis rs))]
                  [heap (build-binary-heap src :comparator comparator)])
             (binary-heap-check heap)
             (suck-all heap))))
  (define (do-scan lis pred item)
    (test* (format "find, remove, delete ~s" lis)
           (list (boolean (find pred lis))
                 (remove pred lis)
                 (delete item lis))
           (let ([heap (build-binary-heap (list->vector (shuffle lis rs)))])
             (list (if-let1 r (binary-heap-find pred heap)
                     (pred r)
                     #f)
                   (let1 h (binary-heap-copy heap)
                     (binary-heap-remove! h pred)
                     (binary-heap-check h)
                     (suck-all h))
                   (let1 h (binary-heap-copy heap)
                     (binary-heap-delete! h item)
                     (binary-heap-check h)
                     (suck-all h))))))

  (do-heapify '() list->vector default-comparator)
  (do-heapify '(1) list->vector default-comparator)
  (do-heapify (iota 33) list->vector default-comparator)
  (do-heapify '("a" "aa" "b" "bb" "c" "cc" "d" "dd") list->vector
              string-comparator)

  (do-scan '() odd? 1)
  (do-scan (iota 23) odd? 5)
  (do-scan (iota 42) (^n (< (modulo n 3) 2)) 91)
  )

;; check binary-heap-find old & new API
(let ([h (build-binary-heap (vector 0 8 2 16 11 24 4 10))])
  (test* "binary-heap-find (new)" 11
         (binary-heap-find odd? h))
  (test* "binary-heap-find (new)" #f
         (binary-heap-find (cut > <> 100) h))
  (test* "binary-heap-find (new)" 'oops
         (binary-heap-find (cut > <> 100) h (constantly 'oops)))
  (test* "binary-heap-find (old)" '11
         (binary-heap-find h odd?)))


(let ()
  (define (test-swap source actions)
    ;; actions : ((min|max item expected-result expected-min expected-max) ...)
    (let1 hp (build-binary-heap source)
      (dolist [action actions]
        (match-let1 (minmax item xresult xmin xmax) action
          (test* (format "swap ~s ~s ~s" source minmax item)
                 (list xresult xmin xmax)
                 (let1 r ((ecase minmax
                            [(min) binary-heap-swap-min!]
                            [(max) binary-heap-swap-max!])
                          hp item)
                   (list r
                         (binary-heap-find-min hp)
                         (binary-heap-find-max hp))))))))

  (test-swap (vector 1 3 5 7 9 11 13 15 17)
             '((min 4 1 3 17)
               (min 2 3 2 17)
               (max 16 17 2 16)
               (max 1 16 1 15)
               (min 20 1 2 20)
               (min 10 2 4 20)))
  (test-swap (vector 1)
             '((min 2 1 2 2)
               (max 1 2 1 1)
               (min 0 1 0 0)))
  (test-swap (vector 3 1)
             '((min 4 1 3 4)
               (max 5 4 3 5)
               (min 6 3 5 6)
               (max 0 6 0 5)))
  (test-swap (vector 4 2 5)
             '((max 3 5 2 4)
               (max 1 4 1 3)
               (max 1 3 1 2)
               (max 1 2 1 1)
               (max 1 1 1 1)))
  )

(test-end)
