;; testing data.*

;; data.* depends on quite a few modules, so we run this after
;; tests of extension modules are done.

(use gauche.test)
(test-start "data.* modules")

(test-section "data.random")
(use data.random)
(test-module 'data.random)

;; depends on data.random
(test-section "data.heap")
(use data.heap)
(test-module 'data.heap)

(use srfi-1)
(use srfi-27)
(use gauche.sequence)

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
           (make-comparator number? #t (^[a b] (- (compare a b))) #f))
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
             (list (if-let1 r (binary-heap-find heap pred)
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

  

(test-end)
