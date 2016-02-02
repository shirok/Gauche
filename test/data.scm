;; testing data.*

;; data.* depends on quite a few modules, so we run this after
;; tests of extension modules are done.

(use gauche.test)
(test-start "data.* modules")

;;;========================================================================
(test-section "data.cache")
(use data.cache)
(test-module 'data.cache)

;; FIFO cache
(let* ([c (make-fifo-cache 4)])
  (test* "FIFO empty" 'none (cache-lookup! c 'a 'none))
  (test* "FIFO fill" '((a . 4) (b . 2) (c . 3) (d . 5))
         (begin
           (cache-write! c 'a 1)
           (cache-write! c 'b 2)
           (cache-write! c 'c 3)
           (cache-write! c 'a 4)
           (cache-write! c 'd 5)
           (list (cache-check! c 'a)
                 (cache-check! c 'b)
                 (cache-check! c 'c)
                 (cache-check! c 'd))))
  (test* "FIFO non-spill" '((a . 6) (b . 7) (c . 3) (d . 5))
         (begin
           (cache-write! c 'a 6)
           (cache-write! c 'b 7)
           (list (cache-check! c 'a)
                 (cache-check! c 'b)
                 (cache-check! c 'c)
                 (cache-check! c 'd))))
  (test* "FIFO spill" '((a . 6) (b . 7) #f #f (e . 8) (f . 9))
         (begin
           (cache-write! c 'e 8)
           (cache-write! c 'f 9)
           (list (cache-check! c 'a)
                 (cache-check! c 'b)
                 (cache-check! c 'c)
                 (cache-check! c 'd)
                 (cache-check! c 'e)
                 (cache-check! c 'f))))
  (test* "FIFO evict" '(#f (b . 7) #f (d . 10) (e . 8) (f . 9))
         (begin
           (cache-evict! c 'a)
           (cache-write! c 'd 10)
           (list (cache-check! c 'a)
                 (cache-check! c 'b)
                 (cache-check! c 'c)
                 (cache-check! c 'd)
                 (cache-check! c 'e)
                 (cache-check! c 'f))))
  ;; To test renumber, we forcibly change the counter value - do not do this
  ;; in the actual code!
  (set! (~ c'counter) (- (greatest-fixnum) 1))
  (test* "FIFO renumber" '((a . 11) #f (c . 13) (d . 12) #f (f . 9))
         (begin
           (cache-write! c 'a '11)
           (cache-write! c 'd '12)
           (cache-write! c 'c '13)
           (list (cache-check! c 'a)
                 (cache-check! c 'b)
                 (cache-check! c 'c)
                 (cache-check! c 'd)
                 (cache-check! c 'e)
                 (cache-check! c 'f))))
  )

;; LRU cache
(let* ([c (make-lru-cache 4)])
  (test* "LRU empty" 'none (cache-lookup! c 'a 'none))
  (test* "LRU fill" '((a . 4) (b . 2) (c . 3) (d . 5))
         (begin
           (cache-write! c 'a 1)
           (cache-write! c 'b 2)
           (cache-write! c 'c 3)
           (cache-write! c 'a 4)
           (cache-write! c 'd 5)
           (list (cache-check! c 'a)
                 (cache-check! c 'b)
                 (cache-check! c 'c)
                 (cache-check! c 'd))))
  (test* "LRU spill" '((a . 4) #f #f (d . 5) (e . 8) (f . 9))
         (begin
           (cache-check! c 'a)
           (cache-check! c 'd)
           (cache-write! c 'e 8)
           (cache-write! c 'f 9)
           (list (cache-check! c 'a)
                 (cache-check! c 'b)
                 (cache-check! c 'c)
                 (cache-check! c 'd)
                 (cache-check! c 'e)
                 (cache-check! c 'f))))
  (test* "LRU evict" '(#f #f #f (d . 5) (e . 8) (f . 9))
         (begin
           (cache-evict! c 'a)
           (list (cache-check! c 'a)
                 (cache-check! c 'b)
                 (cache-check! c 'c)
                 (cache-check! c 'd)
                 (cache-check! c 'e)
                 (cache-check! c 'f))))
  ;; To test renumber, we forcibly change the counter value - do not do this
  ;; in the actual code!
  (set! (~ c'counter) (- (greatest-fixnum) 1))
  (test* "LRU renumber" '((a . 11) #f (c . 13) (d . 12) #f (f . 9))
         (begin
           (cache-write! c 'a '11)
           (cache-write! c 'd '12)
           (cache-write! c 'c '13)
           (list (cache-check! c 'a)
                 (cache-check! c 'b)
                 (cache-check! c 'c)
                 (cache-check! c 'd)
                 (cache-check! c 'e)
                 (cache-check! c 'f))))
  )

;; TTL cache tester
;; Avoid tests from depending on timing, we mock timestamper.
(let* ([t 0]
       [c0 (make-ttl-cache 10 :timestamper (^[] t))])
  (test* "TTL empty" 'none (cache-lookup! c0 'a 'none))
  (test* "TTL fill" '((a . 1) (b . 2) (c . 3) #f)
         (begin
           (cache-register! c0 'a 1)
           (inc! t)
           (cache-register! c0 'b 2)
           (inc! t)
           (cache-register! c0 'c 3)
           (list (cache-check! c0 'a)
                 (cache-check! c0 'b)
                 (cache-check! c0 'c)
                 (cache-check! c0 'd))))
  (test* "TTL lookup" '(1 2 3 #f)
         (let* ([a (cache-lookup! c0 'a)]
                [b (begin (inc! t) (cache-lookup! c0 'b))]
                [c (begin (inc! t) (cache-lookup! c0 'c))]
                [d (cache-lookup! c0 'd #f)])
           (list a b c d)))
  ;; content and timestamp
  ;;   a -> 1 : 0
  ;;   b -> 2 : 1
  ;;   c -> 3 : 2
  (test* "TTL timeout" '(#f 2 3)
         (begin (set! t 11) ; expire 'a'
                (list
                 (cache-lookup! c0 'a #f)
                 (cache-lookup! c0 'b #f)
                 (cache-lookup! c0 'c #f))))
  ;; content and timestamp
  ;;   b -> 2 : 1
  ;;   c -> 3 : 2
  (test* "TTL through, register" '(6 7 8 9)
         (let* ([a1 (cache-through! c0 'a (^_ 6))]
                [b1 (begin (set! t 12) (cache-through! c0 'b (^_ 7)))]
                [a2 (begin (cache-register! c0 'a 8)
                           (cache-lookup! c0 'a))]
                [b2 (begin (set! t 13)
                           (cache-register! c0 'b 9)
                           (cache-lookup! c0 'b))])
           (list a1 b1 a2 b2)))
  ;; content and timestamp
  ;;   a -> 8 : 12
  ;;   b -> 9 : 13
  (test* "TTL multiple timestamps" '(#f 14)
         (begin
           (set! t 14)
           (cache-write! c0 'b 10)
           (cache-write! c0 'c 11)
           (set! t 15)
           (cache-write! c0 'b 12)
           (cache-write! c0 'c 13)
           (set! t 16)
           (cache-write! c0 'c 14)
           (set! t 26)              ; expire b but not c
           (list (cache-lookup! c0 'b #f)
                 (cache-lookup! c0 'c #f))))
  ;; content and timestamp
  ;;   c -> 14 : 16
  (test* "TTL evict" #f
         (begin
           (cache-evict! c0 'c)
           (cache-lookup! c0 'c #f)))
  )

;; TTLR cache tester
;; Avoid tests from depending on timing, we mock timestamper.
(let* ([t 0]
       [c0 (make-ttlr-cache 10 :timestamper (^[] t))])
  (test* "TTLR empty" 'none (cache-lookup! c0 'a 'none))
  (test* "TTLR fill" '((a . 1) (b . 2) (c . 3) #f)
         (begin
           (cache-register! c0 'a 1)
           (cache-register! c0 'b 2)
           (cache-register! c0 'c 3)
           (list (cache-check! c0 'a)
                 (cache-check! c0 'b)
                 (cache-check! c0 'c)
                 (cache-check! c0 'd))))
  (test* "TTLR lookup" '(1 2 3 #f)
         (let* ([a (cache-lookup! c0 'a)]
                [b (begin (inc! t) (cache-lookup! c0 'b))]
                [c (begin (inc! t) (cache-lookup! c0 'c))]
                [d (cache-lookup! c0 'd #f)])
           (list a b c d)))
  ;; content and timestamp
  ;;   a -> 1 : 0
  ;;   b -> 2 : 1
  ;;   c -> 3 : 2
  (test* "TTLR timeout" '(#f 2 3)
         (begin (set! t 11)
                (list
                 (cache-lookup! c0 'a #f)
                 (cache-lookup! c0 'b #f)
                 (cache-lookup! c0 'c #f))))
  ;; content and timestamp
  ;;   b -> 2 : 11
  ;;   c -> 3 : 11
  (test* "TTLR through, register" '(6 2 8 9)
         (let* ([a1 (cache-through! c0 'a (^_ 6))]
                [b1 (begin (set! t 12) (cache-through! c0 'b (^_ 7)))]
                [a2 (begin (cache-register! c0 'a 8)
                           (cache-lookup! c0 'a))]
                [b2 (begin (set! t 13)
                           (cache-register! c0 'b 9)
                           (cache-lookup! c0 'b))])
           (list a1 b1 a2 b2)))
  ;; content ant timestamp
  ;;   a -> 8 : 12
  ;;   b -> 9 : 13
  ;;   c -> 3 : 11
  (test* "TTLR multiple" '(8 9 #f)
         (begin
           (set! t 22)  ; expires c
           (list (cache-lookup! c0 'a #f)
                 (cache-lookup! c0 'b #f)
                 (cache-lookup! c0 'c #f))))
  ;; content and timestamp
  ;;   a -> 8 : 22
  ;;   b -> 9 : 22
  (test* "TTLR multiple - evict" '(8 #f #f)
         ;; evict an entry that has multiple timestamps
         (begin
           (cache-evict! c0 'b)
           (list (cache-lookup! c0 'a #f)
                 (cache-lookup! c0 'b #f)
                 (cache-lookup! c0 'c #f))))
  ;; content and timestamp
  ;;   a -> 8 : 22
  (test* "TTLR multiple - expire" '(#f #f #f)
         ;; expiring an entry with multiple timestamps
         (begin
           (set! t 33)
           (list (cache-lookup! c0 'a #f)
                 (cache-lookup! c0 'b #f)
                 (cache-lookup! c0 'c #f))))
  )

;; counting cache
(let ([c (make-counting-cache (make-fifo-cache 4))])
  (test* "Counting cache, initial" '(:hits 0 :misses 0)
         (cache-stats c))
  (test* "Counting cache" '(:hits 3 :misses 6)
         (begin
           (cache-through! c 'a symbol->string)  ; miss
           (cache-through! c 'b symbol->string)  ; miss
           (cache-through! c 'c symbol->string)  ; miss
           (cache-through! c 'd symbol->string)  ; miss
           (cache-through! c 'e symbol->string)  ; miss, spills a
           (cache-through! c 'd symbol->string)  ; hit
           (cache-through! c 'c symbol->string)  ; hit
           (cache-through! c 'b symbol->string)  ; hit
           (cache-through! c 'a symbol->string)  ; miss, spills b
           (cache-stats c)))
  (test* "Counting cache" '(:hits 8 :misses 9)
         (begin
           (cache-through! c 'a symbol->string)  ; hit
           (cache-through! c 'b symbol->string)  ; miss, spills c
           (cache-through! c 'c symbol->string)  ; miss, spills d
           (cache-through! c 'd symbol->string)  ; miss, spills e
           (cache-through! c 'a symbol->string)  ; hit
           (cache-through! c 'b symbol->string)  ; hit
           (cache-through! c 'c symbol->string)  ; hit
           (cache-through! c 'd symbol->string)  ; hit
           (cache-stats c))))

;;;========================================================================
(test-section "data.ideque")
(use data.ideque)
(use gauche.generator)
(test-module 'data.ideque)
(use compat.chibi-test)

(chibi-test
 (include "include/ideque-tests"))


;;;========================================================================
(test-section "data.imap")
(use data.imap)
(use gauche.sequence)
(use gauche.dictionary)
(test-module 'data.imap)

(let* ([z (make-imap)]
       [z (begin (test* "empty" #t (imap-empty? z))
                 (do ([i 0 (+ i 1)]
                      [z z (let1 n (modulo (* i 61) 128)
                             (imap-put z n (integer->char n)))])
                     [(= i 128) z]
                   ))]
       [z (begin (test* "exists?" #t
                        (every (cut imap-exists? z <>) (iota 128)))
                 (test* "exists?" #f
                        (imap-exists? z -1))
                 (test* "get" #t
                        (every (^n (eqv? (imap-get z n)
                                         (integer->char n)))
                               (iota 128)))
                 (test* "get" (test-error) (imap-get z -1))
                 (test* "get" 'z (imap-get z -1 'z))
                 (test* "collection"
                        (map (^n (cons n (integer->char n))) (iota 128))
                        (map identity z))
                 (test* "dictionary"
                        (reverse (map (^n (cons n (integer->char n))) (iota 128)))
                        (dict-fold z acons '()))
                 (test* "dictionary"
                        (map (^n (cons n (integer->char n))) (iota 128))
                        (dict-fold-right z acons '()))
                 (imap-put z 0 'meow))])
  (test* "replace" 'meow (imap-get z 0))
  (test* "min/max" `((0 . meow) (127 . ,(integer->char 127)))
         (list (imap-min z)
               (imap-max z)))
  (test* "min/max" `(#f #f)
         (let1 z (make-imap)
           (list (imap-min z)
                 (imap-max z))))
  (let ()
    ;; If we give p which is coprime to 128, the series
    ;; (modulo (* k p) 128) where k = [0..127] will walk all the
    ;; numbers between [0..127].  We delete the elements in that order
    ;; and check if it works.
    (define (delete-test p)
      (test* #"imap-delete (~p)"
             ;; result is a list of
             ;; (index num-elements exists-before-delete exists-after-delete)
             (map (^k (list k (- 127 k) #t #f)) (iota 128))
             (let loop ([ns (iota 128)] [z z] [r '()])
               (if (null? ns)
                 (reverse r)
                 (let* ([k (modulo (* (car ns) p) 128)]
                        [b (imap-exists? z k)]
                        [z (imap-delete z k)])
                   (loop (cdr ns)
                         z
                         (cons (list (car ns)
                                     (dict-fold z (^[k v c](+ c 1)) 0)
                                     b
                                     (imap-exists? z k))
                               r)))))))
    (delete-test 1)  ; in-order deletion
    (delete-test 13)
    (delete-test 31)
    (delete-test 61)
    (delete-test 127) ; (almost) reverse-order deletion
    )

  (let1 data (map (^[i] (cons (integer->char i) i)) (iota 128))
    (test* #"imap and alist conversions"
           data
           (dict->alist (alist->imap data char-comparator))))
  )

;;;========================================================================
(test-section "data.random")
(use data.random)
(test-module 'data.random)

;;;========================================================================
;; depends on data.random
(test-section "data.heap")
(use data.heap)
(test-module 'data.heap)

(use srfi-1)
(use srfi-27)
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

;;;========================================================================
;; ring-buffer
(test-section "data.ring-buffer")
(use data.ring-buffer)
(test-module 'data.ring-buffer)
(use gauche.uvector)
(use srfi-1)


(define (test-ring-buffer initial-storage)
  (define c (class-name (class-of initial-storage)))
  (define (remove-all-from-front rb)
    (unfold ring-buffer-empty?
            ring-buffer-remove-front!
            identity
            rb))
  (define (remove-all-from-back rb)
    (unfold ring-buffer-empty?
            ring-buffer-remove-back!
            identity
            rb))
  (define (test-add-remove n)
    (let1 rb (make-ring-buffer initial-storage)
      (test* #"add front - remove front (~c, ~n))"
             (reverse (iota n))
             (begin
               (dotimes (i n) (ring-buffer-add-front! rb i))
               (remove-all-from-front rb))))
    (let1 rb (make-ring-buffer initial-storage)
      (test* #"add front - remove back (~c, ~n))"
             (iota n)
             (begin
               (dotimes (i n) (ring-buffer-add-front! rb i))
               (remove-all-from-back rb))))
    (let1 rb (make-ring-buffer initial-storage)
      (test* #"add back - remove front (~c, ~n))"
             (iota n)
             (begin
               (dotimes (i n) (ring-buffer-add-back! rb i))
               (remove-all-from-front rb))))
    (let1 rb (make-ring-buffer initial-storage)
      (test* #"add back - remove back (~c, ~n))"
             (reverse (iota n))
             (begin
               (dotimes (i n) (ring-buffer-add-back! rb i))
               (remove-all-from-back rb)))))

  (define (test-basic)
    (let1 rb (make-ring-buffer initial-storage)
      (test* #"empty (~c)" #t (ring-buffer-empty? rb))
      (test* #"empty (~c)" 0  (ring-buffer-num-entries rb))
      (test* #"initial (~c)" '(10 10 1 #f)
             (begin (ring-buffer-add-front! rb 10)
                    (list (ring-buffer-front rb)
                          (ring-buffer-back rb)
                          (ring-buffer-num-entries rb)
                          (ring-buffer-empty? rb))))
      (test* #"2 elemenst (~c)" '(10 11 2 #f)
             (begin (ring-buffer-add-back! rb 11)
                    (list (ring-buffer-front rb)
                          (ring-buffer-back rb)
                          (ring-buffer-num-entries rb)
                          (ring-buffer-empty? rb))))))
  (define (test-overflow)
    (let1 rb (make-ring-buffer initial-storage :overflow-handler 'error)
      (test* #"overflow - error (~c)" (test-error)
             (dotimes [i (+ (size-of initial-storage) 1)]
               (ring-buffer-add-back! rb i))))
    (let1 rb (make-ring-buffer initial-storage :overflow-handler 'overwrite)
      (test* #"overflow - overwrite (~c)"
             (iota (size-of initial-storage)
                   (size-of initial-storage))
             (begin
               (dotimes [i (* (size-of initial-storage) 2)]
                 (ring-buffer-add-back! rb i))
               (remove-all-from-front rb)))))

  ;; body of test-ring-buffer
  (test-basic)
  (test-overflow)
  (test-add-remove 3)
  (test-add-remove 12) ; causes realloc
  )
                  
(test-ring-buffer (make-vector 4))
(test-ring-buffer (make-u8vector 5))

;;;========================================================================
;; trie
(test-section "data.trie")
(use data.trie)
(test-module 'data.trie)
(use gauche.uvector)
(use srfi-1)
(use srfi-13)

(let* ((strs '("kana" "kanaono" "kanawai" "kanawai koa"
               "kanawai mele" "kane" "Kane" "kane make" "kane makua"
               "ku" "kua" "kua`aina" "kua`ana"
               "liliko`i" "lilinoe" "lili`u" "lilo" "maoli" ""))
       (lists (map string->list strs))
       (vecs  (map list->vector lists))
       (uvecs (map string->u8vector strs)))

  ;; string trie tests
  (let1 t1 (make-trie)
    (test* "trie: constructor" '(#t 0)
           (list (trie? t1) (trie-num-entries t1)))
    (test* "trie: exists?" #f (trie-exists? t1 "kane"))

    (test* "trie: put!" 1
           (begin (trie-put! t1 "lilo" 4)
                  (trie-num-entries t1)))
    (test* "trie: get" 4
           (trie-get t1 "lilo"))
    (test* "trie: get (error)" (test-error)
           (trie-get t1 "LILO"))
    (test* "trie: get (fallback)" 'foo
           (trie-get t1 "LILO" 'foo))

    (test* "trie: put! more" (length strs)
           (begin (for-each (lambda (s)
                              (trie-put! t1 s (string-length s)))
                            strs)
                  (trie-num-entries t1)))
    (test* "trie: get more" #t
           (every (lambda (s)
                    (= (trie-get t1 s) (string-length s)))
                  strs))
    (test* "trie: exists? more" #t
           (every (cut trie-exists? t1 <>) strs))
    (test* "trie: exists? on partial key" #f
           (every (cut trie-exists? t1 <>) '("k" "ka" "kan")))
    (test* "trie: partial-key?"
           '(("k" #t) ("kana" #t) ("kanawai koa" #f) ("" #t) ("zzz" #f))
           (map (^k (list k (trie-partial-key? t1 k)))
                '("k" "kana" "kanawai koa" "" "zzz")))
    (test* "trie: longest match" '(("kana" . 4)
                                   ("kana" . 4)
                                   ("kanawai" . 7)
                                   ("" . 0))
           (map (^k (trie-longest-match t1 k #f))
                '("kana" "kanaoka" "kanawai pele" "mahalo")))
    (test* "trie: common-prefix" '(19 12 8 4 4 3)
           (map (^p (length (trie-common-prefix t1 p)))
                '("" "k" "ka" "ku" "li" "lili")))
    (test* "trie: common-prefix" '(("kua" . 3)
                                   ("kua`aina" . 8)
                                   ("kua`ana" . 7))
           (trie-common-prefix t1 "kua")
           (cut lset= equal? <> <>))
    (test* "trie: common-prefix-keys" '("kua" "kua`aina" "kua`ana")
           (trie-common-prefix-keys t1 "kua")
           (cut lset= equal? <> <>))
    (test* "trie: common-prefix-values" '(3 8 7)
           (trie-common-prefix-values t1 "kua")
           (cut lset= = <> <>))
    (test* "trie: common-prefix-fold" 18
           (trie-common-prefix-fold t1 "kua"
                                    (lambda (k v s) (+ v s))
                                    0))
    (test* "trie: common-prefix-map" '("KUA" "KUA`AINA" "KUA`ANA")
           (trie-common-prefix-map t1 "kua"
                                   (lambda (k v) (string-upcase k)))
           (cut lset= equal? <> <>))
    (test* "trie: common-prefix-for-each" '("KUA" "KUA`AINA" "KUA`ANA")
           (let1 p '()
             (trie-common-prefix-for-each t1 "kua"
                                          (lambda (k v)
                                            (push! p (string-upcase k))))
             p)
           (cut lset= equal? <> <>))
    (test* "trie: trie-fold" (fold (lambda (k s) (+ (string-length k) s))
                                      0 strs)
           (trie-fold t1 (lambda (k v s) (+ v s)) 0))
    (test* "trie: trie-map" (fold (lambda (k s) (+ (string-length k) s))
                                     0 strs)
           (apply + (trie-map t1 (lambda (k v) v))))
    (test* "trie: trie-for-each"
           (fold (lambda (k s) (+ (string-length k) s))
                 0 strs)
           (let1 c 0 (trie-for-each t1 (lambda (k v) (inc! c v))) c))
    (test* "trie: trie->list"
           (map (^s (cons s (string-length s))) strs)
           (trie->list t1)
           (cut lset= equal? <> <>))
    (test* "trie: trie-keys"
           strs
           (trie-keys t1)
           (cut lset= equal? <> <>))
    (test* "trie: trie-values"
           (map string-length strs)
           (trie-values t1)
           (cut lset= equal? <> <>))
    (test* "trie: trie-update!" 16
           (begin (trie-update! t1 "liliko`i" (cut + <> 8))
                  (trie-get t1 "liliko`i")))
    (test* "trie: trie-update! (nonexistent)" (test-error)
           (trie-update! t1 "humuhumu" (cut + <> 8)))
    (test* "trie: trie-update! (nonexistent)" 16
           (begin (trie-update! t1 "humuhumu" (cut + <> 8) 8)
                  (trie-get t1 "humuhumu")))
    (test* "trie: delete!" '(19 #f)
           (begin (trie-delete! t1 "humuhumu")
                  (list (trie-num-entries t1)
                        (trie-get t1 "humuhumu" #f))))
    (test* "trie: delete! (nonexistent)" '(19 #f)
           (begin (trie-delete! t1 "HUMUHUMU")
                  (list (trie-num-entries t1)
                        (trie-get t1 "HUMUHUMU" #f))))
    (test* "trie: delete! (everything)" 0
           (begin (for-each (cut trie-delete! t1 <>) strs)
                  (trie-num-entries t1)))
    )
  ;; trie and trie-with-keys
  (let1 t2 (trie '() '("foo" . 0) '("foof" . 1) '("far" . 2))
    (test* "trie: trie" '(("foo" . 0) ("foof" . 1) ("far" . 2))
           (trie->list t2)
           (cut lset= equal? <> <>)))
  (let1 t3 (trie-with-keys '() "foo" "foof" "far")
    (test* "trie: trie-with-keys"
           '(("foo" . "foo") ("foof" . "foof") ("far" . "far"))
           (trie->list t3)
           (cut lset= equal? <> <>)))

  ;; heterogeneous tries
  (let1 t4 (make-trie)
    (for-each (cut for-each
                   (lambda (seq)
                     (trie-put! t4 seq (class-of seq)))
                   <>)
              (list strs lists vecs uvecs))
    (test* "trie(hetero): put!" (* 4 (length strs))
           (trie-num-entries t4))
    (test* "trie(hetero): get" <vector> (trie-get t4 '#()))
    (test* "trie(hetero): get" <u8vector> (trie-get t4 '#u8()))
    (test* "trie(hetero): get" <pair> (trie-get t4 '(#\k #\u)))

    (test* "trie(hetero): delete!" <string>
           (begin (trie-delete! t4 '()) (trie-get t4 "")))
    (test* "trie(hetero): delete!" (* 3 (length strs))
           (begin (for-each (cut trie-delete! t4 <>) lists)
                  (trie-num-entries t4)))
    )

  ;; customizing tables
  (let1 t5 (make-trie list
                      (cut assoc-ref <> <> #f char-ci=?)
                      (lambda (t k v)
                        (if v
                          (assoc-set! t k v char-ci=?)
                          (alist-delete! k t char-ci=?)))
                      (lambda (t f s) (fold f s t)))
    (test* "trie(custom): put!" (- (length strs) 1)
           (begin
             (for-each (^s (trie-put! t5 s (string-length s))) strs)
             (trie-num-entries t5)))
    (test* "trie(custom): get" 99
           (begin
             (trie-put! t5 "LILIKO`I" 99)
             (trie-get t5 "liliko`i")))
    )

  ;; collection api
  (let1 t6 #f
    (test* "trie(collection): builder" (length strs)
           (begin
             (set! t6 (coerce-to <trie> (map (cut cons <> #t) strs)))
             (and (trie? t6) (size-of t6))))
    (test* "trie(collection): iterator" strs
           (let1 p '()
             (call-with-iterator t6
                                 (lambda (end next)
                                   (until (end)
                                     (push! p (car (next))))))
             p)
           (cut lset= equal? <> <>))
    (test* "trie(collection): coerce to list" (map (cut cons <> #t) strs)
           (coerce-to <list> t6)
           (cut lset= equal? <> <>))
    (test* "trie(collection): coerce to vector"
           (map (cut cons <> #t) strs)
           (vector->list (coerce-to <vector> t6))
           (cut lset= equal? <> <>))
    (test* "trie(collection): coerce to hashtable" #t
           (let1 h (coerce-to <hash-table> t6)
             (every (cut hash-table-get h <>) strs)))
    )
  )

(test-end)
