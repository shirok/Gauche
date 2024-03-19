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

;; non-SRFI-134 api
(test* "make-ideque" '(a a a a a)
       (ideque->list (make-ideque 5 'a)))


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

;; data.random is tested in ext/data, for it depends on math.mt-random

;;;========================================================================

;; data.heap is tested in ext/data, for its test depends on math.mt-random.

;;;========================================================================
(test-section "data.priority-map")
(use data.priority-map)
(test-module 'data.priority-map)

(use scheme.list)

(let* ([data '((a . 5) (b . 2) (c . 9) (d . -1) (e . -59) (f . 0) (g . 9))])
  (define (make-populated)
    (rlet1 m (make-priority-map)
      (dolist [p data] (dict-put! m (car p) (cdr p)))))
  (define (compare-minmax-result a b)
    (and (lset= eq? (car a) (car b))
         (= (cdr a) (cdr b))))
  (define (stabilize seq)
    ;; if there's consecutive items in seq whose cdr is the same,
    ;; sort them with their cars' order.  don't change other items orders.
    ($ apply append
       $ map (^[items] (sort-by items car))
       $ group-sequence seq :key cdr))

  (let1 m (make-populated)
    (test* "get" (map cdr data)
           (map (^p (dict-get m (car p))) data))
    (test* "min" '((e) . -59)
           (priority-map-min-all m)
           compare-minmax-result)
    (test* "min" '(e . -59)
           (priority-map-min m))
    (test* "max" '((c g) . 9)
           (priority-map-max-all m)
           compare-minmax-result)
    (test* "max" (test-one-of '(g . 9) '(c . 9))
           (priority-map-max m))
    (test* "delete" (remove (^p (eq? (car p) 'c)) data)
           (begin (dict-delete! m 'c)
                  (sort-by (dict->alist m) car))))

  (let1 m (make-populated)
    (test* "pop, increasing" (stabilize (sort-by data cdr))
           (stabilize ((rec (pop m)
                         (if (zero? (size-of m))
                           '()
                           (let1 z (priority-map-pop-min! m)
                             (cons z (pop m)))))
                       m))))
  (let1 m (make-populated)
    (test* "pop, decreasing" (stabilize (sort-by data cdr >))
           (stabilize ((rec (pop m)
                         (if (zero? (size-of m))
                           '()
                           (let1 z (priority-map-pop-max! m)
                             (cons z (pop m)))))
                       m))))

  (let ([tm (alist->tree-map data)])
    (test* "alist->priority-map" data
           (sort (dict->alist (alist->priority-map data))))
    (test* "dict->priority-map" data
           (sort (dict->alist (dict->priority-map tm)))))
  )

;;;========================================================================
;; range
(test-section "data.range")
(use data.range)
(test-module 'data.range)

;; SRFI-196 is a subset of data.range, and we test it here.

(define-module srfi-196-tests
  (use gauche.test)
  (test-include-r7 "include/srfi-196-test"))

;; gauche extensions

(test* "range-ref out of range" (test-error <error> #/Index out of range/)
       (range-ref (iota-range 10) 11))
(test* "range-ref fallback" 'oops
       (range-ref (iota-range 10) 11 'oops))
(test* "range-ref fallback" 'oops
       (range-ref (iota-range 10) -1 'oops))
(test* "range-first fallback" 'wow
       (range-first (iota-range 0) 'wow))
(test* "range-last fallback" 'boo
       (range-last (iota-range 0) 'boo))

;; https://github.com/shirok/Gauche/pull/1004
(test* "range-append" '(0 1 2 3 4 10 11 12 13 14)
       (range->list (range-append (numeric-range 0 5) (numeric-range 10 15))))

(test* "range-reverse start/end" '(5 4 3)
       (range->list (range-reverse (iota-range 7) 3 6)))
(test* "range-reverse start/end nested" '(5 4 3)
       (range->list (range-reverse (subrange (iota-range 10) 1 9) 2 5)))
(test* "range-reverse start/end nested" '(6 5 4)
       (range->list (range-reverse
                     (subrange (subrange (iota-range 10) 1 9) 1 6)
                     2 5)))

(test* "vector-range start/end" '(c d e f g)
       (range->list (vector-range '#(a b c d e f g) 2)))
(test* "vector-range start/end" '(c d e)
       (range->list (vector-range '#(a b c d e f g) 2 5)))
(test* "uvector-range" '(0 1 2 3 4)
       (range->list (uvector-range '#u8(0 1 2 3 4))))
(test* "uvector-range start/end" '(1 2 3 4)
       (range->list (uvector-range '#u8(0 1 2 3 4 5) 1 5)))
(test* "string-range start/end" '(#\c #\d #\e #\f #\g)
       (range->list (string-range "abcdefg" 2)))
(test* "string-range start/end" '(#\c #\d #\e)
       (range->list (string-range "abcdefg" 2 5)))
(test* "bitvector/bool-range" '(#t #f #t #t)
       (range->list (bitvector/bool-range #*0101100 1 5)))
(test* "bitvector/int-range" '(0 1 1 0 0)
       (range->list (bitvector/int-range #*0101100 2)))

;; sequence protocol
(test* "range as a sequence (ref)" 3 (~ (numeric-range 1 6) 2))
(test* "range as a sequence (iterate)" '(1 2 3 4 5)
       (coerce-to <list> (numeric-range 1 6)))
(test* "range as a sequence (build from list)" '(a b c d e)
       (range->list (coerce-to <range> '(a b c d e))))
(test* "range as a sequence (build from vector)" '(a b c d e)
       (range->list (coerce-to <range> '#(a b c d e))))
(test* "range as a sequence (subseq)" '(3 4 5 6 7)
       (range->list (subseq (numeric-range 0 10) 3 8)))

;; SRFI-42 extension
(define-module range-srfi-42-test
  (use gauche.test)
  (use data.range)
  (use srfi.42)
  (test* "SRFI-42 :range" '((a 0) (b 1) (c 2) (d 3) (e 4))
         (list-ec (: v (index k) (vector-range '#(a b c d e)))
                  (list v k)))
  )

;;;========================================================================
;; skew-list
(test-section "data.skew-list")
(use data.skew-list)
(use gauche.sequence)
(use gauche.generator)
(test-module 'data.skew-list)

(test* "skew-list?" #f (skew-list? '()))
(test* "skew-list?" #f (skew-list? '(a b)))
(test* "skew-list?" #t (skew-list? (list->skew-list '(a b))))

(dotimes [n 30]
  (let1 data (iota n)
    (test* "list -> skew-list ->list" data
           (skew-list->list (list->skew-list data)))))

(test* "skew-list-cons" 'a
       (skew-list-car (skew-list-cons 'a skew-list-null)))
(test* "skew-list-cons/car/cdr" '((z a b c d e) a (b c d e))
       (let1 sl (list->skew-list '(a b c d e))
         (list (skew-list->list (skew-list-cons 'z sl))
               (skew-list-car sl)
               (skew-list->list (skew-list-cdr sl)))))
(test* "skew-list-cons (error)" (test-error)
       (skew-list-cons 'a 'b))
(test* "skew-list-car (error)" (test-error)
       (skew-list-car skew-list-null))
(test* "skew-list-cdr (error)" (test-error)
       (skew-list-cdr skew-list-null))

(dotimes [n 20]
  (test* #"skew-list-length ~n" n
         (skew-list-length (list->skew-list (make-list n)))))

(let ([series '(0 1 2 3 5 8 13 21)])
  (dolist [x series]
    (dolist [y series]
      (test* #"skew-list-length<=? ~x ~y" (<= x y)
             (skew-list-length<=? (list->skew-list (iota x)) y)))))

(let* ([data '(a b c d e f g h i j k l m n o p q r s t u v w x y z)]
       [sl (list->skew-list data)])
  (test* "skew-list-ref" data
         (map-with-index (^[i _] (skew-list-ref sl i)) data))
  (test* "skew-list-ref (out of range)" (test-error)
         (skew-list-ref sl (length data)))
  (test* "skew-list-ref fallback" 'oops
         (skew-list-ref sl (length data) 'oops))
  (dotimes [n (length data)]
    (test* #"skew-list-set ~n"
           (rlet1 seq (list-copy data)
             (list-set! seq n 'z))
           (skew-list->list (skew-list-set sl n 'z)))))

;; mapping
(let1 sl (list->skew-list (iota 22))
  (define (f x) (+ (* 2 x) 1))
  (test* "skew-list-map" (map f (skew-list->list sl))
         (skew-list->list (skew-list-map sl f))))

;; generator & lseq
(let* ([data '(a b c d e f g h i j k l m n o p q r s t u v w x y z)]
       [sl (list->skew-list data)])
  (define (t-gen . args)
    (test* #"skew-list->generator ~args"
           (apply subseq data args)
           (generator->list (apply skew-list->generator sl args)))
    (test* #"skew-list->lseq ~args"
           (apply subseq data args)
           (apply skew-list->lseq sl args)))
  (t-gen)
  (t-gen 5)
  (t-gen 9 17)
  (t-gen 20 20)
  (t-gen 25 26))

;; take
(let ()
  (define (t-take orig-len take-len)
    (let1 input (iota orig-len)
      (test* #"skew-list-take ~|take-len| from ~|orig-len|"
             (take input take-len)
             (skew-list->list
              (skew-list-take (list->skew-list input) take-len)))
      (test* #"skew-list-drop ~|take-len| from ~|orig-len|"
             (drop input take-len)
             (skew-list->list
              (skew-list-drop (list->skew-list input) take-len)))
      (test* #"skew-list-split-at ~|take-len| from ~|orig-len|"
             (values->list (split-at input take-len))
             (receive (a b)
                 (skew-list-split-at (list->skew-list input) take-len)
               (list (skew-list->list a)
                     (skew-list->list b))))
      ))
  ;; We cover all the cases
  ;; edge case
  (t-take 0 0)
  (t-take 10 0)
  ;; trivial prefix
  (t-take 26 1)   ; 26 = [1 3 7 15]
  (t-take 26 4)
  (t-take 26 11)
  (t-take 26 26)
  (t-take 29 7)   ; 29 = [7 7 15]
  (t-take 29 14)  ; 29 = [7 7 15]
  (t-take 29 29)  ; 29 = [7 7 15]
  ;; [1 n ...] case
  (t-take 8 2)    ; 8 = [1 7]
  (t-take 8 5)    ; 8 = [1 7]
  ;; [n ...] (n > 3) case
  (t-take 14 1)   ; 14 = [7 7]
  (t-take 14 2)
  (t-take 14 4)
  (t-take 15 1)   ; 15 = [15]
  (t-take 15 2)
  (t-take 15 5)
  (t-take 15 8)
  ;; [3 ...] case
  (t-take 6 1)    ; 6 = [3 3]
  (t-take 6 2)
  ;; general case
  (t-take 6 3)
  (t-take 6 4)
  (t-take 6 5)
  (t-take 26 2)
  (t-take 26 3)
  (t-take 26 5)
  (t-take 26 10)
  (t-take 26 15)
  )

;; append
(let ()
  (define (t-append i j)
    (let ([x (iota i)]
          [y (iota j)])
      (test* "skew-list-append" (append x y)
             (skew-list->list (skew-list-append (list->skew-list x)
                                                (list->skew-list y))))))
  (t-append 0 0)
  (t-append 0 1)
  (t-append 1 0)
  (t-append 1 1)
  (t-append 1 2)
  (t-append 1 3)
  (t-append 1 4)
  (t-append 1 5)
  (t-append 2 1)
  (t-append 2 2)
  (t-append 2 3)
  (t-append 2 6)
  (t-append 3 1)
  (t-append 3 2)
  (t-append 3 3)
  (t-append 3 4)
  (t-append 3 6)
  (t-append 3 7)
  (t-append 3 10)
  (t-append 5 10)
  )

;; sequence protocol
(test* "skew-list size-of" 5
       (size-of (list->skew-list '(a b c d e))))
(test* "skew-list iterator" '(a b c d e f g h i j k l m n o p q)
       (coerce-to <list> (list->skew-list '(a b c d e f g h i j k l m n o p q))))
(test* "skew-list fold" '(e d c b a)
       (fold cons '() (list->skew-list '(a b c d e))))
(test* "skew-list ref" '(a b c d e)
       (map (cute ref (list->skew-list '(a b c d e)) <>) (iota 5)))

;;;========================================================================
;; ulid
(test-section "data.ulid")
(use data.ulid)
(test-module 'data.ulid)

(define-module ulid-test
  (use gauche.test)
  (use data.ulid)
  (test-include-r7 "include/ulid-test" (exclude (ulid))))

(test-end)
