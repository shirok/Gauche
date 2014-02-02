;;;
;;; tests for some builtin list operations
;;;

(use gauche.test)
(test-start "builtin list operations")

;;--------------------------------------------------------------------------
(test-section "ref,set!")

(test* "list-ref" '(e d c b a)
       (map (^k (list-ref '(a b c d e) k)) '(4 3 2 1 0)))
(test* "list-set!" '(a (b) c (d) e)
       (rlet1 lis (list 'a 'b 'c 'd 'e)
         (for-each (lambda (k)
                     (when (odd? k)
                       (list-set! lis k (list (list-ref lis k)))))
                   '(4 3 2 1 0))))
(let ()
  (define (ensure-error name proc)
    (test* #"~name (negative)" (test-error) (proc -1))
    (test* #"~name (too big)" (test-error) (proc 5))
    (test* #"~name (nonexact)" (test-error) (proc 1.0))
    (test* #"~name (noninteger)" (test-error) (proc 1/4)))

  (ensure-error "list-ref" (^k (list-ref '(a b c d e) k)))
  (ensure-error "list-set!" (^k (list-set! (list 'a 'b 'c 'd 'e) k 0)))
  )

;;--------------------------------------------------------------------------
(test-section "iota")

(test* "iota" '(0 1 2 3 4) (iota 5))
(test* "iota" '(5 6 7 8 9) (iota 5 5))
(test* "iota" '(10 20 30 40 50) (iota 5 10 10))
(test* "iota" '(1.0 1.5 2.0 2.5 3.0) (iota 5 1 0.5))

;;--------------------------------------------------------------------------
(test-section "append and reverse")

(test* "append" '(1 2 3 4 5) (append '(1 2 3) '() '(4 5)))
(test* "append" '(1 2 3 . 5) (append '(1 2 3) 5))
(test* "append!" '(1 2 3 4 5) (append (list 1 2 3) '() '(4 5)))
(test* "append!" '(1 2 3 . 5) (append (list 1 2 3) 5))
(test* "reverse" '(5 4 3 2 1) (reverse '(1 2 3 4 5)))
(test* "reverse" '() (reverse '()))
(test* "reverse!" '(5 4 3 2 1) (reverse! (list 1 2 3 4 5)))
(test* "reverse!" '() (reverse! '()))
(test* "reverse 2args" '(1 2 3 4 5)    (reverse '(3 2 1) '(4 5)))
(test* "reverse 2args" '(1 2 3 4 . 5)  (reverse '(4 3 2 1) 5))
(test* "reverse! 2args" '(1 2 3 4 5)   (reverse! (list 3 2 1) (list 4 5)))
(test* "reverse! 2args" '(1 2 3 4 . 5) (reverse! (list 4 3 2 1) 5))

;;--------------------------------------------------------------------------
(test-section "folding")

(test* "fold" 55
       (fold + 0 '(1 2 3 4 5 6 7 8 9 10)))
(test* "fold" '(e d c b a)
       (fold cons '() '(a b c d e)))
(test* "fold" 3
       (fold (^[x c] (if (symbol? x) (+ c 1) c))
             0
             '(a 3 b 8 c 9)))
(test* "fold" '(c 3 b 2 a 1)
       (fold list* '() '(a b c) '(1 2 3 4 5)))
(test* "fold-right" '(1 2 3 4 5)
       (fold-right cons '() '(1 2 3 4 5)))
(test* "fold-right" '(2 4 6)
       (fold-right (^[x l] (if (even? x) (cons x l) l))
                   '()
                   '(1 2 3 4 5 6 7)))
(test* "fold-right" '(a 1 b 2 c 3)
       (fold-right list* '() '(a b c) '(1 2 3 4 5)))

(test* "fold-left" '(((z . a) . b) . c)
       (fold-left cons 'z '(a b c)))
(test* "fold-left" '(c b a . z)
       (fold-left (^[a b] (cons b a)) 'z '(a b c)))
(test* "fold-left" 21
       (fold-left + 0 '(1 2 3) '(4 5 6)))
(test* "fold-left" '(((z a A) b B) c C)
       (fold-left list 'z '(a b c) '(A B C)))
(test* "count" 3 (count even? '(3 1 4 1 5 9 2 6 5)))
(test* "count" 3
       (count < '(1 2 4 8) '(2 4 6 8 10 12 14 16)))
(test* "count" 2
       (count < '(3 1 4 1) '#0=(1 10 . #0#)))
(test* "reduce" 55 (reduce + 0 (iota 10 1)))
(test* "reduce-right" '(1 2 3 4 5 6 7 8 9 . 10)
       (reduce-right cons 0 (iota 10 1)))
(test* "concatenate" '(1 2 3 4 5)
       (concatenate '((1 2 3) () (4 5))))
(test* "concatenate" '(1 2 3 4 . 5)
       (concatenate '((1 2 3) () (4) 5)))
(test* "concatenate!" '(1 2 3 4 5)
       (concatenate (list (list 1 2 3) '() (list 4 5))))
(test* "concatenate!" '(1 2 3 4 . 5)
       (concatenate (list (list 1 2 3) '() (list 4) 5)))
(test* "append-reverse" '(1 2 3 4 5)
       (append-reverse '(3 2 1) '(4 5)))
(test* "append-reverse" '(1 2 3 4 . 5)
       (append-reverse '(4 3 2 1) 5))
(test* "append-reverse!" '(1 2 3 4 5)
       (append-reverse! (list 3 2 1) (list 4 5)))
(test* "append-reverse!" '(1 2 3 4 . 5)
       (append-reverse! (list 4 3 2 1) 5))
(test* "append-map" '(1 -1 3 -3 5 -5)
       (append-map (^x (list x (- x))) '(1 3 5)))
(test* "append-map" '(1 -2 3 -4 5 -6)
       (append-map (^[x y] (list x (- y)))
                   '(1 3 5) '(2 4 6 8)))
(test* "append-map!" '(1 -2 3 -4 5 -6)
       (append-map! (^[x y] (list x (- y)))
                    '(1 3 5) '(2 4 6 8)))

;;--------------------------------------------------------------------------
(test-section "filtering")

(test* "filter-map" '(1 9 49)
       (filter-map (^x (and (number? x) (* x x)))
                   '(a 1 b 3 c 7)))
(test* "filter" '(0 8 8 -4)
       (filter even? '(0 7 8 8 9 -4)))
(test* "remove" '(7 43)
       (remove even? '(0 7 8 8 43 -4)))

(test* "filter!" '(0 8 8 -4)
       (filter even? (list 0 7 8 8 9 -4)))
(test* "remove!" '(7 43)
       (remove! even? (list 0 7 8 8 43 -4)))

;;--------------------------------------------------------------------------
(test-section "searching")

(test* "find" 4
       (find even? '(3 1 4 1 5 9)))
(test* "find" #f
       (find even? '(3 1 1 5 9)))
(test* "find-tail" '(-8 -5 0 0)
       (find-tail even? '(3 1 -8 -5 0 0)))
(test* "find-tail" #f
       (find-tail even? '(3 1 -9 -5 1 -1)))

(test* "member" #f
       (member '"b" '("a" "b" "c") eq?))
(test* "member" '(2.0 3.0)
       (member '2.0 '(1.0 2.0 3.0) eqv?))
(test* "member" '("b" "c")
       (member '"b" '("a" "b" "c") equal?))
(test* "member" '("b" "c")
       (member '"b" '("a" "b" "c")))
(test* "member" '("b" "c")
       (member '"b" '("a" "b" "c") string=?))
(test* "member" '("b" "c")
       (member '"B" '("a" "b" "c") string-ci=?))
(test* "delete" '(b c d e)
       (delete 'a '(a b a c a d a e a) eq?))
(test* "delete" '(b c d e)
       (delete 'a '(a b a c a d a e) eq?))
(test* "delete" '(b c d e)
       (delete 'a '(b a c a d a e a) eq?))
(test* "delete" '(b c d e)
       (delete 'a '(b a c a d a e) eq?))
(test* "delete" '(b c d e)
       (delete 'a '(b c d e) eq?))
(test* "delete" '(b c d e . f)
       (delete 'a '(b a c a d a e . f) eq?))
(test* "delete" '(2.0 3.0 4.0 5.0)
       (delete '1.0 '(1.0 2.0 1.0 3.0 4.0 1.0 5.0) eqv?))
(test* "delete" '("b" "c" "d" "e")
       (delete "a" '("a" "b" "a" "c" "d" "a" "e")))
(test* "delete" '("b" "c" "d" "e")
       (delete "a" '("a" "b" "a" "c" "d" "a" "e") string=?))
(test* "delete" '("b" "c" "d" "e")
       (delete "A" '("a" "b" "a" "c" "d" "a" "e") string-ci=?))
(test* "delete!" '(b c d e)
       (delete! 'a '(a b a c a d a e a) eq?))
(test* "delete!" '(b c d e)
       (delete! 'a '(a b a c a d a e) eq?))
(test* "delete!" '(b c d e)
       (delete! 'a '(b a c a d a e a) eq?))
(test* "delete!" '(b c d e)
       (delete! 'a '(b a c a d a e) eq?))
(test* "delete!" '(b c d e)
       (delete! 'a '(b c d e) eq?))
(test* "delete!" '(b c d e . f)
       (delete! 'a '(b c a d e . f) eq?))
(test* "delete!" '(2.0 3.0 4.0 5.0)
       (delete! '1.0 '(1.0 2.0 1.0 3.0 4.0 1.0 5.0) eqv?))
(test* "delete!" '("b" "c" "d" "e")
       (delete! "a" '("a" "b" "a" "c" "d" "a" "e")))
(test* "delete!" '("b" "c" "d" "e")
       (delete! "a" '("a" "b" "a" "c" "d" "a" "e") string=?))
(test* "delete!" '("b" "c" "d" "e")
       (delete! "A" '("a" "b" "a" "c" "d" "a" "e") string-ci=?))
(test* "delete-duplicates" '(a b c d e)
       (delete-duplicates '(a b c d e) eq?))
(test* "delete-duplicates" '(a b c d e)
       (delete-duplicates '(a b a c b a d d a e) eq?))
(test* "delete-duplicates" '(a b c d . e)
       (delete-duplicates '(a b a c b a d d a . e) eq?))
(test* "delete-duplicates" '(1.0 2.0 3.0 4.0 5.0)
       (delete-duplicates '(1.0 2.0 1.0 2.0 3.0 3.0 4.0 1.0 5.0) eqv?))
(test* "delete-duplicates" '("a" "b" "c" "d" "e")
       (delete-duplicates '("a" "b" "b" "a" "b" "c" "d" "a" "e")))
(test* "delete-duplicates" '("a" "b" "c" "d" "e")
       (delete-duplicates '("a" "b" "a" "a" "c" "d" "a" "e") string=?))
(test* "delete-duplicates" '("A" "b" "c" "d" "e")
       (delete-duplicates '("A" "b" "a" "B" "c" "d" "a" "e") string-ci=?))
(test* "delete-duplicates!" '(a b c d e)
       (delete-duplicates! '(a b c d e) eq?))
(test* "delete-duplicates!" '(a b c d e)
       (delete-duplicates! '(a b a c b a d d a e) eq?))
(test* "delete-duplicates!" '(1.0 2.0 3.0 4.0 5.0)
       (delete-duplicates! '(1.0 2.0 1.0 2.0 3.0 3.0 4.0 1.0 5.0) eqv?))
(test* "delete-duplicates!" '("a" "b" "c" "d" "e")
       (delete-duplicates! '("a" "b" "b" "a" "b" "c" "d" "a" "e")))
(test* "delete-duplicates!" '("a" "b" "c" "d" "e")
       (delete-duplicates! '("a" "b" "a" "a" "c" "d" "a" "e") string=?))
(test* "delete-duplicates!" '("A" "b" "c" "d" "e")
       (delete-duplicates! '("A" "b" "a" "B" "c" "d" "a" "e") string-ci=?))

(test* "any" #f (any even? '()))

(test* "any" #f (any even? '(1 3)))

(test* "any" #t (any even? '(1 2)))

(test* "any" 1 (any string->number '("1" "a")))

(test* "any" 1 (any string->number '("1" "2")))

(test* "any" 1
       (any string->number '("1" "2") '(10 10)))

(test* "any" 1
       (any string->number '("1" "2") '(10)))

(test* "any" #f
       (any string->number '("1" "2") '()))

(test* "every" #t (every odd? '()))

(test* "every" #t (every odd? '(1 3)))

(test* "every" #f (every odd? '(1 2)))

(test* "every" #f (every string->number '("1" "a")))

(test* "every" 2 (every string->number '("1" "2")))

(test* "every" 2
       (every string->number '("1" "2") '(10 10)))

(test* "every" 1
       (every string->number '("1" "2") '(10)))

(test* "every" #t
       (every string->number '("1" "2") '()))

;;--------------------------------------------------------------------------
(test-section "take and drop")

(test* "take" '(a b)   (take '(a b c d e) 2))
(test* "drop" '(c d e) (drop '(a b c d e) 2))
(test* "take" '(1 2)   (take '(1 2 3 . d) 2))
(test* "drop" '(3 . d) (drop '(1 2 3 . d) 2))
(test* "take" '(1 2 3) (take '(1 2 3 . d) 3))
(test* "drop" 'd       (drop '(1 2 3 . d) 3))
(test* "take-right" '(d e)     (take-right '(a b c d e) 2))
(test* "drop-right" '(a b c)   (drop-right '(a b c d e) 2))
(test* "take-right" '(2 3 . d) (take-right '(1 2 3 . d) 2))
(test* "drop-right" '(1)       (drop-right '(1 2 3 . d) 2))
(test* "take-right" 'd         (take-right '(1 2 3 . d) 0))
(test* "drop-right" '(1 2 3)   (drop-right '(1 2 3 . d) 0))
(test* "take!"      '(1 2)     (take! '(1 2 3 . d) 2))
(test* "drop-right!" '(1 2)    (drop-right! '(1 2 3 . d) 1))
(test* "split-at" '((a b c) (d e f g h))
       (call-with-values (^[] (split-at '(a b c d e f g h) 3))
         list))
(test* "split-at!" '((a b c) (d e f g h))
       (call-with-values
           (^[] (split-at! (list 'a 'b 'c 'd 'e 'f 'g 'h) 3))
         list))
(test* "partition" '((one four five) (2 3 6))
       (values->list (partition symbol? '(one 2 3 four five 6))))

(test* "split-at* (normal)" '((a b c) (d))
       (receive r (split-at* '(a b c d) 3) r))
(test* "split-at* (boundary)" '(() (a b c d))
       (receive r (split-at* '(a b c d) 0) r))
(test* "split-at* (boundary)" '((a b c d) ())
       (receive r (split-at* '(a b c d) 4) r))
(test* "split-at* (error)" (test-error)
       (receive r (split-at* '(a b c d) -1) r))
(test* "split-at* (shorten)" '((a b c d) ())
       (receive r (split-at* '(a b c d) 5) r))
(test* "split-at* (fill)" '((a b c d #f #f) ())
       (receive r (split-at* '(a b c d) 6 #t) r))
(test* "split-at* (fill)" '((a b c d z z) ())
       (receive r (split-at* '(a b c d) 6 #t 'z) r))

(test* "take* (normal)" '(a b c)      (take* '(a b c d) 3))
(test* "take* (boundary)" '()         (take* '(a b c d) 0))
(test* "take* (boundary)" '(a b c d)  (take* '(a b c d) 4))
(test* "take* (error)" (test-error)   (take* '(a b c d) -1))
(test* "take* (shorten)" '(a b c d)   (take* '(a b c d) 5))
(test* "take* (fill)" '(a b c d #f #f) (take* '(a b c d) 6 #t))
(test* "take* (fill)" '(a b c d z z)  (take* '(a b c d) 6 #t 'z))

(test* "drop* (normal)" '(c d)       (drop* '(a b c d) 2))
(test* "drop* (boundary)" '(a b c d) (drop* '(a b c d) 0))
(test* "drop* (boundary)" '()        (drop* '(a b c d) 4))
(test* "drop* (error)" (test-error)  (drop* '(a b c d) -3))
(test* "drop* (past)" '()            (drop* '(a b c d) 5))

(test* "take-right* (normal)" '(b c d)  (take-right* '(a b c d) 3))
(test* "take-right* (boundary)" '()     (take-right* '(a b c d) 0))
(test* "take-right* (boundary)" '(a b c d) (take-right* '(a b c d) 4))
(test* "take-right* (error)" (test-error)  (take-right* '(a b c d) -1))
(test* "take-right* (shorten)" '(a b c d)  (take-right* '(a b c d) 6))
(test* "take-right* (fill)" '(z z a b c d) (take-right* '(a b c d) 6 #t 'z))

(test* "take-right* (long)" '(a b c d e f)
       (take-right* (fold (^[i r] (list* 'a 'b 'c 'd 'e 'f r))
                          '() (iota 1668))
                    6))

(test* "drop-right* (normal)" '(a b c)  (drop-right* '(a b c d) 1))
(test* "drop-right* (boundary)" '()     (drop-right* '(a b c d) 4))
(test* "drop-right* (boundary)" '(a b c d) (drop-right* '(a b c d) 0))
(test* "drop-right* (error)" (test-error)  (drop-right* '(a b c d) -1))
(test* "drop-right* (past)" '()         (drop-right* '(a b c d) 7))

;;--------------------------------------------------------------------------
;; slice and intercept

(test* "slices (normal)" '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15))
       (slices (iota 16) 4))
(test* "slices (boundary)" '()
       (slices '() 4))
(test* "slices (short)" '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12))
       (slices (iota 13) 4))
(test* "slices (short)" '((0 1))
       (slices (iota 2) 4))
(test* "slices (fill)" '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 #f #f #f))
       (slices (iota 13) 4 #t))
(test* "slices (fill)" '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 -1 -1 -1))
       (slices (iota 13) 4 #t -1))

(test* "intersperse" '(1 + 2 + 3) (intersperse '+ '(1 2 3)))
(test* "intersperse" '(1 + 2) (intersperse '+ '(1 2)))
(test* "intersperse" '(1) (intersperse '+ '(1)))
(test* "intersperse" '() (intersperse '+ '()))

;;--------------------------------------------------------------------------
;; associative lists

(test* "assq" '(a 1) (assq 'a '((a 1) (b 2) (c 3))))
(test* "assq" #f     (assq 'd '((a 1) (b 2) (c 3))))
(test* "assq" #f     (assq (list 'a) '(((a)) ((b)) ((c)))))
(test* "assv" '(b 2) (assv 'b '((a 1) (b 2) (c 3))))
(test* "assv" #f     (assv 'd '((a 1) (b 2) (c 3))))
(test* "assv" #f     (assv (list 'a) '(((a)) ((b)) ((c)))))
(test* "assoc" '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))
(test* "assoc" '("a") (assoc "a" '(("c") ("b") ("a"))))
(test* "assoc" '("a") (assoc "A" '(("c") ("b") ("a")) string-ci=?))
(test* "acons" '((1 . 2) . 3) (acons 1 2 3))
(test* "alist-copy" '((a 1) (a 2))
       (let* ([x '((b 2) (a 1))]
              [y (alist-copy x)])
         (set-cdr! (assq 'a y) (list 2))
         (list (assq 'a x) (assq 'a y))))
(test* "alist-delete" '((a 1) (b 2) (c 3))
       (alist-delete 'x '((a 1) (b 2) (c 3))))
(test* "alist-delete" '((b 2) (c 3))
       (alist-delete 'a '((a 1) (b 2) (c 3))))
(test* "alist-delete" '((a 1) (c 3))
       (alist-delete 'b '((a 1) (b 2) (c 3))))
(test* "alist-delete" '((a 1) (b 2))
       (alist-delete 'c '((a 1) (b 2) (c 3))))
(test* "alist-delete!" '((a 1) (b 2) (c 3))
       (rlet1 l (alist-copy '((a 1) (b 2) (c 3)))
         (alist-delete! 'x l)))
(test* "alist-delete!" '((b 2) (c 3))
       (alist-delete! 'a (alist-copy '((a 1) (b 2) (c 3)))))
(test* "alist-delete!" '(z (b 2) (c 3))
       (alist-delete! 'a (list-copy '(z (a 1) (b 2) (c 3)))))
(test* "alist-delete!" '(z (b 2) (c 3))
       (alist-delete! 'a (list-copy '((a 1) z (b 2) (c 3)))))
(test* "alist-delete!" '((a 1) (c 3))
       (rlet1 l (alist-copy '((a 1) (b 2) (c 3)))
         (alist-delete! 'b l)))
(test* "alist-delete!" '((a 1) (b 2))
       (rlet1 l (alist-copy '((a 1) (b 2) (c 3)))
         (alist-delete! 'c l)))

(test* "rassoc" '(5 . "b")
       (rassoc "b" '((3 . "a") (5 . "b"))))
(test* "rassq" '(5 . b)
       (rassq 'b '((3 . a) (5 . b))))
(test* "rassv" '("b" . 5)
       (rassoc 5 '(("a" . 3) ("b" . 5))))

(test* "assoc-ref" 5
       (assoc-ref '(("a" . 3) ("b" . 5)) "b"))
(test* "assoc-ref" 7
       (assoc-ref '(("a" . 3) ("b" . 5)) "c" 7))
(test* "assq-ref" 5
       (assq-ref '((a . 3) (b . 5)) 'b))
(test* "assq-ref" 7
       (assq-ref '((a . 3) (b . 5)) 'c 7))
(test* "assv-ref" 'b
       (assv-ref '((3 . a) (5 . b)) 5))
(test* "assv-ref" 'c
       (assv-ref '((3 . a) (5 . b)) 7 'c))

(test* "rassoc-ref" 5
       (rassoc-ref '((3 . "a") (5 . "b")) "b"))
(test* "rassoc-ref" 7
       (rassoc-ref '((3 . "a") (5 . "b")) "c" 7))
(test* "rassq-ref" 5
       (rassq-ref '((3 . a) (5 . b)) 'b))
(test* "rassq-ref" #f
       (rassq-ref '((3 . a) (5 . b)) 'c))
(test* "rassv-ref" 'b
       (rassv-ref '((a . 3) (b . 5)) 5))
(test* "rassv-ref" #f
       (rassv-ref '((a . 3) (b . 5)) 7))

(test* "assoc-set!" '(("a" . 3) ("b" . 9))
       (assoc-set! (list (cons "a" 3) (cons "b" 5)) "b" 9))
(test* "assoc-set!" '(("c" . 9) ("a" . 3) ("b" . 5))
       (assoc-set! (list (cons "a" 3) (cons "b" 5)) "c" 9))
(test* "assq-set!" '((a . 3) (b . 9))
       (assq-set! (list (cons 'a 3) (cons 'b 5)) 'b 9))
(test* "assq-set!" '((c . 9) (a . 3) (b . 5))
       (assq-set! (list (cons 'a 3) (cons 'b 5)) 'c 9))
(test* "assv-set!" '((3 . a) (5 . c))
       (assv-set! (list (cons 3 'a) (cons 5 'b)) 5 'c))
(test* "assv-set!" '((9 . c) (3 . a) (5 . b))
       (assv-set! (list (cons 3 'a) (cons 5 'b)) 9 'c))

;;--------------------------------------------------------------------------

(test-section "circular list and equality")

;; At this moment we haven't tested #n=, #n# notation, so we build
;; circular structure at runtime.

(let ()
  (define (cdr-cycle . lis)
    (set-cdr! (last-pair lis) lis)
    lis)
  (define (car-cycle . lis)
    (if (null? lis)
      lis
      (let* ([head (cons #f (car lis))]
             [next (let loop ([lis (cdr lis)])
                     (if (null? lis)
                       head
                       (cons (loop (cdr lis)) (car lis))))])
        (set-car! head next)
        head)))

  (test* "equal? w/ cdr-cycle 1" #t
         (equal? (cdr-cycle 1 2 3) (cdr-cycle 1 2 3 1 2 3)))
  (test* "equal? w/ cdr-cycle 2" #f
         (equal? (cdr-cycle 1 2 3) (cdr-cycle 1 2 3 1 2)))
  (test* "equal? w/ cdr-cycle 3" #f
         (equal? (cdr-cycle 1 2 3)
                 (apply append (make-list 10000 (list 1 2 3)))))
  (test* "equal? w/ cdr-cycle 4" #t
         (equal? (cdr-cycle 1 2 3)
                 (apply cdr-cycle (apply append (make-list 10000 (list 1 2 3))))))
  (test* "equal? w/ cdr-cycle 5" #f
         (equal? (cdr-cycle 1 2 3) '(1)))

  (test* "equal? w/ car-cycle 1" #t
         (equal? (car-cycle 1) (car-cycle 1)))
  (test* "equal? w/ car-cycle 2" #f
         (equal? (car-cycle 1) (car-cycle 2)))
  (test* "equal? w/ car-cycle 3" #t
         (equal? (car-cycle 1 2 3) (car-cycle 1 2 3 1 2 3)))
  (test* "equal? w/ car-cycle 4" #f
         (equal? (car-cycle 1 2 3) (car-cycle 1 2 3 1 2)))

  (test* "equal? w/ car/cdr-cycle 1" #t
         (equal? (cdr-cycle (car-cycle 1))
                 (cdr-cycle (car-cycle 1))))
  (test* "equal? w/ car/cdr-cycle 2" #f
         (equal? (cdr-cycle (car-cycle 1))
                 (car-cycle (cdr-cycle 1))))
  (test* "equal? w/ car/cdr-cycle 3" #t
         (equal? (cdr-cycle (car-cycle 1 2 3))
                 (cdr-cycle (car-cycle 1 2 3 1 2 3))))
  (test* "equal? w/ car/cdr-cycle 4" #t
         (equal? (cdr-cycle (car-cycle 1 2 3))
                 (cdr-cycle (car-cycle 1 2 3) (car-cycle 1 2 3 1 2 3))))
  (test* "equal? w/ car/cdr-cycle 4" #f
         (equal? (cdr-cycle (car-cycle 1 2 3))
                 (cdr-cycle (car-cycle 1 2 3) (car-cycle 1 2 3 1 2 3 1))))
  )

;;--------------------------------------------------------------------------

(test-section "monotonic-merge")

;; monotonic-merge is a core function to implement Dylan-style class
;; precedence list.  those tests are taken from examples in
;;   http://www.webcom.com/~haahr/dylan/linearization-oopsla96.html

(test "monotonic-merge"
      '(menu choice-widget popup-mixin object)
      (^[] (monotonic-merge
            '((menu choice-widget object)
              (menu popup-mixin)
              (popup-mixin object)))))

(test "monotonic-merge"
      '(pedal-wheel-boat engineless small-catamaran
        small-multihull day-boat wheel-boat boat object)
      (^[] (monotonic-merge
            '((pedal-wheel-boat engineless day-boat wheel-boat boat object)
              (small-catamaran small-multihull day-boat boat object)
              (pedal-wheel-boat small-catamaran)))))

(test "monotonic-merge"
      #f
      (^[] (monotonic-merge
            '((hv-grid vh-grid)
              (hv-grid horizontal-grid vertical-grid grid-layout object)
              (vh-grid vertical-grid horizontal-grid grid-layout object)))))

(test-end)
