;; Test gauche.* extensions

(use gauche.test)
(test-start "collections and sequences")

(use data.queue)
(use srfi-1)
(use srfi-13)
(use gauche.collection)
(use gauche.sequence)

(test-module 'gauche.collection)
(test-module 'gauche.sequence)

;; User-defined collection class test

(define-class <string-seq-meta> (<class>) ())

(define-class <string-seq> (<sequence>)
  ((strings :initform '() :init-keyword :strings))
  :metaclass <string-seq-meta>)

(define-method call-with-iterator ((seq <string-seq>) proc :key (start #f)
                                   :allow-other-keys)
  (let* ([ss (slot-ref seq 'strings)]
         [p  (if start (list-tail ss start) ss)])
    (proc (^[] (null? p))
          (^[] (pop! p)))))

(define-method call-with-builder ((seq <string-seq-meta>) proc . _)
  (let1 q (make-queue)
    (proc (^[item] (enqueue! q (x->string item)))
          (^[] (make <string-seq> :strings (dequeue-all! q))))))

(define-method referencer ((seq <string-seq>))
  (lambda (o i . args)
    (apply list-ref (slot-ref o 'strings) i args)))

(define-method modifier ((seq <string-seq>))
  (lambda (o i v)
    (set! (ref (slot-ref o 'strings) i) (x->string v))))

(define (sseq . elements)
  (make <string-seq> :strings (map x->string elements)))

;;------------------------------------------------------------------
;; basic tests
;;

(test-section "mapping methods")

(test* "fold (list)" '(6 5 4 3 2 1)
       (fold cons '() '(1 2 3 4 5 6)))
(test* "fold (vector)" '(6 5 4 3 2 1)
       (fold cons '() '#(1 2 3 4 5 6)))
(test* "fold (string)" '(#\f #\e #\d #\c #\b #\a)
       (fold cons '() "abcdef"))
(test* "fold (hash-table)" '((c . "c") (b . "b") (a . "a"))
       (fold cons '() (hash-table 'eq? '(a . "a") '(b . "b") '(c . "c")))
       (cut lset= equal? <> <>))
(test* "fold (custom)" '("f" "e" "d" "c" "b" "a")
       (fold cons '() (sseq 'a 'b 'c 'd 'e 'f)))

(test* "fold (n-ary)" '(f 6 e 5 d 4 c 3 b 2 a 1)
       (fold list* '() '(a b c d e f) '(1 2 3 4 5 6)))
(test* "fold (n-ary)" '(f 6 e 5 d 4 c 3 b 2 a 1)
       (fold list* '() '(a b c d e f) '#(1 2 3 4 5 6)))
(test* "fold (n-ary)" '(f 6 e 5 d 4 c 3 b 2 a 1)
       (fold list* '() '#(a b c d e f) '#(1 2 3 4 5 6)))
(test* "fold (n-ary)" '(#\c "c" c #\b "b" b #\a "a" a)
       (fold list* '() "abc" (sseq 'a 'b 'c) '(a b c)))

(test* "fold2" '(21 (6 5 4 3 2 1))
       (values->list
        (fold2 (^[n s m] (values (+ n s) (cons n m)))
               0 '() '(1 2 3 4 5 6))))
(test* "fold2 (n-ary)" '(195 (5 15 25 4 14 24 3 13 23 2 12 22 1 11 21))
       (values->list
        (fold2 (^[n0 n1 n2 s m]
                 (values (+ n0 n1 n2 s)
                         (list* n0 n1 n2 m)))
               0 '() '(1 2 3 4 5 6) '(11 12 13 14 15) '(21 22 23 24 25 26))))

(test* "fold3" '(21 720 (6 5 4 3 2 1))
       (values->list
        (fold3 (^[n s m l] (values (+ n s) (* n m) (cons n l)))
               0 1 '() '(1 2 3 4 5 6))))
(test* "fold3 (n-ary)" '(195 275701345920000
                         (5 15 25 4 14 24 3 13 23 2 12 22 1 11 21))
       (values->list
        (fold3 (^[n0 n1 n2 s m l]
                 (values (+ n0 n1 n2 s)
                         (* n0 n1 n2 m)
                         (list* n0 n1 n2 l)))
               0 1 '()
               '(1 2 3 4 5 6) '(11 12 13 14 15) '(21 22 23 24 25 26))))

(test* "map (list)" '(2 4 6 8 10)
       (map (^x (* x 2)) '(1 2 3 4 5)))
(test* "map (vector)" '(2 4 6 8 10)
       (map (^x (* x 2)) '#(1 2 3 4 5)))
(test* "map (string)" '(2 4 6 8 10)
       (map (^x (* (digit->integer x) 2)) "12345"))
(test* "map (hash-table)" '((c . "c") (b . "b") (a . "a"))
       (map identity (hash-table 'eq? '(a . "a") '(b . "b") '(c . "c")))
       (cut lset= equal? <> <>))
(test* "map (custom)" '(2 4 6 8 10)
       (map (^x (* (string->number x) 2))
            (sseq 1 2 3 4 5)))

(test* "map (n-ary)" '(3 5 7 9 11)
       (map + '(1 2 3 4 5) '(2 3 4 5 6)))
(test* "map (n-ary)" '(3 5 7 9 11)
       (map + '(1 2 3 4 5) '#(2 3 4 5 6)))
(test* "map (n-ary)" '(3 5 7 9 11)
       (map + '#(1 2 3 4 5) '#(2 3 4 5 6)))
(test* "map (n-ary)" '("123" "234" "345" "456")
       (map (^ arg (apply string-append (map x->string arg)))
            "12345" (sseq 2 3 4 5) '#(3 4 5 6 7 8)))

(test* "map-to (list->vector)" '#(2 4 6 8)
       (map-to <vector> (^x (* x 2)) '(1 2 3 4)))
(test* "map-to (list->string)" "2468"
       (map-to <string> (^x (integer->digit (* x 2)))
               '(1 2 3 4)))
(test* "map-to (list->custom)" '("2" "4" "6" "8")
       (slot-ref
        (map-to <string-seq> (^x (* x 2)) '(1 2 3 4))
        'strings))
(test* "map-to (vector->list" '(2 4 6 8)
       (map-to <list> (^x (* x 2)) '#(1 2 3 4)))
(test* "map-to (vector->string" "1234"
       (map-to <string> integer->digit '#(1 2 3 4)))
(test* "map-to (vector->custom)" '("2" "4" "6" "8")
       (slot-ref
        (map-to <string-seq> (^x (* x 2)) '#(1 2 3 4))
        'strings))

(test* "map-to (nary)" '#(3 5 7 9 11)
       (map-to <vector> + '(1 2 3 4 5) '#(2 3 4 5 6)))
(test* "map-to (nary, uneven)" '#(3 5 7 9)
       (map-to <vector> + '(1 2 3 4) '#(2 3 4 5 6)))
(test* "map-to (nary)" '#(3 5 7 9)
       (map-to <vector> + '(1 2 3 4 5) '#(2 3 4 5)))

(test* "map-accum" '((45 30 15) 5)
       (values->list
        (map-accum (^[elt seed] (values (* elt seed) seed))
                   5 '(9 6 3))))
(test* "map-accum" '((10 28 88) 19)
       (values->list
        (map-accum (^[elt seed] (values (* elt seed) (+ elt seed)))
                   5 '(2 4 8))))
(test* "map-accum (nary)" '((10 11 16) 15)
       (values->list
        (map-accum (^[x y seed] (values (+ x y seed) (+ x seed)))
                   1 '(2 4 8) '(7 4 1))))

(test* "for-each (list)" '(5 4 3 2 1)
       (rlet1 p '()
         (for-each (^x (push! p x)) '(1 2 3 4 5))))
(test* "for-each (vector)" '(5 4 3 2 1)
       (rlet1 p '()
         (for-each (^x (push! p x)) '#(1 2 3 4 5))))
(test* "for-each (string)" '(#\5 #\4 #\3 #\2 #\1)
       (rlet1 p '()
         (for-each (^x (push! p x)) "12345")))
(test* "for-each (hash-table)" '((c . "c") (b . "b") (a . "a"))
       (rlet1 p '()
         (for-each (^x (push! p x))
                   (hash-table 'eq? '(a . "a") '(b . "b") '(c . "c"))))
       (cut lset= equal? <> <>))
(test* "for-each (custom)" '("5" "4" "3" "2" "1")
       (rlet1 p '()
         (for-each (^x (push! p x)) (sseq 1 2 3 4 5))))

(test-section "searching and selection")

(test* "find (list)" 4
       (find even? '(3 1 7 5 4 8 7)))
(test* "find (vector)" 4
       (find even? '#(3 1 7 5 4 8 7)))
(test* "find (string)" #\a
       (find char-lower-case? "YAEUB4309aBrnar"))
(test* "find (hash-table)" '(b . "b")
       (find (^x (string=? "b" (cdr x)))
             (hash-table 'eq? '(a . "a") '(b . "b") '(c . "c"))))
(test* "find (custom)" "zoo"
       (find (^s (= (size-of s) 3))
             (sseq 'najr 'ej 'zoo 'bunr)))

(define (test-find-minmax msg ex-min ex-max coll . args)
  (test* #"find-min (~msg)" ex-min (apply find-min coll args))
  (test* #"find-max (~msg)" ex-max (apply find-max coll args))
  (test* #"find-min&max (~msg)" (list ex-min ex-max)
         (values->list (apply find-min&max coll args))))


(test-find-minmax "list 0" -1 9
                  '(3 9 -1 6))
(test-find-minmax "list 1" '(c . -1) '(b . 9)
                  '((a . 3) (b . 9) (c . -1) (d . 6)) :key cdr)
(test-find-minmax "list 2" '(c . -1) '(b . 9)
                  '((c . -1) (a . 3) (b . 9) (d . 6)) :key cdr)
(test-find-minmax "list 3" '(c . -1) '(c . -1)
                  '((c . -1)) :key cdr)
(test-find-minmax "list 4" #f #f
                  '() :key cdr)
(test-find-minmax "list 5" 'foo 'foo
                  '() :key cdr :default 'foo)
(test-find-minmax "list 6" '("a" . 3) '("d" . 6)
                  '(("c" . -1) ("b" . 9) ("a" . 3) ("d" . 6))
                  :key car :compare string<?)
(test-find-minmax "vector" '(c . -1) '(b . 9)
                  '#((a . 3) (b . 9) (c . -1) (d . 6)) :key cdr)
(test-find-minmax "string" '#\a '#\Z
                  "IBaenRuZgaeKG" :compare char-ci<?)
(test-find-minmax "hash-table" '(c . -1) '(b . 9)
                  (hash-table 'eq? '(a . 3) '(b . 9) '(c . -1) '(d . 6))
                  :key cdr)

(test* "filter (list)" '(2 4 6)
       (filter even? '(1 2 3 4 5 6 7)))
(test* "filter (vector)" '(2 4 6)
       (filter even? '#(1 2 3 4 5 6 7)))
(test* "filter (string)" '(#\a #\r #\b)
       (filter char-lower-case? "UBaBrGLbO"))
(test* "filter (hash-table)" '((b . "b"))
       (filter (^x (string=? "b" (cdr x)))
               (hash-table 'eq? '(a . "a") '(b . "b") '(c . "c")))
       (cut lset= equal? <> <>))
(test* "filter (custom)" '("zoo" "zn")
       (filter (^s (string-prefix? "z" s))
               (sseq 'urnb 'zoo 'nbak 'zn 'run)))

(test* "filter-to (vector)" '#(2 4 6)
       (filter-to <vector> even? '#(1 2 3 4 5 6 7)))
(test* "filter-to (custom)" '("2" "4" "6")
       (slot-ref
        (filter-to <string-seq> even? '#(1 2 3 4 5 6 7))
        'strings))
(test* "filter-to (hash-table)" '((b . "b") (z . "b"))
       (hash-table-map
        (filter-to <hash-table>
                   (^x (string=? "b" (cdr x)))
                   (hash-table 'eq?
                               '(a . "a") '(b . "b")
                               '(c . "c") '(z . "b")))
        cons)
       (cut lset= equal? <> <>))

(test* "remove (list)" '(1 3 5 7)
       (remove even? '(1 2 3 4 5 6 7)))
(test* "remove (vector)" '(1 3 5 7)
       (remove even? '#(1 2 3 4 5 6 7)))
(test* "remove (string)" '(#\U #\B #\B #\G #\L #\O)
       (remove char-lower-case? "UBaBrGLbO"))
(test* "remove (hash-table)" '((a . "a") (c . "c"))
       (remove (^x (string=? "b" (cdr x)))
               (hash-table 'eq? '(a . "a") '(b . "b") '(c . "c")))
       (cut lset= equal? <> <>))
(test* "remove (custom)" '("urnb" "nbak" "run")
       (remove (^s (string-prefix? "z" s))
               (sseq 'urnb 'zoo 'nbak 'zn 'run)))

(test* "remove-to (vector)" '#(1 3 5 7)
       (remove-to <vector> even? '#(1 2 3 4 5 6 7)))
(test* "remove-to (hash-table)" '((a . "a") (c . "c"))
       (hash-table-map
        (remove-to <hash-table>
                   (^x (string=? "b" (cdr x)))
                   (hash-table 'eq?
                               '(a . "a") '(b . "b")
                               '(c . "c") '(z . "b")))
        cons)
       (cut lset= equal? <> <>))
(test* "remove-to (custom)" '("1" "3" "5" "7")
       (slot-ref
        (remove-to <string-seq> even? '#(1 2 3 4 5 6 7))
        'strings))

(test* "partition (list)" '((2 4 6) (1 3 5 7))
       (values->list (partition even? '(1 2 3 4 5 6 7))))
(test* "partition (vector)" '((2 4 6) (1 3 5 7))
       (values->list (partition even? '#(1 2 3 4 5 6 7))))
(test* "partition (custom)" '(("2" "4" "6") ("1" "3" "5" "7"))
       (values->list (partition (^e (even? (string->number e)))
                                (sseq 1 2 3 4 5 6 7))))

(test* "partition-to (string)" '("ACE" "bdf")
       (values->list (partition-to <string> char-upper-case? "AbCdEf")))
(test* "partition-to (vector)" '(#(2 4 6) #(1 3 5 7))
       (values->list (partition-to <vector> even? '#(1 2 3 4 5 6 7))))

(test-section "miscellaneous")

(test* "size-of (list)"   5 (size-of '(1 2 3 4 5)))
(test* "size-of (vector)" 5 (size-of '#(1 2 3 4 5)))
(test* "size-of (string)" 5 (size-of "12345"))
(test* "size-of (custom)" 5 (size-of (sseq 1 2 3 4 5)))
(test* "size-of (hash-table)" 4
       (size-of (hash-table 'eq? '(a . "a") '(b . "b") '(c . "c") '(z . "b"))))
(test* "size-of (char-set)" 0 (size-of (char-set)))
(test* "size-of (char-set)" 5 (size-of #[abAB0]))

(test* "coerce-to (list->list)" '(1 2 3)
       (coerce-to <list> '(1 2 3)))
(test* "coerce-to (list->vector)" '#(1 2 3)
       (coerce-to <vector> '(1 2 3)))
(test* "coerce-to (list->string)" "123"
       (coerce-to <string> '(#\1 #\2 #\3)))
(test* "coerce-to (list->hash-table)" '((a . "a") (b . "b") (c . "c"))
       (hash-table-map
        (coerce-to <hash-table> '((a . "a") (b . "b") (c . "c")))
        cons)
       (cut lset= equal? <> <>))
(test* "coerce-to (list->custom)" '("1" "2" "3")
       (slot-ref (coerce-to <string-seq> '(#\1 #\2 #\3)) 'strings))
(test* "coerce-to (vector->list)" '(1 2 3)
       (coerce-to <list> '#(1 2 3)))
(test* "coerce-to (vector->vector)" '#(1 2 3)
       (coerce-to <vector> '#(1 2 3)))
(test* "coerce-to (vector->string)" "123"
       (coerce-to <string> '#(#\1 #\2 #\3)))
(test* "coerce-to (vector->custom)" '("1" "2" "3")
       (slot-ref (coerce-to <string-seq> '#(#\1 #\2 #\3)) 'strings))
(test* "coerce-to (string->list)" '(#\1 #\2 #\3)
       (coerce-to <list> "123"))
(test* "coerce-to (string->vector)" '#(#\1 #\2 #\3)
       (coerce-to <vector> "123"))
(test* "coerce-to (string->string)" "123"
       (coerce-to <string> "123"))
(test* "coerce-to (string->custom)" '("1" "2" "3")
       (slot-ref (coerce-to <string-seq> "123") 'strings))
(test* "coerce-to (hash-table->list)" '((a . "a") (b . "b") (c . "c"))
       (coerce-to <list>
                  (hash-table 'eq? '(a . "a") '(b . "b") '(c . "c")))
       (cut lset= equal? <> <>))
(test* "coerce-to (hash-table->hash-table)" '((a . "a") (b . "b") (c . "c"))
       (hash-table-map
        (coerce-to <hash-table>
                   (hash-table 'eq? '(a . "a") '(b . "b") '(c . "c")))
        cons)
       (cut lset= equal? <> <>))
(test* "coerce-to (custom->list)" '("1" "2" "3")
       (coerce-to <list> (sseq 1 2 3)))
(test* "coerce-to (custom->vector)" '#("1" "2" "3")
       (coerce-to <vector> (sseq 1 2 3)))
(test* "coerce-to (custom->custom)" '("1" "2" "3")
       (slot-ref (coerce-to <string-seq> (sseq 1 2 3)) 'strings))

(test* "group-collection" '((1 1 1) (2 2 2 2 2) (3 3 3 3))
       (group-collection '(1 2 3 2 3 1 2 1 2 3 2 3)))
(test* "group-collection w/test" '((1 3 3 1 1 3 3) (2 2 2 2 2))
       (group-collection '(1 2 3 2 3 1 2 1 2 3 2 3)
                         :test (^[x y] (= (modulo x 2) (modulo y 2)))))
(test* "group-collection w/key" '(((1 a) (1 c)) ((2 b) (2 q)) ((3 c) (3 d)))
       (group-collection '((1 a) (2 b) (3 c) (1 c) (3 d) (2 q))
                         :key car))
(test* "group-collection (vector)" '((1 1 1) (2 2 2 2 2) (3 3 3 3))
       (group-collection '#(1 2 3 2 3 1 2 1 2 3 2 3)))
(test* "group-collection (string)" '((#\a #\a #\a #\a #\a) (#\b #\b)
                                     (#\r #\r) (#\c) (#\d))
       (group-collection "abracadabra"))

(test-section "sequence operations")

(test* "ref (list)" 3     (ref '(1 2 3 4 5) 2))
(test* "ref (vector)" 3   (ref '#(1 2 3 4 5) 2))
(test* "ref (string)" #\3 (ref "12345" 2))
(test* "ref (custom)" "3" (ref (sseq 1 2 3 4 5) 2))

(test* "(setter ref) (list)" '(1 2 a 4 5)
       (let1 x (list 1 2 3 4 5)
         (set! (ref x 2) 'a) x))
(test* "(setter ref) (vector)" '#(1 2 a 4 5)
       (let1 x (vector 1 2 3 4 5)
         (set! (ref x 2) 'a) x))
(test* "(setter ref) (string)" "12a45"
       (let1 x (string-copy "12345")
         (set! (ref x 2) #\a) x))
(test* "(setter ref) (custom)" '("1" "2" "a" "4" "5")
       (let1 x (sseq 1 2 3 4 5)
         (set! (ref x 2) 'a) (slot-ref x 'strings)))

(test* "subseq (list)" '(3 4 5)
       (subseq '(1 2 3 4 5) 2))
(test* "subseq (list)" '(3 4)
       (subseq '(1 2 3 4 5) 2 4))
(test* "subseq (list)" '(1 2 3 4)
       (subseq '(1 2 3 4 5) 0 -1))
(test* "subseq (vector)" '#(3 4 5)
       (subseq '#(1 2 3 4 5) 2))
(test* "subseq (vector)" '#(3 4)
       (subseq '#(1 2 3 4 5) 2 4))
(test* "subseq (vector)" '#(1 2 3 4)
       (subseq '#(1 2 3 4 5) 0 -1))
(test* "subseq (string)" "345"
       (subseq "12345" 2))
(test* "subseq (string)" "34"
       (subseq "12345" 2 4))
(test* "subseq (string)" "1234"
       (subseq "12345" 0 -1))
(test* "subseq (custom)" '("3" "4" "5")
       (slot-ref (subseq (sseq 1 2 3 4 5) 2) 'strings))
(test* "subseq (custom)" '("3" "4")
       (slot-ref (subseq (sseq 1 2 3 4 5) 2 4) 'strings))
(test* "subseq (custom)" '("1" "2" "3" "4")
       (slot-ref (subseq (sseq 1 2 3 4 5) 0 -1) 'strings))

(test* "for-each-with-index (list)" '((2 . c) (1 . b) (0 . a))
       (rlet1 r '()
         (for-each-with-index (^[i e] (push! r (cons i e))) '(a b c))))
(test* "for-each-with-index (vector)" '((2 . c) (1 . b) (0 . a))
       (rlet1 r '()
         (for-each-with-index (^[i e] (push! r (cons i e))) '#(a b c))))
(test* "for-each-with-index (string)" '((2 . #\c) (1 . #\b) (0 . #\a))
       (rlet1 r '()
         (for-each-with-index (^[i e] (push! r (cons i e))) "abc")))
(test* "for-each-with-index (custom)" '((2 . "c") (1 . "b") (0 . "a"))
       (rlet1 r '()
         (for-each-with-index (^[i e] (push! r (cons i e)))
                              (sseq 'a 'b 'c))))
(test* "for-each-with-index (custom)" '((2 "c" c c) (1 "b" b b) (0 "a" a a))
       (rlet1 r '()
         (for-each-with-index (^ x (push! r x))
                              (sseq 'a 'b 'c)
                              '(a b c)
                              '#(a b c))))
(test* "for-each-with-index (boundary)" '()
       (rlet1 r '()
         (for-each-with-index (^[i e] (push! r (cons i e))) '())))
(test* "for-each-with-index (boundary)" '()
       (rlet1 r '()
         (for-each-with-index (^[i e] (push! r (cons i e))) (sseq))))
(test* "for-each-with-index (boundary)" '()
       (rlet1 r '()
         (for-each-with-index (^[i e] (push! r (cons i e))) (sseq) '())))

(test* "map-with-index (list)" '((0 . a) (1 . b) (2 . c))
       (map-with-index cons '(a b c)))
(test* "map-with-index (vector)" '((0 . a) (1 . b) (2 . c))
       (map-with-index cons '#(a b c)))
(test* "map-with-index (string)" '((0 . #\a) (1 . #\b) (2 . #\c))
       (map-with-index cons "abc"))
(test* "map-with-index (custom)" '((0 . "a") (1 . "b") (2 . "c"))
       (map-with-index cons (sseq 'a 'b 'c)))
(test* "map-with-index (custom)" '((0 "a" a a) (1 "b" b b) (2 "c" c c))
       (map-with-index list (sseq 'a 'b 'c) '(a b c) '#(a b c)))
(test* "map-with-index (boundary)" '()
       (map-with-index cons '()))
(test* "map-with-index (boundary)" '()
       (map-with-index cons (sseq)))
(test* "map-with-index (boundary)" '()
       (map-with-index cons (sseq) '()))

(test* "map-to-with-index (list->list)" '((0 . a) (1 . b) (2 . c))
       (map-to-with-index <list> cons '(a b c)))
(test* "map-to-with-index (list->vector)" '#((0 . a) (1 . b) (2 . c))
       (map-to-with-index <vector> cons '(a b c)))
(test* "map-to-with-index (vector->vector)" '#((0 . a) (1 . b) (2 . c))
       (map-to-with-index <vector> cons '#(a b c)))
(test* "map-to-with-index (string->vector)" '#((0 . #\a) (1 . #\b) (2 . #\c))
       (map-to-with-index <vector> cons "abc"))
(test* "map-to-with-index (custom)" '#((0 . "a") (1 . "b") (2 . "c"))
       (map-to-with-index <vector> cons (sseq 'a 'b 'c)))
(test* "map-to-with-index (custom)" '#((0 "a" a a) (1 "b" b b) (2 "c" c c))
       (map-to-with-index <vector> list (sseq 'a 'b 'c) '(a b c) '#(a b c)))
(test* "map-to-with-index (boundary)" '#()
       (map-to-with-index <vector> cons '()))
(test* "map-to-with-index (boundary)" '#()
       (map-to-with-index <vector> cons (sseq)))
(test* "map-to-with-index (boundary)" '#()
       (map-to-with-index <vector> cons (sseq) '()))
(test* "map-to-with-index (uneven lengths)" '#((0 a d) (1 b e))
       (map-to-with-index <vector> list '#(a b c) '#(d e)))

(test* "fold-with-index (list)" '((2 . a) (1 . b) (0 . c))
       (fold-with-index acons '() '(c b a)))
(test* "fold-with-index (vector)" '((2 . a) (1 . b) (0 . c))
       (fold-with-index acons '() '#(c b a)))
(test* "fold-with-index (string)" '((2 . #\a) (1 . #\b) (0 . #\c))
       (fold-with-index acons '() "cba"))
(test* "fold-with-index (custom)" '((2 . "a") (1 . "b") (0 . "c"))
       (fold-with-index acons '() (sseq 'c 'b 'a)))
(test* "fold-with-index (custom)" '((2 "a" a a) (1 "b" b b) (0 "c" c c))
       (fold-with-index (^[i x y z r]
                          (cons (list i x y z) r))
                        '()
                        (sseq 'c 'b 'a) '(c b a) '#(c b a)))
(test* "fold-with-index (boundary)" '()
       (fold-with-index acons '() '()))
(test* "fold-with-index (boundary)" '()
       (fold-with-index acons '() (sseq)))
(test* "fold-with-index (boundary)" '()
       (fold-with-index acons '() (sseq) '() '#()))

(test* "find-with-index (list)" '(2 c)
       (values->list (find-with-index (cut eq? 'c <>) '(a b c d e))))
(test* "find-with-index (list)" '(#f #f)
       (values->list (find-with-index (cut eq? 'f <>) '(a b c d e))))
(test* "find-with-index (vector)" '(2 c)
       (values->list (find-with-index (cut eq? 'c <>) '#(a b c d e))))
(test* "find-with-index (vector)" '(#f #f)
       (values->list (find-with-index (cut eq? 'f <>) '#(a b c d e))))
(test* "find-with-index (string)" '(2 #\c)
       (values->list (find-with-index (cut eqv? #\c <>) "abcde")))
(test* "find-with-index (string)" '(#f #f)
       (values->list (find-with-index (cut eqv? #\f <>) "abcde")))
(test* "find-with-index (custom)" '(2 "c")
       (values->list (find-with-index (cut equal? "c" <>) (sseq 'a 'b 'c 'd 'e))))
(test* "find-with-index (custom)" '(#f #f)
       (values->list (find-with-index (cut equal? "f" <>) (sseq 'a 'b 'c 'd 'e))))
(test* "find-with-index (boundary)" '(#f #f)
       (values->list (find-with-index (cut eq? 'c <>) '())))
(test* "find-with-index (boundary)" '(#f #f)
       (values->list (find-with-index (cut eq? 'c <>) '#())))
(test* "find-with-index (boundary)" '(#f #f)
       (values->list (find-with-index (cut eq? 'c <>) (sseq))))

(test* "find-index (list)" 2
       (find-index (cut eq? 'c <>) '(a b c d e)))
(test* "find-index (vector)" 2
       (find-index (cut eq? 'c <>) '#(a b c d e)))
(test* "find-index (string)" 2
       (find-index (cut eqv? #\c <>) "abcde"))
(test* "find-index (custom)" 2
       (find-index (cut equal? "c" <>) (sseq 'a 'b 'c 'd 'e)))

(test* "fold-right (list)" '(a b c d e)
       (fold-right cons '() '(a b c d e)))
(test* "fold-right (vector)" '(a b c d e)
       (fold-right cons '() '#(a b c d e)))
(test* "fold-right (string)" '(#\a #\b #\c #\d #\e)
       (fold-right cons '() "abcde"))
(test* "fold-right (custom)" '("a" "b" "c" "d" "e")
       (fold-right cons '() (sseq 'a 'b 'c 'd 'e)))
(test* "fold-right (list+list)" '(a 0 b 1 c 2 d 3 e 4)
       (fold-right cons* '() '(a b c d e) '(0 1 2 3 4)))
(test* "fold-right (list+vector)" '(a 0 b 1 c 2 d 3 e 4)
       (fold-right cons* '() '(a b c d e) '#(0 1 2 3 4)))
(test* "fold-right (string+vector)" '(#\a 0 #\b 1 #\c 2 #\d 3 #\e 4)
       (fold-right cons* '() "abcde" '#(0 1 2 3 4)))
(test* "fold-right (different lengths)" '(#\a 0 x #\b 1 y #\c 2 z)
       (fold-right cons* '() "abcd" '#(0 1 2 3 4) '(x y z)))
(test* "fold-right (different lengths)" '(#\a 0 u #\b 1 v #\c 2 w #\d 3 x)
       (fold-right cons* '() "abcdef" '#(0 1 2 3) '(u v w x y z)))
(test* "fold-right (different lengths)" '(#\a 0 u #\b 1 v)
       (fold-right cons* '() "ab" '#(0 1 2 3 4) '(u v w x y z)))

(test* "group-sequence" '((1 1 1) (2) (3) (4 4) (2 2) (3) (1 1) (3))
       (group-sequence '(1 1 1 2 3 4 4 2 2 3 1 1 3)))
(test* "group-sequence w/key" '((1 1 1) (2) (3) (4 4 2 2) (3 1 1 3))
       (group-sequence '(1 1 1 2 3 4 4 2 2 3 1 1 3) :key (cut modulo <> 2)))
(test* "group-sequence w/key" '((1 1 1 2) (3 4 4) (2 2) (3) (1 1) (3))
       (group-sequence '(1 1 1 2 3 4 4 2 2 3 1 1 3) :key (cut < <> 3)))
(test* "group-sequence w/test" '((1 1 1) (2) (3) (4 4 2 2) (3 1 1 3))
       (group-sequence '(1 1 1 2 3 4 4 2 2 3 1 1 3)
                       :test (^[x y] (= (modulo x 2) (modulo y 2)))))

(test* "group-contiguous-sequence" '()
       (group-contiguous-sequence '()))
(test* "group-contiguous-sequence" '((1))
       (group-contiguous-sequence '(1)))
(test* "group-contiguous-sequence" '((1 2))
       (group-contiguous-sequence '(1 2)))
(test* "group-contiguous-sequence" '((1) (3))
       (group-contiguous-sequence '(1 3)))
(test* "group-contiguous-sequence" '((1 2 3))
       (group-contiguous-sequence '(1 2 3)))
(test* "group-contiguous-sequence" '((1) (3 4 5))
       (group-contiguous-sequence '(1 3 4 5)))
(test* "group-contiguous-sequence"
       '((1 2 3 4) (7 8 9) (11) (13 14) (16))
       (group-contiguous-sequence '(1 2 3 4 7 8 9 11 13 14 16)))
(test* "group-contiguous-sequence" '((#\a #\b #\c) (#\e) (#\g #\h))
       ($ group-contiguous-sequence "abcegh"
          :next (^c (integer->char (+ 1 (char->integer c))))
          :test char=?))
(test* "group-contiguous-sequence squeeze" '()
       (group-contiguous-sequence '() :squeeze #t))
(test* "group-contiguous-sequence squeeze" '((1))
       (group-contiguous-sequence '(1) :squeeze #t))
(test* "group-contiguous-sequence squeeze" '((1 2))
       (group-contiguous-sequence '(1 2) :squeeze #t))
(test* "group-contiguous-sequence squeeze" '((1) (3))
       (group-contiguous-sequence '(1 3) :squeeze #t))
(test* "group-contiguous-sequence squeeze" '((1 3))
       (group-contiguous-sequence '(1 2 3) :squeeze #t))
(test* "group-contiguous-sequence squeeze" '((1) (3 5))
       (group-contiguous-sequence '(1 3 4 5) :squeeze #t))
(test* "group-contiguous-sequence squeeze" '((1 4) (7 9) (11) (13 14) (16))
       (group-contiguous-sequence '(1 2 3 4 7 8 9 11 13 14 16) :squeeze #t))
(test* "group-contiguous-sequence squeeze" '((#\a #\c) (#\e) (#\g #\h))
       ($ group-contiguous-sequence "abcegh"
          :next (^c (integer->char (+ 1 (char->integer c))))
          :test char=?
          :squeeze #t))

(test* "delete-neighbor-dups" '(1 2 3 4 2 3 1 3)
       (delete-neighbor-dups '(1 1 1 2 3 4 4 2 2 3 1 1 3)))
(test* "delete-neighbor-dups" '#(1 2 3 4 2 3 1 3)
       (delete-neighbor-dups '#(1 1 1 2 3 4 4 2 2 3 1 1 3)))
(test* "delete-neighbor-dups" "12342313"
       (delete-neighbor-dups "1112344223113"))
(test* "delete-neighbor-dups" '()
       (delete-neighbor-dups '()))
(test* "delete-neighbor-dups" '(1)
       (delete-neighbor-dups '(1 1 1 1 1)))
(test* "delete-neighbor-dups w/key" '(1 2 3 4 3)
       (delete-neighbor-dups '(1 1 1 2 3 4 4 2 2 3 1 1 3)
                             :key (cut modulo <> 2)))
(test* "delete-neighbor-dups w/test" '(1 2 3 4 3)
       (delete-neighbor-dups '(1 1 1 2 3 4 4 2 2 3 1 1 3)
                             :test (^[x y] (= (modulo x 2) (modulo y 2)))))
(test* "delete-neighbor-dups w/range" "12342"
       (delete-neighbor-dups "1112344223113" :start 1 :end 9))
(test* "delete-neighbor-dups w/range" "2342"
       (delete-neighbor-dups "1112344223113" :start 3 :end 9))
(test* "delete-neighbor-dups w/range" ""
       (delete-neighbor-dups "1112344223113" :start 2 :end 2))
(test* "delete-neighbor-dups w/range" "4"
       (delete-neighbor-dups "1112344223113" :start 5 :end 6))
(test* "delete-neighbor-dups w/range" "4"
       (delete-neighbor-dups "1112344223113" :start 5 :end 7))

(test* "delete-neighbor-dups!" '(8 . #(1 2 3 4 2 3 1 3 2 3 1 1 3))
       (let* ([v (vector-copy '#(1 1 1 2 3 4 4 2 2 3 1 1 3))]
              [r (delete-neighbor-dups! v)])
         (cons r v)))
(test* "delete-neighbor-dups! w/range"
       '(10 . #(1 1 1 2 3 4 2 3 1 3 1 1 3))
       (let* ([v (vector-copy '#(1 1 1 2 3 4 4 2 2 3 1 1 3))]
              [r (delete-neighbor-dups! v :start 2)])
         (cons r v)))
(test* "delete-neighbor-dups! w/range"
       '(10 . (1 1 1 2 3 4 2 3 1 3 1 1 3))
       (let* ([v (list-copy '(1 1 1 2 3 4 4 2 2 3 1 1 3))]
              [r (delete-neighbor-dups! v :start 2)])
         (cons r v)))

(let ([data '(1 1 1 2 3 4 4 2 2 3 1 1 3 3)])
  (test* "delete-neighbor-dups-squeeze!" '(#t #t)
         (let* ([l0 (list-copy data)]
                [l2 (delete-neighbor-dups-squeeze! l0)])
           (list (eq? l0 l2) (equal? l2 (delete-neighbor-dups l0)))))
  (test* "delete-neighbor-dups-squeeze! w/range"
         '(1 2 3 4 2 3 1 3)
         (delete-neighbor-dups-squeeze! (list-copy data) :start 1))
  (test* "delete-neighbor-dups-squeeze! w/range"
         '(1 2 3 4 2)
         (delete-neighbor-dups-squeeze! (list-copy data) :end 8))
  )

(test* "sequence-contains" '(#f 0 9 #f)
       (let1 pat "abrabrabre"
         (list (sequence-contains "abracadabra" pat)
               (sequence-contains "abrabrabrebrea" pat)
               (sequence-contains "abrabrabrabrabrabrebra" pat)
               (sequence-contains "abrabrabrabrabrabrabra" pat))))
(test* "sequence-contains" 0
       (sequence-contains "abracadabra" '()))

(let ([data
       ;; (input needle before after)
       '(((1 2 3 4 5 4 5 6 7 8 9) (4 5 4 5 6) (1 2 3) (4 5 4 5 6 7 8 9))
         ((4 5 4 5 6)             (4 5 4 5 6) ()      (4 5 4 5 6))
         ((4 5 4 5 5 4 5 4 5 6)   (4 5 4 5 6) (4 5 4 5 5) (4 5 4 5 6))
         (()                      (4 5 4 5 6) ()      ())
         ((4 5 6 4 5 4 5 1)       (4 5 4 5 6) (4 5 6 4 5 4 5 1) ())
         ((4 5 6 4 5 4 5 1)       ()          () (4 5 6 4 5 4 5 1))
         )])
  (define (tester !? in needle before after)
    (test* (format "break-list-by-sequence~a ~s" (if !? '! "") in)
           (list before after)
           (receive (a b) ((if !?
                             break-list-by-sequence!
                             break-list-by-sequence)
                           in needle)
             (and (or (not !?)
                      (null? a)
                      (eq? a in))  ; check if head part is shared
                  (list a b)))))
    
  (dolist [datum data]
    (apply tester #f datum)
    (apply tester #t datum)))
  
(test* "common-prefix" '(a b c)
       (common-prefix-to <list> '(a b c d e) '(a b c e d)))
(test* "common-prefix" '(a b c)
       (common-prefix '(a b c d e) '#(a b c e d)))
(test* "common-prefix" '#(a b c)
       (common-prefix '#(a b c e d) '(a b c d e)))
(test* "common-prefix" "xyz"
       (common-prefix "xyz" "xyzw"))
(test* "common-prefix" ""
       (common-prefix "xyz" "abc"))
(test* "common-prefix" '()
       (common-prefix '(a b c) ""))
(test* "common-prefix" "ABC"
       (common-prefix "ABCE" "abcd" :test char-ci=?))

(define (permute-tester msg expected source order . fallback)
  (define (unit type elt-coercer order-type)
    (test* #"permute~msg ~type by ~order-type"
           (map-to type elt-coercer expected)
           (apply permute
                  (map-to type elt-coercer source)
                  (coerce-to order-type order)
                  (map elt-coercer fallback)))
    (test* #"permute!~msg ~type by ~order-type"
           (if (= (size-of source) (size-of expected))
             (map-to type elt-coercer expected)
             *test-error*)
           (let1 imp (map-to type elt-coercer source)
             (apply permute!
                    imp
                    (coerce-to order-type order)
                    (map elt-coercer fallback))
             imp)))
  (dolist (order-type `(,<list> ,<vector>))
    (dolist (t `((,<list> ,values)
                 (,<vector> ,values)
                 (,<string> ,(^[elt] (string-ref (x->string elt) 0)))))
      (unit (car t) (cadr t) order-type)))
  )

(permute-tester "" '(d a c b) '(a b c d) '(3 0 2 1))
(permute-tester " (short)" '(d a) '(a b c d) '(3 0))
(permute-tester " (long)"  '(d a z c b) '(a b c d) '(3 0 4 2 1) 'z)

(define (shuffle-tester source)
  (define (cmp a b)
    (lset= eqv? (coerce-to <list> a) (coerce-to <list> b)))
  (define (unit type)
    (test* #"shuffle ~type"
           (coerce-to type source)
           (shuffle (coerce-to type source))
           cmp)
    (test* #"shuffle! ~type"
           (coerce-to type source)
           (let1 imp (coerce-to type source)
             (shuffle! imp)
             imp)
           cmp))
  (unit <list>) (unit <vector>) (unit <string>))

(shuffle-tester '(#\a #\b #\c #\d #\e #\f #\g))
(shuffle-tester '())

(test-end)
