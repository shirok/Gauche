;;
;; testing collections and sequences
;;

(use gauche.test)
(test-start "collections and sequences")

(use util.queue)
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

(define-method call-with-iterator ((seq <string-seq>) proc . opts)
  (let* ((start (get-keyword :start opts #f))
         (ss    (slot-ref seq 'strings))
         (p     (if start (list-tail ss start) ss)))
    (proc (lambda () (null? p))
          (lambda () (pop! p)))))

(define-method call-with-builder ((seq <string-seq-meta>) proc . opts)
  (let ((q (make-queue)))
    (proc (lambda (item) (enqueue! q (x->string item)))
          (lambda () (make <string-seq> :strings (dequeue-all! q))))))

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

(test* "map (list)" '(2 4 6 8 10)
       (map (lambda (x) (* x 2)) '(1 2 3 4 5)))
(test* "map (vector)" '(2 4 6 8 10)
       (map (lambda (x) (* x 2)) '#(1 2 3 4 5)))
(test* "map (string)" '(2 4 6 8 10)
       (map (lambda (x) (* (digit->integer x) 2)) "12345"))
(test* "map (custom)" '(2 4 6 8 10)
       (map (lambda (x) (* (string->number x) 2))
            (sseq 1 2 3 4 5)))

(test* "map (n-ary)" '(3 5 7 9 11)
       (map + '(1 2 3 4 5) '(2 3 4 5 6)))
(test* "map (n-ary)" '(3 5 7 9 11)
       (map + '(1 2 3 4 5) '#(2 3 4 5 6)))
(test* "map (n-ary)" '(3 5 7 9 11)
       (map + '#(1 2 3 4 5) '#(2 3 4 5 6)))
(test* "map (n-ary)" '("123" "234" "345" "456")
       (map (lambda arg (apply string-append (map x->string arg)))
            "12345" (sseq 2 3 4 5) '#(3 4 5 6 7 8)))

(test* "map-to (list->vector)" '#(2 4 6 8)
       (map-to <vector> (lambda (x) (* x 2)) '(1 2 3 4)))
(test* "map-to (list->string)" "2468"
       (map-to <string> (lambda (x) (integer->digit (* x 2)))
               '(1 2 3 4)))
(test* "map-to (list->custom)" '("2" "4" "6" "8")
       (slot-ref
        (map-to <string-seq> (lambda (x) (* x 2)) '(1 2 3 4))
        'strings))
(test* "map-to (vector->list" '(2 4 6 8)
       (map-to <list> (lambda (x) (* x 2)) '#(1 2 3 4)))
(test* "map-to (vector->string" "1234"
       (map-to <string> integer->digit '#(1 2 3 4)))
(test* "map-to (vector->custom)" '("2" "4" "6" "8")
       (slot-ref
        (map-to <string-seq> (lambda (x) (* x 2)) '#(1 2 3 4))
        'strings))

(test* "map-to (nary)" '#(3 5 7 9 11)
       (map-to <vector> + '(1 2 3 4 5) '#(2 3 4 5 6)))

(test* "for-each (list)" '(5 4 3 2 1)
       (let ((p '()))
         (for-each (lambda (x) (push! p x)) '(1 2 3 4 5))
         p))
(test* "for-each (vector)" '(5 4 3 2 1)
       (let ((p '()))
         (for-each (lambda (x) (push! p x)) '#(1 2 3 4 5))
         p))
(test* "for-each (string)" '(#\5 #\4 #\3 #\2 #\1)
       (let ((p '()))
         (for-each (lambda (x) (push! p x)) "12345")
         p))
(test* "for-each (custom)" '("5" "4" "3" "2" "1")
       (let ((p '()))
         (for-each (lambda (x) (push! p x)) (sseq 1 2 3 4 5))
         p))

(test-section "searching and selection")

(test* "find (list)" 4
       (find even? '(3 1 7 5 4 8 7)))
(test* "find (vector)" 4
       (find even? '#(3 1 7 5 4 8 7)))
(test* "find (string)" #\a
       (find char-lower-case? "YAEUB4309aBrnar"))
(test* "find (custom)" "zoo"
       (find (lambda (s) (= (size-of s) 3))
             (sseq 'najr 'ej 'zoo 'bunr)))

(test* "filter (list)" '(2 4 6)
       (filter even? '(1 2 3 4 5 6 7)))
(test* "filter (vector)" '(2 4 6)
       (filter even? '#(1 2 3 4 5 6 7)))
(test* "filter (string)" '(#\a #\r #\b)
       (filter char-lower-case? "UBaBrGLbO"))
(test* "filter (custom)" '("zoo" "zn")
       (filter (lambda (s) (string-prefix? "z" s))
               (sseq 'urnb 'zoo 'nbak 'zn 'run)))

(test* "filter-to (vector)" '#(2 4 6)
       (filter-to <vector> even? '#(1 2 3 4 5 6 7)))
(test* "filter-to (custom)" '("2" "4" "6")
       (slot-ref
        (filter-to <string-seq> even? '#(1 2 3 4 5 6 7))
        'strings))

(test* "remove (list)" '(1 3 5 7)
       (remove even? '(1 2 3 4 5 6 7)))
(test* "remove (vector)" '(1 3 5 7)
       (remove even? '#(1 2 3 4 5 6 7)))
(test* "remove (string)" '(#\U #\B #\B #\G #\L #\O)
       (remove char-lower-case? "UBaBrGLbO"))
(test* "remove (custom)" '("urnb" "nbak" "run")
       (remove (lambda (s) (string-prefix? "z" s))
               (sseq 'urnb 'zoo 'nbak 'zn 'run)))

(test* "remove-to (vector)" '#(1 3 5 7)
       (remove-to <vector> even? '#(1 2 3 4 5 6 7)))
(test* "remove-to (custom)" '("1" "3" "5" "7")
       (slot-ref
        (remove-to <string-seq> even? '#(1 2 3 4 5 6 7))
        'strings))

(test* "partition (list)" '((2 4 6) (1 3 5 7))
       (receive r (partition even? '(1 2 3 4 5 6 7))
         r))
(test* "partition (vector)" '((2 4 6) (1 3 5 7))
       (receive r (partition even? '#(1 2 3 4 5 6 7))
         r))
(test* "partition (custom)" '(("2" "4" "6") ("1" "3" "5" "7"))
       (receive r (partition (lambda (e) (even? (string->number e)))
                             (sseq 1 2 3 4 5 6 7))
         r))

(test* "partition-to (string)" '("ACE" "bdf")
       (receive r (partition-to <string> char-upper-case? "AbCdEf")
         r))
(test* "partition-to (vector)" '(#(2 4 6) #(1 3 5 7))
       (receive r (partition-to <vector> even? '#(1 2 3 4 5 6 7))
         r))

(test-section "miscellaneous")

(test* "size-of (list)"   5 (size-of '(1 2 3 4 5)))
(test* "size-of (vector)" 5 (size-of '#(1 2 3 4 5)))
(test* "size-of (string)" 5 (size-of "12345"))
(test* "size-of (custom)" 5 (size-of (sseq 1 2 3 4 5)))

(test* "coerce-to (list->list)" '(1 2 3)
       (coerce-to <list> '(1 2 3)))
(test* "coerce-to (list->vector)" '#(1 2 3)
       (coerce-to <vector> '(1 2 3)))
(test* "coerce-to (list->string)" "123"
       (coerce-to <string> '(#\1 #\2 #\3)))
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
(test* "coerce-to (custom->list)" '("1" "2" "3")
       (coerce-to <list> (sseq 1 2 3)))
(test* "coerce-to (custom->vector)" '#("1" "2" "3")
       (coerce-to <vector> (sseq 1 2 3)))
(test* "coerce-to (custom->custom)" '("1" "2" "3")
       (slot-ref (coerce-to <string-seq> (sseq 1 2 3)) 'strings))

(test-section "sequence operations")

(test* "ref (list)" 3     (ref '(1 2 3 4 5) 2))
(test* "ref (vector)" 3   (ref '#(1 2 3 4 5) 2))
(test* "ref (string)" #\3 (ref "12345" 2))
(test* "ref (custom)" "3" (ref (sseq 1 2 3 4 5) 2))

(test* "(setter ref) (list)" '(1 2 a 4 5)
       (let ((x (list 1 2 3 4 5)))
         (set! (ref x 2) 'a) x))
(test* "(setter ref) (vector)" '#(1 2 a 4 5)
       (let ((x (vector 1 2 3 4 5)))
         (set! (ref x 2) 'a) x))
(test* "(setter ref) (string)" "12a45"
       (let ((x (string-copy "12345")))
         (set! (ref x 2) #\a) x))
(test* "(setter ref) (custom)" '("1" "2" "a" "4" "5")
       (let ((x (sseq 1 2 3 4 5)))
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

(test-end)
