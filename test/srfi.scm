;;
;; Test for SRFIs
;;

;; $Id: srfi.scm,v 1.15 2001-05-19 11:04:35 shirok Exp $

(use gauche.test)

(test-start "SRFIs")

;;-----------------------------------------------------------------------
(test-section "srfi-1")
(use srfi-1)

(test "xcons" '(a . b) (lambda () (xcons 'b 'a)))
(test "cons*" '() (lambda () (cons*)))
(test "cons*" 'o  (lambda () (cons* 'o)))
(test "cons*" '(1 2 3 . 4) (lambda () (cons* 1 2 3 4)))
(test "make-list" 5 (lambda () (length (make-list 5))))
(test "make-list" '() (lambda () (make-list 0)))
(test "make-list" '(m m m m m) (lambda () (make-list 5 'm)))
(test "list-tabulate" '(0 1 2 3 4)
      (lambda () (list-tabulate 5 values)))
(test "list-tabulate" '(#\0 #\1 #\2 #\3 #\4)
      (lambda () (list-tabulate 5 (lambda (i) (integer->char (+ i 48))))))
(test "list-copy" '(1 2 3) (lambda () (list-copy '(1 2 3))))
(test "list-copy" '() (lambda () (list-copy '())))
(test "list-copy" '(1 2 3)
      (lambda ()
        (let ((x (list 1 2 3)))
          (set-car! (list-copy x) 0)
          x)))
(test "circular-list" 'a
      (lambda () (list-ref (circular-list 'a 'b) 4)))
(test "iota" '(0 1 2 3 4) (lambda () (iota 5)))
(test "iota" '(5 6 7 8 9) (lambda () (iota 5 5)))
(test "iota" '(10 20 30 40 50) (lambda () (iota 5 10 10)))
(test "proper-list?" #t (lambda () (proper-list? '(1 2 4))))
(test "proper-list?" #t (lambda () (proper-list? '())))
(test "proper-list?" #f (lambda () (proper-list? 2)))
(test "proper-list?" #f (lambda () (proper-list? '(3 . 1))))
(test "proper-list?" #f (lambda () (proper-list? (circular-list 1 2))))
(test "circular-list?" #t (lambda () (circular-list? (circular-list 1 2))))
(test "circular-list?" #f (lambda () (circular-list? '())))
(test "circular-list?" #f (lambda () (circular-list? '(1 . 2))))
(test "dotted-list?" #t (lambda () (dotted-list? '(1 . 2))))
(test "dotted-list?" #t (lambda () (dotted-list? 3)))
(test "dotted-list?" #f (lambda () (dotted-list? '(1 2 3))))
(test "dotted-list?" #f (lambda () (dotted-list? (circular-list 1 2 3))))
(test "dotted-list?" #f (lambda () (dotted-list? '())))
(test "null-list?" #t (lambda () (null-list? '())))
(test "null-list?" #f (lambda () (null-list? '(1))))
(test "not-pair?" #f (lambda () (not-pair? '(1))))
(test "not-pair?" #t (lambda () (not-pair? '())))
(test "list=" #t (lambda () (list= eq?)))
(test "list=" #t (lambda () (list= eq? '(a))))
(test "list=" #t (lambda () (list= eq? '(a) '(a))))
(test "list=" #f (lambda () (list= eq? '(a) '(a b))))
(test "list=" #t (lambda () (list= char-ci=? '(#\a #\b #\z) '(#\A #\B #\Z))))
(test "list=" #f (lambda () (list= char-ci=? '(#\a #\b #\z) '(#\A #\B))))
(test "first"  1 (lambda () (first '(1 2 3 4 5 6 7 8 9 10))))
(test "second" 2 (lambda () (second '(1 2 3 4 5 6 7 8 9 10))))
(test "third"  3 (lambda () (third '(1 2 3 4 5 6 7 8 9 10))))
(test "fourth" 4 (lambda () (fourth '(1 2 3 4 5 6 7 8 9 10))))
(test "fifth"  5 (lambda () (fifth '(1 2 3 4 5 6 7 8 9 10))))
(test "sixth"  6 (lambda () (sixth '(1 2 3 4 5 6 7 8 9 10))))
(test "seventh" 7 (lambda () (seventh '(1 2 3 4 5 6 7 8 9 10))))
(test "eighth" 8 (lambda () (eighth '(1 2 3 4 5 6 7 8 9 10))))
(test "ninth"  9 (lambda () (ninth '(1 2 3 4 5 6 7 8 9 10))))
(test "tenth"  10 (lambda () (tenth '(1 2 3 4 5 6 7 8 9 10))))
(test "car+cdr" '(a (b c))
      (lambda () (call-with-values (lambda () (car+cdr '(a b c))) list)))
(test "take" '(a b)   (lambda () (take '(a b c d e) 2)))
(test "drop" '(c d e) (lambda () (drop '(a b c d e) 2)))
(test "take" '(1 2)   (lambda () (take '(1 2 3 . d) 2)))
(test "drop" '(3 . d) (lambda () (drop '(1 2 3 . d) 2)))
(test "take" '(1 2 3) (lambda () (take '(1 2 3 . d) 3)))
(test "drop" 'd       (lambda () (drop '(1 2 3 . d) 3)))
(test "take-right" '(d e)     (lambda () (take-right '(a b c d e) 2)))
(test "drop-right" '(a b c)   (lambda () (drop-right '(a b c d e) 2)))
(test "take-right" '(2 3 . d) (lambda () (take-right '(1 2 3 . d) 2)))
(test "drop-right" '(1)       (lambda () (drop-right '(1 2 3 . d) 2)))
(test "take-right" 'd         (lambda () (take-right '(1 2 3 . d) 0)))
(test "drop-right" '(1 2 3)   (lambda () (drop-right '(1 2 3 . d) 0)))
(test "take!"      '(1 2)     (lambda () (take! '(1 2 3 . d) 2)))
(test "drop-right!" '(1 2)    (lambda () (drop-right! '(1 2 3 . d) 1)))
(test "split-at" '((a b c) (d e f g h))
      (lambda () (call-with-values
                     (lambda () (split-at '(a b c d e f g h) 3))
                   list)))
(test "split-at!" '((a b c) (d e f g h))
      (lambda () (call-with-values
                     (lambda () (split-at! (list 'a 'b 'c 'd 'e 'f 'g 'h) 3))
                   list)))
(test "last" 'c (lambda () (last '(a b c))))
(test "last-pair" '(c) (lambda () (last-pair '(a b c))))
(test "length+" '(5 #f)
      (lambda () (list (length+ '(1 2 3 4 5))
                       (length+ (circular-list 1 2 3 4 5)))))
(test "append" '(1 2 3 4 5) (lambda () (append '(1 2 3) '() '(4 5))))
(test "append" '(1 2 3 . 5) (lambda () (append '(1 2 3) 5)))
(test "append!" '(1 2 3 4 5) (lambda () (append (list 1 2 3) '() '(4 5))))
(test "append!" '(1 2 3 . 5) (lambda () (append (list 1 2 3) 5)))
(test "concatenate" '(1 2 3 4 5)
      (lambda () (concatenate '((1 2 3) () (4 5)))))
(test "concatenate" '(1 2 3 4 . 5)
      (lambda () (concatenate '((1 2 3) () (4) 5))))
(test "concatenate!" '(1 2 3 4 5)
      (lambda () (concatenate (list (list 1 2 3) '() (list 4 5)))))
(test "concatenate!" '(1 2 3 4 . 5)
      (lambda () (concatenate (list (list 1 2 3) '() (list 4) 5))))
(test "reverse" '(5 4 3 2 1) (lambda () (reverse '(1 2 3 4 5))))
(test "reverse" '() (lambda () (reverse '())))
(test "reverse!" '(5 4 3 2 1) (lambda () (reverse! (list 1 2 3 4 5))))
(test "reverse!" '() (lambda () (reverse! '())))
(test "append-reverse" '(1 2 3 4 5)
      (lambda () (append-reverse '(3 2 1) '(4 5))))
(test "append-reverse" '(1 2 3 4 . 5)
      (lambda () (append-reverse '(4 3 2 1) 5)))
(test "append-reverse!" '(1 2 3 4 5)
      (lambda () (append-reverse! (list 3 2 1) (list 4 5))))
(test "append-reverse!" '(1 2 3 4 . 5)
      (lambda () (append-reverse! (list 4 3 2 1) 5)))
(test "zip" '((one 1 odd) (two 2 even) (three 3 odd))
      (lambda () (zip '(one two three) '(1 2 3)
                      '(odd even odd even odd even))))
(test "zip" '((1) (2) (3)) (lambda () (zip '(1 2 3))))
(test "zip" '((3 #f) (1 #t) (4 #f) (1 #t))
      (lambda () (zip '(3 1 4 1) (circular-list #f #t))))
(test "zip" '() (lambda () (zip '(a b) '() '(c d))))
(define unzip-data '((1 2 3 4 5 6 7 8)
                     (a b c d e)
                     (#\a #\b #\c #\d #\e)
                     ("a" "b" "c" "d" "e")))
(test "unzip1" '(1 a #\a "a") (lambda () (unzip1 unzip-data)))
(test "unzip2" '((1 a #\a "a") (2 b #\b "b"))
      (lambda () (call-with-values (lambda () (unzip2 unzip-data)) list)))
(test "unzip3" '((1 a #\a "a") (2 b #\b "b") (3 c #\c "c"))
      (lambda () (call-with-values (lambda () (unzip3 unzip-data)) list)))
(test "unzip4" '((1 a #\a "a") (2 b #\b "b") (3 c #\c "c") (4 d #\d "d"))
      (lambda () (call-with-values (lambda () (unzip4 unzip-data)) list)))
(test "unzip5"
      '((1 a #\a "a") (2 b #\b "b") (3 c #\c "c") (4 d #\d "d") (5 e #\e "e"))
      (lambda () (call-with-values (lambda () (unzip5 unzip-data)) list)))
(test "count" 3 (lambda () (count even? '(3 1 4 1 5 9 2 6 5))))
(test "count" 3
      (lambda () (count < '(1 2 4 8) '(2 4 6 8 10 12 14 16))))
(test "count" 2
      (lambda () (count < '(3 1 4 1) (circular-list 1 10))))
(test "fold" 55
      (lambda () (fold + 0 '(1 2 3 4 5 6 7 8 9 10))))
(test "fold" '(e d c b a)
      (lambda () (fold cons '() '(a b c d e))))
(test "fold" 3
      (lambda () (fold (lambda (x c) (if (symbol? x) (+ c 1) c))
                       0
                       '(a 3 b 8 c 9))))
(test "fold" '(c 3 b 2 a 1)
      (lambda () (fold cons* '() '(a b c) '(1 2 3 4 5))))
(test "fold-right" '(1 2 3 4 5)
      (lambda () (fold-right cons '() '(1 2 3 4 5))))
(test "fold-right" '(2 4 6)
      (lambda () (fold-right (lambda (x l) (if (even? x) (cons x l) l))
                             '()
                             '(1 2 3 4 5 6 7))))
(test "fold-right" '(a 1 b 2 c 3)
      (lambda () (fold-right cons* '() '(a b c) '(1 2 3 4 5))))
(test "pair-fold" '(5 4 3 2 1)
      (lambda () (pair-fold (lambda (p t) (set-cdr! p t) p) '()
                            (list 1 2 3 4 5))))
(test "pair-fold-right" '((a b c) (b c) (c))
      (lambda () (pair-fold-right cons '() '(a b c))))
(test "reduce" 55 (lambda () (reduce + 0 (iota 10 1))))
(test "reduce-right" '(1 2 3 4 5 6 7 8 9 . 10)
      (lambda () (reduce-right cons 0 (iota 10 1))))
(test "unfold" '(1 4 9 16 25 36 49 64 81 100)
      (lambda () (unfold (lambda (x) (> x 10))
                         (lambda (x) (* x x))
                         (lambda (x) (+ x 1))
                         1)))
(test "unfold-right" '(1 4 9 16 25 36 49 64 81 100)
      (lambda () (unfold-right zero?
                               (lambda (x) (* x x))
                               (lambda (x) (- x 1))
                               10)))
(test "map" '(4 1 5 1)
      (lambda () (map + '(3 1 4 1) (circular-list 1 0))))
(test "for-each" '#(0 2 2 4 4)
      (lambda () (let ((v (make-vector 5)))
                   (for-each (lambda (i n)
                               (vector-set! v i (+ i n)))
                             '(0 1 2 3 4)
                             (circular-list 0 1))
                   v)))
(test "append-map" '(1 -1 3 -3 5 -5)
      (lambda () (append-map (lambda (x) (list x (- x))) '(1 3 5))))
(test "append-map" '(1 -2 3 -4 5 -6)
      (lambda () (append-map (lambda (x y) (list x (- y)))
                             '(1 3 5) '(2 4 6 8))))
(test "append-map!" '(1 -2 3 -4 5 -6)
      (lambda () (append-map! (lambda (x y) (list x (- y)))
                             '(1 3 5) '(2 4 6 8))))
(test "map!" '(4 1 5 1)
      (lambda () (map! + '(3 1 4 1) (circular-list 1 0))))
(test "map-in-order"  '(4 1 5 1)
      (lambda () (map-in-order + '(3 1 4 1) (circular-list 1 0))))
(test "pair-for-each" '((c) (b c) (a b c))
      (lambda () (let ((r '()))
                   (pair-for-each (lambda (l) (set! r (cons l r)))
                                  '(a b c))
                   r)))
(test "filter-map" '(1 9 49)
      (lambda () (filter-map (lambda (x) (and (number? x) (* x x)))
                             '(a 1 b 3 c 7))))
(test "filter" '(0 8 8 -4)
      (lambda () (filter even? '(0 7 8 8 9 -4))))
(test "partition" '((one four five) (2 3 6))
      (lambda () (receive x (partition symbol? '(one 2 3 four five 6)) x)))
(test "remove" '(7 43)
      (lambda () (remove even? '(0 7 8 8 43 -4))))
(test "filter!" '(0 8 8 -4)
      (lambda () (filter even? (list 0 7 8 8 9 -4))))
(test "partition!" '((one four five) (2 3 6))
      (lambda () (receive x (partition! symbol? (list 'one 2 3 'four 'five 6)) x)))
(test "remove!" '(7 43)
      (lambda () (remove! even? (list 0 7 8 8 43 -4))))
(test "find" 4
      (lambda () (find even? '(3 1 4 1 5 9))))
(test "find" #f
      (lambda () (find even? '(3 1 1 5 9))))
(test "fid-tail" '(-8 -5 0 0)
      (lambda () (find-tail even? '(3 1 -8 -5 0 0))))
(test "find-tail" #f
      (lambda () (find-tail even? '(3 1 -9 -5 1 -1))))
(test "take-while" '(2 18)
      (lambda () (take-while even? '(2 18 3 10 22 9))))
(test "take-while!" '(2 18)
      (lambda () (take-while! even? (list 2 18 3 10 22 9))))
(test "drop-while" '(3 10 22 9)
      (lambda () (drop-while even? '(3 10 22 9))))
(test "span" '((2 18) (3 10 22 9))
      (lambda () (receive x (span even? '(2 18 3 10 22 9)) x)))
(test "break" '((2 18) (3 10 22 9))
      (lambda () (receive x (break odd? '(2 18 3 10 22 9)) x)))
(test "span!" '((2 18) (3 10 22 9))
      (lambda () (receive x (span! even? (list 2 18 3 10 22 9)) x)))
(test "break!" '((2 18) (3 10 22 9))
      (lambda () (receive x (break! odd? (list 2 18 3 10 22 9)) x)))
(test "any" #t
      (lambda () (any integer? '(a 3 b 2.8))))
(test "any" #f
      (lambda () (any integer? '(a 3.1 b 2.8))))
(test "any" #t
      (lambda () (any < '(3 1 4 1 5) '(2 7 1 8 2))))
(test "every" #f
      (lambda () (every integer? '(a 3 b 2.8))))
(test "every" #t
      (lambda () (every integer? '(2.0 3.0 8.0 -1.0))))
(test "every" #t
      (lambda () (every <= '(1 2 3 4 5) '(2 3 3 5 5))))
(test "list-index" 2
      (lambda () (list-index even? '(3 1 4 1 5 9))))
(test "list-index" 1
      (lambda () (list-index < '(3 1 4 1 5 9) '(2 7 1))))
(test "list-index" #f
      (lambda () (list-index = '(3 1 4 1 5 9) '(2 7 1))))
(test "member" #f
      (lambda () (member '"b" '("a" "b" "c") eq?)))
(test "member" '(2.0 3.0)
      (lambda () (member '2.0 '(1.0 2.0 3.0) eqv?)))
(test "member" '("b" "c")
      (lambda () (member '"b" '("a" "b" "c") equal?)))
(test "member" '("b" "c")
      (lambda () (member '"b" '("a" "b" "c"))))
(test "member" '("b" "c")
      (lambda () (member '"b" '("a" "b" "c") string=?)))
(test "member" '("b" "c")
      (lambda () (member '"B" '("a" "b" "c") string-ci=?)))
(test "delete" '(b c d e)
      (lambda () (delete 'a '(a b a c a d a e) eq?)))
(test "delete" '(2.0 3.0 4.0 5.0)
      (lambda () (delete '1.0 '(1.0 2.0 1.0 3.0 4.0 1.0 5.0) eqv?)))
(test "delete" '("b" "c" "d" "e")
      (lambda () (delete "a" '("a" "b" "a" "c" "d" "a" "e"))))
(test "delete" '("b" "c" "d" "e")
      (lambda () (delete "a" '("a" "b" "a" "c" "d" "a" "e") string=?)))
(test "delete" '("b" "c" "d" "e")
      (lambda () (delete "A" '("a" "b" "a" "c" "d" "a" "e") string-ci=?)))
(test "delete!" '(b c d e)
      (lambda () (delete! 'a '(a b a c a d a e) eq?)))
(test "delete!" '(2.0 3.0 4.0 5.0)
      (lambda () (delete! '1.0 '(1.0 2.0 1.0 3.0 4.0 1.0 5.0) eqv?)))
(test "delete!" '("b" "c" "d" "e")
      (lambda () (delete! "a" '("a" "b" "a" "c" "d" "a" "e"))))
(test "delete!" '("b" "c" "d" "e")
      (lambda () (delete! "a" '("a" "b" "a" "c" "d" "a" "e") string=?)))
(test "delete!" '("b" "c" "d" "e")
      (lambda () (delete! "A" '("a" "b" "a" "c" "d" "a" "e") string-ci=?)))
(test "delete-duplicates" '(a b c d e)
      (lambda () (delete-duplicates '(a b a c b a d d a e) eq?)))
(test "delete-duplicates" '(1.0 2.0 3.0 4.0 5.0)
      (lambda () (delete-duplicates '(1.0 2.0 1.0 2.0 3.0 3.0 4.0 1.0 5.0) eqv?)))
(test "delete-duplicates" '("a" "b" "c" "d" "e")
      (lambda () (delete-duplicates '("a" "b" "b" "a" "b" "c" "d" "a" "e"))))
(test "delete-duplicates" '("a" "b" "c" "d" "e")
      (lambda () (delete-duplicates '("a" "b" "a" "a" "c" "d" "a" "e") string=?)))
(test "delete-duplicates" '("A" "b" "c" "d" "e")
      (lambda () (delete-duplicates '("A" "b" "a" "B" "c" "d" "a" "e") string-ci=?)))
(test "delete-duplicates!" '(a b c d e)
      (lambda () (delete-duplicates! '(a b a c b a d d a e) eq?)))
(test "delete-duplicates!" '(1.0 2.0 3.0 4.0 5.0)
      (lambda () (delete-duplicates! '(1.0 2.0 1.0 2.0 3.0 3.0 4.0 1.0 5.0) eqv?)))
(test "delete-duplicates!" '("a" "b" "c" "d" "e")
      (lambda () (delete-duplicates! '("a" "b" "b" "a" "b" "c" "d" "a" "e"))))
(test "delete-duplicates!" '("a" "b" "c" "d" "e")
      (lambda () (delete-duplicates! '("a" "b" "a" "a" "c" "d" "a" "e") string=?)))
(test "delete-duplicates!" '("A" "b" "c" "d" "e")
      (lambda () (delete-duplicates! '("A" "b" "a" "B" "c" "d" "a" "e") string-ci=?)))
(test "assq" '(a 1) (lambda () (assq 'a '((a 1) (b 2) (c 3)))))
(test "assq" #f     (lambda () (assq 'd '((a 1) (b 2) (c 3)))))
(test "assq" #f     (lambda () (assq (list 'a) '(((a)) ((b)) ((c))))))
(test "assv" '(b 2) (lambda () (assv 'b '((a 1) (b 2) (c 3)))))
(test "assv" #f     (lambda () (assv 'd '((a 1) (b 2) (c 3)))))
(test "assv" #f     (lambda () (assv (list 'a) '(((a)) ((b)) ((c))))))
(test "assoc" '((a)) (lambda () (assoc (list 'a) '(((a)) ((b)) ((c))))))
(test "assoc" '("a") (lambda () (assoc "a" '(("c") ("b") ("a")))))
(test "assoc" '("a") (lambda () (assoc "A" '(("c") ("b") ("a")) string-ci=?)))
(test "alist-cons" '((1 . 2) . 3) (lambda () (alist-cons 1 2 3)))
(test "alist-copy" '((a 1) (a 2))
      (lambda ()
        (let* ((x '((b 2) (a 1)))
               (y (alist-copy x)))
          (set-cdr! (assq 'a y) (list 2))
          (list (assq 'a x) (assq 'a y)))))

;; TODO: lset stuff

;;-----------------------------------------------------------------------
(test-section "srfi-2")
(use srfi-2)

(define (srfi-2-look-up key alist)
  (and-let* ((x (assq key alist))) (cdr x)))
(test "and-let*" 3
      (lambda () (srfi-2-look-up 'c '((a . 1) (b . 2) (c . 3)))))
(test "and-let*" #f
      (lambda () (srfi-2-look-up 'd '((a . 1) (b . 2) (c . 3)))))
(test "and-let*" 3
      (lambda ()
        (let ((x 3))
          (and-let* (((positive? x))
                     (y x))
                    y))))
(test "and-let*" #f
      (lambda ()
        (let ((x -3))
          (and-let* (((positive? x))
                     (y x))
                    y))))

;;-----------------------------------------------------------------------
(test-section "srfi-13")
(use srfi-13)

(test "string-null?" #f (lambda () (string-null? "abc")))
(test "string-null?" #t (lambda () (string-null? "")))
(test "string-every" #t (lambda () (string-every #\a "")))
(test "string-every" #t (lambda () (string-every #\a "aaaa")))
(test "string-every" #f (lambda () (string-every #\a "aaba")))
(test "string-every" #t (lambda () (string-every #[a-z] "aaba")))
(test "string-every" #f (lambda () (string-every #[a-z] "aAba")))
(test "string-every" #t (lambda () (string-every #[a-z] "")))
(test "string-every" #t (lambda () (string-every (lambda (x) (char-ci=? x #\a)) "aAaA")))
(test "string-every" #f (lambda () (string-every (lambda (x) (char-ci=? x #\a)) "aAbA")))
(test "string-every" (char->integer #\A)
      (lambda () (string-every (lambda (x) (char->integer x)) "aAbA")))
(test "string-every" #t
      (lambda () (string-every (lambda (x) (error "hoge")) "")))
(test "string-any" #t (lambda () (string-any #\a "aaaa")))
(test "string-any" #f (lambda () (string-any #\a "Abcd")))
(test "string-any" #f (lambda () (string-any #\a "")))
(test "string-any" #t (lambda () (string-any #[a-z] "ABcD")))
(test "string-any" #f (lambda () (string-any #[a-z] "ABCD")))
(test "string-any" #f (lambda () (string-any #[a-z] "")))
(test "string-any" #t (lambda () (string-any (lambda (x) (char-ci=? x #\a)) "CAaA")))
(test "string-any" #f (lambda () (string-any (lambda (x) (char-ci=? x #\a)) "ZBRC")))
(test "string-any" #f (lambda () (string-any (lambda (x) (char-ci=? x #\a)) "")))
(test "string-any" (char->integer #\a)
      (lambda () (string-any (lambda (x) (char->integer x)) "aAbA")))
(test "string-tabulate" "0123456789"
      (lambda () (string-tabulate (lambda (code)
                                    (integer->char (+ code (char->integer #\0))))
                                  10)))
(test "string-tabulate" ""
      (lambda () (string-tabulate (lambda (code)
                                    (integer->char (+ code (char->integer #\0))))
                                  0)))
(test "reverse-list->string" "cBa"
      (lambda () (reverse-list->string '(#\a #\B #\c))))
(test "reverse-list->string" ""
      (lambda () (reverse-list->string '())))
; string-join : Gauche builtin.
(test "substring/shared" "cde" (lambda () (substring/shared "abcde" 2)))
(test "substring/shared" "cd"  (lambda () (substring/shared "abcde" 2 4)))
(test "string-copy!" "abCDEfg"
      (lambda () (let ((x (string-copy "abcdefg")))
                   (string-copy! x 2 "CDE")
                   x)))
(test "string-copy!" "abCDEfg"
      (lambda () (let ((x (string-copy "abcdefg")))
                   (string-copy! x 2 "ZABCDE" 3)
                   x)))
(test "string-copy!" "abCDEfg"
      (lambda () (let ((x (string-copy "abcdefg")))
                   (string-copy! x 2 "ZABCDEFG" 3 6)
                   x)))
(test "string-take" "Pete S"  (lambda () (string-take "Pete Szilagyi" 6)))
(test "string-take" ""        (lambda () (string-take "Pete Szilagyi" 0)))
(test "string-take" "Pete Szilagyi" (lambda () (string-take "Pete Szilagyi" 13)))
(test "string-drop" "zilagyi" (lambda () (string-drop "Pete Szilagyi" 6)))
(test "string-drop" "Pete Szilagyi" (lambda () (string-drop "Pete Szilagyi" 0)))
(test "string-drop" ""        (lambda () (string-drop "Pete Szilagyi" 13)))

(test "string-take-right" "rules" (lambda () (string-take-right "Beta rules" 5)))
(test "string-take-right" ""      (lambda () (string-take-right "Beta rules" 0)))
(test "string-take-right" "Beta rules" (lambda () (string-take-right "Beta rules" 10)))
(test "string-drop-right" "Beta " (lambda () (string-drop-right "Beta rules" 5)))
(test "string-drop-right" "Beta rules" (lambda () (string-drop-right "Beta rules" 0)))
(test "string-drop-right" ""      (lambda () (string-drop-right "Beta rules" 10)))

(test "string-pad" "  325" (lambda () (string-pad "325" 5)))
(test "string-pad" "71325" (lambda () (string-pad "71325" 5)))
(test "string-pad" "71325" (lambda () (string-pad "8871325" 5)))
(test "string-pad" "~~325" (lambda () (string-pad "325" 5 #\~)))
(test "string-pad" "~~~25" (lambda () (string-pad "325" 5 #\~ 1)))
(test "string-pad" "~~~~2" (lambda () (string-pad "325" 5 #\~ 1 2)))
(test "string-pad-right" "325  " (lambda () (string-pad-right "325" 5)))
(test "string-pad-right" "71325" (lambda () (string-pad-right "71325" 5)))
(test "string-pad-right" "88713" (lambda () (string-pad-right "8871325" 5)))
(test "string-pad-right" "325~~" (lambda () (string-pad-right "325" 5 #\~)))
(test "string-pad-right" "25~~~" (lambda () (string-pad-right "325" 5 #\~ 1)))
(test "string-pad-right" "2~~~~" (lambda () (string-pad-right "325" 5 #\~ 1 2)))

(test "string-trim"  "a b c d  \r\n"
      (lambda () (string-trim "  \t  a b c d  \r\n")))
(test "string-trim"  "\t  a b c d  \r\n"
      (lambda () (string-trim "  \t  a b c d  \r\n" #\space)))
(test "string-trim"  "a b c d  \r\n"
      (lambda () (string-trim "4358948a b c d  \r\n" #[\d])))

(test "string-trim-right"  "  \t  a b c d"
      (lambda () (string-trim-right "  \t  a b c d  \r\n")))
(test "string-trim-right"  "  \t  a b c d  "
      (lambda () (string-trim-right "  \t  a b c d  \r\n" #[\r\n])))
(test "string-trim-right"  "349853a b c d"
      (lambda () (string-trim-right "349853a b c d03490" #[\d])))

(test "string-trim-both"  "a b c d"
      (lambda () (string-trim-both "  \t  a b c d  \r\n")))
(test "string-trim-both"  "  \t  a b c d  "
      (lambda () (string-trim-both "  \t  a b c d  \r\n" #[\r\n])))
(test "string-trim-both"  "a b c d"
      (lambda () (string-trim-both "349853a b c d03490" #[\d])))

; string-fill - in string.scm

(test "string-compare" 5
      (lambda () (string-compare "The cat in the hat" "abcdefgh"
                                 values values values
                                 4 6 2 4)))
(test "string-compare-ci" 5
      (lambda () (string-compare-ci "The cat in the hat" "ABCDEFGH"
                                 values values values
                                 4 6 2 4)))

; TODO: bunch of string= families

(test "string-prefix-length" 5
      (lambda () (string-prefix-length "cancaNCAM" "cancancan")))
(test "string-prefix-length-ci" 8
      (lambda () (string-prefix-length-ci "cancaNCAM" "cancancan")))
(test "string-suffix-length" 4
      (lambda () (string-suffix-length "CanCan" "cankancan")))
(test "string-suffix-length-ci" 1
      (lambda () (string-suffix-length-ci "CanCan" "cankancan")))

(test "string-prefix?" #t    (lambda () (string-prefix? "abcd" "abcdefg")))
(test "string-prefix?" #f    (lambda () (string-prefix? "abcf" "abcdefg")))
(test "string-prefix-ci?" #t (lambda () (string-prefix-ci? "abcd" "aBCDEfg")))
(test "string-prefix-ci?" #f (lambda () (string-prefix-ci? "abcf" "aBCDEfg")))
(test "string-suffix?" #t    (lambda () (string-suffix? "defg" "abcdefg")))
(test "string-suffix?" #f    (lambda () (string-suffix? "aefg" "abcdefg")))
(test "string-suffix-ci?" #t (lambda () (string-suffix-ci? "defg" "aBCDEfg")))
(test "string-suffix-ci?" #f (lambda () (string-suffix-ci? "aefg" "aBCDEfg")))

(test "string-index" 4
      (lambda () (string-index "abcd:efgh;ijkl" #\:)))
(test "string-index" 4
      (lambda () (string-index "abcd:efgh;ijkl" #[\W])))
(test "string-index" #f
      (lambda () (string-index "abcd:efgh;ijkl" #[\d])))
(test "string-index-right" 4
      (lambda () (string-index-right "abcd:efgh;ijkl" #\:)))
(test "string-index-right" 9
      (lambda () (string-index-right "abcd:efgh;ijkl" #[\W])))
(test "string-index-right" #f
      (lambda () (string-index-right "abcd:efgh;ijkl" #[\d])))

(test "string-count" 2
      (lambda () (string-count "abc def\tghi jkl" #\space)))
(test "string-count" 3
      (lambda () (string-count "abc def\tghi jkl" #[\s])))
(test "string-count" 2
      (lambda () (string-count "abc def\tghi jkl" #[\s] 4)))
(test "string-count" 1
      (lambda () (string-count "abc def\tghi jkl" #[\s] 4 9)))
(test "string-contains" 3
      (lambda () (string-contains "Ma mere l'oye" "mer")))
(test "string-contains" #f
      (lambda () (string-contains "Ma mere l'oye" "Mer")))
(test "string-contains-ci" 3
      (lambda () (string-contains-ci "Ma mere l'oye" "Mer")))
(test "string-contains-ci" #f
      (lambda () (string-contains-ci "Ma mere l'oye" "Meer")))

(test "string-titlecase" "--Capitalize This Sentence."
      (lambda () (string-titlecase "--capitalize tHIS sentence.")))
(test "string-titlecase" "3Com Makes Routers."
      (lambda () (string-titlecase "3com makes routers.")))
(test "string-titlecase!" "alSo Whatever"
      (lambda ()
        (let ((s (string-copy "also whatever")))
          (string-titlecase! s 2 9)
          s)))

(test "string-upcase" "SPEAK LOUDLY"
      (lambda () (string-upcase "speak loudly")))
(test "string-upcase" "PEAK"
      (lambda () (string-upcase "speak loudly" 1 5)))
(test "string-upcase!" "sPEAK loudly"
      (lambda ()
        (let ((s (string-copy "speak loudly")))
          (string-upcase! s 1 5)
          s)))

(test "string-downcase" "speak softly"
      (lambda () (string-downcase "SPEAK SOFTLY")))
(test "string-downcase" "peak"
      (lambda () (string-downcase "SPEAK SOFTLY" 1 5)))
(test "string-downcase!" "Speak SOFTLY"
      (lambda ()
        (let ((s (string-copy "SPEAK SOFTLY")))
          (string-downcase! s 1 5)
          s)))

(test "string-reverse" "nomel on nolem on"
      (lambda () (string-reverse "no melon no lemon")))
(test "string-reverse" "nomel on"
      (lambda () (string-reverse "no melon no lemon" 9)))
(test "string-reverse" "on"
      (lambda () (string-reverse "no melon no lemon" 9 11)))
(test "string-reverse!" "nomel on nolem on"
      (lambda ()
        (let ((s (string-copy "no melon no lemon")))
          (string-reverse! s) s)))
(test "string-reverse!" "no melon nomel on"
      (lambda ()
        (let ((s (string-copy "no melon no lemon")))
          (string-reverse! s 9) s)))
(test "string-reverse!" "no melon on lemon"
      (lambda ()
        (let ((s (string-copy "no melon no lemon")))
          (string-reverse! s 9 11) s)))

(test "string-append" #f
      (lambda () (let ((s "test")) (eq? s (string-append s)))))
(test "string-concatenate" #f
      (lambda () (let ((s "test")) (eq? s (string-concatenate (list s))))))
(test "string-concatenate" "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
      (lambda () (string-concatenate
                  '("A" "B" "C" "D" "E" "F" "G" "H"
                    "I" "J" "K" "L" "M" "N" "O" "P"
                    "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
                    "a" "b" "c" "d" "e" "f" "g" "h"
                    "i" "j" "k" "l" "m" "n" "o" "p"
                    "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))))
(test "string-concatenate/shared" "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
      (lambda () (string-concatenate/shared
                  '("A" "B" "C" "D" "E" "F" "G" "H"
                    "I" "J" "K" "L" "M" "N" "O" "P"
                    "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
                    "a" "b" "c" "d" "e" "f" "g" "h"
                    "i" "j" "k" "l" "m" "n" "o" "p"
                    "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))))
(test "string-concatenate-reverse" "zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLKJIHGFEDCBA"
      (lambda () (string-concatenate-reverse
                  '("A" "B" "C" "D" "E" "F" "G" "H"
                    "I" "J" "K" "L" "M" "N" "O" "P"
                    "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
                    "a" "b" "c" "d" "e" "f" "g" "h"
                    "i" "j" "k" "l" "m" "n" "o" "p"
                    "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))))
(test "string-concatenate-reverse" #f
      (lambda () (let ((s "test"))
                   (eq? s (string-concatenate-reverse (list s))))))
(test "string-concatenate-reverse/shared" "zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLKJIHGFEDCBA"
      (lambda () (string-concatenate-reverse/shared
                  '("A" "B" "C" "D" "E" "F" "G" "H"
                    "I" "J" "K" "L" "M" "N" "O" "P"
                    "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
                    "a" "b" "c" "d" "e" "f" "g" "h"
                    "i" "j" "k" "l" "m" "n" "o" "p"
                    "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"))))

(test "string-map" "svool"
      (lambda () (string-map (lambda (c)
                               (integer->char (- 219 (char->integer c))))
                             "hello")))
(test "string-map" "vool"
      (lambda () (string-map (lambda (c)
                               (integer->char (- 219 (char->integer c))))
                             "hello" 1)))
(test "string-map" "vo"
      (lambda () (string-map (lambda (c)
                               (integer->char (- 219 (char->integer c))))
                             "hello" 1 3)))
(test "string-map!" "svool"
      (lambda ()
        (let ((s (string-copy "hello")))
          (string-map! (lambda (c)
                         (integer->char (- 219 (char->integer c))))
                       s)
          s)))
(test "string-map!" "hvool"
      (lambda ()
        (let ((s (string-copy "hello")))
          (string-map! (lambda (c)
                         (integer->char (- 219 (char->integer c))))
                       s 1)
          s)))
(test "string-map!" "hvolo"
      (lambda ()
        (let ((s (string-copy "hello")))
          (string-map! (lambda (c)
                         (integer->char (- 219 (char->integer c))))
                       s 1 3)
          s)))

(test "string-fold" '(#\o #\l #\l #\e #\h . #t)
      (lambda () (string-fold cons #t "hello")))
(test "string-fold" '(#\l #\e . #t)
      (lambda () (string-fold cons #t "hello" 1 3)))
(test "string-fold-right" '(#\h #\e #\l #\l #\o . #t)
      (lambda () (string-fold-right cons #t "hello")))
(test "string-fold-right" '(#\e #\l . #t)
      (lambda () (string-fold-right cons #t "hello" 1 3)))

(test "string-unfold" "hello"
      (lambda () (string-unfold null? car cdr '(#\h #\e #\l #\l #\o))))
(test "string-unfold" "hi hello"
      (lambda () (string-unfold null? car cdr '(#\h #\e #\l #\l #\o) "hi ")))
(test "string-unfold" "hi hello ho"
      (lambda () (string-unfold null? car cdr
                                '(#\h #\e #\l #\l #\o) "hi "
                                (lambda (x) " ho"))))

(test "string-unfold-right" "olleh"
      (lambda () (string-unfold-right null? car cdr '(#\h #\e #\l #\l #\o))))
(test "string-unfold-right" "olleh hi"
      (lambda () (string-unfold-right null? car cdr '(#\h #\e #\l #\l #\o) " hi")))
(test "string-unfold-right" "ho olleh hi"
      (lambda () (string-unfold-right null? car cdr
                                '(#\h #\e #\l #\l #\o) " hi"
                                (lambda (x) "ho "))))

(test "string-for-each" "CLtL"
      (lambda ()
        (let ((out (open-output-string))
              (prev #f))
          (string-for-each (lambda (c)
                             (if (or (not prev)
                                     (char-whitespace? prev))
                                 (write-char c out))
                             (set! prev c))
                           "Common Lisp, the Language")

          (get-output-string out))))
(test "string-for-each" "oLtL"
      (lambda ()
        (let ((out (open-output-string))
              (prev #f))
          (string-for-each (lambda (c)
                             (if (or (not prev)
                                     (char-whitespace? prev))
                                 (write-char c out))
                             (set! prev c))
                           "Common Lisp, the Language" 1)
          (get-output-string out))))
(test "string-for-each" "oL"
      (lambda ()
        (let ((out (open-output-string))
              (prev #f))
          (string-for-each (lambda (c)
                             (if (or (not prev)
                                     (char-whitespace? prev))
                                 (write-char c out))
                             (set! prev c))
                           "Common Lisp, the Language" 1 10)
          (get-output-string out))))
(test "string-for-each-index" '(4 3 2 1 0)
      (lambda ()
        (let ((r '()))
          (string-for-each-index (lambda (i) (set! r (cons i r))) "hello")
          r)))
(test "string-for-each-index" '(4 3 2 1)
      (lambda ()
        (let ((r '()))
          (string-for-each-index (lambda (i) (set! r (cons i r))) "hello" 1)
          r)))
(test "string-for-each-index" '(2 1)
      (lambda ()
        (let ((r '()))
          (string-for-each-index (lambda (i) (set! r (cons i r))) "hello" 1 3)
          r)))

(test "xsubstring" "cdefab"
      (lambda () (xsubstring "abcdef" 2)))
(test "xsubstring" "efabcd"
      (lambda () (xsubstring "abcdef" -2)))
(test "xsubstring" "abcabca"
      (lambda () (xsubstring "abc" 0 7)))
(test "xsubstring" "abcabca"
      (lambda () (xsubstring "abc"
                             30000000000000000000000000000000
                             30000000000000000000000000000007)))
(test "xsubstring" "defdefd"
      (lambda () (xsubstring "abcdefg" 0 7 3 6)))
(test "xsubstring" ""
      (lambda () (xsubstring "abcdefg" 9 9 3 6)))

(test "string-xcopy!" "ZZcdefabZZ"
      (lambda ()
        (let ((s (make-string 10 #\Z)))
          (string-xcopy! s 2 "abcdef" 2)
          s)))
(test "string-xcopy!" "ZZdefdefZZ"
      (lambda ()
        (let ((s (make-string 10 #\Z)))
          (string-xcopy! s 2 "abcdef" 0 6 3)
          s)))

(test "string-replace" "abcdXYZghi"
      (lambda () (string-replace "abcdefghi" "XYZ" 4 6)))
(test "string-replace" "abcdZghi"
      (lambda () (string-replace "abcdefghi" "XYZ" 4 6 2)))
(test "string-replace" "abcdZefghi"
      (lambda () (string-replace "abcdefghi" "XYZ" 4 4 2)))
(test "string-replace" "abcdefghi"
      (lambda () (string-replace "abcdefghi" "XYZ" 4 4 1 1)))
(test "string-replace" "abcdhi"
      (lambda () (string-replace "abcdefghi" "" 4 7)))

(test "string-tokenize" '("Help" "make" "programs" "run," "run," "RUN!")
      (lambda () (string-tokenize "Help make programs run, run, RUN!")))
(test "string-tokenize" '("Help" "make" "programs" "run" "run" "RUN")
      (lambda ()
        (string-tokenize "Help make programs run, run, RUN!"
                         #[a-zA-Z])))
(test "string-tokenize" '("programs" "run" "run" "RUN")
      (lambda ()
        (string-tokenize "Help make programs run, run, RUN!"
                         #[a-zA-Z] 10)))
(test "string-tokenize" '("elp" "make" "programs" "run" "run")
      (lambda ()
        (string-tokenize "Help make programs run, run, RUN!"
                         #[a-z])))

(test "string-filter" "rrrr"
      (lambda () (string-filter "Help make programs run, run, RUN!" #\r )))
(test "string-filter" "HelpmakeprogramsrunrunRUN"
      (lambda () (string-filter "Help make programs run, run, RUN!"
                                #[a-zA-Z])))
(test "string-filter" "programsrunrun"
      (lambda () (string-filter "Help make programs run, run, RUN!"
                                (lambda (c) (char-lower-case? c)) 10)))
(test "string-filter" ""
      (lambda () (string-filter "" (lambda (c) (char-lower-case? c)))))
(test "string-delete" "Help make pogams un, un, RUN!"
      (lambda () (string-delete "Help make programs run, run, RUN!" #\r)))
(test "string-delete" "   , , !"
      (lambda () (string-delete "Help make programs run, run, RUN!"
                                #[a-zA-Z])))
(test "string-delete" " , , RUN!"
      (lambda () (string-delete "Help make programs run, run, RUN!"
                                (lambda (c) (char-lower-case? c)) 10)))
(test "string-delete" ""
      (lambda () (string-delete "" (lambda (c) (char-lower-case? c)))))

;;-----------------------------------------------------------------------
(test-section "srfi-14")
(use srfi-14)

;; Test samples taken from Olin Shivers' test suite,
;; http://srfi.schemers.org/srfi-14/srfi-14-tests.scm
;; TODO: This doesn't test characters beyond ASCII.  See char-set.euc.scm.
(define (vowel? c) (member c '(#\a #\e #\i #\o #\u)))

(test "char-set?" #f (lambda () (char-set? 5)))
(test "char-set?" #t (lambda () (char-set? (char-set #\a #\e #\i #\o #\u))))
(test "char-set=" #t (lambda () (char-set=)))
(test "char-set=" #t (lambda () (char-set= (char-set))))
(test "char-set=" #t (lambda () (char-set= (char-set #\a #\e #\i #\o #\u)
                                           (string->char-set "ioeauaiii"))))
(test "char-set=" #f (lambda () (char-set= (char-set #\e #\i #\o #\u)
                                           (string->char-set "ioeauaiii"))))
(test "char-set<=" #t (lambda () (char-set<=)))
(test "char-set<=" #t (lambda () (char-set<= (char-set))))
(test "char-set<=" #t (lambda () (char-set<= (char-set #\a #\e #\i #\o #\u)
                                             (string->char-set "ioeauaiii"))))
(test "char-set<=" #t (lambda () (char-set<= (char-set #\e #\i #\o #\u)
                                             (string->char-set "ioeauaiii"))))
(test "char-set-hash" #t
      (lambda () (<= 0 (char-set-hash char-set:graphic 100) 99)))
(test "char-set-fold" #t
      (lambda ()
        (= 4 (char-set-fold (lambda (c i) (+ i 1)) 0
                            (char-set #\e #\i #\o #\u #\e #\e)))))
(test "char-set-unfold" #t
      (lambda ()
        (char-set= (string->char-set "eiaou2468013579999")
                   (char-set-unfold null? car cdr
                                    '(#\a #\e #\i #\o #\u #\u #\u)
                                    char-set:digit))))
(test "char-set-unfold!" #t
      (lambda () 
        (char-set= (string->char-set "eiaou246801357999")
                   (char-set-unfold! null? car cdr '(#\a #\e #\i #\o #\u)
                                     (string->char-set "0123456789")))))
(test "char-set-unfold!" #f
      (lambda ()
        (char-set= (string->char-set "eiaou246801357")
                   (char-set-unfold! null? car cdr '(#\a #\e #\i #\o #\u)
                                     (string->char-set "0123456789")))))
(test "char-set-for-each" #t
      (lambda ()
        (let ((cs (string->char-set "0123456789")))
          (char-set-for-each (lambda (c) (set! cs (char-set-delete cs c)))
                             (string->char-set "02468000"))
          (char-set= cs (string->char-set "97531")))))
(test "char-set-for-each" #t
      (lambda ()
        (not (let ((cs (string->char-set "0123456789")))
               (char-set-for-each (lambda (c) (set! cs (char-set-delete cs c)))
                                  (string->char-set "02468"))
               (char-set= cs (string->char-set "7531"))))))
(test "char-set-map" #t
      (lambda ()
        (char-set= (char-set-map char-upcase (string->char-set "aeiou"))
                   (string->char-set "IOUAEEEE"))))
(test "char-set-map" #f
      (lambda ()
        (char-set= (char-set-map char-upcase (string->char-set "aeiou"))
                   (string->char-set "OUAEEEE"))))
(test "char-set-copy" #t
      (lambda ()
        (char-set= (char-set-copy (string->char-set "aeiou"))
                   (string->char-set "aeiou"))))
(test "string->char-set" #t
      (lambda ()
        (char-set= (char-set #\x #\y) (string->char-set "xy"))))
(test "string->char-set" #t
      (lambda ()
        (not (char-set= (char-set #\x #\y #\z) (string->char-set "xy")))))
(test "list->char-set" #t
      (lambda ()
        (char-set= (string->char-set "xy") (list->char-set '(#\x #\y)))))
(test "list->char-set" #f
      (lambda () 
        (char-set= (string->char-set "axy") (list->char-set '(#\x #\y)))))
(test "list->char-set" #t
      (lambda () 
        (char-set= (string->char-set "xy12345")
                   (list->char-set '(#\x #\y) (string->char-set "12345")))))
(test "list->char-set" #f
      (lambda ()
        (char-set= (string->char-set "y12345")
                   (list->char-set '(#\x #\y) (string->char-set "12345")))))
(test "list->char-set!" #t
      (lambda ()
        (char-set= (string->char-set "xy12345")
                   (list->char-set! '(#\x #\y) (string->char-set "12345")))))
(test "list->char-set!" #f
      (lambda ()
        (char-set= (string->char-set "y12345")
                   (list->char-set! '(#\x #\y) (string->char-set "12345")))))
(test "char-set-filter" #t
      (lambda ()
        (char-set= (string->char-set "aeiou12345")
                   (char-set-filter vowel? char-set:ascii
                                    (string->char-set "12345")))))
(test "char-set-filter" #f
      (lambda ()
        (char-set= (string->char-set "aeou12345")
                   (char-set-filter vowel? char-set:ascii
                                    (string->char-set "12345")))))
(test "char-set-filter!" #t
      (lambda ()
        (char-set= (string->char-set "aeiou12345")
                   (char-set-filter! vowel? char-set:ascii
                                     (string->char-set "12345")))))
(test "char-set-filter!" #f
      (lambda ()
        (char-set= (string->char-set "aeou12345")
                   (char-set-filter! vowel? char-set:ascii
                                     (string->char-set "12345")))))
(test "ucs-range->char-set" #t
      (lambda ()
        (char-set= (string->char-set "abcdef12345")
                   (ucs-range->char-set 97 103 #t
                                        (string->char-set "12345")))))
(test "ucs-range->char-set" #f
      (lambda ()
        (char-set= (string->char-set "abcef12345")
                   (ucs-range->char-set 97 103 #t
                                        (string->char-set "12345")))))
(test "ucs-range->char-set!" #t
      (lambda ()
        (char-set= (string->char-set "abcdef12345")
                   (ucs-range->char-set! 97 103 #t
                                         (string->char-set "12345")))))
(test "ucs-range->char-set!" #f
      (lambda ()
        (char-set= (string->char-set "abcef12345")
                   (ucs-range->char-set! 97 103 #t
                                         (string->char-set "12345")))))
(test "->char-set" #t
      (lambda ()
        (char-set= (->char-set #\x)
                   (->char-set "x")
                   (->char-set (char-set #\x)))))
(test "->char-set" #f
      (lambda ()
        (char-set= (->char-set #\x)
                   (->char-set "y")
                   (->char-set (char-set #\x)))))
(test "char-set-size" 10
      (lambda ()
        (char-set-size (char-set-intersection char-set:ascii char-set:digit))))
(test "char-set-count" 5
      (lambda ()
        (char-set-count vowel? char-set:ascii)))
(test "char-set->list" #t
      (lambda ()
        (equal? '(#\x) (char-set->list (char-set #\x)))))
(test "char-set->list" #f
      (lambda ()
        (equal? '(#\X) (char-set->list (char-set #\x)))))
(test "char-set->string" #t
      (lambda ()
        (equal? "x" (char-set->string (char-set #\x)))))
(test "char-set->string" #f
      (lambda ()
        (equal? "X" (char-set->string (char-set #\x)))))
(test "char-set-contains?" #t
      (lambda ()
        (char-set-contains? (->char-set "xyz") #\x)))
(test "char-set-contains?" #f
      (lambda ()
        (char-set-contains? (->char-set "xyz") #\a)))
(test "char-set-every" #t
      (lambda ()
        (char-set-every char-lower-case? (->char-set "abcd"))))
(test "char-set-every" #f
      (lambda ()
        (char-set-every char-lower-case? (->char-set "abcD"))))
(test "char-set-any" #t
      (lambda ()
        (char-set-any char-lower-case? (->char-set "abcd"))))
(test "char-set-any" #f
      (lambda ()
        (char-set-any char-lower-case? (->char-set "ABCD"))))
(test "char-set iterators" #t
      (lambda ()
        (char-set= (->char-set "ABCD")
                   (let ((cs (->char-set "abcd")))
                     (let lp ((cur (char-set-cursor cs)) (ans '()))
                       (if (end-of-char-set? cur) (list->char-set ans)
                           (lp (char-set-cursor-next cs cur)
                               (cons (char-upcase (char-set-ref cs cur)) ans))))))))
(test "char-set-adjoin" #t
      (lambda ()
        (char-set= (char-set-adjoin (->char-set "123") #\x #\a)
                   (->char-set "123xa"))))
(test "char-set-adjoin" #f
      (lambda ()
        (char-set= (char-set-adjoin (->char-set "123") #\x #\a)
                   (->char-set "123x"))))
(test "char-set-adjoin!" #t
      (lambda ()
        (char-set= (char-set-adjoin! (->char-set "123") #\x #\a)
                   (->char-set "123xa"))))
(test "char-set-adjoin!" #f
      (lambda ()
        (char-set= (char-set-adjoin! (->char-set "123") #\x #\a)
                   (->char-set "123x"))))
(test "char-set-delete" #t
      (lambda ()
        (char-set= (char-set-delete (->char-set "123") #\2 #\a #\2)
                   (->char-set "13"))))
(test "char-set-delete" #f
      (lambda ()
        (char-set= (char-set-delete (->char-set "123") #\2 #\a #\2)
                   (->char-set "13a"))))
(test "char-set-delete!" #t
      (lambda ()
        (char-set= (char-set-delete! (->char-set "123") #\2 #\a #\2)
                   (->char-set "13"))))
(test "char-set-delete!" #f
      (lambda ()
        (char-set= (char-set-delete! (->char-set "123") #\2 #\a #\2)
                   (->char-set "13a"))))
(test "char-set-intersection" #t
      (lambda ()
        (char-set= (char-set-intersection char-set:hex-digit (char-set-complement char-set:digit))
                   (->char-set "abcdefABCDEF"))))
(test "char-set-intersection!" #t
      (lambda ()
        (char-set= (char-set-intersection! (char-set-complement! (->char-set "0123456789"))
                                           char-set:hex-digit)
                   (->char-set "abcdefABCDEF"))))
(test "char-set-union" #t
      (lambda ()
        (char-set= (char-set-union char-set:hex-digit
                                   (->char-set "abcdefghijkl"))
                   (->char-set "abcdefABCDEFghijkl0123456789"))))
(test "char-set-union!" #t
      (lambda ()
        (char-set= (char-set-union! (->char-set "abcdefghijkl")
                                    char-set:hex-digit)
                   (->char-set "abcdefABCDEFghijkl0123456789"))))
(test "char-set-difference" #t
      (lambda ()
        (char-set= (char-set-difference (->char-set "abcdefghijklmn")
                                        char-set:hex-digit)
                   (->char-set "ghijklmn"))))
(test "char-set-difference!" #t
      (lambda ()
        (char-set= (char-set-difference! (->char-set "abcdefghijklmn")
                                         char-set:hex-digit)
                   (->char-set "ghijklmn"))))
(test "char-set-xor" #t
      (lambda ()
        (char-set= (char-set-xor (->char-set "0123456789")
                                 char-set:hex-digit)
                   (->char-set "abcdefABCDEF"))))
(test "char-set-xor!" #t
      (lambda ()
        (char-set= (char-set-xor! (->char-set "0123456789")
                                  char-set:hex-digit)
                   (->char-set "abcdefABCDEF"))))
(test "char-set-diff+intersection" #t
      (lambda ()
        (call-with-values (lambda ()
                            (char-set-diff+intersection char-set:hex-digit
                                                        char-set:letter))
          (lambda (d i)
            (and (char-set= d (->char-set "0123456789"))
                 (char-set= i (->char-set "abcdefABCDEF")))))))
(test "char-set-diff+intersection!" #t
      (lambda ()
        (call-with-values (lambda ()
                            (char-set-diff+intersection! (char-set-copy char-set:hex-digit)
                                                         (char-set-copy char-set:letter)))
          (lambda (d i)
            (and (char-set= d (->char-set "0123456789"))
                 (char-set= i (->char-set "abcdefABCDEF")))))))

;;-----------------------------------------------------------------------
(test-section "srfi-17")

(define x (cons 1 2))
(test "(setter car)" '((3 3) . 2)
      (lambda () (set! (car x) (list 3 3)) x))
(test "(setter cdr)" '((3 3) 4 5)
      (lambda () (set! (cdr x) (list 4 5)) x))
(test "(setter caar)" '(((8 9) 3) 4 5)
      (lambda () (set! (caar x) (list 8 9)) x))
(test "(setter cadr)" '(((8 9) 3) (7 6) 5)
      (lambda () (set! (cadr x) (list 7 6)) x))
(test "(setter cdar)" '(((8 9) 4 5) (7 6) 5)
      (lambda () (set! (cdar x) (list 4 5)) x))
(test "(setter cddr)" '(((8 9) 4 5) (7 6) 11 12)
      (lambda () (set! (cddr x) (list 11 12)) x))
(test "(setter caaar)" '((((13 14) 9) 4 5) (7 6) 11 12)
      (lambda () (set! (caaar x) (list 13 14)) x))
(test "(setter caadr)" '((((13 14) 9) 4 5) ((0 1) 6) 11 12)
      (lambda () (set! (caadr x) (list 0 1)) x))
(test "(setter cadar)" '((((13 14) 9) (2 3) 5) ((0 1) 6) 11 12)
      (lambda () (set! (cadar x) (list 2 3)) x))
(test "(setter caddr)" '((((13 14) 9) (2 3) 5) ((0 1) 6) (4 5) 12)
      (lambda () (set! (caddr x) (list 4 5)) x))
(test "(setter cdaar)" '((((13 14) 5 6) (2 3) 5) ((0 1) 6) (4 5) 12)
      (lambda () (set! (cdaar x) (list 5 6)) x))
(test "(setter cdadr)" '((((13 14) 5 6) (2 3) 5) ((0 1) 7 8) (4 5) 12)
      (lambda () (set! (cdadr x) (list 7 8)) x))
(test "(setter cddar)" '((((13 14) 5 6) (2 3) 9 10) ((0 1) 7 8) (4 5) 12)
      (lambda () (set! (cddar x) (list 9 10)) x))
(test "(setter cdddr)" '((((13 14) 5 6) (2 3) 9 10) ((0 1) 7 8) (4 5) -1 -2)
      (lambda () (set! (cdddr x) (list -1 -2)) x))
(test "(setter caaaar)" '(((((1 3) 14) 5 6) (2 3) 9 10) ((0 1) 7 8) (4 5) -1 -2)
      (lambda () (set! (caaaar x) (list 1 3)) x))
(test "(setter caaadr)" '(((((1 3) 14) 5 6) (2 3) 9 10) (((2 3) 1) 7 8) (4 5) -1 -2)
      (lambda () (set! (caaadr x) (list 2 3)) x))
(test "(setter caadar)" '(((((1 3) 14) 5 6) ((0 1) 3) 9 10) (((2 3) 1) 7 8) (4 5) -1 -2)
      (lambda () (set! (caadar x) (list 0 1)) x))
(test "(setter caaddr)" '(((((1 3) 14) 5 6) ((0 1) 3) 9 10) (((2 3) 1) 7 8) ((0 1) 5) -1 -2)
      (lambda () (set! (caaddr x) (list 0 1)) x))
(test "(setter cadaar)" '(((((1 3) 14) (0 1) 6) ((0 1) 3) 9 10) (((2 3) 1) 7 8) ((0 1) 5) -1 -2)
      (lambda () (set! (cadaar x) (list 0 1)) x))
(test "(setter cadadr)" '(((((1 3) 14) (0 1) 6) ((0 1) 3) 9 10) (((2 3) 1) (0 1) 8) ((0 1) 5) -1 -2)
      (lambda () (set! (cadadr x) (list 0 1)) x))
(test "(setter caddar)" '(((((1 3) 14) (0 1) 6) ((0 1) 3) (0 1) 10) (((2 3) 1) (0 1) 8) ((0 1) 5) -1 -2)
      (lambda () (set! (caddar x) (list 0 1)) x))
(test "(setter cadddr)" '(((((1 3) 14) (0 1) 6) ((0 1) 3) (0 1) 10) (((2 3) 1) (0 1) 8) ((0 1) 5) (0 1) -2)
      (lambda () (set! (cadddr x) (list 0 1)) x))
(test "(setter cdaaar)" '(((((1 3) 0 1) (0 1) 6) ((0 1) 3) (0 1) 10) (((2 3) 1) (0 1) 8) ((0 1) 5) (0 1) -2)
      (lambda () (set! (cdaaar x) (list 0 1)) x))
(test "(setter cdaadr)" '(((((1 3) 0 1) (0 1) 6) ((0 1) 3) (0 1) 10) (((2 3) 0 1) (0 1) 8) ((0 1) 5) (0 1) -2)
      (lambda () (set! (cdaadr x) (list 0 1)) x))
(test "(setter cdadar)" '(((((1 3) 0 1) (0 1) 6) ((0 1) 0 1) (0 1) 10) (((2 3) 0 1) (0 1) 8) ((0 1) 5) (0 1) -2)
      (lambda () (set! (cdadar x) (list 0 1)) x))
(test "(setter cdaddr)" '(((((1 3) 0 1) (0 1) 6) ((0 1) 0 1) (0 1) 10) (((2 3) 0 1) (0 1) 8) ((0 1) 0 1) (0 1) -2)
      (lambda () (set! (cdaddr x) (list 0 1)) x))
(test "(setter cddaar)" '(((((1 3) 0 1) (0 1) 0 1) ((0 1) 0 1) (0 1) 10) (((2 3) 0 1) (0 1) 8) ((0 1) 0 1) (0 1) -2)
      (lambda () (set! (cddaar x) (list 0 1)) x))
(test "(setter cddadr)" '(((((1 3) 0 1) (0 1) 0 1) ((0 1) 0 1) (0 1) 10) (((2 3) 0 1) (0 1) 0 1) ((0 1) 0 1) (0 1) -2)
      (lambda () (set! (cddadr x) (list 0 1)) x))
(test "(setter cdddar)" '(((((1 3) 0 1) (0 1) 0 1) ((0 1) 0 1) (0 1) 0 1) (((2 3) 0 1) (0 1) 0 1) ((0 1) 0 1) (0 1) -2)
      (lambda () (set! (cdddar x) (list 0 1)) x))
(test "(setter cddddr)" '(((((1 3) 0 1) (0 1) 0 1) ((0 1) 0 1) (0 1) 0 1) (((2 3) 0 1) (0 1) 0 1) ((0 1) 0 1) (0 1) 0 1)
      (lambda () (set! (cddddr x) (list 0 1)) x))

(define x '#(1 2 3 4 5))
(test "(setter vector-ref)" '#(1 2 3 #f 5)
      (lambda () (set! (vector-ref x 3) #f) x))

(define x (string-copy "abcde"))
(test "(setter string-ref)" "abcQe"
      (lambda () (set! (string-ref x 3) #\Q) x))

(define (set-kar! p v) (set-car! p v))
(define kar (getter-with-setter (lambda (p) (car p)) set-kar!))

(define x (cons 1 2))
(test "(setter kar)" '(3 . 2) (lambda () (set! (kar x) 3) x))

;; see it works as the normal set!
(test "set!" '#f (lambda () (set! x #f) x))

(test-end)
