;;;
;;; Test extension srfis
;;;

(use gauche.test)

(test-start "extension srfi modules")

;;
;; Test srfi-1
;;

(test-section "srfi-1")
(use srfi-1)
(test-module 'srfi-1)

(test* "xcons" '(a . b) (xcons 'b 'a))
(test* "cons*" '() (cons*))
(test* "cons*" 'o  (cons* 'o))
(test* "cons*" '(1 2 3 . 4) (cons* 1 2 3 4))
(test* "make-list" 5 (length (make-list 5)))
(test* "make-list" '() (make-list 0))
(test* "make-list" '(m m m m m) (make-list 5 'm))
(test* "list-tabulate" '(0 1 2 3 4) (list-tabulate 5 values))
(test* "list-tabulate" '(#\0 #\1 #\2 #\3 #\4)
       (list-tabulate 5 (lambda (i) (integer->char (+ i 48)))))
(test* "list-tabulate" '(0 2 4 6) (list-tabulate 4 (lambda (i) (* i 2))))
(test* "list-copy" '(1 2 3) (list-copy '(1 2 3)))
(test* "list-copy" '() (list-copy '()))
(test* "list-copy" '(1 . 2) (list-copy '(1 . 2)))
(test* "list-copy" '(1 2 . 3) (list-copy '(1 2 . 3)))
(test* "list-copy" '(1 2 3)
       (let ((x (list 1 2 3)))
         (set-car! (list-copy x) 0)
         x))
(test* "list-copy" '(1 2 . 3)
       (let ((x '(1 2 . 3)))
         (set-cdr! (cdr (list-copy x)) 0)
         x))
(test* "circular-list" 'a
       (list-ref (circular-list 'a 'b) 4))
(test* "iota" '(0 1 2 3 4) (iota 5))
(test* "iota" '(5 6 7 8 9) (iota 5 5))
(test* "iota" '(10 20 30 40 50) (iota 5 10 10))
(test* "proper-list?" #t (proper-list? '(1 2 4)))
(test* "proper-list?" #t (proper-list? '()))
(test* "proper-list?" #f (proper-list? 2))
(test* "proper-list?" #f (proper-list? '(3 . 1)))
(test* "proper-list?" #f (proper-list? (circular-list 1 2)))
(test* "proper-list?" #f (proper-list? (cons 0 (circular-list 1 2))))
(test* "circular-list?" #t (circular-list? (circular-list 1 2)))
(test* "circular-list?" #f (circular-list? '()))
(test* "circular-list?" #f (circular-list? '(1 . 2)))
(test* "circular-list?" #t (circular-list? (cons 1 (circular-list 2 3))))
(test* "dotted-list?" #t (dotted-list? '(1 . 2)))
(test* "dotted-list?" #t (dotted-list? 3))
(test* "dotted-list?" #f (dotted-list? '(1 2 3)))
(test* "dotted-list?" #f (dotted-list? (circular-list 1 2 3)))
(test* "dotted-list?" #f (dotted-list? (cons 0 (circular-list 1 2 3))))
(test* "dotted-list?" #f (dotted-list? '()))
(test* "null-list?" #t (null-list? '()))
(test* "null-list?" #f (null-list? '(1)))
(test* "not-pair?" #f (not-pair? '(1)))
(test* "not-pair?" #t (not-pair? '()))
(test* "list=" #t (list= eq?))
(test* "list=" #t (list= eq? '(a)))
(test* "list=" #t (list= eq? '(a) '(a)))
(test* "list=" #f (list= eq? '(a) '(a b)))
(test* "list=" #t (list= char-ci=? '(#\a #\b #\z) '(#\A #\B #\Z)))
(test* "list=" #f (list= char-ci=? '(#\a #\b #\z) '(#\A #\B)))
(test* "first"  1 (first '(1 2 3 4 5 6 7 8 9 10)))
(test* "second" 2 (second '(1 2 3 4 5 6 7 8 9 10)))
(test* "third"  3 (third '(1 2 3 4 5 6 7 8 9 10)))
(test* "fourth" 4 (fourth '(1 2 3 4 5 6 7 8 9 10)))
(test* "fifth"  5 (fifth '(1 2 3 4 5 6 7 8 9 10)))
(test* "sixth"  6 (sixth '(1 2 3 4 5 6 7 8 9 10)))
(test* "seventh" 7 (seventh '(1 2 3 4 5 6 7 8 9 10)))
(test* "eighth" 8 (eighth '(1 2 3 4 5 6 7 8 9 10)))
(test* "ninth"  9 (ninth '(1 2 3 4 5 6 7 8 9 10)))
(test* "tenth"  10 (tenth '(1 2 3 4 5 6 7 8 9 10)))
(test* "car+cdr" '(a (b c))
       (call-with-values (lambda () (car+cdr '(a b c))) list))
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
       (call-with-values
           (lambda () (split-at '(a b c d e f g h) 3))
         list))
(test* "split-at!" '((a b c) (d e f g h))
       (call-with-values
           (lambda () (split-at! (list 'a 'b 'c 'd 'e 'f 'g 'h) 3))
         list))
(test* "last" 'c (last '(a b c)))
(test* "last-pair" '(c) (last-pair '(a b c)))
(test* "length+" '(5 #f)
       (list (length+ '(1 2 3 4 5))
             (length+ (circular-list 1 2 3 4 5))))
(test* "append" '(1 2 3 4 5) (append '(1 2 3) '() '(4 5)))
(test* "append" '(1 2 3 . 5) (append '(1 2 3) 5))
(test* "append!" '(1 2 3 4 5) (append (list 1 2 3) '() '(4 5)))
(test* "append!" '(1 2 3 . 5) (append (list 1 2 3) 5))
(test* "concatenate" '(1 2 3 4 5)
       (concatenate '((1 2 3) () (4 5))))
(test* "concatenate" '(1 2 3 4 . 5)
       (concatenate '((1 2 3) () (4) 5)))
(test* "concatenate!" '(1 2 3 4 5)
       (concatenate (list (list 1 2 3) '() (list 4 5))))
(test* "concatenate!" '(1 2 3 4 . 5)
       (concatenate (list (list 1 2 3) '() (list 4) 5)))
(test* "reverse" '(5 4 3 2 1) (reverse '(1 2 3 4 5)))
(test* "reverse" '() (reverse '()))
(test* "reverse!" '(5 4 3 2 1) (reverse! (list 1 2 3 4 5)))
(test* "reverse!" '() (reverse! '()))
(test* "append-reverse" '(1 2 3 4 5)
       (append-reverse '(3 2 1) '(4 5)))
(test* "append-reverse" '(1 2 3 4 . 5)
       (append-reverse '(4 3 2 1) 5))
(test* "append-reverse!" '(1 2 3 4 5)
       (append-reverse! (list 3 2 1) (list 4 5)))
(test* "append-reverse!" '(1 2 3 4 . 5)
       (append-reverse! (list 4 3 2 1) 5))
(test* "zip" '((one 1 odd) (two 2 even) (three 3 odd))
       (zip '(one two three) '(1 2 3)
            '(odd even odd even odd even)))
(test* "zip" '((1) (2) (3)) (zip '(1 2 3)))
(test* "zip" '((3 #f) (1 #t) (4 #f) (1 #t))
       (zip '(3 1 4 1) (circular-list #f #t)))
(test* "zip" '() (zip '(a b) '() '(c d)))
(define unzip-data '((1 2 3 4 5 6 7 8)
                     (a b c d e)
                     (#\a #\b #\c #\d #\e)
                     ("a" "b" "c" "d" "e")))
(test* "unzip1" '(1 a #\a "a") (unzip1 unzip-data))
(test* "unzip2" '((1 a #\a "a") (2 b #\b "b"))
       (call-with-values (lambda () (unzip2 unzip-data)) list))
(test* "unzip3" '((1 a #\a "a") (2 b #\b "b") (3 c #\c "c"))
       (call-with-values (lambda () (unzip3 unzip-data)) list))
(test* "unzip4" '((1 a #\a "a") (2 b #\b "b") (3 c #\c "c") (4 d #\d "d"))
       (call-with-values (lambda () (unzip4 unzip-data)) list))
(test* "unzip5"
       '((1 a #\a "a") (2 b #\b "b") (3 c #\c "c") (4 d #\d "d") (5 e #\e "e"))
       (call-with-values (lambda () (unzip5 unzip-data)) list))
(test* "count" 3 (count even? '(3 1 4 1 5 9 2 6 5)))
(test* "count" 3
       (count < '(1 2 4 8) '(2 4 6 8 10 12 14 16)))
(test* "count" 2
       (count < '(3 1 4 1) (circular-list 1 10)))
(test* "fold" 55
       (fold + 0 '(1 2 3 4 5 6 7 8 9 10)))
(test* "fold" '(e d c b a)
       (fold cons '() '(a b c d e)))
(test* "fold" 3
       (fold (lambda (x c) (if (symbol? x) (+ c 1) c))
             0
             '(a 3 b 8 c 9)))
(test* "fold" '(c 3 b 2 a 1)
       (fold cons* '() '(a b c) '(1 2 3 4 5)))
(test* "fold-right" '(1 2 3 4 5)
       (fold-right cons '() '(1 2 3 4 5)))
(test* "fold-right" '(2 4 6)
       (fold-right (lambda (x l) (if (even? x) (cons x l) l))
                   '()
                   '(1 2 3 4 5 6 7)))
(test* "fold-right" '(a 1 b 2 c 3)
       (fold-right cons* '() '(a b c) '(1 2 3 4 5)))
(test* "pair-fold" '(5 4 3 2 1)
       (pair-fold (lambda (p t) (set-cdr! p t) p) '()
                  (list 1 2 3 4 5)))
(test* "pair-fold-right" '((a b c) (b c) (c))
       (pair-fold-right cons '() '(a b c)))
(test* "reduce" 55 (reduce + 0 (iota 10 1)))
(test* "reduce-right" '(1 2 3 4 5 6 7 8 9 . 10)
       (reduce-right cons 0 (iota 10 1)))
(test* "unfold" '(1 4 9 16 25 36 49 64 81 100)
       (unfold (lambda (x) (> x 10))
               (lambda (x) (* x x))
               (lambda (x) (+ x 1))
               1))
(test* "unfold-right" '(1 4 9 16 25 36 49 64 81 100)
       (unfold-right zero?
                     (lambda (x) (* x x))
                     (lambda (x) (- x 1))
                     10))
(test* "map" '(4 1 5 1)
       (map + '(3 1 4 1) (circular-list 1 0)))
(test* "for-each" '#(0 2 2 4 4)
       (let ((v (make-vector 5)))
         (for-each (lambda (i n)
                     (vector-set! v i (+ i n)))
                   '(0 1 2 3 4)
                   (circular-list 0 1))
         v))
(test* "append-map" '(1 -1 3 -3 5 -5)
       (append-map (lambda (x) (list x (- x))) '(1 3 5)))
(test* "append-map" '(1 -2 3 -4 5 -6)
       (append-map (lambda (x y) (list x (- y)))
                   '(1 3 5) '(2 4 6 8)))
(test* "append-map!" '(1 -2 3 -4 5 -6)
       (append-map! (lambda (x y) (list x (- y)))
                    '(1 3 5) '(2 4 6 8)))
(test* "map!" '(4 1 5 1)
       (map! + '(3 1 4 1) (circular-list 1 0)))
(test* "map-in-order"  '(4 1 5 1)
       (map-in-order + '(3 1 4 1) (circular-list 1 0)))
(test* "pair-for-each" '((c) (b c) (a b c))
       (let ((r '()))
         (pair-for-each (lambda (l) (set! r (cons l r)))
                        '(a b c))
         r))
(test* "filter-map" '(1 9 49)
       (filter-map (lambda (x) (and (number? x) (* x x)))
                   '(a 1 b 3 c 7)))
(test* "filter" '(0 8 8 -4)
       (filter even? '(0 7 8 8 9 -4)))
(test* "partition" '((one four five) (2 3 6))
       (receive x (partition symbol? '(one 2 3 four five 6)) x))
(test* "remove" '(7 43)
       (remove even? '(0 7 8 8 43 -4)))
(test* "filter!" '(0 8 8 -4)
       (filter even? (list 0 7 8 8 9 -4)))
(test* "partition!" '((one four five) (2 3 6))
       (receive x (partition! symbol? (list 'one 2 3 'four 'five 6)) x))
(test* "remove!" '(7 43)
       (remove! even? (list 0 7 8 8 43 -4)))
(test* "find" 4
       (find even? '(3 1 4 1 5 9)))
(test* "find" #f
       (find even? '(3 1 1 5 9)))
(test* "fid-tail" '(-8 -5 0 0)
       (find-tail even? '(3 1 -8 -5 0 0)))
(test* "find-tail" #f
       (find-tail even? '(3 1 -9 -5 1 -1)))
(test* "take-while" '(2 18)
       (take-while even? '(2 18 3 10 22 9)))
(test* "take-while!" '(2 18)
       (take-while! even? (list 2 18 3 10 22 9)))
(test* "drop-while" '(3 10 22 9)
       (drop-while even? '(3 10 22 9)))
(test* "span" '((2 18) (3 10 22 9))
       (receive x (span even? '(2 18 3 10 22 9)) x))
(test* "break" '((2 18) (3 10 22 9))
       (receive x (break odd? '(2 18 3 10 22 9)) x))
(test* "span!" '((2 18) (3 10 22 9))
       (receive x (span! even? (list 2 18 3 10 22 9)) x))
(test* "break!" '((2 18) (3 10 22 9))
       (receive x (break! odd? (list 2 18 3 10 22 9)) x))
(test* "any" #t
       (any integer? '(a 3 b 2.8)))
(test* "any" #f
       (any integer? '(a 3.1 b 2.8)))
(test* "any" #t
       (any < '(3 1 4 1 5) '(2 7 1 8 2)))
(test* "every" #f
       (every integer? '(a 3 b 2.8)))
(test* "every" #t
       (every integer? '(2.0 3.0 8.0 -1.0)))
(test* "every" #t
       (every <= '(1 2 3 4 5) '(2 3 3 5 5)))
(test* "list-index" 2
       (list-index even? '(3 1 4 1 5 9)))
(test* "list-index" 1
       (list-index < '(3 1 4 1 5 9) '(2 7 1)))
(test* "list-index" #f
       (list-index = '(3 1 4 1 5 9) '(2 7 1)))
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
(test* "assq" '(a 1) (assq 'a '((a 1) (b 2) (c 3))))
(test* "assq" #f     (assq 'd '((a 1) (b 2) (c 3))))
(test* "assq" #f     (assq (list 'a) '(((a)) ((b)) ((c)))))
(test* "assv" '(b 2) (assv 'b '((a 1) (b 2) (c 3))))
(test* "assv" #f     (assv 'd '((a 1) (b 2) (c 3))))
(test* "assv" #f     (assv (list 'a) '(((a)) ((b)) ((c)))))
(test* "assoc" '((a)) (assoc (list 'a) '(((a)) ((b)) ((c)))))
(test* "assoc" '("a") (assoc "a" '(("c") ("b") ("a"))))
(test* "assoc" '("a") (assoc "A" '(("c") ("b") ("a")) string-ci=?))
(test* "alist-cons" '((1 . 2) . 3) (alist-cons 1 2 3))
(test* "alist-copy" '((a 1) (a 2))
       (let* ((x '((b 2) (a 1)))
              (y (alist-copy x)))
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
       (let ((l (alist-copy '((a 1) (b 2) (c 3)))))
         (alist-delete! 'x l)
         l))
(test* "alist-delete!" '((b 2) (c 3))
       (alist-delete! 'a (alist-copy '((a 1) (b 2) (c 3)))))
(test* "alist-delete!" '(z (b 2) (c 3))
       (alist-delete! 'a (list-copy '(z (a 1) (b 2) (c 3)))))
(test* "alist-delete!" '(z (b 2) (c 3))
       (alist-delete! 'a (list-copy '((a 1) z (b 2) (c 3)))))
(test* "alist-delete!" '((a 1) (c 3))
       (let ((l (alist-copy '((a 1) (b 2) (c 3)))))
         (alist-delete! 'b l)
         l))
(test* "alist-delete!" '((a 1) (b 2))
       (let ((l (alist-copy '((a 1) (b 2) (c 3)))))
         (alist-delete! 'c l)
         l))

;; TODO: lset stuff

;;;
;;; test srfi-13
;;;

(test-section "srfi-13")
(use srfi-13)
(test-module 'srfi-13)

(test* "string-null?" #f (string-null? "abc"))
(test* "string-null?" #t (string-null? ""))
(test* "string-every" #t (string-every #\a ""))
(test* "string-every" #t (string-every #\a "aaaa"))
(test* "string-every" #f (string-every #\a "aaba"))
(test* "string-every" #t (string-every #[a-z] "aaba"))
(test* "string-every" #f (string-every #[a-z] "aAba"))
(test* "string-every" #t (string-every #[a-z] ""))
(test* "string-every" #t (string-every (lambda (x) (char-ci=? x #\a)) "aAaA"))
(test* "string-every" #f (string-every (lambda (x) (char-ci=? x #\a)) "aAbA"))
(test* "string-every" (char->integer #\A)
       (string-every (lambda (x) (char->integer x)) "aAbA"))
(test* "string-every" #t
       (string-every (lambda (x) (error "hoge")) ""))
(test* "string-any" #t (string-any #\a "aaaa"))
(test* "string-any" #f (string-any #\a "Abcd"))
(test* "string-any" #f (string-any #\a ""))
(test* "string-any" #t (string-any #[a-z] "ABcD"))
(test* "string-any" #f (string-any #[a-z] "ABCD"))
(test* "string-any" #f (string-any #[a-z] ""))
(test* "string-any" #t (string-any (lambda (x) (char-ci=? x #\a)) "CAaA"))
(test* "string-any" #f (string-any (lambda (x) (char-ci=? x #\a)) "ZBRC"))
(test* "string-any" #f (string-any (lambda (x) (char-ci=? x #\a)) ""))
(test* "string-any" (char->integer #\a)
       (string-any (lambda (x) (char->integer x)) "aAbA"))
(test* "string-tabulate" "0123456789"
       (string-tabulate (lambda (code)
                          (integer->char (+ code (char->integer #\0))))
                        10))
(test* "string-tabulate" ""
       (string-tabulate (lambda (code)
                          (integer->char (+ code (char->integer #\0))))
                        0))
(test* "reverse-list->string" "cBa"
       (reverse-list->string '(#\a #\B #\c)))
(test* "reverse-list->string" ""
       (reverse-list->string '()))
; string-join : Gauche builtin.
(test* "substring/shared" "cde" (substring/shared "abcde" 2))
(test* "substring/shared" "cd"  (substring/shared "abcde" 2 4))
(test* "string-copy!" "abCDEfg"
       (let ((x (string-copy "abcdefg")))
         (string-copy! x 2 "CDE")
         x))
(test* "string-copy!" "abCDEfg"
       (let ((x (string-copy "abcdefg")))
         (string-copy! x 2 "ZABCDE" 3)
         x))
(test* "string-copy!" "abCDEfg"
       (let ((x (string-copy "abcdefg")))
         (string-copy! x 2 "ZABCDEFG" 3 6)
         x))
(test* "string-take" "Pete S"  (string-take "Pete Szilagyi" 6))
(test* "string-take" ""        (string-take "Pete Szilagyi" 0))
(test* "string-take" "Pete Szilagyi" (string-take "Pete Szilagyi" 13))
(test* "string-drop" "zilagyi" (string-drop "Pete Szilagyi" 6))
(test* "string-drop" "Pete Szilagyi" (string-drop "Pete Szilagyi" 0))
(test* "string-drop" ""        (string-drop "Pete Szilagyi" 13))

(test* "string-take-right" "rules" (string-take-right "Beta rules" 5))
(test* "string-take-right" ""      (string-take-right "Beta rules" 0))
(test* "string-take-right" "Beta rules" (string-take-right "Beta rules" 10))
(test* "string-drop-right" "Beta " (string-drop-right "Beta rules" 5))
(test* "string-drop-right" "Beta rules" (string-drop-right "Beta rules" 0))
(test* "string-drop-right" ""      (string-drop-right "Beta rules" 10))

(test* "string-pad" "  325" (string-pad "325" 5))
(test* "string-pad" "71325" (string-pad "71325" 5))
(test* "string-pad" "71325" (string-pad "8871325" 5))
(test* "string-pad" "~~325" (string-pad "325" 5 #\~))
(test* "string-pad" "~~~25" (string-pad "325" 5 #\~ 1))
(test* "string-pad" "~~~~2" (string-pad "325" 5 #\~ 1 2))
(test* "string-pad-right" "325  " (string-pad-right "325" 5))
(test* "string-pad-right" "71325" (string-pad-right "71325" 5))
(test* "string-pad-right" "88713" (string-pad-right "8871325" 5))
(test* "string-pad-right" "325~~" (string-pad-right "325" 5 #\~))
(test* "string-pad-right" "25~~~" (string-pad-right "325" 5 #\~ 1))
(test* "string-pad-right" "2~~~~" (string-pad-right "325" 5 #\~ 1 2))

(test* "string-trim"  "a b c d  \r\n"
       (string-trim "  \t  a b c d  \r\n"))
(test* "string-trim"  "\t  a b c d  \r\n"
       (string-trim "  \t  a b c d  \r\n" #\space))
(test* "string-trim"  "a b c d  \r\n"
       (string-trim "4358948a b c d  \r\n" #[\d]))

(test* "string-trim-right"  "  \t  a b c d"
       (string-trim-right "  \t  a b c d  \r\n"))
(test* "string-trim-right"  "  \t  a b c d  "
       (string-trim-right "  \t  a b c d  \r\n" #[\r\n]))
(test* "string-trim-right"  "349853a b c d"
       (string-trim-right "349853a b c d03490" #[\d]))

(test* "string-trim-both"  "a b c d"
       (string-trim-both "  \t  a b c d  \r\n"))
(test* "string-trim-both"  "  \t  a b c d  "
       (string-trim-both "  \t  a b c d  \r\n" #[\r\n]))
(test* "string-trim-both"  "a b c d"
       (string-trim-both "349853a b c d03490" #[\d]))

;; string-fill - in string.scm

(test* "string-compare" 5
       (string-compare "The cat in the hat" "abcdefgh"
                       values values values
                       4 6 2 4))
(test* "string-compare-ci" 5
       (string-compare-ci "The cat in the hat" "ABCDEFGH"
                          values values values
                          4 6 2 4))

;; TODO: bunch of string= families

(test* "string-prefix-length" 5
       (string-prefix-length "cancaNCAM" "cancancan"))
(test* "string-prefix-length" 1
       (string-prefix-length "abc" "a"))
(test* "string-prefix-length" 1
       (string-prefix-length "a" "abc"))
(test* "string-prefix-length" 0
       (string-prefix-length "abc" ""))
(test* "string-prefix-length" 0
       (string-prefix-length "" "abc"))
(test* "string-prefix-length" 0
       (string-prefix-length "" ""))
(test* "string-prefix-length-ci" 8
       (string-prefix-length-ci "cancaNCAM" "cancancan"))
(test* "string-suffix-length" 2
       (string-suffix-length "CanCan" "cankancan"))
(test* "string-suffix-length-ci" 5
       (string-suffix-length-ci "CanCan" "cankancan"))
(test* "string-prefix-length-ci" 1
       (string-prefix-length-ci "abc" "A"))
(test* "string-prefix-length-ci" 1
       (string-prefix-length-ci "A" "abc"))
(test* "string-prefix-length-ci" 0
       (string-prefix-length-ci "abc" ""))
(test* "string-prefix-length-ci" 0
       (string-prefix-length-ci "" "abc"))
(test* "string-prefix-length-ci" 0
       (string-prefix-length-ci "" ""))

(test* "string-prefix?" #t    (string-prefix? "abcd" "abcdefg"))
(test* "string-prefix?" #f    (string-prefix? "abcf" "abcdefg"))
(test* "string-prefix-ci?" #t (string-prefix-ci? "abcd" "aBCDEfg"))
(test* "string-prefix-ci?" #f (string-prefix-ci? "abcf" "aBCDEfg"))
(test* "string-suffix?" #t    (string-suffix? "defg" "abcdefg"))
(test* "string-suffix?" #f    (string-suffix? "aefg" "abcdefg"))
(test* "string-suffix-ci?" #t (string-suffix-ci? "defg" "aBCDEfg"))
(test* "string-suffix-ci?" #f (string-suffix-ci? "aefg" "aBCDEfg"))

(test* "string-index" 4
       (string-index "abcd:efgh:ijkl" #\:))
(test* "string-index" 4
       (string-index "abcd:efgh;ijkl" #[\W]))
(test* "string-index" #f
       (string-index "abcd:efgh;ijkl" #[\d]))
(test* "string-index" 9
       (string-index "abcd:efgh:ijkl" #\: 5))
(test* "string-index-right" 4
       (string-index-right "abcd:efgh;ijkl" #\:))
(test* "string-index-right" 9
       (string-index-right "abcd:efgh;ijkl" #[\W]))
(test* "string-index-right" #f
       (string-index-right "abcd:efgh;ijkl" #[\d]))
(test* "string-index-right" 4
       (string-index-right "abcd:efgh;ijkl" #[\W] 2 5))

(test* "string-count" 2
       (string-count "abc def\tghi jkl" #\space))
(test* "string-count" 3
       (string-count "abc def\tghi jkl" #[\s]))
(test* "string-count" 2
       (string-count "abc def\tghi jkl" #[\s] 4))
(test* "string-count" 1
       (string-count "abc def\tghi jkl" #[\s] 4 9))
(test* "string-contains" 3
       (string-contains "Ma mere l'oye" "mer"))
(test* "string-contains" #f
       (string-contains "Ma mere l'oye" "Mer"))
(test* "string-contains-ci" 3
       (string-contains-ci "Ma mere l'oye" "Mer"))
(test* "string-contains-ci" #f
       (string-contains-ci "Ma mere l'oye" "Meer"))
(test* "string-contains" 15
       (string-contains "eek -- what a geek." "ee" 12 18))
(test* "string-contains-ci" 15
       (string-contains-ci "Eek -- what a geek." "EE" 12 18))

(test* "string-titlecase" "--Capitalize This Sentence."
       (string-titlecase "--capitalize tHIS sentence."))
(test* "string-titlecase" "3Com Makes Routers."
       (string-titlecase "3com makes routers."))
(test* "string-titlecase!" "alSo Whatever"
       (let ((s (string-copy "also whatever")))
         (string-titlecase! s 2 9)
         s))

(test* "string-upcase" "SPEAK LOUDLY"
       (string-upcase "speak loudly"))
(test* "string-upcase" "PEAK"
       (string-upcase "speak loudly" 1 5))
(test* "string-upcase!" "sPEAK loudly"
       (let ((s (string-copy "speak loudly")))
         (string-upcase! s 1 5)
         s))

(test* "string-downcase" "speak softly"
       (string-downcase "SPEAK SOFTLY"))
(test* "string-downcase" "peak"
       (string-downcase "SPEAK SOFTLY" 1 5))
(test* "string-downcase!" "Speak SOFTLY"
       (let ((s (string-copy "SPEAK SOFTLY")))
         (string-downcase! s 1 5)
         s))

(test* "string-reverse" "nomel on nolem on"
       (string-reverse "no melon no lemon"))
(test* "string-reverse" "nomel on"
       (string-reverse "no melon no lemon" 9))
(test* "string-reverse" "on"
       (string-reverse "no melon no lemon" 9 11))
(test* "string-reverse!" "nomel on nolem on"
       (let ((s (string-copy "no melon no lemon")))
         (string-reverse! s) s))
(test* "string-reverse!" "no melon nomel on"
       (let ((s (string-copy "no melon no lemon")))
         (string-reverse! s 9) s))
(test* "string-reverse!" "no melon on lemon"
       (let ((s (string-copy "no melon no lemon")))
         (string-reverse! s 9 11) s))

(test* "string-append" #f
       (let ((s "test")) (eq? s (string-append s))))
(test* "string-concatenate" "" (string-concatenate '()))
(test* "string-concatenate" #f
       (let ((s "test")) (eq? s (string-concatenate (list s)))))
(test* "string-concatenate" "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
       (string-concatenate
        '("A" "B" "C" "D" "E" "F" "G" "H"
          "I" "J" "K" "L" "M" "N" "O" "P"
          "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
          "a" "b" "c" "d" "e" "f" "g" "h"
          "i" "j" "k" "l" "m" "n" "o" "p"
          "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))
(test* "string-concatenate (long list)" #t
       (string=? (make-string 20000 #\a)
                 (string-concatenate (make-list 20000 "a"))))
(test* "string-concatenate/shared" "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
       (string-concatenate/shared
        '("A" "B" "C" "D" "E" "F" "G" "H"
          "I" "J" "K" "L" "M" "N" "O" "P"
          "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
          "a" "b" "c" "d" "e" "f" "g" "h"
          "i" "j" "k" "l" "m" "n" "o" "p"
          "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))
(test* "string-concatenate-reverse" "zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLKJIHGFEDCBA"
       (string-concatenate-reverse
        '("A" "B" "C" "D" "E" "F" "G" "H"
          "I" "J" "K" "L" "M" "N" "O" "P"
          "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
          "a" "b" "c" "d" "e" "f" "g" "h"
          "i" "j" "k" "l" "m" "n" "o" "p"
          "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))
(test* "string-concatenate-reverse" #f
       (let ((s "test"))
         (eq? s (string-concatenate-reverse (list s)))))
(test* "string-concatenate-reverse (optarg)"
       "Hello, I must be going.XXXXX"
       (string-concatenate-reverse '(" must be" "Hello, I") " going.XXXXX"))
(test* "string-concatenate-reverse (optarg)"
       "Hello, I must be going."
       (string-concatenate-reverse '(" must be" "Hello, I") " going.XXXXX" 7))

(test* "string-concatenate-reverse/shared" "zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLKJIHGFEDCBA"
       (string-concatenate-reverse/shared
        '("A" "B" "C" "D" "E" "F" "G" "H"
          "I" "J" "K" "L" "M" "N" "O" "P"
          "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
          "a" "b" "c" "d" "e" "f" "g" "h"
          "i" "j" "k" "l" "m" "n" "o" "p"
          "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))

(test* "string-map" "svool"
       (string-map (lambda (c)
                     (integer->char (- 219 (char->integer c))))
                   "hello"))
(test* "string-map" "vool"
       (string-map (lambda (c)
                     (integer->char (- 219 (char->integer c))))
                   "hello" 1))
(test* "string-map" "vo"
       (string-map (lambda (c)
                     (integer->char (- 219 (char->integer c))))
                   "hello" 1 3))
(test* "string-map!" "svool"
       (let ((s (string-copy "hello")))
         (string-map! (lambda (c)
                        (integer->char (- 219 (char->integer c))))
                      s)
         s))
(test* "string-map!" "hvool"
       (let ((s (string-copy "hello")))
         (string-map! (lambda (c)
                        (integer->char (- 219 (char->integer c))))
                      s 1)
         s))
(test* "string-map!" "hvolo"
       (let ((s (string-copy "hello")))
         (string-map! (lambda (c)
                        (integer->char (- 219 (char->integer c))))
                      s 1 3)
         s))

(test* "string-fold" '(#\o #\l #\l #\e #\h . #t)
       (string-fold cons #t "hello"))
(test* "string-fold" '(#\l #\e . #t)
       (string-fold cons #t "hello" 1 3))
(test* "string-fold-right" '(#\h #\e #\l #\l #\o . #t)
       (string-fold-right cons #t "hello"))
(test* "string-fold-right" '(#\e #\l . #t)
       (string-fold-right cons #t "hello" 1 3))

(test* "string-unfold" "hello"
       (string-unfold null? car cdr '(#\h #\e #\l #\l #\o)))
(test* "string-unfold" "hi hello"
       (string-unfold null? car cdr '(#\h #\e #\l #\l #\o) "hi "))
(test* "string-unfold" "hi hello ho"
       (string-unfold null? car cdr
                      '(#\h #\e #\l #\l #\o) "hi "
                      (lambda (x) " ho")))

(test* "string-unfold-right" "olleh"
       (string-unfold-right null? car cdr '(#\h #\e #\l #\l #\o)))
(test* "string-unfold-right" "olleh hi"
       (string-unfold-right null? car cdr '(#\h #\e #\l #\l #\o) " hi"))
(test* "string-unfold-right" "ho olleh hi"
       (string-unfold-right null? car cdr
                            '(#\h #\e #\l #\l #\o) " hi"
                            (lambda (x) "ho ")))

(test* "string-for-each" "CLtL"
       (let ((out (open-output-string))
             (prev #f))
         (string-for-each (lambda (c)
                            (if (or (not prev)
                                    (char-whitespace? prev))
                                (write-char c out))
                            (set! prev c))
                          "Common Lisp, the Language")

         (get-output-string out)))
(test* "string-for-each" "oLtL"
       (let ((out (open-output-string))
             (prev #f))
         (string-for-each (lambda (c)
                            (if (or (not prev)
                                    (char-whitespace? prev))
                                (write-char c out))
                            (set! prev c))
                          "Common Lisp, the Language" 1)
         (get-output-string out)))
(test* "string-for-each" "oL"
       (let ((out (open-output-string))
             (prev #f))
         (string-for-each (lambda (c)
                            (if (or (not prev)
                                    (char-whitespace? prev))
                                (write-char c out))
                            (set! prev c))
                          "Common Lisp, the Language" 1 10)
         (get-output-string out)))
(test* "string-for-each-index" '(4 3 2 1 0)
       (let ((r '()))
         (string-for-each-index (lambda (i) (set! r (cons i r))) "hello")
         r))
(test* "string-for-each-index" '(4 3 2 1)
       (let ((r '()))
         (string-for-each-index (lambda (i) (set! r (cons i r))) "hello" 1)
         r))
(test* "string-for-each-index" '(2 1)
       (let ((r '()))
         (string-for-each-index (lambda (i) (set! r (cons i r))) "hello" 1 3)
         r))

(test* "xsubstring" "cdefab"
       (xsubstring "abcdef" 2))
(test* "xsubstring" "efabcd"
       (xsubstring "abcdef" -2))
(test* "xsubstring" "abcabca"
       (xsubstring "abc" 0 7))
(test* "xsubstring" "abcabca"
       (xsubstring "abc"
                   30000000000000000000000000000000
                   30000000000000000000000000000007))
(test* "xsubstring" "defdefd"
       (xsubstring "abcdefg" 0 7 3 6))
(test* "xsubstring" ""
       (xsubstring "abcdefg" 9 9 3 6))

(test* "string-xcopy!" "ZZcdefabZZ"
       (let ((s (make-string 10 #\Z)))
         (string-xcopy! s 2 "abcdef" 2)
         s))
(test* "string-xcopy!" "ZZdefdefZZ"
       (let ((s (make-string 10 #\Z)))
         (string-xcopy! s 2 "abcdef" 0 6 3)
         s))

(test* "string-replace" "abcdXYZghi"
       (string-replace "abcdefghi" "XYZ" 4 6))
(test* "string-replace" "abcdZghi"
       (string-replace "abcdefghi" "XYZ" 4 6 2))
(test* "string-replace" "abcdZefghi"
       (string-replace "abcdefghi" "XYZ" 4 4 2))
(test* "string-replace" "abcdefghi"
       (string-replace "abcdefghi" "XYZ" 4 4 1 1))
(test* "string-replace" "abcdhi"
       (string-replace "abcdefghi" "" 4 7))

(test* "string-tokenize" '("Help" "make" "programs" "run," "run," "RUN!")
       (string-tokenize "Help make programs run, run, RUN!"))
(test* "string-tokenize" '("Help" "make" "programs" "run" "run" "RUN")
       (string-tokenize "Help make programs run, run, RUN!"
                        #[a-zA-Z]))
(test* "string-tokenize" '("programs" "run" "run" "RUN")
       (string-tokenize "Help make programs run, run, RUN!"
                        #[a-zA-Z] 10))
(test* "string-tokenize" '("elp" "make" "programs" "run" "run")
       (string-tokenize "Help make programs run, run, RUN!"
                        #[a-z]))

(test* "string-filter" "rrrr"
       (string-filter "Help make programs run, run, RUN!" #\r ))
(test* "string-filter" "HelpmakeprogramsrunrunRUN"
       (string-filter "Help make programs run, run, RUN!"
                      #[a-zA-Z]))
(test* "string-filter" "programsrunrun"
       (string-filter "Help make programs run, run, RUN!"
                      (lambda (c) (char-lower-case? c)) 10))
(test* "string-filter" ""
       (string-filter "" (lambda (c) (char-lower-case? c))))
(test* "string-delete" "Help make pogams un, un, RUN!"
       (string-delete "Help make programs run, run, RUN!" #\r))
(test* "string-delete" "   , , !"
       (string-delete "Help make programs run, run, RUN!"
                      #[a-zA-Z]))
(test* "string-delete" " , , RUN!"
       (string-delete "Help make programs run, run, RUN!"
                      (lambda (c) (char-lower-case? c)) 10))
(test* "string-delete" ""
       (string-delete "" (lambda (c) (char-lower-case? c))))

;;
;; testing srfi-19
;;

(test-section "srfi-19")
(use srfi-19)
(test-module 'srfi-19)

(test* "make-time" '(#t time-utc 100 5555)
       (let1 t (make-time time-utc 5555 100)
         (list (time? t) (time-type t) (time-second t)
               (time-nanosecond t))))
(test* "copy-time" '(#t time-utc 100 5555)
       (let1 t (copy-time (make-time time-utc 5555 100))
         (list (time? t) (time-type t) (time-second t)
               (time-nanosecond t))))
(test* "current-time" '(time-utc time-tai time-monotonic)
       (map time-type (list (current-time)
                            (current-time time-tai)
                            (current-time time-monotonic))))
(test* "time comparison"
       '(#t #f #f
            #t #f #t #f #f 
            #t #f #t #f #t
            #f #t #f #t #f
            #f #t #f #t #t)
       (let ((t0 (make-time time-tai 345676543 23456))
             (t1 (make-time time-tai 293892851 93853))
             (t2 (make-time time-tai 893892851 93853)))
         (list (time=?  t0 t0)
               (time=?  t0 t1)
               (time=?  t1 t2)
               (time<?  t0 t1)
               (time<?  t1 t0)
               (time<?  t1 t2)
               (time<?  t2 t1)
               (time<?  t1 t1)
               (time<=? t0 t1)
               (time<=? t1 t0)
               (time<=? t1 t2)
               (time<=? t2 t1)
               (time<=? t0 t0)
               (time>?  t0 t1)
               (time>?  t1 t0)
               (time>?  t1 t2)
               (time>?  t2 t1)
               (time>?  t0 t0)
               (time>=? t0 t1)
               (time>=? t1 t0)
               (time>=? t1 t2)
               (time>=? t2 t1)
               (time>=? t0 t0))))
(test* "time difference" '(#t #t #t #t #t #t)
       (let* ((t0 (current-time))
              (t1 (make-time time-utc 333333333 1000000000))
              (dt (time-difference t1 t0))
              (r0 (eq? (time-type dt) time-duration))
              (t2 (add-duration t0 dt))
              (r1 (eq? (time-type t2) (time-type t0)))
              (r2 (time=? t1 t2))
              (r3 (begin (subtract-duration! t2 dt)
                         (time=? t2 t0)))
              (r4 (begin (add-duration! t0 dt)
                         (time=? t1 t0)))
              (r5 (begin (time-difference! t0 t2)
                         (time=? t0 dt))))
         (list r0 r1 r2 r3 r4 r5)))
(test* "time conversion" '(#t #t)
       (let* ((t0 (current-time))
              (ta (time-utc->time-tai t0))
              (tb (time-tai->time-utc ta))
              (r0 (time=? t0 tb))
              (r1 (time=? ta (begin (time-utc->time-tai! t0) t0))))
         (list r0 r1)))
(test* "time conversion" '(#t #t)
       (let* ((t0 (current-time))
              (ta (time-utc->time-monotonic t0))
              (tb (time-monotonic->time-utc ta))
              (r0 (time=? t0 tb))
              (r1 (time=? ta (begin (time-utc->time-monotonic! t0) t0))))
         (list r0 r1)))
(let ((now (current-time)))
  (test* "make-date"
         (let1 d1 (sys-localtime (time-second now))
           (list (+ (slot-ref d1 'year) 1900)
                 (+ (slot-ref d1 'mon) 1)
                 (slot-ref d1 'mday)
                 (slot-ref d1 'hour)
                 (slot-ref d1 'min)
                 (slot-ref d1 'sec)
                 (time-nanosecond now)
                 (+ (slot-ref d1 'yday) 1)
                 (slot-ref d1 'wday)))
         (let1 d0  (time-utc->date now)
           (list (date-year d0) 
                 (date-month d0)
                 (date-day d0)
                 (date-hour d0)
                 (date-minute d0)
                 (date-second d0)
                 (date-nanosecond d0)
                 (date-year-day d0)
                 (date-week-day d0)))
         ))
(test* "date conversion"
       '(#t #t #t #t)
       (let* ((t0 (make-time 'time-utc 0 0))
              (t1 (make-time 'time-utc 48375295 1022191954))
              (t2 (make-time 'time-tai 0 0))
              (t3 (make-time 'time-tai 48375295 1022191954)))
         (list (time=? t0 (date->time-utc (time-utc->date t0)))
               (time=? t1 (date->time-utc (time-utc->date t1)))
               (time=? t2 (date->time-tai (time-tai->date t2)))
               (time=? t3 (date->time-tai (time-tai->date t3)))
               )))

;; NB: in Gauche, the round-trip conversion from time -> julian-day -> time
;; can't be guaranteed because of the limited precision of julian-day
;; calcularion.   We round the nanosecond range.
(define (round-to-seconds time)
  (let ((n (time-nanosecond time)))
    (set! (ref time 'nanosecond) 0)
    (when (> n 500000000)
      (add-duration! time (make-time 'time-duration 0 1)))
    time))
  
(let1 t0 (make-time time-utc 0 1022191954)
  (test "julian day number, via time-utc"
        t0
        (lambda ()
          (round-to-seconds (julian-day->time-utc (time-utc->julian-day t0))))
        time=?))
(let1 jd 2453311.0
  (test "julian day number, via date"
        jd
        (lambda ()
          (date->julian-day (julian-day->date jd)))))
(let1 t0 (make-time time-utc 0 1022191954)
  (test "modified julian day number"
        t0
        (lambda ()
          (round-to-seconds
           (modified-julian-day->time-utc (time-utc->modified-julian-day t0))))
        time=?))


(test* "date->string"
       "2002/05/15 01:23:34.001234567 -1000 3"
       (date->string (make-date 1234567 34 23 1 15 5 2002 -36000)
                     "~Y/~m/~d ~H:~M:~S.~N ~z ~w"))

(test* "date->string"
       "02/05/ 1| 2|14"
       (date->string (make-date 1234567 4 3 14 1 5 2002 -36000)
                     "~y/~m/~e|~l|~k"))

(test* "string->date"
       '(2002 5 15 12 34 56 -36000)
       (let1 d (string->date "2002/5/15 12:34:56 (-1000)"
                             "~Y/~m/~d ~H:~M:~S (~z)")
         (map (lambda (s) (slot-ref d s))
              '(year month day hour minute second zone-offset))))

;; NB: this test will fail when locale-dependent date name is supported.
(test* "string->date"
       '(2002 11 2 7 14 11 32400)
       (let1 d (string->date "02/Nov/2002:07:14:11 +0900"
                             "~d~b~Y~H~M~S~z")
         (map (lambda (s) (slot-ref d s))
              '(year month day hour minute second zone-offset))))

(test-end)
