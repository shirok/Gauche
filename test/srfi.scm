;;
;; Test for SRFIs
;;

;; $Id: srfi.scm,v 1.33 2004-01-25 11:11:56 shirok Exp $

(use gauche.test)

(test-start "SRFIs")

;;-----------------------------------------------------------------------
(test-section "srfi-0")

(test* "cond-expand" 0
       (cond-expand (srfi-0 0) (else 1)))
(test* "cond-expand" 1
       (cond-expand (hogehoge 0) (else 1)))
(test* "cond-expand" 0
       (cond-expand ((and srfi-0 srfi-1) 0) (else 1)))
(test* "cond-expand" 0
       (cond-expand ((or hogehoge srfi-1) 0) (else 1)))
(test* "cond-expand" 0
       (cond-expand ((or srfi-1 hogehoge) 0) (else 1)))
(test* "cond-expand" 1
       (cond-expand ((or (not srfi-1) hogehoge) 0) (else 1)))
(test* "cond-expand" 0
       (cond-expand (gauche 0) (else 1)))
(test* "cond-expand" 0
       (cond-expand (scm -1) (gauche 0) (else 1)))

;;-----------------------------------------------------------------------
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

;;-----------------------------------------------------------------------
(test-section "srfi-2")
(use srfi-2)
(test-module 'srfi-2)

(define (srfi-2-look-up key alist)
  (and-let* ((x (assq key alist))) (cdr x)))
(test* "and-let*" 3
       (srfi-2-look-up 'c '((a . 1) (b . 2) (c . 3))))
(test* "and-let*" #f
       (srfi-2-look-up 'd '((a . 1) (b . 2) (c . 3))))
(test* "and-let*" 3
       (let ((x 3))
         (and-let* (((positive? x))
                    (y x))
           y)))
(test* "and-let*" #f
       (let ((x -3))
         (and-let* (((positive? x))
                    (y x))
           y)))

;;-----------------------------------------------------------------------
(test-section "srfi-5")
;; NB: srfi-5 replaces the binding of 'let'.  We don't want it to interfere
;; with the rest of file, so we segregate it within a dummy module.

(define-module srfi-5-test
  (use gauche.test)
  (use srfi-5)
  (test-module 'srfi-5)

  (test* "let - standard" 3
         (let ((x 1) (y 2))
           (let ()
             (+ x y))))

  (test* "let - standard" 1
         (let ((x 1) (y 2))
           (let ((y x) (x y))
             (- x y))))

  (test* "let - standard" 1
         (let ()
           (define x 1)
           (* x x)))

  (test* "let - standard, named" 55
         (let loop ((x 1) (sum 0))
           (if (> x 10) sum (loop (+ x 1) (+ sum x)))))

  (test* "let - signature style" 55
         (let (loop (x 1) (sum 0))
           (if (> x 10) sum (loop (+ x 1) (+ sum x)))))

  (test* "let - signature style" #t
         (let (loop)
           (procedure? loop)))

  (test* "let - rest binding" '(0 1 (2 3 4))
         (let ((x 0) (y 1) . (z 2 3 4)) (list x y z)))

  (test* "let - rest binding, named" '((2 3 4) 0 (1))
         (let loop ((x 0) (y 1) . (z 2 3 4))
           (if (list? x) (list x y z) (loop z x y))))
  )

;;-----------------------------------------------------------------------
(test-section "srfi-7")

;; NB: srfi-7 is a "meta-language".   The 'program' form doesn't need
;; to be evaluated within Scheme---an implementation can use a preprocessor
;; to produce an evaluatable form from the 'program' form.
;; Gauche directly expands it within the macro processor and evaluates it.
;;
;; These tests also relies on how Gauche compiles the empty begin form.
;; See the notes in lib/srfi-7.scm for the details.

(sys-system "rm -rf test.o")
(sys-system "mkdir test.o")
(with-output-to-file "test.o/a.scm"
  (lambda ()
    (write '(define x 3))))
(with-output-to-file "test.o/b.scm"
  (lambda ()
    (write '(define (y) (+ x x)))))

(test* "program (empty)" 'ok
       (begin (eval '(program) (make-module #f))
              'ok))

(test* "program (requires, code)" #t
       (eval '(program
               (requires srfi-1)
               (code (procedure? list-tabulate)))
             (make-module #f)))
(test* "program (requires, multiple code)" '(1 2 1)
       (eval '(program
               (requires srfi-1)
               (code (define foo (circular-list 1 2)))
               (requires srfi-2)
               (code (and-let* ((x (circular-list? foo)))
                       (take foo 3))))
             (make-module #f)))
(test* "program (requires, no such feature)" *test-error*
       (eval '(program
               (requires no-such-feature))
             (make-module #f)))
(test* "program (files (empty))" 3
       (eval '(program
               (files)
               (code (+ 1 2)))
             (make-module #f)))
(test* "program (files)" 6
       (eval '(program
               (files "./test.o/a")
               (files "./test.o/b")
               (code (y)))
             (make-module #f)))
(test* "program (files (multi))" 6
       (eval '(program
               (files "./test.o/a" "./test.o/b")
               (code (y)))
             (make-module #f)))
(test* "program (feature-cond)" 2
       (eval '(program
               (feature-cond
                ((and srfi-1 srfi-2) (code (define x 1)))
                (else (code (define x 2))))
               (code (+ x x)))
             (make-module #f)))
(test* "program (feature-cond)" 4
       (eval '(program
               (feature-cond
                ((and srfi-1 no-such-feature) (code (define x 1)))
                (else (code (define x 2))))
               (code (+ x x)))
             (make-module #f)))
(test* "program (feature-cond)" 6
       (eval '(program
               (feature-cond
                ((or srfi-1 no-such-feature) (code (define x 3)))
                (else (code (define x 2))))
               (code (+ x x)))
             (make-module #f)))
(test* "program (feature-cond w/o else)" *test-error*
       (eval '(program
               (feature-cond
                ((not srfi-1) (code (define x 5)))))
             (make-module #f)))

(sys-system "rm -rf test.o")

;;-----------------------------------------------------------------------
(test-section "srfi-9")
(use srfi-9)
(test-module 'srfi-9)

(define-record-type pare
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr))

(test* "pare kons" #t (pare? (kons 1 2)))
(test* "pare kons" #f (pare? (cons 1 2)))
(test* "pare kar" 1 (kar (kons 1 2)))
(test* "pare kdr" 2 (kdr (kons 1 2)))
(test* "pare set-kar!" 3 (let ((k (kons 1 2))) (set-kar! k 3) (kar k)))

(define-record-type xpare
  (xkons y x)
  xpare?
  (x kar)
  (y kdr))

(test* "xpare kons" '(1 . 2)
       (let ((k (xkons 2 1))) (cons (kar k) (kdr k))))

;;-----------------------------------------------------------------------
(test-section "srfi-10")
(use srfi-10)
(test-module 'srfi-10)

(test "read ctor 1a" '(1 2 #f "4 5")
      (lambda ()
        (define-reader-ctor 'list list)
        (with-input-from-string "#,(list 1 2 #f \"4 5\")" read)))
(test "read ctor 1b" 3
      (lambda ()
        (define-reader-ctor '+ +)
        (with-input-from-string "#,(+ 1 2)" read)))
(define-reader-ctor 'my-vector
  (lambda x (apply vector (cons 'my-vector x))))
(test* "read ctor 2a" '#(my-vector (my-vector 1 2))
       (with-input-from-string "#,(my-vector (my-vector 1 2))" read))
(test* "read ctor 2b" '#(my-vector #(my-vector 1 2))
       (with-input-from-string "#,(my-vector #,(my-vector 1 2))" read))

;;-----------------------------------------------------------------------
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
(test* "string-prefix-length-ci" 8
       (string-prefix-length-ci "cancaNCAM" "cancancan"))
(test* "string-suffix-length" 4
       (string-suffix-length "CanCan" "cankancan"))
(test* "string-suffix-length-ci" 1
       (string-suffix-length-ci "CanCan" "cankancan"))

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

;;-----------------------------------------------------------------------
(test-section "srfi-14")
(use srfi-14)
(test-module 'srfi-14)

;; Test samples taken from Olin Shivers' test suite,
;; http://srfi.schemers.org/srfi-14/srfi-14-tests.scm
;; TODO: This doesn't test characters beyond ASCII.  See char-set.euc.scm.
(define (vowel? c) (member c '(#\a #\e #\i #\o #\u)))

(test* "char-set?" #f (char-set? 5))
(test* "char-set?" #t (char-set? (char-set #\a #\e #\i #\o #\u)))
(test* "char-set=" #t (char-set=))
(test* "char-set=" #t (char-set= (char-set)))
(test* "char-set=" #t (char-set= (char-set #\a #\e #\i #\o #\u)
                                 (string->char-set "ioeauaiii")))
(test* "char-set=" #f (char-set= (char-set #\e #\i #\o #\u)
                                 (string->char-set "ioeauaiii")))
(test* "char-set<=" #t (char-set<=))
(test* "char-set<=" #t (char-set<= (char-set)))
(test* "char-set<=" #t (char-set<= (char-set #\a #\e #\i #\o #\u)
                                   (string->char-set "ioeauaiii")))
(test* "char-set<=" #t (char-set<= (char-set #\e #\i #\o #\u)
                                   (string->char-set "ioeauaiii")))

(test* "char-set-hash" #t
       (<= 0 (char-set-hash char-set:graphic 100) 99))
(test* "char-set-fold" #t
       (= 4 (char-set-fold (lambda (c i) (+ i 1)) 0
                           (char-set #\e #\i #\o #\u #\e #\e))))
(test* "char-set-unfold" #t
       (char-set= (string->char-set "eiaou2468013579999")
                  (char-set-unfold null? car cdr
                                   '(#\a #\e #\i #\o #\u #\u #\u)
                                   char-set:digit)))
(test* "char-set-unfold!" #t
       
       (char-set= (string->char-set "eiaou246801357999")
                  (char-set-unfold! null? car cdr '(#\a #\e #\i #\o #\u)
                                    (string->char-set "0123456789"))))
(test* "char-set-unfold!" #f
       (char-set= (string->char-set "eiaou246801357")
                  (char-set-unfold! null? car cdr '(#\a #\e #\i #\o #\u)
                                    (string->char-set "0123456789"))))
(test* "char-set-for-each" #t
       (let ((cs (string->char-set "0123456789")))
         (char-set-for-each (lambda (c) (set! cs (char-set-delete cs c)))
                            (string->char-set "02468000"))
         (char-set= cs (string->char-set "97531"))))
(test* "char-set-for-each" #t
       (not (let ((cs (string->char-set "0123456789")))
              (char-set-for-each (lambda (c) (set! cs (char-set-delete cs c)))
                                 (string->char-set "02468"))
              (char-set= cs (string->char-set "7531")))))
(test* "char-set-map" #t
       (char-set= (char-set-map char-upcase (string->char-set "aeiou"))
                  (string->char-set "IOUAEEEE")))
(test* "char-set-map" #f
       (char-set= (char-set-map char-upcase (string->char-set "aeiou"))
                  (string->char-set "OUAEEEE")))
(test* "char-set-copy" #t
       (char-set= (char-set-copy (string->char-set "aeiou"))
                  (string->char-set "aeiou")))
(test* "string->char-set" #t
       (char-set= (char-set #\x #\y) (string->char-set "xy")))
(test* "string->char-set" #t
       (not (char-set= (char-set #\x #\y #\z) (string->char-set "xy"))))
(test* "list->char-set" #t
       (char-set= (string->char-set "xy") (list->char-set '(#\x #\y))))
(test* "list->char-set" #f
       
       (char-set= (string->char-set "axy") (list->char-set '(#\x #\y))))
(test* "list->char-set" #t
       
       (char-set= (string->char-set "xy12345")
                  (list->char-set '(#\x #\y) (string->char-set "12345"))))
(test* "list->char-set" #f
       (char-set= (string->char-set "y12345")
                  (list->char-set '(#\x #\y) (string->char-set "12345"))))
(test* "list->char-set!" #t
       (char-set= (string->char-set "xy12345")
                  (list->char-set! '(#\x #\y) (string->char-set "12345"))))
(test* "list->char-set!" #f
       (char-set= (string->char-set "y12345")
                  (list->char-set! '(#\x #\y) (string->char-set "12345"))))
(test* "char-set-filter" #t
       (char-set= (string->char-set "aeiou12345")
                  (char-set-filter vowel? char-set:ascii
                                   (string->char-set "12345"))))
(test* "char-set-filter" #f
       (char-set= (string->char-set "aeou12345")
                  (char-set-filter vowel? char-set:ascii
                                   (string->char-set "12345"))))
(test* "char-set-filter!" #t
       (char-set= (string->char-set "aeiou12345")
                  (char-set-filter! vowel? char-set:ascii
                                    (string->char-set "12345"))))
(test* "char-set-filter!" #f
       (char-set= (string->char-set "aeou12345")
                  (char-set-filter! vowel? char-set:ascii
                                    (string->char-set "12345"))))
(test* "ucs-range->char-set" #t
       (char-set= (string->char-set "abcdef12345")
                  (ucs-range->char-set 97 103 #t
                                       (string->char-set "12345"))))
(test* "ucs-range->char-set" #f
       (char-set= (string->char-set "abcef12345")
                  (ucs-range->char-set 97 103 #t
                                       (string->char-set "12345"))))
(test* "ucs-range->char-set!" #t
       (char-set= (string->char-set "abcdef12345")
                  (ucs-range->char-set! 97 103 #t
                                        (string->char-set "12345"))))
(test* "ucs-range->char-set!" #f
       (char-set= (string->char-set "abcef12345")
                  (ucs-range->char-set! 97 103 #t
                                        (string->char-set "12345"))))
(test* "integer-range->char-set" #t
       (char-set= (string->char-set "abcdef12345")
                  (integer-range->char-set 97 103 #t
                                           (string->char-set "12345"))))
(test* "integer-range->char-set" #f
       (char-set= (string->char-set "abcef12345")
                  (integer-range->char-set 97 103 #t
                                           (string->char-set "12345"))))
(test* "integer-range->char-set!" #t
       (char-set= (string->char-set "abcdef12345")
                  (integer-range->char-set! 97 103 #t
                                            (string->char-set "12345"))))
(test* "integer-range->char-set!" #f
       (char-set= (string->char-set "abcef12345")
                  (integer-range->char-set! 97 103 #t
                                            (string->char-set "12345"))))

(test* "->char-set" #t
       (char-set= (->char-set #\x)
                  (->char-set "x")
                  (->char-set (char-set #\x))))
(test* "->char-set" #f
       (char-set= (->char-set #\x)
                  (->char-set "y")
                  (->char-set (char-set #\x))))
(test* "char-set-size" 10
       (char-set-size (char-set-intersection char-set:ascii char-set:digit)))
(test* "char-set-count" 5
       (char-set-count vowel? char-set:ascii))
(test* "char-set->list" #t
       (equal? '(#\x) (char-set->list (char-set #\x))))
(test* "char-set->list" #f
       (equal? '(#\X) (char-set->list (char-set #\x))))
(test* "char-set->string" #t
       (equal? "x" (char-set->string (char-set #\x))))
(test* "char-set->string" #f
       (equal? "X" (char-set->string (char-set #\x))))
(test* "char-set-contains?" #t
       (char-set-contains? (->char-set "xyz") #\x))
(test* "char-set-contains?" #f
       (char-set-contains? (->char-set "xyz") #\a))
(test* "char-set-every" #t
       (char-set-every char-lower-case? (->char-set "abcd")))
(test* "char-set-every" #f
       (char-set-every char-lower-case? (->char-set "abcD")))
(test* "char-set-any" #t
       (char-set-any char-lower-case? (->char-set "abcd")))
(test* "char-set-any" #f
       (char-set-any char-lower-case? (->char-set "ABCD")))
(test* "char-set iterators" #t
       (char-set= (->char-set "ABCD")
                  (let ((cs (->char-set "abcd")))
                    (let lp ((cur (char-set-cursor cs)) (ans '()))
                      (if (end-of-char-set? cur) (list->char-set ans)
                          (lp (char-set-cursor-next cs cur)
                              (cons (char-upcase (char-set-ref cs cur)) ans)))))))
(test* "char-set-adjoin" #t
       (char-set= (char-set-adjoin (->char-set "123") #\x #\a)
                  (->char-set "123xa")))
(test* "char-set-adjoin" #f
       (char-set= (char-set-adjoin (->char-set "123") #\x #\a)
                  (->char-set "123x")))
(test* "char-set-adjoin!" #t
       (char-set= (char-set-adjoin! (->char-set "123") #\x #\a)
                  (->char-set "123xa")))
(test* "char-set-adjoin!" #f
       (char-set= (char-set-adjoin! (->char-set "123") #\x #\a)
                  (->char-set "123x")))
(test* "char-set-delete" #t
       (char-set= (char-set-delete (->char-set "123") #\2 #\a #\2)
                  (->char-set "13")))
(test* "char-set-delete" #f
       (char-set= (char-set-delete (->char-set "123") #\2 #\a #\2)
                  (->char-set "13a")))
(test* "char-set-delete" #t
       (char-set= (char-set-adjoin (char-set-delete char-set:full #\;) #\;)
                  char-set:full))
(test* "char-set-delete!" #t
       (char-set= (char-set-delete! (->char-set "123") #\2 #\a #\2)
                  (->char-set "13")))
(test* "char-set-delete!" #f
       (char-set= (char-set-delete! (->char-set "123") #\2 #\a #\2)
                  (->char-set "13a")))
(test* "char-set-intersection" #t
       (char-set= (char-set-intersection char-set:hex-digit (char-set-complement char-set:digit))
                  (->char-set "abcdefABCDEF")))
(test* "char-set-intersection!" #t
       (char-set= (char-set-intersection! (char-set-complement! (->char-set "0123456789"))
                                          char-set:hex-digit)
                  (->char-set "abcdefABCDEF")))
(test* "char-set-union" #t
       (char-set= (char-set-union char-set:hex-digit
                                  (->char-set "abcdefghijkl"))
                  (->char-set "abcdefABCDEFghijkl0123456789")))
(test* "char-set-union!" #t
       (char-set= (char-set-union! (->char-set "abcdefghijkl")
                                   char-set:hex-digit)
                  (->char-set "abcdefABCDEFghijkl0123456789")))
(test* "char-set-difference" #t
       (char-set= (char-set-difference (->char-set "abcdefghijklmn")
                                       char-set:hex-digit)
                  (->char-set "ghijklmn")))
(test* "char-set-difference!" #t
       (char-set= (char-set-difference! (->char-set "abcdefghijklmn")
                                        char-set:hex-digit)
                  (->char-set "ghijklmn")))
(test* "char-set-xor" #t
       (char-set= (char-set-xor (->char-set "0123456789")
                                char-set:hex-digit)
                  (->char-set "abcdefABCDEF")))
(test* "char-set-xor!" #t
       (char-set= (char-set-xor! (->char-set "0123456789")
                                 char-set:hex-digit)
                  (->char-set "abcdefABCDEF")))
(test* "char-set-diff+intersection" #t
       (call-with-values (lambda ()
                           (char-set-diff+intersection char-set:hex-digit
                                                       char-set:letter))
         (lambda (d i)
           (and (char-set= d (->char-set "0123456789"))
                (char-set= i (->char-set "abcdefABCDEF"))))))
(test* "char-set-diff+intersection!" #t
       (call-with-values (lambda ()
                           (char-set-diff+intersection! (char-set-copy char-set:hex-digit)
                                                        (char-set-copy char-set:letter)))
         (lambda (d i)
           (and (char-set= d (->char-set "0123456789"))
                (char-set= i (->char-set "abcdefABCDEF"))))))

;;-----------------------------------------------------------------------
(test-section "srfi-16")

(test* "case-lambda (plus)" '(0 1 3 6 10)
       (let ()
         (define plus
           (case-lambda 
            (() 0)
            ((x) x)
            ((x y) (+ x y))
            ((x y z) (+ (+ x y) z))
            (args (apply + args))))
         (list (plus) (plus 1) (plus 1 2) (plus 1 2 3) (plus 1 2 3 4))))

(test* "case-lambda (mul2)" *test-error*
       (let ()
         (define mul2
           (case-lambda 
            (() 1)
            ((x) x)
            ((x y) (* x y))))
         (mul2 1 2 3)))

(test* "case-lambda (matching order)" '(1 (2))
       ((case-lambda
         ((x . y) (list x y))
         ((x y) (list x y)))
        1 2))

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

;; see if it works as the normal set!
(test "set!" '#f (lambda () (set! x #f) x))

;;-----------------------------------------------------------------------
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
;; calcularion.
'(let1 t0 (make-time time-utc 0 1022191954)
   (test "julian day number"
         t0
         (lambda ()
           (julian-day->time-utc (time-utc->julian-day t0)))
         time=?))
'(let1 t0 (make-time time-utc 0 1022191954)
   (test "modified julian day number"
         t0
         (lambda ()
           (modified-julian-day->time-utc (time-utc->modified-julian-day t0)))
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

;;-----------------------------------------------------------------------
(test-section "srfi-26")

(use srfi-26)
(test-module 'srfi-26)

;; The test cases are taken from the SRFI-26 test program by Sebastian Egner.
(test* "cut list" '() ((cut list)))
(test* "cut list <...>" '() ((cut list <...>)))
(test* "cut list 1" '(1) ((cut list 1)))
(test* "cut list <>" '(1) ((cut list <>) 1))
(test* "cut list <...>" '(1) ((cut list <...>) 1))
(test* "cut list 1 2" '(1 2) ((cut list 1 2)))
(test* "cut list 1 <>" '(1 2) ((cut list 1 <>) 2))
(test* "cut list 1 <...>" '(1 2) ((cut list 1 <...>) 2))
(test* "cut list 1 <...>" '(1 2 3 4) ((cut list 1 <...>) 2 3 4))
(test* "cut list 1 <> 3 <>" '(1 2 3 4) ((cut list 1 <> 3 <>) 2 4))
(test* "cut list 1 <> 3 <...>" '(1 2 3 4 5 6) ((cut list 1 <> 3 <...>) 2 4 5 6))
(test* "cut (eval order)" '(ok)
       (let* ((x 'wrong) (y (cut list x))) (set! x 'ok) (y)))
(test* "cut (eval order)" 2
       (let ((a 0))
         (map (cut + (begin (set! a (+ a 1)) a) <>)
              '(1 2))
         a))

(test* "cute list" '() ((cute list)))
(test* "cute list <...>" '() ((cute list <...>)))
(test* "cute list 1" '(1) ((cute list 1)))
(test* "cute list <>" '(1) ((cute list <>) 1))
(test* "cute list <...>" '(1) ((cute list <...>) 1))
(test* "cute list 1 2" '(1 2) ((cute list 1 2)))
(test* "cute list 1 <>" '(1 2) ((cute list 1 <>) 2))
(test* "cute list 1 <...>" '(1 2) ((cute list 1 <...>) 2))
(test* "cute list 1 <...>" '(1 2 3 4) ((cute list 1 <...>) 2 3 4))
(test* "cute list 1 <> 3 <>" '(1 2 3 4) ((cute list 1 <> 3 <>) 2 4))
(test* "cute list 1 <> 3 <...>" '(1 2 3 4 5 6) ((cute list 1 <> 3 <...>) 2 4 5 6))
(test* "cute (eval order)" '(ok)
       (let* ((x 'ok) (y (cute list x))) (set! x 'wrong) (y)))
(test* "cute (eval order)" 1
       (let ((a 0))
         (map (cute + (begin (set! a (+ a 1)) a) <>)
              '(1 2))
         a))

;;-----------------------------------------------------------------------
(test-section "srfi-29")

(define-module srfi-29-test
  (use srfi-29)
  (use gauche.test)
  (test-module 'srfi-29)

  ;; test taken from the example of srfi-29 document.
  (let ((translations
         '(((en) . ((time . "Its ~a, ~a.")
                    (goodbye . "Goodbye, ~a.")))
           ((fr) . ((time . "~1@*~a, c'est ~a.")
                    (goodbye . "Au revoir, ~a."))))))
    (for-each (lambda (translation)
                (let ((bundle-name (cons 'hello-program (car translation))))
                  (if (not (load-bundle! bundle-name))
                    (begin
                      (declare-bundle! bundle-name (cdr translation))
                      (store-bundle! bundle-name)))))
              translations))

  (define localized-message
    (lambda (message-name . args)
      (apply format (cons (localized-template 'hello-program
                                              message-name)
                          args))))

  (let ((myname "Fred"))
    (test* "localized-message (en)"
           '("Its 12:00, Fred."  "Goodbye, Fred.")
           (begin
             (current-language 'en)
             (current-country 'us)
             (list (localized-message 'time "12:00" myname)
                   (localized-message 'goodbye myname))))

    (test* "localized-message (fr)"
           '("Fred, c'est 12:00."  "Au revoir, Fred.")
           (begin
             (current-language 'fr)
             (current-country 'fr)
             (list (localized-message 'time "12:00" myname)
                   (localized-message 'goodbye myname))))
    ))

;;-----------------------------------------------------------------------
(test-section "srfi-30")

(test "srfi-30" 1
      (lambda () #|hohoho|# 1))

(test "srfi-30" '(1)
      (lambda ()
        '(#|hohoho|# 1)))

(test "srfi-30, multiline" '(1)
      (lambda ()
        '(
          #|
          hohoho
          |#
          1)))

(test "srfi-30, multiline" '(1)
      (lambda ()
        '(1
          #|
          hohoho
          |#
          )))

(test "srfi-30, multiline" '()
      (lambda ()
        '(
          #|
          hohoho
          |#
          )))

(test "srfi-30, nesting" '(1)
      (lambda ()
        '(#| nested #| nested |# nested |# 1)))

(test "srfi-30, nesting" '(1)
      (lambda ()
        '(#| nested #| nested; |# nested |# 1)))

(test "srfi-30, nesting" '(1)
      (lambda ()
        '(#|##|###|#|||#### ||#|||||#|#1)))

(test "srfi-30, intertwined" '(1)
      (lambda ()
        '(;; #| this is a single-line comment
          1 #|
          ;; this is a multi-line comment #|
          we're still in multi-line comment ;;
          #|#|multi-line comment can be nested many times|#|#
              ;; yes, this is still in multi-line comment |#
              finally, this closes the comment |#
                                        ;and this is in single-line comment
              )))

(test "srfi-30, dot syntax" '(1 . 1)
      (lambda ()
        '(1 . #|foo bar|#1)))

(test "srfi-30, quasiquote" '(1 #(2 3))
      (lambda ()
        (let ((x 1) (y 2))
          `(#|foo|# ,x #|foo|# #(#|,|# ,y ,(+ #|x|# x y #|y|#) #|foo|#)))))

;;-----------------------------------------------------------------------
(test-section "srfi-31")

;; srfi-31 is autoloaded

;; taken from srfi-31 document

(define f (rec (f n)
            ((rec (g k l)
               (if (zero? k)
                   l
                   (g (- k 1) (* k l)))) n 1)))

(test "srfi-31" 1 (lambda () (f 0)))
(test "srfi-31" 3628800 (lambda () (f 10)))

(test "srfi-31" "11111"
      (lambda ()
        (with-output-to-string
          (lambda ()
            (let loop ((i 0)
                       (stream (rec s (cons 1 (delay s)))))
              (when (< i 5)
                (display (car stream))
                (loop (+ i 1) (force (cdr stream)))))))))

;;-----------------------------------------------------------------------
(test-section "srfi-37")

(use srfi-37)

(define options
  (list (option '(#\l "long-display") #f #f
                (lambda (option name arg seed1 seed2)
                  (values (cons 'l seed1) seed2)))
        (option '(#\o "output-file") #t #f
                (lambda (option name arg seed1 seed2)
                  (values (acons 'o arg seed1) seed2)))
        (option '(#\d "debug") #f #t
                (lambda (option name arg seed1 seed2)
                  (values (acons 'd arg seed1) seed2)))
        (option '(#\b "batch") #f #f
                (lambda (option name arg seed1 seed2)
                  (values (cons 'b seed1) seed2)))
        (option '(#\i "interactive") #f #f
                (lambda (option name arg seed1 seed2)
                  (values (cons 'i seed1) seed2)))
        ))

(define (test-options . args)
  (receive (opts operands)
      (args-fold args options
                 (lambda (option name arg seed1 seed2) ;; unrecognized-proc
                   (values (acons '? name seed1) seed2))
                 (lambda (arg seed1 seed2)      ;; operand-proc
                   (values seed1 (cons arg seed2)))
                 '() '())
    (list (reverse opts) (reverse operands))))

(test* "srfi-37 (short)" '((i l b) ())
       (test-options "-ilb"))

(test* "srfi-37 (short, arg)" '((i (o . "foo") l (d . "8")) ())
       (test-options "-iofoo" "-l" "-d8"))

(test* "srfi-37 (short, arg)" '(((o . "foo") (d . #f)) ("bar"))
       (test-options "-o" "foo" "-d" "bar"))

(test* "srfi-37 (short, missing arg)" '(((o . "-d")) ("bar"))
       (test-options "-o" "-d" "bar"))

(test* "srfi-37 (short, missing arg)" '(((o . #f)) ())
       (test-options "-o"))

(test* "srfi-37 (long, arg)" '((l i (d . "v") b (d . #f)) ())
       (test-options "--long-display" "--interactive" "--debug=v" "-bd"))

(test* "srfi-37 (long, arg)" '((l i (d . "v") b (d . #f)) ())
       (test-options "--long-display" "--interactive" "--debug=v" "-bd"))

(test* "srfi-37 (operand)" '((i b) ("foo" "bar"))
       (test-options "-i" "foo" "-b" "bar"))

(test* "srfi-37 (operand)" '((i) ("foo" "-b" "bar"))
       (test-options "-i" "--" "foo" "-b" "bar"))

(test* "srfi-37 (operand)" '((i b) ("-" "foo" "bar"))
       (test-options "-i" "-" "foo" "-b" "bar"))

(test-end)
