;;;
;;;  Test scheme.*
;;;

(use gauche.test)

(test-start "extension scheme modules")

;;;
;;; scheme.list (SRFI-1)
;;;

(test-section "scheme.list")
(use scheme.list)
(test-module 'scheme.list)

(define Apply apply)                    ;avoid inline expansion

(test* "xcons" '(a . b) (xcons 'b 'a))
(test* "cons*" (test-error) (Apply cons* '())) ; use apply to avoid compile error during inlining
(test* "cons*" 'o  (cons* 'o))
(test* "cons*" '(1 2 3 . 4) (cons* 1 2 3 4))
(test* "make-list" 5 (length (make-list 5)))
(test* "make-list" '() (make-list 0))
(test* "make-list" '(m m m m m) (make-list 5 'm))
(test* "list-tabulate" '(0 1 2 3 4) (list-tabulate 5 values))
(test* "list-tabulate" '(#\0 #\1 #\2 #\3 #\4)
       (list-tabulate 5 (^i (integer->char (+ i 48)))))
(test* "list-tabulate" '(0 2 4 6) (list-tabulate 4 (^i (* i 2))))
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
(test* "list=" #t (list= eq? '(a b) '(a b) '(a b)))
(test* "list=" #f (list= eq? '(a b) '(a b) '(a b c)))
(test* "list=" #f (list= eq? '(a b) '(a b) '(a)))
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
       (call-with-values (^[] (car+cdr '(a b c))) list))
(test* "last" 'c (last '(a b c)))
(test* "last-pair" '(c) (last-pair '(a b c)))
(test* "length+" '(5 #f)
       (list (length+ '(1 2 3 4 5))
             (length+ (circular-list 1 2 3 4 5))))
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
       (call-with-values (^[] (unzip2 unzip-data)) list))
(test* "unzip3" '((1 a #\a "a") (2 b #\b "b") (3 c #\c "c"))
       (call-with-values (^[] (unzip3 unzip-data)) list))
(test* "unzip4" '((1 a #\a "a") (2 b #\b "b") (3 c #\c "c") (4 d #\d "d"))
       (call-with-values (^[] (unzip4 unzip-data)) list))
(test* "unzip5"
       '((1 a #\a "a") (2 b #\b "b") (3 c #\c "c") (4 d #\d "d") (5 e #\e "e"))
       (call-with-values (^[] (unzip5 unzip-data)) list))
(test* "pair-fold" '(5 4 3 2 1)
       (pair-fold (^[p t] (set-cdr! p t) p) '()
                  (list 1 2 3 4 5)))
(test* "pair-fold-right" '((a b c) (b c) (c))
       (pair-fold-right cons '() '(a b c)))
(test* "unfold" '(1 4 9 16 25 36 49 64 81 100)
       (unfold (^x (> x 10))
               (^x (* x x))
               (^x (+ x 1))
               1))
(test* "unfold-right" '(1 4 9 16 25 36 49 64 81 100)
       (unfold-right zero?
                     (^x (* x x))
                     (^x (- x 1))
                     10))
(test* "map" '(4 1 5 1)
       (map + '(3 1 4 1) (circular-list 1 0)))
(test* "for-each" '#(0 2 2 4 4)
       (rlet1 v (make-vector 5)
         (for-each (^[i n] (vector-set! v i (+ i n)))
                   '(0 1 2 3 4)
                   (circular-list 0 1))))
(test* "map!" '(4 1 5 1)
       (map! + (list 3 1 4 1) (circular-list 1 0)))
(test* "map-in-order"  '(4 1 5 1)
       (map-in-order + '(3 1 4 1) (circular-list 1 0)))
(test* "pair-for-each" '((c) (b c) (a b c))
       (rlet1 r '()
         (pair-for-each (^l (set! r (cons l r))) '(a b c))))
(test* "partition!" '((one four five) (2 3 6))
       (values->list (partition! symbol? (list 'one 2 3 'four 'five 6))))
(test* "take-while" '(2 18)
       (take-while even? '(2 18 3 10 22 9)))
(test* "take-while!" '(2 18)
       (take-while! even? (list 2 18 3 10 22 9)))
(test* "drop-while" '(3 10 22 9)
       (drop-while even? '(3 10 22 9)))
(test* "span" '((2 18) (3 10 22 9))
       (values->list (span even? '(2 18 3 10 22 9))))
(test* "break" '((2 18) (3 10 22 9))
       (values->list (break odd? '(2 18 3 10 22 9))))
(test* "span!" '((2 18) (3 10 22 9))
       (values->list (span! even? (list 2 18 3 10 22 9))))
(test* "break!" '((2 18) (3 10 22 9))
       (values->list (break! odd? (list 2 18 3 10 22 9))))
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
;; TODO: lset stuff

(test* "lset=" #t (lset= char-ci=? '(#\a #\B #\c) '(#\C #\A #\b)))
(test* "lset=" #f (lset= char-ci=? '(#\a #\B #\c #\d) '(#\C #\A #\b)))
(test* "lset=" #f (lset= char-ci=? '(#\a #\B #\c) '(#\C #\d #\A #\b)))

;; This tests the argument order of '=' predicate
(test* "lset= (order)" #t
       (lset= (^[x y] (assume (char<=? x y)) (char-ci=? x y))
              '(#\A #\B #\C) '(#\c #\b #\a)))

(test* "lset<=" #t (lset<= char-ci=? '(#\a #\B #\c) '(#\C #\A #\b)))
(test* "lset<=" #f (lset<= char-ci=? '(#\a #\B #\c #\d) '(#\C #\A #\b)))
(test* "lset<=" #t (lset<= char-ci=? '(#\a #\B #\c) '(#\C #\d #\A #\b)))


;;;
;;; scheme.vector (SRFI-133)
;;;

(test-section "scheme.vector")

(use scheme.vector)
(test-module 'scheme.vector)

(use compat.chibi-test)
(use srfi.11)

(chibi-test
 (include "../../test/include/vectors-test.scm"))

;;;
;;; scheme.charset (SRFI-14)
;;;

(test-section "scheme.charset")
(use scheme.charset)
(test-module 'scheme.charset)

;; built-in char-set features are tested in test/char.scm

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
       (= 4 (char-set-fold (^[c i] (+ i 1)) 0
                           (char-set #\e #\i #\o #\u #\e #\e))))
(test* "char-set-unfold" #t
       (char-set= (string->char-set "eiaou2468013579999")
                  (char-set-unfold null? car cdr
                                   '(#\a #\e #\i #\o #\u #\u #\u)
                                   char-set:ascii-digit)))
(test* "char-set-unfold (default)" #t
       (char-set= (string->char-set "aeiou")
                  (char-set-unfold null? car cdr
                                   '(#\a #\e #\i #\o #\u #\u #\u))))
(test* "char-set-unfold!" #t

       (char-set= (string->char-set "eiaou246801357999")
                  (char-set-unfold! null? car cdr '(#\a #\e #\i #\o #\u)
                                    (string->char-set "0123456789"))))
(test* "char-set-unfold!" #f
       (char-set= (string->char-set "eiaou246801357")
                  (char-set-unfold! null? car cdr '(#\a #\e #\i #\o #\u)
                                    (string->char-set "0123456789"))))
(test* "char-set-for-each" #t
       (let ([cs (string->char-set "0123456789")])
         (char-set-for-each (^c (set! cs (char-set-delete cs c)))
                            (string->char-set "02468000"))
         (char-set= cs (string->char-set "97531"))))
(test* "char-set-for-each" #t
       (not (let ([cs (string->char-set "0123456789")])
              (char-set-for-each (^c (set! cs (char-set-delete cs c)))
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
(test* "->char-set" #t
       (char-set= (->char-set "abc")
                  (->char-set '#(#\c #\a #\b #\a))))
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
(test* "char-set-complement (ascii, nohit)" #f
       (char-set-contains? (char-set-complement (char-set #\a)) #\a))
(test* "char-set-complement (ascii, hit)" #t
       (char-set-contains? (char-set-complement (char-set #\a)) #\b))
(test* "char-set-complement (~#x80, nohit)" #f
       (char-set-contains? (char-set-complement (char-set (integer->char #x80)))
                           (integer->char #x80)))
(test* "char-set-complement (~#x80, hit)" #t
       (char-set-contains? (char-set-complement (char-set (integer->char #x80)))
                           (integer->char #x81)))
(test* "char-set-complement (~~#x80, hit)" #t
       (char-set-contains? (char-set-complement
                            (char-set-complement
                             (char-set (integer->char #x80))))
                           (integer->char #x80)))
(test* "char-set-complement (~~#x80, nohit)" #f
       (char-set-contains? (char-set-complement
                            (char-set-complement
                             (char-set (integer->char #x80))))
                           (integer->char #x81)))
(test* "char-set-complement (~#x82, nohit)" #f
       (char-set-contains? (char-set-complement (char-set (integer->char #x82)))
                           (integer->char #x82)))
(test* "char-set-complement (~#x82, hit-upper)" #t
       (char-set-contains? (char-set-complement (char-set (integer->char #x82)))
                           (integer->char #x83)))
(test* "char-set-complement (~#x82, hit-lower)" #t
       (char-set-contains? (char-set-complement (char-set (integer->char #x82)))
                           (integer->char #x81)))
(test* "char-set-complement (~~#x82, hit)" #t
       (char-set-contains? (char-set-complement
                            (char-set-complement
                             (char-set (integer->char #x82))))
                           (integer->char #x82)))
(test* "char-set-complement (~~#x82, nohit-upper)" #f
       (char-set-contains? (char-set-complement
                            (char-set-complement
                             (char-set (integer->char #x82))))
                           (integer->char #x83)))
(test* "char-set-complement (~~#x82, nohit-lower)" #f
       (char-set-contains? (char-set-complement
                            (char-set-complement
                             (char-set (integer->char #x82))))
                           (integer->char #x81)))
(test* "char-set-complement (~empty)" #t
       (char-set-contains? (char-set-complement (char-set))
                           (integer->char #x80)))
(test* "char-set-complement (~~empty)" #f
       (char-set-contains? (char-set-complement (char-set-complement (char-set)))
                           (integer->char #x80)))
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
                  (let ([cs (->char-set "abcd")])
                    (let lp ([cur (char-set-cursor cs)] [ans '()])
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
(test* "char-set-delete!" #[\x81\x83\x84\x86]
       (char-set-delete! (->char-set '(#\x81 #\x82 #\x83 #\x84 #\x85 #\x86 #\x87))
                         #\x82 #\x87 #\x85)
       char-set=)
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
(test* "char-set-union!" #[\x81-\x89]
       (char-set-union! (->char-set '(#\x81 #\x83 #\x84 #\x86 #\x87))
                        (->char-set '(#\x82 #\x85 #\x86 #\x88 #\x89)))
       char-set=)
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
       (call-with-values (^[] (char-set-diff+intersection char-set:hex-digit
                                                          char-set:letter))
         (^[d i]
           (and (char-set= d (->char-set "0123456789"))
                (char-set= i (->char-set "abcdefABCDEF"))))))
(test* "char-set-diff+intersection!" #t
       (call-with-values (^[]
                           (char-set-diff+intersection! (char-set-copy char-set:hex-digit)
                                                        (char-set-copy char-set:letter)))
         (^[d i]
           (and (char-set= d (->char-set "0123456789"))
                (char-set= i (->char-set "abcdefABCDEF"))))))

(test* "char-set object-apply" '(#t #f)
       (list (#[a-z] #\a) (#[a-z] #\A)))

;; https://github.com/shirok/Gauche/pull/500
(test* "CharSetAdd large flag bug" #t
       (char-set= (char-set-intersection
                   (char-set-union (string->char-set "_")
                                   char-set:letter+digit)
                   char-set:full)
                  (char-set-intersection
                   (char-set-union char-set:letter+digit
                                   (string->char-set "_"))
                   char-set:full)))

;;;
;;; scheme.flonum (SRFI-144)
;;;

(test-section "scheme.flonum")
(use scheme.flonum)
(test-module 'scheme.flonum)

(define-module scheme-flonum-test

  (define-module srfi.144 (extend scheme.flonum))
  (define-module tests.scheme.test
    (use gauche.test)
    (export test test/one-of test/unspec-or-exn test/approx test/approx-1ulp)
    (define (f=? a b)
      (cond [(flonum? a)
             (and (flonum? b)
                  (cond [(nan? a) (nan? b)]
                        [(negative-zero? a) (negative-zero? b)]
                        [else (eqv? a b)]))]
            [(list? a)
             (and (list? b) (= (length a) (length b))
                  (every f=? a b))]
            [else (equal? a b)]))
    (define-syntax test
      (syntax-rules ()
        [(_ expr expected)
         (test* #"~'expr" expected expr f=?)]))
    (define-syntax test/one-of
      (syntax-rules ()
        [(_ expr expected-list)
         (test* #"~'expr" expected-list expr
                (^[elis r] (any (cut f=? <> r) elis)))]))
    (define-syntax test/unspec-or-exn
      (syntax-rules ()
        [(_ expr _)
         (test* #"~'expr (error)" (test-error) expr)]))
    (define-syntax test/approx-1ulp
      (syntax-rules ()
        [(_ expr expected)
         (test* #"~'expr (approx 1ulp)" expected expr
                (^[x y]
                  (if (list? x)
                    (and (list? y) (every approx=? x y))
                    (approx=? x y))))]))
    ;; NB: MinGW's bessel function jn has greater errors and we need
    ;; to relax approx check.
    (define rel-tolerance
      (cond-expand
       [gauche.os.windows 5e-7]
       [else 1e-9]))
    (define-syntax test/approx
      (syntax-rules ()
        [(_ expr expected)
         (test* #"~'expr (approx)" expected expr
                (cut approx=? <> <> rel-tolerance 1e-15))])))
  (use scheme.base)
  (include "../../test/include/srfi-144-tests.scm")
  (with-module tests.scheme.flonum
    (run-flonum-tests)))

(test-end)
