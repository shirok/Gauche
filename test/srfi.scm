;;
;; Test for SRFIs
;;

;; $Id: srfi.scm,v 1.6 2001-04-06 11:16:19 shiro Exp $

(add-load-path "../lib")
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
(test "take!" '(1 3) (lambda() (take! (circular-list 1 3 5) 8)))

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

(define x "abcde")
(test "(setter string-ref)" "abcQe"
      (lambda () (set! (string-ref x 3) #\Q) x))

(define (set-kar! p v) (set-car! p v))
(define kar (getter-with-setter (lambda (p) (car p)) set-kar!))

(define x (cons 1 2))
(test "(setter kar)" '(3 . 2) (lambda () (set! (kar x) 3) x))

;; see it works as the normal set!
(test "set!" '#f (lambda () (set! x #f) x))

(test-end)
