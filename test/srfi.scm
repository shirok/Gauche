;;
;; Test for SRFIs
;;

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
(test* "cond-expand" #t
       (cond-expand ((and srfi-2 srfi-1) (procedure? xcons)) (else #f)))
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
(test* "program (requires, no such feature)" (test-error)
       (eval '(program
               (requires no-such-feature))
             (make-module #f)))
(test* "program (files (empty))" '(1 . 2)
       (eval '(program
               (files)
               (code (cons 1 2)))
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
(test* "program (feature-cond w/o else)" (test-error)
       (eval '(program
               (feature-cond
                ((not srfi-1) (code (define x 5)))))
             (make-module #f)))

(sys-system "rm -rf test.o")

;;-----------------------------------------------------------------------
(test-section "srfi-14")
(use srfi-14)
(test-module 'srfi-14)

;; char-set writer
(let ()
  (define (char-set-printer-tester p)
    (test* "char-set-printer" (car p)
           (write-to-string (list->char-set (cdr p)))))
  (for-each char-set-printer-tester
            '(("#[ace]" #\a #\e #\c)
              ("#[ab]"  #\a #\b)
              ("#[a-c]" #\a #\b #\c)
              ("#[a-d]" #\a #\b #\c #\d)
              ("#[a-ce]" #\a #\b #\c #\e)
              ("#[acd]" #\a #\c #\d)
              ("#[ac-e]" #\a #\c #\d #\e)
              ("#[ac-e]" #\a #\c #\d #\e)
              ("#[\\-\\[\\]]" #\[ #\] #\-)
              ("#[\\^a]" #\^ #\a)
              ("#[!^]" #\^ #\!))))

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
;; srfi-16 case-lambda : moved to procedure.scm (builtin)

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

;;-----------------------------------------------------------------------
(test-section "srfi-42")

(use srfi-42)

;; tests took from examples of srfi-42 reference implementation.

(test* "do-ec" 1
       (let ((x 0)) (do-ec (set! x (+ x 1))) x))
(test* "do-ec" 10
       (let ((x 0)) (do-ec (:range i 10) (set! x (+ x 1))) x))
(test* "do-ec" 45
       (let ((x 0)) (do-ec (:range n 10) (:range k n) (set! x (+ x 1))) x))


(test* "list-ec" '(1)
       (list-ec 1))
(test* "list-ec" '(0 1 2 3)
       (list-ec (:range i 4) i))
(test* "list-ec" '((0 0) (1 0) (1 1) (2 0) (2 1) (2 2))
       (list-ec (:range n 3) (:range k (+ n 1)) (list n k)))
(test* "list-ec" '((0 0) (2 0) (2 1) (2 2) (4 0) (4 1) (4 2) (4 3) (4 4))
       (list-ec (:range n 5) (if (even? n)) (:range k (+ n 1)) (list n k)))
(test* "list-ec" '((1 0) (1 1) (3 0) (3 1) (3 2) (3 3))
       (list-ec (:range n 5) (not (even? n)) (:range k (+ n 1)) (list n k)))
(test* "list-ec" '((4 0) (4 1) (4 2) (4 3) (4 4))
       (list-ec (:range n 5) 
                (and (even? n) (> n 2)) 
                (:range k (+ n 1)) 
                (list n k)))
(test* "list-ec" '((0 0) (2 0) (2 1) (2 2) (4 0) (4 1) (4 2) (4 3) (4 4))
       (list-ec (:range n 5) 
                (or (even? n) (> n 3)) 
                (:range k (+ n 1)) 
                (list n k)))
(test* "list-ec" 10
       (let ((x 0)) (list-ec (:range n 10) (begin (set! x (+ x 1))) n) x))
(test* "list-ec" '(0 0 1)
       (list-ec (nested (:range n 3) (:range k n)) k))

(test* "append-ec" '(a b)     (append-ec '(a b)))
(test* "append-ec" '()        (append-ec (:range i 0) '(a b)))
(test* "append-ec" '(a b)     (append-ec (:range i 1) '(a b)))
(test* "append-ec" '(a b a b) (append-ec (:range i 2) '(a b)))

(test* "string-ec" "a"  (string-ec #\a))
(test* "string-ec" ""   (string-ec (:range i 0) #\a))
(test* "string-ec" "a"  (string-ec (:range i 1) #\a))
(test* "string-ec" "aa" (string-ec (:range i 2) #\a))

(test* "string-append-ec" "ab"   (string-append-ec "ab"))
(test* "string-append-ec" ""     (string-append-ec (:range i 0) "ab"))
(test* "string-append-ec" "ab"   (string-append-ec (:range i 1) "ab"))
(test* "string-append-ec" "abab" (string-append-ec (:range i 2) "ab"))

(test* "vector-ec" '#(1)   (vector-ec 1))
(test* "vector-ec" '#()    (vector-ec (:range i 0) i))
(test* "vector-ec" '#(0)   (vector-ec (:range i 1) i))
(test* "vector-ec" '#(0 1) (vector-ec (:range i 2) i))

(test* "vector-of-length-ec" '#(1)   (vector-of-length-ec 1 1))
(test* "vector-of-length-ec" '#()    (vector-of-length-ec 0 (:range i 0) i))
(test* "vector-of-length-ec" '#(0)   (vector-of-length-ec 1 (:range i 1) i))
(test* "vector-of-length-ec" '#(0 1) (vector-of-length-ec 2 (:range i 2) i))

(test* "sum-ec" 1 (sum-ec 1))
(test* "sum-ec" 0 (sum-ec (:range i 0) i))
(test* "sum-ec" 0 (sum-ec (:range i 1) i))
(test* "sum-ec" 1 (sum-ec (:range i 2) i))
(test* "sum-ec" 3 (sum-ec (:range i 3) i))

(test* "product-ec" 1 (product-ec 1))
(test* "product-ec" 1 (product-ec (:range i 1 0) i))
(test* "product-ec" 1 (product-ec (:range i 1 1) i))
(test* "product-ec" 1 (product-ec (:range i 1 2) i))
(test* "product-ec" 2 (product-ec (:range i 1 3) i))
(test* "product-ec" 6 (product-ec (:range i 1 4) i))

(test* "min-ec" 1 (min-ec 1))
(test* "min-ec" 0 (min-ec (:range i 1) i))
(test* "min-ec" 0 (min-ec (:range i 2) i))

(test* "max-ec" 1 (max-ec 1))
(test* "max-ec" 0 (max-ec (:range i 1) i))
(test* "max-ec" 1 (max-ec (:range i 2) i))

(test* "first-ec" 1  (first-ec #f 1))
(test* "first-ec" #f (first-ec #f (:range i 0) i))
(test* "first-ec" 0  (first-ec #f (:range i 1) i))
(test* "first-ec" 0  (first-ec #f (:range i 2) i))
(test* "first-ec" 0
       (let ((last-i -1))
         (first-ec #f (:range i 10) (begin (set! last-i i)) i)
         last-i))

(test* "last-ec" 1  (last-ec #f 1))
(test* "last-ec" #f (last-ec #f (:range i 0) i))
(test* "last-ec" 0  (last-ec #f (:range i 1) i))
(test* "last-ec" 1  (last-ec #f (:range i 2) i))

(test* "any-ec" #f (any?-ec #f))
(test* "any-ec" #t (any?-ec #t))
(test* "any-ec" #f (any?-ec (:range i 2 2) (even? i)))
(test* "any-ec" #t (any?-ec (:range i 2 3) (even? i)))

(test* "every-ec" #f (every?-ec #f))
(test* "every-ec" #t (every?-ec #t))
(test* "every-ec" #t (every?-ec (:range i 2 2) (even? i)))
(test* "every-ec" #t (every?-ec (:range i 2 3) (even? i)))
(test* "every-ec" #f (every?-ec (:range i 2 4) (even? i)))

(test* "fold-ec" 285
       (let ((sum-sqr (lambda (x result) (+ result (* x x)))))
         (fold-ec 0 (:range i 10) i sum-sqr)))
(test* "fold3-ec" 284
       (let ((minus-1 (lambda (x) (- x 1)))
             (sum-sqr (lambda (x result) (+ result (* x x)))))
         (fold3-ec (error "wrong") (:range i 10) i minus-1 sum-sqr)))
(test* "fold3-ec" 'infinity
       (fold3-ec 'infinity (:range i 0) i min min))

(test* ":list" '()
       (list-ec (:list x '()) x))
(test* ":list" '(1)
       (list-ec (:list x '(1)) x))
(test* ":list" '(1 2 3)
       (list-ec (:list x '(1 2 3)) x))
(test* ":list" '(1 2)
       (list-ec (:list x '(1) '(2)) x))
(test* ":list" '(1 2 3)
       (list-ec (:list x '(1) '(2) '(3)) x))

(test* ":string" '()
       (list-ec (:string c "") c))
(test* ":string" '(#\1)
       (list-ec (:string c "1") c))
(test* ":string" '(#\1 #\2 #\3)
       (list-ec (:string c "123") c))
(test* ":string" '(#\1 #\2)
       (list-ec (:string c "1" "2") c))
(test* ":string" '(#\1 #\2 #\3)
       (list-ec (:string c "1" "2" "3") c))

(test* ":vector" '()
       (list-ec (:vector x (vector)) x))
(test* ":vector" '(1)
       (list-ec (:vector x (vector 1)) x))
(test* ":vector" '(1 2 3)
       (list-ec (:vector x (vector 1 2 3)) x))
(test* ":vector" '(1 2)
       (list-ec (:vector x (vector 1) (vector 2)) x))
(test* ":vector" '(1 2 3)
       (list-ec (:vector x (vector 1) (vector 2) (vector 3)) x))

(test* ":range" '()
       (list-ec (:range x -2) x))
(test* ":range" '()
       (list-ec (:range x -1) x))
(test* ":range" '()
       (list-ec (:range x  0) x))
(test* ":range" '(0)
       (list-ec (:range x  1) x))
(test* ":range" '(0 1)
       (list-ec (:range x  2) x))
(test* ":range" '(0 1 2)
       (list-ec (:range x  0  3) x))
(test* ":range" '(1 2)
       (list-ec (:range x  1  3) x))
(test* ":range" '(-2)
       (list-ec (:range x -2 -1) x))
(test* ":range" '()
       (list-ec (:range x -2 -2) x))
(test* ":range" '(1 3)
       (list-ec (:range x 1 5  2) x))
(test* ":range" '(1 3 5)
       (list-ec (:range x 1 6  2) x))
(test* ":range" '(5 3)
       (list-ec (:range x 5 1 -2) x))
(test* ":range" '(6 4 2)
       (list-ec (:range x 6 1 -2) x))

(test* ":real-range" '(0. 1. 2.)
       (list-ec (:real-range x 0.0 3.0)     x))
(test* ":real-range" '(0. 1. 2.)
       (list-ec (:real-range x 0   3.0)     x))
(test* ":real-range" '(0. 1. 2.)
       (list-ec (:real-range x 0   3   1.0) x))

(test* ":char-range" "abcdefghijklmnopqrstuvwxyz"
       (string-ec (:char-range c #\a #\z) c) )

(sys-system "rm -f tmp1.o")

(test* ":port" (list-ec (:range n 10) n)
       (begin
         (with-output-to-file "tmp1.o"
           (lambda ()
             (do-ec (:range n 10) (begin (write n) (newline)))))
         (call-with-input-file "tmp1.o"
           (lambda (port) (list-ec (:port x port read) x)))))

(sys-system "rm -f tmp1.o")

(test* ":do" '(0 1 2 3) (list-ec (:do ((i 0)) (< i 4) ((+ i 1))) i))
(test* ":do" '(10 9 8 7)
       (list-ec 
        (:do (let ((x 'x)))
             ((i 0)) 
             (< i 4) 
             (let ((j (- 10 i))))
             #t
             ((+ i 1)))
        j))

(test* ":let" '(1) (list-ec (:let x 1) x))
(test* ":let" '(2) (list-ec (:let x 1) (:let y (+ x 1)) y))
(test* ":let" '(2) (list-ec (:let x 1) (:let x (+ x 1)) x))

(test* ":parallel" '((1 a) (2 b) (3 c))
       (list-ec (:parallel (:range i 1 10) (:list x '(a b c))) (list i x)))
(test* ":until" '(1 2 3 4 5)
       (list-ec (:until (:range i 1 10) (>= i 5)) i))
(test* ":while" '(1 2 3 4)
       (list-ec (:while (:list i '(1 2 3 4 5 6 7 8 9)) (< i 5)) i))
(test* ":until" '(1 2 3 4 5)
       (list-ec (:until (:list i '(1 2 3 4 5 6 7 8 9)) (>= i 5)) i))

(test* ":while and :parallel" '((1 1) (2 2) (3 3) (4 4))
       (list-ec (:while (:parallel (:range i 1 10)
                                   (:list j '(1 2 3 4 5 6 7 8 9)))
                        (< i 5))
                (list i j)))
(test* ":until and :parallel" '((1 1) (2 2) (3 3) (4 4) (5 5))
       (list-ec (:until (:parallel (:range i 1 10)
                                   (:list j '(1 2 3 4 5 6 7 8 9)))
                        (>= i 5))
                (list i j)))
(test* ":while stopping loop" 5
       (let ((n 0))
         (do-ec (:while (:range i 1 10) (begin (set! n (+ n 1)) (< i 5)))
                (if #f #f))
         n))
(test* ":until stopping loop" 5
       (let ((n 0))
         (do-ec (:until (:range i 1 10) (begin (set! n (+ n 1)) (>= i 5)))
                (if #f #f))
         n))
(test* ":while stopping loop" 5
       (let ((n 0))
         (do-ec (:while (:parallel (:range i 1 10)
                                   (:do () (begin (set! n (+ n 1)) #t) ()))
                        (< i 5))
                (if #f #f))
         n))
(test* ":until stopping loop" 5
       (let ((n 0))
         (do-ec (:until (:parallel (:range i 1 10)
                                   (:do () (begin (set! n (+ n 1)) #t) ()))
                        (>= i 5))
                (if #f #f))
         n))

(test* ": list" '(a b)     (list-ec (: c '(a b)) c))
(test* ": list" '(a b c d) (list-ec (: c '(a b) '(c d)) c))

(test* ": string" '(#\a #\b)         (list-ec (: c "ab") c))
(test* ": string" '(#\a #\b #\c #\d) (list-ec (: c "ab" "cd") c))

(test* ": vector" '(a b)   (list-ec (: c (vector 'a 'b)) c))
(test* ": vector" '(a b c) (list-ec (: c (vector 'a 'b) (vector 'c)) c))

(test* ": range" '()  (list-ec (: i 0) i))
(test* ": range" '(0) (list-ec (: i 1) i))
(test* ": range" '(0 1 2 3 4 5 6 7 8 9) (list-ec (: i 10) i))
(test* ": range" '(1) (list-ec (: i 1 2) i))
(test* ": range" '(1) (list-ec (: i 1 2 3) i))
(test* ": range" '(1 4 7) (list-ec (: i 1 9 3) i))

(test* ": real-range" '(0. 0.2 0.4 0.6 0.8) (list-ec (: i 0.0 1.0 0.2) i)
       (lambda (x y)
         (every (lambda (p q) (< (abs (- p q)) 1.0e-5)) x y)))
(test* ": char" '(#\a #\b #\c) (list-ec (: c #\a #\c) c))

(sys-system "rm -f tmp1.o")
(test* ": port" (list-ec (:range n 10) n)
       (begin
         (with-output-to-file "tmp1.o"
           (lambda ()
             (do-ec (:range n 10) (begin (write n) (newline)))))
         (call-with-input-file "tmp1.o"
           (lambda (port) (list-ec (: x port read) x)))))
             
(sys-system "rm -f tmp1.o")
(test* ": port" (list-ec (:range n 10) n)
       (begin
         (with-output-to-file "tmp1.o"
           (lambda ()
             (do-ec (:range n 10) (begin (write n) (newline)))))
         (call-with-input-file "tmp1.o"
           (lambda (port) (list-ec (: x port) x)))))       

(sys-system "rm -f tmp1.o")

(test* ":list index" '((a 0) (b 1))
       (list-ec (:list c (index i) '(a b)) (list c i)))
(test* ":string index" '((#\a 0))
       (list-ec (:string c (index i) "a") (list c i)))
(test* ":vector index" '((a 0))
       (list-ec (:vector c (index i) (vector 'a)) (list c i)))
(test* ":range index" '((0 0) (-1 1) (-2 2))
       (list-ec (:range i (index j) 0 -3 -1) (list i j)) )
(test* ":real-range index" '((0. 0) (0.2 1) (0.4 2) (0.6 3) (0.8 4))
       (list-ec (:real-range i (index j) 0 1 0.2) (list i j))
       (lambda (x y)
         (every (lambda (p q) (and (< (abs (- (car p) (car q))) 1e-5)
                                   (= (cadr p) (cadr q))))
                x y)))
(test* ":char-range index" '((#\a 0) (#\b 1) (#\c 2))
       (list-ec (:char-range c (index i) #\a #\c) (list c i)) )
(test* ": index" '((a 0) (b 1) (c 2) (d 3))
       (list-ec (: x (index i) '(a b c d)) (list x i)) )
(sys-system "rm -f tmp1.o")
(test* ": index port"
       '((0 0) (1 1) (2 2) (3 3) (4 4) (5 5) (6 6) (7 7) (8 8) (9 9))
       (begin
         (with-output-to-file "tmp1.o"
           (lambda ()
             (do-ec (:range n 10) (begin (write n) (newline)))))
         (call-with-input-file "tmp1.o"
           (lambda (port) (list-ec (: x (index i) port) (list x i))))))
(sys-system "rm -f tmp1.o")

(test* "example 1" '(0 1 4 9 16)
       (list-ec (: i 5) (* i i)))
(test* "example 2" '((1 0) (2 0) (2 1) (3 0) (3 1) (3 2))
       (list-ec (: n 1 4) (: i n) (list n i)))
(test* "example 3" '((#\a 0) (#\b 1) (#\c 2))
       (list-ec (: x (index i) "abc") (list x i)) )
(test* "example 4" '((#\a . 0) (#\b . 1))
       (list-ec (:string c (index i) "a" "b") (cons c i)) )

(test* ":range :range" '(0 0 1 0 1 2 0 1 2 3)
       (list-ec (:range x 5) (:range x x) x))
(test* ":list :" '(0 1 #\2 #\3 4)
       (list-ec (:list x '(2 "23" (4))) (: y x) y))
(test* ":parallel :integers :do" '((0 10) (1 9) (2 8) (3 7) (4 6))
       (list-ec (:parallel (:integers x) 
                           (:do ((i 10)) (< x i) ((- i 1))))
                (list x i)))

(let ()
  (define (factorial n) ; n * (n-1) * .. * 1 for n >= 0
    (product-ec (:range k 2 (+ n 1)) k) )

  (test* "factorial" 1   (factorial 0))
  (test* "factorial" 1   (factorial 1))
  (test* "factorial" 6   (factorial 3))
  (test* "factorial" 120 (factorial 5))
  )

(use srfi-4)
(let ()
  (define (eratosthenes n) ; primes in {2..n-1} for n >= 1
    (let ((p? (make-u8vector n 1)))
      (do-ec (:range k 2 n)
             (if (= (u8vector-ref p? k) 1))
             (:range i (* 2 k) n k)
             (u8vector-set! p? i 0))
      (list-ec (:range k 2 n) (if (= (u8vector-ref p? k) 1)) k) ))
  (test* "eratosthenes" '(2 3 5 7 11 13 17 19 23 29 31 37 41 43 47)
         (eratosthenes 50))
  (test* "eratosthenes" 9592 (length (eratosthenes 100000)))
  )

(let ()
  (define (pythagoras n) ; a, b, c s.t. 1 <= a <= b <= c <= n, a^2 + b^2 = c^2
    (list-ec 
     (:let sqr-n (* n n))
     (:range a 1 (+ n 1))
     (:let sqr-a (* a a))
     (:range b a (+ n 1)) 
     (:let sqr-c (+ sqr-a (* b b)))
     (if (<= sqr-c sqr-n))
     (:range c b (+ n 1))
     (if (= (* c c) sqr-c))
     (list a b c) ))
  (test* "pythagoras" '((3 4 5) (5 12 13) (6 8 10) (9 12 15))
         (pythagoras 15))
  (test* "pythagoras" 127 (length (pythagoras 200)))
  )

(let ()
  (define (qsort xs) ; stable
    (if (null? xs)
      '()
      (let ((pivot (car xs)) (xrest (cdr xs)))
        (append
         (qsort (list-ec (:list x xrest) (if (<  x pivot)) x))
         (list pivot)
         (qsort (list-ec (:list x xrest) (if (>= x pivot)) x)) ))))
  (test* "qsort" '(1 1 2 2 3 3 4 4 5 5) (qsort '(1 5 4 2 4 5 3 2 1 3)))
  )

(let ()
  (define (pi-BBP m) ; approx. of pi within 16^-m (Bailey-Borwein-Plouffe)
    (sum-ec 
     (:range n 0 (+ m 1))
     (:let n8 (* 8 n))
     (* (- (/ 4 (+ n8 1))
           (+ (/ 2 (+ n8 4))
              (/ 1 (+ n8 5))
              (/ 1 (+ n8 6))))
        (/ 1 (expt 16 n)) )))
  (test* "pi-BBP" (/ 40413742330349316707 12864093722915635200)
         (pi-BBP 5))
  )

(let ()
  (define (read-line port) ; next line (incl. #\newline) of port
    (let ((line
           (string-ec 
            (:until (:port c port read-char)
                    (char=? c #\newline) )
            c )))
      (if (string=? line "")
        (read-char port) ; eof-object
        line )))

  (define (read-lines filename) ; list of all lines
    (call-with-input-file filename
      (lambda (port)
        (list-ec (:port line port read-line) line) )))

  (sys-system "rm -f tmp1.o")
  (test* "read-lines" (list-ec (:char-range c #\0 #\9) (string c #\newline))
         (begin
           (with-output-to-file "tmp1.o"
             (lambda ()
               (do-ec (:range n 10) (begin (write n) (newline)))))
           (read-lines "tmp1.o")))
  )

(test-end)
