;;
;; Test for SRFIs
;;

(use gauche.test)
(use gauche.parameter)

(test-start "SRFIs")

;;-----------------------------------------------------------------------
(test-section "srfi-0")

(test* "cond-expand" 0
       (cond-expand [srfi-0 0] [else 1]))
(test* "cond-expand" 1
       (cond-expand [hogehoge 0] [else 1]))
(test* "cond-expand" 0
       (cond-expand [(and srfi-0 srfi-1) 0] [else 1]))
(test* "cond-expand" #t
       (cond-expand [(and srfi-2 srfi-1) (procedure? xcons)] [else #f]))
(test* "cond-expand" 0
       (cond-expand [(or hogehoge srfi-1) 0] [else 1]))
(test* "cond-expand" 0
       (cond-expand [(or srfi-1 hogehoge) 0] [else 1]))
(test* "cond-expand" 1
       (cond-expand [(or (not srfi-1) hogehoge) 0] [else 1]))
(test* "cond-expand" 0
       (cond-expand [gauche 0] [else 1]))
(test* "cond-expand" 0
       (cond-expand [scm -1] [gauche 0] [else 1]))
(test* "cond-expand (library)" 0
       (cond-expand [(library (gauche time)) 0] [else 1]))

;;-----------------------------------------------------------------------
(test-section "srfi-2")
(use srfi-2)
(test-module 'srfi-2)

(define (srfi-2-look-up key alist)
  (and-let* ([x (assq key alist)]) (cdr x)))
(test* "and-let*" 3
       (srfi-2-look-up 'c '((a . 1) (b . 2) (c . 3))))
(test* "and-let*" #f
       (srfi-2-look-up 'd '((a . 1) (b . 2) (c . 3))))
(test* "and-let*" 3
       (let ([x 3])
         (and-let* ([(positive? x)]
                    [y x])
           y)))
(test* "and-let*" #f
       (let ([x -3])
         (and-let* ([(positive? x)]
                    [y x])
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
         (let ([x 1] [y 2])
           (let ()
             (+ x y))))

  (test* "let - standard" 1
         (let ([x 1] [y 2])
           (let ([y x] [x y])
             (- x y))))

  (test* "let - standard" 1
         (let ()
           (define x 1)
           (* x x)))

  (test* "let - standard, named" 55
         (let loop ([x 1] [sum 0])
           (if (> x 10) sum (loop (+ x 1) (+ sum x)))))

  (test* "let - signature style" 55
         (let (loop [x 1] [sum 0])
           (if (> x 10) sum (loop (+ x 1) (+ sum x)))))

  (test* "let - signature style" #t
         (let (loop)
           (procedure? loop)))

  (test* "let - rest binding" '(0 1 (2 3 4))
         (let ([x 0] [y 1] . [z 2 3 4]) (list x y z)))

  (test* "let - rest binding, named" '((2 3 4) 0 (1))
         (let loop ([x 0] [y 1] . [z 2 3 4])
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
  (^[] (write '(define x 3))))
(with-output-to-file "test.o/b.scm"
  (^[] (write '(define (y) (+ x x)))))

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
               (code (and-let* ([x (circular-list? foo)])
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
                [(and srfi-1 srfi-2) (code (define x 1))]
                [else (code (define x 2))])
               (code (+ x x)))
             (make-module #f)))
(test* "program (feature-cond)" 4
       (eval '(program
               (feature-cond
                [(and srfi-1 no-such-feature) (code (define x 1))]
                [else (code (define x 2))])
               (code (+ x x)))
             (make-module #f)))
(test* "program (feature-cond)" 6
       (eval '(program
               (feature-cond
                [(or srfi-1 no-such-feature) (code (define x 3))]
                [else (code (define x 2))])
               (code (+ x x)))
             (make-module #f)))
(test* "program (feature-cond w/o else)" (test-error)
       (eval '(program
               (feature-cond
                [(not srfi-1) (code (define x 5))]))
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
       (= 4 (char-set-fold (^[c i] (+ i 1)) 0
                           (char-set #\e #\i #\o #\u #\e #\e))))
(test* "char-set-unfold" #t
       (char-set= (string->char-set "eiaou2468013579999")
                  (char-set-unfold null? car cdr
                                   '(#\a #\e #\i #\o #\u #\u #\u)
                                   char-set:digit)))
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
(cond-expand
 ;; Only test in utf-8, for the literal notation is interpreted differently
 ;; in other encodings.
 [gauche.ces-utf-8
  (test* "char-set-delete!" #[\x81\x83\x84\x86]
         (char-set-delete! (->char-set '(#\x81 #\x82 #\x83 #\x84 #\x85 #\x86 #\x87))
                           #\x82 #\x87 #\x85)
         char-set=)]
 [else])
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
(cond-expand
 ;; Only test in utf-8, for the literal notation is interpreted differently
 ;; in other encodings.
 [gauche.ces.utf-8
  (test* "char-set-union!" #[\x81-\x89]
         (char-set-union! (->char-set '(#\x81 #\x83 #\x84 #\x86 #\x87))
                          (->char-set '(#\x82 #\x85 #\x86 #\x88 #\x89)))
         char-set=)]
 [else])
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

;;-----------------------------------------------------------------------
;; srfi-16 case-lambda : moved to procedure.scm (builtin)

;;-----------------------------------------------------------------------
(test-section "srfi-17")

(define x (cons 1 2))
(test "(setter car)" '((3 3) . 2)
      (^[] (set! (car x) (list 3 3)) x))
(test "(setter cdr)" '((3 3) 4 5)
      (^[] (set! (cdr x) (list 4 5)) x))
(test "(setter caar)" '(((8 9) 3) 4 5)
      (^[] (set! (caar x) (list 8 9)) x))
(test "(setter cadr)" '(((8 9) 3) (7 6) 5)
      (^[] (set! (cadr x) (list 7 6)) x))
(test "(setter cdar)" '(((8 9) 4 5) (7 6) 5)
      (^[] (set! (cdar x) (list 4 5)) x))
(test "(setter cddr)" '(((8 9) 4 5) (7 6) 11 12)
      (^[] (set! (cddr x) (list 11 12)) x))
(test "(setter caaar)" '((((13 14) 9) 4 5) (7 6) 11 12)
      (^[] (set! (caaar x) (list 13 14)) x))
(test "(setter caadr)" '((((13 14) 9) 4 5) ((0 1) 6) 11 12)
      (^[] (set! (caadr x) (list 0 1)) x))
(test "(setter cadar)" '((((13 14) 9) (2 3) 5) ((0 1) 6) 11 12)
      (^[] (set! (cadar x) (list 2 3)) x))
(test "(setter caddr)" '((((13 14) 9) (2 3) 5) ((0 1) 6) (4 5) 12)
      (^[] (set! (caddr x) (list 4 5)) x))
(test "(setter cdaar)" '((((13 14) 5 6) (2 3) 5) ((0 1) 6) (4 5) 12)
      (^[] (set! (cdaar x) (list 5 6)) x))
(test "(setter cdadr)" '((((13 14) 5 6) (2 3) 5) ((0 1) 7 8) (4 5) 12)
      (^[] (set! (cdadr x) (list 7 8)) x))
(test "(setter cddar)" '((((13 14) 5 6) (2 3) 9 10) ((0 1) 7 8) (4 5) 12)
      (^[] (set! (cddar x) (list 9 10)) x))
(test "(setter cdddr)" '((((13 14) 5 6) (2 3) 9 10) ((0 1) 7 8) (4 5) -1 -2)
      (^[] (set! (cdddr x) (list -1 -2)) x))
(test "(setter caaaar)" '(((((1 3) 14) 5 6) (2 3) 9 10) ((0 1) 7 8) (4 5) -1 -2)
      (^[] (set! (caaaar x) (list 1 3)) x))
(test "(setter caaadr)" '(((((1 3) 14) 5 6) (2 3) 9 10) (((2 3) 1) 7 8) (4 5) -1 -2)
      (^[] (set! (caaadr x) (list 2 3)) x))
(test "(setter caadar)" '(((((1 3) 14) 5 6) ((0 1) 3) 9 10) (((2 3) 1) 7 8) (4 5) -1 -2)
      (^[] (set! (caadar x) (list 0 1)) x))
(test "(setter caaddr)" '(((((1 3) 14) 5 6) ((0 1) 3) 9 10) (((2 3) 1) 7 8) ((0 1) 5) -1 -2)
      (^[] (set! (caaddr x) (list 0 1)) x))
(test "(setter cadaar)" '(((((1 3) 14) (0 1) 6) ((0 1) 3) 9 10) (((2 3) 1) 7 8) ((0 1) 5) -1 -2)
      (^[] (set! (cadaar x) (list 0 1)) x))
(test "(setter cadadr)" '(((((1 3) 14) (0 1) 6) ((0 1) 3) 9 10) (((2 3) 1) (0 1) 8) ((0 1) 5) -1 -2)
      (^[] (set! (cadadr x) (list 0 1)) x))
(test "(setter caddar)" '(((((1 3) 14) (0 1) 6) ((0 1) 3) (0 1) 10) (((2 3) 1) (0 1) 8) ((0 1) 5) -1 -2)
      (^[] (set! (caddar x) (list 0 1)) x))
(test "(setter cadddr)" '(((((1 3) 14) (0 1) 6) ((0 1) 3) (0 1) 10) (((2 3) 1) (0 1) 8) ((0 1) 5) (0 1) -2)
      (^[] (set! (cadddr x) (list 0 1)) x))
(test "(setter cdaaar)" '(((((1 3) 0 1) (0 1) 6) ((0 1) 3) (0 1) 10) (((2 3) 1) (0 1) 8) ((0 1) 5) (0 1) -2)
      (^[] (set! (cdaaar x) (list 0 1)) x))
(test "(setter cdaadr)" '(((((1 3) 0 1) (0 1) 6) ((0 1) 3) (0 1) 10) (((2 3) 0 1) (0 1) 8) ((0 1) 5) (0 1) -2)
      (^[] (set! (cdaadr x) (list 0 1)) x))
(test "(setter cdadar)" '(((((1 3) 0 1) (0 1) 6) ((0 1) 0 1) (0 1) 10) (((2 3) 0 1) (0 1) 8) ((0 1) 5) (0 1) -2)
      (^[] (set! (cdadar x) (list 0 1)) x))
(test "(setter cdaddr)" '(((((1 3) 0 1) (0 1) 6) ((0 1) 0 1) (0 1) 10) (((2 3) 0 1) (0 1) 8) ((0 1) 0 1) (0 1) -2)
      (^[] (set! (cdaddr x) (list 0 1)) x))
(test "(setter cddaar)" '(((((1 3) 0 1) (0 1) 0 1) ((0 1) 0 1) (0 1) 10) (((2 3) 0 1) (0 1) 8) ((0 1) 0 1) (0 1) -2)
      (^[] (set! (cddaar x) (list 0 1)) x))
(test "(setter cddadr)" '(((((1 3) 0 1) (0 1) 0 1) ((0 1) 0 1) (0 1) 10) (((2 3) 0 1) (0 1) 0 1) ((0 1) 0 1) (0 1) -2)
      (^[] (set! (cddadr x) (list 0 1)) x))
(test "(setter cdddar)" '(((((1 3) 0 1) (0 1) 0 1) ((0 1) 0 1) (0 1) 0 1) (((2 3) 0 1) (0 1) 0 1) ((0 1) 0 1) (0 1) -2)
      (^[] (set! (cdddar x) (list 0 1)) x))
(test "(setter cddddr)" '(((((1 3) 0 1) (0 1) 0 1) ((0 1) 0 1) (0 1) 0 1) (((2 3) 0 1) (0 1) 0 1) ((0 1) 0 1) (0 1) 0 1)
      (^[] (set! (cddddr x) (list 0 1)) x))

(define x '#(1 2 3 4 5))
(test "(setter vector-ref)" '#(1 2 3 #f 5)
      (^[] (set! (vector-ref x 3) #f) x))

(define x (string-copy "abcde"))
(test "(setter string-ref)" "abcQe"
      (^[] (set! (string-ref x 3) #\Q) x))

(define (set-kar! p v) (set-car! p v))
(define kar (getter-with-setter (^p (car p)) set-kar!))

(define x (cons 1 2))
(test "(setter kar)" '(3 . 2) (^[] (set! (kar x) 3) x))

;; see if it works as the normal set!
(test "set!" '#f (^[] (set! x #f) x))

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
       (let* ([x 'wrong] [y (cut list x)]) (set! x 'ok) (y)))
(test* "cut (eval order)" 2
       (let ([a 0])
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
       (let* ([x 'ok] [y (cute list x)]) (set! x 'wrong) (y)))
(test* "cute (eval order)" 1
       (let ([a 0])
         (map (cute + (begin (set! a (+ a 1)) a) <>)
              '(1 2))
         a))

;;-----------------------------------------------------------------------
;; srfi-27 depends on mt-random, and will be tested in ext/mt-random/test.scm

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
    (for-each (^[translation]
                (let ([bundle-name (cons 'hello-program (car translation))])
                  (if (not (load-bundle! bundle-name))
                    (begin
                      (declare-bundle! bundle-name (cdr translation))
                      (store-bundle! bundle-name)))))
              translations))

  (define localized-message
    (^[message-name . args]
      (apply format (cons (localized-template 'hello-program
                                              message-name)
                          args))))

  (let ([myname "Fred"])
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
      (^[] #|hohoho|# 1))

(test "srfi-30" '(1)
      (^[] '(#|hohoho|# 1)))

(test "srfi-30, multiline" '(1)
      (^[]
        '(
          #|
          hohoho
          |#
          1)))

(test "srfi-30, multiline" '(1)
      (^[]
        '(1
          #|
          hohoho
          |#
          )))

(test "srfi-30, multiline" '()
      (^[]
        '(
          #|
          hohoho
          |#
          )))

(test "srfi-30, nesting" '(1)
      (^[]
        '(#| nested #| nested |# nested |# 1)))

(test "srfi-30, nesting" '(1)
      (^[]
        '(#| nested #| nested; |# nested |# 1)))

(test "srfi-30, nesting" '(1)
      (^[]
        '(#|##|###|#|||#### ||#|||||#|#1)))

(test "srfi-30, intertwined" '(1)
      (^[]
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
      (^[]
        '(1 . #|foo bar|#1)))

(test "srfi-30, quasiquote" '(1 #(2 3))
      (^[]
        (let ([x 1] [y 2])
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

(test "srfi-31" 1 (^[] (f 0)))
(test "srfi-31" 3628800 (^[] (f 10)))

(test "srfi-31" "11111"
      (^[] (with-output-to-string
             (^[] (let loop ([i 0]
                             [stream (rec s (cons 1 (delay s)))])
                    (when (< i 5)
                      (display (car stream))
                      (loop (+ i 1) (force (cdr stream)))))))))

;;-----------------------------------------------------------------------
(test-section "srfi-37")

(use srfi-37)

(define options
  (list (option '(#\l "long-display") #f #f
                (^[option name arg seed1 seed2]
                  (values (cons 'l seed1) seed2)))
        (option '(#\o "output-file") #t #f
                (^[option name arg seed1 seed2]
                  (values (acons 'o arg seed1) seed2)))
        (option '(#\d "debug") #f #t
                (^[option name arg seed1 seed2]
                  (values (acons 'd arg seed1) seed2)))
        (option '(#\b "batch") #f #f
                (^[option name arg seed1 seed2]
                  (values (cons 'b seed1) seed2)))
        (option '(#\i "interactive") #f #f
                (^[option name arg seed1 seed2]
                  (values (cons 'i seed1) seed2)))
        ))

(define (test-options . args)
  (receive (opts operands)
      (args-fold args options
                 (^[option name arg seed1 seed2] ;; unrecognized-proc
                   (values (acons '? name seed1) seed2))
                 (^[arg seed1 seed2]      ;; operand-proc
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
       (let ([x 0]) (do-ec (set! x (+ x 1))) x))
(test* "do-ec" 10
       (let ([x 0]) (do-ec (:range i 10) (set! x (+ x 1))) x))
(test* "do-ec" 45
       (let ([x 0]) (do-ec (:range n 10) (:range k n) (set! x (+ x 1))) x))


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
       (let ([sum-sqr (^[x result] (+ result (* x x)))])
         (fold-ec 0 (:range i 10) i sum-sqr)))
(test* "fold3-ec" 284
       (let ([minus-1 (^[x] (- x 1))]
             [sum-sqr (^[x result] (+ result (* x x)))])
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
           (^[] (do-ec (:range n 10) (begin (write n) (newline)))))
         (call-with-input-file "tmp1.o"
           (^[port] (list-ec (:port x port read) x)))))

(test* ":generator" (list-ec (:range n 10) (cons (- 9 n) n))
       ;; we can't load gauche.generator yet
       (letrec ([k 10]
                [g (^[] (if (zero? k) (eof-object) (begin (dec! k) k)))])
         (list-ec (:generator v (index i) g) (cons v i))))

(test* ":generator" (list-ec (:range n 10) (cons (- 9 n) n))
       ;; we can't load gauche.generator yet
       (letrec ([k 10]
                [g (^[] (if (zero? k) (eof-object) (begin (dec! k) k)))])
         (list-ec (: v (index i) g) (cons v i))))

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
       (rlet1 n 0
         (do-ec (:while (:range i 1 10) (begin (set! n (+ n 1)) (< i 5)))
                (if #f #f))))
(test* ":until stopping loop" 5
       (rlet1 n 0
         (do-ec (:until (:range i 1 10) (begin (set! n (+ n 1)) (>= i 5)))
                (if #f #f))))
(test* ":while stopping loop" 5
       (rlet1 n 0
         (do-ec (:while (:parallel (:range i 1 10)
                                   (:do () (begin (set! n (+ n 1)) #t) ()))
                        (< i 5))
                (if #f #f))))
(test* ":until stopping loop" 5
       (rlet1 n 0
         (do-ec (:until (:parallel (:range i 1 10)
                                   (:do () (begin (set! n (+ n 1)) #t) ()))
                        (>= i 5))
                (if #f #f))))

;; Those tests manifest the bug in the original reference implementation
(test* ":while loop boundary condition" '(1)
       (list-ec (:while (:list x '(1 2)) (= x 1)) x))
(test* ":while loop off-by-one error" '(1)
       (list-ec (:while (:list x '(1)) #t) x))
(test* ":while loop off-by-one error on vector" '(1 2 3 4 5)
       (list-ec (:while (:vector x (index i) '#(1 2 3 4 5)) (< x 10)) x))


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
       (^[x y]
         (every (^[p q] (< (abs (- p q)) 1.0e-5)) x y)))
(test* ": char" '(#\a #\b #\c) (list-ec (: c #\a #\c) c))

(sys-system "rm -f tmp1.o")
(test* ": port" (list-ec (:range n 10) n)
       (begin
         (with-output-to-file "tmp1.o"
           (^[]
             (do-ec (:range n 10) (begin (write n) (newline)))))
         (call-with-input-file "tmp1.o"
           (^[port] (list-ec (: x port read) x)))))
             
(sys-system "rm -f tmp1.o")
(test* ": port" (list-ec (:range n 10) n)
       (begin
         (with-output-to-file "tmp1.o"
           (^[]
             (do-ec (:range n 10) (begin (write n) (newline)))))
         (call-with-input-file "tmp1.o"
           (^[port] (list-ec (: x port) x)))))       

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
       (^[x y]
         (every (^[p q] (and (< (abs (- (car p) (car q))) 1e-5)
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
           (^[]
             (do-ec (:range n 10) (begin (write n) (newline)))))
         (call-with-input-file "tmp1.o"
           (^[port] (list-ec (: x (index i) port) (list x i))))))
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
    (let ([p? (make-u8vector n 1)])
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
    (let ([line
           (string-ec 
            (:until (:port c port read-char)
                    (char=? c #\newline) )
            c )])
      (if (string=? line "")
        (read-char port) ; eof-object
        line )))

  (define (read-lines filename) ; list of all lines
    (call-with-input-file filename
      (^[port]
        (list-ec (:port line port read-line) line) )))

  (sys-system "rm -f tmp1.o")
  (test* "read-lines" (list-ec (:char-range c #\0 #\9) (string c #\newline))
         (begin
           (with-output-to-file "tmp1.o"
             (^[]
               (do-ec (:range n 10) (begin (write n) (newline)))))
           (read-lines "tmp1.o")))
  )

;;-----------------------------------------------------------------------
(test-section "srfi-60")

(use srfi-60)
(test-module 'srfi-60)

;; Most procedures are builtin and tested in test/numbers.scm.

(test* "bitwise-if" #b01100110
       (bitwise-if #b10101100 #b00110101 #b11001010))

(for-each (^(n r) (test* (format "log2-binary-factors(~a)" n) r
                         (log2-binary-factors n)))
          '(0  1  2  3  4  5  6  7  8  9  10  11  12  13  14  15  16
              -1 -2 -3 -4 -5 -6 -7 -8 -9 -10 -11 -12 -13 -14 -15 -16)
          '(-1 0  1  0  2  0  1  0  3  0   1   0   2   0   1   0   4
               0  1  0  2  0  1  0  3  0   1   0   2   0   1   0   4))

(define (test-rotate-bit-field n c s e r)
  (test* (format "rotate-bit-field(~s,~a,~a,~a)" (number->string n 2) c s e) r
         (number->string (rotate-bit-field n c s e) 2)))
(test-rotate-bit-field #b0100 3 4 0 "100") ; trivial path
(test-rotate-bit-field #b0100 3 0 4 "10")
(test-rotate-bit-field #b0100 -1 0 4 "10")
(test-rotate-bit-field #b0100 10 0 4 "1")
(test-rotate-bit-field #b110100100010000 -1 5 9 "110100010010000")
(test-rotate-bit-field #b110100100010000 1 5 9 "110100000110000")

(define (test-reverse-bit-field n s e r)
  (test* (format "reverse-bit-field(~s,~a,~a)" (number->string n 2) s e) r
         (number->string (reverse-bit-field n s e) 2)))
(test-reverse-bit-field #xa7 8 0 "10100111")
(test-reverse-bit-field #xa7 0 8 "11100101")
(test-reverse-bit-field #xa7 1 5 "10111001")

(test* "integer->list" '(#t #f #f #t) (integer->list 9))
(test* "integer->list" '(#f #f #t #f #f #t) (integer->list 9 6))
(test* "list->integer" 9 (list->integer '(#t #f #f #t)))
(test* "list->integer" 9 (list->integer '(#f #f #t #f #f #t)))
;; tests bignum path
(test* "list->integer" (+ (expt 2 63) (expt 2 62) (expt 2 31) (expt 2 30) 1)
       (list->integer '(#t #t #f #f #f #f #f #f
                        #f #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f #f
                        #t #t #f #f #f #f #f #f
                        #f #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f #f
                        #f #f #f #f #f #f #f #t)))

(test* "booleans->integer" 9 (booleans->integer #f #f #t #f #f #t))

;;-----------------------------------------------------------------------
(test-section "srfi-69")
(use srfi-69)
(test-module 'srfi-69)

;; These test is contributed by Takashi Kato @tk_riple
(letrec-syntax
    ([test-error
      (syntax-rules ()
        ((_ expr)
         (test* 'expr #t (guard (e (else #t)) expr #f))))]
     [test-equal
      (syntax-rules ()
        ((_ expect expr)
         (test* 'expr expect expr)))]
     [test-assert
      (syntax-rules ()
        ((_ expr)
         (test-assert 'expr expr))
        ((_ name expr)
         (test* name #t expr (lambda (a r) (and a r)))))]
     [test-not
      (syntax-rules ()
        ((_ expr)
         (test-not 'expr expr))
        ((_ name expr)
         (test-assert name (not expr))))]
     [test-lset-eq?
      (syntax-rules ()
        ((test-lset= a b)
         (test-assert 'a (lset= eq? a b))))]
     [test-lset-equal?
      (syntax-rules ()
        ((test-lset-equal? a b)
         (test-assert 'a (lset= equal? a b))))])

  (let ((ht (make-hash-table eq?)))
    ;; 3 initial elements

    (test-equal 0 (hash-table-size ht))
    (hash-table-set! ht 'cat 'black)
    (hash-table-set! ht 'dog 'white)
    (hash-table-set! ht 'elephant 'pink)

    (test-equal 3 (hash-table-size ht))
    (test-assert (hash-table-exists? ht 'dog))
    (test-assert (hash-table-exists? ht 'cat))
    (test-assert (hash-table-exists? ht 'elephant))
    (test-not (hash-table-exists? ht 'goose))
    (test-equal 'white (hash-table-ref ht 'dog))
    (test-equal 'black (hash-table-ref ht 'cat))
    (test-equal 'pink (hash-table-ref ht 'elephant))
    (test-error (hash-table-ref ht 'goose))
    (test-equal 'grey (hash-table-ref ht 'goose (lambda () 'grey)))
    (test-equal 'grey (hash-table-ref/default ht 'goose 'grey))
    (test-lset-eq? '(cat dog elephant) (hash-table-keys ht))
    (test-lset-eq? '(black white pink) (hash-table-values ht))
    (test-lset-equal? '((cat . black) (dog . white) (elephant . pink))
                      (hash-table->alist ht))

    ;; remove an element
    (hash-table-delete! ht 'dog)
    (test-equal 2 (hash-table-size ht))
    (test-not (hash-table-exists? ht 'dog))
    (test-assert (hash-table-exists? ht 'cat))
    (test-assert (hash-table-exists? ht 'elephant))
    (test-error (hash-table-ref ht 'dog))
    (test-equal 'black (hash-table-ref ht 'cat))
    (test-equal 'pink (hash-table-ref ht 'elephant))
    (test-lset-eq? '(cat elephant) (hash-table-keys ht))
    (test-lset-eq? '(black pink) (hash-table-values ht))
    (test-lset-equal? '((cat . black) (elephant . pink)) (hash-table->alist ht))

    ;; remove a non-existing element
    (hash-table-delete! ht 'dog)
    (test-equal 2 (hash-table-size ht))
    (test-not (hash-table-exists? ht 'dog))

    ;; overwrite an existing element
    (hash-table-set! ht 'cat 'calico)
    (test-equal 2 (hash-table-size ht))
    (test-not (hash-table-exists? ht 'dog))
    (test-assert (hash-table-exists? ht 'cat))
    (test-assert (hash-table-exists? ht 'elephant))
    (test-error (hash-table-ref ht 'dog))
    (test-equal 'calico (hash-table-ref ht 'cat))
    (test-equal 'pink (hash-table-ref ht 'elephant))
    (test-lset-eq? '(cat elephant) (hash-table-keys ht))
    (test-lset-eq? '(calico pink) (hash-table-values ht))
    (test-lset-equal? '((cat . calico) (elephant . pink)) (hash-table->alist ht))

    ;; walk and fold
    (test-lset-equal?
     '((cat . calico) (elephant . pink))
     (let ((a '()))
       (hash-table-walk ht (lambda (k v) (set! a (cons (cons k v) a))))
       a))
    (test-lset-equal? '((cat . calico) (elephant . pink))
                      (hash-table-fold ht (lambda (k v a) (cons (cons k v) a)) '()))

    ;; copy
    (let ((ht2 (hash-table-copy ht)))
      (test-equal 2 (hash-table-size ht2))
      (test-not (hash-table-exists? ht2 'dog))
      (test-assert (hash-table-exists? ht2 'cat))
      (test-assert (hash-table-exists? ht2 'elephant))
      (test-error (hash-table-ref ht2 'dog))
      (test-equal 'calico (hash-table-ref ht2 'cat))
      (test-equal 'pink (hash-table-ref ht2 'elephant))
      (test-lset-eq? '(cat elephant) (hash-table-keys ht2))
      (test-lset-eq? '(calico pink) (hash-table-values ht2))
      (test-lset-equal? '((cat . calico) (elephant . pink))
                        (hash-table->alist ht2)))

    ;; merge
    (let ((ht2 (make-hash-table eq?)))
      (hash-table-set! ht2 'bear 'brown)
      (test-equal 1 (hash-table-size ht2))
      (test-not (hash-table-exists? ht2 'dog))
      (test-assert (hash-table-exists? ht2 'bear))
      (hash-table-merge! ht2 ht)
      (test-equal 3 (hash-table-size ht2))
      (test-assert (hash-table-exists? ht2 'bear))
      (test-assert (hash-table-exists? ht2 'cat))
      (test-assert (hash-table-exists? ht2 'elephant))
      (test-not (hash-table-exists? ht2 'goose))
      (test-equal 'brown (hash-table-ref ht2 'bear))
      (test-equal 'calico (hash-table-ref ht2 'cat))
      (test-equal 'pink (hash-table-ref ht2 'elephant))
      (test-error (hash-table-ref ht2 'goose))
      (test-equal 'grey (hash-table-ref/default ht2 'goose 'grey))
      (test-lset-eq? '(bear cat elephant) (hash-table-keys ht2))
      (test-lset-eq? '(brown calico pink) (hash-table-values ht2))
      (test-lset-equal? '((cat . calico) (bear . brown) (elephant . pink))
                        (hash-table->alist ht2)))

    ;; alist->hash-table
    (test-lset-equal? (hash-table->alist ht)
                      (hash-table->alist
                       (alist->hash-table
                        '((cat . calico) (elephant . pink)))))
    )

  ;; update
  (let ((ht (make-hash-table eq?))
        (add1 (lambda (x) (+ x 1))))
    (hash-table-set! ht 'sheep 0)
    (hash-table-update! ht 'sheep add1)
    (hash-table-update! ht 'sheep add1)
    (test-equal 2 (hash-table-ref ht 'sheep))
    (hash-table-update!/default ht 'crows add1 0)
    (hash-table-update!/default ht 'crows add1 0)
    (hash-table-update!/default ht 'crows add1 0)
    (test-equal 3 (hash-table-ref ht 'crows)))

  ;; string keys
  (let ((ht (make-hash-table equal?)))
    (hash-table-set! ht "cat" 'black)
    (hash-table-set! ht "dog" 'white)
    (hash-table-set! ht "elephant" 'pink)
    (hash-table-ref/default ht "dog" #f)
    (test-equal 'white (hash-table-ref ht "dog"))
    (test-equal 'black (hash-table-ref ht "cat"))
    (test-equal 'pink (hash-table-ref ht "elephant"))
    (test-error (hash-table-ref ht "goose"))
    (test-equal 'grey (hash-table-ref/default ht "goose" 'grey))
    (test-lset-equal? '("cat" "dog" "elephant") (hash-table-keys ht))
    (test-lset-equal? '(black white pink) (hash-table-values ht))
    (test-lset-equal?
     '(("cat" . black) ("dog" . white) ("elephant" . pink))
     (hash-table->alist ht)))

  ;; string-ci keys
  (let ((ht (make-hash-table string-ci=? string-ci-hash)))
    (hash-table-set! ht "cat" 'black)
    (hash-table-set! ht "dog" 'white)
    (hash-table-set! ht "elephant" 'pink)
    (hash-table-ref/default ht "DOG" #f)
    (test-equal 'white (hash-table-ref ht "DOG"))
    (test-equal 'black (hash-table-ref ht "Cat"))
    (test-equal 'pink (hash-table-ref ht "eLePhAnT"))
    (test-error (hash-table-ref ht "goose"))
    (test-lset-equal? '("cat" "dog" "elephant") (hash-table-keys ht))
    (test-lset-equal? '(black white pink) (hash-table-values ht))
    (test-lset-equal?
     '(("cat" . black) ("dog" . white) ("elephant" . pink))
     (hash-table->alist ht)))

  ;; stress test
  (test-equal 625
              (let ((ht (make-hash-table)))
                (do ((i 0 (+ i 1))) ((= i 1000))
                  (hash-table-set! ht i (* i i)))
                (hash-table-ref/default ht 25 #f)))
  )

;;-----------------------------------------------------------------------
(test-section "srfi-98")
(use srfi-98)
(test-module 'srfi-98)

;;-----------------------------------------------------------------------
(test-section "srfi-111")
(use srfi-111)
(test-module 'srfi-111)

;; srfi-111 is built-in.
(test* "box primitives"
       '(#t #f 2 3)
       (let1 b (box 2)
         (list (box? b) (box? 2) (unbox b)
               (begin(set-box! b 3)
                     (unbox b)))))

(test* "box compare"
       '(#t #f #f #t #f)
       (let ([b1 (box 2)]
             [b2 (box 2)]
             [b3 (box 3)])
         (list (equal? b1 b2)
               (equal? b2 b3)
               (equal? b1 2)
               (eqv?   b1 b1)
               (eqv?   b1 b2))))

;;-----------------------------------------------------------------------
;; srfi-113 depends on srfi-114, so we test this first.
(test-section "srfi-114")
(use srfi-114)
(test-module 'srfi-114)

;; builtin comparators are tested in test/compare.scm
(let ()
  (define (test-cmp msg cmpr data) ; data = ((a b result) ...)
    (test* msg data
           (map (^x (list (car x) (cadr x)
                          (comparator-compare cmpr (car x) (cadr x))))
                data)))

  (test-cmp "inexact1"
            (make-inexact-real-comparator 0.25 'round 'min)
            '((1.0 1.1 0)
              (1.1 1.2 -1)
              (1.2 1.1 1)
              (1.1 1.0 0)
              (1.2 1.3 0)
              (+nan.0 1.1 -1)
              (1.1 +nan.0 1)
              (+nan.0 +nan.0 0)))

  (test-cmp "inexact2"
            (make-inexact-real-comparator 0.25 'round 'max)
            '((+nan.0 1.1 1)
              (1.1 +nan.0 -1)
              (+nan.0 +nan.0 0)))

  (test-cmp "inexact3 - nan handling"
            ($ make-inexact-real-comparator 0.25 'round
               (^x (comparator-compare default-comparator 0 x)))
            '((+nan.0 1.1 -1)
              (+nan.0 0 0)
              (+nan.0 -1.1 1)
              (+nan.0 +nan.0 0)))

  (test-cmp "list" (make-list-comparator
                    (make-inexact-real-comparator 0.25 'round 'error))
            '(((1 2 3) (1 2 3) 0)
              ((1 2 3) (1.1 2 3) 0)
              ((1 2 3) (1.1 2) 1)
              ((1 2 3) (1.2 2) -1)
              ((1 2 3) (1.2 2 3) -1)
              (() () 0)
              (() (1) -1)
              ((1 2 3) (1 2 2.9) 0)
              ((1 2 3) (1 2.2 2.9) -1)
              ((1 2.2 2.9) (1 2 3) 1)
              ))
            
  (test-cmp "vector" (make-vector-comparator
                      (make-inexact-real-comparator 0.25 'round 'error))
            '((#(1 2 3) #(1 2 3) 0)
              (#(1 2 3) #(1.1 2 3) 0)
              (#(1 2 3) #(1.1 2) 1)
              (#(1 2 3) #(1.2 2) 1)
              (#(1 2 3) #(1.2 2 3) -1)
              (#() #() 0)
              (#() #(1) -1)
              (#(1 2 3) #(1 2 2.9) 0)
              (#(1 2 3) #(1 2.2 2.9) -1)
              (#(1 2.2 2.9) #(1 2 3) 1)
              ))
            
  )

(let ([lw (make-listwise-comparator vector?
                                    (make-reverse-comparator number-comparator)
                                    (^x (= (vector-length x) 0))
                                    (^x (vector-ref x 0))
                                    (^x (vector-copy x 1)))])
  (define (test-lw a b expect)
    (test* (format "listwise ~s ~s" a b)
           (list (zero? expect) expect (- expect) #t)
           (list (comparator-equal? lw a b)
                 (comparator-compare lw a b)
                 (comparator-compare lw b a)
                 (integer? (comparator-hash lw a)))))
  (test-lw '#(1) '#(2) 1)
  (test-lw '#(1 4) '#(1 3 5) -1)
  (test-lw '#(1 2) '#(1 3 5) 1)
  (test-lw '#(1 3 5) '#(1 3 5) 0)
  (test-lw '#(1 3 6) '#(1 3 5) -1)
  (test-lw '#() '#() 0)
  (test-lw '#() '#(1) -1))

(let ([vw (make-vectorwise-comparator list?
                                      (make-reverse-comparator number-comparator)
                                      length
                                      list-ref)])
  (define (test-vw a b expect)
    (test* (format "vectorwise ~s ~s" a b)
           (list (zero? expect) expect (- expect) #t)
           (list (comparator-equal? vw a b)
                 (comparator-compare vw a b)
                 (comparator-compare vw b a)
                 (integer? (comparator-hash vw a)))))
  (test-vw '(1) '(2) 1)
  (test-vw '(1 4) '(1 3 5) -1)
  (test-vw '(1 2) '(1 3 5) -1)
  (test-vw '(1 3 5) '(1 3 5) 0)
  (test-vw '(1 3 6) '(1 3 5) -1)
  (test-vw '() '() 0)
  (test-vw '() '(1) -1))

(let ()
  (define (test-minmax msg cmp min-item max-item args)
    (test* (format "test-minmax ~a" msg)
           (list min-item max-item max-item min-item)
           (list (apply comparator-min cmp args)
                 (apply comparator-max cmp args)
                 (apply comparator-min (make-reverse-comparator cmp) args)
                 (apply comparator-max (make-reverse-comparator cmp) args))))

  (test-minmax "default" default-comparator 1 9
               '(3 1 4 5 9 2 6 8 7))
  )

;;-----------------------------------------------------------------------
(test-section "srfi-113")
(use srfi-113)
(test-module 'srfi-113)

;; We use test suite provided by srfi-113 reference implementation.
;; The following is a quick adaptation of their test suite; the tests
;; themselves are copied verbatim.
(let1 current-test-comparator (make-parameter equal?)
  (define-syntax gauche:parameterize parameterize)
  (define gauche:test-error test-error)
  (letrec-syntax
      ([begin* (syntax-rules ()
                 [(_ x) (let () x)]
                 [(_ x . y) (let () x (begin* . y))])]
       [test-group (syntax-rules () [(_ name . xs) (begin* . xs)])]
       [test (syntax-rules ()
               [(_ expected expr)
                (test* 'expected expected expr (current-test-comparator))]
               [(_ name expected expr)
                (test* name expected expr (current-test-comparator))])]
       [test-assert (syntax-rules ()
                      [(_ expr) (test* 'expr #t (boolean expr))])]
       [test-error (syntax-rules ()
                     [(_ expr)
                      (test* 'expr (gauche:test-error) expr)])]
       [parameterize (syntax-rules ()
                       [(_ bindings . xs)
                        (gauche:parameterize bindings (begin* . xs))])]
       )

(test-group "sets"
(define (big x) (> x 5))

(test-group "sets"
(test-group "sets/simple"
  (define nums (set number-comparator))
  ;; nums is now {}
  (define syms (set eq-comparator 'a 'b 'c 'd))
  ;; syms is now {a, b, c, d}
  (define nums2 (set-copy nums))
  ;; nums2 is now {}
  (define syms2 (set-copy syms))
  ;; syms2 is now {a, b, c, d}
  (define esyms (set eq-comparator))
  ;; esyms is now {}
  (test-assert (set-empty? esyms))
  (define total 0)
  (test-assert (set? nums))
  (test-assert (set? syms))
  (test-assert (set? nums2))
  (test-assert (set? syms2))
  (test-assert (not (set? 'a)))
  (set-adjoin! nums 2)
  (set-adjoin! nums 3)
  (set-adjoin! nums 4)
  (set-adjoin! nums 4)
  ;; nums is now {2, 3, 4}
  (test 4 (set-size (set-adjoin nums 5)))
  (test 3 (set-size nums))
  (test 3 (set-size (set-delete syms 'd)))
  (test 2 (set-size (set-delete-all syms '(c d))))
  (test 4 (set-size syms))
  (set-adjoin! syms 'e 'f)
  ;; syms is now {a, b, c, d, e, f}
  (test 4 (set-size (set-delete-all! syms '(e f))))
  ;; syms is now {a, b, c, d}
  (test 0 (set-size nums2))
  (test 4 (set-size syms2))
  (set-delete! nums 2)
  ;; nums is now {3, 4}
  (test 2 (set-size nums))
  (set-delete! nums 1)
  (test 2 (set-size nums))
  (set! nums2 (set-map (lambda (x) (* 10 x)) number-comparator nums))
  ;; nums2 is now {30, 40}
  (test-assert (set-contains? nums2 30))
  (test-assert (not (set-contains? nums2 3)))
  (set-for-each (lambda (x) (set! total (+ total x))) nums2)
  (test 70 total)
  (test 10 (set-fold + 3 nums))
  (set! nums (set eqv-comparator 10 20 30 40 50))
  ;; nums is now {10, 20, 30, 40, 50}
  (test-assert
    (set=? nums (set-unfold
       (lambda (i) (= i 0))
       (lambda (i) (* i 10))
       (lambda (i) (- i 1))
       5
       eqv-comparator)))
  (test '(a) (set->list (set eq-comparator 'a)))
  (set! syms2 (list->set eq-comparator '(e f)))
  ;; syms2 is now {e, f}
  (test 2 (set-size syms2))
  (test-assert (set-contains? syms2 'e))
  (test-assert (set-contains? syms2 'f))
  (list->set! syms2 '(a b))
  (test 4 (set-size syms2))
) ; end sets/simple

(test-group "sets/search"
  (define yam (set char-comparator #\y #\a #\m))
  (define (failure/insert insert ignore)
    (insert 1))
  (define (failure/ignore insert ignore)
    (ignore 2))
  (define (success/update element update remove)
    (update #\b 3))
  (define (success/remove element update remove)
    (remove 4))
  (define yam! (set char-comparator #\y #\a #\m #\!))
  (define bam (set char-comparator #\b #\a #\m))
  (define ym (set char-comparator #\y #\m))
  (define-values (set1 obj1)
    (set-search! (set-copy yam) #\! failure/insert error))
  (test-assert (set=? yam! set1))
  (test 1 obj1)
  (define-values (set2 obj2)
    (set-search! (set-copy yam) #\! failure/ignore error))
  (test-assert (set=? yam set2))
  (test 2 obj2)
  (define-values (set3 obj3)
    (set-search! (set-copy yam) #\y error success/update))
  (test-assert (set=? bam set3))
  (test 3 obj3)
  (define-values (set4 obj4)
    (set-search! (set-copy yam) #\a error success/remove))
  (test-assert (set=? ym set4))
  (test 4 obj4)
) ; end sets/search

(test-group "sets/subsets"
  (define set2 (set number-comparator 1 2))
  (define other-set2 (set number-comparator 1 2))
  (define set3 (set number-comparator 1 2 3))
  (define set4 (set number-comparator 1 2 3 4))
  (define setx (set number-comparator 10 20 30 40))
  (test-assert (set=? set2 other-set2))
  (test-assert (not (set=? set2 set3)))
  (test-assert (not (set=? set2 set3 other-set2)))
  (test-assert (set<? set2 set3 set4))
  (test-assert (not (set<? set2 other-set2)))
  (test-assert (set<=? set2 other-set2 set3))
  (test-assert (not (set<=? set2 set3 other-set2)))
  (test-assert (set>? set4 set3 set2))
  (test-assert (not (set>? set2 other-set2)))
  (test-assert (set>=? set3 other-set2 set2))
  (test-assert (not (set>=? other-set2 set3 set2)))
) ; end sets/subsets

(test-group "sets/ops"
  ;; Potentially mutable
  (define abcd (set eq-comparator 'a 'b 'c 'd))
  (define efgh (set eq-comparator 'e 'f 'g 'h))
  (define abgh (set eq-comparator 'a 'b 'g 'h))
  ;; Never get a chance to be mutated
  (define other-abcd (set eq-comparator 'a 'b 'c 'd))
  (define other-efgh (set eq-comparator 'e 'f 'g 'h))
  (define other-abgh (set eq-comparator 'a 'b 'g 'h))
  (define all (set eq-comparator 'a 'b 'c 'd 'e 'f 'g 'h))
  (define none (set eq-comparator))
  (define ab (set eq-comparator 'a 'b))
  (define cd (set eq-comparator 'c 'd))
  (define ef (set eq-comparator 'e 'f))
  (define gh (set eq-comparator 'g 'h))
  (define cdgh (set eq-comparator 'c 'd 'g 'h))
  (define abcdgh (set eq-comparator 'a 'b 'c 'd 'g 'h))
  (define abefgh (set eq-comparator 'a 'b 'e 'f 'g 'h))
  (test-assert (set-disjoint? abcd efgh))
  (test-assert (not (set-disjoint? abcd ab)))
  (parameterize ((current-test-comparator set=?))
    (test all (set-union abcd efgh))
    (test abcdgh (set-union abcd abgh))
    (test abefgh (set-union efgh abgh))
    (define efgh2 (set-copy efgh))
    (set-union! efgh2 abgh)
    (test abefgh efgh2)
    (test none (set-intersection abcd efgh))
    (define abcd2 (set-copy abcd))
    (set-intersection! abcd2 efgh)
    (test none abcd2)
    (test ab (set-intersection abcd abgh))
    (test ab (set-intersection abgh abcd))
    (test cd (set-difference abcd ab))
    (test abcd (set-difference abcd gh))
    (test none (set-difference abcd abcd))
    (define abcd3 (set-copy abcd))
    (set-difference! abcd3 abcd)
    (test none abcd3)
    (test cdgh (set-xor abcd abgh))
    (test all (set-xor abcd efgh))
    (test none (set-xor abcd other-abcd))
    (define abcd4 (set-copy abcd))
    ;; don't test xor! effect
    (test none (set-xor! abcd4 other-abcd))
    (test "abcd smashed?" other-abcd abcd)
    (test "efgh smashed?" other-efgh efgh)
    (test "abgh smashed?" other-abgh abgh))
) ; end sets/subsets

(test-group "sets/mismatch"
  (define nums (set number-comparator 1 2 3))
  (define syms (set eq-comparator 'a 'b 'c))
  (test-error (set=? nums syms))
  (test-error (set<? nums syms))
  (test-error (set<=? nums syms))
  (test-error (set>? nums syms))
  (test-error (set>=? nums syms))
  (test-error (set-union nums syms))
  (test-error (set-intersection nums syms))
  (test-error (set-difference nums syms))
  (test-error (set-xor nums syms))
  (test-error (set-union! nums syms))
  (test-error (set-intersection! nums syms))
  (test-error (set-difference! nums syms))
  (test-error (set-xor! nums syms))
) ; end sets/mismatch

(test-group "sets/whole"
  (define whole (set eqv-comparator 1 2 3 4 5 6 7 8 9 10))
  (define whole2 (set-copy whole))
  (define whole3 (set-copy whole))
  (define whole4 (set-copy whole))
  (define bottom (set eqv-comparator 1 2 3 4 5))
  (define top (set eqv-comparator 6 7 8 9 10))
  (define-values (topx bottomx)
    (set-partition big whole))
  (set-partition! big whole4)
  (parameterize ((current-test-comparator set=?))
    (test top (set-filter big whole))
    (test bottom (set-remove big whole))
    (set-filter! big whole2)
    (test-assert (not (set-contains? whole2 1)))
    (set-remove! big whole3)
    (test-assert (not (set-contains? whole3 10)))
    (test top topx)
    (test bottom bottomx)
    (test top whole4))
  (test 5 (set-count big whole))
  (define hetero (set eqv-comparator 1 2 'a 3 4))
  (define homo (set eqv-comparator 1 2 3 4 5))
  (test 'a (set-find symbol? hetero (lambda () (error "wrong"))))
  (test-error  (set-find symbol? homo (lambda () (error "wrong"))))
  (test-assert (set-any? symbol? hetero))
  (test-assert (set-any? number? hetero))
  (test-assert (not (set-every? symbol? hetero)))
  (test-assert (not (set-every? number? hetero)))
  (test-assert (not (set-any? symbol? homo)))
  (test-assert (set-every? number? homo))
) ; end sets/whole

(test-group "sets/lowlevel"
  (define bucket (set string-ci-comparator "abc" "def"))
  (test string-ci-comparator (set-element-comparator bucket))
  (test-assert (set-contains? bucket "abc"))
  (test-assert (set-contains? bucket "ABC"))
  (test "def" (set-member bucket "DEF" "fqz"))
  (test "fqz" (set-member bucket "lmn" "fqz"))
  (define nums (set number-comparator 1 2 3))
  ;; nums is now {1, 2, 3}
  (define nums2 (set-replace nums 2.0))
  ;; nums2 is now {1, 2.0, 3}
  (test-assert (set-any? inexact? nums2))
  (set-replace! nums 2.0)
  ;; nums is now {1, 2.0, 3}
  (test-assert (set-any? inexact? nums))
  (define sos
    (set set-comparator
      (set eqv-comparator 1 2)
      (set eqv-comparator 1 2)))
  (test 1 (set-size sos))
) ; end sets/lowlevel

) ; end sets

(test-group "bags"
(test-group "bags/simple"
  (define nums (bag number-comparator))
  ;; nums is now {}
  (define syms (bag eq-comparator 'a 'b 'c 'd))
  ;; syms is now {a, b, c, d}
  (define nums2 (bag-copy nums))
  ;; nums2 is now {}
  (define syms2 (bag-copy syms))
  ;; syms2 is now {a, b, c, d}
  (define esyms (bag eq-comparator))
  ;; esyms is now {}
  (test-assert (bag-empty? esyms))
  (define total 0)
  (test-assert (bag? nums))
  (test-assert (bag? syms))
  (test-assert (bag? nums2))
  (test-assert (bag? syms2))
  (test-assert (not (bag? 'a)))
  (bag-adjoin! nums 2)
  (bag-adjoin! nums 3)
  (bag-adjoin! nums 4)
  ;; nums is now {2, 3, 4}
  (test 4 (bag-size (bag-adjoin nums 5)))
  (test 3 (bag-size nums))
  (test 3 (bag-size (bag-delete syms 'd)))
  (test 2 (bag-size (bag-delete-all syms '(c d))))
  (test 4 (bag-size syms))
  (bag-adjoin! syms 'e 'f)
  ;; syms is now {a, b, c, d, e, f}
  (test 4 (bag-size (bag-delete-all! syms '(e f))))
  ;; syms is now {a, b, c, d}
  (test 3 (bag-size nums))
  (bag-delete! nums 1)
  (test 3 (bag-size nums))
  (set! nums2 (bag-map (lambda (x) (* 10 x)) number-comparator nums))
  ;; nums2 is now {20, 30, 40}
  (test-assert (bag-contains? nums2 30))
  (test-assert (not (bag-contains? nums2 3)))
  (bag-for-each (lambda (x) (set! total (+ total x))) nums2)
  (test 90 total)
  (test 12 (bag-fold + 3 nums))
  (set! nums (bag eqv-comparator 10 20 30 40 50))
  ;; nums is now {10, 20, 30, 40, 50}
  (test-assert
    (bag=? nums (bag-unfold
       (lambda (i) (= i 0))
       (lambda (i) (* i 10))
       (lambda (i) (- i 1))
       5
       eqv-comparator)))
  (test '(a) (bag->list (bag eq-comparator 'a)))
  (set! syms2 (list->bag eq-comparator '(e f)))
  ;; syms2 is now {e, f}
  (test 2 (bag-size syms2))
  (test-assert (bag-contains? syms2 'e))
  (test-assert (bag-contains? syms2 'f))
  (list->bag! syms2 '(e f))
  ;; syms2 is now {e, e, f, f}
  (test 4 (bag-size syms2))
) ; end bags/simple

(test-group "bags/search"
  (define yam (bag char-comparator #\y #\a #\m))
  (define (failure/insert insert ignore)
    (insert 1))
  (define (failure/ignore insert ignore)
    (ignore 2))
  (define (success/update element update remove)
    (update #\b 3))
  (define (success/remove element update remove)
    (remove 4))
  (define yam! (bag char-comparator #\y #\a #\m #\!))
  (define bam (bag char-comparator #\b #\a #\m))
  (define ym (bag char-comparator #\y #\m))
  (define-values (bag1 obj1)
    (bag-search! (bag-copy yam) #\! failure/insert error))
  (test-assert (bag=? yam! bag1))
  (test 1 obj1)
  (define-values (bag2 obj2)
    (bag-search! (bag-copy yam) #\! failure/ignore error))
  (test-assert (bag=? yam bag2))
  (test 2 obj2)
  (define-values (bag3 obj3)
    (bag-search! (bag-copy yam) #\y error success/update))
  (test-assert (bag=? bam bag3))
  (test 3 obj3)
  (define-values (bag4 obj4)
    (bag-search! (bag-copy yam) #\a error success/remove))
  (test-assert (bag=? ym bag4))
  (test 4 obj4)
) ; end bags/search

(test-group "bags/elemcount"
  (define mybag (bag eqv-comparator 1 1 1 1 1 2 2))
  (test 5 (bag-element-count mybag 1))
  (test 0 (bag-element-count mybag 3))
) ; end bags/elemcount

(test-group "bags/subbags"
  (define bag2 (bag number-comparator 1 2))
  (define other-bag2 (bag number-comparator 1 2))
  (define bag3 (bag number-comparator 1 2 3))
  (define bag4 (bag number-comparator 1 2 3 4))
  (define bagx (bag number-comparator 10 20 30 40))
  (test-assert (bag=? bag2 other-bag2))
  (test-assert (not (bag=? bag2 bag3)))
  (test-assert (not (bag=? bag2 bag3 other-bag2)))
  (test-assert (bag<? bag2 bag3 bag4))
  (test-assert (not (bag<? bag2 other-bag2)))
  (test-assert (bag<=? bag2 other-bag2 bag3))
  (test-assert (not (bag<=? bag2 bag3 other-bag2)))
  (test-assert (bag>? bag4 bag3 bag2))
  (test-assert (not (bag>? bag2 other-bag2)))
  (test-assert (bag>=? bag3 other-bag2 bag2))
  (test-assert (not (bag>=? other-bag2 bag3 bag2)))
) ; end bags/subbags

(test-group "bags/multi"
  (define one (bag eqv-comparator 10))
  (define two (bag eqv-comparator 10 10))
  (test-assert (not (bag=? one two)))
  (test-assert (bag<? one two))
  (test-assert (not (bag>? one two)))
  (test-assert (bag<=? one two))
  (test-assert (not (bag>? one two)))
  (test-assert (bag=? two two))
  (test-assert (not (bag<? two two)))
  (test-assert (not (bag>? two two)))
  (test-assert (bag<=? two two))
  (test-assert (bag>=? two two))
  (test '((10 . 2))
    (let ((result '()))
      (bag-for-each-unique
         (lambda (x y) (set! result (cons (cons x y) result)))
         two)
      result))
  (test 25 (bag-fold + 5 two))
  (test 12 (bag-fold-unique (lambda (k n r) (+ k n r)) 0 two))
) ; end bags/multi

(test-group "bags/ops"
  ;; Potentially mutable
  (define abcd (bag eq-comparator 'a 'b 'c 'd))
  (define efgh (bag eq-comparator 'e 'f 'g 'h))
  (define abgh (bag eq-comparator 'a 'b 'g 'h))
  ;; Never get a chance to be mutated
  (define other-abcd (bag eq-comparator 'a 'b 'c 'd))
  (define other-efgh (bag eq-comparator 'e 'f 'g 'h))
  (define other-abgh (bag eq-comparator 'a 'b 'g 'h))
  (define all (bag eq-comparator 'a 'b 'c 'd 'e 'f 'g 'h))
  (define none (bag eq-comparator))
  (define ab (bag eq-comparator 'a 'b))
  (define cd (bag eq-comparator 'c 'd))
  (define ef (bag eq-comparator 'e 'f))
  (define gh (bag eq-comparator 'g 'h))
  (define cdgh (bag eq-comparator 'c 'd 'g 'h))
  (define abcdgh (bag eq-comparator 'a 'b 'c 'd 'g 'h))
  (define abefgh (bag eq-comparator 'a 'b 'e 'f 'g 'h))
  (test-assert (bag-disjoint? abcd efgh))
  (test-assert (not (bag-disjoint? abcd ab)))
  (parameterize ((current-test-comparator bag=?))
    (test all (bag-union abcd efgh))
    (test abcdgh (bag-union abcd abgh))
    (test abefgh (bag-union efgh abgh))
    (define efgh2 (bag-copy efgh))
    (bag-union! efgh2 abgh)
    (test abefgh efgh2)
    (test none (bag-intersection abcd efgh))
    (define abcd2 (bag-copy abcd))
    (bag-intersection! abcd2 efgh)
    (test none abcd2)
    (test ab (bag-intersection abcd abgh))
    (test ab (bag-intersection abgh abcd))
    (test cd (bag-difference abcd ab))
    (test abcd (bag-difference abcd gh))
    (test none (bag-difference abcd abcd))
    (define abcd3 (bag-copy abcd))
    (bag-difference! abcd3 abcd)
    (test none abcd3)
    (test cdgh (bag-xor abcd abgh))
    (test all (bag-xor abcd efgh))
    (test none (bag-xor abcd other-abcd))
    (define abcd4 (bag-copy abcd))
    (test none (bag-xor! abcd4 other-abcd))
    (define abab (bag eq-comparator 'a 'b 'a 'b))
    (define ab2 (bag-copy ab))
    (test abab (bag-sum! ab2 ab))
    (test abab ab2)
    (test abab (bag-product 2 ab))
    (define ab3 (bag-copy ab))
    (bag-product! 2 ab3)
    (test abab ab3)
    (test "abcd smashed?" other-abcd abcd)
    (test "abcd smashed?" other-abcd abcd)
    (test "efgh smashed?" other-efgh efgh)
    (test "abgh smashed?" other-abgh abgh))
) ; end bags/ops

(test-group "bags/mismatch"
  (define nums (bag number-comparator 1 2 3))
  (define syms (bag eq-comparator 'a 'b 'c))
  (test-error (bag=? nums syms))
  (test-error (bag<? nums syms))
  (test-error (bag<=? nums syms))
  (test-error (bag>? nums syms))
  (test-error (bag>=? nums syms))
  (test-error (bag-union nums syms))
  (test-error (bag-intersection nums syms))
  (test-error (bag-difference nums syms))
  (test-error (bag-xor nums syms))
  (test-error (bag-union! nums syms))
  (test-error (bag-intersection! nums syms))
  (test-error (bag-difference! nums syms))
) ; end bags/mismatch

(test-group "bags/whole"
  (define whole (bag eqv-comparator 1 2 3 4 5 6 7 8 9 10))
  (define whole2 (bag-copy whole))
  (define whole3 (bag-copy whole))
  (define whole4 (bag-copy whole))
  (define bottom (bag eqv-comparator 1 2 3 4 5))
  (define top (bag eqv-comparator 6 7 8 9 10))
  (define-values (topx bottomx)
    (bag-partition big whole))
  (bag-partition! big whole4)
  (parameterize ((current-test-comparator bag=?))
    (test top (bag-filter big whole))
    (test bottom (bag-remove big whole))
    (bag-filter! big whole2)
    (test-assert (not (bag-contains? whole2 1)))
    (bag-remove! big whole3)
    (test-assert (not (bag-contains? whole3 10)))
    (test top topx)
    (test bottom bottomx)
    (test top whole4))
  (test 5 (bag-count big whole))
  (define hetero (bag eqv-comparator 1 2 'a 3 4))
  (define homo (bag eqv-comparator 1 2 3 4 5))
  (test 'a (bag-find symbol? hetero (lambda () (error "wrong"))))
  (test-error  (bag-find symbol? homo (lambda () (error "wrong"))))
  (test-assert (bag-any? symbol? hetero))
  (test-assert (bag-any? number? hetero))
  (test-assert (not (bag-every? symbol? hetero)))
  (test-assert (not (bag-every? number? hetero)))
  (test-assert (not (bag-any? symbol? homo)))
  (test-assert (bag-every? number? homo))
) ; end bags/whole

(test-group "bags/lowlevel"
  (define bucket (bag string-ci-comparator "abc" "def"))
  (test string-ci-comparator (bag-element-comparator bucket))
  (test-assert (bag-contains? bucket "abc"))
  (test-assert (bag-contains? bucket "ABC"))
  (test "def" (bag-member bucket "DEF" "fqz"))
  (test "fqz" (bag-member bucket "lmn" "fqz"))
  (define nums (bag number-comparator 1 2 3))
  ;; nums is now {1, 2, 3}
  (define nums2 (bag-replace nums 2.0))
  ;; nums2 is now {1, 2.0, 3}
  (test-assert (bag-any? inexact? nums2))
  (bag-replace! nums 2.0)
  ;; nums is now {1, 2.0, 3}
  (test-assert (bag-any? inexact? nums))
  (define bob
    (bag bag-comparator
      (bag eqv-comparator 1 2)
      (bag eqv-comparator 1 2)))
  (test 2 (bag-size bob))
) ; end bags/lowlevel


(test-group "bags/semantics"
  (define mybag (bag number-comparator 1 2))
  ;; mybag is {1, 2}
  (test 2 (bag-size mybag))
  (bag-adjoin! mybag 1)
  ;; mybag is {1, 1, 2}
  (test 3 (bag-size mybag))
  (test 2 (bag-unique-size mybag))
  (bag-delete! mybag 2)
  ;; mybag is {1, 1}
  (bag-delete! mybag 2)
  (test 2 (bag-size mybag))
  (bag-increment! mybag 1 3)
  ;; mybag is {1, 1, 1, 1, 1}
  (test 5 (bag-size mybag))
  (test-assert (bag-decrement! mybag 1 2))
  ;; mybag is {1, 1, 1}
  (test 3 (bag-size mybag))
  (bag-decrement! mybag 1 5)
  ;; mybag is {}
  (test 0 (bag-size mybag))
) ; end bags/semantics

(test-group "bags/convert"
  (define multi (bag eqv-comparator 1 2 2 3 3 3))
  (define single (bag eqv-comparator 1 2 3))
  (define singleset (set eqv-comparator 1 2 3))
  (define minibag (bag eqv-comparator 'a 'a))
  (define alist '((a . 2)))
  (test alist (bag->alist minibag))
  (test-assert (bag=? minibag (alist->bag eqv-comparator alist)))
  (test-assert (set=? singleset (bag->set single)))
  (test-assert (set=? singleset (bag->set multi)))
  (test-assert (bag=? single (set->bag singleset)))
  (test-assert (not (bag=? multi (set->bag singleset))))
  (set->bag! minibag singleset)
  ;; minibag is now {a, a, a, a, 1, 2, 3}
  (test-assert (bag-contains? minibag 1))
) ; end bags/convert

(test-group "bags/sumprod"
  (define abb (bag eq-comparator 'a 'b 'b))
  (define aab (bag eq-comparator 'a 'a 'b))
  (define total (bag-sum abb aab))
  (test 3 (bag-count (lambda (x) (eqv? x 'a)) total))
  (test 3 (bag-count (lambda (x) (eqv? x 'b)) total))
  (test 12 (bag-size (bag-product 2 total)))
  (define bag1 (bag eqv-comparator 1))
  (bag-sum! bag1 bag1)
  (test 2 (bag-size bag1))
  (bag-product! 2 bag1)
  (test 4 (bag-size bag1))
) ; end bag/sumprod

) ; end bags

(test-group "comparators"
  (define a (set number-comparator 1 2 3))
  (define b (set number-comparator 1 2 4))
  (define aa (bag number-comparator 1 2 3))
  (define bb (bag number-comparator 1 2 4))
  (test-assert (not (=? set-comparator a b)))
  (test-assert (=? set-comparator a (set-copy a)))
  (test-error (<? set-comparator a b))
  (test-assert (not (=? bag-comparator aa bb)))
  (test-assert (=? bag-comparator aa (bag-copy aa)))
  (test-error (<? bag-comparator aa bb))
  (test-assert (not (=? default-comparator a aa)))
) ; end comparators
  
)))

(test-end)

