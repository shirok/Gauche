;;
;; Test hash table
;;

(use gauche.test)
(use srfi-1)
(use srfi-13)

;; Note: test/object.scm contains extra tests of hashtables using
;; object-equal? and object-hash overload.

(test-start "hash tables")

;;------------------------------------------------------------------
(test-section "hash function")

;; Test portable hash function.  We must guarantee hash values by
;; portable-hash won't change.

(let ([data '((#f 3428989595 3428989595)
              (#t 64744347 64744347)
              (#\a 3754065427 3754065427)
              (#\null 3668339987 3668339987)
              (0 0 2654435761)
              (1 2654435761 387276917)
              (-1 2654435761 626627309)
              (45174651375937459317569347657 3238994630 3238994630)
              (1.1 324783707 2919879337)
              (-1.1 1508195907 1375087959)
              (3.767278962604361e-10 3208588342 0)
              ;; NB: with old 'hash' code, the following was 0 on 8087
              (3.767278962604362e-10 3983142176 1)
              (3.767278962604363e-10 462728714 1)
              (-3.767278962604361e-10 2477197406 0)
              ;; NB: with old 'hash' code, the following was 0 on 8087
              (-3.767278962604362e-10 3251751240 4294967295)
              (-3.767278962604363e-10 4026305074 4294967295)
              ;; NB: with old 'hash' code, this was 1981270711 on 8087
              (3.474701543e9 3535247838 1981271040)
              (3.474701544e9 3780614622 0)
              (-3.474701544e9 3597766888 0)
              (+inf.0 0 0)
              (+nan.0 0 0)
              (1/2 1401181143 1401181143)
              (-5/4 3964193037 91423867)
              (3.0+1.0i 2174816445 2027808452)
              (() 995466395 995466395)
              ((#f a 0) 590094886 509715512)
              (#(#t :b 3.2) 2915469923 1522869008)
              ("" 3183946422 0)
              ("zye" 1276445264 121094)
              (abc 4121390185 96354)
              (:foo 696220911 101574))])
  (test* "portable hash (new, legacy)" data
         (map (^p (list (car p)
                        (portable-hash (car p) 0)
                        (hash (car p))))
              data)))

;;------------------------------------------------------------------
(test-section "eq?-hash")

(define h-eq (make-hash-table))

(test* "make-hash-table" #t
       (hash-table? h-eq))

(test* "hash-table-type" 'eq?
       (hash-table-type h-eq))

(test* "a => 8" 8
       (begin
         (hash-table-put! h-eq 'a 8)
         (hash-table-get  h-eq 'a)))

(test* "a adjoin" 8
       (begin
         (hash-table-adjoin! h-eq 'a 9)
         (hash-table-get h-eq 'a)))

(test* "a replace" 7
       (begin
         (hash-table-replace! h-eq 'a 7)
         (hash-table-get h-eq 'a)))

(test* "b => non" #t
       (hash-table-get  h-eq 'b #t))

(test* "b => error" (test-error)
       (hash-table-get h-eq 'b))

(test* "b replace" #f
       (begin
         (hash-table-replace! h-eq 'b 100)
         (hash-table-get h-eq 'b #f)))

(test* "b => \"b\"" "b"
       (begin
         (hash-table-put! h-eq 'b "b")
         (hash-table-get  h-eq 'b)))

(test* "c => #\C" #\C
       (begin
         (hash-table-put! h-eq 'c #\C)
         (hash-table-get  h-eq 'c)))

(test* "c => #\c" #\c
       (begin
         (hash-table-put! h-eq 'c #\c)
         (hash-table-get  h-eq 'c)))

(test* "d adjoin" "D"
       (begin
         (hash-table-adjoin! h-eq 'd "D")
         (hash-table-adjoin! h-eq 'd "d")
         (hash-table-get h-eq 'd)))

(test* "e => 10" 10
       (begin
         (hash-table-put! h-eq 'e 8)
         (hash-table-update! h-eq 'e (^x (+ x 1)))
         (hash-table-update! h-eq 'e (^x (+ x 1)))
         (hash-table-get h-eq 'e)))

(test* "f => 1" 3
       (begin
         (hash-table-update! h-eq 'f (^x (+ x 1)) 2)
         (hash-table-get h-eq 'f)))

(test* "eq? test" 8
       (begin
         (hash-table-put! h-eq (string #\d) 4)
         (hash-table-put! h-eq (string #\d) 5)
         (length (hash-table-keys h-eq))))

(test* "hash-table-values(1)" #t
       (lset= equal? (hash-table-values h-eq) '(7 "b" #\c "D" 3 4 5 10)))

(test* "delete!" '(#t #f #f)
       (let* ((a (hash-table-delete! h-eq 'c))
              (b (hash-table-delete! h-eq 'c)))
         (list a b (hash-table-get h-eq 'c #f))))

(test* "clear!" '()
       (begin (hash-table-clear! h-eq)
              (hash-table-keys h-eq)))

;;------------------------------------------------------------------
(test-section "eqv?-hash")

(define h-eqv (make-hash-table 'eqv?))

(test* "make-hash-table" #t
       (hash-table? h-eqv))

(test* "hash-table-type" 'eqv?
       (hash-table-type h-eqv))

(test* "a => 8" 8
       (begin
         (hash-table-put! h-eqv 'a 8)
         (hash-table-get  h-eqv 'a)))

(test* "b => non" #t
       (hash-table-get  h-eqv 'b #t))

(test* "b => error" (test-error)
       (hash-table-get  h-eqv 'b))

(test* "b => \"b\"" "b"
       (begin
         (hash-table-put! h-eqv 'b "b")
         (hash-table-get  h-eqv 'b)))

(test* "2.0 => #\C" #\C
       (begin
         (hash-table-put! h-eqv 2.0 #\C)
         (hash-table-get  h-eqv 2.0)))

(test* "2.0 => #\c" #\c
       (begin
         (hash-table-put! h-eqv 2.0 #\c)
         (hash-table-get  h-eqv 2.0)))


(test* "87592876592374659237845692374523694756 => 0" 0
       (begin
         (hash-table-put! h-eqv 87592876592374659237845692374523694756 0)
         (hash-table-get  h-eqv 87592876592374659237845692374523694756)))

(test* "87592876592374659237845692374523694756 => -1" -1
       (begin
         (hash-table-put! h-eqv 87592876592374659237845692374523694756 -1)
         (hash-table-get  h-eqv 87592876592374659237845692374523694756)))

(test* "377/120 => pi" 'pi
       (begin
         (hash-table-put! h-eqv 377/120 'pi)
         (hash-table-get  h-eqv 377/120)))

(test* "377/120 => PI" 'PI
       (begin
         (hash-table-put! h-eqv 377/120 'PI)
         (hash-table-get  h-eqv 377/120)))

(test* "eqv? test" 7
       (begin
         (hash-table-put! h-eqv (string #\d) 4)
         (hash-table-put! h-eqv (string #\d) 5)
         (length (hash-table-keys h-eqv))))

(test* "hash-table-values(2)" #t
       (lset= equal? (hash-table-values h-eqv) '(8 "b" #\c -1 4 5 PI)))

(define h-eqv-copied (hash-table-copy h-eqv))

(test* "delete!" #f
       (begin
         (hash-table-delete! h-eqv 87592876592374659237845692374523694756)
         (hash-table-get h-eqv 87592876592374659237845692374523694756 #f)))

(test* "copy" #t
       (lset= equal? (hash-table-values h-eqv-copied) '(8 "b" #\c -1 4 5 PI)))

;;------------------------------------------------------------------
(test-section "equal?-hash")

(define h-equal (make-hash-table 'equal?))

(test* "make-hash-table" #t
       (hash-table? h-equal))

(test* "hash-table-type" 'equal?
       (hash-table-type h-equal))

(test* "a => 8" 8
       (begin
         (hash-table-put! h-equal 'a 8)
         (hash-table-get  h-equal 'a)))

(test* "b => non" #t
       (hash-table-get  h-equal 'b #t))

(test* "b => error" (test-error)
       (hash-table-get  h-equal 'b))

(test* "b => \"b\"" "b"
       (begin
         (hash-table-put! h-equal 'b "b")
         (hash-table-get  h-equal 'b)))

(test* "2.0 => #\C" #\C
       (begin
         (hash-table-put! h-equal 2.0 #\C)
         (hash-table-get  h-equal 2.0)))

(test* "2.0 => #\c" #\c
       (begin
         (hash-table-put! h-equal 2.0 #\c)
         (hash-table-get  h-equal 2.0)))

(test* "87592876592374659237845692374523694756 => 0" 0
       (begin
         (hash-table-put! h-equal 87592876592374659237845692374523694756 0)
         (hash-table-get  h-equal 87592876592374659237845692374523694756)))

(test* "87592876592374659237845692374523694756 => -1" -1
       (begin
         (hash-table-put! h-equal 87592876592374659237845692374523694756 -1)
         (hash-table-get  h-equal 87592876592374659237845692374523694756)))

(test* "e => \"e\"" "E"
       (begin
         (hash-table-put! h-equal 'e "e")
         (hash-table-update! h-equal 'e (^x (string-upcase x)))
         (hash-table-get h-equal 'e)))

(test* "equal? test" 6
       (begin
         (hash-table-put! h-equal (string #\d) 4)
         (hash-table-put! h-equal (string #\d) 5)
         (length (hash-table-keys h-equal))))

(test* "equal? test" 7
       (begin
         (hash-table-put! h-equal (cons 'a 'b) 6)
         (hash-table-put! h-equal (cons 'a 'b) 7)
         (length (hash-table-keys h-equal))))

(test* "equal? test" 8
       (begin
         (hash-table-put! h-equal (vector (cons 'a 'b) 3+3i) 60)
         (hash-table-put! h-equal (vector (cons 'a 'b) 3+3i) 61)
         (length (hash-table-keys h-equal))))

(test* "hash-table-values(3)" #t
       (lset= equal? (hash-table-values h-equal) '(8 "b" #\c -1 "E" 5 7 61)))

(test* "delete!" #f
       (begin
         (hash-table-delete! h-equal (vector (cons 'a 'b) 3+3i))
         (hash-table-get h-equal (vector (cons 'a 'b) 3+3i) #f)))

;;------------------------------------------------------------------
(test-section "string?-hash")

(define h-string (make-hash-table 'string=?))

(test* "make-hash-table" #t
       (hash-table? h-string))

(test* "hash-table-type" 'string=?
       (hash-table-type h-string))

(test* "\"a\" => 8" 8
       (begin
         (hash-table-put! h-string "a" 8)
         (hash-table-get  h-string "a")))

(test* "\"b\" => non" #t
       (hash-table-get  h-string "b" #t))

(test* "\"b\" => non" (test-error)
       (hash-table-get  h-string "b"))

(test* "\"b\" => \"b\"" "b"
       (begin
         (hash-table-put! h-string "b" "b")
         (hash-table-get  h-string "b")))

(test* "string=? test" 3
       (begin
         (hash-table-put! h-string (string #\d) 4)
         (hash-table-put! h-string (string #\d) 5)
         (length (hash-table-keys h-string))))

(test* "\"e\" => 9" 9
       (begin
         (hash-table-put! h-string "e" 8)
         (hash-table-update! h-string "e" (^x (+ x 1)))
         (hash-table-get h-string "e")))

(test* "hash-table-values(4)" #t
       (lset= equal? (hash-table-values h-string) '(8 "b" 5 9)))

(test* "delete!" #f
       (begin
         (hash-table-delete! h-string "d")
         (hash-table-get h-string "d" #f)))

;;------------------------------------------------------------------
(test-section "generic hash")

(let* ([c (make-comparator (^x (and (integer? x) (<= 0 x 255)))
                           (^[a b] (= (modulo a 13) (modulo b 13)))
                           #f
                           (^x (eqv-hash (modulo x 13))))]
       [h (make-hash-table c)])
  (test* "construction" 'general (hash-table-type h))
  (test* "retrieve comparator" c (hash-table-comparator h))
  (test* "nonexistent" #f (hash-table-exists? h 0))
  (test* "domain error 1" (test-error) (hash-table-exists? h 'a))
  (test* "domain error 2" (test-error) (hash-table-exists? h -1))
  (test* "put/get" 'a
         (begin (hash-table-put! h 1 'a)
                (hash-table-get h 1)))
  (test* "hash and equality check" '(#t a)
         (list (hash-table-exists? h 14)
               (hash-table-get h 27)))
  (test* "hash and equality check 2" 'b
         (begin (hash-table-put! h 40 'b)
                (hash-table-get h 1)))
  (test* "hash and equality check 3"
         '((0 . 247) (1 . 248) (2 . 249) (3 . 250) (4 . 251)
           (5 . 252) (6 . 253) (7 . 254) (8 . 255) (9 . 243)
           (10 . 244) (11 . 245) (12 . 246))
         (begin
           (dotimes [n 256]
             (hash-table-put! h n n))
           (sort-by (hash-table->alist h) car)))
  )

(let ([x `((eq? . ,eq-comparator)
           (eqv? . ,eqv-comparator)
           (equal? . ,equal-comparator)
           (string=? . ,string-comparator))])
  (test* "recognizing special comparators"
         (map car x)
         (map (^p (hash-table-type (make-hash-table (cdr p)))) x))
  (test* "retrieving comparators"
         (map cdr x)
         (map (^p (hash-table-comparator (make-hash-table (car p)))) x)))

;;------------------------------------------------------------------
(test-section "iterators")

(define h-it (hash-table-from-pairs 'eq?
                                    '(a . 3)
                                    '(c . 8)
                                    '(b . 4)
                                    '(d . 10)))

(test* "hash-table"
       '(a b c d)
       (hash-table-keys h-it)
       (^[a b] (lset= equal? a b)))

(test* "hash-table-map"
       '((a . 3) (b . 4) (c . 8) (d . 10))
       (hash-table-map h-it cons)
       (^[a b] (lset= equal? a b)))

(test* "hash-table-for-each"
       '((a . 3) (b . 4) (c . 8) (d . 10))
       (let ((r '()))
         (hash-table-for-each h-it (^[k v] (push! r (cons k v))))
         r)
       (^[a b] (lset= equal? a b)))

(test* "hash-table-fold"
       '((a . 3) (b . 4) (c . 8) (d . 10))
       (hash-table-fold h-it acons '())
       (^[a b] (lset= equal? a b)))

(test* "alist->hash-table" '(a b)
       (let1 ht (alist->hash-table '((5 . b) (3 . a)) 'eqv?)
         (list (hash-table-get ht 3)
               (hash-table-get ht 5))))
(test* "hash-table->alist" '(("a" . 3) ("b" . 5))
       (let1 a (hash-table->alist
                (hash-table 'equal? '("a" . 3) '("b" . 5)))
         (list (assoc "a" a)
               (assoc "b" a))))

(test* "hash-table-find" 16
       (hash-table-find h-it (^[k v] (and (eq? k 'c) (* v 2)))))

(test* "hash-table-find" 'oops
       (hash-table-find h-it (^[k v] (and (eq? k 'e) (* v 2)))
                        (^[] 'oops)))

;;------------------------------------------------------------------
(test-section "compare as sets")

(let ([a (hash-table-r7 eq-comparator 'a 1)]
      [b (hash-table-r7 eq-comparator 'a 1 'b 2)]
      [c (hash-table-r7 eq-comparator 'a 1 'b 2 'c 3)]
      [d (hash-table-r7 eq-comparator 'a 1 'b 2 'c -3)]
      [e (hash-table-r7 eq-comparator 'a 1 'e 2)]
      [f (hash-table-r7 eq-comparator 'a 1 'b 2 'c 3 'e 2)]
      [g (hash-table-r7 eq-comparator 'a 1 'b 2 'c 3)])
  (define (inv a) (and a (- a)))
  (define-syntax D
    (syntax-rules ()
      ([_ x y expected] `(,x ,y x y ,expected))))
  (define data
    (list (D a b -1)
          (D a c -1)
          (D b c -1)
          (D b d -1)
          (D b e #f)
          (D c d #f)
          (D c e #f)
          (D d e #f)
          (D e f -1)
          (D c g 0)))

  (dolist [d data]
    (receive (x y xname yname expected) (apply values d)
      (test* "hash-table-compare-as-sets"
             `((,xname ,yname ,expected)
               (,yname ,xname ,(and expected (- expected))))
             (list
              `(,xname ,yname ,(hash-table-compare-as-sets x y eqv? #f))
              `(,yname ,xname ,(hash-table-compare-as-sets y x eqv? #f))))))

  (test* "hash-table=? c d" #f
         (hash-table=? eqv-comparator c d))
  (test* "hash-table=? c g" #t
         (hash-table=? eqv-comparator c g))
  (test* "hash-table=? c c" #t
         (hash-table=? eqv-comparator c c))
  (test* "hash-table=? f a" #f
         (hash-table=? eqv-comparator f a))
  (test* "hash-table=? c d (custom compar)" #t
         (hash-table=? (make-comparator number? (^[a b] (= (abs a) (abs b)))
                                        #f #f)
                       c d)))
              
(test-module 'gauche.hashutil) ; autoloaded module

(test-end)
