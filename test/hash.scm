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

(test* "b => non" #t
       (hash-table-get  h-eq 'b #t))

(test* "b => error" (test-error)
       (hash-table-get h-eq 'b))

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

(test* "eq? test" 7
       (begin
         (hash-table-put! h-eq (string #\d) 4)
         (hash-table-put! h-eq (string #\d) 5)
         (length (hash-table-keys h-eq))))

(test* "hash-table-values(1)" #t
       (lset= equal? (hash-table-values h-eq) '(8 "b" #\c 3 4 5 10)))

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

(define h-it (hash-table 'eq?
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

(test-module 'gauche.hashutil) ; autoloaded module

(test-end)
