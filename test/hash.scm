;;
;; Test hash table
;;

;; $Id: hash.scm,v 1.3 2003-01-07 12:06:53 shirok Exp $

(use gauche.test)
(use srfi-1)

(test-start "hash tables")

;;------------------------------------------------------------------
(test-section "eq?-hash")

(define h-eq (make-hash-table))

(test "make-hash-table" #t
      (lambda () (hash-table? h-eq)))

(test "a => 8" 8
      (lambda ()
        (hash-table-put! h-eq 'a 8)
        (hash-table-get  h-eq 'a)))

(test "b => non" #t
      (lambda ()
        (hash-table-get  h-eq 'b #t)))

(test "b => \"b\"" "b"
      (lambda ()
        (hash-table-put! h-eq 'b "b")
        (hash-table-get  h-eq 'b)))

(test "c => #\C" #\C
      (lambda ()
        (hash-table-put! h-eq 'c #\C)
        (hash-table-get  h-eq 'c)))

(test "c => #\c" #\c
      (lambda ()
        (hash-table-put! h-eq 'c #\c)
        (hash-table-get  h-eq 'c)))

(test "eq? test" 5
      (lambda ()
        (hash-table-put! h-eq (string #\d) 4)
        (hash-table-put! h-eq (string #\d) 5)
        (length (hash-table-keys h-eq))))

(test "hash-table-values" #t
      (lambda ()
        (lset= equal? (hash-table-values h-eq) '(8 "b" #\c 4 5))))

;;------------------------------------------------------------------
(test-section "eqv?-hash")

(define h-eqv (make-hash-table 'eqv?))

(test "make-hash-table" #t
      (lambda () (hash-table? h-eqv)))

(test "a => 8" 8
      (lambda ()
        (hash-table-put! h-eqv 'a 8)
        (hash-table-get  h-eqv 'a)))

(test "b => non" #t
      (lambda ()
        (hash-table-get  h-eqv 'b #t)))

(test "b => \"b\"" "b"
      (lambda ()
        (hash-table-put! h-eqv 'b "b")
        (hash-table-get  h-eqv 'b)))

(test "2.0 => #\C" #\C
      (lambda ()
        (hash-table-put! h-eqv 2.0 #\C)
        (hash-table-get  h-eqv 2.0)))

(test "2.0 => #\c" #\c
      (lambda ()
        (hash-table-put! h-eqv 2.0 #\c)
        (hash-table-get  h-eqv 2.0)))

(test "87592876592374659237845692374523694756 => 0" 0
      (lambda ()
        (hash-table-put! h-eqv 87592876592374659237845692374523694756 0)
        (hash-table-get  h-eqv 87592876592374659237845692374523694756)))

(test "87592876592374659237845692374523694756 => -1" -1
      (lambda ()
        (hash-table-put! h-eqv 87592876592374659237845692374523694756 -1)
        (hash-table-get  h-eqv 87592876592374659237845692374523694756)))

(test "eqv? test" 6
      (lambda ()
        (hash-table-put! h-eqv (string #\d) 4)
        (hash-table-put! h-eqv (string #\d) 5)
        (length (hash-table-keys h-eqv))))

(test "hash-table-values" #t
      (lambda ()
        (lset= equal? (hash-table-values h-eqv) '(8 "b" #\c -1 4 5))))

;;------------------------------------------------------------------
(test-section "equal?-hash")

(define h-equal (make-hash-table 'equal?))

(test "make-hash-table" #t
      (lambda () (hash-table? h-equal)))

(test "a => 8" 8
      (lambda ()
        (hash-table-put! h-equal 'a 8)
        (hash-table-get  h-equal 'a)))

(test "b => non" #t
      (lambda ()
        (hash-table-get  h-equal 'b #t)))

(test "b => \"b\"" "b"
      (lambda ()
        (hash-table-put! h-equal 'b "b")
        (hash-table-get  h-equal 'b)))

(test "2.0 => #\C" #\C
      (lambda ()
        (hash-table-put! h-equal 2.0 #\C)
        (hash-table-get  h-equal 2.0)))

(test "2.0 => #\c" #\c
      (lambda ()
        (hash-table-put! h-equal 2.0 #\c)
        (hash-table-get  h-equal 2.0)))

(test "87592876592374659237845692374523694756 => 0" 0
      (lambda ()
        (hash-table-put! h-equal 87592876592374659237845692374523694756 0)
        (hash-table-get  h-equal 87592876592374659237845692374523694756)))

(test "87592876592374659237845692374523694756 => -1" -1
      (lambda ()
        (hash-table-put! h-equal 87592876592374659237845692374523694756 -1)
        (hash-table-get  h-equal 87592876592374659237845692374523694756)))

(test "equal? test" 5
      (lambda ()
        (hash-table-put! h-equal (string #\d) 4)
        (hash-table-put! h-equal (string #\d) 5)
        (length (hash-table-keys h-equal))))

(test "equal? test" 6
      (lambda ()
        (hash-table-put! h-equal (cons 'a 'b) 6)
        (hash-table-put! h-equal (cons 'a 'b) 7)
        (length (hash-table-keys h-equal))))

(test "equal? test" 7
      (lambda ()
        (hash-table-put! h-equal (vector (cons 'a 'b) 3+3i) 60)
        (hash-table-put! h-equal (vector (cons 'a 'b) 3+3i) 61)
        (length (hash-table-keys h-equal))))

(test "hash-table-values" #t
      (lambda ()
        (lset= equal? (hash-table-values h-equal) '(8 "b" #\c -1 5 7 61))))

;;------------------------------------------------------------------
(test-section "string?-hash")

(define h-string (make-hash-table 'string=?))

(test "make-hash-table" #t
      (lambda () (hash-table? h-string)))

(test "\"a\" => 8" 8
      (lambda ()
        (hash-table-put! h-string "a" 8)
        (hash-table-get  h-string "a")))

(test "\"b\" => non" #t
      (lambda ()
        (hash-table-get  h-string "b" #t)))

(test "\"b\" => \"b\"" "b"
      (lambda ()
        (hash-table-put! h-string "b" "b")
        (hash-table-get  h-string "b")))

(test "string=? test" 3
      (lambda ()
        (hash-table-put! h-string (string #\d) 4)
        (hash-table-put! h-string (string #\d) 5)
        (length (hash-table-keys h-string))))

(test "hash-table-values" #t
      (lambda ()
        (lset= equal? (hash-table-values h-string) '(8 "b" 5))))

;;------------------------------------------------------------------
(test-section "iterators")

(define h-it (hash-table 'eq?
                         '(a . 3)
                         '(c . 8)
                         '(b . 4)
                         '(d . 10)))

(test "hash-table"
      '(a b c d)
      (lambda () (hash-table-keys h-it))
      (lambda (a b) (lset= equal? a b)))

(test "hash-table-map"
      '((a . 3) (b . 4) (c . 8) (d . 10))
      (lambda ()
        (hash-table-map h-it cons))
      (lambda (a b) (lset= equal? a b)))

(test "hash-table-for-each"
      '((a . 3) (b . 4) (c . 8) (d . 10))
      (lambda ()
        (let ((r '()))
          (hash-table-for-each h-it (lambda (k v) (push! r (cons k v))))
          r))
      (lambda (a b) (lset= equal? a b)))

(test "hash-table-fold"
      '((a . 3) (b . 4) (c . 8) (d . 10))
      (lambda () (hash-table-fold h-it acons '()))
      (lambda (a b) (lset= equal? a b)))


      
(test-end)
