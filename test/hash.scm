;;
;; Test hash table
;;

;; $Id: hash.scm,v 1.1 2001-10-13 11:45:40 shirok Exp $

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





      
(test-end)
