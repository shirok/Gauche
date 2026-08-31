;;
;; test for weak vector / weak hash table
;;

;; The collection of garbage is affected by lots of factors.
;; The following tests make effort to cause the weakly referenced
;; objects collected, but may fail on certain occasions.

(use gauche.test)
(use scheme.list)

(test-start "weak pointers")

;; A dummy function to overwrite the VM stack, so that we can clear out
;; any dangling reference to the object that are pointed by weak structure.
;; (This wouldn't be necessary once we implement a sane GC mark handler
;; on the VM stack.)
(define (fact n)
  (if (zero? n)
    1
    (* n (fact (- n 1)))))

(define (clear-references)
  (fact 1000) ;; clear the stack
  (dotimes (n 10) (gc)))

(test-section "weak vector")

(define x (make-weak-vector 5))

(test* "make-weak-vector" #t (is-a? x <weak-vector>))
(test* "weak-vector-length" 5 (weak-vector-length x))

(test* "weak-vector-set!/ref" '((1 2 3) (4 5 6) (7 8 9) #f #f)
       (begin (weak-vector-set! x 0 (list 1 2 3))
              (weak-vector-set! x 1 (list 4 5 6))
              (weak-vector-set! x 2 (list 7 8 9))
              (map (cut weak-vector-ref x <>) '(0 1 2 3 4))))

(clear-references)

(test* "weak-vector-set!/ref (after gc)" '(#f #f #f #f #f)
       (map (cut weak-vector-ref x <>) '(0 1 2 3 4)))

;; A disappearing link can only be registered on a GC-allocated object; attempt
;; to register other values would calse GC crash later.  These tests checks
;; we do not register such values.
(let1 y (make-weak-vector 3)
  (weak-vector-set! y 0 <integer>)
  (weak-vector-set! y 1 <string>)
  (weak-vector-set! y 2 'quote)
  (clear-references)
  (test* "weak-vector with statically allocated objects" '(#t #t #t)
         (map (^i (and (weak-vector-ref y i) #t)) '(0 1 2))))

(define (reciprocate x) (/ 1.0 x)) ; avoid constant-folded

;; The link must be registered on the base of the object, not on the ScmObj:
;; a heap flonum's ScmObj is base+6 and an extended pair's is base+8.
(let* ([f  (reciprocate 3)]
       [xp (extended-cons 'a 'b)]
       [s  (string-copy "hello")]
       [y  (make-weak-vector 3)])
  (weak-vector-set! y 0 f)
  (weak-vector-set! y 1 xp)
  (weak-vector-set! y 2 s)
  (clear-references)
  (test* "weak-vector with interior-pointer objects" (list f xp s)
         (map (cut weak-vector-ref y <>) '(0 1 2)))

  (set! f #f)
  (set! xp #f)
  (set! s #f)
  (clear-references)
  (test* "weak-vector with interior-pointer objects" '(#f #f #f)
         (map (cut weak-vector-ref y <>) '(0 1 2))))

; (test-section "weak hash table")

; (define x (make-weak-hash-table 'eqv? 'value 'gone))

; (test* "make-weak-hash-table (value-weak)" <weak-hash-table>
;        (class-of x))

; (test* "weak-hash-table-type" 'eqv? (weak-hash-table-type x))
; (test* "weak-hash-table-weakness" 'value (weak-hash-table-weakness x))

; (test* "weak-hash-table-get (nonexistent)" (test-error)
;        (weak-hash-table-get x 123))
; (test* "weak-hash-table-get (nonexistent)" 'foo
;        (weak-hash-table-get x 123 'foo))

; (test* "weak-hash-table-put!/get" '(1 2 3)
;        (begin
;          (weak-hash-table-put! x 123 (list 1 2 3))
;          (weak-hash-table-get x 123)))
; (test* "weak-hash-table-put!/get" '(4 5 6)
;        (begin
;          (weak-hash-table-put! x 456 (list 4 5 6))
;          (weak-hash-table-get x 456)))

; (clear-references)

; (test* "weak-hash-table-get (after gc)" '(gone gone)
;        (map (cut weak-hash-table-get x <>) '(123 456)))

; (test* "weak-hash-table-keys & values" '((111 222 123 456)
;                                          ((1 1 1) (2 2 2) gone gone))
;        (let ((ones (list 1 1 1))
;              (twos (list 2 2 2)))
;          (weak-hash-table-put! x 111 ones)
;          (weak-hash-table-put! x 222 twos)
;          (list (weak-hash-table-keys x)
;                (weak-hash-table-values x)))
;        (lambda (expected got)
;          (and (lset= equal? (car expected) (car got))
;               (lset= equal? (cadr expected) (cadr got)))))

; (define x (make-weak-hash-table 'equal? 'key 'gone))

; (test* "make-weak-hash-table (key-weak)" <weak-hash-table>
;        (class-of x))

; (test* "weak-hash-table-weakness" 'key (weak-hash-table-weakness x))

; (test* "weak-hash-table-get (nonexistent)" (test-error)
;        (weak-hash-table-get x (list 1 2 3)))
; (test* "weak-hash-table-get (nonexistent)" 'foo
;        (weak-hash-table-get x (list 1 2 3) 'foo))


; (define y (list 7 8 9))

; (test* "weak-hash-table-put!/get" 123
;        (begin
;          (weak-hash-table-put! x (list 1 2 3) 123)
;          (weak-hash-table-get x '(1 2 3) 'huh?)))
; (test* "weak-hash-table-put!/get" 456
;        (begin
;          (weak-hash-table-put! x (list 4 5 6) 456)
;          (weak-hash-table-get x '(4 5 6) 'huh?)))
; (test* "weak-hash-table-put!/get" 789
;        (begin
;          (weak-hash-table-put! x y 789)
;          (weak-hash-table-get x '(7 8 9) 'huh?)))

; (clear-references)

; (test* "weak-hash-table-get (after gc)" '(foo foo 789)
;        (map (cut weak-hash-table-get x <> 'foo) '((1 2 3) (4 5 6) (7 8 9))))

; (test* "weak-hash-table-keys&values" '(((7 8 9)) (789))
;        (list (weak-hash-table-keys x)
;              (weak-hash-table-values x)))

(test-end)
