;;
;; test util modules
;;

(use gauche.test)
(test-start "util")

(use srfi-1)

;;-----------------------------------------------
(test-section "util.combinations")
(use util.combinations)
(test-module 'util.combinations)

(test* "permutations (boundary)" '(())
       (permutations '()))
(test* "permutations (boundary)" '((a))
       (permutations '(a)))
(test* "permutations" '((a b) (b a))
       (permutations '(a b)))
(test* "permutations" '((a a) (a a))
       (permutations '(a a)))
(test* "permutations" '((a b c) (a c b) (b a c) (b c a) (c a b) (c b a))
       (permutations '(a b c)))
(test* "permutations" '((a b c d) (a b d c) (a c b d) (a c d b)
                        (a d b c) (a d c b) (b a c d) (b a d c)
                        (b c a d) (b c d a) (b d a c) (b d c a)
                        (c a b d) (c a d b) (c b a d) (c b d a)
                        (c d a b) (c d b a) (d a b c) (d a c b)
                        (d b a c) (d b c a) (d c a b) (d c b a))
       (permutations '(a b c d)))

(test* "permutations* (boundary)" '(())
       (permutations* '()))
(test* "permutations* (boundary)" '((a))
       (permutations* '(a)))
(test* "permutations*" '((a b) (b a))
       (permutations* '(a b)))
(test* "permutations*" '((a a))
       (permutations* '(a a)))
(test* "permutations*" '((a b c) (a c b) (b a c) (b c a) (c a b) (c b a))
       (permutations* '(a b c)))
(test* "permutations*" '((a a b) (a b a) (b a a))
       (permutations* '(a a b)))
(test* "permutations*" '((a b a) (a a b) (b a a))
       (permutations* '(a b a)))
(test* "permutations*" '((b a a) (a b a) (a a b))
       (permutations* '(b a a)))
(test* "permutations*" '((a a a))
       (permutations* '(a a a)))
(test* "permutations*" '((a b c d) (a b d c) (a c b d) (a c d b)
                         (a d b c) (a d c b) (b a c d) (b a d c)
                         (b c a d) (b c d a) (b d a c) (b d c a)
                         (c a b d) (c a d b) (c b a d) (c b d a)
                         (c d a b) (c d b a) (d a b c) (d a c b)
                         (d b a c) (d b c a) (d c a b) (d c b a))
       (permutations* '(a b c d)))
(test* "permutations*" '((a a b c) (a a c b) (a b a c) (a b c a)
                         (a c a b) (a c b a) (b a a c) (b a c a)
                         (b c a a) (c a a b) (c a b a) (c b a a))
       (permutations* '(a a b c)))
(test* "permutations*" '((a b a c) (a b c a) (a a b c) (a a c b)
                         (a c b a) (a c a b) (b a a c) (b a c a)
                         (b c a a) (c a b a) (c a a b) (c b a a))
       (permutations* '(a b a c)))
(test* "permutations*" '((a b c a) (a b a c) (a c b a) (a c a b)
                         (a a b c) (a a c b) (b a c a) (b a a c)
                         (b c a a) (c a b a) (c a a b) (c b a a))
       (permutations* '(a b c a)))
(test* "permutations*" '((a b a b) (a b b a) (a a b b)
                         (b a a b) (b a b a) (b b a a))
       (permutations* '(a b a b)))
(test* "permutations*" '((a a a b) (a a b a) (a b a a) (b a a a))
       (permutations* '(a a a b)))
(test* "permutations*" '((a b a a) (a a b a) (a a a b) (b a a a))
       (permutations* '(a b a a)))
(test* "permutations*" '((a a a a))
       (permutations* '(a a a a)))

(test* "permutations*" '(("a" "b" "b" "a") ("a" "b" "a" "b") ("a" "a" "b" "b")
                         ("b" "a" "b" "a") ("b" "a" "a" "b") ("b" "b" "a" "a"))
       (permutations* '("a" "b" "b" "a") string=?))

(test* "permutations-for-each"
       '()
       (let1 r '()
         (permutations-for-each (lambda (p) (push! r p)) '())
         (reverse r)))
(test* "permutations-for-each"
       '((a))
       (let1 r '()
         (permutations-for-each (lambda (p) (push! r p)) '(a))
         (reverse r)))
(test* "permutations-for-each"
       '((a b c) (a c b) (b a c) (b c a) (c a b) (c b a))
       (let1 r '()
         (permutations-for-each (lambda (p) (push! r p)) '(a b c))
         (reverse r)))
(test* "permutations*-for-each"
       '()
       (let1 r '()
         (permutations*-for-each (lambda (p) (push! r p)) '())
         (reverse r)))
(test* "permutations*-for-each"
       '((a))
       (let1 r '()
         (permutations*-for-each (lambda (p) (push! r p)) '(a))
         (reverse r)))
(test* "permutations*-for-each"
       '((a b c) (a c b) (b a c) (b c a) (c a b) (c b a))
       (let1 r '()
         (permutations*-for-each (lambda (p) (push! r p)) '(a b c))
         (reverse r)))
(test* "permutations*-for-each"
       '((a a b) (a b a) (b a a))
       (let1 r '()
         (permutations*-for-each (lambda (p) (push! r p)) '(a a b))
         (reverse r)))
(test* "permutations*-for-each"
       '((a a a))
       (let1 r '()
         (permutations*-for-each (lambda (p) (push! r p)) '(a a a))
         (reverse r)))
(test* "permutations*-for-each"
       '(("a" "a" "b") ("a" "b" "a") ("b" "a" "a"))
       (let1 r '()
         (permutations*-for-each (lambda (p) (push! r p)) '("a" "a" "b")
                                 string=?)
         (reverse r)))

(test* "combinations" '(())
       (combinations '() 0))
(test* "combinations" '((a))
       (combinations '(a) 1))
(test* "combinations" '((a) (b) (c) (d))
       (combinations '(a b c d) 1))
(test* "combinations" '((a b) (a c) (b c))
       (combinations '(a b c) 2))
(test* "combinations" '((a b c))
       (combinations '(a b c) 3))
(test* "combinations" '((a b c) (a b d) (a c d) (b c d))
       (combinations '(a b c d) 3))

(test* "combinations*" '(())
       (combinations* '() 0))
(test* "combinations*" '((a))
       (combinations* '(a) 1))
(test* "combinations*" '((a) (b) (c) (d))
       (combinations* '(a b c d) 1))
(test* "combinations*" '((a b) (a c) (b c))
       (combinations* '(a b c) 2))
(test* "combinations*" '((a b c))
       (combinations* '(a b c) 3))
(test* "combinations*" '((a b c) (a b d) (a c d) (b c d))
       (combinations* '(a b c d) 3))
(test* "combinations*" '((a) (b))
       (combinations* '(a a b) 1))
(test* "combinations*" '((a a) (a b))
       (combinations* '(a a b) 2))
(test* "combinations*" '((a a b))
       (combinations* '(a a b) 3))
(test* "combinations*" '((a b) (a a))
       (combinations* '(a b a a) 2))
(test* "combinations*" '((a b a) (a a a))
       (combinations* '(a b a a) 3))
(test* "combinations*" '((a b b) (a b a))
       (combinations* '(a b b a) 3))
(test* "combinations*" '(("a" "b" "b") ("a" "b" "a"))
       (combinations* '("a" "b" "b" "a") 3 string=?))

(test* "combinations-for-each" '(())
       (let1 r '()
         (combinations-for-each (lambda (c) (push! r c)) '() 0)
         (reverse! r)))
(test* "combinations-for-each" '((a))
       (let1 r '()
         (combinations-for-each (lambda (c) (push! r c)) '(a) 1)
         (reverse! r)))
(test* "combinations-for-each" '((a) (b) (c) (d))
       (let1 r '()
         (combinations-for-each (lambda (c) (push! r c)) '(a b c d) 1)
         (reverse! r)))
(test* "combinations-for-each" '((a b) (a c) (b c))
       (let1 r '()
         (combinations-for-each (lambda (c) (push! r c)) '(a b c) 2)
         (reverse! r)))
(test* "combinations-for-each" '((a b c))
       (let1 r '()
         (combinations-for-each (lambda (c) (push! r c)) '(a b c) 3)
         (reverse! r)))
(test* "combinations-for-each" '((a b c) (a b d) (a c d) (b c d))
       (let1 r '()
         (combinations-for-each (lambda (c) (push! r c)) '(a b c d) 3)
         (reverse! r)))

(test* "combinations*-for-each" '(())
       (let1 r '()
         (combinations*-for-each (lambda (c) (push! r c)) '() 0)
         (reverse! r)))
(test* "combinations*-for-each" '((a))
       (let1 r '()
         (combinations*-for-each (lambda (c) (push! r c)) '(a) 1)
         (reverse! r)))
(test* "combinations*-for-each" '((a) (b) (c) (d))
       (let1 r '()
         (combinations*-for-each (lambda (c) (push! r c)) '(a b c d) 1)
         (reverse! r)))
(test* "combinations*-for-each" '((a b) (a c) (b c))
       (let1 r '()
         (combinations*-for-each (lambda (c) (push! r c)) '(a b c) 2)
         (reverse! r)))
(test* "combinations*-for-each" '((a b c))
       (let1 r '()
         (combinations*-for-each (lambda (c) (push! r c)) '(a b c) 3)
         (reverse! r)))
(test* "combinations*-for-each" '((a b c) (a b d) (a c d) (b c d))
       (let1 r '()
         (combinations*-for-each (lambda (c) (push! r c)) '(a b c d) 3)
         (reverse! r)))
(test* "combinations*-for-each" '((a) (b))
       (let1 r '()
         (combinations*-for-each (lambda (c) (push! r c)) '(a a b) 1)
         (reverse! r)))
(test* "combinations*-for-each" '((a a) (a b))
       (let1 r '()
         (combinations*-for-each (lambda (c) (push! r c)) '(a a b) 2)
         (reverse! r)))
(test* "combinations*-for-each" '((a a b))
       (let1 r '()
         (combinations*-for-each (lambda (c) (push! r c)) '(a a b) 3)
         (reverse! r)))
(test* "combinations*-for-each" '((a b) (a a))
       (let1 r '()
         (combinations*-for-each (lambda (c) (push! r c)) '(a b a a) 2)
         (reverse! r)))
(test* "combinations*-for-each" '((a b a) (a a a))
       (let1 r '()
         (combinations*-for-each (lambda (c) (push! r c)) '(a b a a) 3)
         (reverse! r)))
(test* "combinations*-for-each" '((a b b) (a b a))
       (let1 r '()
         (combinations*-for-each (lambda (c) (push! r c)) '(a b b a) 3)
         (reverse! r)))
(test* "combinations*-for-each" '(("a" "b" "b") ("a" "b" "a"))
       (let1 r '()
         (combinations*-for-each (lambda (c) (push! r c)) '("a" "b" "b" "a") 3
                                 string=?)
         (reverse! r)))

(test* "power-set-binary" '(())
       (power-set-binary '()))
(test* "power-set-binary" '(() (a))
       (power-set-binary '(a)))
(test* "power-set-binary" '(() (c) (b) (b c) (a) (a c) (a b) (a b c))
       (power-set-binary '(a b c)))

(test* "power-set" '(())
       (power-set '()))
(test* "power-set" '(() (a))
       (power-set '(a)))
(test* "power-set" '(() (a) (b) (c) (a b) (a c) (b c) (a b c))
       (power-set '(a b c)))

(test* "power-set*" '(())
       (power-set* '()))
(test* "power-set*" '(() (a))
       (power-set* '(a)))
(test* "power-set*" '(() (a) (b) (a a) (a b) (a a b))
       (power-set* '(a a b)))
(test* "power-set*" '(() ("a") ("b") ("a" "a") ("a" "b") ("a" "a" "b"))
       (power-set* '("a" "a" "b") string=?))

(test* "power-set-for-each" '(())
       (let1 r '()
         (power-set-for-each (lambda (s) (push! r s)) '())
         (reverse! r)))
(test* "power-set-for-each" '(() (a))
       (let1 r '()
         (power-set-for-each (lambda (s) (push! r s))  '(a))
         (reverse! r)))
(test* "power-set-for-each" '(() (a) (b) (c) (a b) (a c) (b c) (a b c))
       (let1 r '()
         (power-set-for-each (lambda (s) (push! r s))  '(a b c))
         (reverse! r)))

(test* "power-set*-for-each" '(())
       (let1 r '()
         (power-set*-for-each (lambda (s) (push! r s)) '())
         (reverse! r)))
(test* "power-set*-for-each" '(() (a))
       (let1 r '()
         (power-set*-for-each (lambda (s) (push! r s))  '(a))
         (reverse! r)))
(test* "power-set*-for-each" '(() (a) (b) (a a) (a b) (a a b))
       (let1 r '()
         (power-set*-for-each (lambda (s) (push! r s))  '(a a b))
         (reverse! r)))
(test* "power-set*-for-each" '(() ("a") ("b") ("a" "a") ("a" "b") ("a" "a" "b"))
       (let1 r '()
         (power-set*-for-each (lambda (s) (push! r s))  '("a" "a" "b")
                              string=?)
         (reverse! r)))

(test* "cartesian-product" '((a 0) (a 1) (b 0) (b 1) (c 0) (c 1))
       (cartesian-product '((a b c) (0 1))))
(test* "cartesian-product" '((a 0 0) (a 0 1) (a 1 0) (a 1 1)
                             (b 0 0) (b 0 1) (b 1 0) (b 1 1))
       (cartesian-product '((a b) (0 1) (0 1))))
(test* "cartesian-product-right" '((a 0) (b 0) (c 0) (a 1) (b 1) (c 1))
       (cartesian-product-right '((a b c) (0 1))))

;;-----------------------------------------------
(test-section "util.isomorph")
(use util.isomorph)
(test-module 'util.isomorph)

(define-class <isomorph-test> ()
  ((a :init-keyword :a)
   (b :init-keyword :b)))

(define-method object-isomorphic? ((x <isomorph-test>)
                                   (y <isomorph-test>)
                                   context)
  (and (isomorphic? (ref x 'a) (ref y 'a) context)
       (isomorphic? (ref x 'b) (ref y 'b) context)))

(define (make-data type)
  (let* ((z (vector #f #f #f #f))
         (x (circular-list "a" 'b 4 9845938427094857239485 #\z 8+5i z))
         (y (circular-list "a" 'b 4 9845938427094857239485 #\z 8+5i z))
         (w (make <isomorph-test> :a z)))
    (vector-set! z 0 x)
    (vector-set! z 1 y)
    (vector-set! z 2 w)
    (slot-set! w 'b w)
    (if type (vector-set! z 3 x) (vector-set! z 3 y))
    z))

(test* "isomorphic?" #t
       (isomorphic? (make-data #f) (make-data #f)))
(test* "isomorphic?" #f
       (isomorphic? (make-data #t) (make-data #f)))

;;-----------------------------------------------
(test-section "util.lcs")
(use util.lcs)
(test-module 'util.lcs)

(test* "lcs skip" '(a c)
       (lcs '(a b c) '(a c)))

(test* "lcs head" '(a b)
       (lcs '(a b c) '(a b)))

(test* "lcs tail" '(b c)
       (lcs '(a b c) '(b c)))

(test* "lcs same" '(a b c)
       (lcs '(a b c) '(a b c)))

(test* "lcs no common" '()
       (lcs '(a b c) '(x y z)))

(test* "lcs empty" '()
       (lcs '(a b c) '()))

(test* "lcs mislead" '(a x b y c z)
       (lcs '(a x b y c z p d q) '(a b c a x b y c z)))

(test* "lcs mislead count"
       '(6 ((a 0 0) (x 1 4) (b 2 5) (y 3 6) (c 4 7) (z 5 8)))
       (lcs-with-positions '(a x b y c z p d q) '(a b c a x b y c z)))

(let1 z (iota 200)
  (test* "lcs (long, same)" #t (equal? z (lcs z z)))
  (test* "lcs (long, none)" '(199) (lcs (reverse z) z))
  (test* "lcs (long)" '(0 1 2 3 4 5 6 7 8 9)
         (lcs z (apply append (make-list 10 (iota 10)))))
  (test* "lcs (long)" '(0 1 2 3 4 5 6 7 8 9)
         (lcs z (apply append (make-list 10 (iota 10 9 -1)))))
  )

(test* "lcs edit-list"
       '(((- 0 a))
         ((+ 2 d))
         ((- 4 h) (+ 4 f))
         ((+ 6 k))
         ((- 8 n) (- 9 p) (+ 9 r) (+ 10 s) (+ 11 t)))
       (lcs-edit-list '(a b c e h j l m n p)
                      '(b c d e f j k l m r s t)))

(test* "lcs edit-list"
       '(((- 0 a) (- 1 b) (- 2 c) (- 3 d) (+ 0 e) (+ 1 f) (+ 2 g) (+ 3 h)))
       (lcs-edit-list '(a b c d) '(e f g h)))

(test* "lcs edit-list"
       '()
       (lcs-edit-list '(a b c d) '(a b c d)))

(test* "lcs edit-list"
       '(((- 0 a) (- 1 b) (- 2 c) (- 3 d)))
       (lcs-edit-list '(a b c d) '()))

(test* "lcs edit-list"
       '(((+ 0 a) (+ 1 b) (+ 2 c) (+ 3 d)))
       (lcs-edit-list '() '(a b c d)))
      
(test* "lcs edit-list"
       '(((+ 0 b)) ((- 1 b)))
       (lcs-edit-list '(a b) '(b a)))

(test* "lcs edit-list"
       '(((- 1 b)))
       (lcs-edit-list '(a b) '(a)))

(test* "lcs edit-list"
       '(((+ 1 b)))
       (lcs-edit-list '(a) '(a b)))

;;-----------------------------------------------
(test-section "util.list")
(use util.list)
(test-module 'util.list)

(test* "split-at* (normal)" '((a b c) (d))
       (receive r (split-at* '(a b c d) 3) r))
(test* "split-at* (boundary)" '(() (a b c d))
       (receive r (split-at* '(a b c d) 0) r))
(test* "split-at* (boundary)" '((a b c d) ())
       (receive r (split-at* '(a b c d) 4) r))
(test* "split-at* (error)" *test-error*
       (receive r (split-at* '(a b c d) -1) r))
(test* "split-at* (shorten)" '((a b c d) ())
       (receive r (split-at* '(a b c d) 5) r))
(test* "split-at* (fill)" '((a b c d #f #f) ())
       (receive r (split-at* '(a b c d) 6 #t) r))
(test* "split-at* (fill)" '((a b c d z z) ())
       (receive r (split-at* '(a b c d) 6 #t 'z) r))

(test* "take* (normal)" '(a b c)      (take* '(a b c d) 3))
(test* "take* (boundary)" '()         (take* '(a b c d) 0))
(test* "take* (boundary)" '(a b c d)  (take* '(a b c d) 4))
(test* "take* (error)" *test-error*   (take* '(a b c d) -1))
(test* "take* (shorten)" '(a b c d)   (take* '(a b c d) 5))
(test* "take* (fill)" '(a b c d #f #f) (take* '(a b c d) 6 #t))
(test* "take* (fill)" '(a b c d z z)  (take* '(a b c d) 6 #t 'z))

(test* "drop* (normal)" '(c d)       (drop* '(a b c d) 2))
(test* "drop* (boundary)" '(a b c d) (drop* '(a b c d) 0))
(test* "drop* (boundary)" '()        (drop* '(a b c d) 4))
(test* "drop* (error)" *test-error*  (drop* '(a b c d) -3))
(test* "drop* (past)" '()            (drop* '(a b c d) 5))

(test* "take-right* (normal)" '(b c d)  (take-right* '(a b c d) 3))
(test* "take-right* (boundary)" '()     (take-right* '(a b c d) 0))
(test* "take-right* (boundary)" '(a b c d) (take-right* '(a b c d) 4))
(test* "take-right* (error)" *test-error*  (take-right* '(a b c d) -1))
(test* "take-right* (shorten)" '(a b c d)  (take-right* '(a b c d) 6))
(test* "take-right* (fill)" '(z z a b c d) (take-right* '(a b c d) 6 #t 'z))

(test* "drop-right* (normal)" '(a b c)  (drop-right* '(a b c d) 1))
(test* "drop-right* (boundary)" '()     (drop-right* '(a b c d) 4))
(test* "drop-right* (boundary)" '(a b c d) (drop-right* '(a b c d) 0))
(test* "drop-right* (error)" *test-error*  (drop-right* '(a b c d) -1))
(test* "drop-right* (past)" '()         (drop-right* '(a b c d) 7))

(test* "slices (normal)" '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15))
       (slices (iota 16) 4))
(test* "slices (boundary)" '()
       (slices '() 4))
(test* "slices (short)" '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12))
       (slices (iota 13) 4))
(test* "slices (short)" '((0 1))
       (slices (iota 2) 4))
(test* "slices (fill)" '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 #f #f #f))
       (slices (iota 13) 4 #t))
(test* "slices (fill)" '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 -1 -1 -1))
       (slices (iota 13) 4 #t -1))

(test* "intersperse" '(1 + 2 + 3) (intersperse '+ '(1 2 3)))
(test* "intersperse" '(1 + 2) (intersperse '+ '(1 2)))
(test* "intersperse" '(1) (intersperse '+ '(1)))
(test* "intersperse" '() (intersperse '+ '()))

(test* "cond-list" '() (cond-list))
(test* "cond-list" '(a) (cond-list ('a)))
(test* "cond-list" '(a) (cond-list (#t 'a) (#f 'b)))
(test* "cond-list" '(b) (cond-list (#f 'a) (#t 'b)))
(test* "cond-list" '(a b d) (cond-list (#t 'a) (#t 'b) (#f 'c) (#t 'd)))
(test* "cond-list" '((b)) (cond-list (#f 'a) ('b => list)))

(test* "alist->hash-table" '(a b)
       (let ((ht (alist->hash-table '((5 . b) (3 . a)) 'eqv?)))
         (list (hash-table-get ht 3)
               (hash-table-get ht 5))))
(test* "hash-table->alist" '(("a" . 3) ("b" . 5))
       (let ((a (hash-table->alist
                 (hash-table 'equal? '("a" . 3) '("b" . 5)))))
         (list (assoc "a" a)
               (assoc "b" a))))

(test* "rassoc" '(5 . "b")
       (rassoc "b" '((3 . "a") (5 . "b"))))
(test* "rassq" '(5 . b)
       (rassq 'b '((3 . a) (5 . b))))
(test* "rassv" '("b" . 5)
       (rassoc 5 '(("a" . 3) ("b" . 5))))

(test* "assoc-ref" 5
       (assoc-ref '(("a" . 3) ("b" . 5)) "b"))
(test* "assoc-ref" 7
       (assoc-ref '(("a" . 3) ("b" . 5)) "c" 7))
(test* "assq-ref" 5
       (assq-ref '((a . 3) (b . 5)) 'b))
(test* "assq-ref" 7
       (assq-ref '((a . 3) (b . 5)) 'c 7))
(test* "assv-ref" 'b
       (assv-ref '((3 . a) (5 . b)) 5))
(test* "assv-ref" 'c
       (assv-ref '((3 . a) (5 . b)) 7 'c))

(test* "rassoc-ref" 5
       (rassoc-ref '((3 . "a") (5 . "b")) "b"))
(test* "rassoc-ref" 7
       (rassoc-ref '((3 . "a") (5 . "b")) "c" 7))
(test* "rassq-ref" 5
       (rassq-ref '((3 . a) (5 . b)) 'b))
(test* "rassq-ref" #f
       (rassq-ref '((3 . a) (5 . b)) 'c))
(test* "rassv-ref" 'b
       (rassv-ref '((a . 3) (b . 5)) 5))
(test* "rassv-ref" #f
       (rassv-ref '((a . 3) (b . 5)) 7))

(test* "assoc-set!" '(("a" . 3) ("b" . 9))
       (assoc-set! (list (cons "a" 3) (cons "b" 5)) "b" 9))
(test* "assoc-set!" '(("c" . 9) ("a" . 3) ("b" . 5))
       (assoc-set! (list (cons "a" 3) (cons "b" 5)) "c" 9))
(test* "assq-set!" '((a . 3) (b . 9))
       (assq-set! (list (cons 'a 3) (cons 'b 5)) 'b 9))
(test* "assq-set!" '((c . 9) (a . 3) (b . 5))
       (assq-set! (list (cons 'a 3) (cons 'b 5)) 'c 9))
(test* "assv-set!" '((3 . a) (5 . c))
       (assv-set! (list (cons 3 'a) (cons 5 'b)) 5 'c))
(test* "assv-set!" '((9 . c) (3 . a) (5 . b))
       (assv-set! (list (cons 3 'a) (cons 5 'b)) 9 'c))

;;-----------------------------------------------
(test-section "util.queue")
(use util.queue)
(test-module 'util.queue)

(test* "queue?" #f (queue? (cons 'a 'b)))
(test* "queue?" #f (queue? 3))
(test* "queue?" #f (queue? '()))
(define q (make-queue))

(test* "queue?" #t (queue? q))
(test* "enqueue!" #t (begin (enqueue! q 'a) (queue? q)))
(test* "enqueue!" #t (begin (enqueue! q 'b) (queue? q)))
(test* "enqueue!" #t (begin (enqueue! q 'c) (queue? q)))

(test* "queue-front" 'a (queue-front q))
(test* "queue-rear" 'c (queue-rear q))

(test* "enqueue!" '(a f)
       (begin
         (enqueue! q 'd 'e 'f)
         (list (queue-front q) (queue-rear q))))

(test* "dequeue!" 'a (dequeue! q))
(test* "dequeue!" 'b (dequeue! q))
(test* "queue-empty?" #f (queue-empty? q))
(test* "dequeue!" 'c (dequeue! q))
(test* "dequeue!" 'd (dequeue! q))
(test* "dequeue!" 'e (dequeue! q))
(test* "dequeue!" 'f (dequeue! q))
(test* "queue-empty?" #t (queue-empty? q))

(test* "queue-push!" '(c a)
       (begin
         (queue-push! q 'a) (queue-push! q 'b) (queue-push! q 'c)
         (list (queue-front q) (queue-rear q))))
(test* "queue-push!" '(f a)
       (begin
         (queue-push! q 'd 'e 'f)
         (list (queue-front q) (queue-rear q))))
(test* "queue-pop!" 'f (queue-pop! q))
(test* "queue-pop!" 'e (queue-pop! q))
(test* "queue-empty?" #f (queue-empty? q))
(test* "queue-pop!" 'd (queue-pop! q))
(test* "queue-pop!" 'c (queue-pop! q))
(test* "queue-pop!" 'b (queue-pop! q))
(test* "queue-pop!" 'a (queue-pop! q))
(test* "queue-empty?" #t (queue-empty? q))

(test* "dequeue-all!" '(a b c d e)
       (begin (enqueue! q 'a 'b 'c 'd 'e) (dequeue-all! q)))
(test* "dequeue-all!" '()
       (dequeue-all! q))
(test* "dequeue-all!" #t
       (queue-empty? q))

(test* "find-in-queue" #f
       (find-in-queue (cut eq? <> 'a) q))
(test* "find-in-queue" 'a
       (begin (enqueue! q 'a 'b 'c 'd 'e)
              (find-in-queue (cut eq? <> 'a) q)))
(test* "find-in-queue" 'c
       (find-in-queue (cut eq? <> 'c) q))
(test* "find-in-queue" 'e
       (find-in-queue (cut eq? <> 'e) q))
(test* "find-in-queue" '#f
       (find-in-queue (cut eq? <> 'f) q))

(test* "remove-from-queue!" #f
       (remove-from-queue! (cut eq? <> 'f) q))
(test* "remove-from-queue!" #t
       (remove-from-queue! (cut eq? <> 'e) q))
(test* "remove-from-queue!" #f
       (remove-from-queue! (cut eq? <> 'e) q))
(test* "remove-from-queue!" #t
       (remove-from-queue! (cut eq? <> 'a) q))
(test* "remove-from-queue!" #t
       (remove-from-queue! (cut memq <> '(b c)) q))
(test* "remove-from-queue!" #t
       (remove-from-queue! (cut eq? <> 'd) q))
(test* "remove-from-queue!" #t
       (queue-empty? q))
(test* "remove-from-queue!" #f
       (remove-from-queue! (cut eq? <> 'd) q))


(let ((q (make-queue)))
  (test* "enqueue-unique!" '("a")
         (begin (enqueue-unique! q equal? "a")
                (queue->list q)))
  (test* "enqueue-unique!" '("a" "b")
         (begin (enqueue-unique! q equal? "b")
                (queue->list q)))
  (test* "enqueue-unique!" '("a" "b")
         (begin (enqueue-unique! q equal? "a")
                (queue->list q)))
  (test* "enqueue-unique!" '("a" "b" "c" "d")
         (begin (enqueue-unique! q equal? "a" "b" "c" "d")
                (queue->list q)))
  (test* "queue-push-unique!" '("e" "a" "b" "c" "d")
         (begin (queue-push-unique! q equal? "d" "e")
                (queue->list q)))
  (set! q (make-queue))
  (test* "queue-push-unique!" '("e" "d")
         (begin (queue-push-unique! q equal? "d" "e")
                (queue->list q)))
  (test* "queue-push-unique!" '("c" "b" "a" "e" "d")
         (begin (queue-push-unique! q equal? "a" "b" "c" "d" "e")
                (queue->list q)))
  )

;;-----------------------------------------------
(test-section "util.record")
(use util.record)
(test-module 'util.record)

(define record:1 (make-record-type "record:1" '(a b c)))

(test* "make-record-type" #t
       (is-a? record:1 <class>))

(test* "make-record-type (error)" *test-error*
       (make-record-type "record:1" '((x 2) y)))

(test* "make-record-type (error)" *test-error*
       (make-record-type "record:1" '(a b a)))

(test* "record-constructor" #t
       (is-a? ((record-constructor record:1)) <record>))

(test* "record-constructor" '(7 8 9)
       (let ((obj ((record-constructor record:1 '(c b a)) 9 8 7)))
         (map (cut slot-ref obj <>) '(a b c))))

(test* "record-predicate" #t
       (let ((obj ((record-constructor record:1))))
         ((record-predicate record:1) obj)))

(test* "record-predicate" #f
       (let ((obj ((record-constructor record:1))))
         ((record-predicate record:1) (make <record>))))

(test* "record-accessor" 1
       (let ((obj ((record-constructor record:1 '(b)) 1))
             (acc (record-accessor record:1 'b)))
         (acc obj)))

(test* "record-modifier" 2
       (let ((obj ((record-constructor record:1 '(c)) 3))
             (acc (record-accessor record:1 'c))
             (mod (record-modifier record:1 'c)))
         (mod obj 2)
         (acc obj)))

;;-----------------------------------------------
(test-section "util.relation")
(use util.relation)
(test-module 'util.relation)
(use gauche.sequence)

(let ((r #f))
  (test* "simple-relation" '<simple-relation>
         (begin
           (set! r (make <simple-relation>
                     :columns '(a b c d e)
                     :rows (list
                            (vector 0 1 2 3 4)
                            (vector 5 6 7 8 9)
                            (vector 10 11 12 13 14)
                            (vector 15 16 17 18 19))))
           (class-name (class-of r))))

  (test* "simple-relation (relation-ref)" '(0 5 10 15)
         (map (cut relation-ref r <> 'a) r))

  (test* "simple-relation (relation-accessor)" '(1 6 11 16)
         (let1 ref (relation-accessor r)
           (map (cut ref <> 'b) r)))

  (test* "simple-relation (relation-column-getter)" '(2 7 12 17)
         (let1 getter (relation-column-getter r 'c)
           (map getter r)))

  (test* "simple-relation (relation-set!)" '(100 105 110 115)
         (begin
           (for-each (lambda (row)
                       (relation-set! r row 'a
                                      (+ (relation-ref r row 'a) 100)))
                     r)
           (map (cut relation-ref r <> 'a) r)))

  (test* "simple-relation (relation-modifier)" '(201 206 211 216)
         (let ((set (relation-modifier r))
               (ref (relation-accessor r)))
           (for-each (lambda (row)
                       (set row 'b (+ (ref row 'b) 200)))
                     r)
           (map (cut ref <> 'b) r)))

  (test* "simple-relation (relation-column-setter)" '(304 309 314 319)
         (let ((getter (relation-column-getter r 'e))
               (setter (relation-column-setter r 'e)))
           (for-each (lambda (row) (setter row (+ (getter row) 300))) r)
           (map getter r)))
           
  )

(define-class <object-set-test> ()
  ((a :init-value 0 :init-keyword :a)
   (b :init-value 1 :init-keyword :b)
   (c :init-value 2 :init-keyword :c)))

(let ((r #f))
  (test* "object-set-relation" '<object-set-relation>
         (begin
           (set! r (make <object-set-relation>
                     :class <object-set-test>
                     :rows (list
                            (make <object-set-test> :a 0 :b 1 :c 2)
                            (make <object-set-test> :a 3 :b 4 :c 5)
                            (make <object-set-test> :a 6 :b 7 :c 8)
                            (make <object-set-test> :a 9 :b 10 :c 11))))
           (class-name (class-of r))))

  (test* "object-set-relation (relation-ref)" '(0 3 6 9)
         (map (cut relation-ref r <> 'a) r))

  (test* "object-set-relation (relation-accessor)" '(1 4 7 10)
         (let1 ref (relation-accessor r)
           (map (cut ref <> 'b) r)))

  (test* "object-set-relation (relation-column-getter)" '(2 5 8 11)
         (let1 getter (relation-column-getter r 'c)
           (map getter r)))

  (test* "object-set-relation (relation-set!)" '(100 103 106 109)
         (begin
           (for-each (lambda (row)
                       (relation-set! r row 'a
                                      (+ (relation-ref r row 'a) 100)))
                     r)
           (map (cut relation-ref r <> 'a) r)))

  (test* "object-set-relation (relation-modifier)" '(201 204 207 210)
         (let ((set (relation-modifier r))
               (ref (relation-accessor r)))
           (for-each (lambda (row)
                       (set row 'b (+ (ref row 'b) 200)))
                     r)
           (map (cut ref <> 'b) r)))

  (test* "object-set-relation (relation-column-setter)" '(302 305 308 311)
         (let ((getter (relation-column-getter r 'c))
               (setter (relation-column-setter r 'c)))
           (for-each (lambda (row) (setter row (+ (getter row) 300))) r)
           (map getter r)))
           
  )
;;-----------------------------------------------
(test-section "util.stream")
(use util.stream)
(test-module 'util.stream)

(test* "stream?" #t
       (stream? (stream-cons 'a stream-null)))

(test* "stream-cons/car/cdr" '(1 1 1)
       (letrec ((s (stream-cons 1 s)))
         (list (stream-car s)
               (stream-cadr s)
               (stream-caddr s))))

(test* "stream-delay" #t
       (stream? (stream-delay (error "Boo"))))

(test* "stream-unfoldn" '((0 2 4 6) (1 3 5 7))
       (receive (s0 s1)
           (stream-unfoldn (lambda (s)
                             (values (+ s 2)
                                     (list s)
                                     (list (+ s 1))))
                           0 2)
         (map (lambda (s)
                (list (stream-first s)
                      (stream-second s)
                      (stream-third s)
                      (stream-fourth s)))
              (list s0 s1))))

(test* "stream-map/stream-ref" '(2 4 6 8)
       (let1 s
           (stream-map (cut * 2 <>)
                       (stream-unfoldn (lambda (s) (values (+ s 1) (list s)))
                                       0 1))
         (map (cut stream-ref s <>) '(1 2 3 4))))

(test* "stream-for-each/stream-iota" '(1 3 5 7)
       (let1 r '()
         (stream-for-each (lambda (x y)
                            (push! r (+ x y)))
                          (stream-iota 4)
                          (stream-iota -1 1))
         (reverse r)))

(test* "stream-filter/take/drop" '(12 15 18 21)
       (let1 s (stream-filter (lambda (x) (zero? (modulo x 3)))
                              (stream-iota #f))
         (stream->list (stream-take (stream-drop s 4) 4))))

;;-----------------------------------------------
(test-section "util.toposort")
(use util.toposort)
(test-module 'util.toposort)


;; using famous socks example (Corman)
(test* "topological-sort"
       '(socks undershorts watch shirt tie pants belt jacket shoes)
       (topological-sort '((shirt tie belt)
                           (tie jacket)
                           (belt jacket)
                           (watch)
                           (pants shoes belt)
                           (undershorts pants shoes)
                           (socks shoes))
                         eq?))

(test* "topological-sort"
       '("socks" "undershorts" "watch" "shirt" "tie" "pants" "belt" "jacket" "shoes")
       (topological-sort '(("shirt" "tie" "belt")
                           ("tie" "jacket")
                           ("belt" "jacket")
                           ("watch")
                           ("pants" "shoes" "belt")
                           ("undershorts" "pants" "shoes")
                           ("socks" "shoes"))
                         equal?))

(test-end)
