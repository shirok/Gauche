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
(test-section "util.rbtree")
(use util.rbtree)
(use gauche.sequence)
(use srfi-27)
(test-module 'util.rbtree)

(define rbtree-check
  (with-module util.rbtree rbtree-check))

(let1 tree1 #f
  (test* "make-rbtree" #t
         (begin (set! tree1 (make-rbtree = <))
                (rbtree? tree1)))
  (test* "rbtree-get" (test-error)
         (rbtree-get #f 0 'foo))
  (test* "rbtree-get" 'not-found
         (rbtree-get tree1 0 'not-found))
  (test* "rbtree-get" (test-error)
         (rbtree-get tree1 0))
  (test* "rbtree-put!" (test-error)
         (rbtree-put! #f 0 'foo))
  (test* "rbtree-put!" "0"
         (begin (rbtree-put! tree1 0 "0")
                (rbtree-get tree1 0)))
  (test* "rbtree-put!" '("0" "1")
         (begin (rbtree-put! tree1 1 "1")
                (list (rbtree-get tree1 0)
                      (rbtree-get tree1 1))))
  (test* "rbtree-put!" 'bar
         (begin (rbtree-put! tree1 2 'foo)
                (rbtree-put! tree1 2 'bar)
                (rbtree-get tree1 2)))
  (test* "rbtree-check" #t
         (rbtree-check tree1))
  (test* "rbtree-fold" '(2 bar 1 "1" 0 "0")
         (rbtree-fold tree1 list* '()))
  (test* "rbtree-fold-right" '(0 "0" 1 "1" 2 bar)
         (rbtree-fold-right tree1 list* '()))
  (test* "rbtree-delete! (exiting key)" '(#t not-found)
         (let1 r (rbtree-delete! tree1 1)
           (list r (rbtree-get tree1 1 'not-found))))
  (test* "rbtree-delete! (non-existing key)" #f
         (rbtree-delete! tree1 1))
  (test* "rbtree-delete!" 'no-error
         (begin (rbtree-delete! tree1 1)
                'no-error))
  (test* "rbtree->alist" '()
         (rbtree->alist (make-rbtree = <)))
  (test* "rbtree->alist" '((0 . "0") (1 . "1") (2 . "2"))
         (let1 tree (make-rbtree = <)
           (for-each (lambda (p) (rbtree-put! tree (car p) (cdr p)))
                     '((0 . "0") (1 . "1") (2 . "2")))
           (rbtree->alist tree)))
  (test* "alist->rbtree" '((0 . "0") (1 . "1") (2 . "2"))
         (rbtree->alist
          (alist->rbtree '((0 . "0") (1 . "1") (2 . "2")) = <)))
  (test* "rbtree-empty?" #f
         (rbtree-empty? tree1))
  (test* "rbtree-empty?" #t
         (rbtree-empty? (make-rbtree < =)))
  (test* "rbtree-empty?" (test-error)
         (rbtree-empty? 'wrong-arg))
  (test* "rbtree-exists?" '(#t #f)
         (let1 tree (make-rbtree = <)
           (rbtree-put! tree 1 'foo)
           (map (cut rbtree-exists? tree <>)
                '(1 2))))
  (test* "rbtree-num-entries" '(0 1 0)
         (let* ((t (make-rbtree = <))
                (a (rbtree-num-entries t))
                (b (begin (rbtree-put! t 7 7)
                          (rbtree-num-entries t)))
                (c (begin (rbtree-delete! t 7)
                          (rbtree-num-entries t))))
           (list a b c)))
  (test* "rbtree-push!" '(bar foo)
         (let1 tree (make-rbtree = <)
           (rbtree-push! tree 1 'foo)
           (rbtree-push! tree 1 'bar)
           (rbtree-get tree 1)))
  (test* "rbtree-pop!" '(foo bar)
         (let1 tree (alist->rbtree '((1 foo bar)) = <)
           (let1 r (rbtree-pop! tree 1)
             (list r (rbtree-pop! tree 1)))))
  (test* "rbtree-update!" 2
         (let1 tree (make-rbtree = <)
           (rbtree-update! tree 1 (cut + 1 <>) 0)
           (rbtree-update! tree 1 (cut + 1 <>) 0)
           (rbtree-get tree 1)))
  )

;; iterators
(let ((empty (make-rbtree = <))
      (tree2 (alist->rbtree '((1 . "1") (0 . "0") (2 . "2")) = <)))

  (test* "rbtree-min" '(0 . "0") (rbtree-min tree2))
  (test* "rbtree-max" '(2 . "2") (rbtree-max tree2))
  (test* "rbtree-min" (test-error) (rbtree-min 'wrong-arg))
  (test* "rbtree-min" (test-error) (rbtree-min 'wrong-arg))
  (test* "rbtree-min" 'default (rbtree-min empty 'default))
  (test* "rbtree-max" 'default (rbtree-max empty 'default))

  (test* "rbtree-keys" '(0 1 2)
         (rbtree-keys tree2))

  (test* "rbtree-values" '("0" "1" "2")
         (rbtree-values tree2))

  (test* "rbtree-copy" #t
         (let1 new (rbtree-copy tree2)
           (rbtree-check new)
           (equal? (rbtree->alist tree2)
                   (rbtree->alist new))))

  (test* "for-each" (rbtree->alist tree2)
         (let1 r '()
           (for-each (lambda (e) (push! r e)) tree2)
           (reverse! r)))

  (test* "map" (rbtree->alist tree2)
         (map values tree2))

  (test* "fold" (rbtree->alist tree2)
         (reverse! (fold cons '() tree2)))

  (test* "fold-right" (rbtree->alist tree2)
         (fold-right cons '() tree2))

  (test* "ref" '("0" "1" "2")
         (map (cut ref tree2 <>) '(0 1 2)))

  (test* "rbtree-extract-min!" '((0 . "0") (1 . "1"))
         (let1 r (rbtree-extract-min! tree2)
           (list r (rbtree-min tree2))))

  (test* "rbtree-extract-max!" '((2 . "2") (1 . "1"))
         (let1 r (rbtree-extract-max! tree2)
           (list r (rbtree-max tree2))))
  )

;; collection interface
(let1 tree4 (alist->rbtree '(("0" . 0) ("1" . 1) ("2" . 1))
                           string=? string<?)
  (test* "map" '(("0" . 0) ("1" . 1) ("2" . 1))
         (map values tree4))

  (test* "call-with-builder" '(("0" . 0) ("1" . 1) ("2" . 1))
         (call-with-builder <rbtree>
                            (lambda (add! get)
                              (for-each add! '(("0" . 0) ("1" . 1) ("2" . 1)))
                              (rbtree->alist (get)))
                            :key=? string=? :key<? string<?))
  )

;; random insertion/deletion

(let1 tree4 (make-rbtree = <)
  (random-source-randomize! default-random-source)

  (test* "random insertion/deletion" 'done
         (begin (dotimes (_ 5)
                  (dotimes (_ 30)
                    (let1 i (random-integer 30)
                      (rbtree-put! tree4 i i)
                      (rbtree-check tree4)))
                  (dotimes (_ 30)
                    (let1 i (random-integer 30)
                      (rbtree-delete! tree4 i)
                      (rbtree-check tree4))))
                'done))
  )


;;-----------------------------------------------
(test-section "util.record")
(use util.record)
(test-module 'util.record)

(define record:1 (make-record-type "record:1" '(a b c)))

(test* "make-record-type" #t
       (is-a? record:1 <class>))

(test* "make-record-type (error)" (test-error)
       (make-record-type "record:1" '((x 2) y)))

(test* "make-record-type (error)" (test-error)
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

  (test* "simple-relation (relation-fold)" 1264
         (relation-fold r + 0 'a 'b))
           
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
           
  (test* "simple-relation (relation-fold)" 1644
         (relation-fold r + 0 'a 'c))
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

(test* "stream-remove" '(0 2 4 6)
       (stream->list (stream-remove odd? (stream-iota 8))))

(test* "stream-count" 4
       (stream-count odd? (stream-iota 8)))

(test* "stream-any" #f
       (stream-any even? stream-null))

(test* "stream-any" #f
       (stream-any even? (list->stream '(1 3))))

(test* "stream-any" #t
       (stream-any even? (list->stream '(1 2))))

(test* "stream-any" 1
       (stream-any string->number (list->stream '("1" "a"))))

(test* "stream-any" 1
       (stream-any string->number (list->stream '("1" "2"))))

(test* "stream-any" 1
       (stream-any string->number
                   (list->stream '("1" "2"))
                   (list->stream '(10 10))))

(test* "stream-any" 1
       (stream-any string->number
                   (list->stream '("1" "2"))
                   (list->stream '(10))))

(test* "stream-any" #f
       (stream-any string->number
                   (list->stream '("1" "2"))
                   (list->stream '())))

(test* "stream-every" #t
       (stream-every odd? stream-null))

(test* "stream-every" #t
       (stream-every odd? (list->stream '(1 3))))

(test* "stream-every" #f
       (stream-every odd? (list->stream '(1 2))))

(test* "stream-every" #f
       (stream-every string->number (list->stream '("1" "a"))))

(test* "stream-every" 2
       (stream-every string->number (list->stream '("1" "2"))))

(test* "stream-every" 2
       (stream-every string->number
                     (list->stream '("1" "2"))
                     (list->stream '(10 10))))

(test* "stream-every" 1
       (stream-every string->number
                     (list->stream '("1" "2"))
                     (list->stream '(10))))

(test* "stream-every" #t
       (stream-every string->number
                   (list->stream '("1" "2"))
                   (list->stream '())))

;;-----------------------------------------------
(test-section "util.toposort")
(use util.toposort)
(test-module 'util.toposort)


;; using famous socks example (Corman)
(let ([sorted '(socks undershorts watch shirt tie pants belt jacket shoes)]
      [input '((shirt tie belt)
               (tie jacket)
               (belt jacket)
               (watch)
               (pants shoes belt)
               (undershorts pants shoes)
               (socks shoes))])

  (test* "topological-sort, default" sorted
         (topological-sort input))

  (test* "topological-sort, eq?" sorted
         (topological-sort input eq?))

  (test* "topological-sort, string=?"
         (map symbol->string sorted)
         (topological-sort (map (cut map symbol->string <>) input) string=?))
  )

;; cycle
(test* "topological-sort: error" (test-error)
       (topological-sort '((watch tie)
			   (tie watch))))
(test* "topological-sort: error" (test-error)
       (topological-sort '((shirt watch)
			   (watch tie)
			   (tie watch))))

;;-----------------------------------------------
(test-section "util.trie")
(use util.trie)
(test-module 'util.trie)
(use gauche.uvector)
(use srfi-1)
(use srfi-13)

(let* ((strs '("kana" "kanaono" "kanawai" "kanawai koa"
               "kanawai mele" "kane" "Kane" "kane make" "kane makua"
               "ku" "kua" "kua`aina" "kua`ana"
               "liliko`i" "lilinoe" "lili`u" "lilo" "maoli" ""))
       (lists (map string->list strs))
       (vecs  (map list->vector lists))
       (uvecs (map string->u8vector strs)))

  ;; string trie tests
  (let1 t1 (make-trie)
    (test* "trie: constructor" '(#t 0)
           (list (trie? t1) (trie-num-entries t1)))
    (test* "trie: exists?" #f (trie-exists? t1 "kane"))

    (test* "trie: put!" 1
           (begin (trie-put! t1 "lilo" 4)
                  (trie-num-entries t1)))
    (test* "trie: get" 4
           (trie-get t1 "lilo"))
    (test* "trie: get (error)" (test-error)
           (trie-get t1 "LILO"))
    (test* "trie: get (fallback)" 'foo
           (trie-get t1 "LILO" 'foo))

    (test* "trie: put! more" (length strs)
           (begin (for-each (lambda (s)
                              (trie-put! t1 s (string-length s)))
                            strs)
                  (trie-num-entries t1)))
    (test* "trie: get more" #t
           (every (lambda (s)
                    (= (trie-get t1 s) (string-length s)))
                  strs))
    (test* "trie: exists? more" #t
           (every (cut trie-exists? t1 <>) strs))
    (test* "trie: common-prefix" '(19 12 8 4 4 3)
           (map (lambda (p) (length (trie-common-prefix t1 p)))
                '("" "k" "ka" "ku" "li" "lili")))
    (test* "trie: common-prefix" '(("kua" . 3)
                                      ("kua`aina" . 8)
                                      ("kua`ana" . 7))
           (trie-common-prefix t1 "kua")
           (cut lset= equal? <> <>))
    (test* "trie: common-prefix-keys" '("kua" "kua`aina" "kua`ana")
           (trie-common-prefix-keys t1 "kua")
           (cut lset= equal? <> <>))
    (test* "trie: common-prefix-values" '(3 8 7)
           (trie-common-prefix-values t1 "kua")
           (cut lset= = <> <>))
    (test* "trie: common-prefix-fold" 18
           (trie-common-prefix-fold t1 "kua"
                                    (lambda (k v s) (+ v s))
                                    0))
    (test* "trie: common-prefix-map" '("KUA" "KUA`AINA" "KUA`ANA")
           (trie-common-prefix-map t1 "kua"
                                   (lambda (k v) (string-upcase k)))
           (cut lset= equal? <> <>))
    (test* "trie: common-prefix-for-each" '("KUA" "KUA`AINA" "KUA`ANA")
           (let1 p '()
             (trie-common-prefix-for-each t1 "kua"
                                          (lambda (k v)
                                            (push! p (string-upcase k))))
             p)
           (cut lset= equal? <> <>))
    (test* "trie: trie-fold" (fold (lambda (k s) (+ (string-length k) s))
                                      0 strs)
           (trie-fold t1 (lambda (k v s) (+ v s)) 0))
    (test* "trie: trie-map" (fold (lambda (k s) (+ (string-length k) s))
                                     0 strs)
           (apply + (trie-map t1 (lambda (k v) v))))
    (test* "trie: trie-for-each"
           (fold (lambda (k s) (+ (string-length k) s))
                 0 strs)
           (let1 c 0 (trie-for-each t1 (lambda (k v) (inc! c v))) c))
    (test* "trie: trie->list"
           (map (lambda (s) (cons s (string-length s))) strs)
           (trie->list t1)
           (cut lset= equal? <> <>))
    (test* "trie: trie-keys"
           strs
           (trie-keys t1)
           (cut lset= equal? <> <>))
    (test* "trie: trie-values"
           (map string-length strs)
           (trie-values t1)
           (cut lset= equal? <> <>))
    (test* "trie: trie-update!" 16
           (begin (trie-update! t1 "liliko`i" (cut + <> 8))
                  (trie-get t1 "liliko`i")))
    (test* "trie: trie-update! (nonexistent)" (test-error)
           (trie-update! t1 "humuhumu" (cut + <> 8)))
    (test* "trie: trie-update! (nonexistent)" 16
           (begin (trie-update! t1 "humuhumu" (cut + <> 8) 8)
                  (trie-get t1 "humuhumu")))
    (test* "trie: delete!" '(19 #f)
           (begin (trie-delete! t1 "humuhumu")
                  (list (trie-num-entries t1)
                        (trie-get t1 "humuhumu" #f))))
    (test* "trie: delete! (nonexistent)" '(19 #f)
           (begin (trie-delete! t1 "HUMUHUMU")
                  (list (trie-num-entries t1)
                        (trie-get t1 "HUMUHUMU" #f))))
    (test* "trie: delete! (everything)" 0
           (begin (for-each (cut trie-delete! t1 <>) strs)
                  (trie-num-entries t1)))
    )
  ;; trie and trie-with-keys
  (let1 t2 (trie '() '("foo" . 0) '("foof" . 1) '("far" . 2))
    (test* "trie: trie" '(("foo" . 0) ("foof" . 1) ("far" . 2))
           (trie->list t2)
           (cut lset= equal? <> <>)))
  (let1 t3 (trie-with-keys '() "foo" "foof" "far")
    (test* "trie: trie-with-keys"
           '(("foo" . "foo") ("foof" . "foof") ("far" . "far"))
           (trie->list t3)
           (cut lset= equal? <> <>)))

  ;; heterogeneous tries
  (let1 t4 (make-trie)
    (for-each (cut for-each
                   (lambda (seq)
                     (trie-put! t4 seq (class-of seq)))
                   <>)
              (list strs lists vecs uvecs))
    (test* "trie(hetero): put!" (* 4 (length strs))
           (trie-num-entries t4))
    (test* "trie(hetero): get" <vector> (trie-get t4 '#()))
    (test* "trie(hetero): get" <u8vector> (trie-get t4 '#u8()))
    (test* "trie(hetero): get" <pair> (trie-get t4 '(#\k #\u)))

    (test* "trie(hetero): delete!" <string>
           (begin (trie-delete! t4 '()) (trie-get t4 "")))
    (test* "trie(hetero): delete!" (* 3 (length strs))
           (begin (for-each (cut trie-delete! t4 <>) lists)
                  (trie-num-entries t4)))
    )

  ;; customizing tables
  (let1 t5 (make-trie list
                      (cut assoc-ref <> <> #f char-ci=?)
                      (lambda (t k v)
                        (if v
                          (assoc-set! t k v char-ci=?)
                          (alist-delete! k t char-ci=?)))
                      (lambda (t f s) (fold f s t)))
    (test* "trie(custom): put!" (- (length strs) 1)
           (begin
             (for-each (lambda (s) (trie-put! t5 s (string-length s))) strs)
             (trie-num-entries t5)))
    (test* "trie(custom): get" 99
           (begin
             (trie-put! t5 "LILIKO`I" 99)
             (trie-get t5 "liliko`i")))
    )

  ;; collection api
  (let1 t6 #f
    (test* "trie(collection): builder" (length strs)
           (begin
             (set! t6 (coerce-to <trie> (map (cut cons <> #t) strs)))
             (and (trie? t6) (size-of t6))))
    (test* "trie(collection): iterator" strs
           (let1 p '()
             (call-with-iterator t6
                                 (lambda (end next)
                                   (until (end)
                                     (push! p (car (next))))))
             p)
           (cut lset= equal? <> <>))
    (test* "trie(collection): coerce to list" (map (cut cons <> #t) strs)
           (coerce-to <list> t6)
           (cut lset= equal? <> <>))
    (test* "trie(collection): coerce to vector"
           (map (cut cons <> #t) strs)
           (vector->list (coerce-to <vector> t6))
           (cut lset= equal? <> <>))
    (test* "trie(collection): coerce to hashtable" #t
           (let1 h (coerce-to <hash-table> t6)
             (every (cut hash-table-get h <>) strs)))
    )
  )

(test-end)
