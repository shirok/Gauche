;;
;; test dictionary framework
;;

(use gauche.test)
(use srfi-1)

(test-start "dictionary framework")
(use gauche.dictionary)
(test-module 'gauche.dictionary)

(define (test-basics d)
  (dict-put! d 'a 1)
  (set! (dict-get d 'b) 2)
  (test* "put/get" 1 (dict-get d 'a))
  (test* "put/get" 2 (dict-get d 'b))
  (test* "get nonexistent" (test-error) (dict-get d 'c))
  (test* "get default" 3 (dict-get d 'c 3))
  (test* "fold" '((a . 1) (b . 2)) (dict-fold d acons '())
         (cut lset= equal? <> <>))
  (test* "exists?" #f (dict-exists? d 'c))
  (test* "exists?" #t (dict-exists? d 'a))
  (test* "delete!" #f (begin (dict-delete! d 'a) (dict-exists? d 'a)))
  )

(test-section "hash-table as dictionary")

(test-basics (make-hash-table 'eq?))

(test-section "tree-map as dictionary")

(test-basics
 (make-tree-map eq? (^[a b] (string<? (x->string a) (x->string b)))))

(test-section "bimap")

(test-basics (make-bimap (make-hash-table 'eq?) (make-hash-table 'eqv?)))

(use gauche.collection)

(let1 bm (make-bimap (make-hash-table 'eq?) (make-hash-table 'eqv?))
  (bimap-put! bm 'a 1)
  (bimap-put! bm 'b 2)
  (test* "right lookup" '(a b)
         (list (bimap-right-get bm 1)
               (bimap-right-get bm 2)))

  (bimap-put! bm 'c 3)
  (bimap-put! bm 'd 1)
  (test* "left lookup" '(3 1)
         (list (bimap-left-get bm 'c)
               (bimap-left-get bm 'd)))

  (test* "override by reverse insertion" #f
         (bimap-left-get bm 'a #f)) ; must be overridden by right-put! 1 'd

  ;; collection framework
  (test* "collection protocol" '((b . 2) (c . 3) (d . 1))
         (fold cons '() bm)
         (cut lset= equal? <> <>))
  )

(test-end)

