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

(let1 bm (make-bimap (make-hash-table 'eqv?) (make-hash-table 'eqv?))
  (bimap-put! bm 'a 1)
  (test* "bimap conflict (left)" (test-error)
         (bimap-put! bm 'a 2 :on-conflict :error))
  (test* "bimap conflict (right)" (test-error)
         (bimap-put! bm 'b 1 :on-conflict :error))
  (test* "bimap conflict (ignore)" 1
         (begin
           (bimap-put! bm 'a 3 :on-conflict #f)
           (bimap-left-get bm 'a))))

(let1 bm (make-bimap (make-hash-table 'eqv?) (make-hash-table 'eqv?)
                     :on-conflict :error)
  (bimap-put! bm 'a 1)
  (test* "bimap conflict default (left)" (test-error)
         (bimap-put! bm 'a 2))
  (test* "bimap conflict default (right)" (test-error)
         (bimap-put! bm 'b 1))
  (test* "bimap conflict default override (ignore)" 3
         (begin
           (bimap-put! bm 'a 3 :on-conflict :supersede)
           (bimap-left-get bm 'a))))

(test-section "stacked map")

(let* ([m0 (alist->hash-table '((a . 0) (b . 1) (c . 2)) 'eq?)]
       [m1 (alist->hash-table '((a . 10) (d . 11)) 'eq?)]
       [sm (make-stacked-map m1 m0)])
  (test* "stacked map search" '(10 1 2 11 none)
         (map (cut dict-get sm <> 'none) '(a b c d e)))
  (test* "stacked map search" (test-error)
         (dict-get sm 'e))
  (test* "stacked map exists?" '(#t #t #f)
         (map (cut dict-exists? sm <>) '(a c e)))
  (test* "stacked-map put!" '(12 12 2)
         (begin (dict-put! sm 'c 12)
                (list (dict-get sm 'c)
                      (dict-get m1 'c)
                      (dict-get m0 'c))))
  (test* "stacked-map fold" '((b . 1) (a . 10) (d . 11) (c . 12))
         (sort-by (dict-fold sm acons '()) cdr))
  (test* "stacked-map delete"'(#f #f #f)
         (begin (dict-delete! sm 'c)
                (list (dict-get sm 'c #f)
                      (dict-get m1 'c #f)
                      (dict-get m0 'c #f))))

  (test* "stacked-map push!"
         '((d . 11) (a . 100) (b . 101) (c . 102) (e . 103))
         (begin (stacked-map-push! sm (alist->hash-table '((a . 100)
                                                           (b . 101)
                                                           (c . 102)
                                                           (e . 103))))
                (sort-by (dict->alist sm) cdr)))
  )

(test-end)

