;;
;; test for tree-map
;;

(use gauche.test)
(test-start "treemap")

(define %tree-map-check-consistency
  (with-module gauche.internal %tree-map-check-consistency))

;; Basic stuff
(define (do-tree-map ctor)
  (let1 tree1 #f
    (test* "make-tree-map" #t
           (begin (set! tree1 (ctor)) (tree-map? tree1)))
    (test* "tree-map-get" (test-error)
           (tree-map-get #f 0 'foo))
    (test* "tree-map-get" 'not-found
           (tree-map-get tree1 0 'not-found))
    (test* "tree-map-get" (test-error)
           (tree-map-get tree1 0))
    (test* "tree-map-put!" (test-error)
           (tree-map-put! #f 0 'foo))
    (test* "tree-map-put!" "0"
           (begin (tree-map-put! tree1 0 "0")
                  (tree-map-get tree1 0)))
    (test* "tree-map-put!" '("0" "1")
           (begin (tree-map-put! tree1 1 "1")
                  (list (tree-map-get tree1 0)
                        (tree-map-get tree1 1))))
    (test* "tree-map-put!" 'bar
           (begin (tree-map-put! tree1 2 'foo)
                  (tree-map-put! tree1 2 'bar)
                  (tree-map-get tree1 2)))
    (test* "tree-map-adjoin! (no overwrite)" 'bar
           (begin (tree-map-adjoin! tree1 2 'foo)
                  (tree-map-get tree1 2)))
    (test* "tree-map-replace! (nonexistent)" #f
           (begin (tree-map-replace! tree1 3 'foo)
                  (tree-map-get tree1 3 #f)))
    (test* "tree-map-adjoin! (nonexistent)" 'baz
           (begin (tree-map-adjoin! tree1 3 'baz)
                  (tree-map-get tree1 3)))
    (test* "tree-map-replace! (exists)" 'boo
           (begin (tree-map-replace! tree1 3 'boo)
                  (tree-map-get tree1 3)))
    (test* "tree-map-fold" '(3 boo 2 bar 1 "1" 0 "0")
           (tree-map-fold tree1 list* '()))
    (test* "tree-map-fold-right" '(0 "0" 1 "1" 2 bar 3 boo)
           (tree-map-fold-right tree1 list* '()))
    (test* "tree-map-delete! (exiting key)" '(#t not-found)
           (let1 r (tree-map-delete! tree1 1)
             (list r (tree-map-get tree1 1 'not-found))))
    (test* "tree-map-delete! (non-existing key)" #f
           (tree-map-delete! tree1 1))
    (test* "tree-map-delete!" 'no-error
           (begin (tree-map-delete! tree1 1)
                  'no-error))
    (test* "tree-map->alist" '()
           (tree-map->alist (ctor)))
    (test* "tree-map->alist" '((0 . "0") (1 . "1") (2 . "2"))
           (let1 tree (ctor)
             (for-each (^p (tree-map-put! tree (car p) (cdr p)))
                       '((0 . "0") (1 . "1") (2 . "2")))
             (tree-map->alist tree)))
    (test* "alist->tree-map" '((0 . "0") (1 . "1") (2 . "2"))
           (tree-map->alist
            (alist->tree-map '((0 . "0") (1 . "1") (2 . "2")) = <)))
    (test* "tree-map-empty?" #f
           (tree-map-empty? tree1))
    (test* "tree-map-empty?" #t
           (tree-map-empty? (ctor)))
    (test* "tree-map-empty?" (test-error)
           (tree-map-empty? 'wrong-arg))
    (test* "tree-map-exists?" '(#t #f)
           (let1 tree (ctor)
             (tree-map-put! tree 1 'foo)
             (map (cut tree-map-exists? tree <>)
                  '(1 2))))
    (test* "tree-map-num-entries" '(0 1 0)
           (let* ((t (ctor))
                  (a (tree-map-num-entries t))
                  (b (begin (tree-map-put! t 7 7)
                            (tree-map-num-entries t)))
                  (c (begin (tree-map-delete! t 7)
                            (tree-map-num-entries t))))
             (list a b c)))
    (test* "tree-map-push!" '(bar foo)
           (let1 tree (ctor)
             (tree-map-push! tree 1 'foo)
             (tree-map-push! tree 1 'bar)
             (tree-map-get tree 1)))
    (test* "tree-map-pop!" '(foo bar)
           (let1 tree (alist->tree-map '((1 foo bar)) = <)
             (let1 r (tree-map-pop! tree 1)
               (list r (tree-map-pop! tree 1)))))
    (test* "tree-map-update!" 2
           (let1 tree (ctor)
             (tree-map-update! tree 1 (cut + 1 <>) 0)
             (tree-map-update! tree 1 (cut + 1 <>) 0)
             (tree-map-get tree 1)))
    ))

(do-tree-map (cut make-tree-map = <))
(do-tree-map (cut make-tree-map (^[a b] (cond [(< a b) -1][(= a b) 0][else 1]))))
(do-tree-map (cut make-tree-map))

;; Min, max, iterators
(let ((empty (make-tree-map = <))
      (tree2 (alist->tree-map '((1 . "1") (0 . "0") (2 . "2")) = <)))

  (test* "tree-map-min" '(0 . "0") (tree-map-min tree2))
  (test* "tree-map-max" '(2 . "2") (tree-map-max tree2))
  (test* "tree-map-min" (test-error) (tree-map-min 'wrong-arg))
  (test* "tree-map-max" (test-error) (tree-map-max 'wrong-arg))

  (test* "tree-map-keys" '(0 1 2)
         (tree-map-keys tree2))

  (test* "tree-map-values" '("0" "1" "2")
         (tree-map-values tree2))

  (test* "tree-map-copy" #t
         (let1 new (tree-map-copy tree2)
           (%tree-map-check-consistency new)
           (equal? (tree-map->alist tree2)
                   (tree-map->alist new))))

  (test* "tree-map-pop-min!" '((0 . "0") (1 . "1"))
         (let1 r (tree-map-pop-min! tree2)
           (list r (tree-map-min tree2))))

  (test* "tree-map-pop-max!" '((2 . "2") (1 . "1"))
         (let1 r (tree-map-pop-max! tree2)
           (list r (tree-map-max tree2))))
  )

;; The following test sequence is carefully assembled so that
;; it goes through every path in the rbtree manipulation routine.
;; The "case" numbers corresponds to BALANCE_CASE/DELETE_CASE macros
;; in treemap.c.

(let1 tree (make-tree-map = <)
  (define (i . args) (dolist (k args) (tree-map-put! tree k k)))
  (define (d . args) (dolist (k args) (tree-map-delete! tree k)))
  (define (c) (%tree-map-check-consistency tree))
  
  ;; Insertion
  ;;  case 0. adding to an empty tree
  ;;    B:0
  (test* "insertion case 0" #t (begin (i 0) (c)))

  ;;  case 2. adding to a black parent.
  ;;    B:0
  ;;      R:1
  (test* "insetrion case 2" #t (begin (i 1) (c)))

  ;;  case 3&1. adding to a red parent, while uncle is also red.
  ;;        R:-2
  ;;      B:-1
  ;;    B:0
  ;;      B:1
  ;;        R:2
  (test* "insertion case 3&1" #t (begin (i -1 -2 2) (c)))

  ;;  case 5b. adding to a red parent, while uncle is black.
  ;;           new node is on the right side of parent,
  ;;           while parent is the left side of grandparent.
  ;;           this goes through rotate_left, then rotate_right.
  ;;        R:-2 => -2
  ;;      B:-1 => -1
  ;;    B:0 => 0
  ;;        R:1 => 1
  ;;      B:1.5 => 1.5
  ;;        R:2 => 2
  (test* "insertion case 5b" #t (begin (i 1.5) (c)))

  ;;  case 5a. same as 3a except left and right is swapped.
  ;;        R:-2 => -2
  ;;      B:-1.5 => -1.5
  ;;        R:-1 => -1
  ;;    B:0 => 0
  ;;        R:1 => 1
  ;;      B:1.5 => 1.5
  ;;        R:2 => 2
  (test* "insertion case 5a" #t (begin (i -1.5) (c)))

  ;; Deletion.
  ;;   case 1.  deleting the leaf red node.
  ;;        R:-2 => -2
  ;;      B:-1.5 => -1.5
  ;;        R:-1 => -1
  ;;    B:0 => 0
  ;;        R:1 => 1
  ;;      B:1.5 => 1.5
  (test* "deletion case 1" #t (begin (d 2) (c)))

  ;;   prepare for next test
  ;;          R:-3 => -3
  ;;        B:-2 => -2
  ;;      R:-1.5 => -1.5
  ;;        B:-1 => -1
  ;;    B:0 => 0
  ;;        B:1 => 1
  ;;      R:1.5 => 1.5
  ;;        B:2 => 2
  ;;          R:3 => 3
  (i 2 3 -2 -3)

  ;;   case 8a.  deleting the leaf black node.
  ;;             parent is red, sibling is black.
  ;;          R:-3 => -3
  ;;        B:-2 => -2
  ;;      R:-1.5 => -1.5
  ;;        B:-1 => -1
  ;;    B:0 => 0
  ;;        B:1.5 => 1.5
  ;;      R:2 => 2
  ;;        B:3 => 3
  (test* "deletion case 8a" #t (begin (d 1) (c)))

  ;;   case 8b.  same, except left/right swapped
  ;;        B:-3 => -3
  ;;      R:-2 => -2
  ;;        B:-1.5 => -1.5
  ;;    B:0 => 0
  ;;        B:1.5 => 1.5
  ;;      R:2 => 2
  ;;        B:3 => 3
  (test* "deletion case8b" #t (begin (d -1) (c)))

  ;;   case 6.  deleting red node w/ both children
  ;;            In our implementation, the node is first replaced by
  ;;            its previous node until we get a single-child case,
  ;;            then the balancing is applied.  In this particular case
  ;;            it degenerates to the deletion of a black node whose
  ;;            parent is red and whose sibling is black.
  ;;        B:-3 => -3
  ;;      R:-2 => -2
  ;;        B:-1.5 => -1.5
  ;;    B:0 => 0
  ;;      B:1.5 => 1.5
  ;;        R:3 => 3
  (test* "deletion case6" #t (begin (d 2) (c)))

  ;;   case 2.  deleting black node, whose parent is black and sibing is red.
  ;;        B:-3 => -3
  ;;      R:-2 => -2
  ;;        B:-1.5 => -1.5
  ;;    B:0 => 0
  ;;      B:3 => 3
  (test* "deletion case2" #t (begin (d 1.5) (c)))

  ;; preparation
  ;;        B:-3 => -3
  ;;      B:-2 => -2
  ;;        B:-1.5 => -1.5
  ;;    B:0 => 0
  ;;          R:0.5 => 0.5
  ;;        B:1 => 1
  ;;      B:2 => 2
  ;;          B:2.5 => 2.5
  ;;        R:3 => 3
  ;;          B:3.5 => 3.5
  ;;            R:4 => 4
  (i 2 1 .5 2.5 3.5 4)

  ;;   case 5.  deleting black node, whose parent and sibling is black
  ;;            and sibling's both child is black.
  ;;        B:-2 => -2
  ;;          R:-1.5 => -1.5
  ;;      B:0 => 0
  ;;          R:0.5 => 0.5
  ;;        B:1 => 1
  ;;    B:2 => 2
  ;;        B:2.5 => 2.5
  ;;      B:3 => 3
  ;;        B:3.5 => 3.5
  ;;          R:4 => 4
  (test* "deletion case5" #t (begin (d -3) (c)))

  ;; preparation
  ;;          B:-2 => -2
  ;;        R:-1.5 => -1.5
  ;;            R:-1 => -1
  ;;          B:-0.5 => -0.5
  ;;      B:0 => 0
  ;;          R:0.5 => 0.5
  ;;        B:1 => 1
  ;;    B:2 => 2
  ;;        B:2.5 => 2.5
  ;;      B:3 => 3
  ;;        B:3.5 => 3.5
  ;;          R:4 => 4
  (i -0.5 -1)

  ;;   case 7a  deleting black node, which is a left child of its parent,
  ;;            and its sibling is black, and sibling's left child is red.
  ;;            This goes through a nested if-statement in delete_node1()
  ;;            and involves swapping sibling.
  (test* "deletion case7a" #t (begin (d -2) (c)))

  ;;   case 4b.  deleting black node, whose sibling is red.
  (test* "deletion case4b" #t (begin (d 0.5 1) (c)))

  ;;   case 3.  this goes through parent==NULL stop condition in the
  ;;            loop in delete_node1.
  ;;      B:-1.5 => -1.5
  ;;        R:-0.5 => -0.5
  ;;    B:0 => 0
  ;;        B:2.5 => 2.5
  ;;      R:3 => 3
  ;;        B:3.5 => 3.5
  ;;          R:4 => 4
  (test* "deletion case3" #t (begin (d 2 -1) (c)))

  ;; preparation
  ;;          R:-4 => -4
  ;;        B:-3 => -3
  ;;          R:-2 => -2
  ;;      R:-1 => -1
  ;;          R:-0.7 => -0.7
  ;;        B:-0.5 => -0.5
  ;;    B:0 => 0
  ;;      B:2.5 => 2.5
  ;;        R:4 => 4
  (d 3.5 3 -1.5)
  (i -1 -2 -3 -4 -0.7)

  ;; insertion case 4a.
  ;;          R:-4 => -4
  ;;        B:-3 => -3
  ;;          R:-2 => -2
  ;;      R:-1 => -1
  ;;          R:-0.7 => -0.7
  ;;        B:-0.6 => -0.6
  ;;          R:-0.5 => -0.5
  ;;    B:0 => 0
  ;;      B:2.5 => 2.5
  ;;        R:4 => 4
  (test* "insertion case4a" #t (begin (i -0.6) (c)))

  ;; preparation
  ;;      B:-2 => -2
  ;;    B:-1 => -1
  ;;        B:-0.7 => -0.7
  ;;      R:-0.6 => -0.6
  ;;          R:-0.5 => -0.5
  ;;        B:0 => 0
  (d 2.5 4 -3 -4)

  ;;   case 4a.  the reverse case of 4b.
  ;;      B:-1 => -1
  ;;        R:-0.7 => -0.7
  ;;    B:-0.6 => -0.6
  ;;        R:-0.5 => -0.5
  ;;      B:0 => 0
  (test* "deletion case4a" #t (begin (d -2) (c)))

  ;; preparation
  ;;          R:-2 => -2
  ;;      B:-1 => -1
  ;;    B:-0.6 => -0.6
  ;;        B:-0.5 => -0.5
  ;;          R:-0.3 => -0.3
  ;;      R:0 => 0
  ;;          R:1 => 1
  ;;        B:2 => 2
  ;;          R:3 => 3
  (d -0.7)
  (i -2 2 1 3 -0.3)

  ;; insertion case 4b
  ;;        R:-2 => -2
  ;;      B:-1 => -1
  ;;    B:-0.6 => -0.6
  ;;          R:-0.5 => -0.5
  ;;        B:-0.4 => -0.4
  ;;          R:-0.3 => -0.3
  ;;      R:0 => 0
  ;;          R:1 => 1
  ;;        B:2 => 2
  ;;          R:3 => 3
  (test* "insertion case4b" #t (begin (i -0.4) (c)))
  )

;; deletion during traversal
(let ()
  (define (make-tm) (alist->tree-map '((a . 1) (b . 2) (c . 3))
                                     default-comparator))
  (test* "deletion during traversal 1" '((b c) (a c) (a b))
         (map (^s
               (let1 tm (make-tm)
                 ($ tree-map-fold tm
                    (^[k v _] (when (eq? k s) (tree-map-delete! tm k)))
                    #f)
                 (sort (tree-map-keys tm))))
              '(a b c)))
  (test* "deletion during traversal 1 (reverse)" '((b c) (a c) (a b))
         (map (^s
               (let1 tm (make-tm)
                 ($ tree-map-fold-right tm
                    (^[k v _] (when (eq? k s) (tree-map-delete! tm k)))
                    #f)
                 (sort (tree-map-keys tm))))
              '(a b c)))
  (test* "deletion during traversal 2" '((a) (b) (c))
         (map (^s
               (let1 tm (make-tm)
                 ($ tree-map-fold tm
                    (^[k v _] (unless (eq? k s) (tree-map-delete! tm k)))
                    #f)
                 (sort (tree-map-keys tm))))
              '(a b c)))
  (test* "deletion during traversal 2 (reverse)" '((a) (b) (c))
         (map (^s
               (let1 tm (make-tm)
                 ($ tree-map-fold-right tm
                    (^[k v _] (unless (eq? k s) (tree-map-delete! tm k)))
                    #f)
                 (sort (tree-map-keys tm))))
              '(a b c)))  
  (test* "deletion during traversal 3" '(() () ())
         (map (^s
               (let1 tm (make-tm)
                 (tree-map-fold tm (^[k v _] (tree-map-delete! tm k)) #f)
                 (sort (tree-map-keys tm))))
              '(a b c)))
  (test* "deletion during traversal 3 (reverse)" '(() () ())
         (map (^s
               (let1 tm (make-tm)
                 (tree-map-fold-right tm (^[k v _] (tree-map-delete! tm k)) #f)
                 (sort (tree-map-keys tm))))
              '(a b c)))  
  )

;;
;; tree-map-{floor|ceiling|predecessor|successor}
;;

(let* ((alist '(("ai" . "to eat")
                ("aka" . "but")
                ("aka`aka" . "to laugh")
                ("alani" . "orange")
                ("aloha" . "love")
                ("aoao" . "page")
                ("aumoe" . "midnight")))
       (tree (alist->tree-map alist string=? string<?)))

  (define (get-val key) (if (string? key) (cdr (assoc key alist)) key))

  (define (test-key-val name key expected-key both-proc key-proc val-proc)
    (test* #"~name (~key)" (list expected-key (get-val expected-key))
           (receive (k v) (both-proc tree key) (list k v)))
    (test* #"~|name|-key (~key)" expected-key
           (key-proc tree key))
    (test* #"~|name|-value (~key)" (get-val expected-key)
           (val-proc tree key))
    (unless expected-key
      (test* #"~name (~key) / fallback" (list 0 1)
             (receive (k v) (both-proc tree key 0 1) (list k v)))
      (test* #"~|name|-key (~key) / fallback" 0
             (key-proc tree key 0))
      (test* #"~|name|-value (~key) / fallback" 0
             (val-proc tree key 0))))

  (define (tester key floor ceil pred succ)
    (test-key-val "tree-map-floor" key floor
                  tree-map-floor tree-map-floor-key tree-map-floor-value)
    (test-key-val "tree-map-ceiling" key ceil
                  tree-map-ceiling tree-map-ceiling-key tree-map-ceiling-value)
    (test-key-val "tree-map-predecessor" key pred
                  tree-map-predecessor tree-map-predecessor-key tree-map-predecessor-value)
    (test-key-val "tree-map-successor" key succ
                  tree-map-successor tree-map-successor-key tree-map-successor-value))

  (tester "am" "aloha" "aoao" "aloha" "aoao")
  (tester "aoao" "aoao" "aoao" "aloha" "aumoe")
  (tester "av" "aumoe" #f "aumoe" #f)
  (tester "aumoe" "aumoe" "aumoe" "aoao" #f)
  (tester "aii" "ai" "aka" "ai" "aka")
  (tester "ai"  "ai" "ai" #f "aka")
  (tester "aa"  #f "ai" #f "ai")

  (tree-map-clear! tree)

  (tester "xx"  #f #f #f #f)
  )

(let1 tree (make-tree-map = <)
  (test* "tree-map-pop-min! and num-entries" 0 
         (begin
           (tree-map-put! tree 0 0)
           (tree-map-pop-min! tree)
           (tree-map-num-entries tree))))

(let* ([comparator (make-comparator/compare string? #t
                                            (^[a b] (compare (string->number a)
                                                             (string->number b)))
                                            #f)]
       [tmap (make-tree-map comparator)])
  (test* "custom comparator" '(("00" . a) ("1" . b) ("10" . c))
         (begin
           (dolist [p '(("00" . x) ("1" . t) ("10" . c) ("01" . b)
                        ("000" . y) ("0" . a))]
             (tree-map-put! tmap (car p) (cdr p)))
           (tree-map->alist tmap)))
  (test* "comparator error check" (test-error)
         (tree-map-put! tmap 3 'z))
  )

(let* ([tm1 (alist->tree-map '((1 . "a") (2 . "b") (3 . "c"))
                             default-comparator)]
       [tm2 (alist->tree-map '((1 . "a") (2 . "b") (3 . "c") (4 . "d"))
                             default-comparator)]
       [tm3 (alist->tree-map '((2 . "b") (4 . "d"))
                             default-comparator)]
       [tm4 (alist->tree-map '((3 . "c") (2 . "b") (1 . "a"))
                             default-comparator)]
       [tm5 (alist->tree-map '((1 . "a") (2 . "b") (3 . "x"))
                             default-comparator)]
       [tm6 (alist->tree-map '((1 . "a") (2 . "b") (3 . "c"))
                             (make-comparator integer? eqv? < #f))]
       [tm7 (alist->tree-map '((2 . "B") (1 . "a") (3 . "C"))
                             default-comparator)])
  (test* "compare-as-sets - same" 0 (tree-map-compare-as-sets tm1 tm1))
  (test* "compare-as-sets - different comparator"
         (test-error <error> #/different comparator/)
         (tree-map-compare-as-sets tm1 tm6))
  (test* "compare-as-sets <" -1
         (tree-map-compare-as-sets tm1 tm2))
  (test* "compare-as-sets >" 1
         (tree-map-compare-as-sets tm2 tm1))
  (test* "compare-as-sets <" -1
         (tree-map-compare-as-sets tm3 tm2))
  (test* "compare-as-sets >" 1
         (tree-map-compare-as-sets tm2 tm3))
  (test* "compare-as-sets =" 0
         (tree-map-compare-as-sets tm1 tm4))
  (test* "compare-as-sets - unorderable" (test-error <error> #/can't be ordered/)
         (tree-map-compare-as-sets tm1 tm5))
  (test* "compare-as-sets - unorderable, fallback" #f
         (tree-map-compare-as-sets tm1 tm5 equal? #f))
  (test* "compare-as-sets - custom value=?" #f
         (tree-map-compare-as-sets tm1 tm7 string=? #f))
  (test* "compare-as-sets - custom value=?" 0
         (tree-map-compare-as-sets tm1 tm7 string-ci=?))

  (test* "compare-as-sequences - same" 0
         (tree-map-compare-as-sequences tm1 tm1))
  (test* "compare-as-sequences - different comparator"
         (test-error <error> #/different comparator/)
         (tree-map-compare-as-sequences tm1 tm6))
  (test* "compare-as-sequences tm1 < tm2" -1
         (tree-map-compare-as-sequences tm1 tm2))
  (test* "compare-as-sequences tm2 > tm1" 1
         (tree-map-compare-as-sequences tm2 tm1))
  (test* "compare-as-sequences tm1 < tm3" -1
         (tree-map-compare-as-sequences tm1 tm3))
  (test* "compare-as-sequences tm4 = tm1" 0
         (tree-map-compare-as-sequences tm4 tm1))
  (test* "compare-as-sequences tm1 < tm5" -1
         (tree-map-compare-as-sequences tm1 tm5))
  (test* "compare-as-sequences tm1 > tm7" 1
         (tree-map-compare-as-sequences tm1 tm7))
  (test* "compare-as-sequences tm1 = tm7" 0
         (tree-map-compare-as-sequences tm1 tm7 string-ci-comparator))
  )

(test-end)

