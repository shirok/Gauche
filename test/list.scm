;;;
;;; tests for some builtin list operations
;;;

(use gauche.test)
(test-start "builtin list operations")

;;--------------------------------------------------------------------------
(test-section "iota")

(test* "iota" '(0 1 2 3 4) (iota 5))
(test* "iota" '(5 6 7 8 9) (iota 5 5))
(test* "iota" '(10 20 30 40 50) (iota 5 10 10))
(test* "iota" '(1.0 1.5 2.0 2.5 3.0) (iota 5 1 0.5))

;;--------------------------------------------------------------------------
(test-section "append and reverse")

(test* "append" '(1 2 3 4 5) (append '(1 2 3) '() '(4 5)))
(test* "append" '(1 2 3 . 5) (append '(1 2 3) 5))
(test* "append!" '(1 2 3 4 5) (append (list 1 2 3) '() '(4 5)))
(test* "append!" '(1 2 3 . 5) (append (list 1 2 3) 5))
(test* "reverse" '(5 4 3 2 1) (reverse '(1 2 3 4 5)))
(test* "reverse" '() (reverse '()))
(test* "reverse!" '(5 4 3 2 1) (reverse! (list 1 2 3 4 5)))
(test* "reverse!" '() (reverse! '()))
(test* "reverse 2args" '(1 2 3 4 5)    (reverse '(3 2 1) '(4 5)))
(test* "reverse 2args" '(1 2 3 4 . 5)  (reverse '(4 3 2 1) 5))
(test* "reverse! 2args" '(1 2 3 4 5)   (reverse! (list 3 2 1) (list 4 5)))
(test* "reverse! 2args" '(1 2 3 4 . 5) (reverse! (list 4 3 2 1) 5))

;;--------------------------------------------------------------------------
(test-section "folding")

(test* "fold" 55
       (fold + 0 '(1 2 3 4 5 6 7 8 9 10)))
(test* "fold" '(e d c b a)
       (fold cons '() '(a b c d e)))
(test* "fold" 3
       (fold (^[x c] (if (symbol? x) (+ c 1) c))
             0
             '(a 3 b 8 c 9)))
(test* "fold" '(c 3 b 2 a 1)
       (fold list* '() '(a b c) '(1 2 3 4 5)))
(test* "fold-right" '(1 2 3 4 5)
       (fold-right cons '() '(1 2 3 4 5)))
(test* "fold-right" '(2 4 6)
       (fold-right (^[x l] (if (even? x) (cons x l) l))
                   '()
                   '(1 2 3 4 5 6 7)))
(test* "fold-right" '(a 1 b 2 c 3)
       (fold-right list* '() '(a b c) '(1 2 3 4 5)))

;;--------------------------------------------------------------------------
(test-section "take and drop")

(test* "take" '(a b)   (take '(a b c d e) 2))
(test* "drop" '(c d e) (drop '(a b c d e) 2))
(test* "take" '(1 2)   (take '(1 2 3 . d) 2))
(test* "drop" '(3 . d) (drop '(1 2 3 . d) 2))
(test* "take" '(1 2 3) (take '(1 2 3 . d) 3))
(test* "drop" 'd       (drop '(1 2 3 . d) 3))
(test* "take-right" '(d e)     (take-right '(a b c d e) 2))
(test* "drop-right" '(a b c)   (drop-right '(a b c d e) 2))
(test* "take-right" '(2 3 . d) (take-right '(1 2 3 . d) 2))
(test* "drop-right" '(1)       (drop-right '(1 2 3 . d) 2))
(test* "take-right" 'd         (take-right '(1 2 3 . d) 0))
(test* "drop-right" '(1 2 3)   (drop-right '(1 2 3 . d) 0))
(test* "take!"      '(1 2)     (take! '(1 2 3 . d) 2))
(test* "drop-right!" '(1 2)    (drop-right! '(1 2 3 . d) 1))
(test* "split-at" '((a b c) (d e f g h))
       (call-with-values (^[] (split-at '(a b c d e f g h) 3))
         list))
(test* "split-at!" '((a b c) (d e f g h))
       (call-with-values
           (^[] (split-at! (list 'a 'b 'c 'd 'e 'f 'g 'h) 3))
         list))

(test* "split-at* (normal)" '((a b c) (d))
       (receive r (split-at* '(a b c d) 3) r))
(test* "split-at* (boundary)" '(() (a b c d))
       (receive r (split-at* '(a b c d) 0) r))
(test* "split-at* (boundary)" '((a b c d) ())
       (receive r (split-at* '(a b c d) 4) r))
(test* "split-at* (error)" (test-error)
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
(test* "take* (error)" (test-error)   (take* '(a b c d) -1))
(test* "take* (shorten)" '(a b c d)   (take* '(a b c d) 5))
(test* "take* (fill)" '(a b c d #f #f) (take* '(a b c d) 6 #t))
(test* "take* (fill)" '(a b c d z z)  (take* '(a b c d) 6 #t 'z))

(test* "drop* (normal)" '(c d)       (drop* '(a b c d) 2))
(test* "drop* (boundary)" '(a b c d) (drop* '(a b c d) 0))
(test* "drop* (boundary)" '()        (drop* '(a b c d) 4))
(test* "drop* (error)" (test-error)  (drop* '(a b c d) -3))
(test* "drop* (past)" '()            (drop* '(a b c d) 5))

(test* "take-right* (normal)" '(b c d)  (take-right* '(a b c d) 3))
(test* "take-right* (boundary)" '()     (take-right* '(a b c d) 0))
(test* "take-right* (boundary)" '(a b c d) (take-right* '(a b c d) 4))
(test* "take-right* (error)" (test-error)  (take-right* '(a b c d) -1))
(test* "take-right* (shorten)" '(a b c d)  (take-right* '(a b c d) 6))
(test* "take-right* (fill)" '(z z a b c d) (take-right* '(a b c d) 6 #t 'z))

(test* "drop-right* (normal)" '(a b c)  (drop-right* '(a b c d) 1))
(test* "drop-right* (boundary)" '()     (drop-right* '(a b c d) 4))
(test* "drop-right* (boundary)" '(a b c d) (drop-right* '(a b c d) 0))
(test* "drop-right* (error)" (test-error)  (drop-right* '(a b c d) -1))
(test* "drop-right* (past)" '()         (drop-right* '(a b c d) 7))

;;--------------------------------------------------------------------------
;; slice and intercept

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

;;--------------------------------------------------------------------------
;; associative lists

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

;;--------------------------------------------------------------------------

(test-section "monotonic-merge")

;; monotonic-merge is a core function to implement Dylan-style class
;; precedence list.  those tests are taken from examples in
;;   http://www.webcom.com/~haahr/dylan/linearization-oopsla96.html

(test "monotonic-merge"
      '(menu choice-widget popup-mixin object)
      (^[] (monotonic-merge
            '((menu choice-widget object)
              (menu popup-mixin)
              (popup-mixin object)))))

(test "monotonic-merge"
      '(pedal-wheel-boat engineless small-catamaran
        small-multihull day-boat wheel-boat boat object)
      (^[] (monotonic-merge
            '((pedal-wheel-boat engineless day-boat wheel-boat boat object)
              (small-catamaran small-multihull day-boat boat object)
              (pedal-wheel-boat small-catamaran)))))

(test "monotonic-merge"
      #f
      (^[] (monotonic-merge
            '((hv-grid vh-grid)
              (hv-grid horizontal-grid vertical-grid grid-layout object)
              (vh-grid vertical-grid horizontal-grid grid-layout object)))))

(test-end)
