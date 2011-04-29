;;;
;;; tests for some builtin list operations
;;;

(use gauche.test)
(test-start "builtin list operations")

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
       (fold (lambda (x c) (if (symbol? x) (+ c 1) c))
             0
             '(a 3 b 8 c 9)))
(test* "fold" '(c 3 b 2 a 1)
       (fold list* '() '(a b c) '(1 2 3 4 5)))
(test* "fold-right" '(1 2 3 4 5)
       (fold-right cons '() '(1 2 3 4 5)))
(test* "fold-right" '(2 4 6)
       (fold-right (lambda (x l) (if (even? x) (cons x l) l))
                   '()
                   '(1 2 3 4 5 6 7)))
(test* "fold-right" '(a 1 b 2 c 3)
       (fold-right list* '() '(a b c) '(1 2 3 4 5)))

;;--------------------------------------------------------------------------

(test-section "monotonic-merge")

;; monotonic-merge is a core function to implement Dylan-style class
;; precedence list.  those tests are taken from examples in
;;   http://www.webcom.com/~haahr/dylan/linearization-oopsla96.html

(test "monotonic-merge"
      '(menu choice-widget popup-mixin object)
      (lambda ()
        (monotonic-merge
         '((menu choice-widget object)
           (menu popup-mixin)
           (popup-mixin object)))))

(test "monotonic-merge"
      '(pedal-wheel-boat engineless small-catamaran
        small-multihull day-boat wheel-boat boat object)
      (lambda ()
        (monotonic-merge
         '((pedal-wheel-boat engineless day-boat wheel-boat boat object)
           (small-catamaran small-multihull day-boat boat object)
           (pedal-wheel-boat small-catamaran)))))

(test "monotonic-merge"
      #f
      (lambda ()
        (monotonic-merge
         '((hv-grid vh-grid)
           (hv-grid horizontal-grid vertical-grid grid-layout object)
           (vh-grid vertical-grid horizontal-grid grid-layout object)))))

(test-end)
