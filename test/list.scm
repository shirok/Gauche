;;;
;;; tests for some builtin list operations
;;;

(use gauche.test)
(test-start "builtin list operations")

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
