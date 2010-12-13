;;;
;;; tests for some builtin list operations
;;;

(use gauche.test)

;; monotonic-merge is a core function to implement Dylan-style class
;; precedence list.  those tests are taken from examples in
;;   http://www.webcom.com/~haahr/dylan/linearization-oopsla96.html

(define (get-super elt)
  (let ((p (assq elt
                 '((popup-menu menu popup-mixin)
                   (menu choice-widget)
                   (popup-mixin object)
                   (pedalo pedal-wheel-boat small-catamaran)
                   (pedal-wheel-boat engineless wheel-boat)
                   (engineless day-boat)
                   (day-boat boat)
                   (boat object)
                   (wheel-boat boat)
                   (small-catamaran small-multihull)
                   (small-multihull day-boat)
                   (confused-grid hv-grid vh-grid)
                   (hv-grid horizontal-grid vertical-grid)
                   (vh-grid vertical-grid horizontal-grid)
                   (horizontal-grid grid-layout)
                   (vertical-grid grid-layout)
                   (grid-layout object)))))
    (if (pair? p) (cdr p) #f)))

(test "monotonic-merge"
      '(popup-menu menu choice-widget popup-mixin object)
      (lambda ()
        (monotonic-merge
         'popup-menu
         '((menu choice-widget object)
           (menu popup-mixin)
           (popup-mixin object))
         get-super)))

(test "monotonic-merge"
      '(pedalo pedal-wheel-boat engineless small-catamaran
        small-multihull day-boat wheel-boat boat object)
      (lambda ()
        (monotonic-merge
         'pedalo
         '((pedal-wheel-boat engineless day-boat wheel-boat boat object)
           (small-catamaran small-multihull day-boat boat object)
           (pedal-wheel-boat small-catamaran))
         get-super)))

(test "monotonic-merge"
      #f
      (lambda ()
        (monotonic-merge
         'confused-grid
         '((hv-grid vh-grid)
           (hv-grid horizontal-grid vertical-grid grid-layout object)
           (vh-grid vertical-grid horizontal-grid grid-layout object))
         get-super)))


