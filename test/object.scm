;;
;; Test object system
;;

;; $Id: object.scm,v 1.1 2001-03-25 06:32:40 shiro Exp $

(add-load-path "../lib")
(use gauche.test)

(test-start "object system")

;;----------------------------------------------------------------
(test-section "class definition and object instantiation")

(define-class <x> () (a b c))
(test "define-class <x>" '<x> (lambda () (class-name <x>)))
(test "define-class <x>" 3 (lambda () (slot-ref <x> 'num-instance-slots)))
(test "define-class <x>" <class> (lambda () (class-of <x>)))

(define x (make <x>))
(define y (make <x>))

(test "make <x>" <x> (lambda () (class-of x)))
(test "make <x>" <x> (lambda () (class-of y)))

(slot-set! x 'a 4)
(slot-set! x 'b 5)
(slot-set! x 'c 6)
(slot-set! y 'a 7)
(slot-set! y 'b 8)
(slot-set! y 'c 9)

(test "slot-ref" '(4 5 6)
      (lambda () (map (lambda (slot) (slot-ref x slot)) '(a b c))))
(test "slot-ref" '(7 8 9)
      (lambda () (map (lambda (slot) (slot-ref y slot)) '(a b c))))




      

(test-end)
