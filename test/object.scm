;;
;; Test object system
;;

;; $Id: object.scm,v 1.3 2001-03-25 10:55:28 shiro Exp $

(add-load-path "../lib")
(use gauche.test)

(test-start "object system")

;;----------------------------------------------------------------
(test-section "class definition")

(define-class <x> () (a b c))
(test "define-class <x>" '<x> (lambda () (class-name <x>)))
(test "define-class <x>" 3 (lambda () (slot-ref <x> 'num-instance-slots)))
(test "define-class <x>" <class> (lambda () (class-of <x>)))
(test "define-class <x>" '(<x> <object> <top>)
      (lambda () (map class-name (class-precedence-list <x>))))

(define-class <y> (<x>) (c d e))
(test "define-class <y>" 5 (lambda () (slot-ref <y> 'num-instance-slots)))
(test "define-class <y>" <class> (lambda () (class-of <y>)))
(test "define-class <y>" '(<y> <x> <object> <top>)
      (lambda () (map class-name (class-precedence-list <y>))))

(define-class <z> (<object>) ())
(test "define-class <z>" 0 (lambda () (slot-ref <z> 'num-instance-slots)))
(test "define-class <z>" <class> (lambda () (class-of <z>)))
(test "define-class <z>" '(<z> <object> <top>)
      (lambda () (map class-name (class-precedence-list <z>))))

(define-class <w> (<z> <y>) (e f))
(test "define-class <w>" 6 (lambda () (slot-ref <w> 'num-instance-slots)))
(test "define-class <w>" <class> (lambda () (class-of <w>)))
(test "define-class <w>" '(<w> <z> <y> <x> <object> <top>)
      (lambda () (map class-name (class-precedence-list <w>))))

(define-class <w2> (<y> <z>) (e f))
(test "define-class <w2>" '(<w2> <y> <x> <z> <object> <top>)
      (lambda () (map class-name (class-precedence-list <w2>))))

;;----------------------------------------------------------------
(test-section "instancing")

(define x1 (make <x>))
(define x2 (make <x>))

(test "make <x>" <x> (lambda () (class-of x1)))
(test "make <x>" <x> (lambda () (class-of x2)))

(slot-set! x1 'a 4)
(slot-set! x1 'b 5)
(slot-set! x1 'c 6)
(slot-set! x2 'a 7)
(slot-set! x2 'b 8)
(slot-set! x2 'c 9)

(test "slot-ref" '(4 5 6)
      (lambda () (map (lambda (slot) (slot-ref x1 slot)) '(a b c))))
(test "slot-ref" '(7 8 9)
      (lambda () (map (lambda (slot) (slot-ref x2 slot)) '(a b c))))

;;----------------------------------------------------------------
(test-section "slot parameters")

(define-class <r> ()
  ((a :init-keyword :a :initform 4)
   (b :init-keyword :b :init-value 5)))

(define r1 (make <r>))
(define r2 (make <r> :a 9))
(define r3 (make <r> :b 100 :a 20))

(define-method slot-values ((obj <r>))
  (map (lambda (s) (slot-ref obj s)) '(a b)))

(test "make <r>" '(4 5) (lambda () (slot-values r1)))
(test "make <r> :a" '(9 5) (lambda () (slot-values r2)))
(test "make <r> :a :b" '(20 100) (lambda () (slot-values r3)))


;;----------------------------------------------------------------
(test-section "next method")

(define (nm obj) 'fallback)

(define-method nm ((obj <x>))  (list 'x-in (next-method) 'x-out))
(define-method nm ((obj <y>))  (list 'y-in (next-method) 'y-out))
(define-method nm ((obj <z>))  (list 'z-in (next-method) 'z-out))
(define-method nm ((obj <w>))  (list 'w-in (next-method) 'w-out))
(define-method nm ((obj <w2>))  (list 'w2-in (next-method) 'w2-out))
  
(test "next method"
      '(y-in (x-in fallback x-out) y-out)
      (lambda () (nm (make <y>))))
(test "next-method"
      '(w-in (z-in (y-in (x-in fallback x-out) y-out) z-out) w-out)
      (lambda () (nm (make <w>))))

;; This test fails, since sort-methods is not implemented properly.
;(test "next-method"
;      '(w2-in (y-in (x-in (z-in fallback z-out) x-out) y-out) w2-out)
;      (lambda () (nm (make <w2>))))

(test-end)
