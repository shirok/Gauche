;;
;; Test object system
;;

;; $Id: object.scm,v 1.9 2001-04-03 10:30:34 shiro Exp $

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
(test-section "slot initialization")

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
(test-section "slot allocations")

(define-class <s> ()
  ((i :allocation :instance      :init-keyword :i :init-value #\i)
   (c :allocation :class         :init-keyword :c :init-value #\c)
   (s :allocation :each-subclass :init-keyword :s :init-value #\s)
   (v :allocation :virtual       :init-keyword :v
      :slot-ref (lambda (o) (cons (slot-ref o 'i) (slot-ref o 'c)))
      :slot-set! (lambda (o v)
                   (slot-set! o 'i (car v))
                   (slot-set! o 'c (cdr v))))
   ))

(define-method slot-values ((obj <s>))
  (map (lambda (s) (slot-ref obj s)) '(i c s v)))

(define s1 (make <s>))
(define s2 (make <s>))

(test "make <s>" '(#\i #\c #\s (#\i . #\c)) (lambda () (slot-values s1)))
(test "slot-set! :instance"
      '((#\I #\c #\s (#\I . #\c)) (#\i #\c #\s (#\i . #\c)))
      (lambda ()
        (slot-set! s1 'i #\I)
        (list (slot-values s1) (slot-values s2))))
(test "slot-set! :class"
      '((#\I #\C #\s (#\I . #\C)) (#\i #\C #\s (#\i . #\C)))
      (lambda ()
        (slot-set! s1 'c #\C)
        (list (slot-values s1) (slot-values s2))))
(test "slot-set! :each-subclass"
      '((#\I #\C #\S (#\I . #\C)) (#\i #\C #\S (#\i . #\C)))
      (lambda ()
        (slot-set! s1 's #\S)
        (list (slot-values s1) (slot-values s2))))
(test "slot-set! :virtual"
      '((i c #\S (i . c)) (#\i c #\S (#\i . c)))
      (lambda ()
        (slot-set! s1 'v '(i . c))
        (list (slot-values s1) (slot-values s2))))

(define-class <ss> (<s>)
  ())

(define s3 (make <ss> :i "i" :c "c" :s "s"))

(test "make <ss>"
      '(("i" "c" "s" ("i" . "c")) (i "c" #\S (i . "c")))
      (lambda () (list (slot-values s3) (slot-values s1))))
(test "slot-set! :class"
      '(("i" "C" "s" ("i" . "C")) (i "C" #\S (i . "C")))
      (lambda ()
        (slot-set! s3 'c "C")
        (list (slot-values s3) (slot-values s1))))
(test "slot-set! :each-subclass"
      '(("i" "C" "s" ("i" . "C")) (i "C" "S" (i . "C")))
      (lambda ()
        (slot-set! s1 's "S")
        (list (slot-values s3) (slot-values s1))))
(test "slot-set! :each-subclass"
      '(("i" "C" 5 ("i" . "C")) (i "C" "S" (i . "C")))
      (lambda ()
        (slot-set! s3 's 5)
        (list (slot-values s3) (slot-values s1))))

(define s4 (make <ss> :v '(1 . 0)))

(test "make <ss> :v"
      '((1 0 5 (1 . 0)) ("i" 0 5 ("i" . 0)))
      (lambda () (list (slot-values s4) (slot-values s3))))

(test "class-slot-ref"
      '(0 "S" 0 5)
      (lambda ()
        (list (class-slot-ref <s> 'c)  (class-slot-ref <s> 's)
              (class-slot-ref <ss> 'c) (class-slot-ref <ss> 's))))
(test "class-slot-set!"
      '(100 99 100 5)
      (lambda ()
        (class-slot-set! <s> 'c 100)
        (class-slot-set! <s> 's 99)
        (list (class-slot-ref <s> 'c)  (class-slot-ref <s> 's)
              (class-slot-ref <ss> 'c) (class-slot-ref <ss> 's))))
(test "class-slot-set!"
      '(101 99 101 55)
      (lambda ()
        (class-slot-set! <ss> 'c 101)
        (class-slot-set! <ss> 's 55)
        (list (class-slot-ref <s> 'c)  (class-slot-ref <s> 's)
              (class-slot-ref <ss> 'c) (class-slot-ref <ss> 's))))

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
(test "next-method"
      '(w2-in (y-in (x-in (z-in fallback z-out) x-out) y-out) w2-out)
      (lambda () (nm (make <w2>))))

;;----------------------------------------------------------------
(test-section "metaclass")

(define-class <listing-class> (<class>)
  ((classes :allocation :class :init-value '() :accessor classes-of))
  )

(define-method initialize ((class <listing-class>) initargs)
  (next-method)
  (set! (classes-of class) (cons (class-name class) (classes-of class))))

(define-class <xx> ()
  ()
  :metaclass <listing-class>)

(define-class <yy> (<xx>)
  ())

(test "metaclass" '(<yy> <xx>)
      (lambda () (class-slot-ref <listing-class> 'classes)))

(define-class <auto-accessor-class> (<class>)
  ())

(define-method initialize ((class <auto-accessor-class>) initargs)
  (let ((slots (get-keyword :slots initargs '())))
    (for-each (lambda (slot)
                (unless (get-keyword :accessor (cdr slot) #f)
                  (set-cdr! slot (list* :accessor
                                        (string->symbol
                                         (format #f "~a-of" (car slot)))
                                        (cdr slot)))))
              slots)
    (next-method)))

(define-class <zz> ()
  (a b c)
  :metaclass <auto-accessor-class>)

(test "metaclass" '(1 2 3)
      (lambda ()
        (let ((zz (make <zz>)))
          (set! (a-of zz) 1)
          (set! (b-of zz) 2)
          (set! (c-of zz) 3)
          (map (lambda (s) (slot-ref zz s)) '(a b c)))))

(define-class <uu> (<zz>)
  (d e f))

(test "metaclass" '(1 2 3 4 5 6)
      (lambda ()
        (let ((uu (make <uu>)))
          (set! (a-of uu) 1)
          (set! (b-of uu) 2)
          (set! (c-of uu) 3)
          (set! (d-of uu) 4)
          (set! (e-of uu) 5)
          (set! (f-of uu) 6)
          (map (lambda (s) (slot-ref uu s)) '(a b c d e f)))))

(define-class <vv> (<zz> <xx>)
  ())

(test "metaclass" '(1 2 3)
      (lambda ()
        (let ((vv (make <vv>)))
          (set! (a-of vv) 1)
          (set! (b-of vv) 2)
          (set! (c-of vv) 3)
          (map (lambda (s) (slot-ref vv s)) '(a b c)))))
(test "metaclass" '(<vv> <yy> <xx>)
      (lambda () (class-slot-ref <listing-class> 'classes)))
      
(define-class <ww> (<uu> <yy>)
  ())

(test "metaclass" #t
      (lambda () (eq? (class-of <ww>) (class-of <vv>))))
(test "metaclass" '(1 2 3 4 5 6)
      (lambda ()
        (let ((ww (make <ww>)))
          (set! (a-of ww) 1)
          (set! (b-of ww) 2)
          (set! (c-of ww) 3)
          (set! (d-of ww) 4)
          (set! (e-of ww) 5)
          (set! (f-of ww) 6)
          (map (lambda (s) (slot-ref ww s)) '(a b c d e f)))))
(test "metaclass" '(<ww> <vv> <yy> <xx>)
      (lambda () (class-slot-ref <listing-class> 'classes)))

(test-end)
