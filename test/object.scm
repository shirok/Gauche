;;
;; Test object system
;;

;; $Id: object.scm,v 1.21 2003-01-03 21:06:26 shirok Exp $

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

(define-method nm (obj . a)
  (if (null? a) (list 't*-in (next-method) 't*-out) 't*))
(define-method nm ((obj <y>) a) (list 'y1-in (next-method) 'y1-out))
(define-method nm ((obj <y>) . a) (list 'y*-in (next-method) 'y*-out))

(test "next-method"
      '(y1-in (y*-in t* y*-out) y1-out)
      (lambda () (nm (make <y>) 3)))
(test "next-method"
      '(y-in (y*-in (x-in (t*-in fallback t*-out) x-out) y*-out) y-out)
      (lambda () (nm (make <y>))))

;;----------------------------------------------------------------
(test-section "setter method definition")

(define-method s-get-i ((self <s>)) (slot-ref self 'i))
(define-method (setter s-get-i) ((self <s>) v) (slot-set! self 'i v))
(define-method (setter s-get-i) ((self <ss>) v) (slot-set! self 'i (cons v v)))

(test "setter of s-get-i(<s>)" '("i" "j")
      (lambda ()
        (let* ((s (make <s> :i "i"))
               (i (s-get-i s))
               (j (begin (set! (s-get-i s) "j") (s-get-i s))))
          (list i j))))
(test "setter of s-get-i(<ss>)" '("i" ("j" . "j"))
      (lambda ()
        (let* ((s (make <ss> :i "i"))
               (i (s-get-i s))
               (j (begin (set! (s-get-i s) "j") (s-get-i s))))
          (list i j))))

;;----------------------------------------------------------------
(test-section "object comparison protocol")

(define-class <cmp> () ((x :init-keyword :x)))

(define-method object-equal? ((x <cmp>) (y <cmp>))
  (equal? (slot-ref x 'x) (slot-ref y 'x)))

(define-method object-compare ((x <cmp>) (y <cmp>))
  (compare (slot-ref x 'x) (slot-ref y 'x)))

(test "object-equal?" #t
      (lambda ()
        (equal? (make <cmp> :x 3) (make <cmp> :x 3))))

(test "object-equal?" #f
      (lambda ()
        (equal? (make <cmp> :x 3) (make <cmp> :x 2))))

(test "object-equal?" #t
      (lambda ()
        (equal? (make <cmp> :x (list 1 2))
                (make <cmp> :x (list 1 2)))))

(test "object-equal?" #f
      (lambda ()
        (equal? (make <cmp> :x 5) 5)))

(test "object-compare" -1 (lambda () (compare 0 1)))
(test "object-compare" 0  (lambda () (compare 0 0)))
(test "object-compare" 1  (lambda () (compare 1 0)))
(test "object-compare" -1 (lambda () (compare "abc" "abd")))
(test "object-compare" 0  (lambda () (compare "abc" "abc")))
(test "object-compare" 1  (lambda () (compare "abd" "abc")))
(test "object-compare" -1 (lambda () (compare #\a #\b)))
(test "object-compare" 0  (lambda () (compare #\a #\a)))
(test "object-compare" 1  (lambda () (compare #\b #\a)))
(test-error "object-compare"
            (lambda () (compare #\b 4)))
(test-error "object-compare"
            (lambda () (compare "zzz" 4)))
(test-error "object-compare"
            (lambda () (compare 2+i 3+i)))
(test "object-compare" -1
      (lambda () (compare (make <cmp> :x 3) (make <cmp> :x 4))))
(test "object-compare" 0
      (lambda () (compare (make <cmp> :x 3) (make <cmp> :x 3))))
(test "object-compare" 1
      (lambda () (compare (make <cmp> :x 4) (make <cmp> :x 3))))

;;----------------------------------------------------------------
(test-section "object-apply protocol")

(define-class <applicable> ()
  ((v :initform (make-vector 5 #f))))

(define-method object-apply ((self <applicable>) (i <integer>))
  (vector-ref (ref self 'v) i))

(define-method (setter object-apply) ((self <applicable>) (i <integer>) v)
  (vector-set! (ref self 'v) i v))

(define-method object-apply ((self <applicable>) (s <symbol>))
  (case s
    ((list)   (vector->list (ref self 'v)))
    ((vector) (ref self 'v))
    (else #f)))

(define applicable (make <applicable>))

(test "object-apply" #f
      (lambda () (applicable 2)))
(test "object-apply" 'a
      (lambda () (set! (applicable 3) 'a) (applicable 3)))
(test "object-apply" '(d b c a q)
      (lambda ()
        (for-each (lambda (i v) (set! (applicable i) v))
                  '(2 4 1 0) '(c q b d))
        (map applicable '(0 1 2 3 4))))
(test "object-apply" '((d b c a q) #(d b c a q))
      (lambda ()
        (map applicable '(list vector))))

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

(test-section "metaclass w/ slots")

(define-class <documentation-meta> (<class>)
  ((doc :init-keyword :doc :initform #f)))

(define-class <xxx> ()
  (a b c)
  :metaclass <documentation-meta>
  :doc "Doc doc")

(test "class slot in meta" "Doc doc"
      (lambda () (slot-ref <xxx> 'doc)))

;;----------------------------------------------------------------
(test-section "metaclass/singleton")

(use gauche.singleton)

(define-class <single> ()
  ((foo :init-keyword :foo :initform 4))
  :metaclass <singleton-meta>)

(define single-obj (make <single> :foo 5))

(test "singleton" #t
      (lambda () (eq? single-obj (make <single>))))

(test "singleton" #t
      (lambda () (eq? single-obj (instance-of <single>))))

(test "singleton" 5
      (lambda () (slot-ref (make <single>) 'foo)))

(define-class <single-2> () () :metaclass <singleton-meta>)

(test "singleton" #f (lambda () (eq? single-obj (make <single-2>))))

;;----------------------------------------------------------------
(test-section "metaclass/validator")

(use gauche.mop.validator)

(define-class <validator> ()
  ((a :accessor a-of
      :initform 'doo
      :validator (lambda (obj value) (x->string value)))
   (b :accessor b-of
      :initform 99
      :validator (lambda (obj value)
                   (if (integer? value)
                       value
                       (error "integer required for slot b")))))
  :metaclass <validator-meta>)

(define v (make <validator>))

(test "validator" "doo"
      (lambda () (slot-ref v 'a)))

(test "validator" "foo"
      (lambda () (slot-set! v 'a 'foo) (slot-ref v 'a)))

(test "validator" "1234"
      (lambda () (set! (a-of v) 1234)  (a-of v)))

(test "validator" 99
      (lambda () (slot-ref v 'b)))

(test "validator" 55
      (lambda () (slot-set! v 'b 55) (slot-ref v 'b)))

(test "validator" #t
      (lambda ()
        (with-error-handler (lambda (e) #t)
                            (lambda () (slot-set! v 'b 3.4)))))
(test "validator" #t
      (lambda ()
        (with-error-handler (lambda (e) #t)
                            (lambda () (set! (b-of v) 3.4)))))

;;----------------------------------------------------------------
(test-section "metaclass/propagate")

(use gauche.mop.propagate)

(define-class <propagate-x> ()
  ((value :init-keyword :value :init-value 3)))

(define-class <propagate-y> ()
  ((x0 :init-keyword :x0 :init-form (make <propagate-x> :value 0))
   (x1 :init-keyword :x1 :init-form (make <propagate-x> :value 1))
   (value :allocation :propagated :propagate 'x0 :init-keyword :value)
   (vv    :allocation :propagated :propagate '(x1 value)))
  :metaclass <propagate-meta>)

(test "propagate ref default" '(0 0)
      (lambda ()
        (let* ((y (make <propagate-y>))
               (x (slot-ref y 'x0)))
          (list (slot-ref y 'value)
                (slot-ref x 'value)))))
(test "propagate ref init" '(3 1)
      (lambda ()
        (let ((y (make <propagate-y> :x0 (make <propagate-x>))))
          (list (slot-ref y 'value) (slot-ref y 'vv)))))
(test "propagate ref init2" '(99 99)
      (lambda ()
        (let* ((y (make <propagate-y> :x0 (make <propagate-x>) :value 99))
               (x (slot-ref y 'x0)))
          (list (slot-ref y 'value)
                (slot-ref x 'value)))))

(test "propagate set" '(888 888)
      (lambda ()
        (let ((y (make <propagate-y>)))
          (set! (slot-ref y 'value) 888)
          (list (slot-ref y 'value) (slot-ref (slot-ref y 'x0) 'value)))))

(test "propagate set" '(999 999)
      (lambda ()
        (let ((y (make <propagate-y>)))
          (set! (slot-ref y 'vv) 999)
          (list (slot-ref y 'vv) (slot-ref (slot-ref y 'x1) 'value)))))

(define-class <propagate-validator-meta> (<validator-meta>
                                          <propagate-meta>)
  ())

(define-class <propagate-validator> ()
  ((x :initform (make <propagate-x>))
   (value :allocation :propagated :propagate 'x
          :validator (lambda (o v) (x->string v)))
   )
  :metaclass <propagate-validator-meta>)

(test "propagate-validator" "999"
      (lambda ()
        (let ((y (make <propagate-validator>)))
          (set! (slot-ref y 'value) 999)
          (slot-ref y 'value))))

;;----------------------------------------------------------------
(test-section "metaclass/instance-pool")

(use srfi-1)
(use gauche.mop.instance-pool)

(define-class <pool-x> (<instance-pool-mixin>) ())
(define-class <pool-y> (<instance-pool-mixin>) ())
(define-class <pool-z> (<pool-x> <pool-y>) ())

(define pool-x1 (make <pool-x>))
(define pool-z1 (make <pool-z>))
(define pool-y1 (make <pool-y>))
(define pool-y2 (make <pool-y>))
(define pool-x2 (make <pool-x>))
(define pool-z2 (make <pool-z>))

(test "instance-pool (pool)" #t
      (lambda ()
        (and (memq pool-x1 (instance-pool->list <pool-x>))
             (memq pool-x2 (instance-pool->list <pool-x>))
             (not (memq pool-y1 (instance-pool->list <pool-x>)))
             (not (memq pool-y2 (instance-pool->list <pool-x>)))
             (memq pool-y1 (instance-pool->list <pool-y>))
             (memq pool-y2 (instance-pool->list <pool-y>))
             (not (memq pool-x1 (instance-pool->list <pool-y>)))
             (not (memq pool-x2 (instance-pool->list <pool-y>)))
             (memq pool-z1 (instance-pool->list <pool-x>))
             (memq pool-z1 (instance-pool->list <pool-y>))
             #t
             )))

(test "instance-pool-find" pool-x1
      (lambda ()
        (instance-pool-find <pool-x> (cut eq? pool-x1 <>))))

(test "instance-pool-find" pool-z1
      (lambda ()
        (instance-pool-find <pool-x> (cut eq? pool-z1 <>))))

(test "instance-pool-find" pool-y1
      (lambda ()
        (instance-pool-find <pool-y> (cut eq? pool-y1 <>))))

(test "instance-pool-find" #f
      (lambda ()
        (instance-pool-find <pool-x> (cut eq? pool-y2 <>))))

(test "instance-pool-fold" (list pool-x1 pool-z1 pool-x2 pool-z2)
      (lambda ()
        (instance-pool-fold <pool-x> cons '()))
      (cut lset= eq? <> <>))

(test "instance-pool-map" (list pool-x1 pool-z1 pool-x2 pool-z2)
      (lambda ()
        (instance-pool-map <pool-x> identity))
      (cut lset= eq? <> <>))

(test "instance-pool-for-each" (list pool-x1 pool-z1 pool-x2 pool-z2)
      (lambda ()
        (let ((r '()))
          (instance-pool-for-each <pool-x> (lambda (p) (push! r p)))
          r))
      (cut lset= eq? <> <>))

(test "instance-pool-remove!" #f
      (lambda ()
        (instance-pool-remove! <pool-x> (cut eq? pool-z1 <>))
        (instance-pool-find <pool-x> (cut eq? pool-z1 <>))))

(test "instance-pool-remove!" pool-z1
      (lambda ()
        (instance-pool-find <pool-y> (cut eq? pool-z1 <>))))

(test "instance-pool-remove!" #f
      (lambda ()
        (instance-pool-remove! <pool-z> (cut eq? pool-z2 <>))
        (or (instance-pool-find <pool-x> (cut eq? pool-z2 <>))
            (instance-pool-find <pool-y> (cut eq? pool-z2 <>)))))

(test-end)

