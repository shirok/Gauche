(use gauche.uvector)
(use gauche.sequence)
(use gauche.test)
(test-start "records")

(use gauche.record)
(test-module 'gauche.record)

;;--------------------------------------------------------------------
(test-section "rtd")

(define rtd1 #f)
(define rtd2 #f)

(test* "make-rtd" #t
       (begin (set! rtd1 (make-rtd 'rtd1 #(a b (immutable c))))
              (rtd? rtd1)))

(test* "make-rtd (subrecord)" #t
       (begin (set! rtd2 (make-rtd 'rtd2 #((immutable d) c (immutable b)) rtd1))
              (rtd? rtd2)))

(test* "rtd-parent" #f (rtd-parent rtd1))
(test* "rtd-parent" rtd1 (rtd-parent rtd2))

(test* "field-names" '#(a b c) (rtd-field-names rtd1))
(test* "field-names" '#(d c b) (rtd-field-names rtd2))
(test* "all-field-names" '#(a b c) (rtd-all-field-names rtd1))
(test* "all-field-names" '#(a b c d c b) (rtd-all-field-names rtd2))

(test* "rtd-field-mutable?" '(#t #t #f)
       (map (cut rtd-field-mutable? rtd1 <>) '(a b c)))
(test* "rtd-field-mutable?" '(#t #f #t #f)
       (map (cut rtd-field-mutable? rtd2 <>) '(a b c d)))

(test* "rtd-field-mutable?" (test-error) (rtd-field-mutable? rtd1 'd))

;;--------------------------------------------------------------------
(test-section "instances")

(let ([r1a #f] [r1b #f] [r2 #f])
  (test* "rtd-constructor" rtd1
         (begin (set! r1a ((rtd-constructor rtd1) 1 2 3))
                (and (record? r1a) (record-rtd r1a))))
  (test* "rtd-accessor" '(1 2 3)
         (map (^f ((rtd-accessor rtd1 f) r1a)) '(a b c)))
  (test* "rtd-accessor" (test-error) (rtd-accessor rtd1 'd))

  (test* "rtd-constructor w/ fields" #f
         (begin (set! r1b ((rtd-constructor rtd1 '#(c a b)) 6 4 5))
                (eqv? r1a r1b)))
  (test* "multiple instance" '((1 2 3) (4 5 6))
         (map (^r (map (^f ((rtd-accessor rtd1 f) r)) '(a b c)))
              (list r1a r1b)))

  (test* "rtd-mutator" '(100 200 3)
         (begin ((rtd-mutator rtd1 'a) r1a 100)
                ((rtd-mutator rtd1 'b) r1a 200)
                (map (^f ((rtd-accessor rtd1 f) r1a)) '(a b c))))

  (test* "rtd-mutator" (test-error) (rtd-mutator rtd1 'c))

  (test* "rtd-constructor" rtd2
         (begin (set! r2 ((rtd-constructor rtd2) 1 2 3 4 5 6))
                (and (record? r2) (record-rtd r2))))

  (test* "rtd-accessor" '(1 6 5 4)
         (map (^f ((rtd-accessor rtd2 f) r2)) '(a b c d)))

  (test* "rtd-mutator" '(100 6 500 4)
         (begin ((rtd-mutator rtd2 'a) r2 100)
                ((rtd-mutator rtd2 'c) r2 500)
                (map (^f ((rtd-accessor rtd2 f) r2)) '(a b c d))))
  (test* "rtd-mutator" (test-error) (rtd-mutator rtd2 'b))
  (test* "rtd-mutator" (test-error) (rtd-mutator rtd2 'd))

  (test* "accessing base record type" '(100 2 3)
         (map (^f ((rtd-accessor rtd1 f) r2)) '(a b c)))
  (test* "modifying base record type" '(1000 200 3)
         (begin ((rtd-mutator rtd1 'a) r2 1000)
                ((rtd-mutator rtd1 'b) r2 200)
                (map (^f ((rtd-accessor rtd1 f) r2)) '(a b c))))
  (test* "modifying base record type / shadowing" '(1000 6 500 4)
         (map (^f ((rtd-accessor rtd2 f) r2)) '(a b c d)))
  )

;; srfi-99 example

(let ()
  (define rtd1
    (make-rtd 'rtd1 '#((immutable x1) (immutable x2))))

  (define rtd2
    (make-rtd 'rtd2 '#((immutable x3) (immutable x4)) rtd1))

  (define rtd3
    (make-rtd 'rtd3 '#((immutable x5) (immutable x6)) rtd2))

  (define protocol1
    (^p (^[a b c] (p (+ a b) (+ b c)))))

  (define protocol2
    (^n (^[a b c d e f]
          (let1 p (n a b c)
            (p (+ d e) (+ e f))))))

  (define protocol3
    (^n (^[a b c d e f g h i]
          (let1 p (n a b c d e f)
            (p (+ g h) (+ h i))))))

  (define make-rtd1
    (protocol1 (rtd-constructor rtd1)))

  (define make-rtd2
    (let ((maker2 (rtd-constructor rtd2)))
      (protocol2
       (protocol1
        (^[x1 x2]
          (^[x3 x4]
            (maker2 x1 x2 x3 x4)))))))

  (define make-rtd3
    (let ((maker3 (rtd-constructor rtd3)))
      (protocol3
       (protocol2
        (protocol1
         (^[x1 x2]
           (^[x3 x4]
             (^[x5 x6]
               (maker3 x1 x2 x3 x4 x5 x6)))))))))

  (test* "srfi-99 example 1" '(3 5 9 11 15 17)
         (let1 r (make-rtd3 1 2 3 4 5 6 7 8 9)
           (map (^f ((rtd-accessor rtd3 f) r))
                '(x1 x2 x3 x4 x5 x6))))
  )

;; srfi-99 example 2
(let ()
  (define point (make-rtd 'point '#((mutable x) (mutable y))))

  (define make-point (rtd-constructor point))

  (define point? (rtd-predicate point))
  (define point-x (rtd-accessor point 'x))
  (define point-y (rtd-accessor point 'y))
  (define point-x-set! (rtd-mutator point 'x))
  (define point-y-set! (rtd-mutator point 'y))

  (define p1 (make-point 1 2))
  (test* "point?" #t (point? p1))
  (test* "point-x" 1 (point-x p1))
  (test* "point-y" 2 (point-y p1))
  (test* "point-x" 5 (begin (point-x-set! p1 5)
                            (point-x p1)))

  (let ()
    (define point2
      (make-rtd 'point2 '#((mutable x) (mutable y)) point))

    (define make-point2 (rtd-constructor point2))
    (define point2? (rtd-predicate point2))
    (define point2-xx (rtd-accessor point2 'x))
    (define point2-yy (rtd-accessor point2 'y))

    (define p2 (make-point2 1 2 3 4))
    (test* "point?" #t (point? p2))
    (test* "point-x" 1 (point-x p2))
    (test* "point-y" 2 (point-y p2))
    (test* "point2-xx" 3 (point2-xx p2))
    (test* "point2-yy" 4 (point2-yy p2))
    )

  (let ()
    (define make-point/abs
      (let1 maker (rtd-constructor point)
        (^[x y] (maker (abs x) (abs y)))))

    (test* "point-x make-point/abs" 1 (point-x (make-point/abs -1 -2)))
    (test* "point-y make-point/abs" 2 (point-y (make-point/abs -1 -2)))
    )

  (let ()
    (define cpoint
      (make-rtd 'cpoint '#((mutable rgb)) point))

    (define make-cpoint
      (let1 maker (rtd-constructor cpoint)
        (^[x y c] (maker x y (color->rgb c)))))

    (define make-cpoint/abs
      (let1 maker (rtd-constructor cpoint)
        (^[x y c] (maker (abs x) (abs y) (color->rgb c)))))

    (define cpoint-rgb
      (rtd-accessor cpoint 'rgb))

    (define (color->rgb c)
      (cons 'rgb c))

    (test* "cpoint-rgb" '(rgb . red)
           (cpoint-rgb (make-cpoint -1 -3 'red)))
    (test* "point-x make-cpoint" -1 (point-x (make-cpoint -1 -3 'red)))
    (test* "point-x make-cpoint/abs" 1 (point-x (make-cpoint/abs -1 -3 'red)))
    )
  )

;;--------------------------------------------------------------------
(test-section "syntactic layer")

;; srfi-9 compatibility
(define-record-type pare
  (kons x y)
  pare?
  (x kar set-kar!)
  (y kdr))

(test* "pare kons" #t (pare? (kons 1 2)))
(test* "pare kons" #f (pare? (cons 1 2)))
(test* "pare kar" 1 (kar (kons 1 2)))
(test* "pare kdr" 2 (kdr (kons 1 2)))
(test* "pare set-kar!" 3 (let1 k (kons 1 2) (set-kar! k 3) (kar k)))

(define-record-type xpare (xkons tail head) #t head tail)

(test* "xpare kons" '(1 . 2)
       (let1 k (xkons 2 1)
         (cons (xpare-head k) (xpare-tail k))))

;; record with parents.
(define-record-type (triple pare) %triple #t z)

(test* "triple" '(1 2 3)
       (let1 t (%triple 1 2 3)
         (list (kar t) (kdr t) (triple-z t))))

(test* "triple" '(1 2 3) ;this failed in 0.9.1 because of a bug.
       (let1 t (%triple 1 2 3)
         (map (cut slot-ref t <>) '(x y z))))

;; constructor argumetns
(define-record-type r0
  (make-r0 b)
  r0?
  (a r0-a)
  (b r0-b))

(test* "custom constructor argument" 3
       (r0-b (make-r0 3)))

(define-record-type r1
  (make-r1 b a)
  r1?
  (a r1-a)
  (b r1-b))

(test* "custom constructor argument" '(4 5)
       (let1 r (make-r1 5 4)
         (list (r1-a r) (r1-b r))))

;;--------------------------------------------------------------------
(test-section "pseudo record")

;; we make this a macro only because define-inline (generated by define-
;; record-type) need to be in toplevel in the current version of Gauche.
(define-macro (pseudo-record-test base-class maker)
  (define p1  (string->symbol #"~|base-class|-pseudo-record1"))
  (define p1? (string->symbol #"~|base-class|-pseudo-record1?"))
  (define make-p1 (string->symbol #"make-~|p1|"))
  (define p1-x (string->symbol #"~|p1|-x"))
  (define p1-y (string->symbol #"~|p1|-y"))
  (define p1-z (string->symbol #"~|p1|-z"))
  (define p2 (string->symbol #"~|base-class|-pseudo-record2"))
  (define make-p2 (string->symbol #"make-~|p2|"))
  (define p2-x (string->symbol #"~|p2|-x"))
  (define p2-y (string->symbol #"~|p2|-y"))
  (define p2-z (string->symbol #"~|p2|-z"))
  (define p2-x-set! (string->symbol #"~|p2|-x-set!"))
  (define p2-y-set! (string->symbol #"~|p2|-y-set!"))
  (define p2-z-set! (string->symbol #"~|p2|-z-set!"))
  `(begin
     (define-record-type (,p1 (pseudo-rtd ,base-class)) #t #t x y z)
     (let1 v #f
       (test* (format "~a record ctor" (class-name ,base-class))
              (,maker 1 2 3)
              (begin (set! v (,make-p1 1 2 3)) v))
       (test* (format "~a record predicate" (class-name ,base-class))
              '(#t #f #t)
              (list (,p1? (,maker 1 2 3))
                    (,p1? (,maker 1 2))
                    (,p1? (,maker 1 2 3 4))))
       (test* (format "~a record accessor" (class-name ,base-class))
              (coerce-to <list> v)
              (list (,p1-x v) (,p1-y v) (,p1-z v)))
       (test* (format "~a can't mutate" (class-name ,base-class))
              (test-error)
              (set! (,p1-x v) 10)))
     (define-record-type (,p2 (pseudo-rtd ,base-class)) #t #f (x) (y) (z))
     (let1 v #f
       (test* (format "~a record ctor" (class-name ,base-class))
              (,maker 1 2 3)
              (begin (set! v (,make-p2 1 2 3)) v))
       (test* (format "~a record accessor" (class-name ,base-class))
              (coerce-to <list> v)
              (list (,p2-x v) (,p2-y v) (,p2-z v)))
       (test* (format "~a mutator" (class-name ,base-class))
              (,maker 10 20 30)
              (begin (,p2-x-set! v 10)
                     (,p2-y-set! v 20)
                     (,p2-z-set! v 30)
                     v))
       (test* (format "~a record srfi-17 mutator" (class-name ,base-class))
              (coerce-to <list> (,maker 15 25 35))
              (begin (set! (,p2-x v) 15)
                     (set! (,p2-y v) 25)
                     (set! (,p2-z v) 35)
                     (list (,p2-x v) (,p2-y v) (,p2-z v))))
       )))
  
(pseudo-record-test <list> list)
(pseudo-record-test <vector> vector)
(pseudo-record-test <u8vector> u8vector)
(pseudo-record-test <s8vector> s8vector)
(pseudo-record-test <u16vector> u16vector)
(pseudo-record-test <s16vector> s16vector)
(pseudo-record-test <u32vector> u32vector)
(pseudo-record-test <s32vector> s32vector)
(pseudo-record-test <u64vector> u64vector)
(pseudo-record-test <s64vector> s64vector)
(pseudo-record-test <f16vector> f16vector)
(pseudo-record-test <f32vector> f32vector)
(pseudo-record-test <f64vector> f64vector)

;;--------------------------------------------------------------------
(test-section "inheritance")

(define-record-type base0 #t #t
  (a) (b) (c))

(define-record-type (sub0 base0) #t #t
  (a) (d) (b))

(define-record-type (subsub0 sub0) #t #t
  (e) (c))

(test* "inheritance sub0" '(1 2 3 4 5 6)
       (let1 z (make-sub0 1 2 3 4 5 6)
         (list (base0-a z)
               (base0-b z)
               (base0-c z)
               (sub0-a z)
               (sub0-d z)
               (sub0-b z))))

(test* "inheritance sub0 modifier" '(10 20 30 40 50 60)
       (let1 z (make-sub0 1 2 3 4 5 6)
         (base0-a-set! z 10)
         (base0-b-set! z 20)
         (base0-c-set! z 30)
         (sub0-a-set! z 40)
         (sub0-d-set! z 50)
         (sub0-b-set! z 60)
         (list (base0-a z)
               (base0-b z)
               (base0-c z)
               (sub0-a z)
               (sub0-d z)
               (sub0-b z))))

(test* "inheritance subsub0" '(1 2 3 4 5 6 7 8)
       (let1 z (make-subsub0 1 2 3 4 5 6 7 8)
         (list (base0-a z)
               (base0-b z)
               (base0-c z)
               (sub0-a z)
               (sub0-d z)
               (sub0-b z)
               (subsub0-e z)
               (subsub0-c z))))

(test* "inheritance subsub0 modifier" '(10 20 30 40 50 60 70 80)
       (let1 z (make-subsub0 1 2 3 4 5 6 7 8)
         (base0-a-set! z 10)
         (base0-b-set! z 20)
         (base0-c-set! z 30)
         (sub0-a-set! z 40)
         (sub0-d-set! z 50)
         (sub0-b-set! z 60)
         (subsub0-e-set! z 70)
         (subsub0-c-set! z 80)
         (list (base0-a z)
               (base0-b z)
               (base0-c z)
               (sub0-a z)
               (sub0-d z)
               (sub0-b z)
               (subsub0-e z)
               (subsub0-c z))))

(test* "inheritance access via name" '(4 6 3 5)
       (let1 z (make-sub0 1 2 3 4 5 6)
         (map (^s (slot-ref z s)) '(a b c d))))

(test* "inheritance access via name" '(4 6 8 5 7)
       (let1 z (make-subsub0 1 2 3 4 5 6 7 8)
         (map (^s (slot-ref z s)) '(a b c d e))))

(test* "inheritance mutation via name" '(40 60 30 50)
       (let1 z (make-sub0 1 2 3 4 5 6)
         (for-each (^s (slot-set! z s (* (slot-ref z s) 10))) '(a b c d))
         (map (^s (slot-ref z s)) '(a b c d))))

(test* "inheritance mutation via name" '(40 60 80 50 70)
       (let1 z (make-subsub0 1 2 3 4 5 6 7 8)
         (for-each (^s (slot-set! z s (* (slot-ref z s) 10))) '(a b c d e))
         (map (^s (slot-ref z s)) '(a b c d e))))


(define-record-type (base1 (pseudo-rtd <vector>)) #t #t
  (a) (b) (c))

(define-record-type (sub1 base1) #t #t
  (a) (d) (b))

(test* "inheritance (pseudo rtd)" '#(1 2 3 4 5 6)
       (make-sub1 1 2 3 4 5 6))

(test* "inheritance (pseudo-rtd" '(1 2 3 4 5 6)
       (let1 z (make-sub1 1 2 3 4 5 6)
         (list (base1-a z)
               (base1-b z)
               (base1-c z)
               (sub1-a z)
               (sub1-d z)
               (sub1-b z))))

;;--------------------------------------------------------------------
(test-section "describe")

(define-record-type Describe-Test #t #t a)
(define-record-type (Describe-Sub Describe-Test) #t #t a)

(test* "describe - base" #t
       (let1 x (with-output-to-string
                 (cut describe (make-Describe-Test 1)))
         (or (boolean (#/is an instance of class Describe-Test.*a\s*:\s*1/ x))
             x)))

(test* "describe - base (unbound)" #t
       (let1 x (with-output-to-string
                 (cut describe (make Describe-Test)))
         (or (boolean (#/slots:.*a\s*:\s*#<unbound>/ x))
             x)))

(test* "describe - derived" #t
       (let1 x (with-output-to-string
                 (cut describe (make-Describe-Sub 1 2)))
         (or (boolean (#/is an instance of class Describe-Sub.*slots:\s*a\s*:\s*1\s*a\s*:\s*2/ x))
             x)))

(test* "describe - derived (unbound)" #t
       (let1 x (with-output-to-string
                 (cut describe (make Describe-Sub)))
         (or (boolean (#/is an instance of class Describe-Sub.*slots:\s*a\s*:\s*#<unbound>\s*a\s*:\s*#<unbound>/ x))
             x)))

;;--------------------------------------------------------------------
(test-section "positional match")

(use util.match)

(define-record-type m0 #t #t
  (x) (y) (z))

(define-record-type (m1 m0) #t #t
  (x) (w))

(define-record-type (m2 m1) #t #t
  (w) (y))

(test* "inherited record and positional match"
       '(1 2 3 4 5 6 7)
       (match (make-m2 1 2 3 4 5 6 7)
         [($ m2 a b c d e f g)
          (list a b c d e f g)]))

(test* "inherited record and positional match (superclass)"
       '(1 2 3)
       (match (make-m2 1 2 3 4 5 6 7)
         [($ m0 a b c)
          (list a b c)]))

(test* "inherited record and positional match (superclass)"
       '(1 2 3 4 5)
       (match (make-m2 1 2 3 4 5 6 7)
         [($ m1 a b c d e)
          (list a b c d e)]))

(test* "inherited record and named match"
       '(4 7 3 6)
       (match (make-m2 1 2 3 4 5 6 7)
         [(@ m2 (x a) (y b) (z c) (w d))
          (list a b c d)]))

(test* "inherited record and named match (superclass)"
       '(4 7 3)
       (match (make-m2 1 2 3 4 5 6 7)
         [(@ m0 (x a) (y b) (z c))
          (list a b c)]))

(test-end)
