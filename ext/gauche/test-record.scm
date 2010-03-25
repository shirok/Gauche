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

(let ((r1a #f) (r1b #f) (r2 #f))
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
    (lambda (p)
      (lambda (a b c)
        (p (+ a b) (+ b c)))))

  (define protocol2
    (lambda (n)
      (lambda (a b c d e f)
        (let ((p (n a b c)))
          (p (+ d e) (+ e f))))))

  (define protocol3
    (lambda (n)
      (lambda (a b c d e f g h i)
        (let ((p (n a b c d e f)))
          (p (+ g h) (+ h i))))))

  (define make-rtd1
    (protocol1 (rtd-constructor rtd1)))

  (define make-rtd2
    (let ((maker2 (rtd-constructor rtd2)))
      (protocol2
       (protocol1
        (lambda (x1 x2)
          (lambda (x3 x4)
            (maker2 x1 x2 x3 x4)))))))

  (define make-rtd3
    (let ((maker3 (rtd-constructor rtd3)))
      (protocol3
       (protocol2
        (protocol1
         (lambda (x1 x2)
           (lambda (x3 x4)
             (lambda (x5 x6)
               (maker3 x1 x2 x3 x4 x5 x6)))))))))

  (test* "srfi-99 example 1" '(3 5 9 11 15 17)
         (let1 r (make-rtd3 1 2 3 4 5 6 7 8 9)
           (map (lambda (f) ((rtd-accessor rtd3 f) r))
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
      (let ((maker (rtd-constructor point)))
        (lambda (x y)
          (maker (abs x) (abs y)))))

    (test* "point-x make-point/abs" 1 (point-x (make-point/abs -1 -2)))
    (test* "point-y make-point/abs" 2 (point-y (make-point/abs -1 -2)))
    )

  (let ()
    (define cpoint
      (make-rtd 'cpoint '#((mutable rgb)) point))

    (define make-cpoint
      (let ((maker (rtd-constructor cpoint)))
        (lambda (x y c)
          (maker x y (color->rgb c)))))

    (define make-cpoint/abs
      (let ((maker (rtd-constructor cpoint)))
        (lambda (x y c)
          (maker (abs x) (abs y) (color->rgb c)))))

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
(test* "pare set-kar!" 3 (let ((k (kons 1 2))) (set-kar! k 3) (kar k)))

(define-record-type xpare (xkons tail head) #t head tail)

(test* "xpare kons" '(1 . 2)
       (let ((k (xkons 2 1)))
         (cons (xpare-head k) (xpare-tail k))))

;;--------------------------------------------------------------------
(test-section "pseudo record")

(define-record-type (vec-rec1 (pseudo-rtd <vector>)) #t #f x y z)
(let ((v #f))
  (test* "vector record ctor" '#(1 2 3)
         (begin (set! v (make-vec-rec1 1 2 3)) v))
  (test* "accessor" '(1 2 3)
         (list (vec-rec1-x v) (vec-rec1-y v) (vec-rec1-z v)))
  (test* "can't mutate" (test-error)
         (set! (vec-rec1-x v) 10)))

(define-record-type (vec-rec2 (pseudo-rtd <vector>)) #t #f (x) (y) (z))
(let ((v #f))
  (test* "vector record ctor" '#(1 2 3)
         (begin (set! v (make-vec-rec2 1 2 3)) v))
  (test* "accessor" '(1 2 3)
         (list (vec-rec2-x v) (vec-rec2-y v) (vec-rec2-z v)))
  (test* "mutator"  '(10 20 30)
         (begin (vec-rec2-x-set! v 10)
                (vec-rec2-y-set! v 20)
                (vec-rec2-z-set! v 30)
                (list (vec-rec2-x v) (vec-rec2-y v) (vec-rec2-z v))))
  (test* "srfi-17 mutator" '(100 200 300)
         (begin (set! (vec-rec2-x v) 100)
                (set! (vec-rec2-y v) 200)
                (set! (vec-rec2-z v) 300)
                (list (vec-rec2-x v) (vec-rec2-y v) (vec-rec2-z v))))
  )

(test-end)
