;;
;; Testing gauche.mop.native-wrapper
;;

(use gauche.test)
(use gauche.native-type)
(use gauche.sequence)

(test-start "native wrapper")

;; Single-level struct/union

(let ()
  (define st (native-type '(.struct st (a::int b::char))))
  (define cst)
  (define ist)
  (define ut (native-type '(.union ut (a::int b::char))))
  (define cut)
  (define iut)

  (test* "struct to class - is a subclass of <wrapped-c-struct>" #T
         (begin
           (set! cst (make-native-wrapper-class st))
           (subclass? cst <wrapped-c-struct>)))

  (test* "struct to class - slots" '(a b %handle)
         (map slot-definition-name (class-slots cst)))

  (test* "struct to class - class identity" #t
         (eq? cst (make-native-wrapper-class st)))
  (test* "struct to class - class identity" #f
         (eq? cst ($ make-native-wrapper-class
                     (native-type '(.struct st (a::int b::char))))))

  (test* "struct instantiation" #t
         (begin
           (set! ist (make cst :a 10 :b #\x))
           (is-a? ist <wrapped-c-struct>)))

  (test* "struct instantiation - slot ref" '(10 #\x)
         (list (~ ist 'a) (~ ist 'b)))

  (test* "struct instantiation - slot set!" '(12 #\y)
         (begin
           (set! (~ ist 'a) 12)
           (set! (~ ist 'b) #\y)
           (list (~ ist 'a) (~ ist 'b))))

  (test* "union to class - is a subclass of <wrapped-c-union>" #T
         (begin
           (set! cut (make-native-wrapper-class ut))
           (subclass? cut <wrapped-c-union>)))

  (test* "union to class - slots" '(a b %handle)
         (map slot-definition-name (class-slots cut)))

  (test* "union instantiation" #t
         (begin
           (set! iut (make cut :a 10))
           (is-a? iut <wrapped-c-union>)))

  (test* "union instantiation - slot ref" 10
         (~ iut 'a))

  (test* "union instantiation - slot set!" #\z
         (begin
           (set! (~ iut 'b) #\z)
           (~ iut 'b)))
  )

(let ()
  (define st (native-type '(.struct st (a::int b::int))))
  (define h (make-native-handle st))
  (define w)

  (test* "wrapping pointer to struct" #t
         (begin
           (set! w (wrap-native-handle h))
           (is-a? w <wrapped-c-struct>)))

  (set! (native. h 'a) 1)
  (set! (native. h 'b) 2)
  (test* "accessing through wrapped pointer" '(1 2)
         (list (~ w'a) (~ w'b)))

  (test* "modifyhing through wrapperd pointer" '(3 4)
         (begin
           (set! (~ w'a) 3)
           (set! (~ w'b) 4)
           (list (native. h 'a) (native. h 'b))))
  )

(let ()
  (define st (native-type '(.struct st (a::int b::int))))
  (define h (null-pointer-handle (make-c-pointer-type st)))

  (test* "wrapping null pointer yields #f" #f
         (wrap-native-handle h)))

;; Nested structures
(let ()
  (define s1 (native-type '(.struct (a::int b::char))))
  (define s2 (native-type `(.struct (c::int s::,s1 t::(,s1 *)))))
  (define h (make-native-handle s2))
  (define w)

  (set! (native. h 't) (native& h 's))

  (test* "nested struct to class" #t
         (begin
           (set! w (wrap-native-handle h))
           (is-a? w <wrapped-c-struct>)))

  (test* "inner struct, embedded" '(0 #\null)
         (and (is-a? (~ w's) <wrapped-c-struct>)
              (list (~ w's'a) (~ w's'b))))

  (test* "inner struct, pointed" '(0 #\null)
         (and (is-a? (~ w't) <wrapped-c-struct>)
              (list (~ w't'a) (~ w't'b))))
  )

;; arrays
(let ()
  (define a1 (native-type '(.array int (10))))
  (define h (make-native-handle a1))
  (define w)

  (dotimes [i 10]
    (set! (native-aref h i) (* i i)))
  (test* "wrapped array" #t
         (begin
           (set! w (wrap-native-handle h))
           (is-a? w <wrapped-c-array>)))
  (test* "wrapped array size-of" 10  (size-of w))
  (test* "wrapped array sequence iterator" '(0 1 4 9 16 25 36 49 64 81)
         (coerce-to <list> w))
  (test* "wrapped array sequence set! and ref" '(0 1 2 3 4 5 6 7 8 9)
         (begin
           (dotimes [i 10] (set! (~ w i) i))
           (map (cut ~ w <>) (iota 10))))
  )

(let ()
  (define a1 (native-type '(.array int8_t (10 5))))
  (define h (make-native-handle a1))
  (define w)

  (dotimes [i 10]
    (dotimes [j 5]
      (set! (native-aref h i j) (* i j))))
  (test* "wrapped array 2d" #t
         (begin
           (set! w (wrap-native-handle h))
           (is-a? w <wrapped-c-array>)))
  (test* "wrapped array 2d size-of" 10
         (size-of w)) ; sequence size, so it's 10
  (test* "wrapped array 2d subarray type" #t
         (is-a? (~ w 0) <wrapped-c-array>))
  (test* "wrapped array 2d subarray" '(0 2 4 6 8)
         (coerce-to <list> (~ w 2)))
  (test* "wrapped array 2d ref element" 36
         (ref w '(9 4)))
  (test* "wrapped array 2d ~ element" 36
         (~ w 9 4))
  (test* "wrapped array 2d set element" '(0 9 18 27 -1)
         (begin (set! (~ w 9 4) -1)
                (coerce-to <list> (~ w 9))))
  (test* "wrapped array subarray set" '(-1 -2 -3 -4 -5)
         (begin
           (set! (~ w 3)
                 (make-native-handle (native-type '(.array int8_t (5)))
                                     '#s8(-1 -2 -3 -4 -5)))
           (coerce-to <list> (~ w 3))))
  )

(test-end)
