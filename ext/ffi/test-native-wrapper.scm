;;
;; Testing gauche.mop.native-wrapper
;;

(use gauche.test)
(use gauche.native-type)

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

;; Nested structures
(let ()
  (define s1 (native-type '(.struct (a::int b::char))))
  (define s2 (native-type `(.struct (c::int s::,s1 t::(,s1 *)))))
  (define h (uvector->native-handle (make-u8vector (~ s2'size))
                                    s2))
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
(test-end)
