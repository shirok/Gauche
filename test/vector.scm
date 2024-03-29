;;
;; tests for vectors
;;

(use gauche.test)

(test-start "vectors")

;;-------------------------------------------------------------------
(test-section "builtins")

(test* "vector" '#(a b c d e f g) (vector 'a 'b 'c 'd  'e 'f 'g))
(test* "vector" '#() (vector))

(test* "list->vector" `(#(a b c d e f g) #(b c d e) #(c))
       (let1 data '(a b c d e f g)
         (map (^i (list->vector data i (- (length data) (* 2 i))))
              (iota 3))))
(test* "list->vector" (test-error) (list->vector '(a) 3))
(test* "list->vector" (test-error) (list->vector '(a b c) 2 1))
(test* "list->vector" '#() (list->vector '(a b c) 2 2))
(test* "reverse-list->vector" `(#(a b c d e f g) #(c d e f) #(e))
       (let1 data '(g f e d c b a)
         (map (^i (reverse-list->vector data i (- (length data) (* 2 i))))
              (iota 3))))
(test* "reverse-list->vector" (test-error) (reverse-list->vector '(a) 3))
(test* "reverse-list->vector" (test-error) (reverse-list->vector '(a b c) 2 1))
(test* "reverse-list->vector" '#() (reverse-list->vector '(a b c) 2 2))

(test* "vector-tabulate" '#(0 2 4 6 8)
       (vector-tabulate 5 (cut * <> 2)))
(test* "vector-tabulate with restart"
       '(#(0 1 yo 6 8) #(0 1 2 3 4))
       (let ([v0 #f]
             [v1 #f]
             [cont #f])
         (set! v0 (vector-tabulate 5 (^i (cond [v0 (* i 2)]
                                               [(= i 2) (let/cc c
                                                          (set! cont c)
                                                          i)]
                                               [else i]))))
         (if v1
           (list v0 v1)
           (begin (set! v1 v0) (cont 'yo)))))

(test* "vector-copy 0" '#(a b c d e f g h i)
       (vector-copy '#(a b c d e f g h i)))
(test* "vector-copy 1" '#(g h i)
       (vector-copy '#(a b c d e f g h i) 6))
(test* "vector-copy 2" '#(d e f)
       (vector-copy '#(a b c d e f g h i) 3 6))
(test* "vector-copy 3" '#(g h i x x x)
       (vector-copy '#(a b c d e f g h i) 6 12 'x))
(test* "vector-copy 4" '#()
       (vector-copy '#(a b c d e f g h i) 6 6))
(test* "vector-copy 5" (test-error)
       (vector-copy '#(a b c d e f g h i) 4 2))

(test* "vector-copy! 0" '#(1 2 3 d e)
       (rlet1 v (vector 'a 'b 'c 'd 'e)
         (vector-copy! v 0 '#(1 2 3))))
(test* "vector-copy! 1" '#(a b 1 2 3)
       (rlet1 v (vector 'a 'b 'c 'd 'e)
         (vector-copy! v 2 '#(1 2 3))))
(test* "vector-copy! 2" '#(a b 2 3 e)
       (rlet1 v (vector 'a 'b 'c 'd 'e)
         (vector-copy! v 2 '#(1 2 3) 1)))
(test* "vector-copy! 3" '#(a b 3 4 5)
       (rlet1 v (vector 'a 'b 'c 'd 'e)
         (vector-copy! v 2 '#(1 2 3 4 5) 2 5)))
(test* "vector-copy! 4" '#(a b c d e)
       (rlet1 v (vector 'a 'b 'c 'd 'e)
         (vector-copy! v 2 '#(1 2 3) 1 1)))
(test* "vector-copy! self0" '#(b c c d e)
       (rlet1 v (vector 'a 'b 'c 'd 'e)
         (vector-copy! v 0 v 1 3)))
(test* "vector-copy! self1" '#(a b b c d)
       (rlet1 v (vector 'a 'b 'c 'd 'e)
         (vector-copy! v 2 v 1 4)))
(test* "vector-copy! self2" '#(a b c d e)
       (rlet1 v (vector 'a 'b 'c 'd 'e)
         (vector-copy! v 0 v 0)))
(test* "vector-copy! e0" (test-error) (vector-copy! (vector 1 2) 3 '#(1 2 3)))
(test* "vector-copy! e1" (test-error) (vector-copy! (vector 1 2) 0 '#(1 2 3)))
(test* "vector-copy! e2" (test-error) (vector-copy! (vector 1 2) 1 '#(1 2 3) 1))

(test* "vector-append 0" '#(x y)
              (vector-append '#(x) '#(y)))
(test* "vector-append 1" '#(x y x y x y)
              (let ((v '#(x y)))
                (vector-append v v v)))
(test* "vector-append 2" '#(x y)
              (vector-append '#(x) '#() '#(y)))
(test* "vector-append 3" '#()
              (vector-append))
(test* "vector-append 4" (test-error) (vector-append '#() 'b 'c))

;; immutable vectors
;; literal vectors are immutable only when compiled with API_VERSION >= 1000
(when (vector-immutable? '#(1 2 3))
  (test* "vector-immutable?" #f
         (vector-immutable? (vector 1 2 3)))
  (test* "set! to immutalbe vector" (test-error)
         (vector-set! '#(1 2 3) 0 3))
  (test* "set! to immutalbe vector" (test-error)
         (set! (vector-ref '#(1 2 3) 0) 3))
  (test* "fill! to immutalbe vector" (test-error)
         (vector-fill!! '#(1 2 3) 0)))

;;-------------------------------------------------------------------
(test-section "bitvector builtins")

;; we have comprehensive tests in srfi-178-tests.  here we cover
;; non-srfi procedures.

(test* "bitvector-any-value? #t 1" #t (bitvector-any-value? #*0001000 1))
(test* "bitvector-any-value? #t 2" #t
       (bitvector-any-value? #*000000000000000000000000000000000000000000000000000000000000000000000000000000000000001000 #t))
(test* "bitvector-any-value? #t 3" #t
       (bitvector-any-value? #*000100000000000000000000000000000000000000000000000000000000000000000000000000000000000000 1))
(test* "bitvector-any-value? #t 4" #f
       (bitvector-any-value? #*00000000000 1))
(test* "bitvector-any-value? #t 5" #f
       (bitvector-any-value? #* 1))
(test* "bitvector-any-value? #t 6" #f
       (bitvector-any-value? #*000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000 1))

(test* "bitvector-any-value? #f 1" #t (bitvector-any-value? #*1110111 0))
(test* "bitvector-any-value? #f 2" #f (bitvector-any-value? #*1111111 0))
(test* "bitvector-any-value? #f 3" #f (bitvector-any-value? #* 0))
(test* "bitvector-any-value? #f 4" #t
       (bitvector-any-value? #*1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111011 0))
(test* "bitvector-any-value? #f 5" #f
       (bitvector-any-value? #*1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 0))

(test* "bitvector-every-value? #t 1" #t (bitvector-every-value? #*11111 1))
(test* "bitvector-every-value? #t 2" #t (bitvector-every-value? #* #t))
(test* "bitvector-every-value? #t 3" #t
       (bitvector-every-value? #*11111111111111111111111111111111111111111111111111111111111111111111111111111111111111 1))
(test* "bitvector-every-value? #t 4" #f (bitvector-every-value? #*11101 1))
(test* "bitvector-every-value? #t 5" #f
       (bitvector-every-value? #*11111111111111111111111111111111111111111111111111111111111111111111111111111111111110 1))
(test* "bitvector-every-value? #t 6" #f
       (bitvector-every-value? #*11111111101111111111111111111111111111111111111111111111111111111111111111111111111111 1))

(test* "bitvector-every-value? #f 1" #t (bitvector-every-value? #*00000 0))
(test* "bitvector-every-value? #f 2" #t (bitvector-every-value? #* #f))
(test* "bitvector-every-value? #f 3" #t
       (bitvector-every-value? #*00000000000000000000000000000000000000000000000000000000000000000000000000000000000000 0))
(test* "bitvector-every-value? #f 4" #f
       (bitvector-every-value? #*00000000000000000000000000000000000000000000000000000000000000000000000000000000000010 #f))
(test* "bitvector-every-value? #f 5" #f
       (bitvector-every-value? #*00000001000000000000000000000000000000000000000000000000000000000000000000000000000000 0))

(test-end)
