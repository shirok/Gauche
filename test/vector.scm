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

(test-end)

