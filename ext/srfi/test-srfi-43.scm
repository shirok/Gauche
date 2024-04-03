;;;
;;; testing SRFI-43
;;;

(use gauche.test)
(test-start "SRFI-43")
(test-section "SRFI-43")

(define-module srfi-43-tests
  (use gauche.test)

  (use srfi.43)
  (test-module 'srfi.43)

  (define-syntax assert-equal
    (syntax-rules ()
      [(_ msg expr expected)
       (test* msg expected expr)]))

  (define-syntax assert-error
    (syntax-rules ()
      [(_ msg expr)
       (test* msg (test-error) expr)]))

  (assert-equal "make-vector 0"
                (vector-length (make-vector 5))
                5)
  (assert-equal "make-vector 1"
                (make-vector 0)
                '#())
  (assert-error "make-vector 2"
                (make-vector -4))

  (assert-equal "make-vector 3"
                (make-vector 5 3)
                '#(3 3 3 3 3))
  (assert-equal "make-vector 4"
                (make-vector 0 3)
                '#())
  (assert-error "make-vector 5"
                (make-vector -1 3))

  (assert-equal "vector 0"
                (vector)
                '#())
  (assert-equal "vector 1"
                (vector 1 2 3 4 5)
                '#(1 2 3 4 5))

  (assert-equal "vector-unfold 0"
                (vector-unfold (^[i x] (values x (- x 1))) 10 0)
                '#(0 -1 -2 -3 -4 -5 -6 -7 -8 -9))
  (assert-equal "vector-unfold 1"
                (vector-unfold values 10)
                '#(0 1 2 3 4 5 6 7 8 9))
  (assert-equal "vector-unfold 2"
                (vector-unfold values 0)
                '#())
  (assert-error "vector-unfold 3"
                (vector-unfold values -1))

  (assert-equal "vector-unfold-right 0"
                (vector-unfold-right (^[i x] (values x (+ x 1))) 10 0)
                '#(9 8 7 6 5 4 3 2 1 0))
  (assert-equal "vector-unfold-right 1"
                (let ((vector '#(a b c d e)))
                  (vector-unfold-right
                   (^[i x] (values (vector-ref vector x) (+ x 1)))
                   (vector-length vector)
                   0))
                '#(e d c b a))
  (assert-equal "vector-unfold-right 2"
                (vector-unfold-right values 0)
                '#())
  (assert-error "vector-unfold-right 3"
                (vector-unfold-right values -1))

  ;; vector-copy - now built-in

  (assert-equal "vector-reverse-copy 0"
                (vector-reverse-copy '#(a b c d e))
                '#(e d c b a))
  (assert-equal "vector-reverse-copy 1"
                (vector-reverse-copy '#(a b c d e) 1 4)
                '#(d c b))
  (assert-equal "vector-reverse-copy 2"
                (vector-reverse-copy '#(a b c d e) 1 1)
                '#())
  (assert-error "vector-reverse-copy 3"
                (vector-reverse-copy '#(a b c d e) 2 1))


  ;; vector-append - now built-in

  (assert-equal "vector-concatenate 0"
                (vector-concatenate '(#(a b) #(c d)))
                '#(a b c d))
  (assert-equal "vector-concatenate 1"
                (vector-concatenate '())
                '#())
  (assert-error "vector-concatenate 2"
                (vector-concatenate '(#(a b) c)))


;;;
;;; Predicates
;;;

  (assert-equal "vector? 0" (vector? '#()) #t)
  (assert-equal "vector? 1" (vector? '#(a b)) #t)
  (assert-equal "vector? 2" (vector? '(a b)) #f)
  (assert-equal "vector? 3" (vector? 'a) #f)

  (assert-equal "vector-empty? 0" (vector-empty? '#()) #t)
  (assert-equal "vector-empty? 1" (vector-empty? '#(a)) #f)

  (assert-equal "vector= 0"
                (vector= eq? '#(a b c d) '#(a b c d))
                #t)
  (assert-equal "vector= 1"
                (vector= eq? '#(a b c d) '#(a b c d) '#(a b c d))
                #t)
  (assert-equal "vector= 2"
                (vector= eq? '#() '#())
                #t)
  (assert-equal "vector= 3"
                (vector= eq?)
                #t)
  (assert-equal "vector= 4"
                (vector= eq? '#(a))
                #t)
  (assert-equal "vector= 5"
                (vector= eq? '#(a b c d) '#(a b d c))
                #f)
  (assert-equal "vector= 6"
                (vector= eq? '#(a b c d) '#(a b c d) '#(a b d c))
                #f)
  (assert-equal "vector= 7"
                (vector= eq? '#(a b c) '#(a b d c))
                #f)
  (assert-equal "vector= 8"
                (vector= eq? '#() '#(a b d c))
                #f)
  (assert-equal "vector= 9"
                (vector= eq? '#(a b d c) '#())
                #f)
  (assert-equal "vector= 10"
                (vector= equal? '#("a" "b" "c") '#("a" "b" "c"))
                #t)
  (assert-error "vector= 11"
                (vector= equal? '#("a" "b" "c") '("a" "b" "c")))

;;;
;;; Selectors
;;;

  (assert-equal "vector-ref 0" (vector-ref '#(a b c) 0) 'a)
  (assert-equal "vector-ref 1" (vector-ref '#(a b c) 1) 'b)
  (assert-equal "vector-ref 2" (vector-ref '#(a b c) 2) 'c)
  (assert-error "vector-ref 3" (vector-ref '#(a b c) -1))
  (assert-error "vector-ref 4" (vector-ref '#(a b c) 3))
  (assert-error "vector-ref 5" (vector-ref '#() 0))

  (assert-equal "vector-length 0" (vector-length '#()) 0)
  (assert-equal "vector-length 1" (vector-length '#(a b c)) 3)
  (assert-error "vector-length 2" (vector-length '(a b c)))

;;;
;;; Iteration
;;;

  (assert-equal "vector-fold 0"
                (vector-fold (^[i seed val] (+ seed val))
                             0
                             '#(0 1 2 3 4))
                10)
  (assert-equal "vector-fold 1"
                (vector-fold (^[i seed val] (+ seed val))
                             'a
                             '#())
                'a)
  (assert-equal "vector-fold 2"
                (vector-fold (^[i seed val] (+ seed (* i val)))
                             0
                             '#(0 1 2 3 4))
                30)
  (assert-equal "vector-fold 3"
                (vector-fold (^[i seed x y] (cons (- x y) seed))
                             '()
                             '#(6 1 2 3 4) '#(7 0 9 2))
                '(1 -7 1 -1))

  (assert-equal "vector-fold-right 0"
                (vector-fold-right (^[i seed val] (cons (cons i val) seed))
                                   '()
                                   '#(a b c d e))
                '((0 . a) (1 . b) (2 . c) (3 . d) (4 . e)))
  (assert-equal "vector-fold-right 1"
                (vector-fold-right (^[i seed x y] (cons (- x y) seed))
                                   '()
                                   '#(6 1 2 3 7) '#(7 0 9 2))
                '(-1 1 -7 1))

  (assert-equal "vector-map 0"
                (vector-map cons '#(a b c d e))
                '#((0 . a) (1 . b) (2 . c) (3 . d) (4 . e)))
  (assert-equal "vector-map 1"
                (vector-map cons '#())
                '#())
  (assert-equal "vector-map 2"
                (vector-map + '#(0 1 2 3 4) '#(5 6 7 8))
                '#(5 8 11 14))

  (assert-equal "vector-map! 0"
                (rlet1 v (vector 0 1 2 3 4)
                  (vector-map! * v))
                '#(0 1 4 9 16))
  (assert-equal "vector-map! 1"
                (rlet1 v (vector)
                  (vector-map! * v))
                '#())
  (assert-equal "vector-map! 2"
                (rlet1 v (vector 0 1 2 3 4)
                  (vector-map! + v '#(5 6 7 8)))
                '#(5 8 11 14 4))

  (assert-equal "vector-for-each 0"
                (rlet1 sum 0
                  (vector-for-each (^[i x] (set! sum (+ sum (* i x))))
                                   '#(0 1 2 3 4)))
                30)
  (assert-equal "vector-for-each 1"
                (rlet1 sum 0
                  (vector-for-each (^[i x] (set! sum (+ sum (* i x))))
                                   '#()))
                0)

  (assert-equal "vector-count 0"
                (vector-count (^[i x] (even? x)) '#(0 1 2 3 4 5 6))
                4)
  (assert-equal "vector-count 1"
                (vector-count values '#())
                0)
  (assert-equal "vector-count 2"
                (vector-count (^[i x y] (< x y))
                              '#(8 2 7 4 9 1 0)
                              '#(7 6 8 3 1 1 9))
                3)

;;;
;;; Searching
;;;

  (assert-equal "vector-index 0"
                (vector-index even? '#(3 1 4 1 5 9))
                2)
  (assert-equal "vector-index 1"
                (vector-index < '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))
                1)
  (assert-equal "vector-index 2"
                (vector-index = '#(3 1 4 1 5 9 2 5 6) '#(2 7 1 8 2))
                #f)
  (assert-equal "vector-index 3"
                (vector-index < '#() '#(2 7 1 8 2))
                #f)

  (assert-equal "vector-index-right 0"
                (vector-index-right even? '#(3 1 4 1 5 9 2))
                6)
  (assert-equal "vector-index-right 1"
                (vector-index-right < '#(3 1 4 1 5) '#(2 7 1 8 2))
                3)
  (assert-equal "vector-index-right 2"
                (vector-index-right = '#(3 1 4 1 5) '#(2 7 1 8 2))
                #f)
  (assert-equal "vector-index-right 3"
                (vector-index-right even? #())
                #f)

  (assert-equal "vector-skip 0"
                (vector-skip odd? '#(3 1 4 1 5 9))
                2)
  (assert-equal "vector-skip 1"
                (vector-skip < '#(3 1 4 1 5 9 2 5 6) '#(4 9 5 0 2 4))
                3)
  (assert-equal "vector-skip 2"
                (vector-skip < '#(3 1 4 1 5 2 5 6) '#(4 9 5 9 9 9))
                #f)
  (assert-equal "vector-skip 3"
                (vector-skip < '#() '#(4 9 5 9 9 9))
                #f)

  (assert-equal "vector-skip-right 0"
                (vector-skip-right odd? '#(3 1 4 1 5 9 2 6 5 3))
                7)
  (assert-equal "vector-skip-right 1"
                (vector-skip-right < '#(8 3 7 3 1 0) '#(4 9 5 0 2 4))
                3)
  (assert-equal "vector-skip-right 2"
                (vector-skip-right < '#() '#(4 9 5 0 2 4))
                #f)

  (define (char-cmp c1 c2)
    (cond [(char<? c1 c2) -1]
          [(char=? c1 c2) 0]
          [else 1]))

  (assert-equal "vector-binary-search 0"
                (vector-binary-search
                 '#(#\a #\b #\c #\d #\e #\f #\g #\h)
                 #\g
                 char-cmp)
                6)
  (assert-equal "vector-binary-search 1"
                (vector-binary-search
                 '#(#\a #\b #\c #\d #\e #\f #\g)
                 #\q
                 char-cmp)
                #f)
  (assert-equal "vector-binary-search 2"
                (vector-binary-search
                 '#(#\a)
                 #\a
                 char-cmp)
                0)
  (assert-equal "vector-binary-search 3"
                (vector-binary-search
                 '#()
                 #\a
                 char-cmp)
                #f)
  (assert-error "vector-binary-search 4"
                (vector-binary-search
                 '(#\a #\b #\c)
                 #\a
                 char-cmp))
  (assert-equal "vector-binary-search 5"
                (vector-binary-search
                 '#(#\a #\b #\c #\d #\e #\f #\g #\h)
                 #\d
                 char-cmp
                 2 6)
                3)
  (assert-equal "vector-binary-search 6"
                (vector-binary-search
                 '#(#\a #\b #\c #\d #\e #\f #\g #\h)
                 #\g
                 char-cmp
                 2 6)
                #f)

  (assert-equal "vector-any 0"
                (vector-any even? '#(3 1 4 1 5 9 2))
                #t)
  (assert-equal "vector-any 1"
                (vector-any even? '#(3 1 5 1 5 9 1))
                #f)
  (assert-equal "vector-any 2"
                (vector-any even? '#(3 1 4 1 5 #f 2))
                #t)
  (assert-equal "vector-any 3"
                (vector-any even? '#())
                #f)
  (assert-equal "vector-any 4"
                (vector-any < '#(3 1 4 1 5 #f) '#(1 0 1 2 3))
                #t)
  (assert-equal "vector-any 5"
                (vector-any < '#(3 1 4 1 5 #f) '#(1 0 1 0 3))
                #f)

  (assert-equal "vector-every 0"
                (vector-every odd? '#(3 1 4 1 5 9 2))
                #f)
  (assert-equal "vector-every 1"
                (vector-every odd? '#(3 1 5 1 5 9 1))
                #t)
  (assert-equal "vector-every 2"
                (vector-every odd? '#(3 1 4 1 5 #f 2))
                #f)
  (assert-equal "vector-every 3"
                (vector-every even? '#())
                #t)
  (assert-equal "vector-every 4"
                (vector-every >= '#(3 1 4 1 5) '#(1 0 1 2 3 #f))
                #f)
  (assert-equal "vector-every 5"
                (vector-every >= '#(3 1 4 1 5) '#(1 0 1 0 3 #f))
                #t)

;;;
;;; Mutators
;;;

  (assert-equal "vector-set! 0"
                (rlet1 v (vector 0 1 2)
                  (vector-set! v 1 'a))
                '#(0 a 2))
  (assert-error "vector-set! 1" (vector-set! (vector 0 1 2) 3 'a))
  (assert-error "vector-set! 2" (vector-set! (vector 0 1 2) -1 'a))
  (assert-error "vector-set! 3" (vector-set! (vector) 0 'a))

  (assert-equal "vector-swap! 0"
                (rlet1 v (vector 'a 'b 'c)
                  (vector-swap! v 0 1))
                '#(b a c))
  (assert-equal "vector-swap! 1"
                (rlet1 v (vector 'a 'b 'c)
                  (vector-swap! v 1 1))
                '#(a b c))
  (assert-error "vector-swap! e0" (vector-swap! (vector 'a 'b 'c) 0 3))
  (assert-error "vector-swap! e1" (vector-swap! (vector 'a 'b 'c) -1 1))
  (assert-error "vector-swap! e2" (vector-swap! (vector) 0 0))

  (assert-equal "vector-fill! 0"
                (rlet1 v (vector 'a 'b 'c 'd 'e)
                  (vector-fill! v 'z))
                '#(z z z z z))
  (assert-equal "vector-fill! 1"
                (rlet1 v (vector 'a 'b 'c 'd 'e)
                  (vector-fill! v 'z 2))
                '#(a b z z z))
  (assert-equal "vector-fill! 2"
                (rlet1 v (vector 'a 'b 'c 'd 'e)
                  (vector-fill! v 'z 1 3))
                '#(a z z d e))
  (assert-equal "vector-fill! 3"
                (rlet1 v (vector 'a 'b 'c 'd 'e)
                  (vector-fill! v 'z 0 5))
                '#(z z z z z))
  (assert-equal "vector-fill! 4"
                (rlet1 v (vector 'a 'b 'c 'd 'e)
                  (vector-fill! v 'z 2 2))
                '#(a b c d e))
  (assert-error "vector-fill! e0" (vector-fill! (vector 'a 'b 'c) 'z 0 4))
  (assert-error "vector-fill! e1" (vector-fill! (vector 'a 'b 'c) 'z 2 1))
  (assert-error "vector-fill! e2" (vector-fill! (vector 'a 'b 'c) 'z -1 1))
                                        ;(assert-error "vector-fill! e3" (vector-fill! (vector) 'z 0 0))

  (assert-equal "vector-reverse! 0"
                (rlet1 v (vector 'a 'b 'c 'd 'e)
                  (vector-reverse! v))
                '#(e d c b a))
  (assert-equal "vector-reverse! 1"
                (rlet1 v (vector 'a 'b 'c 'd 'e 'f)
                  (vector-reverse! v 1 4))
                '#(a d c b e f))
  (assert-equal "vector-reverse! 2"
                (rlet1 v (vector 'a 'b 'c 'd 'e 'f)
                  (vector-reverse! v 3 3))
                '#(a b c d e f))
  (assert-equal "vector-reverse! 3"
                (rlet1 v (vector 'a 'b 'c 'd 'e 'f)
                  (vector-reverse! v 3 4))
                '#(a b c d e f))
  (assert-equal "vector-reverse! 4"
                (rlet1 v (vector)
                  (vector-reverse! v))
                '#())
  (assert-error "vector-reverse! e0" (vector-reverse! (vector 'a 'b) 0 3))
  (assert-error "vector-reverse! e1" (vector-reverse! (vector 'a 'b) 2 1))
  (assert-error "vector-reverse! e2" (vector-reverse! (vector 'a 'b) -1 1))

  ;; vector-copy! - now built-in

  (assert-equal "vector-reverse-copy! 0"
                (rlet1 v (vector 'a 'b 'c 'd 'e)
                  (vector-reverse-copy! v 0 '#(1 2 3)))
                '#(3 2 1 d e))
  (assert-equal "vector-reverse-copy! 1"
                (rlet1 v (vector 'a 'b 'c 'd 'e)
                  (vector-reverse-copy! v 2 '#(1 2 3)))
                '#(a b 3 2 1))
  (assert-equal "vector-reverse-copy! 2"
                (rlet1 v (vector 'a 'b 'c 'd 'e)
                  (vector-reverse-copy! v 2 '#(1 2 3) 1))
                '#(a b 3 2 e))
  (assert-equal "vector-reverse-copy! 3"
                (rlet1 v (vector 'a 'b 'c 'd 'e)
                  (vector-reverse-copy! v 2 '#(1 2 3 4 5) 1 4))
                '#(a b 4 3 2))
  (assert-equal "vector-reverse-copy! 4"
                (rlet1 v (vector 'a 'b 'c 'd 'e)
                  (vector-reverse-copy! v 2 '#(1 2 3 4 5) 2 2))
                '#(a b c d e))
  (assert-equal "vector-reverse-copy! self0"
                (rlet1 v (vector 'a 'b 'c 'd 'e)
                  (vector-reverse-copy! v 0 v))
                '#(e d c b a))
  (assert-equal "vector-reverse-copy! self1"
                (rlet1 v (vector 'a 'b 'c 'd 'e)
                  (vector-reverse-copy! v 0 v 0 2))
                '#(b a c d e))
  (assert-error "vector-reverse-copy! e0"
                (vector-reverse-copy! (vector 'a 'b) 2 '#(a b)))
  (assert-error "vector-reverse-copy! e1"
                (vector-reverse-copy! (vector 'a 'b) -1 '#(a b)))
  (assert-error "vector-reverse-copy! e2"
                (vector-reverse-copy! (vector 'a 'b) 0 '#(a b c)))
  (assert-error "vector-reverse-copy! e3"
                (vector-reverse-copy! (vector 'a 'b) 0 '#(a b c) 1 4))
  (assert-error "vector-reverse-copy! e4"
                (vector-reverse-copy! (vector 'a 'b) 0 '#(a b c) -1 2))
  (assert-error "vector-reverse-copy! e5"
                (vector-reverse-copy! (vector 'a 'b) 0 '#(a b c) 2 1))

;;;
;;; Conversion
;;;

  (assert-equal "vector->list 0"
                (vector->list '#(a b c))
                '(a b c))
  (assert-equal "vector->list 1"
                (vector->list '#(a b c) 1)
                '(b c))
  (assert-equal "vector->list 2"
                (vector->list '#(a b c d e) 1 4)
                '(b c d))
  (assert-equal "vector->list 3"
                (vector->list '#(a b c d e) 1 1)
                '())
  (assert-equal "vector->list 4"
                (vector->list '#())
                '())
  (assert-error "vector->list e0" (vector->list '#(a b c) 1 6))
  (assert-error "vector->list e1" (vector->list '#(a b c) -1 1))
  (assert-error "vector->list e2" (vector->list '#(a b c) 2 1))

  (assert-equal "reverse-vector->list 0"
                (reverse-vector->list '#(a b c))
                '(c b a))
  (assert-equal "reverse-vector->list 1"
                (reverse-vector->list '#(a b c) 1)
                '(c b))
  (assert-equal "reverse-vector->list 2"
                (reverse-vector->list '#(a b c d e) 1 4)
                '(d c b))
  (assert-equal "reverse-vector->list 3"
                (reverse-vector->list '#(a b c d e) 1 1)
                '())
  (assert-equal "reverse-vector->list 4"
                (reverse-vector->list '#())
                '())
  (assert-error "reverse-vector->list e0" (reverse-vector->list '#(a b c) 1 6))
  (assert-error "reverse-vector->list e1" (reverse-vector->list '#(a b c) -1 1))
  (assert-error "reverse-vector->list e2" (reverse-vector->list '#(a b c) 2 1))

  (assert-equal "list->vector 0"
                (list->vector '(a b c))
                '#(a b c))
  (assert-equal "list->vector 1"
                (list->vector '())
                '#())
  (assert-equal "list->vector 2"
                (list->vector '(0 1 2 3) 2)
                '#(2 3))
  (assert-equal "list->vector 3"
                (list->vector '(0 1 2 3) 0 2)
                '#(0 1))
  (assert-equal "list->vector 4"
                (list->vector '(0 1 2 3) 2 2)
                '#())
  (assert-error "list->vector e0" (list->vector '(0 1 2 3) 0 5))
  (assert-error "list->vector e1" (list->vector '(0 1 2 3) -1 1))
  (assert-error "list->vector e2" (list->vector '(0 1 2 3) 2 1))

  (assert-equal "reverse-list->vector 0"
                (reverse-list->vector '(a b c))
                '#(c b a))
  (assert-equal "reverse-list->vector 1"
                (reverse-list->vector '())
                '#())
  (assert-equal "reverse-list->vector 2"
                (reverse-list->vector '(0 1 2 3) 2)
                '#(3 2))
  (assert-equal "reverse-list->vector 3"
                (reverse-list->vector '(0 1 2 3) 0 2)
                '#(1 0))
  (assert-equal "reverse-list->vector 4"
                (reverse-list->vector '(0 1 2 3) 2 2)
                '#())
  (assert-error "reverse-list->vector e0"
                (reverse-list->vector '(0 1 2 3) 0 5))
  (assert-error "reverse-list->vector e1"
                (reverse-list->vector '(0 1 2 3) -1 1))
  (assert-error "reverse-list->vector e2"
                (reverse-list->vector '(0 1 2 3) 2 1))
  )

(test-end)
