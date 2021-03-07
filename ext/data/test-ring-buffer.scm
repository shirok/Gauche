;; ring-buffer
(test-section "data.ring-buffer")
(use data.ring-buffer)
(test-module 'data.ring-buffer)
(use gauche.sequence)
(use gauche.uvector)
(use scheme.list)


(define (test-ring-buffer initial-storage)
  (define c (class-name (class-of initial-storage)))
  (define (remove-all-from-front rb)
    (unfold ring-buffer-empty?
            ring-buffer-remove-front!
            identity
            rb))
  (define (remove-all-from-back rb)
    (unfold ring-buffer-empty?
            ring-buffer-remove-back!
            identity
            rb))
  (define (test-add-remove n)
    (let1 rb (make-ring-buffer initial-storage)
      (test* #"add front - remove front (~c, ~n))"
             (reverse (iota n))
             (begin
               (dotimes (i n) (ring-buffer-add-front! rb i))
               (remove-all-from-front rb))))
    (let1 rb (make-ring-buffer initial-storage)
      (test* #"add front - remove back (~c, ~n))"
             (iota n)
             (begin
               (dotimes (i n) (ring-buffer-add-front! rb i))
               (remove-all-from-back rb))))
    (let1 rb (make-ring-buffer initial-storage)
      (test* #"add back - remove front (~c, ~n))"
             (iota n)
             (begin
               (dotimes (i n) (ring-buffer-add-back! rb i))
               (remove-all-from-front rb))))
    (let1 rb (make-ring-buffer initial-storage)
      (test* #"add back - remove back (~c, ~n))"
             (reverse (iota n))
             (begin
               (dotimes (i n) (ring-buffer-add-back! rb i))
               (remove-all-from-back rb)))))

  (define (test-basic)
    (let1 rb (make-ring-buffer initial-storage)
      (test* #"empty (~c)" #t (ring-buffer-empty? rb))
      (test* #"empty (~c)" 0  (ring-buffer-num-entries rb))
      (test* #"initial (~c)" '(10 10 1 #f)
             (begin (ring-buffer-add-front! rb 10)
                    (list (ring-buffer-front rb)
                          (ring-buffer-back rb)
                          (ring-buffer-num-entries rb)
                          (ring-buffer-empty? rb))))
      (test* #"2 elemenst (~c)" '(10 11 2 #f)
             (begin (ring-buffer-add-back! rb 11)
                    (list (ring-buffer-front rb)
                          (ring-buffer-back rb)
                          (ring-buffer-num-entries rb)
                          (ring-buffer-empty? rb))))))
  (define (test-overflow)
    (let1 rb (make-ring-buffer initial-storage :overflow-handler 'error)
      (test* #"overflow - error (~c)" (test-error)
             (dotimes [i (+ (size-of initial-storage) 1)]
               (ring-buffer-add-back! rb i))))
    (let1 rb (make-ring-buffer initial-storage :overflow-handler 'overwrite)
      (test* #"overflow - overwrite forward (~c)"
             (iota (size-of initial-storage)
                   (size-of initial-storage))
             (begin
               (dotimes [i (* (size-of initial-storage) 2)]
                 (ring-buffer-add-back! rb i))
               (remove-all-from-front rb))))
    (let1 rb (make-ring-buffer initial-storage :overflow-handler 'overwrite)
      (test* #"overflow - overwrite backward (~c)"
             (reverse (iota (size-of initial-storage)
                            (size-of initial-storage)))
             (begin
               (dotimes [i (* (size-of initial-storage) 2)]
                 (ring-buffer-add-front! rb i))
               (remove-all-from-front rb)))))

  ;; body of test-ring-buffer
  (test-basic)
  (test-overflow)
  (test-add-remove 3)
  (test-add-remove 12) ; causes realloc
  )

(test-ring-buffer (make-vector 4))
(test-ring-buffer (make-u8vector 5))

;; ring-buffer->xvector
(let ((buf (make-ring-buffer (make-u8vector 4))))
  (test* "->xvector (empty)" '#u8()
         (ring-buffer->xvector buf))
  (ring-buffer-add-back! buf 1)
  (test* "->xvector (1)" '#u8(1)
         (ring-buffer->xvector buf))
  (ring-buffer-add-back! buf 2)
  (test* "->xvector (2)" '#u8(1 2)
         (ring-buffer->xvector buf))
  (ring-buffer-add-back! buf 3)
  (test* "->xvector (3)" '#u8(1 2 3)
         (ring-buffer->xvector buf))
  (test* "->xvector (3, range)" '#u8(2 3)
         (ring-buffer->xvector buf 1))
  (test* "->xvector (3, range)" '#u8(2)
         (ring-buffer->xvector buf 1 2))
  (ring-buffer-remove-front! buf)
  (ring-buffer-remove-front! buf)
  (test* "->xvector (-2)" '#u8(3)
         (ring-buffer->xvector buf))
  (ring-buffer-add-back! buf 4)
  (ring-buffer-add-back! buf 5)
  (test* "->xvector (3)" '#u8(3 4 5)
         (ring-buffer->xvector buf))
  (test* "->xvector (3, range)" '#u8(5)
         (ring-buffer->xvector buf 2))
  (test* "->xvector (3, range)" '#u8(3 4)
         (ring-buffer->xvector buf 0 2))
  (ring-buffer-remove-front! buf)
  (ring-buffer-remove-front! buf)
  (ring-buffer-remove-front! buf)
  (test* "->xvector (0)" '#u8()
         (ring-buffer->xvector buf))
  )

;; from the manual
(let ((rb (make-ring-buffer (make-vector 4))))
  (test* "->xvector (empty)" '#()
         (ring-buffer->xvector rb))
  (ring-buffer-add-back! rb 'a)
  (ring-buffer-add-back! rb 'b)
  (ring-buffer-add-front! rb 'z)
  (ring-buffer-add-front! rb 'y)
  (test* "-> xvector (4)" '#(y z a b)
         (ring-buffer->xvector rb))
  (test* "-> xvector (4, range)" '#(z a)
         (ring-buffer->xvector rb 1 3))
  )

;; test with initial data
(let ((buf (make-ring-buffer (vector 'a 'b 'c 'd 'e)
                             :initial-head-index 1
                             :initial-tail-index 5)))
  (test* "ring-buffer/initial" '(4 b e)
         (list (ring-buffer-num-entries buf)
               (ring-buffer-front buf)
               (ring-buffer-back buf)))
  (ring-buffer-add-back! buf 'f)
  (ring-buffer-add-front! buf 'z)
  (test* "ring-buffer/initial expand" '#(z b c d e f)
         (ring-buffer->xvector buf))
  (ring-buffer-remove-front! buf)
  (ring-buffer-remove-front! buf)
  (ring-buffer-remove-front! buf)
  (ring-buffer-add-back! buf 'g)
  (ring-buffer-add-back! buf 'h)
  (ring-buffer-add-back! buf 'i)
  (ring-buffer-add-back! buf 'j)
  (ring-buffer-add-back! buf 'k)
  (ring-buffer-add-back! buf 'l)
  (ring-buffer-add-back! buf 'm)
  (test* "ring-buffer/initial expand" '#(d e f g h i j k l m)
         (ring-buffer->xvector buf))

  (test* "ring-buffer-clear!" '#()
         (begin
           (ring-buffer-clear! buf)
           (ring-buffer->xvector buf)))

  (ring-buffer-add-front! buf 'A)
  (ring-buffer-add-front! buf 'B)
  (ring-buffer-add-back! buf 'Z)
  (test* "ring-buffer add after clear" '#(B A Z)
         (ring-buffer->xvector buf))
  )

;; flat vector fill
(let ((buf (make-ring-buffer (make-s16vector 5))))
  (ring-buffer-add-back! buf 1)
  (ring-buffer-add-back! buf 2)
  (ring-buffer-add-back! buf 3)
  (ring-buffer-add-back! buf 4)
  (ring-buffer-add-back! buf 5)
  (ring-buffer-remove-front! buf)
  (ring-buffer-remove-front! buf)
  (ring-buffer-add-back! buf 6)
  (ring-buffer-add-back! buf 7)

  (test* "ring-buffer->xvector!"
         '#s16(0 0 0 3 4 5 6 7 0 0)
         (rlet1 v (make-s16vector 10)
           (ring-buffer->xvector! v 3 buf)))
  (ring-buffer-add-front! buf -1)
  (ring-buffer-add-front! buf -2)
  (ring-buffer-add-front! buf -3)
  (test* "ring-buffer->xvector!"
         '#s16(-2 -1 3 4 5 0 0 0 0 0)
         (rlet1 v (make-s16vector 10)
           (ring-buffer->xvector! v 0 buf 1 6)))
  )

;; insert
(let ((buf (make-ring-buffer (make-vector 4)
                             :room-maker (^[rb old size] (make-vector size)))))
  (ring-buffer-add-back! buf 'a)
  (ring-buffer-add-back! buf 'b)
  (ring-buffer-add-back! buf 'c)
  (ring-buffer-add-front! buf 'z)
  (test* "ring-buffer-insert-all!"
         '#(z a p q r b c)
         (begin
           (ring-buffer-insert-all! buf 2 '(p q r))
           (ring-buffer->xvector buf)))
  (test* "ring-buffer-insert-all!"
         '#(z a p q r b c d e f g h i j k l m n o s t u v)
         (begin
           (ring-buffer-insert-all! buf 7 '#(d e f g h i j k l m n o s t u v))
           (ring-buffer->xvector buf)))
  )

;; sequence protocol
(let ((buf (make-ring-buffer)))
  (ring-buffer-add-back! buf 'a)
  (ring-buffer-add-back! buf 'b)
  (ring-buffer-add-back! buf 'c)
  (ring-buffer-add-front! buf 'z)
  (ring-buffer-add-front! buf 'y)
  (ring-buffer-add-front! buf 'x)
  (test* "ring-buffer sequence protocol" '(x y z a b c)
         (coerce-to <list> buf)))
