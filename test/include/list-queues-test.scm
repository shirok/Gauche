(test-group "list-queues"

(test-group "list-queues/simple"
  (test '(1 1 1) (list-queue-list (make-list-queue '(1 1 1))))
  (define x (list-queue 1 2 3))
  (test '(1 2 3) (list-queue-list x))
  (define x1 (list 1 2 3))
  (define x2 (make-list-queue x1 (cddr x1)))
  (test 3 (list-queue-back x2))
  (define y (list-queue 4 5))
  (test-assert (list-queue? y))
  (define z (list-queue-append x y))
  (test '(1 2 3 4 5) (list-queue-list z))
  (define z2 (list-queue-append! x (list-queue-copy y)))
  (test '(1 2 3 4 5) (list-queue-list z2))
  (test 1 (list-queue-front z))
  (test 5 (list-queue-back z))
  (list-queue-remove-front! y)
  (test '(5) (list-queue-list y))
  (list-queue-remove-back! y)
  (test-assert (list-queue-empty? y))
  (test-error (list-queue-remove-front! y))
  (test-error (list-queue-remove-back! y))
  (test '(1 2 3 4 5) (list-queue-list z))
  (test '(1 2 3 4 5) (list-queue-remove-all! z2))
  (test-assert (list-queue-empty? z2))
  (list-queue-remove-all! z)
  (list-queue-add-front! z 1)
  (list-queue-add-front! z 0)
  (list-queue-add-back! z 2)
  (list-queue-add-back! z 3)
  (test '(0 1 2 3) (list-queue-list z))
) ; end list-queues/simple

(test-group "list-queues/whole"
  (define a (list-queue 1 2 3))
  (define b (list-queue-copy a))
  (test '(1 2 3) (list-queue-list b))
  (list-queue-add-front! b 0)
  (test '(1 2 3) (list-queue-list a))
  (test 4 (length (list-queue-list b)))
  (define c (list-queue-concatenate (list a b)))
  (test '(1 2 3 0 1 2 3) (list-queue-list c))
) ; end list-queues/whole

(test-group "list-queues/map"
  (define r (list-queue 1 2 3))
  (define s (list-queue-map (lambda (x) (* x 10)) r))
  (test '(10 20 30) (list-queue-list s))
  (list-queue-map! (lambda (x) (+ x 1)) r)
  (test '(2 3 4) (list-queue-list r))
  (define sum 0)
  (list-queue-for-each (lambda (x) (set! sum (+ sum x))) s)
  (test 60 sum)
) ; end list-queues/map

(test-group "list-queues/conversion"
  (define n (list-queue 5 6))
  (list-queue-set-list! n (list 1 2))
  (test '(1 2) (list-queue-list n))
  (define d (list 1 2 3))
  (define e (cddr d))
  (define f (make-list-queue d e))
  (define-values (dx ex) (list-queue-first-last f))
  (test-assert (eq? d dx))
  (test-assert (eq? e ex))
  (test '(1 2 3) (list-queue-list f))
  (list-queue-add-front! f 0)
  (list-queue-add-back! f 4)
  (test '(0 1 2 3 4) (list-queue-list f))
  (define g (make-list-queue d e))
  (test '(1 2 3 4) (list-queue-list g))
  (define h (list-queue 5 6))
  (list-queue-set-list! h d e)
  (test '(1 2 3 4) (list-queue-list h))
); end list-queues/conversion

(test-group "list-queues/unfold"
  (define (double x) (* x 2))
  (define (done? x) (> x 3))
  (define (add1 x) (+ x 1))
  (define x (list-queue-unfold done? double add1 0))
  (test '(0 2 4 6) (list-queue-list x))
  (define y (list-queue-unfold-right done? double add1 0))
  (test '(6 4 2 0) (list-queue-list y))
  (define x0 (list-queue 8))
  (define x1 (list-queue-unfold done? double add1 0 x0))
  (test '(0 2 4 6 8) (list-queue-list x1))
  (define y0 (list-queue 8))
  (define y1 (list-queue-unfold-right done? double add1 0 y0))
  (test '(8 6 4 2 0) (list-queue-list y1))

) ; end list-queues/unfold

) ; end list-queues

(test-exit)
