(use gauche.test)

(test-start "data.* extensions")

;;-----------------------------------------------
(test-section "data.queue")
(use data.queue)
(test-module 'data.queue)

(define (queue-basic-test what maker)
  (define q (maker))

  (test* #"~what queue?" #f (queue? (cons 'a 'b)))
  (test* #"~what queue?" #f (queue? 3))
  (test* #"~what queue?" #f (queue? '()))
  (test* #"~what queue?" #t (queue? q))
  (test* #"~what enqueue!" #t (begin (enqueue! q 'a) (queue? q)))
  (test* #"~what enqueue!" #t (begin (enqueue! q 'b) (queue? q)))
  (test* #"~what enqueue!" #t (begin (enqueue! q 'c) (queue? q)))

  (test* #"~what queue-front" 'a (queue-front q))
  (test* #"~what queue-rear" 'c (queue-rear q))

  (test* #"~what enqueue!" '(a f)
         (begin
           (enqueue! q 'd 'e 'f)
           (list (queue-front q) (queue-rear q))))

  (test* #"~what dequeue!" 'a (dequeue! q))
  (test* #"~what dequeue!" 'b (dequeue! q))
  (test* #"~what queue-empty?" #f (queue-empty? q))
  (test* #"~what dequeue!" 'c (dequeue! q))
  (test* #"~what dequeue!" 'd (dequeue! q))
  (test* #"~what dequeue!" 'e (dequeue! q))
  (test* #"~what dequeue!" 'f (dequeue! q))
  (test* #"~what queue-empty?" #t (queue-empty? q))

  (test* #"~what dequeue! (error)" (test-error) (dequeue! q))
  (test* #"~what dequeue! (fallback)" "empty!" (dequeue! q "empty!"))
  (test* #"~what queue-front (error)" (test-error) (queue-front q))
  (test* #"~what queue-front (fallback)" "foo" (queue-front q "foo"))
  (test* #"~what queue-rear (error)" (test-error) (queue-rear q))
  (test* #"~what queue-rear (fallback)" "foo" (queue-rear q "foo"))

  (test* #"~what queue-push!" '(c a)
         (begin
           (queue-push! q 'a) (queue-push! q 'b) (queue-push! q 'c)
           (list (queue-front q) (queue-rear q))))
  (test* #"~what queue-push!" '(f a)
         (begin
           (queue-push! q 'd 'e 'f)
           (list (queue-front q) (queue-rear q))))
  (test* #"~what queue-pop!" 'f (queue-pop! q))
  (test* #"~what queue-pop!" 'e (queue-pop! q))
  (test* #"~what queue-empty?" #f (queue-empty? q))
  (test* #"~what queue-pop!" 'd (queue-pop! q))
  (test* #"~what queue-pop!" 'c (queue-pop! q))
  (test* #"~what queue-pop!" 'b (queue-pop! q))
  (test* #"~what queue-pop!" 'a (queue-pop! q))
  (test* #"~what queue-empty?" #t (queue-empty? q))

  (test* #"~what dequeue-all!" '(a b c d e)
         (begin (enqueue! q 'a 'b 'c 'd 'e) (dequeue-all! q)))
  (test* #"~what dequeue-all!" '() (dequeue-all! q))
  (test* #"~what dequeue-all!" #t  (queue-empty? q))

  (test* #"~what find-in-queue" #f (find-in-queue (cut eq? <> 'a) q))
  (test* #"~what find-in-queue" 'a (begin (enqueue! q 'a 'b 'c 'd 'e)
                                             (find-in-queue (cut eq? <> 'a) q)))
  (test* #"~what find-in-queue" 'c (find-in-queue (cut eq? <> 'c) q))
  (test* #"~what find-in-queue" 'e (find-in-queue (cut eq? <> 'e) q))
  (test* #"~what find-in-queue" '#f (find-in-queue (cut eq? <> 'f) q))

  (test* #"~what any-in-queue?" 'ok
         (any-in-queue (^x (and (eq? x 'c) 'ok)) q))
  (test* #"~what any-in-queue?" #f
         (any-in-queue (^x (and (eq? x 'z) 'ok)) q))
  (test* #"~what every-in-queue?" #t (every-in-queue symbol? q))
  (test* #"~what every-in-queue?" #f (every-in-queue (cut eq? <> 'a) q))

  (test* #"~what remove-from-queue!" #f
         (remove-from-queue! (cut eq? <> 'f) q))
  (test* #"~what remove-from-queue!" #t
         (remove-from-queue! (cut eq? <> 'e) q))
  (test* #"~what remove-from-queue!" #f
         (remove-from-queue! (cut eq? <> 'e) q))
  (test* #"~what remove-from-queue!" #t
         (remove-from-queue! (cut eq? <> 'a) q))
  (test* #"~what remove-from-queue!" #t
         (remove-from-queue! (cut memq <> '(b c)) q))
  (test* #"~what remove-from-queue!" #t
         (remove-from-queue! (cut eq? <> 'd) q))
  (test* #"~what remove-from-queue!" #t
         (queue-empty? q))
  (test* #"~what remove-from-queue!" #f
         (remove-from-queue! (cut eq? <> 'd) q))


  (let1 q (make-queue)
    (test* #"~what enqueue-unique!" '("a")
           (begin (enqueue-unique! q equal? "a")
                  (queue->list q)))
    (test* #"~what enqueue-unique!" '("a" "b")
           (begin (enqueue-unique! q equal? "b")
                  (queue->list q)))
    (test* #"~what enqueue-unique!" '("a" "b")
           (begin (enqueue-unique! q equal? "a")
                  (queue->list q)))
    (test* #"~what enqueue-unique!" '("a" "b" "c" "d")
           (begin (enqueue-unique! q equal? "a" "b" "c" "d")
                  (queue->list q)))
    (test* #"~what queue-push-unique!" '("e" "a" "b" "c" "d")
           (begin (queue-push-unique! q equal? "d" "e")
                  (queue->list q)))
    (set! q (make-queue))
    (test* #"~what queue-push-unique!" '("e" "d")
           (begin (queue-push-unique! q equal? "d" "e")
                  (queue->list q)))
    (test* #"~what queue-push-unique!" '("c" "b" "a" "e" "d")
           (begin (queue-push-unique! q equal? "a" "b" "c" "d" "e")
                  (queue->list q)))
    )
  )

(queue-basic-test "simple queue" make-queue)
(queue-basic-test "mtqueue"      make-mtqueue)

(let1 q (make-mtqueue :max-length 3)
  (test* "mtqueue room" 3 (mtqueue-room q))

  (test* "mtqueue maxlen" 'c
         (begin (enqueue! q 'a)
                (enqueue! q 'b)
                (enqueue! q 'c)
                (queue-rear q)))
  (test* "mtqueue maxlen (enqueue! overflow)" (test-error)
         (enqueue! q 'd))
  (test* "mtqueue maxlen (enqueue! unchanged after overflow)" '(a b c)
         (queue->list q))
  (test* "mtqueue room" 0 (mtqueue-room q))
  (test* "mtqueue maxlen (enqueue! multiarg overflow)" (test-error)
         (begin (dequeue! q)
                (enqueue! q 'd 'e 'f)))
  (test* "mtqueue maxlen (enqueue! atomicity)" '(b c)
         (queue->list q))
  (test* "mtqueue room" 1 (mtqueue-room q))

  (test* "mtqueue maxlen (queue-push! overflow)" (test-error)
         (begin (queue-push! q 'a)
                (queue-push! q 'z)))
  (test* "mtqueue maxlen (queue-push! postcheck)" '(a b c)
         (queue->list q))
  (test* "mtqueue maxlen (queue-push! multiarg overflow)" (test-error)
         (begin (dequeue! q)
                (queue-push! q 'd 'e 'f)))
  (test* "mtqueue maxlen (queue-push! atomicity)" '(b c)
         (queue->list q))
  )

(let1 q (make-mtqueue :max-length 3)
  (test* "mtqueue room" 3 (mtqueue-room q))

  (test* "mtqueue maxlen (with enqueue-unique!)" 'c
         (begin (enqueue-unique! q eq? 'a)
                (enqueue-unique! q eq? 'b)
                (enqueue-unique! q eq? 'c)
                (queue-rear q)))
  (test* "mtqueue maxlen (enqueue-unique! overflow)" (test-error)
         (enqueue-unique! q eq? 'd))
  (test* "mtqueue maxlen (enqueue-unique! unchanged after overflow)" '(a b c)
         (queue->list q))
  (test* "mtqueue room" 0 (mtqueue-room q))
  (test* "mtqueue maxlen (enqueue-unique! multiarg overflow)" (test-error)
         (begin (dequeue! q)
                (enqueue-unique! q eq? 'c 'd 'e)))
  (test* "mtqueue maxlen (enqueue-unique! atomicity)" '(b c)
         (queue->list q))
  (test* "mtqueue room" 1 (mtqueue-room q))
  (test* "mtqueue maxlen (enqueue-unique! duplicate multiarg)" '(b c d)
         (begin (enqueue-unique! q eq? 'c 'd 'b 'd)
                (queue->list q)))

  (test* "mtqueue maxlen (queue-push-unique! overflow)" (test-error)
         (begin (dequeue! q)
                (queue-push-unique! q eq? 'b)
                (queue-push-unique! q eq? 'z)))
  (test* "mtqueue maxlen (queue-push-unique! postcheck)" '(b c d)
         (queue->list q))
  (test* "mtqueue maxlen (queue-push-unique! multiarg overflow)" (test-error)
         (begin (dequeue! q)
                (queue-push-unique! q eq? 'c 'b 'a)))
  (test* "mtqueue maxlen (queue-push-unique! atomicity)" '(c d)
         (queue->list q))
  (test* "mtqueue maxlen (queue-push-unique! duplicate multiarg)" '(b c d)
         (begin (dequeue! q)
                (queue-push-unique! q eq? 'c 'b 'c 'd)
                (queue->list q)))
  )

(test* "mtqueue room" +inf.0 (mtqueue-room (make-mtqueue)))

;; Note: */wait! APIs are tested in ext/threads/test.scm instead of here,
;; since we need threads working.

(test-end)
