;;
;; test util modules
;;

(use gauche.test)
(test-start "util")

(use srfi-1)

;;-----------------------------------------------
(test-section "util.isomorph")
(use util.isomorph)

(define (make-data type)
  (let* ((z (vector #f #f #f))
         (x (circular-list "a" 'b 4 9845938427094857239485 #\z 8+5i z))
         (y (circular-list "a" 'b 4 9845938427094857239485 #\z 8+5i z)))
    (vector-set! z 0 x)
    (vector-set! z 1 y)
    (if type (vector-set! z 2 x) (vector-set! z 2 y))
    z))

(test "isomorphic?" #t
      (lambda () (isomorphic? (make-data #f) (make-data #f))))
(test "isomorphic?" #f
      (lambda () (isomorphic? (make-data #t) (make-data #f))))

;;-----------------------------------------------
(test-section "util.queue")
(use util.queue)

(test "queue?" #f (lambda () (queue? (cons 'a 'b))))
(test "queue?" #f (lambda () (queue? 3)))
(test "queue?" #f (lambda () (queue? '())))
(define q (make-queue))

(test "queue?" #t (lambda () (queue? q)))
(test "enqueue!" #t (lambda () (enqueue! q 'a) (queue? q)))
(test "enqueue!" #t (lambda () (enqueue! q 'b) (queue? q)))
(test "enqueue!" #t (lambda () (enqueue! q 'c) (queue? q)))

(test "queue-front" 'a (lambda () (queue-front q)))
(test "queue-rear" 'c (lambda () (queue-rear q)))

(test "enqueue!" '(a f)
      (lambda ()
        (enqueue! q 'd 'e 'f)
        (list (queue-front q) (queue-rear q))))

(test "dequeue!" 'a (lambda () (dequeue! q)))
(test "dequeue!" 'b (lambda () (dequeue! q)))
(test "queue-empty?" #f (lambda () (queue-empty? q)))
(test "dequeue!" 'c (lambda () (dequeue! q)))
(test "dequeue!" 'd (lambda () (dequeue! q)))
(test "dequeue!" 'e (lambda () (dequeue! q)))
(test "dequeue!" 'f (lambda () (dequeue! q)))
(test "queue-empty?" #t (lambda () (queue-empty? q)))

(test "queue-push!" '(c a)
      (lambda ()
        (queue-push! q 'a) (queue-push! q 'b) (queue-push! q 'c)
        (list (queue-front q) (queue-rear q))))
(test "queue-push!" '(f a)
      (lambda ()
        (queue-push! q 'd 'e 'f)
        (list (queue-front q) (queue-rear q))))
(test "queue-pop!" 'f (lambda () (queue-pop! q)))
(test "queue-pop!" 'e (lambda () (queue-pop! q)))
(test "queue-empty?" #f (lambda () (queue-empty? q)))
(test "queue-pop!" 'd (lambda () (queue-pop! q)))
(test "queue-pop!" 'c (lambda () (queue-pop! q)))
(test "queue-pop!" 'b (lambda () (queue-pop! q)))
(test "queue-pop!" 'a (lambda () (queue-pop! q)))
(test "queue-empty?" #t (lambda () (queue-empty? q)))

;;-----------------------------------------------
(test-section "util.toposort")
(use util.toposort)


;; using famous socks example (Corman)
(test "topological-sort"
      '(socks undershorts watch shirt tie pants belt jacket shoes)
      (lambda ()
        (topological-sort '((shirt tie belt)
                            (tie jacket)
                            (belt jacket)
                            (watch)
                            (pants shoes belt)
                            (undershorts pants shoes)
                            (socks shoes))
                          eq?)))

(test "topological-sort"
      '("socks" "undershorts" "watch" "shirt" "tie" "pants" "belt" "jacket" "shoes")
      (lambda ()
        (topological-sort '(("shirt" "tie" "belt")
                            ("tie" "jacket")
                            ("belt" "jacket")
                            ("watch")
                            ("pants" "shoes" "belt")
                            ("undershorts" "pants" "shoes")
                            ("socks" "shoes"))
                          equal?)))

(test-end)



