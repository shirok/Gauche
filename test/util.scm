;;
;; test util modules
;;

(use gauche.test)
(test-start "util")

(use srfi-1)

;;-----------------------------------------------
(test-section "util.isomorph")
(use util.isomorph)
(test-module 'util.isomorph)

(define (make-data type)
  (let* ((z (vector #f #f #f))
         (x (circular-list "a" 'b 4 9845938427094857239485 #\z 8+5i z))
         (y (circular-list "a" 'b 4 9845938427094857239485 #\z 8+5i z)))
    (vector-set! z 0 x)
    (vector-set! z 1 y)
    (if type (vector-set! z 2 x) (vector-set! z 2 y))
    z))

(test* "isomorphic?" #t
       (isomorphic? (make-data #f) (make-data #f)))
(test* "isomorphic?" #f
       (isomorphic? (make-data #t) (make-data #f)))

;;-----------------------------------------------
(test-section "util.queue")
(use util.queue)
(test-module 'util.queue)

(test* "queue?" #f (queue? (cons 'a 'b)))
(test* "queue?" #f (queue? 3))
(test* "queue?" #f (queue? '()))
(define q (make-queue))

(test* "queue?" #t (queue? q))
(test* "enqueue!" #t (begin (enqueue! q 'a) (queue? q)))
(test* "enqueue!" #t (begin (enqueue! q 'b) (queue? q)))
(test* "enqueue!" #t (begin (enqueue! q 'c) (queue? q)))

(test* "queue-front" 'a (queue-front q))
(test* "queue-rear" 'c (queue-rear q))

(test* "enqueue!" '(a f)
       (begin
         (enqueue! q 'd 'e 'f)
         (list (queue-front q) (queue-rear q))))

(test* "dequeue!" 'a (dequeue! q))
(test* "dequeue!" 'b (dequeue! q))
(test* "queue-empty?" #f (queue-empty? q))
(test* "dequeue!" 'c (dequeue! q))
(test* "dequeue!" 'd (dequeue! q))
(test* "dequeue!" 'e (dequeue! q))
(test* "dequeue!" 'f (dequeue! q))
(test* "queue-empty?" #t (queue-empty? q))

(test* "queue-push!" '(c a)
       (begin
         (queue-push! q 'a) (queue-push! q 'b) (queue-push! q 'c)
         (list (queue-front q) (queue-rear q))))
(test* "queue-push!" '(f a)
       (begin
         (queue-push! q 'd 'e 'f)
         (list (queue-front q) (queue-rear q))))
(test* "queue-pop!" 'f (queue-pop! q))
(test* "queue-pop!" 'e (queue-pop! q))
(test* "queue-empty?" #f (queue-empty? q))
(test* "queue-pop!" 'd (queue-pop! q))
(test* "queue-pop!" 'c (queue-pop! q))
(test* "queue-pop!" 'b (queue-pop! q))
(test* "queue-pop!" 'a (queue-pop! q))
(test* "queue-empty?" #t (queue-empty? q))

(test* "dequeue-all!" '(a b c d e)
       (begin (enqueue! q 'a 'b 'c 'd 'e) (dequeue-all! q)))
(test* "dequeue-all!" '()
       (dequeue-all! q))
(test* "dequeue-all!" #t
       (queue-empty? q))

;;-----------------------------------------------
(test-section "util.toposort")
(use util.toposort)
(test-module 'util.toposort)


;; using famous socks example (Corman)
(test* "topological-sort"
       '(socks undershorts watch shirt tie pants belt jacket shoes)
       (topological-sort '((shirt tie belt)
                           (tie jacket)
                           (belt jacket)
                           (watch)
                           (pants shoes belt)
                           (undershorts pants shoes)
                           (socks shoes))
                         eq?))

(test* "topological-sort"
       '("socks" "undershorts" "watch" "shirt" "tie" "pants" "belt" "jacket" "shoes")
       (topological-sort '(("shirt" "tie" "belt")
                           ("tie" "jacket")
                           ("belt" "jacket")
                           ("watch")
                           ("pants" "shoes" "belt")
                           ("undershorts" "pants" "shoes")
                           ("socks" "shoes"))
                         equal?))

(test-end)
