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
(test-section "util.list")
(use util.list)
(test-module 'util.list)

(test* "split-at* (normal)" '((a b c) (d))
       (receive r (split-at* '(a b c d) 3) r))
(test* "split-at* (boundary)" '(() (a b c d))
       (receive r (split-at* '(a b c d) 0) r))
(test* "split-at* (boundary)" '((a b c d) ())
       (receive r (split-at* '(a b c d) 4) r))
(test* "split-at* (error)" *test-error*
       (receive r (split-at* '(a b c d) -1) r))
(test* "split-at* (shorten)" '((a b c d) ())
       (receive r (split-at* '(a b c d) 5) r))
(test* "split-at* (fill)" '((a b c d #f #f) ())
       (receive r (split-at* '(a b c d) 6 #t) r))
(test* "split-at* (fill)" '((a b c d z z) ())
       (receive r (split-at* '(a b c d) 6 #t 'z) r))

(test* "take* (normal)" '(a b c)      (take* '(a b c d) 3))
(test* "take* (boundary)" '()         (take* '(a b c d) 0))
(test* "take* (boundary)" '(a b c d)  (take* '(a b c d) 4))
(test* "take* (error)" *test-error*   (take* '(a b c d) -1))
(test* "take* (shorten)" '(a b c d)   (take* '(a b c d) 5))
(test* "take* (fill)" '(a b c d #f #f) (take* '(a b c d) 6 #t))
(test* "take* (fill)" '(a b c d z z)  (take* '(a b c d) 6 #t 'z))

(test* "drop* (normal)" '(c d)       (drop* '(a b c d) 2))
(test* "drop* (boundary)" '(a b c d) (drop* '(a b c d) 0))
(test* "drop* (boundary)" '()        (drop* '(a b c d) 4))
(test* "drop* (error)" *test-error*  (drop* '(a b c d) -3))
(test* "drop* (past)" '()            (drop* '(a b c d) 5))

(test* "slices (normal)" '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 13 14 15))
       (slices (iota 16) 4))
(test* "slices (boundary)" '()
       (slices '() 4))
(test* "slices (short)" '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12))
       (slices (iota 13) 4))
(test* "slices (short)" '((0 1))
       (slices (iota 2) 4))
(test* "slices (fill)" '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 #f #f #f))
       (slices (iota 13) 4 #t))
(test* "slices (fill)" '((0 1 2 3) (4 5 6 7) (8 9 10 11) (12 -1 -1 -1))
       (slices (iota 13) 4 #t -1))

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
