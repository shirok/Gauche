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



