;;
;; test for weak vector / weak hash table
;; $Id: weak.scm,v 1.1 2005-07-28 05:02:54 shirok Exp $

;; The collection of garbage is affected by lots of factors.
;; The following tests make effort to cause the weakly referenced
;; objects collected, but may fail on certain occasions.

(use gauche.test)

(test-start "weak pointers")

;; A dummy function to overwrite the VM stack, so that we can clear out
;; any dangling reference to the object that are pointed by weak structure.
;; (This wouldn't be necessary once we implement a sane GC mark handler
;; on the VM stack.)
(define (fact n)
  (if (zero? n)
    1
    (* n (fact (- n 1)))))

(test-section "weak vector")

(define x (make-weak-vector 5))

(test* "make-weak-vector" #t (is-a? x <weak-vector>))
(test* "weak-vector-length" 5 (weak-vector-length x))

(test* "weak-vector-set!/ref" '((1 2 3) (4 5 6) (7 8 9) #f #f)
       (begin (weak-vector-set! x 0 (list 1 2 3))
              (weak-vector-set! x 1 (list 4 5 6))
              (weak-vector-set! x 2 (list 7 8 9))
              (map (cut weak-vector-ref x <>) '(0 1 2 3 4))))

(fact 100) ;; clear out the stack

(dotimes (n 10) (gc))

(test* "weak-vector-set!/ref (after gc)" '(#f #f #f #f #f)
       (map (cut weak-vector-ref x <>) '(0 1 2 3 4)))

;(test-section "weak hash table")

(test-end)


