;;; SPDX-FileCopyrightText: 2024 Shiro Kawai, John Cowan, Arne Babenhauserheide
;;; SPDX-License-Identifier: MIT

(cond-expand
  (guile
   (import (scheme base)
           (srfi 234)
           (srfi 1)
           (srfi srfi-64)))
  (chibi
   (import (scheme base)
           (srfi 234)
           (srfi 1)
           (srfi 11) ;; let-values
           (rename (except (chibi test) test-equal)
                   (test test-equal))))
  (chicken
    (load "srfi/srfi-234.so")
    (import (scheme base)
            (srfi 234)
            (srfi 1)
            (srfi 64)))
  (else
   (import (scheme base)
           (srfi 234)
           (srfi 1)
           (srfi 64))))


(test-begin "srfi-234")

(test-equal
    '(a b d c)
  (topological-sort '((a b c)
                      (b d)
                      (c)
                      (d c))))

;; details: multiple values
(test-equal
    '((a b d c) #f #f)
  (let-values
      (((v0 v1 v2)
        (topological-sort/details '((a b c)
                              (b d)
                              (c)
                              (d c)))))
    (list v0 v1 v2)))

;; cycle
(test-equal
    #f
  (topological-sort '((a b)
                      (b a))))

;; cycle error details
(test-equal
    '(#f "graph has circular dependency" (a b))
  (let-values
      (((v0 v1 v2)
        (topological-sort/details '((a b)
                                    (b a)))))
    (list v0 v1 v2)))

(test-equal
    '("a" "b" "d" "c")
  (topological-sort '(("a" "b" "c")
                      ("b" "d")
                      ("c")
                      ("d" "c"))
                    string=?))

(test-equal
    '((a b c) (b e))
  (edgelist->graph '((a b) (a c) (b e))))

(test-equal
    '((a b) (a c) (b e))
  (graph->edgelist '((a b c) (b e))))

(test-equal
    '((a b c) (b e))
  (edgelist/inverted->graph '((b a) (c a) (e b))))

(test-equal
    '((b a) (c a) (e b))
  (graph->edgelist/inverted '((a b c) (b e))))

(test-equal
    '((0 1) (1 2) (2 0) (3 1 2 4) (4 3 5) (5 2 6) (6 5) (7 4 6 7))
   (edgelist->graph '((0 1)
                      (1 2)
                      (2 0)
                      (3 1) (3 2) (3 4)
                      (4 3) (4 5)
                      (5 2) (5 6)
                      (6 5)
                      (7 4) (7 6) (7 7))))

(test-equal
    '((1 0 3) (2 1 3 5) (0 2) (4 3 7) (3 4) (5 4 6) (6 5 7) (7 7))
   (edgelist/inverted->graph '((0 1)
                               (1 2)
                               (2 0)
                               (3 1) (3 2) (3 4)
                               (4 3) (4 5)
                               (5 2) (5 6)
                               (6 5)
                               (7 4) (7 6) (7 7))))

(test-equal
    '((2 0 1) (6 5) (3 4) (7))
  (connected-components
   (edgelist->graph '((0 1)
                      (1 2)
                      (2 0)
                      (3 1) (3 2) (3 4)
                      (4 3) (4 5)
                      (5 2) (5 6)
                      (6 5)
                      (7 4) (7 6) (7 7)))))

(define (permutations edgelist)
  (if (null? edgelist) '(())
      (apply append
             (map (lambda (edge)
                    (map (lambda (permutation)
                           (cons edge permutation))
                         (permutations (delete edge edgelist))))
                  edgelist))))

(test-equal #t
  (every (lambda (edgelist)
           (let* ((graph (edgelist->graph edgelist))
                  (order (topological-sort graph equal?)))
             (cond
              ((equal? order '(top left right bottom)) #t)
              ((equal? order '(top right left bottom)) #t)
              (else order))))
         (permutations '((top left) (top right) (left bottom) (right bottom)))))

(test-equal '(libnewsboat regex-rs strprintf)
  (topological-sort (edgelist->graph '((libnewsboat strprintf) (libnewsboat regex-rs) (regex-rs strprintf)))))

(test-end)
