;;;
;;; combinations.scm - combinations and that sort of stuff.
;;;
;;;  Copyright(C) 2003 by Alex Shinn (foof@synthcode.com)
;;;  Copyright(C) 2003 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: combinations.scm,v 1.2 2003-02-05 00:55:47 shirok Exp $
;;;

;; Initially written by Alex Shinn.
;; Modifided by Shiro Kawai; mainly adding support of duplicated
;; entries.

(define-module util.combinations
  (use srfi-1)
  (use gauche.sequence)
  (export permutations permutations*
          permutations-for-each permutations*-for-each
          combinations combinations*
          combinations-for-each combinations*-for-each
          power-set-binary power-set power-set-for-each
          power-set* power-set*-for-each
          cartesian-product cartesian-product-for-each
          cartesian-product-right cartesian-product-right-for-each)
  )
(select-module util.combinations)

;;----------------------------------------------------------------
;; permuations
;;

;; return a list of k-th element is removed
(define (but-kth lis k)
  (receive (head tail) (split-at lis k)
    (append! head (cdr tail))))

;; permute set.  all elements are considered distinct.
(define (permutations set)
  (cond ((null? set) (list '()))
        ((null? (cdr set)) (list set))
        (else
         (reverse!
          (fold-with-index
           (lambda (ind elt acc)
             (fold (lambda (subperm acc) (acons elt subperm acc))
                   acc
                   (permutations (but-kth set ind))))
           '()
           set)))))

;; permute set, considering equal elements, a.k.a multiset permutations
(define (permutations* set . maybe-eq)
  (cond ((null? set) (list '()))
        ((null? (cdr set)) (list set))
        (else
         (let1 eq (get-optional maybe-eq eqv?)
           (let loop ((i 0)
                      (seen '())
                      (p set)
                      (r '()))
             (cond ((null? p) (reverse! r))
                   ((member (car p) seen eq) (loop (+ i 1) seen (cdr p) r))
                   (else
                    (loop (+ i 1)
                          (cons (car p) seen)
                          (cdr p)
                          (fold (lambda (subperm r) (acons (car p) subperm r))
                                r
                                (permutations* (but-kth set i) eq))))
                   ))))))

(define (permutations-for-each proc set)
  (cond ((null? set))
        ((null? (cdr set)) (proc set))
        (else
         (for-each-with-index
          (lambda (ind elt)
            (permutations-for-each
             (lambda (subperm) (proc (cons elt subperm)))
             (but-kth set ind)))
          set))))

(define (permutations*-for-each proc set . maybe-eq)
  (cond ((null? set))
        ((null? (cdr set)) (proc set))
        (else
         (let1 eq (get-optional maybe-eq eqv?)
           (let loop ((i 0)
                      (seen '())
                      (p set))
             (cond ((null? p))
                   ((member (car p) seen eq) (loop (+ i 1) seen (cdr p)))
                   (else
                    (permutations*-for-each
                     (lambda (subperm) (proc (cons (car p) subperm)))
                     (but-kth set i)
                     eq)
                    (loop (+ i 1) (cons (car p) seen) (cdr p)))
                   ))))))
                          
;;----------------------------------------------------------------
;; combinations
;;

(define (combinations set n)
  (if (not (positive? n))
      (list '())
      (pair-fold-right
       (lambda (pr acc)
         (fold-right (cut acons (car pr) <> <>)
                     acc
                     (combinations (cdr pr) (- n 1))))
       '()
       set)))

(define (combinations* set n . maybe-eq)
  (if (not (positive? n))
      (list '())
      (let1 eq (get-optional maybe-eq eqv?)
        (let loop ((p set)
                   (seen '())
                   (r '()))
          (cond ((null? p) (reverse! r))
                ((member (car p) seen eq) (loop (cdr p) seen r))
                (else
                 (loop (cdr p)
                       (cons (car p) seen)
                       (fold (cut acons (car p) <> <>)
                             r
                             (combinations* (lset-difference eq (cdr p) seen)
                                            (- n 1) eq))))
                )))))

(define (combinations-for-each proc set n)
  (if (not (positive? n))
      (proc '())
      (pair-for-each
       (lambda (pr)
         (combinations-for-each
          (lambda (sub-comb) (proc (cons (car pr) sub-comb)))
          (cdr pr)
          (- n 1)))
       set)))

(define (combinations*-for-each proc set n . maybe-eq)
  (if (not (positive? n))
      (proc '())
      (let1 eq (get-optional maybe-eq eqv?)
        (let loop ((p set)
                   (seen '()))
          (cond ((null? p))
                ((member (car p) seen eq) (loop (cdr p) seen))
                (else
                 (combinations*-for-each
                  (lambda (sub-comb) (proc (cons (car p) sub-comb)))
                  (lset-difference eq (cdr p) seen)
                  (- n 1)
                  eq)
                 (loop (cdr p) (cons (car p) seen)))
                )))))

;;----------------------------------------------------------------
;; power sets (all subsets of any size of a given set)
;;

;; the easy binary way
(define (power-set-binary set)
  (if (null? set)
      (list '())
      (let ((x (car set))
            (rest (power-set-binary (cdr set))))
        (append rest (map (lambda (s) (cons x s)) rest)))))

;; use combinations for nice ordering
(define (power-set set)
  (let ((size (length set)))
    (let loop ((i 0))
      (if (> i size)
          '()
          (append! (combinations set i)
                   (loop (+ i 1)))))))

;; also ordered
(define (power-set-for-each proc set)
  (let ((size (length set)))
    (let loop ((i 0))
      (if (> i size)
          '()
          (begin
            (combinations-for-each proc set i)
            (loop (+ i 1)))))))

;; w/o duplicate entry
(define (power-set* set . maybe-eq)
  (let ((size (length set)))
    (let loop ((i 0))
      (if (> i size)
          '()
          (append! (apply combinations* set i maybe-eq)
                   (loop (+ i 1)))))))

(define (power-set*-for-each proc set . maybe-eq)
  (let ((size (length set)))
    (let loop ((i 0))
      (if (> i size)
          '()
          (begin
            (apply combinations*-for-each proc set i maybe-eq)
            (loop (+ i 1)))))))

;;----------------------------------------------------------------
;; cartesian product (all combinations of one element from each set)
;;

(define (cartesian-product lol)
  (if (null? lol)
      (list '())
      (let ((l (car lol))
            (rest (cartesian-product (cdr lol))))
        (append-map!
         (lambda (x)
           (map (lambda (sub-prod) (cons x sub-prod)) rest))
         l))))

(define (cartesian-product-for-each proc lol)
  (if (null? lol)
      (proc '())
      (for-each
       (lambda (x)
         (cartesian-product-for-each
          (lambda (sub-prod)
            (proc (cons x sub-prod)))
          (cdr lol)))
       (car lol))))

;; The above is left fixed (it varies elements to the right first).
;; Below is a right fixed product which could be defined with two
;; reverses but is short enough to warrant the performance gain of a
;; separate procedure.

;;(define (cartesian-product-right lol)
;;  (map reverse (cartesian-product (reverse lol))))

(define (cartesian-product-right lol)
  (if (null? lol)
      (list '())
      (let ((l (car lol))
            (rest (cartesian-product (cdr lol))))
        (append-map!
         (lambda (sub-prod)
           (map (lambda (x) (cons x sub-prod)) l))
         rest))))

(define (cartesian-product-right-for-each proc lol)
  (if (null? lol)
      (proc '())
      (cartesian-product-right-for-each
       (lambda (sub-prod)
         (for-each (lambda (x) (proc (cons x sub-prod))) (car lol)))
       (cdr lol))))


(provide "util/combinations")

