;;;
;;; combinations.scm - combinations and that sort of stuff.
;;;
;;;  Copyright(C) 2003 by Alex Shinn (foof@synthcode.com)
;;;  Copyright (c) 2003-2019  Shiro Kawai  <shiro@acm.org>
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

;; Initially written by Alex Shinn.
;; Modifided by Shiro Kawai

(define-module util.combinations
  (use srfi-1)
  (use util.match)
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
  (case k
    [(0) (cdr lis)]
    [(1) (cons (car lis) (cddr lis))]
    [(2) (list* (car lis) (cadr lis) (cdddr lis))]
    [(3) (list* (car lis) (cadr lis) (caddr lis) (cddddr lis))]
    [else (receive (head tail) (split-at lis k)
            (append! head (cdr tail)))]))

;; permute set.  all elements are considered distinct.
;; the shortcut for 3 elements or less speeds up a bit.
(define (permutations set)
  (match set
    [() (list '())]
    [(a) (list set)]
    [(a b) `(,set (,b ,a))]
    [(a b c)
     `(,set (,a ,c ,b) (,b ,a ,c) (,b ,c ,a) (,c ,a ,b) (,c ,b ,a))]
    [else
     (reverse!
      (fold-with-index
       (lambda (ind elt acc)
         (fold (lambda (subperm acc) (acons elt subperm acc))
               acc
               (permutations (but-kth set ind))))
       '()
       set))]))

;; permute set, considering equal elements, a.k.a multiset permutations
(define (permutations* set :optional (eq eqv?))
  (define (rec set)
    (match set
      [() (list '())]
      [(a) (list set)]
      [(a b) (if (eq a b) (list set) `(,set (,b ,a)))]
      [else
       (let loop ((i 0)
                  (seen '())
                  (p set)
                  (r '()))
         (cond [(null? p) (reverse! r)]
               [(member (car p) seen eq) (loop (+ i 1) seen (cdr p) r)]
               [else
                (loop (+ i 1)
                      (cons (car p) seen)
                      (cdr p)
                      (fold (lambda (subperm r) (acons (car p) subperm r))
                            r
                            (rec (but-kth set i))))]))]))
  (rec set))

;; permutations without generating entire list.
;; We use shortcut for (<= length 4) case, which boosts performace.
(define-inline (p/each3 proc x1 x2 x3)
  (proc `(,x1 ,x2 ,x3)) (proc `(,x1 ,x3 ,x2))
  (proc `(,x2 ,x1 ,x3)) (proc `(,x2 ,x3 ,x1))
  (proc `(,x3 ,x1 ,x2)) (proc `(,x3 ,x2 ,x1)))
(define (p/each4 proc x1 x2 x3 x4)
  (p/each3 (lambda (xs) (proc (cons x1 xs))) x2 x3 x4)
  (p/each3 (lambda (xs) (proc (cons x2 xs))) x1 x3 x4)
  (p/each3 (lambda (xs) (proc (cons x3 xs))) x1 x2 x4)
  (p/each3 (lambda (xs) (proc (cons x4 xs))) x1 x2 x3))
(define (p/each* proc len xs)
  (if (= len 4)
    (apply p/each4 proc xs)
    (let1 len1 (- len 1)
      (for-each-with-index
       (lambda (ind elt)
         (p/each* (lambda (subperm) (proc (cons elt subperm)))
                  len1
                  (but-kth xs ind)))
       xs))))
(define (permutations-for-each proc set)
  (match set
    [() (undefined)]
    [(x) (proc set)]
    [(x1 x2) (proc `(,x1 ,x2)) (proc `(,x2 ,x1))]
    [(x1 x2 x3) (p/each3 proc x1 x2 x3)]
    [(x1 x2 x3 x4) (p/each4 proc x1 x2 x3 x4)]
    [else (p/each* proc (length set) set)]))

;; Like permutations-for-each, but considering duplications.
(define (permutations*-for-each proc set :optional (eq eqv?))
  (define (rec proc set)
    (match set
      [() (undefined)]
      [(a) (proc set)]
      [(a b) (cond [(eq a b) (proc set)] [else (proc set) (proc `(,b ,a))])]
      [else
       (let loop ((i 0)
                  (seen '())
                  (p set))
         (cond [(null? p)]
               [(member (car p) seen eq) (loop (+ i 1) seen (cdr p))]
               [else (rec (lambda (subperm) (proc (cons (car p) subperm)))
                       (but-kth set i))
                     (loop (+ i 1) (cons (car p) seen) (cdr p))]))]))
  (rec proc set))

;;----------------------------------------------------------------
;; combinations
;;

(define (combinations set n)
  (define (rec set tail)
    (cond [(null? tail) (list set)]
          [(eq? (cdr set) tail) (map list set)]
          [else (fold-right (cut acons (car set) <> <>)
                            (rec (cdr set) (cdr tail))
                            (rec (cdr set) tail))]))
  (cond [(not (positive? n)) (list '())]
        [(list-tail set n #f) => (cut rec set <>)]
        [else '()]))

(define (combinations* set n :optional (eq eqv?))
  (define (rec set n)
    (if (not (positive? n))
      (list '())
      (let loop ((p set)
                 (seen '())
                 (r '()))
        (cond [(null? p) (reverse! r)]
              [(member (car p) seen eq) (loop (cdr p) seen r)]
              [else
               (loop (cdr p)
                     (cons (car p) seen)
                     (fold (cut acons (car p) <> <>)
                           r
                           (rec (lset-difference eq (cdr p) seen) (- n 1))))]
              ))))
  (rec set n))

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

(define (combinations*-for-each proc set n :optional (eq eqv?))
  (define (rec proc set n)
    (if (not (positive? n))
      (proc '())
      (let loop ((p set)
                 (seen '()))
        (cond [(null? p)]
              [(member (car p) seen eq) (loop (cdr p) seen)]
              [else
               (rec (lambda (sub-comb) (proc (cons (car p) sub-comb)))
                 (lset-difference eq (cdr p) seen)
                 (- n 1))
               (loop (cdr p) (cons (car p) seen))]))))
  (rec proc set n))

;;----------------------------------------------------------------
;; power sets (all subsets of any size of a given set)
;;

;; the easy binary way
(define (power-set-binary set)
  (if (null? set)
      (list '())
      (let ((x (car set))
            (rest (power-set-binary (cdr set))))
        (append rest (map (^s (cons x s)) rest)))))

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
            (rest (cartesian-product-right (cdr lol))))
        (append-map!
         (lambda (sub-prod)
           (map (^x (cons x sub-prod)) l))
         rest))))

(define (cartesian-product-right-for-each proc lol)
  (if (null? lol)
      (proc '())
      (cartesian-product-right-for-each
       (lambda (sub-prod)
         (for-each (^x (proc (cons x sub-prod))) (car lol)))
       (cdr lol))))



