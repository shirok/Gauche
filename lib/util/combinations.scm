;;;; combinations.scm -- combination library

;;; Created:    <2003-01-15 12:06:11 foof>
;;; Time-stamp: <2003-01-16 18:08:00 foof>
;;; Author:     Alex Shinn <foof@synthcode.com>

;; $Id: combinations.scm,v 1.1 2003-01-30 10:45:55 shirok Exp $

(define-module util.combinations
  (use srfi-1)
  (export-all)
  )
(select-module util.combinations)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; permuations and combinations

(define (permutations set)
  (if (null? set)
      (list '())
      (append-map
       (lambda (x)
         (map (cut cons x <>)
              (permutations (delete x set))))
       set)))

(define (permutations-for-each proc set)
  (if (null? set)
      (proc '())
      (for-each
       (lambda (x)
         (permutations-for-each
          (lambda (sub-perm)
            (proc (cons x sub-perm)))
          (delete x set)))
       set)))

(define (combinations set n)
  (if (zero? n)
      (list '())
      (let ((n2 (- n 1)))
        (pair-fold-right
         (lambda (pr acc)
           (let ((first (car pr)))
             (append (map (cut cons first <>)
                          (combinations (cdr pr) n2))
                     acc)))
         '()
         set))))

(define (combinations-for-each proc set n)
  (if (zero? n)
      (proc '())
      (let ((n2 (- n 1)))
        (pair-for-each
         (lambda (pr)
           (let ((first (car pr)))
             (combinations-for-each
              (lambda (sub-comb)
                (proc (cons first sub-comb)))
              (cdr pr)
              n2)))
         set))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; power sets (all subsets of any size of a given set)

;; the easy binary way
(define (power-set-binary set)
  (if (null? set)
      (list '())
      (let ((x (car set))
            (rest (power-set (cdr set))))
        (append rest (map (lambda (s) (cons x s)) rest)))))

;; use combinations for nice ordering
(define (power-set set)
  (let ((size (length set)))
    (let loop ((i 0))
      (if (> i size)
          '()
          (append (combinations set i)
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cartesian product (all combinations of one element from each set)

(define (cartesian-product lol)
  (if (null? lol)
      (list '())
      (let ((l (car lol))
            (rest (cartesian-product (cdr lol))))
        (append-map
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
        (append-map
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

