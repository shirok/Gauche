;;;
;;; srfi-162 - comparator sublibraries
;;;

;; This is an addition to srfi-128.

(define-module srfi-162
  (export comparator-min comparator-max
          comparator-min-in-list comparator-max-in-list

          ;; The followings are built-in.
          default-comparator
          boolean-comparator
          real-comparator
          char-comparator
          char-ci-comparator
          string-comparator
          string-ci-comparator
          pair-comparator
          list-comparator
          vector-comparator
          eq-comparator
          eqv-comparator
          equal-comparator))
(select-module srfi-162)

(define (comparator-min-in-list cmpr lis)
  (assume (not (null? lis)))
  (let loop ([r (car lis)] [xs (cdr lis)])
    (cond [(null? xs) r]
          [(<? cmpr (car xs) r) (loop (car xs) (cdr xs))]
          [else (loop r (cdr xs))])))

(define (comparator-max-in-list cmpr lis)
  (assume (not (null? lis)))
  (let loop ([r (car lis)] [xs (cdr lis)])
    (cond [(null? xs) r]
          [(>? cmpr (car xs) r) (loop (car xs) (cdr xs))]
          [else (loop r (cdr xs))])))
  
(define (comparator-min cmpr x . xs)
  (comparator-min-in-list cmpr (cons x xs)))

(define (comparator-max cmpr x . xs)
  (comparator-max-in-list cmpr (cons x xs)))



