;;;
;;; util/list.scm - more list library
;;;
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
;;;  $Id: list.scm,v 1.5 2003-02-27 11:58:52 shirok Exp $
;;;

;; This module adds useful list utility procedures that are not in SRFI-1.

(define-module util.list
  (use srfi-1)
  (export take* drop* take-right* drop-right* split-at*
          slices intersperse cond-list)
  )
(select-module util.list)

;;-----------------------------------------------------------------
;; permissive take and drop - if the length of given list is shorter
;; than index, returns shorter list or fills the rest.

(define (split-at* lis k . args)
  (when (or (not (integer? k)) (negative? k))
    (error "index must be non-negative integer" k))
  (let-optionals* args ((fill? #f)
                        (filler #f))
    (let loop ((i 0)
               (lis lis)
               (r '()))
      (cond ((= i k) (values (reverse! r) lis))
            ((null? lis)
             (values (if fill?
                         (append! (reverse! r) (make-list (- k i) filler))
                         (reverse! r))
                     lis))
            (else (loop (+ i 1) (cdr lis) (cons (car lis) r)))))))

(define (take* lis k . args)
  (receive (h t) (apply split-at* lis k args) h))

(define (drop* lis k)
  (when (or (not (integer? k)) (negative? k))
    (error "index must be non-negative integer" k))
  (let loop ((i 0)
             (lis lis))
    (cond ((= i k) lis)
          ((null? lis) '())
          (else (loop (+ i 1) (cdr lis))))))

(define (take-right* lis k . args)
  (when (or (not (integer? k)) (negative? k))
    (error "index must be non-negative integer" k))
  (let-optionals* args ((fill? #f)
                        (filler #f))
    (let1 len (length lis)
      (cond ((<= k len) (drop lis (- len k)))
            (fill? (append! (make-list (- k len) filler) lis))
            (else lis)))))

(define (drop-right* lis k)
  (let1 len (length lis)
    (if (<= k len)
        (take lis (- len k))
        '())))

;;-----------------------------------------------------------------
;; slices - split a list to a bunch of sublists of length k
;;

(define (slices lis k . args)
  (unless (and (integer? k) (positive? k))
    (error "index must be positive integer" k))
  (let loop ((lis lis)
             (r '()))
    (if (null? lis)
        (reverse! r)
        (receive (h t) (apply split-at* lis k args)
          (loop t (cons h r))))))

;;-----------------------------------------------------------------
;; intersperse - insert ITEM between elements in the list.
;; (the order of arguments is taken from Haskell's intersperse)

(define (intersperse item lis)
  (define (rec l r)
    (if (null? l)
        (reverse! r)
        (rec (cdr l) (list* (car l) item r))))
  (if (null? lis)
      '()
      (rec (cdr lis) (list (car lis)))))

;;-----------------------------------------------------------------
;; cond-list - a syntax to construct a list
;;
;;   (cond-list clause clause2 ...)
;;
;;   clause : (test expr ...)
;;          | (test => proc)

(define-syntax cond-list
  (syntax-rules (=>)
    ((_) '())
    ((_ (test) . rest)
     (let* ((tmp test)
            (r (cond-list . rest)))
       (if tmp (cons tmp r) r)))
    ((_ (test => proc) . rest)
     (let* ((tmp test)
            (r (cond-list . rest)))
       (if tmp (cons (proc tmp) r) r)))
    ((_ (test . expr) . rest)
     (let* ((tmp test)
            (r (cond-list . rest)))
       (if tmp (cons (begin . expr) r) r)))
    ))

(provide "util/list")
