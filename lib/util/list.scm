;;;
;;; util/list.scm - more list library
;;;
;;;  Copyright (c) 2003-2010  Shiro Kawai  <shiro@acm.org>
;;;  Copyright(C) 2003 by Alex Shinn (foof@synthcode.com)
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

;; This module adds useful list utility procedures that are not in SRFI-1.

(define-module util.list
  (use srfi-1)
  (export take* drop* take-right* drop-right* split-at*
          slices intersperse cond-list
          alist->hash-table hash-table->alist
          rassq rassv rassoc
          assq-ref assv-ref assoc-ref
          rassq-ref rassv-ref rassoc-ref
          assq-set! assv-set! assoc-set!)
  )
(select-module util.list)

;;-----------------------------------------------------------------
;; permissive take and drop - if the length of given list is shorter
;; than index, returns shorter list or fills the rest.

(define (split-at* lis k :optional (fill? #f) (filler #f))
  (when (or (not (integer? k)) (negative? k))
    (error "index must be non-negative integer" k))
  (let loop ((i 0)
             (lis lis)
             (r '()))
    (cond [(= i k) (values (reverse! r) lis)]
          [(null? lis)
           (values (if fill?
                     (append! (reverse! r) (make-list (- k i) filler))
                     (reverse! r))
                   lis)]
          [else (loop (+ i 1) (cdr lis) (cons (car lis) r))])))

(define (take* lis k . args)
  (receive (h t) (apply split-at* lis k args) h))

(define (drop* lis k)
  (when (or (not (integer? k)) (negative? k))
    (error "index must be non-negative integer" k))
  (let loop ((i 0)
             (lis lis))
    (cond [(= i k) lis]
          [(null? lis) '()]
          [else (loop (+ i 1) (cdr lis))])))

(define (take-right* lis k :optional (fill? #f) (filler #f))
  (when (or (not (integer? k)) (negative? k))
    (error "index must be non-negative integer" k))
  (let1 len (length lis)
    (cond [(<= k len) (drop lis (- len k))]
          [fill? (append! (make-list (- k len) filler) lis)]
          [else lis])))

(define (drop-right* lis k)
  (let1 len (length lis)
    (if (<= k len) (take lis (- len k)) '())))

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
;;          | (test @ expr ...) ;; intersperse
;;          | (test => @ proc)  ;; intersperse

(define-syntax cond-list
  (syntax-rules (=> @)
    ((_) '())
    ((_ (test) . rest)
     (let* ((tmp test)
            (r (cond-list . rest)))
       (if tmp (cons tmp r) r)))
    ((_ (test => proc) . rest)
     (let* ((tmp test)
            (r (cond-list . rest)))
       (if tmp (cons (proc tmp) r) r)))
    ((_ (test => @ proc) . rest)
     (let* ((tmp test)
            (r (cond-list . rest)))
       (if tmp (append (proc tmp) r) r)))
    ((_ (test @ . expr) . rest)
     (let* ((tmp test)
            (r (cond-list . rest)))
       (if tmp (append (begin . expr) r) r)))
    ((_ (test . expr) . rest)
     (let* ((tmp test)
            (r (cond-list . rest)))
       (if tmp (cons (begin . expr) r) r)))
    ))

;;-----------------------------------------------------------------
;; Associative list library - based on Alex Shinn's implementation
;;

;; conversion to/from hash-table
(define (alist->hash-table a . opt-eq)
  (let ((tb (apply make-hash-table opt-eq)))
    (for-each (lambda (x) (hash-table-put! tb (car x) (cdr x))) a)
    tb))

(define (hash-table->alist h)
  (hash-table-map h cons))

;; `reverse' alist search fn
(define (rassoc key alist :optional (eq equal?))
  (find (lambda (elt) (and (pair? elt) (eq (cdr elt) key))) alist))

(define rassq (cut rassoc <> <> eq?))
(define rassv (cut rassoc <> <> eqv?))

;; 'assoc-ref', a shortcut of value retrieval w/ default value
;; Default parameter comes first, following the convention of
;; other *-ref functions.
(define (assoc-ref alist key :optional (default #f) (eq equal?))
  (cond [(assoc key alist eq) => cdr]
        [else default]))

(define (assq-ref alist key . opts)
  (assoc-ref alist key (get-optional opts #f) eq?))
(define (assv-ref alist key . opts)
  (assoc-ref alist key (get-optional opts #f) eqv?))

(define (rassoc-ref alist key :optional (default #f) (eq equal?))
  (cond [(rassoc key alist eq) => car]
        [else default]))

(define (rassq-ref alist key . opts)
  (rassoc-ref alist key (get-optional opts #f) eq?))
(define (rassv-ref alist key . opts)
  (rassoc-ref alist key (get-optional opts #f) eqv?))

;; 'assoc-set!'
(define (assoc-set! alist key val :optional (eq equal?))
  (cond [(assoc key alist eq)
         => (lambda (p) (set-cdr! p val) alist)]
        [else (acons key val alist)]))

(define assq-set!  (cut assoc-set! <> <> <> eq?))
(define assv-set!  (cut assoc-set! <> <> <> eqv?))

