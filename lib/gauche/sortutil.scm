;;; File   : sort.scm
;;; Author : Richard A. O'Keefe (based on Prolog code by D.H.D.Warren)
;;; Updated: 11 June 1991
;;; Defines: sorted?, merge, merge!, sort, sort!

;; Public Domain.
;; [SK]: I just added module mechanics to fit Gauche, modified API to
;; keep backward-compatibility with Gauche's original sort function,
;; and stripped the long explanatory comment.  See sort.orig.scm contained
;; in tarball for the original form of this file.
;; $Id: sortutil.scm,v 1.1 2003-09-14 12:41:56 shirok Exp $

;; To be autoloaded
(define-module gauche.sortutil
  (export sorted? merge merge! sort sort!))
(select-module gauche.sortutil)

(define (default-less? x y) (< (compare x y) 0))

;;; (sorted? sequence less?)
;;; is true when sequence is a list (x0 x1 ... xm) or a vector #(x0 ... xm)
;;; such that for all 1 <= i <= m,
;;;	(not (less? (list-ref list i) (list-ref list (- i 1)))).

(define (sorted? seq . maybe-less?)
  (let1 less? (get-optional maybe-less? default-less?)
    (cond
     ((null? seq) #t)
     ((vector? seq)
      (let ((n (vector-length seq)))
        (if (<= n 1)
          #t
          (do ((i 1 (+ i 1)))
              ((or (= i n)
                   (less? (vector-ref seq (- i 1))
                          (vector-ref seq i)))
               (= i n)) )) ))
     (else
      (let loop ((last (car seq)) (next (cdr seq)))
        (or (null? next)
            (and (not (less? (car next) last))
                 (loop (car next) (cdr next)) )) )) )))


;;; (merge a b less?)
;;; takes two lists a and b such that (sorted? a less?) and (sorted? b less?)
;;; and returns a new list in which the elements of a and b have been stably
;;; interleaved so that (sorted? (merge a b less?) less?).
;;; Note:  this does _not_ accept vectors.  See below.

(define (merge a b less?)
  (cond
   ((null? a) b)
   ((null? b) a)
   (else (let loop ((x (car a)) (a (cdr a)) (y (car b)) (b (cdr b)))
           ;; The loop handles the merging of non-empty lists.  It has
           ;; been written this way to save testing and car/cdring.
           (if (less? y x)
             (if (null? b)
               (cons y (cons x a))
               (cons y (loop x a (car b) (cdr b)) ))
             ;; x <= y
             (if (null? a)
               (cons x (cons y b))
               (cons x (loop (car a) (cdr a) y b)) )) )) ))

;;; (merge! a b less?)
;;; takes two sorted lists a and b and smashes their cdr fields to form a
;;; single sorted list including the elements of both.
;;; Note:  this does _not_ accept vectors.

(define (merge! a b less?)
  (define (loop r a b)
    (if (less? (car b) (car a))
      (begin
        (set-cdr! r b)
        (if (null? (cdr b))
          (set-cdr! b a)
          (loop b a (cdr b)) ))
      ;; (car a) <= (car b)
      (begin
        (set-cdr! r a)
        (if (null? (cdr a))
          (set-cdr! a b)
          (loop a (cdr a) b)) )) )
  (cond
   ((null? a) b)
   ((null? b) a)
   ((less? (car b) (car a))
    (if (null? (cdr b))
      (set-cdr! b a)
      (loop b a (cdr b)))
    b)
   (else ; (car a) <= (car b)
    (if (null? (cdr a))
      (set-cdr! a b)
      (loop a (cdr a) b))
    a)))

;;; (sort! sequence less?)
;;; sorts the list or vector sequence destructively.  It uses a version
;;; of merge-sort invented, to the best of my knowledge, by David H. D.
;;; Warren, and first used in the DEC-10 Prolog system.  R. A. O'Keefe
;;; adapted it to work destructively in Scheme.

(define (sort! seq . maybe-less?)
  (let1 less? (get-optional maybe-less? default-less?)
    (define (step n)
      (cond
       ((> n 2)
        (let* ((j (ash n -1))
               (a (step j))
               (k (- n j))
               (b (step k)))
          (merge! a b less?)))
       ((= n 2)
        (let ((x (car seq))
              (y (cadr seq))
              (p seq))
          (set! seq (cddr seq))
          (if (less? y x) (begin
                            (set-car! p y)
                            (set-car! (cdr p) x)))
          (set-cdr! (cdr p) '())
          p))
       ((= n 1)
        (let ((p seq))
          (set! seq (cdr seq))
          (set-cdr! p '())
          p))
       (else
        '()) ))
    (if (vector? seq)
      (let ((n (vector-length seq)))
        (set! seq (vector->list seq))
        (do ((p (step n) (cdr p))
             (i 0 (+ i 1)))
            ((null? p) vector)
          (vector-set! vector i (car p)) ))
      ;; otherwise, assume it is a list
      (step (length seq)) )))


;;; (sort sequence less?)
;;; sorts a vector or list non-destructively.  It does this by sorting a
;;; copy of the sequence.

(define (sort seq . maybe-less?)
    (if (vector? seq)
	(list->vector (apply sort! (vector->list seq) maybe-less?))
	(apply sort! (list-copy seq) maybe-less?)))

(provide "gauche/sortutil")

