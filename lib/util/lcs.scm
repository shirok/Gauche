;;;; lcs.scm -- find out the longest common sequence

;;; Created:    <2002-06-21 15:36:46 foof>
;;; Time-stamp: <2003-02-15 00:09:55 foof>
;;; Author:     Alex Shinn <foof@synthcode.com>

;;; Modified a bit by Shiro Kawai

(define-module util.lcs
  (use gauche.sequence)
  (use srfi-1)
  (use srfi-11)
  (export lcs lcs-with-positions lcs-fold lcs-edit-list))
(select-module util.lcs)

;; Utility for lcs-with-positions
(define (max-seq . opt-ls)
  (if (null? opt-ls)
    (list 0 '())
    (let loop ((a (car opt-ls)) (ls (cdr opt-ls)))
      (if (null? ls)
        a
        (let ((b (car ls)))
          (if (>= (car a) (car b))
            (loop a (cdr ls))
            (loop b (cdr ls))))))))

;; The base algorithm.   This algorithm consumes NxM space and time
;; where N = (length a-ls) and M = (length b-ls)
(define (lcs-with-positions a-ls b-ls . opt-eq)
  (let* ((eq (get-optional opt-eq equal?))
         (a-size (+ 1 (size-of a-ls)))
         (b-size (+ 1 (size-of b-ls)))
         (undef  (cons #f #f))
         (cache  (make-vector (* a-size b-size) undef)))
    (let-syntax ((cache-ref
                  (syntax-rules ()
                    ((_ a b) (vector-ref cache (+ (* a b-size) b)))))
                 (cache-set!
                  (syntax-rules ()
                    ((_ a b v) (vector-set! cache (+ (* a b-size) b) v)))))
      (let loop ((a a-ls) (a-pos 0) (b b-ls) (b-pos 0))
        ;; cache this step if not already done
        (when (eq? (cache-ref a-pos b-pos) undef)
          (cache-set!
           a-pos b-pos
           (if (or (null? a) (null? b))
               (list 0 '()) ;; base case
               (let ((a1 (car a))
                     (b1 (car b))
                     (a-tail (loop (cdr a) (+ a-pos 1) b b-pos))
                     (b-tail (loop a a-pos (cdr b) (+ b-pos 1))))
                 (cond ((eq a1 b1)
                        ;; match found, we either use it or we don't
                        (let* ((a-b-tail (loop (cdr a) (+ a-pos 1)
                                               (cdr b) (+ b-pos 1)))
                               (a-b-res (list (+ 1 (car a-b-tail))
                                              (cons (list a1 a-pos b-pos)
                                                    (cadr a-b-tail)))))
                          (max-seq a-b-res a-tail b-tail)))
                       (else
                        ;; not a match
                        (max-seq a-tail b-tail)))))))
        ;; return the computed cache
        (cache-ref a-pos b-pos) ))))

;; Just returns the LCS
(define (lcs a b . opt-eq)
  (map car (cadr (lcs-with-positions a b (get-optional opt-eq equal?)))))

;; Fundamental iterator to deal with editlist.
;;   Similar to Perl's Algorith::Diff's traverse_sequence.
(define (lcs-fold a-only b-only both seed a b . opt-eq)
  (let1 common (cadr (lcs-with-positions a b (get-optional opt-eq equal?)))
    ;; Calculates edit-list from the LCS.
    ;; Loop parameters:
    ;;   common - list of common elements
    ;;   seed   - seed value
    ;;   a      - head of sequence a
    ;;   a-pos  - current position count of sequence a
    ;;   b      - head of sequence b
    ;;   b-pos  - current position count of sequence b
    (let loop ((common common) (seed seed)
               (a a) (a-pos 0) (b b) (b-pos 0))
      (if (null? common)
        ;; No more common elements.  Fold the tail of a and b.
        (fold b-only (fold a-only seed a) b)
        ;; We have a common element.
        (let* ((elt   (car common))
               (a-off (cadr elt))
               (a-skip (- a-off a-pos))
               (b-off (caddr elt))
               (b-skip (- b-off b-pos)))
          (let-values (((a-head a-tail) (split-at a a-skip))
                       ((b-head b-tail) (split-at b b-skip)))
            (loop (cdr common)
                  (both (car elt)
                        (fold b-only (fold a-only seed a-head) b-head))
                  (cdr a-tail) (+ a-off 1) (cdr b-tail) (+ b-off 1)))))
      )
    ))

;; Returns an 'edit-list', which is a list of command sequences
;; that turns the sequence a to the sequence b.
;; The return value is a list of hunks, where each hunk is a
;; list of edit commands, (<command> <index> <element>).

(define (lcs-edit-list a b . opt-eq)
  (define a-pos -1)  ;; we use pre-increment, so begin from -1.
  (define b-pos -1)  ;; ditto
  (define hunks '())
  (let1 last
      (apply lcs-fold
             (lambda (elt hunk)  ;; a-only - remove
               (inc! a-pos) `((- ,a-pos ,elt) ,@hunk))
             (lambda (elt hunk)  ;; b-only - add
               (inc! b-pos) `((+ ,b-pos ,elt) ,@hunk))
             (lambda (elt hunk)  ;; same - reset hunks
               (inc! a-pos) (inc! b-pos)
               (unless (null? hunk) (push! hunks (reverse! hunk)))
               '())
             '()
             a b opt-eq)
    (unless (null? last) (push! hunks (reverse! last))))
  (reverse! hunks))

(provide "util/lcs")
