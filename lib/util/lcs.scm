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

;; The base algorithm.   This algorithm consumes NxM space and time
;; where N = (length a-ls) and M = (length b-ls)

;; [SK] The original version by Alex, using recursion and memoization,
;; was clean and elegant, but the current Gauche doesn't handle deep
;; recursion well (its stack handler sucks).  So I rewrote it by
;; imperative style (ugh).  Once I fixed Gauche's stack handler,
;; I may get Alex' version back.

(define (lcs-with-positions a-ls b-ls . opt-eq)
  (let* ((eq (get-optional opt-eq equal?))
         (a-size (+ 1 (size-of a-ls)))
         (b-size (+ 1 (size-of b-ls)))
         (a-rev  (reverse a-ls))
         (b-rev  (reverse b-ls))
         (undef  (cons #f #f))
         (len-tab (make-vector (* a-size b-size) 0))
         (seq-tab (make-vector (* a-size b-size) 0)))
    (let-syntax ((tref
                  (syntax-rules ()
                    ((_ t a b) (vector-ref t (+ (* a b-size) b)))))
                 (tset!
                  (syntax-rules ()
                    ((_ t a b v) (vector-set! t (+ (* a b-size) b) v)))))

      ;; set up boundary
      (dotimes (pos a-size)
        (tset! len-tab pos (- b-size 1) 0)
        (tset! seq-tab pos (- b-size 1) '()))
      (dotimes (pos b-size)
        (tset! len-tab (- a-size 1) pos 0)
        (tset! seq-tab (- a-size 1) pos '()))
      ;; fill the table from the bottom-right
      (let a-loop ((a a-rev) (a-pos (- a-size 2)))
        (unless (null? a)
          (let b-loop ((b b-rev) (b-pos (- b-size 2)))
            (if (null? b)
              (a-loop (cdr a) (- a-pos 1))
              (let* ((a-pos1 (+ a-pos 1))
                     (b-pos1 (+ b-pos 1))
                     (a-tail-len (tref len-tab a-pos1 b-pos))
                     (b-tail-len (tref len-tab a-pos b-pos1)))
                (receive (len seq)
                    (if (eq (car a) (car b))
                      ;; we got match.
                      (let1 a-b-tail-len (+ (tref len-tab a-pos1 b-pos1) 1)
                        (if (>= a-tail-len b-tail-len)
                          (if (>= a-b-tail-len a-tail-len)
                            (values a-b-tail-len
                                    (cons (list (car a) a-pos b-pos)
                                          (tref seq-tab a-pos1 b-pos1)))
                            (values a-tail-len (tref seq-tab a-pos1 b-pos)))
                          (if (>= a-b-tail-len b-tail-len)
                            (values a-b-tail-len
                                    (cons (list (car a) a-pos b-pos)
                                          (tref seq-tab a-pos1 b-pos1)))
                            (values b-tail-len (tref seq-tab a-pos b-pos1)))))
                      ;; non-common 
                      (if (>= a-tail-len b-tail-len)
                        (values a-tail-len (tref seq-tab a-pos1 b-pos))
                        (values b-tail-len (tref seq-tab a-pos b-pos1))))
                  (tset! len-tab a-pos b-pos len)
                  (tset! seq-tab a-pos b-pos seq)
                  (b-loop (cdr b) (- b-pos 1))))))))
      
      (list (tref len-tab 0 0) (tref seq-tab 0 0))
      )))

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
