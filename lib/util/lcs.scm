;;;; lcs.scm -- find out the longest common sequence
;;;
;;;   Copyright (c) 2002-2003 by Alex Shinn, All rights reserved.
;;;   Copyright (c) 2002-2019  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;;; Created:    <2002-06-21 15:36:46 foof>
;;; Time-stamp: <2003-02-15 00:09:55 foof>
;;; Author:     Alex Shinn <foof@synthcode.com>

;;; Modified by Shiro Kawai
;;;  - added lcs-fold and rewrote lcs-edit-list using lcs-fold
;;;  - replaced base algorithm from DP to Myers'

(define-module util.lcs
  (use gauche.sequence)
  (use srfi-1)
  (use srfi-11)
  (export lcs lcs-with-positions lcs-fold lcs-edit-list))
(select-module util.lcs)

;; The base algorithm.   This code implements
;; Eugene Myers, "An O(ND) Difference Algorithm and Its Variations",
;; Algorithmica Vol. 1 No. 2, 1986, pp. 251-266.
;; It takes O((M+N)D) time and O((M+N)L) space, where
;; N = (length a), M = (length b), D is the length of the smallest edit
;; sequence (SES), and L is the length of the longest common subsequence (LCS).
;; In most applications the difference is small, so it is much better than
;; DP algorithm that is generally O(MN) time and space complextiy.
;; The worst case where a and b totally differ is O((M+N)^2).
;; The Myers's paper gives refinement of the algorithm
;; that improves worst case behavior, but I don't implement it yet. --[SK]

(define (lcs-with-positions a-ls b-ls :optional (eq equal?))
  (let* ((A  (list->vector a-ls))
         (B  (list->vector b-ls))
         (N  (vector-length A))
         (M  (vector-length B))
         (M+N (+ N M))
         (V_d (make-vector (+ (* 2 M+N) 1) 0))
         (V_r (make-vector (+ (* 2 M+N) 1) '()))
         (V_l (make-vector (+ (* 2 M+N) 1) 0)))

    (let-syntax ((vd
                  (syntax-rules ()
                    ((vd i) (vector-ref V_d (+ i M+N)))
                    ((vd i x) (vector-set! V_d (+ i M+N) x))))
                 (vr
                  (syntax-rules ()
                    ((vr i) (vector-ref V_r (+ i M+N)))
                    ((vr i x) (vector-set! V_r (+ i M+N) x))))
                 (vl
                  (syntax-rules ()
                    ((vl i) (vector-ref V_l (+ i M+N)))
                    ((vl i x) (vector-set! V_l (+ i M+N) x)))))

      (define (finish)
        (let loop ((i (- M+N)) (maxl 0) (r '()))
          (cond ((> i M+N) (list maxl (reverse! r)))
                ((> (vl i) maxl)
                 (loop (+ i 1) (vl i) (vr i)))
                (else
                 (loop (+ i 1) maxl r)))))

      (if (zero? M+N)
        '(0 ()) ;; boundary case
        (let d-loop ((d 0))
          (if (> d M+N)
            (error "lcs-with-positions; something's wrong (implementation error?)")
            (let k-loop ((k (- d)))
              (if (> k d)
                (d-loop (+ d 1))
                (receive (x l r)
                    (if (or (= k (- d))
                            (and (not (= k d))
                                 (< (vd (- k 1)) (vd (+ k 1)))))
                      (values (vd (+ k 1)) (vl (+ k 1)) (vr (+ k 1)))
                      (values (+ (vd (- k 1)) 1) (vl (- k 1)) (vr (- k 1))))
                  (receive (x y l r)
                      (let xy-loop ((x x) (y (- x k)) (l l) (r r))
                        (cond ((>= x N) (values x y l r))
                              ((>= y M) (values x y l r))
                              ((eq (vector-ref A x) (vector-ref B y))
                               (xy-loop (+ x 1) (+ y 1) (+ l 1)
                                        (cons (list (vector-ref A x) x y)
                                              r)))
                              (else (values x y l r))))
                    (vd k x)
                    (vr k r)
                    (vl k l)
                    (if (and (>= x N) (>= y M))
                      (finish)
                      (k-loop (+ k 2))))
                  )))
            )))
      )))

;; Just returns the LCS
(define (lcs a b :optional (eq equal?))
  (map car (cadr (lcs-with-positions a b eq))))

;; Fundamental iterator to deal with editlist.
;;   Similar to Perl's Algorith::Diff's traverse_sequence.
(define (lcs-fold a-only b-only both seed a b :optional (eq equal?))
  (let1 common (cadr (lcs-with-positions a b eq))
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

