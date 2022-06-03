;;;; lcs.scm -- find out the longest common sequence
;;;
;;;   Copyright (c) 2002-2003 by Alex Shinn, All rights reserved.
;;;   Copyright (c) 2002-2022  Shiro Kawai  <shiro@acm.org>
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
  (use gauche.record)
  (use scheme.list)
  (use util.match)
  (use srfi-11)
  (export lcs lcs-with-positions lcs-fold lcs-edit-list
          lcs-edit-list/context lcs-edit-list/unified))
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

;; lcs-edit-list/context
;;   Similar to 'lcs-edit-list', but each hunk is surrounded by
;;   CONTEXT-SIZE elements that are common to both input.
;;
;;   Returns a list of hunks.  Each hunk has a form:
;;
;;     #((<A-start-pos> <A-end-pos> (<sign> <elt>) ...)
;;       (<B-start-pos> <B-end-pos> (<sign> <elt>) ...))
;;
;;   where <sign> may be one of the followings.
;;     = (common),
;;     + (inserted, only appear in B elements),
;;     - (deleted, only appear in A elements),
;;     ! (replaced, appears in both).
;;
;;   The position is 0-origin.  Start-pos is inclusive, end-pos is excluisve.
;;
;; Strategy:
;;   In the first pass, we create a bidirectional graph of nodes
;;    #(item a-pos a-prev a-next b-pos b-prev b-next)
;;   In the second pass, we extract hunks from the nodes.

(define-record-type (Node (pseudo-rtd <vector>)) %make-node #f
  item
  (a-pos)
  (a-prev)
  (a-next)
  (b-pos)
  (b-prev)
  (b-next))

(define (make-node item a-pos b-pos)
  (%make-node item a-pos #f #f b-pos #f #f))

;; The graph begins with head node, and end with tail node, both have
;; #f in the item field.
(define (build-graph a b eq)
  (rlet1 head (make-node #f -1 -1)
    ;; The seed value is (<tip-of-a-graph> . <tip-of-b-graph>)
    (define (a-tip tips) (car tips))
    (define (b-tip tips) (cdr tips))
    (let1 tips
        (lcs-fold (^[elt tips]              ;a-only
                    (let1 n (make-node elt (+ 1 (Node-a-pos (a-tip tips))) #f)
                      (Node-a-prev-set! n (a-tip tips))
                      (Node-a-next-set! (a-tip tips) n)
                      (cons n (b-tip tips))))
                  (^[elt tips]              ;b-only
                    (let1 n (make-node elt #f (+ 1 (Node-b-pos (b-tip tips))))
                      (Node-b-prev-set! n (b-tip tips))
                      (Node-b-next-set! (b-tip tips) n)
                      (cons (a-tip tips) n)))
                  (^[elt tips]              ;common
                    (let1 n (make-node elt
                                       (+ 1 (Node-a-pos (a-tip tips)))
                                       (+ 1 (Node-b-pos (b-tip tips))))
                      (Node-a-prev-set! n (a-tip tips))
                      (Node-a-next-set! (a-tip tips) n)
                      (Node-b-prev-set! n (b-tip tips))
                      (Node-b-next-set! (b-tip tips) n)
                      (cons n n)))
                  (cons head head) a b eq)
      (let1 tail (make-node #f
                            (+ 1 (Node-a-pos (a-tip tips)))
                            (+ 1 (Node-b-pos (b-tip tips))))
        (Node-a-prev-set! tail (a-tip tips))
        (Node-a-next-set! (a-tip tips) tail)
        (Node-b-prev-set! tail (b-tip tips))
        (Node-b-next-set! (b-tip tips) tail)))))

(define (split-point? node)
  (not (eq? (Node-a-next node) (Node-b-next node))))

(define (common-node? node)             ; node is any node but head
  (and (Node-a-prev node) (Node-b-prev node)))

(define (anchor-node? node) (not (Node-item node)))

;; Returns a node where a and b diverges.  Returns #f if it reaches
;; the end of the grpah.
(define (find-split-point node)
  (cond [(or (not (Node-a-next node)) (not (Node-b-next node))) #f]
        [(split-point? node) node]
        [else (find-split-point (Node-a-next node))]))

;; node must be common to both a-path and b-path.  go down the
;; common path, up to N nodes, and returns the last common node.
;; If the paths splits before N nodes, returns the split point node.
(define (forward-path node n)
  (cond [(zero? n) node]
        [(or (not (Node-a-next node)) (not (Node-b-next node))) node]
        [(split-point? node) node]
        [else (forward-path (Node-a-next node) (- n 1))]))

;; node must be common to both paths.  go back upstream up to N nodes.
(define (backward-path node n)
  (cond [(zero? n) node]
        [(not (eq? (Node-a-prev node) (Node-b-prev node))) node]
        [(not (Node-a-prev node)) node]
        [else (backward-path (Node-a-prev node) (- n 1))]))

;; Starting from a split-point node, find a merge-point node.
;; There's always a merge-point (since we have a commin tail node).
(define (find-merge-point node)
  (define (loop a-node)
    (if (common-node? a-node)
      a-node
      (loop (Node-a-next a-node))))
  (loop (Node-a-next node)))

;;Starting from the given common node, extract a next hunk if any.
(define (find-hunk node maker context-size)
  (if-let1 start (find-split-point node)
    (let loop ([hd start])
      (let* ([tl (find-merge-point hd)]
             [next (forward-path tl (- (* 2 context-size) 1))])
        (if (split-point? next)
          (loop next) ; continue
          ;; Need -1, for 'start' and 'next' are both common node.
          (values (maker (backward-path start (- context-size 1))
                         (forward-path tl (- context-size 1)))
                  next))))
    (values #f #f)))

(define (make-context-hunk start-node end-node)
  (define (gather this-next other-next sign)
    (let rec ([n start-node]
              [change? (and (anchor-node? start-node)
                            (not (common-node? (other-next start-node))))])
      (let1 tail (cond [(eq? n end-node) '()]
                       [(other-next n)
                        => (^[nn]
                             (if (common-node? nn)
                               (rec (this-next n) #f)
                               (rec (this-next n) #t)))]
                       [else (rec (this-next n) change?)])
        (cond [(anchor-node? n) tail]
              [(common-node? n) `((= ,(Node-item n)) ,@tail)]
              [change? `((! ,(Node-item n)) ,@tail)]
              [else `((,sign ,(Node-item n)) ,@tail)]))))
  (define (make-half-hunk this-pos this-next other-next sign)
    (let ([start-index (if (anchor-node? start-node)
                         0
                         (this-pos start-node))]
          [end-index (if (anchor-node? end-node)
                       (this-pos end-node)
                       (+ 1 (this-pos end-node)))]
          [path (gather this-next other-next sign)])
      (list* start-index end-index path)))

  (vector (make-half-hunk Node-a-pos Node-a-next Node-b-next '-)
          (make-half-hunk Node-b-pos Node-b-next Node-a-next '+)))

;; API
(define (lcs-edit-list/context a b :optional (eq equal?)
                               :key (context-size 3))
  (assume (and (exact-integer? context-size)
               (<= 1 context-size)))
  (let1 graph (build-graph a b eq)
    (let loop ([hunks '()] [next graph])
      (receive (hunk next) (find-hunk next make-context-hunk context-size)
        (if hunk
          (loop (cons hunk hunks) next)
          (reverse hunks))))))

;; lcs-edit-list/unified
;;
;;  Hunk format
;;   #(<a-start> <a-len> <b-start> <b-len> (<edit> ...))
;;
;;   <edit> : (= <element>)
;;          | (- <element>)
;;          | (+ <element>)
;;

;; API
(define (lcs-edit-list/unified a b :optional (eq equal?)
                               :key (context-size 3))
  (define (make-unified-hunk start-node end-node)
    (define (common n rs)
      (let1 rs (if (anchor-node? n) rs `((= ,(Node-item n)) ,@rs))
        (if (eq? n end-node)
          (reverse rs)
          (let ([a (Node-a-next n)]
                [b (Node-b-next n)])
            (if (eq? a b)
              (common a rs)
              (a-only a b rs))))))
    (define (a-only n b rs)
      (if (common-node? n)
        (b-only b rs)
        (a-only (Node-a-next n) b `((- ,(Node-item n)) ,@rs))))
    (define (b-only n rs)
      (if (common-node? n)
        (common n rs)
        (b-only (Node-b-next n) `((+ ,(Node-item n)) ,@rs))))
    (define (start-index n pos)
      (if (anchor-node? n) (+ 1 (pos n)) (pos n)))
    (define (end-index n pos)
      (if (anchor-node? n) (pos n) (+ 1 (pos n))))
    (define (hunk-size s e pos)
      (- (end-index e pos) (start-index s pos)))

    `#(,(start-index start-node Node-a-pos)
       ,(hunk-size start-node end-node Node-a-pos)
       ,(start-index start-node Node-b-pos)
       ,(hunk-size start-node end-node Node-b-pos)
       ,(common start-node '())))

  (assume (and (exact-integer? context-size)
               (<= 1 context-size)))
  (let1 graph (build-graph a b eq)
    (let loop ([hunks '()] [next graph])
      (receive (hunk next) (find-hunk next make-unified-hunk context-size)
        (if hunk
          (loop (cons hunk hunks) next)
          (reverse hunks))))))
