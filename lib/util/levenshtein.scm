;;;
;;; util.levenshtein - Calculate edit distance between two sequences
;;;
;;;   Copyright (c) 2016-2019  Shiro Kawai  <shiro@acm.org>
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

(define-module util.levenshtein
  (use srfi-1)
  (use srfi-133)
  (use gauche.array)
  (use gauche.sequence)
  (export l-distance l-distances
          re-distance re-distances
          dl-distance dl-distances))
(select-module util.levenshtein)

;; Procedures to calculate Levenshtein, Damerau-Levenshtein, and
;; Restricted edit distances of two sequences
;; (not limited to strings).

;; Note: 
;;  Those widely known algorithms were extensively researched in 60s
;;  to 70s, and sometimes different researchers reported same or similar
;;  algorithms without attaching specific name.  We took widely known
;;  names but the source we found didn't have that names.
;;
;;  Levenshtein distance: Wagner & Fischer, The String-to-string correction
;;  problem, JACM 21(1), 1974, shows DP algorithm to calculate this, but
;;  there's no mention of "Levenshtein" in the paper.
;;
;;  Damerau-Levenshtein-distance: Lowrance & Wagner, An extension of
;;  the string-to-string correction problem, JACM 22(2), 1975.  Again,
;;  there's no mention of "Damerau" nor "Levenshtein".
;;
;;  Restricted edit distance: Oommen & Loke, Pattern
;;  recognition of strings with substitutions, insertions, deletions
;;  and generalized transpositions, Pattern Recognition 30(5), 1997.
;;  This one is mentioned as Optimal string alignment distance in
;;  Wikipedia.  The paper doesn't give itself a name, but claims it's
;;  "less restrictive" than Lowrance & Wagner since it removes the cost
;;  restriction that Lowrance & Wagner requires to work.  On the other
;;  hand, this one restricts how transposition is applied and is not
;;  fully compatible to Damerau-Levenshtein.

;; It is often explained using (N+k)x(M+k) array for dynamic programming
;; (k=1 or 2), but we only need to refer to look back at most k rows,
;; so we can run the algorithm with k+1 rows and rotating them.
;;
;;          s e q u e n c e - A
;;      . . . . . . . . . . . .
;;      . . . . . . . . . . . .
;;   s  . . . . . . . . . . . .
;;   e  . . . . . . . . . . . .
;;   q  . . . . . . . . . . . .    "active" rows
;;   u  . . . . . . . . . . . .    < row-2
;;   e  . . . . . . . . . . . .    < row-1
;;   n  . . . . . . . . X . . .    < row-0     X @ (i,j)
;;   c  . . . . . . . . . . . .
;;   e  . . . . . . . . . . . .
;;   |  . . . . . . . . . . . .
;;   B  . . . . . . . . . . . .
;;
;;
;; Three algorithms only differ slightly in details and it's tempting to
;; factor the common part out.  However such arrangement introduces overhead
;; and the resulting code isn't that clean.  So we decided to implement
;; each separately, allowing some redundancy among their code.
;;
;; NB: Usually, the algorithm is described as sequences with 1-base index,
;; and array index start from 0 or -1 up to the length of the sequence.
;; We use 0-base vector, so the row index is offset by 1 or 2.
;;
;; The base implementation of every algorithm has the following signature:
;;
;;  (base A Bs elt= cutoff)
;;
;; This calculates distance between sequence A and each sequence in the
;; list of sequence Bs, and returns the list of distances.
;; elt= is used to compare elements in the sequence.  cutoff can be #f
;; or a positive integer, and if it's positive integer, we stop calculating
;; the distance of a particular pair as soon as the minimum distance
;; exceeds the value; the distance for that pair becomes #f.

;; Basic Levenshtein
(define (l-base A Bs elt= cutoff)
  (let* ([alen (size-of A)]
         [A    (if (length>=? Bs 3)
                 (coerce-to <list> A)
                 A)]
         [rows (circular-list (make-vector (+ alen 1))
                              (make-vector (+ alen 1)))])
    (define (run B check)
      (vector-unfold! identity (cadr rows) 0 (+ alen 1))
      (for-each-with-index
       (^[j b]
         (let ([row-0 (car rows)]
               [row-1 (cadr rows)]
               [dmin (+ j 1)])
           (vector-set! row-0 0 (+ j 1))
           (for-each-with-index
            (^[i a]
              (let1 d (min (+ (vector-ref row-0 i)       1)
                           (+ (vector-ref row-1 (+ i 1)) 1)
                           (+ (vector-ref row-1 i) (if (elt= a b) 0 1)))
                (vector-set! row-0 (+ i 1) d)
                (when (< d dmin) (set! dmin d))))
            A)
           (when check (check dmin)))
         (pop! rows))
       B)
      (rlet1 r (vector-ref (cadr rows) alen)
        (when check (check r))))

    (define f
      (if cutoff
        (^[B] (let/cc break
                (run B (^d (if (< cutoff d) (break #f))))))
        (cut run <> #f)))

    (map f Bs)))
      
(define (l-distance A B :key (elt= eqv?) (cutoff #f))
  (car (l-base A (list B) elt= cutoff)))

(define (l-distances A Bs  :key (elt= eqv?) (cutoff #f))
  (l-base A Bs elt= cutoff))

;; Restricted Edit distance
;;
;; This one treats transposition as one operation, but does not allow
;; editing a transposed pair afterwards.
;;
;; We need two columns/rows for the boundary, so when we're looking
;; at A[i] and B[j], the matrix cell is (i+2, j+2) and we look at
;;  (i+1, j+2) - deletion
;;  (i+2, j+1) - insertion
;;  (i+1, j+1) - same or substitution
;;  (i, j)     - transposition
;; and j+2 = row-0, j+1 = row-1, j = row-2.

(define (re-base A Bs elt= cutoff)
  (let* ([alen (size-of A)]
         [A    (if (length>=? Bs 3)
                 (coerce-to <vector> A)
                 A)]
         [aref (cute (referencer A) A <>)]
         [rows (circular-list (make-vector (+ alen 2))
                              (make-vector (+ alen 2))
                              (make-vector (+ alen 2)))])
    (define (run B check)
      (define blen (size-of B))
      (define bref (cute (referencer B) B <>))
      (define dmin-1 0)
      (vector-fill! (caddr rows) (+ alen blen))
      (vector-unfold! (^i (if (zero? i) (+ alen blen) (- i 1)))
                      (cadr rows) 0 (+ alen 2))
      (for-each-with-index
       (^[j b]
         (let ([row-0 (car rows)]
               [row-1 (cadr rows)]
               [dmin-0 (+ j 1)])
           (vector-set! row-0 0 (+ alen blen))
           (vector-set! row-0 1 (+ j 1))
           (for-each-with-index
            (^[i a]
              (let* ([d (min (+ (vector-ref row-0 (+ i 1)) 1)
                             (+ (vector-ref row-1 (+ i 2)) 1)
                             (+ (vector-ref row-1 (+ i 1)) (if (elt= a b) 0 1)))]
                     [d (if (and (> i 1) (> j 1)
                                 (elt= (aref i)       (bref (- j 1)))
                                 (elt= (aref (- i 1)) (bref j)))
                          (min d (+ (vector-ref (caddr rows) i)
                                    (if (elt= a b) 0 1)))
                          d)])
                (when (< d dmin-0) (set! dmin-0 d))
                (vector-set! row-0 (+ i 2) d)))
            A)
           (when check (check (min dmin-0 dmin-1)))
           (set! dmin-1 dmin-0))
         (pop! rows)
         (pop! rows))
       B)
      (rlet1 r (vector-ref (cadr rows) (+ alen 1))
        (when check (check r))))

    (define f
      (if cutoff
        (^[B] (let/cc break
                (run B (^d (if (< cutoff d) (break #f))))))
        (cut run <> #f)))

    (map f Bs)))
      
(define (re-distance A B :key (elt= eqv?) (cutoff #f))
  (car (re-base A (list B) elt= cutoff)))

(define (re-distances A Bs  :key (elt= eqv?) (cutoff #f))
  (re-base A Bs elt= cutoff))

;; Damerau-Levenshtein distance
;; We need a way to look up the last character position seen in A.
;; Typical DL algorithm uses an array indexed by character code, but
;; that does not work with large character set or general objects.
;; For the time being, we use an assoc list.  It needs O(length(A))
;; to look up, but in general length(A) is small and the constant
;; factor is very small for alist (we could go with tree map, for
;; example, if an element comparator is provided, but the constant factor
;; of it could surpass the log order benefit if length(A) is small.)

(define (dl-base A Bs elt= cutoff)
  (let* ([alen (size-of A)]
         [A    (if (length>=? Bs 3)
                 (coerce-to <list> A)
                 A)]
         [rows (let* ([maxb (apply max (map size-of Bs))]
                      [rows (vector-tabulate (+ maxb 2)
                                             (^_ (make-vector (+ alen 2))))])
                 ;; The second row is (almost) constant
                 (vector-unfold! (^i (- i 1)) (vector-ref rows 1) 0 (+ alen 2))
                 rows)])

    (define (run B)
      (define DA '())  ; alist to map element -> index in A (0-base)
      (define blen (size-of B))
      (vector-unfold! (^_ (+ alen blen)) (vector-ref rows 0) 0 (+ alen 2))
      (vector-for-each-with-index (^[j v]
                                    (unless (= j 0)
                                      (vector-set! v 0 (+ alen blen))
                                      (vector-set! v 1 (- j 1))))
                                  rows)
      (let-syntax
          ([D (syntax-rules ()
                [(_ i j) (vector-ref (vector-ref rows j) i)]
                [(_ i j v) (vector-set! (vector-ref rows j) i v)])])
        (for-each-with-index
         (^[i a]
           (define DB -1) ; the last j such that A[i] == B[j] (0-base)
           (for-each-with-index
            (^[j b]
              (let* ([k (assoc-ref DA b -1 elt=)]
                     [l DB]
                     [cost (if (elt= a b)
                             (begin (set! DB j) 0)
                             1)]
                     [d (min (+ (D (+ i 2) (+ j 1)) 1)
                             (+ (D (+ i 1) (+ j 2)) 1)
                             (+ (D (+ i 1) (+ j 1)) cost)
                             (+ (D (+ k 1) (+ l 1))
                                (- i k)
                                (- j l 1)))])
                (D (+ i 2) (+ j 2) d)))
            B)
           (push! DA (cons a i)))
         A)
        (D (+ alen 1) (+ blen 1))))

    ;; Because of transposition propagation, early cutoff isn't trivial.
    ;; for now, we just run the entire algorithm then check the result.
    (define f
      (if cutoff
        (^[B] (let1 r (run B)
                (and (<= r cutoff) r)))
        run))

    (map f Bs)))

(define (dl-distance A B :key (elt= eqv?) (cutoff #f))
  (car (dl-base A (list B) elt= cutoff)))

(define (dl-distances A Bs  :key (elt= eqv?) (cutoff #f))
  (dl-base A Bs elt= cutoff))
