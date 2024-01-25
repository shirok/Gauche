;;;
;;; math/simplex.scm - Simplex solver
;;;
;;;   Copyright (c) 2023-2024  Shiro Kawai  <shiro@acm.org>
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

;; Linear program solver using Revised simplex method

(define-module math.simplex
  (use gauche.array)
  (use gauche.sequence)
  (use gauche.uvector)
  (use srfi.42)
  (export simplex-solve)
  )
(select-module math.simplex)

(define-constant *debug-dump* #f)

(define-macro (debug-dump . exprs)
  (if *debug-dump*
    `(begin ,@exprs)
    `(begin)))

;; Input and output:
;;  A is an m x n array
;;  b is a uvector of length n (bounds)
;;  c is a uvector of length m (coefficients)
;;  Compute x, uvector of length n, that minimizes or maximizes
;;  Z = c^T . x under the constraint of Ax <= b, x_i >= 0.

;; Returns uvector of length m for solution x, or #f if it's unsolvable.

;; Terminology:
;;  IxB : u32vector of length n, indices of basic variables
;;  IxN : u32vector of length m, indices of non-basic variabls
;;  B^  : n x n matrix, inverse of basis matrix.
;;  p   : f64vector of length n, RHS values.

;; We first extend the matrix A  by adding an nxn identity matrix on its right,
;; and extend c accordingly.
;; Hence initially, basis matrix is an nxn identity matrix.
;; Suppose the given A as 3x4 matrix:
;;
;;    a00 a01 a02
;;    a10 a11 a12
;;    a20 a21 a22
;;    a30 a31 a32
;;
;; and the given b as uvector:
;;
;;    b0  b1  b2  b3
;;
;; and the given c as uvector:
;;
;;    c0  c1  c2
;;
;; Then the initial tableau will be as follows:
;;
;;    -c0 -c1 -c2  0   0   0   0
;;
;;    a00 a01 a02  1   0   0   0    b0
;;    a10 a11 a12  0   1   0   0    b1
;;    a20 a21 a22  0   0   1   0    b2
;;    a30 a31 a32  0   0   0   1    b3
;;
;; The initial IxB is [3 4 5 6], IxN is [0 1 2].
;; Basic feasible solution is [0 0 0 b0 b1 b2 b3]

;; If any of elements in b is negative, we switch to two-phase simplex method.

(define (simplex-solve A b goal c)
  (define maximize?
    (case goal
      [(:minimize) #f]
      [(:maximize) #t]
      [else (error "Goal must be either :minimize or :maximize, but got:"
                   goal)]))
  (define-values (n m Na AA cc p B^ IxB IxN) (%canonicalize A b c))
  (debug-dump
   (print "cc=" cc)
   (print "IxB=" IxB)
   (print "IxN=" IxN)
   (print "p=" p)
   (display "AA:")
   (pretty-print-array AA #t :readable? #f :left #\[ :right #\]))
  (if (zero? Na)
    (%solve-1-phase n m Na AA cc p B^ IxB IxN maximize?)
    (%solve-2-phase n m Na AA c cc p B^ IxB IxN maximize?)))

(define (%solve-1-phase n m Na AA cc p B^ IxB IxN maximize?)
  (receive (AA p B^ IxB IxN)
      (%simplex-solve n m Na AA cc p B^ IxB IxN maximize?)
    (%result-variable-vector n m p IxB)))

(define (%solve-2-phase n m Na AA c cc p B^ IxB IxN maximize?)
  (define AN (%project-columns AA IxN))
  (receive (AA p B^ IxB IxN)
      (%simplex-solve n m Na AA cc p B^ IxB IxN #f)
    (debug-dump
     (print "\nPhase 1 solution:")
     (print "IxB=" IxB)
     (print "IxN=" IxN)
     (print "p=" p)
     (display "B^:")
     (pretty-print-array B^ #t :readable? #f :left #\[ :right #\]))
    (and (%phase2-feasible? p IxB n m)
         ;; Constrct the second phase tableau.  Non-basis columns
         ;; are the negation of the corresponding column of B^
         (let* ([cc (f64vector-append (f64vector-mul c -1)
                                      (make-f64vector n 0))]
                [IxN (%prepare-phase2-non-basis n m IxN)]
                [AA (%prepare-phase2-matrix n m AN B^ IxB IxN)]
                [B^ (identity-array n <f64array>)])
           (debug-dump
            (print "\nPhase 2:")
            (print "cc=" cc)
            (print "IxB=" IxB)
            (print "IxN=" IxN)
            (print "p=" p)
            (display "AA:")
            (pretty-print-array AA #t :readable? #f :left #\[ :right #\]))
           (receive (AA p B^ IxB IxN)
               (%simplex-solve n m 0 AA cc p B^ IxB IxN maximize?)
             (%result-variable-vector n m p IxB))))))

;; Returns the following values:
;;   n, m - # of rows and columns of original problem
;;   Na   - # of auxiliary variables.  If this is >zero, we need 2-phase
;;         method.
;;   AA  - n x (m + n + Na) Matrix extended with slack and artificial
;;         varaibles.
;;   cc   - m + n + Na element vector of coefficient row.
;;   p   - n element vector for RHS.  All elements are posivitive.
;;   B^  - n x n matrix, the inverse of basis array.  Initially it is
;;         an identity matrix.
;;   IxB - n element integer vector for basis indices
;;   IxN - m + Na element integer vector for non-basis indices
(define (%canonicalize A b c)
  (define-values (n m) (array2d-size-check A b c))
  (let1 Na (fold (^[e cnt] (if (negative? e) (+ cnt 1) cnt)) 0 b)
    (if (zero? Na)
      ;; single-phase
      (let ([AA (array-concatenate A (identity-array n <f64array>) 1)]
            [cc (f64vector-append (if (f64vector? c)
                                    (f64vector-mul c -1)
                                    (map-to <f64vector> - c))
                                  (make-f64vector n 0))]
            [p (coerce-to <f64vector> b)]
            [B^ (identity-array n <f64array>)]
            [IxB (list->u32vector (iota n m))]
            [IxN (list->u32vector (iota m))])
        (values n m Na AA cc p B^ IxB IxN))
      ;; phase 1 for two-phase
      (let ([AA (make-f64array (shape 0 n 0 (+ m n Na)) 0)]
            [cc (make-f64vector (+ m n Na) 0)]
            [p  (coerce-to <f64vector> b)]
            [B^ (identity-array n <f64array>)]
            [IxB (make-u32vector n)]
            [IxN (make-u32vector (+ m Na))])
        (let loop-rows ([i 0]
                        [cs m]          ; slack var column
                        [ca (+ m n)])   ; aux var column
          (when (< i n)
            (cond [(not (negative? (~ b i)))
                   ;; We copy original A row and add a slack var.
                   ;; The slack var consists one of the basis.
                   (do-ec (: j m)
                          (array-set! AA i j (array-ref A i j)))
                   (array-set! AA i cs 1)
                   (set! (~ p i) (~ b i))
                   (set! (~ IxB i) cs)
                   (loop-rows (+ i 1) (+ cs 1) ca)]
                  [else
                   ;; We negate the original A row, subtract a slack var,
                   ;; and add an aux var  The aux var consists one of the basis.
                   (do-ec (: j m)
                          (array-set! AA i j (- (array-ref A i j))))
                   (array-set! AA i cs -1)
                   (array-set! AA i ca 1)
                   (set! (~ p i) (- (~ b i)))
                   (set! (~ cc ca) -1)
                   (set! (~ IxB i) ca)
                   (loop-rows (+ i 1) (+ cs 1) (+ ca 1))])))
        ;; Fill IxN
        (let loop-cols ([j 0] [in 0])
          (when (< j (+ m n Na))
            (if (find (cut = <> j) IxB)
              (loop-cols (+ j 1) in)
              (begin
                (set! (~ IxN in) j)
                (loop-cols (+ j 1) (+ in 1))))))
        (values n m Na AA cc p B^ IxB IxN)))))

;; Returns solved AA, p, B^, IxB, IxN.
(define (%simplex-solve n m Na AA cc p B^ IxB IxN maximize?)
  (define pp   (make-f64vector n 0))     ;cc↑B . B^

  (define pivot-selection-rule 'bland)

  (define (compute-multiplier-vector!)
    (dotimes [i n]
      (uvector-set! pp i
                    (sum-ec (: j n)
                            (* (uvector-ref cc (uvector-ref IxB j))
                               (array-ref B^ j i))))))

  ;; Returns i such that IxN[i] identifies the entering variable
  (define (find-entering-variable)
    (compute-multiplier-vector!)
    (debug-dump
     (display "B^:")
     (pretty-print-array B^ #t :readable? #f :left #\[ :right #\])
     (format #t "pp = ~s\n" pp))
    (let loop ([i 0]                    ;loop over IxN
               [picked-value #f]
               [picked-index #f])
      (if (= i m)
        picked-index                    ;if #f, we're optimal
        (let* ([in (u32vector-ref IxN i)]
               [c_in (- (uvector-ref cc in)
                        (sum-ec (: j n)
                                (* (uvector-ref pp j)
                                   (array-ref AA j in))))])
          (debug-dump
           (print "c_in[" (u32vector-ref IxN i) "] = " c_in))
          (if (and ((if maximize? negative? positive?) c_in)
                   (ecase pivot-selection-rule
                     [(min-ratio) (if maximize?
                                    (< c_in picked-value)
                                    (> c_in picked-value))]
                     [(bland) (or (not picked-index)
                                  (< in (u32vector-ref IxN picked-index)))]))
            (loop (+ i 1) c_in i)
            (loop (+ i 1) picked-value picked-index))))))

  (define (find-leaving-row A_i x_k)
    (debug-dump
     (format #t "A_i = ~s\n" A_i)
     (format #t "x_k = ~s\n" x_k))
    (let loop ([i 0]
               [min-ratio +inf.0]
               [min-row-index -1])
      (cond [(= i n)
             (debug-dump
              (format #t "leaving row: ~s\n" min-row-index))
             min-row-index]
            [(<= (f64vector-ref x_k i) 0)
             (loop (+ i 1) min-ratio min-row-index)]
            [else
             (let ([ratio (/ (f64vector-ref p i) (f64vector-ref x_k i))])
               (debug-dump
                (format #t "ratio[~s] = ~s\n" i ratio))
               (if (case pivot-selection-rule
                     [(min-ratio) (< ratio min-ratio)]
                     [(bland) (or (< ratio min-ratio)
                                  (and (= ratio min-ratio)
                                       (< (u32vector-ref IxB i)
                                          (u32vector-ref IxB min-row-index))))])
                 (loop (+ i 1) ratio i)
                 (loop (+ i 1) min-ratio min-row-index)))])))

  (define (pivot! entering leaving x_k)
    (let ([factor (/ (f64vector-ref p leaving)
                     (f64vector-ref x_k leaving))])
      ;; Make x_k[leaving] 1, and adjust the same row of B^
      (dotimes [j n]
        (array-set! B^ leaving j
                    (/ (array-ref B^ leaving j)
                       (f64vector-ref x_k leaving))))
      (f64vector-set! p leaving factor)
      ;; Make x_k[i] zero, and adjust the same row of B^
      (dotimes [i n]
        (unless (= i leaving)
          (dotimes [j n]
            (array-set! B^ i j
                        (- (array-ref B^ i j)
                           (* (f64vector-ref x_k i)
                              (array-ref B^ leaving j)))))
          (f64vector-set! p i
                          (- (f64vector-ref p i)
                             (* (f64vector-ref x_k i) factor)))))
      ;; Swap pivotted index
      (rotate! (u32vector-ref IxB leaving) (u32vector-ref IxN entering))
      ))

  (let loop ([entering (find-entering-variable)]
             [iter 0])
    (when entering
      (debug-dump
       (format #t "entering var = ~s\n" (u32vector-ref IxN entering)))
      (let* ([A_i (map-to <f64vector>
                          (^i (array-ref AA i (u32vector-ref IxN entering)))
                          (iota n))]
             [x_k (array-vector-mul B^ A_i)]
             [leaving (find-leaving-row A_i x_k)])

        (when (>= leaving 0)

          (pivot! entering leaving x_k)

          (debug-dump
           (print "pivot!")
           (format #t "p=~s\n" p)
           (print "IxB=" IxB)
           (print "IxN=" IxN))

          (loop (find-entering-variable) (+ iter 1))
          ))))

  ;; Results
  (values AA p B^ IxB IxN))

;; Create a result vector of variables
(define (%result-variable-vector n m p IxB)
  (rlet1 rvec (make-f64vector m 0)
    (dotimes [i n]
      (when (< (u32vector-ref IxB i) m)
        (f64vector-set! rvec (u32vector-ref IxB i)
                        (f64vector-ref p i))))))


;; See the phase-1 result of two-phase method to see if there's a feasible
;; answer.  We add aux var at the end of the tableau.  If all aux vars
;; in the solution is 0, it is feasible.
(define (%phase2-feasible? p IxB n m)
  (every?-ec (: col (index ib) IxB)
             (or (< col (+ n m))
                 (zero? (uvector-ref p ib)))))

;; Returns a non-basis mapping vec that excludes aux vars.
(define (%prepare-phase2-non-basis n m IxN)
  (list->u32vector
   (list-ec (: j IxN)
            (if (< j (+ n m)))
            j)))

;; From array X, constract a subarray Y by picking rows listed in V.
(define (%project-columns X V)
  (let ([rows (array-length X 0)]
        [cols (array-length X 1)]
        [new-cols (size-of V)])
    (rlet1 Y (make-f64array (shape 0 rows 0 new-cols) 0)
      (do-ec (: c (index j) V)
             (: r rows)
             (array-set! Y r j (array-ref X r c))))))

;; Construct the subject matrix for phase 2.
;; AN is the non-basis columns of original array.
;; B^ and IxB are from the phase1 result.
;; IxN is from the phase1 result, minus aux var columns.
(define (%prepare-phase2-matrix n m AN B^ IxB IxN)
  (define AA* (array-concatenate (array-mul B^ AN) B^ 1))
  (%project-columns AA* (iota (+ n m))))

;; returns m, n of 2d array, with argument check
(define (array2d-size-check a b c)
  (assume (= (array-rank a) 2)
          "Rank 2 array is required, but got:" a)
  (assume (every (^i (zero? (array-start a i))) '(0 1))
          "Array index must be 0-based, but got:" a)
  (let ([rows (array-length a 0)]
        [cols (array-length a 1)])
    (assume (= cols (size-of c))
            "Coefficient vector and matrix size don't match:" c)
    (assume (= rows (size-of b))
            "Limit value vector and matrix size don't match:" b)
    (values rows cols)))
