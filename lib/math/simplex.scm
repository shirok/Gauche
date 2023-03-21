;;;
;;; math/simplex.scm - Simplex solver
;;;
;;;   Copyright (c) 2023  Shiro Kawai  <shiro@acm.org>
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
  (use srfi-42)
  (use gauche.sequence)
  (use gauche.array)
  (use gauche.uvector)
  (export simplex-solve)
  )
(select-module math.simplex)

;; Input and output:
;;  A is an m x n array
;;  b is a uvector of length n (bounds)
;;  c is a uvector of length m (coefficients)
;;  Compute x, uvector of length n, that minimizes c^T . x under the constraint
;;  of Ax <= b.
;;  NB: A and c do not contain entries for slack variables.
;;  Elements of x are all non-negative.

;; Terminology:
;;  IxB : u32vector of length n, indices of basic variables
;;  IxN : u32vector of length m, indices of non-basic variabls
;;  B^  : n x n matrix, inverse of basis matrix.
;;  β   : u64vector of length n, basic solution

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
;;    c0  c1  c2  b0  b1  b2  b3
;;
;;    a00 a01 a02  1   0   0   0    b0
;;    a10 a11 a12  0   1   0   0    b1
;;    a20 a21 a22  0   0   1   0    b2
;;    a30 a31 a32  0   0   0   1    b3
;;
;; The initial IxB is [3 4 5 6], IxN is [0 1 2].
;; Basic feasible solution is [0 0 0 b0 b1 b2 b3]
;;
;;

(define (simplex-solve A b c)
  (define-values (n m) (array2d-size-check A b c))
  ;; The following variables are updated for each iteration
  (define IxB (list->u32vector (iota n m))) ; basis indices
  (define IxN (list->u32vector (iota m)))   ; non-basis indices
  (define cc  (f64vector-append c (make-f64vector n 0))) ; extended
  (define B^  (identity-array n <f64array>)) ; inverse of basis array
  (define AA  (array-concatenate A B^ 1))   ; extended
  (define π   (make-f64vector n 0))     ;cc↑B . B^
  (define p   (f64vector-copy b))

  (define (compute-multiplier-vector!)
    (dotimes [i n]
      (uvector-set! π i
                    (sum-ec (: j n)
                            (* (uvector-ref cc (uvector-ref IxB j))
                               (array-ref B^ j i))))))

  (define (find-entering-variable)
    (compute-multiplier-vector!)
    (let loop ([i 0]                    ;loop over IxN
               [min-negative +inf.0]
               [min-index #f])
      (if (= i m)
        min-index                       ;if #f, we're optimal
        (let* ([in (u32vector-ref IxN i)]
               [c_in (- (uvector-ref cc in)
                        (sum-ec (: j n)
                                (* (uvector-ref π j)
                                   (array-ref AA j in))))])
          (if (and (negative? c_in)
                   (< c_in min-negative))
            (loop (+ i 1) c_in i)
            (loop (+ i 1) min-negative min-index))))))

  (define (find-leaving-row A_i x_k)
    (let loop ([i 0]
               [min-ratio +inf.0]
               [min-row-index -1])
      (if (= i n)
        min-row-index
        (let ([ratio (/ (f64vector-ref p i) (f64vector-ref x_k i))])
          (if (and (positive? (f64vector-ref x_k i)) (< ratio min-ratio))
            (loop (+ i 1) ratio i)
            (loop (+ i 1) min-ratio min-row-index))))))

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

  ;; (print "cc=" cc)
  ;; (print "IxB=" IxB)
  ;; (print "IxN=" IxN)
  ;; (print "p=" p)
  ;; (pretty-print-array A #t :readable? #f :left #\[ :right #\])

  (let loop ([entering (find-entering-variable)]
             [iter 0])
    (and entering
         (let* ([A_i (map-to <f64vector>
                             (^i (array-ref AA i entering))
                             (iota n))]
                [x_k (array-vector-mul B^ A_i)]
                [leaving (find-leaving-row A_i x_k)])
           ;; (pretty-print-array B^ #t :readable? #f :left #\[ :right #\])
           ;; (format #t "π = ~s\n" π)
           ;; (format #t "A_i = ~s\n" A_i)
           ;; (format #t "x_k = ~s\n" x_k)
           ;; (format #t "entering = ~s\n" entering)
           ;; (format #t "leaving = ~s\n" leaving)

           (pivot! entering leaving x_k)

           ;; (print "pivot!")
           ;; (pretty-print-array B^ #t :readable? #f :left #\[ :right #\])
           ;; (format #t "p=~s\n" p)
           ;; (print "IxB=" IxB)
           ;; (print "IxN=" IxN)

           (loop (find-entering-variable) (+ iter 1))
           )))

  (rlet1 rvec (make-f64vector m 0)
    (dotimes [i n]
      (when (< (u32vector-ref IxB i) m)
        (f64vector-set! rvec (u32vector-ref IxB i)
                        (f64vector-ref p i)))))
  )

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
