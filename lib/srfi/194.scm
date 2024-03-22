;;;
;;; SRFI-194 - Random data generators
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

(define-module srfi.194
  (use gauche.sequence)
  (use data.random)
  (use math.const)
  (use util.match)
  (use srfi.27)
  (use srfi.42)
  (export current-random-source with-random-source
          make-random-source-generator
          make-random-integer-generator
          make-random-u1-generator
          make-random-u8-generator
          make-random-s8-generator
          make-random-u16-generator
          make-random-s16-generator
          make-random-u32-generator
          make-random-s32-generator
          make-random-u64-generator
          make-random-s64-generator
          clamp-real-number
          make-random-real-generator
          make-random-rectangular-generator
          make-random-polar-generator
          make-random-boolean-generator
          make-random-char-generator
          make-random-string-generator

          make-bernoulli-generator
          make-binomial-generator
          make-categorical-generator
          make-normal-generator
          make-exponential-generator
          make-geometric-generator
          make-poisson-generator
          make-zipf-generator
          make-sphere-generator
          make-ellipsoid-generator
          make-ball-generator
          gsampling
          ))
(select-module srfi.194)

(define current-random-source random-data-random-source)

(define (with-random-source rs thunk)
  (parameterize ((current-random-source rs))
    (thunk)))

(define (make-random-source-generator i)
  (let1 j 0
    (^[]
      (rlet1 rs (make-random-source)
        (random-source-pseudo-randomize! rs i j)
        (inc! j)))))

(define (make-random-integer-generator lb ub)
  (assume-type lb <fixnum>)
  (assume-type ub <fixnum>)
  (assume (< lb ub))
  (integers$ (- ub lb) lb))

(define (make-random-u1-generator)  (integers$ 2))
(define (make-random-u8-generator)  (uint8s$))
(define (make-random-u16-generator) (uint16s$))
(define (make-random-u32-generator) (uint32s$))
(define (make-random-u64-generator) (uint64s$))
(define (make-random-s8-generator)  (int8s$))
(define (make-random-s16-generator) (int16s$))
(define (make-random-s32-generator) (int32s$))
(define (make-random-s64-generator) (int64s$))

(define (clamp-real-number lb ub value)
  (assume-type lb <real>)
  (assume-type ub <real>)
  (assume (< lb ub))
  (clamp value lb ub))

(define (make-random-real-generator lb ub) (reals-between$ lb ub))

(define make-random-rectangular-generator complexes-rectangular$)

(define make-random-polar-generator complexes-polar$)

(define (make-random-boolean-generator) (booleans$))

(define (make-random-char-generator str)
  (assume-type str <string>)
  (assume (not (equal? str "")) "Empty string not allowed")
  (samples$ str))

(define (make-random-string-generator k str)
  (assume (and (exact-integer? k) (> k 0))
          "Length bound must be positive exact integer:" k)
  (assume-type str <string>)
  (assume (not (equal? str "")) "Empty string not allowed")
  (strings-of (integers$ k) (samples$ str)))

(define (make-bernoulli-generator p)
  (define gen-real (random-source-make-reals (current-random-source)))
  (assume-type p <real>)
  (assume (<= 0.0 p 1.0))
  (^[] (if (< (gen-real) p) 1 0)))

;; NB: This may be generially useful, so we might move it to some
;; other library later.
(define (binomial-coefficient n k)
  (define (C n k numer denom)
    (if (= k 0) (/ numer denom) (C (- n 1) (- k 1) (* n numer) (* k denom))))
  (assume-type n <integer>)
  (assume-type k <integer>)
  (assume (<= 0 k n))
  (cond [(= k 0) 1]
        [(= k n) 1]
        [(< n (* 2 k)) (C n (- n k) 1 1)]
        [else          (C n k 1 1)]))

(define (make-binomial-generator n p)
  (assume-type n <integer>)
  (assume-type p <real>)
  (assume (< 0 n))
  (assume (<= 0 p 1))
  (let1 weights (make-vector (+ n 1))
    (do-ec (: k (+ (ash n -1) 1))
           (let1 nCk (binomial-coefficient n k)
             (vector-set! weights k
                          (* nCk (expt p k) (expt (- 1 p) (- n k))))
             (vector-set! weights (- n k)
                          (* nCk (expt p (- n k)) (expt (- 1 p) k)))))
    (make-categorical-generator weights)))

(define (make-categorical-generator weight-vec)
  (assume-type weight-vec (<Vector> <real>))
  (let1 weighted (map-with-index (^[i v] (cons v (constantly i))) weight-vec)
    (weighted-samples-from weighted)))

(define (make-normal-generator :optional (mean 0.0) (deviation 1.0))
  (reals-normal$ mean deviation))

(define (make-exponential-generator mean)
  (reals-exponential$ mean))

(define (make-geometric-generator p)
  (integers-geometric$ p))

(define (make-poisson-generator L)
  (integers-poisson$ L))

(autoload srfi.194.zipf-zri make-zipf-generator)

(autoload srfi.194.sphere
          make-sphere-generator make-ellipsoid-generator make-ball-generator)

(define (gsampling . generators) (samples-from generators))
