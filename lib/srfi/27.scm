;;;
;;; SRFI-27.SCM - Sources of Random Bits
;;;
;;;   Copyright (c) 2000-2025  Shiro Kawai  <shiro@acm.org>
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

;; Implements SRFI-27 interface on top of math.mt-random module.

(define-module srfi.27
  (use math.mt-random)
  (use gauche.uvector)
  (use binary.io)
  (export random-integer random-real default-random-source
          make-random-source random-source?
          random-source-state-ref random-source-state-set!
          random-source-randomize! random-source-pseudo-randomize!
          random-source-make-integers random-source-make-reals
          ))
(select-module srfi.27)

;; Operations on random source
(define (make-random-source) (make-mersenne-twister))
(define (random-source? obj) (is-a? obj <mersenne-twister>))
(define default-random-source (make-random-source))

(define (random-source-state-ref source)
  (mt-random-get-state source))
(define (random-source-state-set! source state)
  (mt-random-set-state! source state))

;; Randomize
(define (random-source-randomize! source)
  (assume (random-source? source))
  (cond
   [(sys-access "/dev/urandom" R_OK)
    (call-with-input-file "/dev/urandom"
      (^p
       (let1 seedv (make-u32vector 4)
         (u32vector-set! seedv 0 (read-u32 p))
         (u32vector-set! seedv 1 (read-u32 p))
         (u32vector-set! seedv 2 (read-u32 p))
         (u32vector-set! seedv 3 (read-u32 p))
         (mt-random-set-seed! source seedv))))]
   [else
    (let1 t (current-time)
      (mt-random-set-seed! source
                           (* (~ t'second) (~ t'nanosecond)
                              (sys-getpid))))]))

(define (random-source-pseudo-randomize! source i j)
  ;; This procedure is effectively required to map integers (i,j) into
  ;; a seed value in a deterministic way.  Talking advantage of the fact
  ;; that Mersenne Twister can take vector of numbers.

  ;; interleave-i and interleave-j creates a list of integers, each
  ;; is less than 2^32, consisted by interleaving each 32-bit chunk of i and j.
  (define (interleave-i i j lis)
    (if (zero? i)
      (if (zero? j) lis (interleave-j 0 j (cons 0 lis)))
      (receive (q r) (quotient&remainder i #x100000000)
        (interleave-j q j (cons r lis)))))

  (define (interleave-j i j lis)
    (if (zero? j)
      (if (zero? i) lis (interleave-i i 0 (cons 0 lis)))
      (receive (q r) (quotient&remainder j #x100000000)
        (interleave-i i q (cons r lis)))))

  ;; main body
  (assume (random-source? source))
  (when (or (not (integer? i)) (not (integer? j))
            (negative? i) (negative? j))
    (errorf "indices must be non-negative integers: ~s, ~s" i j))
  (mt-random-set-seed! source
                       (list->u32vector (interleave-i i j '(#xffffffff))))
  )

;; Obtain generators from random source.
(define-inline (random-source-make-integers source)
  (assume (random-source? source))
  (^n (mt-random-integer source n)))

(define-inline random-source-make-reals
  (case-lambda
    [(source)
     (assume (random-source? source))
     (^[] (mt-random-real source))]
    [(source unit)
     (assume (random-source? source))
     (unless (< 0 unit 1)
       (error "unit must be between 0.0 and 1.0 (exclusive), but got" unit))
     (let* ([1/unit (/ unit)]
            [range (- (floor->exact 1/unit) 1)])
       (^[] (/ (+ 1 (mt-random-integer source range)) 1/unit)))]))

;; Default random generators.
(define-values (random-integer random-real)
  (let1 src default-random-source
    (values (^n (mt-random-integer src n))
            (^[]  (mt-random-real src)))))
