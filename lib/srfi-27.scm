;;;
;;; srfi-27.scm - Sources of Random Bits
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi-27
  (use math.mt-random)
  (use srfi-4)
  (export random-integer random-real default-random-source
          make-random-source random-source?
          random-source-state-ref random-source-state-set!
          random-source-randomize! random-source-pseudo-randomize!
          random-source-make-integers random-source-make-reals
          ))
(select-module srfi-27)

;; Assumes random source is <mersenne-twister> random object for now.
;; It is possible that I extend the implementation so that users can
;; specify the class of random source in future.
(define-constant random-source <mersenne-twister>)

;; Operations on random source
(define (make-random-source) (make random-source))
(define (random-source? obj) (is-a? obj random-source))
(define default-random-source (make-random-source))

(define (random-source-state-ref source)
  (mt-random-get-state source))
(define (random-source-state-set! source state)
  (mt-random-set-state! source state))

(define (%ensure-random-source source)
  (unless (random-source? source)
    (error "random source required, but got" source)))

;; Randomize
(define (random-source-randomize! source)
  (%ensure-random-source source)
  (mt-random-set-seed! source
                       (let1 s (* (exact (sys-time)) (sys-getpid))
                         (logior s (ash s -16)))))

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
  (%ensure-random-source source)
  (when (or (not (integer? i)) (not (integer? j))
            (negative? i) (negative? j))
    (errorf "indices must be non-negative integers: ~s, ~s" i j))
  (mt-random-set-seed! source
                       (list->u32vector (interleave-i i j '(#xffffffff))))
  )

;; Obtain generators from random source.
(define (random-source-make-integers source)
  (%ensure-random-source source)
  (^n (mt-random-integer source n)))

(define random-source-make-reals
  (case-lambda
    [(source)
     (%ensure-random-source source)
     (^[] (mt-random-real source))]
    [(source unit)
     (%ensure-random-source source)
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

