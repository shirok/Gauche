;;;
;;; SRFI-194 - Random data generators
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

(define-module srfi.194
  (use data.random)
  (use math.const)
  (use srfi.27)
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

(define (make-random-u1-generator) (integers$ 2))
(define (make-random-u8-generator)  uint8s)
(define (make-random-u16-generator) uint16s)
(define (make-random-u32-generator) uint32s)
(define (make-random-u64-generator) uint64s)
(define (make-random-s8-generator)  int8s)
(define (make-random-s16-generator) int16s)
(define (make-random-s32-generator) int32s)
(define (make-random-s64-generator) int64s)

(define (clamp-real-number lb ub value)
  (assume-type lb <real>)
  (assume-type ub <real>)
  (assume (< lb ub))
  (clamp value lb ub))

(define (make-random-real-generator lb ub) (reals-between$ lb ub))

(define (make-random-rectangular-generator real-lb real-ub imag-lb imag-ub)
  (let ([rgen (reals-between$ real-lb real-ub)]
        [igen (reals-between$ imag-lb imag-ub)])
    (^[] (make-rectangular (rgen) (igen)))))

(define make-random-polar-generator
  (case-lambda
    [(mag-lb mag-ub)
     (make-random-polarg-generator 0 mag-lb mag.ub 0 (* 2 pi))]
    [(origin mag-lb mag-ub)
     (make-random-polarg-generator origin mag-lb mag.ub 0 (* 2 pi))]
    [(mag-lb mag-ub ang-lb ang-ub)
     (make-random-polarg-generator 0 mag-lb mag.ub ang-lb ang-ub)]
    [(origin mag-lb mag-ub ang-lb ang-ub)
     (let ([b (square mag-lb)]
           [m (- (square mag-ub) b)]
           [tgen (reals$)]
           [phigen (reals-between$ ang-lb ang-ub)])
       (^[] (+ origin (make-polar (sqrt (+ (* m (tgen)) b)) (phigen)))))]))

(define (make-random-boolean-generator) booleans)

(define (make-random-char-generator str)
  (assume-type str <string>)
  (samples$ str))

(define (make-random-string-generator k str)
  (assume (exact-integer? k))
  (assume-type str <string>)
  (strings-of k (samples$ str)))

(define (make-bernoulli-generator p)
  (assume-type p <real>)
  (assume (<= 0.0 p 1.0))
  (^[] (let1 r ((random-source-make-reals (current-random-source)))
         (if (< r p) 1 0))))
