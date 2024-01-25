;;;
;;; mt-random - Mersenne Twister interface
;;;
;;;   Copyright (c) 2000-2024  Shiro Kawai  <shiro@acm.org>
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

(define-module math.mt-random
  (export <mersenne-twister>
          make-mersenne-twister
          mt-random-get-seed
          mt-random-set-seed!
          mt-random-get-state
          mt-random-set-state!
          mt-random-real
          mt-random-real0
          mt-random-integer
          mt-random-fill-u32vector!
          mt-random-fill-f32vector!
          mt-random-fill-f64vector!)
  )
(select-module math.mt-random)

(inline-stub
 (declcode
  (.include "mt-random.h"))

 (define-cclass <mersenne-twister> "ScmMersenneTwister*"
   "Scm_MersenneTwisterClass"
   (c "SCM_CLASS_DEFAULT_CPL")
   ()                                   ;no external slot
   (allocator (let* ([seed (Scm_GetKeyword ':seed initargs '#f)]
                     [priv (Scm_GetKeyword ':private? initargs '#f)]
                     [flags::u_int (?: (SCM_FALSEP priv)
                                       0
                                       SCM_MERSENNE_TWISTER_PRIVATE)])
                (cast void klass)
                (return (Scm_MakeMT seed flags)))))

 (define-cproc make-mersenne-twister (:optional (seed #f)
                                                (private?::<boolean> #f))
   (let* ([flags::u_int 0])
     (when private?
       (set! flags SCM_MERSENNE_TWISTER_PRIVATE))
     (return (Scm_MakeMT seed flags))))

 (define-cproc mt-random-get-seed (mt::<mersenne-twister>)
   (return (-> mt seed)))

 (define-cproc mt-random-set-seed! (mt::<mersenne-twister> init) ::<void>
   Scm_MTSetSeed)

 (define-cproc mt-random-get-state (mt::<mersenne-twister>)
   Scm_MTGetState)

 (define-cproc mt-random-set-state! (mt::<mersenne-twister> state::<u32vector>)
   ::<void>
   Scm_MTSetState)

 (define-cproc mt-random-real (mt::<mersenne-twister>) ::<double>
   (return (Scm_MTGenrandF64 mt TRUE)))

 (define-cproc mt-random-real0 (mt::<mersenne-twister>) ::<double>
   (return (Scm_MTGenrandF64 mt FALSE)))

 (define-cproc %mt-random-integer (mt::<mersenne-twister> n)
   Scm_MTGenrandInt)

 (define-cproc %mt-random-uint32 (mt::<mersenne-twister>) ::<ulong>
   Scm_MTGenrandU32)

 (define-cproc mt-random-fill-u32vector! (mt::<mersenne-twister> v::<u32vector>)
   (return (Scm_MTFillUvector mt (SCM_OBJ v))))

 (define-cproc mt-random-fill-f32vector! (mt::<mersenne-twister> v::<f32vector>)
   (return (Scm_MTFillUvector mt (SCM_OBJ v))))

 (define-cproc mt-random-fill-f64vector! (mt::<mersenne-twister> v::<f64vector>)
   (return (Scm_MTFillUvector mt (SCM_OBJ v))))
 )

(define (%get-nword-random-int mt n)
  (let loop ([i 0] [r (%mt-random-uint32 mt)])
    (if (= i n)
      r
      (loop (+ i 1)
            (+ (ash r 32) (%mt-random-uint32 mt))))))

(define (mt-random-integer mt n)
  (when (not (positive? n)) (error "invalid range" n))
  (if (<= n #x100000000)
    (%mt-random-integer mt n)
    (let* ([siz (ash (integer-length n) -5)]
           [q   (quotient (ash 1 (* 32 (+ siz 1))) n)]
           [qn  (* q n)])
      (let loop ([r (%get-nword-random-int mt siz)])
        (if (< r qn)
          (quotient r q)
          (loop (%get-nword-random-int mt siz)))))))
