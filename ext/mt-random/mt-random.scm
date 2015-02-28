;;;
;;; mt-random - Mersenne Twister interface
;;;
;;;   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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
  (use gauche.uvector)
  (export <mersenne-twister>
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
 (declcode "#include \"mt-random.h\"")
 (initcode (Scm_Init_mt_random))
 
 (define-type <mersenne-twister> "ScmMersenneTwister*")
 (define-type <u32vector> "ScmU32Vector*")
 (define-type <f32vector> "ScmF32Vector*")
 (define-type <f64vector> "ScmF64Vector*")

 (define-cproc mt-random-set-seed! (mt::<mersenne-twister> init) ::<void>
   Scm_MTSetSeed)

 (define-cproc mt-random-get-state (mt::<mersenne-twister>)
   (let* ([v (Scm_MakeU32Vector (+ N 1) 0)])
     (dotimes (i N)
       (set! (aref (SCM_U32VECTOR_ELEMENTS v) i) (aref (-> mt mt) i)))
     (set! (aref (SCM_U32VECTOR_ELEMENTS v) N) (-> mt mti))
     (return v)))

 (define-cproc mt-random-set-state! (mt::<mersenne-twister> state::<u32vector>)
   ::<void>
   (unless (== (SCM_U32VECTOR_SIZE state) (+ N 1))
     (Scm_Error "u32vector of length %d is required, but got length %d"
                (+ N 1) (SCM_U32VECTOR_SIZE state)))
   (dotimes [i N]
     (set! (aref (-> mt mt) i)
           (aref (SCM_U32VECTOR_ELEMENTS state) i)))
   (set! (-> mt mti) (aref (SCM_U32VECTOR_ELEMENTS state) N)))

 (define-cproc mt-random-real (mt::<mersenne-twister>) ::<double>
   (return (Scm_MTGenrandF64 mt TRUE)))

 (define-cproc mt-random-real0 (mt::<mersenne-twister>) ::<double>
   (return (Scm_MTGenrandF64 mt FALSE)))

 (define-cproc %mt-random-integer (mt::<mersenne-twister> n)
   Scm_MTGenrandInt)

 (define-cproc %mt-random-uint32 (mt::<mersenne-twister>) ::<ulong>
   Scm_MTGenrandU32)

 (define-cproc mt-random-fill-u32vector! (mt::<mersenne-twister> v::<u32vector>)
   (let* ([p::ScmUInt32* (SCM_U32VECTOR_ELEMENTS v)])
     (dotimes [i (SCM_U32VECTOR_SIZE v)]
       (set! (* (post++ p)) (Scm_MTGenrandU32 mt)))
     (return (SCM_OBJ v))))

 (define-cproc mt-random-fill-f32vector! (mt::<mersenne-twister> v::<f32vector>)
   (let* ([p::float* (SCM_F32VECTOR_ELEMENTS v)])
     (dotimes [i (SCM_F32VECTOR_SIZE v)]
       (set! (* (post++ p)) (Scm_MTGenrandF32 mt TRUE)))
     (return (SCM_OBJ v))))

 (define-cproc mt-random-fill-f64vector! (mt::<mersenne-twister> v::<f64vector>)
   (let* ([p::double* (SCM_F64VECTOR_ELEMENTS v)])
     (dotimes [i (SCM_F64VECTOR_SIZE v)]
       (set! (* (post++ p)) (Scm_MTGenrandF64 mt TRUE)))
     (return (SCM_OBJ v))))
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

