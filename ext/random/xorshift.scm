;;;
;;; math.xorshift - Xorshift PRNG
;;;
;;;   Copyright (c) 2006  Shiro Kawai  <shiro@acm.org>
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

;; David Blackman, Sebastiano Vigna,
;; Scrambled Linear Pseudorandom Number Generators
;; https://arxiv.org/abs/1805.01407

;; For SplitMix64 generator,
;; Guy L Steele Jr, Doug Lea, Christine H Flood,
;; Fast Splittable Pseudorandom Number Generators
;; https://dl.acm.org/doi/pdf/10.1145/2714064.2660195

(define-module math.xorshift
  (export <xoshiro256>
          make-xoshiro
          xoshiro-get-seed
          xoshiro-set-seed!
          xoshiro-u64
          xoshiro-real
          xoshiro-real0))
(select-module math.xorshift)

(inline-stub
 (.include "gauche/priv/numberP.h")

 (declcode
  (define-ctype ScmXoshiro256::(.struct
                                (SCM_HEADER :: ""
                                 s::(.array uint64_t (4))
                                 seed::uint64_t))) ; original seed
  )

 (define-cclass <xoshiro256> :private :no-meta
   "ScmXoshiro256*"
   "ScmXoshiroClass"
   (c "SCM_CLASS_DEFAULT_CPL")
   ()
   (allocator (let* ([seed_s (Scm_GetKeyword ':seed initargs '#f)]
                     [seed::uint64_t (Scm_GetIntegerU64 seed_s)]
                     ;[priv (Scm_GetKeyword ':private? initargs '#f)]
                     [gen::ScmXoshiro256* (SCM_NEW ScmXoshiro256)])
                (SCM_SET_CLASS gen klass)
                (set! (-> gen seed) seed)
                (xoshiro256-init gen seed)
                (return (SCM_OBJ gen)))))

 ;; For initial state generation.  See SplitMix paper for all the constants.
 (declcode
  (define-ctype SplitMix64::(.struct
                             (seed::uint64_t
                              gamma::uint64_t)))
  )

 (define-cfn next-seed (s::SplitMix64*) ::uint64_t :static
   (set! (-> s seed) (+ (-> s seed) (-> s gamma)))
   (return (-> s seed)))

 (define-cfn mix64 (z::uint64_t) ::uint64_t :static
   (set! z (* (logxor z (>> z 33)) (C: "0xff51afd7ed558ccdULL")))
   (set! z (* (logxor z (>> z 33)) (C: "0xc4ceb9fe1a85ec53ULL")))
   (return (logxor z (>> z 33))))

 (define-cfn mix64-init (s::SplitMix64* seed::uint64_t) ::void  :static
   (set! (-> s seed) seed
         (-> s gamma) (C: "0x9e3779b97f4a7c15ULL"))) ;Golden Gamma

 ;; Xoshiro256++
 (define-cfn rotate64 (x::uint64_t k::int) ::uint64_t :static :inline
   (return (logior (<< x k) (>> x (- 64 k)))))

 (define-cfn xoshiro256++ (gen::ScmXoshiro256*) ::uint64_t :static
   (let* ([s::uint64_t* (-> gen s)]
          [result::uint64_t (+ (rotate64 (+ (aref s 0) (aref s 3)) 23)
                               (aref s 0))]
          [t::uint64_t (<< (aref s 1) 17)])
     (logxor= (aref s 2) (aref s 0))
     (logxor= (aref s 3) (aref s 1))
     (logxor= (aref s 1) (aref s 2))
     (logxor= (aref s 0) (aref s 3))
     (logxor= (aref s 2) t)
     (set! (aref s 3) (rotate64 (aref s 3) 45))
     (return result)))

 (define-cfn xoshiro256-init (gen::ScmXoshiro256* seed::uint64_t)
   ::void :static
   (let* ([mixstate::SplitMix64])
     (mix64-init (& mixstate) seed)
     (set! (aref (-> gen s) 0) (mix64 (next-seed (& mixstate))))
     (set! (aref (-> gen s) 1) (mix64 (next-seed (& mixstate))))
     (set! (aref (-> gen s) 2) (mix64 (next-seed (& mixstate))))
     (set! (aref (-> gen s) 3) (mix64 (next-seed (& mixstate))))))
 )

;; API
(define (make-xoshiro :key (seed 42))
  (make <xoshiro256> :seed seed))

;; API
(define-cproc xoshiro-get-seed (gen::<xoshiro256>) ::<uint64>
  (return (-> gen seed)))

;; API
(define-cproc xoshiro-set-seed! (gen::<xoshiro256> seed::<uint64>) ::<void>
  (set! (-> gen seed) seed)
  (xoshiro256-init gen seed))

;; API
(define-cproc xoshiro-u64 (gen::<xoshiro256>) ::<uint64>
  (return (xoshiro256++ gen)))

(inline-stub
 (define-cfn get-real (gen::ScmXoshiro256* exclude0::_Bool) ::double :static
   (for ()
     (let* ([v::uint64_t (xoshiro256++ gen)]
            [sign::int (>> v 63)]
            [mant::uint64_t (logand (>> v 10)
                                    (C: #x000f_ffff_ffff_ffff))]
            [d::double (Scm__EncodeDouble64 v #x3fe sign)])
       (unless (and exclude0 (== d 0.0))
         (return d)))))
 )

;; API
(define-cproc xoshiro-real (gen::<xoshiro256>) ::<double>
  (return (get-real gen TRUE)))
(define-cproc xoshiro-real0 (gen::<xoshiro256>) ::<double>
  (return (get-real gen FALSE)))
