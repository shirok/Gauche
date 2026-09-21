;;;
;;; math.random.xos - xoshiro PRNG
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

(define-module math.random.xos
  (export <xoshiro256>
          make-xoshiro256
          copy-xoshiro256
          copy-xoshiro256!
          xos-random-get-seed
          xos-random-set-seed!
          xos-random-u64
          xos-random-real
          xos-random-real0
          xos-random-fill-u64vector!
          xos-random-fill-f32vector!
          xos-random-fill-f64vector!))
(select-module math.random.xos)

(inline-stub
 (.include "gauche/priv/numberP.h")

 (declcode
  (define-ctype ScmXoshiro256::(.struct
                                (SCM_HEADER :: ""
                                 s::(.array uint64_t (4))
                                 seed::uint64_t     ; original seed
                                 flags::u_long
                                 lock::ScmInternalMutex)))

  ;; flags
  ;; SCM_XOSHIRO_PRIVATE - Do not use mutex.
  (.define SCM_XOSHIRO_PRIVATE (<< 1 0))
  (.define XOSHIRO_NEED_LOCK (xos)
           (not (logand (-> xos flags) SCM_XOSHIRO_PRIVATE)))
  )

 ;; Grab the lock while updating the state, unless the generator is private.
 ;; NB: BODY must not escape (no 'return' etc.).
 (define-cise-stmt with-xos-lock
   [(_ xos . body)
    `(begin
       (when (XOSHIRO_NEED_LOCK ,xos)
         (SCM_INTERNAL_MUTEX_LOCK (-> ,xos lock)))
       ,@body
       (when (XOSHIRO_NEED_LOCK ,xos)
         (SCM_INTERNAL_MUTEX_UNLOCK (-> ,xos lock))))])

 (define-cclass <xoshiro256> :private :no-meta
   "ScmXoshiro256*"
   "ScmXoshiroClass"
   (c "SCM_CLASS_DEFAULT_CPL")
   ()
   (allocator (let* ([seed_s (Scm_GetKeyword ':seed initargs '#f)]
                     [seed::uint64_t (Scm_GetIntegerU64 seed_s)]
                     [priv (Scm_GetKeyword ':private? initargs '#f)]
                     [xos::ScmXoshiro256* (SCM_NEW ScmXoshiro256)])
                (SCM_SET_CLASS xos klass)
                (set! (-> xos flags) (?: (SCM_FALSEP priv)
                                         0
                                         SCM_XOSHIRO_PRIVATE))
                (set! (-> xos seed) seed)
                (xoshiro256-init xos seed)
                (SCM_INTERNAL_MUTEX_INIT (-> xos lock))
                (return (SCM_OBJ xos)))))

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

 ;; Caller must hold the lock.
 (define-cfn xoshiro256++ (xos::ScmXoshiro256*) ::uint64_t :static
   (let* ([s::uint64_t* (-> xos s)]
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

 ;; Caller must hold the lock.
 (define-cfn xoshiro256-init (xos::ScmXoshiro256* seed::uint64_t)
   ::void :static
   (let* ([mixstate::SplitMix64])
     (mix64-init (& mixstate) seed)
     (set! (aref (-> xos s) 0) (mix64 (next-seed (& mixstate))))
     (set! (aref (-> xos s) 1) (mix64 (next-seed (& mixstate))))
     (set! (aref (-> xos s) 2) (mix64 (next-seed (& mixstate))))
     (set! (aref (-> xos s) 3) (mix64 (next-seed (& mixstate))))))
 )

;; API
(define (make-xoshiro256 :key (seed 42) (private? #f))
  (make <xoshiro256> :seed seed :private? private?))

;; API
;;  This can be used to take a snapshot of RNG state.
(define-cproc copy-xoshiro256 (xos::<xoshiro256>)
  (let* ([new-xos::ScmXoshiro256* (SCM_NEW ScmXoshiro256)])
    (SCM_SET_CLASS new-xos (& ScmXoshiroClass))
    (SCM_INTERNAL_MUTEX_INIT (-> xos lock))
    (with-xos-lock xos
      (memcpy (-> new-xos s) (-> xos s) (sizeof (-> xos s)))
      (set! (-> new-xos seed) (-> xos seed)))
    (return (SCM_OBJ new-xos))))

;; API
;;  This can be used to restore RNG state.
(define-cproc copy-xoshiro256! (dst::<xoshiro256> src::<xoshiro256>) ::<void>
  (with-xos-lock dst
    (with-xos-lock src
      (memcpy (-> dst s) (-> src s) (sizeof (-> src s)))
      (set! (-> dst seed) (-> src seed)))))

;; API
(define-cproc xos-random-get-seed (xos::<xoshiro256>) ::<uint64>
  (return (-> xos seed)))

;; API
(define-cproc xos-random-set-seed! (xos::<xoshiro256> seed::<uint64>) ::<void>
  (with-xos-lock xos
    (set! (-> xos seed) seed)
    (xoshiro256-init xos seed)))

;; API
(define-cproc xos-random-u64 (xos::<xoshiro256>) ::<uint64>
  (let* ([r::uint64_t 0])
    (with-xos-lock xos (set! r (xoshiro256++ xos)))
    (return r)))

(inline-stub
 ;; Caller must hold the lock.
 (define-cfn get-real (xos::ScmXoshiro256* exclude0::_Bool) ::double :static
   (for ()
     (let* ([v::uint64_t (xoshiro256++ xos)]
            [d::double (* v (/ 1.0 18446744073709551616.0))])
       (unless (and exclude0 (== d 0.0))
         (return d)))))
 )

;; API
(define-cproc xos-random-real (xos::<xoshiro256>) ::<double>
  (let* ([r::double 0.0])
    (with-xos-lock xos (set! r (get-real xos TRUE)))
    (return r)))
(define-cproc xos-random-real0 (xos::<xoshiro256>) ::<double>
  (let* ([r::double 0.0])
    (with-xos-lock xos (set! r (get-real xos FALSE)))
    (return r)))
(define-cproc xos-random-fill-u64vector! (xos::<xoshiro256> v::<u64vector>)
  (with-xos-lock xos
    (dotimes (i (SCM_U64VECTOR_SIZE v))
      (set! (SCM_U64VECTOR_ELEMENT v i) (xoshiro256++ xos))))
  (return (SCM_OBJ v)))
(define-cproc xos-random-fill-f32vector! (xos::<xoshiro256> v::<f32vector>)
  (with-xos-lock xos
    (dotimes (i (SCM_F32VECTOR_SIZE v))
      (set! (SCM_F32VECTOR_ELEMENT v i) (cast float (get-real xos TRUE)))))
  (return (SCM_OBJ v)))
(define-cproc xos-random-fill-f64vector! (xos::<xoshiro256> v::<f64vector>)
  (with-xos-lock xos
    (dotimes (i (SCM_F64VECTOR_SIZE v))
      (set! (SCM_F64VECTOR_ELEMENT v i) (get-real xos TRUE))))
  (return (SCM_OBJ v)))
