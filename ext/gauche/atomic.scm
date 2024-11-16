;;;
;;; gauche.atomic - Atomic operations
;;;
;;;   Copyright (c) 2024  Shiro Kawai  <shiro@acm.org>
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

;; SRFI-230 atomic operations.

;; Ideally, atomic operations should be inline-expanded, for their
;; main reason is for performance. For now, we provide procedural API
;; so that we can run code written with srfi-230.

(define-module gauche.atomic
  (use gauche.threads)
  (export make-atomic-box atomic-box?
          atomic-box-ref atomic-box-set!
          atomic-box-swap! atomic-box-compare-and-swap!

          make-atomic-flag atomic-flag?
          atomic-flag-test-and-set! atomic-flag-clear!

          make-atomic-fxbox atomic-fxbox?
          atomic-fxbox-ref atomic-fxbox-set!
          atomic-fxbox-swap! atomic-fxbox-compare-and-swap!
          atomic-fxbox+/fetch! atomic-fxbox-/fetch!
          atomic-fxbox-and/fetch!
          atomic-fxbox-ior/fetch! atomic-fxbox-xor/fetch!

          make-atomic-pair atomic-pair?
          atomic-pair-ref atomic-pair-set!
          atomic-pair-swap! atomic-pair-compare-and-swap!)
  )
(select-module gauche.atomic)

;;;
;;; Atomic flag
;;;
(inline-stub
 (declare-stub-type <atomic-flag> "ScmAtomicBox*" "atomic flag")

 (define-cproc make-atomic-flag ()
   (return (SCM_OBJ (Scm_MakeAtomicBox SCM_CLASS_ATOMIC_FLAG SCM_FALSE))))
 (define-cproc atomic-flag? (obj) ::<boolean>
   (return (SCM_XTYPEP obj SCM_CLASS_ATOMIC_FLAG)))
 (define-cproc atomic-flag-test-and-set! (atomic-flag::<atomic-flag>
                                          :optional _)
   (return (Scm_AtomicBoxSwap atomic-flag SCM_TRUE)))
 (define-cproc atomic-flag-clear! (atomic-flag::<atomic-flag>
                                   :optional _)
   ::<void>
   (Scm_AtomicBoxSet atomic-flag SCM_FALSE))
 )

;;;
;;; Atomic box (Scheme)
;;;
(inline-stub
 (declare-stub-type <atomic-box> "ScmAtomicBox*" "atomic box")

 (define-cproc make-atomic-box (obj)
   (return (SCM_OBJ (Scm_MakeAtomicBox SCM_CLASS_ATOMIC_BOX obj))))
 (define-cproc atomic-box? (obj) ::<boolean>
   (return (SCM_XTYPEP obj SCM_CLASS_ATOMIC_BOX)))
 (define-cproc atomic-box-ref (atomic-box::<atomic-box>
                               :optional _) ;; memory-order
   (return (Scm_AtomicBoxRef atomic-box)))
 (define-cproc atomic-box-set! (atomic-box::<atomic-box>
                                obj
                                :optional _) ;; memory-order
   ::<void>
   (Scm_AtomicBoxSet atomic-box obj))
 (define-cproc atomic-box-swap! (atomic-box::<atomic-box>
                                 obj
                                 :optional _) ;; memory-order
   (return (Scm_AtomicBoxSwap atomic-box obj)))
 (define-cproc atomic-box-compare-and-swap! (atomic-box::<atomic-box>
                                             expected
                                             desired
                                             :optional _) ;; memory-order
   (return (Scm_AtomicBoxCompareAndSwap atomic-box expected desired)))
 )

;;;
;;; Atomic fxbox
;;;
;;;  We define fetch-and-modify operations in Scheme, for
;;;  it's cumbersome to use atomicP primitives from extension.

(inline-stub
 (declare-stub-type <atomic-fxbox> "ScmAtomicBox*" "atomic fxbox")

 (define-cproc make-atomic-fxbox (obj::<fixnum>)
   (return (SCM_OBJ (Scm_MakeAtomicBox SCM_CLASS_ATOMIC_FXBOX
                                       (SCM_MAKE_INT obj)))))
 (define-cproc atomic-fxbox? (obj) ::<boolean>
   (return (SCM_XTYPEP obj SCM_CLASS_ATOMIC_FXBOX)))
 (define-cproc atomic-fxbox-ref (atomic-fxbox::<atomic-fxbox>
                                 :optional _)
   (return (Scm_AtomicBoxRef atomic-fxbox)))
 (define-cproc atomic-fxbox-set! (atomic-fxbox::<atomic-fxbox>
                                  obj::<fixnum>
                                  :optional _) ;; memory-order
   ::<void>
   (Scm_AtomicBoxSet atomic-fxbox (SCM_MAKE_INT obj)))
 (define-cproc atomic-fxbox-swap! (atomic-fxbox::<atomic-fxbox>
                                   obj::<fixnum>
                                   :optional _) ;; memory-order
   (return (Scm_AtomicBoxSwap atomic-fxbox (SCM_MAKE_INT obj))))
 (define-cproc atomic-fxbox-compare-and-swap! (atomic-fxbox::<atomic-fxbox>
                                               expected::<fixnum>
                                               desired::<fixnum>
                                               :optional _) ;; memory-order
   (return (Scm_AtomicBoxCompareAndSwap atomic-fxbox
                                        (SCM_MAKE_INT expected)
                                        (SCM_MAKE_INT desired))))
 )

(define-syntax define-atomic-fxbox-fetch-and-modify
  (er-macro-transformer
   (^[f r c]
     (let ([name (cadr f)]
           [op (caddr f)])
       (quasirename r
         `(define (,name fxbox val :optional _)
            (let retry ()
              (let* ([prev (atomic-fxbox-ref fxbox)]
                     [r (atomic-fxbox-compare-and-swap! fxbox
                                                        prev
                                                        (,op prev val))])
                (cond [(eqv? prev r) r]
                      [else (thread-yield!) (retry)])))))))))

(define-atomic-fxbox-fetch-and-modify atomic-fxbox+/fetch! +)
(define-atomic-fxbox-fetch-and-modify atomic-fxbox-/fetch! -)
(define-atomic-fxbox-fetch-and-modify atomic-fxbox-and/fetch! logand)
(define-atomic-fxbox-fetch-and-modify atomic-fxbox-ior/fetch! logior)
(define-atomic-fxbox-fetch-and-modify atomic-fxbox-xor/fetch! logxor)

;;;
;;; Atomic pairs
;;;

(inline-stub
 (declare-stub-type <atomic-pair> "ScmAtomicBox*" "atomic pair")

 (define-cproc make-atomic-pair (ca cd)
   (return (SCM_OBJ (Scm_MakeAtomicBox SCM_CLASS_ATOMIC_PAIR
                                       (Scm_Cons ca cd)))))
 (define-cproc atomic-pair? (obj) ::<boolean>
   (return (SCM_XTYPEP obj SCM_CLASS_ATOMIC_PAIR)))
 (define-cproc atomic-pair-ref (pair::<atomic-pair> :optional _)
   ::(<top> <top>)
   (let* ((p (Scm_AtomicBoxRef pair)))
     (SCM_ASSERT (SCM_PAIRP p))
     (return (SCM_CAR p) (SCM_CDR p))))
 (define-cproc atomic-pair-set! (pair::<atomic-pair> ca cd :optional _)
   ::<void>
   (Scm_AtomicBoxSet pair (Scm_Cons ca cd)))
 (define-cproc atomic-pair-swap! (pair::<atomic-pair> ca cd :optional _)
   ::(<top> <top>)
   (let* ((p (Scm_AtomicBoxSwap pair (Scm_Cons ca cd))))
     (SCM_ASSERT (SCM_PAIRP p))
     (return (SCM_CAR p) (SCM_CDR p))))
 (define-cproc atomic-pair-compare-and-swap! (pair::<atomic-pair>
                                              expected-car
                                              expected-cdr
                                              desired-car
                                              desired-cdr)
   ::(<top> <top>)
   (let* ([newp (Scm_Cons desired-car desired-cdr)])
     (loop
      (let* ([p (Scm_AtomicBoxRef pair)])
        (SCM_ASSERT (SCM_PAIRP p))
        (let* ([prev-car (SCM_CAR p)]
               [prev-cdr (SCM_CDR p)])
          (if (and (SCM_EQ prev-car expected-car)
                   (SCM_EQ prev-cdr expected-cdr))
            (let* ([p2 (Scm_AtomicBoxCompareAndSwap pair p newp)])
              (when (SCM_EQ p p2)
                (return prev-car prev-cdr)) ;success
              (Scm_YieldCPU))
            (return prev-car prev-cdr)))))))
 )
