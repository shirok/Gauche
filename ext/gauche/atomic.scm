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
  (export make-atomic-box atomic-box?
          atomic-box-ref atomic-box-set!
          atomic-box-swap! atomic-box-compare-and-swap!

          make-atomic-flag atomic-flag?
          atomic-flag-test-and-set! atomic-flag-clear!)
  )
(select-module gauche.atomic)

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
