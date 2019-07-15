;;;
;;; gauche/mop/validator.scm - validator slot option
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

(define-module gauche.mop.validator
  (export <validator-meta> <validator-mixin>)
  )
(select-module gauche.mop.validator)

(define-class <validator-meta> (<class>)
  ())

(define-method compute-get-n-set ((class <validator-meta>) slot)
  (let ([pre  (slot-definition-option slot :validator #f)]
        [post (slot-definition-option slot :observer #f)])
    (if (or pre post)
      (let* ([acc (compute-slot-accessor class slot (next-method))]
             [getter (^o (slot-ref-using-accessor o acc))]
             [bound? (^o (slot-bound-using-accessor? o acc))]
             [setter (cond [(and pre post)
                            (^[o v]
                              (slot-set-using-accessor! o acc (pre o v))
                              (post o (slot-ref-using-accessor o acc)))]
                           [pre
                            (^[o v]
                              (slot-set-using-accessor! o acc (pre o v)))]
                           [else
                            (^[o v]
                              (slot-set-using-accessor! o acc v)
                              (post o (slot-ref-using-accessor o acc)))])])
        ;; the last #t enables initialization by :initform etc.
        (list getter setter bound? #t))
      (next-method))))

;; convenience base class.  you can either inherit <validator-mixin>,
;; or specifying :metaclass <validator-meta> to your class.
(define-class <validator-mixin> ()
  ()
  :metaclass <validator-meta>)

