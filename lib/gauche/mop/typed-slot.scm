;;;
;;; gauche.mop.typed-slot - Slot type annotation
;;;
;;;   Copyright (c) 2023-2024  Shiro Kawai  <shiro@acm.org>
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

;; EXPERIMENTAL: This metaclass adds :type slot definition option.

(define-module gauche.mop.typed-slot
  (use util.match)
  (export <typed-slot-meta> <typed-slot-mixin>)
  )
(select-module gauche.mop.typed-slot)

(define-class <typed-slot-meta> (<class>) ())

(define-class <typed-slot-mixin> () () :metaclass <typed-slot-meta>)

(define-method compute-slot-accessor ((class <typed-slot-meta>) slotdef gns)
  (let1 accessor (next-method)
    (if-let1 type (slot-definition-option slotdef :type #f)
      (make-type-checked-accessor class slotdef accessor type)
      gns)))

(define (make-type-checked-accessor class slotdef accessor type)
  (define (check v)
    (unless (of-type? v type)
      (errorf "Slot ~a of ~a must be of type ~a, but got: ~s"
              (slot-definition-name slotdef)
              (class-name class) (~ type 'name) v)))
  ;; TODO: Probably we should have a built-in utility to create a
  ;; 'wrapper' accessor.
  (make <slot-accessor>
    :class class :name (slot-definition-name slotdef)
    :getter (^o (slot-ref-using-accessor o accessor))
    :setter (^[o v] (check v) (slot-set-using-accessor! o accessor v))
    :bound? (^o (slot-bound-using-accessor? o accessor))
    :initializable (~ accessor'initializable)
    :init-value (if (slot-bound? accessor 'init-value)
                  (~ accessor'init-value)
                  (undefined))
    :init-keyword (if (slot-bound? accessor 'init-keyword)
                    (~ accessor'init-keyword)
                    (undefined))
    :init-thunk (if (slot-bound? accessor 'init-thunk)
                  (~ accessor'init-thunk)
                  (undefined))
    :immutable (~ accessor'immutable)
    ))
