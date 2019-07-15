;;;
;;; gauche.mop.bound-slot - ensure slots are bound after initialization
;;;
;;;   Copyright (c) 2016-2019  Shiro Kawai  <shiro@acm.org>
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

;; EXPERIMENTAL: This is a helper class to catch trivial mistakes
;; that some slots are inadvertently left uninitialized---it can happen
;; when you update class definition but forget to change one of 'make'
;; calls, or misspelled initialization keyword arguments, etc.
;; The question is whether we should provide such a feature through
;; mixin class---it doesn't really add any new feature to the class,
;; so it seems out-of-place in the superclass list in the class definition.
;; Some sort of annotation or declaration would fit the bill, probably.

(define-module gauche.mop.bound-slot
  (export <bound-slot-meta> <bound-slot-mixin>)
  )
(select-module gauche.mop.bound-slot)

(define-class <bound-slot-meta> (<class>) ())

(define-class <bound-slot-mixin> () () :metaclass <bound-slot-meta>)

(define-method make ((class <bound-slot-meta>) . args)
  (rlet1 instance (next-method)
    (dolist [s (class-slots class)]
      (unless (slot-bound? instance (slot-definition-name s))
        (errorf "Slot `~s' of ~s isn't initialized."
                (slot-definition-name s) instance)))))
