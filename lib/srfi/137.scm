;;;
;;; SRFI-137 Minimal Unique Types
;;;
;;;   Copyright (c) 2025  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi.137
  (export make-type))
(select-module srfi.137)

(define-class <srfi-137-class-meta> (<class>) ())

(define (%make-type type-payload super)
  (define name (string->symbol (format "[~,,,,30:a]" type-payload)))
  (define type-slot (gensym #"type-~name"))
  (define instance-slot (gensym #"instance-~name"))
  (define class
    (make <srfi-137-class-meta>
      :name name
      :supers (list super)
      :slots `((,type-slot :init-value ,type-payload)
               (,instance-slot))))
  (define (type-accessor) type-payload)
  (define (constructor instance-payload)
    (rlet1 instance (make class)
      (set! (slot-ref instance instance-slot) instance-payload)))
  (define (predicate obj) (is-a? obj class))
  (define (accessor obj)
    (assume (is-a? obj class))
    (slot-ref obj instance-slot))
  (define (make-subtype type-payload)
    (%make-type type-payload class))
  (values type-accessor
          constructor
          predicate
          accessor
          make-subtype))

;; API
(define (make-type type-payload)
  (%make-type type-payload <object>))
