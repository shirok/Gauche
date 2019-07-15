;;;
;;; srfi-124 - Ephemerons
;;;
;;;   Copyright (c) 2019  Shiro Kawai  <shiro@acm.org>
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

;; NB: This is an *incomplete* emulation of ephemerons.  Notably,
;; if a key has a strong reference from its datum, the key won't be
;; collected even if it is not strongly referenced from elsewhere.
;; The complete implementation would require to cooperate closely with GC.

(define-module srfi-124
  (use gauche.record)
  (export ephemeron? make-ephemeron ephemeron-broken?
          ephemeron-key ephemeron-datum
          reference-barrier))
(select-module srfi-124)

(define-record-type <ephemeron> %make-ephemeron ephemeron?
  (key-box %ephemeron-key-box)
  (datum   ephemeron-datum %ephemeron-datum-set!))

(define *broken-mark* (list 'broken))

(define (make-ephemeron key datum)
  (let1 v (make-weak-vector 1)
    (weak-vector-set! v 0 key)
    (%make-ephemeron v datum)))

(define (ephemeron-broken? e)
  (let1 k (weak-vector-ref (%ephemeron-key-box e) 0 *broken-mark*)
    (and (eq? k *broken-mark*)
         (begin (%ephemeron-datum-set! e #f) ; remove ref to datum
                #t))))

(define (ephemeron-key e)
  (weak-vector-ref (%ephemeron-key-box e) 0 #f))

(define (reference-barrier key) #t)





