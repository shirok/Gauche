;;;
;;; logical.scm - logical (bitwise) operations.  to be autoloaded.
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

(select-module gauche)

;; SLIB compatible interface.

(define (logtest . args)  ;; can be optimized for two-arg case
  (not (zero? (apply logand args))))

(define (logbit? index n)
  (not (zero? (logand n (ash 1 index)))))

(define (copy-bit index from bit)
  (if bit
    (logior (ash 1 index) from)
    (logand (lognot (ash 1 index)) from)))

(define (bit-field n start end)
  (check-arg integer? start)
  (check-arg integer? end)
  (if (< start end)
    (let1 mask (- (ash 1 (- end start)) 1)
      (logand (ash n (- start)) mask))
    0))

(define (copy-bit-field to from start end)
  (check-arg integer? start)
  (check-arg integer? end)
  (if (< start end)
    (let1 mask (- (ash 1 (- end start)) 1)
      (logior (logand to (lognot (ash mask start)))
              (ash (logand from mask) start)))
    from))
