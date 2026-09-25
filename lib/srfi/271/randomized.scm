;;;
;;; SRFI-271 - random ports / randomized
;;;
;;;   Copyright (c) 2026  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi.271.randomized
  (use gauche.vport)
  (use gauche.uvector)
  (export make-random-port)
  )
(select-module srfi.271.randomized)

(define (make-random-port . _)
  (cond-expand
   [gauche.os.windows
    (let ([buf 0]
          [cnt 0])
      (make <virtual-input-port>
        :getb (^[]
                (when (zero? cnt)
                  (set! buf ((with-module gauche.internal sys-win-get-random))))
                (rlet1 b (logand buf #xff)
                  (set! buf (ash buf -8))
                  (set! cnt (modulo (+ cnt 1) 8))))))]
   [else
    ;; We avoid holding the device file open.
    (make <buffered-input-port>
      :fill (^[buf]
              (call-with-input-file "/dev/urandom"
                (^p (dotimes [i (u8vector-length buf)]
                      (u8vector-set! buf i (read-u8 p)))))
              (u8vector-length buf)))]))
