;;;
;;; srfi-14/query - character set (querying contents)
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

;; Say `(use srfi-14)' and this file is autoloaded on demand.

(select-module srfi-14)

(define (char-set-count pred cs)
  (char-set-fold (lambda (c r) (if (pred c) (+ r 1) r)) 0 cs))

(define (char-set->list cs)
  (char-set-fold cons '() cs))

(define (char-set->string cs)
  (list->string (char-set-fold cons '() cs)))

;; char-set-contains? : native

(define (char-set-every pred cs)
  (let loop ((cursor (char-set-cursor cs)))
    (cond ((end-of-char-set? cursor))
          ((pred (char-set-ref cs cursor))
           (loop (char-set-cursor-next cs cursor)))
          (else #f))))

(define (char-set-any pred cs)
  (let loop ((cursor (char-set-cursor cs)))
    (cond ((end-of-char-set? cursor) #f)
          ((pred (char-set-ref cs cursor)) #t)
          (else (loop (char-set-cursor-next cs cursor))))))

