;;;
;;; hmac - HMAC keyed-hashing
;;;
;;;   Copyright (c) 2002-2003 Kimura Fuyuki, All rights reserved.
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

;;; RFC 2104 HMAC: Keyed-Hashing for Message Authentication

(define-module rfc.hmac
  (use util.digest)
  (use gauche.uvector)
  (export <hmac> hmac-update! hmac-final! hmac-digest hmac-digest-string))

(select-module rfc.hmac)

(define-class <hmac> ()
  ((key :getter key-of)
   (hasher :getter hasher-of)))

(define-method initialize ((self <hmac>) initargs)
  (next-method)
  (let-keywords initargs ([key #f]
                          [hasher #f]
                          [block-size #f])
    (unless (and key hasher)
      (error "key and hasher must be given"))
    (let1 block-size (or block-size (slot-ref hasher 'hmac-block-size))
      (when (> (string-size key) block-size)
        (set! key (digest-string hasher key)))
      (slot-set! self 'key
                 (string-append key
                                (make-byte-string (- block-size
                                                     (string-size key))
                                                  #x0)))
      (slot-set! self 'hasher (make hasher))
      (let* ([v (string->u8vector (key-of self))]
             [ipad (u8vector->string (u8vector-xor v #x36))])
        (digest-update! (hasher-of self) ipad)))))

(define-method hmac-update! ((self <hmac>) data)
  (digest-update! (hasher-of self) data))

(define-method hmac-final! ((self <hmac>))
  (let* ((v (string->u8vector (key-of self)))
         (opad (u8vector->string (u8vector-xor v #x5c)))
         (inner (digest-final! (hasher-of self)))
         (outer (digest-string (class-of (hasher-of self))
                               (string-append opad inner))))
    outer))

(define (hmac-digest . args)
  (let1 hmac (apply make <hmac> args)
    (generator-for-each
     (cut hmac-update! hmac <>)
     (cut read-block 4096))
    (hmac-final! hmac)))

(define (hmac-digest-string string . args)
  (with-input-from-string string
    (cut apply hmac-digest args)))
