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
  (use gauche.sequence)
  (use gauche.uvector)
  (use gauche.mop.typed-slot)
  (export hmac-to hmac-message-to hmac-message hmac-verify

          ;; Low-level API
          make-hmac <hmac> hmac-update! hmac-final!

          ;; Deprecated
          hmac-digest hmac-digest-string))
(select-module rfc.hmac)

(autoload gauche.vport open-input-uvector)

;; User API
(define (hmac-to target algorithm key)
  (let1 hmac (make-hmac algorithm key)
    (generator-for-each
     (cut hmac-update! hmac <>)
     (cut read-block 4096))
    (hmac-final! hmac target)))

;; User API
(define (hmac-message-to target algorithm key message)
  (define (thunk) (hmac-to target algorithm key))
  (etypecase message
    [<string> (with-input-from-string message thunk)]
    [<u8vector> (with-input-from-port (open-input-uvector message) thunk)]))

;; User API
(define (hmac-message algorithm key message)
  (hmac-message-to <u8vector> algorithm key message))

;; User API
(define (hmac-verify algorithm given-digest key message)
  (assume-type given-digest <u8vector>)
  ;; We compare two u8vectors in constant time
  (let* ([computed-digest (hmac-message algorithm key message)]
         [given-len (u8vector-length given-digest)]
         [computed-len (u8vector-length computed-digest)]
         [len (min given-len computed-len)])
    (let loop ([i 0] [ok #t])
      (if (= i len)
        (and ok (= given-len computed-len))
        (loop (+ i 1)
              (and (= (u8vector-ref given-digest i)
                      (u8vector-ref computed-digest i))
                   ok))))))

;; Low-level API

(define-class <hmac> ()
  ;; All slots are private
  ((key    :immutable #t :init-keyword :key)
   (hasher :immutable #t :init-keyword :hasher)))

(define (make-hmac algorithm key :optional (block-size #f))
  (assume-type key <string>)
  (assume-type algorithm <message-digest-algorithm-meta>)
  (let1 block-size (or block-size (~ algorithm'hmac-block-size))
    (when (> (string-size key) block-size)
      (set! key (digest-message-to <string> algorithm key)))
    (rlet1 hmac (make <hmac>
                  :key (string-append key
                                      (make-byte-string (- block-size
                                                           (string-size key))
                                                        #x0))
                  :hasher (make algorithm))
      (let* ([v (string->u8vector (~ hmac'key))]
             [ipad (u8vector->string (u8vector-xor v #x36))])
        (digest-update! (~ hmac'hasher) ipad)))))

(define-method hmac-update! ((hmac <hmac>) data)
  (digest-update! (~ hmac'hasher) data))

(define-method hmac-final! ((hmac <hmac>) :optional (target <string>))
  (let* ([v (string->u8vector (~ hmac'key))]
         [opad (u8vector->string (u8vector-xor v #x5c))]
         [inner (digest-final! (~ hmac'hasher))]
         [outer (digest-message-to target
                                   (class-of (~ hmac'hasher))
                                   (string-append opad inner))])
    outer))

;; Deprecated
(define (hmac-digest :key key hasher)
  (hmac-to <string> hasher key))
;; Deprecated
(define (hmac-digest-string string . args)
  (with-input-from-string string (cut apply hmac-digest args)))
