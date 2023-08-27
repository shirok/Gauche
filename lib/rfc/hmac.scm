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
  (use gauche.mop.typed-slot)
  (export hmac-to hmac-message-to hmac-message hmac-verify

          ;; Deprecated
          <hmac> hmac-update! hmac-final!
          hmac-digest hmac-digest-string))
(select-module rfc.hmac)

(autoload gauche.vport open-input-uvector)

;; User API
(define (hmac-to target algorithm key)
  (%hmac-digest-to target :key key :hasher algorithm))

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
;;
;; Internal API
;;   We export these for historical reasons, but the user doesn't
;;   need to care them.

(define-class <hmac> ()
  ;; NB: Slots are set in the specialized initialize method instead of
  ;; init-keywords.
  ((key    :immutable #t
           :type <string>)
   (hasher :immutable #t
           :type <message-digest-algorithm>))
  :metaclass <typed-slot-meta>)

(define-method initialize ((self <hmac>) initargs)
  (next-method)
  (let-keywords initargs ([key #f]
                          [hasher #f]
                          [block-size #f])
    (unless (and key hasher)
      (error "key and hasher must be given"))
    (let1 block-size (or block-size (~ hasher 'hmac-block-size))
      (when (> (string-size key) block-size)
        (set! key (digest-string hasher key)))
      (set! (~ self'key)
            (string-append key
                           (make-byte-string (- block-size
                                                (string-size key))
                                             #x0)))
      (set! (~ self'hasher) (make hasher))
      (let* ([v (string->u8vector (~ self'key))]
             [ipad (u8vector->string (u8vector-xor v #x36))])
        (digest-update! (~ self'hasher) ipad)))))

(define-method hmac-update! ((self <hmac>) data)
  (digest-update! (~ self'hasher) data))

(define-method hmac-final! ((self <hmac>) :optional (target <string>))
  (let* ([v (string->u8vector (~ self'key))]
         [opad (u8vector->string (u8vector-xor v #x5c))]
         [inner (digest-final! (~ self'hasher))]
         [outer (digest-message-to target
                                   (class-of (~ self'hasher))
                                   (string-append opad inner))])
    outer))

(define (%hmac-digest-to target . args)
  (let1 hmac (apply make <hmac> args)
    (generator-for-each
     (cut hmac-update! hmac <>)
     (cut read-block 4096))
    (hmac-final! hmac target)))

(define (hmac-digest . args) (apply %hmac-digest-to <string> args))
(define (hmac-digest-string string . args)
  (with-input-from-string string (cut apply hmac-digest args)))
