;;;
;;; sha1 - SHA-1 message-digest
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
;;;  $Id: sha1.scm,v 1.6 2004-12-15 12:46:06 shirok Exp $
;;;

;;; RFC 3174 US Secure Hash Algorithm 1 (SHA1)

(define-module rfc.sha1
  (use gauche.uvector)
  (extend util.digest)
  (export <sha1> sha1-digest sha1-digest-string)
  )
(select-module rfc.sha1)
(dynamic-load "sha1")

(define-class <sha1-meta> (<message-digest-algorithm-meta>)
  ())

(define-class <sha1> (<message-digest-algorithm>)
  ((context :getter context-of))
  :metaclass <sha1-meta>)

(define-method initialize ((self <sha1>) initargs)
  (next-method)
  (slot-set! self 'context (make <sha1-context>)))

;(define (sha1-digest)
;  (let ((sha1 (make <sha1-context>)))
;    (port-for-each
;     (lambda (b) (%sha1-update sha1 b))
;     (lambda () (read-block 4096)))
;    (%sha1-final sha1)))

(define-constant *sha1-unit-len* 4096)

(define (sha1-digest)
  (let ((sha1 (make <sha1-context>))
        (buf (make-u8vector *sha1-unit-len*)))
    (port-for-each
     (lambda (x) (%sha1-update sha1 x))
     (lambda ()
       (let1 count (read-block! buf)
         (if (eof-object? count)
           count
           (if (< count *sha1-unit-len*)
             (uvector-alias <u8vector> buf 0 count)
             buf)))))
    (%sha1-final sha1)))

(define (sha1-digest-string string)
  (with-input-from-string string sha1-digest))

;; digest framework
(define-method digest-update! ((self <sha1>) data)
  (%sha1-update (context-of self) data))
(define-method digest-final! ((self <sha1>))
  (%sha1-final (context-of self)))
(define-method digest ((class <sha1-meta>))
  (sha1-digest))

(provide "rfc/sha1")
