;;;
;;; hmac - HMAC keyed-hashing
;;;
;;;  Copyright(C) 2002 by Kimura Fuyuki (fuyuki@hadaly.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: hmac.scm,v 1.1 2002-12-03 01:19:16 shirok Exp $
;;;

(define-module rfc.hmac
  (use util.digest)
  (use gauche.uvector)
  (export-all))

(select-module rfc.hmac)

;; temporary defined here
(define (make-byte-string length byte)
  (u8vector->string (make-u8vector length byte)))

(define-class <hmac> ()
  ((key :getter key-of)
   (hasher :getter hasher-of)))

(define-method initialize ((self <hmac>) initargs)
  (next-method)
  (let-keywords* initargs ((key #f)
			   (hasher #f)
			   (block-size 64))
    (unless (and key hasher)
      (error "key and hasher must be given"))
    (if (> (string-size key) block-size)
	(set! key (digest-string hasher key)))
    (slot-set! self 'key
	       (string-append key
			      (make-byte-string (- block-size
						   (string-size key))
						#x0)))
    (slot-set! self 'hasher (make hasher))
    (let* ((v (string->u8vector (key-of self)))
	   (ipad (u8vector->string (u8vector-xor v #x36))))
      (digest-update (hasher-of self) ipad))))

(define-method hmac-update ((self <hmac>) data)
  (digest-update (hasher-of self) data))

(define-method hmac-final ((self <hmac>))
  (let* ((v (string->u8vector (key-of self)))
	 (opad (u8vector->string (u8vector-xor v #x5c)))
	 (inner (digest-final (hasher-of self)))
	 (outer (digest-string (class-of (hasher-of self))
			       (string-append opad inner))))
    outer))

(define (hmac-digest . args)
  (let ((hmac (apply make <hmac> args)))
    (let loop ((b (read-block 8192)))
      (unless (eof-object? b)
	(hmac-update hmac b)
	(loop (read-block 8192))))
    (hmac-final hmac)))

(define (hmac-digest-string string . args)
  (with-input-from-string string
    (lambda() (apply hmac-digest args))))

(provide "rfc/hmac")
