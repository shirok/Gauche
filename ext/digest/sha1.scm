;;;
;;; sha1 - SHA-1 message-digest
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
;;;  $Id: sha1.scm,v 1.2 2002-12-07 00:23:41 shirok Exp $
;;;

;;; RFC 3174 US Secure Hash Algorithm 1 (SHA1)

(define-module rfc.sha1
  (extend util.digest)
  (export <sha1>
	  sha1-update! sha1-final! sha1-digest sha1-digest-string))

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

(define-method sha1-update! ((self <sha1>) data)
  (%sha1-update! (context-of self) data))

(define-method sha1-final! ((self <sha1>))
  (%sha1-final! (context-of self)))

(define (sha1-digest)
  (let ((sha1 (make <sha1-context>)))
    (port-for-each
     (lambda (b) (%sha1-update! sha1 b))
     (lambda () (read-block 4096)))
    (%sha1-final! sha1)))

(define (sha1-digest-string string)
  (with-input-from-string string sha1-digest))

(define-method digest-update! ((self <sha1>) data)
  (sha1-update! self data))
(define-method digest-final! ((self <sha1>))
  (sha1-final! self))
(define-method digest ((class <sha1-meta>))
  (sha1-digest))
(define-method digest-string ((class <sha1-meta>) string)
  (sha1-digest-string string))

(provide "rfc/sha1")
