;;;
;;; md5 - MD5 message-digest
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
;;;  $Id: md5.scm,v 1.1 2002-12-03 01:19:16 shirok Exp $
;;;

(define-module rfc.md5
  (extend util.digest)
  (export-all))

(select-module rfc.md5)
(dynamic-load "md5")

(define-class <md5-meta> (<message-digest-algorithm-meta>)
  ())

(define-class <md5> (<message-digest-algorithm>)
  ((context :getter context-of))
  :metaclass <md5-meta>)

(define-method initialize ((self <md5>) initargs)
  (next-method)
  (slot-set! self 'context (make <md5-context>)))

(define-method md5-update ((self <md5>) data)
  (%md5-update (context-of self) data))

(define-method md5-final ((self <md5>))
  (%md5-final (context-of self)))

(define (md5-digest)
  (let ((md5 (make <md5-context>)))
    (let loop ((b (read-block 8192)))
      (unless (eof-object? b)
	(%md5-update md5 b)
	(loop (read-block 8192))))
    (%md5-final md5)))

(define (md5-digest-string string)
  (with-input-from-string string md5-digest))

(define-method digest-update ((self <md5>) data)
  (md5-update self data))
(define-method digest-final ((self <md5>))
  (md5-final self))
(define-method digest ((class <md5-meta>))
  (md5-digest))
(define-method digest-string ((class <md5-meta>) string)
  (md5-digest-string string))

(provide "rfc/md5")
