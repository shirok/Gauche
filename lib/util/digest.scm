;;;
;;; digest - abstract base class for message digest algorithms
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
;;;  $Id: digest.scm,v 1.1 2002-12-03 01:19:16 shirok Exp $
;;;

(define-module util.digest
  (export-all))

(select-module util.digest)

(define-class <message-digest-algorithm-meta> (<class>)
  ())

(define-class <message-digest-algorithm> ()
  ()
  :metaclass <message-digest-algorithm-meta>)

(define-method digest-update ((self <message-digest-algorithm>) data)
  #f)
(define-method digest-final ((self <message-digest-algorithm>))
  #f)
(define-method digest ((class <message-digest-algorithm-meta>))
  #f)
(define-method digest-string ((class <message-digest-algorithm-meta>) string)
  #f)

;; these are just examples, can be removed safely
(define (hexify string)
  (with-string-io string
    (lambda ()
      (let loop ((b (read-byte)))
	(unless (eof-object? b)
	  (format #t "~2,'0x" b)
	  (loop (read-byte)))))))

(define-method digest-hex ((class <message-digest-algorithm-meta>))
  (hexify (digest class)))

(define-method digest-string-hex ((class <message-digest-algorithm-meta>)
				  string)
  (hexify (digest-string class string)))
;; end

(provide "util/digest")
