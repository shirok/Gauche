;;;
;;; quoted-printable.scm - quoted-printable encoding/decoding routine
;;;
;;;  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: quoted-printable.scm,v 1.2 2001-09-19 07:46:49 shirok Exp $
;;;


;; Ref: RFC2045 section 6.7  <http://www.rfc-editor.org/rfc/rfc2045.txt>

(define-module rfc.quoted-printable
  (export quoted-printable-encode quoted-printable-encode-string
          quoted-printable-decode quoted-printable-decode-string)
  )
(select-module rfc.quoted-printable)

;(define (quoted-printable-encode)
;  (let loop ((c (read-byte))
;             (lcnt 0))
;    (cond ((eof-object? c))
;          ((= c #x0d)

(define (quoted-printable-decode)
  (let loop ((c (read-char)))
    (cond ((eof-object? c))
          ((char=? c #\=)
           (let ((c1 (read-char)))
             ((eof-object? c1) (write-char c))
             ((char=? c1 #\newline) (loop (read-char))) ; soft newline
             ((char=? c1 #\return)      ; soft newline
              (let ((c2 (read-char)))
                (if (char=? c2 #\newline) (loop (read-char)) (loop c2))))
             ((digit->integer c1 16)
              => (lambda (num1)
                   (let ((c2 (read-char)))
                     (cond ((eof-object? c2) (write-char c) (write-char c1))
                           ((digit->integer c2 16)
                            => (lambda (num2)
                                 (write-byte (+ (* num1 16) num2))
                                 (loop (read-char))))
                           (else
                            (write-char c) (write-char c1) (loop c2))))))
             (else
              (write-char c) (loop c1))))
          (else
           (write-char c) (loop (read-char))))
    )
  )
             
(define (quoted-printable-decode-string string)
  (with-string-io string quoted-printable-decode))

(provide "rfc/quoted-printable")
