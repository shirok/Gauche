;;;
;;; quoted-printable.scm - quoted-printable encoding/decoding routine
;;;
;;;  Copyright(C) 2001-2002 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: quoted-printable.scm,v 1.5 2002-09-16 09:01:33 shirok Exp $
;;;


;; Ref: RFC2045 section 6.7  <http://www.rfc-editor.org/rfc/rfc2045.txt>

(define-module rfc.quoted-printable
  (export quoted-printable-encode quoted-printable-encode-string
          quoted-printable-decode quoted-printable-decode-string)
  )
(select-module rfc.quoted-printable)

;; TODO: binary encoding
(define (quoted-printable-encode . args)
  (define binary? (get-keyword :binary? args #t))
  (let loop ((c (read-byte))
             (lcnt 0))
    (cond ((eof-object? c))
          ((>= lcnt 73) (display "=\r\n") (loop c 0)) ;soft newline
          ((= c #x3d) ; '='
           (display "=3D") (loop (read-byte) (+ lcnt 3)))
          ((and (>= lcnt 72)            ; space or tab at the end of line
                (or (= c #x20) (= c #x09)))
           (write-byte c) (display "=\r\n") (loop (read-byte) 0))
          ((= c #x0d)
           (let ((c1 (read-byte)))
             (cond ((= c1 #x0a) (display "\r\n") (loop (read-byte) 0))
                   (else (display "\r\n") (loop c1 0)))))
          ((= c #x0a)
           (display "\r\n") (loop (read-byte) 0))
          ((<= #x21 c #x7e)
           (write-byte c) (loop (read-byte) (+ lcnt 1)))
          (else (format #t "=~2,'0X" c) (loop (read-byte) (+ lcnt 3))))
    ))

(define (quoted-printable-encode-string string . args)
  (with-string-io string (lambda () (apply quoted-printable-encode args))))

(define (quoted-printable-decode)
  (let loop ((c (read-char)))
    (cond ((eof-object? c))
          ((char=? c #\=)
           (let ((c1 (read-char)))
             (cond
              ((eof-object? c1)) ; illegal, but we recognize it as a soft newline
              ((char=? c1 #\newline) (loop (read-char))) ; soft newline
              ((char=? c1 #\return)      ; soft newline
               (let ((c2 (read-char)))
                 (if (char=? c2 #\newline) (loop (read-char)) (loop c2))))
              ((memv c1 '(#\tab #\space)) ; possibly soft newline
               (let loop2 ((c2 (read-char))
                           (r (list c1 c)))
                 (cond ((eof-object? c2))
                       ((char=? c2 #\newline) (loop (read-char)))
                       ((char=? c2 #\return)
                        (let ((c3 (read-char)))
                          (if (char=? c3 #\newline)
                              (loop (read-char))
                              (loop c3))))
                       ((memv c2 '(#\tab #\space))
                        (loop2 (read-char) (cons c2 r)))
                       (else
                        (for-each write-char (reverse r))
                        (loop c2)))))
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
               (write-char c) (loop c1)))))
          (else
           (write-char c) (loop (read-char))))
    )
  )
             
(define (quoted-printable-decode-string string)
  (with-string-io string quoted-printable-decode))

(provide "rfc/quoted-printable")
