;;;
;;; quoted-printable.scm - quoted-printable encoding/decoding routine
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
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


;; Ref: RFC2045 section 6.7  <http://www.rfc-editor.org/rfc/rfc2045.txt>

(define-module rfc.quoted-printable
  (export quoted-printable-encode quoted-printable-encode-string
          quoted-printable-decode quoted-printable-decode-string)
  )
(select-module rfc.quoted-printable)

;; The minimum line width is 4, since one encoded octed and one soft
;; line break requires 4 characters.
;; If binary is #f, we encode CR and LF.  See RFC2045 for this consideration.
(define (quoted-printable-encode :key (line-width 76) (binary #f))
  (let1 limit (if (and line-width (>= line-width 4)) (- line-width 3) #f)
    (let loop ([c (read-byte)]
               [lcnt 0])
      (cond [(eof-object? c)]
            [(and limit (>= lcnt limit)) (display "=\r\n") (loop c 0)]
            [(and limit (>= lcnt limit) (or (= c #x20) (= c #x09)))
             ;; space or tab at the end of line
             (write-byte c) (display "=\r\n") (loop (read-byte) 0)]
            [(and binary (or (= c #x0a) (= c #x0d)))
             (format #t "=0~X" c) (loop (read-byte) (+ lcnt 1))]
            [(= c #x0d)
             (let1 c1 (read-byte)
               (cond ((= c1 #x0a) (display "\r\n") (loop (read-byte) 0))
                     (else (display "\r\n") (loop c1 0))))]
            [(= c #x0a)
             (display "\r\n") (loop (read-byte) 0)]
            ;; NB: we escape '?' as well, for it interferes the header
            ;; field encoding defined in RFC2047.
            [(or (< #x20 c #x3d) (= c #x3e) (< #x3f c #x7f))
             (write-byte c) (loop (read-byte) (+ lcnt 1))]
            [else
             (format #t "=~2,'0X" c) (loop (read-byte) (+ lcnt 3))]))
    ))

(define (quoted-printable-encode-string string . args)
  (with-string-io string (cut apply quoted-printable-encode args)))

(define (quoted-printable-decode)
  (let loop ([c (read-char)])
    (cond [(eof-object? c)]
          [(char=? c #\=)
           (let1 c1 (read-char)
             (cond
              [(eof-object? c1)] ; illegal, but we recognize it as a soft newline
              [(char=? c1 #\newline) (loop (read-char))] ; soft newline
              [(char=? c1 #\return)      ; soft newline
               (let1 c2 (read-char)
                 (if (char=? c2 #\newline) (loop (read-char)) (loop c2)))]
              [(memv c1 '(#\tab #\space)) ; possibly soft newline
               (let loop2 ([c2 (read-char)]
                           [r (list c1 c)])
                 (cond [(eof-object? c2)]
                       [(char=? c2 #\newline) (loop (read-char))]
                       [(char=? c2 #\return)
                        (let1 c3 (read-char)
                          (if (char=? c3 #\newline)
                            (loop (read-char))
                            (loop c3)))]
                       [(memv c2 '(#\tab #\space))
                        (loop2 (read-char) (cons c2 r))]
                       [else
                        (for-each write-char (reverse r))
                        (loop c2)]))]
              [(digit->integer c1 16)
               => (^[num1]
                    (let1 c2 (read-char)
                      (cond [(eof-object? c2) (write-char c) (write-char c1)]
                            [(digit->integer c2 16)
                             => (^[num2]
                                  (write-byte (+ (* num1 16) num2))
                                  (loop (read-char)))]
                            [else (write-char c) (write-char c1) (loop c2)])))]
              [else (write-char c) (loop c1)]))]
          [else (write-char c) (loop (read-char))])
    ))

(define (quoted-printable-decode-string string)
  (with-string-io string quoted-printable-decode))

