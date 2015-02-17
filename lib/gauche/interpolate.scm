;;;
;;; interpolate.scm - string interpolation; to be autoloaded
;;;
;;;   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

;; Legacy syntax:
;;  #`"The value is ,(foo)." => (string-append "The value is " (foo) ".")
;;
;; New syntax:
;;  #"The value is ~(foo)." => (string-append "The value is " (foo) ".")
;;
;; We use 'read' to get expression after reading an unquote character
;; (#\, for the legacy syntax, #\~ for the new syntax.)  This presents
;; a problem if the expression is an indentfier, immediately followed by
;; a non-delimiting character.  Suppose we have #"It's ~foo."  After
;; reading #\~, we call 'read', which reads an identifier named "foo.",
;; but the original intention might be an identifier "foo", followed by
;; a literal ".".
;;
;; An easy solution, which we currently use, is to delimit the symbol
;; by vertical bars: #"It's ~|foo|."   It is logical---the 'read' procedure
;; just reads "|foo|" as a symbol foo, regadless of what's following---but
;; I don't like it, for it looks visually as if we have a special syntax
;; for the single-variable case, even though we don't.
;;
;; One idea is to use curly-braces: ~{foo}.  Instead of having it a 'special'
;; syntax, we can interpret it as a srfi-105 (curly-infix) syntax, in which
;; {expr} is just expr.  If we support srfi-105 by default, this feature comes
;; for free.  We don't know yet, though.

(define-module gauche.interpolate
  (export string-interpolate)
  )
(select-module gauche.interpolate)

(define (string-interpolate str :optional (legacy? #f))
  (if (string? str)
    (%string-interpolate str (if legacy? #\, #\~))
    (errorf "malformed string-interpolate: ~s" (list 'string-interpolate str))))

(define (%string-interpolate str unquote-char)
  (define (accum c acc)
    (cond [(eof-object? c) (list (get-output-string acc))]
          [(char=? c unquote-char)
           (let1 c2 (peek-char)
             (cond [(eof-object? c2) (write-char c acc) (accum c2 acc)]
                   [(char=? c2 unquote-char)
                    (write-char (read-char) acc) (accum (read-char) acc)]
                   [(char-set-contains? #[\u0000-\u0020\),\;\\\]\}\u007f] c2)
                    (write-char c acc) (accum (read-char) acc)]
                   [else (cons (get-output-string acc) (insert))]))]
          [else (write-char c acc) (accum (read-char) acc)]))
  (define (insert)
    (let* ([item (guard (e [(<read-error> e)
                            (errorf "unmatched parenthesis in interpolating string: ~s" str)])
                   (read))]
           [rest (accum (read-char) (open-output-string))])
      (cons `(x->string ,item) rest)))
  (cons 'string-append
        (with-input-from-string str
          (^[] (accum (read-char) (open-output-string))))))

