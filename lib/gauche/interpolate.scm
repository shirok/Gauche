;;;
;;; interpolate.scm - string interpolation; to be autoloaded
;;;
;;;   Copyright (c) 2000-2013  Shiro Kawai  <shiro@acm.org>
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

;;; #`"The value is ,|foo|." => (string-append "The value is " foo ".")
;;;

(define-module gauche.interpolate
  (export string-interpolate)
  )
(select-module gauche.interpolate)

(define (string-interpolate str)
  (if (string? str)
    (%string-interpolate str)
    (errorf "malformed string-interpolate: ~s" (list 'string-interpolate str))))

(define (%string-interpolate str)
  (define (accum c acc)
    (cond [(eof-object? c) (list (get-output-string acc))]
          [(char=? c #\,)
           (let1 c2 (peek-char)
             (cond [(eof-object? c2) (write-char c acc) (accum c2 acc)]
                   [(char=? c2 #\,)
                    (write-char (read-char) acc) (accum (read-char) acc)]
                   [(char-set-contains? #[\x00-\x20\),\;\\\]\}\x7f] c2)
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

