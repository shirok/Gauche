;;;
;;; interpolate.scm - string interpolation; to be autoloaded
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


;; Legacy syntax:
;;  #`"The value is ,(foo)." => (string-append "The value is " (foo) ".")
;;
;; New syntax:
;;  #"The value is ~(foo)." => (string-append "The value is " (foo) ".")
;;
;; The reader reads #"... ~expr ..."  as #,(string-interpolate "... ~expr ...").
;; The reader-ctor calls string-interpolate, which parses the given string
;; and expands the whole form into a macro call:
;;  (string-interpolate* "... ~expr ..." ("... " expr " ..."))
;; The macro string-interpolate* further expands into string-append and
;; x->string:
;;  (string-append "... " (x->string expr) " ...")
;;
;; We need this multi-step expansion for the following reasons:
;;
;;  1) We have to parse target string and extract embedded expressions
;;     _before_ macro expansion.  Otherwise we can't handle hygiene of
;;     embedded expressions.
;;  2) If we do some source-to-source translation, the expansion of
;;     the reader macro will be embedded in the output.  For the possibility
;;     of future expansion, we'd like to keep intermediate form that does
;;     minimal expansion (in our case, parsing the template string and
;;     extract expressions).  By that, we can tweak the macro later.
;;
;; One caveat of reader macro approach is that it inserts global identifier
;; (string-interpolate*) unhygienically.  We could cheat by directly inserting
;; an identifier, but it turned out a bad idea because it won't work well
;; with source-to-source translators.   So we gave up hygiene and ask
;; users that they need to use #"..." where gauche#string-interpolate* is
;; visible.  After all, it's Gauche's extension, so you can't use it
;; in portable R7RS code, anyway.
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
  (export string-interpolate
          parse-string-interpolation-template
          string-interpolate*)
  )
(select-module gauche.interpolate)

(define (string-interpolate str :optional (legacy? #f))
  (if (string? str)
    `(string-interpolate*
      ,(parse-string-interpolation-template str (if legacy? #\, #\~)))
    (errorf "malformed string-interpolate: ~s" (list 'string-interpolate str))))

(define (parse-string-interpolation-template str unquote-char)
  (define (accum c acc)
    (cond [(eof-object? c)
           (let1 r (get-output-string acc)
             (if (equal? r "") '() (list r)))]
          [(char=? c unquote-char)
           (let1 c2 (peek-char)
             (cond [(eof-object? c2) (write-char c acc) (accum c2 acc)]
                   [(char=? c2 unquote-char)
                    (write-char (read-char) acc) (accum (read-char) acc)]
                   [(char-set-contains? #[\u0000-\u0020\),\;\\\]\}\u007f] c2)
                    (write-char c acc) (accum (read-char) acc)]
                   [else (let1 r (get-output-string acc)
                           (if (equal? r "")
                             (insert)
                             (cons (get-output-string acc) (insert))))]))]
          [else (write-char c acc) (accum (read-char) acc)]))
  (define (insert)
    (let* ([item (guard (e [(<read-error> e)
                            (errorf "unmatched parenthesis in interpolating string: ~s" str)])
                   (read))]
           [rest (accum (read-char) (open-output-string))])
      (cons item rest)))
  (with-input-from-string str
    (^[] (accum (read-char) (open-output-string)))))

(define-syntax string-interpolate*
  (er-macro-transformer
   (^[f r c] (apply %string-interpolate r (cdr f)))))

;; NB: We allow extra args for possible future extension.  For the time
;; being, we ignore them.
(define (%string-interpolate rename elts . _)
  (define x->string. (rename 'x->string))
  (define string-append. (rename 'string-append))
  ;; Trying to simplify output form
  (cond [(null? elts) ""]
        [(null? (cdr elts))
         (if (string? (car elts))
           (car elts)
           `(,x->string. ,(car elts)))]
        [else
         `(,string-append.
           ,@(map (^e (if (string? e)
                        e
                        `(,x->string. ,e)))
                  elts))]))
