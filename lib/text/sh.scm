;;;
;;; text.sh - Shell-compatible text processing
;;;
;;;   Copyright (c) 2024  Shiro Kawai  <shiro@acm.org>
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

;; This module provides utilities that emulate text manipulation operations
;; seen in typical Unix shells.  It is handy to port shell scripts to
;; Gauche.

(define-module text.sh
  (use srfi.13)
  (use util.match)
  (export shell-escape-string shell-tokenize-string
          shell-match shell-case)
  )
(select-module text.sh)

;;
;; Argument processing
;;

;; API
(define (shell-escape-string str :optional (flavor
                                            (cond-expand
                                             [gauche.os.windows 'windows]
                                             [else 'posix])))
  (ecase flavor
   ;; Windows flavors is supported in src/libsys.scm.  See the comment in it.
   [(windows)
    (%sys-escape-windows-command-line str #f)]
   [(windows-batchfile)
    (%sys-escape-windows-command-line str #t)]
   [(posix)
    ;; We follow standard unix shell convention: if STR contains special
    ;; chars, we quote the entire STR by single-quotes.  If STR contains
    ;; a single quote, we replace it with '"'"'.
    (cond [(string-null? str) "''"]
          [(string-index str #[\s\\\"\'`*?$<>!\[\](){}])
           (string-append "'" (regexp-replace-all #/'/ str "'\"'\"'") "'")]
          [else str])]))

;; API
;; Aux procedure to tokenize command-line string into command name and
;; arglist.   We don't deal with shell metacharacters; just recognize
;; quotes.  For the time being, we reject any unquoted shell metacharacters.
;; POSIX shell syntax:
;; http://pubs.opengroup.org/onlinepubs/009695399/utilities/xcu_chap02.html
;; About wicked MS shell syntax, see the comment of
;; %sys-escape-windows-command-line in src/libsys.scm.
;; NB: We don't expect STR to be too long, so just recurse.
(define (shell-tokenize-string str :optional (flavor
                                              (cond-expand
                                               [gauche.os.windows 'windows]
                                               [else 'posix])))
  (define (skip-ws)
    (let loop ([c (peek-char)])
      (when (and (char? c) (char-whitespace? c))
        (begin (read-char) (loop (peek-char))))))
  (define (err fmt . args)
    (apply errorf #"Bad shell syntax - ~fmt" args))
  (define (err-meta c)
    (err "shell metacharacter ~s not allowed, in ~s" c str))
  (define (err-backslash)
    (err "stray backslash in ~s" str))

  (define (tokenize-posix)
    (with-input-from-string str
      (^[]
        (define (read-next)
          (let1 c (read-char)
            (cond [(eof-object? c) '()]
                  [(char-whitespace? c) (skip-ws) '()]
                  [(eqv? c #\') (read-sq)]
                  [(eqv? c #\") (read-dq)]
                  [(eqv? c #\\) (read-escaped)]
                  [(#[|&\;<>()$`] c) (err-meta c)]
                  [else (read-unquoted c)])))
        (define (read-escaped)
          (let1 c (read-char)
            (if (eof-object? c)
              (err-backslash)
              (read-unquoted c))))
        (define (read-unquoted c0)
          (let1 c (peek-char)
            (cond [(eof-object? c) (list c0)]
                  [(char-whitespace? c) (skip-ws) (list c0)]
                  [(eqv? c #\') (read-char) (cons c0 (read-sq))]
                  [(eqv? c #\") (read-char) (cons c0 (read-dq))]
                  [(eqv? c #\\) (read-char) (cons c0 (read-escaped))]
                  [(#[|&\;<>()$`] c) (err-meta c)]
                  [else (read-char) (cons c0 (read-unquoted c))])))
        (define (read-sq)
          (let loop ([c (read-char)] [cs '()])
            (cond [(eof-object? c) (err "unclosed single quote: ~s" str)]
                  [(eqv? c #\') (reverse cs (read-next))]
                  [else (loop (read-char) (cons c cs))])))
        (define (read-dq)
          (let loop ([c (read-char)] [cs '()])
            (cond [(eof-object? c) (err "unclosed double quote: ~s" str)]
                  [(eqv? c #\") (reverse cs (read-next))]
                  [(eqv? c #\\) (let1 c (read-char) ; section 2.2.3
                                  (cond [(eof-object? c) (err-backslash)]
                                        [(#[$`\"\\\n] c)
                                         (loop (read-char) (cons c cs))]
                                        [else
                                         (loop (read-char) (cons* c #\\ cs))]))]
                  [(#[$`] c) (err-meta c)]
                  [else (loop (read-char) (cons c cs))])))

        ;; body
        (skip-ws)
        (let loop ([words '()])
          (if (eof-object? (peek-char))
            (reverse words)
            (loop (cons (list->string (read-next)) words)))))))

  (define (tokenize-windows-cmd)
    (with-input-from-string str
      (^[]
        (define (read-next)
          (let1 c (read-char)
            (cond [(eof-object? c) '()]
                  [(char-whitespace? c) (skip-ws) '()]
                  [(eqv? c #\") (read-quoted)]
                  [else (read-unquoted c)])))
        (define (read-unquoted c)
          (let loop ([c c] [cs '()])
            (cond [(eof-object? c) (reverse cs)]
                  [(char-whitespace? c) (skip-ws) (reverse cs)]
                  [(eqv? c #\\) (let1 cs (read-backslash cs)
                                  (loop (read-char) cs))]
                  [(eqv? c #\") (reverse cs (read-quoted))]
                  [else (loop (read-char) (cons c cs))])))
        (define (read-quoted)
          (let loop ([c (read-char)] [cs '()])
            (cond [(eof-object? c) (reverse cs)] ;implicitly closed
                  [(eqv? c #\\) (let1 cs (read-backslash cs)
                                  (loop (read-char) cs))]
                  [(eqv? c #\") (read-after-closing cs)]
                  [else (loop (read-char) (cons c cs))])))
        (define (read-backslash cs)
          (let loop ([n 1] [cs cs])
            (let1 c (peek-char)
              (cond [(eqv? c #\\) (read-char) (loop (+ n 1) cs)]
                    [(eqv? c #\")
                     (if (even? n)
                       (append (make-list (ash n -1) #\\) cs)
                       (cons (read-char)
                             (append (make-list (ash n -1) #\\) cs)))]
                    [else (append (make-list n #\\) cs)]))))
        (define (read-after-closing cs)
          (let1 c (peek-char)
            (if (eqv? c #\") ; this is treated as literal dq
              (begin (read-char)
                     (reverse (cons c cs) (read-next)))
              (reverse cs (read-next)))))
        ;; body
        (skip-ws)
        (let loop ([words '()])
          (if (eof-object? (peek-char))
            (reverse words)
            (loop (cons (list->string (read-next)) words)))))))

  (ecase flavor
    [(windows) (tokenize-windows-cmd)]
    [(posix) (tokenize-posix)]))

;; Shell-style string pattern match
;;
;; PATTERN+ := PATTERN | (PATTERN PATTERN ...)
;;
;; PATTERN may contain the following special characters:
;;
;;    *       Matches zero or more characters.
;;    ?       Matches any single character.
;;    [...]   Character class.  It is interpreted as #[...] in Gauche,
;;            except that when '!' appears first, it is understood as
;;            complement operator, the same as '^'.
;;            POSIX character class [:classs:] is supported, but not
;;            equivalence class [=c=], nor collating symbol [.symbol.]
;;            are supported yet.
;;    \c      same as c.


;; API
;;   (shell-match pattern-expr string) => boolean
;; pattern-expr must evaluate to a string or a list of strings,
(define (shell-match pattern+ str)
  (cond
   [(string? pattern+)
    (boolean ((glob-component->regexp pattern+ :mode :shell) str))]
   [(of-type? pattern+ (<List> <string>))
    (boolean (any (^p ((glob-component->regexp p :mode :shell) str)) pattern+))]
   [else (error "Invalid pattern for shell-match:" pattern+)]))

;; API
;;  Like shell's 'case'
(define-syntax shell-case
  (syntax-rules ()
    [(_ str clause ...)
     (let ((tmp str))
       (%shell-case tmp clause ...))]))

(define-syntax %shell-case
  (syntax-rules (else)
    [(_ str) (undefined)]
    [(_ str (else expr ...)) (begin expr ...)]
    [(_ str (pattern+ expr ...) . rest)
     (if (shell-match 'pattern+ str)
       (begin expr ...)
       (%shell-case str . rest))]))
