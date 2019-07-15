;;;
;;; text.sql - SQL parsing
;;;
;;;   Copyright (c) 2005-2019  Shiro Kawai  <shiro@acm.org>
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

;; *EXPERIMENTAL*
;; This module will define an S-expr notation of SQL and conversion
;; routines from/to the standard SQL syntax.
;; For the time being, we only use tokenizer which is used by dbi/dbd
;; modules.

(define-module text.sql
  (use srfi-13)
  (use util.match)
  (export <sql-parse-error>
          sql-tokenize
          ;sql->sxql
          ))
(select-module text.sql)

;;;-----------------------------------------------------------------
;;; Conditions
;;;

(define-condition-type <sql-parse-error> <error> #f
  (sql-string))         ;; original SQL string

;;;-----------------------------------------------------------------
;;; Parser
;;;

;(define (sql->sxql sql-string)

;  (define (parse-stmt tokens)
;    (match tokens
;      (() (values '() '()))
;      (('select . rest) (parse-select (cdr tokens)))
;      (('insert . rest) (parse-insert (cdr tokens)))
;      (('delete . rest) (parse-delete (cdr tokens)))
;      (('create . rest) (parse-create (cdr tokens)))
;      (else
;       (err "Invalid or unsupported statement type '~a' in ~s"
;            (car tokens) sql-string))))

;  (define (parse-select tokens)
;    (parse-select-columns tokens '()))

;  (define (parse-select-columns tokens columns)
;    (cond
;     ((or (null? tokens) (eqv? (car tokens) #\;))
;      (err "Unterminated SELECT statement in ~s" sql-string))
;     ((eq? (car tokens) 'from)
;      (parse-select-from (cdr tokens) (reverse! columns)))
;     (else
;      (parse-select-column tokens columns))))

;  (define (parse-select-column tokens columns)
;    (cond
;     ((eq? (car tokens)
;           ))))

;  ;; error
;  (define (err fmt . args)
;    (apply errorf <sql-parse-error> :sql-string sql-string fmt args))

;  ;; The body of sql->sxql
;  (parse-stmt (sql-tokenize sql-string)))

;;;-----------------------------------------------------------------
;;; Tokenizer
;;;   Returns a list of tokens.  Each token is either one of the
;;;   following form.
;;;
;;;    <string>              Regular identifiers.
;;;    <symbol>              Special delimiters:
;;;                           + - * / < = > <> <= >= ||
;;;    <char>                Special delimiters:
;;;                           , . ( ) ;
;;;    (delimited <string>)  Delimited identifier
;;;    (parameter <num>)     Positional parameter (?)
;;;    (parameter <string>)  Named parameter (:foo)
;;;    (string    <string>)  Character string literal
;;;    (number    <string>)  Numeric literal
;;;    (bitstring <string>)  Binary string.  <string> is like "01101"
;;;    (hexstring <string>)  Binary string.  <string> is like "3AD20"
;;;
;;;   NB: The difference of two kinds of special delimiters is that
;;;       the former is sometimes written with surrounding
;;;       whitespaces, while the latter is almost never written with them.

(define (sql-tokenize sql-string)
  (define parameter-count 0) ;; positional parameter count
  ;;
  ;; skip whitespaces
  ;;
  (define (skip-ws s)
    (cond ((#/^\s+(--)?/ s)
           => (^m (if (m 1) (skip-comment s) (m 'after))))
          (else s)))
  (define (skip-comment s)
    (cond ((string-scan s "\n" 'after))
          (else "")))
  ;;
  ;; main dispatcher
  ;;
  (define (entry s r)
    (let1 s (skip-ws s)
      (if (string-null? s)
        (reverse! r)
        (let1 c (string-ref s 0)
          (cond
           [(char=? c #\') (scan-string s r)]
           [(char=? c #\") (scan-delimited s r)]
           [(char=? c #\?)
            (entry (string-drop s 1)
                   (cons `(parameter ,(begin0 parameter-count
                                              (inc! parameter-count)))
                         r))]
           [(#/^[+-]?(?:\d+(?:\.\d*)?|(?:\.\d+))(?:[eE][+-]?\d+)?/ s)
            => (lambda (m)
                 (entry (m 'after) (cons `(number ,(m)) r)))]
           [(#/^(<>|<=|>=|\|\|)/ s)
            (entry (string-drop s 2)
                   (cons (string->symbol (string-take s 2)) r))]
           [(#/^[bB]'/ s) (scan-bitstring s r)]
           [(#/^[xX]'/ s) (scan-hexstring s r)]
           [(char-set-contains? #[-+*/<=>] c)
            (entry (string-drop s 1) (cons (string->symbol (string c)) r))]
           [(char-set-contains? #[,.()\;] c)
            (entry (string-drop s 1) (cons c r))]
           [(#/^:(\w+)/ s)
            => (lambda (m)
                 (entry (m 'after) (cons `(parameter ,(m 1)) r)))]
           [(#/^\w+/ s)
            => (lambda (m)
                 (entry (m 'after) (cons (m) r)))]
           [else (e "invalid SQL token beginning with ~s in: ~s"
                    c sql-string)])))))
  ;;
  ;; subscanners
  ;;
  (define (scan-quote s r q succ efmt)
    (let loop ((pos 0))
      (or (and-let* ([pos (string-index s q pos)])
            (if (eqv? (string-ref s (+ pos 1) #f) q)
              (loop (+ pos 2))
              (succ (string-take s pos) (string-drop s (+ pos 1)))))
          (e efmt sql-string))))

  (define (scan-string s r)
    (scan-quote (string-drop s 1) r #\'
                (lambda (body rest)
                  (entry rest
                         `((string ,(regexp-replace-all #/''/ body "'"))
                           . ,r)))
                "unterminated string literal in SQL: ~s"))

  (define (scan-delimited s r)
    (scan-quote (string-drop s 1) r #\"
                (lambda (body rest)
                  (entry rest
                         `((delimited ,(regexp-replace-all #/""/ body "\""))
                           . ,r)))
                "unterminated delimited identifier in SQL: ~s"))

  (define (scan-bitstring s r)
    (cond [(#/^.'([01]+)'/ s)
           => (lambda (m)
                (entry (m 'after)
                       (cons `(bitstring ,(m 1)) r)))]
          [else
           (e "unterminated bitstring literal in SQL: ~s" sql-string)]))
  (define (scan-hexstring s r)
    (cond [(#/^.'([\da-fA-F]+)'/ s)
           => (lambda (m)
                (entry (m 'after)
                       (cons `(hexstring ,(m 1)) r)))]
          [else
           (e "unterminated bitstring literal in SQL: ~s" sql-string)]))
  ;;
  ;; raising an error
  ;;
  (define (e fmt . args)
    (apply errorf <sql-parse-error> :sql-string sql-string fmt args))
  ;;
  ;; main entry
  ;;
  (entry (skip-ws sql-string) '()))


