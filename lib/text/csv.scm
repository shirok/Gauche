;;;
;;; csv.scm - read and write CSV (actually, xSV) format.
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

(define-module text.csv
  (use srfi-1)
  (use srfi-11)
  (use srfi-13)
  (export <csv>
          make-csv-reader
          make-csv-writer)
  )
(select-module text.csv)

;; Parameters:
;;   separator - a character to be used to separate fields.
;;   columns   - a list of column specification.
;;       (<name> :default <default> :iconv <iconv> :oconv <oconv>)

(define-class <csv> ()
  ((port      :init-keyword :port :initform #f)
   (separator :init-keyword :separator :initform #\,)
   (columns   :init-keyword :columns :initform #f)
   (ioproc))
  )

(define-method initialize ((self <csv>) initargs)
  (next-method)
  (let1 p (slot-ref self 'port)
    (unless (is-a? (slot-ref self 'port) <port>)
      (error "port must be given to instantiate <csv>"))
    (slot-set! self 'ioproc
               (if (input-port? p)
                 (make-csv-reader (slot-ref self 'separator))
                 (make-csv-writer (slot-ref self 'separator))))))

;; API
(define (make-csv-reader separator :optional (quote-char #\"))
  (^[:optional (port (current-input-port))]
    (csv-reader separator quote-char port)))

(define (csv-reader sep quo port)
  (define (eor? ch) (or (eqv? ch #\newline) (eof-object? ch)))

  (define (start fields)
    (let1 ch (read-char port)
      (cond [(eor? ch) (reverse! (cons "" fields))]
            [(eqv? ch sep) (start (cons "" fields))]
            [(eqv? ch quo) (quoted fields)]
            [(char-whitespace? ch) (start fields)]
            [else (unquoted (list ch) fields)])))

  (define (unquoted chs fields)
    (let loop ([ch (read-char port)] [last chs] [chs chs])
      (cond [(eor? ch) (reverse! (cons (finish last) fields))]
            [(eqv? ch sep) (start (cons (finish last) fields))]
            [(char-whitespace? ch) (loop (read-char port) last (cons ch chs))]
            [else (let1 chs (cons ch chs)
                    (loop (read-char port) chs chs))])))

  (define (finish rchrs) (list->string (reverse! rchrs)))

  (define (quoted fields)
    (let loop ([ch (read-char port)] [chs '()])
      (cond [(eof-object? ch) (error "unterminated quoted field")]
            [(eqv? ch quo)
             (if (eqv? (peek-char port) quo)
               (begin (read-char port) (loop (read-char port) (cons quo chs)))
               (quoted-tail (cons (finish chs) fields)))]
            [else (loop (read-char port) (cons ch chs))])))

  (define (quoted-tail fields)
    (let loop ([ch (read-char port)])
      (cond [(eor? ch) (reverse! fields)]
            [(eqv? ch sep) (start fields)]
            [else (loop (read-char port))])))

  (if (eof-object? (peek-char port))
    (eof-object)
    (start '())))

(define (make-csv-writer separator :optional (newline "\n") (quote-char #\"))
  (let* ([quote-string (string quote-char)]
         [quote-escape (string-append quote-string quote-string)]
         [quote-rx (string->regexp (regexp-quote quote-string))]
         [separator-chars (if (string? separator)
                            (string->list separator)
                            (list separator))]
         [special-chars
          (apply char-set quote-char #\space #\newline #\return
                 separator-chars)])

    (^[port fields]
      (define (write-a-field field)
        (if (string-index field special-chars)
          (begin (display quote-char port)
                 (display (regexp-replace-all quote-rx field quote-escape) port)
                 (display quote-char port))
          (display field port)))

      (unless (null? fields)
        (write-a-field (car fields))
        (dolist [field (cdr fields)]
          (display separator port)
          (write-a-field field)))
      (display newline port))))

