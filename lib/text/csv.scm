;;;
;;; csv.scm - read and write CSV (actually, xSV) format.
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

(define-module text.csv
  (use srfi-1)
  (use srfi-11)
  (use srfi-13)
  (use gauche.regexp)
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
  (let ((p (slot-ref self 'port)))
    (unless (is-a? (slot-ref self 'port) <port>)
      (error "port must be given to instantiate <csv>"))
    (slot-set! self 'ioproc
               (if (input-port? p)
                   (make-csv-reader (slot-ref self 'separator))
                   (make-csv-writer (slot-ref self 'separator))))))

(define (make-csv-reader separator . args)
  (let* ((quote-char (get-optional args #\"))
         (quote-string (string quote-char)))
    (lambda (port)
      (define (start line fields)
        (if (eof-object? line)
          (reverse! fields)
          (let ((next (string-trim line #[ ])))
            (if (string-prefix? quote-string next)
              (quoted (string-drop next 1) fields '())
              (noquote next fields)))))
      (define (noquote line fields)
        (cond ((string-index line separator)
               => (lambda (i)
                    (start (string-drop line (+ i 1))
                           (cons (string-trim-right (string-take line i))
                                 fields))))
              (else (reverse! (cons (string-trim-right line #[ ]) fields)))))
      (define (quoted line fields partial)
        (cond ((eof-object? line) (error "unterminated quoted field"))
              ((string-null? line)
               (quoted (read-line port) fields (cons "\n" partial)))
              (else
               (receive (this next) (string-scan line quote-char 'both)
                 (if this
                   (if (string-prefix? quote-string next)
                     (quoted (string-drop next 1) fields
                             (list* quote-string this partial))
                     (let ((next-next (string-scan next separator 'after))
                           (f (string-concatenate-reverse (cons this partial))))
                       (if next-next
                         (start next-next (cons f fields))
                         (reverse!(cons f fields)))))
                   (quoted (read-line port) fields
                           (list*  "\n" line partial)))))))
      (let ((line (read-line port)))
        (if (eof-object? line)
          line
          (start line '())))
      )))

(define (make-csv-writer separator :optional (newline "\n") (quote-char #\"))
  (let* ((quote-string (string quote-char))
         (quote-escape (string-append quote-string quote-string))
         (quote-rx (string->regexp (regexp-quote quote-string)))
         (separator-chars (if (string? separator)
                            (string->list separator)
                            (list separator)))
         (special-chars
          (apply char-set quote-char #\space #\newline #\return
                 separator-chars)))

    (lambda (port fields)
      (define (write-a-field field)
        (if (string-index field special-chars)
          (begin (display quote-char port)
                 (display (regexp-replace-all quote-rx field quote-escape) port)
                 (display quote-char port))
          (display field port)))

      (unless (null? fields)
        (write-a-field (car fields))
        (for-each (lambda (field)
                    (display separator port)
                    (write-a-field field))
                  (cdr fields)))
      (display newline port))))

