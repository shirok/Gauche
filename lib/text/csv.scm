;;;
;;; csv.scm - read and write CSV (actually, xSV) format.
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
;;;  $Id: csv.scm,v 1.4 2001-09-24 08:50:04 shirok Exp $
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

(define (make-csv-reader separator)
  (lambda (port)
    (define (start line fields)
      (if (eof-object? line)
          (reverse! fields)
          (let ((next (string-trim line)))
            (if (string-prefix? "\"" next)
                (quoted (string-drop next 1) fields '())
                (noquote next fields)))))
    (define (noquote line fields)
      (cond ((string-index line separator)
             => (lambda (i)
                  (start (string-drop line (+ i 1))
                         (cons (string-trim-right (string-take line i))
                               fields))))
            (else (reverse! (cons (string-trim-right line) fields)))))
    (define (quoted line fields partial)
      (cond ((eof-object? line) (error "unterminated quoted field"))
            ((string-null? line)
             (quoted (read-line port) fields (cons "\n" partial)))
            (else
             (receive (this next) (string-scan line #\" 'both)
               (if this
                   (if (string-prefix? "\"" next)
                       (quoted (string-drop next 1) fields
                               (list* "\"" this partial))
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
    ))

(define (make-csv-writer separator . args)
  (define newline (if (pair? args) (car args) "\n"))

  (lambda (port fields)
    (define (write-a-field field)
      (if (string-index field #[ ,\"\n\r])
          (begin (display #\" port)
                 (display (regexp-replace-all #/\"/ field "\"\"") port)
                 (display #\" port))
          (display field port)))

    (unless (null? fields)
      (write-a-field (car fields))
      (for-each (lambda (field)
                  (display separator port)
                  (write-a-field field))
                (cdr fields)))
    (display newline port)))

(provide "text/csv")
