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
;;;  $Id: csv.scm,v 1.2 2001-09-21 09:58:33 shirok Exp $
;;;

(define-module text.csv
  (use srfi-1)
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
            ((string-index line #\")
             => (lambda (i)
                  (let ((this (string-take line i))
                        (next (string-drop line (+ i 1))))
                    (if (string-prefix? "\"" next)
                        (quoted (string-drop next 1) fields
                                (list* "\"" this partial))
                        (let ((j (string-index next separator))
                              (f (string-concatenate-reverse
                                  (cons this partial))))
                          (if j
                              (start (string-drop next (+ j 1)) (cons f fields))
                              (reverse!(cons f fields))))))))
            (else
             (quoted (read-line port) fields (list*  "\n" line partial)))))
    (let ((line (read-line port)))
      (if (eof-object? line)
          line
          (start line '())))
    ))

  
  

(provide "text/csv")
