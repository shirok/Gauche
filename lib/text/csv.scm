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
;;;  $Id: csv.scm,v 1.1 2001-09-20 20:44:32 shirok Exp $
;;;

(define-module text.csv
  (use srfi-1)
  (use srfi-13)
  (export <csv>
          make-csv-reader
          make-csv-writer)
  )
(select-module text.csv)

(define-class <csv> ()
  ((port      :init-keyword :port :initform #f)
   (separator :init-keyword :separator :initform #\,)
   (ioproc))
  )

(define (make-csv-reader separator)
  (lambda (port)
    (define (noquote line fields)
      (cond ((eof-object? line) (reverse! fields))
            ((string-index line separator)
             => (lambda (i)
                  (let ((this (string-trim-right (string-take line i)))
                        (next (string-trim (string-drop line (+ i 1)))))
                    (if (string-prefix? "\"" next)
                        (quoted (string-drop next 1) (cons this fields) '())
                        (noquote line (cons this fields))))))
            (else (reverse! (cons (string-trim-right line) fields)))))
    (define (quoted line fields partial)
      (cond ((eof-object? line) (error ""))))
    
    (noquote (string-trim (read-line port)))))

(provide "text/csv")
