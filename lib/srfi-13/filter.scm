;;;
;;; srfi-13/filter - string library (filter)
;;;
;;;  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: filter.scm,v 1.1 2001-04-30 18:37:45 shirok Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

(define (string-filter s c/s/p . args)
  (check-arg string? s)
  (let ((src (open-input-string (apply %maybe-substring s args)))
        (dest (open-output-string))
        (pred (%get-char-pred c/s/p)))
    (let loop ((ch (read-char src)))
      (cond ((eof-object? ch) (get-output-string dest))
            ((pred ch) (write-char ch dest) (loop (read-char src)))
            (else (loop (read-char src)))))
    ))

(define (string-delete s c/s/p . args)
  (check-arg string? s)
  (let ((src (open-input-string (apply %maybe-substring s args)))
        (dest (open-output-string))
        (pred (%get-char-pred c/s/p)))
    (let loop ((ch (read-char src)))
      (cond ((eof-object? ch) (get-output-string dest))
            ((pred ch) (loop (read-char src)))
            (else (write-char ch dest) (loop (read-char src)))))
    ))


              