;;;
;;; srfi-13/generator - string library (generators)
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
;;;  $Id: generator.scm,v 1.1 2001-04-25 07:33:31 shiro Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

(define (string-tabulate proc len)
  (check-arg (lambda (l) (and (integer? l) (positive? l))) len)
  (let ((sink (open-output-string)))
    (do ((i 0 (+ i 1)))
        ((>= i len) (get-output-string sink))
      (write-char (proc i) sink))))

(define (reverse-list->string char-list)
  (list->string (reverse char-list)))
