;;;
;;; srfi-13/revapp - string library (reverse, append)
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
;;;  $Id: revapp.scm,v 1.1 2001-04-28 07:29:53 shirok Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

(define (string-reverse s . args)
  (let ((sp (apply make-string-pointer s args))
        (dst (open-output-string)))
    (let loop ((ch (string-pointer-prev! s)))
      (if (eof-object? ch)
          (get-output-string dst)
          (begin (write-char ch dst)
                 (loop (string-pointer-prev! s)))))
    ))

(define (string-concatenate list)
  (apply string-append list)) ;; fixme

(define (string-concatenate/shared list)
  (apply string-append list)) ;; fixme

(define string-append/shared string-append)

(define (string-concatenate-reverse list)
  (string-concatenate (reverse list)))

(define (string-concatenate-reverse/shared list)
  (string-concatenate (reverse list)))

