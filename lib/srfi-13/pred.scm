;;;
;;; srfi-13/pred - string library (predicates)
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
;;;  $Id: pred.scm,v 1.1 2001-04-25 07:33:52 shiro Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

(define (string-null? str)
  (check-arg string? str)
  (equal? "" str))

(define (string-every c/s/p s . args)
  (let* ((src  (open-input-string (apply %maybe-substring s args)))
         (pred (%get-char-pred c/s/p)))
    (let loop ((ch (read-char src))
               (r  #t))
      (cond ((not r) #f)
            ((eof-object? ch) r)
            (else (loop (read-char src) (pred ch)))))))

(define (string-any c/s/p s . args)
  (let* ((src  (open-input-string (apply %maybe-substring s args)))
         (pred (%get-char-pred c/s/p)))
    (let loop ((ch (read-char src)))
      (cond ((pred ch))
            ((eof-object? ch) #f)
            (else (loop (read-char src)))))))


