;;;
;;; srfi-13/hash - string library (hash functions)
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
;;;  $Id: hash.scm,v 1.2 2001-06-29 20:32:47 shirok Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

(define (string-hash s . args)
  (check-arg string? s)
  (let-optionals* args (bound start end)
    (%hash-string (%maybe-substring s start end) bound)))

(define (string-hash-ci s . args)
  (check-arg string? s)
  (let-optionals* args (bound start end)
    ;; oops! optimization required!
    (%hash-string (string-upcase (%maybe-substring s start end)) bound)))

