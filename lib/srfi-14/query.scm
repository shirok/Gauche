;;;
;;; srfi-14/query - character set (querying contents)
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
;;;  $Id: query.scm,v 1.1 2001-04-25 07:43:36 shiro Exp $
;;;

;; Say `(use srfi-14)' and this file is autoloaded on demand.

(select-module srfi-14)

(define (char-set-size cs)
  (let ((count 0))
    (for-each (lambda (range)
                (set! count (+ count (- (cdr range) (car range)) 1)))
              (%char-set-ranges cs))
    count))

(define (char-set-count pred cs)
  (char-set-fold (lambda (c r) (if (pred c) (+ r 1) r)) 0 cs))

(define (char-set->list cs)
  (char-set-fold cons '() cs))

(define (char-set->string cs)
  (list->string (char-set-fold cons '() cs)))

;; char-set-contains? : native

(define (char-set-every pred cs)
  (let loop ((cursor (char-set-cursor cs)))
    (cond ((end-of-char-set? cursor))
          ((pred (char-set-ref cs cursor))
           (loop (char-set-cursor-next cs cursor)))
          (else #f))))

(define (char-set-any pred cs)
  (let loop ((cursor (char-set-cursor cs)))
    (cond ((end-of-char-set? cursor) #f)
          ((pred (char-set-ref cs cursor)) #t)
          (else (loop (char-set-cursor-next cs cursor))))))

