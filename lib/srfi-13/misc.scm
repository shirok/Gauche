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
;;;  $Id: misc.scm,v 1.2 2001-05-01 06:44:35 shirok Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

(define (string-replace s1 s2 start1 end1 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (string-append (substring s1 0 start1)
                 (apply %maybe-substring s2 args)
                 (substring s1 end1 (string-length s1))))

(define (string-tokenize s . args)
  (check-arg string? s)
  (let-optional* args ((token-set #[\S]) start end)
    (letrec ((src (open-input-string (%maybe-substring s start end)))
             (in-word (lambda (ch dst r)
                        (cond ((eof-object? ch)
                               (reverse! (cons (get-output-string dst) r)))
                              ((char-set-contains? token-set ch)
                               (write-char ch dst)
                               (in-word (read-char src) dst r))
                              (else
                               (out-word (read-char src)
                                         (cons (get-output-string dst) r))))))
             (out-word (lambda (ch r)
                         (cond ((eof-object? ch) (reverse! r))
                               ((char-set-contains? token-set ch)
                                (let ((dst (open-output-string)))
                                  (write-char ch dst)
                                  (in-word (read-char src) dst r)))
                               (else
                                (out-word (read-char src) r))))))
      (out-word (read-char src) '()))))


  