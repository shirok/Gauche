;;;
;;; srfi-13/casemap - string library
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
;;;  $Id: casemap.scm,v 1.1 2001-04-28 07:29:53 shirok Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

(define (string-upcase s . args)
  (let ((src (open-input-string (apply %maybe-substring s args)))
        (dst (open-output-string)))
    (let loop ((ch (read-char src)))
      (if (eof-object? ch)
          (get-output-string dst)
          (write-char (char-upcase ch) dst)))))

(define (string-downcase s . args)
  (let ((src (open-input-string (apply %maybe-substring s args)))
        (dst (open-output-string)))
    (let loop ((ch (read-char src)))
      (if (eof-object? ch)
          (get-output-string dst)
          (write-char (char-downcase ch) dst)))))

(define *%cased-char-set* #[A-Za-z]) ;; fixme

(define (string-titlecase s . args)
  (let ((src (open-input-string (apply %maybe-substring s args)))
        (dst (open-output-string)))
    (let loop ((title? #t)
               (ch   (read-char src)))
      (cond ((eof-object? ch) (get-output-string dst))
            ((char-set-contains? *%cased-char-set* ch)
             (if title?
                 (write-char (char-titlecase ch) dst)
                 (write-char (char-downcase ch) dst))
             (loop #f (read-char src)))
            (else
             (write-char ch dst)
             (loop #t (read-char src))))
      )))

(define (string-upcase! s . args)
  (let-optional* args ((start 0) end)
    (string-substitute! s start (string-upcase s start end))))

(define (string-downcase! s . args)
  (let-optional* args ((start 0) end)
    (string-substitute! s start (string-upcase s start end))))

(define (string-titlecase! s . args)
  (let-optional* args ((start 0) end)
    (string-substitute! s start (string-titlecase s start end))))

