;;;
;;; srfi-13/rotator - string library (rotation)
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
;;;  $Id: rotator.scm,v 1.5 2001-06-30 09:42:38 shirok Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

(define (xsubstring s from . args)
  (check-arg string? s)
  (check-arg (lambda (x) (and (integer? x) (exact? x))) from)
  (let-optionals* args (to start end)
    (let* ((str (%maybe-substring s start end))
           (len (string-length str))
           (from-rank (quotient from len))
           (from-mod  (modulo from len))
           (dest (open-output-string)))
      (cond ((undefined? to)
             (set! from from-mod)
             (set! to (+ from len)))
            ((not (and (integer? to) (exact? to)))
             (error "argument out of domain:" to))
            ((= len 0)
             (error "zero length source string is not allowed"))
            ((< to from)
             (errorf "argument out of range (from, to): (~s, ~s)" from to))
            (else
             (set! from from-mod)
             (set! to (- to (* from-rank len)))))
      (let ((sp (make-string-pointer str from)))
        (let loop ((count from)
                   (ch (string-pointer-next! sp)))
          (cond ((>= count to) (get-output-string dest))
                ((eof-object? ch)
                 (string-pointer-set! sp 0)
                 (write-char (string-pointer-next! sp) dest)
                 (loop (+ count 1) (string-pointer-next! sp)))
                (else
                 (write-char ch dest)
                 (loop (+ count 1) (string-pointer-next! sp)))))
        )
      ))
  )

(define (string-xcopy! target tstart s sfrom . args)
  (check-arg string? target)
  (check-arg (lambda (x) (and (integer? x) (exact? x))) tstart)
  (let ((result (apply xsubstring s sfrom args)))
    (string-substitute! target tstart result)))
