;;;
;;; srfi-13/selector - string library (selectors)
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
;;;  $Id: selector.scm,v 1.2 2001-04-26 08:23:42 shiro Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(use srfi-14)                           ;for char-set:whitespace

(define substring/shared string-copy)  ; same in Gauche

(define (string-copy! target tstart s . args)
  (check-arg string? target)
  (check-arg (lambda (x) (and (integer? x) (positive? x))) tstart)
  (let* ((str (apply %maybe-substring s args))
         (slen (string-length str))
         (tlen (string-length target)))
    (when (> (+ tstart slen) tlen)
      (error "copy operation runs off the target string: ~s" target))
    (string-substitute! target tstart (+ tstart slen) str)))

(define (string-pad s len . args)
  (let-optional* args ((char #\space) start end)
    (check-arg char? char)
    (let* ((str (%maybe-substring s start end))
           (slen (string-length str)))
      (cond ((< slen len)
             (string-append (make-string (- len slen) char) str))
            ((> slen len)
             (string-take-right str len))
            (else str)))))

(define (string-pad-right s len . args)
  (let-optional* args ((char #\space) start end)
    (check-arg char? char)
    (let* ((str (%maybe-substring s start end))
           (slen (string-length str)))
      (cond ((< slen len)
             (string-append str (make-string (- len slen) char)))
            ((> slen len)
             (string-take str len))
            (else str)))))


    

    