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
;;;  $Id: selector.scm,v 1.4 2001-05-02 08:20:25 shirok Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

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

(define (string-take s nchars)
  (check-arg string? s)
  (when (or (< nchars 0) (> nchars (string-length s)))
    (error "argument out of range: ~s" nchars))
  (%maybe-substring s 0 nchars))

(define (string-drop s nchars)
  (check-arg string? s)
  (when (or (< nchars 0) (> nchars (string-length s)))
    (error "argument out of range: ~s" nchars))
  (%maybe-substring s nchars))

(define (string-take-right s nchars)
  (check-arg string? s)
  (when (or (< nchars 0) (> nchars (string-length s)))
    (error "argument out of range: ~s" nchars))
  (%maybe-substring s (- (string-length s) nchars)))

(define (string-drop-right s nchars)
  (check-arg string? s)
  (when (or (< nchars 0) (> nchars (string-length s)))
    (error "argument out of range: ~s" nchars))
  (%maybe-substring s 0 (- (string-length s) nchars)))

(define (string-trim s . args)
  (check-arg string? s)
  (let-optional* args ((c/s/p #[\s]) start end)
    (let ((pred (%get-char-pred c/s/p))
          (sp (make-string-pointer (%maybe-substring s start end))))
      (let loop ((ch (string-pointer-next! sp)))
        (cond ((eof-object? ch) "")
              ((pred ch) (loop (string-pointer-next! sp)))
              (else (string-pointer-prev! sp)
                    (string-pointer-substring sp :after #t))))
      ))
  )

(define (string-trim-right s . args)
  (check-arg string? s)
  (let-optional* args ((c/s/p #[\s]) start end)
    (let ((pred (%get-char-pred c/s/p))
          (sp (make-string-pointer (%maybe-substring s start end) -1)))
      (let loop ((ch (string-pointer-prev! sp)))
        (cond ((eof-object? ch) "")
              ((pred ch) (loop (string-pointer-prev! sp)))
              (else (string-pointer-next! sp)
                    (string-pointer-substring sp))))
      ))
  )

(define (string-trim-both s . args)
  (check-arg string? s)
  (let-optional* args ((c/s/p #[\s]) start end)
    (let ((pred (%get-char-pred c/s/p))
          (sp (make-string-pointer (%maybe-substring s start end))))
      (let loop ((ch (string-pointer-next! sp)))
        (cond ((eof-object? ch) "")
              ((pred ch) (loop (string-pointer-next! sp)))
              (else (string-pointer-prev! sp)
                    (let ((sp (make-string-pointer
                               (string-pointer-substring sp :after #t) -1)))
                      (let loop ((ch (string-pointer-prev! sp)))
                        (cond ((eof-object? ch) "")
                              ((pred ch) (loop (string-pointer-prev! sp)))
                              (else (string-pointer-next! sp)
                                    (string-pointer-substring sp))))
                      ))))
      ))
  )









