;;;
;;; srfi-13/folder - string library (generic mappers)
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
;;;  $Id: folder.scm,v 1.3 2001-05-03 10:28:38 shirok Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

(define (string-map proc s . args)
  (check-arg procedure? proc)
  (check-arg string? s)
  (let ((src (open-input-string (apply %maybe-substring s args)))
        (dest (open-output-string)))
    (let loop ((ch (read-char src)))
      (if (eof-object? ch)
          (get-output-string dest)
          (begin (write-char (proc ch) dest)
                 (loop (read-char src)))))
    ))

(define (string-map! proc s . args)
  (check-arg procedure? proc)
  (check-arg string? s)
  (let-optional* args ((start 0) end)
     (let ((mapped (apply string-map proc s args)))
       (string-substitute! s start mapped))))

(define (string-fold kons knil s . args)
  (check-arg procedure? kons)
  (check-arg string? s)
  (let ((src (open-input-string (apply %maybe-substring s args))))
    (let loop ((ch (read-char src))
               (r  knil))
      (if (eof-object? ch)
          r
          (loop (read-char src) (kons ch r))))
    ))

(define (string-fold-right kons knil s . args)
  (check-arg procedure? kons)
  (check-arg string? s)
  (let ((src (make-string-pointer (apply %maybe-substring s args) -1)))
    (let loop ((ch (string-pointer-prev! src))
               (r  knil))
      (if (eof-object? ch)
          r
          (loop (string-pointer-prev! src) (kons ch r))))
    ))

(define (string-unfold p f g seed . args)
  (check-arg procedure? p)
  (check-arg procedure? f)
  (check-arg procedure? g)
  (let-optional* args ((base "") (make-final (lambda (_) "")))
    (let ((dest (open-output-string)))
      (display base dest)
      (let loop ((seed seed))
        (if (p seed)
            (begin (display (make-final seed) dest)
                   (get-output-string dest))
            (begin (write-char (f seed) dest)
                   (loop (g seed))))))
    ))

(define (string-unfold-right p f g seed . args)
  (check-arg procedure? p)
  (check-arg procedure? f)
  (check-arg procedure? g)
  (let-optional* args ((base "") (make-final (lambda (_) "")))
    (let ((dest (open-output-string)))
      (let loop ((seed seed))
        (if (p seed)
            (string-append (make-final seed)
                           (string-reverse (get-output-string dest))
                           base)
            (begin (write-char (f seed) dest)
                   (loop (g seed))))))
    ))

(define (string-for-each proc s . args)
  (check-arg procedure? proc)
  (check-arg string? s)
  (let ((src (open-input-string (apply %maybe-substring s args))))
    (let loop ((ch (read-char src)))
      (unless (eof-object? ch)
        (proc ch)
        (loop (read-char src)))))
  )

(define (string-for-each-index proc s . args)
  (check-arg procedure? proc)
  (check-arg string? s)
  (let-optional* args ((start 0) (end (string-length s)))
    (do ((i start (+ i 1)))
        ((>= i end))
      (proc i)))
  )




    

