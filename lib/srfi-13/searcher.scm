;;;
;;; srfi-13/searcher - string library (searchers)
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
;;;  $Id: searcher.scm,v 1.2 2001-05-02 08:20:25 shirok Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

(define (string-index s c/s/p . args)
  (check-arg string? s)
  (let ((pred (%get-char-pred c/s/p))
        (sp (make-string-pointer (apply %maybe-substring s args))))
    (let loop ((ch (string-pointer-next! sp)))
      (cond ((eof-object? ch) #f)
            ((pred ch) (- (string-pointer-index sp) 1))
            (else (loop (string-pointer-next! sp)))))))

(define (string-index-right s c/s/p . args)
  (check-arg string? s)
  (let ((pred (%get-char-pred c/s/p))
        (sp (make-string-pointer (apply %maybe-substring s args) -1)))
    (let loop ((ch (string-pointer-prev! sp)))
      (cond ((eof-object? ch) #f)
            ((pred ch) (string-pointer-index sp))
            (else (loop (string-pointer-prev! sp)))))))

(define (string-skip s c/s/p . args)
  (check-arg string? s)
  (let ((pred (%get-char-pred c/s/p))
        (sp (make-string-pointer (apply %maybe-substring s args))))
    (let loop ((ch (string-pointer-next! sp)))
      (cond ((eof-object? ch) #f)
            ((pred ch) (loop (string-pointer-next! sp)))
            (else (- (string-pointer-index sp) 1))))))

(define (string-skip-right s c/s/p . args)
  (check-arg string? s)
  (let ((pred (%get-char-pred c/s/p))
        (sp (make-string-pointer (apply %maybe-substring s args) -1)))
    (let loop ((ch (string-pointer-prev! sp)))
      (cond ((eof-object? ch) #f)
            ((pred ch) (loop (string-pointer-prev! sp)))
            (else (string-pointer-index sp))))))

(define (string-count s c/s/p . args)
  (check-arg string? s)
  (let ((pred (%get-char-pred c/s/p))
        (sp (make-string-pointer (apply %maybe-substring s args))))
    (let loop ((ch (string-pointer-next! sp))
               (count 0))
      (cond ((eof-object? ch) count)
            ((pred ch) (loop (string-pointer-next! sp) (+ count 1)))
            (else      (loop (string-pointer-next! sp) count))))))

(define (string-contains s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (let-optional* args (start1 end1 start2 end2)
    (let ((%string-contains (with-module gauche string-contains))
          (str1 (%maybe-substring s1 start1 end1))
          (str2 (%maybe-substring s2 start2 end2)))
      (%string-contains str1 str2))))

;; not tuned (maybe to be moved to native)
(define (string-contains-ci s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (let-optional* args (start1 end1 start2 end2)
    (let ((%string-contains (with-module gauche string-contains))
          (str1 (string-upcase (%maybe-substring s1 start1 end1)))
          (str2 (string-upcase (%maybe-substring s2 start2 end2))))
      (%string-contains str1 str2))))

