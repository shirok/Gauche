;;;
;;; srfi-13/prefix - string library (prefix/suffix)
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
;;;  $Id: prefix.scm,v 1.3 2001-06-29 20:32:47 shirok Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

(define (%string-prefix-int str1 str2 = action)
  (let ((sp1 (make-string-pointer str1))
        (sp2 (make-string-pointer str2)))
    (let loop ((ch1 (string-pointer-next! sp1))
               (ch2 (string-pointer-next! sp2)))
      (cond ((eof-object? ch1) (action (string-pointer-index sp1) #t))
            ((eof-object? ch2) (action (string-pointer-index sp1) #f))
            ((= ch1 ch2) (loop (string-pointer-next! sp1)
                               (string-pointer-next! sp2)))
            (else (action (- (string-pointer-index sp1) 1) #f)))
      )))

(define (string-prefix-length s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (let-optionals* args (start1 end1 start2 end2)
    (let ((str1 (%maybe-substring s1 start1 end1))
          (str2 (%maybe-substring s2 start2 end2)))
      (%string-prefix-int str1 str2 char=? (lambda (cnt flag) cnt)))))

(define (string-prefix-length-ci s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (let-optionals* args (start1 end1 start2 end2)
    (let ((str1 (%maybe-substring s1 start1 end1))
          (str2 (%maybe-substring s2 start2 end2)))
      (%string-prefix-int str1 str2 char-ci=? (lambda (cnt flag) cnt)))))

(define (string-prefix? s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (let-optionals* args (start1 end1 start2 end2)
    (let ((str1 (%maybe-substring s1 start1 end1))
          (str2 (%maybe-substring s2 start2 end2)))
      (%string-prefix-int str1 str2 char=? (lambda (cnt flag) flag)))))

(define (string-prefix-ci? s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (let-optionals* args (start1 end1 start2 end2)
    (let ((str1 (%maybe-substring s1 start1 end1))
          (str2 (%maybe-substring s2 start2 end2)))
      (%string-prefix-int str1 str2 char-ci=? (lambda (cnt flag) flag)))))



(define (%string-suffix-int str1 str2 = action)
  (let ((sp1 (make-string-pointer str1 -1))
        (sp2 (make-string-pointer str2 -1)))
    (let loop ((ch1 (string-pointer-prev! sp1))
               (ch2 (string-pointer-prev! sp2)))
      (cond ((eof-object? ch1) (action (string-pointer-index sp1) #t))
            ((eof-object? ch2) (action (string-pointer-index sp1) #f))
            ((= ch1 ch2) (loop (string-pointer-prev! sp1)
                               (string-pointer-prev! sp2)))
            (else (action (+ (string-pointer-index sp1) 1) #f)))
      )))

(define (string-suffix-length s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (let-optionals* args (start1 end1 start2 end2)
    (let ((str1 (%maybe-substring s1 start1 end1))
          (str2 (%maybe-substring s2 start2 end2)))
      (%string-suffix-int str1 str2 char=? (lambda (cnt flag) cnt)))))

(define (string-suffix-length-ci s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (let-optionals* args (start1 end1 start2 end2)
    (let ((str1 (%maybe-substring s1 start1 end1))
          (str2 (%maybe-substring s2 start2 end2)))
      (%string-suffix-int str1 str2 char-ci=? (lambda (cnt flag) cnt)))))

(define (string-suffix? s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (let-optionals* args (start1 end1 start2 end2)
    (let ((str1 (%maybe-substring s1 start1 end1))
          (str2 (%maybe-substring s2 start2 end2)))
      (%string-suffix-int str1 str2 char=? (lambda (cnt flag) flag)))))

(define (string-suffix-ci? s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (let-optionals* args (start1 end1 start2 end2)
    (let ((str1 (%maybe-substring s1 start1 end1))
          (str2 (%maybe-substring s2 start2 end2)))
      (%string-suffix-int str1 str2 char-ci=? (lambda (cnt flag) flag)))))



