;;;
;;; srfi-13/comparer - string library (comparison)
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
;;;  $Id: comparer.scm,v 1.1 2001-04-27 09:22:27 shirok Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

(define (%string-compare-int off str1 str2 cmp< cmp> proc< proc= proc>)
  (let ((sp1 (make-string-pointer str1))
        (sp2 (make-string-pointer str2)))
    (let loop ((ch1 (string-pointer-next! sp1))
               (ch2 (string-pointer-next! sp2)))
      (cond ((eof-object? ch1)
             (if (eof-object? ch2)
                 (proc= (+ off (string-pointer-index sp1)))
                 (proc< (+ off (string-pointer-index sp1)))))
            ((eof-object? ch2)
             (proc> (+ off (string-pointer-index sp1))))
            ((cmp< ch1 ch2) (proc< (+ off (string-pointer-index sp1))))
            ((cmp> ch1 ch2) (proc> (+ off (string-pointer-index sp1))))
            (else (loop (string-pointer-next! sp1)
                        (string-pointer-next! sp2))))
      )))

(define (string-compare s1 s2 proc< proc= proc> . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (let-optional* args ((start1 0) end1 start2 end2)
    (let ((str1 (%maybe-substring s1 start1 end1))
          (str2 (%maybe-substring s2 start1 end1)))
      (%string-compare-int start1 str1 str2
                           char<? char>?
                           proc< proc= proc>))))

(define (string-compare-ci s1 s2 proc< proc= proc> . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (let-optional* args ((start1 0) end1 start2 end2)
    (let ((str1 (%maybe-substring s1 start1 end1))
          (str2 (%maybe-substring s2 start1 end1)))
      (%string-compare-int start1 str1 str2
                           char-ci<? char-ci>?
                           proc< proc= proc>))))

(define (string= s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare
         s1 s2 (lambda () #f) (lambda () #t) (lambda () #f)
         args))

(define (string<> s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare
         s1 s2 (lambda () #t) (lambda () #f) (lambda () #t)
         args))

(define (string< s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare
         s1 s2 (lambda () #t) (lambda () #f) (lambda () #f)
         args))

(define (string<= s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare
         s1 s2 (lambda () #t) (lambda () #t) (lambda () #f)
         args))

(define (string> s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare
         s1 s2 (lambda () #f) (lambda () #f) (lambda () #t)
         args))

(define (string>= s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare
         s1 s2 (lambda () #f) (lambda () #t) (lambda () #t)
         args))

(define (string-ci= s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare-ci
         s1 s2 (lambda () #f) (lambda () #t) (lambda () #f)
         args))

(define (string-ci<> s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare-ci
         s1 s2 (lambda () #t) (lambda () #f) (lambda () #t)
         args))

(define (string-ci< s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare-ci
         s1 s2 (lambda () #t) (lambda () #f) (lambda () #f)
         args))

(define (string-ci<= s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare-ci
         s1 s2 (lambda () #t) (lambda () #t) (lambda () #f)
         args))

(define (string-ci> s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare-ci
         s1 s2 (lambda () #f) (lambda () #f) (lambda () #t)
         args))

(define (string-ci>= s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare-ci
         s1 s2 (lambda () #f) (lambda () #t) (lambda () #t)
         args))

