;;;
;;; srfi-13/comparer - string library (comparison)
;;;  
;;;   Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  
;;;  $Id: comparer.scm,v 1.5 2003-07-05 03:29:12 shirok Exp $
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
  (let-optionals* args ((start1 0) end1 start2 end2)
    (let ((str1 (%maybe-substring s1 start1 end1))
          (str2 (%maybe-substring s2 start2 end2)))
      (%string-compare-int (- start1 1) str1 str2
                           char<? char>?
                           proc< proc= proc>))))

(define (string-compare-ci s1 s2 proc< proc= proc> . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (let-optionals* args ((start1 0) end1 start2 end2)
    (let ((str1 (%maybe-substring s1 start1 end1))
          (str2 (%maybe-substring s2 start2 end2)))
      (%string-compare-int (- start1 1) str1 str2
                           char-ci<? char-ci>?
                           proc< proc= proc>))))

(define (string= s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare
         s1 s2 (lambda (_) #f) (lambda (_) #t) (lambda (_) #f)
         args))

(define (string<> s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare
         s1 s2 (lambda (_) #t) (lambda (_) #f) (lambda (_) #t)
         args))

(define (string< s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare
         s1 s2 (lambda (_) #t) (lambda (_) #f) (lambda (_) #f)
         args))

(define (string<= s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare
         s1 s2 (lambda (_) #t) (lambda (_) #t) (lambda (_) #f)
         args))

(define (string> s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare
         s1 s2 (lambda (_) #f) (lambda (_) #f) (lambda (_) #t)
         args))

(define (string>= s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare
         s1 s2 (lambda (_) #f) (lambda (_) #t) (lambda (_) #t)
         args))

(define (string-ci= s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare-ci
         s1 s2 (lambda (_) #f) (lambda (_) #t) (lambda (_) #f)
         args))

(define (string-ci<> s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare-ci
         s1 s2 (lambda (_) #t) (lambda (_) #f) (lambda (_) #t)
         args))

(define (string-ci< s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare-ci
         s1 s2 (lambda (_) #t) (lambda (_) #f) (lambda (_) #f)
         args))

(define (string-ci<= s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare-ci
         s1 s2 (lambda (_) #t) (lambda (_) #t) (lambda (_) #f)
         args))

(define (string-ci> s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare-ci
         s1 s2 (lambda (_) #f) (lambda (_) #f) (lambda (_) #t)
         args))

(define (string-ci>= s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (apply string-compare-ci
         s1 s2 (lambda (_) #f) (lambda (_) #t) (lambda (_) #t)
         args))

