;;;
;;; srfi-13/prefix - string library (prefix/suffix)
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
;;;  $Id: prefix.scm,v 1.4 2003-07-05 03:29:12 shirok Exp $
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



