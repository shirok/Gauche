;;;
;;; srfi-13/searcher - string library (searchers)
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
;;;  $Id: searcher.scm,v 1.7 2004-03-14 01:42:42 shirok Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

(define (string-index s c/s/p . args)
  (check-arg string? s)
  (let ((pred (%get-char-pred c/s/p))
        (offset (if (pair? args) (car args) 0))
        (sp (apply make-string-pointer s 0 args)))
    (let loop ((ch (string-pointer-next! sp)))
      (cond ((eof-object? ch) #f)
            ((pred ch) (+ offset (- (string-pointer-index sp) 1)))
            (else (loop (string-pointer-next! sp)))))))

(define (string-index-right s c/s/p . args)
  (check-arg string? s)
  (let ((pred (%get-char-pred c/s/p))
        (offset (if (pair? args) (car args) 0))
        (sp (apply make-string-pointer s -1 args)))
    (let loop ((ch (string-pointer-prev! sp)))
      (cond ((eof-object? ch) #f)
            ((pred ch) (+ offset (string-pointer-index sp)))
            (else (loop (string-pointer-prev! sp)))))))

(define (string-skip s c/s/p . args)
  (check-arg string? s)
  (let ((pred (%get-char-pred c/s/p))
        (offset (if (pair? args) (car args) 0))
        (sp (apply make-string-pointer s 0 args)))
    (let loop ((ch (string-pointer-next! sp)))
      (cond ((eof-object? ch) #f)
            ((pred ch) (loop (string-pointer-next! sp)))
            (else (+ offset (- (string-pointer-index sp) 1)))))))

(define (string-skip-right s c/s/p . args)
  (check-arg string? s)
  (let ((pred (%get-char-pred c/s/p))
        (offset (if (pair? args) (car args) 0))
        (sp (apply make-string-pointer s -1 args)))
    (let loop ((ch (string-pointer-prev! sp)))
      (cond ((eof-object? ch) #f)
            ((pred ch) (loop (string-pointer-prev! sp)))
            (else (+ offset (string-pointer-index sp)))))))

(define (string-count s c/s/p . args)
  (check-arg string? s)
  (let ((pred (%get-char-pred c/s/p))
        (sp (apply make-string-pointer s 0 args)))
    (let loop ((ch (string-pointer-next! sp))
               (count 0))
      (cond ((eof-object? ch) count)
            ((pred ch) (loop (string-pointer-next! sp) (+ count 1)))
            (else      (loop (string-pointer-next! sp) count))))))

(define (string-contains s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (let-optionals* args ((start1 0) end1 start2 end2)
    (let* ((str1 (%maybe-substring s1 start1 end1))
           (str2 (%maybe-substring s2 start2 end2))
           (res  (string-scan str1 str2)))
      (and res (+ start1 res)))))

;; not tuned (maybe to be moved to native)
(define (string-contains-ci s1 s2 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (let-optionals* args ((start1 0) end1 start2 end2)
    (let ((str1 (string-upcase (%maybe-substring s1 start1 end1)))
          (str2 (string-upcase (%maybe-substring s2 start2 end2)))
          (res  (string-scan str1 str2)))
      (and res (+ start1 res)))))
