;;;
;;; srfi-13/revapp - string library (reverse, append)
;;;  
;;;   Copyright (c) 2000-2004 Shiro Kawai, All rights reserved.
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
;;;  $Id: revapp.scm,v 1.8 2004-02-25 11:11:20 shirok Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

(define (string-reverse s . args)
  (let ((sp (apply make-string-pointer s -1 args))
        (dst (open-output-string)))
    (let loop ((ch (string-pointer-prev! sp)))
      (if (eof-object? ch)
          (get-output-string dst)
          (begin (write-char ch dst)
                 (loop (string-pointer-prev! sp)))))
    ))

(define (string-reverse! s . args)
  (let-optionals* args ((start 0) end)
    (let ((rev (apply string-reverse s args)))
      (string-substitute! s start rev))))

(define (string-concatenate lis)
  (cond
   ((null? lis) "")
   ((not (pair? lis))
    (error "string-concatenate: argument ouf of domain:" lis))
   ((and (null? (cdr lis)) (string? (car lis)))
    (string-copy (car lis)))
   (else
    (let loop ((l lis)
               (out (open-output-string :private? #t))
               (incomplete? #f))
      (if (pair? l)
        (let ((e (car l)))
          (unless (string? e)
            (error "string-concatenate: argument contains non-string:" e))
          (display e out)
          (loop (cdr l) out (or incomplete? (string-incomplete? e))))
        (if incomplete?
          (string-complete->incomplete (get-output-string out))
          (get-output-string out)))))))

(define string-concatenate/shared string-concatenate)

(define string-append/shared string-append)

(define (string-concatenate-reverse list . args)
  (cond ((null? args)
         (string-concatenate (reverse list)))
        ((null? (cdr args))
         (string-concatenate (reverse (cons (car args) list))))
        (else
         (string-concatenate (reverse (cons (string-take (car args)
                                                         (cadr args))
                                            list))))))

(define string-concatenate-reverse/shared
  string-concatenate-reverse)


