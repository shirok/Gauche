;;;
;;; srfi-13/casemap - string library
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
;;;  $Id: casemap.scm,v 1.5 2003-07-05 03:29:12 shirok Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

(define (string-upcase s . args)
  (let ((src (open-input-string (apply %maybe-substring s args)))
        (dst (open-output-string)))
    (let loop ((ch (read-char src)))
      (if (eof-object? ch)
          (get-output-string dst)
          (begin (write-char (char-upcase ch) dst)
                 (loop (read-char src)))))))

(define (string-downcase s . args)
  (let ((src (open-input-string (apply %maybe-substring s args)))
        (dst (open-output-string)))
    (let loop ((ch (read-char src)))
      (if (eof-object? ch)
          (get-output-string dst)
          (begin (write-char (char-downcase ch) dst)
                 (loop (read-char src)))))))

(define *%cased-char-set* #[A-Za-z]) ;; fixme

(define (string-titlecase s . args)
  (let ((src (open-input-string (apply %maybe-substring s args)))
        (dst (open-output-string)))
    (let loop ((title? #t)
               (ch   (read-char src)))
      (cond ((eof-object? ch) (get-output-string dst))
            ((char-set-contains? *%cased-char-set* ch)
             (if title?
                 (write-char (char-upcase ch) dst) ;; This should be char-titlecase
                 (write-char (char-downcase ch) dst))
             (loop #f (read-char src)))
            (else
             (write-char ch dst)
             (loop #t (read-char src))))
      )))

(define (string-upcase! s . args)
  (let-optionals* args ((start 0) end)
    (string-substitute! s start (string-upcase s start end))))

(define (string-downcase! s . args)
  (let-optionals* args ((start 0) end)
    (string-substitute! s start (string-downcase s start end))))

(define (string-titlecase! s . args)
  (let-optionals* args ((start 0) end)
    (string-substitute! s start (string-titlecase s start end))))

