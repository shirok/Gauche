;;;
;;; srfi-13/misc - string library (miscellaneous)
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
;;;  $Id: misc.scm,v 1.5 2003-07-05 03:29:12 shirok Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

(define (string-replace s1 s2 start1 end1 . args)
  (check-arg string? s1)
  (check-arg string? s2)
  (string-append (substring s1 0 start1)
                 (apply %maybe-substring s2 args)
                 (substring s1 end1 (string-length s1))))

(define (string-tokenize s . args)
  (check-arg string? s)
  (let-optionals* args ((token-set #[\S]) start end)
    (letrec ((src (open-input-string (%maybe-substring s start end)))
             (in-word (lambda (ch dst r)
                        (cond ((eof-object? ch)
                               (reverse! (cons (get-output-string dst) r)))
                              ((char-set-contains? token-set ch)
                               (write-char ch dst)
                               (in-word (read-char src) dst r))
                              (else
                               (out-word (read-char src)
                                         (cons (get-output-string dst) r))))))
             (out-word (lambda (ch r)
                         (cond ((eof-object? ch) (reverse! r))
                               ((char-set-contains? token-set ch)
                                (let ((dst (open-output-string)))
                                  (write-char ch dst)
                                  (in-word (read-char src) dst r)))
                               (else
                                (out-word (read-char src) r))))))
      (out-word (read-char src) '()))))


  