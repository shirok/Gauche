;;;
;;; srfi-13/folder - string library (generic mappers)
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
;;;  $Id: folder.scm,v 1.6 2003-07-05 03:29:12 shirok Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

(define (string-map proc s . args)
  (check-arg procedure? proc)
  (check-arg string? s)
  (let ((src  (apply make-string-pointer s 0 args))
        (dest (open-output-string)))
    (let loop ((ch (string-pointer-next! src)))
      (if (eof-object? ch)
          (get-output-string dest)
          (begin (write-char (proc ch) dest)
                 (loop (string-pointer-next! src)))))
    ))

(define (string-map! proc s . args)
  (check-arg procedure? proc)
  (check-arg string? s)
  (let-optionals* args ((start 0) end)
     (let ((mapped (apply string-map proc s args)))
       (string-substitute! s start mapped))))

(define (string-fold kons knil s . args)
  (check-arg procedure? kons)
  (check-arg string? s)
  (let ((src (apply make-string-pointer s 0 args)))
    (let loop ((ch (string-pointer-next! src))
               (r  knil))
      (if (eof-object? ch)
          r
          (loop (string-pointer-next! src) (kons ch r))))
    ))

(define (string-fold-right kons knil s . args)
  (check-arg procedure? kons)
  (check-arg string? s)
  (let ((src (apply make-string-pointer s -1 args)))
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
  (let-optionals* args ((base "") (make-final (lambda (_) "")))
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
  (let-optionals* args ((base "") (make-final (lambda (_) "")))
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
  (let ((src (apply make-string-pointer s 0 args)))
    (let loop ((ch (string-pointer-next! src)))
      (unless (eof-object? ch)
        (proc ch)
        (loop (string-pointer-next! src)))))
  )

(define (string-for-each-index proc s . args)
  (check-arg procedure? proc)
  (check-arg string? s)
  (let-optionals* args ((start 0) (end (string-length s)))
    (do ((i start (+ i 1)))
        ((>= i end))
      (proc i)))
  )
