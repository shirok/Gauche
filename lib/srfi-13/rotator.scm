;;;
;;; srfi-13/rotator - string library (rotation)
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
;;;  $Id: rotator.scm,v 1.6 2003-07-05 03:29:12 shirok Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

(define (xsubstring s from . args)
  (check-arg string? s)
  (check-arg (lambda (x) (and (integer? x) (exact? x))) from)
  (let-optionals* args (to start end)
    (let* ((str (%maybe-substring s start end))
           (len (string-length str))
           (from-rank (quotient from len))
           (from-mod  (modulo from len))
           (dest (open-output-string)))
      (cond ((undefined? to)
             (set! from from-mod)
             (set! to (+ from len)))
            ((not (and (integer? to) (exact? to)))
             (error "argument out of domain:" to))
            ((= len 0)
             (error "zero length source string is not allowed"))
            ((< to from)
             (errorf "argument out of range (from, to): (~s, ~s)" from to))
            (else
             (set! from from-mod)
             (set! to (- to (* from-rank len)))))
      (let ((sp (make-string-pointer str from)))
        (let loop ((count from)
                   (ch (string-pointer-next! sp)))
          (cond ((>= count to) (get-output-string dest))
                ((eof-object? ch)
                 (string-pointer-set! sp 0)
                 (write-char (string-pointer-next! sp) dest)
                 (loop (+ count 1) (string-pointer-next! sp)))
                (else
                 (write-char ch dest)
                 (loop (+ count 1) (string-pointer-next! sp)))))
        )
      ))
  )

(define (string-xcopy! target tstart s sfrom . args)
  (check-arg string? target)
  (check-arg (lambda (x) (and (integer? x) (exact? x))) tstart)
  (let ((result (apply xsubstring s sfrom args)))
    (string-substitute! target tstart result)))
