;;;
;;; srfi-13/selector - string library (selectors)
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
;;;  $Id: selector.scm,v 1.8 2003-07-05 03:29:12 shirok Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

(define substring/shared string-copy)  ; same in Gauche

(define (string-copy! target tstart s . args)
  (check-arg string? target)
  (check-arg (lambda (x) (and (integer? x) (positive? x))) tstart)
  (let* ((str (apply %maybe-substring s args))
         (slen (string-length str))
         (tlen (string-length target)))
    (when (> (+ tstart slen) tlen)
      (error "copy operation runs off the target string:" target))
    (string-substitute! target tstart str)))

(define (string-pad s len . args)
  (let-optionals* args ((char #\space) start end)
    (check-arg char? char)
    (let* ((str (%maybe-substring s start end))
           (slen (string-length str)))
      (cond ((< slen len)
             (string-append (make-string (- len slen) char) str))
            ((> slen len)
             (string-take-right str len))
            (else str)))))

(define (string-pad-right s len . args)
  (let-optionals* args ((char #\space) start end)
    (check-arg char? char)
    (let* ((str (%maybe-substring s start end))
           (slen (string-length str)))
      (cond ((< slen len)
             (string-append str (make-string (- len slen) char)))
            ((> slen len)
             (string-take str len))
            (else str)))))

(define (string-take s nchars)
  (check-arg string? s)
  (when (or (< nchars 0) (> nchars (string-length s)))
    (error "argument out of range:" nchars))
  (%maybe-substring s 0 nchars))

(define (string-drop s nchars)
  (check-arg string? s)
  (when (or (< nchars 0) (> nchars (string-length s)))
    (error "argument out of range:" nchars))
  (%maybe-substring s nchars))

(define (string-take-right s nchars)
  (check-arg string? s)
  (when (or (< nchars 0) (> nchars (string-length s)))
    (error "argument out of range:" nchars))
  (%maybe-substring s (- (string-length s) nchars)))

(define (string-drop-right s nchars)
  (check-arg string? s)
  (when (or (< nchars 0) (> nchars (string-length s)))
    (error "argument out of range:" nchars))
  (%maybe-substring s 0 (- (string-length s) nchars)))

(define (string-trim s . args)
  (check-arg string? s)
  (let-optionals* args ((c/s/p #[\s]) start end)
    (let ((pred (%get-char-pred c/s/p))
          (sp (make-string-pointer (%maybe-substring s start end))))
      (let loop ((ch (string-pointer-next! sp)))
        (cond ((eof-object? ch) "")
              ((pred ch) (loop (string-pointer-next! sp)))
              (else (string-pointer-prev! sp)
                    (string-pointer-substring sp :after #t))))
      ))
  )

(define (string-trim-right s . args)
  (check-arg string? s)
  (let-optionals* args ((c/s/p #[\s]) start end)
    (let ((pred (%get-char-pred c/s/p))
          (sp (make-string-pointer (%maybe-substring s start end) -1)))
      (let loop ((ch (string-pointer-prev! sp)))
        (cond ((eof-object? ch) "")
              ((pred ch) (loop (string-pointer-prev! sp)))
              (else (string-pointer-next! sp)
                    (string-pointer-substring sp))))
      ))
  )

(define (string-trim-both s . args)
  (check-arg string? s)
  (let-optionals* args ((c/s/p #[\s]) start end)
    (let ((pred (%get-char-pred c/s/p))
          (sp (make-string-pointer (%maybe-substring s start end))))
      (let loop ((ch (string-pointer-next! sp)))
        (cond ((eof-object? ch) "")
              ((pred ch) (loop (string-pointer-next! sp)))
              (else (string-pointer-prev! sp)
                    (let ((sp (make-string-pointer
                               (string-pointer-substring sp :after #t) -1)))
                      (let loop ((ch (string-pointer-prev! sp)))
                        (cond ((eof-object? ch) "")
                              ((pred ch) (loop (string-pointer-prev! sp)))
                              (else (string-pointer-next! sp)
                                    (string-pointer-substring sp))))
                      ))))
      ))
  )









