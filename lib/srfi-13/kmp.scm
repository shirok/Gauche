;;;
;;; srfi-13/kmp - string library (Knuth-Morris-Pratt search)
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
;;;  $Id: kmp.scm,v 1.4 2003-07-05 03:29:12 shirok Exp $
;;;

;; Say `(use srfi-13)' and this file will be autoloaded on demand.

(select-module srfi-13)

;; Knuth-Morris-Pratt search constructs.
;;
;; The SRFI-13 specification assumes accessing the pattern by index is
;; a lightweight operation, but it may not be true in Gauche if the pattern
;; contains multibyte characters.  So the programs using these functions
;; may not be very efficient, in spite of the efforts for efficiency put
;; in the original SRFI design.

(define (make-kmp-restart-vector s . args)
  (let-optionals* args ((c= char=?) start end)
    (let* ((pat (%maybe-substring s start end))
           (rv (make-vector (string-length pat) -1))
           (plen (string-length pat))
           (plen-1 (- plen 1)))
      (do ((i 0 (+ i 1)))
          ((= i plen-1) rv)
        (let loop ((k (+ (vector-ref rv i) 1)))
          (if (and (> k 0)
                   (not (c= (string-ref pat i) (string-ref pat (- k 1)))))
              (loop (+ (vector-ref rv (- k 1)) 1))
              (vector-set! rv (+ i 1) k)))
        )
      )))

(define (kmp-step pat rv c i c= p-start)
  (let loop ((i i))
    (if (c= c (string-ref pat (+ i p-start)))
        (+ i 1)
        (let ((i (vector-ref rv i)))
          (if (= i -1) 0 (loop i))))))

;; This is inefficient if input string s contains multibyte characters.
(define (string-kmp-partial-search pat rv s i . args)
  (let-optionals* args ((c= char=?) (p-start 0) start end)
    (let ((patlen (vector-length rv)))
      (let lp ((si s-start)
               (vi i))
        (cond ((= vi patlen) (- si)) 
              ((= si end) vi)
              (else (lp (+ si 1)
                        (kmp-step pat rv (string-ref s si)
                                  vi c= p-start))))))))
