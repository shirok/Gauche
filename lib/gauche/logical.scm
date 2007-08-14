;;;
;;; logical.scm - logical (bitwise) operations.  to be autoloaded.
;;;  
;;;   Copyright (c) 2000-2007  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: logical.scm,v 1.5 2007-08-14 01:00:44 shirok Exp $
;;;

(select-module gauche)

;; SLIB compatible interface.

(define (logtest . args)  ;; can be optimized for two-arg case
  (not (zero? (apply logand args))))

(define (logbit? index n)
  (not (zero? (logand n (ash 1 index)))))

(define (copy-bit index from bit)
  (if bit
      (logior (ash 1 index) from)
      (logand (lognot (ash 1 index)) from)))

(define (bit-field n start end)
  (check-arg integer? start)
  (check-arg integer? end)
  (if (< start end)
      (let ((mask (- (ash 1 (- end start)) 1)))
        (logand (ash n (- start)) mask))
      0))

(define (copy-bit-field to start end from)
  (check-arg integer? start)
  (check-arg integer? end)
  (if (< start end)
      (let ((mask (- (ash 1 (- end start)) 1)))
        (logior (logand to (lognot (ash mask start)))
                (ash (logand from mask) start)))
      from))

;;; The following code uses the algorithm from SLIB's logical.scm,
;;; adapted to Gauche

;;;; "logical.scm", bit access and operations for integers for Scheme
;;; Copyright (C) 1991, 1993 Aubrey Jaffer.
;
;Permission to copy this software, to redistribute it, and to use it
;for any purpose is granted, subject to the following restrictions and
;understandings.
;
;1.  Any copy made of this software must include this copyright notice
;in full.
;
;2.  I have made no warrantee or representation that the operation of
;this software will be error-free, and I am under no obligation to
;provide any services, by way of maintenance, update, or otherwise.
;
;3.  In conjunction with products arising from the use of this
;material, there shall be no use of my name in any advertising,
;promotional, or sales literature without prior written consent in
;each case.

(define (integer-length n)
  (check-arg integer? n)
  (letrec ((rec
            (lambda (n)
              (case n
                ((0 -1) 0)
                ((1 -2) 1)
                ((2 3 -3 -4) 2)
                ((4 5 6 7 -5 -6 -7 -8) 3)
                (else (+ 4 (rec (ash n -4))))))))
    (rec n)))
