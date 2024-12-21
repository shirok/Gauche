;;;
;;; numioutil.scm - auxiliary numeric reader/writer
;;;
;;;   Copyright (c) 2000-2024  Shiro Kawai  <shiro@acm.org>
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

;; This module is not meant to be use'd.  This is used by
;; Gauche core to handle less-frequent numeric read/write operations.
;;
;; We splitted this from gauche.numutil, for we don't want these
;; routines to be imported to #<module gauche>.

(define-module gauche.numioutil
  (export print-exact-decimal-point-number
          )
  )
(select-module gauche.numioutil)

;; If a rational can be exactly represented with decimal-point notation,
;; return the string notation.  Otherwise, return #f.
;; This is called during number to string conversion.
;; We can extend this other than base 10, but for now, only decimal is
;; supported.
(define (print-exact-decimal-point-number n port)
  (assume-type n <rational>)
  (assume-type port <port>)
  (and-let* ([denom (denominator n)]
             [k2 (twos-exponent-factor denom)]
             [d2 (quotient denom (ash 1 k2))]
             [k5 (let factorize-5 ([d d2] [k 0])
                   (if (= d 1)
                     k
                     (receive (q r) (quotient&remainder d 5)
                       (and (zero? r) (factorize-5 q (+ k 1))))))]
             ;; now that denominator is 2^{k2} * 5^{k5}.  adjust it
             ;; to 10^{k}.
             [k (max k2 k5)]
             [f (if (< k2 k5) (expt 2 (- k5 k2)) (expt 5 (- k2 k5)))]
             [number (* f (numerator n))]
             [abs-number (abs number)]
             [s (number->string abs-number 10)]
             [slen (string-length s)])
    (receive (digits diglen)
        (if (> slen k)
          (values s slen)
          (values (string-append (make-string (- (+ k 1) slen) #\0) s)
                  (+ k 1)))
      (display "#e" port)
      (when (negative? number) (display "-" port))
      ;; NB: Avoid string-take-right etc., for it will depend on srfi.13
      (display (substring digits 0 (- diglen k)))
      (display ".")
      (display (substring digits (- diglen k) diglen))
      (+ diglen 3 (if (negative? number) 1 0)))))
