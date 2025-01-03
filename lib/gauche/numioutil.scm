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

(define-module gauche.numioutil)
(select-module gauche.numioutil)

;; Internal API - Directly called from writer.c
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

;; Repeating decimals
;;   We support notation of 1.2#34 as 1.23434343434...
;;
;; - The caller deals with numeric prefixes and exponent part, so
;;   the 'main' numeric part, which consists of digits, '#', '.',
;;   and '_', is passed.
;; - Returns either a rational or #f.

(define (read-repeating-decimal word)
  (define (digits&scale deci) ; "12.3#4" -> "123#4" & 2
    (if-let1 m (#/\./ deci)
      (let* ([integ (m 'before)]
             [frac (m 'after)]
             [digits (string-append integ frac)])
        (if (string-scan frac #\#)
          (values digits (- (string-length frac) 1))
          (values digits (string-length frac))))
      (values deci 0)))
  (define (split-repeats digits) ; "123#45" -> (* (+ 123 45/99) 100)
    (receive (pre post) (string-scan digits #\# 'both)
      (let* ([ndigs-repeating (string-length post)]
             [factor (expt 10 ndigs-repeating)]
             [repeating (string->number post)]
             [non-repeating (if (equal? pre "")
                              0
                              (string->number pre))])
        (* (+ non-repeating (/ repeating (- factor 1))) factor))))
  ;; avoid depending on srfi.13
  (define (strcount str char)
    (define e (string-cursor-end str))
    (let loop ((c (string-cursor-start str))
               (count 0))
      (cond [(string-cursor=? c e) count]
            [(char=? (string-ref str c) char)
             (loop (string-cursor-next str c) (+ count 1))]
            [else
             (loop (string-cursor-next str c) count)])))

  (assume-type word <string>)
  (and (not (#/__/ word))               ;don't allow consecutive '_'
       (<= (strcount word #\.) 1)
       (= (strcount word #\#) 1)
       (receive (digits scale) (digits&scale (regexp-replace-all #/_/ word ""))
         (* (split-repeats digits) (expt 10 (- scale))))))
