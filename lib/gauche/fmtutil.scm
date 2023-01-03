;;;
;;; gauche.fmtutil - less-used format features
;;;
;;;   Copyright (c) 2023  Shiro Kawai  <shiro@acm.org>
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

;; This module is autoloaded.

(define-module gauche.fmtutil
  (use util.match)
  (export format-numeral-R))
(select-module gauche.fmtutil)

;; Entry point
(define (format-numeral-R port value atflag colon upcase?)
  (if atflag
    (format-roman port value colon upcase?)
    (format-english port value colon upcase?)))

(define (format-english port value ordinal? upcase?)
  (error "English notation is not implemented yet"))


;; Unicode roman numerals
(define *rn-1000* #\u2180)             ; just in case; usually "M"
(define *rn-5000* #\u2181)
(define *rn-10000* #\u2182)
(define *rn-50000* #\u2187)
(define *rn-100000* #\u2188)

;; Roman numerals
(define *rns*
  `((10000 ,*rn-10000* ,*rn-50000* ,*rn-100000*)
    ( 1000 #\m ,*rn-5000* ,*rn-10000*)
    (  100 #\c #\d #\m)
    (   10 #\x #\l #\c)
    (    1 #\i #\v #\x)))

(define (format-roman port value old? upcase?)
  (define (P c)
    (if upcase?
      (display (char-upcase c) port)
      (display c port)))
  (assume (and (exact-integer? value)
               (< 0 value 1000000))
          "Value is out of domain for Roman numeral:" value)
  (when (>= value 100000)
    (dotimes [(quotient value 100000)] (P *rn-100000*)))
  (let loop ([value (modulo value 100000)]
             [rns *rns*])
    (unless (null? rns)
      (match-let1 [divisor i v x] (car rns)
        (let1 digit (quotient value divisor)
          (cond
           [(<= 1 digit 3) (dotimes [digit] (P i))]
           [(= digit 4) (if old?
                          (dotimes [4] (P i))
                          (begin (P i) (P v)))]
           [(<= 5 digit 8) (P v) (dotimes [(- digit 5)] (P i))]
           [(= digit 9) (if old?
                          (begin (P v) (dotimes [4] (P i)))
                          (begin (P i) (P x)))]))
        (loop (modulo value divisor) (cdr rns))))))
