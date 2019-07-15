;;;
;;; srfi-143 - Fixnums
;;;
;;;   Copyright (c) 2017-2019  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi-143
  (use srfi-60 :only (bitwise-if rotate-bit-field reverse-bit-field))
  (use srfi-141 :only (balanced/))
  (export fx-width fx-greatest fx-least
          fixnum? fx=? fx<? fx<=? fx>? fx>=?
          fxzero? fxpositive? fxnegative? fxodd? fxeven?
          fxmax fxmin
          fx+ fx- fxneg fx* fxquotient fxremainder fxabs fxsquare fxsqrt
          fxnot fxand fxior fxxor fxarithmetic-shift
          fxarithmetic-shift-left fxarithmetic-shift-right
          fxbit-count fxlength fxif fxbit-set?
          fxcopy-bit fxfirst-set-bit
          fxbit-field fxbit-field-rotate fxbit-field-reverse
          fx+/carry fx-/carry fx*/carry))
(select-module srfi-143)
          
;; We don't do define-constant, for these may differ among platforms
;; and making them constant would interfere with cross-compilation
(define fx-width (fixnum-width))
(define fx-greatest (greatest-fixnum))
(define fx-least (least-fixnum))

;; fixnum? - builtin

;; In Gauche, using standard operators is the most efficient.
(define-inline fx=?  =)
(define-inline fx<?  <)
(define-inline fx<=? <=)
(define-inline fx>?  >)
(define-inline fx>=? >=)
(define-inline fxzero? zero?)
(define-inline fxpositive? positive?)
(define-inline fxnegative? negative?)
(define-inline fxodd?  odd?)
(define-inline fxeven? even?)
(define-inline fxmax   max)
(define-inline fxmin   min)

(define-inline (fx+ i j) (+ i j))
(define-inline (fx- i j) (- i j))
(define-inline (fxneg i) (- i))
(define-inline (fx* i j) (* i j))
(define-inline (fxquotient  i j) (quotient i j))
(define-inline (fxremainder i j) (remainder i j))
(define-inline (fxabs i) (abs i))
(define-inline (fxsquare i) (square i))
(define-inline (fxsqrt i)  (exact-integer-sqrt i))

(define-inline (fxnot i)   (lognot i))
(define-inline fxand       logand)
(define-inline fxior       logior)
(define-inline fxxor       logxor)
(define-inline (fxarithmetic-shift i c) (ash i c))
(define-inline (fxarithmetic-shift-left i c) (ash i c))
(define-inline (fxarithmetic-shift-right i c) (ash i (- c)))
(define-inline (fxbit-count i) (logcount i))
(define-inline (fxlength i) (integer-length i))
(define-inline (fxif mask i j) (bitwise-if mask i j))
(define-inline (fxbit-set? index i) (logbit? index i))
(define-inline (fxcopy-bit index i boolean) (copy-bit index i boolean))
(define-inline (fxfirst-set-bit i) (twos-exponent-factor i))
(define-inline (fxbit-field i start end) (bit-field i start end))
(define-inline (fxbit-field-rotate i count start end)
  (rotate-bit-field i count start end))
(define-inline (fxbit-field-reverse i start end)
  (reverse-bit-field i start end))

;; Procedures that require additional definitions
;; Can be more efficient.

(define *modulo* (%expt 2 (fixnum-width)))

(define (fx+/carry i j k)
  (let1 v (+ i j k)
    (receive (q r) (balanced/ v *modulo*)
      (values r q))))

(define (fx-/carry i j k)
  (let1 v (- i j k)
    (receive (q r) (balanced/ v *modulo*)
      (values r q))))

(define (fx*/carry i j k)
  (let1 v (+ (* i j) k)
    (receive (q r) (balanced/ v *modulo*)
      (values r q))))





