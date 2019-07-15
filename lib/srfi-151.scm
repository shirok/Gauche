;;;
;;; srfi-151 - Bitwise operations
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

(define-module srfi-151
  (use srfi-1)
  (use srfi-60)
  (use srfi-133)
  (use gauche.generator)
  (export bitwise-not                   ; srfi-60
          bitwise-and                   ; srfi-60
          bitwise-ior                   ; srfi-60
          bitwise-xor                   ; srfi-60
          bitwise-eqv
          bitwise-nand  bitwise-nor 
          bitwise-andc1 bitwise-andc2
          bitwise-orc1  bitwise-orc2 

          arithmetic-shift              ; srfi-60
          bit-count                     ; builtin
          integer-length                ; builtin

          bitwise-if                    ; srfi-60
          bit-set?                      ; builtin
          copy-bit                      ; builtin
          bit-swap
          any-bit-set?
          every-bit-set?
          first-set-bit                 ; builtin

          bit-field                     ; builtin
          bit-field-any?
          bit-field-every?
          bit-field-clear
          bit-field-set
          bit-field-replace
          bit-field-replace-same
          bit-field-rotate
          bit-field-reverse

          bits->list
          list->bits
          bits->vector
          vector->bits
          bits
          bitwise-fold
          bitwise-for-each
          bitwise-unfold
          make-bitwise-generator))
(select-module srfi-151)

(define (%bitwise-eqv-2 a b) (lognot (logxor a b)))

(define bitwise-eqv
  (case-lambda
    [() -1]
    [(a) a]
    [(a b) (%bitwise-eqv-2 a b)]
    [(a b . args) (apply bitwise-eqv (%bitwise-eqv-2 a b) args)]))

;; TODO: These can be a lot more efficiently implemented natively, and
;; eventually, we'd rather optimize expressons like (lognot (logand a b))
;; into built-in bitwise-nand etc.
(define (bitwise-nand a b)  (lognot (logand a b)))
(define (bitwise-nor a b)   (lognot (logior a b)))
(define (bitwise-andc1 a b) (logand (lognot a) b))
(define (bitwise-andc2 a b) (logand a (lognot b)))
(define (bitwise-orc1 a b)  (logior (lognot a) b))
(define (bitwise-orc2 a b)  (logior a (lognot b)))

(define (bit-swap index1 index2 n)
  (let ([a (bit-set? index1 n)]
        [b (bit-set? index2 n)])
    (copy-bit index1 (copy-bit index2 n a) b)))

(define (any-bit-set? test-bits n)   (not (zero? (logand test-bits n))))
(define (every-bit-set? test-bits n) (= (logand test-bits n) test-bits))

(define (bit-field-any? n start end) (not (zero? (bit-field n start end))))
(define (bit-field-every? n start end) (zero? (bit-field (lognot n) start end)))

(define (bit-field-clear n start end) (copy-bit-field n 0 start end))
(define (bit-field-set n start end)   (copy-bit-field n -1 start end))

(define bit-field-replace copy-bit-field)

(define (bit-field-replace-same dst src start end)
  (copy-bit-field dst (bit-field src start end) start end))

(define bit-field-rotate rotate-bit-field) ;srfi-60
(define bit-field-reverse reverse-bit-field) ;srfi-60

(define (bits->list n . opts)
  (assume (not (negative? n)))
  (reverse (apply integer->list n opts)))

(define (bits->vector n :optional len)
  (assume (not (negative? n)))
  (vector-unfold (cut bit-set? <> n)
                 (if (undefined? len) (integer-length n) len)))

(define (list->bits bs)   (list->integer (reverse bs)))
(define (vector->bits bs) (list->integer (reverse-vector->list bs)))
(define (bits . bs) (list->bits bs))

(define (bitwise-fold proc seed n)
  (define len (integer-length n))
  (do ([len len (- len 1)]
       [n   n   (ash n -1)]
       [seed seed (proc (odd? n) seed)])
      [(<= len 0) seed]))

(define (bitwise-for-each proc n)
  (bitwise-fold (^[b _] (proc b)) 0 n))

(define (bitwise-unfold p f g seed)
  (do ([seed seed (g seed)]
       [i    0    (+ i 1)]
       [r    0    (copy-bit i r (f seed))])
      [(p seed) r]))

;; Unlike bits->generator in gauche.generator, this one is infinite.
(define (make-bitwise-generator n)
  (^[] (begin0 (odd? n) (set! n (ash n -1)))))

  
  
