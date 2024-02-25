;;;
;;; Register set
;;;
;;;   Copyright (c) 2024  Shiro Kawai  <shiro@acm.org>
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

;; A common infrastructure to treat register set as a bitmap.
;; Each processor implementation should provide a vector of
;; register names (regvec).

;; Technically, we could implement this on top of srfi-209 (enums); however,
;; this module is used from Gauche internals and we don't want to depend
;; on another srfi.

(define-module lang.asm.regset
  (use util.match)
  (use gauche.bitvector)
  (export <regset> make-regset regset regset-copy
          regset->list list->regset
          regset-union regset-union!
          regset-difference regset-difference!
          regset-intersection regset-intersection!)
  )
(select-module lang.asm.regset)


(define-class <regset> (<sequence>)
  (;; all slots are private
   (%regvec :init-keyword :regvec)
   (%bits   :init-keyword :bits)))

(define-method write-object ((regset <regset>) port)
  (format port "#<regset ~:w>" (regset->list regset)))

;;; Constructors

(define (make-regset regvec :optional (regs '()))
  (assume-type regvec <vector>)
  (let1 rs (make <regset>
             :regvec regvec
             :bits (make-bitvector (vector-length regvec) 0))
    (if (null? regs)
      rs
      (apply regset-adjoin! rs regs))))

(define (regset regvec . regs) (make-regset regvec regs))

(define (regset-copy regset)
  (make <regset>
    :regvec (~ regset'%regvec)
    :bits (bitvector-copy (~ regset'%bits))))

;;; Internal utilities

(define (%regname->pos regvec reg)
  ;; NB: We roll our own to avoid depending on scheme.vector
  (let1 len (vector-length regvec)
    (let loop ([i 0])
      (cond [(= i len) (error "No such register:" reg)]
            [(eq? (vector-ref regvec i) reg) i]
            [else (loop (+ i 1))]))))

(define (%pos->regname regvec pos)
  (vector-ref regvec pos))

;;; Converters

(define (regset->list regset)
  (assume-type regset <regset>)
  (bitvector-value-map-index->list
   (cute %pos->regname (~ regset'%regvec))
   (~ regset'%bits)
   #t))

(define (list->regset regvec lis)
  (make-regset regvec lis))

;;; Set ops

(define (regset-adjoin! regset . regs)
  (define regvec (~ regset'%regvec))
  (let loop ([regs regs] [bv (~ regset'%bits)])
    (match regs
      [(reg . regs) (loop regs (bitvector-set! bv (%regname->pos regvec reg) 1))]
      [_ (set! (~ regset'%bits) bv) regset])))

(define (regset-adjoin regset . regs)
  (apply regset-adjoin! (regset-copy regset) regs))

(define (regset-union! regset1 . regsets)
  (let1 bits (apply bitvector-ior! (~ regset1'%bits)
                    (map (cut ~ <> '%bits) regsets))
    (set! (~ regset1'%bits) bits)
    regset1))

(define (regset-union regset1 . regsets)
  (apply regset-union! (regset-copy regset) regset1 regsets))

(define (regset-intersection! regset1 . regsets)
  (let1 bits (apply bitvector-and! (~ regset1'%bits)
                    (map (cut ~ <> '%bits) regsets))
    (set! (~ regset1'%bits) bits)
    regset1))

(define (regset-intersection regset1 . regsets)
  (apply regset-intersection! (regset-copy regset) regset1 regsets))

(define (regset-difference! regset1 . regsets)
  (let1 bits (fold (^[rs r] (bitvector-andc2! r (~ rs'%bits)))
                   (~ regset1'%bits)
                   regsets)
    (set! (~ regset1'%bits) bits)
    regset1))

(define (regset-difference regset1 regsets)
  (apply regset-difference (regset-copy regset) regsets))
