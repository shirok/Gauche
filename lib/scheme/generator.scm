;;;
;;; scheme.generator - Generators and Accumulators (R7RS Tangerine)
;;;
;;;   Copyright (c) 2020-2022  Shiro Kawai  <shiro@acm.org>
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

;; Originally srfi-158
;; (In R7RS Red edition, it was srfi-121).
;; Generator library is built-in.   We provide accumulator implementation here.

(define-module scheme.generator
  (use gauche.unicode)
  (use gauche.generator)
  (export generator circular-generator make-iota-generator make-range-generator
          make-coroutine-generator list->generator vector->generator
          reverse-vector->generator string->generator
          bytevector->generator
          make-for-each-generator make-unfold-generator
          gcons* gappend gflatten ggroup gmerge gmap gcombine gfilter gremove
          gstate-filter ggroup generator-map->list
          gtake gdrop gtake-while gdrop-while
          gdelete gdelete-neighbor-dups gindex gselect
          generator->list generator->reverse-list generator-map->list
          generator->vector generator->vector!  generator->string
          generator-fold generator-for-each generator-find
          generator-count generator-any generator-every generator-unfold

          ;; accumulators
          make-accumulator count-accumulator list-accumulator
          reverse-list-accumulator vector-accumulator
          reverse-vector-accumulator vector-accumulator!
          string-accumulator bytevector-accumulator bytevector-accumulator!
          sum-accumulator product-accumulator))
(select-module scheme.generator)

(define (make-accumulator kons knil finalizer)
  (^v
   (if (eof-object? v)
     (finalizer knil)
     (begin (set! knil (kons v knil)) (undefined)))))

(define (list-accumulator)
  (make-accumulator cons '() reverse))

(define (reverse-list-accumulator)
  (make-accumulator cons '() identity))

(define (vector-accumulator)
  (make-accumulator cons '() reverse-list->vector))

(define (reverse-vector-accumulator)
  (make-accumulator cons '() list->vector))

(define (vector-accumulator! vec at)
  (make-accumulator (^[v i]
                      (when (>= i (vector-length vec))
                        (error "vector is full"))
                      (vector-set! vec i v)
                      (+ i 1))
                    at
                    (^_ vec)))

(define (string-accumulator)
  (make-accumulator (^[c p] (write-char c p) p)
                    (open-output-string)
                    get-output-string))

(define (bytevector-accumulator)
  (make-accumulator (^[b p] (write-byte b p) p)
                    (open-output-string)
                    (^p (string->utf8 (get-output-string p)))))

(define (bytevector-accumulator! vec at)
  (assume-type vec <u8vector>)
  (make-accumulator (^[v i]
                      (when (>= i (uvector-length vec))
                        (error "bytevector is full"))
                      (u8vector-set! vec i v)
                      (+ i 1))
                    at
                    (^_ vec)))

(define (sum-accumulator)
  (make-accumulator + 0 identity))
(define (product-accumulator)
  (make-accumulator * 1 identity))
(define (count-accumulator)
  (make-accumulator (^[_ c] (+ c 1)) 0 identity))
