;;;
;;; srfi-221 - Generator/accumulator sub-library
;;;
;;;   Copyright (c) 2021-2022  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi-221
  (use gauche.generator)
  (use scheme.vector :only (vector-every))
  (use util.stream)
  (export gcompose-left gcompose-right
          accumulate-generated-values
          genumerate
          gchoice
          stream->generator
          generator->stream)
  )
(select-module srfi-221)

(define (gcompose-left ctor . ops)
  (let loop ([g (ctor)] [ops ops])
    (if (null? ops)
      g
      (loop ((car ops) g) (cdr ops)))))

(define (gcompose-right arg . ops&ctor)
  (if (null? ops&ctor)
    (arg)                               ;returns a generator
    (let1 ctor (last ops&ctor)
      (let rec ([ops (cons arg (drop-right ops&ctor 1))])
        (if (null? ops)
          (ctor)
          ((car ops) (rec (cdr ops))))))))


(define (accumulate-generated-values acc gen)
  (let loop ([v (gen)])
    (if (eof-object? v)
      (acc v)                           ;returns accumulated value
      (begin (acc v) (loop (gen))))))

(define (genumerate gen)
  (define counter -1)
  (^[] (glet1 v (gen)
         (begin (inc! counter)
                (cons counter v)))))

(define (gchoice choice-gen . source-gens)
  (let1 srcs (list->vector source-gens)
    (rec (gen)
      (glet1 c (choice-gen)
        (assume (and (exact-integer? c)
                     (<= 0 c (- (vector-length srcs) 1))))
        (let1 g (vector-ref srcs c)
          (if g
            (let1 v (g)
              (if (eof-object? v)
                (begin
                  (vector-set! srcs c #f)
                  (if (vector-every not srcs)
                    (eof-object)
                    (gen)))
                v))
            (gen)))))))

(define (stream->generator stream)
  (^[] (if (stream-null? stream)
         (eof-object)
         (begin0 (stream-car stream)
           (set! stream (stream-cdr stream))))))

(define (generator->stream gen)
  (stream-delay
   (let loop ([v (gen)])
     (if (eof-object? v)
       stream-null
       (stream-cons v (loop (gen)))))))
