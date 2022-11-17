;;;
;;; srfi-228 - Composing comparators
;;;
;;;   Copyright (c) 2022  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi-228
  (export make-wrapper-comparator
          make-product-comparator
          make-sum-comparator))
(select-module srfi-228)

(define (make-wrapper-comparator type-test unwrap contents-comparator)
  (make-comparator
   type-test
   (^[a b] (=? contents-comparator (unwrap a) (unwrap b)))
   (and (comparator-ordered? contents-comparator)
        (^[a b] (comparator-compare contents-comparator (unwrap a) (unwrap b))))
   (and (comaprator-hashable? contents-comparator)
        (^[a] (comparator-hash contents-comparator (unwrap a))))))

(define (make-product-comparator . cmps)
  (make-comparator
   (^[a] (every (cut comparator-check-type <> a) cmps))
   (^[a b] (let loop ([cmps cmps])
             (cond [(null? cmps) #t]
                   [(=? (car cmps) a b) (loop (cdr cmps))]
                   [else #f])))
   (and (every comparator-ordered? cmps)
        (^[a b] (let loop ([cmps cmps])
                  (cond [(null? cmps) #f]
                        [(<? (car cmps) a b) #t]
                        [(=? (car cmps) a b) (loop (cdr cmps))]
                        [else #f]))))
   (and (every comparator-hashable? cmps)
        (^[a] (let loop ([cmps cmps] [h (hash-salt)])
                (if (null? cmps)
                  h
                  (loop (cdr cmps)
                        (combine-hash-value (comparator-hash (car cmps) a)))))))))
