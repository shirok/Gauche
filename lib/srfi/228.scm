;;;
;;; SRFI-228 - Composing comparators
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

(define-module srfi.228
  (export make-wrapper-comparator
          make-product-comparator
          make-sum-comparator
          comparator-one
          comparator-zero))
(select-module srfi.228)

(define (make-wrapper-comparator type-test unwrap contents-comparator)
  (make-comparator
   type-test
   (^[a b] (=? contents-comparator (unwrap a) (unwrap b)))
   (and (comparator-ordered? contents-comparator)
        (^[a b] (<? contents-comparator (unwrap a) (unwrap b))))
   (and (comparator-hashable? contents-comparator)
        (^[a] (comparator-hash contents-comparator (unwrap a))))))

(define-constant comparator-one
  (make-comparator (constantly #t)
                   (constantly #t)
                   (constantly #f)
                   (constantly 0)))

(define-constant comparator-zero
  (make-comparator (constantly #f)
                   (^[a b] (error "Can't test equality with this comparator"))
                   #f #f))

(define (make-product-comparator . cmps)
  (if (null? cmps)
    comparator-one
    (make-comparator
     (^[a] (every (cut comparator-test-type <> a) cmps))
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
          (^[a]
            (fold (^[cmp h] (combine-hash-value h (comparator-hash cmp a)))
                  (hash-salt)
                  cmps))))))

(define (select-cmp cmps obj)
  (let loop ((cmps cmps) (index 0))
    (cond [(null? cmps) (values #f #f)]
          [(comparator-test-type (car cmps) obj) (values (car cmps) index)]
          [else (loop (cdr cmps) (+ index 1))])))

(define (make-sum-comparator . cmps)
  (if (null? cmps)
    comparator-zero
    (make-comparator
     (^[a] (any (cut comparator-test-type <> a) cmps))
     (^[a b] (let loop ([cmps cmps])
               (cond [(null? cmps) #f]
                     [(comparator-test-type (car cmps) a)
                      (and (comparator-test-type (car cmps) b)
                           (=? (car cmps) a b))]
                     [else (loop (cdr cmps))])))
     (and (every comparator-ordered? cmps)
          (^[a b]
            (let loop ([cmps cmps])
              (cond [(null? cmps) #f]
                    [(comparator-test-type (car cmps) a)
                     (if (comparator-test-type (car cmps) b)
                       (<? (car cmps) a b)
                       #t)]
                    [(comparator-test-type (car cmps) b) #f]
                    [else (loop (cdr cmps))]))))
     (and (every comparator-hashable? cmps)
          (^[a] (if-let1 cmp (find (cut comparator-test-type <> a) cmps)
                  (comparator-hash cmp a)
                  0))))))
