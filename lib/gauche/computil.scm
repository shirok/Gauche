;;;
;;; computil.scm - comparator primitives.  autoloaded
;;;
;;;   Copyright (c) 2014-2015  Shiro Kawai  <shiro@acm.org>
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

;; Provides some essential comparator objects & procedures.
;; Autoloaded.
;; Complete comparator API will be provided by srfi-114 module.

(define-module gauche.computil
  (export default-comparator
          boolean-comparator char-comparator char-ci-comparator
          string-ci-comparator symbol-comparator
          exact-integer-comparator integer-comparator rational-comparator
          real-comparator complex-comparator number-comparator
          pair-comparator list-comparator vector-comparator
          bytevector-comparator uvector-comparator

          make-reverse-comparator make-key-comparator
          make-car-comparator make-cdr-comparator
          make-tuple-comparator))
(select-module gauche.computil)

;; Needed to have string-ci compare.
;; TODO: We don't want to depend on text.unincode here, though.
(autoload text.unicode string-ci=? string-foldcase)

(define (a-number? x) (and (number? x) (not (nan? x))))
(define (a-real-number? x) (and (real? x) (not (nan? x))))

;; any object can be made comparable thru object-compare, so we just accept
;; any object for default-comparator.
(define default-comparator
  (make-comparator #t #t compare hash 'default-comparator))

;; eq-comparator, eqv-comparator, equal-comparator - in libomega.scm

(define boolean-comparator
  (make-comparator boolean? eqv? compare eq-hash 'boolean-comparator))
(define char-comparator
  (make-comparator char? eqv? compare eqv-hash 'char-comparator))
(define char-ci-comparator
  ($ make-comparator char? char-ci=? 
     (^[a b] (compare (char-foldcase a) (char-foldcase b)))
     eqv-hash 'char-ci-comparator))

;; string-comparator - in libomega.scm

(define string-ci-comparator
  ($ make-comparator string? string-ci=?
     (^[a b] (compare (string-foldcase a) (string-foldcase b)))
     (^s ((with-module gauche.internal %hash-string) (string-foldcase s)))
     'string-ci-comparator))
(define symbol-comparator
  (make-comparator symbol? eq? compare eq-hash 'symbol-comparator))

;; Number comparators
;; For integer-comparator hash, we need 1 and 1.0 to yield the same
;; hash value.  Any inexact integer can be mapped to exact integer,
;; so we convert the former to the latter to hash.
(define exact-integer-comparator
  (make-comparator exact-integer? eqv? compare eqv-hash
                   'exact-integer-comparator))
(define integer-comparator
  (make-comparator integer? = compare (^n (eqv-hash (exact n)))
                   'integer-comparator))
(define rational-comparator
  (make-comparator rational? = compare eqv-hash 'rational-comparator))
(define real-comparator
  (make-comparator a-real-number? = compare eqv-hash 'real-comparator))
(define complex-comparator
  (make-comparator a-number? = compare eqv-hash 'complex-comparator))
(define number-comparator complex-comparator)

(define pair-comparator
  (make-comparator pair? #t compare hash 'pair-comparator))
(define list-comparator
  (make-comparator list? #t compare hash 'list-comparator))
(define vector-comparator
  (make-comparator vector? #t compare hash 'vector-comparator))
(define uvector-comparator
  (make-comparator uvector? equal? compare hash 'uvector-comparator))
(define bytevector-comparator
  (make-comparator (cut is-a? <> <u8vector>) equal? compare hash
                   'bytevector-comparator))

(define (make-reverse-comparator comparator)
  (unless (comparator-comparison-procedure? comparator)
    (error "make-reverse-comparator requires a comparator with comparison \
            procedure, but got:" comparator))
  (make-comparator
   (comparator-type-test-procedure comparator)
   (comparator-equality-predicate comparator)
   (let1 cmp (comparator-comparison-procedure comparator)
     (^[a b] (- (cmp a b))))
   (and (comparator-hash-function? comparator)
        (comparator-hash-function comparator))))

;; This is not in srfi-114, but generally useful.
;; Compare with (accessor obj). 
(define (make-key-comparator comparator test key)
  (let ([ts  (comparator-type-test-procedure comparator)]
        [eq  (comparator-equality-predicate comparator)]
        [cmp (comparator-comparison-procedure comparator)]
        [hsh (comparator-hash-function comparator)])
    (make-comparator 
     (^x (and (test x) (ts (key x))))
     (^[a b] (eq (key a) (key b)))
     (and (comparator-comparison-procedure? comparator)
          (^[a b] (cmp (key a) (key b))))
     (and (comparator-hash-function? comparator)
          (^x (hsh (key x)))))))

(define (make-car-comparator comparator)
  (make-key-comparator comparator pair? car))

(define (make-cdr-comparator comparator)
  (make-key-comparator comparator pair? cdr))

(define (make-tuple-comparator comparator1 . comparators)
  (let1 cprs (cons comparator1 comparators)
    (let ([size (length cprs)]
          [ts   (map comparator-type-test-procedure cprs)]
          [eqs  (map comparator-equality-predicate cprs)]
          [cmps (map comparator-comparison-procedure cprs)]
          [hs   (map comparator-hash-function cprs)])
      (define (tester ts xs)
        (or (null? ts)
            (and ((car ts) (car xs)) (tester (cdr ts) (cdr xs)))))
      (define (equality eqs as bs)
        (or (null? eqs)
            (and ((car eqs) (car as) (car bs))
                 (equality (cdr eqs) (cdr as) (cdr bs)))))
      (define (refining-compare cmps as bs)
        (let1 r ((car cmps) (car as) (car bs))
          (if (or (null? (cdr cmps)) (not (= r 0)))
            r
            (refining-compare (cdr cmps) (cdr as) (cdr bs)))))
      (define (hasher hs xs)
        (let1 r ((car hs) (car xs))
          (if (null? (cdr hs))
            r
            (combine-hash-value r (hasher (cdr hs) (cdr xs))))))
      (make-comparator
       (^x (and (eqv? (length+ x) size) (tester ts x)))
       (^[a b] (equality eqs a b))
       (and (every comparator-comparison-procedure? cprs)
            (^[a b] (refining-compare cmps a b)))
       (and (every comparator-hash-function? cprs)
            (^x (hasher hs x)))))))
