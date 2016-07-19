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

(define-module gauche.computil
  (export default-comparator
          boolean-comparator char-comparator char-ci-comparator
          string-ci-comparator symbol-comparator
          exact-integer-comparator integer-comparator rational-comparator
          real-comparator complex-comparator number-comparator
          pair-comparator list-comparator vector-comparator
          bytevector-comparator uvector-comparator

          make-default-comparator make-eq-comparator
          make-eqv-comparator make-equal-comparator

          make-pair-comparator make-list-comparator

          make-reverse-comparator make-key-comparator
          make-car-comparator make-cdr-comparator
          make-tuple-comparator

          =? <? <=? >? >=? comparator-if<=>))
(select-module gauche.computil)

;; Needed to have string-ci compare.
(autoload gauche.unicode string-ci=? string-foldcase)

(define (a-number? x) (and (number? x) (not (nan? x))))
(define (a-real-number? x) (and (real? x) (not (nan? x))))

;; any object can be made comparable thru object-compare, so we just accept
;; any object for default-comparator.
(define default-comparator
  (make-comparator/compare #t #t compare hash 'default-comparator))

(define (make-default-comparator) default-comparator) ;srfi-128

;; eq-comparator, eqv-comparator, equal-comparator - in libomega.scm

;; NB: srfi-128 specifies the comparators returned by make-eq-comparator
;; and make-eqv-comparator must use default-hash.  So they can be a lot
;; more inefficient than eq-comparator and eqv-comparator, and they can't
;; be used to hash mutable objects based on their identity.
(define make-eq-comparator            ; srfi-128
  (let1 c (make-comparator/compare #t eq? eq-compare hash) ; singleton
    (^[] c)))
(define make-eqv-comparator           ; srfi-128
  (let1 c (make-comparator/compare #t eqv? #f hash) ; singleton
    (^[] c)))
(define (make-equal-comparator) equal-comparator); srfi-128

(define boolean-comparator
  (make-comparator/compare boolean? eqv? compare eq-hash 'boolean-comparator))
(define char-comparator
  (make-comparator/compare char? eqv? compare eqv-hash 'char-comparator))
(define char-ci-comparator
  ($ make-comparator/compare char? char-ci=? 
     (^[a b] (compare (char-foldcase a) (char-foldcase b)))
     eqv-hash 'char-ci-comparator))

;; string-comparator - in libomega.scm

(define string-ci-comparator
  ($ make-comparator/compare string? string-ci=?
     (^[a b] (compare (string-foldcase a) (string-foldcase b)))
     (^s ((with-module gauche.internal %hash-string) (string-foldcase s)))
     'string-ci-comparator))
(define symbol-comparator
  (make-comparator/compare symbol? eq? compare eq-hash 'symbol-comparator))

;; Number comparators
;; For integer-comparator hash, we need 1 and 1.0 to yield the same
;; hash value.  Any inexact integer can be mapped to exact integer,
;; so we convert the former to the latter to hash.
(define exact-integer-comparator
  (make-comparator/compare exact-integer? eqv? compare eqv-hash
                           'exact-integer-comparator))
(define integer-comparator
  (make-comparator/compare integer? = compare (^n (eqv-hash (exact n)))
                           'integer-comparator))
(define rational-comparator
  (make-comparator/compare rational? = compare eqv-hash 'rational-comparator))
(define real-comparator
  (make-comparator/compare a-real-number? = compare eqv-hash 'real-comparator))
(define complex-comparator
  (make-comparator/compare a-number? = compare eqv-hash 'complex-comparator))
(define number-comparator complex-comparator)

(define pair-comparator
  (make-comparator/compare pair? #t compare hash 'pair-comparator))
(define list-comparator
  (make-comparator/compare list? #t compare hash 'list-comparator))
(define vector-comparator
  (make-comparator/compare vector? #t compare hash 'vector-comparator))
(define uvector-comparator
  (make-comparator/compare uvector? equal? compare hash 'uvector-comparator))
(define bytevector-comparator
  (make-comparator/compare (cut is-a? <> <u8vector>) equal? compare hash
                           'bytevector-comparator))

(define (make-pair-comparator car-comparator cdr-comparator)
  (make-comparator/compare
   (^x (and (pair? x)
            (comparator-test-type car-comparator (car x))
            (comparator-test-type cdr-comparator (cdr x))))
   (^[a b] (and (=? car-comparator (car a) (car b))
                (=? cdr-comparator (cdr a) (cdr b))))
   (and (comparator-ordered? car-comparator)
        (comparator-ordered? cdr-comparator)
        (^[a b] (let1 r (comparator-compare car-comparator
                                            (car a) (car b))
                  (if (= r 0)
                    (comparator-compare cdr-comparator (cdr a) (cdr b))
                    r))))
   (and (comparator-hashable? car-comparator)
        (comparator-hashable? cdr-comparator)
        (^x (combine-hash-value (comparator-hash car-comparator (car x))
                                (comparator-hash cdr-comparator (cdr x)))))))

;; NB: There's lots of room for optimization.

(define (%gen-listwise-compare elt-compare null? car cdr)
  (rec (f a b)
    (if (null? a)
      (if (null? b) 0 -1)
      (if (null? b)
        1
        (let1 r (elt-compare (car a) (car b))
          (if (= r 0) (f (cdr a) (cdr b)) r))))))

(define (%gen-listwise-hash elt-hash null? car cdr)
  (^[x]
    (let loop ([v 10037] [x x])
      (if (null? x)
        v
        (loop (combine-hash-value (elt-hash (car x)) v) (cdr x))))))

(define (make-list-comparator elt-comparator
                              :optional (test list?)
                                        (empty? null?)
                                        (head car)
                                        (tail cdr))
  (make-comparator/compare test #t
                           (%gen-listwise-compare
                            (comparator-comparison-procedure elt-comparator)
                            empty? head tail)
                           (and (comparator-hashable? elt-comparator)
                                (%gen-listwise-hash
                                 (comparator-hash-function elt-comparator)
                                 empty? head tail))))

(define (%gen-vectorwise-compare elt-compare len ref)
  (^[a b]
    (let ([alen (len a)]
          [blen (len b)])
      (cond [(< alen blen) -1]
            [(> alen blen) 1]
            [else
             (let loop ([i 0])
               (if (= i alen)
                 0
                 (let1 r (elt-compare (ref a i) (ref b i))
                   (if (= r 0)
                     (loop (+ i 1))
                     r))))]))))

(define (%gen-vectorwise-hash elt-hash len ref)
  (^[x]
    (let loop ([v 10037] [i (- (len x) 1)])
    (if (< i 0)
      v
      (loop (combine-hash-value (elt-hash (ref x i)) v) (- i 1))))))

(define (make-vector-comparator elt-comparator
                                :optional (test vector?)
                                          (len vector-length)
                                          (ref vector-ref))
  (make-comparator/compare test #t
                           (%gen-vectorwise-compare
                            (comparator-comparison-procedure elt-comparator)
                            len ref)
                           (and (comparator-hashable? elt-comparator)
                                (%gen-vectorwise-hash
                                 (comparator-hash-function elt-comparator)
                                 len ref))))

(define (make-reverse-comparator comparator)
  (unless (comparator-ordered? comparator)
    (error "make-reverse-comparator requires an ordered comparator, \
            but got:" comparator))
  (make-comparator/compare
   (comparator-type-test-predicate comparator)
   (comparator-equality-predicate comparator)
   (let1 cmp (comparator-comparison-procedure comparator)
     (^[a b] (- (cmp a b))))
   (and (comparator-hashable? comparator)
        (comparator-hash-function comparator))))

;; This is not in srfi-114, but generally useful.
;; Compare with (accessor obj). 
(define (make-key-comparator comparator test key)
  (let ([ts  (comparator-type-test-predicate comparator)]
        [eq  (comparator-equality-predicate comparator)]
        [cmp (comparator-comparison-procedure comparator)]
        [hsh (comparator-hash-function comparator)])
    (make-comparator/compare 
     (^x (and (test x) (ts (key x))))
     (^[a b] (eq (key a) (key b)))
     (and (comparator-ordered? comparator)
          (^[a b] (cmp (key a) (key b))))
     (and (comparator-hashable? comparator)
          (^x (hsh (key x)))))))

(define (make-car-comparator comparator)
  (make-key-comparator comparator pair? car))

(define (make-cdr-comparator comparator)
  (make-key-comparator comparator pair? cdr))

(define (make-tuple-comparator comparator1 . comparators)
  (let1 cprs (cons comparator1 comparators)
    (let ([size (length cprs)]
          [ts   (map comparator-type-test-predicate cprs)]
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
      (make-comparator/compare
       (^x (and (eqv? (length+ x) size) (tester ts x)))
       (^[a b] (equality eqs a b))
       (and (every comparator-ordered? cprs)
            (^[a b] (refining-compare cmps a b)))
       (and (every comparator-hashable? cprs)
            (^x (hasher hs x)))))))

(define-syntax n-ary
  (syntax-rules ()
    [(_ cmp a b args test)
     (begin
       (comparator-check-type cmp a)
       (comparator-check-type cmp b)
       (and test
            (or (null? args)
                (apply (rec (f a b . args)
                         (comparator-check-type cmp b)
                         (and test
                              (or (null? args)
                                  (apply f b args))))
                       b args))))]))
(define (=? cmp a b . args)
  (let1 ==? (comparator-equality-predicate cmp)
    (n-ary cmp a b args (==? a b))))

(define (<? cmp a b . args)
  (case (comparator-flavor cmp)
    [(ordering)
     (let ([lt (comparator-ordering-predicate cmp)])
       (n-ary cmp a b args (lt a b)))]
    [(comparison)
     (let ([<=> (comparator-comparison-procedure cmp)])
       (n-ary cmp a b args (< (<=> a b) 0)))]))

(define (<=? cmp a b . args)
  (case (comparator-flavor cmp)
    [(ordering)
     (let ([eq (comparator-equality-predicate cmp)]
           [lt (comparator-ordering-predicate cmp)])
       (n-ary cmp a b args (or (eq a b) (lt a b))))]
    [(comparison)
     (let ([<=> (comparator-comparison-procedure cmp)])
       (n-ary cmp a b args (<= (<=> a b) 0)))]))

(define (>? cmp a b . args)
  (case (comparator-flavor cmp)
    [(ordering)
     (let ([eq (comparator-equality-predicate cmp)]
           [lt (comparator-ordering-predicate cmp)])
       (n-ary cmp a b args (not (or (eq a b) (lt a b)))))]
    [(comparison)
     (let ([<=> (comparator-comparison-procedure cmp)])
       (n-ary cmp a b args (> (<=> a b) 0)))]))

(define (>=? cmp a b . args)
  (case (comparator-flavor cmp)
    [(ordering)
     (let ([lt (comparator-ordering-predicate cmp)])
       (n-ary cmp a b args (not (lt a b))))]
    [(comparison)
     (let ([<=> (comparator-comparison-procedure cmp)])
       (n-ary cmp a b args (>= (<=> a b) 0)))]))

(define-syntax comparator-if<=>
  (syntax-rules ()
    [(_ a b lt eq gt)
     (comparator-if<=> default-comparator a b lt eq gt)]
    [(_ cmp a b lt eq gt)
     (case (comparator-flavor cmp)
       [(ordering)
        (let ([a. a] [b. b])
          (if (<? cmp a. b.)
            lt
            (if (=? cmp a. b.)
              eq
              gt)))]
       [(comparison)
        (case (comparator-compare cmp a b)
          [(-1) lt]
          [(0)  eq]
          [(1)  gt])])]))
