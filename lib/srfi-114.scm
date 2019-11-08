;;;
;;; srfi-114 - comparators
;;;
;;;   Copyright (c) 2014-2019  Shiro Kawai  <shiro@acm.org>
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

;; Basic support of comparators are built-in, for the built-in
;; data structures such as hashtables would make use of them.
;; This module provides complete set of srfi-114

;; TODO: Importing srfi-114 shadows make-comparator, but many procedures
;; here are generally useful.  We might want to split those procs
;; in separate module so that they can be used without worring
;; different make-comparator APIs.
;; TODO: Comparator combinators are written for srfi-114 and assumes
;; the given comparator has comparison proc.  We might be able to optimize
;; it by checking if the passed one is srfi-128.

(define-module srfi-114
  (use gauche.uvector)
  (use srfi-162) ; comparator-min, comparator-max
  (export comparator?                   ;builtin (srfi-128)
          comparator-comparison-procedure?
          comparator-hash-function?

          boolean-comparator char-comparator char-ci-comparator
          string-comparator string-ci-comparator symbol-comparator
          exact-integer-comparator integer-comparator rational-comparator
          real-comparator complex-comparator number-comparator
          pair-comparator list-comparator vector-comparator
          bytevector-comparator uvector-comparator ;all builtin

          default-comparator            ;builtin
          
          (rename make-comparator/compare make-comparator) ;builtin
          make-car-comparator make-cdr-comparator
          
          make-inexact-real-comparator
          make-vector-comparator ; builtin
          make-bytevector-comparator
          make-list-comparator ; builtin
          make-vectorwise-comparator make-listwise-comparator
          make-pair-comparator ; builtin
          make-improper-list-comparator
          make-selecting-comparator make-refining-comparator
          make-reverse-comparator make-debug-comparator

          eq-comparator                 ;builtin
          eqv-comparator                ;builtin
          equal-comparator              ;builtin

          comparator-type-test-procedure
          comparator-equality-predicate   ;builtin
          comparator-comparison-procedure ;builtin
          comparator-hash-function        ;builtin

          comparator-test-type          ;builtin
          comparator-check-type         ;builtin
          comparator-equal?
          comparator-compare            ;builtin
          comparator-hash               ;builtin

          make-comparison< make-comparison> make-comparison<=
          make-comparison>= make-comparison=/< make-comparison=/>

          if3 if=? if<? if>? if<=? if>=? if-not=?

          =? <? >? <=? >=?              ;builtin

          make=? make<?  make>? make<=? make>=?

          in-open-interval? in-closed-interval?
          in-open-closed-interval? in-closed-open-interval?

          comparator-min comparator-max ;srfi-162
          ))
(select-module srfi-114)

;;;
;;; Predicates
;;;
(define (comparator-comparison-procedure? c)
  (comparator-ordered? c))              ;srfi-128
(define (comparator-hash-function? c)
  (comparator-hashable? c))             ;srfi-128
(define (comparator-type-test-procedure c)
  (comparator-type-test-predicate c))   ;srfi-128
(define (comparator-equal? c a b) (=? c a b))

;;;
;;; Inexact comparator
;;;

(define (((%make-scaler rounder) epsilon) num) (rounder (/ num epsilon)))
(define %scale-round    (%make-scaler round))
(define %scale-ceiling  (%make-scaler ceiling))
(define %scale-floor    (%make-scaler floor))
(define %scale-truncate (%make-scaler truncate))

(define (%handle-nan which num nan-handling)
  (case nan-handling
    [(min) (if (eq? which 'a) -1  1)]
    [(max) (if (eq? which 'a)  1 -1)]
    [else
     (if (applicable? nan-handling <real>)
       (nan-handling num)
       (error "nan-handling argument must be either a procedure taking one argument, or a symbol min or max, but got:" nan-handling))]))

(define (make-inexact-real-comparator epsilon rounding nan-handling)
  (define scaler
    (case rounding
      [(round)    (%scale-round epsilon)]
      [(ceiling)  (%scale-ceiling epsilon)]
      [(floor)    (%scale-floor epsilon)]
      [(truncate) (%scale-truncate epsilon)]
      [else (if (applicable? rounding <real> <real>)
              (^[num] (rounding num epsilon))
              (error "make-inexact-comparator requires `rounding' argument to be either a procedure taking two real numbers, or one of the symbols 'round, 'ceiling, 'floor or 'truncate, but got:" rounding))]))
  (define nan-handler
    (case nan-handling
      [(min) (^[which num] (if (eq? which 'a) -1  1))]
      [(max) (^[which num] (if (eq? which 'a)  1 -1))]
      [(error) (^[which num]
                 (error "attempt to compare NaN with non-NaN: ~a" num))]
      [else
       (if (applicable? nan-handling <real>)
         (^[which num] (nan-handling num))
         (error "make-inexact-comparator requires `nan-handling' argument to be either a procedure taking one real number, or one of the symbols 'min or 'max, but got:" nan-handling))]))
  (make-comparator/compare real?
                           #t
                           (^[a b]
                             (if (nan? a)
                               (if (nan? b)
                                 0
                                 (nan-handler 'a b))
                               (if (nan? b)
                                 (nan-handler 'b a)
                                 (compare (scaler a) (scaler b)))))
                           (^[a] (if (nan? a) 0 (eqv-hash (scaler a))))))

;;;
;;; Comparator transformers
;;;

(define (make-car-comparator comparator)
  (make-key-comparator comparator pair? car))

(define (make-cdr-comparator comparator)
  (make-key-comparator comparator pair? cdr))

(define (make-listwise-comparator test elt-comparator null? car cdr)
  (make-list-comparator elt-comparator test null? car cdr)) ; srfi-128

(define (make-vectorwise-comparator test elt-comparator len ref)
  (make-vector-comparator elt-comparator test len ref)) ; srfi-128

(define (make-bytevector-comparator elt-comparator)
  (make-vectorwise-comparator u8vector? elt-comparator
                              u8vector-length u8vector-ref))

(define (make-improper-list-comparator elt-comparator)
  ($ make-comparator/compare #t
     (^[a b] (cond [(null? a) (null? b)]
                   [(pair? a)
                    (and (pair? b)
                         (comparator-equal? elt-comparator (car a) (car b))
                         (comparator-equal? elt-comparator (cdr a) (cdr b)))]
                   [else (comparator-equal? elt-comparator a b)]))
     (and (comparator-comparison-procedure? elt-comparator)
          (^[a b] (cond [(null? a) (if (null? b) 0 -1)]
                        [(pair? a)
                         (if (pair? b)
                           (let1 r (comparator-compare elt-comparator
                                                       (car a) (car b))
                             (if (= r 0)
                               (comparator-compare elt-comparator
                                                   (cdr a) (cdr b))
                               r))
                           -1)]
                        [else (comparator-compare elt-comparator a b)])))
     (and (comparator-hash-function? elt-comparator)
          (^x (if (pair? x)
                (combine-hash-value (comparator-hash elt-comparator (car x))
                                    (comparator-hash elt-comparator (cdr x)))
                (comparator-hash elt-comparator x))))))

(define (make-selecting-comparator cmp . cmps)
  (define (selector x) ; returns a comparator
    (let loop ([cmp cmp] [cmps cmps])
      (cond [(comparator-test-type cmp x) cmp]
            [(null? cmps) #f]
            [else (loop (car cmps) (cdr cmps))])))
  (define (selector2 a b) ; returns a comparator
    (let loop ([cmp cmp] [cmps cmps])
      (cond [(and (comparator-test-type cmp a)
                  (comparator-test-type cmp b))
             cmp]
            [(null? cmps) #f]
            [else (loop (car cmps) (cdr cmps))])))
  (make-comparator/compare
   (^x (boolean (selector x)))
   (^[a b] (and-let* ([cmp (selector2 a b)])
             (comparator-equal? cmp a b)))
   (^[a b] (if-let1 cmp (selector2 a b)
             (comparator-compare cmp a b)
             (error "argument isn't comparable")))
   (^x (if-let1 cmp (selector x)
         (comparator-hash cmp x)
         (error "argument isn't hashable:" x)))))
          
(define (make-refining-comparator cmp . cmps)    
  (define (selector x) ; returns a comparator
    (let loop ([cmp cmp] [cmps cmps])
      (cond [(comparator-test-type cmp x) cmp]
            [(null? cmps) #f]
            [else (loop (car cmps) (cdr cmps))])))
  (define (selector2 a b cmps) ; returns a comparator and the rest
    (let loop ([cmps cmps])
      (cond [(null? cmps) (values #f #f)]
            [(and (comparator-test-type (car cmps) a)
                  (comparator-test-type (car cmps) b))
             (values (car cmps) (cdr cmps))]
            [else (loop (cdr cmps))])))
  (make-comparator/compare
   (^x (boolean (selector x)))
   (^[a b] (receive (cmp _) (selector2 a b (cons cmp cmps))
             (if cmp
               (comparator-equal? cmp a b)
               #f)))
   (^[a b] (let loop ([cmps (cons cmp cmps)]
                      [r #f])
             (receive (cmp cmps) (selector2 a b cmps)
               (if cmp
                 (let1 r (comparator-compare cmp a b)
                   (if (= r 0)
                     (loop cmps r)
                     r))
                 (or r (error "argument isn't comparable"))))))
   (^x (if-let1 cmp (selector x)
         (comparator-hash cmp x)
         (error "argument isn't hashable:" x)))))

(define (make-reverse-comparator cmp)
  (make-comparator/compare (comparator-type-test-procedure cmp)
                           (comparator-equality-predicate cmp)
                           (and (comparator-comparison-procedure? cmp)
                                (^[a b] (- (comparator-compare cmp a b))))
                           (and (comparator-hash-function? cmp)
                                (comparator-hash-function cmp))))

(define (make-debug-comparator cmp)
  cmp)                                  ;WRITEME

(define (make-comparison< pred)
  (^[a b] (cond [(pred a b) -1]
                [(pred b a) 1]
                [else 0])))

(define (make-comparison> pred)
  (^[a b] (cond [(pred a b) 1]
                [(pred b a) -1]
                [else 0])))

(define (make-comparison<= pred)
  (^[a b] (if (pred a b) (if (pred b a) 0 -1) 1)))

(define (make-comparison>= pred)
  (^[a b] (if (pred a b) (if (pred b a) 0 1) -1)))

(define (make-comparison=/< eq-pred lt-pred)
  (^[a b] (cond [(eq-pred a b) 0]
                [(lt-pred a b) -1]
                [else 1])))

(define (make-comparison=/> eq-pred gt-pred)
  (^[a b] (cond [(eq-pred a b) 0]
                [(gt-pred a b) 1]
                [else -1])))

(define-syntax if3
  (syntax-rules ()
    [(_ expr less equal greater)
     (let1 r expr
       (cond [(= r 0) equal]
             [(< r 0) less]
             [else greater]))]
    [(_ . x) (syntax-error "malformed if3:" (if3 . x))]))

(define-syntax define-ifx
  (syntax-rules ()
    [(_ name op)
     (define-syntax name
       (syntax-rules ()
         [(_ expr then . opt) (if (op expr 0) then . opt)]))]))

(define (!= a b) (not (= a b)))

(define-ifx if=? =)
(define-ifx if<? <)
(define-ifx if>? >)
(define-ifx if<=? <=)
(define-ifx if>=? >=)
(define-ifx if-not=? !=)

(define (make=? comparator)
  (rec (cmp a b . args)
    (and (comparator-equal? comparator a b)
         (or (null? args)
             (apply cmp b args)))))

(define (gen-make-x op)
  (^[comparator]
    (rec (cmp a b . args)
      (let1 r (comparator-compare comparator a b)
        (and (op r 0)
             (if (null? args)
               #t
               (apply cmp b args)))))))

(define make<? (gen-make-x <))
(define make>? (gen-make-x >))
(define make<=? (gen-make-x <=))
(define make>=? (gen-make-x >=))

(define in-open-interval?
  (case-lambda
    [(cmp a b c) (<? cmp a b c)]
    [(a b c) (<? default-comparator a b c)]))

(define in-closed-interval?
  (case-lambda
    [(cmp a b c) (<=? cmp a b c)]
    [(a b c) (<=? default-comparator a b c)]))

(define in-open-closed-interval?
  (case-lambda
    [(cmp a b c) (and (<? cmp a b) (<=? cmp b c))]
    [(a b c) (in-open-closed-interval? default-comparator a b c)]))

(define in-closed-open-interval?
  (case-lambda
    [(cmp a b c) (and (<=? cmp a b) (<? cmp b c))]
    [(a b c) (in-closed-open-interval? default-comparator a b c)]))
