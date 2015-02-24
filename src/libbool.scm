;;;
;;; libbool.scm - builtin boolean/comparison procedures
;;;
;;;   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

(select-module gauche.internal)

(inline-stub
 (declcode (.include <gauche/vminsn.h>))
 )

;;
;; Equivalence predicates
;;

(select-module scheme)
(define-cproc eqv? (obj1 obj2) ::<boolean> :fast-flonum :constant
  (inliner EQV) Scm_EqvP)
(define-cproc eq? (obj1 obj2)  ::<boolean> :fast-flonum :constant
  (inliner EQ) SCM_EQ)
(define-cproc equal? (obj1 obj2) ::<boolean> :fast-flonum Scm_EqualP)

;;
;; Booleans
;;

(select-module scheme)
(define-cproc not (obj) ::<boolean> :fast-flonum :constant
  (inliner NOT) SCM_FALSEP)
(define-cproc boolean? (obj) ::<boolean> :fast-flonum :constant SCM_BOOLP)

(select-module gauche)
;; a convenient coercer
(define-cproc boolean (obj) ::<boolean> :constant
  (return (not (SCM_FALSEP obj))))

;; R7RS
(define (boolean=? a b . args)
  (if-let1 z (find ($ not $ boolean? $) (list* a b args))
    (error "boolean value required, but got:" z))
  (if a
    (and b (every identity args))
    (and (not b) (every not args))))

;; Circular-safe equal?
;; Based on Michael D. Adams and R. Kent Dybvig: Efficient Nondestructive
;; Equality Checking for Trees and Graphs, Proceedings of ICFP 08, pp. 179-188.
;; http://www.cs.indiana.edu/~dyb/pubs/equal.pdf
;;
;; NB: To choose restart value of K0, (modulo (sys-random) (* 2 K0)) isn't
;; strictly uniform, but for our purpose it's ok.   We chose K0 to be prime
;; so that it would compensate if the quality of randomness of low-bits of
;; sys-random is not good.
;;
;; %interleave-equal? is invoked from C routine Scm_EqualP, which handles
;; non-aggregate values and user-defined datatypes, so we recurse to equal?
;; instead of using eqv? for such case (strings are also handled in equal?).

(select-module gauche.internal)
(define-constant K0 401)
(define-constant Kb -40)
(define (%interleave-equal? x y) (%interleave? x y K0))

(define (%interleave? x y k)
  (let1 ht #f
    (define (call-union-find x y)
      (unless ht (set! ht (make-hash-table 'eq?)))
      (%union-find ht x y))
    (define (e? x y k)
      (if (<= k 0)
        (if (= k Kb)
          (fast? x y (modulo (sys-random) (* 2 K0)))
          (slow? x y k))
        (fast? x y k)))
    (define (slow? x y k)
      (cond
       [(eqv? x y) k]
       [(pair? x)
        (and (pair? y)
             (if (call-union-find x y)
               0
               (and-let* ([k (e? (car x) (car y) (- k 1))])
                 (e? (cdr x) (cdr y) k))))]
       [(vector? x)
        (and (vector? y)
             (let1 n (vector-length x)
               (and (= (vector-length y) n)
                    (if (call-union-find x y)
                      0
                      (let loop ([i 0] [k (- k 1)])
                        (if (= i n)
                          k
                          (and-let* ([k (e? (vector-ref x i)
                                            (vector-ref y i) k)])
                            (loop (+ i 1) k))))))))]
       [else (and (equal? x y) k)]))
    (define (fast? x y k)
      (let1 k (- k 1)
        (cond
         [(eqv? x y) k]
         [(pair? x)
          (and-let* ([ (pair? y) ]
                     [k (e? (car x) (car y) k)])
            (e? (cdr x) (cdr y) k))]
         [(vector? x)
          (and (vector? y)
               (let1 n (vector-length x)
                 (and (= (vector-length y) n)
                      (let loop ([i 0] [k k])
                        (if (= i n)
                          k
                          (and-let* ([k (e? (vector-ref x i)
                                            (vector-ref y i) k)])
                            (loop (+ i 1) k)))))))]
         [else (and (equal? x y) k)])))
    (boolean (e? x y k))))

;; Replace these once we support srfi-111
(define-macro (%box? x) `(pair? ,x))
(define-macro (%set-box! x v) `(set-car! ,x ,v))
(define-macro (%unbox x) `(car ,x))
(define-macro (%make-box x) `(list ,x))

(define (%union-find ht x y)
  (define (find b)
    (let1 n (%unbox b)
      (if (%box? n)
        (let loop ([b b] [n n])
          (let1 nn (%unbox n)
            (if (%box? nn)
              (begin
                (%set-box! b nn)
                (loop n nn))
              n)))
        b)))
  (let ([bx (hash-table-get ht x #f)]
        [by (hash-table-get ht y #f)])
    (if (not bx)
      (if (not by)
        (let1 b (%make-box 1)
          (hash-table-put! ht x b)
          (hash-table-put! ht y b)
          #f)
        (let1 ry (find by)
          (hash-table-put! ht x ry)
          #f))
      (if (not by)
        (let1 rx (find bx)
          (hash-table-put! ht y rx)
          #f)
        (let ([rx (find bx)] [ry (find by)])
          (or (eq? rx ry)
              (let ([nx (%unbox rx)]
                    [ny (%unbox ry)])
                (if (> nx ny)
                  (begin
                    (%set-box! ry rx)
                    (%set-box! rx (+ nx ny))
                    #f)
                  (begin
                    (%set-box! rx ry)
                    (%set-box! ry (+ ny nx))
                    #f)))))))))
