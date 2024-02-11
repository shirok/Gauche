;; Copyright (C) Marc Nieper-Wißkirchen (2023).  All Rights Reserved.
;;
;; SPDX-License-Identifier: MIT

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(import
  (rename (rnrs)
    (partition rnrs:partition))
  (srfi :247 syntactic-monads))

(assert (equal? '(3 2 4)
                (let ()
                  (define-syntactic-monad $ a b)
                  (letrec ([f ($ lambda (c)
                                ($ list ([a (+ a b)]) c))])
                    (f 1 2 4)))))

(assert (equal? '(3 2 4)
                (let ()
                  (define-syntactic-monad $ a b)
                  (letrec ([f ($ case-lambda
                                [(c)
                                 ($ list ([a (+ a b)]) c)])])
                    (f 1 2 4)))))

(assert (equal? '(3 2 4)
                (let ()
                  (define-syntactic-monad $ a b)
                  ($ define (f c)
                    ($ list ([a (+ a b)]) c))
                  (f 1 2 4))))

(assert (equal? '(3 2 4)
                (let ()
                  (define-syntactic-monad $ a b)
                  ($ let f ([a 1] [b 2] [c 4])
                    ($ list ([a (+ a b)]) c)))))

(define partition
  (lambda (pred ls)
    (define-syntactic-monad $ in out)
    (let f ([ls ls])
      (if (null? ls)
          ($ values ([in '()] [out '()]))
          ($ let*-values ([() (f (cdr ls))])
            (let ([x (car ls)])
              (if (pred x)
                  ($ values ([in (cons x in)]))
                  ($ values ([out (cons x out)])))))))))

(assert (equal? '((one four five) (2 3 6))
                (let-values ([(in out)
                              (partition symbol? '(one 2 3 four five 6))])
                  (list in out))))

(define factor
  (lambda (n)
    (define-syntactic-monad $ n i)
    ($ let f ([i 2])
      (cond
        [(>= i n) (list n)]
        [(integer? (/ n i))
         (cons i ($ f [(n (/ n i))]))]
        [else ($ f [(i (+ i 1))])]))))

(assert (equal? '(2 2 2 2 2 2 2 2 3 3 3 3 5 5 7)
                (factor 3628800)))

(define run1
  (lambda (prog a b c)
    (define a->b
      (lambda (prog a b c)
        (run1 prog (cdr a) (cons (car a) b) c)))
    (define a->c
      (lambda (prog a b c)
        (run1 prog (cdr a) b (cons (car a) c))))
    (define b->a
      (lambda (prog a b c)
        (run1 prog (cons (car b) a) (cdr b) c)))
    (define b->c
      (lambda (prog a b c)
        (run1 prog a (cdr b) (cons (car b) c))))
    (define c->a
      (lambda (prog a b c)
        (run1 prog (cons (car c) a) b (cdr c))))
    (define c->b
      (lambda (prog a b c)
        (run1 prog a (cons (car c) b) (cdr c))))
    (if (null? prog)
        (values a b c)
        (let ([ins (car prog)] [prog (cdr prog)])
          (case ins
            [(a->b) (a->b prog a b c)]
            [(a->c) (a->c prog a b c)]
            [(b->a) (b->a prog a b c)]
            [(b->c) (b->c prog a b c)]
            [(c->a) (c->a prog a b c)]
            [(c->b) (c->b prog a b c)])))))

(assert (equal? '(() () (1 2))
                (let-values ([(a b c)
                              (run1 '(a->b b->c a->c) '(2 1) '() '())])
                  (list a b c))))

(define-syntactic-monad $ prog a b c)
(define run2
  ($ lambda ()
    ($ define (a->b)
      ($ run2 ([a (cdr a)] [b (cons (car a) b)])))
    ($ define (a->c)
      ($ run2 ([a (cdr a)] [c (cons (car a) c)])))
    ($ define (b->a)
      ($ run2 ([a (cons (car b) a)] [b (cdr b)])))
    ($ define (b->c)
      ($ run2 ([b (cdr b)] [c (cons (car b) c)])))
    ($ define (c->a)
      ($ run2 ([a (cons (car c) a)] [c (cdr c)])))
    ($ define (c->b)
      ($ run2 ([b (cons (car c) b)] [c (cdr c)])))
    (if (null? prog)
        (values a b c)
        (let ([ins (car prog)] [prog (cdr prog)])
          (case ins
            [(a->b) ($ a->b)]
            [(a->c) ($ a->c)]
            [(b->a) ($ b->a)]
            [(b->c) ($ b->c)]
            [(c->a) ($ c->a)]
            [(c->b) ($ c->b)])))))

(assert (equal? '(() () (1 2))
                (let-values ([(a b c)
                              (run2 '(a->b b->c a->c) '(2 1) '() '())])
                  (list a b c))))

;; Local Variables:
;; mode: scheme
;; End:
