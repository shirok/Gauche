;; Copyright (C) 2020 Wolfgang Corcoran-Mathe

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:

;; The above copyright notice and this permission notice shall be included
;; in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(import (scheme base))
(import (scheme write))
(import (srfi-189))                     ; draft

(cond-expand
  ((library (srfi 78))
   (import (srfi 78)))
  (else
    (begin
      (define-syntax check
        (syntax-rules (=>)
          ((check expr)
           (check expr => #t))
          ((check expr => expected)
           (if (equal? expr expected)
             (begin
               (display 'expr)
               (display " => ")
               (display expected)
               (display " ; correct")
               (newline))
             (begin
               (display "FAILED: for ")
               (display 'expr)
               (display " expected ")
               (display expected)
               (display " but got ")
               (display expr)
               (newline))
             ))))
      (define (check-report) #t))))

;;;; Utility

(define (identity x) x)

(define (print-header message)
  (newline)
  (display ";;; ")
  (display message)
  (newline))

(define-syntax constantly
  (syntax-rules ()
    ((_ obj) (lambda _ obj))))

;; Gives the value of expr, or 'exception if an exception was raised.
(define-syntax catch-exceptions
  (syntax-rules ()
    ((_ expr)
     (exception-satisfies (constantly 'exception) expr))))

;; If an exception is raised by expr, gives the result of applying pred
;; to the raised object.
(define-syntax exception-satisfies
  (syntax-rules ()
    ((_ pred expr)
     (call-with-current-continuation
      (lambda (k)
        (with-exception-handler
         (lambda (obj) (k (pred obj)))
         (lambda () expr)))))))

;; Gives the values of expr as a list.
(define-syntax values~>list
  (syntax-rules ()
    ((_ expr)
     (call-with-values (lambda () expr) list))))

(define always (constantly #t))
(define never (constantly #f))

;; Verify that a Maybe is a Just of 'z, a dummy object.
(define (just-of-z? m)
  (and (maybe? m) (maybe= eqv? m (just 'z))))

;; Verify that an Either is a Right of 'z, a dummy object.
(define (right-of-z? e)
  (and (either? e) (either= eqv? e (right 'z))))

;; Verify that an Either is a Left of 'z, a dummy object.
(define (left-of-z? e)
  (and (either? e) (either= eqv? e (left 'z))))

;;;; Tests

(define (check-misc)
  ;; Uniqueness of the Nothing object.
  (check (eq? (nothing) (nothing)) => #t)

  ;; either-swap
  (check (either= eqv? (right #t #t) (either-swap (left #t #t))) => #t)
  (check (either= eqv? (left #t #t) (either-swap (right #t #t))) => #t))

;;;; Predicates

(define (check-predicates)
  (print-header "Testing predicates...")

  (check (just? (just 'z))    => #t)
  (check (just? (nothing))    => #f)
  (check (nothing? (just 'z)) => #f)
  (check (nothing? (nothing)) => #t)
  (check (maybe? (just 'z))   => #t)
  (check (maybe? (nothing))   => #t)

  (check (right? (right 'z))  => #t)
  (check (right? (left 'z))   => #f)
  (check (left? (right 'z))   => #f)
  (check (left? (left 'z))    => #t)
  (check (either? (right 'z)) => #t)
  (check (either? (left 'z))  => #t)

  (check (maybe= eqv? (just #t) (just #t)) => #t)
  (check (maybe= eqv? (just #t) (just #f)) => #f)
  (check (maybe= eqv? (nothing) (nothing)) => #t)
  (check (maybe= eqv? (just #t) (nothing)) => #f)

  (check (maybe= eqv? (just #t #f) (just #t #f)) => #t)
  (check (maybe= eqv? (just #t #f) (just #t 'z)) => #f)
  (check (maybe= eqv? (just #t #f) (just #t))    => #f)

  (check (either= eqv? (right #t) (right #t)) => #t)
  (check (either= eqv? (right #t) (right #f)) => #f)
  (check (either= eqv? (left #t) (left #t))   => #t)
  (check (either= eqv? (left #t) (left #f))   => #f)
  (check (either= eqv? (right #t) (left #t))  => #f)

  (check (either= eqv? (right #t #f) (right #t #f)) => #t)
  (check (either= eqv? (right #t #f) (right #t 'z)) => #f)
  (check (either= eqv? (right #t #f) (right #t))    => #f)
  (check (either= eqv? (left #t #f) (left #t #f))   => #t)
  (check (either= eqv? (left #t #f) (left #t 'z))   => #f)
  (check (either= eqv? (left #t #f) (left #t))      => #f)
  (check (either= eqv? (left #t #f) (right #t #f))  => #f))

;;;; Accessors

(define (check-accessors)
  (print-header "Testing accessors...")

  (check (maybe-ref (just #t))                       => #t)
  ;;(check (exception-satisfies maybe-ref-error? (maybe-ref (nothing))) => #t)
  (check (catch-exceptions (maybe-ref (nothing)))    => 'exception)
  (check (maybe-ref (nothing) (lambda () #f))        => #f)
  (check (maybe-ref (just #t) (lambda () #f) values) => #t)
  (check (maybe-ref (nothing) (lambda () #f) values) => #f)

  (check (values~>list (maybe-ref (just #t #f)))      => '(#t #f))
  (check (maybe-ref (just #t #f) (lambda () #f) list) => '(#t #f))

  (check (either-ref (right #t))                        => #t)
  (check (catch-exceptions (either-ref (left #t)))      => 'exception)
  (check (either-ref (left #t) (constantly #f))         => #f)
  (check (either-ref (right #t) (constantly #f) values) => #t)
  (check (either-ref (left #t) values (constantly #f))  => #t)

  (check (values~>list (either-ref (right #t #f)))       => '(#t #f))
  (check (either-ref (right #t #f) (constantly #f) list) => '(#t #f))
  (check (either-ref (left #t #f) list (constantly #f))  => '(#t #f))

  (check (maybe-ref/default (just #t) #f) => #t)
  (check (maybe-ref/default (nothing) #f) => #f)
  (check (values~>list (maybe-ref/default (just #t #t) #f #f)) => '(#t #t))
  (check (values~>list (maybe-ref/default (nothing) #f #f))    => '(#f #f))

  (check (either-ref/default (right #t) #f) => #t)
  (check (either-ref/default (left #t) #f)  => #f)
  (check (values~>list (either-ref/default (right #t #t) #f #f))
    => '(#t #t))
  (check (values~>list (either-ref/default (left #t) #f #f))
    => '(#f #f)))

;;;; Join and bind

(define (check-join-and-bind)
  (print-header "Testing join and bind...")

  ;; maybe-join
  (check (just-of-z? (maybe-join (just (just 'z)))) => #t)
  (check (nothing? (maybe-join (just (nothing))))   => #t)
  (check (nothing? (maybe-join (nothing)))          => #t)
  (check (catch-exceptions (maybe-join (just #t)))  => 'exception)
  (check (catch-exceptions (maybe-join (just (just #t) (just #t))))
    => 'exception)

  ;; either-join
  (check (right-of-z? (either-join (right (right 'z)))) => #t)
  (check (left-of-z? (either-join (right (left 'z))))   => #t)
  (check (left-of-z? (either-join (left 'z)))           => #t)
  (check (catch-exceptions (either-join (right #t)))    => 'exception)
  (check (catch-exceptions (either-join (right (right #t) (right #t))))
    => 'exception)

  ;; maybe-bind
  (check (nothing? (maybe-bind (nothing) just)) => #t)

  (check (just-of-z? (maybe-bind (just 'z) just)) => #t)

  (check (let ((m (just #t #f)))
           (maybe= eqv? m (maybe-bind m just)))
    => #t)

  ;; Associativity of bind.
  (let ((k (lambda (n) (just (* n 2))))
        (h (lambda (n) (just (+ n 5))))
        (m (just 1)))
    (check (maybe= eqv?
                   (maybe-bind m (lambda (n) (maybe-bind (k n) h)))
                   (maybe-bind (maybe-bind m k) h))
      => #t))

  ;; Bind with multiple mprocs.
  (let ((neg (lambda (b) (just (not b)))))
    (check (maybe= eqv? (just #f) (maybe-bind (just #t) neg neg neg))
      => #t)
    (check (nothing? (maybe-bind (just #t) neg (constantly (nothing)) neg))
      => #t))

  ;; maybe-compose
  ;; (check (nothing? ((maybe-compose just) (nothing)))   => #t)
  ;; (check (just-of-z? ((maybe-compose just) (just 'z))) => #t)

  ;; ;; Compose with multiple mprocs.
  ;; (let ((neg (lambda (b) (just (not b)))))
  ;;   (check (maybe= eqv? (just #t) ((maybe-compose neg neg neg) (just #f)))
  ;;     => #t))

  ;; either-bind
  (check (left? (either-bind (left #f) right)) => #t)

  (check (right-of-z? (either-bind (right 'z) right)) => #t)

  (check (let ((e (right #t #f)))
           (either= eqv? e (either-bind e right)))
    => #t)

  ;; Associativity of bind.
  (let ((k (lambda (n) (right (* n 2))))
        (h (lambda (n) (right (+ n 5))))
        (e (right 1)))
    (check
     (either= eqv? (either-bind e (lambda (n) (either-bind (k n) h)))
                   (either-bind (either-bind e k) h))
     => #t))

  ;; Bind with multiple mprocs.
  (let ((neg (lambda (b) (right (not b)))))
    (check (either= eqv? (right #f) (either-bind (right #t) neg neg neg))
      => #t)
    (check (either= eqv? (left #f) (either-bind (right #t) neg left neg))
      => #t))

  ;; ;; either-compose
  ;; (check (left-of-z? ((either-compose right) (left 'z)))               => #t)
  ;; (check (either= eqv? (right #t) ((either-compose right) (right #t))) => #t)

  ;; ;; Compose with multiple mprocs.
  ;; (let ((neg (lambda (b) (right (not b)))))
  ;;   (check (either= eqv? (right #t) ((either-compose neg neg neg) (right #f)))
  ;;     => #t))
  )

;;;; Sequence operations

(define (check-sequence-operations)
  (define (both b c) (and b c))

  (print-header "Testing sequence operations...")

  (check (maybe-length (nothing)) => 0)
  (check (maybe-length (just #t)) => 1)

  (check (either-length (left #t))  => 0)
  (check (either-length (right #t)) => 1)

  ;; maybe-filter & maybe-remove
  (check (just-of-z? (maybe-filter always (just 'z))) => #t)
  (check (nothing? (maybe-filter never (just #t)))    => #t)
  (check (nothing? (maybe-filter always (nothing)))   => #t)

  (check (maybe= eqv? (just #t #t) (maybe-filter both (just #t #t))) => #t)

  (check (just-of-z? (maybe-remove never (just 'z))) => #t)
  (check (nothing? (maybe-remove always (just #t)))  => #t)
  (check (nothing? (maybe-remove always (nothing)))  => #t)

  (check (maybe= eqv? (just #t #f) (maybe-remove both (just #t #f))) => #t)

  ;; maybe-sequence
  (check (maybe= equal? (maybe-sequence (map just '(#t #f)) map identity)
                        (just '(#t #f)))
    => #t)
  (check (maybe= equal? (maybe-sequence (list (just 1 #t) (just 2 #f))
                                        map
                                        list)
                        (just '((1 #t) (2 #f))))
    => #t)
  (check (nothing? (maybe-sequence (list (just #t) (nothing)) map identity))
    => #t)

  ;; either-filter & either-remove
  (check (right-of-z? (either-filter always (right 'z) #f)) => #t)
  (check (left-of-z? (either-filter never (right #t) 'z))   => #t)
  (check (left-of-z? (either-filter always (left #t) 'z))   => #t)

  (check (either= eqv? (right #t #t) (either-filter both (right #t #t) #f))
    => #t)

  (check (right-of-z? (either-remove never (right 'z) #f)) => #t)
  (check (left-of-z? (either-remove always (right #t) 'z)) => #t)
  (check (left-of-z? (either-remove never (left #t) 'z))   => #t)

  (check (either= eqv? (right #t #f) (either-remove both (right #t #f) #f))
    => #t)

  ;; either-sequence
  (check (either= equal? (either-sequence (map right (list 1 2)) map identity)
                         (right (list 1 2)))
    => #t)
  (check (left-of-z? (either-sequence (list (right #t) (left 'z)) map identity))
    => #t)
  (check (either= equal? (either-sequence (list (right 1 #t) (right 2 #f))
                                          map
                                          list)
                         (right '((1 #t) (2 #f))))
    => #t))

;;;; Conversion procedures

(define (check-conversions)
  (print-header "Testing conversions...")

  ;; maybe->either and either->maybe
  (check (left-of-z? (maybe->either (nothing) 'z))                    => #t)
  (check (right-of-z? (maybe->either (just 'z) #f))                   => #t)
  (check (either= eqv? (right #t #t) (maybe->either (just #t #t) #f)) => #t)
  (check (nothing? (either->maybe (left #t)))                         => #t)
  (check (just-of-z? (either->maybe (right 'z)))                      => #t)
  (check (maybe= eqv? (just #t #t) (either->maybe (right #t #t)))     => #t)

  ;; list->just and list->right
  (check (maybe= eqv? (just #t #t) (list->just '(#t #t)))    => #t)
  (check (either= eqv? (right #t #t) (list->right '(#t #t))) => #t)

  ;; maybe->list and either->list
  (check (maybe->list (nothing))      => '())
  (check (maybe->list (just #t #t))   => '(#t #t))
  (check (either->list (right #t #t)) => '(#t #t))
  (check (either->list (left #t #t))  => '(#t #t))

  ;; maybe->lisp and lisp->maybe
  (check (maybe->lisp (nothing))                       => #f)
  (check (maybe->lisp (just #t))                       => #t)
  (check (catch-exceptions (maybe->lisp (just #t #t))) => 'exception)
  (check (nothing? (lisp->maybe #f))                   => #t)
  (check (just-of-z? (lisp->maybe 'z))                 => #t)

  ;; maybe->eof and eof->maybe
  (check (eof-object? (maybe->eof (nothing)))         => #t)
  (check (maybe->eof (just #t))                       => #t)
  (check (catch-exceptions (maybe->eof (just #t #t))) => 'exception)
  (check (nothing? (eof->maybe (eof-object)))         => #t)
  (check (just-of-z? (eof->maybe 'z))                 => #t)

  ;; maybe->values and friends
  (check (maybe->values (just #t))                => #t)
  (check (values~>list (maybe->values (nothing))) => '())

  (check (values~>list (maybe->lisp-values (nothing)))        => '(#f #f))
  (check (values~>list (maybe->lisp-values (just #t)))        => '(#t #t))
  (check (catch-exceptions (maybe->lisp-values (just #t #t))) => 'exception)

  (check (nothing? (values->maybe (lambda () (values)))) => #t)
  (check (just-of-z? (values->maybe (lambda () 'z)))     => #t)
  (check (maybe->values (values->maybe (lambda () #t)))  => #t)
  (check (just-of-z? (values->maybe (lambda ()
                                      (maybe->values (just 'z)))))
    => #t)

  ;; either->values and friends
  (check (either->values (right #t)) => #t)
  (check (values~>list (either->values (left 'z))) => '())

  (check (values~>list (either->lisp-values (left #t)))  => '(#f #f))
  (check (values~>list (either->lisp-values (right #t))) => '(#t #t))
  (check (catch-exceptions (either->lisp-values (right #t #f)))
   => 'exception)

  (check (left-of-z? (values->either (lambda () (values)) 'z)) => #t)
  (check (right-of-z? (values->either (lambda () 'z) #f))      => #t)
  (check (either->values (values->either (lambda () #t) #f))   => #t)
  (check (right-of-z? (values->either (lambda ()
                                        (either->values (right 'z)))
                                      #f))
    => #t))

;;;; Map, fold, and unfold

'(define (check-map-fold-and-unfold)
  (print-header "Testing maps, folds, and unfolds...")

  ;; maybe-map
  (check (nothing? (maybe-map not (nothing)))              => #t)
  (check (maybe= eqv? (just #f) (maybe-map not (just #t))) => #t)

  (check (maybe= eqv? (just #t #f) (maybe-map values (just #t #f))) => #t)

  ;; either-map
  ;; Verify that the result is the same Left (in the sense of eqv?).
  (check (let ((e (left #t))) (eqv? e (either-map not e)))     => #t)
  (check (either= eqv? (right #f) (either-map not (right #t))) => #t)

  (check (let ((e (right #t #f)))
           (either= eqv? e (either-map values e)))
    => #t)

  ;; maybe-for-each
  (check (let ((x #f))
           (maybe-for-each (lambda (y) (set! x y)) (just #t))
           x)
    => #t)
  ; Given Nothing, ensure the proc argument is not executed.
  (check (let ((x #f))
           (maybe-for-each (lambda (_) (set! x #t)) (nothing))
           x)
    => #f)

  ;; either-for-each
  (check (let ((x #f))
           (either-for-each (lambda (y) (set! x y)) (right #t))
           x)
    => #t)
  ;; Given a Left, ensure the proc argument is not executed.
  (check (let ((x #f))
           (either-for-each (lambda (_) (set! x #t)) (left 'z))
           x)
    => #f)

  (check (maybe-fold cons '() (nothing)) => '())
  (check (maybe-fold cons '() (just #t)) => '(#t))
  (check (maybe-fold * 2 (just 3 4))     => 24)

  (check (either-fold cons '() (left #t))  => '())
  (check (either-fold cons '() (right #t)) => '(#t))
  (check (either-fold * 2 (right 3 4))     => 24)

  (check (nothing? (maybe-unfold always not #f #f))             => #t)
  (check (maybe= eqv? (just #t) (maybe-unfold never not #f #f)) => #t)
  (check (maybe= eqv? (just #t 'z)
                      (maybe-unfold never values #f #t 'z))
    => #t)

  (check (left-of-z? (either-unfold always not #f 'z))             => #t)
  (check (either= eqv? (right #t) (either-unfold never not #f #f)) => #t)
  (check (either= eqv? (right #t 'z)
                       (either-unfold never values #f #t 'z))
    => #t)
  (check (either= eqv? (left #t 'z)
                       (either-unfold always values #f #t 'z))
    => #t))

;;;; Conditional syntax

'(define (check-syntax)
  (print-header "Testing syntax...")

  (check (maybe-if (just #t) #t #f)             => #t)
  (check (maybe-if (nothing) #t #f)             => #f)
  (check (catch-exceptions (maybe-if 'z #t #f)) => 'exception))

;;;; Trivalent logic

'(define (check-trivalent)
  (define (tri-true? m)
    (and (just? m) (maybe-ref m)))

  (define (tri-false? m)
    (and (just? m) (not (maybe-ref m))))

  (print-header "Testing trivalent logic...")

  (check (tri-true? (tri-not (just #f)))  => #t)
  (check (tri-false? (tri-not (just #t))) => #t)
  (check (nothing? (tri-not (nothing)))   => #t)

  (check (tri-true? (tri=? (just #t) (just 1) (just 'x))) => #t)
  (check (tri-true? (tri=? (just #f) (just #f)))          => #t)
  (check (tri-true? (tri=? (just #f) (just #f)))          => #t)
  (check (tri-false? (tri=? (just #f) (just #t)))         => #t)
  (check (tri-false? (tri=? (just #f) (nothing)))         => #t)

  (check (tri-true? (tri-and (just #t) (just 1) (just 'x))) => #t)
  (check (nothing? (tri-and (just #t) (nothing)))           => #t)
  (check (tri-false? (tri-and (just #f) (just #t)))         => #t)
  (check (tri-true? (tri-and))                              => #t)

  (check (tri-false? (tri-or (just #f) (just #f) (just #f))) => #t)
  (check (nothing? (tri-or (just #f) (nothing)))             => #t)
  (let ((m-true (just 'x)))
    (check (maybe= eqv? m-true (tri-or (just #f) m-true))    => #t))
  (check (tri-false? (tri-or))                               => #t)

  (check (nothing? (tri-merge (nothing) (nothing) (nothing)))  => #t)
  (let ((m-true (just 'x)))
    (check (maybe= eqv? m-true (tri-merge (nothing) m-true))   => #t))
  (let ((m-false (just #f)))
    (check (maybe= eqv? m-false (tri-merge (nothing) m-false)) => #t))
  (check (nothing? (tri-merge))                                => #t))

(define (check-all)
  (check-misc)
  (check-predicates)
  (check-accessors)
  (check-join-and-bind)
  (check-sequence-operations)
  ;;(check-conversions)
  ;;(check-map-fold-and-unfold)
  ;;(check-syntax)
  ;;(check-trivalent)

  (check-report))

(check-all)
