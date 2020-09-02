;;; Copyright (C) 2020 Wolfgang Corcoran-Mathe

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:

;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(import (scheme base))
(import (scheme write))
(import (srfi 178))

(cond-expand
  ((library (srfi 78))
   (import (srfi 78)))
  (else
    (begin
      (define *tests-failed* 0)
      (define-syntax check
        (syntax-rules (=>)
          ((check expr => expected)
           (if (equal? expr expected)
             (begin
               (display 'expr)
               (display " => ")
               (display expected)
               (display " ; correct")
               (newline))
             (begin
               (set! *tests-failed* (+ *tests-failed* 1))
               (display "FAILED: for ")
               (display 'expr)
               (display " expected ")
               (display expected)
               (display " but got ")
               (display expr)
               (newline))))))
      (define (check-report)
        (if (zero? *tests-failed*)
            (begin
             (display "All tests passed.")
             (newline))
            (begin
             (display "TESTS FAILED: ")
             (display *tests-failed*)
             (newline)))))))

(cond-expand
  ((library (srfi 158))
   (import (only (srfi 158) generator-for-each generator->list)))
  (else
   (begin
    (define (generator-for-each proc g)
      (let ((v (g)))
        (unless (eof-object? v)
          (proc v)
          (generator-for-each proc g))))

    (define (generator->list g)
      (let ((v (g)))
        (if (eof-object? v)
            '()
            (cons v (generator->list g))))))))

(define (print-header message)
  (newline)
  (display ";;; ")
  (display message)
  (newline))

;;;; Utility

(define (proc-or a b) (or a b))

(define (constantly x) (lambda (_) x))

(define bitvector= bitvector=?)

(define (check-bit-conversions)
  (print-header "Checking bit conversions...")

  (check (bit->integer 0)  => 0)
  (check (bit->integer 1)  => 1)
  (check (bit->integer #f) => 0)
  (check (bit->integer #t) => 1)
  (check (bit->boolean 0)  => #f)
  (check (bit->boolean 1)  => #t)
  (check (bit->boolean #f) => #f)
  (check (bit->boolean #t) => #t))

(define (check-predicates)
  (print-header "Checking predicates...")

  (check (bitvector? (bitvector))        => #t)
  (check (bitvector? (make-bitvector 1)) => #t)

  (check (bitvector-empty? (bitvector))   => #t)
  (check (bitvector-empty? (bitvector 1)) => #f)

  (check (bitvector= (bitvector) (bitvector)) => #t)
  (check (bitvector= (bitvector 1 0 0) (bitvector 1 0 0)) => #t)
  (check (bitvector= (bitvector 1 0 0) (bitvector 1 0 1)) => #f)
  (check (bitvector= (bitvector 1 0 0) (bitvector 1 0))   => #f)
  (check (bitvector= (bitvector 1 0 0)
                     (bitvector 1 0 0)
                     (bitvector 1 0 0))
   => #t)
  (check (bitvector= (bitvector 1 0 0)
                     (bitvector 1 0 1)
                     (bitvector 1 0 0))
   => #f))

(include "test/constructors.scm")
(include "test/iterators.scm")
(include "test/selectors.scm")
(include "test/conversions.scm")
(include "test/mutators.scm")
(include "test/quasi-string.scm")
(include "test/gen-accum.scm")
(include "test/logic-ops.scm")
(include "test/quasi-ints.scm")
(include "test/fields.scm")

(define (check-all)
  ;; Check predicates, bitvector conversions, and selectors first,
  ;; since they're used extensively in later tests.
  (check-predicates)
  (check-bitvector-conversions)
  (check-selectors)
  (check-bit-conversions)
  (check-constructors)
  (check-iterators)
  (check-quasi-string-ops)
  (check-mutators)
  (check-bitwise-operations)
  (check-quasi-integer-operations)
  (check-bit-field-operations)

  (newline)
  (check-report))

(check-all)
