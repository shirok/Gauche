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
(import (srfi 1))
(import (srfi 196))

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
   (import (only (srfi 158) generator->list)))
  (else
   (begin
    (define (generator->list g)
      (let ((v (g)))
        (if (eof-object? v)
            '()
            (cons v (generator->list g))))))))

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

(define always (constantly #t))
(define never (constantly #f))

(define (range=/eqv? ra rb)
  (range=? eqv? ra rb))

(define (%range-empty? r) (zero? (range-length r)))

;;;; Test ranges

(define test-num-range (numeric-range 10 30))

(define test-num-seq (iota 20 10))

(define test-empty-range (numeric-range 0 0))

;; Produces the range {#f, #t}.
(define test-bool-range
  (range 2 (lambda (n) (not (zero? n)))))

;;;; Conversion

(define (check-conversion)
  (print-header "Running conversion tests...")

  (check (range->list test-empty-range) => '())
  (check (range->list test-bool-range)  => '(#f #t))
  (check (range->list test-num-range)   => test-num-seq)

  (check (generator->list (range->generator test-num-range))
   => test-num-seq)

  (check (vector->list (range->vector test-num-range)) => test-num-seq)

  (check (range->string test-empty-range) => "")
  (let ((s "0123456789"))
    (check (range->string (string-range s)) => s))

  (let* ((vec (vector 1 3 5 7 9))
         (vrange (vector->range vec)))
    (check (range-length vrange)  => (vector-length vec))
    (check (range-first vrange)   => (vector-ref vec 0))
    (check (range-last vrange)    => (vector-ref vec 4))
    (check (range->vector vrange) => vec)
    (check (range->list (begin (vector-set! vec 0 0) vrange))
     => '(1 3 5 7 9)))
)

(define (check-constructors)
  (print-header "Running constructor tests...")

  (check (%range-empty? (numeric-range 1 1))      => #t)
  (check (range->list (numeric-range -5 -1))      => (iota 4 -5))
  (check (range->list (numeric-range 1 -5 -1))    => (iota 6 1 -1))
  (check (range->list (numeric-range 4/3 16/3))   => (iota 4 4/3))
  (check (range->list (numeric-range 0 9 4))      => (iota 3 0 4))
  (check (%range-empty? (numeric-range 0 10 -1))  => #t)
  (check (%range-empty? (numeric-range 0 -10))    => #t)
  (check (range->list (numeric-range 5 1 -1))     => (iota 4 5 -1))
  (check (range->list (numeric-range -2 2))       => (iota 4 -2))
  (check (range->list (numeric-range 2 -2 -1))    => (iota 4 2 -1))
  (check (range->list (numeric-range -4 -8 -1))   => (iota 4 -4 -1))
  (check (range->list (numeric-range -1 -4 -2/3)) => (iota 5 -1 -2/3))

  (check (range=/eqv? (iota-range 10 0 0)
                      (range 10 (lambda (_) 0)))
   => #t)
  (check (%range-empty? (iota-range 0))     => #t)
  (check (range->list (iota-range 10))      => (iota 10))
  (check (range->list (iota-range 10 0))    => (iota 10))
  (check (range->list (iota-range 10 0 1))  => (iota 10))
  (check (range->list (iota-range 10 10 2)) => (iota 10 10 2))
  (check (range->list (iota-range 10 0 -1)) => (iota 10 0 -1))
  (check (range->list (iota-range 10 5 -2)) => (iota 10 5 -2))
  (check (range->list (iota-range 10 1/2))  => (iota 10 1/2))

  (let ((vec (vector 1 3 5 7 9)))
    (check (range-length (vector-range vec))  => (vector-length vec))
    (check (range-first (vector-range vec))   => (vector-ref vec 0))
    (check (range-last (vector-range vec))    => (vector-ref vec 4))
    (check (range->vector (vector-range vec)) => vec))

  (let* ((s "0123456789") (srange (string-range s)))
    (check (range-length srange) => (string-length s))
    (check (range-first srange)  => (string-ref s 0))
    (check (range-last srange)   => (string-ref s 9))
    (check (range->list srange)  => (string->list s)))
)

;;;; Predicates

(define (check-predicates)
  (print-header "Running predicate tests...")

  (check (range=? eqv? (numeric-range 0 0) (numeric-range 5 5))  => #t)
  (check (range=? eqv? (numeric-range 0 0) test-num-range)       => #f)
  (check (range=? eqv? test-num-range test-num-range)            => #t)
  (check (range=? eqv? test-num-range (numeric-range 10 30))     => #t)
  (check (range=? eqv? test-num-range (numeric-range 10 20))     => #f)
  (check (range=? eqv? test-bool-range (vector-range #(#f #t))) => #t)
  (check (range=? eqv? test-bool-range (vector-range #(#t #f))) => #f)
  (check (range=? eqv?
                  test-num-range
                  (numeric-range 10 30)
                  (subrange (numeric-range 0 50) 10 30))
   => #t)
  (check (range=? eqv?
                  test-bool-range
                  (numeric-range 10 30)
                  (subrange (numeric-range 0 50) 10 30))
   => #f)
  (check (range=? eqv?
                  test-num-range
                  (numeric-range 11 31)
                  (subrange (numeric-range 0 50) 10 30))
   => #f)
)

;;;; Accessors

(define (check-accessors)
  (print-header "Running accessor tests...")

  (check (range-ref test-num-range 0)  => 10)
  (check (range-ref test-bool-range 1) => #t)
)

;;;; Iteration

(define (check-iteration)
  (print-header "Running iteration tests...")

  ;; Check lengths of ranges returned by range-split-at.
  (let ((n 10))
    (check (let-values (((ra rb) (range-split-at test-num-range n)))
             (list (range-length ra) (range-length rb)))
     => (list n (- (range-length test-num-range) n))))

  ;; Joining the two ranges returned by range-split-at gives the
  ;; original range.
  (check (let-values (((ra rb) (range-split-at test-bool-range 1)))
           (range=/eqv? (range-append ra rb) test-bool-range))
   => #t)

  (check (range=/eqv?
          (subrange test-bool-range 0 (range-length test-bool-range))
          test-bool-range)
   => #t)
  (let ((a 5) (b 10))
    (check (= (range-length (subrange test-num-range a b)) (- b a))
     => #t)
    (check (range=/eqv? (subrange test-num-range a b)
                        (range-take (range-drop test-num-range a) (- b a)))
     => #t)
    (check (range=/eqv? (subrange test-num-range 0 b)
                        (range-take test-num-range b))
     => #t)
    (check (range=/eqv?
            (subrange test-num-range a (range-length test-num-range))
            (range-drop test-num-range a))
     => #t))

  ;; range-take r n returns a range of length n.
  (check (range-length (range-take test-num-range 10)) => 10)
  (check (range-length
          (range-take test-num-range (range-length test-num-range)))
   => (range-length test-num-range))
  (check (range->list (range-take test-num-range 5))
   => (take test-num-seq 5))

  ;; range-take-right r n returns a range of length n.
  (check (range-length (range-take-right test-num-range 10)) => 10)
  (check (range-length
          (range-take-right test-num-range (range-length test-num-range)))
   => (range-length test-num-range))
  (check (range->list (range-take-right test-num-range 5))
   => (drop test-num-seq 15))

  ;; range-drop r n returns a range of length (range-length r) - n.
  (check (range-length (range-drop test-num-range 10))
   => (- (range-length test-num-range) 10))
  (check (range-length
          (range-drop test-num-range (range-length test-num-range)))
   => 0)
  (check (range->list (range-drop test-num-range 15))
   => (drop test-num-seq 15))

  ;; range-drop-right r n returns a range of length (range-length r) - n.
  (check (range-length (range-drop-right test-num-range 10))
   => (- (range-length test-num-range) 10))
  (check (range-length
          (range-drop-right test-num-range (range-length test-num-range)))
   => 0)
  (check (range->list (range-drop-right test-num-range 15))
   => (take test-num-seq 5))

  (check (range=/eqv? (car (range-segment test-num-range 5))
                      (range-take test-num-range 5))
   => #t)
  (check (range=/eqv? (apply range-append
                             (cdr (range-segment test-num-range 5)))
                      (range-drop test-num-range 5))
   => #t)
  (check (range=/eqv? (apply range-append (range-segment test-num-range 5))
                      test-num-range)
   => #t)
  (check (fold + 0 (map range-length (range-segment test-num-range 5)))
   => (range-length test-num-range))
  (check (fold + 0 (map range-length (range-segment test-num-range 7)))
   => (range-length test-num-range))

  (check (range-count always test-num-range) => (range-length test-num-range))
  (check (range-count never test-num-range)  => 0)
  (check (range-count even? test-num-range)  => (count even? test-num-seq))
  (check (range-count (lambda (x y) y) test-num-range test-bool-range)
   => 1)
  (check (range-count (lambda (x y) (zero? (+ x y)))
                      test-num-range
                      (range-map - test-num-range))
   => (range-length test-num-range))

  (check (range-any even? test-num-range) => #t)
  (check (range-any never test-num-range) => #f)
  (check (range-any (lambda (x y) y) test-num-range test-bool-range)
   => #t)
  (check (range-any (lambda (x y) (zero? (+ x y)))
                    test-num-range
                    test-num-range)
   => #f)

  (check (range-every number? test-num-range) => #t)
  (check (range-every even? test-num-range)   => #f)
  (check (range-every (lambda (x y) y) test-num-range test-bool-range)
   => #f)
  (check (range-every (lambda (x y) (zero? (+ x y)))
                      test-num-range
                      (range-map - test-num-range))
   => #t)

  ;;; map, filter-map, & for-each

  (check (range=/eqv? (range-map (lambda (x) (+ 1 x)) test-num-range)
                      (numeric-range 11 31))
   => #t)
  (check (equal? (range->list (range-map square test-num-range))
                 (map square test-num-seq))
   => #t)
  (check (range=/eqv? (range-map + test-num-range test-num-range)
                      (numeric-range 20 60 2))
   => #t)
  ;; range-map over ranges with unequal lengths terminates when
  ;; the shortest range is exhausted.
  (check (range=/eqv?
          (range-map (lambda (x _) x) test-num-range test-bool-range)
          (range-take test-num-range (range-length test-bool-range)))
   => #t)

  ;; (range-map->list f r) = (map f (range->list r))
  (check (equal? (range-map->list not test-bool-range)
                 (map not (range->list test-bool-range)))
   => #t)
  (check (equal? (range-map->list + test-num-range test-num-range)
                 (map + test-num-seq test-num-seq))
   => #t)

  ;; (range-map->vector f r) = (map f (range->vector r))
  (check (equal? (range-map->vector not test-bool-range)
                 (vector-map not (range->vector test-bool-range)))
   => #t)
  (let ((num-vec (list->vector test-num-seq)))
    (check (equal? (range-map->vector + test-num-range test-num-range)
                   (vector-map + num-vec num-vec))
     => #t))

  (check (%range-empty? (range-filter-map never test-bool-range)) => #t)
  (check (range=/eqv? (range-filter-map values test-num-range)
                      test-num-range)
   => #t)
  (check (equal?
          (range->list (range-filter-map (lambda (x) (and (even? x) x))
                                         test-num-range))
          (filter-map (lambda (x) (and (even? x) x)) test-num-seq))
   => #t)
  (let ((proc (lambda (x y) (and (even? x) (even? y) (+ x y)))))
    (check (range=/eqv? (range-filter-map proc test-num-range test-num-range)
                        (numeric-range 20 60 4))
     => #t))

  (check (range-filter-map->list never test-bool-range) => '())
  (check (equal? (range-filter-map->list values test-num-range)
                 test-num-seq)
   => #t)
  (check (equal?
          (range-filter-map->list (lambda (x) (and (even? x) x))
                                  test-num-range)
          (filter-map (lambda (x) (and (even? x) x)) test-num-seq))
   => #t)
  (let ((proc (lambda (x y) (and (even? x) (even? y) (+ x y)))))
    (check (equal? (range-filter-map->list proc
                                           test-num-range
                                           test-num-range)
                   (filter-map proc test-num-seq test-num-seq))
     => #t))

  (check (let ((v #f))
           (range-for-each (lambda (x) (set! v x)) test-bool-range)
           v)
   => #t)
  (check (let ((v #f))
           (range-for-each (lambda (x y) (when y (set! v x)))
                           test-num-range
                           test-bool-range)
           v)
   => 11)

  ;;; filter & remove

  (check (range=/eqv? (range-filter always test-bool-range)
                      test-bool-range)
   => #t)
  (check (%range-empty? (range-filter never test-bool-range)) => #t)
  (check (equal? (range->list (range-filter even? test-num-range))
                 (filter even? test-num-seq))
   => #t)

  (check (range-filter->list always test-bool-range) => '(#f #t))

  (check (null? (range-filter->list never test-bool-range)) => #t)

  ;; (range-filter->list pred r) = (filter pred (range->list r))
  (check (equal? (range-filter->list even? test-num-range)
                 (filter even? test-num-seq))
   => #t)

  (check (range=/eqv? (range-remove never test-bool-range)
                      test-bool-range)
   => #t)
  (check (%range-empty? (range-remove always test-bool-range))
   => #t)
  (check (equal? (range->list (range-remove even? test-num-range))
                 (remove even? test-num-seq))
   => #t)

  (check (equal? (range-remove->list never test-bool-range)
                 (range->list test-bool-range))
   => #t)

  (check (null? (range-remove->list always test-bool-range)) => #t)

  ;; (range-remove->list pred r) = (remove pred (range->list r))
  (check (equal? (range-remove->list even? test-num-range)
                 (remove even? test-num-seq))
   => #t)

  ;; (range-fold (lambda (b) (+ 1 b)) 0 r) = (range-length r)
  (check (= (range-fold (lambda (b _) (+ b 1)) 0 test-num-range)
            (range-length test-num-range))
   => #t)

  ;; (range-fold proc nil r) = (fold proc nil (range->list r))
  (check (equal? (range-fold + 0 test-num-range)
                 (fold + 0 test-num-seq))
   => #t)

  (check (= (range-fold + 0 test-num-range test-num-range)
            (fold + 0 test-num-seq test-num-seq))
   => #t)

  ;; range-fold over ranges with unequal lengths terminates when
  ;; the shortest range is exhausted.
  (check (= (range-fold (lambda (s x _) (+ s x))
                        0
                        test-num-range
                        test-bool-range)
            (range-fold + 0 (range-take test-num-range
                                        (range-length test-bool-range))))
   => #t)

  ;; (range-fold-right (lambda (b) (+ 1 b)) 0 r) = (range-length r)
  (check (= (range-fold-right (lambda (b _) (+ b 1)) 0 test-num-range)
            (range-length test-num-range))
   => #t)

  ;; (range-fold-right r proc nil) = (fold-right proc nil (range->list r))
  (check (equal? (range-fold-right + 0 test-num-range)
                 (fold-right + 0 test-num-seq))
   => #t)

  (check (= (range-fold-right + 0 test-num-range test-num-range)
            (fold-right + 0 test-num-seq test-num-seq))
   => #t)

  ;; range-fold-right over ranges with unequal lengths terminates when
  ;; the shortest range is exhausted.
  (check (= (range-fold-right (lambda (s x _) (+ s x))
                              0
                              test-num-range
                              test-bool-range)
            (range-fold-right + 0 (range-take test-num-range
                                              (range-length
                                               test-bool-range))))
   => #t)

  (check (eqv? (range-first (range-reverse test-bool-range))
               (range-last test-bool-range))
   => #t)

  (check (eqv? (range-last (range-reverse test-bool-range))
               (range-first test-bool-range))
   => #t)

  (check (equal? (range->list (range-reverse test-num-range))
                 (reverse test-num-seq))
   => #t)

  (check (%range-empty? (range-append)) => #t)
  (check (range->list (range-append test-bool-range)) => '(#f #t))
  (check (range=/eqv? (range-append (numeric-range 10 20)
                                    (numeric-range 20 30))
                      test-num-range)
   => #t)
  (check (range=/eqv? (range-append (numeric-range 10 15)
                                    (numeric-range 15 20)
                                    (numeric-range 20 25)
                                    (numeric-range 25 30))
                      test-num-range)
   => #t)
)

;;;; Searching

(define (check-searching)
  (print-header "Running search tests...")

  (check (range-index always test-num-range) => 0)
  (check (range-index never test-num-range)  => #f)
  (check (range-index values test-bool-range) => 1)
  (check (range-index (lambda (x y) (and (odd? x) y))
                      test-num-range
                      test-bool-range)
   => 1)

  (check (eqv? (range-index-right always test-num-range)
               (- (range-length test-num-range) 1))
   => #t)
  (check (range-index-right never test-num-range)  => #f)
  (check (range-index-right values test-bool-range) => 1)
  (check (range-index-right (lambda (x y) (< (+ x y) 30))
                            test-num-range
                            test-num-range)
   => 4)

  ;; range-index and range-index-right produce the same index if pred
  ;; is only satisfied by the element at that index.
  (let ((fifteen? (lambda (n) (= n 15))))
    (check (= (range-index fifteen? test-num-range)
              (range-index-right fifteen? test-num-range)
              (list-index fifteen? test-num-seq))
     => #t))

  ;; (range-take-while always r) = r
  (check (range=/eqv? (range-take-while always test-bool-range)
                      test-bool-range)
   => #t)

  ;; (range-take-while never r) = [empty range]
  (check (%range-empty? (range-take-while never test-bool-range)) => #t)

  (let ((pred (lambda (n) (< n 15))))
    (check (range->list (range-take-while pred test-num-range))
     => (take-while pred test-num-seq)))

  ;; (range-drop-while always r) = [empty range]
  (check (%range-empty? (range-drop-while always test-bool-range)) => #t)

  ;; (range-drop-while never r) = r
  (check (range=/eqv? (range-drop-while never test-bool-range)
                      test-bool-range)
   => #t)

  (let ((pred (lambda (n) (< n 15))))
    (check (range->list (range-drop-while pred test-num-range))
     => (drop-while pred test-num-seq)))

  ;; (range-append (range-take-while p r) (range-drop-while p r)) = r
  (let ((pred (lambda (n) (< n 10))))
    (check (range=/eqv?
            (range-append (range-take-while pred test-num-range)
                          (range-drop-while pred test-num-range))
            test-num-range)
     => #t))

  ;; (range-take-while-right always r) = r
  (check (range=/eqv? (range-take-while-right always test-bool-range)
                      test-bool-range)
   => #t)

  ;; (range-take-while-right never r) = [empty range]
  (check (%range-empty? (range-take-while-right never test-bool-range)) => #t)

  (let ((pred (lambda (n) (>= n 15))))
    (check (range->list (range-take-while-right pred test-num-range))
     => (iota 15 15)))

  ;; (range-drop-while-right always r) = [empty range]
  (check (%range-empty? (range-drop-while-right always test-bool-range)) => #t)

  ;; (range-drop-while-right never r) = r
  (check (range=/eqv? (range-drop-while-right never test-bool-range)
                      test-bool-range)
   => #t)

  (let ((pred (lambda (n) (>= n 15))))
    (check (range->list (range-drop-while-right pred test-num-range))
     => (take test-num-seq 5)))

  ;; (range-append (range-drop-while-right p r)
  ;;               (range-take-while-right p r)) = r
  (let ((pred (lambda (n) (< n 10))))
    (check (range=/eqv?
            (range-append (range-drop-while-right pred test-num-range)
                          (range-take-while-right pred test-num-range))
            test-num-range)
     => #t))
)

(define (check-all)
  (check-predicates)
  (check-conversion)
  (check-constructors)
  (check-accessors)
  (check-iteration)
  (check-searching)

  (newline)
  (check-report))

(check-all)
