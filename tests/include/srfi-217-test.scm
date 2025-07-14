;;; Copyright (C) 2020 Wolfgang Corcoran-Mathe
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included
;;; in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
;;; OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(import (scheme base)
        (scheme write)
        (srfi 217)
        (only (srfi 1) iota any every last take-while drop-while count
                       fold filter remove last partition)
        )

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

;;; (chibi test) / SRFI 78 shims

(define-syntax test-assert
  (syntax-rules ()
    ((_ expr)
     (check expr => #t))))

(define-syntax test-not
  (syntax-rules ()
    ((_ expr)
     (check expr => #f))))

(define-syntax test
  (syntax-rules ()
    ((_ expected expr)
     (check expr => expected))))

(define-syntax test-equal
  (syntax-rules ()
    ((_ equal expected expr)
     (check (equal expr expected) => #t))))

;; Reify multiple values as lists.  Unfortunately, this looks rather
;; ugly on output.  Why doesn't SRFI 78 provide a real MV test form?
(define-syntax test-values
  (syntax-rules ()
    ((test-values expected expr)
     (check (let-values ((vs expr)) vs)
      => (let-values ((us expected)) us)))))

;;;; Utility

(define (print-header message)
  (newline)
  (display ";;; ")
  (display message)
  (display " ...")
  (newline))

(define (init xs)
  (if (null? (cdr xs))
      '()
      (cons (car xs) (init (cdr xs)))))

(define (constantly x)
  (lambda (_) x))

(define pos-seq (iota 20 100 3))
(define neg-seq (iota 20 -100 3))
(define mixed-seq (iota 20 -10 3))
(define sparse-seq (iota 20 -10000 1003))

(define pos-set (list->iset pos-seq))
(define pos-set+ (iset-adjoin pos-set 9))
(define neg-set (list->iset neg-seq))
(define mixed-set (list->iset mixed-seq))
(define dense-set (make-range-iset 0 49))
(define sparse-set (list->iset sparse-seq))

(define all-test-sets
  (list pos-set neg-set mixed-set dense-set sparse-set))

;; Most other test groups use iset=?, so test this first.
(define (check-iset=?)
  (print-header "iset=?")

  (test-assert (iset=? (iset) (iset)))
  (test-not (iset=? (iset 1) (iset)))
  (test-not (iset=? (iset) (iset 1)))
  (test-assert (iset=? (iset 1 2 3 4) (iset 1 2 3 4)))
  (test-assert (iset=? (iset 1 2 3 4) (iset 2 1 4 3) (iset 3 2 1 4)))
  (test-not (iset=? (iset 1 2 3 4) (iset 2 3 4)))
  (test-not (iset=? pos-set neg-set))
  )

(define (check-copying-and-conversion)
  (print-header "Copying and conversion")

  ;;; iset-copy
  (test-assert (not (eqv? (iset-copy pos-set) pos-set)))
  (test-assert (every (lambda (set)
                        (iset-every? (lambda (n) (iset-contains? set n))
                                     (iset-copy set)))
                      all-test-sets))

  ;;; iset->list

  (test '() (iset->list (iset)))
  (test pos-seq (iset->list pos-set))
  (test neg-seq (iset->list neg-set))
  (test mixed-seq (iset->list mixed-set))
  (test sparse-seq (iset->list sparse-set))

  (test-equal iset=? (iset 1) (list->iset! (iset) '(1)))
  (test-equal iset=?
              (iset-adjoin pos-set 2 4 6)
              (list->iset! (iset-copy pos-set) '(2 4 6)))
  )

(define (check-constructors)
  (print-header "Constructors")

  (test-equal iset=?
              (list->iset (iota 10 0 4))
              (iset-unfold (lambda (i) (> i 36))
                           values
                           (lambda (i) (+ i 4))
                           0))

  (test-equal iset=?
              (list->iset (iota 20 -10))
              (make-range-iset -10 10))
  (test-equal iset=?
              (list->iset (iota 10 -10 2))
              (make-range-iset -10 10 2))
  )

(define (check-predicates)
  (print-header "Predicates")

  (test-not (iset-contains? (iset) 1))
  (test-assert (every (lambda (n) (iset-contains? pos-set n))
                      (iota 20 100 3)))
  (test-assert (not (any (lambda (n) (iset-contains? pos-set n))
                         (iota 20 -100 3))))

  (test-assert (iset-empty? (iset)))
  (test-not (iset-empty? pos-set))

  (test-assert (iset-disjoint? (iset) (iset)))
  (test-assert (iset-disjoint? pos-set neg-set))
  (test-assert (iset-disjoint? (iset) pos-set))
  (test-not (iset-disjoint? dense-set sparse-set))
  (test-not (iset-disjoint? (make-range-iset 20 30) (make-range-iset 29 39)))
  )

(define (check-accessors)
  (print-header "Accessors")

  (test 103 (iset-member pos-set 103 #f))
  (test 'z (iset-member pos-set 104 'z))

  (test-not (iset-min (iset)))
  (test 1 (iset-min (iset 1 2 3)))
  (test (car pos-seq) (iset-min pos-set))
  (test (car neg-seq) (iset-min neg-set))
  (test (car mixed-seq) (iset-min mixed-set))

  (test-not (iset-max (iset)))
  (test 3 (iset-max (iset 1 2 3)))
  (test (last pos-seq) (iset-max pos-set))
  (test (last neg-seq) (iset-max neg-set))
  (test (last mixed-seq) (iset-max mixed-set))
  )

(define (check-updaters)
  (print-header "Updaters")

  (test '(1) (iset->list (iset-adjoin (iset) 1)))
  (test-assert (iset-contains? (iset-adjoin neg-set 10) 10))
  (test-assert (iset-contains? (iset-adjoin dense-set 100) 100))
  (test-assert (iset-contains? (iset-adjoin sparse-set 100) 100))
  (test-equal iset=?
              (list->iset (cons -3 (iota 20 100 3)))
              (iset-adjoin pos-set -3))

  (test '() (iset->list (iset-delete (iset 1) 1)))
  (test-not (iset-contains? (iset-delete neg-set 10) 10))
  (test-not (iset-contains? (iset-delete dense-set 1033) 1033))
  (test-not (iset-contains? (iset-delete sparse-set 30) 30))
  (test-equal iset=?
              (list->iset (cdr (iota 20 100 3)))
              (iset-delete pos-set 100))

  (test-assert (iset-empty? (iset-delete-all (iset) '())))
  (test-equal iset=? pos-set (iset-delete-all pos-set '()))
  (test-equal iset=?
              (iset 100 103 106)
              (iset-delete-all pos-set (iota 17 109 3)))

  ;; iset-search insertion
  (test-assert
   (call-with-values
    (lambda ()
      (iset-search mixed-set
                   1
                   (lambda (insert _) (insert #t))
                   (lambda (x update _) (update 1 #t))))
    (lambda (set _) (iset=? (iset-adjoin mixed-set 1) set))))

  ;; iset-search ignore
  (test-assert
   (call-with-values
    (lambda ()
      (iset-search mixed-set
                   1
                   (lambda (_ ignore) (ignore #t))
                   (lambda (x _ remove) (remove #t))))
    (lambda (set _) (iset=? mixed-set set))))

  ;; iset-search update with same element.
  (test-assert
   (call-with-values
    (lambda ()
      (iset-search mixed-set
                   2
                   (lambda (insert _) (insert #t))
                   (lambda (x update _) (update 2 #t))))
    (lambda (set _) (iset=? mixed-set set))))

  ;; iset-search update with different element.
  (test-assert
   (call-with-values
    (lambda ()
      (iset-search mixed-set
                   2
                   (lambda (insert _) (insert #t))
                   (lambda (x update _) (update 3 #t))))
    (lambda (set _)
      (iset=? (iset-adjoin (iset-delete mixed-set 2) 3) set))))

  ;; iset-search remove
  (test-assert
   (call-with-values
    (lambda ()
      (iset-search mixed-set
                   2
                   (lambda (_ ignore) (ignore #t))
                   (lambda (x _ remove) (remove #t))))
    (lambda (set _) (iset=? (iset-delete mixed-set 2) set))))

  ;;; iset-delete-min / -max

  (test-values (values #t #t)
               (let-values (((n mixed-set*) (iset-delete-min mixed-set)))
                 (values (= n (car mixed-seq))
                         (iset=? mixed-set* (list->iset (cdr mixed-seq))))))
  (test-values (values #t #t)
               (let-values (((n sparse-set*) (iset-delete-min sparse-set)))
                 (values (= n (car sparse-seq))
                         (iset=? sparse-set* (list->iset (cdr sparse-seq))))))

  (test-values (values #t #t)
               (let-values (((n mixed-set*) (iset-delete-max mixed-set)))
                 (values (= n (last mixed-seq))
                         (iset=? mixed-set* (list->iset (init mixed-seq))))))
  (test-values (values #t #t)
               (let-values (((n sparse-set*) (iset-delete-max sparse-set)))
                 (values (= n (last sparse-seq))
                         (iset=? sparse-set* (list->iset (init sparse-seq))))))
  )

(define (check-whole-set)
  (print-header "Whole set operations")

  (test 0 (iset-size (iset)))
  (test (length pos-seq) (iset-size pos-set))
  (test (length mixed-seq) (iset-size mixed-set))
  (test (length sparse-seq) (iset-size sparse-set))

  (test 8 (iset-find even? (iset 1 3 5 7 8 9 10) (lambda () #f)))
  (test 'z (iset-find negative? pos-set (lambda () 'z)))

  (test #f (iset-any? even? (iset)))
  (test-assert (iset-any? even? pos-set))
  (test-not (iset-any? negative? pos-set))
  (test-assert (iset-any? (lambda (n) (> n 100)) sparse-set))
  (test-not (iset-any? (lambda (n) (> n 100)) dense-set))

  (test #t (iset-every? even? (iset)))
  (test-not (iset-every? even? pos-set))
  (test-assert (iset-every? negative? neg-set))
  (test-not (iset-every? (lambda (n) (> n 100)) sparse-set))
  (test-assert (iset-every? (lambda (n) (< n 100)) dense-set))

  (test 0 (iset-count even? (iset)))
  (test (count even? pos-seq) (iset-count even? pos-set))
  (test (count even? neg-seq) (iset-count even? neg-set))
  (test (count even? sparse-seq) (iset-count even? sparse-set))
  )

(define (check-iterators)
  (print-header "Iterators")

  ;;; folds

  (test (fold + 0 pos-seq) (iset-fold + 0 pos-set))
  (test (fold + 0 sparse-seq) (iset-fold + 0 sparse-set))
  (test (iset-size neg-set) (iset-fold (lambda (_ c) (+ c 1)) 0 neg-set))
  (test (reverse pos-seq) (iset-fold cons '() pos-set))
  (test (reverse mixed-seq) (iset-fold cons '() mixed-set))

  (test (fold + 0 pos-seq) (iset-fold-right + 0 pos-set))
  (test (fold + 0 sparse-seq) (iset-fold-right + 0 sparse-set))
  (test (iset-size neg-set) (iset-fold-right (lambda (_ c) (+ c 1)) 0 neg-set))
  (test pos-seq (iset-fold-right cons '() pos-set))
  (test mixed-seq (iset-fold-right cons '() mixed-set))

  ;;; iset-map

  (test-assert (iset-empty? (iset-map values (iset))))
  (test-equal iset=? pos-set (iset-map values pos-set))
  (test-equal iset=?
              (list->iset (map (lambda (n) (* n 2)) mixed-seq))
              (iset-map (lambda (n) (* n 2)) mixed-set))
  (test-equal iset=? (iset 1) (iset-map (constantly 1) pos-set))

  ;;; iset-for-each

  (test (iset-size mixed-set)
        (let ((n 0))
          (iset-for-each (lambda (_) (set! n (+ n 1))) mixed-set)
          n))
  (test (fold + 0 sparse-seq)
        (let ((sum 0))
          (iset-for-each (lambda (n) (set! sum (+ sum n))) sparse-set)
          sum))
  (test (reverse mixed-seq)
        (let ((xs '()))
          (iset-for-each (lambda (n) (set! xs (cons n xs))) mixed-set)
          xs))

  ;;; filter, remove, & partition

  (test-assert (iset-empty? (iset-filter (constantly #f) pos-set)))
  (test-equal iset=?
              pos-set
              (iset-filter (constantly #t) pos-set))
  (test-equal iset=?
              (list->iset (filter even? mixed-seq))
              (iset-filter even? mixed-set))
  (test-assert (iset-empty? (iset-remove (constantly #t) pos-set)))
  (test-equal iset=?
              pos-set
              (iset-remove (constantly #f) pos-set))
  (test-equal iset=?
              (list->iset (remove even? mixed-seq))
              (iset-remove even? mixed-set))
  (test-assert
   (let-values (((in out) (iset-partition (constantly #f) pos-set)))
     (and (iset-empty? in) (iset=? pos-set out))))
  (test-assert
   (let-values (((in out) (iset-partition (constantly #t) pos-set)))
     (and (iset=? pos-set in) (iset-empty? out))))
  (test-assert
   (let-values (((in out) (iset-partition even? mixed-set))
                ((lin lout) (partition even? mixed-seq)))
     (and (iset=? in (list->iset lin))
          (iset=? out (list->iset lout)))))
  )

(define (check-comparison)
  (print-header "Comparison")

  (test-assert (iset<? (iset) pos-set))
  (test-assert (iset<? pos-set pos-set+))
  (test-not    (iset<? pos-set pos-set))
  (test-not    (iset<? pos-set+ pos-set))
  (test-assert (iset<? (iset) pos-set pos-set+))
  (test-not    (iset<? (iset) pos-set pos-set))
  (test-assert (iset<=? (iset) pos-set))
  (test-assert (iset<=? pos-set pos-set+))
  (test-assert (iset<=? pos-set pos-set))
  (test-not    (iset<=? pos-set+ pos-set))
  (test-assert (iset<=? (iset) pos-set pos-set+))
  (test-assert (iset<=? (iset) pos-set pos-set))
  (test-not    (iset>? (iset) pos-set))
  (test-not    (iset>? pos-set pos-set+))
  (test-not    (iset>? pos-set pos-set))
  (test-assert (iset>? pos-set+ pos-set))
  (test-assert (iset>? pos-set+ pos-set (iset)))
  (test-not    (iset>? pos-set+ pos-set pos-set))
  (test-not    (iset>=? (iset) pos-set))
  (test-not    (iset>=? pos-set pos-set+))
  (test-assert (iset>=? pos-set pos-set))
  (test-assert (iset>=? pos-set+ pos-set))
  (test-assert (iset>=? pos-set+ pos-set (iset)))
  (test-assert (iset>=? pos-set+ pos-set pos-set))
  )

(define (check-set-theory)
  (print-header "Set theory")

  (test-equal iset=? mixed-set (iset-union! (iset) mixed-set))
  (test-equal iset=?
              (list->iset (append (iota 20 100 3) (iota 20 -100 3)))
              (iset-union pos-set neg-set))
  (test-equal iset=? pos-set (iset-union pos-set pos-set))
  (test-equal iset=?
              (list->iset (iota 30 100 3))
              (iset-union pos-set (list->iset (iota 20 130 3))))
  (test-equal iset=?
              (list->iset (iota 10))
              (iset-union (iset 0 1 2) (iset 3 5 8) (iset 4 6 7 9)))

  ;; iset-intersection
  (test-assert (iset-empty? (iset-intersection (iset) mixed-set)))
  (test-equal iset=? neg-set (iset-intersection neg-set neg-set))
  (test-equal iset=? (iset -97) (iset-intersection (iset -97) neg-set))
  (test-equal iset=? (iset) (iset-intersection pos-set neg-set))
  (test-equal iset=?
              (list->iset (drop-while negative? mixed-seq))
              (iset-intersection mixed-set dense-set))
  (test-equal iset=?
              (iset 0 1)
              (iset-intersection (iset 0 1 2) (iset 0 1 3 4) (iset 10 7 0 1)))

  ;; iset-difference
  (test-assert (iset-empty? (iset-difference neg-set neg-set)))
  (test-equal iset=? pos-set (iset-difference pos-set neg-set))
  (test-equal iset=? pos-set (iset-difference pos-set neg-set))
  (test-equal iset=?
              (iset 100)
              (iset-difference pos-set (list->iset (cdr pos-seq))))
  (test-equal iset=?
              (list->iset (take-while negative? mixed-seq))
              (iset-difference mixed-set dense-set))
  (test-equal iset=?
              (iset 0 1)
              (iset-intersection (iset 0 1 2 5) (iset 0 1 3 4) (iset 10 7 0 1)))

  ;; iset-xor
  (test-equal iset=? mixed-set (iset-xor (iset) mixed-set))
  (test-equal iset=?
              (list->iset (append (iota 20 100 3) (iota 20 -100 3)))
              (iset-xor pos-set neg-set))
  (test-equal iset=? (iset) (iset-xor pos-set pos-set))
  (test-equal iset=?
              (list->iset '(100 103 106))
              (iset-xor pos-set (list->iset (iota 17 109 3))))
  )

(define (check-subsets)
  (print-header "Subsets")

  (test-assert (iset-empty? (iset-open-interval (iset) 0 10)))
  (test-equal iset=?
              (iset 103 106)
              (iset-open-interval pos-set 100 109))
  (test-assert (iset-empty? (iset-open-interval neg-set 0 50)))

  (test-assert (iset-empty? (iset-closed-interval (iset) 0 10)))
  (test-equal iset=?
              (iset 100 103 106 109)
              (iset-closed-interval pos-set 100 109))
  (test-assert (iset-empty? (iset-closed-interval neg-set 0 50)))

  (test-assert (iset-empty? (iset-open-closed-interval (iset) 0 10)))
  (test-equal iset=?
              (iset 103 106 109)
              (iset-open-closed-interval pos-set 100 109))
  (test-assert (iset-empty? (iset-open-closed-interval neg-set 0 50)))

  (test-assert (iset-empty? (iset-closed-open-interval (iset) 0 10)))
  (test-equal iset=?
              (iset 100 103 106)
              (iset-closed-open-interval pos-set 100 109))
  (test-assert (iset-empty? (iset-closed-open-interval neg-set 0 50)))

  ;;; isubset*

  (test-assert (iset-empty? (isubset= pos-set 90)))
  (test-equal iset=? (iset 100) (isubset= pos-set 100))

  (test-assert (iset-empty? (isubset< (iset) 10)))
  (test-equal iset=?
              (iset 100 103 106)
              (isubset< pos-set 109))
  (test-equal iset=?
              (iset -10 -7)
              (isubset< mixed-set -4))
  (test-assert (iset-empty? (isubset< mixed-set -15)))

  (test-assert (iset-empty? (isubset<= (iset) 10)))
  (test-equal iset=?
              (iset 100 103 106 109)
              (isubset<= pos-set 109))
  (test-equal iset=?
              (iset -10 -7 -4)
              (isubset<= mixed-set -4))
  (test-assert (iset-empty? (isubset<= mixed-set -15)))

  (test-assert (iset-empty? (isubset> (iset) 10)))
  (test-equal iset=?
              (iset 151 154 157)
              (isubset> pos-set 148))
  (test-equal iset=?
              (iset 41 44 47)
              (isubset> mixed-set 38))
  (test-assert (iset-empty? (isubset> mixed-set 50)))

  (test-assert (iset-empty? (isubset>= (iset) 10)))
  (test-equal iset=?
              (iset 148 151 154 157)
              (isubset>= pos-set 148))
  (test-equal iset=?
              (iset 38 41 44 47)
              (isubset>= mixed-set 38))
  (test-assert (iset-empty? (isubset>= mixed-set 50)))
  )

(define (check-all)
  (check-iset=?)
  (check-copying-and-conversion)
  (check-constructors)
  (check-predicates)
  (check-accessors)
  (check-updaters)
  (check-whole-set)
  (check-iterators)
  (check-comparison)
  (check-set-theory)
  (check-subsets)
  (newline)
  (check-report))

(check-all)
