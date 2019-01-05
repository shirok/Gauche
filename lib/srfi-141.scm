;;;
;;; srfi-141
;;;

(define-module srfi-141
  (export ceiling/ ceiling-quotient ceiling-remainder
          round/ round-quotient round-remainder
          euclidean/ euclidean-quotient euclidean-remainder
          balanced/ balanced-quotient balanced-remainder

          ;; These are in core (R7RS)
          floor/ floor-quotient floor-remainder
          truncate/ truncate-quotient truncate-remainder))
(select-module srfi-141)

(define (ceiling/ n d)
  (receive (q r) (quotient&remainder n d)
    (if (>= n 0)
      (if (and (> d 0) (not (zero? r)))
        (values (+ q 1) (- r d))
        (values q r))
      (if (or (> d 0) (zero? r))
        (values q r)
        (values (+ q 1) (- r d))))))

(define (ceiling-quotient n d)
  (values-ref (ceiling/ n d) 0))

(define (ceiling-remainder n d)
  (values-ref (ceiling/ n d) 1))

;; euclidean is same as R6RS div/mod, except checks for integers.

(define (euclidean/ n d)
  (check-arg integer? n)
  (check-arg integer? d)
  (div-and-mod n d))

(define (euclidean-quotient n d)
  (check-arg integer? n)
  (check-arg integer? d)
  (div n d))

(define (euclidean-remainder n d)
  (check-arg integer? n)
  (check-arg integer? d)
  (mod n d))

;; balanced is same as R6RS div0/mod0, except checks for integers.

(define (balanced/ n d)
  (check-arg integer? n)
  (check-arg integer? d)
  (div0-and-mod0 n d))

(define (balanced-quotient n d)
  (check-arg integer? n)
  (check-arg integer? d)
  (div0 n d))

(define (balanced-remainder n d)
  (check-arg integer? n)
  (check-arg integer? d)
  (mod0 n d))

;; round and balanced only differ when n/d exactly falls on the midpoint.
;; in inexact case, we simply use 'round' for it's faster.  we do need
;; zero divisor check for that.

(define (%exact-round/ n d)
  (receive (q r) (div0-and-mod0 n d)
    (if (and (odd? q)
             (even? d)
             (= (* (abs r) 2) (abs d)))
      (if (> d 0)
        (values (- q 1) (- r))
        (values (+ q 1) (- r)))
      (values q r))))

(define (round/ n d)
  (check-arg integer? n)
  (check-arg integer? d)
  (when (zero? d) (error "Attempt to calculate a division by zero"))
  (if (and (exact? n) (exact? d))
    (%exact-round/ n d)
    (let1 q (round (/ n d))
      (values q (- n (* d q))))))

(define (round-quotient n d)
  (check-arg integer? n)
  (check-arg integer? d)
  (when (zero? d) (error "Attempt to calculate a division by zero"))
  (if (and (exact? n) (exact? d))
    (values-ref (%exact-round/ n d) 0)
    (round (/ n d))))

(define (round-remainder n d)
  (check-arg integer? n)
  (check-arg integer? d)
  (when (zero? d) (error "Attempt to calculate a division by zero"))
  (if (and (exact? n) (exact? d))
    (values-ref (%exact-round/ n d) 1)
    (- n (* d (round (/ n d))))))
