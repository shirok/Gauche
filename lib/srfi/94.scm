;;;
;;; SRFI-94 Type-Restricted Numerical Functions
;;;

;; Most of the procedures in SRFI-94 are built-in.

(define-module srfi.94
  (export
   ;; built-in
   real-exp real-ln real-sin real-cos real-tan
   real-asin real-acos real-atan atan
   real-sqrt integer-expt real-expt
   make-rectangular make-polar

   ;; SRFI-94 specific
   real-log integer-sqrt integer-log quo rem mod ln abs
   quotient remainder modulo
   ))
(select-module srfi.94)

;; SRFI-94's real-log takes base first, while R6RS log takes optional base
;; in the second argument.  To avoid confusion, Gauche does not provide
;; built-in real-log.
(define (real-log base x)
  (/ (real-ln x) (real-ln base)))

(define integer-sqrt exact-integer-sqrt)

;; Returns max n such that base^n <= k
;; Based on the reference implementation
(define (integer-log base k)
  (define (ilog m b k n)
    (if (< k b)
      (values k n)
      (let1 n (+ n m)
        (receive (q nn) (ilog (+ m m) (* b b) (quotient k b) n)
          (if (< q b)
            (values q nn)
            (values (quotient q b) (+ nn m)))))))
  (assume (and (exact? base) (>= base 1))
          "Base must be exact integer >= 1, but got: " base)
  (assume (and (exact? k) (>= k 0))
          "K must be exact integer >= 1, but got: " k)
  (if (< k base)
    0
    (values-ref (ilog 1 base (quotient k base) 1) 1)))

(define (ln z) (log z))

(define (abs x)
  (assume (real? x))
  ((with-module gauche abs) x))

;; quo and rem is a kind of middle-ground between R6RS div/mod
;; and R7RS truncate-quotient/trunctate-remainder.
;; They allow non-integer arguments like the former, but round
;; towards zero like the latter.

(define (quo n d) (truncate (/ n d)))
(define (rem n d) (- n (* d (quo n d))))
(define (mod n d) (- n (* d (floor (/ n d)))))

;; SRFI-94's quotient, remainder and modulo require exact integers,
;; while Gauche's built-ins allow inexact integers.

(define (quotient n1 n2)
  (assume (exact-integer? n1))
  (assume (exact-integer? n2))
  ((with-module gauche quotient) n1 n2))

(define (remainder n1 n2)
  (assume (exact-integer? n1))
  (assume (exact-integer? n2))
  ((with-module gauche remainder) n1 n2))

(define (modulo n1 n2)
  (assume (exact-integer? n1))
  (assume (exact-integer? n2))
  ((with-module gauche remainder) n1 n2))
