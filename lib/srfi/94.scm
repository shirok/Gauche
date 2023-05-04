;;;
;;; SRFI-94 Type-Restricted Numerical Functions
;;;

;; Most of the procedures in SRFI-94 are built-in.

(define-module srfi.94
  (export
   ;; built-in
   real-exp real-ln real-sin real-cos real-tan
   real-asin real-acos real-atan atan
   real-sqrt integer-sqrt integer-expt
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

;; Returns max k such that base^k <= n
(define (integer-log base n)
  ;; WRITEME
  )

(define (ln z) (log z))

(define (abs x)
  (assume (real? x))
  ((with-module gauche abs) x))

;; quo and rem is a kind of middle-ground between R6RS div/mod
;; and R7RS truncate-quotient/trunctate-remainder.
;; They allow non-integer arguments like the former, but round
;; towards zero like the latter.

(define (quo n d)
  (if (or (negative? n) (negative? d))
    (- (div (- n) d))
    (div n d)))

(define (rem n d)
  (- n (* d (quo n d))))

(define (mod n d)
  (let1 r (rem n d)
    (if (and (negative? r) (negative? n))
      (- r)
      r)))

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
