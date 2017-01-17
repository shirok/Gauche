;;;
;;; srfi-66 - Octet vectors
;;;

;; It is mostly a subset of srfi-4, except

;;  u8vector=?       - gauche.uvector provides this
;;  u8vector-compare - gauche.uvector provides this
;;  u8vector-copy!   - this has different argument order

(define-module srfi-66
  (use gauche.uvector :rename ((u8vector-copy! gauche:u8vector-copy!)))
  (export u8vector? make-u8vector u8vector
          u8vector->list list->u8vector
          u8vector-length u8vector-ref u8vector-set!
          u8vector=? u8vector-compare
          u8vector-copy! u8vector-copy))
(select-module srfi-66)

(define (u8vector-copy! src src-start target target-start n)
  (gauche:u8vector-copy! target target-start src src-start (+ n src-start)))



