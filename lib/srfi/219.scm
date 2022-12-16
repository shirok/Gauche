;;
;; SRFI-219 - Define higher-order lambda
;;

;; The feature is natively supported in Gauche.  This is for the portable code.
;; NB: SRFI-219 exports null#define, for the portable code using SRFI-219
;; most likely expects it.
(define-module srfi.219
  (define-syntax vanilla-define (with-module null define))
  (export (rename vanilla-define define)))
