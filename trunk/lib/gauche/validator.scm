;;;
;;; gauche/validator.scm - validator slot option
;;;

;; NB: validator is moved to gauche.mop.validator.
;; This file provides backward compatibility.

(define-module gauche.validator
  (extend gauche.mop.validator))

(warn "gauche.validator is obsoleted.  use gauche.mop.validator.")

