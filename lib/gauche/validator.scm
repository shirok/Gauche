;;;
;;; gauche/validator.scm - validator slot option
;;;  $Id: validator.scm,v 1.6 2003-07-05 03:29:11 shirok Exp $
;;;

;; NB: validator is moved to gauche.mop.validator.
;; This file provides backward compatibility.

(define-module gauche.validator
  (extend gauche.mop.validator))

(provide "gauche/validator")
