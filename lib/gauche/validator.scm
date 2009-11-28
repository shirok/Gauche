;;;
;;; gauche/validator.scm - validator slot option
;;;  $Id: validator.scm,v 1.7 2005-09-12 03:40:00 shirok Exp $
;;;

;; NB: validator is moved to gauche.mop.validator.
;; This file provides backward compatibility.

(define-module gauche.validator
  (extend gauche.mop.validator))

(warn "gauche.validator is obsoleted.  use gauche.mop.validator.")

