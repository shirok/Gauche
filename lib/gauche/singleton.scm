;;;
;;; singleton.scm - implements singleton mixin
;;;

;; NB: singleton is moved to gauche.mop.singleton.
;; This file provides backward compatibility.

(define-module gauche.singleton
  (extend gauche.mop.singleton))

(warn "gauche.singleton is obsoleted.  use gauche.mop.sigleton.")

