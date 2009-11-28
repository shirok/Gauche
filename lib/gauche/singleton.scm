;;;
;;; singleton.scm - implements singleton mixin
;;;  $Id: singleton.scm,v 1.4 2005-09-12 03:40:00 shirok Exp $
;;;

;; NB: singleton is moved to gauche.mop.singleton.
;; This file provides backward compatibility.

(define-module gauche.singleton
  (extend gauche.mop.singleton))

(warn "gauche.singleton is obsoleted.  use gauche.mop.sigleton.")

