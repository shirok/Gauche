;;;
;;; singleton.scm - implements singleton mixin
;;;  $Id: singleton.scm,v 1.3 2003-07-05 03:29:11 shirok Exp $
;;;

;; NB: singleton is moved to gauche.mop.singleton.
;; This file provides backward compatibility.

(define-module gauche.singleton
  (extend gauche.mop.singleton))

(provide "gauche/singleton")
