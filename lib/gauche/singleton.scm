;;;
;;; singleton.scm - implements singleton mixin
;;;
;;;  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: singleton.scm,v 1.2 2002-10-15 04:06:47 shirok Exp $
;;;

;; NB: singleton is moved to gauche.mop.singleton.
;; This file provides backward compatibility.

(define-module gauche.singleton
  (extend gauche.mop.singleton))

(provide "gauche/singleton")
