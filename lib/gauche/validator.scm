;;;
;;; gauche/validator.scm - validator slot option
;;;
;;;  Copyright(C) 2001-2002 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: validator.scm,v 1.5 2002-10-07 08:59:43 shirok Exp $
;;;

;; NB: validator is moved to gauche.mop.validator.
;; This file provides backward compatibility.

(define-module gauche.validator
  (extend gauche.mop.validator))

(provide "gauche/validator")
