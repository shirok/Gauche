;;;
;;; SRFI-4  homogeneous numeric vectors
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
;;;  $Id: srfi-4.scm,v 1.6 2002-08-30 00:18:53 shirok Exp $
;;;

;; Procedures of SRFI-4 are now defined in gauche.uvector.

(define-module srfi-4
  (extend gauche.uvector)
  )

(provide "srfi-4")
