;;;
;;; auxsys - Auxiliary system interface
;;;
;;;  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: auxsys.scm,v 1.1 2002-02-21 08:55:52 shirok Exp $
;;;

(define-module gauche.auxsys
  (export-all)
  )
(select-module gauche.auxsys)

(dynamic-load "auxsys")

(provide "gauche/auxsys")
