;;;
;;; termios - termios interface
;;;
;;;  Copyright(C) 2001-2003 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: termios.scm,v 1.4 2003-01-09 11:36:51 shirok Exp $
;;;

(define-module gauche.termios
  (export-all)
  )

(select-module gauche.termios)

(dynamic-load "termios" :export-symbols #t)


(provide "gauche/termios")


