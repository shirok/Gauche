;;;
;;; charconv - character code conversion module
;;;
;;;  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: charconv.scm,v 1.1 2001-06-04 03:59:57 shirok Exp $
;;;

(define-module gauche.charconv
  (export open-input-conversion-port
          open-output-conversion-port
          ces-conversion-supported?
          ces-convert))

(select-module gauche.charconv)

(dynamic-load "charconv")

(define (ces-convert string fromcode . args)
  (if (null? args)
      (port->string
       (open-input-conversion-port (open-input-string string) fromcode))
      (port->byte-string
       (open-input-conversion-port (open-input-string string) fromcode
                                   (car args)))))

(provide "gauche/charconv")
