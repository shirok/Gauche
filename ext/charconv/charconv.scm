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
;;;  $Id: charconv.scm,v 1.4 2001-06-08 09:36:52 shirok Exp $
;;;

(define-module gauche.charconv
  (export open-input-conversion-port
          open-output-conversion-port
          ces-conversion-supported?
          ces-convert
          ces-guess-from-string))

(select-module gauche.charconv)

(dynamic-load "libcharconv")

(define (ces-convert string fromcode . args)
  (let-optional* args ((tocode #f))
    (port->string
     (open-input-conversion-port (open-input-string string) fromcode
                                 :to-code tocode
                                 :buffer-size (string-size string)))))

(provide "gauche/charconv")
