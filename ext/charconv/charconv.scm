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
;;;  $Id: charconv.scm,v 1.7 2001-12-23 03:38:52 shirok Exp $
;;;

(define-module gauche.charconv
  (use gauche.let-opt)
  (export open-input-conversion-port
          open-output-conversion-port
          ces-conversion-supported?
          ces-convert
          ces-guess-from-string))

(select-module gauche.charconv)

(dynamic-load "libcharconv" :export-symbols #t)

(define (ces-convert string fromcode . args)
  (let-optionals* args ((tocode #f))
    (let ((out (open-output-string)))
      (copy-port
       (open-input-conversion-port (open-input-string string) fromcode
                                   :to-code tocode
                                   :buffer-size (string-size string))
       out :unit 'byte)
      (get-output-string out))))

(provide "gauche/charconv")
