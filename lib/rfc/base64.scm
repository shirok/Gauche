;;;
;;; base64.scm - base64 encoding/decoding routine
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
;;;  $Id: base64.scm,v 1.1 2001-03-30 09:37:14 shiro Exp $
;;;

;; Implements Base64 encoding/decoding routine
;; Ref: RFC2045 section 6.8  <http://www.rfc-editor.org/rfc/rfc2045.txt>

(define-module rfc.base64
  (export base64-encode base64-encode-string
          base64-decode base64-decode-string))
(select-module rfc.base64)





(provide "rfc/base64")

