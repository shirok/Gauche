;;;
;;; uri.scm - parse and construct URIs
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
;;;  $Id: uri.scm,v 1.2 2001-06-29 20:01:00 shirok Exp $
;;;

;; Main reference:
;; RFC2396 Uniform Resource Identifiers (URI): Generic Syntax
;;  <ftp://ftp.isi.edu/in-notes/rfc2396.txt>

;; Historical:
;; RFC1738 Uniform Resource Locators
;;  <ftp://ftp.isi.edu/in-notes/rfc1738.txt>
;; RFC1808 Relative Uniform Resource Locators
;;  <ftp://ftp.isi.edu/in-notes/rfc1808.txt>
;; RFC2368 The mailto URL Scheme
;;  <ftp://ftp.isi.edu/in-notes/rfc2368.txt>

(define-module rfc.uri
  (use srfi-13)
  (use gauche.regexp)
  )
(select-module rfc.uri)

;;==============================================================
;; Generic parser
;;

;; Splits URI scheme and the scheme specific part from given URI.
;; If URI doesn't follow the generic URI syntax, it is regarded
;; as a relative URI and #f is returned for the scheme.
;; The escaped characters of the scheme specific part is not unescaped;
;; their interpretation is dependent on the scheme.

(define (uri-scheme&specific uri)
  (cond ((rxmatch #/^([A-Za-z][A-Za-z0-9+.-]*):/ uri)
         => (lambda (match)
              (values (string-downcase (rxmatch-substring match 1))
                      (rxmatch-after match))))
        (else (values #f uri))))

(define (uri-decompose-hierarchical specific)
  (rxmatch-if
      (rxmatch #/^(\/\/([^\/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?$/ specific)
      (#f #f authority path #f query #f fragment)
    (values authority path query fragment)
    (values #f #f #f #f)))

(define (uri-decompose-authority authority)
  (rxmatch-if
      (rxmatch #/^([^@]*@)?([^:]*)(:(\d*))?$/ authority)
      (#f userinfo host #f port)
    (values userinfo host port)
    (values #f #f #f)))



;;==============================================================
;; Relative -> Absolute
;;


;;==============================================================
;; Encoding & decoding
;;



(provide "rfc/uri")
