;;;
;;; cgi.scm - CGI utility
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
;;;  $Id: cgi.scm,v 1.1 2001-09-18 20:00:04 shirok Exp $
;;;

;; Surprisingly, there's no ``formal'' definition of CG.
;; The most reliable document I found is in <http://CGI-Spec.Golux.Com/>

(define-module cgi
  (use srfi-1)
  (use srfi-13)
  (use rfc.uri)
  (export cgi-get-input)
  )
(select-module cgi)

(define (cgi-get-input)
  (let ((method (sys-getenv "REQUEST_METHOD")))
    (cond ((not method)  ;; interactive use.
           (if (sys-isatty (current-input-port))
               (begin
                 (display "Enter parameters (name=value).  ^D to stop.\n")
                 (flush)
                 (let loop ((line (read-line))
                            (params '()))
                   (format #t "~s\n" params)
                   (if (eof-object? line)
                       (string-join (reverse params) "&")
                       (loop (read-line) (cons line params)))))
               (error "REQUEST_METHOD not defined")))
          ((or (string-ci=? method "GET")
               (string-ci=? method "HEAD"))
           (or (sys-getenv "QUERY_STRING") ""))
          ((string-ci=? method "POST")
           (string-concatenate (port->string-list (current-input-port))))
          (else (error "unknown REQUEST_METHOD" method)))))




(provide "cgi")
