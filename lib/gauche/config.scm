;;;
;;; gauche/config.scm - retrieve configuration information from Scheme
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
;;;  $Id: config.scm,v 1.1 2002-10-23 23:38:42 shirok Exp $
;;;

;; This module simply calls gauche-config script to retrieve the
;; configuration parameter of the running Scheme.

(define-module gauche.config
  (use gauche.process)
  (use file.util)
  (use srfi-13)
  (export gauche-config))
(select-module gauche.config)

(define *gauche-config*
  (build-path (gauche-architecture-directory) "gauche-config"))

(define (gauche-config param)
  (let1 s (process-output->string #`",*gauche-config* ,param")
    (if (string-prefix? "Usage: gauche-config [option]" s)
        (error "unknown configuration parameter name" param)
        s)))

(provide "gauche/config")
