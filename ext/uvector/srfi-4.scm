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
;;;  $Id: srfi-4.scm,v 1.4 2002-06-27 10:41:40 shirok Exp $
;;;

;; Procedures of SRFI-4 are now defined in gauche.uvector.

(define-module srfi-4
  (use gauche.uvector)

  ;; hack until i implement a proper module inheritance ...
  (define-macro (extend module)
    (cons 'begin
          (hash-table-map (module-table (find-module module))
                          (lambda (k v)
                            `(define ,k (with-module ,module ,k))))))
  (extend gauche.uvector)
  (export-all)
  )

(provide "srfi-4")
