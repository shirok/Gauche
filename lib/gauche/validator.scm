;;;
;;; gauche/validator.scm - validator slot option
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
;;;  $Id: validator.scm,v 1.3 2001-12-01 21:44:24 shirok Exp $
;;;

(define-module gauche.validator
  (export <validator-meta>)
  )
(select-module gauche.validator)

(define-class <validator-meta> (<class>)
  ())

;; TODO: need to enable :init-value / :init-thunk 

(define-method compute-get-n-set ((class <validator-meta>) slot)
  (cond ((slot-definition-option slot :validator #f)
         => (lambda (validator)
              (unless (procedure? validator)
                (error "a procedure required for validator, but got"
                       validator))
              (let ((acc (compute-slot-accessor class slot (next-method))))
                (list (lambda (o) (slot-ref-using-accessor o acc))
                      (lambda (o v)
                        (slot-set-using-accessor o acc (validator o v)))
                      ;; the last #t enables initialization by :initform etc.
                      #t))))
        (else (next-method))))

(provide "gauche/validator")
