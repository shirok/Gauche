;;;
;;; gauche/mop/validator.scm - validator slot option
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
;;;  $Id: validator.scm,v 1.4 2002-12-09 13:52:53 shirok Exp $
;;;

(define-module gauche.mop.validator
  (export <validator-meta> <validator-mixin>)
  )
(select-module gauche.mop.validator)

(define-class <validator-meta> (<class>)
  ())

(define-method compute-get-n-set ((class <validator-meta>) slot)
  (let ((pre  (slot-definition-option slot :validator #f))
        (post (slot-definition-option slot :observer #f)))
    (if (or pre post)
        (let* ((acc (compute-slot-accessor class slot (next-method)))
               (getter (lambda (o) (slot-ref-using-accessor o acc)))
               (setter (cond ((and pre post)
                              (lambda (o v)
                                (slot-set-using-accessor o acc (pre o v))
                                (post o (slot-ref-using-accessor o acc))))
                             (pre
                              (lambda (o v)
                                (slot-set-using-accessor o acc (pre o v))))
                             (else
                              (lambda (o v)
                                (slot-set-using-accessor o acc v)
                                (post o (slot-ref-using-accessor o acc)))))))
          ;; the last #t enables initialization by :initform etc.
          (list getter setter #t))
        (next-method))))

;; convenience base class.  you can either inherit <validator-mixin>,
;; or specifying :metaclass <validator-meta> to your class.
(define-class <validator-mixin> ()
  ()
  :metaclass <validator-meta>)

(provide "gauche/mop/validator")
