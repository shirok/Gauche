;;;
;;; propagate-slot.scm - propagate slot option
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
;;;  $Id: propagate-slot.scm,v 1.1 2002-10-07 08:59:43 shirok Exp $
;;;

;; EXPERIMENTAL.   THE API MAY CHANGE.

(define-module gauche.mop.propagate-slot
  (use srfi-2)
  (export <propagate-slot-meta>)
  )
(select-module gauche.mop.propagate-slot)

;; 'propagate' slot option sends get/set request to other object.
;; The idea is taken from STk's "composite metaclass".
;;
;; The slot must have ':propagated' allocation, and ':propagate' option.
;; ':Propagate' option may be a symbol or a list of two elements.
;; Suppose the slot foo is a propagated slot.  If a symbol bar is given
;; to the :propagate option, reading of the slot returns
;; (slot-ref (slot-ref obj 'bar) 'foo), and writing to the slot causes
;; (slot-set! (slot-ref obj 'bar) 'foo value).  If a list (bar baz)
;; is given, baz is used as the actual slot name instead of foo.

(define-class <propagate-slot-meta> (<class>)
  ())

(define-method compute-get-n-set ((class <propagate-slot-meta>) slot)
  (let ((name  (slot-definition-name slot))
        (alloc (slot-definition-allocation slot)))
    (cond ((eq? alloc :propagated)
           (let1 prop (slot-definition-option slot :propagate)
             (cond ((symbol? prop)
                    (list (lambda (o)
                            (slot-ref (slot-ref o prop) name))
                          (lambda (o v)
                            (slot-set! (slot-ref o prop) name v))))
                   ((and-let* (((list? prop))
                               ((= (length prop) 2))
                               (object-slot (car prop))
                               ((symbol? object-slot))
                               (real-slot (cadr prop))
                               ((symbol? real-slot)))
                      (list (lambda (o)
                              (slot-ref (slot-ref o object-slot) real-slot))
                            (lambda (o v)
                              (slot-set! (slot-ref o object-slot) real-slot v))
                            )))
                   (else
                    (errorf "bad :propagated slot option value ~s for slot ~s of class ~s"
                            prop name class))
                   )))
          (else (next-method)))))

(provide "gauche/mop/propagate")
