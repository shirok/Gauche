;;;
;;; srfi-9.scm - defining record types
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
;;;  $Id: srfi-9.scm,v 1.1 2001-07-08 19:17:34 shirok Exp $
;;;

;; Implements record types on top of Gauche objects.
;;
;;  * A record type is realized as a class inherits <record>.
;;  * Accessors, and modifiers are realized as a method.

(define-module srfi-9
  (use srfi-1)
  (export <record> define-record-type))
(select-module srfi-9)

(define-class <record> ()
  ())

(define-syntax define-record-type
  (syntax-rules ()
    ((_ type
        (constructor init-tag ...)
        predicate
        (field accessor . maybe-modifier) ...)
     (begin
       (ensure-record-consistency 'type '(init-tag ...) '(field ...))
       (define-class type (<record>)
         (field ...))
       (define (constructor init-tag ...)
         (let ((instance (make type)))
           (slot-set! instance 'init-tag init-tag) ...
           instance))
       (define (predicate self)
         (is-a? self type))
       (define-record-field type field accessor . maybe-modifier) ...)
     )))

;; auxiliary macro
(define-syntax define-record-field
  (syntax-rules ()
    ((_ type field accessor)
     (define-method accessor ((self type))
       (slot-ref self 'field)))
    ((_ type field accessor modifier)
     (begin
       (define-method accessor ((self type))
         (slot-ref self 'field))
       (define-method modifier ((self type) value)
         (slot-set! self 'field value))))
    ))

;; auxiliary proc
(define (ensure-record-consistency type init-tags fields)
  (cond ((find (lambda (x) (not (symbol? x))) fields)
         => (lambda (x)
              (errorf "invalid define-record-type for ~a: field name must be a symbol, but got ~s" type x))))
  (for-each (lambda (init-tag)
              (unless (memq init-tag fields)
                (errorf "invalid define-record-type for ~a: constructor definition contains a tag ~a which is not a field" type init-tag)))
            init-tags))

(provide "srfi-9")
