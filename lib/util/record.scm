;;;
;;; util/record.scm - guile and SCM compatible make-record-type
;;;
;;;  Copyright(C) 2002 by Alex Shinn (foof@synthcode.com)
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

;;; This code is originally written by Alex Shinn as a patch to
;;; srfi-9.  Shiro Kawai modified it slightly to be an independent
;;; module.

(define-module util.record
  (extend srfi-9)
  (use srfi-1)
  (export record? make-record-type
          record-constructor record-predicate record-accessor
          record-modifier record-type-descriptor record-type-name
          record-type-fields))
(select-module util.record)

;;
;; Guile and SCM compatible make-record-type
;;

(define (record? obj) (is-a? obj <record>))

(define (make-record-type name fields)
  (unless (every symbol? fields)
    (error "make-record-type: fields must be a list of symbols" fields))
  (unless (= (length fields) (length (delete-duplicates fields)))
    (error "make-record-type: fields must not contain duplicates" fields))
  (make <class>
    :supers (list <record>)
    :slots (map list fields)
    :name (string->symbol (x->string name))))

(define (record-constructor record-class :optional (fields (record-type-fields record-class)))
  (lambda args
    (let ((rec (make record-class)))
      (for-each (lambda (f v) (slot-set! rec f v)) fields args)
      rec)))

(define (record-predicate record-class)
  (lambda (obj) (is-a? obj record-class)))

(define (record-accessor record-class field)
  (lambda (rec) (slot-ref rec field)))

(define (record-modifier record-class field)
  (lambda (rec value) (slot-set! rec field value)))

(define record-type-descriptor class-of)

(define (record-type-name record-class)
  (slot-ref record-class 'name))

(define (record-type-fields record-class)
  (map (^x (if (pair? x) (car x) x))
       (compute-slots record-class)))

