;;;
;;; serializer.scm - generic serializer framework
;;;
;;;  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: serializer.scm,v 1.1 2001-04-03 11:14:50 shiro Exp $
;;;

;; THIS MODULE IS UNDER DEVELOPMENT.
;;  This is a straightforward port from the module I wrote for STk before.
;;  I need to rethink API to make it reasonably extendable.

(define-module gauche.serializer
  (export <serializer>
          write-to-serializer
          read-from-serializer
          port-of
          direction-of
          serializer?
          input-serializer?
          output-serializer?
          write-to-string-with-serializer
          write-to-file-with-serializer
          read-from-string-with-serializer
          read-from-file-with-serializer
          get-serializable-slots)
  )
(select-module gauche.serializer)

;;;
;;; Class <SERIALIZER>
;;;
;;;   Defines a common interface of the serializer
;;;

(define-class <serializer> ()
  ((port       :init-keyword :port      :getter port-of)
   (direction  :init-keyword :direction :getter direction-of)
   ))

(define-method initialize ((self <serializer>) initargs)
  (next-method)
  (if (or (not (slot-bound? self 'port))
          (not (or (input-port? (port-of self))
                   (output-port? (port-of self)))))
      (error "<serializer> class requires :port argument in initialization"))
  (if (not (slot-bound? self 'direction))
      (if (input-port? (port-of self))
          (slot-set! self 'direction :in)
          (slot-set! self 'direction :out))
      (let ((dir (direction-of self)))
        (unless (or (and (eq? dir :in) (input-port? (port-of self)))
                    (and (eq? dir :out) (output-port? (port-of self))))
          (error "While initializing <serializer>, port type and direction don't match"))))
  )

(define-method write-to-serializer ((self <serializer>) object)
  ;; need to be implemented by a derived class
  '()
  )

(define-method read-from-serializer ((self <serializer>))
  ;; need to be implemented by a derived class
  '()
  )

(define (serializer? obj) (is-a? obj <serializer>))
(define (input-serializer? obj)
  (and (serializer? obj) (eq? (direction-of obj) :in)))
(define (output-serializer? obj)
  (and (serializer? obj) (eq? (direction-of self) :out)))

;; Utility method

(define-method write-to-string-with-serializer ((class <class>) obj . options)
  (let ((out (open-output-string)))
    (write-to-serializer
     (apply make class :port out :direction :out options)
     obj)
    (get-output-string out)))

(define-method read-from-string-with-serializer ((class <class>) str . options)
  (let ((in (open-input-string str)))
    (read-from-serializer
     (apply make class :port in :direction :in options))))

(define-method write-to-file-with-serializer ((class <class>) obj file . options)
  (call-with-output-file file
   (lambda (port)
     (write-to-serializer
      (apply make class :port port :direction :out options)
      obj))))

(define-method read-from-file-with-serializer ((class <class>) file . options)
  (call-with-input-file file
   (lambda (port)
     (read-from-serializer
      (apply make class :port port :direction :in options)))))

;; For generic object serializer
(define-method get-serializable-slots ((obj <object>))
  (let loop ((slots (class-slots (class-of obj)))
             (result '()))
    (cond ((null? slots) (reverse result))
          ((eqv? (slot-definition-allocation (car slots)) :virtual)
           (loop (cdr slots) (cons (slot-definition-name (car slots)) result)))
          (else (loop (cdr slots) result)))))

(provide "gauche/serializer")

