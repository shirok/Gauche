;;;
;;; parameter.scm - parameter support
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
;;;  $Id: parameter.scm,v 1.4 2002-12-10 13:16:09 shirok Exp $
;;;

;; The API is upper-compatible to ChezScheme and Chicken's.

(define-module gauche.parameter
  (use gauche.hook)
  (export <parameter> make-parameter parameterize
          parameter-pre-observers
          parameter-post-observers
          parameter-observer-add!
          parameter-observer-delete!)
  )
(select-module gauche.parameter)

(define-class <parameter> ()
  (;; all slots should be private
   (filter :init-keyword :filter :init-value identity)
   (setter)
   (getter)
   (pre-observers  :init-value #f)
   (post-observers :init-value #f)
   ))

(define-method initialize ((self <parameter>) initargs)
  (next-method)
  (let ((index (%vm-make-parameter-slot))
        (filter (slot-ref self 'filter)))
    (slot-set! self 'getter (lambda () (%vm-parameter-ref index)))
    (slot-set! self 'setter
               (lambda (val)
                 (let ((new (filter val))
                       (old (%vm-parameter-ref index)))
                   (cond ((slot-ref self 'pre-observers)
                          => (cut run-hook <> new)))
                   (%vm-parameter-set! index new)
                   (cond ((slot-ref self 'post-observers)
                          => (cut run-hook <> new)))
                   old)))
    ))

(define-method object-apply ((self <parameter>) . maybe-newval)
  (if (pair? maybe-newval)
      (if (null? (cdr maybe-newval))
          ((slot-ref self 'setter) (car maybe-newval))
          (error "wrong number of arguments for parameter" maybe-newval))
      ((slot-ref self 'getter))))

(define (make-parameter value . maybe-filter)
  (let1 p (make <parameter> :filter (get-optional maybe-filter identity))
    (p value)
    p))

(define-syntax parameterize
  (syntax-rules ()
    ((_ (binds ...) . body)
     (%parameterize () () () () (binds ...) body))))

(define-syntax %parameterize
  (syntax-rules ()
    ((_ (param ...) (val ...) (tmp1 ...) (tmp2 ...) () body)
     (let ((tmp1 val) ... (tmp2 #f) ...)
       (dynamic-wind
        (lambda () (set! tmp2 (param tmp1)) ...)
        (lambda () . body)
        (lambda () (param tmp2) ...))))
    ((_ (param ...) (val ...) (tmp1 ...) (tmp2 ...) ((p v) . more) body)
     (%parameterize (param ... p) (val ... v) (tmp1 ... tmp1a) (tmp2 ... tmp2a) more body))
    ((_ params vals vars other body)
     (syntax-error "malformed binding list for parameterize" other))
    ))

;; hooks

(define-method parameter-pre-observers ((self <parameter>))
  (or (slot-ref self 'pre-observers)
      (let1 h (make-hook 1)
        (slot-set! self 'pre-observers h)
        h)))

(define-method parameter-post-observers ((self <parameter>))
  (or (slot-ref self 'post-observers)
      (let1 h (make-hook 1)
        (slot-set! self 'post-observers h)
        h)))

(define-method parameter-observer-add! ((self <parameter>) proc . args)
  (let ((before? #f)
        (append? #f))
    (for-each (lambda (opt)
                (case opt
                  ((before)  (set! before? #t))
                  ((after)   (set! before? #f))
                  ((prepend) (set! append? #f))
                  ((append)  (set! append? #t))
                  (else (error "bad option for parameter-observer-add!: must be a combination of {before, after} and {prepend, append}" opt))))
              args)
    (add-hook! (if before?
                   (parameter-pre-observers self)
                   (parameter-post-observers self))
               proc append?)))
                
(define-method parameter-observer-delete! ((self <parameter>) proc . where)
  (let ((before? (case (get-optional where 'after)
                   ((before) #t)
                   ((after) #f)
                   (else (error "bad option for parameter-observer-delete!: must be either before or after" (car where))))))
    (delete-hook! (if before?
                      (parameter-pre-observers self)
                      (parameter-post-observers self))
                  proc)))

(provide "gauche/parameter")

     
     