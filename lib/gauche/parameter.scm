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
;;;  $Id: parameter.scm,v 1.6 2002-12-11 08:57:07 shirok Exp $
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
   (filter :init-keyword :filter :init-value #f)
   (setter)
   (getter)
   (pre-observers)
   (post-observers)
   ))

(define-method initialize ((self <parameter>) initargs)
  (next-method)
  (let ((index (%vm-make-parameter-slot))
        (filter (slot-ref self 'filter))
        (pre-hook #f)
        (post-hook #f))
    (slot-set! self 'getter (lambda () (%vm-parameter-ref index)))
    (slot-set! self 'setter
               (if filter
                   (lambda (val)
                     (let ((new (filter val))
                           (old (%vm-parameter-ref index)))
                       (when pre-hook (run-hook pre-hook old new))
                       (%vm-parameter-set! index new)
                       (when post-hook (run-hook post-hook old new))
                       old))
                   (lambda (val)
                     (let ((old (%vm-parameter-ref index)))
                       (when pre-hook (run-hook pre-hook old val))
                       (%vm-parameter-set! index val)
                       (when post-hook (run-hook post-hook old val))
                       old))))
    (let-syntax ((hook-ref
                  (syntax-rules ()
                    ((_ var)
                     (lambda ()
                       (or var
                           (let ((h (make-hook 2)))
                             (set! var h)
                             h)))))))
      (slot-set! self 'pre-observers (hook-ref pre-hook))
      (slot-set! self 'post-observers (hook-ref post-hook)))
    ))

(define-method object-apply ((self <parameter>) . maybe-newval)
  (if (pair? maybe-newval)
      (if (null? (cdr maybe-newval))
          ((slot-ref self 'setter) (car maybe-newval))
          (error "wrong number of arguments for parameter" maybe-newval))
      ((slot-ref self 'getter))))

(define (make-parameter value . maybe-filter)
  (let1 p (make <parameter> :filter (get-optional maybe-filter #f))
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
  ((ref self 'pre-observers)))

(define-method parameter-post-observers ((self <parameter>))
  ((ref self 'post-observers)))

(define-method parameter-observer-add! ((self <parameter>) proc . args)
  (let-optionals* args ((when 'after)
                        (where 'append))
    (unless (memq when '(before after))
      (error "`when' argument of parameter-observer-add! must be either 'before or 'after" when))
    (unless (memq where '(prepend append))
      (error "`where' argument of parameter-observer-add! must be either 'prepend or 'append" when))
    (add-hook! (if (eq? when 'before)
                   (parameter-pre-observers self)
                   (parameter-post-observers self))
               proc
               (eq? where 'append))))
                
(define-method parameter-observer-delete! ((self <parameter>) proc . args)
  (let ((where (get-optional args #f)))
    (unless (eq? where 'after)
      (delete-hook! (parameter-pre-observers self) proc))
    (unless (eq? where 'before)
      (delete-hook! (parameter-post-observers self) proc))
    ))

(provide "gauche/parameter")

     
     