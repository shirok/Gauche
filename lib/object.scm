;;;
;;; object.scm - object system
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
;;;  $Id: object.scm,v 1.6 2001-03-22 10:38:10 shiro Exp $
;;;

(select-module gauche)

;;;
;;; This module is A WORK IN PROGRESS.   Don't expect this to work.
;;;

;; Bootstrapping "make"
;;   We already have generic-function for "make" defined in C.  Just wanted
;;   to make a method specialized for <class>, i.e. the most common "make".
;;   However, we can't say (make <method> ...) before we have make method.
;;   So we have to "hard wire" the method creation.

(let ((%make (lambda (class . initargs)
               (let ((obj (allocate-instance class initargs)))
                 (initialize obj initargs)
                 obj)))
      (body  (lambda (class initargs next-method)
               (let ((obj (allocate-instance class initargs)))
                 (initialize obj initargs)
                 obj))))
  (add-method! make
               (%make <method>
                      :generic make
                      :specializers (list <class>)
                      :lambda-list '(class . initargs)
                      :body body)))


;;----------------------------------------------------------------
;; Generic function
;;

(define-syntax define-generic
  (syntax-rules ()
    ((_ ?name)
     (define ?name (make <generic> :name '?name)))))

;;----------------------------------------------------------------
;; Method
;;

(define-syntax define-method
  (syntax-rules ()
    ;; main expansion
    ((_ "args" ?name () (?body ...) (?specs ...) ?lambda-list (?body-arg ...))
     (begin
       (define ?name
         (if (symbol-bound? '?name)
             (if (is-a? ?name <generic>)
                 ?name
                 (error "define-method: ~s is not bound to a generic function"
                        '?name))
             (make <generic> :name '?name)))
       (add-method! ?name
                    (make <method>
                      :generic ?name
                      :specializers (list ?specs ...)
                      :lambda-list  '?lambda-list
                      :body (lambda (?body-arg ... next-method)
                              ?body ...)))))
    ;; terminating case
    ((_ "args" ?name ((?arg ?class)) ?body
        (?spec ...) (?llist ...) (?barg ...))
     (define-method "args" ?name () ?body
       (?spec ... ?class) (?llist ... ?arg) (?barg ... ?arg)))
    ((_ "args" ?name (?arg) ?body
        (?spec ...) (?llist ...) (?barg ...))
     (define-method "args" ?name () ?body
       (?spec ... <top>) (?llist ... ?arg) (?barg ... ?arg)))
    ;; normal case
    ((_ "args" ?name ((?arg ?class) . ?args) ?body
        (?spec ...) (?llist ...) (?barg ...))
     (define-method "args" ?name ?args ?body
       (?spec ... ?class) (?llist ... ?arg) (?barg ... ?arg)))
    ((_ "args" ?name (?arg . ?args) ?body
        (?spec ...) (?llist ...) (?barg ...))
     (define-method "args" ?name ?args ?body
       (?spec ... <top>) (?llist ... ?arg) (?barg ... ?arg)))
    ;; &rest arg.  we need special case for single &rest arg.
    ((_ "args" ?name ?arg ?body 
        () () ())
     (define-method "args" ?name () ?body
       () ?arg (?arg)))
    ((_ "args" ?name ?arg ?body 
        (?spec ...) (?llist ...) (?barg ...))
     (define-method "args" ?name () ?body
       (?spec ...) (?llist ... . ?arg) (?barg ... ?arg)))
    ;; entry
    ((_ ?name ?args ?body ...)
     (define-method "args" ?name ?args (?body ...) () () ()))
    ))

;;----------------------------------------------------------------
;; Class
;;

(define-syntax define-class
  (syntax-rules ()
    ((_ ?name (?super ...) (?slot ...) ?options ...)
     (define ?name
       (make <class>
             :supers (list ?super ...)
             :slots (list (%parse-slot-definition ?slot) ...)
             ?options ...
             :name '?name)))))

(define-syntax %parse-slot-definition
  (syntax-rules ()
    ((_ (?name ?options ...))
     (%parse-slot-definition "parse options" ?name (?options ...) ()))
    ((_ "parse options" ?name () (?rest ...))
     (?name ?rest ...))
    ((_ "parse options" ?name (:initform ?form ?options ...) (?rest ...))
     (%parse-slot-definition "parse options"
                             ?name (?options ...)
                             (?rest ... :init-thunk (lambda () ?form))))
    ((_ "parse options" ?name (:init-form ?form ?options ...) (?rest ...))
     (%parse-slot-definition "parse options"
                             ?name (?options ...)
                             (?rest ... :init-thunk (lambda () ?form))))
    ((_ "parse options" ?name (?key ?value ?options ...) (?rest ...))
     (%parse-slot-definition "parse options"
                             ?name (?options ...)
                             (?rest ... ?key ?value)))
    ((_ "parse options" ?name (?stray) (?rest ...))
     (error "bad slot definition (uneven keyword list) for slot ~s" '?name))
    ((_ ?name)
     '?name)
    ))

(define-method initialize ((class <class>) initargs)
  (let ((name   (get-keyword :name   initargs #f))
        (slots  (get-keyword :slots  initargs '()))
        (supers (let ((s (get-keyword :supers initargs '())))
                  (if (null? s) (list <object>) s))))
    (slot-set! class 'name name)
    (slot-set! class 'direct-supers supers)
    (slot-set! class 'cpl (compute-cpl class))
    (slot-set! class 'direct-slots
               (map (lambda (s) (if (pair? s) s (list s))) slots))
    (slot-set! class 'slots (compute-slots class))
    ))

(define-method compute-slots ((class <class>))
  (let ((cpl (slot-ref class 'cpl))
        (slots '()))
    (for-each (lambda (c)
                (for-each (lambda (slot)
                            (unless (assq (car slot) slots)
                              (set! slots (cons slot slots))))
                          (slot-ref c 'direct-slots)))
              cpl)
    (reverse slots)))

;;----------------------------------------------------------------
;; Introspection routines
;;

(define (class-name class) (slot-ref class 'name))
(define (class-precedence-list class) (slot-ref class 'cpl))
(define (class-direct-supers class) (slot-ref class 'direct-supers))
(define (class-direct-slots class) (slot-ref class 'direct-slots))
(define (class-slots class) (slot-ref class 'slots))

;; more to come ...

;;----------------------------------------------------------------
;; Handy for debug (just for now)
;;

(define (%inspect obj)
  (if (is-a? obj <object>)
      (for-each (lambda (slot)
                  (format #t "~s: ~s\n" (car slot) (slot-ref obj (car slot))))
                (class-slots (class-of obj)))
      obj))
