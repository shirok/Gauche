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
;;;  $Id: object.scm,v 1.3 2001-03-25 04:48:05 shiro Exp $
;;;

(select-module gauche)

;; Bootstrapping "make"
;;   We already have generic-function for "make" defined in C.  Just wanted
;;   to make a method specialized for <class>, i.e. the most common "make".
;;   However, we can't say (make <method> ...) before we have a make method.
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
       (%ensure-generic-function '?name (current-module))
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

;; TODO: class redefinition
(define-macro (define-class name supers slots . options)
  (define (transform-slot-definition slot)
    (if (pair? slot)
        (let loop ((opts (cdr slot)) (r '()))
          (cond ((null? opts) `(list ',(car slot) ,@(reverse! r)))
                ((not (and (pair? opts) (pair? (cdr opts))))
                 (error "bad slot specification: ~s" slot))
                (else
                 (case (car opts)
                   ((:initform :init-form)
                    (loop (cddr opts)
                          (list* `(lambda () ,(cadr opts)) :init-thunk r)))
                   (else
                    (loop (cddr opts) (list* (cadr opts) (car opts) r))))
                 )))
        `'(,slot)))
  (let ((slot-defs (map transform-slot-definition slots))
        (class     (get-keyword :metaclass options '<class>)))
    `(define ,name
       (make ,class
             :name ',name
             :supers ,supers
             :slots (list ,@slot-defs)
             ,@options))))

(define-method initialize ((class <class>) initargs)
  (let ((name   (get-keyword :name   initargs #f))
        (slots  (get-keyword :slots  initargs '()))
        (supers (let ((s (get-keyword :supers initargs '())))
                  (if (null? s) (list <object>) s))))
    ;; The order of initialization is somewhat important, since calculation
    ;; of values of some slots depends on the other slots.
    (slot-set! class 'name name)
    (slot-set! class 'direct-supers supers)
    (slot-set! class 'cpl (compute-cpl class))
    (slot-set! class 'direct-slots
               (map (lambda (s) (if (pair? s) s (list s))) slots))
    ;; note: num-instance-slots is set up during compute-get-n-set.
    (let* ((slots (compute-slots class)))
      (slot-set! class 'slots slots)
      (slot-set! class 'accessors
                 (map (lambda (s) (%compute-accessor class s)) slots))
      )
    ))

(define (%compute-accessor class slot)
  (let ((name (car slot))
        (gns  (compute-get-n-set class slot)))
    (if (is-a? gns <slot-accessor>)
        (cons name gns)
        (let* ((unbound "unbound")
               (initval (get-keyword :init-value (cdr slot) unbound))
               (initkey (get-keyword :init-keyword (cdr slot) unbound))
               (inittnk (get-keyword :init-thunk (cdr slot) unbound)))
          (cons name
                (apply make <slot-accessor>
                       `(,@(if (eq? initval unbound)
                               '()
                               (list :init-value initval))
                         ,@(if (eq? initkey unbound)
                               '()
                               (list :init-keyword initkey))
                         ,@(if (eq? inittnk unbound)
                               '()
                               (list :init-thunk inittnk))
                         ,@(cond ((integer? gns)
                                  (list :slot-number gns))
                                 ((pair? gns)
                                  (list :slot-ref (car gns)
                                        :slot-set! (cdr gns)))
                                 (else
                                  (error "bad getter-and-setter returned by compute-get-n-set for ~s: ~s"
                                         class gns)))
                         )))
          )
        ))
  )

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

(define-method compute-get-n-set ((class <class>) slot)
  (let ((alloc (get-keyword :allocation (cdr slot) :instance)))
    (case alloc
      ((:instance)
       (let ((num (slot-ref class 'num-instance-slots)))
         (slot-set! class 'num-instance-slots (+ num 1))
         num))
      ((:builtin)
       (let ((acc (get-keyword :accessor (cdr slot) #f)))
         (unless acc
           (error "builtin slot ~s of class ~s doesn't have associated accessor"
                  (car slot) class))
         acc))
      (else
       (error "unsupported slot allocation: ~s" alloc)))))

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
