;;;
;;; object.scm - object system
;;;
;;;  Copyright(C) 2000-2002 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: object.scm,v 1.40 2002-12-11 05:56:51 shirok Exp $
;;;

;; This module is not meant to be `use'd.   It is just to hide
;; auxiliary procedures from the rest of the system.  The initialization
;; file loads this file, and this file inserts exported symbols into
;; gauche module explicitly.
(define-module gauche.object )
(select-module gauche.object)

;;; I'm trying to make MOP as close to STklos and Goops as possible.
;;; The perfect compatibility can't be done since the underlying implemenation
;;; in C differs a bit.

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

(define-macro (define-generic name)
  (receive (true-name getter-name) (%check-setter-name name)
    (if getter-name
        `(begin
           (define ,true-name (make <generic> :name ',true-name))
           (set! (setter ,getter-name) ,true-name))
        `(define ,true-name (make <generic> :name ',true-name)))))

;; allow (setter name) type declaration
(define (%check-setter-name name)
  (cond ((symbol? name) (values name #f))
        ((identifier? name) (values name #f))
        ((and (pair? name) (eq? (car name) 'setter)
              (pair? (cdr name)) (or (symbol? (cadr name))
                                     (identifier? (cadr name)))
              (null? (cddr name)))
         (values (%make-setter-name (cadr name)) (cadr name)))
        (else (error "Bad name for generic function or method" name))))

(define (%make-setter-name name)
  (string->symbol (format #f "setter of ~a" name)))

;;----------------------------------------------------------------
;; Method
;;

(define-macro (define-method name specs . body)
  (receive (specializers lambda-list body-args)
      (let loop ((ss specs))
        (cond ((null? ss)
               (values '() '() (list 'next-method)))
              ((not (pair? ss))
               (values '() ss (list ss 'next-method)))
              ((pair? (car ss))
               (receive result (loop (cdr ss))
                 (apply values (map cons
                                    (list (cadar ss) (caar ss) (caar ss))
                                    result))))
              (else
               (receive result (loop (cdr ss))
                 (apply values (map cons
                                    (list '<top> (car ss) (car ss))
                                    result))))
              ))
    (receive (true-name getter-name) (%check-setter-name name)
      (let ((gf (gensym)))
        `(let ((,gf (%ensure-generic-function ',true-name (current-module))))
           (add-method! ,gf
                        (make <method>
                          :generic ,gf
                          :specializers (list ,@specializers)
                          :lambda-list ',lambda-list
                          :body (lambda ,body-args ,@body)))
           ,@(if getter-name
                 `((unless (has-setter? ,getter-name)
                     (set! (setter ,getter-name) ,gf)))
                 '())
           ,gf)))
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
                 (error "bad slot specification:" slot))
                (else
                 (case (car opts)
                   ((:initform :init-form)
                    (loop (cddr opts)
                          (list* `(lambda () ,(cadr opts)) :init-thunk r)))
                   ((:getter :setter :accessor)
                    (loop (cddr opts)
                          (list* `',(cadr opts) (car opts) r)))
                   (else
                    (loop (cddr opts) (list* (cadr opts) (car opts) r))))
                 )))
        `'(,slot)))
  (let ((slot-defs (map transform-slot-definition slots))
        (metaclass (or (get-keyword :metaclass options #f)
                       `(,%get-default-metaclass (list ,@supers))))
        (class     (gensym))
        (slot      (gensym)))
    `(define ,name
       (let ((,class (make ,metaclass
                       :name ',name
                       :supers (list ,@supers)
                       :slots (list ,@slot-defs)
                       ,@options)))
         (for-each (lambda (,slot)
                     (,%make-accessor ,class ,slot (current-module)))
                   (class-slots ,class))
         ,class))
    ))

;; Determine default metaclass, that is a class inheriting all the metaclasses
;; of supers.  The idea is taken from stklos.  The difference is that
;; metaclass calculation is done at runtime in Gauche, while at compile-time
;; in STklos.
(define %get-default-metaclass
  (let ((generated-metas '()))
    (define (find-metaclass metasupers)
      (cond ((assoc metasupers generated-metas)
             => (lambda (got) (cdr got)))
            (else (make-metaclass metasupers))))
    (define (make-metaclass metasupers)
      (let ((meta (make <class>
                    :supers metasupers :name (gensym "metaclass") :slots '())))
        (set! generated-metas (acons metasupers meta generated-metas))
        meta))

    (lambda (supers)
      (if (null? supers)
          <class>
          (let* ((all-metas (map class-of supers))
                 (all-cpls  (apply append
                                   (map (lambda (m)
                                          (cdr (class-precedence-list m)))
                                        all-metas)))
                 (needed '()))
            (for-each
             (lambda (m)
               (when (and (not (memq m all-cpls))
                          (not (memq m needed)))
                 (set! needed (cons m needed))))
             all-metas)
            (if (null? (cdr needed))
                (car needed)
                (find-metaclass (reverse! needed))))))
    ))

;;; Method INITIALIZE (class <class>) initargs
(define-method initialize ((class <class>) initargs)
  (next-method)
  (let ((slots  (get-keyword :slots  initargs '()))
        (supers (append (get-keyword :supers initargs '()) `(,<object>))))
    ;; The order of initialization is somewhat important, since calculation
    ;; of values of some slots depends on the other slots.
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
    (cons name (compute-slot-accessor class slot gns))))

(define (%make-accessor class slot module)
  (let ((%getter   (slot-definition-getter slot))
        (%setter   (slot-definition-setter slot))
        (%accessor (slot-definition-accessor slot))
        (name     (slot-definition-name slot)))
    (when %getter
      (let ((gf (%ensure-generic-function %getter module)))
        (add-method! gf
                     (make <method> :generic gf :specializers `(,class)
                           :lambda-list '(obj)
                           :body (lambda (obj next-method)
                                   (slot-ref obj name))))))
    (when %setter
      (let ((gf (%ensure-generic-function %setter module)))
        (add-method! gf
                     (make <method> :generic gf
                           :specializers `(,class ,<top>)
                           :lambda-list '(obj val)
                           :body (lambda (obj val next-method)
                                   (slot-set! obj name val))))))
    (when %accessor
      (let ((gf  (%ensure-generic-function %accessor module))
            (gfs (%ensure-generic-function (%make-setter-name %accessor)
                                           module)))
        (add-method! gf
                     (make <method> :generic gf :specializers `(,class)
                           :lambda-list '(obj)
                           :body (lambda (obj next-method)
                                   (slot-ref obj name))))
        (add-method! gfs
                     (make <method> :generic gfs
                           :specializers `(,class ,<top>)
                           :lambda-list '(obj val)
                           :body (lambda (obj val next-method)
                                   (slot-set! obj name val))))
        (set! (setter gf) gfs)
        ))
    ))
 
;;; Method COMPUTE-SLOTS (class <class>)
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

;;; Method COMPUTE-GET-N-SET (class <class>) slot
(define-method compute-get-n-set ((class <class>) slot)

  ;; NB: STklos ignores :initform slot option for class slots, but
  ;;     I think it's sometimes useful.
  (define (make-class-slot)
    (let ((cell (undefined))
          (init-value (slot-definition-option slot :init-value (undefined)))
          (init-thunk (slot-definition-option slot :init-thunk #f)))
      (list (lambda (o)
              (when (undefined? cell)
                (cond ((not (undefined? init-value)) (set! cell init-value))
                      ((procedure? init-thunk) (set! cell (init-thunk)))))
              cell)
            (lambda (o v)
              (set! cell v)))))
  
  (let ((slot-name (slot-definition-name slot))
        (alloc (slot-definition-allocation slot)))
    (case alloc
      ((:instance)
       (let ((num (slot-ref class 'num-instance-slots)))
         (slot-set! class 'num-instance-slots (+ num 1))
         num))
      ((:class)
       (if (assq slot-name (class-direct-slots class))
           (make-class-slot)
           (let loop ((cpl (class-precedence-list class)))
             (cond ((null? cpl)
                    (error "something wrong with slot inheritance of" class))
                   ((assq slot-name (class-direct-slots (car cpl)))
                    (class-slot-accessor (car cpl) slot-name))
                   (else (loop (cdr cpl)))))))
      ((:each-subclass)
       (make-class-slot))
      ((:virtual)
       (let ((getter (slot-definition-option slot :slot-ref #f))
             (setter (slot-definition-option slot :slot-set! #f)))
         (unless (and (procedure? getter) (procedure? setter))
           (error "virtual slot requires both :slot-ref and :slot-set!:"
                  slot))
         (list getter setter)))
      ((:builtin)
       (or (slot-definition-option slot :slot-accessor #f)
           (errorf "builtin slot ~s of class ~s doesn't have associated slot accessor"
                   (car slot) class)))
      (else
       (error "unsupported slot allocation:" alloc)))))

;; METHOD COMPUTE-SLOT-ACCESSOR (class <class>) g-n-s
;;  this method doesn't have equivalent one in STklos.
(define-method compute-slot-accessor ((class <class>) slot gns)
  (if (is-a? gns <slot-accessor>)
      gns
      (apply make <slot-accessor>
             :class class :name (slot-definition-name slot)
             `(,@(cond
                  ((integer? gns) (list :slot-number gns :initializable #t))
                  ((list? gns)
                   (list :getter-n-setter (cons (car gns) (list-ref gns 1 #f))
                         :initializable (list-ref gns 2 #f)))
                  (else
                   (errorf "bad getter-and-setter returned by compute-get-n-set for ~s ~s: ~s"
                           class slot gns)))
               ,@(cdr slot)))))

;; access class allocated slot.  API compatible with Goops.
(define (%class-slot-gns class slot-name)
  (cond ((class-slot-definition class slot-name)
         => (lambda (slot)
              (if (memv (slot-definition-allocation slot)
                        '(:class :each-subclass))
                  (slot-ref (class-slot-accessor class slot-name)
                            'getter-n-setter)
                  (errorf "attempt to access non-class allocated slot ~s of class ~s as a class slot." slot-name class))))
        (else
         (errorf "attempt to access non-existent slot ~s of class ~s as a class slot." slot-name class))))

(define (class-slot-set! class slot-name val)
  (apply (cdr (%class-slot-gns class slot-name)) (list #f val)))

(define class-slot-ref
  (getter-with-setter
   (lambda (class slot-name)
     (let ((val (apply (car (%class-slot-gns class slot-name)) '(#f))))
       (if (undefined? val)
           (slot-unbound class slot-name)
           val)))
   class-slot-set!))

;; default class printer
(define-method write-object ((obj <class>) out)
  (format out "#<class ~a>" (class-name obj)))

;; convenient routine to push the value to the slot.
;; this can be optimized later.
(define (slot-push! obj slot value)
  (slot-set! obj slot (cons value (slot-ref obj slot))))

;; default unbound slot and missing slot handlers.
;; we avoid printing object itself in the error message, for it might
;; cause an infinite loop (via write-object method).
(define-method slot-unbound ((class <class>) obj slot)
  (errorf "slot ~s of object of class ~a is unbound" slot class))

(define-method slot-missing ((class <class>) obj slot . value)
  (errorf "object of class ~s doesn't have such slot: ~s" class slot))

(define (slot-exists? obj slot)
  (slot-exists-using-class? (class-of obj) obj slot))

(define-method slot-exists-using-class? (class obj slot)
  (not (not (assq slot (class-slots class)))))

;;----------------------------------------------------------------
;; Class Redifinition
;;

;; Not implemented yet.

;;----------------------------------------------------------------
;; Method Application
;;

;; Like stklos or goops, pure generic is handled completely in C
;; and the following protocol is skipped.
;;
;; apply-generic [GF]
;;   compute-applicable-methods [GF, method defined in C]
;;   sort-applicable-methods [GF]
;;     method-more-specific? [GF, method defined in C]
;;   apply-methods [GF]
;;     apply-method [GF]
;;
;; The protocol mimics STklos, but the underlying application mechanism
;; differs a bit.

(define-method apply-generic ((gf <generic>) args)
  (let ((methods (compute-applicable-methods gf args)))
    (apply-methods gf (sort-applicable-methods gf methods args) args)))

(define-method sort-applicable-methods ((gf <generic>) methods args)
  (let ((types (map class-of args)))
    (sort methods (lambda (x y) (method-more-specific? x y types)))))

(define-method apply-methods ((gf <generic>) methods args)
  (apply-method gf methods %make-next-method args))

(define-method apply-method ((gf <generic>) methods build-next args)
  (apply (build-next gf methods args) args))
      
;;----------------------------------------------------------------
;; Introspection routines
;;

(define (class-name class) (slot-ref class 'name))
(define (class-precedence-list class) (slot-ref class 'cpl))
(define (class-direct-supers class) (slot-ref class 'direct-supers))
(define (class-direct-slots class) (slot-ref class 'direct-slots))
(define (class-slots class) (slot-ref class 'slots))

(define (slot-definition-name slot) (car slot))
(define (slot-definition-options slot) (cdr slot))
(define (slot-definition-option slot key . default)
  (apply get-keyword key (cdr slot) default))
(define (slot-definition-allocation slot)
  (get-keyword :allocation (cdr slot) :instance))
(define (slot-definition-getter slot)
  (get-keyword :getter (cdr slot) #f))
(define (slot-definition-setter slot)
  (get-keyword :setter (cdr slot) #f))
(define (slot-definition-accessor slot)
  (get-keyword :accessor (cdr slot) #f))

(define (class-slot-definition class slot-name)
  (assq slot-name (slot-ref class 'slots)))
(define (class-slot-accessor class slot-name)
  (cond ((assq slot-name (slot-ref class 'accessors)) => cdr)
        (else #f)))

;;----------------------------------------------------------------
;; Generic coercion
;;  (should this be in separate file, e.g. coerce.scm?
;;   autoload may have problem with autoloading generic fn.)

(define-method x->string ((obj <string>)) obj)
(define-method x->string ((obj <number>)) (number->string obj))
(define-method x->string ((obj <symbol>)) (symbol->string obj))
(define-method x->string ((obj <char>)) (string obj))
(define-method x->string ((obj <top>))
  (with-output-to-string (lambda () (display obj))))

(define-method x->integer ((obj <integer>)) obj)
(define-method x->integer ((obj <real>))
  (inexact->exact (round obj)))
(define-method x->integer ((obj <number>)) 0) ;complex numbers to 0
(define-method x->integer ((obj <char>)) (char->integer obj))
(define-method x->integer ((obj <top>))
  (x->integer (x->number obj)))

(define-method x->number  ((obj <number>)) obj)
(define-method x->number  ((obj <string>))
  (or (string->number obj) 0))
(define-method x->number  ((obj <char>)) (char->integer obj))
(define-method x->number  ((obj <top>)) 0)

;; shorthand notation
(define-method ref ((obj <top>) (slot <symbol>))
  (slot-ref obj slot))
(define-method (setter ref) ((obj <top>) (slot <symbol>) value)
  (slot-set! obj slot value))

;;----------------------------------------------------------------
;; Generalized application hooks
;;  (should this be in separate file, e.g. apply.scm?)

(define-method object-apply ((self <regexp>) (s <string>))
  (rxmatch self s))
(define-method object-apply ((self <regmatch>))
  (rxmatch-substring self))
(define-method object-apply ((self <regmatch>) (i <integer>))
  (rxmatch-substring self i))
(define-method object-apply ((self <regmatch>) (s <symbol>))
  (object-apply self s 0))
(define-method object-apply ((self <regmatch>) (s <symbol>) (i <integer>))
  (case s
    ((before) (rxmatch-before self i))
    ((after)  (rxmatch-after self i))
    (else
     (errorf "bad symbol argument to ~s: ~s: must be either 'before or 'after"
             self s))))

;;;
;;; Make exported symbol visible from outside
;;;

(define-syntax insert-symbols
  (syntax-rules ()
    ((_ symbol ...)
     (begin (define-in-module gauche symbol symbol) ...))
    ))

(insert-symbols define-generic define-method define-class
                compute-slots compute-get-n-set compute-slot-accessor
                class-slot-ref class-slot-set!
                slot-push! slot-unbound slot-missing
                slot-exists? slot-exists-using-class?
                apply-generic sort-applicable-methods
                apply-methods apply-method
                class-name class-precedence-list class-direct-supers
                class-direct-slots class-slots
                slot-definition-name slot-definition-options
                slot-definition-option
                slot-definition-allocation slot-definition-getter
                slot-definition-setter slot-definition-accessor
                class-slot-definition class-slot-accessor
                x->string x->integer x->number ref |setter of ref|)

(provide "gauche/object")
