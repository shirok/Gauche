;;;
;;; object.scm - object system
;;;  
;;;   Copyright (c) 2000-2007  Shiro Kawai  <shiro@acm.org>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  
;;;  $Id: objlib.scm,v 1.6 2007-03-02 07:39:14 shirok Exp $
;;;

;; This module is not meant to be `use'd.   It is just to hide
;; auxiliary procedures from the rest of the system.  The initialization
;; file loads this file, and this file inserts exported symbols into
;; gauche module explicitly.
(define-module gauche.object)
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

;(define-macro (define-generic name . opts)
;  (%expand-define-generic name opts))

(define (%expand-define-generic name opts)
  (receive (true-name getter-name) (%check-setter-name name)
    (let ((class (get-keyword :class opts <generic>))
          (other (delete-keyword :class opts)))
      (if getter-name
        `(begin
           (define ,true-name (make ,class :name ',true-name ,@other))
           (set! (setter ,getter-name) ,true-name))
        `(define ,true-name (make ,class :name ',true-name ,@other))))))

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

;(define-macro (define-method name specs . body)
;  (%expand-define-macro name specs body))

(define (%expand-define-method name specs body)
  (receive (specializers lambda-list body-args)
      (let loop ((ss specs))
        (cond ((null? ss)
               (values '() '() (list 'next-method)))
              ((not (pair? ss))
               (values '() ss (list ss 'next-method)))
              ((pair? (car ss))
               (receive result (loop (cdr ss))
                 (apply values (map cons
                                    (list (car (cdar ss)) (caar ss) (caar ss))
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

;(define-macro (define-class name supers slots . options)
;  (%expand-define-class name supers slots options))

(define make-identifier (with-module gauche.internal make-identifier))

(define (%expand-define-class name supers slots options)
  (let* ((metaclass (or (get-keyword :metaclass options #f)
                        `(,(make-identifier '%get-default-metaclass
                                            (current-module) '())
                          (list ,@supers))))
         (slot-defs (map %process-slot-definition slots))
         (class     (gensym))
         (slot      (gensym)))
    `(define ,name
       (let ((,class (make ,metaclass
                       :name ',name
                       :supers (list ,@supers)
                       :slots (list ,@slot-defs)
                       :defined-modules (list (current-module))
                       ,@options)))
         (when (%check-class-binding ',name (current-module))
           (redefine-class! ,name ,class))
         (for-each (lambda (,slot)
                     (,(make-identifier '%make-accessor (current-module) '())
                      ,class ,slot (current-module)))
                   (class-slots ,class))
         ,class))
    ))

(define (%process-slot-definition sdef)
  (if (pair? sdef)
    (let loop ((opts (cdr sdef)) (r '()))
      (cond ((null? opts) `(list ',(car sdef) ,@(reverse! r)))
            ((not (and (pair? opts) (pair? (cdr opts))))
             (error "bad slot specification:" sdef))
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
    `'(,sdef)))

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
;;;  NB: we always add <object> to the direct supers, for C defined
;;;  base classes may not be inheriting from it.
(define-method initialize ((class <class>) initargs)
  (next-method)
  (let* ((slots  (get-keyword :slots  initargs '()))
         (sup    (get-keyword :supers initargs '()))
         (supers (append sup (list <object>)))
         )
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
                 (map (lambda (s)
                        ;; returns (name . #<slot-accessor>)
                        (cons (car s)
                              (compute-slot-accessor
                               class s
                               (compute-get-n-set class s))))
                      slots))
      )
    ;; bookkeeping for class redefinition
    (slot-set! class 'initargs initargs)
    (for-each (lambda (super) (%add-direct-subclass! super class))
              supers)
    ))

(define (%make-accessor class slot module)
  (let* ((name      (slot-definition-name slot))
         (sa        (class-slot-accessor class name))
         (%getter   (slot-definition-getter slot))
         (%setter   (slot-definition-setter slot))
         (%accessor (slot-definition-accessor slot)))

    (define (make-getter gf)
      (add-method! gf
                   (make <accessor-method>
                     :generic gf :specializers (list class)
                     :slot-accessor sa :lambda-list '(obj)
                     :body (lambda (obj next-method) #f) ;; dummy
                     )))

    (define (make-setter gf)
      (add-method! gf
                   (make <accessor-method>
                     :generic gf :specializers (list class <top>)
                     :slot-accessor sa :lambda-list '(obj val)
                     :body (lambda (obj val next-method) #f) ;; dummy
                     )))

    (when %getter
      (make-getter (%ensure-generic-function %getter module)))
    (when %setter
      (make-setter (%ensure-generic-function %setter module)))
    (when %accessor
      (let ((gf  (%ensure-generic-function %accessor module))
            (gfs (%ensure-generic-function (%make-setter-name %accessor)
                                           module)))
        (make-getter gf)
        (make-setter gfs)
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
;;;   May return:
;;;      integer for instance slot
;;;      list    (getter [setter [bound? [allocate?]]])
;;;      slot accessor
(define-method compute-get-n-set ((class <class>) slot)

  ;; NB: STklos ignores :initform slot option for class slots, but
  ;;     I think it's sometimes useful.
  (define (make-class-slot)
    (let* ((init-value (slot-definition-option slot :init-value (undefined)))
           (init-thunk (slot-definition-option slot :init-thunk #f)))
      (if init-thunk
        (%make-class-slot (init-thunk))
        (%make-class-slot init-value))))
  
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
             (setter (slot-definition-option slot :slot-set! #f))
             (bound? (slot-definition-option slot :slot-bound? #f)))
         (unless (procedure? getter)
           (error "virtual slot requires at least :slot-ref:" slot))
         (list getter setter bound?)))
      ((:builtin)
       (or (slot-definition-option slot :slot-accessor #f)
           (errorf "builtin slot ~s of class ~s doesn't have associated slot accessor"
                   (car slot) class)))
      (else
       (error "unsupported slot allocation:" alloc)))))

(define (%make-class-slot cell)
  (list (lambda (o)   cell)
        (lambda (o v) (set! cell v))
        (lambda (o)   (not (undefined? cell)))))

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
                   (list :getter (car gns)
                         :setter (list-ref gns 1 #f)
                         :bound? (list-ref gns 2 #f)
                         :initializable (list-ref gns 3 #f)))
                  (else
                   (errorf "bad getter-and-setter returned by compute-get-n-set for ~s ~s: ~s"
                           class slot gns)))
               ,@(cdr slot)))))

;; access class allocated slot.  API compatible with Goops.
(define (%class-slot-gns class slot-name acc-type)
  (cond ((class-slot-definition class slot-name)
         => (lambda (slot)
              (if (memv (slot-definition-allocation slot)
                        '(:class :each-subclass))
                (slot-ref (class-slot-accessor class slot-name) acc-type)
                (errorf "attempt to access non-class allocated slot ~s of class ~s as a class slot." slot-name class))))
        (else
         (errorf "attempt to access non-existent slot ~s of class ~s as a class slot." slot-name class))))

(define (class-slot-set! class slot-name val)
  (apply (%class-slot-gns class slot-name 'setter) (list #f val)))

(define class-slot-ref
  (getter-with-setter
   (lambda (class slot-name)
     (let ((val (apply (%class-slot-gns class slot-name 'getter) '(#f))))
       (if (undefined? val)
           (slot-unbound class slot-name)
           val)))
   class-slot-set!))

(define (class-slot-bound? class slot-name)
  (apply (%class-slot-gns class slot-name 'bound?) '(#f)))

;; default class printer.  Avoid using class-name so that in case
;; when obj's class has been redefined, this wouldn't trigger updating obj.
(define-method write-object ((obj <class>) out)
  (format out "#<class ~a>"
          (slot-ref-using-class (current-class-of obj) obj 'name)))

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
;; Class Redefinition
;;

;; implemented in gauche/redefutil.scm 
;(autoload "gauche/redefutil"
;          redefine-class! class-redefinition
;          update-direct-subclass! change-object-class)
;(with-module gauche
;  (autoload "gauche/redefutil"
;            redefine-class! class-redefinition
;            update-direct-subclass! change-object-class))

;; change-class gf is defined in C, so we can't use autoload for it.
(define-method change-class ((obj <object>) (new-class <class>))
  (change-object-class obj (current-class-of obj) new-class))

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
(define (class-direct-methods class) (slot-ref class 'direct-methods))
(define (class-direct-subclasses class) (slot-ref class 'direct-subclasses))
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

;;----------------------------------------------------------------
;; Generic accessor
;;

(define-method ref ((obj <top>) (slot <symbol>))
  (slot-ref obj slot))
(define-method (setter ref) ((obj <top>) (slot <symbol>) value)
  (slot-set! obj slot value))

(define-method ref ((obj <hash-table>) key)
  (hash-table-get obj key))
(define-method ref ((obj <hash-table>) key fallback)
  (hash-table-get obj key fallback))
(define-method (setter ref) ((obj <hash-table>) key value)
  (hash-table-put! obj key value))

;; gauche.sequence has the generic version for <sequence>, but these
;; shortcuts would be faster.
(define-method ref ((obj <list>) (index <integer>))
  (list-ref obj index))
(define-method ref ((obj <vector>) (index <integer>))
  (vector-ref obj index))
(define-method ref ((obj <string>) (index <integer>))
  (string-ref obj index))
(define-method (setter ref) ((obj <vector>) (index <integer>) val)
  (vector-set! obj index val))
(define-method (setter ref) ((obj <string>) (index <integer>) val)
  (string-set! obj index val))

;; ref* - chain of refs.
;;                                        
;; This allows us to write
;;   (ref (ref (ref foo 'bar) 'baz) 'bang)
;; as
;;   (ref* foo 'bar 'baz 'bang)
;; Credit to Issac Trotts.

;; Once we have a compiler-macro, or optimized case-lambda, we
;; can make them more efficient by expanding ref* to refs in
;; fixed-argument case.
(define (ref* x y . more)
  (if (null? more)
    (ref x y)
    (apply ref* (ref x y) more)))

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
  (case s
    ((before after) (object-apply self s 0))
    (else (rxmatch-substring self s))))
(define-method object-apply ((self <regmatch>) (s <symbol>) group)
  (case s
    ((before) (rxmatch-before self group))
    ((after)  (rxmatch-after self group))
    (else
     (errorf "bad symbol argument to ~s: ~s: must be either 'before or 'after"
             self s))))

;; A trick to let a condition type behave its own predicate
(define-method object-apply ((type <condition-meta>) obj)
  (condition-has-type? obj type))

;;;
;;; Make exported symbol visible from outside
;;;

(define-macro (insert-symbols . syms)
  `(begin ,@(map (lambda (s)
                   `(define-in-module gauche ,s ,s))
                 syms)
          ))

(insert-symbols ;define-generic define-method define-class
                compute-slots compute-get-n-set compute-slot-accessor
                class-slot-ref class-slot-set! class-slot-bound?
                slot-push! slot-unbound slot-missing
                slot-exists? slot-exists-using-class?
                change-class
                apply-generic sort-applicable-methods
                apply-methods apply-method
                class-name class-precedence-list class-direct-supers
                class-direct-methods class-direct-subclasses
                class-direct-slots class-slots
                slot-definition-name slot-definition-options
                slot-definition-option
                slot-definition-allocation slot-definition-getter
                slot-definition-setter slot-definition-accessor
                class-slot-definition class-slot-accessor
                x->string x->integer x->number ref ref* |setter of ref|)

(provide "gauche/object")
