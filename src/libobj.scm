;;;
;;; object.scm - object system
;;;
;;;   Copyright (c) 2000-2018  Shiro Kawai  <shiro@acm.org>
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

(declare) ;; a dummy form to suppress generation of "sci" file

;; preparing inline stub code
(inline-stub
 (declcode (.include <gauche/class.h>
                     <gauche/priv/classP.h>
                     <gauche/vminsn.h>))
 (define-type <slot-accessor> "ScmSlotAccessor*")
 (define-type <generic> "ScmGeneric*")
 (define-type <method> "ScmMethod*")
 )

;; This module is not meant to be `use'd.   It is just to hide
;; auxiliary procedures from the rest of the system.  The necessary
;; bindings are injected into 'gauche' module at the initialization time.
(define-module gauche.object)
(select-module gauche.object)

;; TRANSIENT: If GAUCHE_KEYWORD_IS_SYMBOL, we need to rename keywords as well
;; to preserve hygiene.  The symbol? check allows us to pass a keyword to
;; %id, regardless of GAUCHE_KEYWORD_IS_SYMBOL setting.
(define (%id name)
  (if (symbol? name)
    ((with-module gauche.internal make-identifier)
     name (find-module 'gauche.object) '())
    name))

;;; I'm trying to make MOP as close to STklos and Goops as possible.
;;; The perfect compatibility can't be done since the underlying implemenation
;;; in C differs a bit.

;; Bootstrapping "make"
;;   We already have generic-function for "make" defined in C.  Just wanted
;;   to make a method specialized for <class>, i.e. the most common "make".
;;   However, we can't say (make <method> ...) before we have a make method.
;;   So we have to "hard wire" the method creation.

(let ([%make (^[class . initargs]
               (rlet1 obj (allocate-instance class initargs)
                 (initialize obj initargs)))]
      [body  (^[class initargs next-method]
               (rlet1 obj (allocate-instance class initargs)
                 (initialize obj initargs)))])
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
    (let ([class (get-keyword :class opts <generic>)]
          [other (delete-keyword :class opts)])
      (if getter-name
        (quasirename %id
          (define ,true-name
            (rlet1 ,true-name (make ,class ':name ',true-name ,@other)
              (set! (setter ,getter-name) ,true-name))))
        (quasirename %id
          (define ,true-name
            (make ,class ':name ',true-name ,@other)))))))

;; allow (setter name) type declaration
(define (%check-setter-name name)
  (cond [(symbol? name) (values name #f)]
        [(identifier? name) (values name #f)]
        [(and (pair? name) (eq? (car name) 'setter)
              (pair? (cdr name)) (or (symbol? (cadr name))
                                     (identifier? (cadr name)))
              (null? (cddr name)))
         (values (%make-setter-name (cadr name)) (cadr name))]
        [else (error "Bad name for generic function or method" name)]))

(define (%make-setter-name name)
  (string->symbol (format #f "setter of ~a" name)))

;;----------------------------------------------------------------
;; Method
;;

;(define-macro (define-method name [quals ...] specs . body)
;  (%expand-define-macro name quals specs body))

(define (%expand-define-method name quals specs body)
  ;; check qualifiers.  we only support :locked for now
  ;; TRANSIENT: Must use hygienic compare.
  (if-let1 bad (find (^q (not (memq q '(:locked)))) quals)
    (error "syntax-error: unsupported method qualifier:" bad))

  ;; classify arguments to required, rest, and optionals
  ;;  ((a <x>) b (c <y>))   => r:((a <x>) b (c <y>)) r:#f o:#f
  ;;  ((a <x>) b . c)       => r:((a <x>) b) r:c o:#f
  ;;  ((a <x>) b :optional x :key y)
  ;;                        => r:((a <x>) b) r:#:G01 o:(:optional x :key y))
  (receive (reqs rest opts)
      (let loop ([ss specs] [rs '()])
        (cond [(null? ss)          (values (reverse rs) #f #f)]
              [(not (pair? ss))    (values (reverse rs) ss #f)]
              [(keyword? (car ss)) (values (reverse rs) (gensym) ss)]
              [else (loop (cdr ss) (cons (car ss) rs))]))
    (let* ([specializers (map (^s (if (pair? s) (cadr s) (%id'<top>))) reqs)]
           [reqargs      (map (^s (if (pair? s) (car s) s)) reqs)]
           [lambda-list  (if rest `(,@reqargs . ,rest) reqargs)]
           [real-args    (if rest
                           `(,@reqargs ,rest next-method)
                           `(,@reqargs next-method))]
           [real-body (if opts
                        (quasirename %id
                          (lambda ,real-args
                            (apply (lambda ,opts ,@body) ,rest)))
                        (quasirename %id
                          (lambda ,real-args ,@body)))])
      (receive (true-name getter-name) (%check-setter-name name)
        (let1 gf (gensym)
          (quasirename %id
            (rlet1 ,gf (%ensure-generic-function ',true-name (current-module))
              (add-method! ,gf (make <method>
                                 ':generic ,gf
                                 ':specializers (list ,@specializers)
                                 ':lambda-list ',lambda-list
                                 ':method-locked (boolean (memq ':locked ',quals))
                                 ':body ,real-body))
              ,@(cond-list [getter-name
                            (quasirename %id
                              (unless (has-setter? ,getter-name)
                                (set! (setter ,getter-name) ,gf)))]))))))))

(inline-stub
 ;; internal for %ensure-generic-function
 (define-cfn call-fallback-proc (args::ScmObj* nargs::int gf::ScmGeneric*)
  :static
  (return (Scm_VMApply (SCM_OBJ (-> gf data)) (Scm_ArrayToList args nargs))))

 ;; Ensure a named generic function is defined within the given module.
 ;; This is called from method definition (either explicit definition by
 ;; define-method, or implicit definition by slot's :getter/:setter option).
 ;;
 ;; If NAME has a visible binding to a subr or a closure, we use it as
 ;; a fallback method.  NB: this feature is debatable, since it may breach
 ;; the isolation of namespaces.  We may drop this feature in future.
 ;;
 ;; After the call to this procedure, a binding of NAME is inserted in
 ;; the current module.
 (define-cproc %ensure-generic-function (name::<symbol> module::<module>)
   (let* ([val (Scm_GlobalVariableRef module name 0)])
     (cond
      [(Scm_TypeP val SCM_CLASS_GENERIC)
       (unless (Scm_GlobalVariableRef module name SCM_BINDING_STAY_IN_MODULE)
         (Scm_Define module name val))]
      [else
       (if (or (SCM_SUBRP val) (SCM_CLOSUREP val))
         (set! val (Scm_MakeBaseGeneric (SCM_OBJ name) call_fallback_proc val))
         (set! val (Scm_MakeBaseGeneric (SCM_OBJ name) NULL NULL)))
       (Scm_Define module name val)])   ;redefine
     (return val)))

 (define-cproc %make-next-method (gf methods::<list> args::<list>)
   (let* ([argv::ScmObj*] [argc::int])
     (unless (Scm_TypeP gf SCM_CLASS_GENERIC)
       (Scm_Error "generic function requied, but got %S" gf))
     (dolist [mp methods]
       (unless (Scm_TypeP mp SCM_CLASS_METHOD)
         (Scm_Error "method required, but got %S" mp)))
     (set! argv (Scm_ListToArray args (& argc) NULL TRUE))
     (return (Scm_MakeNextMethod (SCM_GENERIC gf) methods argv argc
                                 FALSE FALSE))))
 )

;;----------------------------------------------------------------
;; Class
;;

;; TRANSIENT: We should employ er-macro-transformer to expand
;; define-class etc., but this file need to be compiled by 0.9.5
;; and we've improved the transformer since then.  So we postponed
;; rewriting these after 0.9.6 release.  Meanwhile, we manually
;; replacing identifiers.

;(define-macro (define-class name supers slots . options)
;  (%expand-define-class name supers slots options))

(define (%expand-define-class name supers slots options)
  (let* ([metaclass (or (get-keyword :metaclass options #f)
                        (quasirename %id
                          (%get-default-metaclass (list ,@supers))))]
         [slot-defs (map %process-slot-definition slots)]
         [class     (gensym)]
         [slot      (gensym)])
    (quasirename %id
      (define ,name
        (rlet1 ,class (make ,metaclass
                        ':name ',name ':supers (list ,@supers)
                        ':slots (list ,@slot-defs)
                        ':defined-modules (list (current-module))
                        ,@options)
          (when (%check-class-binding ',name (current-module))
            (redefine-class! ,name ,class))
          (for-each (lambda (,slot)
                      (%make-accessor ,class ,slot (current-module)))
                    (class-slots ,class)))))))

(define (%process-slot-definition sdef)
  (if (pair? sdef)
    (let loop ([opts (cdr sdef)] [r '()])
      (cond [(null? opts) (quasirename %id (list ',(car sdef) ,@(reverse! r)))]
            [(not (and (pair? opts) (pair? (cdr opts))))
             (error "bad slot specification:" sdef)]
            [else
             ;; TRANSIENT: These comparison must be done hygienically, once
             ;; we replace expander with er transformer.
             (case (car opts)
               [(:initform :init-form)
                (loop (cddr opts)
                      (quasirename %id
                        ((lambda () ,(cadr opts)) ':init-thunk ,@r)))]
               [(:getter :setter :accessor)
                (loop (cddr opts)
                      (quasirename %id (',(cadr opts) ',(car opts) ,@r)))]
               [else
                (loop (cddr opts)
                      (quasirename %id (,(cadr opts) ',(car opts) ,@r)))])]))
    (quasirename %id '(,sdef))))

;; Determine default metaclass, that is a class inheriting all the metaclasses
;; of supers.  The idea is taken from stklos.  The difference is that
;; metaclass calculation is done at runtime in Gauche, while at compile-time
;; in STklos.
(define %get-default-metaclass
  (let1 generated-metas '()
    (define (find-metaclass metasupers)
      (cond [(assoc metasupers generated-metas) => cdr]
            [else (make-metaclass metasupers)]))
    (define (make-metaclass metasupers)
      (rlet1 meta (make <class>
                   :supers metasupers :name (gensym "metaclass") :slots '())
        (set! generated-metas (acons metasupers meta generated-metas))))

    (^[supers]
      (if (null? supers)
        <class>
        (let* ([all-metas (map class-of supers)]
               [all-cpls  (apply append
                                 (map (^m (cdr (class-precedence-list m)))
                                      all-metas))]
               [needed '()])
          (dolist [m all-metas]
            (when (and (not (memq m all-cpls))
                       (not (memq m needed)))
              (set! needed (cons m needed))))
          (if (null? (cdr needed))
            (car needed)
            (find-metaclass (reverse! needed))))))
    ))

;;;
;;; Method INITIALIZE (class <class>) initargs
;;;

(define-method initialize :locked ((class <class>) initargs)
  (next-method)
  (let* ([slots  (get-keyword :slots  initargs '())]
         [sup    (get-keyword :supers initargs '())]
         ;;  NB: we always add <object> to the direct supers, for C defined
         ;;  base classes may not be inheriting from it.
         [supers (append sup (list <object>))])
    ;; The order of initialization is somewhat important, since calculation
    ;; of values of some slots depends on the other slots.
    (slot-set! class 'direct-supers supers)
    (slot-set! class 'cpl (compute-cpl class))
    (slot-set! class 'direct-slots
               (map (^s (if (pair? s) s (list s))) slots))
    ;; note: num-instance-slots is set up during compute-get-n-set.
    (let1 slots (compute-slots class)
      (slot-set! class 'slots slots)
      (slot-set! class 'accessors
                 (map (^s ;; returns (name . #<slot-accessor>)
                       (cons (car s)
                             (compute-slot-accessor
                              class s
                              (compute-get-n-set class s))))
                      slots))
      )
    ;; bookkeeping for class redefinition
    (slot-set! class 'initargs initargs)
    (dolist [super supers] (%add-direct-subclass! super class))
    (class-post-initialize class initargs)
    ;; seal the class
    (%finish-class-initialization! class)
    ))

;; This is a hook to tweak critical slots of class at initialization time.
;; The core slots becomes immutable once class initialization is done,
;; so any tweaks that requires to modify them needs to be implemented here.
(define-method class-post-initialize ((class <class>) initargs) #f)

(define (%make-accessor class slot module)
  (let* ([name      (slot-definition-name slot)]
         [sa        (class-slot-accessor class name)]
         [%getter   (slot-definition-getter slot)]
         [%setter   (slot-definition-setter slot)]
         [%accessor (slot-definition-accessor slot)])

    (define (make-getter gf)
      (add-method! gf
                   (make <accessor-method>
                     :generic gf :specializers (list class)
                     :slot-accessor sa :lambda-list '(obj)
                     :body (^[obj next-method] #f) ;; dummy
                     )))

    (define (make-setter gf)
      (add-method! gf
                   (make <accessor-method>
                     :generic gf :specializers (list class <top>)
                     :slot-accessor sa :lambda-list '(obj val)
                     :body (^[obj val next-method] #f) ;; dummy
                     )))

    (when %getter
      (make-getter (%ensure-generic-function %getter module)))
    (when %setter
      (make-setter (%ensure-generic-function %setter module)))
    (when %accessor
      (let ([gf  (%ensure-generic-function %accessor module)]
            [gfs (%ensure-generic-function (%make-setter-name %accessor)
                                           module)])
        (make-getter gf)
        (make-setter gfs)
        (set! (setter gf) gfs)
        ))
    ))

;;; Method COMPUTE-SLOTS (class <class>)
(define-method compute-slots :locked ((class <class>))
  (let ([cpl (slot-ref class 'cpl)]
        [slots '()])
    (dolist [c cpl]
      (dolist [slot (slot-ref c 'direct-slots)]
        (unless (assq (car slot) slots)
          (set! slots (cons slot slots)))))
    (reverse slots)))

;;; Method COMPUTE-GET-N-SET (class <class>) slot
;;;   May return:
;;;      integer for instance slot
;;;      list    (getter [setter [bound? [allocate?]]])
;;;      slot accessor
(define-method compute-get-n-set :locked ((class <class>) slot)

  ;; NB: STklos ignores :initform slot option for class slots, but
  ;;     I think it's sometimes useful.
  (define (make-class-slot)
    (let* ([init-value (slot-definition-option slot :init-value (undefined))]
           [init-thunk (slot-definition-option slot :init-thunk #f)])
      (if init-thunk
        (%make-class-slot (init-thunk))
        (%make-class-slot init-value))))

  (let ([slot-name (slot-definition-name slot)]
        [alloc (slot-definition-allocation slot)])
    (case alloc
      [(:instance)
       (rlet1 num (slot-ref class 'num-instance-slots)
         (slot-set! class 'num-instance-slots (+ num 1)))]
      [(:class)
       (if (assq slot-name (class-direct-slots class))
         (make-class-slot)
         (let loop ((cpl (class-precedence-list class)))
           (cond [(null? cpl)
                  (error "something wrong with slot inheritance of" class)]
                 [(assq slot-name (class-direct-slots (car cpl)))
                  (class-slot-accessor (car cpl) slot-name)]
                 [else (loop (cdr cpl))])))]
      [(:each-subclass) (make-class-slot)]
      [(:virtual)
       (let ([getter (slot-definition-option slot :slot-ref #f)]
             [setter (slot-definition-option slot :slot-set! #f)]
             [bound? (slot-definition-option slot :slot-bound? #f)])
         (unless (procedure? getter)
           (error "virtual slot requires at least :slot-ref:" slot))
         (list getter setter bound?))]
      [(:builtin)
       (or (slot-definition-option slot :slot-accessor #f)
           (errorf "builtin slot ~s of class ~s doesn't have associated slot accessor"
                   (car slot) class))]
      [else (error "unsupported slot allocation:" alloc)])))

(define (%make-class-slot cell)
  (list (^[o]   cell)
        (^[o v] (set! cell v))
        (^[o]   (not (undefined? cell)))))

;; METHOD COMPUTE-SLOT-ACCESSOR (class <class>) g-n-s
;;  this method doesn't have equivalent one in STklos.
(define-method compute-slot-accessor :locked ((class <class>) slot gns)
  (if (is-a? gns <slot-accessor>)
    gns
    (apply make <slot-accessor>
           :class class :name (slot-definition-name slot)
           `(,@(cond
                [(integer? gns) (list :slot-number gns :initializable #t)]
                [(list? gns)
                 (list :getter (car gns)
                       :setter (list-ref gns 1 #f)
                       :bound? (list-ref gns 2 #f)
                       :initializable (list-ref gns 3 #f))]
                [else
                 (errorf "bad getter-and-setter returned by compute-get-n-set for ~s ~s: ~s"
                         class slot gns)])
             ,@(cdr slot)))))

;; access class allocated slot.  API compatible with Goops.
(define (%class-slot-gns class slot-name acc-type)
  (cond [(class-slot-definition class slot-name)
         => (^[slot]
              (if (memv (slot-definition-allocation slot)
                        '(:class :each-subclass))
                (slot-ref (class-slot-accessor class slot-name) acc-type)
                (errorf "attempt to access non-class allocated slot ~s of class ~s as a class slot." slot-name class)))]
        [else
         (errorf "attempt to access non-existent slot ~s of class ~s as a class slot." slot-name class)]))

(define (class-slot-set! class slot-name val)
  (apply (%class-slot-gns class slot-name 'setter) (list #f val)))

(define class-slot-ref
  (getter-with-setter
   (^[class slot-name]
     (let1 val (apply (%class-slot-gns class slot-name 'getter) '(#f))
       (if (undefined? val) (slot-unbound class slot-name) val)))
   class-slot-set!))

(define (class-slot-bound? class slot-name)
  (apply (%class-slot-gns class slot-name 'bound?) '(#f)))

;; default class printer.  Avoid using class-name so that in case
;; when obj's class has been redefined, this wouldn't trigger updating obj.
(define-method write-object ((obj <class>) out)
  (format out "#<class ~a>"
          (slot-ref-using-class (current-class-of obj) obj 'name)))

;; convenient routine to push/pop a value to the slot.
;; this can be optimized later.
(define (slot-push! obj slot value)
  (slot-set! obj slot (cons value (slot-ref obj slot))))
(define (slot-pop! obj slot . default)
  (if (and (not (null? default))
           (or (not (slot-bound? obj slot))
               (not (pair? (slot-ref obj slot)))))
    (car default)
    (let1 r (slot-ref obj slot)
      (slot-set! obj slot (cdr r))
      (car r))))

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

(define-cproc %check-class-binding (name module::<module>) Scm_CheckClassBinding)
(define-cproc class-of (obj) Scm_VMClassOf)

;; current-class-of doesn't updates OBJ, and returns possibly the old class
;; which has been redefined.  Should only be used in class redefinition
;; routines.
(define-cproc current-class-of (obj) (return (SCM_OBJ (Scm_ClassOf obj))))
(define-cproc is-a? (obj klass::<class>) (inliner IS-A) Scm_VMIsA)
(define-cproc subtype? (c1::<class> c2::<class>) ::<boolean> Scm_SubtypeP)

(define-cproc slot-ref (obj slot)
  (inliner SLOT-REF) (setter slot-set!)
  (return (Scm_VMSlotRef obj slot FALSE)))

(define-cproc slot-set! (obj slot value) (inliner SLOT-SET) Scm_VMSlotSet)

(define-cproc slot-bound? (obj slot) Scm_VMSlotBoundP)

(define-cproc slot-ref-using-accessor (obj accessor::<slot-accessor>)
  (return (Scm_VMSlotRefUsingAccessor obj accessor FALSE)))

(define-cproc slot-bound-using-accessor? (obj accessor::<slot-accessor>)
  (return (Scm_VMSlotRefUsingAccessor obj accessor TRUE)))

(define-cproc slot-set-using-accessor! (obj accessor::<slot-accessor> value)
  Scm_VMSlotSetUsingAccessor)

(define-cproc slot-initialize-using-accessor! (obj accessor::<slot-accessor>
                                                   initargs)
  Scm_VMSlotInitializeUsingAccessor)

;; Internal API - undocumented
(define-cproc instance-slot-ref (obj num::<fixnum> :optional fallback)
  (let* ([v (Scm_InstanceSlotRef obj num)])
    (if (SCM_UNBOUNDP v)
      (if (SCM_UNBOUNDP fallback)
        (Scm_Error "Slot #%d of object %S is unbound." num obj)
        (return fallback))
      (return v))))
;; Internal API - undocumented
(define-cproc instance-slot-set! (obj num::<fixnum> value) ::<void>
  Scm_InstanceSlotSet)

(define-cproc %finish-class-initialization! (klass::<class>) ::<void>
  (Scm_ClassMalleableSet klass FALSE))

;;
;; Record related builtins
;;
(define-cproc %make-record (klass::<class>
                            :optarray (inits numinits 10)
                            :rest rinits)
  (let* ([obj (Scm__AllocateAndInitializeInstance klass inits numinits 0)])
    (when (== numinits 10)
      (let* ([i::int 10])
        (dolist [init rinits] (Scm_InstanceSlotSet obj (post++ i) init))))
    (return obj)))

(define-cproc %make-recordv (klass::<class> argv::<vector>)
  (let* ([v::ScmObj* (SCM_VECTOR_ELEMENTS argv)]
         [n::int     (SCM_VECTOR_SIZE argv)])
    (return (Scm__AllocateAndInitializeInstance klass v n 0))))

(define-cproc %record-ref (klass::<class> obj k::<fixnum>)
  (unless (SCM_ISA obj klass)
    (Scm_Error "record-ref: instance of %S expected, got %S" klass obj))
  (return (Scm_InstanceSlotRef obj k)))

(define-cproc %record-set! (klass::<class> obj k::<fixnum> val) ::<void>
  (unless (SCM_ISA obj klass)
    (Scm_Error "record-set!: instance of %S expected, got %S" klass obj))
  (Scm_InstanceSlotSet obj k val))

(define-cproc touch-instance! (obj) Scm_VMTouchInstance)

;;----------------------------------------------------------------
;; Class Redefinition
;;

;; implemented in gauche/redefutil.scm
;(autoload "gauche/redefutil"
;          redefine-class! class-redefinition
;          update-direct-subclass! change-object-class)

;; change-class gf is defined in C, so we can't use autoload for it.
(define-method change-class :locked ((obj <object>) (new-class <class>))
  (change-object-class obj (current-class-of obj) new-class))

;; C bindings used by class redefinition routine.
(define-cproc %start-class-redefinition! (k::<class>) ::<void>
  Scm_StartClassRedefinition)
(define-cproc %commit-class-redefinition! (k::<class> newk) ::<void>
  Scm_CommitClassRedefinition)
(define-cproc %replace-class-binding! (k::<class> newk::<class>) ::<void>
  Scm_ReplaceClassBinding)
(define-cproc %add-direct-subclass! (super::<class> sub::<class>) ::<void>
  Scm_AddDirectSubclass)
(define-cproc %delete-direct-subclass! (super::<class> sub::<class>) ::<void>
  Scm_DeleteDirectSubclass)
(define-cproc %add-direct-method! (super::<class> m::<method>) ::<void>
  Scm_AddDirectMethod)
(define-cproc %delete-direct-method! (super::<class> m::<method>) ::<void>
  Scm_DeleteDirectMethod)
(define-cproc %transplant-instance! (src dst) ::<void>
  Scm_TransplantInstance)

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

(define-method apply-generic :locked ((gf <generic>) args)
  (let1 methods (compute-applicable-methods gf args)
    (apply-methods gf (sort-applicable-methods gf methods args) args)))

(define-method sort-applicable-methods :locked ((gf <generic>) methods args)
  (let1 types (map class-of args)
    (sort methods (^[x y] (method-more-specific? x y types)))))

(define-method apply-methods :locked ((gf <generic>) methods args)
  (apply-method gf methods %make-next-method args))

(define-method apply-method :locked ((gf <generic>) methods build-next args)
  (apply (build-next gf methods args) args))

;; internal, but useful to expose
(define-cproc method-applicable-for-classes? (m::<method> :rest classes)
  ::<boolean>
  (let* ([argc::int (Scm_Length classes)]
         [cp::ScmClass** (SCM_NEW_ARRAY (ScmClass*) argc)]
         [n::int 0])
    (for-each (lambda (c)
                (unless (SCM_CLASSP c)
                  (Scm_Error "class required, but got %S" c))
                (set! (aref cp n) (SCM_CLASS c))
                (post++ n))
              classes)
    (return (Scm_MethodApplicableForClasses m cp argc))))

;; Manually trigger dispatch table construction
;; (This is for development - eventually we set this triggered automatically)
(define-cproc generic-build-dispatcher! (gf::<generic> axis::<fixnum>)
  Scm__GenericBuildDispatcher)

(define-cproc generic-dispatcher-vector (gf::<generic>)
  (return (-> gf dispatcher)))

(define-cproc generic-invalidate-dispatcher! (gf::<generic>) ::<void>
  Scm__GenericInvalidateDispatcher)

(define-cproc %generic-dispatcher-dump (gf::<generic>
                                        :optional (port::<port>
                                                   (current-output-port)))
  ::<void> Scm__GenericDispatcherDump)

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
  (cond [(assq slot-name (slot-ref class 'accessors)) => cdr]
        [else #f]))

;;----------------------------------------------------------------
;; Generic coercion
;;  (should this be in separate file, e.g. coerce.scm?
;;   autoload may have problem with autoloading generic fn.)

(define-method x->string :locked ((obj <string>)) obj)
(define-method x->string :locked ((obj <number>)) (number->string obj))
(define-method x->string :locked ((obj <symbol>)) (symbol->string obj))
(define-method x->string :locked ((obj <char>))   (string obj))
(define-method x->string :locked ((obj <top>))    (write-to-string obj display))

(define-method x->integer :locked ((obj <integer>)) obj)
(define-method x->integer :locked ((obj <real>))    (round->exact obj))
(define-method x->integer :locked ((obj <number>))  (round->exact (real-part obj)))
(define-method x->integer :locked ((obj <char>))    (char->integer obj))
(define-method x->integer :locked ((obj <top>))     (x->integer (x->number obj)))

(define-method x->number  :locked ((obj <number>)) obj)
(define-method x->number  :locked ((obj <string>)) (or (string->number obj) 0))
(define-method x->number  :locked ((obj <char>))   (char->integer obj))
(define-method x->number  :locked ((obj <top>))    0)

;;----------------------------------------------------------------
;; Generic accessor
;;

(define-method ref :locked ((obj <top>) (slot <symbol>))
  (slot-ref obj slot))
(define-method ref :locked ((obj <top>) (slot <symbol>) fallback)
  (if (slot-bound? obj slot)
    (slot-ref obj slot)
    fallback))
(define-method (setter ref) :locked ((obj <top>) (slot <symbol>) value)
  (slot-set! obj slot value))

(define-method ref :locked ((obj <hash-table>) key)
  (hash-table-get obj key))
(define-method ref :locked ((obj <hash-table>) key fallback)
  (hash-table-get obj key fallback))
(define-method (setter ref) :locked ((obj <hash-table>) key value)
  (hash-table-put! obj key value))

(define-method ref :locked ((obj <tree-map>) key)
  (tree-map-get obj key))
(define-method ref :locked ((obj <tree-map>) key fallback)
  (tree-map-get obj key fallback))
(define-method (setter ref) :locked ((obj <tree-map>) key value)
  (tree-map-put! obj key value))

;; gauche.sequence has the generic version for <sequence>, but these
;; shortcuts would be faster.
(define-method ref :locked ((obj <list>) (index <integer>))
  (list-ref obj index))
(define-method ref :locked ((obj <vector>) (index <integer>))
  (vector-ref obj index))
(define-method ref :locked ((obj <string>) (index <integer>))
  (string-ref obj index))
(define-method (setter ref) :locked ((obj <list>) (index <integer>) val)
  (set-car! (list-tail obj index) val))
(define-method (setter ref) :locked ((obj <vector>) (index <integer>) val)
  (vector-set! obj index val))
(define-method (setter ref) :locked ((obj <string>) (index <integer>) val)
  (string-set! obj index val))

;; Universal accessor
(define ~
  (getter-with-setter
   (case-lambda
     [(obj selector) (ref obj selector)]
     [(obj selector . more) (apply ~ (ref obj selector) more)])
   (case-lambda
     [(obj selector val) ((setter ref) obj selector val)]
     [(obj selector selector2 . rest)
      (apply (setter ~) (ref obj selector) selector2 rest)])))

(define ref* ~)                         ;for the backward compatibility

;;----------------------------------------------------------------
;; Generalized application hooks
;;  (should this be in separate file, e.g. apply.scm?)

(define-method object-apply :locked ((self <regexp>) (s <string>))
  (rxmatch self s))
(define-method object-apply :locked ((self <regmatch>))
  (rxmatch-substring self))
(define-method object-apply :locked ((self <regmatch>) (i <integer>))
  (rxmatch-substring self i))
(define-method object-apply :locked ((self <regmatch>) (s <symbol>))
  (case s
    [(before after) (object-apply self s 0)]
    [else (rxmatch-substring self s)]))
(define-method object-apply :locked ((self <regmatch>) (s <symbol>) group)
  (case s
    [(before) (rxmatch-before self group)]
    [(after)  (rxmatch-after self group)]
    [else
     (errorf "bad symbol argument to ~s: ~s: must be either 'before or 'after"
             self s)]))

;; Char-set membership
(define-method object-apply :locked ((self <char-set>) (c <char>))
  (char-set-contains? self c))

;; A trick to let a condition type behave its own predicate
(define-method object-apply :locked ((type <condition-meta>) obj)
  (condition-has-type? obj type))

;; Regexp printer.  It doesn't need speed, and it's easier to write in Scheme.
;; This is defined here since we can only use it after define-method support.
;; NB: case-folding regexp is expressed by (?i:...) in regexp->string.
(define-method write-object ((obj <regexp>) out)
  (with-input-from-string (regexp->string obj)
    (^[]
      (display "#/" out)
      (let loop ([c (read-char)])
        (unless (eof-object? c)
          (cond [(char=? #\/ c) (display #\\ out) (display c out)]
                [(memq (char-general-category c) '(Mn Mc Me Cc Cf Cs Co Cn))
                 (let1 code (char->ucs c)
                   (if (< code #x10000)  
                     (format out "\\u~4,'0x" code)
                     (format out "\\U~8,'0x" code)))]
                [else (display c out)])
          (loop (read-char))))
      (display "/" out))))

(define-method write-object ((obj <write-controls>) port)
  (format port "#<write-controls ~s>"
          `(:length ,(~ obj'length)
            :level  ,(~ obj'level)
            :base   ,(~ obj'base)
            :radix  ,(~ obj'radix)
            :pretty ,(~ obj'pretty)
            :width  ,(~ obj'width))))

;;----------------------------------------------------------------
;; Describe
;;

;; This feature used to be provided in gauche.interactive.  However,
;; it is better that libraries can customize describe on their data
;; structures without loading gauche.interactive, so we provide the core
;; methods here.  Other specialized methods on built-in objects are
;; still defined in gauche.interactive.

(define (describe-common obj)
  (format #t "~s is an instance of class ~a\n" obj (class-name (class-of obj))))

(define-method describe-slots (obj)
  (let* ([class (class-of obj)]
         [slots (class-slots class)])
    (unless (null? slots)
      (format #t "slots:\n")
      (dolist [s (map slot-definition-name slots)]
        (format #t "  ~10s: ~a\n" s
                (if (slot-bound? obj s)
                  (with-output-to-string
                    (^[] (write-limited (slot-ref obj s) 60)))
                  "#<unbound>"))))))

(define-method describe (object) ; default
  (describe-common object)
  (describe-slots object)
  (values))

;;;
;;; Make exported symbol visible from outside
;;;

(define-macro (insert-symbols . syms)
  `(begin ,@(map (^[s] `(define-in-module gauche ,s ,s)) syms)))

(insert-symbols ;define-generic define-method define-class
                compute-slots compute-get-n-set compute-slot-accessor
                class-slot-ref class-slot-set! class-slot-bound?
                slot-push! slot-pop! slot-unbound slot-missing
                slot-exists? slot-exists-using-class?
                change-class
                apply-generic sort-applicable-methods
                apply-methods apply-method
                class-of current-class-of is-a? subtype? slot-ref slot-set!
                slot-bound? slot-ref-using-accessor slot-bound-using-accessor?
                slot-set-using-accessor! slot-initialize-using-accessor!
                instance-slot-ref instance-slot-set! touch-instance!
                class-name class-precedence-list class-direct-supers
                class-direct-methods class-direct-subclasses
                class-direct-slots class-slots
                class-post-initialize
                slot-definition-name slot-definition-options
                slot-definition-option
                slot-definition-allocation slot-definition-getter
                slot-definition-setter slot-definition-accessor
                class-slot-definition class-slot-accessor
                x->string x->integer x->number ref |setter of ref|
                ~ ref*
                describe describe-common describe-slots

                ;; These shouldn't be necessary to be injected into gauche
                ;; module; unfortunately, the current define-method and
                ;; define-class are unhygienic, and we need them visible
                ;; from the expanded code.  Should be removed once we rewrite
                ;; related macros hygienic.
                %ensure-generic-function
                %check-class-binding
                )

