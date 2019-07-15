;;;
;;; redefutil.scm - class redefinition protocol (autoloaded)
;;;
;;;   Copyright (c) 2003-2019  Shiro Kawai  <shiro@acm.org>
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

;; This file is autoloaded

(select-module gauche.object)
(use gauche.parameter) ;; used by instance update protocol
(use srfi-1)           ;; used by instance update protocol

;;----------------------------------------------------------------
;; Class Redefinition protocol
;;

;; redefine-class! old new  [function]
;;   <with locking old>
;;     class-redefinition old new   [gf]
;;       <update direct methods>
;;       <update direct supers>
;;       update-direct-subclass! c orig clone      [gf]

(define (redefine-class! old new)
  (%start-class-redefinition! old) ;; MT safety
  (guard (e [else
             (%commit-class-redefinition! old #f)
             (warn "Class redefinition of ~S is aborted.  The state of the class may be inconsistent" old)])
    (class-redefinition old new)
    (%commit-class-redefinition! old new)))

(define-generic class-redefinition)
(define-method class-redefinition ((old <class>) (new <class>))
  (for-each (^m (if (is-a? m <accessor-method>)
                  (delete-method! (slot-ref m 'generic) m)
                  (update-direct-method! m old new)))
            (class-direct-methods old))
  (for-each (^[sup] (%delete-direct-subclass! sup old))
            (class-direct-supers old))
  (for-each (^[sub] (update-direct-subclass! sub old new))
            (class-direct-subclasses old))
  )

(define-generic update-direct-subclass!)
(define-method update-direct-subclass! ((sub <class>)
                                        (orig <class>)
                                        (new <class>))
  (define (new-supers supers)
    (map (^s (if (eq? s orig) new s)) supers))

  (define (fix-initargs initargs supers metaclass)
    (let loop ([args initargs] [r '()])
      (cond [(null? args) (reverse! r)]
            [(eq? (car args) :supers)
             (loop (cddr args) (list* supers :supers r))]
            [(eq? (car args) :metaclass)
             (loop (cddr args) (list* metaclass :metaclass r))]
            [else (loop (cddr args) (list* (cadr args) (car args) r))])))
  
  (let* ([initargs (slot-ref sub 'initargs)]
         [supers   (new-supers (class-direct-supers sub))]
         ;; NB: this isn't really correct!
         [metaclass (or (get-keyword :metaclass initargs #f)
                        (%get-default-metaclass supers))]
         [new-sub  (apply make metaclass
                          (fix-initargs initargs supers metaclass))])
    (redefine-class! sub new-sub)
    ;; Trick: redefine-class! above removes subclass form original
    ;; superclass's direct-subclass list, but we want to keep it.
    ;; so we add to it again.  We can't do this within class-redefinition,
    ;; for we don't know if it is called on the top of redefinition
    ;; or in the subclasses' redefinition.
    (for-each (^[sup] (%add-direct-subclass! sup sub))
              (class-direct-supers sub))
    ;; If the subclass has global bindings, replace them.
    (%replace-class-binding! sub new-sub)))

;;----------------------------------------------------------------
;; Instance update protocol
;;

;; By default, the following slots of the new class are carried over:
;;  - it is instance allocated.
;;  - its allocation is either class or each-subclass, without having
;;    the default value.
;;  - it is a builtin slot and settable in the new class.
;;
;; If you want to carry over other slots, the first thing you'd want
;; to try is to customize change-class method.  Save the old slots before
;; calling next-method and set the new slots afterwards.  It can also be
;; used to carry over a value when the name of the slot is changed.
;; If you want to prevent some slots from being carried over by the
;; base change-class method, you can override
;; change-object-class-carry-over-slot? method to do so.

(define-generic change-object-class-carry-over-slot?)
(define-method change-object-class-carry-over-slot? ((obj <object>)
                                                     old-class new-class slot)
  (let ([slot-name (slot-definition-name slot)]
        [alloc     (slot-definition-allocation slot)])
    (or (eq? alloc :instance)
        (and (memq alloc '(:class :each-subclass))
             (not (class-slot-bound? new-class slot-name)))
        (and-let* ([ (eq? alloc :builtin) ]
                   [sa (slot-definition-option slot :slot-accessor #f)])
          (slot-ref sa 'settable)))))

;; A dynamic stack that keeps change-class invocation.  We need
;; this to prevent inadvertent infinit recursive call of change-class.
;; Each element is (<instance> . <continuation>)
(define instance-changing-class-stack (make-parameter '()))

;; Change class.
(define (change-object-class obj old-class new-class)

  (let ([new (allocate-instance new-class '())]
        [new-slots (filter (^s (change-object-class-carry-over-slot?
                                obj old-class new-class s))
                           (class-slots new-class))])
    (if-let1 p (assq obj (instance-changing-class-stack))
      ;; change-class is called recursively.  abort change-class protocol.
      ((cdr p) #f)
      ;; normal course of action
      (dolist (slot new-slots)
        (let1 slot-name (slot-definition-name slot)
          (or (and
               (slot-exists-using-class? old-class obj slot-name)
               (let/cc cont
                 (parameterize
                     ([instance-changing-class-stack
                       (acons obj cont (instance-changing-class-stack))])
                   (and (slot-bound-using-class? old-class obj slot-name)
                        (let1 val
                            (slot-ref-using-class old-class obj slot-name)
                          (slot-set-using-class! new-class new slot-name val)
                          #t)))))
              (let1 acc (class-slot-accessor new-class slot-name)
                (slot-initialize-using-accessor! new acc '()))))))
    ;; overwrite original obj's content with the new one.
    (%transplant-instance! new obj)
    obj))

;; Intercept metaclass change; that is, we're about to replace the
;; class C's metaclass by NEW-META.
;; We have to prevent the cpl slot from being carried over by default,
;; for it has extra consistency check that interferes with our purpose.
;;
;; NB: At this moment it is impossible that changing metaclass
;; affects the structure of the instance, since (initialize <new-metaclass>)
;; isn't called,   Initialize is the only place where the structure
;; of the instance can be determined.  This fact is important, since
;; the instance update protocol won't run on instances whose class's
;; metaclass is changed (we can't, since the class maintains its identity
;; before and after metaclass change.)   If the updated class changed
;; instance structure, accessing old instances would cause bad things.
;; In future we may introduce reinitialize method which is called
;; right after change-object is done; in such case we need extra safety
;; mechanism to ensure instance structure isn't changed by metaclass
;; change.

(define-method change-class ((c <class>) (new-meta <class>))
  (let* ([old-meta (current-class-of c)]
         [old-cpl (slot-ref-using-class old-meta c 'cpl)]
         [old-nis (slot-ref-using-class old-meta c 'num-instance-slots)])
    (next-method)
    (slot-set-using-class! new-meta c 'cpl old-cpl)
    (%finish-class-initialization! c) ; seal the class
    c))

(define-method change-object-class-carry-over-slot?
    ((c <class>) old-meta new-meta slot)
  (and (not (eq? (slot-definition-name slot) 'cpl))
       (next-method)))

;; inject definitions into gauche module
(define-in-module gauche redefine-class! redefine-class!)
(define-in-module gauche class-redefinition class-redefinition)
(define-in-module gauche update-direct-subclass! update-direct-subclass!)
(define-in-module gauche change-object-class change-object-class)

