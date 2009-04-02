;;;
;;; redefutil.scm - class redefinition protocol (autoloaded)
;;;  
;;;   Copyright (c) 2003-2009  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: redefutil.scm,v 1.7 2008-05-10 13:35:56 shirok Exp $
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
  (guard (e (else
             (%commit-class-redefinition! old #f)
             (warn "Class redefinition of ~S is aborted.  The state of the class may be inconsistent" old)))
    (class-redefinition old new)
    (%commit-class-redefinition! old new)))

(define-generic class-redefinition)
(define-method class-redefinition ((old <class>) (new <class>))
  (for-each (lambda (m)
              (if (is-a? m <accessor-method>)
                (delete-method! (slot-ref m 'generic) m)
                (update-direct-method! m old new)))
            (class-direct-methods old))
  (for-each (lambda (sup) (%delete-direct-subclass! sup old))
            (class-direct-supers old))
  (for-each (lambda (sub) (update-direct-subclass! sub old new))
            (class-direct-subclasses old))
  )

(define-generic update-direct-subclass!)
(define-method update-direct-subclass! ((sub <class>)
                                        (orig <class>)
                                        (new <class>))
  (define (new-supers supers)
    (map (lambda (s) (if (eq? s orig) new s)) supers))

  (define (fix-initargs initargs supers metaclass)
    (let loop ((args initargs) (r '()))
      (cond ((null? args) (reverse! r))
            ((eq? (car args) :supers)
             (loop (cddr args) (list* supers :supers r)))
            ((eq? (car args) :metaclass)
             (loop (cddr args) (list* metaclass :metaclass r)))
            (else
             (loop (cddr args) (list* (cadr args) (car args) r))))))

  (let* ((initargs (slot-ref sub 'initargs))
         (supers   (new-supers (class-direct-supers sub)))
         ;; NB: this isn't really correct!
         (metaclass (or (get-keyword :metaclass initargs #f)
                        (%get-default-metaclass supers)))
         (new-sub  (apply make metaclass
                          (fix-initargs initargs supers metaclass))))
    (redefine-class! sub new-sub)
    ;; Trick: redefine-class! above removes subclass form original
    ;; superclass's direct-subclass list, but we want to keep it.
    ;; so we add to it again.  We can't do this within class-redefinition,
    ;; for we don't know if it is called on the top of redefinition
    ;; or in the subclasses' redefinition.
    (for-each (lambda (sup) (%add-direct-subclass! sup sub))
              (class-direct-supers sub))
    ;; If the subclass has global bindings, replace them.
    (%replace-class-binding! sub new-sub)))

;;----------------------------------------------------------------
;; Instance update protocol
;;

;; By default, only the instance-allocated slots in the old class
;; are carried over.
;; If the user wants to keep other slot values, (s)he needs to
;; overload change-class method.

;; A dynamic stack that keeps change-class invocation.  We need
;; this to prevent inadvertent infinit recursive call of change-class.
;; Each element is (<instance> . <continuation>)
(define instance-changing-class-stack (make-parameter '()))

;; Change class.
(define (change-object-class obj old-class new-class)

  (define (default-carry-over-slot? slot)
    (let ((slot-name (slot-definition-name slot))
          (alloc     (slot-definition-allocation slot)))
      (and (or (eq? alloc :instance)
               (and (memq alloc '(:class :each-subclass))
                    (not (class-slot-bound? new-class slot-name))))
           (cons slot-name slot))))

  (let ((new (allocate-instance new-class '()))
        (new-slots (filter default-carry-over-slot? (class-slots new-class)))
        )
    (cond
     ((assq obj (instance-changing-class-stack))
      ;; change-class is called recursively.  abort change-class protocol.
      => (lambda (p) ((cdr p) #f)))
     (else
      (dolist (slot new-slots)
        (let ((slot-name (slot-definition-name slot)))
          (or (and
               (slot-exists-using-class? old-class obj slot-name)
               (call/cc
                (lambda (cont)
                  (parameterize
                      ((instance-changing-class-stack
                        (acons obj cont (instance-changing-class-stack))))
                    (and (slot-bound-using-class? old-class obj slot-name)
                         (let1 val
                             (slot-ref-using-class old-class obj slot-name)
                           (slot-set-using-class! new-class new slot-name val)
                           #t))))))
              (let ((acc (class-slot-accessor new-class slot-name)))
                (slot-initialize-using-accessor! new acc '())))))
      ))
    (%transplant-instance! new obj)
    obj))



;; inject definitions into gauche module
(define-in-module gauche redefine-class! redefine-class!)
(define-in-module gauche class-redefinition class-redefinition)
(define-in-module gauche update-direct-subclass! update-direct-subclass!)
(define-in-module gauche change-object-class change-object-class)

(provide "gauche/redefutil")
