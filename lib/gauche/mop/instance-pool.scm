;;;
;;; instance-pool.scm - instance pool metaclass
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
;;;  $Id: instance-pool.scm,v 1.2 2002-10-14 01:50:39 shirok Exp $
;;;

;; EXPERIMENTAL.   THE API MAY CHANGE.

(define-module gauche.mop.instance-pool
  (use srfi-1)
  (use util.queue)
  (export <instance-pool-meta> <instance-pool-mixin>
          <instance-table-meta> <instance-table-mixin>
          instance-pool->list instance-pool-find instance-pool-remove
          instance-pool:create-pool instance-pool:compute-pools
          instance-pool:add instance-pool:find instance-pool:remove
          instance-pool:->list)
  )
(select-module gauche.mop.instance-pool)

;; 'Instance pool' class is a class that keeps the list of instances
;; of itself and its subclasses.
;; A class that inherits <instance-pool-mixin> directly becomes a 'root'
;; class of the pool.  Instances of the subclass of the root class will
;; be added to the pool.
;;
;; An application can have multiple root classes.  If a class inherits
;; from two or more pooled classes, its instances will be added to
;; all the pools.
;;
;; The actual implementation of how to manage pools can be customizable
;; by subclassing <instance-pool-meta> and overloading methods.

(define-class <instance-pool-meta> (<class>)
  ((instance-pool:pool  :accessor instance-pool-of :init-value #f)
   (instance-pool:pools :accessor instance-pools-of)
   ))

(define-method instance-pool-of (obj) #f) ; fallback

(define-class <instance-pool-mixin> ()
  ()
  :metaclass <instance-pool-meta>)

;; Pool management protocol
;;
;;  instance-pool-create-pool (class <instance-pool-meta>)
;;     Called on the class that directly inherits <instance-pool-mixin>,
;;     and has to return a pool object.  A pool object can be any object
;;     that responds the following methods:
;;
;;     (instance-pool:add pool instance) - add instance to pool.
;;     (instance-pool:find pool pred)
;;     (instance-pool:remove! pool pred)
;;     (instance-pool:->list pool)
;;
;;  instance-pool-compute-pools (class <instance-pool-meta>)

;; Default pool is just a list
(define-class <instance-pool:list-pool> ()
  ((instances :init-value '())))

(define-method instance-pool:add ((pool <instance-pool:list-pool>) instance)
  (push! (ref pool 'instances) instance))
  
(define-method instance-pool:find ((pool <instance-pool:list-pool>) pred)
  (find pred (ref pool 'instances)))
  
(define-method instance-pool:remove! ((pool <instance-pool:list-pool>) pred)
  (update! (ref pool 'instances) (cut remove! pred <>)))

(define-method instance-pool:->list ((pool <instance-pool:list-pool>))
  (reverse (ref pool 'instances)))

;; The default protocol
(define-method instance-pool:create-pool ((class <instance-pool-meta>))
  (make <instance-pool:list-pool>))

(define-method instance-pool:compute-pools ((class <instance-pool-meta>))
  (filter-map instance-pool-of (class-precedence-list class)))

(define-method write-object ((class <instance-pool-meta>) out)
  (format out "#<<instance-pool-meta> ~a>" (class-name class)))

(define-method initialize ((class <instance-pool-meta>) initargs)
  (next-method)
  (when (memq <instance-pool-mixin> (class-direct-supers class))
    (set! (instance-pool-of class)
          (instance-pool:create-pool class)))
  (set! (instance-pools-of class)
        (instance-pool:compute-pools class))
  )

(define-method initialize ((self <instance-pool-mixin>) initargs)
  (next-method)
  (for-each (cut instance-pool:add <> self)
            (instance-pools-of (class-of self))))

;; External operations
(define-method instance-pool->list ((class <instance-pool-meta>))
  (cond ((instance-pool-of class) => (cut instance-pool:->list <>))
        (else
         (apply lset-union eq?
                (map (cut instance-pool:->list <>)
                     (instance-pools-of class))))
        ))

(define-method instance-pool-find ((class <instance-pool-meta>) pred)
  (cond ((instance-pool-of class) => (cut instance-pool:find <> pred))
        (else (map instance-pool-find (instance-pools-of class)))))

(define-method instance-pool-remove ((class <instance-pool-meta>) pred)
  (cond ((instance-pool-of class) => (cut instance-pool:remove <> pred))
        (else (map instance-pool-find (instance-pools-of class)))))

;; The operations to the instance propagates to its class
(define-method instance-pool->list ((self <instance-pool-mixin>))
  (instance-pool->list (class-of self)))
(define-method instance-pool-find ((self <instance-pool-mixin>) pred)
  (instance-pool-find (class-of self)))
(define-method instance-pool-remove ((self <instance-pool-mixin>) pred)
  (instance-pool-remove (class-of self)))

(provide "gauche/mop/instance-pool")

