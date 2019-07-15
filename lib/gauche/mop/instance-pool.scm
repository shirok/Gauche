;;;
;;; instance-pool.scm - instance pool metaclass
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
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

;; EXPERIMENTAL.   THE API MAY CHANGE.

(define-module gauche.mop.instance-pool
  (use srfi-1)
  (export <instance-pool-meta> <instance-pool-mixin>
          <instance-table-meta> <instance-table-mixin>
          instance-pool-of instance-pools-of
          |setter of instance-pool-of| |setter of instance-pools-of|
          instance-pool->list instance-pool-find instance-pool-remove!
          instance-pool-fold instance-pool-for-each instance-pool-map
          instance-pool:create-pool instance-pool:compute-pools
          instance-pool:add instance-pool:find instance-pool:remove!
          instance-pool:->list
          instance-pool:fold instance-pool:for-each instance-pool:map)
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

(define-method instance-pool:fold ((pool <instance-pool:list-pool>) kons knil)
  (fold kons knil (ref pool 'instances)))

(define-method instance-pool:for-each ((pool <instance-pool:list-pool>) proc)
  (for-each proc (ref pool 'instances)))

(define-method instance-pool:map ((pool <instance-pool:list-pool>) proc)
  (map proc (ref pool 'instances)))

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
        (else (map (cut instance-pool:find <> pred) (instance-pools-of class)))
        ))

(define-method instance-pool-remove! ((class <instance-pool-meta>) pred)
  (cond ((instance-pool-of class) => (cut instance-pool:remove! <> pred))
        (else (map (cut instance-pool:remove! <> pred)
                   (instance-pools-of class)))
        ))

(define-method instance-pool-fold ((class <instance-pool-meta>) kons knil)
  (cond ((instance-pool-of class) => (cut instance-pool:fold <> kons knil))
        (else (errorf "class ~s doesn't own an instance pool" class))))

(define-method instance-pool-for-each ((class <instance-pool-meta>) proc)
  (cond ((instance-pool-of class) => (cut instance-pool:for-each <> proc))
        (else (errorf "class ~s doesn't own an instance pool" class))))

(define-method instance-pool-map ((class <instance-pool-meta>) proc)
  (cond ((instance-pool-of class) => (cut instance-pool:map <> proc))
        (else (errorf "class ~s doesn't own an instance pool" class))))

;; The operations to the instance propagates to its class
(define-method instance-pool->list ((self <instance-pool-mixin>))
  (instance-pool->list (class-of self)))
(define-method instance-pool-find ((self <instance-pool-mixin>) pred)
  (instance-pool-find (class-of self) pred))
(define-method instance-pool-remove! ((self <instance-pool-mixin>) pred)
  (instance-pool-remove! (class-of self) pred))
(define-method instance-pool-fold ((self <instance-pool-mixin>) kons knil)
  (instance-pool-fold (class-of self) kons knil))
(define-method instance-pool-for-each ((self <instance-pool-mixin>) proc)
  (instance-pool-for-each (class-of self) proc))
(define-method instance-pool-map ((self <instance-pool-mixin>) proc)
  (instance-pool-map (class-of self) proc))


