;;;
;;; relations.scm - relational operaions
;;;
;;;  Copyright(C) 2005 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: relations.scm,v 1.2 2005-07-15 01:53:36 shirok Exp $
;;;

;;; Given set of values S1, S2, ..., Sn, a relation R is a set of tuples
;;; such that the first element of a tuple is from S1, the second from
;;; S2, ..., and the n-th from Sn.  In another word, R is a subset of
;;; Cartesian product of S1, ..., Sn.
;;;
;;; (This is actually THE definition from the original Codd's 1970 paper,
;;; "A Relational Model of Data for Large Shared Data Banks", in
;;; CACM 13(6) pp.377--387.)
;;;
;;; This definition can be applied to various datasets: A set of Gauche
;;; instances is a relation, if you view each instance as a tuple and
;;; each slot value as the actual values.  A list of lists can be a
;;; relation.  A stream that reads from CSV table produces a relation.
;;; Thus it would be useful to provide a module that implements generic
;;; operations on relations, no matter how the actual representation is.
;;;
;;; From the operational point of view, we can treat any datastructure
;;; that provides the following two properties.
;;;
;;;  - a collection of tuples, that is, the structure should implement
;;;    <collection> framework.
;;;  - metadata to retrieve individual values from a given tuple.
;;;

(define-module util.relations
  (use gauche.sequence)
  (use srfi-1)
  (use util.list)
  (export <relation>
          relation-columns
          relation-column-getter
          relation-column-setter
          ))
(select-module util.relations)

;; An abstract base class of <relation>.
(define-class <relation> (<collection>) ())

;;; Minimal set of generic functions.  Concrete implementations must
;;; implement the followings.

(define-method relation-column-names ((r <relation>)) '())

(define-method relation-column-getter ((r <relation>) column-key)
  (lambda (row) #f))

(define-method relation-column-setter ((r <relation>) column-key)
  (lambda (row v) (undefined)))

;; Optional methods
;(define-method relation-accessor ((r <relation>))
;  (let1 cols (relation-column-names r)
;    (lambda (row col . maybe-default)
;      (if (find col cols)
;        ((relation-column-getter r col) o)
;        (error 

;; Optionally a subclass may provide them.
(define-method relation-insert ((r <relation>) tuple) #f)
(define-method relation-delete ((r <relation>) tuple) #f)

;;
;; Generic stuff
;;

;; Cf. E.F.Codd: Extending the database relational model to capture
;; more meaning, ACM trans. on database systems 4(4) pp.397--434, Dec. 1979.

;; project
;; select
;; join

;;=============================================================
;; Concrete implementations
;;

;; <simple-relation>
;;   
;;
(define-class <simple-relation> (<relation>)
  ((column-alist :init-keyword :column-alist :init-value '())
   (data :init-keyword :data :init-value '())) ;; list of vectors

(define-method relation-columns ((r <simple-relation>))
  (map car (ref r 'column-alist)))

(define-method relation-column-getter ((r <simple-relation>) key)
  (let1 ind (assoc-ref (ref r 'column-alist) key)
    (unless ind
      (error "simple relation: invalid column key:" key))
    (lambda (o) (vector-ref o ind))))

(define-method relation-column-setter ((r <simple-relation>) key)
  (let1 ind (assoc-ref (ref r 'column-alist) key)
    (unless ind
      (error "simple relation: invalid column key:" key))
    (lambda (o v) (vector-set! o ind v))))

;;
;; <object-set-relation>
;;

(define-class <object-set-relation> (<relation>)
  ((class :init-keyword :class :init-value <object>)
   (data  :init-keyword :data  :init-value '())  ;; list of instances of class
   ))

(define-method relation-columns ((r <object-set-relation>))
  (map slot-definition-name (class-slots (ref r 'class))))

(define-method relation-column-getter ((r <object-set-relation>) key)
  (lambda (o) (slot-ref o key)))

(define-method relation-column-setter ((r <object-set-relation>) key)
  (lambda (o v) (slot-set! o key v)))

(provide "util/relations")


