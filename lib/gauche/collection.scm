;;;
;;; collection.scm - collection generics
;;;
;;;  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: collection.scm,v 1.1 2001-10-11 11:55:59 shirok Exp $
;;;

;; Defines generic operations over collection.   A collection is
;; a set of objects, possibly containing infinite objects.
;; A class that implements a collection generic interface must
;; define at least one method:
;;
;;  (g-fold proc knil (obj <collection))
;;
;; The other operations are derived from the generic fold+, but
;; the specific collection inplementation can override some of them
;; for efficiency.

(define-module gauche.collection
  (export g-size
          g-fold
          g-map
          g-for-each
          g-collect
          g-find
          g-filter
          g-remove
          g-any?
          g-every?
          )
  )
(select-module gauche.collection)

;;-------------------------------------------------------------
;; g-fold - a fundamental iterator over a collection.
;;
;;  (g-fold proc collection knil knil2 ...)
;;
;; Note that the order of argument is different from fold in SRFI-1.

(define-generic g-fold )

;; Other generic methods.  The specific implementation of a
;; collection can override any of them.
;; Note that the exhausive map operation may not terminate
;; if the collection has unlimited number of elements.
;; You can use call/cc to break out of the loop, or use g-enumerate
;; generic function.

;; Returns number of items in the collection.
(define-method g-size (coll) (g-fold + coll 0))

;; Map proc to each element in the collection, collecting the
;; results into a list.
(define-method g-map (proc coll)
  (reverse (g-fold (lambda (elt r) (cons (proc elt) r)) coll '())))

;; Apply proc to each element in the collection.
(define-method g-for-each (proc coll)
  (g-fold (lambda (elt r) (proc elt)) coll '())
  (undefined))


;; auxiliary macros to support differential list accumulation
(define-syntax dlist-seed
  (syntax-rules ()
    ((_) (let ((x (cons '() '()))) (cons x x)))))

(define-syntax dlist-append!
  (syntax-rules ()
    ((_ dlist item)
     (begin (set! (cddr dlist) item)
            (set! (cdr dlist) (last-pair (cddr dlist)))))))


;; Like map, but the result of proc is appended to the result.
;; Common Lisp's mapcan.
(define-method g-collect (proc coll)
  (cdr (g-fold (lambda (elt r) (dlist-append! r (proc elt)))
               coll
               (dlist-seed))))

;; Enumerate
;;  Cf. http://pobox.com/~oleg/ftp/Scheme/enumerators-callcc.html

(define-method g-enumerate (proc coll)
  (call/cc
   (lambda (break)
     (g-fold (lambda (elt r)
               (unless (proc elt) (break)))
             coll '()))))

(define-method g-enumerate (proc coll arg1)
  (call/cc
   (lambda (break)
     (g-fold (lambda (elt r)
               (receive (flag x) (proc elt r)
                 (if flag
                     x
                     (break r))))
             coll
             arg1))))

(define-method g-enumerate (proc coll arg1 arg2)
  (call/cc
   (lambda (break)
     (g-fold (lambda (elt r0 r1)
               (receive (flag x0 x1) (proc elt r0 r1)
                 (if flag
                     (values x0 x1)
                     (break r0 r1))))
             coll
             arg1 arg2))))

(define-method g-enumerate (proc coll arg1 arg2 arg3)
  (call/cc
   (lambda (break)
     (g-fold (lambda (elt r0 r1 r2)
               (receive (flag x0 x1 x2) (proc elt r0 r1 r2)
                 (if flag
                     (values x0 x1 x2)
                     (break r0 r1 r2))))
             coll
             arg1 arg2 arg3))))

;; TODO g-enumerate (proc coll arg1 arg2 arg3 . rest)
          
(provide "gauche/collection")
