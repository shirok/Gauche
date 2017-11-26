;; -*- coding:utf-8 -*-
;; Copyright (C) Marc Nieper-Wißkirchen (2017).  All Rights Reserved. 

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

(define-library (srfi 150 test)
  (export run-tests)
  (import (except (scheme base) define-record-type)
	  (srfi 64)
	  (srfi 150))
  (begin
    (define (run-tests)
      (test-begin "SRFI 150")

      (test-group "Simple"

		  (define-record-type <pare>
		    (kons x y)
		    pare?
		    (x kar set-kar!)
		    (y kdr))

		  (test-assert (pare? (kons 1 2)))
		  (test-assert (not (pare? (cons 1 2))))
		  (test-eqv 1 (kar (kons 1 2)))
		  (test-eqv 2 (kdr (kons 1 2)))
		  (test-eqv 3 (let ((k (kons 1 2)))
				(set-kar! k 3)
				(kar k))))

      (test-group "Inheritance"

		  (define-record-type <parent>
		    (make-parent x)
		    parent?
		    (x parent-field parent-set-field!))

		  (define-record-type (<child> <parent>)
		    (make-child x y)
		    child?
		    (y child-field child-set-field!))

		  (test-assert (parent? (make-child 1 2)))
		  (test-assert (child? (make-child 1 2)))
		  (test-assert (not (child? (make-parent 1))))
		  (test-eqv 1 (parent-field (make-child 1 2)))
		  (test-eqv 2 (child-field (make-child 1 2)))
		  (test-eqv 3 (let ((c (make-child 1 2)))
				(parent-set-field! c 3)
				(parent-field c)))
		  (test-eqv 3 (let ((c (make-child 1 2)))
				(child-set-field! c 3)
				(child-field c))))

      (test-group "Implicit constructor arguments"

		  (define-record-type <parent>
		    (make-parent)
		    parent?
		    (x parent-field))

		  (define-record-type (<child> <parent>)
		    make-child
		    child?
		    (y child-field))

		  (test-eqv 1 (parent-field (make-child 1 2)))
		  (test-eqv 2 (child-field (make-child 1 2))))

      (test-group "Shadowing of parent fields"

		  (define-record-type <parent>
		    (make-parent x)
		    parent?
		    (x parent-field parent-set-field!))

		  (define-record-type (<child> <parent>)
		    (%make-child x)
		    child?
		    (x child-field))

		  (define (make-child x)
		    (let ((c (%make-child x)))
		      (parent-set-field! c 'undefined)
		      c))
		  
		  (test-eqv 1 (child-field (make-child 1)))
		  (test-eqv 'undefined (parent-field (make-child 1))))

      (test-group "Field referral through accessors"

		  (define-record-type <record>
		    (make-record x y get-z)
		    record?
		    (x get-x)
		    (y x)
		    (z get-z))

		  (test-eqv 1 (get-x (make-record 1 2 3)))
		  (test-eqv 2 (x (make-record 1 2 3)))
		  (test-eqv 3 (get-z (make-record 1 2 3))))

      (test-group "Hygiene 1"

		  (define a #f)
		  
		  (define-syntax def
		    (syntax-rules ()
		      ((def b make-record get-a get-b)
		       (define-record-type <record>
			 (make-record a b)
			 record?
			 (a get-a)
			 (b get-b)))))
		  
		  (def a make-record get-a get-b)

		  (test-eqv 1 (get-a (make-record 1 2)))
		  (test-eqv 2 (get-b (make-record 1 2))))

      (test-group "Hygiene 2"

		  (define x #f)
		  
		  (define-record-type <parent>
		    (make-parent x)
		    parent?
		    (x parent-get))

		  (define-syntax define-child
		    (syntax-rules ()
		      ((define-child make-child child-get parent-field)
		       (define-record-type (<child> <parent>)
			 (make-child parent-field x)
			 child?
			 (x child-get)))))

		  (define-child make-child child-get x)

		  (test-eqv 1 (parent-get (make-child 1 2)))
		  (test-eqv 2 (child-get (make-child 1 2))))
      
      (test-group "Alex Shinn's example"

		  (define-syntax define-tuple-type
		    (syntax-rules ()
		      ((define-tuple-type name make pred x-ref (defaults ...))
		       (deftuple name (make) pred x-ref (defaults ...) (defaults ...) ()))))

		  (define-syntax deftuple
		    (syntax-rules ()
		      ((deftuple name (make args ...) pred x-ref defaults (default . rest)
			 (fields ...))
		       (deftuple name (make args ... tmp) pred x-ref  defaults rest
			 (fields ... (tmp tmp))))
		      ((deftuple name (make args ...) pred x-ref (defaults ...) ()
			 ((field-name get) ...))
		       (begin
			 (define-record-type name (make-tmp args ...) pred
			   (field-name get) ...)
			 (define (make . o)
			   (if (pair? o) (apply make-tmp o) (make-tmp defaults ...)))
			 (define x-ref
			   (let ((accessors (vector get ...)))
			     (lambda (x i)
			       ((vector-ref accessors i) x))))))))

		  (define-tuple-type point make-point point? point-ref (0 0))

		  (let ((pt (make-point)))
		    (test-equal '(0 0) (list (point-ref pt 0) (point-ref pt 1))))
		  (let ((pt (make-point 1 2)))
		    (test-equal '(1 2) (list (point-ref pt 0) (point-ref pt 1)))))
      
      (test-end))
))
