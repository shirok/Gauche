;;;
;;; List predicates of SRFI-1
;;;

;; $Id: pred.scm,v 1.1 2001-04-06 09:53:46 shiro Exp $

;; This code is based on the reference implementation by Olin Shivers
;;
;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;; this code as long as you do not remove this copyright notice or
;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.

(select-module srfi-1)

;;; <proper-list> ::= ()			; Empty proper list
;;;		  |   (cons <x> <proper-list>)	; Proper-list pair
;;; Note that this definition rules out circular lists -- and this
;;; function is required to detect this case and return false.

(define (proper-list? x)
  (let lp ((x x) (lag x))
    (if (pair? x)
	(let ((x (cdr x)))
	  (if (pair? x)
	      (let ((x   (cdr x))
		    (lag (cdr lag)))
		(and (not (eq? x lag)) (lp x lag)))
	      (null? x)))
	(null? x))))

;;; A dotted list is a finite list (possibly of length 0) terminated
;;; by a non-nil value. Any non-cons, non-nil value (e.g., "foo" or 5)
;;; is a dotted list of length 0.
;;;
;;; <dotted-list> ::= <non-nil,non-pair>	; Empty dotted list
;;;               |   (cons <x> <dotted-list>)	; Proper-list pair

(define (dotted-list? x)
  (let lp ((x x) (lag x))
    (if (pair? x)
	(let ((x (cdr x)))
	  (if (pair? x)
	      (let ((x   (cdr x))
		    (lag (cdr lag)))
		(and (not (eq? x lag)) (lp x lag)))
	      (not (null? x))))
	(not (null? x)))))

(define (circular-list? x)
  (let lp ((x x) (lag x))
    (and (pair? x)
	 (let ((x (cdr x)))
	   (and (pair? x)
		(let ((x   (cdr x))
		      (lag (cdr lag)))
		  (or (eq? x lag) (lp x lag))))))))

(define (list= = . lists)
  (or (null? lists) ; special case

      (let lp1 ((list-a (car lists)) (others (cdr lists)))
	(or (null? others)
	    (let ((list-b (car others))
		  (others (cdr others)))
	      (if (eq? list-a list-b)	; EQ? => LIST=
		  (lp1 list-b others)
		  (let lp2 ((list-a list-a) (list-b list-b))
		    (if (null-list? list-a)
			(and (null-list? list-b)
			     (lp1 list-b others))
			(and (not (null-list? list-b))
			     (= (car list-a) (car list-b))
			     (lp2 (cdr list-a) (cdr list-b)))))))))))
			

(define (length+ x)			; Returns #f if X is circular.
  (let lp ((x x) (lag x) (len 0))
    (if (pair? x)
	(let ((x (cdr x))
	      (len (+ len 1)))
	  (if (pair? x)
	      (let ((x   (cdr x))
		    (lag (cdr lag))
		    (len (+ len 1)))
		(and (not (eq? x lag)) (lp x lag len)))
	      len))
	len)))
