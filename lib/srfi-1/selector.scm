;;;
;;; Selectors of SRFI-1
;;;

;; $Id: selector.scm,v 1.1 2001-04-06 09:53:46 shiro Exp $

;; This code is based on the reference implementation by Olin Shivers
;;
;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;; this code as long as you do not remove this copyright notice or
;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.

(select-module srfi-1)

;;; take & drop

(define (take lis k)
  (check-arg integer? k)
  (let recur ((lis lis) (k k))
    (if (zero? k) '()
	(cons (car lis)
	      (recur (cdr lis) (- k 1))))))

(define drop list-tail)  ; Gauche has list-tail

(define (take! lis k)
  (check-arg integer? k)
  (if (zero? k) '()
      (begin (set-cdr! (drop lis (- k 1)) '())
	     lis)))

;;; TAKE-RIGHT and DROP-RIGHT work by getting two pointers into the list, 
;;; off by K, then chasing down the list until the lead pointer falls off
;;; the end.

(define (take-right lis k)
  (check-arg integer? k)
  (let lp ((lag lis)  (lead (drop lis k)))
    (if (pair? lead)
	(lp (cdr lag) (cdr lead))
	lag)))

(define (drop-right lis k)
  (check-arg integer? k)
  (let recur ((lag lis) (lead (drop lis k)))
    (if (pair? lead)
	(cons (car lag) (recur (cdr lag) (cdr lead)))
	'())))

;;; In this function, LEAD is actually K+1 ahead of LAG. This lets
;;; us stop LAG one step early, in time to smash its cdr to ().
(define (drop-right! lis k)
  (check-arg integer? k)
  (let ((lead (drop lis k)))
    (if (pair? lead)

	(let lp ((lag lis)  (lead (cdr lead)))	; Standard case
	  (if (pair? lead)
	      (lp (cdr lag) (cdr lead))
	      (begin (set-cdr! lag '())
		     lis)))

	'())))	; Special case dropping everything -- no cons to side-effect.

(define (split-at x k)
  (check-arg integer? k)
  (let recur ((lis x) (k k))
    (if (zero? k) (values '() lis)
	(receive (prefix suffix) (recur (cdr lis) (- k 1))
	  (values (cons (car lis) prefix) suffix)))))

(define (split-at! x k)
  (check-arg integer? k)
  (if (zero? k) (values '() x)
      (let* ((prev (drop x (- k 1)))
	     (suffix (cdr prev)))
	(set-cdr! prev '())
	(values x suffix))))

(define (last lis) (car (last-pair lis)))
