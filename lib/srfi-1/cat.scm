;;;
;;; Concatenators of SRFI-1
;;;

;; $Id: cat.scm,v 1.1 2001-04-06 09:53:46 shiro Exp $

;; This code is based on the reference implementation by Olin Shivers
;;
;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;; this code as long as you do not remove this copyright notice or
;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.

(select-module srfi-1)

(define (append! . lists)
  ;; First, scan through lists looking for a non-empty one.
  (let lp ((lists lists) (prev '()))
    (if (not (pair? lists)) prev
	(let ((first (car lists))
	      (rest (cdr lists)))
	  (if (not (pair? first)) (lp rest first)

	      ;; Now, do the splicing.
	      (let lp2 ((tail-cons (last-pair first))
			(rest rest))
		(if (pair? rest)
		    (let ((next (car rest))
			  (rest (cdr rest)))
		      (set-cdr! tail-cons next)
		      (lp2 (if (pair? next) (last-pair next) tail-cons)
			   rest))
		    first)))))))

(define (append-reverse rev-head tail)
  (let lp ((rev-head rev-head) (tail tail))
    (if (null-list? rev-head) tail
	(lp (cdr rev-head) (cons (car rev-head) tail)))))

(define (append-reverse! rev-head tail)
  (let lp ((rev-head rev-head) (tail tail))
    (if (null-list? rev-head) tail
	(let ((next-rev (cdr rev-head)))
	  (set-cdr! rev-head tail)
	  (lp next-rev rev-head)))))


(define (concatenate  lists) (reduce-right append  '() lists))
(define (concatenate! lists) (reduce-right append! '() lists))

