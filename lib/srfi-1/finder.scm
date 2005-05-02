;;;
;;; Find and alike of SRFI-1
;;;

;; $Id: finder.scm,v 1.4 2005-05-02 10:30:39 shirok Exp $

;; This code is based on the reference implementation by Olin Shivers
;;
;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;; this code as long as you do not remove this copyright notice or
;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.

(select-module srfi-1)

(define (find$ pred) (pa$ find pred))

(define (find-tail pred list)
  (let lp ((list list))
    (and (not (null-list? list))
	 (if (pred (car list))
             list
	     (lp (cdr list))))))

(define (find-tail$ pred) (pa$ find-tail pred))

;; use tail-recursion
(define (take-while pred lis)
  (let recur ((lis lis)
              (r '()))
    (if (null-list? lis)
        (reverse! r)
        (if (pred (car lis))
            (recur (cdr lis) (cons (car lis) r))
            (reverse! r)))))

(define (drop-while pred lis)
  (let lp ((lis lis))
    (if (null-list? lis)
        '()
	(if (pred (car lis))
	    (lp (cdr lis))
	    lis))))

(define (take-while! pred lis)
  (if (or (null-list? lis)
          (not (pred (car lis))))
      '()
      (begin (let lp ((prev lis) (rest (cdr lis)))
	       (if (pair? rest)
                   (if (pred (car rest))
                       (lp rest (cdr rest))
                       (set-cdr! prev '()))))
	     lis)))

(define (span pred lis)
  (let recur ((lis lis)
              (in  '()))
    (if (null-list? lis)
        (values (reverse! in) '())
        (if (pred (car lis))
            (recur (cdr lis) (cons (car lis) in))
            (values (reverse! in) lis)))))

(define (span! pred lis)
  (if (or (null-list? lis)
          (not (pred (car lis))))
      (values '() lis)
      (let ((suffix (let lp ((prev lis) (rest (cdr lis)))
		      (if (null-list? rest) rest
			  (let ((x (car rest)))
			    (if (pred x)
                                (lp rest (cdr rest))
				(begin (set-cdr! prev '())
				       rest)))))))
	(values lis suffix))))

(define (break  pred lis) (span  (lambda (x) (not (pred x))) lis))
(define (break! pred lis) (span! (lambda (x) (not (pred x))) lis))

(define (any$ pred) (pa$ any pred))

(define (every pred lis1 . lists)
  (if (pair? lists)

      ;; N-ary case
      (receive (heads tails) (%cars+cdrs (cons lis1 lists))
	(or (not (pair? heads))
	    (let lp ((heads heads) (tails tails))
	      (receive (next-heads next-tails) (%cars+cdrs tails)
		(if (pair? next-heads)
		    (and (apply pred heads) (lp next-heads next-tails))
		    (apply pred heads)))))) ; Last PRED app is tail call.

      ;; Fast path
      (or (null-list? lis1)
	  (let lp ((head (car lis1))  (tail (cdr lis1)))
	    (if (null-list? tail)
		(pred head)	; Last PRED app is tail call.
		(and (pred head) (lp (car tail) (cdr tail))))))))

(define (every$ pred) (pa$ every pred))

(define (list-index pred lis1 . lists)
  (if (pair? lists)

      ;; N-ary case
      (let lp ((lists (cons lis1 lists)) (n 0))
	(receive (heads tails) (%cars+cdrs lists)
	  (and (pair? heads)
	       (if (apply pred heads) n
		   (lp tails (+ n 1))))))

      ;; Fast path
      (let lp ((lis lis1) (n 0))
	(and (not (null-list? lis))
	     (if (pred (car lis)) n (lp (cdr lis) (+ n 1)))))))
