;;;
;;; Mapper of SRFI-1
;;;

;; $Id: mapper.scm,v 1.1 2001-04-06 09:53:46 shiro Exp $

;; This code is based on the reference implementation by Olin Shivers
;;
;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;; this code as long as you do not remove this copyright notice or
;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.

(select-module srfi-1)

(define (append-map f lis1 . lists)
  (really-append-map append-map  append  f lis1 lists))
(define (append-map! f lis1 . lists) 
  (really-append-map append-map! append! f lis1 lists))

(define (really-append-map who appender f lis1 lists)
  (check-arg procedure? f)
  (if (pair? lists)
      (receive (cars cdrs) (%cars+cdrs (cons lis1 lists))
	(if (null? cars) '()
	    (let recur ((cars cars) (cdrs cdrs))
	      (let ((vals (apply f cars)))
		(receive (cars2 cdrs2) (%cars+cdrs cdrs)
		  (if (null? cars2) vals
		      (appender vals (recur cars2 cdrs2))))))))

      ;; Fast path
      (if (null-list? lis1) '()
	  (let recur ((elt (car lis1)) (rest (cdr lis1)))
	    (let ((vals (f elt)))
	      (if (null-list? rest) vals
		  (appender vals (recur (car rest) (cdr rest)))))))))

(define (pair-for-each proc lis1 . lists)
  (check-arg procedure? proc)
  (if (pair? lists)

      (let lp ((lists (cons lis1 lists)))
	(let ((tails (%cdrs lists)))
	  (if (pair? tails)
	      (begin (apply proc lists)
		     (lp tails)))))

      ;; Fast path.
      (let lp ((lis lis1))
	(if (not (null-list? lis))
	    (let ((tail (cdr lis)))	; Grab the cdr now,
	      (proc lis)		; in case PROC SET-CDR!s LIS.
	      (lp tail))))))

;;; We stop when LIS1 runs out, not when any list runs out.
(define (map! f lis1 . lists)
  (check-arg procedure? f)
  (if (pair? lists)
      (let lp ((lis1 lis1) (lists lists))
	(if (not (null-list? lis1))
	    (receive (heads tails) (%cars+cdrs/no-test lists)
	      (set-car! lis1 (apply f (car lis1) heads))
	      (lp (cdr lis1) tails))))

      ;; Fast path.
      (pair-for-each (lambda (pair) (set-car! pair (f (car pair)))) lis1))
  lis1)


;;; Map F across L, and save up all the non-false results.
(define (filter-map f lis1 . lists)
  (check-arg procedure? f)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))
	(receive (cars cdrs) (%cars+cdrs lists)
	  (if (pair? cars)
	      (cond ((apply f cars) => (lambda (x) (cons x (recur cdrs))))
		    (else (recur cdrs))) ; Tail call in this arm.
	      '())))
	    
      ;; Fast path.
      (let recur ((lis lis1))
	(if (null-list? lis) lis
	    (let ((tail (recur (cdr lis))))
	      (cond ((f (car lis)) => (lambda (x) (cons x tail)))
		    (else tail)))))))

