;;;
;;; List generators of SRFI-1
;;;

;; $Id: generator.scm,v 1.2 2002-10-13 09:03:00 shirok Exp $

;; This code is based on the reference implementation by Olin Shivers
;;
;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;; this code as long as you do not remove this copyright notice or
;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.

(select-module srfi-1)

;;; Occasionally useful as a value to be passed to a fold or other
;;; higher-order procedure.
(define (xcons d a) (cons a d))

;;; Make a list of length LEN. Elt i is (PROC i) for 0 <= i < LEN.
(define (list-tabulate len proc)
  (check-arg (lambda (n) (and (integer? n) (>= n 0))) len)
  (do ((i (- len 1) (- i 1))
       (ans '() (cons (proc i) ans)))
      ((< i 0) ans)))

;;; IOTA count [start step]	(start start+step ... start+(count-1)*step)

(define (iota count . args)
  (if (< count 0) (error "Negative step count" iota count))
  (let ((start (if (pair? args) (car args) 0))
        (step  (if (and (pair? args) (pair? (cdr args))) (cadr args) 1)))
    (check-arg number? start)
    (check-arg number? step)
    (let ((last-val (+ start (* (- count 1) step))))
      (do ((count count (- count 1))
	   (val last-val (- val step))
	   (ans '() (cons val ans)))
	  ((<= count 0)  ans)))))
	  
(define (circular-list val1 . vals)
  (let ((ans (cons val1 vals)))
    (set-cdr! (last-pair ans) ans)
    ans))
