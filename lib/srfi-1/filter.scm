;;;
;;; Filters of SRFI-1
;;;

;; $Id: filter.scm,v 1.3 2002-10-13 09:03:00 shirok Exp $

;; This code is based on the reference implementation by Olin Shivers
;;
;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;; this code as long as you do not remove this copyright notice or
;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.

;; NB: original SRFI-1 reference implementation uses non-tail recursive
;; calls a lot.  Unfortunately, Gauche is awkward to handle very deep
;; non-tail recursive calls, since the stack overflow handler is not
;; optimized well.  The simple tail-recursion and reverse!-ing the result
;; is much much faster.

(select-module srfi-1)

(define (filter pred lis)
  (let loop ((lis lis) (r '()))
    (cond
     ((not (pair? lis)) (reverse! r))
     ((pred (car lis))
      (loop (cdr lis) (cons (car lis) r)))
     (else
      (loop (cdr lis) r)))))

(define (filter$ pred) (pa$ filter pred))

(define (filter! pred lis)
  (let lp ((ans lis))
    (cond ((not (pair? ans)) ans)			; Scan looking for
	  ((not (pred (car ans))) (lp (cdr ans)))	; first cons of result.

	  ;; ANS is the eventual answer.
	  ;; SCAN-IN: (CDR PREV) = LIS and (CAR PREV) satisfies PRED.
	  ;;          Scan over a contiguous segment of the list that
	  ;;          satisfies PRED.
	  ;; SCAN-OUT: (CAR PREV) satisfies PRED. Scan over a contiguous
	  ;;           segment of the list that *doesn't* satisfy PRED.
	  ;;           When the segment ends, patch in a link from PREV
	  ;;           to the start of the next good segment, and jump to
	  ;;           SCAN-IN.
	  (else (letrec ((scan-in (lambda (prev lis)
				    (if (pair? lis)
					(if (pred (car lis))
					    (scan-in lis (cdr lis))
					    (scan-out prev (cdr lis))))))
			 (scan-out (lambda (prev lis)
				     (let lp ((lis lis))
				       (if (pair? lis)
					   (if (pred (car lis))
					       (begin (set-cdr! prev lis)
						      (scan-in lis (cdr lis)))
					       (lp (cdr lis)))
					   (set-cdr! prev lis))))))
		  (scan-in ans (cdr ans))
		  ans)))))

;; Avoid non-tail recursion.
(define (partition pred lis)
  (let recur ((lis lis)
              (in  '())
              (out '()))
    (if (not (pair? lis))
        (values (reverse! in) (reverse! out))
        (if (pred (car lis))
            (recur (cdr lis) (cons (car lis) in) out)
            (recur (cdr lis) in (cons (car lis) out))))))

(define (partition$ pred) (pa$ partition pred))

(define (partition! pred lis)
  (check-arg procedure? pred)
  (if (null-list? lis) (values lis lis)

      ;; This pair of loops zips down contiguous in & out runs of the
      ;; list, splicing the runs together. The invariants are
      ;;   SCAN-IN:  (cdr in-prev)  = LIS.
      ;;   SCAN-OUT: (cdr out-prev) = LIS.
      (letrec ((scan-in (lambda (in-prev out-prev lis)
			  (let lp ((in-prev in-prev) (lis lis))
			    (if (pair? lis)
				(if (pred (car lis))
				    (lp lis (cdr lis))
				    (begin (set-cdr! out-prev lis)
					   (scan-out in-prev lis (cdr lis))))
				(set-cdr! out-prev lis))))) ; Done.

	       (scan-out (lambda (in-prev out-prev lis)
			   (let lp ((out-prev out-prev) (lis lis))
			     (if (pair? lis)
				 (if (pred (car lis))
				     (begin (set-cdr! in-prev lis)
					    (scan-in lis out-prev (cdr lis)))
				     (lp lis (cdr lis)))
				 (set-cdr! in-prev lis)))))) ; Done.

	;; Crank up the scan&splice loops.
	(if (pred (car lis))
	    ;; LIS begins in-list. Search for out-list's first pair.
	    (let lp ((prev-l lis) (l (cdr lis)))
	      (cond ((not (pair? l)) (values lis l))
		    ((pred (car l)) (lp l (cdr l)))
		    (else (scan-out prev-l l (cdr l))
			  (values lis l))))	; Done.

	    ;; LIS begins out-list. Search for in-list's first pair.
	    (let lp ((prev-l lis) (l (cdr lis)))
	      (cond ((not (pair? l)) (values l lis))
		    ((pred (car l))
		     (scan-in l prev-l (cdr l))
		     (values l lis))		; Done.
		    (else (lp l (cdr l)))))))))

(define (remove! pred l) (filter! (lambda (x) (not (pred x))) l))
(define (remove  pred l) (filter  (lambda (x) (not (pred x))) l))

(define (remove$ pred) (pa$ remvoe pred))
