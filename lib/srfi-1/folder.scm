;;;
;;; Folders of SRFI-1
;;;

;; $Id: folder.scm,v 1.3 2002-10-13 09:03:00 shirok Exp $

;; This code is based on the reference implementation by Olin Shivers
;;
;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;; this code as long as you do not remove this copyright notice or
;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.

(select-module srfi-1)

(define (count pred list1 . lists)
  (if (pair? lists)

      ;; N-ary case
      (let lp ((list1 list1) (lists lists) (i 0))
	(if (null-list? list1) i
	    (receive (as ds) (%cars+cdrs lists)
	      (if (null? as) i
		  (lp (cdr list1) ds
		      (if (apply pred (car list1) as) (+ i 1) i))))))

      ;; Fast path
      (let lp ((lis list1) (i 0))
	(if (null-list? lis) i
	    (lp (cdr lis) (if (pred (car lis)) (+ i 1) i))))))

(define (count$ pred) (pa$ count pred))

(define (unfold-right p f g seed . maybe-tail)
  (let lp ((seed seed) (ans (%optional maybe-tail '())))
    (if (p seed) ans
	(lp (g seed)
	    (cons (f seed) ans)))))


(define (unfold p f g seed . maybe-tail-gen)
  (if (pair? maybe-tail-gen)

      (let ((tail-gen (car maybe-tail-gen)))
	(if (pair? (cdr maybe-tail-gen))
	    (apply error "Too many arguments" unfold p f g seed maybe-tail-gen)

	    (let recur ((seed seed))
	      (if (p seed) (tail-gen seed)
		  (cons (f seed) (recur (g seed)))))))

      (let recur ((seed seed))
	(if (p seed) '()
	    (cons (f seed) (recur (g seed)))))))
      

(define (fold kons knil lis1 . lists)
  (if (pair? lists)
      (let lp ((lists (cons lis1 lists)) (ans knil))	; N-ary case
	(receive (cars+ans cdrs) (%cars+cdrs+ lists ans)
	  (if (null? cars+ans) ans ; Done.
	      (lp cdrs (apply kons cars+ans)))))
	    
      (let lp ((lis lis1) (ans knil))			; Fast path
	(if (null-list? lis) ans
	    (lp (cdr lis) (kons (car lis) ans))))))

(define (fold$ kons . maybe-knil)
  (lambda lists (apply fold kons (append maybe-knil lists))))

(define (fold-right kons knil lis1 . lists)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))		; N-ary case
	(let ((cdrs (%cdrs lists)))
	  (if (null? cdrs) knil
	      (apply kons (%cars+ lists (recur cdrs))))))

      (let recur ((lis lis1))				; Fast path
	(if (null-list? lis) knil
	    (let ((head (car lis)))
	      (kons head (recur (cdr lis))))))))

(define (fold-right$ kons . maybe-knil)
  (lambda lists (apply fold-right kons (append maybe-knil lists))))

(define (pair-fold-right f zero lis1 . lists)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists)))		; N-ary case
	(let ((cdrs (%cdrs lists)))
	  (if (null? cdrs) zero
	      (apply f (append! lists (list (recur cdrs)))))))

      (let recur ((lis lis1))				; Fast path
	(if (null-list? lis) zero (f lis (recur (cdr lis)))))))

(define (pair-fold f zero lis1 . lists)
  (if (pair? lists)
      (let lp ((lists (cons lis1 lists)) (ans zero))	; N-ary case
	(let ((tails (%cdrs lists)))
	  (if (null? tails) ans
	      (lp tails (apply f (append! lists (list ans)))))))

      (let lp ((lis lis1) (ans zero))
	(if (null-list? lis) ans
	    (let ((tail (cdr lis)))		; Grab the cdr now,
	      (lp tail (f lis ans)))))))	; in case F SET-CDR!s LIS.

      

;;; REDUCE and REDUCE-RIGHT only use RIDENTITY in the empty-list case.
;;; These cannot meaningfully be n-ary.

(define (reduce f ridentity lis)
  (if (null-list? lis) ridentity
      (fold f (car lis) (cdr lis))))

(define (reduce$ f . maybe-ridentity)
  (lambda args (apply reduce f (append maybe-ridentity args))))

(define (reduce-right f ridentity lis)
  (if (null-list? lis) ridentity
      (let recur ((head (car lis)) (lis (cdr lis)))
	(if (pair? lis)
	    (f head (recur (car lis) (cdr lis)))
	    head))))

(define (reduce-right$ f . maybe-ridentity)
  (lambda args (apply reduce-right f (append maybe-ridentity args))))
