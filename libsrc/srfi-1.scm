;;;
;;; SRFI-1 - List processing library
;;;

;; $Id: srfi-1.scm,v 1.2 2007-08-10 08:48:06 shirok Exp $

;; This code is based on the reference implementation by Olin Shivers
;;
;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;; this code as long as you do not remove this copyright notice or
;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.

;; [SK] I added module stuff, deleted procedures that are supported
;; by Gauche core, and tweaked some functions that works well in Gauche.
;; You can obtain the original version from http://srfi.schemers.org
;; NB: Gauche 0.8.5 and before included some optional procedures (such
;; as fold$) in srfi-1 module.  As of 0.8.6, they're moved to gauche.procedure,
;; which is autoloaded.  The user doesn't need to see this change.

(define-module srfi-1
  (export xcons cons* list-tabulate circular-list iota
          not-pair?
          list=
          first second third fourth fifth sixth seventh eighth
          ninth tenth car+cdr take drop take-right drop-right
          take! drop-right! split-at! last
          concatenate concatenate!
          append-reverse append-reverse!
          zip unzip1 unzip2 unzip3 unzip4 unzip5
          count unfold pair-fold reduce unfold-right
          pair-fold-right reduce-right append-map append-map!
          map! pair-for-each filter-map map-in-order
          filter partition remove filter! partition! remove!
          member find-tail every list-index
          take-while drop-while take-while! span break span! break!
          delete delete-duplicates delete! delete-duplicates!
          assoc alist-cons alist-copy alist-delete alist-delete!
          lset<= lset= lset-adjoin lset-union lset-union!
          lset-intersection lset-intersection! lset-difference
          lset-difference! lset-xor lset-xor!
          lset-diff+intersection lset-diff+intersection!))
(select-module srfi-1)

(define (xcons a b) (cons b a))
(define cons* list*)

(define-inline (not-pair? x) (not (pair? x)))


;;;
;;; List generators of SRFI-1
;;;

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

;;;
;;; List predicates of SRFI-1
;;;

(define (list= = . lists)
  (or (null? lists) ; special case

      (let lp1 ((list-a (car lists))
                (others (cdr lists)))
	(or (null? others)
	    (let ((list-b (car others))
		  (others (cdr others)))
	      (if (eq? list-a list-b)	; EQ? => LIST=
                (lp1 list-b others)
                (let lp2 ((list-a list-a) (list-c list-b))
                  (if (null-list? list-a)
                    (and (null-list? list-c)
                         (lp1 list-b others))
                    (and (not (null-list? list-c))
                         (= (car list-a) (car list-c))
                         (lp2 (cdr list-a) (cdr list-c)))))))))))
			

;;;
;;; Zipper and unzipper of SRFI-1
;;;

(define (zip list1 . more-lists) (apply map list list1 more-lists))

(define (unzip1 lis) (map car lis))

(define (unzip2 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis)	; Use NOT-PAIR? to handle
	(let ((elt (car lis)))			; dotted lists.
	  (receive (a b) (recur (cdr lis))
	    (values (cons (car  elt) a)
		    (cons (cadr elt) b)))))))

(define (unzip3 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis lis)
	(let ((elt (car lis)))
	  (receive (a b c) (recur (cdr lis))
	    (values (cons (car   elt) a)
		    (cons (cadr  elt) b)
		    (cons (caddr elt) c)))))))

(define (unzip4 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis lis lis)
	(let ((elt (car lis)))
	  (receive (a b c d) (recur (cdr lis))
	    (values (cons (car    elt) a)
		    (cons (cadr   elt) b)
		    (cons (caddr  elt) c)
		    (cons (cadddr elt) d)))))))

(define (unzip5 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis lis lis lis)
	(let ((elt (car lis)))
	  (receive (a b c d e) (recur (cdr lis))
	    (values (cons (car     elt) a)
		    (cons (cadr    elt) b)
		    (cons (caddr   elt) c)
		    (cons (cadddr  elt) d)
		    (cons (car (cddddr  elt)) e)))))))

;;;
;;; N-th functions of SRFI-1
;;;

(define first  car)
(define second cadr)
(define third  caddr)
(define fourth cadddr)
(define (fifth   x) (car    (cddddr x)))
(define (sixth   x) (cadr   (cddddr x)))
(define (seventh x) (caddr  (cddddr x)))
(define (eighth  x) (cadddr (cddddr x)))
(define (ninth   x) (car  (cddddr (cddddr x))))
(define (tenth   x) (cadr (cddddr (cddddr x))))

;;;
;;; Selectors of SRFI-1
;;;
;;; take & drop

(define (take lis k)
  (check-arg integer? k)
  (let recur ((lis lis) (k k))
    (if (zero? k)
      '()
      (cons (car lis)
            (recur (cdr lis) (- k 1))))))

(define drop list-tail)  ; Gauche has list-tail

(define (take! lis k)
  (check-arg integer? k)
  (if (zero? k)
    '()
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

(define (split-at! x k)
  (check-arg integer? k)
  (if (zero? k)
    (values '() x)
    (let* ((prev (drop x (- k 1)))
           (suffix (cdr prev)))
      (set-cdr! prev '())
      (values x suffix))))

(define (last lis) (car (last-pair lis)))

;;;
;;; Utility functions for n-ary operation in SRFI-1
;;;

(define (car+cdr pair) (values (car pair) (cdr pair)))

;;; Return (map cdr lists). 
;;; However, if any element of LISTS is empty, just abort and return '().
(define (%cdrs lists)
  (call-with-current-continuation
    (lambda (abort)
      (let recur ((lists lists))
	(if (pair? lists)
	    (let ((lis (car lists)))
	      (if (null-list? lis) (abort '())
		  (cons (cdr lis) (recur (cdr lists)))))
	    '())))))

(define (%cars+ lists last-elt)	; (append! (map car lists) (list last-elt))
  (let recur ((lists lists))
    (if (pair? lists) (cons (caar lists) (recur (cdr lists))) (list last-elt))))

;;; LISTS is a (not very long) non-empty list of lists.
;;; Return two lists: the cars & the cdrs of the lists.
;;; However, if any of the lists is empty, just abort and return [() ()].

(define (%cars+cdrs lists)
  (call-with-current-continuation
   (lambda (abort)
     (let recur ((lists lists))
       (if (pair? lists)
         (receive (list other-lists) (car+cdr lists)
           (if (null-list? list) (abort '() '()) ; LIST is empty -- bail out
               (receive (a d) (car+cdr list)
                 (receive (cars cdrs) (recur other-lists)
                   (values (cons a cars) (cons d cdrs))))))
         (values '() '()))))))

;;; Like %CARS+CDRS, but we pass in a final elt tacked onto the end of the
;;; cars list. What a hack.
(define (%cars+cdrs+ lists cars-final)
  (call-with-current-continuation
   (lambda (abort)
     (let recur ((lists lists))
       (if (pair? lists)
         (receive (list other-lists) (car+cdr lists)
           (if (null-list? list) (abort '() '()) ; LIST is empty -- bail out
               (receive (a d) (car+cdr list)
                 (receive (cars cdrs) (recur other-lists)
                   (values (cons a cars) (cons d cdrs))))))
         (values (list cars-final) '()))))))

;;; Like %CARS+CDRS, but blow up if any list is empty.
(define (%cars+cdrs/no-test lists)
  (let recur ((lists lists))
    (if (pair? lists)
      (receive (list other-lists) (car+cdr lists)
        (receive (a d) (car+cdr list)
          (receive (cars cdrs) (recur other-lists)
            (values (cons a cars) (cons d cdrs)))))
      (values '() '()))))

;;;
;;; Concatenators of SRFI-1
;;;

(define (append-reverse rev-head tail)
  (let lp ((rev-head rev-head) (tail tail))
    (if (null-list? rev-head)
      tail
      (lp (cdr rev-head) (cons (car rev-head) tail)))))

(define (append-reverse! rev-head tail)
  (let lp ((rev-head rev-head) (tail tail))
    (if (null-list? rev-head)
      tail
      (let ((next-rev (cdr rev-head)))
        (set-cdr! rev-head tail)
        (lp next-rev rev-head)))))

(define (concatenate  lists) (reduce-right append  '() lists))
(define (concatenate! lists) (reduce-right append! '() lists))

;;;
;;; Folders of SRFI-1
;;;

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

(define (unfold-right p f g seed . maybe-tail)
  (let lp ((seed seed) (ans (get-optional maybe-tail '())))
    (if (p seed)
      ans
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

;; fold and fold-right are built-in      
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
  (if (null-list? lis)
    ridentity
    (fold f (car lis) (cdr lis))))

(define (reduce-right f ridentity lis)
  (if (null-list? lis)
    ridentity
    (let recur ((head (car lis)) (lis (cdr lis)))
      (if (pair? lis)
        (f head (recur (car lis) (cdr lis)))
        head))))

;;;
;;; Mapper of SRFI-1
;;;

(define (append-map f lis1 . lists)
  (really-append-map append-map  append  f lis1 lists))
(define (append-map! f lis1 . lists) 
  (really-append-map append-map! append! f lis1 lists))

(define (really-append-map who appender f lis1 lists)
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
  (if (pair? lists)
    (let recur ((lists (cons lis1 lists))
                (r '()))
      (receive (cars cdrs) (%cars+cdrs lists)
        (cond ((null-list? cars) (reverse! r))
              ((apply f cars) => (lambda (x) (recur cdrs (cons x r))))
              (else (recur cdrs r)))))
    
    ;; Fast path.
    (let recur ((lis lis1)
                (r   '()))
      (cond ((null-list? lis) (reverse! r))
            ((f (car lis)) => (lambda (x) (recur (cdr lis) (cons x r))))
            (else (recur (cdr lis) r))))))

;;;
;;; Filters of SRFI-1
;;;

(define (filter pred lis)
  (let loop ((lis lis) (r '()))
    (cond
     ((null-list? lis) (reverse! r))
     ((pred (car lis))
      (loop (cdr lis) (cons (car lis) r)))
     (else
      (loop (cdr lis) r)))))

(define (filter! pred lis)
  (let lp ((ans lis))
    (cond ((null-list? ans) ans)			; Scan looking for
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
    (if (null-list? lis)
      (values (reverse! in) (reverse! out))
      (if (pred (car lis))
        (recur (cdr lis) (cons (car lis) in) out)
        (recur (cdr lis) in (cons (car lis) out))))))

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
            (cond ((null-list? l) (values lis l))
                  ((pred (car l)) (lp l (cdr l)))
                  (else (scan-out prev-l l (cdr l))
                        (values lis l))))	; Done.

          ;; LIS begins out-list. Search for in-list's first pair.
          (let lp ((prev-l lis) (l (cdr lis)))
            (cond ((null-list? l) (values l lis))
                  ((pred (car l))
                   (scan-in l prev-l (cdr l))
                   (values l lis))		; Done.
                  (else (lp l (cdr l)))))))))

(define (remove! pred l) (filter! (lambda (x) (not (pred x))) l))
(define (remove  pred l) (filter  (lambda (x) (not (pred x))) l))

;;;
;;; Find and alike of SRFI-1
;;;

(define (find-tail pred list)
  (let lp ((list list))
    (and (not (null-list? list))
	 (if (pred (car list))
           list
           (lp (cdr list))))))

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

;;;
;;; Lists as sets of SRFI-1
;;;

;;; This is carefully tuned code; do not modify casually.
;;; - It is careful to share storage when possible;
;;; - Side-effecting code tries not to perform redundant writes.
;;; - It tries to avoid linear-time scans in special cases where constant-time
;;;   computations can be performed.
;;; - It relies on similar properties from the other list-lib procs it calls.
;;;   For example, it uses the fact that the implementations of MEMBER and
;;;   FILTER in this source code share longest common tails between args
;;;   and results to get structure sharing in the lset procedures.

(define (%lset2<= = lis1 lis2) (every (lambda (x) (member x lis2 =)) lis1))

(define (lset<= = . lists)
  (check-arg procedure? =)
  (or (not (pair? lists)) ; 0-ary case
      (let lp ((s1 (car lists)) (rest (cdr lists)))
	(or (not (pair? rest))
	    (let ((s2 (car rest))  (rest (cdr rest)))
	      (and (or (eq? s2 s1)	; Fast path
		       (%lset2<= = s1 s2)) ; Real test
		   (lp s2 rest)))))))

(define (lset= = . lists)
  (check-arg procedure? =)
  (or (not (pair? lists)) ; 0-ary case
      (let lp ((s1 (car lists)) (rest (cdr lists)))
	(or (not (pair? rest))
	    (let ((s2   (car rest))
		  (rest (cdr rest)))
	      (and (or (eq? s1 s2)	; Fast path
		       (and (%lset2<= = s1 s2) (%lset2<= = s2 s1))) ; Real test
		   (lp s2 rest)))))))


(define (lset-adjoin = lis . elts)
  (check-arg procedure? =)
  (fold (lambda (elt ans) (if (member elt ans =) ans (cons elt ans)))
	lis elts))


(define (lset-union = . lists)
  (check-arg procedure? =)
  (reduce (lambda (lis ans)		; Compute ANS + LIS.
	    (cond ((null? lis) ans)	; Don't copy any lists
		  ((null? ans) lis) 	; if we don't have to.
		  ((eq? lis ans) ans)
		  (else
		   (fold (lambda (elt ans) (if (any (lambda (x) (= x elt)) ans)
                                             ans
                                             (cons elt ans)))
			 ans lis))))
	  '() lists))

(define (lset-union! = . lists)
  (check-arg procedure? =)
  (reduce (lambda (lis ans)		; Splice new elts of LIS onto the front of ANS.
	    (cond ((null? lis) ans)	; Don't copy any lists
		  ((null? ans) lis) 	; if we don't have to.
		  ((eq? lis ans) ans)
		  (else
		   (pair-fold (lambda (pair ans)
				(let ((elt (car pair)))
				  (if (any (lambda (x) (= x elt)) ans)
                                    ans
                                    (begin (set-cdr! pair ans) pair))))
			      ans lis))))
	  '() lists))


(define (lset-intersection = lis1 . lists)
  (check-arg procedure? =)
  (let ((lists (delete lis1 lists eq?))) ; Throw out any LIS1 vals.
    (cond ((any null-list? lists) '())		; Short cut
	  ((null? lists)          lis1)		; Short cut
	  (else (filter (lambda (x)
			  (every (lambda (lis) (member x lis =)) lists))
			lis1)))))

(define (lset-intersection! = lis1 . lists)
  (check-arg procedure? =)
  (let ((lists (delete lis1 lists eq?))) ; Throw out any LIS1 vals.
    (cond ((any null-list? lists) '())		; Short cut
	  ((null? lists)          lis1)		; Short cut
	  (else (filter! (lambda (x)
			   (every (lambda (lis) (member x lis =)) lists))
			 lis1)))))


(define (lset-difference = lis1 . lists)
  (check-arg procedure? =)
  (let ((lists (filter pair? lists)))	; Throw out empty lists.
    (cond ((null? lists)     lis1)	; Short cut
	  ((memq lis1 lists) '())	; Short cut
	  (else (filter (lambda (x)
			  (every (lambda (lis) (not (member x lis =)))
				 lists))
			lis1)))))

(define (lset-difference! = lis1 . lists)
  (check-arg procedure? =)
  (let ((lists (filter pair? lists)))	; Throw out empty lists.
    (cond ((null? lists)     lis1)	; Short cut
	  ((memq lis1 lists) '())	; Short cut
	  (else (filter! (lambda (x)
			   (every (lambda (lis) (not (member x lis =)))
				  lists))
			 lis1)))))


(define (lset-xor = . lists)
  (check-arg procedure? =)
  (reduce (lambda (b a)			; Compute A xor B:
	    ;; Note that this code relies on the constant-time
	    ;; short-cuts provided by LSET-DIFF+INTERSECTION,
	    ;; LSET-DIFFERENCE & APPEND to provide constant-time short
	    ;; cuts for the cases A = (), B = (), and A eq? B. It takes
	    ;; a careful case analysis to see it, but it's carefully
	    ;; built in.

	    ;; Compute a-b and a^b, then compute b-(a^b) and
	    ;; cons it onto the front of a-b.
	    (receive (a-b a-int-b)   (lset-diff+intersection = a b)
	      (cond ((null? a-b)     (lset-difference = b a))
		    ((null? a-int-b) (append b a))
		    (else (fold (lambda (xb ans)
				  (if (member xb a-int-b =) ans (cons xb ans)))
				a-b
				b)))))
	  '() lists))


(define (lset-xor! = . lists)
  (check-arg procedure? =)
  (reduce (lambda (b a)			; Compute A xor B:
	    ;; Note that this code relies on the constant-time
	    ;; short-cuts provided by LSET-DIFF+INTERSECTION,
	    ;; LSET-DIFFERENCE & APPEND to provide constant-time short
	    ;; cuts for the cases A = (), B = (), and A eq? B. It takes
	    ;; a careful case analysis to see it, but it's carefully
	    ;; built in.

	    ;; Compute a-b and a^b, then compute b-(a^b) and
	    ;; cons it onto the front of a-b.
	    (receive (a-b a-int-b)   (lset-diff+intersection! = a b)
	      (cond ((null? a-b)     (lset-difference! = b a))
		    ((null? a-int-b) (append! b a))
		    (else (pair-fold (lambda (b-pair ans)
				       (if (member (car b-pair) a-int-b =) ans
					   (begin (set-cdr! b-pair ans) b-pair)))
				     a-b
				     b)))))
	  '() lists))


(define (lset-diff+intersection = lis1 . lists)
  (check-arg procedure? =)
  (cond ((every null-list? lists) (values lis1 '()))	; Short cut
	((memq lis1 lists)        (values '() lis1))	; Short cut
	(else (partition (lambda (elt)
			   (not (any (lambda (lis) (member elt lis =))
				     lists)))
			 lis1))))

(define (lset-diff+intersection! = lis1 . lists)
  (check-arg procedure? =)
  (cond ((every null-list? lists) (values lis1 '()))	; Short cut
	((memq lis1 lists)        (values '() lis1))	; Short cut
	(else (partition! (lambda (elt)
			    (not (any (lambda (lis) (member elt lis =))
				      lists)))
			  lis1))))

(define map-in-order map) ; Gauche's map is already in order

;; In the common case, these procs uses Gauche native, even not loading
;; the generic filter routine.

(define-syntax %case-by-cmp
  (syntax-rules ()
    ((_ args = eq-case eqv-case equal-case default-case)
     (let ((= (if (pair? args) (car args) equal?)))
       (cond ((eq? = eq?)    eq-case)
             ((eq? = eqv?)   eqv-case)
             ((eq? = equal?) equal-case)
             (else default-case))))))

(define (delete x lis . args)
  (%case-by-cmp args =
                (%delete x lis 'eq?)
                (%delete x lis 'eqv?)
                (%delete x lis 'equal?)
                (filter (lambda (y) (not (= x y))) lis)))

(define (delete! x lis . args)
  (%case-by-cmp args =
                (%delete! x lis 'eq?)
                (%delete! x lis 'eqv?)
                (%delete! x lis 'equal?)
                (filter! (lambda (y) (not (= x y))) lis)))

;;; Extended from R4RS to take an optional comparison argument.
(define (member x lis . args)
  (let ((%member (with-module scheme member))) ;save original func
    (%case-by-cmp args =
                  (memq x lis)
                  (memv x lis)
                  (%member x lis)
                  (find-tail (lambda (y) (= x y)) lis))))

(define (delete-duplicates lis . args)
  (%case-by-cmp args =
                (%delete-duplicates lis 'eq?)
                (%delete-duplicates lis 'eqv?)
                (%delete-duplicates lis 'equal?)
                (let recur ((lis lis))
                  (if (null-list? lis) lis
                      (let* ((x (car lis))
                             (tail (cdr lis))
                             (new-tail (recur (delete x tail =))))
                        (if (eq? tail new-tail) lis (cons x new-tail)))))))

(define (delete-duplicates! lis . args)
  (%case-by-cmp args =
                (%delete-duplicates! lis 'eq?)
                (%delete-duplicates! lis 'eqv?)
                (%delete-duplicates! lis 'equal?)
                (let recur ((lis lis))
                  (if (null-list? lis) lis
                      (let* ((x (car lis))
                             (tail (cdr lis))
                             (new-tail (recur (delete! x tail =))))
                        (if (eq? tail new-tail) lis (cons x new-tail)))))))

;;; Extended from R4RS to take an optional comparison argument.
(define (assoc x lis . args)
  (let ((%assoc (with-module scheme assoc)))
    (%case-by-cmp args =
                  (assq x lis)
                  (assv x lis)
                  (%assoc x lis)
                  (find (lambda (entry) (= x (car entry))) lis))))

(define alist-cons acons)

(define (alist-copy alist)
  (map (lambda (elt) (cons (car elt) (cdr elt)))
       alist))

(define (alist-delete key alist . args)
  (%case-by-cmp args =
                (%alist-delete key alist 'eq?)
                (%alist-delete key alist 'eqv?)
                (%alist-delete key alist 'equal?)
                (filter (lambda (elt) (not (= key (car elt)))) alist)))

(define (alist-delete! key alist . args)
  (%case-by-cmp args =
                (%alist-delete! key alist 'eq?)
                (%alist-delete! key alist 'eqv?)
                (%alist-delete! key alist 'equal?)
                (filter! (lambda (elt) (not (= key (car elt)))) alist)))

