;;;
;;; SRFI-116 Immutable List Library
;;;

;; Gauche supports immutable pairs natively.  They work transparently
;; as mutable pairs, except set-car! and set-cdr!. 
;; Procedures that don't cons are just alias of builtin procedures.
;; Whenever a procedure need to cons, we make an ipair instead of an mpair.

;; Note that we won't reject mpair passed to these procedures.

(define-module srfi-116
  (use srfi-1)
  (use util.match)
  (export ipair                         ;builtin
          ilist                         ;builtin
          xipair ipair* make-ilist ilist-tabulate
          ilist-copy iiota

          ipair?                        ;builtin
          ;; NB: Other predicates don't distinguish mutable and immutable
          ;; pairs.
          (rename proper-list? proper-ilist?)
          (rename list? ilist?)
          (rename dotted-list? dotted-ilist?)
          (rename not-pair? not-ipair?)
          (rename null-list? null-ilist?)
          (rename list= ilist=)

          (rename car icar) (rename cdr icdr)
          (rename caar icaar) (rename cadr icadr)
          (rename cdar icdar) (rename cddr icddr)
          (rename caaar icaaar) (rename caadr icaadr)
          (rename cadar icadar) (rename caddr icaddr)
          (rename cdaar icdaar) (rename cdadr icdadr)
          (rename cddar icddar) (rename cdddr icdddr)
          (rename caaaar icaaaar) (rename caaadr icaaadr)
          (rename caadar icaadar) (rename caaddr icaaddr)
          (rename cadaar icadaar) (rename cadadr icadadr)
          (rename caddar icaddar) (rename cadddr icadddr)
          (rename cdaaar icdaaar) (rename cdaadr icdaadr)
          (rename cdadar icdadar) (rename cdaddr icdaddr)
          (rename cddaar icddaar) (rename cddadr icddadr)
          (rename cdddar icdddar) (rename cddddr icddddr)
          (rename list-ref ilist-ref)

          (rename first ifirst) (rename second isecond)
          (rename third ithird)
          (rename fourth ifourth)
          (rename fifth ififth)
          (rename sixth isixth)
          (rename seventh iseventh)
          (rename eighth ieighth)
          (rename ninth ininth)
          (rename tenth itenth)
          (rename car+cdr icar+cdr)
          itake
          (rename drop idrop)
          (rename list-tail ilist-tail)
          (rename take-right itake-right)
          idrop-right
          isplit-at
          (rename last ilast)
          (rename ast-ipair last-ipair)

          (rename length ilength)
          iappend
          iconcatenate
          ireverse
          iappend-reverse
          izip
          iunzip1
          iunzip2
          iunzip3
          iunzip4
          iunzip5
          (rename count icount)

          ;; imap
          ;; (rename for-each ifor-each)
          ;; (rename fold ifold)
          ;; iunfold
          ;; (rename pair-fold ipair-fold)
          ;; (rename reduce ireduce)
          ;; (rename fold-right ifold-right)
          ;; iunfold-right
          ;; (rename pair-fold-right ipair-fold-right)
          ;; (rename reduce-right ireduce-right)
          ;; iappend-map
          ;; (rename pair-for-each ipair-for-each)
          ;; ifilter-map
          ;; imap-in-order

          ;; ifilter
          ;; ipartition
          ;; iremove

          ;; (rename member imember)
          ;; (rename memq imemq)
          ;; (rename memv imemv)
          ;; (rename find ifind)
          ;; (rename find-tail ifind-tail)
          ;; (rename any iany)
          ;; (rename every ievery)
          ;; (rename list-index ilist-index)
          ;; itake-while
          ;; (rename drop-while idrop-while)
          ;; ispan
          ;; ibreak

          ;; idelete
          ;; idelete-duplicates

          ;; (rename assoc iassoc)
          ;; (rename assq iassq)
          ;; (rename assv iassv)
          ;; ialist-cons
          ;; ialist-delete

          ;; replace-icar
          ;; replace-icdr

          ;; pair->ipair
          ;; ipair->pair
          ;; list->ilist
          ;; ilist->list
          ;; tree->itree
          ;; itree->tree
          ;; gtree->itree
          ;; gtree->tree

          ;; (rename apply iapply)

          ;; ipair-comparator
          ;; ilist-comparator
          ;; make-ilist-comparator
          ;; make-improper-ilist-comparator
          ;; make-icar-comparator
          ;; mkae-icdr-comparator
          ))
(select-module srfi-116)

(define (xipair cd ca) (ipair ca cd))
(define (ipair* x . xs)
  (if (null? xs)
    x
    (ipair x (apply ipair* xs))))
(define (make-ilist n :optional (fill (undefined)))
  (let loop ([r '()] [n n])
    (if (<= n 0)
      r
      (loop (ipair fill r) (- n 1)))))
(define (ilist-tabulate n init-proc)
  (let loop ([r '()] [n (- n 1)])
    (if (<= n 0)
      r
      (loop (ipair (init-proc n) r) (- n 1)))))
(define (ilist-copy lis)
  (fold ipair '() (reverse lis)))

;; Almost identical code of iota in src/liblist.scm, but using ipair instead
;; of cons.
(define (iiota count :optional (start 0) (step 1))
  (unless (and (integer? count) (>= count 0))
    (error "count must be nonnegative integer: " count))
  (if (and (exact? start) (exact? step))
    ;; we allow inexact integer as 'count', for the consistency of
    ;; giota and liota in which we can also accept +inf.0 as count.
    (let1 count (exact count)
      (do ([c count (- c 1)]
           [v (+ start (* (- count 1) step)) (- v step)]
           [r '() (ipair v r)])
          [(<= c 0) r]))
    ;; for inexact numbers, we use multiplication to avoid error accumulation.
    (do ([c count (- c 1)]
         [r '() (ipair (+ start (*. (- c 1) step)) r)])
        [(<= c 0) r])))  

(define (itake lis i)
  (assume exact-integer? i)
  (if (<= i 0)
    '()
    (ipair (car lis) (itake (cdr lis) (- i 1)))))
(define (idrop-right lis i)
  (assume exact-integer? i)
  (let rec ([p0 (list-tail lis k)] [p1 lis])
    (if (pair? p0) (ipair (car p1) (rec (cdr p0) (cdr p1))) '())))
(define (isplit-at lis i)
  (assume exact-integer? i)
  (let rec ([lis lis] [i i])
    (if (<= i 0)
      (values '() lis)
      (receive (hd tl) (rec (cdr lis) (- i 1))
        (values (ipair (car lis) hd) tl)))))

(define (iappend . liss) (iconcatenate liss))

(define (iconcatenate liss)
  (match liss
    ([] '())
    ([lis] lis)
    ([lis1 . liss]
     (let1 tail (iconcatenate liss)
       (let rec ([lis1 lis1])
         (if (null? lis1)
           tail
           (ipair (car lis1) (rec (cdr lis1)))))))))

(define (ireverse lis) (fold ipair '() lis))

(define (iappend-reverse rev-head tail) (fold ipair tail rev-head))

(define (izip lis1 . liss) (apply imap ilist lis1 liss))

;; These are dupe of srfi-1.scm except map/cons replaced with imap/ipair
(define (iunzip1 lis) (imap car lis))

(define (iunzip2 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis)       ; Use NOT-PAIR? to handle
        (let ((elt (car lis)))                  ; dotted lists.
          (receive (a b) (recur (cdr lis))
            (values (ipair (car  elt) a)
                    (ipair (cadr elt) b)))))))

(define (iunzip3 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis lis)
        (let ((elt (car lis)))
          (receive (a b c) (recur (cdr lis))
            (values (ipair (car   elt) a)
                    (ipair (cadr  elt) b)
                    (ipair (caddr elt) c)))))))

(define (iunzip4 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis lis lis)
        (let ((elt (car lis)))
          (receive (a b c d) (recur (cdr lis))
            (values (ipair (car    elt) a)
                    (ipair (cadr   elt) b)
                    (ipair (caddr  elt) c)
                    (ipair (cadddr elt) d)))))))

(define (iunzip5 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis lis lis lis)
        (let ((elt (car lis)))
          (receive (a b c d e) (recur (cdr lis))
            (values (ipair (car     elt) a)
                    (ipair (cadr    elt) b)
                    (ipair (caddr   elt) c)
                    (ipair (cadddr  elt) d)
                    (ipair (car (cddddr  elt)) e)))))))


