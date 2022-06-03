;;;
;;; scheme.ilist - immutable lists (R7RS Red)
;;;
;;;   Copyright (c) 2017-2022  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;; Originally srfi-116

;; Gauche supports immutable pairs natively.  They work transparently
;; as mutable pairs, except set-car! and set-cdr!.
;; Procedures that don't cons are just alias of builtin procedures.
;; Whenever a procedure need to cons, we make an ipair instead of an mpair.

;; Note that we won't reject mpair passed to these procedures.

(define-module scheme.ilist
  (use scheme.list)
  (use srfi-114 :only (make-car-comparator
                       make-cdr-comparator
                       make-pair-comparator
                       make-improper-list-comparator))
  (use util.match)
  (export ipair                         ;builtin
          ilist                         ;builtin
          xipair ipair* make-ilist ilist-tabulate
          ilist-copy iiota

          iq

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
          (rename car+cdr icar+icdr)
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
          itake
          (rename drop idrop)
          (rename list-tail ilist-tail)
          (rename take-right itake-right)
          idrop-right
          isplit-at
          (rename last ilast)
          (rename last-pair last-ipair)

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

          imap
          (rename for-each ifor-each)
          (rename fold ifold)
          iunfold
          (rename pair-fold ipair-fold)
          (rename reduce ireduce)
          (rename fold-right ifold-right)
          iunfold-right
          (rename pair-fold-right ipair-fold-right)
          (rename reduce-right ireduce-right)
          iappend-map
          (rename pair-for-each ipair-for-each)
          ifilter-map
          imap-in-order

          ifilter
          ipartition
          iremove

          (rename member imember)
          (rename memq imemq)
          (rename memv imemv)
          ifind
          (rename find-tail ifind-tail)
          (rename any iany)
          (rename every ievery)
          (rename list-index ilist-index)
          itake-while
          (rename drop-while idrop-while)
          ispan
          ibreak

          idelete
          idelete-duplicates

          (rename assoc iassoc)
          (rename assq iassq)
          (rename assv iassv)
          ialist-cons
          ialist-delete

          replace-icar
          replace-icdr

          pair->ipair
          ipair->pair
          list->ilist
          ilist->list
          tree->itree
          itree->tree
          gtree->itree
          gtree->tree

          (rename apply iapply)

          ipair-comparator
          ilist-comparator
          (rename make-list-comparator make-ilist-comparator)
          (rename make-improper-list-comparator make-improper-ilist-comparator)
          (rename make-car-comparator make-icar-comparator)
          (rename make-cdr-comparator make-icdr-comparator)
          (rename make-pair-comparator make-ipair-comparator)
          ))
(select-module scheme.ilist)

(define-syntax iq
  (syntax-rules ()
    [(iq x ...)
     (gtree->itree '(x ...))]))

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
    (if (< n 0)
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
  (let rec ([p0 (list-tail lis i)] [p1 lis])
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


(define imap
  (case-lambda
    ([proc lis] (fold-right (^[x ys] (ipair (proc x) ys)) '() lis))
    ([proc lis . liss]
     (ireverse (apply fold-left (^[ys . xs] (cons (apply proc xs) ys)) '()
                      lis liss)))))

(define (iunfold p f g seed :optional (tail-gen (^_ '())))
  (let rec ((seed seed))
    (if (p seed)
      (tail-gen seed)
      (ipair (f seed) (rec (g seed))))))

(define (iunfold-right p f g seed :optional (ans '()))
  (let loop ((seed seed) (ans ans))
    (if (p seed)
      ans
      (loop (g seed)
            (ipair (f seed) ans)))))

(define (iappend-map proc lis . lists)
  (iconcatenate (apply map proc lis lists)))

(define (ifilter-map proc lis . lists)
  (if (null? lists)
    (let loop ([lis lis] [r '()])
      (cond [(null-list? lis) (ireverse r)]
            [(proc (car lis)) => (^x (loop (cdr lis) (cons x r)))]
            [else (loop (cdr lis) r)]))
    (let loop ([liss (cons lis lists)] [r '()])
      (receive (cars cdrs)
          ((with-module gauche.internal %zip-nary-args) liss)
        (cond [(not cars) (ireverse r)]
              [(apply proc cars) => (^x (loop cdrs (cons x r)))]
              [else (loop cdrs r)])))))

(define imap-in-order
  (case-lambda
    ([proc lis] (ireverse (fold (^[x ys] (cons (proc x) ys)) '() lis)))
    ([proc lis . liss]
     (ireverse (apply fold-left (^[ys . xs] (cons (apply proc xs) ys)) '()
                      lis liss)))))

(define (ifilter pred lis)
  (let loop ([lis lis] [r '()])
    (cond [(null-list? lis) (ireverse r)]
          [(pred (car lis)) (loop (cdr lis) (cons (car lis) r))]
          [else (loop (cdr lis) r)])))

(define (iremove  pred l) (ifilter  (^x (not (pred x))) l))

;;built-in find tolerate improper lists.  we're bit more strict here.
(define (ifind pred lis)
  (cond [(null? lis) #f]
        [(not (pair? lis)) (error "pair expected, but got:" lis)]
        [(pred (car lis)) (car lis)]
        [else (ifind pred (cdr lis))]))

(define (ipartition pred lis)
  (let rec ([lis lis] [xs '()] [ys '()])
    (if (null-list? lis)
      (values (ireverse xs) (ireverse ys))
      (if (pred (car lis))
        (rec (cdr lis) (cons (car lis) xs) ys)
        (rec (cdr lis) xs (cons (car lis) ys))))))

(define (itake-while pred lis)
  (cond [(null? lis) '()]
        [(pred (car lis)) (ipair (car lis) (itake-while pred (cdr lis)))]
        [else '()]))

(define (ispan pred lis)
  (cond [(null? lis) '()]
        [(pred (car lis))
         (receive (pre post) (ispan pred (cdr lis))
           (values (ipair (car lis) pre) post))]
        [else (values '() lis)]))

(define (ibreak pred lis) (ispan (complement pred) lis))

(define (idelete x lis :optional (eq equal?))
  (if (null? lis)
    '()
    (let1 tail (idelete x (cdr lis) eq)
      (cond [(eq x (car lis)) tail]
            [(eq? (cdr lis) tail) lis]
            [else (ipair (car lis) tail)]))))

(define (idelete-duplicates lis :optional (eq equal?))
  (cond [(null? lis) lis]
        [(null? (cdr lis)) lis]
        [else (let1 tail (idelete (car lis)
                                  (idelete-duplicates (cdr lis) eq)
                                  eq)
                (if (eq? tail (cdr lis))
                  lis
                  (ipair (car lis) tail)))]))

(define (ialist-cons k d alis)
  (ipair (ipair k d) alis))

(define (ialist-delete k alis :optional (eq equal?))
  (cond [(null? alis) '()]
        [(eq k (caar alis)) (ialist-delete k (cdr alis) eq)]
        [else (let1 tail (ialist-delete k (cdr alis) eq)
                (if (eq? tail (cdr alis))
                  alis
                  (ipair (car alis) tail)))]))

(define (replace-icar p obj) (ipair obj (cdr p)))
(define (replace-icdr p obj) (ipair (car p) obj))

(define (pair->ipair p) (ipair (car p) (cdr p)))
(define (ipair->pair p) (cons (car p) (cdr p)))

(define (list->ilist p)
  (cond [(null? p) '()]
        [(ipair? p) (let1 tail (list->ilist (cdr p))
                      (if (eq? (cdr p) tail)
                        p
                        (ipair (car p) tail)))]
        [(pair? p) (ipair (car p) (list->ilist (cdr p)))]
        [else p]))
(define (ilist->list p)
  (cond [(null? p) '()]
        [(pair? p) (let1 tail (ilist->list (cdr p))
                      (if (eq? (cdr p) tail)
                        p
                        (cons (car p) tail)))]
        [(ipair? p) (cons (car p) (ilist->list (cdr p)))]
        [else p]))

(define (tree->itree p)
  (cond [(ipair? p)
         (let ([ca (tree->itree (car p))]
               [cd (tree->itree (cdr p))])
           (if (and (eq? ca (car p)) (eq? cd (cdr p)))
             p
             (ipair ca cd)))]
        [(pair? p)
         (ipair (tree->itree (car p)) (tree->itree (cdr p)))]
        [else p]))
(define (itree->tree p)
  (cond [(pair? p)
         (let ([ca (itree->tree (car p))]
               [cd (itree->tree (cdr p))])
           (if (and (eq? ca (car p)) (eq? cd (cdr p)))
             p
             (cons ca cd)))]
        [(ipair? p)
         (cons (itree->tree (car p)) (itree->tree (cdr p)))]
        [else p]))

;; Our itree->tree and tree->itree accepts mixed trees, so these are the same.
(define (gtree->itree obj) (tree->itree obj))
(define (gtree->tree obj)  (itree->tree obj))


(define ipair-comparator
  (make-comparator/compare ipair? #t compare default-hash 'ipair-comparator))
(define ilist-comparator
  (make-comparator/compare list? #t compare default-hash 'ilist-comparator))
