;;;
;;; SRFI-1 - List processing library
;;;

;; $Id: srfi-1.scm,v 1.13 2002-10-26 09:02:40 shirok Exp $

;; This code is based on the reference implementation by Olin Shivers
;;
;; Copyright (c) 1998, 1999 by Olin Shivers. You may do as you please with
;; this code as long as you do not remove this copyright notice or
;; hold me liable for its use. Please send bug reports to shivers@ai.mit.edu.

;; [SK] I splitted the original source into several subfiles, which will
;; be autoloaded on demand.  It makes `(use srfi-1)' much lighter, and
;; you don't need to carry around the entire srfi-1 just to use fold().
;; I also tweaked some functions in order to use the native functions
;; as much as possible.
;; Also I added a few extra procedures that falls into the category of
;; list processing library.
;; You can obtain the original version from http://srfi.schemers.org

(define-module srfi-1
  (export xcons cons* make-list list-tabulate list-copy circular-list iota
          proper-list? circular-list? dotted-list? not-pair?
          null-list? list=
          first second third fourth fifth sixth seventh eighth
          ninth tenth car+cdr take drop take-right drop-right
          take! drop-right! split-at split-at! last
          length+ concatenate append! concatenate! reverse!
          append-reverse append-reverse!
          zip unzip1 unzip2 unzip3 unzip4 unzip5
          count fold unfold pair-fold reduce fold-right unfold-right
          count$ fold$ fold-right$ reduce$ reduce-right$
          pair-fold-right reduce-right append-map append-map!
          map! pair-for-each filter-map map-in-order
          filter partition remove filter! partition! remove!
          filter$ partition$ remove$
          member find find-tail any every list-index
          member$ find$ find-tail$ any$ every$
          take-while drop-while take-while! span break span! break!
          delete delete-duplicates delete! delete-duplicates!
          delete$
          assoc alist-cons alist-copy alist-delete alist-delete!
          assoc$
          lset lset= lset-adjoin lset-union lset-union!
          lset-intersection lset-intersection! lset-difference
          lset-difference! lset-xor lset-xor!
          lset-diff+intersection lset-diff+intersection!))
(select-module srfi-1)

(autoload "srfi-1/generator"  list-tabulate iota circular-list)
(autoload "srfi-1/pred"       proper-list? dotted-list? circular-list? length+)
(autoload "srfi-1/zipper"     zip unzip1 unzip2 unzip3 unzip4 unzip5)
(autoload "srfi-1/nth"        first second third fourth fifth sixth
                              seventh eighth ninth tenth)
(autoload "srfi-1/selector"   take drop take! take-right
                              drop-right drop-right!
                              split-at split-at! last)
(autoload "srfi-1/nary"       car+cdr %cdrs %cars+ %cars+cdrs %cars+cdrs+
                              %cars+cdrs/no-test)
(autoload "srfi-1/cat"        append! append-reverse append-reverse!
                              concatenate concatenate!)
(autoload "srfi-1/folder"     count unfold-right unfold fold fold-right
                              pair-fold-right pair-fold reduce reduce-right
                              count$ fold$ fold-right$ reduce$ reduce-right$)
(autoload "srfi-1/mapper"     append-map append-map! pair-for-each map!
                              filter-map)
(autoload "srfi-1/filter"     filter filter! partition partition!
                              remove remove!
                              filter$ partition$ remove$)
(autoload "srfi-1/finder"     find find-tail take-while drop-while span break
                              any every list-index
                              find$ find-tail$ any$ every$)
(autoload "srfi-1/set"        lset<= lset= lset-adjoin lset-union lset-union!
                              lset-intersection lset-intersection!
                              lset-difference lset-differnce!
                              lset-xor lset-xor!
                              lset-diff+intersection lset-diff+intersection!)

;; Gauche natives
;;   make-list list-copy reverse!

(define (xcons a b) (cons b a))
(define cons* list*)

(define (not-pair? x) (not (pair? x)))

(define (null-list? l)
  (cond ((pair? l) #f)
	((null? l) #t)
	(else (error "argument out of domain:" l))))

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

(define (delete$ x) (pa$ delete x))

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

(define (member$ x) (pa$ member x))

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

(define (assoc$ x) (pa$ assoc x))

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

(provide "srfi-1")
