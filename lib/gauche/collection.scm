;;;
;;; collection.scm - collection generics
;;;
;;;  Copyright(C) 2001-2002 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: collection.scm,v 1.11 2003-01-06 03:19:33 shirok Exp $
;;;

;; Defines generic operations over collection.   A collection is
;; a set of objects, possibly containing infinite objects.

(define-module gauche.collection
  (use srfi-1)
  (use util.queue)
  (export call-with-iterator with-iterator call-with-iterators
          call-with-builder  with-builder
          fold map map-to for-each fold$ map$ for-each$
          find filter filter-to remove remove-to partition partition-to
          size-of lazy-size-of coerce-to)
  )
(select-module gauche.collection)

;;-------------------------------------------------
;; Call-with-iterator - the fundamental iterator
;;

(define-syntax with-iterator 
  (syntax-rules ()
    ((_ (coll end? next . opts) . body)
     (call-with-iterator coll (lambda (end? next) . body) . opts))))

(define-method call-with-iterator ((coll <list>) proc . args)
  (let* ((start (get-keyword :start args #f))
         (p (if start (list-tail coll start) coll)))
    (proc (lambda () (null? p))
          (lambda () (pop! p)))))

(define-method call-with-iterator ((coll <vector>) proc . args)
  (let ((len (vector-length coll))
        (i   (get-keyword :start args 0)))
    (proc (lambda () (>= i len))
          (lambda () (begin0 (vector-ref coll i) (inc! i))))))

(define-method call-with-iterator ((coll <weak-vector>) proc . args)
  (let ((len (weak-vector-length coll))
        (i   (get-keyword :start args 0)))
    (proc (lambda () (>= i len))
          (lambda () (begin0 (weak-vector-ref coll i) (inc! i))))))

(define-method call-with-iterator ((coll <string>) proc . args)
  (let* ((start (get-keyword :start args #f))
         (s     (open-input-string (if start (string-copy coll start) coll)))
         (ch    (read-char s)))
    (proc (lambda () (eof-object? ch))
          (lambda () (let ((c ch))
                       (set! ch (read-char s))
                       c)))))

(define-method call-with-iterator ((coll <hash-table>) proc . args)
  (let* ((iter (%hash-table-iter coll))
         (kv   (call-with-values iter cons)))
    (proc (lambda () (eof-object? (car kv)))
          (lambda () (let ((r kv))
                       (set! kv (call-with-values iter cons))
                       r)))))

;; n-ary case aux. proc
(define (call-with-iterators colls proc)
  (let loop ((colls colls)
             (eprocs '())
             (nprocs '()))
    (if (null? colls)
        (proc (reverse eprocs) (reverse nprocs))
        (with-iterator ((car colls) end? next)
          (loop (cdr colls) (cons end? eprocs) (cons next nprocs))))))

;;-------------------------------------------------
;; Call-with-builder - the fundamental constructor
;;

(define-syntax with-builder
  (syntax-rules ()
    ((_ (class add! get . opts) . body)
     (call-with-builder class (lambda (add! get) . body) . opts))))

(define-method call-with-builder ((class <list-meta>) proc . args)
  (let ((q (make-queue)))
    (proc (lambda (item) (enqueue! q item))
          (lambda () (dequeue-all! q)))))

(define-method call-with-builder ((class <vector-meta>) proc . args)
  (let ((size (get-keyword :size args #f)))
    (if size
        (let ((v (make-vector size))
              (i 0))
          (proc (lambda (item)
                  (when (< i size)
                    (vector-set! v i item)
                    (inc! i)))
                (lambda () v)))
        (let ((q (make-queue)))
          (proc (lambda (item) (enqueue! q item))
                (lambda () (list->vector (dequeue-all! q))))))))

(define-method call-with-builder ((class <weak-vector-meta>) proc . args)
  (let ((size (get-keyword :size args #f)))
    (if size
        (let ((v (make-weak-vector size))
              (i 0))
          (proc (lambda (item)
                  (when (< i size)
                    (weak-vector-set! v i item)
                    (inc! i)))
                (lambda () v)))
        (let ((q (make-queue))
              (cnt 0))
          (proc (lambda (item) (enqueue! q item) (inc! cnt))
                (lambda ()
                  (let ((v (make-weak-vector cnt)))
                    (do ((i 0 (+ i 1)))
                        ((= i cnt) v)
                      (weak-vector-set! v i (dequeue! q)))))))
        )))

(define-method call-with-builder ((class <string-meta>) proc . args)
  (let ((s (open-output-string)))
    (proc (lambda (item)
            (unless (char? item)
              (error "character required to build a string, but got" item))
            (write-char item s))
          (lambda () (get-output-string s)))))

(define-method call-with-builder ((class <hash-table-meta>) proc . args)
  (let* ((type (get-keyword :type args 'eq?))
         (h    (make-hash-table type)))
    (proc (lambda (item)
            (unless (pair? item)
              (error "pair required to build a hashtable, but got" item))
            (hash-table-put! h (car item) (cdr item)))
          (lambda () h))))

;;----------------------------------------------------
;; Derived operations
;;

;; fold -------------------------------------------------

;; generic way.   This effectively shadows SRFI-1 fold.
(define-method fold (proc knil (coll <collection>) . more)
  (if (null? more)
      (with-iterator (coll end? next)
        (do ((r knil (proc (next) r)))
            ((end?) r)
          #f))
      (call-with-iterators
       (cons coll more)
       (lambda (ends? nexts)
         (do ((r knil (apply proc (fold-right (lambda (p r) (cons (p) r))
                                              (list r)
                                              nexts))))
             ((any (lambda (p) (p)) ends?) r)
           #f)))))

;; for list arguments, SRFI-1 implementation is slightly faster.
(define-method fold (proc knil (coll <list>))
  ((with-module srfi-1 fold) proc knil coll))

(define-method fold (proc knil (coll <list>) (coll2 <list>))
  ((with-module srfi-1 fold) proc knil coll coll2))

;; partial applied version
(define-method fold$ (proc)
  (lambda (knil . lists) (apply fold proc knil lists)))
(define-method fold$ (proc knil)
  (lambda lists (apply fold proc knil lists)))

;; map --------------------------------------------------

;; generic way.  this shadows builtin map.
(define-method map (proc (coll <collection>) . more)
  (if (null? more)
      (with-iterator (coll end? next)
        (do ((q (make-queue)))
            ((end?) (dequeue-all! q))
          (enqueue! q (proc (next)))))
      (let ((%map (with-module gauche map)))
        (call-with-iterators
         (cons coll more)
         (lambda (ends? nexts)
           (do ((q (make-queue)))
               ((any (lambda (p) (p)) ends?)
                (dequeue-all! q))
             (enqueue! q (apply proc (%map (lambda (p) (p)) nexts))))))
        )))

;; for list arguments, built-in map is much faster.
(define-method map (proc (coll <list>) . more)
  (let ((%map (with-module gauche map)))
    (if (null? more)
        (%map proc coll)
        (if (every pair? more)
            (apply %map proc coll more)
            (next-method)))))

;; redefine map$ to use generic version of map
(define (map$ proc) (pa$ map proc))

;; map-to -----------------------------------------------

;; generic way.
(define-method map-to ((class <class>) proc (coll <collection>) . more)
  (if (null? more)
      (with-builder (class add! get :size (size-of coll))
        (with-iterator (coll end? next)
          (do ()
              ((end?) (get))
            (add! (proc (next))))))
      (with-builder (class add! get :size (size-of coll))
        (call-with-iterators
         (cons coll more)
         (lambda (ends? nexts)
           (do ()
               ((any (lambda (p) (p)) ends?) (get))
             (add! (apply proc (map (lambda (p) (p)) nexts)))))))))

;; map-to <list> is equivalent to map.
(define-method map-to ((class <list-meta>) proc coll . more)
  (apply map proc coll more))


;; for-each ---------------------------------------------

;; generic way.  this shadows builtin for-each.
(define-method for-each (proc (coll <collection>) . more)
  (if (null? more)
      (with-iterator (coll end? next)
        (until (end?) (proc (next))))
      (let ((%map (with-module gauche map)))
        (call-with-iterators
         (cons coll more)
         (lambda (ends? nexts)
           (until (any (lambda (p) (p)) ends?)
             (apply proc (%map (lambda (p) (p)) nexts))))))))

;; for list arguments, built-in for-each is much faster.
(define-method for-each (proc (coll <list>) . more)
  (let ((%for-each (with-module gauche for-each)))
    (if (null? more)
        (%for-each proc coll)
        (if (every pair? more)
            (apply %for-each proc coll more)
            (next-method)))))

;; redefine for-each$ to use generic version of for-each
(define (for-each$ proc) (pa$ for-each proc))

;; size-of ----------------------------------------------

;; generic way
(define-method size-of ((coll <collection>))
  (fold (lambda (e r) (+ r 1)) 0 coll))

(define-method lazy-size-of ((coll <collection>))
  (delay (size-of coll)))

;; shortcut
(define-method size-of ((coll <list>))        (length coll))
(define-method size-of ((coll <vector>))      (vector-length coll))
(define-method size-of ((coll <weak-vector>)) (weak-vector-length coll))
(define-method size-of ((coll <string>))      (string-length coll))

(define-method lazy-size-of ((coll <list>))        (length coll))
(define-method lazy-size-of ((coll <vector>))      (vector-length coll))
(define-method lazy-size-of ((coll <weak-vector>)) (weak-vector-length coll))
(define-method lazy-size-of ((coll <string>))      (string-length coll))

;; find -------------------------------------------------

;; generic way
(define-method find (pred (coll <collection>))
  (with-iterator (coll end? next)
    (let loop ()
      (if (end?)
          #f
          (let ((e (next)))
            (if (pred e) e (loop)))))))

;; shortcut
(define-method find (pred (coll <list>))
  ((with-module srfi-1 find) pred coll))

;; filter -----------------------------------------------

;; generic way
(define-method filter (pred (coll <collection>))
  (let ((q (make-queue)))
    (with-iterator (coll end? next)
      (until (end?) (let ((e (next))) (when (pred e) (enqueue! q e))))
      (dequeue-all! q))))

(define-method filter-to ((class <class>) pred (coll <collection>))
  (with-builder (class add! get)
    (with-iterator (coll end? next)
      (do ()
          ((end?) (get))
        (let ((e (next))) (when (pred e) (add! e)))))))

;; shortcut
(define-method filter (pred (coll <list>))
  ((with-module srfi-1 filter) pred coll))

(define-method filter-to ((class <list-meta>) pred coll)
  (filter pred coll))

;; remove -----------------------------------------------

;; generic way
(define-method remove (pred (coll <collection>))
  (let ((q (make-queue)))
    (with-iterator (coll end? next)
      (until (end?) (let ((e (next))) (unless (pred e) (enqueue! q e))))
      (dequeue-all! q))))

(define-method remove-to ((class <class>) pred (coll <collection>))
  (with-builder (class add! get)
    (with-iterator (coll end? next)
      (do ()
          ((end?) (get))
        (let ((e (next))) (unless (pred e) (add! e)))))))

;; shortcut
(define-method remove (pred (coll <list>))
  ((with-module srfi-1 remove) pred coll))

(define-method remove-to ((class <list-meta>) pred coll)
  (remove pred coll))

;; partition ---------------------------------------------

;; generic way
(define-method partition (pred (coll <collection>))
  (with-iterator (coll end? next)
    (let loop ((y '()) (n '()))
      (if (end?)
          (values (reverse y) (reverse n))
          (let ((e (next)))
            (if (pred e)
                (loop (cons e y) n)
                (loop y (cons e n))))))))

(define-method partition-to ((class <class>) pred (coll <collection>))
  (with-builder (class add0! get0)
    (with-builder (class add1! get1)
      (with-iterator (coll end? next)
        (do ()
            ((end?) (values (get0) (get1)))
          (let ((e (next)))
            (if (pred e) (add0! e) (add1! e))))))))

;; shortcut
(define-method partition (pred (coll <list>))
  ((with-module srfi-1 partition) pred coll))

(define-method partition-to ((class <list-meta>) pred coll)
  (partition pred coll))

;; coercion ---------------------------------------------

(define-method coerce-to ((class <class>) (coll <collection>))
  (with-builder (class add! get :size (size-of coll))
    (with-iterator (coll end? next)
      (do ()
          ((end?) (get))
        (add! (next))))))

;; shortcut
(define-method coerce-to ((class <list-meta>) (coll <list>))
  (list-copy coll))
(define-method coerce-to ((class <list-meta>) (coll <vector>))
  (vector->list coll))
(define-method coerce-to ((class <list-meta>) (coll <string>))
  (string->list coll))
(define-method coerce-to ((class <vector-meta>) (coll <list>))
  (list->vector coll))
(define-method coerce-to ((class <string-meta>) (coll <list>))
  (list->string coll))


(provide "gauche/collection")
