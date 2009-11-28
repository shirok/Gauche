;;;
;;; queue.scm - queue (fifo) implementation
;;;
;;;  Written by Shiro Kawai (shiro@acm.org)  2001
;;;  Public Domain..  I guess lots of Scheme programmers have already
;;;  written similar code.
;;;
;;;  $Id: queue.scm,v 1.11 2007-07-03 05:35:54 shirok Exp $
;;;

;; This queue implementation is tuned for speed.  A queue is simply
;; a pair that holds the head and tail of queue, instead of using
;; a dedicated structure.  It bypasses consistency checks.
;; So it is unsafe, but faster.
;; The API is upper compatible with SLIB's queue module.

;; NB: this module intentionally avoids using other modules
;; for performance reasons.

(define-module util.queue
  (export make-queue queue? queue-empty? copy-queue
          queue-push! queue-push-unique! enqueue! enqueue-unique!
          queue-pop! dequeue! dequeue-all!
          queue-front queue-rear queue-length
          queue->list list->queue
          find-in-queue remove-from-queue!)
  )
(select-module util.queue)

(define (make-queue) (cons '() '()))

;; this one does (expensive) check.
(define (queue? obj)
  (and (pair? obj)
       (or (and (null? (car obj)) (null? (cdr obj)))
           (and (pair? (car obj)) (pair? (cdr obj))
                (eq? (last-pair (car obj)) (cdr obj))))))

(define (copy-queue q)
  (unless (queue? q) (error "argument is not a queue:" q))
  (list->queue (car q)))

;; internal macro
(define-syntax %empty? 
  (syntax-rules () ((_ obj) (null? (car obj)))))

(define (queue-empty? obj) (%empty? obj))

(define (queue-push! q obj . more-objs)
  (if (%empty? q)
    (let ((cell (list obj)))
      (set-car! q cell)
      (set-cdr! q cell))
    (set-car! q (cons obj (car q))))
  (unless (null? more-objs)
    (let loop ((head (car q))
               (objs more-objs))
      (if (null? objs)
        (set-car! q head)
        (loop (cons (car objs) head) (cdr objs)))))
  q)

(define (queue-push-unique! q eq-proc obj . more-objs)
  (if (%empty? q)
    (apply queue-push! q obj more-objs)
    (begin
      (let loop ((p (car q)))
        (cond ((null? p) (set-car! q (cons obj (car q))))
              ((eq-proc (car p) obj))
              (else (loop (cdr p)))))
      (unless (null? more-objs)
        (apply queue-push-unique! q eq-proc more-objs))
      q)))

(define (enqueue! q obj . more-objs)
  (if (%empty? q)
    (let ((cell (list obj)))
      (set-car! q cell)
      (set-cdr! q cell))
    (begin
      (set-cdr! (cdr q) (list obj))
      (set-cdr! q (cddr q))))
  (unless (null? more-objs)
    (let loop ((tail (cdr q))
               (objs more-objs))
      (if (null? objs)
        (set-cdr! q tail)
        (begin (set-cdr! tail (list (car objs)))
               (loop (cdr tail) (cdr objs))))))
  q)

(define (enqueue-unique! q eq-proc obj . more-objs)
  (if (%empty? q)
    (apply enqueue! q obj more-objs)
    (begin
      (let loop ((p (car q)))
        (cond ((null? p)
               (set-cdr! (cdr q) (list obj))
               (set-cdr! q (cddr q)))
              ((eq-proc (car p) obj))
              (else (loop (cdr p)))))
      (unless (null? more-objs)
        (apply enqueue-unique! q eq-proc more-objs))
      q)))

(define (queue-front q)
  (when (%empty? q) (error "queue is empty" q))
  (caar q))

(define (queue-rear q)
  (when (%empty? q) (error "queue is empty" q))
  (cadr q))

;; When we dequeue an item, we clear the cdr of the cell that becomes
;; unused.  It is necessary to prevent the conservative GC from
;; retaining unbound memory, in case a false pointer happens to point
;; one of this unused cell, which in turn would retain all the cells
;; used for the queue if we don't clear it.
;; See Boehm: "Bounding Space Usage of Conservative Garbage Collectors", 
;; http://www.hpl.hp.com/techreports/2001/HPL-2001-251.pdf
;; for the details.

(define (dequeue! q)
  (when (%empty? q) (error "queue is empty" q))
  (let ((item (caar q)))
    (if (eq? (car q) (cdr q))
      (begin (set-car! q '()) (set-cdr! q '()))
      (let1 h (car q)
        (set-car! q (cdar q))
        (set-cdr! h #f)))      ;; cut the false chain.  see above.
    item))

(define queue-pop! dequeue!)

(define (dequeue-all! q)
  (if (%empty? q)
      '()
      (let ((r (car q)))
        (set-car! q '())
        (set-cdr! q '())
        r)))

(define (queue-length q) (length (car q)))

(define (queue->list q) (list-copy (car q)))

;; copy list and find tail at once
(define (list->queue lis)
  (let loop ((head '())
             (tail '())
             (p    lis))
    (cond ((null? p) (cons head tail))
          ((pair? p)
           (if (null? head)
               (let1 cell (cons (car p) '())
                 (loop cell cell (cdr p)))
               (begin (set-cdr! tail (cons (car p) '()))
                      (loop head (cdr tail) (cdr p)))))
          (else (error "proper list required, but got" lis)))))

;; find and remove.
(define (find-in-queue pred q)
  (let loop ((lis (car q)))
    (cond ((null? lis) #f)
          ((pred (car lis)) (car lis))
          (else (loop (cdr lis))))))

(define (remove-from-queue! pred q)
  (let* ((head (let rec ((lis (car q)))
                 (cond ((null? lis) '())
                       ((pred (car lis)) (rec (cdr lis)))
                       (else (let1 tail (rec (cdr lis))
                               (if (eq? (cdr lis) tail)
                                   lis
                                   (cons (car lis) tail))))
                       )))
         (removed? (not (eq? (car q) head))))
    (when removed?
      (set-car! q head)
      (set-cdr! q (if (null? head) head (last-pair head))))
    removed?))

;; NB: Scheme48 has delete-from-queue!, whose argument order is
;; reversed from 'delete' in SRFI-1.   I leave it undefined here.
;;
;; (define (delete-from-queue! q item)  ;;Scheme48
;;   (remove-from-queue! (lambda (elt) (eq? item elt)) q))


