;;;
;;; queue.scm - queue (fifo) implementation
;;;
;;;  Written by Shiro Kawai (shiro@acm.org)  2001
;;;  Public Domain..  I guess lots of Scheme programmers have already
;;;  written similar code.
;;;
;;;  $Id: queue.scm,v 1.6 2002-08-27 10:24:37 shirok Exp $
;;;

;; This queue implementation is tuned for speed.  A queue is simply
;; a pair that holds the head and tail of queue, rather defining a
;; special structure.   It bypasses consistency checks as well.
;; So it is unsafe, but faster.
;; The API is upper compatible with SLIB's queue module.

(define-module util.queue
  (export make-queue queue? queue-empty?
          queue-push! enqueue! queue-pop! dequeue! dequeue-all!
          queue-front queue-rear queue-length
          queue->list list->queue
          find-in-queue remove-from-queue!)
  )
(select-module util.queue)

(define (make-queue) (cons '() '()))

;; this one we does (expensive) check.
(define (queue? obj)
  (and (pair? obj)
       (or (and (null? (car obj)) (null? (cdr obj)))
           (and (pair? (car obj)) (pair? (cdr obj))
                (eq? (last-pair (car obj)) (cdr obj))))))

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

(define (queue-front q)
  (when (%empty? q) (error "queue is empty" q))
  (caar q))

(define (queue-rear q)
  (when (%empty? q) (error "queue is empty" q))
  (cadr q))

(define (dequeue! q)
  (when (%empty? q) (error "queue is empty" q))
  (let ((item (caar q)))
    (if (eq? (car q) (cdr q))
        (begin (set-car! q '()) (set-cdr! q '()))
        (set-car! q (cdar q)))
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
    (set-car! q head)
    (set-cdr! q (last-pair head))
    removed?))

;; NB: Scheme48 has delete-from-queue!, which has reversed order
;; of arguments of delete in SRFI-1.   I leave it undefined here.
;;
;; (define (delete-from-queue! q item)  ;;Scheme48
;;   (remove-from-queue! (lambda (elt) (eq? item elt)) q))


(provide "util/queue")
