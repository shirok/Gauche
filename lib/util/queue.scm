;;;
;;; queue.scm - queue (fifo) implementation
;;;
;;;  Written by Shiro Kawai (shiro@acm.org)  2001
;;;  Public Domain..  I guess lots of Scheme programmers have already
;;;  written similar code.
;;;
;;;  $Id: queue.scm,v 1.1 2001-11-02 10:23:55 shirok Exp $
;;;

;; This queue implementation is tuned for speed.  A queue is simply
;; a pair that hols the head and tail of queue, rather defining a
;; special structure.   It bypasses consistency checks as well.
;;;So it is unsafe, but faster.
;; The API is compatible with SLIB's queue module.

(define-module util.queue
  )
(select-module util.queue)

(define (make-queue) (cons #f #f))

(define (queue? obj)
  (and (pair? obj)
       (or (and (not (car obj)) (not (cdr obj)))
           (and (pair? (car obj)) (pair? (cdr obj))
                (eq? (last-pair (car obj)) (cdr obj))))))

;; internal macro
(define-syntax %empty? 
  (syntax-rules () ((_ obj) (not (car obj)))))

(define (queue-empty? obj) (%empty? obj))

(define (queue-push! q obj)
  (if (%empty? q)
      (let ((cell (list obj)))
        (set-car! q cell)
        (set-cdr! q cell))
      (set-car! q (cons obj (car q))))
  q)

(define (enqueue! q obj)
  (if (%empty? q)
      (let ((cell (list obj)))
        (set-car! q cell)
        (set-cdr! q cell))
      (begin
        (set-cdr! (cdr q) (list obj))
        (set-cdr! q (cddr q))))
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
        (begin (set-car! q #f) (set-cdr! q #f))
        (set-car! q (cdar q)))
    item))

(define queue-pop! dequeue!)

(provide "util/queue")
