;;;
;;; srfi-117 - Queues based on lists
;;;
;;;   Copyright (c) 2015-2019  Shiro Kawai  <shiro@acm.org>
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

;; srfi-117 list queue is implemented on top of data.queue in Gauche

(define-module srfi-117
  (use data.queue)
  (export make-list-queue list-queue list-queue-copy
          list-queue-unfold list-queue-unfold-right
          list-queue? list-queue-empty?
          list-queue-front list-queue-back
          list-queue-list list-queue-first-last
          list-queue-add-front! list-queue-add-back!
          list-queue-remove-front! list-queue-remove-back!
          list-queue-remove-all!
          list-queue-set-list!
          list-queue-append list-queue-append!
          list-queue-concatenate
          list-queue-map list-queue-map! list-queue-for-each))
(select-module srfi-117)

(define (make-list-queue lis :optional last)
  (if (pair? last)
    (rlet1 q (make-queue) ; needs to satisfy O(1) restriction
      ((with-module data.queue %queue-set-content!) q lis last))
    (list->queue lis)))

(define (list-queue . elts) (list->queue elts))

(define (list-queue-copy q) (copy-queue q))

(define (list-queue-unfold stop? mapper successor seed
                           :optional (q (make-queue)))
  (let loop ((seed seed) (elts '()))
    (if (stop? seed)
      (dolist [e elts] (queue-push! q e))
      (loop (successor seed) (cons (mapper seed) elts))))
  q)

(define (list-queue-unfold-right stop? mapper successor seed
                                 :optional (q (make-queue)))
  (let loop ((seed seed) (elts '()))
    (if (stop? seed)
      (dolist [e elts] (enqueue! q e))
      (loop (successor seed) (cons (mapper seed) elts))))
  q)

(define (list-queue? q) (queue? q))
(define (list-queue-empty? q) (queue-empty? q))
(define (list-queue-front q) (queue-front q))
(define (list-queue-back q) (queue-rear q))
(define (list-queue-list q) (queue-internal-list q))

;; This also returns internal structure.
(define (list-queue-first-last q)
  (when (mtqueue? q)
    (error "Can't get internal pairs of <mtqueue>:" q))
  (values ((with-module data.queue %qhead) q)
          ((with-module data.queue %qtail) q)))

(define (list-queue-add-front! q elt) (queue-push! q elt))
(define (list-queue-add-back! q elt ) (enqueue! q elt))
(define (list-queue-remove-front! q)  (dequeue! q))
(define (list-queue-remove-back! q)
  ;; it's ok to be O(n), so we take gratuitously inefficient path
  (let loop ([n (queue-length q)]
             [elts (dequeue-all! q)])
    (if (= n 1)
      (car elts)
      (begin (enqueue! q (car elts))
             (loop (- n 1) (cdr elts))))))

(define (list-queue-remove-all! q) (dequeue-all! q))

(define (list-queue-set-list! q lis :optional last)
  (let1 last (last-pair lis)
    ((with-module data.queue %queue-set-content!) q lis last)
    q))

(define (list-queue-append . qs)
  (list-queue-concatenate qs))

(define (list-queue-append! . qs)
  (let loop ([qs qs]
             [head '()]
             [tail '()])
    (if (null? qs)
      (make-list-queue head tail)
      (receive (hd tl) (list-queue-first-last (car qs))
        ;; we purge the original queues, so that the mutation of internal
        ;; cells won't confuse them.
        (dequeue-all! (car qs))
        (cond [(null? hd) (loop (cdr qs) head tail)]
              [(pair? tail)
               (begin (set-cdr! tail hd) (loop (cdr qs) head tl))]
              [else (loop (cdr qs) hd tl)])))))

(define (list-queue-concatenate qs)
  (list->queue (concatenate (map list-queue-list qs))))

(define (list-queue-map proc q)
  (list->queue (map proc (queue->list q))))

(define (list-queue-map! proc q)
  (let1 xs (map proc (queue->list q))
    ((with-module data.queue %queue-set-content!) q xs #f)
    q))

(define (list-queue-for-each proc q)
  (for-each proc (queue->list q)))

