;;;
;;; toposort.scm - topological sorting
;;;
;;;  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: toposort.scm,v 1.1 2001-10-29 06:22:39 shirok Exp $
;;;

(define-module util.toposort
  (use srfi-1)
  (export topological-sort)
  )
(select-module util.toposort)

;; (topological-sort nodes &optional =)
;;
;;  nodes : a list of (<from> <to0> <to1> ...)

(define (topological-sort nodes . maybe=)
  (define = (if (pair? maybe=) (car maybe=) eqv?))
  (define table (map (lambda (n) (cons (car n) 0)) nodes))
  (define queue '())
  (define result '())

  ;; set up - compute number of nodes that each node depends on.
  (define (set-up)
    (for-each (lambda (node)
                (for-each (lambda (to)
                            (cond ((assoc to table =)
                                   => (lambda (p) (inc! (cdr p))))
                                  (else
                                   (push! table (cons to 1)))))
                          (cdr node)))
              nodes))

  ;; traverse
  (define (traverse)
    (unless (null? queue)
      (let ((n0 (assoc (pop! queue) nodes =)))
        (when n0
          (for-each (lambda (to)
                      (cond ((assoc to table =)
                             => (lambda (p)
                                  (let ((cnt (- (cdr p) 1)))
                                    (when (= cnt 0)
                                      (push! result to)
                                      (push! queue to))
                                    (set! (cdr p) cnt))))
                            ))
                    (cdr n0)))
        (traverse))))

  (set-up)
  (set! queue (append-map (lambda (p) (if (= (cdr p) 0) (list (car p)) '()))
                          table))
  (set! result queue)
  (when (null? queue) (error "graph has circular dependency" nodes))
  (traverse)
  (reverse result))

(provide "util/toposort")
