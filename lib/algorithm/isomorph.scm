;;;
;;; isomorph.scm - check isomorphism
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
;;;  $Id: isomorph.scm,v 1.1 2001-10-19 08:13:45 shirok Exp $
;;;

(define-module algorithm.isomorph
  (use srfi-1)
  (use gauche.let-opt)
  (export isomorphic? object-isomorphic?)
  )
(select-module algorithm.isomorph)

(define (isomorphic? a b . args)
  (define ctx (if (pair? args)
                  (if (hash-table? (car args))
                      (car args)
                      (error "hash table required, but got" (car args)))
                  (make-hash-table)))

  (define (iso? a b)
    (cond ((or (boolean? a) (char? a) (number? a)) (eqv? a b))
          ((eq? a '()) (eq? b '()))
          ((hash-table-get ctx a #f)   ;node has been appeared
           => (lambda (bb) (eq? bb b)))
          (else
           (hash-table-put! ctx a b)
           (cond ((pair? a) (and (pair? b)
                                 (iso? (car a) (car b))
                                 (iso? (cdr a) (cdr b))))
                 ((string? a) (and (string? b) (string=? a b)))
                 ((symbol? a) (eq? a b))
                 ((keyword? a) (eqv? a b))
                 ((vector? a) (vector-iso? a b))
                 (else (object-isomorphic? a b ctx))
                 ))))

  (define (vector-iso? a b)
    (and (vector? b)
         (let loop ((la (vector->list a)) (lb (vector->list b)))
           (cond ((null? la) (null? lb))
                 ((null? lb) #f)
                 ((iso? (car la) (car lb)) (loop (cdr la) (cdr lb)))
                 (else #f)))))

  (define (hash-iso? a b)
    (and (hash-table? b)
         (let loop ((la (hash-table-map a cons))
                    (lb (hash-table-map b cons)))
           (cond ((null? a) (null? b))
                 ((null? b) #f)
                 ((assq (caar la) lb)
                  => (lambda (p)
                       (and (iso? (cdar la) (cdr p))
                            (loop (cdr la) (alist-delete (caar la) lb)))))
                 (else #f)))))

  (iso? a b)
  )

(define (object-isomorphic? a b context)
  (eq? a b))                            ;default

(provide "algorithm/isomorph")
