;;;
;;; SRFI-0   feature based conditional expansion construct
;;;
;;; $Id: srfi-0.scm,v 1.21 2004-07-26 09:51:41 shirok Exp $
;;;

(define-module srfi-0
  (export cond-expand))
(select-module srfi-0)

;;; Rewritten by a legacy macro, instead of r5rs syntax-rules,
;;; to enable adding features at runtime.  Such capability is
;;; for system management, and not supposed to be used freely
;;; by user programs.
;;; Since it is not hygienic, there may be some binding problems---
;;; need to be rewritten by low-level hygienic macro in future.

;;; The following features are supported in all Gauche versions.
;;;
;;;   srfi-0, srfi-1, srfi-2, srfi-4, srfi-5, 
;;;   srfi-6, srfi-7, srfi-8, srfi-9, srfi-10,
;;;   srfi-11, srfi-13, srfi-14, 
;;;   srfi-16, srfi-17, srfi-18, srfi-19,
;;;   srfi-22, srfi-23, srfi-25, 
;;;   srfi-26, srfi-27, srfi-28, srfi-29, srfi-30,
;;;   srfi-31,
;;;   srfi-37, srfi-39
;;;   gauche
;;;
;;; The following features are conditionally defined depending on
;;; how Gauche has been compiled
;;;   gauche-windows, gauche-eucjp, gauche-sjis, gauche-utf8, gauche-none

;; NB: User programs shouldn't casually modify this list!
(set! *cond-features*
      (append *cond-features*
              '((srfi-0)
                (srfi-1 srfi-1)
                (srfi-2 srfi-2)
                (srfi-4 gauche.vector)
                (srfi-5 srfi-5)
                (srfi-6)
                (srfi-7)
                (srfi-8)
                (srfi-9 srfi-9)
                (srfi-10)
                (srfi-11 srfi-11)
                (srfi-13 srfi-13)
                (srfi-14 srfi-14)
                (srfi-16)
                (srfi-17)
                (srfi-18 gauche.threads)
                (srfi-19 srfi-19)
                (srfi-22)
                (srfi-23)
                (srfi-25 gauche.array)
                (srfi-26)
                (srfi-27 srfi-27)
                (srfi-28)
                (srfi-29 srfi-29)
                (srfi-30)
                (srfi-31)
                (srfi-37 srfi-37)
                (srfi-38)
                (srfi-39 gauche.parameter)
                )))

(define-macro (cond-expand . clauses)

  ;; Check feature requirement.  Returns #f if requirement is not 
  ;; satisfied.  Returns a list of features to be use'd if requirement
  ;; is satisfied (it can be an emptylist, if the requirement is fulfilled
  ;; by Gauche built-in features).
  (define (fulfill? req seed)
    (cond
     ((identifier? req) (fulfill? (identifier->symbol req)))
     ((symbol? req)
      (let ((p (assq req *cond-features*)))
        (and p (if (null? (cdr p)) seed (cons (cadr p) seed)))))
     ((not (pair? req)) (error "Invalid cond-expand feature-id:" req))
     (else
      (case (unwrap-syntax (car req))
        ((and) (fulfill-and (cdr req) seed))
        ((or)  (fulfill-or  (cdr req) seed))
        ((not) (fulfill-not (cadr req) seed))
        (else (error "Invalid cond-expand feature expression:" req))))))

  (define (fulfill-and reqs seed)
    (if (null? reqs)
      seed
      (let ((c1 (fulfill? (car reqs) seed)))
        (and c1 (fulfill-and (cdr reqs) c1)))))

  (define (fulfill-or reqs seed)
    (if (null? reqs)
      #f
      (let ((c1 (fulfill? (car reqs) seed)))
        (or c1 (fulfill-or (cdr reqs) seed)))))

  (define (fulfill-not req seed)
    (if (fulfill? req '()) #f seed))

  (define (rec cls)
    (cond
     ((null? cls) (error "Unfulfilled cond-expand:" cls))
     ((not (pair? (car cls)))
      (error "Bad clause in cond-expand:" (car cls)))
     ((equal? (caar cls) 'else)
      (if (null? (cdr cls))
        `(begin . ,(cdar cls))
        (error "Misplaced else clause in cond-expand:" (car cls))))
     ((fulfill? (caar cls) '())
      => (lambda (uses)
           `(begin ,@(map (lambda (mod) `(use ,mod)) uses)
                   ,@(cdar cls))))
     (else
      (rec (cdr cls)))))

  (rec clauses))
  
(provide "srfi-0")
