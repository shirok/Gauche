;;;
;;; tree.scm - walk tree
;;;
;;;  Written by Shiro Kawai (shiro@acm.org)  2001
;;;  Public Domain..  I guess lots of Scheme programmers have already
;;;  written similar code.
;;;

(define-module util.tree
  (use srfi-1)
  (export tree-fold tree-walk tree-fold-bf tree-walk-bf)
  )
(select-module util.tree)

(define (tree-walk tree proc leaf? walker)
  (define (rec node)
    (walker (^n (if (leaf? n) (proc n) (rec n))) node))
  (if (leaf? tree) (proc tree) (rec tree)))

(define (tree-fold tree proc knil leaf? folder)
  (define (rec node r)
    (folder (lambda (n knil) (if (leaf? n) (proc n knil) (rec n knil)))
            r node))
  (if (leaf? tree) (proc tree knil) (rec tree knil)))

(define (tree-walk-bf tree proc leaf? walker)
  (define (rec node)
    (let* ((branches '()))
      (walker (^n (if (leaf? n) (proc n) (push! branches n))) node)
      (for-each rec (reverse branches))))
  (if (leaf? tree) (proc tree) (rec tree)))

(define (tree-fold-bf tree proc knil leaf? folder)
  (define (rec node r)
    (let* ((branches '())
           (r (folder (lambda (n knil)
                        (if (leaf? n)
                            (proc n knil)
                            (begin (push! branches n) knil)))
                      r node)))
      (fold rec r (reverse branches))))
  (if (leaf? tree) (proc tree knil) (rec tree knil)))


