;;;
;;; tree.scm - walk tree
;;;
;;;  Written by Shiro Kawai (shiro@acm.org)  2001
;;;  Public Domain..  I guess lots of Scheme programmers have already
;;;  written similar code.
;;;
;;;  $Id: tree.scm,v 1.2 2001-10-31 11:22:24 shirok Exp $
;;;

(define-module util.tree
  (use srfi-1)
  (export tree-fold tree-walk tree-fold-bf tree-walk-bf)
  )
(select-module util.tree)

(define (tree-walk tree proc leaf? walker)
  (define (rec node)
    (walker (lambda (n) (if (leaf? n) (proc n) (rec n))) node))
  (if (leaf? tree) (proc tree) (rec tree)))

(define (tree-fold tree proc knil leaf? folder)
  (define (rec node acc)
    (folder (lambda (n knil) (if (leaf? n) (proc n knil) (rec n knil)))
            acc node))
  (if (leaf? tree) (proc tree knil) (rec tree knil)))

(provide "util/tree")

