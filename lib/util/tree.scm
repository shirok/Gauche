;;;
;;; tree.scm - walk tree
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
;;;  $Id: tree.scm,v 1.1 2001-10-29 11:55:04 shirok Exp $
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

