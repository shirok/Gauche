;;;
;;; test/tree.scm - lightweight text generation
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
;;;  $Id: tree.scm,v 1.2 2001-10-24 08:36:56 shirok Exp $
;;;

(define-module text.tree
  (export write-tree
          tree->string)
  )
(select-module text.tree)

(define-method write-tree (tree)
  (write-tree tree (current-output-port)))

(define-method write-tree ((tree <list>) out)
  (for-each (lambda (x) (write-tree x out)) tree))

(define-method write-tree ((tree <top>) out)
  (display tree out))

(define-method tree->string (tree)
  (with-output-to-string (lambda () (write-tree tree))))

(provide "text/tree")
