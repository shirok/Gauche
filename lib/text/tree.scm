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
;;;  $Id: tree.scm,v 1.4 2001-11-20 07:52:02 shirok Exp $
;;;

(define-module text.tree
  (export write-tree
          tree->string)
  )
(select-module text.tree)

(define-method write-tree (tree)
  (write-tree tree (current-output-port)))

(define-method write-tree ((tree <list>) out)
  (let loop ((tree tree))
    (cond ((null? tree))
          ((pair? tree) (write-tree (car tree) out) (loop (cdr tree)))
          (else (write-tree tree out)))))

(define-method write-tree ((tree <top>) out)
  (display tree out))

(define (tree->string tree)
  (with-output-to-string (lambda () (write-tree tree))))

(provide "text/tree")
