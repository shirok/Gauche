;;;
;;; test/tree.scm - lightweight text generation
;;;
;;;  Public Domain.
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

