;;;
;;; rbtree.scm - Red-Black Tree
;;;

;; Red-black tree is now provided as a builtin <tree-map> object.
;; This module is for backward compatibility.

(define-module util.rbtree
  (use gauche.sequence)
  (export <rbtree> make-rbtree rbtree?
          rbtree-get rbtree-put! rbtree-delete!
          rbtree-exists? rbtree-empty?
          rbtree-push! rbtree-pop! rbtree-update!
          rbtree-min rbtree-max
          rbtree-extract-min! rbtree-extract-max!
          rbtree-copy rbtree-num-entries
          rbtree-keys rbtree-values
          rbtree-fold rbtree-fold-right
          rbtree->alist alist->rbtree
          )
  )
(select-module util.rbtree)

(define <rbtree> <tree-map>)
(define make-rbtree make-tree-map)
(define rbtree? tree-map?)
(define rbtree-get tree-map-get)
(define rbtree-put! tree-map-put!)
(define rbtree-delete! tree-map-delete!)
(define rbtree-exists? tree-map-exists?)
(define rbtree-empty?  tree-map-empty?)
(define rbtree-push!   tree-map-push!)
(define rbtree-pop!    tree-map-pop!)
(define rbtree-update! tree-map-update!)
(define rbtree-num-entries tree-map-num-entries)
(define rbtree->alist  tree-map->alist)
(define alist->rbtree  alist->tree-map)
(define rbtree-keys    tree-map-keys)
(define rbtree-values  tree-map-values)

(define (rbtree-min tree . args)
  (or (tree-map-min tree)
      (get-optional args (error "tree is empty:" tree))))
(define (rbtree-max tree . args)
  (or (tree-map-max tree)
      (get-optional args (error "tree is empty:" tree))))
(define (rbtree-extract-min! tree . args)
  (or (tree-map-pop-min! tree)
      (get-optional args (error "tree is empty:" tree))))
(define (rbtree-extract-max! tree . args)
  (or (tree-map-pop-max! tree)
      (get-optional args (error "tree is empty:" tree))))

(define rbtree-copy  tree-map-copy)
(define rbtree-fold  tree-map-fold)
(define rbtree-fold-right tree-map-fold-right)

(define rbtree-check (with-module gauche.internal %tree-map-check-consistency))

