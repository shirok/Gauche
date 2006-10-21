;;;
;;; rbtree.scm - Red-Black Tree
;;;
;;;   Copyright (c) 2005 Rui Ueyama (rui314@gmail.com)
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;
;;;  $Id: rbtree.scm,v 1.1 2006-10-21 23:26:22 shirok Exp $
;;;

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

;; This code implements the red-black tree algorithm, based on Cormen,
;; Leiserson, Rivest, and Stein, "Introduction to Algorithms, Second
;; Edition", Section 13, pp. 273-294.

(define *nil* (list (gensym "nil-")))

(define-class <rbtree-meta> (<class>) ())

(define-class <rbtree> (<sequence>)
  ((key=? :init-keyword :key=?
          :init-form (error "must supply :key=? keyword"))
   (key<? :init-keyword :key<?
          :init-form (error "must supply :key<? keyword"))
   (root  :init-keyword :root
          :init-value *nil*
          :accessor root-of))
  :metaclass <rbtree-meta>)

(define-class <node> ()
  ((key    :init-keyword :key)
   (value  :init-keyword :value)
   (parent :init-keyword :parent
           :init-value *nil*
           :accessor parent)
   (color  :init-keyword :color
           :accessor color)
   (left   :init-keyword :left
           :init-value *nil*
           :accessor left)
   (right  :init-keyword :right
           :init-value *nil*
           :accessor right)))

(define (make-rbtree key=? key<?)
  (make <rbtree> :key=? key=? :key<? key<?))

(define (nil? node)
  (eq? node *nil*))

(define (rbtree? obj)
  (is-a? obj <rbtree>))

(define (black? node)
  (or (nil? node)
      (eq? (ref node 'color) 'black)))

(define (red? node)
  (and (not (nil? node))
       (eq? (ref node 'color) 'red)))

(define (paint-black! node)
  (set! (ref node 'color) 'black))

(define (paint-red! node)
  (set! (ref node 'color) 'red))

(define (guarantee-rbtree obj)
  (unless (rbtree? obj)
    (error "<rbtree> required, gut got" obj)))

(define left*
  (getter-with-setter
   (lambda (node not-invert?)
     (if not-invert? (left node) (right node)))
   (lambda (node not-invert? val)
     (if not-invert?
       (set! (left node) val)
       (set! (right node) val)))))

(define right*
  (getter-with-setter
   (lambda (node not-invert?)
     (if not-invert? (right node) (left node)))
   (lambda (node not-invert? val)
     (if not-invert?
       (set! (right node) val)
       (set! (left node) val)))))

(define (left-rotate! tree x b)
  (let1 y (right* x b)
    (set! (right* x b) (left* y b))
    (unless (nil? (left* y b))
      (set! (parent (left* y b)) x))
    (set! (parent y) (parent x))
    (cond ((nil? (parent x))
           (set! (root-of tree) y))
          ((eq? x (left* (parent x) b))
           (set! (left* (parent x) b) y))
          (else
           (set! (right* (parent x) b) y)))
    (set! (left* y b) x)
    (set! (parent x) y)))

(define (right-rotate! tree x b)
  (left-rotate! tree x (not b)))

(define (rbtree-get tree key . arg)
  (guarantee-rbtree tree)
  (or (and-let* ((node (get-node tree key))
                 ( (not (nil? node)) ))
        (ref node 'value))
      (get-optional arg (error "red-black tree doesn't have an entry for key" key))))

(define (get-node tree key)
  (let ((key=? (ref tree 'key=?))
        (key<? (ref tree 'key<?)))
    (let loop ((node (root-of tree)))
      (cond ((nil? node) node)
            ((key<? key (ref node 'key)) (loop (left node)))
            ((key=? key (ref node 'key)) node)
            (else (loop (right node)))))))

(define (rbtree-put! tree key val)
  (guarantee-rbtree tree)
  (let* ((key=? (ref tree 'key=?))
         (key<? (ref tree 'key<?)))
    (let loop ((x (root-of tree)) (y *nil*))
      (cond ((not (nil? x))
             (cond ((key=? key (ref x 'key))
                    (set! (ref x 'value) val))
                   ((key<? key (ref x 'key))
                    (loop (left x) x))
                   (else (loop (right x) x))))
            ((nil? y)                   ; tree was empty
             (set! (root-of tree)
                   (make <node> :key key :value val :color 'black)))
            (else
             (let1 node (make <node> :key key :value val :color 'red :parent y)
               (if (key<? key (ref y 'key))
                 (set! (left y) node)
                 (set! (right y) node))
               (put-fixup! tree node)))))))

(define (put-fixup! tree z)
  (let loop ((z z))
    (when (red? (parent z))
      (let* ((b (eq? (parent z) (left (parent (parent z)))))
             (y (right* (parent (parent z)) b)))
        (if (red? y)
          (begin
            (paint-black! (parent z))
            (unless (nil? y) (paint-black! y))
            (paint-red! (parent (parent z)))
            (loop (parent (parent z))))
          (let1 z (if (eq? z (right* (parent z) b))
                    (begin0 (parent z)
                            (left-rotate! tree (parent z) b))
                    z)
            (paint-black! (parent z))
            (paint-red! (parent (parent z)))
            (right-rotate! tree (parent (parent z)) b))))))
  (paint-black! (root-of tree)))

(define (rbtree-delete! tree key)
  (guarantee-rbtree tree)
  (and-let* ((node (get-node tree key))
             ( (not (nil? node)) ))
    (delete-node! tree node)
    #t))

(define (delete-node! tree z)
  (let1 z (if (and (not (nil? (left z))) (not (nil? (right z))))
            (let1 y (successor z)
              (set! (ref z 'key) (ref y 'key))
              (set! (ref z 'value) (ref y 'value))
              y)
            z)
    (let ((x (left* z (not (nil? (left z)))))
          (p (parent z)))
      (unless (nil? x)
        (set! (parent x) p))
      (if (nil? p)
        (set! (root-of tree) x)
        (set! (left* p (eq? z (left p))) x))
      (when (black? z)
        (delete-fixup! tree x p))
      (set! (left z) #f)
      (set! (right z) #f)
      (set! (parent z) #f))))

(define (delete-fixup! tree x p)
  (let loop ((x x) (p p))
    (if (or (nil? p) (red? x))
      (unless (nil? x)
        (paint-black! x))
      (let1 b (eq? x (left p))
        (let1 w (let1 w (right* p b)
                  (if (red? w)
                    (begin (paint-black! w)
                           (paint-red! p)
                           (left-rotate! tree p b)
                           (right* p b))
                    w))
          (if (and (black? (left w)) (black? (right w)))
            (begin (paint-red! w)
                   (loop p (parent p)))
            (let1 w (if (black? (right* w b))
                      (begin (unless (nil? (left* w b))
                               (paint-black! (left* w b)))
                             (paint-red! w)
                             (right-rotate! tree w b)
                             (right* p b))
                      w)
              (set! (ref w 'color)
                    (if (nil? p) 'black (ref p 'color)))
              (paint-black! p)
              (paint-black! (right* w b))
              (left-rotate! tree p b)
              (paint-black! (root-of tree)))))))))

(define (rbtree-exists? tree key)
  (guarantee-rbtree tree)
  (not (nil? (get-node tree key))))

(define (rbtree-empty? tree)
  (guarantee-rbtree tree)
  (nil? (root-of tree)))

(define (rbtree-push! tree key value)
  (guarantee-rbtree tree)
  (let1 node (get-node tree key)
    (if (nil? node)
      (rbtree-put! tree key (list value))
      (push! (ref node 'value) value))))

(define (rbtree-pop! tree key . arg)
  (guarantee-rbtree tree)
  (let1 node (get-node tree key)
    (cond ((nil? node)
           (get-optional arg (error "red-black tree doesn't have an entry for key" key)))
          ((not (pair? (ref node 'value)))
           (get-optional arg (errorf "red-black tree value for key ~S is not a pair: ~S"
                                     key (ref node 'value))))
          (else (pop! (ref node 'value))))))

(define (rbtree-update! tree key proc . arg)
  (guarantee-rbtree tree)
  (let1 node (get-node tree key)
    (if (nil? node)
      (let1 newval (proc (get-optional arg (error "red-black tree doesn't have an entry for key" key)))
        (rbtree-put! tree key newval))
      (set! (ref node 'value)
            (proc (ref node 'value))))))

(define (rbtree-num-entries tree)
  (let loop ((node (root-of tree)))
    (if (nil? node)
      0
      (+ 1 (loop (left node)) (loop (right node))))))

(define (rbtree->alist tree)
  (guarantee-rbtree tree)
  (rbtree-fold-right tree acons '()))

(define (alist->rbtree alist key=? key<?)
  (let1 r (make <rbtree> :key=? key=? :key<? key<?)
    (for-each (lambda (e)
                (rbtree-put! r (car e) (cdr e)))
              alist)
    r))

(define (rbtree-keys tree)
  (rbtree-fold-right tree (lambda (k v r) (cons k r)) '()))

(define (rbtree-values tree)
  (rbtree-fold-right tree (lambda (k v r) (cons v r)) '()))

(define-syntax define-accessor
  (syntax-rules ()
    ((_ name lookup delete?)
     (define (name tree . arg)
       (guarantee-rbtree tree)
       (or (and-let* ((node (root-of tree))
                      ( (not (nil? node)) ))
             (let1 node (lookup node)
               (when delete?
                 (delete-node! tree node))
               (cons (ref node 'key) (ref node 'value))))
           (get-optional arg (error "empty tree")))))))

(define-accessor rbtree-min minimum #f)
(define-accessor rbtree-max maximum #f)
(define-accessor rbtree-extract-min! minimum #t)
(define-accessor rbtree-extract-max! maximum #t)

(define (minimum node)
  (if (nil? (left node))
    node
    (minimum (left node))))

(define (maximum node)
  (if (nil? (right node))
    node
    (maximum (right node))))

(define (minimum* node not-invert?)
  (if not-invert? (minimum node) (maximum node)))
(define (maximum* node not-invert?)
  (if not-invert? (maximum node) (minimum node)))

(define-values (successor predecessor)
  (let1 tmpl (lambda (dir proc)
               (lambda (node)
                 (if (not (nil? (dir node)))
                   (proc (dir node))
                   (let loop ((x node)
                              (y (parent node)))
                     (if (and (not (nil? y)) (eq? x (dir y)))
                       (loop y (parent y))
                       y)))))
    (values (tmpl right minimum) (tmpl left maximum))))

(define (successor* node not-invert?)
  (if not-invert? (successor node) (predecessor node)))
(define (predecessor* node not-invert?)
  (if not-invert? (predecessor node) (successor node)))

(define (rbtree-copy tree)
  (define (copy node)
    (let loop ((node node) (p *nil*))
      (if (nil? node)
        node
        (let1 r (node-copy node)
          (set! (parent r) p)
          (set! (left r) (loop (left r) r))
          (set! (right r) (loop (right r) r))
          r))))
  (let1 new-tree (make-rbtree (ref tree 'key=?) (ref tree 'key<?))
    (set! (root-of new-tree) (copy (root-of tree)))
    new-tree))

(define (node-copy node)
  (make <node>
    :key    (slot-ref node 'key)
    :value  (slot-ref node 'value)
    :color  (slot-ref node 'color)
    :parent (slot-ref node 'parent)
    :left   (slot-ref node 'left)
    :right  (slot-ref node 'right)))

(define (rbtree-fold rbtree proc seed)
  (if (nil? (root-of rbtree))
    seed
    (let loop ((node (minimum (root-of rbtree)))
               (seed seed))
      (if (nil? node)
        seed
        (loop (successor node)
              (proc (ref node 'key) (ref node 'value) seed))))
    ))

(define (rbtree-fold-right rbtree proc seed)
  (if (nil? (root-of rbtree))
    seed
    (let loop ((node (maximum (root-of rbtree)))
               (seed seed))
      (if (nil? node)
        seed
        (loop (predecessor node)
              (proc (ref node 'key) (ref node 'value) seed))))
    ))

;;------------------------------------------------------------------
;; Sequence protocol

(define-method call-with-iterator ((tree <rbtree>) proc . opts)
  (define node (let1 r (root-of tree)
                 (if (nil? r) r (minimum r))))
  (proc (lambda () (nil? node))
        (lambda ()
          (begin0 (cons (ref node 'key) (ref node 'value))
                  (set! node (successor node))))))

(define-method call-with-builder ((class <rbtree-meta>) proc . opts)
  (let ((key=? (get-keyword :key=? opts))
        (key<? (get-keyword :key<? opts)))
    (let1 tree (make-rbtree key=? key<?)
      (proc (lambda (e)
              (unless (pair? e)
                (error "pair required to build a rbtree, but got" e))
              (rbtree-put! tree (car e) (cdr e)))
            (lambda () tree)))))

(define (nth-node tree index . maybe-default)
  (define (oor)
    (get-optional maybe-default (error "index out of range:" index)))
  (cond ((< index 0) (oor))
        ((rbtree-empty? tree) (oor))
        (else
         (let rec ((index index) (node (minimum (root-of tree))))
           (cond ((zero? index) node)
                 ((nil? node) (oor))
                 (else (rec (- index 1) (successor node))))))))

(define-method referencer ((tree <rbtree>))
  (lambda (tree index . maybe-default)
    (let1 node (apply nth-node tree index maybe-default)
      (cons (ref node 'key) (ref node 'value)))))

;;--------------------------------------------------------------------
;; For testing.
;;
;; Check consistency of rbtree.  Returns #t if it is consistent,
;; raises an error otherwise.
;;
;; A red-black tree have to satisyfy the following properties:
;;
;;  1. Every node is either red or black.
;;  2. The root is black.
;;  3. Every leaf (nil) is black.
;;  4. If a node is red, then both its children are black.
;;  5. For each node, all paths from the node to descendent leaves
;;     contain the same number of black nodes.

(define (rbtree-check tree)
  (define (check-color root)
    (let loop ((node root))
      (for-each (lambda (child)
                  (unless (nil? child)
                    (unless (eq? node (parent child))
                      (error "parent pointer of a child is improper"))
                    (when (and (red? node) (not (black? child)))
                      (error "child of a red node must be black"))
                    (loop child)))
                (list (left node) (right node)))))
  (define (check-order root)
    (define (node<=? a b)
      (or ((ref tree 'key<?) (ref a 'key) (ref b 'key))
          ((ref tree 'key=?) (ref a 'key) (ref b 'key))))
    (define (check proc a b)
      (or (proc a b)
          (error "improper order:" (ref a 'key) (ref b 'key))))
    (let loop ((node root))
      (let ((rnode (right node))
            (lnode (left node)))
        (unless (nil? lnode)
          (check node<=? lnode node)
          (loop lnode))
        (unless (nil? rnode)
          (check node<=? node rnode)
          (loop rnode)))))
  (define (check-black-height root)
    (let1 black-height #f
      (let loop ((height 0)
                 (node root))
        (if (nil? node)
          (if black-height
            (unless (= black-height height)
              (error "black heights are not same"))
            (set! black-height height))
          (let1 height (if (black? node)
                         (+ height 1)
                         height)
            (loop height (left node))
            (loop height (right node)))))))
  (let1 root (root-of tree)
    (unless (nil? root)
      (unless (black? root)
        (error "the root node must be black"))
      (unless (nil? (parent root))
        (error "parent of the root node must be NIL"))
      (check-color root)
      (check-order root)
      (check-black-height root)))
  #t)

(provide "util/rbtree")
