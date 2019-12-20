;;;
;;; data.skew-list - Skewed Binary Random Access List
;;;
;;;   Copyright (c) 2019  Shiro Kawai  <shiro@acm.org>
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

;; Implements SkewList as described in 
;; Chris Okasaki: Purely Functional Data Structures
;;
;; Tree a = Leaf a | Node a (Tree a) (Tree a)
;; SkewList a = SL [(Int, Tree a)]

(define-module data.skew-list
  (use gauche.record)
  (use util.match)
  (export <skew-list>
          skew-list?
          skew-list-elements
          skew-list-empty?
          skew-list-null
          skew-list-cons
          skew-list-car
          skew-list-cdr
          skew-list-ref
          skew-list-set
          skew-list-fold
          skew-list-length
          skew-list-length<=?
          list->skew-list
          skew-list->list)
  )
(select-module data.skew-list)

(define-record-type <skew-list>
  SL skew-list?
  (elements skew-list-elements))               ; [(Int, Tree)]]

;; Primitives
(define (skew-list-empty? sl)
  (assume-type sl <skew-list>)
  (null? (skew-list-elements sl)))
(define skew-list-null (SL '()))

(define (skew-list-cons x y)
  (assume-type y <skew-list>)
  (match (skew-list-elements y)
    [([w1 . t1] [w2 . t2] . ts)
     (if (= w1 w2)
       (SL `([,(+ 1 w1 w2) . (Node ,x ,t1 ,t2)] ,@ts))
       (SL `([1 . (Leaf ,x)] ,@(skew-list-elements y))))]
    [_ (SL `([1 . (Leaf ,x)] ,@(skew-list-elements y)))]))

(define (skew-list-car sl)
  (assume-type sl <skew-list>)
  (match (skew-list-elements sl)
    [() (error "Attempt to take skew-list-car of empty skew-list")]
    [([_ . ('Leaf x)] . _) x]
    [([_ . ('Node x _ _)] . _) x]))

(define (skew-list-cdr sl)
  (assume-type sl <skew-list>)
  (match (skew-list-elements sl)
    [() (error "Attempt to take skew-list-cdr of empty skew-list")]
    [([_ . ('Leaf _)] . ts) (SL ts)]
    [([w . ('Node x t1 t2)] . ts)
     (let1 w2 (quotient w 2)
       (SL `([,w2 . ,t1] [,w2 . ,t2] ,@ts)))]))

(define (skew-list-ref sl n)
  (define (tree-ref w i t)
    (if (= i 0)
      (cadr t)
      (if (= w 1)
        (error "index out of range" n)
        (match-let1 ('Node x t1 t2) t
          (let1 w2 (quotient w 2)
            (if (<= i w2) 
              (tree-ref w2 (- i 1) t1)
              (tree-ref w2 (- i 1 w2) t2)))))))
  (define (ref i ts)
    (match ts
      [() (error "index out of range" n)]
      [((w . t) . ts)
       (if (< i w) (tree-ref w i t) (ref (- i w) ts))]))
  (assume-type sl <skew-list>)
  (ref n (skew-list-elements sl)))

(define (skew-list-set sl n v)
  (define (tree-set w i t)
    (if (= i 0)
      (match t
        [('Leaf _) `(Leaf ,v)]
        [(`Node _ t1 t2) `(Node ,v ,t1 ,t2)])
      (if (= w 1)
        (error "index out of range" n)
        (match-let1 ('Node x t1 t2) t
          (let1 w2 (quotient w 2)
            (if (<= i w2) 
              `(Node ,x ,(tree-set w2 (- i 1) t1) ,t2)
              `(Node ,x ,t1 ,(tree-set w2 (- i 1 w2) t2))))))))
  (define (set i ts)
    (match ts
      [() (error "index out of range" n)]
      [((w . t) . ts)
       (if (< i w)
         `((,w . ,(tree-set w i t)) ,@ts)
         `((,w . ,t) ,@(set (- i w) ts)))]))
  (assume-type sl <skew-list>)
  (SL (set n (skew-list-elements sl))))
       
;; Can be more efficient
(define (list->skew-list lis)
  (if (null? lis)
    skew-list-null
    (skew-list-cons (car lis) (list->skew-list (cdr lis)))))

(define (skew-list->list sl)
  (assume-type sl <skew-list>)
  (reverse (skew-list-fold sl cons '())))

(define (skew-list-fold sl proc seed)
  (define (tree-fold tree seed)
    (match tree
      [('Leaf x) (proc x seed)]
      [('Node x t1 t2)
       (tree-fold t2 (tree-fold t1 (proc x seed)))]))
  (assume-type sl <skew-list>)
  (fold (^[p s] (tree-fold (cdr p) s)) seed (skew-list-elements sl)))

(define (skew-list-length sl)
  (assume-type sl <skew-list>)
  (fold (^[p s] (+ (car p) s)) 0 (skew-list-elements sl)))

(define (skew-list-length<=? sl k)
  (assume-type sl <skew-list>)
  (let loop ([es (skew-list-elements sl)] [s 0])
    (cond [(null? es) #t]
          [(< k s) #f]
          [else (loop (cdr es) (+ s (caar es)))])))

