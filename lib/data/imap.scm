;;;
;;; data.imap - immutable tree map
;;;
;;;   Copyright (c) 2015  Shiro Kawai  <shiro@acm.org>
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

;; This implements functional red-black tree
;; as described in Chris Okasaki's Purely Functional Data Structures.

(define-module data.imap
  (use gauche.sequence)
  (use gauche.dictionary)
  (use gauche.record)
  (use data.queue)
  (use util.match)
  (use srfi-114)
  (export <imap> <imap-meta>
          make-imap alist->imap tree-map->imap
          imap? imap-empty?
          imap-exists? imap-get imap-put
          imap-delete
          imap-min imap-max)
  )
(select-module data.imap)

;;
;; Internal implementation
;;

(define-record-type (T (pseudo-rtd <vector>)) #t #t color left elem right)
;; NB: We just use #f as E node.
(define (E? x) (not x))

(define balance
  (match-lambda*
    [(or ('B #('R #('R a x b) y c) z d)
         ('B #('R a x #('R b y c)) z d)
         ('B a x #('R #('R b y c) z d))
         ('B a x #('R b y #('R c z d))))
     (make-T 'R (make-T 'B a x b) y (make-T 'B c z d))]
    [(color a x b)
     (make-T color a x b)]))

(define (get key tree cmpr)
  (and (T? tree)
       (match-let1 #(_ a p b) tree
         (if3 (comparator-compare cmpr key (car p))
              (get key a cmpr)
              p
              (get key b cmpr)))))

(define (insert key val tree cmpr)
  (define (ins tree)
    (if (E? tree)
      (make-T 'R #f (cons key val) #f)
      (match-let1 #(color a p b) tree
        (if3 (comparator-compare cmpr key (car p))
             (balance color (ins a) p b)
             (make-T color a (cons key val) b)
             (balance color a p (ins b))))))
  (match-let1 #(_ a p b) (ins tree)
    (make-T 'B a p b)))

(define (populate tree alist cmpr)
  ((rec (f tree alist)
     (if (null? alist)
       tree
       (f (insert (caar alist) (cdar alist) tree cmpr) (cdr alist))))
   tree alist))

(define (delete key tree cmpr)
  (define (del-min tree)
    (match tree
      [#(color #f p #f) (values p #f)]
      [#(color #f p b)  (values p b)]
      [#(color a p b)   (receive (min-p a.) (del-min a)
                          (values min-p (balance color a. p b)))]))
  (define (del tree)
    (if (E? tree)
      tree
      (match-let1 #(color a p b) tree
        (if3 (comparator-compare cmpr key (car p))
             (balance color (del a) p b)
             (if a
               (if b
                 (receive (min-p b.) (del-min b)
                   (balance color a min-p b.))
                 a)
               b)
             (balance color a p (del b))))))
  (match (del tree)
    [#f  #f] ;empty
    [#(_ a p b) (make-T 'B a p b)]))

;; aux fn
(define (%key-proc->comparator key=? key<?)
  (make-comparator #t key=?
                   (^[a b] (cond [(key=? a b) 0]
                                 [(key<? a b) -1]
                                 [else 1]))
                   #f))

;;
;; External interface
;;
(define-class <imap-meta> (<class>) ())

(define-class <imap> (<ordered-dictionary>)
  ((comparator :init-keyword :comparator)
   (tree :init-keyword :tree :init-form #f))
  :metaclass <imap-meta>)

;; API
(define (imap? x) (is-a? x <imap>))

;; API
(define make-imap
  (case-lambda
    [() (make-imap default-comparator)]
    [(cmpr)
     (unless (comparator? cmpr)
       (error "comparator required, but got:" cmpr))
     (make <imap> :comparator cmpr)]
    [(key=? key<?) (make-imap (%key-proc->comparator key=? key<?))])) 

;; API
(define alist->imap
  (case-lambda
    [(alist) (alist->imap alist default-comparator)]
    [(alist cmpr)
     (unless (comparator? cmpr)
       (error "comparator required, but got:" cmpr))
     (make <imap>
       :comparator cmpr :tree (populate #f alist cmpr))]
    [(alist key=? key<?)
     (alist->imap alist (%key-proc->comparator key=? key<?))]))

;; API
(define (tree-map->imap tree-map)
  (alist->imap (tree-map->alist tree-map)
                        (tree-map-comparator tree-map)))

;; API
(define (imap-empty? immap) (E? (~ immap'tree)))

;; API
(define (imap-exists? immap key)
  (boolean (get key (~ immap'tree) (~ immap'comparator))))

;; API
(define (imap-get immap key :optional default)
  (if-let1 p (get key (~ immap'tree) (~ immap'comparator))
    (cdr p)
    (if (undefined? default)
      (errorf "No such key in a imap ~s: ~s" immap key)
      default)))

;; API
(define (imap-put immap key val)
  (make <imap>
    :comparator (~ immap'comparator)
    :tree (insert key val (~ immap'tree) (~ immap'comparator))))

;; API
(define (imap-delete immap key)
  (make <imap>
    :comparator (~ immap'comparator)
    :tree (delete key (~ immap'tree) (~ immap'comparator))))

;; API
(define (imap-min immap)
  (define (descend tree)
    (match-let1 #(_ a p b) tree
      (if (E? a) p (descend a))))
  (let1 t (~ immap'tree)
    (and (not (E? t)) (descend t))))

;; API
(define (imap-max immap)
  (define (descend tree)
    (match-let1 #(_ a p b) tree
      (if (E? b) p (descend b))))
  (let1 t (~ immap'tree)
    (and (not (E? t)) (descend t))))

;; Fundamental iterators
(define (%imap-fold immap proc seed)
  (define (rec tree seed)
    (if (E? tree)
      seed
      (match-let1 #(_ a p b) tree
        (rec b (proc p (rec a seed))))))
  (rec (~ immap'tree) seed))

(define (%imap-fold-right immap proc seed)
  (define (rec tree seed)
    (if (E? tree)
      seed
      (match-let1 #(_ a p b) tree
        (rec a (proc p (rec b seed))))))
  (rec (~ immap'tree) seed))

;; Collection framework
(define-method call-with-iterator ((coll <imap>) proc :allow-other-keys)
  (if (imap-empty? coll)
    (proc (^[] #t) (^[] #t))
    (let1 q (make-queue)  ; only contains T
      (enqueue! q (~ coll'tree))
      (proc (^[] (queue-empty? q))
            (rec (next)
              (match-let1 #(c a p b) (dequeue! q)
                (if (E? a)
                  (if (E? b)
                    p
                    (begin (queue-push! q b) p))
                  (begin (queue-push! q (make-T c #f p b))
                         (queue-push! q a)
                         (next)))))))))

(define-method call-with-builder ((class <imap-meta>) proc
                                  :key (comparator default-comparator)
                                  :allow-other-keys)
  (define alist '())
  (proc (^p (push! alist p))
        (^[] (alist->imap alist comparator))))

;; A couple of conversion methods for the efficiency
(define-method coerce-to ((c <imap-meta>) (src <list>))
  (alist->imap src))
(define-method coerce-to ((c <imap-meta>) (src <tree-map>))
  (tree-map->imap src))

;; Dictionary interface
;; As a dictionary, it behaves as immutable dictionary.
(define-method dict-get ((immap <imap>) key :optional default)
  (imap-get immap key default))

(define-method dict-put! ((immap <imap>) key value)
  (errorf "imap is immutable:" immap))

(define-method dict-comparator ((immap <imap>))
  (~ immap'comparator))

(define-method dict-fold ((immap <imap>) proc seed)
  (%imap-fold immap (^[p s] (proc (car p) (cdr p) s)) seed))

(define-method dict-fold-right ((immap <imap>) proc seed)
  (%imap-fold-right immap (^[p s] (proc (car p) (cdr p) s)) seed))
