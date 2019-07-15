;;;
;;; priority-map.scm - a map, with ability to pull highest/lowest value
;;;
;;;   Copyright (c) 2016-2019  Shiro Kawai  <shiro@acm.org>
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

;; Priority map is a map whose entries are sorted by their values.
;; If what you need is just a priority queue, use data.heap.
;; Priority map is useful when you also need a quick lookup based on key.

(define-module data.priority-map
  (use gauche.sequence)
  (use gauche.dictionary)
  (use util.match)
  (export <priority-map>
          make-priority-map
          priority-map-min
          priority-map-max
          priority-map-pop-min!
          priority-map-pop-max!))
(select-module data.priority-map)

(define-class <priority-map> (<ordered-dictionary>)
  (;; private
   (key-map    :init-keyword :key-map)      ; key -> value  hashtable
   (key-cmpr   :init-keyword :key-cmpr)     ; key comparator, needs hash
   (value-map  :init-keyword :value-map)    ; value -> [key]  treemap
   (value-cmpr :init-keyword :value-cmpr)   ; value comparator, needs order
   ))

(define (make-priority-map :key
                           (key-comparator default-comparator)
                           (value-comparator default-comparator))
  (make <priority-map>
    :key-map    (make-hash-table key-comparator)
    :key-cmpr   key-comparator
    :value-map  (make-tree-map value-comparator)
    :value-cmpr value-comparator))

;; Sequence protocol
;; Iterator iterates increasing order of values
(define-method size-of ((pm <priority-map>))
  (hash-table-size (~ pm'key-map)))

(define-method call-with-iterator ((coll <priority-map>) proc :allow-other-keys)
  (define gen (x->generator (~ coll'value-map)))
  (define keys '())
  (define val #f)
  (define (get1)
    (let1 buf (gen)
      (if (eof-object? buf)
        (set!-values (keys val) (values buf buf))
        (set!-values (keys val) (values (car buf) (cdr buf))))))
  (get1)
  (proc (cut eof-object? keys)
        (^[] (rlet1 p (cons (pop! keys) val)
               (when (null? keys)
                 (get1))))))

;; Dictionary protocol
(define-method dict-get ((pmap <priority-map>) key :optional default)
  (if (undefined? default)
    (hash-table-get (~ pmap 'key-map) key)
    (hash-table-get (~ pmap 'key-map) key default)))

(define-method dict-exists? ((pmap <priority-map>) key)
  (hash-table-exists? (~ pmap 'key-map) key))

(define-method dict-put! ((pmap <priority-map>) key value)
  (let ([kmap (~ pmap 'key-map)]
        [kcmp (~ pmap 'key-cmpr)]
        [vmap (~ pmap 'value-map)]
        [vcmp (~ pmap 'value-cmpr)])
    (and-let* ([v (hash-table-get kmap key #f)]
               [ (not (comparator-equal? vcmp v value)) ])
      ($ tree-map-update! vmap v
         (cute remove key <> (comparator-equality-predicate kcmp)) '()))
    (tree-map-push! vmap value key)
    (hash-table-put! kmap key value)))

(define-method dict-delete! ((pmap <priority-map>) key)
  (let ([kmap (~ pmap 'key-map)]
        [kcmp (~ pmap'key-cmpr)]
        [vmap (~ pmap 'value-map)])
    (and-let1 val (hash-table-get kmap key #f)
      (let1 ks (tree-map-get vmap val)
        (if (null? (cdr ks))
          (tree-map-delete! vmap val)
          (tree-map-put! vmap val (remove (cut =? kcmp key <>) ks)))))
      (hash-table-delete! kmap key)))

(define-method dict-clear! ((pmap <priority-map>))
  (dict-clear! (~ pmap'key-map))
  (dict-clear! (~ pmap'value-map)))

(define-method dict-comparator ((pmap <priority-map>))
  (hash-table-comparator (~ pmap'key-map)))

(define-method dict-fold ((pmap <priority-map>) proc seed)
  ($ tree-map-fold (~ pmap 'value-map)
     (^[v ks s] (fold (^[k s] (proc k v s)) s ks)) seed))

(define-method dict-fold-right ((pmap <priority-map>) proc seed)
  ($ tree-map-fold-right (~ pmap 'value-map)
     (^[v ks s] (fold-right (^[k s] (proc k v s)) s ks) seed)))

;; specific stuff

;; Returns (k . v) where v is min or max; #f if pmap is empty
(define (priority-map-min pmap)
  (and-let* ([p (tree-map-min (~ pmap'value-map))])
    (cons (cdr p) (car p))))

(define (priority-map-max pmap)
  (and-let* ([p (tree-map-max (~ pmap'value-map))])
    (cons (cdr p) (car p))))

(define (priority-map-pop-min! pmap)
  (let ([kmap (~ pmap 'key-map)]
        [vmap (~ pmap 'value-map)])
    (match (tree-map-min vmap)
      [(val . (key . keys))
       (if (null? keys)
         (tree-map-delete! vmap val)
         (tree-map-put! vmap val keys))
       (hash-table-delete! kmap key)
       (cons key val)]
      [#f #f])))

(define (priority-map-pop-max! pmap)
  (let ([kmap (~ pmap 'key-map)]
        [vmap (~ pmap 'value-map)])
    (match (tree-map-max vmap)
      [(val . (key . keys))
       (if (null? keys)
         (tree-map-delete! vmap val)
         (tree-map-put! vmap val keys))
       (hash-table-delete! kmap key)
       (cons key val)]
      [#f #f])))


                          

  
