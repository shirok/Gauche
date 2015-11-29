;;;
;;; collection.scm - collection generics
;;;
;;;   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

;; Defines generic operations over collection.   A collection is
;; a set of objects, possibly containing infinite objects.

(define-module gauche.collection
  (use srfi-1)
  (export call-with-iterator with-iterator call-with-iterators
          call-with-builder  with-builder
          fold fold2 fold3 map map-to map-accum for-each
          fold$ fold2$ fold3$ map$ for-each$
          find find-min find-max find-min&max
          filter filter-to remove remove-to partition partition-to
          size-of lazy-size-of coerce-to
          group-collection)
  )
(select-module gauche.collection)

;; utility - we can't depend on data.queue, so this is a simple
;; alternative.
(define (make-queue)   (let1 anchor (list #f) (cons anchor anchor)))
(define (enqueue! q x) (set! (cddr q) (list x)) (set! (cdr q) (cddr q)))
(define (dequeue! q)   (if (null? (cdar q))
                         (error "queue is empty" q)
                         (rlet1 v (cadar q) (set! (cdar q) (cddar q)))))
(define (queue->list q) (cdar q))

;;-------------------------------------------------
;; Call-with-iterator - the fundamental iterator
;;

(define-syntax with-iterator
  (syntax-rules ()
    [(_ (coll end? next . opts) . body)
     (call-with-iterator coll (^[end? next] . body) . opts)]))

(define-method call-with-iterator ((coll <list>) proc
                                   :key (start #f) :allow-other-keys)
  (let1 p (if start (list-tail coll start) coll)
    (proc (cut null? p) (^[] (pop! p)))))

(define-method call-with-iterator ((coll <vector>) proc
                                   :key (start 0) :allow-other-keys)
  (let1 len (vector-length coll)
    (proc (cut >= start len)
          (^[] (begin0 (vector-ref coll start) (inc! start))))))

(define-method call-with-iterator ((coll <weak-vector>) proc
                                   :key (start 0) :allow-other-keys)
  (let1 len (weak-vector-length coll)
    (proc (cut >= start len)
          (^[] (begin0 (weak-vector-ref coll start) (inc! start))))))

(define-method call-with-iterator ((coll <string>) proc
                                   :key (start #f) :allow-other-keys)
  (let* ([s  (open-input-string (if start (string-copy coll start) coll))]
         [ch (read-char s)])
    (proc (cut eof-object? ch)
          (^[] (rlet1 c ch
                 (set! ch (read-char s)))))))

(define-method call-with-iterator ((coll <hash-table>) proc :allow-other-keys)
  (let ([eof-marker (cons #f #f)]
        [iter (%hash-table-iter coll)])
    (receive (k v) (iter eof-marker)
      (proc (cut eq? k eof-marker)
            (^[] (begin0 (cons k v)
                   (set!-values (k v) (iter eof-marker))))))))

(define-method call-with-iterator ((coll <tree-map>) proc :allow-other-keys)
  (let ([eof-marker (cons #f #f)]
        [iter (%tree-map-iter coll)])
    (receive (k v) (iter eof-marker #f)
      (proc (cut eq? k eof-marker)
            (^[] (begin0 (cons k v)
                   (set!-values (k v) (iter eof-marker #f))))))))

;; NB: Do not depend on srfi-14.scm.
(define-method call-with-iterator ((coll <char-set>) proc :allow-other-keys)
  (let* ([ranges ((with-module gauche.internal %char-set-ranges) coll)]
         [cursor (if (null? ranges) #f (caar ranges))])
    (proc (^[] (not cursor))
          (^[] (rlet1 c (integer->char cursor)
                 (cond [(< cursor (cdar ranges)) (inc! cursor)]
                       [(null? (cdr ranges)) (set! cursor #f)]
                       [else (pop! ranges) (set! cursor (caar ranges))]))))))

;; n-ary case aux. proc
(define (call-with-iterators colls proc)
  (let loop ([colls colls]
             [eprocs '()]
             [nprocs '()])
    (if (null? colls)
      (proc (reverse! eprocs) (reverse! nprocs))
      (with-iterator ((car colls) end? next)
        (loop (cdr colls) (cons end? eprocs) (cons next nprocs))))))

;;-------------------------------------------------
;; Call-with-builder - the fundamental constructor
;;

(define-syntax with-builder
  (syntax-rules ()
    [(_ (class add! get . opts) . body)
     (call-with-builder class (^[add! get] . body) . opts)]))

(define-method call-with-builder ((class <list-meta>) proc :allow-other-keys)
  (let1 q (make-queue)
    (proc (cut enqueue! q <>) (cut queue->list q))))

(define-method call-with-builder ((class <vector-meta>) proc
                                  :key (size #f) :allow-other-keys)
  (if size
    (let ([v (make-vector size)]
          [i 0])
      (proc (^[item] (when (< i size)
                       (vector-set! v i item)
                       (inc! i)))
            (^[] v)))
    (let1 q (make-queue)
      (proc (cut enqueue! q <>)
            (cut list->vector (queue->list q))))))

(define-method call-with-builder ((class <weak-vector-meta>) proc
                                  :key (size #f) :allow-other-keys)
  (if size
    (let ([v (make-weak-vector size)]
          [i 0])
      (proc (^[item] (when (< i size)
                       (weak-vector-set! v i item)
                       (inc! i)))
            (^[] v)))
    (let ([q (make-queue)]
          [cnt 0])
      (proc (^[item] (enqueue! q item) (inc! cnt))
            (^[] (let1 v (make-weak-vector cnt)
                   (do ([i 0 (+ i 1)])
                       [(= i cnt) v]
                     (weak-vector-set! v i (dequeue! q)))))))))

(define-method call-with-builder ((class <string-meta>) proc :allow-other-keys)
  (let1 s (open-output-string)
    (proc (^[item]
            (unless (char? item)
              (error "character required to build a string, but got" item))
            (write-char item s))
          (^[] (get-output-string s)))))

(define-method call-with-builder ((class <hash-table-meta>) proc
                                  :key (comparator #f) (type #f)
                                  :allow-other-keys)
  (let1 h (make-hash-table (or type comparator 'eq?))
    (proc (^[item]
            (unless (pair? item)
              (error "pair required to build a hashtable, but got" item))
            (hash-table-put! h (car item) (cdr item)))
          (^[] h))))

(define-method call-with-builder ((class <tree-map-meta>) proc
                                  :key (comparator default-comparator)
                                       (key=? #f) (key<? #f)
                                  :allow-other-keys)
  (let1 tree (if (and key=? key<?)
               (make-tree-map key=? key<?)
               (make-tree-map comparator))
    (proc (^[item]
            (unless (pair? item)
              (error "pair required to build a tree-map, but got" item))
            (tree-map-put! tree (car item) (cdr item)))
          (^[] tree))))

;; NB: size key is unused
(define-method call-with-builder ((class <char-set-meta>) proc
                                  :key (size #f) :allow-other-keys)
  (let1 cs (char-set)
    (proc (^c (unless (char? c)
                (error "character required to build a char-set, but got" c))
              ((with-module gauche.internal %char-set-add-chars!) cs (list c)))
          (^[] cs))))

;; utility.  return minimum size of collections if it's easily known, or #f.
(define (maybe-minimum-size col more)
  (let1 size (and-let* ([siz (lazy-size-of col)]
                        [ (integer? siz) ])
               siz)
    (if (or (null? more) (not size))
      size  ;; short path
      (let loop ([cols more]
                 [r    size])
        (if (null? cols)
          r
          (let1 size (lazy-size-of (car cols))
            (and (integer? size)
                 (loop (cdr cols) (min r size)))))))))

;;----------------------------------------------------
;; Derived operations
;;

;; fold -------------------------------------------------

(define-syntax define-fold-k
  (syntax-rules ()
    [(gen-fold-k name (seed ...))
     (define-method name (proc seed ... (coll <collection>) . more)
       (if (null? more)
         (with-iterator (coll end? next)
           (let loop ((seed seed) ...)
             (if (end?)
               (values seed ...)
               (receive (seed ...) (proc (next) seed ...)
                 (loop seed ...)))))
         (call-with-iterators
          (cons coll more)
          (^[ends? nexts]
            (let loop ((seed seed) ...)
              (if (any (cut <>) ends?)
                (values seed ...)
                (receive (seed ...)
                    (apply proc (fold-right (^[p r] (cons (p) r))
                                            (list seed ...)
                                            nexts))
                  (loop seed ...))))))
         ))]))

;; generic way.   This shadows builtin fold.
(define-fold-k fold (knil))

;; for list arguments, builtin fold is faster.
(define-method fold (proc knil (coll <list>))
  ((with-module gauche fold) proc knil coll))

(define-method fold (proc knil (coll <list>) (coll2 <list>))
  ((with-module gauche fold) proc knil coll coll2))

;; 2- and 3- seed values
(define-fold-k fold2 (knil1 knil2))
(define-fold-k fold3 (knil1 knil2 knil3))

;; partial applied version
(define-method fold$ (proc)
  (^[knil . lists] (apply fold proc knil lists)))
(define-method fold$ (proc knil)
  (^ lists (apply fold proc knil lists)))

(define-method fold2$ (proc knil1 knil2)
  (^ lists (apply fold2 proc knil1 knil2 lists)))

(define-method fold3$ (proc knil1 knil2 knil3)
  (^ lists (apply fold3 proc knil1 knil2 knil3 lists)))

;; map --------------------------------------------------

;; generic way.  this shadows builtin map.
(define-method map (proc (coll <collection>) . more)
  (if (null? more)
    (with-iterator (coll end? next)
      (do ([q (make-queue)])
          [(end?) (queue->list q)]
        (enqueue! q (proc (next)))))
    (let1 %map (with-module gauche map)
      (call-with-iterators
       (cons coll more)
       (^[ends? nexts]
         (do ([q (make-queue)])
             [(any (cut <>) ends?)
              (queue->list q)]
           (enqueue! q (apply proc (%map (cut <>) nexts))))))
      )))

;; for list arguments, built-in map is much faster.
(define-method map (proc (coll <list>) . more)
  (let1 %map (with-module gauche map)
    (if (null? more)
      (%map proc coll)
      (if (every pair? more)
        (apply %map proc coll more)
        (next-method)))))

;; redefine map$ to use generic version of map
(define (map$ proc) (pa$ map proc))

;; map-to -----------------------------------------------

;; generic way.
(define-method map-to ((class <class>) proc (coll <collection>) . more)
  (if (null? more)
    (with-builder (class add! get :size (size-of coll))
      (with-iterator (coll end? next)
        (do ()
            [(end?) (get)]
          (add! (proc (next))))))
    (with-builder (class add! get :size (maybe-minimum-size coll more))
      (call-with-iterators
       (cons coll more)
       (^[ends? nexts]
         (do ()
             [(any (cut <>) ends?) (get)]
           (add! (apply proc (map (cut <>) nexts)))))))))

;; map-to <list> is equivalent to map.
(define-method map-to ((class <list-meta>) proc coll . more)
  (apply map proc coll more))

;; map-accum --------------------------------------------

;; Like Haskell's mapAccumL, but the order of args are different.
;; 1-ary case:  ((elt, seed) -> (res, seed)), seed, [elt] -> ([res], seed)

(define-method map-accum (proc knil (coll <collection>) . more)
  (if (null? more)
    (receive (res knil)
        (fold2 (^[elt lis knil]
                 (receive (res knil) (proc elt knil)
                   (values (cons res lis) knil)))
               '() knil coll)
      (values (reverse! res) knil))
    (call-with-iterators
     (cons coll more)
     (^[ends? nexts]
       (let loop ([lis '()] [knil knil])
         (if (any (cut <>) ends?)
           (values (reverse! lis) knil)
           (receive (res knil)
               (apply proc (fold-right (^[p r] (cons (p) r))
                                       (list knil)
                                       nexts))
             (loop (cons res lis) knil))))))
    ))

;; for-each ---------------------------------------------

;; generic way.  this shadows builtin for-each.
(define-method for-each (proc (coll <collection>) . more)
  (if (null? more)
    (with-iterator (coll end? next)
      (until (end?) (proc (next))))
    (let1 %map (with-module gauche map)
      (call-with-iterators
       (cons coll more)
       (^[ends? nexts]
         (until (any (cut <>) ends?)
           (apply proc (%map (cut <>) nexts))))))))

;; for list arguments, built-in for-each is much faster.
(define-method for-each (proc (coll <list>) . more)
  (let1 %for-each (with-module gauche for-each)
    (if (null? more)
        (%for-each proc coll)
        (if (every pair? more)
            (apply %for-each proc coll more)
            (next-method)))))

;; redefine for-each$ to use generic version of for-each
(define (for-each$ proc) (pa$ for-each proc))

;; size-of ----------------------------------------------

;; generic way
(define-method size-of ((coll <collection>))
  (fold (^[e r] (+ r 1)) 0 coll))

(define-method lazy-size-of ((coll <collection>))
  (delay (size-of coll)))

;; shortcut
(define-method size-of ((coll <list>))        (length coll))
(define-method size-of ((coll <vector>))      (vector-length coll))
(define-method size-of ((coll <weak-vector>)) (weak-vector-length coll))
(define-method size-of ((coll <string>))      (string-length coll))
(define-method size-of ((coll <char-set>))    (char-set-size coll))

(define-method lazy-size-of ((coll <list>))        (length coll))
(define-method lazy-size-of ((coll <vector>))      (vector-length coll))
(define-method lazy-size-of ((coll <weak-vector>)) (weak-vector-length coll))
(define-method lazy-size-of ((coll <string>))      (string-length coll))

;; find -------------------------------------------------

;; generic way
(define-method find (pred (coll <collection>))
  (with-iterator (coll end? next)
    (let loop ()
      (if (end?)
        #f
        (let1 e (next)
          (if (pred e) e (loop)))))))

;; shortcut
(define-method find (pred (coll <list>))
  ((with-module srfi-1 find) pred coll))

;; find-min, find-max, find-min&max ---------------------

(define-method find-min ((coll <collection>)
                         :key (key identity)
                              (compare <)
                              (default #f))
  (%find-minmax-1 coll key compare default))

(define-method find-max ((coll <collection>)
                         :key (key identity)
                              (compare <)
                              (default #f))
  (%find-minmax-1 coll key (complement compare) default))

(define (%find-minmax-1 coll key compare default)
  (with-iterator (coll end? next)
    (if (end?)
      default
      (let1 elt (next)
        (let loop ([val (key elt)]
                   [elt elt])
          (if (end?)
            elt
            (let* ([e (next)]
                   [v (key e)])
              (if (compare v val)
                (loop v e)
                (loop val elt)))))))))

(define-method find-min&max ((coll <collection>)
                             :key (key identity)
                                  (compare <)
                                  (default #f)
                                  (default-min default)
                                  (default-max default))
  (with-iterator (coll end? next)
    (if (end?)
      (values default-min default-max)
      (let1 elt (next)
        (let loop ([minval (key elt)]
                   [minelt elt]
                   [maxval (key elt)]
                   [maxelt elt])
          (if (end?)
            (values minelt maxelt)
            (let* ([e (next)]
                   [v (key e)])
              (cond [(compare v minval) (loop v e maxval maxelt)]
                    [(compare maxval v) (loop minval minelt v e)]
                    [else (loop minval minelt maxval maxelt)]))))))))

;; filter -----------------------------------------------

;; generic way
(define-method filter (pred (coll <collection>))
  (let1 q (make-queue)
    (with-iterator (coll end? next)
      (until (end?) (let1 e (next) (when (pred e) (enqueue! q e))))
      (queue->list q))))

(define-method filter-to ((class <class>) pred (coll <collection>))
  (with-builder (class add! get)
    (with-iterator (coll end? next)
      (do ()
          [(end?) (get)]
        (let1 e (next) (when (pred e) (add! e)))))))

;; shortcut
(define-method filter (pred (coll <list>))
  ((with-module srfi-1 filter) pred coll))

(define-method filter-to ((class <list-meta>) pred coll)
  (filter pred coll))

;; remove -----------------------------------------------

;; generic way
(define-method remove (pred (coll <collection>))
  (let1 q (make-queue)
    (with-iterator (coll end? next)
      (until (end?) (let1 e (next) (unless (pred e) (enqueue! q e))))
      (queue->list q))))

(define-method remove-to ((class <class>) pred (coll <collection>))
  (with-builder (class add! get)
    (with-iterator (coll end? next)
      (do ()
          [(end?) (get)]
        (let1 e (next) (unless (pred e) (add! e)))))))

;; shortcut
(define-method remove (pred (coll <list>))
  ((with-module srfi-1 remove) pred coll))

(define-method remove-to ((class <list-meta>) pred coll)
  (remove pred coll))

;; partition ---------------------------------------------

;; generic way
(define-method partition (pred (coll <collection>))
  (with-iterator (coll end? next)
    (let loop ([y '()] [n '()])
      (if (end?)
        (values (reverse y) (reverse n))
        (let1 e (next)
          (if (pred e)
            (loop (cons e y) n)
            (loop y (cons e n))))))))

(define-method partition-to ((class <class>) pred (coll <collection>))
  (with-builder (class add0! get0)
    (with-builder (class add1! get1)
      (with-iterator (coll end? next)
        (do ()
            [(end?) (values (get0) (get1))]
          (let1 e (next)
            (if (pred e) (add0! e) (add1! e))))))))

;; shortcut
(define-method partition (pred (coll <list>))
  ((with-module srfi-1 partition) pred coll))

(define-method partition-to ((class <list-meta>) pred coll)
  (partition pred coll))

;; coercion ---------------------------------------------

(define-method coerce-to ((class <class>) (coll <collection>))
  (with-builder (class add! get :size (size-of coll))
    (with-iterator (coll end? next)
      (do ()
          [(end?) (get)]
        (add! (next))))))

;; shortcut
(define-method coerce-to ((class <list-meta>) (coll <list>))
  (list-copy coll))
(define-method coerce-to ((class <list-meta>) (coll <vector>))
  (vector->list coll))
(define-method coerce-to ((class <list-meta>) (coll <string>))
  (string->list coll))
(define-method coerce-to ((class <vector-meta>) (coll <list>))
  (list->vector coll))
(define-method coerce-to ((class <string-meta>) (coll <list>))
  (list->string coll))

;; group-collection---------------------------------------
;;  gather elements with the same key value.
;;  cf. group-sequence in gauche.sequence

(define-method group-collection ((col <collection>)
                                 :key ((:key key-proc) identity)
                                      ((:test test-proc) eqv?))
  (fold (^[b r] (cons (reverse! (cdr b)) r))
        '()
        (fold (^[elt buckets]
                (let1 key (key-proc elt)
                  (cond [(assoc key buckets test-proc)
                         => (^p (push! (cdr p) elt) buckets)]
                        [else (cons (list key elt) buckets)])))
              '()
              col)))

