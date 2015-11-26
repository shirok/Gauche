;;;
;;; data.immutable-map - immutable tree map
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

(define-module data.immutable-map
  (use gauche.sequence)
  (use gauche.dictionary)
  (use gauche.record)
  (use data.queue)
  (use util.match)
  (use srfi-114)
  (export make-immutable-map immutable-map?
          immutable-map-empty?
          immutable-map-exists? immutable-map-get immutable-map-put
          immutable-map-delete
          immutable-map-min immutable-map-max)
  )
(select-module data.immutable-map)

;;
;; Internal implementation
;;

(define-record-type T #t #t color left elem right)
;; NB: We just use #f as E node.
(define (E? x) (not x))

(define balance
  (match-lambda*
    [(or ('B ($ T 'R ($ T 'R a x b) y c) z d)
         ('B ($ T 'R a x ($ T 'R b y c)) z d)
         ('B a x ($ T 'R ($ T 'R b y c) z d))
         ('B a x ($ T 'R b y ($ T 'R c z d))))
     (make-T 'R (make-T 'B a x b) y (make-T 'B c z d))]
    [(color a x b)
     (make-T color a x b)]))

(define (get key tree cmpr)
  (and (T? tree)
       (match-let1 ($ T _ a p b) tree
         (if3 (comparator-compare cmpr key (car p))
              (get key a cmpr)
              p
              (get key b cmpr)))))

(define (insert key val tree cmpr)
  (define (ins tree)
    (if (E? tree)
      (make-T 'R #f (cons key val) #f)
      (match-let1 ($ T color a p b) tree
        (if3 (comparator-compare cmpr key (car p))
             (balance color (ins a) p b)
             (make-T color a (cons key val) b)
             (balance color a p (ins b))))))
  (match-let1 ($ T _ a p b) (ins tree)
    (make-T 'B a p b)))

(define (delete key tree cmpr)
  (define (del-min tree)
    (match tree
      [($ T color #f p #f) (values p #f)]
      [($ T color #f p b)  (values p b)]
      [($ T color a p b)   (receive (min-p a.) (del-min a)
                             (values min-p (balance color a. p b)))]))
  (define (del tree)
    (if (E? tree)
      tree
      (match-let1 ($ T color a p b) tree
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
    [($ T _ a p b) (make-T 'B a p b)]))

;;
;; External interface
;;
(define-class <immutable-map> (<ordered-dictionary>)
  ((comparator :init-keyword :comparator)
   (tree :init-keyword :tree :init-form #f)))

;; API
(define (immutable-map? x) (is-a? x <immutable-map>))

;; API
(define make-immutable-map
  (case-lambda
    [() (make-immutable-map default-comparator)]
    [(cmpr)
     (unless (comparator? cmpr)
       (error "comparator required, but got:" cmpr))
     (make <immutable-map> :comparator cmpr)]
    [(key=? key<?)
     (make-immutable-map (make-comparator #t key=?
                                      (^[a b]
                                        (cond [(key=? a b) 0]
                                              [(key<? a b) -1]
                                              [else 1]))
                                      #f))]))

;; API
(define (immutable-map-empty? ftree) (E? (~ ftree'tree)))

;; API
(define (immutable-map-exists? ftree key)
  (boolean (get key (~ ftree'tree) (~ ftree'comparator))))

;; API
(define (immutable-map-get ftree key :optional default)
  (if-let1 p (get key (~ ftree'tree) (~ ftree'comparator))
    (cdr p)
    (if (undefined? default)
      (errorf "No such key in a immutable-map ~s: ~s" ftree key)
      default)))

;; API
(define (immutable-map-put ftree key val)
  (make <immutable-map>
    :comparator (~ ftree'comparator)
    :tree (insert key val (~ ftree'tree) (~ ftree'comparator))))

;; API
(define (immutable-map-delete ftree key)
  (make <immutable-map>
    :comparator (~ ftree'comparator)
    :tree (delete key (~ ftree'tree) (~ ftree'comparator))))

;; API
(define (immutable-map-min ftree)
  (define (descend tree)
    (match-let1 ($ T _ a p b) tree
      (if (E? a) p (descend a))))
  (let1 t (~ ftree'tree)
    (and (not (E? t)) (descend t))))

;; API
(define (immutable-map-max ftree)
  (define (descend tree)
    (match-let1 ($ T _ a p b) tree
      (if (E? b) p (descend b))))
  (let1 t (~ ftree'tree)
    (and (not (E? t)) (descend t))))

;; Fundamental iterators
(define (%immutable-map-fold ftree proc seed)
  (define (rec tree seed)
    (if (E? tree)
      seed
      (match-let1 ($ T _ a p b) tree
        (rec b (proc p (rec a seed))))))
  (rec (~ ftree'tree) seed))

(define (%immutable-map-fold-right ftree proc seed)
  (define (rec tree seed)
    (if (E? tree)
      seed
      (match-let1 ($ T _ a p b) tree
        (rec a (proc p (rec b seed))))))
  (rec (~ ftree'tree) seed))

;; Collection framework
(define-method call-with-iterator ((coll <immutable-map>) proc :allow-other-keys)
  (if (immutable-map-empty? coll)
    (proc (^[] #t) (^[] #t))
    (let1 q (make-queue)  ; only contains T
      (enqueue! q (~ coll'tree))
      (proc (^[] (queue-empty? q))
            (rec (next)
              (match-let1 ($ T c a p b) (dequeue! q)
                (if (E? a)
                  (if (E? b)
                    p
                    (begin (queue-push! q b) p))
                  (begin (queue-push! q (make-T c #f p b))
                         (queue-push! q a)
                         (next)))))))))

;; Dictionary interface
;; As a dictionary, it behaves as immutable dictionary.
(define-method dict-get ((ftree <immutable-map>) key :optional default)
  (immutable-map-get ftree key default))

(define-method dict-put! ((ftree <immutable-map>) key value)
  (errorf "immutable-map is immutable:" ftree))

(define-method dict-comparator ((ftree <immutable-map>))
  (~ ftree'comparator))

(define-method dict-fold ((ftree <immutable-map>) proc seed)
  (%immutable-map-fold ftree (^[p s] (proc (car p) (cdr p) s)) seed))

(define-method dict-fold-right ((ftree <immutable-map>) proc seed)
  (%immutable-map-fold-right ftree (^[p s] (proc (car p) (cdr p) s)) seed))


