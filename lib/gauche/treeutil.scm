;;;
;;; auxiliary treemap utilities.  to be autoloaded.
;;;
;;;   Copyright (c) 2007-2019  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.treeutil
  (export make-tree-map tree-map-empty?
          tree-map-min tree-map-max tree-map-pop-min! tree-map-pop-max!
          tree-map-seek tree-map-fold tree-map-fold-right
          tree-map-map tree-map-for-each
          tree-map-keys tree-map-values
          tree-map->alist alist->tree-map
          tree-map-compare-as-sets
          tree-map-compare-as-sequences)
  )
(select-module gauche.treeutil)

(define make-tree-map
  (case-lambda
    [() (%make-tree-map default-comparator)]
    [(cmp)
     (if (comparator? cmp)
       (begin
         (unless (comparator-ordered? cmp)
           (error "make-tree-map needs an ordered comparator, but got:" cmp))
         (%make-tree-map cmp))
       (%make-tree-map (make-comparator/compare #t #t cmp #f)))]
    [(=? <?) (%make-tree-map (make-comparator #t =? <? #f))]))

(define (tree-map-empty? tm) (zero? (tree-map-num-entries tm)))

(define (tree-map-min tm)
  ((with-module gauche.internal %tree-map-bound) tm #t #f))
(define (tree-map-max tm)
  ((with-module gauche.internal %tree-map-bound) tm #f #f))
(define (tree-map-pop-min! tm)
  ((with-module gauche.internal %tree-map-bound) tm #t #t))
(define (tree-map-pop-max! tm)
  ((with-module gauche.internal %tree-map-bound) tm #f #t))

(define (%tree-map-fold tm kons knil backward)
  (assume-type tm <tree-map>)
  (let ((eof (cons #f #f))              ;marker
        (i ((with-module gauche.internal %tree-map-iter) tm)))
    (let loop ((r knil))
      (receive (k v) (i eof backward)
        (if (eq? k eof)
          r
          (loop (kons k v r)))))))

(define (tree-map-fold tm kons knil)
  (%tree-map-fold tm kons knil #f))

(define (tree-map-fold-right tm kons knil)
  (%tree-map-fold tm kons knil #t))

(define (tree-map-seek tm pred succ fail)
  (assume-type tm <tree-map>)
  (let ((eof (cons #f #f))              ;marker
        (i ((with-module gauche.internal %tree-map-iter) tm)))
    (let loop ()
      (receive [k v] (i eof #f)
        (cond [(eq? k eof) (fail)]
              [(pred k v) => (^r (succ r k v))]
              [else (loop)])))))

(define (tree-map-map tm proc)
  (tree-map-fold-right tm (lambda (k v r) (cons (proc k v) r)) '()))

(define (tree-map-for-each tm proc)
  (tree-map-fold tm (lambda (k v r) (proc k v) r) (undefined)))

(define (tree-map-keys tm)
  (tree-map-fold-right tm (lambda (k v r) (cons k r)) '()))

(define (tree-map-values tm)
  (tree-map-fold-right tm (lambda (k v r) (cons v r)) '()))

(define (tree-map->alist tm)
  (tree-map-fold-right tm acons '()))

(define (alist->tree-map alist . args)
  (rlet1 tm (apply make-tree-map args)
    (dolist (kv alist)
      (tree-map-put! tm (car kv) (cdr kv)))))

;; Compare two tree-maps as sets.
(define (tree-map-compare-as-sets tm1 tm2
                                  :optional (value=? equal?)
                                  (fallback (undefined)))
  (define eof (cons #f #f))
  (define (fail)
    (if (undefined? fallback)
      (error "tree-maps can't be ordered:" tm1 tm2)
      fallback))
  (define key-cmp
    (let ([c1 (tree-map-comparator tm1)]
          [c2 (tree-map-comparator tm2)])
      (cond [(and c1 c2 (equal? c1 c2)) c1]
            [(or c1 c2) (error "tree-maps with different comparators can't be \
                                compared:" tm1 tm2)]
            [else (error "tree-maps don't have comparators and can't be \
                          compared:" tm1 tm2)])))
  (if (eq? tm1 tm2)
    0                                   ;fast path
    (let ([i1 ((with-module gauche.internal %tree-map-iter) tm1)]
          [i2 ((with-module gauche.internal %tree-map-iter) tm2)])
      (define (loop k1 v1 k2 v2 r)
        (if (eq? k1 eof)
          (if (eq? k2 eof)
            r
            (if (<= r 0) -1 (fail)))
          (if (eq? k2 eof)
            (if (>= r 0) 1 (fail))
            (case (comparator-compare key-cmp k1 k2)
              [(0)  (if (value=? v1 v2)
                      (receive (k1 v1) (i1 eof #f)
                        (receive (k2 v2) (i2 eof #f)
                          (loop k1 v1 k2 v2 r)))
                      (fail))]
              [(-1) (if (>= r 0)
                      (receive (k1 v1) (i1 eof #f)
                        (loop k1 v1 k2 v2 1))
                      (fail))]
              [else (if (<= r 0)
                      (receive (k2 v2) (i2 eof #f)
                        (loop k1 v1 k2 v2 -1))
                      (fail))]))))
      (receive (k1 v1) (i1 eof #f)
        (receive (k2 v2) (i2 eof #f)
          (loop k1 v1 k2 v2 0))))))

;; Compare tree maps as sequence.
;; Returns as soon as difference is found.
;; This gives total ordering, while compare-as-sets only gives partial ordering.
(define (tree-map-compare-as-sequences tm1 tm2
                                       :optional (value-cmp default-comparator))
  (define eof (cons #f #f))
  (define key-cmp
    (let ([c1 (tree-map-comparator tm1)]
          [c2 (tree-map-comparator tm2)])
      (cond [(and c1 c2 (equal? c1 c2)) c1]
            [(or c1 c2) (error "tree-maps with different comparators can't be \
                                compared:" tm1 tm2)]
            [else (error "tree-maps don't have comparators and can't be \
                          compared:" tm1 tm2)])))
  (if (eq? tm1 tm2)
    0                                   ;fast path
    (let ([i1 ((with-module gauche.internal %tree-map-iter) tm1)]
          [i2 ((with-module gauche.internal %tree-map-iter) tm2)])
      (define (loop k1 v1 k2 v2)
        (if (eq? k1 eof)
          (if (eq? k2 eof) 0 -1)
          (if (eq? k2 eof)
            1
            (case (comparator-compare key-cmp k1 k2)
              [(0)  (case (comparator-compare value-cmp v1 v2)
                      [(0) (receive (k1 v1) (i1 eof #f)
                             (receive (k2 v2) (i2 eof #f)
                               (loop k1 v1 k2 v2)))]
                      [else => identity])]
              [else => identity]))))
      (receive (k1 v1) (i1 eof #f)
        (receive (k2 v2) (i2 eof #f)
          (loop k1 v1 k2 v2))))))


;; Gauche-specific way to extend default comparator to handle mappings
;; NB: If <mapping> is synonym to <tree-map>, we probably should put
;; this in libdict.scm.
(define-method object-compare ((a <tree-map>) (b <tree-map>))
  (tree-map-compare-as-sequences a b))

(define-method object-equal? ((a <tree-map>) (b <tree-map>))
  (= (tree-map-compare-as-sequences a b)) 0)

