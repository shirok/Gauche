;;;
;;; srfi-217 - Integer sets
;;;
;;;   Copyright (c) 2021  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi-217
  (use gauche.record)
  (use gauche.sequence)
  (use util.match)
  (export iset iset-unfold make-range-iset
          iset? iset-contains? iset-empty? iset-disjoint?
          iset-member iset-min iset-max
          iset-adjoin iset-adjoin! iset-delete iset-delete!
          iset-delete-all iset-delete-all! iset-search iset-search!
          iset-delete-min iset-delete-min! iset-delete-max iset-delete-max!
          iset-size iset-find iset-count iset-any? iset-every?
          iset-map iset-for-each iset-fold iset-fold-right
          iset-filter iset-filter! iset-remove iset-remove!
          iset-partition iset-partition!
          iset-copy iset->list list->iset list->iset!
          iset=? iset<? iset>? iset<=? iset>=?
          iset-union iset-intersection iset-difference iset-xor
          iset-union! iset-intersection! iset-difference! iset-xor!
          iset-open-interval iset-closed-interval
          iset-open-closed-interval iset-closed-open-interval
          isbuset= isubset< isubset<= isbuset> isbuset>=)
  )
(select-module srfi-217)

(define-record-type <iset> %make-iset iset?
  (tmap))

(define-method write-object ((iset <iset>) port)
  (format port "#<iset ~a entrie(s)>"
          (tree-map-num-entries (~ iset'tmap))))

;; Constructors
(define (iset . args) (list->iset args))

(define (list->iset args)
  (let1 tmap (make-tree-map < =)
    (let loop ([p (group-contiguous-sequence (sort args <) :squeeze #t)])
      (match p
        [() (%make-iset tmap)]
        [((s e) . rest) (tree-map-put! tmap s e) (loop rest)]
        [((v) . rest) (tree-map-put! tmap v v) (loop rest)]))))

(define (iset-unfold p f g seed)
  (list->iset (unfold p f g seed)))

;; Predicates

;; iset? - above

(define (iset-contains? iset k)
  (assume-type iset <iset>)
  (receive (s e) (tree-map-floor (iset-tmap iset) k)
    (<= k e)))

(define (iset-empty? iset) (tree-map-empty? (iset-tmap iset)))

(define (iset-disjoint? iset1 iset2)
  (assume-type iset1 <iset>)
  (assume-type iset2 <iset>)
  (let ([g1 (x->generator iset1)]
        [g2 (x->generator iset2)])
    (let loop ([p1 (g1)] [p2 (g2)])
      (cond [(eof-object? p1)]
            [(eof-object? p2)]
            [(<= (car p1) (car p2))
             (if (< (cdr p1) (car p2))
               (loop (g1) p2)
               #f)]
            [(< (car p2) (car p1))
             (if (< (cdr p2) (car p1))
               (loop p1 (g2))
               #f)]))))

;; Accessors

(define (iset-member iset k fallback)
  (if (iset-contains? iset k)
    k
    fallback))

(define (iset-min iset)
  (assume-type iset <iset>)
  (match (tree-map-min (iset-tmap iset))
    [#f #f]
    [(s . _) s]))

(define (iset-max iset)
  (assume-type iset <iset>)
  (match (tree-map-max (iset-tmap iset))
    [#f #f]
    [(_ . e) e]))

;; Updaters

(define (%adjoin-1 k tmap next)
  (receive (s e) (tree-map-floor tmap k)
    (cond [(not s) (rlet1 tmap2 (make-tree-map < =)
                     (tree-map-put! tmap2 k k))]
          [(<= k e) tmap]             ; subsumed
          [else (receive (s2 e2) (tree-map-floor tmap (+ k 1))
                  (rlet1 tmap2 (next tmap)
                    (if (not s2)
                      (if (= (+ e 1) k)
                        (tree-map-put! tmap2 s k) ; extend
                        (tree-map-put! tmap2 k k)) ; lone
                      (if (= (+ e 1) k)
                        (begin ; k connects two spans
                          (tree-map-delete! tmap2 s2)
                          (tree-map-put! tmap2 s e2))
                        (tree-map-put! tmap2 k k)))))])))

(define (iset-adjoin iset k . ks)
  (define (noclobber tmap)
    (if (eq? tmap (iset-tmap iset))
      (tree-map-copy (iset-tmap iset))
      tmap))
  (let1 tm (if (null? ks)
             (%adjoin-1 k (iset-tmap iset) noclobber)
             (fold (cut %adjoin-1 <> <> noclobber)
                   (%adjoin-1 k (iset-tmap iset) noclobber) ks))
    (if (eq? tm (iset-tmap iset))
      iset
      (%make-iset tm))))

(define (iset-adjoin! iset k . ks)
  (%adjoin-1 k (iset-tmap iset) identity)
  (dolist [k ks] (%adjoin-1 k (iset-tmap iset) identity))
  iset)

(define (%delete-1 k tmap next)
  (receive (s e) (tree-map-floor tmap)
    (if s
      (cond [(= k s) (if (= k e)
                       (rlet1 tmap2 (next tmap)
                         (tree-map-delete! tmap2 k))
                       (tree-map-put! tmap (+ s 1) e))]
            [(< k e) (rlet1 tmap2 (next tmap)
                       (tree-map-put! tmap s (- k 1))
                       (tree-map-put! tmap (+ k 1) e))]
            [(= k e) (rlet1 tmap2 (next tmap)
                       (tree-map-put! tmap s (- k 1)))]
            [else tmap])
      tmap)))

(define (iset-delete iset k . ks)
  (define (noclobber tmap)
    (if (eq? tmap (iset-tmap iset))
      (tree-map-copy (iset-tmap iset))
      tmap))
  (let1 tm (if (null? ks)
             (%delete-1 k (iset-tmap iset) noclobber)
             (fold (cut %delete-1 <> <> noclobber)
                   (%delete-1 k (iset-tmap iset) noclobber) ks))
    (if (eq? tm (iset-tmap iset))
      iset
      (%make-iset tm))))

(define (iset-delete! iset k . ks)
  (%delete-1 k (iset-tmap iset) identity)
  (dolist [k ks] (%delete-1 k (iset-tmap iset) identity))
  iset)

(define (iset-delete-all iset ks)
  (if (null? ks)
    iset
    (apply iset-delete iset ks)))

(define (iset-delete-all! iset ks)
  (if (null? ks)
    iset
    (apply iset-delete! iset ks)))

(define (iset-delete-min iset)
  (match (tree-map-min (iset-tmap iset))
    [#f (error "iset is empty:" iset)]
    [(s . e) (let1 tmap2 (tree-map-copy (iset-tmap iset))
               (tree-map-delete! tmap2 s)
               (when (< s e)
                 (tree-map-put! tmap2 (+ s 1) e))
               (%make-iset tmap2))]))

(define (iset-delete-min! iset)
  (match (tree-map-min (iset-tmap iset))
    [#f (error "iset is empty:" iset)]
    [(s . e) (let1 tmap (iset-tmap iset)
               (tree-map-delete! t s)
               (when (< s e)
                 (tree-map-put! tmap2 (+ s 1) e))
               iset)]))

(define (iset-delete-max iset)
  (match (tree-map-max (iset-tmap iset))
    [#f (error "iset is empty:" iset)]
    [(s . e) (let1 tmap2 (tree-map-copy (iset-tmap iset))
               (if (= s e)
                 (tree-map-delete! tmap2 s)
                 (tree-map-put! tmap2 s (- e 1)))
               (%make-iset tmap2))]))

(define (iset-delete-max! iset)
  (match (tree-map-max (iset-tmap iset))
    [#f (error "iset is empty:" iset)]
    [(s . e) (let1 tmap (iset-tmap iset)
               (if (= s e)
                 (tree-map-delete! tmap2 s)
                 (tree-map-put! tmap2 s (- e 1)))
               iset)]))
