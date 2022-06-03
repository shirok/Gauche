;;;
;;; srfi-217 - Integer sets
;;;
;;;   Copyright (c) 2021-2022  Shiro Kawai  <shiro@acm.org>
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
  (use gauche.generator)
  (use scheme.list)
  (use util.match)
  (use srfi-42)
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
          isubset= isubset< isubset<= isubset> isubset>=)
  )
(select-module srfi-217)

;; Internally, <iset> has a persistent/transient distinction.  Persistent
;; iset is an ordinary functional structure.  Transient iset can be passed
;; to linear-updating procedures, given that the caller will never share the
;; reference to it elsewhere, and the linear-updating procedure may mutate it.
;;
;; If a functional procedure receives a transient iset, or a linear-updating
;; procedure gets a persistent iset, the structure is always copied.
;; Cf. http://blog.practical-scheme.net/gauche/20210531-functional-and-linear-update
;;
;; A functional procedure always returns a persistent iset. A linear-updating
;; procedure returns a transient iset, except when it is an alias of
;; functional version.
;;
;; The distinction is purely internal; the users don't need to worry about it,
;; and they can treat functional version as if it always return a copy, so
;; that it is safe to pass it to the linear updating version as long as
;; she don't keep explicit reference to it.

(define-record-type <iset> %make-iset-int iset?
  (transient? %iset-transient? %iset-transient?-set!)
  (size %iset-size %iset-size-set!)  ;; this is lazily computed
  (tmap iset-tmap))

(define (%make-iset :optional (transient? #t) (tmap #f))
  (%make-iset-int transient?
                  #f                    ;; size to be computed lazily
                  (or tmap (make-tree-map = <))))

;; If you modify internal tree map, call this to invalidate size cache.
;; returns iset itself.
(define (%iset-touch! iset)
  (%iset-size-set! iset #f)
  iset)

(define-method write-object ((iset <iset>) port)
  (format port "#<iset ~a entrie(s)>" (iset-size iset)))

(define (%iset->persistent iset)
  (if (%iset-transient? iset)
    (%make-iset #f (tree-map-copy (iset-tmap iset)))
    iset))

(define (%iset->transient iset)
  (if (%iset-transient? iset)
    iset
    (%make-iset #t (tree-map-copy (iset-tmap iset)))))

(define (%iset-make-persistent! iset) ; only used in constructors
  (%iset-transient?-set! iset #f))

;;;
;;; Constructors
;;;

(define (iset . args) (list->iset args))

(define (list->iset args)
  (let1 tmap (make-tree-map = <)
    (let loop ([p (group-contiguous-sequence (sort args <) :squeeze #t)])
      (match p
        [() (%make-iset #f tmap)]
        [((s e) . rest) (tree-map-put! tmap s e) (loop rest)]
        [((v) . rest) (tree-map-put! tmap v v) (loop rest)]))))

(define (iset-unfold p f g seed)
  (list->iset (unfold p f g seed)))

(define (make-range-iset start end :optional (step 1))
  (if (= step 1)
    (%make-iset #f (alist->tree-map `((,start . ,(- end 1))) = <))
    (list->iset (lrange start end step))))

;;;
;;; Predicates
;;;

;; iset? - above

(define (%tmap-contains? tmap k)
  (receive (s e) (tree-map-floor tmap k)
    (and s (<= k e))))

(define (iset-contains? iset k)
  (assume-type iset <iset>)
  (%tmap-contains? (iset-tmap iset) k))

(define (iset-empty? iset) (tree-map-empty? (iset-tmap iset)))

(define (iset-disjoint? iset1 iset2)
  (assume-type iset1 <iset>)
  (assume-type iset2 <iset>)
  (let ([g1 (x->generator (iset-tmap iset1))]
        [g2 (x->generator (iset-tmap iset2))])
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

;;;
;;; Accessors
;;;

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

;;;
;;; Updaters
;;;

;; The 'noclobber' thing is to avoid unecessary copying of persistent iset.

(define (%adjoin-1! k tmap maybe-copy)
  (receive (s e) (tree-map-floor tmap k)
    (cond [(not s) (rlet1 tmap2 (maybe-copy tmap)
                     (tree-map-put! tmap2 k k))]
          [(<= k e) tmap]             ; subsumed
          [else (receive (s2 e2) (tree-map-floor tmap (+ k 1))
                  (rlet1 tmap2 (maybe-copy tmap)
                    (if (or (not s2) (= s s2))
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
      (tree-map-copy tmap)
      tmap))
  (let* ([input (%iset->persistent iset)]
         [tm (if (null? ks)
               (%adjoin-1! k (iset-tmap input) noclobber)
               (fold (cut %adjoin-1! <> <> noclobber)
                     (%adjoin-1! k (iset-tmap input) noclobber) ks))])
    (if (eq? tm (iset-tmap iset))
      iset
      (%make-iset #f tm))))

(define (iset-adjoin! iset k . ks)
  (let1 input (%iset->transient iset)
    (%adjoin-1! k (iset-tmap input) identity)
    (dolist [k ks] (%adjoin-1! k (iset-tmap input) identity))
    (%iset-touch! input)))

(define (%delete-1! k tmap maybe-copy)
  (receive (s e) (tree-map-floor tmap k)
    (if s
      (cond [(= k s) (rlet1 tmap2 (maybe-copy tmap)
                       (tree-map-delete! tmap2 k)
                       (unless (= k e) (tree-map-put! tmap2 (+ s 1) e)))]
            [(< k e) (rlet1 tmap2 (maybe-copy tmap)
                       (tree-map-put! tmap2 s (- k 1))
                       (tree-map-put! tmap2 (+ k 1) e))]
            [(= k e) (rlet1 tmap2 (maybe-copy tmap)
                       (tree-map-put! tmap2 s (- k 1)))]
            [else tmap])
      tmap)))

(define (iset-delete iset k . ks)
  (define (noclobber tmap)
    (if (eq? tmap (iset-tmap iset))
      (tree-map-copy tmap)
      tmap))
  (let* ([input (%iset->persistent iset)]
         [tm (let loop ([tm (%delete-1! k (iset-tmap input) noclobber)]
                        [ks ks])
              (if (null? ks)
                tm
                (loop (%delete-1! (car ks) tm noclobber) (cdr ks))))])
    (if (eq? tm (iset-tmap iset))
      iset
      (%make-iset #f tm))))

(define (iset-delete! iset k . ks)
  (let1 input (%iset->transient iset)
    (%delete-1! k (iset-tmap input) identity)
    (dolist [k ks] (%delete-1! k (iset-tmap input) identity))
    (%iset-touch! input)))

(define (iset-delete-all iset ks)
  (if (null? ks)
    (%iset->persistent iset)
    (apply iset-delete iset ks)))

(define (iset-delete-all! iset ks)
  (if (null? ks)
    (%iset->transient iset)
    (apply iset-delete! iset ks)))

(define (iset-delete-min iset)
  (match (tree-map-min (iset-tmap iset))
    [#f (error "iset is empty:" iset)]
    [(s . e)
     ;; we copy the tree anyway, so it doesn't matter if input is transient
     ;; or persistent.
     (let1 tmap (tree-map-copy (iset-tmap iset))
       (tree-map-delete! tmap s)
       (when (< s e)
         (tree-map-put! tmap (+ s 1) e))
       (values s (%make-iset #f tmap)))]))

(define (iset-delete-min! iset)
  (match (tree-map-min (iset-tmap iset))
    [#f (error "iset is empty:" iset)]
    [(s . e) (let* ([input (%iset->transient iset)]
                    [tmap (iset-tmap input)])
               (tree-map-delete! tmap s)
               (when (< s e)
                 (tree-map-put! tmap (+ s 1) e))
               (values s (%iset-touch! input)))]))

(define (iset-delete-max iset)
  (match (tree-map-max (iset-tmap iset))
    [#f (error "iset is empty:" iset)]
    [(s . e)
     ;; we copy the tree anyway, so it doesn't matter if input is transient
     ;; or persistent.
     (let1 tmap (tree-map-copy (iset-tmap iset))
       (if (= s e)
         (tree-map-delete! tmap s)
         (tree-map-put! tmap s (- e 1)))
       (values e (%make-iset #f tmap)))]))

(define (iset-delete-max! iset)
  (match (tree-map-max (iset-tmap iset))
    [#f (error "iset is empty:" iset)]
    [(s . e) (let* ([input (%iset->transient iset)]
                    [tmap (iset-tmap input)])
               (if (= s e)
                 (tree-map-delete! tmap s)
                 (tree-map-put! tmap s (- e 1)))
               (values e (%iset-touch! iset)))]))

(define (%iset-search iset k failure success update remove insert)
  (receive (s e) (tree-map-floor (iset-tmap iset) k)
    (if (and s (<= k e))
      (success k update remove)
      (failure insert (^[result] (values iset result))))))

(define (iset-search iset k failure success)
  (define (update new-k result)
    (values (iset-adjoin! (iset-delete iset k) new-k) result))
  (define (remove result)
    (values (iset-delete iset k) result))
  (define (insert result)
    (values (iset-adjoin iset k) result))
  (%iset-search iset k failure success update remove insert))

(define (iset-search! iset k failure success)
  (define (update new-k result)
    (values (iset-adjoin! (iset-delete! iset k) new-k) result))
  (define (remove result)
    (values (iset-delete! iset k) result))
  (define (insert result)
    (values (iset-adjoin! iset k) result))
  (%iset-search iset k failure success update remove insert))

;;;
;;; The whole set
;;;

(define (iset-size iset)
  (or (%iset-size iset)
      (rlet1 s (tree-map-fold (iset-tmap iset)
                              (^[s e count] (+ 1 (- e s) count))
                              0)
        (%iset-size-set! iset s))))

(define (iset-find pred iset failure)
  (let/cc return
    (tree-map-for-each (iset-tmap iset)
                       (^[s e]
                         (do-ec (: k s (+ e 1))
                                (when (pred k) (return k)))))
    (failure)))

(define (iset-count pred iset)
  (rlet1 cnt 0
    (tree-map-for-each (iset-tmap iset)
                       (^[s e]
                         (do-ec (: k s (+ e 1))
                                (when (pred k) (inc! cnt)))))))

(define (iset-any? pred iset)
  (boolean (iset-find pred iset (constantly #f))))

(define (iset-every? pred iset)
  (not (iset-find (complement pred) iset (constantly #f))))

;;;
;;; Mapping and folding
;;;

(define (iset-map proc iset)
  (rlet1 r (%make-iset)
    (tree-map-for-each (iset-tmap iset)
                       (^[s e]
                         (do-ec (: k s (+ e 1))
                                (set! r (iset-adjoin! r (proc k))))))
    (%iset-make-persistent! r)))

(define (iset-for-each proc iset)
  (tree-map-for-each (iset-tmap iset)
                     (^[s e]
                       (do-ec (: k s (+ e 1))
                              (proc k)))))

(define (iset-fold kons knil iset)
  (tree-map-fold (iset-tmap iset)
                 (^[s e seed]
                   (fold-ec seed (: k s (+ e 1)) k kons))
                 knil))

(define (iset-fold-right kons knil iset)
  (tree-map-fold-right (iset-tmap iset)
                       (^[s e seed]
                         (fold-ec seed (: k e (- s 1) -1) k kons))
                       knil))

(define (iset-filter pred iset)
  (rlet1 r (%make-iset)
    (iset-for-each (^k (when (pred k) (set! r (iset-adjoin! r k))))
                   iset)
    (%iset-make-persistent! r)))

;; we don't have an efficient way to modify the internal map in-place.
(define (iset-filter! pred iset) (iset-filter pred iset))

(define (iset-remove pred iset)
  (rlet1 r (%make-iset)
    (iset-for-each (^k (unless (pred k) (set! r (iset-adjoin! r k))))
                   iset)
    (%iset-make-persistent! r)))

(define (iset-remove! pred iset) (iset-remove pred iset))

(define (iset-partition pred iset)
  (let ([in (%make-iset)]
        [out (%make-iset)])
    (iset-for-each (^k (if (pred k)
                         (set! in (iset-adjoin! in k))
                         (set! out (iset-adjoin! out k))))
                   iset)
    (%iset-make-persistent! in)
    (%iset-make-persistent! out)
    (values in out)))

(define (iset-partition! pred iset) (iset-partition pred iset))

;;;
;;; Copying and conversion
;;;

(define (iset-copy iset)
  (%make-iset (%iset-transient? iset) (tree-map-copy (iset-tmap iset))))

(define (iset->list iset)
  (iset-fold-right cons '() iset))

(define (list->iset! iset ks)
  (fold (^[k iset] (iset-adjoin! iset k)) iset ks))

;;;
;;; Subsets
;;;

(define (iset=? iset1 iset2 . isets)
  (and (equal? (iset-tmap iset1) (iset-tmap iset2))
       (or (null? isets)
           (apply iset=? iset2 isets))))

(define (iset<? iset1 iset2 . isets)
  (and (< (iset-size iset1) (iset-size iset2))
       (iset-every? (cut iset-contains? iset2 <>) iset1)
       (or (null? isets)
           (apply iset<? iset2 isets))))

(define (iset>? iset1 iset2 . isets)
  (and (> (iset-size iset1) (iset-size iset2))
       (iset-every? (cut iset-contains? iset1 <>) iset2)
       (or (null? isets)
           (apply iset>? iset2 isets))))

(define (iset<=? iset1 iset2 . isets)
  (and (iset-every? (cut iset-contains? iset2 <>) iset1)
       (or (null? isets)
           (apply iset<=? iset2 isets))))

(define (iset>=? iset1 iset2 . isets)
  (and (iset-every? (cut iset-contains? iset1 <>) iset2)
       (or (null? isets)
           (apply iset>=? iset2 isets))))

;;;
;;; Set theory operations
;;;

(define (%union-2! tmap1 tmap2 maybe-copy)
  (tree-map-fold tmap2
                 (^[s e rtmap]
                   (do-ec [: k s (+ e 1)]
                          (set! rtmap (%adjoin-1! k rtmap maybe-copy)))
                   rtmap)
                 tmap1))

(define (iset-union iset1 iset2 . isets)
  (define (noclobber tmap)
    (if (eq? tmap (iset-tmap iset1))
      (tree-map-copy tmap)
      tmap))
  (define input (%iset->persistent iset1))
  (let loop ([tmap (%union-2! (iset-tmap input) (iset-tmap iset2) noclobber)]
             [isets isets])
    (if (null? isets)
      (if (eq? tmap (iset-tmap iset1))
        iset1
        (%make-iset #f tmap))
      (loop (%union-2! tmap (iset-tmap (car isets)) noclobber)
            (cdr isets)))))

(define (iset-union! iset1 iset2 . isets)
  (define input (%iset->transient iset1))
  (let loop ([tmap (%union-2! (iset-tmap input) (iset-tmap iset2) identity)]
             [isets isets])
    (if (null? isets)
      (%iset-touch! input)
      (loop (%union-2! tmap (iset-tmap (car isets)) identity)
            (cdr isets)))))

(define (%diff-2! tmap1 tmap2 maybe-copy)
  (tree-map-fold tmap2
                 (^[s e rtmap]
                   (do-ec [: k s (+ e 1)]
                          (set! rtmap (%delete-1! k rtmap maybe-copy)))
                   rtmap)
                 tmap1))

(define (iset-difference iset1 iset2 . isets)
  (define (noclobber tmap)
    (if (eq? tmap (iset-tmap iset1))
      (tree-map-copy tmap)
      tmap))
  (define input (%iset->persistent iset1))
  (let loop ([tmap (%diff-2! (iset-tmap input) (iset-tmap iset2) noclobber)]
             [isets isets])
    (if (null? isets)
      (if (eq? tmap (iset-tmap iset1))
        iset1
        (%make-iset #f tmap))
      (loop (%diff-2! tmap (iset-tmap (car isets)) noclobber)
            (cdr isets)))))

(define (iset-difference! iset1 iset2 . isets)
  (define input (%iset->transient iset1))
  (let loop ([tmap (%diff-2! (iset-tmap input) (iset-tmap iset2) identity)]
             [isets isets])
    (if (null? isets)
      (%iset-touch! input)
      (loop (%diff-2! tmap (iset-tmap (car isets)) identity)
            (cdr isets)))))

;; For xor and intersect, it's easier to build a new imap.
;; We might be able to do better when operating on more than two sets.

(define (%xor-2 tmap1 tmap2)
  (rlet1 rmap (tree-map-copy tmap1)
    (tree-map-for-each tmap2
                       (^[s e]
                         (do-ec (: k s (+ e 1))
                                (if (%tmap-contains? tmap1 k)
                                  (%delete-1! k rmap identity)
                                  (%adjoin-1! k rmap identity)))))))

(define (%xor-n transient? iset1 iset2 . isets)
  (let loop ([tmap (%xor-2 (iset-tmap iset1) (iset-tmap iset2))]
             [isets isets])
    (if (null? isets)
      (%make-iset transient? tmap)
      (loop (%xor-2 tmap (iset-tmap (car isets))) (cdr isets)))))

(define (iset-xor iset1 iset2 . isets)
  (apply %xor-n #f iset1 iset2 isets))

(define (iset-xor! iset1 iset2 . isets)
  (apply %xor-n #t iset1 iset2 isets))

(define (%intersect-2 tmap1 tmap2)
  (rlet1 rmap (make-tree-map = <)
    (tree-map-for-each tmap2
                       (^[s e]
                         (do-ec (: k s (+ e 1))
                                (when (%tmap-contains? tmap1 k)
                                  (%adjoin-1! k rmap identity)))))))

(define (%intersect-n transient? iset1 iset2 . isets)
  (let loop ([tmap (%intersect-2 (iset-tmap iset1) (iset-tmap iset2))]
             [isets isets])
    (if (null? isets)
      (%make-iset transient? tmap)
      (loop (%intersect-2 tmap (iset-tmap (car isets))) (cdr isets)))))

(define (iset-intersection iset1 iset2 . isets)
  (apply %intersect-n #f iset1 iset2 isets))

(define (iset-intersection! iset1 iset2 . isets)
  (apply %intersect-n #t iset1 iset2 isets))

(define (isubset= iset0 k)
  (if (iset-contains? iset0 k)
    (if (= (iset-size iset0) 1)
      (%iset->persistent iset0)
      (iset k))
    (iset)))

(define (isubset< iset0 k)
  (let1 tmap (iset-tmap iset0)
    (match (tree-map-max tmap)
      [#f (%iset->persistent iset0)]    ;empty set
      [(s . e)
       (if (<= e k)
         (%iset->persistent iset0)      ;no change
         (let ([rmap (make-tree-map = <)]
               [g (x->generator tmap)])
           (let loop ()
             (match-let1 (s . e) (g)
               (cond
                [(< e k) (tree-map-put! rmap s e) (loop)]
                [(<= k s) (%make-iset #f rmap)]
                [else (tree-map-put! rmap s (- k 1))
                      (%make-iset #f rmap)])))))])))

(define (isubset<= iset0 k) (isubset< iset0 (+ k 1)))

(define (isubset>= iset0 k)
  (let1 tmap (iset-tmap iset0)
    (match (tree-map-min tmap)
      [#f (%iset->persistent iset0)]    ;empty set
      [(s . e)
       (if (<= k s)
         (%iset->persistent iset0)      ;no change
         (let ([rmap (make-tree-map = <)]
               [g (x->generator tmap)])
           (let loop ()
             (match (g)
               [(s . e)
                (cond
                 [(< e k) (loop)]
                 [(<= k s) (tree-map-put! rmap s e) (loop)]
                 [else (tree-map-put! rmap k e) (loop)])]
               [else (%make-iset #f rmap)]))))])))

(define (isubset> iset0 k) (isubset>= iset0 (+ k 1)))


(define (iset-closed-interval iset0 lo hi)
  (let1 tmap (iset-tmap iset0)
    (match (tree-map-min tmap)
      [#f (%iset->persistent iset0)]
      [(s . e)
       (if (< hi s)
         (iset)                         ;empty
         (match-let1 (sz . ez) (tree-map-max tmap)
           (if (< ez lo)
             (iset)                     ;empty
             (let ([rmap (make-tree-map = <)]
                   [g (x->generator tmap)])
               (let loop ()
                 (match (g)
                   [(s . e)
                    (cond
                     [(< e lo) (loop)]               ;non overwrapping
                     [(< hi s) (%make-iset #f rmap)] ;past range
                     [(< lo s) (if (< e hi)
                                 (begin (tree-map-put! rmap s e) (loop))
                                 (begin (tree-map-put! rmap s hi)
                                        (%make-iset #f rmap)))]
                     [else (if (< e hi)
                             (begin (tree-map-put! rmap lo e) (loop))
                             (begin (tree-map-put! rmap lo hi)
                                    (%make-iset #f rmap)))])]))))))])))

(define (iset-open-closed-interval iset0 lo hi)
  (iset-closed-interval iset0 (+ lo 1) hi))
(define (iset-closed-open-interval iset0 lo hi)
  (iset-closed-interval iset0 lo (- hi 1)))
(define (iset-open-interval iset0 lo hi)
  (iset-closed-interval iset0 (+ lo 1) (- hi 1)))
