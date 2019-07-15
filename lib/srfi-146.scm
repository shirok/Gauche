;;;
;;; srfi-146 - mappings
;;;
;;;   Copyright (c) 2017-2019  Shiro Kawai  <shiro@acm.org>
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

(define-module srfi-146
  (export mapping mapping-unfold mapping/ordered mapping-unfold/ordered
	  mapping? mapping-contains? mapping-empty? mapping-disjoint?
	  mapping-ref mapping-ref/default
          mapping-key-comparator

	  mapping-set mapping-set! 
	  mapping-adjoin mapping-adjoin!
	  mapping-replace mapping-replace!
	  mapping-delete mapping-delete!
          mapping-delete-all mapping-delete-all!
	  mapping-intern mapping-intern!
	  mapping-update mapping-update!
          mapping-update/default mapping-update!/default
          mapping-pop mapping-pop!
	  mapping-search mapping-search!
	  mapping-size mapping-find mapping-count
          mapping-any? mapping-every?
	  mapping-keys mapping-values mapping-entries
	  mapping-map mapping-map->list mapping-for-each mapping-fold
	  mapping-filter mapping-filter! mapping-remove mapping-remove!
	  mapping-partition mapping-partition!
	  mapping-copy mapping->alist alist->mapping alist->mapping!
          alist->mapping/ordered alist->mapping/ordered!

	  mapping=? mapping<? mapping>? mapping<=? mapping>=?
	  mapping-union mapping-intersection mapping-difference mapping-xor
	  mapping-union! mapping-intersection! mapping-difference! mapping-xor!
	  make-mapping-comparator
	  mapping-comparator
	  mapping-min-key mapping-max-key
	  mapping-min-value mapping-max-value
	  mapping-key-predecessor mapping-key-successor
	  mapping-range= mapping-range< mapping-range>
          mapping-range<= mapping-range>=
	  mapping-range=! mapping-range<! mapping-range>!
          mapping-range<=! mapping-range>=!
	  mapping-split
	  mapping-catenate mapping-catenate!
	  mapping-map/monotone mapping-map/monotone!
	  mapping-fold/reverse

          ;; builtin
          comparator?))
(select-module srfi-146)

;; We provide <tree-map> as mapping.
(define <mapping> <tree-map>)
(define (mapping? m) (is-a? m <mapping>))

(define-syntax dopairs
  (syntax-rules ()
    [(_ [k v lis] body ...)
     (let1 lis lis
       (do ([xs lis (cddr xs)])
           [(null? xs)]
         (when (null? (cdr xs))
           (error "mapping kv-list isn't even:" lis))
         (let ([k (car xs)]
               [v (cadr xs)])
           body ...)))]))

(define (mapping comparator . args)
  (assume-type comparator <comparator>)
  (rlet1 m (make-tree-map comparator)
    (dopairs [k v args] (tree-map-adjoin! m k v))))

(define (mapping-unfold p f g seed comparator)
  (assume-type comparator <comparator>)
  (rlet1 m (make-tree-map comparator)
    (do ([seed seed (g seed)])
        [(p seed) m]
      (receive (k v) (f seed)
        (tree-map-adjoin! m k v)))))

;; We don't take advantage of, neither check, the ordered keys.
(define mapping/ordered mapping)
(define mapping-unfold/ordered mapping-unfold)

(define (mapping-empty? m) (tree-map-empty? m))
(define (mapping-contains? m key) (tree-map-exists? m key))
(define (mapping-disjoint? m1 m2)
  (assume-type m1 <mapping>)
  (assume-type m2 <mapping>)
  (tree-map-seek m1 (^[k _] (tree-map-exists? m2 k))
                 (^[r k v] #f)
                 (^[] #t)))

(define %unique (list #f))

(define (mapping-ref m key
                     :optional
                     (failure #f)
                     (success identity))
  (assume-type m <mapping>)
  (if failure
    (let1 v (tree-map-get m key %unique)
      (if (eq? v %unique)
        (failure)
        (success v)))
    (success (tree-map-get m key))))    ;let tree-map-get handle failure

(define (mapping-ref/default m key default)
  (assume-type m <mapping>)
  (tree-map-get m key default))

(define (mapping-key-comparator m) (tree-map-comparator m))

(define (mapping-set m . args)
  (if (null? args)
    (begin
      (assume-type m <mapping>)
      m)                                ;shortcut
    (apply mapping-set! (mapping-copy m) args)))

(define (mapping-set! m . args)
  (assume-type m <mapping>)
  (dopairs [k v args] (tree-map-put! m k v))
  m)

(define (mapping-adjoin m . args)
  (if (null? args)
    (begin
      (assume-type m <mapping>)
      m)                                ;shortcut
    (apply mapping-adjoin! (tree-map-copy m) args)))

(define (mapping-adjoin! m . args)
  (assume-type m <mapping>)
  (dopairs [k v args] (tree-map-adjoin! m k v))
  m)

(define (mapping-replace m k v)
  (assume-type m <mapping>)
  (if (tree-map-exists? m k)
    (mapping-replace! (tree-map-copy m) k v)
    m))

(define (mapping-replace! m k v)
  (assume-type m <mapping>)
  (tree-map-replace! m k v)
  m)
   
(define (mapping-delete m . keys) (mapping-delete-all m keys))
(define (mapping-delete! m . keys) (mapping-delete-all! m keys))

(define (mapping-delete-all m keys)
  (assume-type m <mapping>)
  ;; We delay copy until we actually modify the map.
  (fold (^[k t]
          (if (tree-map-exists? t k)
            (rlet1 t (if (eq? t m) (tree-map-copy m) t)
              (tree-map-delete! t k))
            t))
        m keys))

(define (mapping-delete-all! m keys)
  (assume-type m <mapping>)
  (dolist [k keys] (tree-map-delete! m k))
  m)

(define (mapping-intern m k newval)
  (assume-type m <mapping>)
  (let1 v (tree-map-get m k %unique)
    (if (eq? v %unique)
      (let ([t (tree-map-copy m)]
            [v (newval)])
        (tree-map-put! t k v)
        (values t v))
      (values m v))))

(define (mapping-intern! m k newval)
  (assume-type m <mapping>)
  (let1 v (tree-map-get m k %unique)
    (if (eq? v %unique)
      (let1 v (newval)
        (tree-map-put! m k v)
        (values m v))
      (values m v))))

(define (mapping-update m k updater
                        :optional
                        (failure (^[] (errorf "~s doesn't have a key ~s" m k)))
                        (success identity))
  (assume-type m <mapping>)
  ;; We delay copy until we actually modify the map.
  (let* ([v (tree-map-get m k %unique)]
         [v1 (if (eq? v %unique)
               (updater (failure))
               (updater (success v)))])
    (if (eq? v v1)
      m                               ; no action needed
      (rlet1 t (tree-map-copy m)
        (tree-map-put! t k v1)))))

(define (mapping-update! m k updater
                        :optional
                        (failure (^[] (errorf "~s doesn't have a key ~s" m k)))
                        (success identity))
  (assume-type m <mapping>)
  (let* ([v (tree-map-get m k %unique)]
         [v1 (if (eq? v %unique)
               (updater (failure))
               (updater (success v)))])
    (tree-map-put! m k v1))
  m)

(define (mapping-update/default m k updater default)
  (mapping-update m k updater (lambda () default)))

(define (mapping-update!/default m k updater default)
  (mapping-update! m k updater (lambda () default)))

(define (mapping-pop! m
                      :optional
                      (failure (^[] (error "can't pop from an empty map"))))
  (assume-type m <mapping>)
  (if-let1 p (tree-map-pop-min! m)
    (values m (car p) (cdr p))
    (failure)))

(define (mapping-pop m 
                     :optional
                     (failure (^[] (error "can't pop from an empty map"))))
  (assume-type m <mapping>)
  (if (tree-map-empty? m)
    (failure)                           ;avoid unnecessary copying
    (mapping-pop! (mapping-copy m))))

(define (mapping-search m k failure success)
  (assume-type m <mapping>)
  (let1 v (tree-map-get m k %unique)
    (if (eq? v %unique)
      (failure (^[v o] (let1 m (tree-map-copy m) ;insert
                         (tree-map-put! m k v)
                         (values m o)))
               (^[o] (values m o)))     ;ignore
      (success k v
               (^[k v o] (let1 m (tree-map-copy m) ;update
                           (tree-map-put! m k v)
                           (values m o)))
               (^[o] (let1 m (tree-map-copy m) ;remove
                       (tree-map-delete! m k)
                       (values m o)))))))

(define (mapping-search! m k failure success)
  (assume-type m <mapping>)
  (let1 v (tree-map-get m k %unique)
    (if (eq? v %unique)
      (failure (^[v o] (tree-map-put! m k v) (values m o)) ;insert
               (^[o] (values m o)))                        ;ignore
      (success v
               (^[k v o] (tree-map-put! m k v) (values m o)) ;update
               (^[o] (tree-map-delete! m k) (values m o)))))) ;remove

(define (mapping-size m)
  (assume-type m <mapping>)
  (tree-map-num-entries m))

(define (mapping-find pred m failure)
  (assume-type m <mapping>)
  (tree-map-seek m pred (^[r k v] (values k v)) failure))

(define (mapping-count pred m)
  (assume-type m <mapping>)
  (tree-map-fold m (^[k v c] (if (pred k v) (+ 1 c) c)) 0))

(define (mapping-any? pred m)
  (assume-type m <mapping>)
  (tree-map-seek m pred (^[r k v] #t) (^[] #f)))

(define (mapping-every? pred m)
  (assume-type m <mapping>)
  (tree-map-seek m (^[k v] (not (pred k v))) (^[r k v] #f) (^[] #t)))

(define (mapping-keys m) (tree-map-keys m))
(define (mapping-values m) (tree-map-values m))

(define (mapping-entries m)
  (values (tree-map-keys m) (tree-map-values m)))

(define (mapping-map proc cmpr m)
  (assume-type m <mapping>)
  (assume-type cmpr <comparator>)
  (rlet1 r (make-tree-map cmpr)
    (tree-map-for-each m (^[k v] (receive [k v] (proc k v)
                                   (tree-map-put! r k v))))))

(define (mapping-for-each proc m)
  (assume-type m <mapping>)
  (tree-map-for-each m proc))

(define (mapping-fold kons knil m)
  (assume-type m <mapping>)
  (tree-map-fold m kons knil))

(define (mapping-map->list proc m)
  (assume-type m <mapping>)
  (tree-map-map m proc))

(define (mapping-filter pred m)
  (assume-type m <mapping>)
  (rlet1 r (make-tree-map (tree-map-comparator m))
    (tree-map-for-each m (^[k v] (when (pred k v)
                                   (tree-map-put! r k v))))))

(define (mapping-filter! pred m)
  (assume-type m <mapping>)
  (tree-map-for-each m (^[k v] (unless (pred k v)
                                 (tree-map-delete! m k)))))

(define (mapping-remove pred m)
  (assume-type m <mapping>)
  (rlet1 r (make-tree-map (tree-map-comparator m))
    (tree-map-for-each m (^[k v] (unless (pred k v)
                                   (tree-map-put! r k v))))))

(define (mapping-remove! pred m)
  (assume-type m <mapping>)
  (tree-map-for-each m (^[k v] (when (pred k v)
                                 (tree-map-delete! m k)))))

(define (mapping-partition pred m)
  (assume-type m <mapping>)
  (let ([f (make-tree-map (tree-map-comparator m))]
        [r (make-tree-map (tree-map-comparator m))])
    (tree-map-for-each m (^[k v] (if (pred k v)
                                   (tree-map-put! f k v)
                                   (tree-map-put! r k v))))
    (values f r)))

(define (mapping-partition! pred m)
  (assume-type m <mapping>)
  (let1 r (make-tree-map (tree-map-comparator m))
    (tree-map-for-each m (^[k v] (unless (pred k v)
                                   (tree-map-delete! m k)
                                   (tree-map-put! r k v))))
    (values m r)))

(define (mapping-copy m)
  (assume-type m <mapping>)
  (tree-map-copy m))

(define (mapping->alist m)
  (assume-type m <mapping>)
  (tree-map-fold-right m acons '()))

(define (alist->mapping cmpr alist)
  (assume-type cmpr <comparator>)
  (rlet1 m (make-tree-map cmpr)
    (dolist [p alist]
      (tree-map-adjoin! m (car p) (cdr p)))))

(define (alist->mapping! m alist)
  (assume-type m <mapping>)
  (dolist [p alist]
    (tree-map-adjoin! m (car p) (cdr p)))
  m)

;; we don't take advantage of, neither check, ordered keys
(define alist->mapping/ordered alist->mapping)
(define alist->mapping/ordered! alist->mapping!)

(define (%mapping-cmp v=? pred ms)
  (let loop ([ms ms])
    (cond [(null? (cdr ms)) #t]
          [(tree-map-compare-as-sets (car ms) (cadr ms) v=? #f)
           => (^r (and (pred r) (loop (cdr ms))))]
          [else #f])))

(define-syntax define-mapping-cmp
  (syntax-rules ()
    [(_ name op)
     (define (name vcmp m . more)
       (assume-type vcmp <comparator>)
       (%mapping-cmp (comparator-equality-predicate vcmp)
                     (^[x] (op x 0))
                     (cons m more)))]))

(define-mapping-cmp mapping=? =)
(define-mapping-cmp mapping<? <)
(define-mapping-cmp mapping<=? <=)
(define-mapping-cmp mapping>? >)
(define-mapping-cmp mapping>=? >=)

(define (%union-2! m1 m2)
  (tree-map-for-each m2 (^[k v] (tree-map-adjoin! m1 k v)))
  m1)

(define (mapping-union! m1 . more)
  (if (null? more)
    m1
    (apply mapping-union! (%union-2! m1 (car more)) (cdr more))))

(define (mapping-union m1 . more)
  (apply mapping-union! (mapping-copy m1) more))

(define (%intersection-2! m1 m2)
  (tree-map-for-each m1 (^[k v] (unless (tree-map-get m2 k #f)
                                  (tree-map-delete! m1 k))))
  m1)

(define (mapping-intersection! m1 . more)
  (if (null? more)
    m1
    (apply mapping-intersection! (%intersection-2! m1 (car more)) (cdr more))))

(define (mapping-intersection m1 . more)
  (apply mapping-intersection! (mapping-copy m1) more))

(define (%difference-2! m1 m2)
  (tree-map-for-each m2 (^[k v] (tree-map-delete! m1 k)))
  m1)

(define (mapping-difference! m1 . more)
  (let loop ([m1 m1] [more more])
    (if (null? more)
      m1
      (loop (%difference-2! m1 (car more)) (cdr more)))))

(define (mapping-difference m1 . more)
  (apply mapping-difference! (mapping-copy m1) more))

(define (mapping-xor! m1 m2)
  (tree-map-for-each m2 (^[k v] (if (tree-map-get m1 k #f)
                                  (tree-map-delete! m1 k)
                                  (tree-map-put! m1 k v))))
  m1)

(define (mapping-xor m1 m2)
  (mapping-xor! (mapping-copy m1) m2))

(define (mapping-min-key m)
  (assume-type m <mapping>)
  (if-let1 p (tree-map-min m)
    (car p)
    (error "Can't get min key from an empty map:" m)))

(define (mapping-max-key m)
  (assume-type m <mapping>)
  (if-let1 p (tree-map-max m)
    (car p)
    (error "Can't get min key from an empty map:" m)))

(define (mapping-min-value m)
  (assume-type m <mapping>)
  (if-let1 p (tree-map-min m)
    (cdr p)
    (error "Can't get min key from an empty map:" m)))

(define (mapping-max-value m)
  (assume-type m <mapping>)
  (if-let1 p (tree-map-max m)
    (cdr p)
    (error "Can't get min key from an empty map:" m)))

(define (mapping-key-predecessor m probe failure)
  (assume-type m <mapping>)
  (receive [k v] (tree-map-predecessor m probe %unique)
    (if (eq? k %unique)
      (failure)
      k)))

(define (mapping-key-successor m probe failure)
  (assume-type m <mapping>)
  (receive [k v] (tree-map-successor m probe %unique)
    (if (eq? k %unique)
      (failure)
      k)))

(define-syntax define-mapping-range
  (syntax-rules ()
    [(_ name! name op)
     (begin
       (define (name! m probe)
         (assume-type m <mapping>)
         (let1 cmpr (tree-map-comparator m)
           ($ tree-map-for-each m
              (^[k v] (unless (op (comparator-compare cmpr k probe) 0)
                        (tree-map-delete! m k)))))
         m)
       (define (name m probe)
         (name! (mapping-copy m) probe)))]))

(define-mapping-range mapping-range=!  mapping-range=  =)
(define-mapping-range mapping-range<!  mapping-range<  <)
(define-mapping-range mapping-range<=! mapping-range<= <=)
(define-mapping-range mapping-range>!  mapping-range>  >)
(define-mapping-range mapping-range>=! mapping-range>= >=)

(define (mapping-split m probe)
  (assume-type m <mapping>)
  ;; no more efficient than calling each one
  (values (mapping-range< m probe)
          (mapping-range<= m probe)
          (mapping-range= m probe)
          (mapping-range>= m probe)
          (mapping-range> m probe)))

(define (%mapping-catenate! cmpr m1 key val m2 reuse?)
  (define (too-small key m)
    (errorf "Catenating key ~s is too small for ~s" key m))
  (define (too-large key m)
    (errorf "Catenating key ~s is too large for ~s" key m))
  (cond [(and reuse? (equal? cmpr (tree-map-comparator m1)))
         ;; Reuse m1
         (when (and (not (tree-map-empty? m1))
                    (>= (comparator-compare cmpr (car (tree-map-max m1)) key) 0))
           (too-small key m1))
         (tree-map-put! m1 key val)
         ($ tree-map-for-each m2
            (^[k v]
              (when (<= (comparator-compare cmpr k key) 0)
                (too-large key m2))
              (tree-map-put! m1 k v)))
         m1]
        [(and reuse? (equal? cmpr (tree-map-comparator m2)))
         ;; Reuse m2
         (when (and (not (tree-map-empty? m2))
                    (<= (comparator-compare cmpr (car (tree-map-max m2)) key) 0))
           (too-large key m2))
         (tree-map-put! m2 key val)
         ($ tree-map-for-each m1
            (^[k v]
              (when (>= (comparator-compare cmpr k key) 0)
                (too-small key m1))
              (tree-map-put! m2 k v)))
         m2]
        [else
         (rlet1 m (make-tree-map cmpr)
           (tree-map-put! m key val)
           ($ tree-map-for-each m1
              (^[k v]
                (when (>= (comparator-compare cmpr k key) 0)
                  (too-small key m1))
                (tree-map-put! m k v)))
           ($ tree-map-for-each m2
              (^[k v]
                (when (<= (comparator-compare cmpr k key) 0)
                  (too-large key m2))
                (tree-map-put! m k v))))]))

(define (mapping-catenate cmpr m1 key val m2)
  (%mapping-catenate! cmpr m1 key val m2 #f))
(define (mapping-catenate! cmpr m1 key val m2)
  (%mapping-catenate! cmpr m1 key val m2 #t))

;; We don't take advantage of monotone
(define (mapping-map/monotone! proc cmpr m)
  (mapping-map proc cmpr m))
(define (mapping-map/monotone proc cmpr m)
  (mapping-map proc cmpr m))

(define (mapping-fold/reverse kons knil m)
  (tree-map-fold-right m kons knil))

(define (make-mapping-comparator value-cmpr)
  (define (compare a b)
    (tree-map-compare-as-sequences a b value-cmpr))
  (make-comparator/compare mapping? #t compare #f))

(define mapping-comparator (make-mapping-comparator default-comparator))
