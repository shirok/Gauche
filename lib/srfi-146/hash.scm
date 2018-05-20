;;;
;;; srfi-146.hash - hashmaps
;;;

(define-module srfi-146.hash
  (export hashmap hashmap-unfold
	  hashmap? hashmap-contains? hashmap-empty? hashmap-disjoint?
	  hashmap-ref hashmap-ref/default
          hashmap-key-comparator

	  hashmap-set hashmap-set! 
	  hashmap-adjoin hashmap-adjoin!
	  hashmap-replace hashmap-replace!
	  hashmap-delete hashmap-delete!
          hashmap-delete-all hashmap-delete-all!
	  hashmap-intern hashmap-intern!
	  hashmap-update hashmap-update!
          hashmap-update/default hashmap-update!/default
          hashmap-pop hashmap-pop!
	  hashmap-search hashmap-search!
	  hashmap-size hashmap-find hashmap-count
          hashmap-any? hashmap-every?
	  hashmap-keys hashmap-values hashmap-entries
	  hashmap-map hashmap-map->list hashmap-for-each hashmap-fold
	  hashmap-filter hashmap-filter! hashmap-remove hashmap-remove!
	  hashmap-partition hashmap-partition!
	  hashmap-copy hashmap->alist alist->hashmap alist->hashmap!

	  hashmap=? hashmap<? hashmap>? hashmap<=? hashmap>=?
	  hashmap-union hashmap-intersection hashmap-difference hashmap-xor
	  hashmap-union! hashmap-intersection! hashmap-difference! hashmap-xor!
	  make-hashmap-comparator
	  hashmap-comparator

          ;; builtin
          comparator?))
(select-module srfi-146.hash)

;; We provide <hash-table> as hashmap.
(define <hashmap> <hash-table>)
(define (hashmap? m) (is-a? m <hashmap>))

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

(define (hashmap comparator . args)
  (assume-type comparator <comparator>)
  (rlet1 m (make-hash-table comparator)
    (dopairs [k v args] (hash-table-adjoin! m k v))))

(define (hashmap-unfold p f g seed comparator)
  (assume-type comparator <comparator>)
  (rlet1 m (make-hash-table comparator)
    (do ([seed seed (g seed)])
        [(p seed) m]
      (receive (k v) (f seed)
        (hash-table-adjoin! m k v)))))

(define (hashmap-empty? m) (hash-table-empty? m))
(define (hashmap-contains? m key) (hash-table-exists? m key))
(define (hashmap-disjoint? m1 m2)
  (assume-type m1 <hashmap>)
  (assume-type m2 <hashmap>)
  (hash-table-seek m1 (^[k _] (hash-table-exists? m2 k))
                 (^[r k v] #f)
                 (^[] #t)))

(define %unique (list #f))

(define (hashmap-ref m key
                     :optional
                     (failure #f)
                     (success identity))
  (assume-type m <hashmap>)
  (if failure
    (let1 v (hash-table-get m key %unique)
      (if (eq? v %unique)
        (failure)
        (success v)))
    (success (hash-table-get m key))))    ;let hash-table-get handle failure

(define (hashmap-ref/default m key default)
  (assume-type m <hashmap>)
  (hash-table-get m key default))

(define (hashmap-key-comparator m) (hash-table-comparator m))

(define (hashmap-set m . args)
  (if (null? args)
    (begin
      (assume-type m <hashmap>)
      m)                                ;shortcut
    (apply hashmap-set! (hashmap-copy m) args)))

(define (hashmap-set! m . args)
  (assume-type m <hashmap>)
  (dopairs [k v args] (hash-table-put! m k v))
  m)

(define (hashmap-adjoin m . args)
  (if (null? args)
    (begin
      (assume-type m <hashmap>)
      m)                                ;shortcut
    (apply hashmap-adjoin! (hash-table-copy m) args)))

(define (hashmap-adjoin! m . args)
  (assume-type m <hashmap>)
  (dopairs [k v args] (hash-table-adjoin! m k v))
  m)

(define (hashmap-replace m k v)
  (assume-type m <hashmap>)
  (if (hash-table-exists? m k)
    (hashmap-replace! (hash-table-copy m) k v)
    m))

(define (hashmap-replace! m k v)
  (assume-type m <hashmap>)
  (hash-table-replace! m k v)
  m)
   
(define (hashmap-delete m . keys) (hashmap-delete-all m keys))
(define (hashmap-delete! m . keys) (hashmap-delete-all! m keys))

(define (hashmap-delete-all m keys)
  (assume-type m <hashmap>)
  ;; We delay copy until we actually modify the map.
  (fold (^[k t]
          (if (hash-table-exists? t k)
            (rlet1 t (if (eq? t m) (hash-table-copy m) t)
              (hash-table-delete! t k))
            t))
        m keys))

(define (hashmap-delete-all! m keys)
  (assume-type m <hashmap>)
  (dolist [k keys] (hash-table-delete! m k))
  m)

(define (hashmap-intern m k newval)
  (assume-type m <hashmap>)
  (let1 v (hash-table-get m k %unique)
    (if (eq? v %unique)
      (let ([t (hash-table-copy m)]
            [v (newval)])
        (hash-table-put! t k v)
        (values t v))
      (values m v))))

(define (hashmap-intern! m k newval)
  (assume-type m <hashmap>)
  (let1 v (hash-table-get m k %unique)
    (if (eq? v %unique)
      (let1 v (newval)
        (hash-table-put! m k v)
        (values m v))
      (values m v))))

(define (hashmap-update m k updater
                        :optional
                        (failure (^[] (errorf "~s doesn't have a key ~s" m k)))
                        (success identity))
  (assume-type m <hashmap>)
  ;; We delay copy until we actually modify the map.
  (let* ([v (hash-table-get m k %unique)]
         [v1 (if (eq? v %unique)
               (updater (failure))
               (updater (success v)))])
    (if (eq? v v1)
      m                               ; no action needed
      (rlet1 t (hash-table-copy m)
        (hash-table-put! t k v1)))))

(define (hashmap-update! m k updater
                        :optional
                        (failure (^[] (errorf "~s doesn't have a key ~s" m k)))
                        (success identity))
  (assume-type m <hashmap>)
  (let* ([v (hash-table-get m k %unique)]
         [v1 (if (eq? v %unique)
               (updater (failure))
               (updater (success v)))])
    (hash-table-put! m k v1))
  m)

(define (hashmap-update/default m k updater default)
  (hashmap-update m k updater (lambda () default)))

(define (hashmap-update!/default m k updater default)
  (hashmap-update! m k updater (lambda () default)))

(define (hashmap-pop! m
                      :optional
                      (failure (^[] (error "can't pop from an empty map"))))
  (assume-type m <hashmap>)
  ;; We cheat to use internal iterator to avoid traversing whole hashtable
  (let1 iter ((with-module gauche.internal %hash-table-iter) m)
    (receive (k v) (iter %unique)
      (if (eq? k %unique)
        (failure)
        (begin
          (hash-table-delete! m k)
          (values m k v))))))

(define (hashmap-pop m 
                     :optional
                     (failure (^[] (error "can't pop from an empty map"))))
  (assume-type m <hashmap>)
  (if (hash-table-empty? m)
    (failure)                           ;avoid unnecessary copying
    (hashmap-pop! (hashmap-copy m))))

(define (hashmap-search m k failure success)
  (assume-type m <hashmap>)
  (let1 v (hash-table-get m k %unique)
    (if (eq? v %unique)
      (failure (^[v o] (let1 m (hash-table-copy m) ;insert
                         (hash-table-put! m k v)
                         (values m o)))
               (^[o] (values m o)))     ;ignore
      (success k v
               (^[k v o] (let1 m (hash-table-copy m) ;update
                           (hash-table-put! m k v)
                           (values m o)))
               (^[o] (let1 m (hash-table-copy m) ;remove
                       (hash-table-delete! m k)
                       (values m o)))))))

(define (hashmap-search! m k failure success)
  (assume-type m <hashmap>)
  (let1 v (hash-table-get m k %unique)
    (if (eq? v %unique)
      (failure (^[v o] (hash-table-put! m k v) (values m o)) ;insert
               (^[o] (values m o)))                        ;ignore
      (success v
               (^[k v o] (hash-table-put! m k v) (values m o)) ;update
               (^[o] (hash-table-delete! m k) (values m o)))))) ;remove

(define (hashmap-size m)
  (assume-type m <hashmap>)
  (hash-table-num-entries m))

(define (hashmap-find pred m failure)
  (assume-type m <hashmap>)
  (hash-table-seek m pred (^[r k v] (values k v)) failure))

(define (hashmap-count pred m)
  (assume-type m <hashmap>)
  (hash-table-fold m (^[k v c] (if (pred k v) (+ 1 c) c)) 0))

(define (hashmap-any? pred m)
  (assume-type m <hashmap>)
  (hash-table-seek m pred (^[r k v] #t) (^[] #f)))

(define (hashmap-every? pred m)
  (assume-type m <hashmap>)
  (hash-table-seek m (^[k v] (not (pred k v))) (^[r k v] #f) (^[] #t)))

(define (hashmap-keys m) (hash-table-keys m))
(define (hashmap-values m) (hash-table-values m))

(define (hashmap-entries m)
  (values (hash-table-keys m) (hash-table-values m)))

(define (hashmap-map proc cmpr m)
  (assume-type m <hashmap>)
  (assume-type cmpr <comparator>)
  (rlet1 r (make-hash-table cmpr)
    (hash-table-for-each m (^[k v] (receive [k v] (proc k v)
                                   (hash-table-put! r k v))))))

(define (hashmap-for-each proc m)
  (assume-type m <hashmap>)
  (hash-table-for-each m proc))

(define (hashmap-fold kons knil m)
  (assume-type m <hashmap>)
  (hash-table-fold m kons knil))

(define (hashmap-map->list proc m)
  (assume-type m <hashmap>)
  (hash-table-map m proc))

(define (hashmap-filter pred m)
  (assume-type m <hashmap>)
  (rlet1 r (make-hash-table (hash-table-comparator m))
    (hash-table-for-each m (^[k v] (when (pred k v)
                                   (hash-table-put! r k v))))))

(define (hashmap-filter! pred m)
  (assume-type m <hashmap>)
  (hash-table-for-each m (^[k v] (unless (pred k v)
                                 (hash-table-delete! m k)))))

(define (hashmap-remove pred m)
  (assume-type m <hashmap>)
  (rlet1 r (make-hash-table (hash-table-comparator m))
    (hash-table-for-each m (^[k v] (unless (pred k v)
                                   (hash-table-put! r k v))))))

(define (hashmap-remove! pred m)
  (assume-type m <hashmap>)
  (hash-table-for-each m (^[k v] (when (pred k v)
                                 (hash-table-delete! m k)))))

(define (hashmap-partition pred m)
  (assume-type m <hashmap>)
  (let ([f (make-hash-table (hash-table-comparator m))]
        [r (make-hash-table (hash-table-comparator m))])
    (hash-table-for-each m (^[k v] (if (pred k v)
                                   (hash-table-put! f k v)
                                   (hash-table-put! r k v))))
    (values f r)))

(define (hashmap-partition! pred m)
  (assume-type m <hashmap>)
  (let1 r (make-hash-table (hash-table-comparator m))
    (hash-table-for-each m (^[k v] (unless (pred k v)
                                   (hash-table-delete! m k)
                                   (hash-table-put! r k v))))
    (values m r)))

(define (hashmap-copy m)
  (assume-type m <hashmap>)
  (hash-table-copy m))

(define (hashmap->alist m)
  (assume-type m <hashmap>)
  (hash-table-fold m acons '()))

(define (alist->hashmap cmpr alist)
  (assume-type cmpr <comparator>)
  (rlet1 m (make-hash-table cmpr)
    (dolist [p alist]
      (hash-table-adjoin! m (car p) (cdr p)))))

(define (alist->hashmap! m alist)
  (assume-type m <hashmap>)
  (dolist [p alist]
    (hash-table-adjoin! m (car p) (cdr p)))
  m)

(define (%hashmap-cmp v=? pred ms)
  (let loop ([ms ms])
    (cond [(null? (cdr ms)) #t]
          [(hash-table-compare-as-sets (car ms) (cadr ms) v=? #f)
           => (^r (and (pred r) (loop (cdr ms))))]
          [else #f])))

(define-syntax define-hashmap-cmp
  (syntax-rules ()
    [(_ name op)
     (define (name vcmp m . more)
       (assume-type vcmp <comparator>)
       (%hashmap-cmp (comparator-equality-predicate vcmp)
                     (^[x] (op x 0))
                     (cons m more)))]))

(define-hashmap-cmp hashmap=? =)
(define-hashmap-cmp hashmap<? <)
(define-hashmap-cmp hashmap<=? <=)
(define-hashmap-cmp hashmap>? >)
(define-hashmap-cmp hashmap>=? >=)

(define (%union-2! m1 m2)
  (hash-table-for-each m2 (^[k v] (hash-table-adjoin! m1 k v)))
  m1)

(define (hashmap-union! m1 . more)
  (if (null? more)
    m1
    (apply hashmap-union! (%union-2! m1 (car more)) (cdr more))))

(define (hashmap-union m1 . more)
  (apply hashmap-union! (hashmap-copy m1) more))

(define (%intersection-2! m1 m2)
  (hash-table-for-each m1 (^[k v] (unless (hash-table-get m2 k #f)
                                  (hash-table-delete! m1 k))))
  m1)

(define (hashmap-intersection! m1 . more)
  (if (null? more)
    m1
    (apply hashmap-intersection! (%intersection-2! m1 (car more)) (cdr more))))

(define (hashmap-intersection m1 . more)
  (apply hashmap-intersection! (hashmap-copy m1) more))

(define (%difference-2! m1 m2)
  (hash-table-for-each m2 (^[k v] (hash-table-delete! m1 k)))
  m1)

(define (hashmap-difference! m1 . more)
  (let loop ([m1 m1] [more more])
    (if (null? more)
      m1
      (loop (%difference-2! m1 (car more)) (cdr more)))))

(define (hashmap-difference m1 . more)
  (apply hashmap-difference! (hashmap-copy m1) more))

(define (hashmap-xor! m1 m2)
  (hash-table-for-each m2 (^[k v] (if (hash-table-get m1 k #f)
                                  (hash-table-delete! m1 k)
                                  (hash-table-put! m1 k v))))
  m1)

(define (hashmap-xor m1 m2)
  (hashmap-xor! (hashmap-copy m1) m2))

(define (make-hashmap-comparator value-cmpr)
  (define (hash-hash h)
    (let1 key-cmpr (hash-table-comparator h)
      (hash-table-fold h 0
                       (^[k v s]
                         (logxor s (combine-hash-value
                                    (comparator-hash key-cmpr k)
                                    (comparator-hash value-cmpr v)))))))
  (define (hash-equal? a b)
    (and (hash-table? a)
         (hash-table? b)
         (equal? (hash-table-comparator a) (hash-table-comparator b))
         (hashmap=? value-cmpr a b)))
  (make-comparator hashmap? hash-equal? #f hash-hash))

(define hashmap-comparator (make-hashmap-comparator default-comparator))

