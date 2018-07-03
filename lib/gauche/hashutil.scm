;;;
;;; auxiliary hashtable utilities.  to be autoloaded.
;;;
;;;   Copyright (c) 2000-2018  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.hashutil
  (export hash-table hash-table-from-pairs hash-table-r7
          hash-table-empty? hash-table-contains? hash-table-mutable?
          hash-table-empty-copy
          hash-table-size hash-table-entries hash-table-unfold
          hash-table-ref hash-table-ref/default hash-table-set!
          hash-table-update!-r7 hash-table-update!/default
          hash-table-intern!-r7 hash-table-delete!-r7 hash-table-pop!-r7
          hash-table-for-each hash-table-for-each-r7
          hash-table-map hash-table-map-r7 hash-table-map!-r7
          hash-table-map->list-r7
          hash-table-fold hash-table-fold-r7 hash-table-count-r7
          hash-table-prune!-r7
          hash-table-seek
          hash-table-find hash-table-find-r7
          hash-table-compare-as-sets hash-table=?
          hash-table-union! hash-table-intersection!
          hash-table-difference! hash-table-xor!
          boolean-hash char-hash char-ci-hash string-hash string-ci-hash
          symbol-hash number-hash hash-bound))
(select-module gauche.hashutil)

;; TRANSIENT: Precompiling with 0.9.5 doesn't load assume-type yet.
;; Remove this after 0.9.6 release.
(cond-expand
 [gauche-0.9.5 (define-syntax assume-type
                 (syntax-rules ()
                   [(_ x type) (check-arg (cut is-a? <> type) x)]))]
 [else])

(define unique (cons #f #f))

;; vararg constructor 'hash-table' has conflicting API between
;; legacy Gauche and R7RS scheme.hash-table (srfi-125).
;; R7RS's API is more convenient to use, so we eventually
;; switch to it.  Meantime, we provide original hash-table
;; with a different name, hash-table-from-pairs, and R7RS
;; hash-table as hash-table-r7.
(define (hash-table-from-pairs cmpr . kvs)
  (rlet1 h (make-hash-table cmpr)
    (for-each (^[kv] (hash-table-put! h (car kv) (cdr kv))) kvs)))

(define hash-table hash-table-from-pairs) ; transient

(define (hash-table-r7 cmpr . kvs) ; r7rs's hash-table
  (rlet1 h (make-hash-table cmpr)
    (doplist [(k v) kvs] (hash-table-put! h k v))))

(define (hash-table-empty? h) (zero? (hash-table-num-entries h))) ; r7rs
(define hash-table-contains? hash-table-exists?) ; r7rs
(define hash-table-size hash-table-num-entries)  ; r7rs

(define (hash-table-mutable? ht)      ; r7rs
  (assume-type ht <hash-table>)
  #t)

(define (hash-table-empty-copy ht)      ; r7rs
  (make-hash-table (hash-table-comparator ht)))

(define (hash-table-entries ht)         ; r7rs
  ;; can be more efficient
  (values (hash-table-keys ht) (hash-table-values ht)))

(define (hash-table-unfold p f g seed cmpr . args) ; r7rs
  (rlet1 ht (apply make-hash-table cmpr args)
    (do ([seed seed (g seed)])
        [(p seed)]
      (receive [k v] (f seed)
        (hash-table-put! ht k v)))))

;; r7rs accessors

(define (hash-table-ref ht key :optional failure (success identity)) ;r7rs
  ;; We avoid giving default value to failure, for the default closure
  ;; needs to enclose key and it's costly.
  (let1 v (hash-table-get ht key unique)
    (if (eq? v unique)
      (if (undefined? failure)
        (error "hash table doesn't have an entry for key:" key)
        (failure))
      (success v))))

(define (hash-table-ref/default ht key default) ; r7rs
  (hash-table-get ht key default))

(define (hash-table-set! ht . kvs) ; r7rs
  (doplist [[k v] kvs]
    (hash-table-put! ht k v)))

(define (hash-table-intern!-r7 ht key failure) ; r7rs
  ;; we can't use hash-table-adjoin!, for we have to evaluate failure thunk
  ;; after we know the entry's not in the table.
  (let1 v (hash-table-get ht key unique)
    (if (eq? v unique)
      (rlet1 v (failure)
        (hash-table-put! ht key v))
      v)))

(define (hash-table-delete!-r7 ht . keys) ; r7rs
  (fold (^[key count] (if (hash-table-delete! ht key) (+ count 1) count))
        0 keys))
        
(define (hash-table-update!/default ht key updater default) ;r7rs
  (hash-table-update! ht key updater default))

(define (hash-table-update!-r7 ht key updater ; r7rs
                               :optional failure (success identity))
  (let1 v (hash-table-get ht key unique)
    (if (eq? v unique)
      (if (undefined? failure)
        (error "hash table doesn't have an entry for key:" key)
        (hash-table-put! ht key (updater (failure))))
      (hash-table-put! ht key (updater (success v))))))

(define (hash-table-pop!-r7 ht)         ; r7rs
  (assume-type ht <hash-table>)
  (let1 i ((with-module gauche.internal %hash-table-iter) ht)
    (receive [k v] (i unique)
      (when (eq? k unique)
        (error "empty hash table can't be popped:" ht))
      (hash-table-delete! ht k)
      (values k v))))

;; hash-table-map also disagree between Gauche and R7RS.
;; We keep Gauche API, for we have consistent interface with other *-map
;; procedures.  We add a check in case the caller mistook this with R7RS one.
(define (hash-table-map ht proc :optional arg)
  (when (not (undefined? arg))
    (error "Gauche's bulit-in hash-table-map is called with R7RS interface. \
            Use hash-table-map-r7, or say (use scheme.hash-table)."))
  (assume-type ht <hash-table>)
  (let1 i ((with-module gauche.internal %hash-table-iter) ht)
    (let loop ([r '()])
      (receive [k v] (i unique)
        (if (eq? k unique)
          r
          (loop (cons (proc k v) r)))))))

;; This is R7RS version of hash-table-map
(define (hash-table-map-r7 proc cmpr ht) ; r7rs
  (assume-type cmpr <comparator>)
  (assume-type ht <hash-table>)
  (rlet1 r (make-hash-table cmpr)
    (hash-table-for-each ht (^[k v] (hash-table-put! r k (proc v))))))

(define (hash-table-map!-r7 proc ht) ; r7rs
  (assume-type ht <hash-table>)
  (hash-table-for-each ht (^[k v] (hash-table-put! ht k (proc v)))))

(define (hash-table-map->list-r7 proc ht) ; r7rs
  (hash-table-map ht proc))

(define (hash-table-for-each ht proc)
  (assume-type ht <hash-table>)
  (let1 i ((with-module gauche.internal %hash-table-iter) ht)
    (let loop ()
      (receive [k v] (i unique)
        (unless (eq? k unique)
          (proc k v) (loop))))))

(define (hash-table-for-each-r7 proc ht) ; r7rs
  (hash-table-for-each ht proc))

(define (hash-table-fold ht kons knil)
  (assume-type ht <hash-table>)
  (let1 i ((with-module gauche.internal %hash-table-iter) ht)
    (let loop ([r knil])
      (receive [k v] (i unique)
        (if (eq? k unique)
          r
          (loop (kons k v r)))))))

(define (hash-table-fold-r7 kons knil ht) ; r7rs
  (hash-table-fold ht cons knil))

(define (hash-table-prune!-r7 proc ht) ; r7rs
  (hash-table-for-each ht (^[k v] (when (proc k v) (hash-table-delete! ht k)))))

(define (hash-table-seek ht pred succ fail)
  (assume-type ht <hash-table>)
  (let1 i ((with-module gauche.internal %hash-table-iter) ht)
    (let loop ()
      (receive [k v] (i unique)
        (cond [(eq? k unique) (fail)]
              [(pred k v) => (^r (succ r k v))]
              [else (loop)])))))

;; hash-table-find.  This doesn't align with other '*-find' API in a way that
;; it returns the result of PRED.
(define (hash-table-find ht pred :optional (failure (^[] #f)))
  (hash-table-seek ht pred (^[r k v] r) failure))
  
(define (hash-table-find-r7 pred ht failure) ; r7rs
  (hash-table-seek ht pred (^[r k v] r) failure))

(define (hash-table-count-r7 pred ht)   ; r7rs
  ($ hash-table-fold ht
     (^[k v count] (if (pred k v) (+ count 1) count)) 0))

;; Set operations

(define (hash-table-union! ht1 ht2)
  (assume-type ht1 <hash-table>)
  (assume-type ht2 <hash-table>)
  ($ hash-table-for-each ht2
     (^[k v] (unless (hash-table-exists? ht1 k)
               (hash-table-put! ht1 k v))))
  ht1)

(define (hash-table-intersection! ht1 ht2)
  (assume-type ht1 <hash-table>)
  (assume-type ht2 <hash-table>)
  ($ hash-table-for-each ht1
     (^[k v] (unless (hash-table-exists? ht2 k)
               (hash-table-delete! ht1 k))))
  ht1)

(define (hash-table-difference! ht1 ht2)
  (assume-type ht1 <hash-table>)
  (assume-type ht2 <hash-table>)
  ($ hash-table-for-each ht1
     (^[k v] (when (hash-table-exists? ht2 k)
               (hash-table-delete! ht1 k))))
  ht1)

(define (hash-table-xor! ht1 ht2)
  (assume-type ht1 <hash-table>)
  (assume-type ht2 <hash-table>)
  ($ hash-table-for-each ht2
     (^[k v] (if (hash-table-exists? ht1 k)
               (hash-table-delete! ht1 k)
               (hash-table-put! ht1 k v))))
  ht1)

;; We delegate most hash calculation to the built-in default-hash.
;; These type-specific hash functions are mostly
;; for the compatibility of srfi-128.

(define (boolean-hash obj)
  (assume-type obj <boolean>)
  (default-hash obj))

(define (char-hash obj)
  (assume-type obj <char>)
  (default-hash obj))

(define (char-ci-hash obj)
  (assume-type obj <char>)
  (default-hash (char-foldcase obj)))

(define (string-hash obj)
  (assume-type obj <string>)
  (default-hash obj))

(autoload gauche.unicode string-foldcase)

(define (string-ci-hash obj)
  (assume-type obj <string>)
  (default-hash (string-foldcase obj)))

(define (symbol-hash obj)
  (assume-type obj <symbol>)
  (default-hash obj))

(define (number-hash obj)
  (assume-type obj <number>)
  (default-hash obj))

;; This is a placeholder to conform srfi-128.
(define (hash-bound) (greatest-fixnum))

;; Compare two hash-tables as sets.
(define (hash-table-compare-as-sets h1 h2
                                    :optional (value=? equal?)
                                              (fallback (undefined)))
  (define unique (cons #f #f))
  (define (fail)
    (if (undefined? fallback)
      (error "hash-tables can't be ordered:" h1 h2)
      fallback))
  ;; Returns #t iff smaller is a subset of larger.
  (define (subset? smaller larger)
    (hash-table-seek smaller
                     (^[k v] (let1 w (hash-table-get larger k unique)
                               (or (eq? unique w)
                                   (not (value=? v w)))))
                     (^[r k v] #f)
                     (^[] #t)))

  ;; Check comparator compatibility
  (let ([c1 (hash-table-comparator h1)]
        [c2 (hash-table-comparator h2)])
    (cond [(and c1 c2 (equal? c1 c2)) c1]
          [(or c1 c2) (error "hash-tables with different comparators can't be \
                              compared:" h1 h2)]
          [else (error "hash-tables don't have comparators and can't be \
                        compared:" h1 h2)]))  
  ;; Let's start
  (if (eq? h1 h2)
    0                 ;fast path
    (let ([n1 (hash-table-num-entries h1)]
          [n2 (hash-table-num-entries h2)])
      (cond
       [(= n1 n2) (if (subset? h1 h2) 0 (fail))]
       [(< n1 n2) (if (subset? h1 h2) -1 (fail))]
       [else      (if (subset? h2 h1) 1 (fail))]))))

(define (hash-table=? value-cmpr h1 h2)
  (or (eq? h1 h2)
      (and (= (hash-table-num-entries h1) (hash-table-num-entries h2))
           (zero? (hash-table-compare-as-sets h1 h2
                                              (cut =? value-cmpr <> <>)
                                              100))))) ;; any non-zero number
