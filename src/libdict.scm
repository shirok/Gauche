;;;
;;; libdict.scm - dictionary-like data structures (hashtables etc.)
;;;
;;;   Copyright (c) 2000-2020  Shiro Kawai  <shiro@acm.org>
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

(select-module gauche.internal)

;;;
;;;  Dictionary common macros
;;;

(inline-stub
 (define-cise-stmt dict-check-entry
   [(_ dict key expr)
    `(when ,expr (Scm_Error "%S doesn't have an entry for key %S" ,dict ,key))])

 (define-cise-stmt dict-get
   [(_ dict referencer)
    `(let* ([v (,referencer ,dict key fallback)])
       (dict-check-entry ,dict key (SCM_UNBOUNDP v))
       (return v))])

 (define-cise-expr dict-exists?
   [(_ dict referencer)
    `(not (SCM_UNBOUNDP (,referencer ,dict key SCM_UNBOUND)))])

 (define-cise-stmt dict-update!
   [(_ dict searcher xtractor cc) ;; assumes key, proc, and fallback
    `(let* ([e::ScmDictEntry*]
            [data::(.array void* (1))])
       (cond [(SCM_UNBOUNDP fallback)
              (set! e (,searcher (,xtractor ,dict) (cast intptr_t key)
                                 SCM_DICT_GET))
              (dict-check-entry ,dict key (== e NULL))]
             [else
              (set! e (,searcher (,xtractor ,dict) (cast intptr_t key)
                                 SCM_DICT_CREATE))
              (unless (-> e value)
                (cast void (SCM_DICT_SET_VALUE e fallback)))])
       (set! (aref data 0) (cast void* e))
       (Scm_VMPushCC ,cc data 1)
       (return (Scm_VMApply1 proc (SCM_DICT_VALUE e))))])

 (define-cise-stmt dict-push!
   [(_ dict searcher xtractor)
    `(let* ([e::ScmDictEntry* (,searcher (,xtractor ,dict )
                                         (cast intptr_t key)
                                         SCM_DICT_CREATE)]
            [prev (?: (-> e value) (SCM_DICT_VALUE e) '())])
       (cast void (SCM_DICT_SET_VALUE e (Scm_Cons value prev))))])

 (define-cise-stmt dict-pop!
   [(_ dict searcher xtractor)
    `(let* ([e::ScmDictEntry* (,searcher (,xtractor ,dict)
                                         (cast intptr_t key)
                                         SCM_DICT_GET)])
       (cond
        [(not e)
         (dict-check-entry ,dict key (SCM_UNBOUNDP fallback))
         (return fallback)]
        [(not (SCM_PAIRP (SCM_DICT_VALUE e)))
         (when (SCM_UNBOUNDP fallback)
           (Scm_Error "%S's value for key %S is not a pair: %S"
                      ,dict key (SCM_DICT_VALUE e)))
         (return fallback)]
        [else
         (let* ([resultval_ (SCM_CAR (SCM_DICT_VALUE e))])
           (cast void (SCM_DICT_SET_VALUE e (SCM_CDR (SCM_DICT_VALUE e))))
           (return resultval_))]))])
 )

;; An unique object, used to detect exhausted iterator
(define *unique* (cons #f #f))

;;;
;;; Hashtables
;;;

(select-module gauche)
(inline-stub
 ;; this is called only for simple hash types, so don't need to handle
 ;; SCM_HASH_GENEAL.
 (define-cise-stmt set-hash-type!
   [(_ cvar scmvar)
    `(cond [(SCM_EQ ,scmvar 'eq?)      (set! ,cvar SCM_HASH_EQ)]
           [(SCM_EQ ,scmvar 'eqv?)     (set! ,cvar SCM_HASH_EQV)]
           [(SCM_EQ ,scmvar 'equal?)   (set! ,cvar SCM_HASH_EQUAL)]
           [(SCM_EQ ,scmvar 'string=?) (set! ,cvar SCM_HASH_STRING)]
           [else (Scm_Error "unsupported hash type: %S" ,scmvar)])])

 (define-cise-stmt get-hash-type
   [(_ expr)
    `(case ,expr
       [(SCM_HASH_EQ)      (return 'eq?)]
       [(SCM_HASH_EQV)     (return 'eqv?)]
       [(SCM_HASH_EQUAL)   (return 'equal?)]
       [(SCM_HASH_STRING)  (return 'string=?)]
       [(SCM_HASH_GENERAL) (return 'general)]
       [else (return '#f)]           ; TODO: need to think over
       )])
 )

(select-module gauche.internal)
(define-cproc %current-recursive-hash (:optional obj) Scm_CurrentRecursiveHash)

(select-module gauche)
(define-cproc hash-salt () ::<fixnum> Scm_HashSaltRef)

(define-cproc eq-hash (obj)  ::<ulong> :fast-flonum Scm_EqHash)
(define-cproc eqv-hash (obj) ::<ulong> :fast-flonum Scm_EqvHash)
(define-cproc legacy-hash (obj) ::<ulong> :fast-flonum Scm_Hash)
(define-cproc portable-hash (obj salt::<fixnum>) ::<ulong>
  :fast-flonum Scm_PortableHash)
(define-cproc default-hash (obj) ::<fixnum> :fast-flonum Scm_DefaultHash)
(define-cproc combine-hash-value (a::<ulong> b::<ulong>) ::<ulong>
  Scm_CombineHashValue)

;; see object-hash in libomega.scm for the reason of this implementation
(define (hash obj)
  (let1 h ((with-module gauche.internal %current-recursive-hash))
    (if h (h obj) (legacy-hash obj))))

(define-cproc hash-table? (obj) ::<boolean> :fast-flonum SCM_HASH_TABLE_P)

(define-cproc %make-hash-table-simple (type init-size::<int>)
  (let* ([ctype::int 0])
    (set-hash-type! ctype type)
    (return (Scm_MakeHashTableSimple ctype init-size))))

(inline-stub
(define-cfn generic-hashtable-hash (h::(const ScmHashCore*) key::intptr_t)
  ::u_long :static
  (let* ([c::ScmComparator* (cast ScmComparator* (-> h data))]
         [v::ScmObj (Scm_ApplyRec1 (Scm_ComparatorHashFunction c)
                                   (SCM_OBJ key))])
    (unless (or (SCM_INTP v) (SCM_BIGNUMP v))
      (Scm_Error "Comparator %S's hash function should return \
                  an exact integer, but got: %S" c v))
    (return (Scm_GetIntegerUMod v))))

(define-cfn generic-hashtable-hash-typecheck (h::(const ScmHashCore*)
                                              key::intptr_t)
  ::u_long :static
  (let* ([c::ScmComparator* (cast ScmComparator* (-> h data))]
         [t::ScmObj (Scm_ApplyRec1 (-> c typeFn) (SCM_OBJ key))])
    (when (SCM_FALSEP t)
      (Scm_Error "Invalid key for hashtable: %S" (SCM_OBJ key)))
    (return (generic-hashtable-hash h key))))

(define-cfn generic-hashtable-eq (h::(const ScmHashCore*)
                                  a::intptr_t b::intptr_t)
  ::int :static
  (let* ([c::ScmComparator* (cast ScmComparator* (-> h data))]
         [e::ScmObj (Scm_ApplyRec2 (-> c eqFn) (SCM_OBJ a) (SCM_OBJ b))])
    (return (not (SCM_FALSEP e)))))

(define-cfn generic-hashtable-eq-typecheck (h::(const ScmHashCore*)
                                            a::intptr_t b::intptr_t)
  ::int :static
  (let* ([c::ScmComparator* (cast ScmComparator* (-> h data))]
         [t::ScmObj (Scm_ApplyRec1 (-> c typeFn) (SCM_OBJ a))])
    ;; NB: a is the key given from outside, and b is the key that's already
    ;; in the table, so we only need to check a.
    ;; TODO: Currently we may perform typecheck of a multiple times if
    ;; we have more than one items with the same hash value in the table;
    ;; optimization required.
    (when (SCM_FALSEP t)
      (Scm_Error "Invalid key for hashtable: %S" (SCM_OBJ a)))
    (return (generic-hashtable-eq h a b))))
)

(define-cproc %make-hash-table-from-comparator (comparator::<comparator>
                                                init-size::<int>
                                                has-type-check::<boolean>)
  (if has-type-check
    (return (Scm_MakeHashTableFull generic-hashtable-hash-typecheck
                                   generic-hashtable-eq-typecheck
                                   init-size
                                   comparator))
    (return (Scm_MakeHashTableFull generic-hashtable-hash
                                   generic-hashtable-eq
                                   init-size
                                   comparator))))

;; Comparator argument can be <comparator> or one of the symbols
;; eq?, eqv?, equal? or string=?.
(define (make-hash-table :optional (comparator 'eq?) (init-size 0))
  (case comparator
    [(eq? eqv? equal? string=?)
     (%make-hash-table-simple comparator init-size)]
    [else
     (unless (comparator? comparator)
       (error "make-hash-table requires a comparator or \
               one of the symbols in eq?, eqv?, equal? or string=?, but got:"
              comparator))
     ;; If comparator's equality predicate is eq?/eqv?, we ignore
     ;; the its hash function and force using eq-/eqv-hash,
     ;; as permitted in srfi-125.  It allows object that doesn't have
     ;; custom hash method can still be used with hashtables based on
     ;; (make-eq[v]-comparator).
     ;; https://github.com/shirok/Gauche/issues/708
     (cond
      [(or (eq? comparator eq-comparator)
           (eq? (comparator-equality-predicate comparator) eq?))
       (make-hash-table 'eq? init-size)]
      [(or (eq? comparator eqv-comparator)
           (eq? (comparator-equality-predicate comparator) eqv?))
       (make-hash-table 'eqv? init-size)]
      [(eq? comparator equal-comparator)
       (make-hash-table 'equal? init-size)]
      [(eq? comparator string-comparator)
       (make-hash-table 'string=? init-size)]
      [else
       (unless (comparator-hashable? comparator)
         (error "make-hash-table requires a comparator with hash function, \
                 but got:" comparator))
       ($ %make-hash-table-from-comparator
          comparator init-size
          (not (eq? (comparator-type-test-predicate comparator)
                    (with-module gauche.internal default-type-test))))])]))

(define-cproc hash-table-type (hash::<hash-table>)
  (get-hash-type (-> hash type)))

(select-module gauche.internal)
(define-cproc %hash-table-comparator-int (hash::<hash-table>)
  (if (== (Scm_HashTableType hash) SCM_HASH_GENERAL)
    (begin
      (let* ([r (SCM_OBJ (-> (SCM_HASH_TABLE_CORE hash) data))])
        (unless (SCM_COMPARATORP r)
          (Scm_Error "Got some weird hashtable - possibly internal bug: %S"
                     hash))
        (return r)))
    (return '#f)))
(select-module gauche)
(define (hash-table-comparator hash)
  (case (hash-table-type hash)
    [(eq?) eq-comparator]
    [(eqv?) eqv-comparator]
    [(equal?) equal-comparator]
    [(string=?) string-comparator]
    [(general) ((with-module gauche.internal %hash-table-comparator-int) hash)]
    [else (error "unknown hashtable type:" hash)]))

(define-cproc hash-table-num-entries (hash::<hash-table>) ::<int>
  (return (Scm_HashCoreNumEntries (SCM_HASH_TABLE_CORE hash))))

(define-cproc hash-table-clear! (hash::<hash-table>) ::<void>
  (Scm_HashCoreClear (SCM_HASH_TABLE_CORE hash)))

(define-cproc hash-table-get (hash::<hash-table> key :optional fallback)
  (dict-get hash Scm_HashTableRef))

(define-cproc hash-table-put! (hash::<hash-table> key value) ::<void>
  (Scm_HashTableSet hash key value 0))

(define-cproc hash-table-adjoin! (hash::<hash-table> key value) ::<void>
  (Scm_HashTableSet hash key value SCM_DICT_NO_OVERWRITE))

(define-cproc hash-table-replace!  (hash::<hash-table> key value) ::<void>
  (Scm_HashTableSet hash key value SCM_DICT_NO_CREATE))

;; this is hash-table-remove! in STk.  I use `delete' for
;; it's consistent with SRFI-1 and dbm-delete!.
(define-cproc hash-table-delete! (hash::<hash-table> key) ::<boolean>
  (return (not (SCM_UNBOUNDP (Scm_HashTableDelete hash key)))))

(define-cproc hash-table-exists? (hash::<hash-table> key) ::<boolean>
  (return (dict-exists? hash Scm_HashTableRef)))

(inline-stub
 (define-cfn hash-table-update-cc (result (data :: void**)) :static
   (let* ([e::ScmDictEntry* (cast ScmDictEntry* (aref data 0))])
     (cast void (SCM_DICT_SET_VALUE e result))
     (return result)))
 )

(define-cproc hash-table-update! (hash::<hash-table> key proc
                                                     :optional fallback)
  (dict-update! hash Scm_HashCoreSearch SCM_HASH_TABLE_CORE
                hash-table-update-cc))

(define-cproc hash-table-push! (hash::<hash-table> key value) ::<void>
  (dict-push! hash Scm_HashCoreSearch SCM_HASH_TABLE_CORE))

(define-cproc hash-table-pop! (hash::<hash-table> key :optional fallback)
  (dict-pop! hash Scm_HashCoreSearch SCM_HASH_TABLE_CORE))

(inline-stub
 (define-cfn hash-table-iter (args::ScmObj* nargs::int data::void*) :static
   (cast void nargs) ; suppress unused var warning
   (let* ([iter::ScmHashIter* (cast ScmHashIter* data)]
          [e::ScmDictEntry* (Scm_HashIterNext iter)]
          [eofval (aref args 0)])
     (if (== e NULL)
       (return (values eofval eofval))
       (return (values (SCM_DICT_KEY e) (SCM_DICT_VALUE e))))))
 )

(select-module gauche.internal)
(define-cproc %hash-table-iter (hash::<hash-table>)
  (let* ([iter::ScmHashIter* (SCM_NEW ScmHashIter)])
    (Scm_HashIterInit iter (SCM_HASH_TABLE_CORE hash))
    (return (Scm_MakeSubr hash_table_iter iter 1 0 '"hash-table-iterator"))))

(select-module gauche)
(define-cproc hash-table-copy (ht::<hash-table> :optional mutable?)
  ;; we don't have immutable hash table, and ignore mutable? argument (srfi-125)
  (cast void mutable?) ; suppress unused var warning
  (return (Scm_HashTableCopy ht)))
(define-cproc hash-table-keys (ht::<hash-table>)   Scm_HashTableKeys)
(define-cproc hash-table-values (ht::<hash-table>) Scm_HashTableValues)
(define-cproc hash-table-stat (ht::<hash-table>)   Scm_HashTableStat)

;; used in some internal routines
(define (hash-table-fold ht kons knil)
  (assume-type ht <hash-table>)
  (let1 i ((with-module gauche.internal %hash-table-iter) ht)
    (let loop ([r knil])
      (receive [k v] (i (with-module gauche.internal *unique*))
        (if (eq? k (with-module gauche.internal *unique*))
          r
          (loop (kons k v r)))))))

;; conversion to/from hash-table
(define (alist->hash-table a . opt-cmpr)
  (rlet1 tb (apply make-hash-table opt-cmpr)
    (for-each (^x (hash-table-put! tb (car x) (cdr x))) a)))

(define (hash-table->alist h)
  (hash-table-map h cons))

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
    (error "Gauche's built-in hash-table-map is called with R7RS interface. \
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
  (hash-table-for-each ht (^[k v] (hash-table-put! ht k (proc k v)))))

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

(define (hash-table-fold-r7 kons knil ht) ; r7rs
  (hash-table-fold ht kons knil))

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
           (equal? (hash-table-comparator h1) (hash-table-comparator h2))
           (zero? (hash-table-compare-as-sets h1 h2
                                              (cut =? value-cmpr <> <>)
                                              100))))) ;; any non-zero number

;;;
;;; TreeMap
;;;

(select-module gauche)
(inline-stub
 ;; If tree-map-cmp is called, core->data should contain a comparator.
 (define-cfn tree-map-cmp (core::ScmTreeCore* x::intptr_t y::intptr_t)
   ::int :static
   (let* ([cmpr (SCM_OBJ (-> core data))])
     (SCM_ASSERT (and cmpr (SCM_COMPARATORP cmpr)))
     (let* ([r (Scm_ApplyRec2 (Scm_ComparatorComparisonProcedure
                               (SCM_COMPARATOR cmpr))
                              (SCM_OBJ x) (SCM_OBJ y))])
       (unless (SCM_INTP r)
         (Scm_Error "compare procedure of tree-map's comparator %S returned \
                     non-integral value: %S" cmpr r))
       (return (SCM_INT_VALUE r)))))
 )

(define-cproc %make-tree-map (comparator)
  (begin
    (SCM_ASSERT (SCM_COMPARATORP comparator))
    (return (Scm_MakeTreeMap tree_map_cmp comparator))))

;; TODO: We do want to return something even for tree-maps that aren't
;; created from the Scheme world.  But how?
(define-cproc tree-map-comparator (tm::<tree-map>)
  (let* ([d::void* (-> (SCM_TREE_MAP_CORE tm) data)])
    (if (or (== d NULL)
            (!= (-> (SCM_TREE_MAP_CORE tm) cmp) tree-map-cmp))
      (return SCM_FALSE)
      (begin
        (SCM_ASSERT (SCM_COMPARATORP d))
        (return (SCM_OBJ d))))))

(define-cproc tree-map-copy (tm::<tree-map>) Scm_TreeMapCopy)

(define-cproc tree-map? (obj) ::<boolean> SCM_TREE_MAP_P)

(define-cproc tree-map-get (tm::<tree-map> key :optional fallback)
  (dict-get tm Scm_TreeMapRef))

(define-cproc tree-map-put! (tm::<tree-map> key val) ::<void>
  (Scm_TreeMapSet tm key val 0))

(define-cproc tree-map-adjoin! (tm::<tree-map> key val) ::<void>
  (Scm_TreeMapSet tm key val SCM_DICT_NO_OVERWRITE))

(define-cproc tree-map-replace! (tm::<tree-map> key val) ::<void>
  (Scm_TreeMapSet tm key val SCM_DICT_NO_CREATE))

(define-cproc tree-map-delete! (tm::<tree-map> key) ::<boolean>
  (return (not (SCM_UNBOUNDP (Scm_TreeMapDelete tm key)))))

(inline-stub
 (define-cfn tree-map-update-cc (result data::void**) :static
   (let* ([e::ScmDictEntry* (cast ScmDictEntry* (aref data 0))])
     (cast void (SCM_DICT_SET_VALUE e result))
     (return result)))
 )

(define-cproc tree-map-update! (tm::<tree-map> key proc :optional fallback)
  (dict-update! tm Scm_TreeCoreSearch SCM_TREE_MAP_CORE tree_map_update_cc))

(define-cproc tree-map-push! (tm::<tree-map> key value) ::<void>
  (dict-push! tm Scm_TreeCoreSearch SCM_TREE_MAP_CORE))

(define-cproc tree-map-pop! (tm::<tree-map> key :optional fallback)
  (dict-pop! tm Scm_TreeCoreSearch SCM_TREE_MAP_CORE))

(define-cproc tree-map-exists? (tm::<tree-map> key) ::<boolean>
  (return (dict-exists? tm Scm_TreeMapRef)))

(define-cproc tree-map-num-entries (tm::<tree-map>) ::<int>
  (return (Scm_TreeCoreNumEntries (SCM_TREE_MAP_CORE tm))))

(select-module gauche.internal)
(define-cproc %tree-map-bound (tm::<tree-map> min::<boolean> pop::<boolean>)
  (let* ([op::ScmTreeCoreBoundOp (?: min SCM_TREE_CORE_MIN SCM_TREE_CORE_MAX)]
         [e::ScmDictEntry*
          (?: pop
              (Scm_TreeCorePopBound (SCM_TREE_MAP_CORE tm) op)
              (Scm_TreeCoreGetBound (SCM_TREE_MAP_CORE tm) op))])
    (if e
      (return (Scm_Cons (SCM_DICT_KEY e) (SCM_DICT_VALUE e)))
      (return '#f))))

(inline-stub
 (define-cfn tree-map-iter (args::ScmObj* nargs::int data::void*) :static
   (cast void nargs) ; suppress unused var warning
   (let* ([iter::ScmTreeIter* (cast ScmTreeIter* data)]
          [e::ScmDictEntry*   (?: (SCM_FALSEP (aref args 1))
                                  (Scm_TreeIterNext iter)
                                  (Scm_TreeIterPrev iter))])
     (if (not e)
       (return (values (aref args 0) (aref args 0)))
       (return (values (SCM_DICT_KEY e) (SCM_DICT_VALUE e))))))
 )

(select-module gauche.internal)
(define-cproc %tree-map-iter (tm::<tree-map>)
  (let* ([iter::ScmTreeIter* (SCM_NEW ScmTreeIter)])
    (Scm_TreeIterInit iter (SCM_TREE_MAP_CORE tm) NULL)
    (return (Scm_MakeSubr tree_map_iter iter 2 0 '"tree-map-iterator"))))

(select-module gauche.internal)
(define-cproc %tree-map-check-consistency (tm::<tree-map>)
  (Scm_TreeCoreCheckConsistency (SCM_TREE_MAP_CORE tm))
  (return '#t))

(select-module gauche.internal)
(define-cproc %tree-map-dump (tm::<tree-map>) ::<void>
  (Scm_TreeMapDump tm SCM_CUROUT))

(select-module gauche)
(define-cproc tree-map-clear! (tm::<tree-map>) ::<void>
  (Scm_TreeCoreClear (SCM_TREE_MAP_CORE tm)))

(inline-stub
 ;;
 ;; Finds the entry closest to the given key
 ;;
 (define-cise-stmt tree-map-closest-entry
   [(_ inclusive? lh make-result)
    (if inclusive?
      `(let* ([lo::ScmDictEntry* NULL]
              [hi::ScmDictEntry* NULL]
              [eq::ScmDictEntry*
               (Scm_TreeCoreClosestEntries (SCM_TREE_MAP_CORE tm)
                                           (cast intptr_t key)
                                           (& lo) (& hi))])
         (cond
          [(!= eq NULL)  (,make-result (SCM_DICT_KEY eq) (SCM_DICT_VALUE eq))]
          [(!= ,lh NULL) (,make-result (SCM_DICT_KEY ,lh) (SCM_DICT_VALUE ,lh))]
          [else          (,make-result key-fb val-fb)]))
      `(let* ([lo::ScmDictEntry* NULL]
              [hi::ScmDictEntry* NULL])
         (Scm_TreeCoreClosestEntries (SCM_TREE_MAP_CORE tm)
                                     (cast intptr_t key)
                                     (& lo) (& hi))
         (if (!= ,lh NULL)
           (,make-result (SCM_DICT_KEY ,lh) (SCM_DICT_VALUE ,lh))
           (,make-result key-fb val-fb))))])

 (define-cise-stmt tree-map-closest-key-result [(_ k v) `(return ,k)])
 (define-cise-stmt tree-map-closest-val-result [(_ k v) `(return ,v)])
 (define-cise-stmt tree-map-closest-kv-result  [(_ k v) `(return ,k ,v)])
 )

(define-cproc tree-map-floor
  (tm::<tree-map> key :optional (key-fb #f) (val-fb #f)) ::(<top> <top>)
  (tree-map-closest-entry #t lo tree-map-closest-kv-result))
(define-cproc tree-map-floor-key (tm::<tree-map> key :optional (key-fb #f))
  (tree-map-closest-entry #t lo tree-map-closest-key-result))
(define-cproc tree-map-floor-value (tm::<tree-map> key :optional (val-fb #f))
  (tree-map-closest-entry #t lo tree-map-closest-val-result))

(define-cproc tree-map-ceiling
  (tm::<tree-map> key :optional (key-fb #f) (val-fb #f)) ::(<top> <top>)
  (tree-map-closest-entry #t hi tree-map-closest-kv-result))
(define-cproc tree-map-ceiling-key (tm::<tree-map> key :optional (key-fb #f))
  (tree-map-closest-entry #t hi tree-map-closest-key-result))
(define-cproc tree-map-ceiling-value (tm::<tree-map> key :optional (val-fb #f))
  (tree-map-closest-entry #t hi tree-map-closest-val-result))

(define-cproc tree-map-predecessor
  (tm::<tree-map> key :optional (key-fb #f) (val-fb #f)) ::(<top> <top>)
  (tree-map-closest-entry #f lo tree-map-closest-kv-result))
(define-cproc tree-map-predecessor-key
  (tm::<tree-map> key :optional (key-fb #f))
  (tree-map-closest-entry #f lo tree-map-closest-key-result))
(define-cproc tree-map-predecessor-value
  (tm::<tree-map> key :optional (val-fb #f))
  (tree-map-closest-entry #f lo tree-map-closest-val-result))

(define-cproc tree-map-successor
  (tm::<tree-map> key :optional (key-fb #f) (val-fb #f)) ::(<top> <top>)
  (tree-map-closest-entry #f hi tree-map-closest-kv-result))
(define-cproc tree-map-successor-key
  (tm::<tree-map> key :optional (key-fb #f))
  (tree-map-closest-entry #f hi tree-map-closest-key-result))
(define-cproc tree-map-successor-value
  (tm::<tree-map> key :optional (val-fb #f))
  (tree-map-closest-entry #f hi tree-map-closest-val-result))
