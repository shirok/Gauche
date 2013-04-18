;;;
;;; libdict.scm - dictionary-like data structures (hashtables etc.)
;;;
;;;   Copyright (c) 2000-2013  Shiro Kawai  <shiro@acm.org>
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
       (result v))])

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
       (result (Scm_VMApply1 proc (SCM_DICT_VALUE e))))])

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
         (result fallback)]
        [(not (SCM_PAIRP (SCM_DICT_VALUE e)))
         (when (SCM_UNBOUNDP fallback)
           (Scm_Error "%S's value for key %S is not a pair: %S"
                      ,dict key (SCM_DICT_VALUE e)))
         (result fallback)]
        [else
         (result (SCM_CAR (SCM_DICT_VALUE e)))
         (cast void (SCM_DICT_SET_VALUE e (SCM_CDR (SCM_DICT_VALUE e))))]))])
 )

;;;
;;; Hashtables
;;;

(select-module gauche)
(inline-stub
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
       [(SCM_HASH_EQ)      (result 'eq?)]
       [(SCM_HASH_EQV)     (result 'eqv?)]
       [(SCM_HASH_EQUAL)   (result 'equal?)]
       [(SCM_HASH_STRING)  (result 'string=?)]
       [else (result '#f)]           ; TODO: need to think over
       )])
 )

(define-cproc eq-hash (obj)  ::<ulong> :fast-flonum Scm_EqHash)
(define-cproc eqv-hash (obj) ::<ulong> :fast-flonum Scm_EqvHash)
(define-cproc hash (obj)     ::<ulong> :fast-flonum Scm_Hash)
(define-cproc hash-table? (obj) ::<boolean> :fast-flonum SCM_HASH_TABLE_P)

(define-cproc make-hash-table (:optional (type eq?))
  (let* ([ctype::int 0])
    (set-hash-type! ctype type)
    (result (Scm_MakeHashTableSimple ctype 0))))

(define-cproc hash-table-type (hash::<hash-table>)
  (get-hash-type (-> hash type)))

(define-cproc hash-table-num-entries (hash::<hash-table>) ::<int>
  (result (Scm_HashCoreNumEntries (SCM_HASH_TABLE_CORE hash))))

(define-cproc hash-table-clear! (hash::<hash-table>) ::<void>
  (Scm_HashCoreClear (SCM_HASH_TABLE_CORE hash)))

(define-cproc hash-table-get (hash::<hash-table> key :optional fallback)
  (dict-get hash Scm_HashTableRef))

(define-cproc hash-table-put! (hash::<hash-table> key value) ::<void>
  (Scm_HashTableSet hash key value 0))

;; this is hash-table-remove! in STk.  I use `delete' for
;; it's consistent with SRFI-1 and dbm-delete!.
(define-cproc hash-table-delete! (hash::<hash-table> key) ::<boolean>
  (result (not (SCM_UNBOUNDP (Scm_HashTableDelete hash key)))))

(define-cproc hash-table-exists? (hash::<hash-table> key) ::<boolean>
  (result (dict-exists? hash Scm_HashTableRef)))

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
   (let* ([iter::ScmHashIter* (cast ScmHashIter* data)]
          [e::ScmDictEntry* (Scm_HashIterNext iter)]
          [eofval (aref args 0)])
     (if (== e NULL)
       (return (values eofval eofval))
       (return (values (SCM_DICT_KEY e) (SCM_DICT_VALUE e))))))
 )

(define-cproc %hash-table-iter (hash::<hash-table>)
  (let* ([iter::ScmHashIter* (SCM_NEW ScmHashIter)])
    (Scm_HashIterInit iter (SCM_HASH_TABLE_CORE hash))
    (result (Scm_MakeSubr hash_table_iter iter 1 0 '"hash-table-iterator"))))

(define-cproc hash-table-copy (hash::<hash-table>)   Scm_HashTableCopy)
(define-cproc hash-table-keys (hash::<hash-table>)   Scm_HashTableKeys)
(define-cproc hash-table-values (hash::<hash-table>) Scm_HashTableValues)
(define-cproc hash-table-stat (hash::<hash-table>)   Scm_HashTableStat)

;; conversion to/from hash-table
(define (alist->hash-table a . opt-eq)
  (rlet1 tb (apply make-hash-table opt-eq)
    (for-each (^x (hash-table-put! tb (car x) (cdr x))) a)))

(define (hash-table->alist h)
  (hash-table-map h cons))

;;;
;;; TreeMap
;;;

(select-module gauche)
(inline-stub
 (define-cfn tree-map-cmp (core::ScmTreeCore* x::intptr_t y::intptr_t)
   ::int :static
   (let* ([cmp-proc (SCM_OBJ (-> core data))]
          [r (Scm_ApplyRec2 cmp-proc (SCM_OBJ x) (SCM_OBJ y))])
     (return (SCM_INT_VALUE r))))
 )

(define-cproc %make-tree-map (cmp-proc)
  (result (Scm_MakeTreeMap tree_map_cmp cmp_proc)))

(define-cproc tree-map-copy (tm::<tree-map>) Scm_TreeMapCopy)

(define-cproc tree-map? (obj) ::<boolean> SCM_TREE_MAP_P)

(define-cproc tree-map-get (tm::<tree-map> key :optional fallback)
  (dict-get tm Scm_TreeMapRef))

(define-cproc tree-map-put! (tm::<tree-map> key val) ::<void>
  (Scm_TreeMapSet tm key val 0))

(define-cproc tree-map-delete! (tm::<tree-map> key) ::<boolean>
  (result (not (SCM_UNBOUNDP (Scm_TreeMapDelete tm key)))))

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
  (result (dict-exists? tm Scm_TreeMapRef)))

(define-cproc tree-map-num-entries (tm::<tree-map>) ::<int>
  (result (Scm_TreeCoreNumEntries (SCM_TREE_MAP_CORE tm))))

(define-cproc %tree-map-bound (tm::<tree-map> min::<boolean> pop::<boolean>)
  (let* ([op::ScmTreeCoreBoundOp (?: min SCM_TREE_CORE_MIN SCM_TREE_CORE_MAX)]
         [e::ScmDictEntry*
          (?: pop
              (Scm_TreeCorePopBound (SCM_TREE_MAP_CORE tm) op)
              (Scm_TreeCoreGetBound (SCM_TREE_MAP_CORE tm) op))])
    (if e
      (result (Scm_Cons (SCM_DICT_KEY e) (SCM_DICT_VALUE e)))
      (result '#f))))

(inline-stub
 (define-cfn tree-map-iter (args::ScmObj* nargs::int data::void*) :static
   (let* ([iter::ScmTreeIter* (cast ScmTreeIter* data)]
          [e::ScmDictEntry*   (?: (SCM_FALSEP (aref args 1))
                                  (Scm_TreeIterNext iter)
                                  (Scm_TreeIterPrev iter))])
     (if (not e)
       (return (values (aref args 0) (aref args 0)))
       (return (values (SCM_DICT_KEY e) (SCM_DICT_VALUE e))))))
 )

(define-cproc %tree-map-iter (tm::<tree-map>)
  (let* ([iter::ScmTreeIter* (SCM_NEW ScmTreeIter)])
    (Scm_TreeIterInit iter (SCM_TREE_MAP_CORE tm) NULL)
    (result (Scm_MakeSubr tree_map_iter iter 2 0 '"tree-map-iterator"))))

(define-cproc %tree-map-check-consistency (tm::<tree-map>)
  (Scm_TreeCoreCheckConsistency (SCM_TREE_MAP_CORE tm))
  (result '#t))

(define-cproc %tree-map-dump (tm::<tree-map>) ::<void>
  (Scm_TreeMapDump tm SCM_CUROUT))

(define-cproc tree-map-clear! (tm::<tree-map>) ::<void>
  (Scm_TreeCoreClear (SCM_TREE_MAP_CORE tm)))

(inline-stub
 ;;
 ;; Finds the entry closest to the given key
 ;;
 (define-cise-stmt tree-map-closest-entry
   [(_ inclusive? lh make-result)
    `(let* ([lo::ScmDictEntry* NULL]
            [hi::ScmDictEntry* NULL]
            [eq::ScmDictEntry*
             (Scm_TreeCoreClosestEntries (SCM_TREE_MAP_CORE tm)
                                         (cast intptr_t key)
                                         (& lo) (& hi))])
       (cond
        ,@(if inclusive?
            `([(!= eq NULL)  (,make-result (SCM_DICT_KEY eq) (SCM_DICT_VALUE eq))])
            '())
        [(!= ,lh NULL) (,make-result (SCM_DICT_KEY ,lh) (SCM_DICT_VALUE ,lh))]
        [else          (,make-result key-fb val-fb)]))])

 (define-cise-stmt tree-map-closest-key-result [(_ k v) `(result ,k)])
 (define-cise-stmt tree-map-closest-val-result [(_ k v) `(result ,v)])
 (define-cise-stmt tree-map-closest-kv-result  [(_ k v) `(result ,k ,v)])
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



