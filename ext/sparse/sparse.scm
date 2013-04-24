;;;
;;; util.sparse - sparse data structures
;;;
;;;   Copyright (c) 2007-2013  Shiro Kawai  <shiro@acm.org>
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


(define-module util.sparse
  (use gauche.dictionary)
  (export <sparse-table> make-sparse-table sparse-table-num-entries
          sparse-table-ref sparse-table-set! sparse-table-exists?
          sparse-table-clear! sparse-table-delete! sparse-table-copy
          sparse-table-update! sparse-table-push! sparse-table-pop!
          sparse-table-fold sparse-table-map sparse-table-for-each
          sparse-table-keys sparse-table-values
          %sparse-table-dump %sparse-table-check

          <sparse-vector-base> <sparse-vector> <sparse-s8vector>
          <sparse-u8vector> <sparse-s16vector> <sparse-u16vector>
          <sparse-s32vector> <sparse-u32vector> <sparse-s64vector>
          <sparse-u64vector> <sparse-f16vector> <sparse-f32vector>
          <sparse-f64vector>
          sparse-vector-max-index-bits
          make-sparse-vector sparse-vector-num-entries
          sparse-vector-ref sparse-vector-set! sparse-vector-exists?
          sparse-vector-clear! sparse-vector-delete! sparse-vector-copy
          sparse-vector-update! sparse-vector-inc!
          sparse-vector-push! sparse-vector-pop!
          sparse-vector-fold sparse-vector-map sparse-vector-for-each
          sparse-vector-keys sparse-vector-values %sparse-vector-dump
          )
  )
(select-module util.sparse)

(inline-stub
 "#include \"ctrie.h\""
 "#include \"spvec.h\""
 "#include \"sptab.h\""
 )

(define-macro (define-stuff type iter ref set)
  (let ([x-fold     (string->symbol #`",|type|-fold")]
        [x-map      (string->symbol #`",|type|-map")]
        [x-for-each (string->symbol #`",|type|-for-each")]
        [x-keys     (string->symbol #`",|type|-keys")]
        [x-values   (string->symbol #`",|type|-values")]
        [x-update!  (string->symbol #`",|type|-update!")]
        [x-push!    (string->symbol #`",|type|-push!")]
        [x-pop!     (string->symbol #`",|type|-pop!")]
        [x-pop!-aux (string->symbol #`"%,|type|-pop!-aux")])
    `(begin
       (define (,x-fold st proc seed)
         (let ([iter (,iter st)]
               [end  (list #f)])
           (let loop ((seed seed))
             (receive (key val) (iter end)
               (if (eq? key end)
                 seed
                 (loop (proc key val seed)))))))
       (define (,x-map st proc)
         (,x-fold st (^[k v s] (cons (proc k v) s)) '()))
       (define (,x-for-each st proc)
         (,x-fold st (^[k v _] (proc k v)) #f))
       (define (,x-keys st)
         (,x-fold st (^[k v s] (cons k s)) '()))
       (define (,x-values st)
         (,x-fold st (^[k v s] (cons v s)) '()))

       ;; TODO: rewrite these more efficiently
       (define (,x-update! sv k proc . fallback)
         (rlet1 tmp (proc (apply ,ref sv k fallback))
           (,set sv k tmp)))
       (define (,x-push! sv k val)
         (,set sv k (cons val (,ref sv k '()))))
       (define (,x-pop! sv k . fallback)
         (if (null? fallback)
           (,x-pop!-aux sv k (,ref sv k))
           ;; A trick to use the argument list as a unique object to see if
           ;; sv doesn't have an entry
           (let1 p (,ref sv k fallback)
             (if (eq? p fallback) (car p) (,x-pop!-aux sv k p)))))
       (define (,x-pop!-aux sv k p)
         (unless (pair? p)
           (errorf "~s's value for key ~s is not a pair: ~s" sv k p))
         (,set sv k (cdr p))
         (car p))
       )))

;;===============================================================
;; Sparse hashtables
;;

(inline-stub
 (initcode "Scm_Init_sptab(Scm_CurrentModule());")

 (define-type <sparse-table> "SparseTable*" "sparse table"
   "SPARSE_TABLE_P" "SPARSE_TABLE")

 (define-cproc make-sparse-table (type)
   (let* ([t::ScmHashType SCM_HASH_EQ])
     (cond
      [(SCM_EQ type 'eq?)      (set! t SCM_HASH_EQ)]
      [(SCM_EQ type 'eqv?)     (set! t SCM_HASH_EQV)]
      [(SCM_EQ type 'equal?)   (set! t SCM_HASH_EQUAL)]
      [(SCM_EQ type 'string=?) (set! t SCM_HASH_STRING)]
      [else (Scm_Error "unsupported sparse-table hash type: %S" type)])
     (result (MakeSparseTable t 0))))

 (define-cproc sparse-table-num-entries (st::<sparse-table>) ::<ulong>
   (result (-> st numEntries)))

 (define-cproc sparse-table-set! (st::<sparse-table> key value)
   (result (SparseTableSet st key value 0)))

 (define-cproc sparse-table-ref (st::<sparse-table> key :optional fallback)
   (setter sparse-table-set!)
   (let* ([r (SparseTableRef st key fallback)])
     (when (SCM_UNBOUNDP r)
       (Scm_Error "%S doesn't have an entry for key %S" (SCM_OBJ st) key))
     (result r)))

 (define-cproc sparse-table-exists? (st::<sparse-table> key) ::<boolean>
   (let* ([r (SparseTableRef st key SCM_UNBOUND)])
     (result (not (SCM_UNBOUNDP r)))))

 (define-cproc sparse-table-delete! (st::<sparse-table> key) ::<boolean>
   (result (not (SCM_UNBOUNDP (SparseTableDelete st key)))))

 (define-cproc sparse-table-clear! (st::<sparse-table>) ::<void>
   SparseTableClear)

 (define-cproc sparse-table-copy (sv::<sparse-table>) SparseTableCopy)

 (define-cfn sparse-table-iter (args::ScmObj* nargs::int data::void*) :static
   (let* ([iter::SparseTableIter* (cast SparseTableIter* data)]
          [r (SparseTableIterNext iter)]
          [eofval (aref args 0)])
     (if (SCM_FALSEP r)
       (return (values eofval eofval))
       (return (values (SCM_CAR r) (SCM_CDR r))))))

 (define-cproc %sparse-table-iter (st::<sparse-table>)
   (let* ([iter::SparseTableIter* (SCM_NEW SparseTableIter)])
     (SparseTableIterInit iter st)
     (result (Scm_MakeSubr sparse-table-iter iter 1 0 '"sparse-table-iterator"))))

 (define-cproc %sparse-table-dump (st::<sparse-table>) ::<void>
   SparseTableDump)

 (define-cproc %sparse-table-check (st::<sparse-table>) ::<void>
   SparseTableCheck)
 )

(define-stuff sparse-table %sparse-table-iter
  sparse-table-ref sparse-table-set!)

;;===============================================================
;; Sparse vectors
;;
(inline-stub
 (initcode "Scm_Init_spvec(Scm_CurrentModule());")

 (define-type <sparse-vector> "SparseVector*" "sparse vector"
   "SPARSE_VECTOR_BASE_P" "SPARSE_VECTOR")

 (define-cproc make-sparse-vector (:optional (type #f) (flags::<ulong> 0))
   (let* ([klass::ScmClass* NULL])
     (cond [(SCM_CLASSP type)  (set! klass (SCM_CLASS type))]
           [(SCM_FALSEP type)  (set! klass SCM_CLASS_SPARSE_VECTOR)]
           [(SCM_EQ type 's8)  (set! klass SCM_CLASS_SPARSE_S8VECTOR)]
           [(SCM_EQ type 'u8)  (set! klass SCM_CLASS_SPARSE_U8VECTOR)]
           [(SCM_EQ type 's16) (set! klass SCM_CLASS_SPARSE_S16VECTOR)]
           [(SCM_EQ type 'u16) (set! klass SCM_CLASS_SPARSE_U16VECTOR)]
           [(SCM_EQ type 's32) (set! klass SCM_CLASS_SPARSE_S32VECTOR)]
           [(SCM_EQ type 'u32) (set! klass SCM_CLASS_SPARSE_U32VECTOR)]
           [(SCM_EQ type 's64) (set! klass SCM_CLASS_SPARSE_S64VECTOR)]
           [(SCM_EQ type 'u64) (set! klass SCM_CLASS_SPARSE_U64VECTOR)]
           [(SCM_EQ type 'f16) (set! klass SCM_CLASS_SPARSE_F16VECTOR)]
           [(SCM_EQ type 'f32) (set! klass SCM_CLASS_SPARSE_F32VECTOR)]
           [(SCM_EQ type 'f64) (set! klass SCM_CLASS_SPARSE_F64VECTOR)]
           [else (Scm_TypeError "type"
                                "subclass of <sparse-vector-base>, #f, or \
                                 one of symbols s8, u8, s16, u16, s32, u32, \
                                 s64, u64, f16, f32, f64"
                                type)])
     (result (MakeSparseVector klass flags))))

 (define-cproc sparse-vector-max-index-bits () ::<int>
   (result SPARSE_VECTOR_MAX_INDEX_BITS))

 (define-cproc sparse-vector-num-entries (sv::<sparse-vector>) ::<ulong>
   (result (-> sv numEntries)))

 (define-cproc sparse-vector-set!
   (sv::<sparse-vector> index::<ulong> value) ::<void>
   SparseVectorSet)

 (define-cproc sparse-vector-ref
   (sv::<sparse-vector> index::<ulong> :optional fallback)
   (setter sparse-vector-set!)
   (let* ([r (SparseVectorRef sv index fallback)])
     (when (SCM_UNBOUNDP r)
       (Scm_Error "%S doesn't have an entry at index %lu" (SCM_OBJ sv) index))
     (result r)))

 (define-cproc sparse-vector-exists?
   (sv::<sparse-vector> index::<ulong>) ::<boolean>
   (let* ([r (SparseVectorRef sv index SCM_UNBOUND)])
     (result (not (SCM_UNBOUNDP r)))))

 (define-cproc sparse-vector-delete! (sv::<sparse-vector> index::<ulong>)
   ::<boolean>
   (result (not (SCM_UNBOUNDP (SparseVectorDelete sv index)))))

 (define-cproc sparse-vector-clear! (sv::<sparse-vector>) ::<void>
   SparseVectorClear)

 (define-cproc sparse-vector-copy (sv::<sparse-vector>) SparseVectorCopy)

 (define-cproc sparse-vector-inc! (sv::<sparse-vector>
                                   index::<ulong>
                                   delta::<number>
                                   :optional (fallback::<number> 0))
   SparseVectorInc)

 (define-cfn sparse-vector-iter (args::ScmObj* nargs::int data::void*) :static
   (let* ([iter::SparseVectorIter* (cast SparseVectorIter* data)]
          [r (SparseVectorIterNext iter)]
          [eofval (aref args 0)])
     (if (SCM_FALSEP r)
       (return (values eofval eofval))
       (return (values (SCM_CAR r) (SCM_CDR r))))))

 (define-cproc %sparse-vector-iter (sv::<sparse-vector>)
   (let* ([iter::SparseVectorIter* (SCM_NEW SparseVectorIter)])
     (SparseVectorIterInit iter sv)
     (result
      (Scm_MakeSubr sparse-vector-iter iter 1 0 '"sparse-vector-iterator"))))

 (define-cproc %sparse-vector-dump (sv::<sparse-vector>) ::<void>
   SparseVectorDump)
 )

(define-stuff sparse-vector %sparse-vector-iter
  sparse-vector-ref sparse-vector-set!)

;;===============================================================
;; dictionary protocol
;;

(define-dict-interface <sparse-table>
  :get       sparse-table-ref
  :put!      sparse-table-set!
  :delete!   sparse-table-delete!
  :exists?   sparse-table-exists?
  :fold      sparse-table-fold
  :for-each  sparse-table-for-each
  :map       sparse-table-map
  :keys      sparse-table-keys
  :values    sparse-table-values
  :pop!      sparse-table-pop!
  :push!     sparse-table-push!
  :update!   sparse-table-update!)

(define-dict-interface <sparse-vector-base>
  :get       sparse-vector-ref
  :put!      sparse-vector-set!
  :delete!   sparse-vector-delete!
  :exists?   sparse-vector-exists?
  :fold      sparse-vector-fold
  :for-each  sparse-vector-for-each
  :map       sparse-vector-map
  :keys      sparse-vector-keys
  :values    sparse-vector-values
  :pop!      sparse-vector-pop!
  :push!     sparse-vector-push!
  :update!   sparse-vector-update!)
