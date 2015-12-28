;;;
;;; data.sparse - sparse data structures
;;;
;;;   Copyright (c) 2007-2015  Shiro Kawai  <shiro@acm.org>
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


(define-module data.sparse
  (use gauche.collection)
  (use gauche.dictionary)
  (export <sparse-table> make-sparse-table sparse-table-num-entries
          sparse-table-ref sparse-table-set! sparse-table-exists?
          sparse-table-clear! sparse-table-delete! sparse-table-copy
          sparse-table-update! sparse-table-push! sparse-table-pop!
          sparse-table-fold sparse-table-map sparse-table-for-each
          sparse-table-keys sparse-table-values sparse-table-comparator
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
          sparse-vector-keys sparse-vector-values
          %sparse-vector-dump

          <sparse-matrix-base> <sparse-matrix> <sparse-s8matrix>
          <sparse-u8matrix> <sparse-s16matrix> <sparse-u16matrix>
          <sparse-s32matrix> <sparse-u32matrix> <sparse-s64matrix>
          <sparse-u64matrix> <sparse-f16matrix> <sparse-f32matrix>
          <sparse-f64matrix>
          make-sparse-matrix sparse-matrix-num-entries
          sparse-matrix-ref sparse-matrix-set! sparse-matrix-exists?
          sparse-matrix-clear! sparse-matrix-delete! sparse-matrix-copy
          sparse-matrix-update! sparse-matrix-inc!
          sparse-matrix-push! sparse-matrix-pop!
          sparse-matrix-fold sparse-matrix-map sparse-matrix-for-each
          sparse-matrix-keys sparse-matrix-values
          )
  )
(select-module data.sparse)

(inline-stub
 (declcode "#include \"ctrie.h\""
           "#include \"spvec.h\""
           "#include \"sptab.h\""
           "#include <gauche/bits_inline.h>")
 )

(define-macro (define-stuff type class iter ref set)
  (let ([x-fold     (string->symbol #"~|type|-fold")]
        [x-map      (string->symbol #"~|type|-map")]
        [x-for-each (string->symbol #"~|type|-for-each")]
        [x-keys     (string->symbol #"~|type|-keys")]
        [x-values   (string->symbol #"~|type|-values")]
        [x-update!  (string->symbol #"~|type|-update!")]
        [x-push!    (string->symbol #"~|type|-push!")]
        [x-pop!     (string->symbol #"~|type|-pop!")]
        [x-pop!-aux (string->symbol #"%~|type|-pop!-aux")])
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
       ;; Treatment of defauleValue differ between sptab and spvec atm,
       ;; so we define push! separately.
       ;; (define (,x-push! sv k val)
       ;;   (,set sv k (cons val (,ref sv k '()))))
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

       ;; basic generic operations
       (define-method ref ((s ,class) key . maybe-fallback)
         (apply ,ref s key maybe-fallback))
       (define-method (setter ref) ((s ,class) key v)
         (,set s key v))
       (define-method call-with-iterator ((s ,class) proc)
         (let ([iter (,iter s)]
               [sentinel (list #f)])
           (define (next) (receive (k v) (iter sentinel) (cons k v)))
           (define cache (next))
           (proc (^[] (eq? (car cache) sentinel))
                 (^[] (rlet1 v cache (set! cache (next)))))))
       )))

;;===============================================================
;; Sparse hashtables
;;

(inline-stub
 (initcode "Scm_Init_sptab(Scm_CurrentModule());")

 (define-type <sparse-table> "SparseTable*" "sparse table"
   "SPARSE_TABLE_P" "SPARSE_TABLE")

 (define-cproc %make-sparse-table (type cmpr::<comparator>)
   (let* ([t::ScmHashType SCM_HASH_EQ])
     (cond
      [(SCM_EQ type 'eq?)      (set! t SCM_HASH_EQ)]
      [(SCM_EQ type 'eqv?)     (set! t SCM_HASH_EQV)]
      [(SCM_EQ type 'equal?)   (set! t SCM_HASH_EQUAL)]
      [(SCM_EQ type 'string=?) (set! t SCM_HASH_STRING)]
      [else                    (set! t SCM_HASH_GENERAL)])
     (return (MakeSparseTable t cmpr 0))))

 (define-cproc sparse-table-comparator (st::<sparse-table>)
   (return (SCM_OBJ (-> st comparator))))
 
 (define-cproc sparse-table-num-entries (st::<sparse-table>) ::<ulong>
   (return (-> st numEntries)))

 (define-cproc sparse-table-set! (st::<sparse-table> key value)
   (return (SparseTableSet st key value 0)))

 (define-cproc sparse-table-ref (st::<sparse-table> key :optional fallback)
   (setter sparse-table-set!)
   (let* ([r (SparseTableRef st key fallback)])
     (when (SCM_UNBOUNDP r)
       (Scm_Error "%S doesn't have an entry for key %S" (SCM_OBJ st) key))
     (return r)))

 (define-cproc sparse-table-exists? (st::<sparse-table> key) ::<boolean>
   (let* ([r (SparseTableRef st key SCM_UNBOUND)])
     (return (not (SCM_UNBOUNDP r)))))

 (define-cproc sparse-table-delete! (st::<sparse-table> key) ::<boolean>
   (return (not (SCM_UNBOUNDP (SparseTableDelete st key)))))

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
     (return (Scm_MakeSubr sparse-table-iter iter 1 0 '"sparse-table-iterator"))))

 (define-cproc %sparse-table-dump (st::<sparse-table>) ::<void>
   SparseTableDump)

 (define-cproc %sparse-table-check (st::<sparse-table>) ::<void>
   SparseTableCheck)
 )

;; This is a bit complicated, for we want to recognize common cases
;; (eq?, eqv?, equal? and string=?) to allow efficient shortcut.
(define *shortcut-comparators*
  `((eq? . ,eq-comparator)
    (eqv? . ,eqv-comparator)
    (equal? . ,equal-comparator)
    (string=? . ,string-comparator)))

(define (make-sparse-table comparator)
  (define (bad)
    (error "make-sparse-table needs a comparator or one of the symbols eq?, \
            eqv?, equal? or string=?, as an argument, but got:" comparator))
  (receive (type cmpr)
      (cond [(symbol? comparator)
             (if-let1 cmpr (assq-ref *shortcut-comparators* comparator)
               (values comparator cmpr)
               (bad))]
            [(comparator? comparator)
             (if-let1 type (rassq-ref *shortcut-comparators* comparator)
               (values type comparator)
               (values #f comparator))]
            [else (bad)])
    (%make-sparse-table type cmpr)))

(define (sparse-table-push! sptab key val)
  ;; Can be optimized
  (sparse-table-set! sptab key (cons val (sparse-table-ref sptab key '()))))

(define-stuff sparse-table <sparse-table> %sparse-table-iter
  sparse-table-ref sparse-table-set!)

;;===============================================================
;; Sparse vectors
;;

(define (make-sparse-vector :optional (type #f)
                            :key (flags 0) default)
  (%make-sparse-vector type default flags))

(inline-stub
 (initcode "Scm_Init_spvec(Scm_CurrentModule());")

 (define-type <sparse-vector> "SparseVector*" "sparse vector"
   "SPARSE_VECTOR_BASE_P" "SPARSE_VECTOR")

 (define-cproc %make-sparse-vector (type default-value flags::<ulong>)
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
     (return (MakeSparseVector klass default-value 0))))

 (define-cproc sparse-vector-max-index-bits () ::<int>
   (return SPARSE_VECTOR_MAX_INDEX_BITS))

 (define-cproc sparse-vector-num-entries (sv::<sparse-vector>) ::<ulong>
   (return (-> sv numEntries)))

 (define-cproc sparse-vector-set!
   (sv::<sparse-vector> index::<ulong> value) ::<void>
   SparseVectorSet)

 (define-cproc sparse-vector-ref
   (sv::<sparse-vector> index::<integer> :optional fallback)
   (setter sparse-vector-set!)
   (let* ([oor::int FALSE]
          [i::u_long (Scm_GetIntegerUClamp index SCM_CLAMP_NONE (& oor))]
          [r SCM_UNBOUND])
     (when (not oor)
       (set! r (SparseVectorRef sv i fallback)))
     (if (SCM_UNBOUNDP r)
       (if (SCM_UNDEFINEDP (-> sv defaultValue))
         (Scm_Error "%S doesn't have an entry at index %S" (SCM_OBJ sv) index)
         (return (-> sv defaultValue)))
       (return r))))

 (define-cproc sparse-vector-exists?
   (sv::<sparse-vector> index::<integer>) ::<boolean>
   (let* ([oor::int FALSE]
          [i::u_long (Scm_GetIntegerUClamp index SCM_CLAMP_NONE (& oor))]
          [r SCM_UNBOUND])
     (when (not oor)
       (set! r (SparseVectorRef sv i SCM_UNBOUND)))
     (return (not (SCM_UNBOUNDP r)))))

 (define-cproc sparse-vector-default-value (sv::<sparse-vector>)
   (return (-> sv defaultValue)))

 (define-cproc sparse-vector-delete! (sv::<sparse-vector> index::<ulong>)
   ::<boolean>
   (return (not (SCM_UNBOUNDP (SparseVectorDelete sv index)))))

 (define-cproc sparse-vector-clear! (sv::<sparse-vector>) ::<void>
   SparseVectorClear)

 (define-cproc sparse-vector-copy (sv::<sparse-vector>) SparseVectorCopy)

 (define-cproc sparse-vector-inc! (sv::<sparse-vector>
                                   index::<ulong>
                                   delta::<number>
                                   :optional fallback)
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
     (return
      (Scm_MakeSubr sparse-vector-iter iter 1 0 '"sparse-vector-iterator"))))

 (define-cproc %sparse-vector-dump (sv::<sparse-vector>) ::<void>
   SparseVectorDump)
 )

(define (sparse-vector-push! spvec key val)
  ;; Can be optimized
  (if (undefined? (sparse-vector-default-value spvec))
    (sparse-vector-set! spvec key (cons val (sparse-vector-ref spvec key '())))
    (sparse-vector-set! spvec key (cons val (sparse-vector-ref spvec key)))))

(define-stuff sparse-vector <sparse-vector-base> %sparse-vector-iter
  sparse-vector-ref sparse-vector-set!)

;; sparse vector comparator is just a singleton.
(define *sparse-vector-comparator* 
  (make-comparator (^x (and (exact? x) (integer? x) (>= x 0)))
                   eqv? compare eqv-hash
                   'sparse-vector-comparator))

(define-method dict-comparator ((spvec <sparse-vector-base>))
  *sparse-vector-comparator*)

;;===============================================================
;; Sparse matrix
;;

;; 2-dimensional matrix; it is just a disguise of sparse vector,
;; with a twist of indexing.

;; NB: We're planning to make sparse vector index range unlimited.
;; Then we don't need to worry about the range of each index.

(inline-stub
  (define-type <sparse-matrix> "SparseVector*" "sparse matrix"
    "SPARSE_MATRIX_BASE_P" "SPARSE_MATRIX")

  "#define X_OOR  1"
  "#define Y_OOR  2"

  (define-cise-stmt oor-check
    [(_ oor which oorval)
     `(if (== ,oor NULL)
        (Scm_Error ,#"~|which| index is out of range: %S" ,which)
        (begin (set! (* ,oor) ,oorval) (return 0)))])
  
  ;; Calculate linear index from 2d indexes (x,y) by interleaving the bits.
  ;; When the input is out of range, if OOR is provided it is set;
  ;; otherwise an error is signaled.
  ;; We initially thought of taking 5 bits at a time to match up the node
  ;; size; however it made code complicated to deal with the last
  ;; 2 or 4 bits in 32bit/64bit integer.  Also, if key is distributed
  ;; enough, we wonder if there's much sense aligning to the node boundary.
  ;; So we adopt 4 bits interleaving instead.
  "#define INTERLEAVE_SHIFT 4"
  "#define INTERLEAVE_MASK ((1UL<<INTERLEAVE_SHIFT)-1)"
  
  (define-cfn index-combine-2d (x y oor::int*)
    ::u_long :static
    (unless (SCM_INTEGERP x)
      (Scm_Error "Exact integer required for x, but got %S" x))
    (unless (SCM_INTEGERP y)
      (Scm_Error "Exact integer required for y, but got %S" y))
    (let* ([oorx::int FALSE] [oory::int FALSE]
           [ix::u_long (Scm_GetIntegerUClamp x SCM_CLAMP_NONE (& oorx))]
           [iy::u_long (Scm_GetIntegerUClamp y SCM_CLAMP_NONE (& oory))]
           [bx::int  (Scm__HighestBitNumber ix)]
           [by::int  (Scm__HighestBitNumber iy)])
      (when oorx (oor-check oor x X_OOR))
      (when oory (oor-check oor y Y_OOR))
      (when (>= ix (<< (cast (u_long) 1) (/ SPARSE_VECTOR_MAX_INDEX_BITS 2)))
        (oor-check oor x X_OOR))
      (when (>= iy (<< (cast (u_long) 1) (/ SPARSE_VECTOR_MAX_INDEX_BITS 2)))
        (oor-check oor y Y_OOR))
      (let* ([bmax::int (?: (> bx by) bx by)]
             [shift::int 0]
             [ind::u_long 0])
        (while (< shift (+ bmax 1))
          (set! ind (logior ind (<< (logand ix (<< INTERLEAVE_MASK shift))
                                    shift)))
          (set! ind (logior ind (<< (logand iy (<< INTERLEAVE_MASK shift))
                                    (+ shift INTERLEAVE_SHIFT))))
          (set! shift (+ shift INTERLEAVE_SHIFT)))
        (when oor (set! (* oor) 0))
        (return ind))))

  (define-cfn index-split-2d (index::ScmObj px::(u_long*) py::(u_long*))
    ::void :static
    (let* ([x::u_long 0] [y::u_long 0] [shift::int 0]
           [i::u_long (Scm_GetIntegerU index)])
      (while i
        (set! x (logior x (<< (logand i INTERLEAVE_MASK) shift)))
        (set! i (>> i INTERLEAVE_SHIFT))
        (set! y (logior y (<< (logand i INTERLEAVE_MASK) shift)))
        (set! i (>> i INTERLEAVE_SHIFT))
        (set! shift (+ shift INTERLEAVE_SHIFT)))
      (set! (* px) x)
      (set! (* py) y)))
  )

(define (make-sparse-matrix :optional (type #f)
                            :key (flags 0) default)
  (let1 class
      (case type
        [(#f)  <sparse-matrix>]
        [(s8)  <sparse-s8matrix>]
        [(u8)  <sparse-u8matrix>]
        [(s16) <sparse-s16matrix>]
        [(u16) <sparse-u16matrix>]
        [(s32) <sparse-s32matrix>]
        [(u32) <sparse-u32matrix>]
        [(s64) <sparse-s64matrix>]
        [(u64) <sparse-u64matrix>]
        [(f16) <sparse-f16matrix>]
        [(f32) <sparse-f32matrix>]
        [(f64) <sparse-f64matrix>]
        [else (if (and (subtype? type <sparse-matrix-base>)
                       (not (eq? type <sparse-matrix-base>)))
                type
                (error "type argument must be a subclass of \
                        <sparse-matrix-base>, #f, or one of \
                        s8, u8, s16, u16, s32, u32, s64, u64, \
                        f16, f32 or f64, but got:" type))])
    (%make-sparse-vector class default flags)))

(define-cproc sparse-matrix-num-entries (sv::<sparse-matrix>) ::<ulong>
  (return (-> sv numEntries)))

(define-cproc sparse-matrix-set! (sv::<sparse-matrix> x y val) ::<void>
  (SparseVectorSet sv (index-combine-2d x y NULL) val))

(define-cproc sparse-matrix-ref
  (sv::<sparse-matrix> x y :optional fallback)
  (setter sparse-matrix-set!)
  (let* ([oor::int 0]
         [i::u_long (index-combine-2d x y (& oor))]
         [r SCM_UNBOUND])
    (when (not oor)
      (set! r (SparseVectorRef sv i fallback)))
    (if (SCM_UNBOUNDP r)
      (if (SCM_UNDEFINEDP (-> sv defaultValue))
        (Scm_Error "%S doesn't have an entry at index (%S %S)"
                   (SCM_OBJ sv) x y)
        (return (-> sv defaultValue)))
      (return r))))

(define-cproc sparse-matrix-exists?
  (sv::<sparse-matrix> x y) ::<boolean>
  (let* ([oor::int 0]
         [i::u_long (index-combine-2d x y (& oor))]
         [r SCM_UNBOUND])
    (when (not oor)
      (set! r (SparseVectorRef sv i SCM_UNBOUND)))
    (return (not (SCM_UNBOUNDP r)))))

(define-cproc sparse-matrix-default-value (sv::<sparse-matrix>)
  (return (-> sv defaultValue)))

(define-cproc sparse-matrix-delete! (sv::<sparse-matrix> x y) ::<boolean>
  (return (not (SCM_UNBOUNDP (SparseVectorDelete sv (index-combine-2d x y NULL))))))

(define-cproc sparse-matrix-clear! (sv::<sparse-matrix>) ::<void>
  SparseVectorClear)

(define-cproc sparse-matrix-copy (sv::<sparse-matrix>) SparseVectorCopy)

(define-cproc sparse-matrix-inc! (sv::<sparse-matrix>
                                  x y
                                  delta::<number>
                                  :optional fallback)
  (return (SparseVectorInc sv (index-combine-2d x y NULL) delta fallback)))

(inline-stub
 (define-cfn sparse-matrix-iter (args::ScmObj* nargs::int data::void*) :static
   (let* ([iter::SparseVectorIter* (cast SparseVectorIter* data)]
          [r (SparseVectorIterNext iter)]
          [eofval (aref args 0)])
     (if (SCM_FALSEP r)
       (return (values eofval eofval eofval))
       (let* ([x::u_long 0] [y::u_long 0])
         (index-split-2d (SCM_CAR r) (& x) (& y))
         (return (values (Scm_MakeIntegerU x)
                         (Scm_MakeIntegerU y)
                         (SCM_CDR r)))))))
 )

(define-cproc %sparse-matrix-iter (sv::<sparse-matrix>)
  (let* ([iter::SparseVectorIter* (SCM_NEW SparseVectorIter)])
    (SparseVectorIterInit iter sv)
    (return
     (Scm_MakeSubr sparse-matrix-iter iter 1 0 '"sparse-matrix-iterator"))))
       
(define-cproc sparse-matrix-update! (sv::<sparse-matrix> x y proc
                                                         :optional fallback)
  ;; NB: We use j to pass the index to continuation proc, for it will be
  ;; cast to/fro pointer, and sizeof(u_long) may be smaller than the size
  ;; of the pointer.
  (let* ([i::u_long (index-combine-2d x y NULL)]
         [j::uintptr_t i]
         [v (SparseVectorRef sv i fallback)])
    (when (SCM_UNBOUNDP v)
      (when (SCM_UNDEFINEDP (-> sv defaultValue))
        (Scm_Error "%S doesn't hav an entry at (%S %S)" sv x y))
      (set! v (-> sv defaultValue)))
    (let1/cps r (Scm_VMApply1 proc v)
      [sv::SparseVector* j::uintptr_t]
      (SparseVectorSet sv (cast u_long j) r)
      (return SCM_UNDEFINED))))

(define-cproc sparse-matrix-push! (sv::<sparse-matrix> x y val) ::<void>
  (let* ([i::u_long (index-combine-2d x y NULL)]
         [v (SparseVectorRef sv i SCM_UNBOUND)])
    (when (SCM_UNBOUNDP v)
      (if (SCM_UNDEFINEDP (-> sv defaultValue))
        (set! v SCM_NIL)
        (set! v (-> sv defaultValue))))
    (SparseVectorSet sv i (Scm_Cons val v))))

(define-cproc sparse-matrix-pop! (sv::<sparse-matrix> x y)
  (let* ([i::u_long (index-combine-2d x y NULL)]
         [v (SparseVectorRef sv i SCM_UNBOUND)])
    (when (SCM_UNBOUNDP v)
      (if (SCM_UNDEFINEDP (-> sv defaultValue))
        (set! v SCM_NIL)
        (set! v (-> sv defaultValue))))
    (unless (SCM_PAIRP v)
      (Scm_Error "%S's value for key (%S %S) is not a pair: %S" sv x y v))
    (SparseVectorSet sv i (SCM_CDR v))
    (return (SCM_CAR v))))

(define (sparse-matrix-fold sv proc seed)
  (let ([iter (%sparse-matrix-iter sv)]
        [end (list #f)])
    (let loop ([seed seed])
      (receive (x y val) (iter end)
        (if (eq? val end)
          seed
          (loop (proc x y val seed)))))))

(define (sparse-matrix-map sv proc)
  (sparse-matrix-fold sv (^[x y v s] (cons (proc x y v) s)) '()))

(define (sparse-matrix-for-each sv proc)
  (sparse-matrix-fold sv (^[x y v _] (proc x y v)) #f))

(define (sparse-matrix-keys sv)
  (sparse-matrix-fold sv (^[x y _ s] (cons (list x y) s)) '()))

(define (sparse-matrix-values sv)
  (sparse-matrix-fold sv (^[x y v s] (cons v s)) '()))
  
;;===============================================================
;; protocols
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
  :update!   sparse-table-update!
  :comparator sparse-table-comparator)

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

