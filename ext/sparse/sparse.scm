;;;
;;; util.sparse - sparse data structures
;;;  
;;;   Copyright (c) 2007-2009  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: fcntl.scm,v 1.6 2007/03/02 07:39:05 shirok Exp $
;;;


(define-module util.sparse
  (export <spvector> make-spvector spvector-num-elements
          spvector-ref spvector-set! %spvector-dump
          <sptable> make-sptable sptable-num-elements
          sptable-ref sptable-set! %sptable-dump)
  )
(select-module util.sparse)

(inline-stub
 "#include \"ctrie.h\""
 "#include \"spvec.h\""
 "#include \"sptab.h\""
 )

;; Sparse vectors
(inline-stub
 (initcode "Scm_Init_spvec(mod);")

 (define-type <spvector> "SparseVector*" "sparse vector"
   "SPARSE_VECTOR_P" "SPARSE_VECTOR")

 (define-cproc make-spvector ()
   (result (MakeSparseVector 0)))

 (define-cproc spvector-num-elements (sv::<spvector>) ::<ulong>
   (result (-> sv numElements)))
 
 (define-cproc spvector-ref (sv::<spvector> index::<ulong> :optional fallback)
   SparseVectorRef)

 (define-cproc spvector-set! (sv::<spvector> index::<ulong> value) ::<void>
   SparseVectorSet)

 (define-cproc %spvector-dump (sv::<spvector>) ::<void>
   SparseVectorDump)
 )

;; Sparse hashtables
(inline-stub
 (initcode "Scm_Init_sptab(mod);")

 (define-type <sptable> "SparseTable*" "sparse table"
   "SPARSE_TABLE_P" "SPARSE_TABLE")

 (define-cproc make-sptable (type)
   (let* ([t::ScmHashType SCM_HASH_EQ])
     (cond
      [(SCM_EQ type 'eq?)      (set! t SCM_HASH_EQ)]
      [(SCM_EQ type 'eqv?)     (set! t SCM_HASH_EQV)]
      [(SCM_EQ type 'equal?)   (set! t SCM_HASH_EQUAL)]
      [(SCM_EQ type 'string=?) (set! t SCM_HASH_STRING)]
      [else (Scm_Error "unsupported sptable hash type: %S" type)])
     (result (MakeSparseTable t 0))))

 (define-cproc sptable-num-elements (st::<sptable>) ::<ulong>
   (result (-> st numElements)))
 
 (define-cproc sptable-ref (st::<sptable> key :optional fallback)
   (let* ([r (SparseTableRef st key fallback)])
     (when (SCM_UNBOUNDP r)
       (Scm_Error "%S doesn't have an entry for key %S" (SCM_OBJ st) key))
     (result r)))

 (define-cproc sptable-set! (st::<sptable> key value)
   (result (SparseTableSet st key value 0)))

 (define-cproc %sptable-dump (st::<sptable>) ::<void>
   SparseTableDump)
 )

(provide "util/sparse")
