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
  (export-all)
  )
(select-module util.sparse)

(inline-stub
 "#include \"ctrie.h\""
 "#include \"spvec.h\"")

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


(provide "util/sparse")
