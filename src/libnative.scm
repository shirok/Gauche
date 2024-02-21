;;;
;;; libnative.scm - Playing with native code
;;;
;;;   Copyright (c) 2021-2024  Shiro Kawai  <shiro@acm.org>
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

;; This is EXPERIMENTAL and UNDER DEVELOPMENT.

;; Nasty stuff.
;; These bindings are available only during the bootstrap process,
;; for we don't want ordinary code to call those internal routines.
;; For now, it is just a stub for more experiment.  Eventually ffi and jit
;; will use those routines.

(select-module gauche.bootstrap)

(inline-stub
 (.include "gauche/priv/mmapP.h"
           "gauche/priv/nativeP.h"
           "gauche/priv/codeP.h"
           "gauche/vminsn.h")

 ;; For FFI
 (define-cproc %%call-native (tstart::<fixnum>
                              tend::<fixnum>
                              code::<uvector>
                              start::<fixnum>
                              end::<fixnum>
                              entry::<fixnum>
                              patcher rettype)
   (return (Scm__VMCallNative (Scm_VM) tstart tend code start end entry
                              patcher rettype)))

 ;; For JIT
 (define-cproc %%jit-compile-procedure (proc compiler)
   (unless (SCM_CLOSUREP proc)
     (Scm_Error "Closure required, but got: %S" proc))
   (let* ([orig-code (SCM_CLOSURE_CODE proc)]
          [orig-env::ScmEnvFrame*  (SCM_CLOSURE_ENV proc)]
          [builder (Scm_MakeCompiledCodeBuilder (SCM_PROCEDURE_REQUIRED proc)
                                                (SCM_PROCEDURE_OPTIONAL proc)
                                                (SCM_PROCEDURE_INFO proc)
                                                proc
                                                SCM_FALSE)]
          [codevec&offsets (Scm_ApplyRec1 compiler orig-code)])
     (SCM_ASSERT (SCM_PAIRP codevec&offsets))
     (let* ([codevec (SCM_CAR codevec&offsets)]
            [offsets (SCM_CDR codevec&offsets)])
       (SCM_ASSERT (SCM_U8VECTORP codevec))
       (let* ([codepage (Scm__AllocateCodePage (SCM_U8VECTOR codevec))])
         (for-each (lambda (offset)
                     (Scm_CompiledCodeEmit (SCM_COMPILED_CODE builder)
                                           SCM_VM_XINSN 0 0
                                           (SCM_LIST2 codepage offset)
                                           SCM_FALSE))
                   offsets)))
     (Scm_CompiledCodeFinishBuilder (SCM_COMPILED_CODE builder) 0)
     (return (Scm_MakeClosure builder orig-env))))
 )

(select-module gauche.internal)

;; Stub for FFI
(include "native-supp.scm")

;; Stub for JIT
(define-cproc %unsafe-jit-enabled? () ::<boolean>
  (.if GAUCHE_ENABLE_UNSAFE_JIT_API
       (return TRUE)
       (return FALSE)))

(define %jit-compile
  (if (%unsafe-jit-enabled?)
    (let ([jcp (module-binding-ref 'gauche.bootstrap '%%jit-compile-procedure)])
      (^[proc compiler] (jcp proc compiler)))
    (^ _ (error "Operation not allowed"))))

;; Returns offset of VM field
(define-cproc vm-field-offset (field-name::<symbol>)
  (let* ([tab::(static ScmHashTable*) NULL])
    (when (== tab NULL)
      (let* ([t::ScmHashTable*
              (SCM_HASH_TABLE (Scm_MakeHashTableSimple SCM_HASH_EQ 16))])
        (Scm_HashTableSet t 'env  (Scm_MakeInteger (offsetof ScmVM env)) 0)
        (Scm_HashTableSet t 'denv (Scm_MakeInteger (offsetof ScmVM denv)) 0)
        (Scm_HashTableSet t 'cont (Scm_MakeInteger (offsetof ScmVM cont)) 0)
        (Scm_HashTableSet t 'argp (Scm_MakeInteger (offsetof ScmVM argp)) 0)
        (Scm_HashTableSet t 'val0 (Scm_MakeInteger (offsetof ScmVM val0)) 0)
        (Scm_HashTableSet t 'vals (Scm_MakeInteger (offsetof ScmVM vals)) 0)
        (Scm_HashTableSet t 'numVals (Scm_MakeInteger (offsetof ScmVM numVals)) 0)
        (Scm_HashTableSet t 'sp   (Scm_MakeInteger (offsetof ScmVM sp)) 0)
        (Scm_HashTableSet t 'stackEnd (Scm_MakeInteger (offsetof ScmVM stackEnd)) 0)
        (set! tab t)))
    (let* ([off (Scm_HashTableRef tab (SCM_OBJ field-name) SCM_FALSE)])
      (unless (SCM_INTP off)
        (Scm_Error "Unknown VM field: %S" field-name))
      (return off))))

;; TEMPORARY - Returns raw representation of ScmObj.
(define-cproc raw-value (obj) ::<integer>
  (.unless GAUCHE_ENABLE_UNSAFE_JIT_API
    (Scm_Error "Operation not allowed"))
  (return (Scm_IntptrToInteger (cast intptr_t obj))))
