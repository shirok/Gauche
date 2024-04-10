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
 (.include "gauche/priv/configP.h"
           "gauche/priv/mmapP.h"
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
          [builder::ScmCompiledCode*
           (SCM_COMPILED_CODE
            (Scm_MakeCompiledCodeBuilder (SCM_PROCEDURE_REQUIRED proc)
                                         (SCM_PROCEDURE_OPTIONAL proc)
                                         (SCM_PROCEDURE_INFO proc)
                                         proc
                                         SCM_FALSE))]
          [r (Scm_ApplyRec1 compiler orig-code)]
          [_ (SCM_ASSERT (SCM_PAIRP r))]
          [maxdepth (SCM_CAR r)]
          [_ (SCM_ASSERT (SCM_PAIRP (SCM_CDR r)))]
          [codevec (SCM_CADR r)]
          [offsets (SCM_CDDR r)])
     (SCM_ASSERT (SCM_INTP maxdepth))
     (SCM_ASSERT (SCM_U8VECTORP codevec))
     (let* ([codepage (Scm__AllocateCodePage (SCM_U8VECTOR codevec))])
       (for-each (lambda (offset)
                   (Scm_CompiledCodeEmit builder
                                         SCM_VM_XINSN 0 0
                                         (SCM_LIST2 codepage offset)
                                         SCM_FALSE))
                 offsets))
     (Scm_CompiledCodeEmit builder SCM_VM_RET 0 0 SCM_FALSE SCM_FALSE)
     (Scm_CompiledCodeFinishBuilder builder (SCM_INT_VALUE maxdepth))
     (return (Scm_MakeClosure (SCM_OBJ builder) orig-env))))
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

(inline-stub
 (define-cise-stmt with-static-table
   [(_ (var ((key intval) ...)) body ...)
    (let ([tmp (gensym)]
          [initsize (length key)])
      `(let* ([,var :: (static ScmHashTable*) NULL])
         (when (== ,var NULL)
           (let* ([,tmp :: ScmHashTable*
                   (SCM_HASH_TABLE (Scm_MakeHashTableSimple SCM_HASH_EQ ,initsize))])
             ,@(map (^[k v]
                      `(Scm_HashTableSet ,tmp ',k
                                         (Scm_IntptrToInteger (cast intptr_t ,v))
                                         0))
                    key intval)
             (set! ,var ,tmp)))
         ,@body))]))

;; Returns offset of VM field
(define-cproc vm-field-offset (field-name::<symbol>)
  (with-static-table
   (tab ((env      (offsetof ScmVM env))
         (denv     (offsetof ScmVM denv))
         (cont     (offsetof ScmVM cont))
         (argp     (offsetof ScmVM argp))
         (val0     (offsetof ScmVM val0))
         (vals     (offsetof ScmVM vals))
         (numVals  (offsetof ScmVM numVals))
         (sp       (offsetof ScmVM sp))
         (stackEnd (offsetof ScmVM stackEnd))))
   (let* ([off (Scm_HashTableRef tab (SCM_OBJ field-name) SCM_FALSE)])
     (unless (SCM_INTP off)
       (Scm_Error "Unknown VM field: %S" field-name))
     (return off))))

;; Returns address of the named function to be called from JIT code.
;; We want to allow only selected function to be callable, so we create
;; table manually.
(define-cproc vm-function-address (name::<symbol>)
  (with-static-table
   (tab ((Cons   Scm_Cons)
         (NumEq  Scm_NumEq)
         (NumLT  Scm_NumLT)
         (NumGT  Scm_NumGT)
         (Add    Scm_Add)
         (Sub    Scm_Sub)
         (Mul    Scm_Mul)
         (Div    Scm_Div)))
   (let* ([addr (Scm_HashTableRef tab (SCM_OBJ name) SCM_FALSE)])
     (unless (SCM_INTEGERP addr)
       (Scm_Error "Unknown function address: %S" name))
     (return addr))))

;; TEMPORARY - Returns raw representation of ScmObj.
(define-cproc raw-value (obj) ::<integer>
  (.unless GAUCHE_ENABLE_UNSAFE_JIT_API
    (Scm_Error "Operation not allowed"))
  (return (Scm_IntptrToInteger (cast intptr_t obj))))
