;;;
;;; libnative.scm - Playing with native code
;;;
;;;   Copyright (c) 2021-2025  Shiro Kawai  <shiro@acm.org>
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
           "gauche/priv/loadP.h"
           "gauche/priv/mmapP.h"
           "gauche/priv/nativeP.h"
           "gauche/priv/codeP.h"
           "gauche/priv/typeP.h"
           "gauche/vminsn.h")

 ;; This procedure is accessible via gauche.libtype#native-ptr-fill!,
 ;; with dynamic scope restriction to prevent misuse.
 (define-cproc %%native-ptr-fill! (target::<u8vector>
                                   tstart::<fixnum>
                                   size::<fixnum>
                                   type
                                   obj)
   ::<void>
   ;; Fill region [TSTART, TSTART+SIZE) of TARGET with the native pointer
   ;; representation of OBJ according to TYPE.  TYPE must be <c-string>,
   ;; a pointer/array/function native type, or <top>.
   ;; This is called via native-ptr-fill! in libtype.scm.
   (let* ([uvsize::ScmSmallInt (SCM_UVECTOR_SIZE target)]
          [ptrsize::ScmSmallInt (cast ScmSmallInt (sizeof (.type void*)))])
     (when (or (< tstart 0) (< size 0) (> (+ tstart size) uvsize))
       (Scm_Error "native-ptr-fill!: out of bounds \
                   (tstart=%ld, size=%ld, uvsize=%ld)"
                  tstart size uvsize))
     (when (< size ptrsize)
       (Scm_Error "native-ptr-fill!: size %ld too small for pointer \
                   (%ld bytes needed)"
                  size ptrsize))
     (let* ([ptr-val::intptr_t 0])
       (cond
        [(SCM_EQ type (SCM_OBJ SCM_CLASS_TOP))
         (set! ptr-val (cast intptr_t obj))]
        [(SCM_EQ type (Scm_NativeCStringType))
         (unless (SCM_STRINGP obj)
           (Scm_Error "%%native-ptr-fill!: string required for <c-string>, \
                       but got: %S" obj))
         (set! ptr-val (cast intptr_t (Scm_GetStringConst (SCM_STRING obj))))]
        [(or (SCM_C_POINTER_P type) (SCM_C_ARRAY_P type) (SCM_C_FUNCTION_P type))
         (unless (SCM_NATIVE_HANDLE_P obj)
           (Scm_Error "%%native-ptr-fill!: <native-handle> required for \
                       pointer type, but got: %S" obj))
         (set! ptr-val (cast intptr_t (Scm_NativeHandlePtr (SCM_NATIVE_HANDLE obj))))]
        [else
         (Scm_Error "%%native-ptr-fill!: unsupported type: %S" type)])
       (let* ([dst::char* (+ (cast char* (SCM_UVECTOR_ELEMENTS target)) tstart)])
         (memcpy dst (& ptr-val) (sizeof (.type intptr_t)))))))

 ;; For FFI
 (define-cproc %%call-native (tstart::<fixnum>
                              tend::<fixnum>
                              code::<uvector>
                              start::<fixnum>
                              end::<fixnum>
                              entry::<fixnum>
                              win-prolog-end::<fixnum>
                              win-frame-size::<fixnum>)
   (return (Scm__VMCallNative (Scm_VM) tstart tend code start end entry
                              win-prolog-end win-frame-size)))

 (define-cproc %%get-entry-address (name::<string>)
   (return (Scm__InternalGetEntryAddress name)))

 ;; FFI callback codepad primitives — bootstrap-only.  User code
 ;; reaches them through the gauche.internal wrappers below or through
 ;; gauche.ffi.native.

 (define-cclass <ffi-callback-pad>
   "ScmFFICallbackPad*" "Scm_FFICallbackPadClass"
   ()
   ()
   (printer (let* ((p::ScmFFICallbackPad* (SCM_FFI_CALLBACK_PAD obj)))
              (if (-> p destroyed)
                (Scm_Printf port "#<ffi-callback-pad destroyed>")
                (Scm_Printf port "#<ffi-callback-pad %p+%lu>"
                            (-> p xpad ptr)
                            (cast (unsigned long) (-> p entry_off)))))))

 (define-cclass <ffi-callback-context>
   "ScmFFICallbackContext*" "Scm_FFICallbackContextClass"
   ()
   ()
   (printer (let* ((c::ScmFFICallbackContext* (SCM_FFI_CALLBACK_CONTEXT obj)))
              (if (-> c destroyed)
                (Scm_Printf port "#<ffi-callback-context destroyed>")
                (Scm_Printf port "#<ffi-callback-context %p (%d entries)>"
                            (-> c xpad ptr)
                            (-> c n_entries))))))

 (define-cproc %%install-ffi-callback-one (code::<u8vector>
                                           entry::<fixnum>
                                           win-prolog-end::<fixnum>
                                           win-frame-size::<fixnum>)
   (return (Scm__InstallFFICallbackOne code entry
                                       win-prolog-end win-frame-size)))

 ;; Returns a <native-handle> tagged with TYPE whose pointer is the
 ;; pad's executable entry address.  Wrapping the address in a handle
 ;; (rather than returning a raw integer) keeps the trampoline's
 ;; address from leaking to Scheme code as a plain integer, and gives
 ;; the same shape the trampoline-build path in native.scm consumes.
 (define-cproc %%ffi-callback-pad-entry (pad type::<native-type>)
   (unless (SCM_FFI_CALLBACK_PAD_P pad)
     (Scm_Error "<ffi-callback-pad> required, but got: %S" pad))
   (return (Scm_MakeNativeHandleSimple
            (Scm__FFICallbackPadEntry (SCM_FFI_CALLBACK_PAD pad))
            (SCM_OBJ type))))

 (define-cproc %%destroy-ffi-callback-pad! (pad) ::<void>
   (unless (SCM_FFI_CALLBACK_PAD_P pad)
     (Scm_Error "<ffi-callback-pad> required, but got: %S" pad))
   (Scm__DestroyFFICallbackPad (SCM_FFI_CALLBACK_PAD pad)))

 ;; specs is a list of (code entry win-prolog-end win-frame-size).
 (define-cproc %%install-ffi-callback-context (specs::<list>)
   (let* ([n::int (cast int (Scm_Length specs))])
     (when (<= n 0)
       (Scm_Error "FFI callback context spec list must be non-empty"))
     (let* ([buf::ScmFFICallbackSpec* (SCM_NEW_ARRAY ScmFFICallbackSpec n)]
            [i::int 0])
       (for-each
        (lambda (entry)
          (unless (and (SCM_PAIRP entry)
                       (== (Scm_Length entry) 4))
            (Scm_Error "FFI callback spec must be a 4-element list, got: %S"
                       entry))
          (let* ([code (SCM_CAR entry)]
                 [rest (SCM_CDR entry)])
            (unless (SCM_U8VECTORP code)
              (Scm_Error "FFI callback spec: code must be u8vector: %S" code))
            (set! (ref (aref buf i) code) (SCM_U8VECTOR code))
            (set! (ref (aref buf i) entry)
                  (cast ScmSmallInt (Scm_GetInteger (SCM_CAR rest))))
            (set! rest (SCM_CDR rest))
            (set! (ref (aref buf i) win_prolog_end)
                  (cast ScmSmallInt (Scm_GetInteger (SCM_CAR rest))))
            (set! rest (SCM_CDR rest))
            (set! (ref (aref buf i) win_frame_size)
                  (cast ScmSmallInt (Scm_GetInteger (SCM_CAR rest)))))
          (post++ i))
        specs)
       (return (Scm__InstallFFICallbackContext buf n)))))

 (define-cproc %%ffi-callback-context-entry (ctx i::<fixnum>
                                             type::<native-type>)
   (unless (SCM_FFI_CALLBACK_CONTEXT_P ctx)
     (Scm_Error "<ffi-callback-context> required, but got: %S" ctx))
   (return (Scm_MakeNativeHandleSimple
            (Scm__FFICallbackContextEntry (SCM_FFI_CALLBACK_CONTEXT ctx)
                                          (cast int i))
            (SCM_OBJ type))))

 (define-cproc %%destroy-ffi-callback-context! (ctx) ::<void>
   (unless (SCM_FFI_CALLBACK_CONTEXT_P ctx)
     (Scm_Error "<ffi-callback-context> required, but got: %S" ctx))
   (Scm__DestroyFFICallbackContext (SCM_FFI_CALLBACK_CONTEXT ctx)))

 ;; Called from assembly callback trampoline (native-supp.scm)
 (define-cfn Scm__FFINativeCallCallback (fn args)
   (let* ([pkt::ScmEvalPacket]
          [r::int (Scm_Apply fn args (& pkt))])
     (when (< r 0)
       (Scm_Raise (ref pkt exception) SCM_RAISE_NON_CONTINUABLE))
     (return (aref (ref pkt results) 0))))

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

;; Returns a function that lookup the bootstrap functions.
;;  - This should be called during bootstrapping
;;  - Do not save the resulting closure in globally accessible space.
;; The definiton of tab is written in this way so that reference to mod
;; won't be in the closure's env.
(define (%%make-bootstrap-function-table names)
  (let1 tab (rlet1 tab (make-hash-table 'eq?)
              (let1 mod (find-module 'gauche.bootstrap)
                (unless mod
                  (error "You can't call me after the system is fully booted"))
                (dolist [name names]
                  (hash-table-put! tab name (module-binding-ref mod name)))))
    (^[n] (or (hash-table-get tab n #f)
              (error "Unknown procedure:" n)))))

;; Hold references to the bootstrap-only callback primitives so they
;; remain reachable after the gauche.bootstrap module is dismantled.
(define %%install-ffi-callback-one
  (module-binding-ref 'gauche.bootstrap '%%install-ffi-callback-one))
(define %%ffi-callback-pad-entry
  (module-binding-ref 'gauche.bootstrap '%%ffi-callback-pad-entry))
(define %%destroy-ffi-callback-pad!
  (module-binding-ref 'gauche.bootstrap '%%destroy-ffi-callback-pad!))
(define %%install-ffi-callback-context
  (module-binding-ref 'gauche.bootstrap '%%install-ffi-callback-context))
(define %%ffi-callback-context-entry
  (module-binding-ref 'gauche.bootstrap '%%ffi-callback-context-entry))
(define %%destroy-ffi-callback-context!
  (module-binding-ref 'gauche.bootstrap '%%destroy-ffi-callback-context!))

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
