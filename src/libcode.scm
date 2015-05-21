;;;
;;; Interface to VM code generator (code.c)
;;;
;;;   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

(declare) ;; a dummy form to suppress generation of "sci" file

;; This module is for internal use, shared by the runtime compiler,
;; AOT-compiler, and other module that needs to deal with Gauche VM
;; code generation.
(define-module gauche.vm.code
  (export vm-dump-code vm-code->list vm-insn-build
          vm-insn-code->name vm-insn-name->code
          
          make-compiled-code-builder
          compiled-code-emit0! compiled-code-emit0o!
          compiled-code-emit0i! compiled-code-emit0oi!
          compiled-code-emit-PUSH! compiled-code-emit-RET!
          compiled-code-emit1! compiled-code-emit1o!
          compiled-code-emit1i! compiled-code-emit1oi!
          compiled-code-emit2! compiled-code-emit2o!
          compiled-code-emit2i! compiled-code-emit2oi!
          compiled-code-new-label compiled-code-set-label!
          compiled-code-push-info!
          compiled-code-finish-builder
          compiled-code-copy!))
(select-module gauche.vm.code)

;;============================================================
;; Compiled code builder interface
;;

(inline-stub
 (declcode
  (.include <gauche/code.h>
            <gauche/vminsn.h>))

 (define-cproc vm-dump-code (code::<compiled-code>) ::<void>
   Scm_CompiledCodeDump)
 (define-cproc vm-code->list (code::<compiled-code>)
   Scm_CompiledCodeToList)
 (define-cproc vm-insn-build (insn) ::<ulong>
   (return (cast u_long (Scm_VMInsnBuild insn))))
 (define-cproc vm-insn-code->name (opcode::<uint>)
   (return (SCM_INTERN (Scm_VMInsnName opcode))))
 (define-cproc vm-insn-name->code (insn-name) ::<int>
   Scm_VMInsnNameToCode)

 (define-cproc make-compiled-code-builder (reqargs::<uint16> optargs::<uint16>
                                           name arginfo parent intform)
   Scm_MakeCompiledCodeBuilder)

 ;; CompiledCodeEmit is performance critical.  To reduce the overhead of
 ;;  argument passing, we prepare variations for specific code patterns.
 (define-cproc compiled-code-emit0!
   (cc::<compiled-code> code::<int>) ::<void>
   (Scm_CompiledCodeEmit cc code 0 0 '#f '#f))

 (define-cproc compiled-code-emit-PUSH!
   (cc::<compiled-code>) ::<void>
   (Scm_CompiledCodeEmit cc SCM_VM_PUSH 0 0 '#f '#f))

 (define-cproc compiled-code-emit-RET!
   (cc::<compiled-code>) ::<void>
   (Scm_CompiledCodeEmit cc SCM_VM_RET 0 0 '#f '#f))

 (define-cproc compiled-code-emit0o!
   (cc::<compiled-code> code::<int> operand) ::<void>
   (Scm_CompiledCodeEmit cc code 0 0 operand '#f))

 (define-cproc compiled-code-emit0i!
   (cc::<compiled-code> code::<int> info) ::<void>
   (Scm_CompiledCodeEmit cc code 0 0 '#f info))

 (define-cproc compiled-code-emit0oi!
   (cc::<compiled-code> code::<int> operand info) ::<void>
   (Scm_CompiledCodeEmit cc code 0 0 operand info))

 (define-cproc compiled-code-emit1!
   (cc::<compiled-code> code::<int> arg0::<int>) ::<void>
   (Scm_CompiledCodeEmit cc code arg0 0 '#f '#f))

 (define-cproc compiled-code-emit1o!
   (cc::<compiled-code> code::<int> arg0::<int> operand) ::<void>
   (Scm_CompiledCodeEmit cc code arg0 0 operand '#f))

 (define-cproc compiled-code-emit1i!
   (cc::<compiled-code> code::<int> arg0::<int> info) ::<void>
   (Scm_CompiledCodeEmit cc code arg0 0 '#f info))

 (define-cproc compiled-code-emit1oi!
   (cc::<compiled-code> code::<int> arg0::<int> operand info) ::<void>
   (Scm_CompiledCodeEmit cc code arg0 0 operand info))

 (define-cproc compiled-code-emit2!
   (cc::<compiled-code> code::<int> arg0::<int> arg1::<int>) ::<void>
   (Scm_CompiledCodeEmit cc code arg0 arg1 '#f '#f))

 (define-cproc compiled-code-emit2o!
   (cc::<compiled-code> code::<int> arg0::<int> arg1::<int> operand) ::<void>
   (Scm_CompiledCodeEmit cc code arg0 arg1 operand '#f))

 (define-cproc compiled-code-emit2i!
   (cc::<compiled-code> code::<int> arg0::<int> arg1::<int> info) ::<void>
   (Scm_CompiledCodeEmit cc code arg0 arg1 '#f info))

 (define-cproc compiled-code-emit2oi!
   (cc::<compiled-code> code::<int> arg0::<int> arg1::<int> operand info)::<void>
   (Scm_CompiledCodeEmit cc code arg0 arg1 operand info))

 (define-cproc compiled-code-new-label (cc::<compiled-code>)
   Scm_CompiledCodeNewLabel)

 (define-cproc compiled-code-set-label! (cc::<compiled-code> label)
   ::<void> Scm_CompiledCodeSetLabel)

 (define-cproc compiled-code-finish-builder (cc::<compiled-code>
                                             maxstack::<int>)
   ::<void> Scm_CompiledCodeFinishBuilder)

 (define-cproc compiled-code-copy! (dest::<compiled-code>
                                    src::<compiled-code>)
   ::<void> Scm_CompiledCodeCopyX)

 ;; Push source info to (-> compiled-code info).
 ;; Usually the source info is pushed as the instructions are emitted.
 ;; This is mainly used to add info that are not attached to a particular
 ;; insturuction (e.g. the definition of the entire closure).
 (define-cproc compiled-code-push-info! (cc::<compiled-code> info)
   ::<void> Scm_CompiledCodePushInfo)

 ;; Kludge: Let gauche.internal import me.  It must be done before the
 ;; compiler runs. This should eventually be done in the gauche.internal side.
 (initcode
  (Scm_ImportModule (Scm_GaucheInternalModule) 'gauche.vm.code SCM_FALSE 0))
 )
