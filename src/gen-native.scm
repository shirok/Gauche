;;;
;;; Generate native code vector for FFI
;;;

(use scheme.list)
(use util.match)
(use gauche.uvector)
(use file.util)
(use lang.asm.linker)
(use lang.asm.x86_64)
(use lang.asm.fragment)

;;;
;;; Common Prologue
;;;

(define (emit-header port)
  (display ";; libnative.scm supplemental code.\n" port)
  (display ";; Generated automatically by gen-native.scm.  DO NOT EDIT.\n" port)
  (display "\n" port))

;;;
;;;  Foregin function call
;;;


;;; For SYSV AMD64 calling convention: Section 3.2 of
;;; http://refspecs.linux-foundation.org/elf/x86_64-abi-0.95.pdf

(define-asm-fragment amd64-call-reg x86_64
  '(entry6f7:   ((movs_ :farg7-variant) (farg7:) %xmm7)
    entry6f6:   ((movs_ :farg6-variant) (farg6:) %xmm6)
    entry6f5:   ((movs_ :farg5-variant) (farg5:) %xmm5)
    entry6f4:   ((movs_ :farg4-variant) (farg4:) %xmm4)
    entry6f3:   ((movs_ :farg3-variant) (farg3:) %xmm3)
    entry6f2:   ((movs_ :farg2-variant) (farg2:) %xmm2)
    entry6f1:   ((movs_ :farg1-variant) (farg1:) %xmm1)
    entry6f0:   ((movs_ :farg0-variant) (farg0:) %xmm0)
    entry6:     (movq (imm64 :iarg5) %r9)
    entry5:     (movq (imm64 :iarg4) %r8)
    entry4:     (movq (imm64 :iarg3) %rcx)
    entry3:     (movq (imm64 :iarg2) %rdx)
    entry2:     (movq (imm64 :iarg1) %rsi)
    entry1:     (movq (imm64 :iarg0) %rdi)
    entry0:     (push %rbx)               ; align stack (entry has rsp%16==8)
                (movb (imm8 :num-fargs) %al)
                (call (func:))
                ;; %rax, %rdx, %xmm[01] may have the return value at this point

                ;; retkind encoding
                ;;   0 - <top>: No conversion needed
                ;;   1 - <fixnum>
                ;;   2 - integer that may not fit <fixnum>
                ;;   3 - unsigned integer that may not fit <fixnum>
                ;;   4 - <double>
                ;;   5 - <float>
                ;;   6 - <void>
                ;;   7 - <c-string>
                ;;   8 - general pointer
                (movb (imm8 :retkind) %bl)
                (decb %bl)
                (jsl epilog:)
                (jz fixnum:)
                (decb %bl)
                (jz integer:)
                (decb %bl)
                (jz uinteger:)
                (decb %bl)
                (jz double:)
                (decb %bl)
                (jz float:)
                (decb %bl)
                (jz void:)
                (decb %bl)
                (jz cstring:)
    pointer:    (movq %rax %rdi)
                (movq (imm64 :rettype) %rsi)
                (xorq %rax %rax)
                (call (fn-handle:))
                (jmp epilog:)
    fixnum:     (shl 2 %rax)
                (incq %rax)
                (jmp epilog:)
    integer:    (movq %rax %rdi)
                (xorq %rax %rax)
                (call (fn-int:))
                (jmp epilog:)
    uinteger:   (movq %rax %rdi)
                (xorq %rax %rax)
                (call (fn-uint:))
                (jmp epilog:)
    double:     (movb 1 %al)
                (call (fn-flonum:))
                (jmp epilog:)
    float:      (cvtss2sd %xmm0 %xmm0)
                (movb 1 %al)
                (call (fn-flonum:))
                (jmp epilog:)
    void:       (movq (imm64 :SCM_UNDEFINED) %rax)
                (jmp epilog:)
    cstring:    (movq %rax %rdi)
                (movq -1 %rsi)
                (movq -1 %rdx)
                (movq (imm32 :SCM_STRING_COPYING) %rcx)
                (call (fn-string:)) ; cstr, -1, -1, SCM_STRING_COPYING
                ;; fallthrough
    epilog:     (pop %rbx)
                (ret)
                (.endsection text)

                (.section data)
    func:       (.dataq :func)
    fn-flonum:  (.dataq :Scm_MakeFlonum)
    fn-string:  (.dataq :Scm_MakeString)
    fn-handle:  (.dataq :Scm_MakeNativeHandleSimple)
    fn-int:     (.dataq :Scm_IntptrToInteger)
    fn-uint:    (.dataq :Scm_UintptrToInteger)
    farg0:      (.dataq :farg0)
    farg1:      (.dataq :farg1)
    farg2:      (.dataq :farg2)
    farg3:      (.dataq :farg3)
    farg4:      (.dataq :farg4)
    farg5:      (.dataq :farg5)
    farg6:      (.dataq :farg6)
    farg7:      (.dataq :farg7)
    end:))

(define-asm-fragment amd64-call-spill x86_64
  '(entry6f7:   ((movs_ :farg7-variant) (farg7:) %xmm7)
    entry6f6:   ((movs_ :farg6-variant) (farg6:) %xmm6)
    entry6f5:   ((movs_ :farg5-variant) (farg5:) %xmm5)
    entry6f4:   ((movs_ :farg4-variant) (farg4:) %xmm4)
    entry6f3:   ((movs_ :farg3-variant) (farg3:) %xmm3)
    entry6f2:   ((movs_ :farg2-variant) (farg2:) %xmm2)
    entry6f1:   ((movs_ :farg1-variant) (farg1:) %xmm1)
    entry6f0:   ((movs_ :farg0-variant) (farg0:) %xmm0)
    entry6:     (movq (imm64 :iarg5) %r9)
    entry5:     (movq (imm64 :iarg4) %r8)
    entry4:     (movq (imm64 :iarg3) %rcx)
    entry3:     (movq (imm64 :iarg2) %rdx)
    init:       (movq (imm32 :init-spill-size) %rax)
                (leaq (spill:) %rsi)
                (subq (imm8 :align-pad) %rsp)
    loop:       (movq (%rsi) %rdi)
                (push %rdi)
                (addq 8 %rsi)
                (subq 8 %rax)
                (jnz loop:)
    entry2:     (movq (imm64 :iarg1) %rsi)
    entry1:     (movq (imm64 :iarg0) %rdi)
    entry0:     (movb (imm8 :num-fargs) %al)
                (call (func:))
                ;; %rax, %rdx, %xmm[01] may have the return value at this point

                ;; retkind encoding
                ;;   0 - <top>: No conversion needed
                ;;   1 - <fixnum>
                ;;   2 - integer that may not fit <fixnum>
                ;;   3 - unsigned integer that may not fit <fixnum>
                ;;   4 - <double>
                ;;   5 - <float>
                ;;   6 - <void>
                ;;   7 - <c-string>
                ;;   8 - general pointer
                (movb (imm8 :retkind) %bl)
                (decb %bl)
                (jsl epilog:)
                (jz fixnum:)
                (decb %bl)
                (jz integer:)
                (decb %bl)
                (jz uinteger:)
                (decb %bl)
                (jz double:)
                (decb %bl)
                (jz float:)
                (decb %bl)
                (jz void:)
                (decb %bl)
                (jz cstring:)
    pointer:    (movq %rax %rdi)
                (movq (imm64 :rettype) %rsi)
                (xorq %rax %rax)
                (call (fn-handle:))
                (jmp epilog:)
    fixnum:     (shl 2 %rax)
                (incq %rax)
                (jmp epilog:)
    integer:    (movq %rax %rdi)
                (xorq %rax %rax)
                (call (fn-int:))
                (jmp epilog:)
    uinteger:   (movq %rax %rdi)
                (xorq %rax %rax)
                (call (fn-uint:))
                (jmp epilog:)
    double:     (movb 1 %al)
                (call (fn-flonum:))
                (jmp epilog:)
    float:      (cvtss2sd %xmm0 %xmm0)
                (movb 1 %al)
                (call (fn-flonum:))
                (jmp epilog:)
    void:       (movq (imm64 :SCM_UNDEFINED) %rax)
                (jmp epilog:)
    cstring:    (movq %rax %rdi)
                (movq -1 %rsi)
                (movq -1 %rdx)
                (movq (imm32 :SCM_STRING_COPYING) %rcx)
                (call (fn-string:)) ; cstr, -1, -1, SCM_STRING_COPYING
                ;; fallthrough
    epilog:     (addq (imm32 :epilogue-spill-size) %rsp)
                (ret)
                (.endsection text)

                (.section data)
    func:       (.dataq :func)
    fn-flonum:  (.dataq :Scm_MakeFlonum)
    fn-string:  (.dataq :Scm_MakeString)
    fn-handle:  (.dataq :Scm_MakeNativeHandleSimple)
    fn-int:     (.dataq :Scm_IntptrToInteger)
    fn-uint:    (.dataq :Scm_UintptrToInteger)
    farg0:      (.dataq :farg0)
    farg1:      (.dataq :farg1)
    farg2:      (.dataq :farg2)
    farg3:      (.dataq :farg3)
    farg4:      (.dataq :farg4)
    farg5:      (.dataq :farg5)
    farg6:      (.dataq :farg6)
    farg7:      (.dataq :farg7)
    spill:      (.dataq :spill)))

;;; For Windows x86_64 calling convention:
;;; https://docs.microsoft.com/en-us/cpp/build/x64-calling-convention?view=msvc-160

(define-asm-fragment winx64-call-reg x86_64
  '(entry4f3:((movs_ :farg3-variant) (farg3:) %xmm3)
    entry4f2:((movs_ :farg2-variant) (farg2:) %xmm2)
    entry4f1:((movs_ :farg1-variant) (farg1:) %xmm1)
    entry4f0:((movs_ :farg0-variant) (farg0:) %xmm0)
    entry4:  (movq (imm64 :iarg3) %r9)
    entry3:  (movq (imm64 :iarg2) %r8)
    entry2:  (movq (imm64 :iarg1) %rdx)
    entry1:  (movq (imm64 :iarg0) %rcx)
    entry0:  (push %rbx)               ; save rbx; aligns rsp to 16
             (addq -32 %rsp)           ; shadow space (shared with helper calls)
             (call (func:))
             ;; %rax or %xmm0 may have the return value at this point.
             ;; retkind encoding matches amd64-call-reg.
             (movb (imm8 :retkind) %bl)
             (decb %bl)
             (jsl epilog:)             ; retkind=0: return %rax as-is
             (jz fixnum:)              ; retkind=1
             (decb %bl)
             (jz integer:)             ; retkind=2
             (decb %bl)
             (jz uinteger:)            ; retkind=3
             (decb %bl)
             (jz double:)              ; retkind=4
             (decb %bl)
             (jz float:)               ; retkind=5
             (decb %bl)
             (jz void:)                ; retkind=6
             (decb %bl)
             (jz cstring:)             ; retkind=7
    pointer: (movq %rax %rcx)          ; rcx=ptr
             (movq (imm64 :rettype) %rdx) ; rdx=type
             (call (fn-handle:))
             (jmp epilog:)
    fixnum:  (shl 2 %rax)
             (incq %rax)
             (jmp epilog:)
    integer: (movq %rax %rcx)          ; rcx=intptr_t
             (call (fn-int:))
             (jmp epilog:)
    uinteger:(movq %rax %rcx)          ; rcx=uintptr_t
             (call (fn-uint:))
             (jmp epilog:)
    float:   (cvtss2sd %xmm0 %xmm0)
    double:  (call (fn-flonum:))
             (jmp epilog:)
    void:    (movq (imm64 :SCM_UNDEFINED) %rax)
             (jmp epilog:)
    cstring: (movq %rax %rcx)          ; rcx=cstr
             (movq -1 %rdx)            ; rdx=size
             (movq -1 %r8)             ; r8=len
             (movq (imm32 :SCM_STRING_COPYING) %r9) ; r9=flags
             (call (fn-string:))
    epilog:  (addq 32 %rsp)
             (pop %rbx)
             (ret)
             (.endsection text)

             (.section data)
    func:       (.dataq :func)
    fn-flonum:  (.dataq :Scm_MakeFlonum)
    fn-string:  (.dataq :Scm_MakeString)
    fn-handle:  (.dataq :Scm_MakeNativeHandleSimple)
    fn-int:     (.dataq :Scm_IntptrToInteger)
    fn-uint:    (.dataq :Scm_UintptrToInteger)
    farg0:   (.dataq :farg0)
    farg1:   (.dataq :farg1)
    farg2:   (.dataq :farg2)
    farg3:   (.dataq :farg3)
    end:))

(define-asm-fragment winx64-call-spill x86_64
  '(entry:
    entry4f3:((movs_ :farg3-variant) (farg3:) %xmm3)
    entry4f2:((movs_ :farg2-variant) (farg2:) %xmm2)
    entry4f1:((movs_ :farg1-variant) (farg1:) %xmm1)
    entry4f0:((movs_ :farg0-variant) (farg0:) %xmm0)
    init:    (push %rbx)               ; save rbx before spill pushes
             (movq (imm32 :init-spill-size) %rax)
             (leaq (spill: %rip) %r10) ; %r10/%r11: volatile in Win64 ABI
             (subq (imm8 :align-pad) %rsp) ; ensure alignment
    loop:    (movq (%r10) %r11)
             (push %r11)
             (addq 8 %r10)
             (subq 8 %rax)
             (jnz loop:)
    entry4:  (movq (imm64 :iarg3) %r9)
    entry3:  (movq (imm64 :iarg2) %r8)
    entry2:  (movq (imm64 :iarg1) %rdx)
    entry1:  (movq (imm64 :iarg0) %rcx)
    entry0:  (addq -32 %rsp)           ; shadow space (shared with helper calls)
             (call (func:))
             ;; boxing dispatch (Windows x64 calling convention for helpers)
             (movb (imm8 :retkind) %bl)
             (decb %bl)
             (jsl epilog:)
             (jz fixnum:)
             (decb %bl)
             (jz integer:)
             (decb %bl)
             (jz uinteger:)
             (decb %bl)
             (jz double:)
             (decb %bl)
             (jz float:)
             (decb %bl)
             (jz void:)
             (decb %bl)
             (jz cstring:)
    pointer: (movq %rax %rcx)
             (movq (imm64 :rettype) %rdx)
             (call (fn-handle:))
             (jmp epilog:)
    fixnum:  (shl 2 %rax)
             (incq %rax)
             (jmp epilog:)
    integer: (movq %rax %rcx)
             (call (fn-int:))
             (jmp epilog:)
    uinteger:(movq %rax %rcx)
             (call (fn-uint:))
             (jmp epilog:)
    float:   (cvtss2sd %xmm0 %xmm0)
    double:  (call (fn-flonum:))
             (jmp epilog:)
    void:    (movq (imm64 :SCM_UNDEFINED) %rax)
             (jmp epilog:)
    cstring: (movq %rax %rcx)
             (movq -1 %rdx)
             (movq -1 %r8)
             (movq (imm32 :SCM_STRING_COPYING) %r9)
             (call (fn-string:))
    epilog:  (addq 32 %rsp)
    epilogue:(addq (imm32 :epilogue-spill-size) %rsp) ; undo spills+pad before rbx
             (pop %rbx)
             (ret)
             (.endsection text)

             (.section data)
    func:       (.dataq :func)
    fn-flonum:  (.dataq :Scm_MakeFlonum)
    fn-string:  (.dataq :Scm_MakeString)
    fn-handle:  (.dataq :Scm_MakeNativeHandleSimple)
    fn-int:     (.dataq :Scm_IntptrToInteger)
    fn-uint:    (.dataq :Scm_UintptrToInteger)
    farg0:   (.dataq :farg0)
    farg1:   (.dataq :farg1)
    farg2:   (.dataq :farg2)
    farg3:   (.dataq :farg3)
    spill:   (.dataq :spill)))

;;;
;;; Foreign function callabck
;;;

;;; SysV amd64 ABI
;;;
;;; Stack frame (entry rsp%16==8; we push %rbx then sub $176,%rsp):
;;;   %rsp+0..47     int_save[0..5]   — saved %rdi,%rsi,%rdx,%rcx,%r8,%r9
;;;   %rsp+48..111   fp_save[0..7]    — saved %xmm0..%xmm7
;;;   %rsp+112..175  boxed[0..7]      — boxed ScmObj per arg
;;;
;;; %rbx serves as the boxer-loop counter (callee-save, preserved
;;; across helper calls).  All other regs are scratch.
;;;
;;; Per-callback metadata in the data section:
;;;   :nargs                       int8 — argument count (≤ MAX_ARGS)
;;;   :argkind0..:argkind7         int8 — boxing kind per arg
;;;   :argsrc0..:argsrc7           int8 — source-slot index (int_save or fp_save)
;;;   :argtype0..:argtype7         ScmObj — pointer type for kind=8, else SCM_FALSE
;;;   :proc                        ScmObj — the Scheme procedure to apply
;;;
;;; Boxing kinds — see %asm-boxkind for the canonical mapping.  Kind 6
;;; (<void>) only makes sense on the return side and is rejected here.

(define-asm-fragment amd64-callback x86_64
  '(entry:
             (push %rbx)                ; rsp%16: 8 -> 0
             (subq (imm32 176) %rsp)    ; 176 % 16 == 0
    prolog-end:
             ;; Save 6 int arg regs into int_save[0..5] at rsp+0.
             (movq %rdi (0 %rsp))
             (movq %rsi (8 %rsp))
             (movq %rdx (16 %rsp))
             (movq %rcx (24 %rsp))
             (movq %r8  (32 %rsp))
             (movq %r9  (40 %rsp))
             ;; Save 8 xmm arg regs into fp_save[0..7] at rsp+48.
             (movsd %xmm0 (48 %rsp))
             (movsd %xmm1 (56 %rsp))
             (movsd %xmm2 (64 %rsp))
             (movsd %xmm3 (72 %rsp))
             (movsd %xmm4 (80 %rsp))
             (movsd %xmm5 (88 %rsp))
             (movsd %xmm6 (96 %rsp))
             (movsd %xmm7 (104 %rsp))
             ;; rbx = arg index, iterate while rbx < nargs.
             (xorq %rbx %rbx)
    box-loop:
             (movzbq (nargs:) %rax)
             (cmpq %rax %rbx)
             (jgel build-args:)
             ;; %al <- argkinds[rbx], %r10 <- argsrcs[rbx]
             (leaq (argkinds:) %r10)
             (movzbq (%r10 %rbx 1) %rax)
             (leaq (argsrcs:) %r10)
             (movzbq (%r10 %rbx 1) %r10)
             ;; Float kinds first; they read from fp_save not int_save.
             (cmpb 4 %al)
             (jel box-double:)
             (cmpb 5 %al)
             (jel box-float:)
             ;; Int-like: load int_save[r10] into %rdi.
             (movq (0 %rsp %r10 8) %rdi)
             (cmpb 1 %al)
             (jel box-fixnum:)
             (cmpb 2 %al)
             (jel box-intptr:)
             (cmpb 3 %al)
             (jel box-uintptr:)
             (cmpb 7 %al)
             (jel box-cstring:)
             (cmpb 8 %al)
             (jel box-pointer:)
             ;; Fall through: kind 0 (<top>) — value already in %rdi.
             (movq %rdi %rax)
             (jmpl store-boxed:)

    box-fixnum:
             (movq %rdi %rax)
             (shl 2 %rax)
             (incq %rax)
             (jmpl store-boxed:)

    box-intptr:
             (xorq %rax %rax)
             (call (fn-int:))
             (jmpl store-boxed:)

    box-uintptr:
             (xorq %rax %rax)
             (call (fn-uint:))
             (jmpl store-boxed:)

    box-cstring:
             (movq -1 %rsi)
             (movq -1 %rdx)
             (movq (imm32 :SCM_STRING_COPYING) %rcx)
             (call (fn-string:))
             (jmpl store-boxed:)

    box-pointer:
             (leaq (argtypes:) %r11)
             (movq (%r11 %rbx 8) %rsi)  ; %rsi = type ScmObj
             (call (fn-handle:))
             (jmpl store-boxed:)

    box-double:
             (movsd (48 %rsp %r10 8) %xmm0)
             (movb 1 %al)
             (call (fn-flonum:))
             (jmpl store-boxed:)

    box-float:
             (movsd (48 %rsp %r10 8) %xmm0) ; reads 8 bytes; low 32 are the float
             (cvtss2sd %xmm0 %xmm0)
             (movb 1 %al)
             (call (fn-flonum:))
             ;; fall through

    store-boxed:
             (movq %rax (112 %rsp %rbx 8)) ; boxed[rbx] = %rax
             (incq %rbx)
             (jmpl box-loop:)

    build-args:
             ;; acc = '() ; for i from nargs-1 downto 0:
             ;; acc = (cons boxed[i] acc)
             (movq (SCM_NIL:) %rax)
             (movzbq (nargs:) %rbx)
    build-loop:
             (cmpq 0 %rbx)
             (jel apply-it:)
             (decq %rbx)
             (movq %rax %rsi)              ; cdr = acc
             (movq (112 %rsp %rbx 8) %rdi) ; car = boxed[rbx]
             (call (fn-cons:))
             (jmpl build-loop:)

    apply-it:
             (movq (proc:) %rdi)        ; ScmObj proc
             (movq %rax %rsi)           ; args list
             (call (fn-callback:))      ; %rax = Scm__FFINativeCallCallback(...)

             ;; Return-side dispatch: unbox %rax into a C value.  retkind
             ;; encoding mirrors %asm-boxkind / amd64-call-reg.
             (movb (imm8 :retkind) %bl)
             (decb %bl)
             (jsl epilog:)              ; retkind=0 (<top>): %rax already ScmObj
             (jz ret-fixnum:)           ; retkind=1
             (decb %bl)
             (jz ret-intptr:)           ; retkind=2
             (decb %bl)
             (jz ret-uintptr:)          ; retkind=3
             (decb %bl)
             (jz ret-double:)           ; retkind=4
             (decb %bl)
             (jz ret-float:)            ; retkind=5
             (decb %bl)
             (jz ret-void:)             ; retkind=6
             (decb %bl)
             (jz ret-cstring:)          ; retkind=7
             ;; fallthrough: retkind=8 (pointer)
    ret-pointer:
             (movq %rax %rdi)
             (call (fn-handle-ptr:))
             (jmp epilog:)
    ret-fixnum:
             (sar 2 %rax)               ; arithmetic >>2 to recover signed value
             (jmp epilog:)
    ret-intptr:
             (movq %rax %rdi)
             (movq 3 %rsi)              ; SCM_CLAMP_BOTH
             (xorq %rdx %rdx)           ; oor = NULL
             (call (fn-get-int:))
             (jmp epilog:)
    ret-uintptr:
             (movq %rax %rdi)
             (movq 3 %rsi)              ; SCM_CLAMP_BOTH
             (xorq %rdx %rdx)
             (call (fn-get-uint:))
             (jmp epilog:)
    ret-double:
             (movq %rax %rdi)
             (call (fn-get-double:))    ; result in %xmm0
             (jmp epilog:)
    ret-float:
             (movq %rax %rdi)
             (call (fn-get-double:))
             (cvtsd2ss %xmm0 %xmm0)
             (jmp epilog:)
    ret-void:
             (jmp epilog:)
    ret-cstring:
             (movq %rax %rdi)
             (call (fn-get-cstring:))
             ;; fallthrough
    epilog:
             (addq (imm32 176) %rsp)
             (pop %rbx)
             (ret)
             (.endsection text)

             (.section data)
    proc:           (.dataq :proc)
    fn-callback:    (.dataq :Scm__FFINativeCallCallback)
    fn-cons:        (.dataq :Scm_Cons)
    fn-flonum:      (.dataq :Scm_MakeFlonum)
    fn-string:      (.dataq :Scm_MakeString)
    fn-handle:      (.dataq :Scm_MakeNativeHandleSimple)
    fn-int:         (.dataq :Scm_IntptrToInteger)
    fn-uint:        (.dataq :Scm_UintptrToInteger)
    fn-get-int:     (.dataq :Scm_GetIntegerClamp)
    fn-get-uint:    (.dataq :Scm_GetIntegerUClamp)
    fn-get-double:  (.dataq :Scm_GetDouble)
    fn-get-cstring: (.dataq :Scm_GetStringConst)
    fn-handle-ptr:  (.dataq :Scm_NativeHandlePtr)
    SCM_NIL:        (.dataq :SCM_NIL)
    nargs:          (.datab :nargs)
    argkinds:       (.datab :argkind0)
                    (.datab :argkind1)
                    (.datab :argkind2)
                    (.datab :argkind3)
                    (.datab :argkind4)
                    (.datab :argkind5)
                    (.datab :argkind6)
                    (.datab :argkind7)
    argsrcs:        (.datab :argsrc0)
                    (.datab :argsrc1)
                    (.datab :argsrc2)
                    (.datab :argsrc3)
                    (.datab :argsrc4)
                    (.datab :argsrc5)
                    (.datab :argsrc6)
                    (.datab :argsrc7)
    argtypes:       (.dataq :argtype0)
                    (.dataq :argtype1)
                    (.dataq :argtype2)
                    (.dataq :argtype3)
                    (.dataq :argtype4)
                    (.dataq :argtype5)
                    (.dataq :argtype6)
                    (.dataq :argtype7)
    end:))


;;;
;;; Code generators
;;;

(define (gen-stub-amd64 port)
  (define (Ps . exprs)
    (for-each (cut pprint <> :port port) exprs))

  (display ";; Register-only calling\n" port)
  (dump-asm-fragment amd64-call-reg port)

  (display ";; Spill-to-stack case\n" port)
  (dump-asm-fragment amd64-call-spill port)

  (display ";; Callback trampoline\n" port)
  (dump-asm-fragment amd64-callback port)

  (Ps
   `(define (%iarg-type? t)
      (or (eq? t <top>)
          (subtype? t <integer>)
          (eq? t <c-string>)
          (is-a? t <c-pointer>)
          (is-a? t <c-array>)
          (is-a? t <c-function>)))
   `(define (%farg-type? t)
      (or (eq? t <double>)
          (eq? t <float>)))
   ;; Boxing-kind dispatch shared by the FFI return path and the
   ;; callback argument-boxing path.  The integer encoding must match
   ;; the branch code in the assembly fragments in gen-native.scm.
   ;;   0  <top>          pass raw 8-byte value through as ScmObj
   ;;   1  <fixnum>       value fits in fixnum: shl 2; incq
   ;;   2  <intptr_t>     Scm_IntptrToInteger
   ;;   3  <uintptr_t>    Scm_UintptrToInteger
   ;;   4  <double>       Scm_MakeFlonum
   ;;   5  <float>        cvtss2sd; Scm_MakeFlonum
   ;;   6  <void>         (return-only) — produce SCM_UNDEFINED
   ;;   7  <c-string>     Scm_MakeString
   ;;   8  pointer        Scm_MakeNativeHandleSimple
   ;; Kind 6 is invalid as an argument type and the callback builder
   ;; rejects it (native-type->call-canon never produces <void> for
   ;; args anyway).  <uint8> is accepted as an alias for <fixnum>;
   ;; the FFI return path used to spell it that way.
   `(define (%asm-boxkind type)
      (cond [(eq? type <top>)                                     0]
            [(or (eq? type <fixnum>) (eq? type <uint8>))          1]
            [(eq? type <intptr_t>)                                2]
            [(eq? type <uintptr_t>)                               3]
            [(eq? type <double>)                                  4]
            [(eq? type <float>)                                   5]
            [(eq? type <void>)                                    6]
            [(eq? type <c-string>)                                7]
            [(or (is-a? type <c-pointer>)
                 (is-a? type <c-array>)
                 (is-a? type <c-function>))                       8]
            [else (error "unknown FFI box kind for asm path:" type)]))
   `(define-enum SCM_STRING_COPYING)
   )

  ;; (call-amd64 <native-handle> args rettype)
  ;;  args : ((type value) ...)
  ;; NB: In the final form, we won't expose this function to the user; it's
  ;; too error-prone.  You can wreck havoc just by passing a wrong type.
  ;; Instead, we'll require the user to parse the C function declaration
  ;; and we automatically extract the type info.
  (Ps
   `(define call-amd64
      (^[ptr args rettype]
        (let* ([num-iargs (count (^p (%iarg-type? (car p))) args)]
               [num-fargs (count (^p (%farg-type? (car p))) args)]
               [num-spills (+ (max 0 (- num-iargs 6))
                              (max 0 (- num-fargs 8)))])
          (if (zero? num-spills)
            (call-amd64-regs  ptr args num-iargs num-fargs rettype)
            (call-amd64-spill ptr args
                              (min num-iargs 6)
                              (min num-fargs 8)
                              num-spills rettype))))))

  ;; call-amd64-regs: helper addresses prelinked at init! time; per-call
  ;; params supply :func, argument values, :retkind, and :rettype.
  (Ps
   `(define call-amd64-regs
      (let ([% (%%make-bootstrap-function-table '(%%call-native
                                                  %%get-entry-address))]
            [link-tmpl #f] [lbl-off #f] [prelinked-tmpl #f])
        (define (init!)
          (set! link-tmpl (module-binding-ref 'lang.asm.linker 'link-templates))
          (set! lbl-off   (module-binding-ref 'lang.asm.linker 'linked-label-offset))
          (let ([prelink (module-binding-ref 'lang.asm.linker 'prelink-template)]
                [gea     (% '%%get-entry-address)])
            (set! prelinked-tmpl
                  (prelink (amd64-call-reg-tmpl)
                           `((:Scm_MakeFlonum             ,<intptr_t>
                              ,(gea "_Scm_MakeFlonum"))
                             (:Scm_MakeString             ,<intptr_t>
                              ,(gea "_Scm_MakeString"))
                             (:Scm_MakeNativeHandleSimple ,<intptr_t>
                              ,(gea "_Scm_MakeNativeHandleSimple"))
                             (:Scm_IntptrToInteger        ,<intptr_t>
                              ,(gea "_Scm_IntptrToInteger"))
                             (:Scm_UintptrToInteger       ,<intptr_t>
                              ,(gea "_Scm_UintptrToInteger"))
                             (:SCM_STRING_COPYING         ,<int32>
                              ,SCM_STRING_COPYING)
                             (:SCM_UNDEFINED              ,<top>
                              ,(undefined)))))))
        (^[ptr args num-iargs num-fargs rettype]
          (when (not link-tmpl) (init!))
          (let* ([effective-nargs (if (zero? num-fargs)
                                    num-iargs
                                    (+ 6 num-fargs))]
                 [entry-label (~ '#(entry0: entry1: entry2: entry3: entry4:
                                    entry5: entry6:
                                    entry6f0: entry6f1: entry6f2: entry6f3:
                                    entry6f4: entry6f5: entry6f6: entry6f7:)
                                 effective-nargs)]
                 [retkind (%asm-boxkind rettype)]
                 [params
                  (let loop ([args args] [icount 0] [fcount 0] [r '()])
                    (cond [(null? args) r]
                          [(%iarg-type? (caar args))
                           (loop (cdr args) (+ icount 1) fcount
                                 (cons `(,(~ '#(:iarg0 :iarg1 :iarg2
                                                :iarg3 :iarg4 :iarg5)
                                             icount)
                                         ,@(car args))
                                       r))]
                          [(%farg-type? (caar args))
                           (let ([fkey (~ '#(:farg0 :farg1 :farg2 :farg3
                                             :farg4 :farg5 :farg6 :farg7)
                                          fcount)]
                                 [vkey (~ '#(:farg0-variant :farg1-variant
                                             :farg2-variant :farg3-variant
                                             :farg4-variant :farg5-variant
                                             :farg6-variant :farg7-variant)
                                          fcount)])
                             (loop (cdr args) icount (+ fcount 1)
                                   (if (eq? (caar args) <float>)
                                     (list* `(,vkey ,<uint8> movss)
                                            `(,fkey ,@(car args)) r)
                                     (cons `(,fkey ,@(car args)) r))))]
                          [else (error "bad arg entry:" (car args))]))])
            (receive [bytes lbs]
                (link-tmpl (list prelinked-tmpl)
                           `((:func ,<void*> ,ptr)
                             (:num-fargs ,<uint8> ,num-fargs)
                             (:retkind ,<integer> ,retkind)
                             (:rettype ,<top> ,rettype)
                             ,@params))
              ((% '%%call-native) 0 0 bytes 0
                                  (lbl-off lbs 'end:)
                                  (lbl-off lbs entry-label)
                                  0 0)))))))

  ;; call-amd64-spill: named patches handled by link-templates; only raw
  ;; spill-slot offsets remain in the %%call-native patcher list.
  ;; The four C helper addresses and SCM_STRING_COPYING are baked into a
  ;; prelinked template once at init! time.  Per-call params supply :func,
  ;; argument values, :retkind, and :rettype.
  (Ps
   `(define call-amd64-spill
      (let ([% (%%make-bootstrap-function-table '(%%call-native
                                                  %%get-entry-address))]
            [link-tmpl #f] [lbl-off #f] [prelinked-tmpl #f])
        (define (init!)
          (set! link-tmpl (module-binding-ref 'lang.asm.linker 'link-templates))
          (set! lbl-off   (module-binding-ref 'lang.asm.linker 'linked-label-offset))
          (let ([prelink (module-binding-ref 'lang.asm.linker 'prelink-template)]
                [gea     (% '%%get-entry-address)])
            (set! prelinked-tmpl
                  (prelink (amd64-call-spill-tmpl)
                           `((:Scm_MakeFlonum             ,<intptr_t>
                              ,(gea "_Scm_MakeFlonum"))
                             (:Scm_MakeString             ,<intptr_t>
                              ,(gea "_Scm_MakeString"))
                             (:Scm_MakeNativeHandleSimple ,<intptr_t>
                              ,(gea "_Scm_MakeNativeHandleSimple"))
                             (:Scm_IntptrToInteger        ,<intptr_t>
                              ,(gea "_Scm_IntptrToInteger"))
                             (:Scm_UintptrToInteger       ,<intptr_t>
                              ,(gea "_Scm_UintptrToInteger"))
                             (:SCM_STRING_COPYING         ,<int32>
                              ,SCM_STRING_COPYING)
                             (:SCM_UNDEFINED              ,<top>
                              ,(undefined)))))))
        (^[ptr args num-iargs num-fargs num-spills rettype]
          (when (not link-tmpl) (init!))
          (let* ([effective-nargs (if (zero? num-fargs)
                                    num-iargs
                                    (+ 6 num-fargs))]
                 [entry-label (~ '#(entry0: entry1: entry2: entry3: entry4:
                                    entry5: entry6:
                                    entry6f0: entry6f1: entry6f2: entry6f3:
                                    entry6f4: entry6f5: entry6f6: entry6f7:)
                                 effective-nargs)])
            (let loop ([args args] [icount 0] [fcount 0] [scount 0]
                       [named '()] [spill-params '()])
              (if (null? args)
                (let* ([align-pad (if (even? num-spills) 8 0)]
                       [spill-area-bytes (* 8 num-spills)]
                       [retkind (%asm-boxkind rettype)])
                  (receive [bytes lbs]
                      (link-tmpl (list prelinked-tmpl)
                                 `((:func ,<void*> ,ptr)
                                   (:num-fargs ,<uint8> ,num-fargs)
                                   (:init-spill-size
                                    ,<int32>
                                    ,spill-area-bytes)
                                   (:epilogue-spill-size
                                    ,<int32>
                                    ,(+ spill-area-bytes align-pad))
                                   (:align-pad ,<int8> ,align-pad)
                                   (:retkind ,<integer> ,retkind)
                                   (:rettype ,<top> ,rettype)
                                   ,@named
                                   ,@spill-params)
                                 :postamble spill-area-bytes)
                    ((% '%%call-native) 0         ;tstart
                                        0         ;tend (no zero fill)
                                        bytes     ;code
                                        0         ;start
                                        (+ (lbl-off lbs 'spill:)
                                           spill-area-bytes) ;end
                                        (lbl-off lbs entry-label) ;entry
                                        0 0)))  ;win-prolog-end win-frame-size
                (cond [(%iarg-type? (caar args))
                       (if (< icount 6)
                         (loop (cdr args) (+ icount 1) fcount scount
                               (cons `(,(~ '#(:iarg0 :iarg1 :iarg2
                                             :iarg3 :iarg4 :iarg5)
                                           icount)
                                       ,@(car args))
                                     named)
                               spill-params)
                         (loop (cdr args) (+ icount 1) fcount (+ scount 1)
                               named
                               (cons `(:spill ,@(car args)
                                       ,(* (- num-spills scount 1) 8))
                                     spill-params)))]
                      [(%farg-type? (caar args))
                       (if (< fcount 8)
                         (let ([fkey (~ '#(:farg0 :farg1 :farg2 :farg3
                                           :farg4 :farg5 :farg6 :farg7)
                                        fcount)]
                               [vkey (~ '#(:farg0-variant :farg1-variant
                                           :farg2-variant :farg3-variant
                                           :farg4-variant :farg5-variant
                                           :farg6-variant :farg7-variant)
                                        fcount)])
                           (loop (cdr args) icount (+ fcount 1) scount
                                 (if (eq? (caar args) <float>)
                                   (list* `(,vkey ,<uint8> movss)
                                          `(,fkey ,@(car args)) named)
                                   (cons `(,fkey ,@(car args)) named))
                                 spill-params))
                         (loop (cdr args) icount (+ fcount 1) (+ scount 1)
                               named
                               (cons `(:spill ,@(car args)
                                       ,(* (- num-spills scount 1) 8))
                                     spill-params)))]
                      [else (error "bad arg entry:" (car args))]))))))))

  ;; Source-slot index for arg i.  Float args index into fp_save
  ;; (xmm0..xmm7), other args into int_save (rdi..r9).  Returns
  ;; (values per-arg-source-list num-int-used num-fp-used).
  (Ps
   `(define (%asm-argsrcs arg-canons)
      (let loop ([as arg-canons] [ii 0] [fi 0] [acc '()])
        (cond [(null? as)
               (values (reverse acc) ii fi)]
              [(or (eq? (car as) <double>) (eq? (car as) <float>))
               (loop (cdr as) ii (+ fi 1) (cons fi acc))]
              [else
               (loop (cdr as) (+ ii 1) fi (cons ii acc))])))

   ;; Pointer arg type for kind=8; SCM_FALSE for everything else.
   `(define (%asm-argtype type)
      (if (or (is-a? type <c-pointer>)
              (is-a? type <c-array>)
              (is-a? type <c-function>))
        type
        #f)))

  ;; assemble-callback-amd64
  ;;
  ;;   (assemble-callback-amd64 proc arg-canons rettype)
  ;;     →  values code-bytes entry-offset win-prolog-end win-frame-size
  ;;
  ;; arg-canons is a list of canonical native types (per
  ;; native-type->call-canon in gauche.ffi.native).  rettype is the
  ;; canonical native type the trampoline should return to its C
  ;; caller; the trampoline unboxes the Scheme procedure's value
  ;; according to %asm-boxkind.  The result is a u8vector / entry /
  ;; unwind-info tuple ready to feed to %%install-ffi-callback-one or
  ;; one slot of %%install-ffi-callback-context.  win-prolog-end and
  ;; win-frame-size stay 0 on Linux; the winx64 path will populate
  ;; them.
  ;;
  ;; The trampoline currently supports up to 6 integer + 8 float
  ;; register-resident args; spilled args are not yet handled.
  (Ps
   `(define assemble-callback-amd64
      (let ([% (%%make-bootstrap-function-table '(%%get-entry-address))]
            [link-tmpl #f] [lbl-off #f] [prelinked-tmpl #f])
        (define (init!)
          (set! link-tmpl (module-binding-ref 'lang.asm.linker 'link-templates))
          (set! lbl-off   (module-binding-ref 'lang.asm.linker 'linked-label-offset))
          (let ([prelink (module-binding-ref 'lang.asm.linker 'prelink-template)]
                [gea     (% '%%get-entry-address)])
            ;; prelink runs the patcher, which calls native-ptr-fill!
            ;; for <top>-typed patches; wrap.
            (parameterize ([(with-module gauche.typeutil
                              native-ptr-fill-enabled?) #t])
              (set! prelinked-tmpl
                    (prelink (amd64-callback-tmpl)
                             `((:Scm__FFINativeCallCallback ,<intptr_t>
                                ,(gea "_Scm__FFINativeCallCallback"))
                               (:Scm_Cons                   ,<intptr_t>
                                ,(gea "_Scm_Cons"))
                               (:Scm_MakeFlonum             ,<intptr_t>
                                ,(gea "_Scm_MakeFlonum"))
                               (:Scm_MakeString             ,<intptr_t>
                                ,(gea "_Scm_MakeString"))
                               (:Scm_MakeNativeHandleSimple ,<intptr_t>
                                ,(gea "_Scm_MakeNativeHandleSimple"))
                               (:Scm_IntptrToInteger        ,<intptr_t>
                                ,(gea "_Scm_IntptrToInteger"))
                               (:Scm_UintptrToInteger       ,<intptr_t>
                                ,(gea "_Scm_UintptrToInteger"))
                               (:Scm_GetIntegerClamp        ,<intptr_t>
                                ,(gea "_Scm_GetIntegerClamp"))
                               (:Scm_GetIntegerUClamp       ,<intptr_t>
                                ,(gea "_Scm_GetIntegerUClamp"))
                               (:Scm_GetDouble              ,<intptr_t>
                                ,(gea "_Scm_GetDouble"))
                               (:Scm_GetStringConst         ,<intptr_t>
                                ,(gea "_Scm_GetStringConst"))
                               (:Scm_NativeHandlePtr        ,<intptr_t>
                                ,(gea "_Scm_NativeHandlePtr"))
                               (:SCM_STRING_COPYING         ,<int32>
                                ,SCM_STRING_COPYING)
                               (:SCM_NIL                    ,<top>
                                ,'())))))))
        (define MAX-ARGS 8)
        (^[proc arg-canons rettype]
          (when (not link-tmpl) (init!))
          (let* ([nargs (length arg-canons)])
            (when (> nargs MAX-ARGS)
              (errorf "FFI native callback: more than ~a register-resident \
                       args is not supported yet (got ~a)"
                      MAX-ARGS nargs))
            (receive (srcs num-int num-fp) (%asm-argsrcs arg-canons)
              (when (or (> num-int 6) (> num-fp 8))
                (errorf "FFI native callback: spilled args not yet supported \
                         (~a int / ~a fp)" num-int num-fp))
              (let* ([kinds (map %asm-boxkind arg-canons)]
                     ;; <void> is return-only; reject early so the
                     ;; trampoline never sees an unmapped kind.
                     [_ (when (memv 6 kinds)
                          (error "FFI native callback: <void> can't \
                                   appear as an argument type"))]
                     [types (map %asm-argtype arg-canons)]
                     [pad   (^[lst filler]
                              (append lst
                                      (make-list (- MAX-ARGS (length lst))
                                                 filler)))]
                     [k8    (pad kinds 0)]
                     [s8    (pad srcs  0)]
                     [t8    (pad types #f)]
                     [arg-params
                      (append
                       (map (^[i k] `(,(make-keyword
                                        (format "argkind~a" i))
                                      ,<int8> ,k))
                            (iota MAX-ARGS) k8)
                       (map (^[i s] `(,(make-keyword
                                        (format "argsrc~a" i))
                                      ,<int8> ,s))
                            (iota MAX-ARGS) s8)
                       (map (^[i t] `(,(make-keyword
                                        (format "argtype~a" i))
                                      ,<top> ,t))
                            (iota MAX-ARGS) t8))])
                (parameterize ([(with-module gauche.typeutil
                                  native-ptr-fill-enabled?) #t])
                  (receive [bytes lbs]
                      (link-tmpl (list prelinked-tmpl)
                                 `((:proc    ,<top>  ,proc)
                                   (:nargs   ,<int8> ,nargs)
                                   (:retkind ,<int8> ,(%asm-boxkind rettype))
                                   ,@arg-params))
                    (values bytes
                            (lbl-off lbs 'entry:)
                            0       ; win-prolog-end (winx64 will populate)
                            0)))))))))) ; win-frame-size
  )

(define (gen-stub-winx64 port)
  (define (Ps . exprs)
    (for-each (cut pprint <> :port port) exprs))

  (display ";; Register-only calling\n" port)
  (dump-asm-fragment winx64-call-reg port)

  (display ";; Spill-to-stack case\n" port)
  (dump-asm-fragment winx64-call-spill port)

  ;; (call-winx64 <native-handle> args rettype)
  ;;  args : ((type value) ...)
  (Ps
   `(define call-winx64
      (^[ptr args rettype]
        ;; Windows x64 ABI uses shared argument count---even though
        ;; integer args and flonum args use different registers, each args
        ;; up to 4 consumes the shared slot count, and either one of
        ;; the registers is used depending on the argument type.
        ;; We still need to count fargs to determine the entry address.
        (let* ([num-args (length args)]
               [num-fargs (count (^p (%farg-type? (car p))) args)]
               [num-spills (max 0 (- num-args 4))])
          (if (zero? num-spills)
            (call-winx64-regs ptr args num-args num-fargs rettype)
            (call-winx64-spill ptr args num-args num-fargs num-spills rettype))))))

  (Ps
   `(define call-winx64-regs
      (let ([% (%%make-bootstrap-function-table '(%%call-native
                                                  %%get-entry-address))]
            [link-tmpl #f] [lbl-off #f] [prelinked-tmpl #f])
        (define (init!)
          (set! link-tmpl (module-binding-ref 'lang.asm.linker 'link-templates))
          (set! lbl-off   (module-binding-ref 'lang.asm.linker 'linked-label-offset))
          (let ([prelink (module-binding-ref 'lang.asm.linker 'prelink-template)]
                [gea     (% '%%get-entry-address)])
            (set! prelinked-tmpl
                  (prelink (winx64-call-reg-tmpl)
                           `((:Scm_MakeFlonum             ,<intptr_t>
                              ,(gea "_Scm_MakeFlonum"))
                             (:Scm_MakeString             ,<intptr_t>
                              ,(gea "_Scm_MakeString"))
                             (:Scm_MakeNativeHandleSimple ,<intptr_t>
                              ,(gea "_Scm_MakeNativeHandleSimple"))
                             (:Scm_IntptrToInteger        ,<intptr_t>
                              ,(gea "_Scm_IntptrToInteger"))
                             (:Scm_UintptrToInteger       ,<intptr_t>
                              ,(gea "_Scm_UintptrToInteger"))
                             (:SCM_STRING_COPYING         ,<int32>
                              ,SCM_STRING_COPYING)
                             (:SCM_UNDEFINED              ,<top>
                              ,(undefined)))))))
        (^[ptr args num-args num-fargs rettype]
          (when (not link-tmpl) (init!))
          (let* (;; for effective-nargs calculation, we need to consider
                 ;; unused xmm regs for preceding integral args.
                 ;; e.g. if args are int, int, double, we need up to entry4f2
                 ;; even we only have 1 fargs.
                 [effective-nargs (if (zero? num-fargs)
                                    num-args
                                    (+ 4 num-args))]
                 [entry-label (~ '#(entry0: entry1: entry2: entry3: entry4:
                                    entry4f0: entry4f1: entry4f2: entry4f3:)
                                 effective-nargs)]
                 [retkind (%asm-boxkind rettype)]
                 [params
                  (let loop ([args args] [count 0] [r '()])
                    (cond [(null? args) r]
                          [(%iarg-type? (caar args))
                           (loop (cdr args) (+ count 1)
                                 (cons `(,(~ '#(:iarg0 :iarg1 :iarg2 :iarg3)
                                             count)
                                         ,@(car args))
                                       r))]
                          [(%farg-type? (caar args))
                           ;; We load both integer regs and flonum regs.
                           ;; It matters for variadic function call.
                           (let ([fkey (~ '#(:farg0 :farg1 :farg2 :farg3) count)]
                                 [ikey (~ '#(:iarg0 :iarg1 :iarg2 :iarg3) count)]
                                 [vkey (~ '#(:farg0-variant :farg1-variant
                                             :farg2-variant :farg3-variant)
                                          count)])
                             (loop (cdr args) (+ count 1)
                                   (if (eq? (caar args) <float>)
                                     (list* `(,vkey ,<uint8> movss)
                                            `(,fkey ,@(car args))
                                            `(,ikey ,@(car args)) r)
                                     (list* `(,fkey ,@(car args))
                                            `(,ikey ,@(car args)) r))))]
                          [else (error "bad arg entry:" (car args))]))])
            (receive [bytes lbs]
                (link-tmpl (list prelinked-tmpl)
                           (list* `(:func ,<void*> ,ptr)
                                  `(:retkind ,<integer> ,retkind)
                                  `(:rettype ,<top> ,rettype)
                                  params))
              ;; win-frame-size=40: push %rbx (8) + shadow space (32)
              ;; Prolog ends after "push %rbx" (1 byte) + "addq -32 %rsp" (4 bytes)
              ((% '%%call-native) 0 0 bytes 0
                                  (lbl-off lbs 'end:)
                                  (lbl-off lbs entry-label)
                                  (+ (lbl-off lbs 'entry0:) 5) 40)))))))

  (Ps
   `(define call-winx64-spill
      (let ([% (%%make-bootstrap-function-table '(%%call-native
                                                  %%get-entry-address))]
            [link-tmpl #f] [lbl-off #f] [prelinked-tmpl #f])
        (define (init!)
          (set! link-tmpl (module-binding-ref 'lang.asm.linker 'link-templates))
          (set! lbl-off   (module-binding-ref 'lang.asm.linker 'linked-label-offset))
          (let ([prelink (module-binding-ref 'lang.asm.linker 'prelink-template)]
                [gea     (% '%%get-entry-address)])
            (set! prelinked-tmpl
                  (prelink (winx64-call-spill-tmpl)
                           `((:Scm_MakeFlonum             ,<intptr_t>
                              ,(gea "_Scm_MakeFlonum"))
                             (:Scm_MakeString             ,<intptr_t>
                              ,(gea "_Scm_MakeString"))
                             (:Scm_MakeNativeHandleSimple ,<intptr_t>
                              ,(gea "_Scm_MakeNativeHandleSimple"))
                             (:Scm_IntptrToInteger        ,<intptr_t>
                              ,(gea "_Scm_IntptrToInteger"))
                             (:Scm_UintptrToInteger       ,<intptr_t>
                              ,(gea "_Scm_UintptrToInteger"))
                             (:SCM_STRING_COPYING         ,<int32>
                              ,SCM_STRING_COPYING)
                             (:SCM_UNDEFINED              ,<top>
                              ,(undefined)))))))
        (^[ptr args num-args num-fargs num-spills rettype]
          (when (not link-tmpl) (init!))
          (let loop ([args args] [count 0] [scount 0] [named '()] [spill-params '()])
            (if (null? args)
              ;; With push %rbx at entry0, alignment parity is flipped.
              ;; We need rsp%16==0 before "call func:", which requires
              ;; align-pad=(if (even? num-spills) 0 8).
              (let* ([align-pad (if (even? num-spills) 0 8)]
                     [spill-area-bytes (* 8 num-spills)]
                     [retkind (%asm-boxkind rettype)])
                (receive [bytes lbs]
                    (link-tmpl (list prelinked-tmpl)
                               `((:func ,<void*> ,ptr)
                                 (:init-spill-size
                                  ,<int32>
                                  ,spill-area-bytes)
                                 (:epilogue-spill-size
                                  ,<int32>
                                  ,(+ spill-area-bytes align-pad))
                                 (:align-pad ,<int8> ,align-pad)
                                 (:retkind ,<integer> ,retkind)
                                 (:rettype ,<top> ,rettype)
                                 ,@named
                                 ,@spill-params)
                               :postamble spill-area-bytes)
                  ;; win-frame-size = push %rbx (8) + shadow (32) + spill+pad
                  ;; Prolog ends after "addq -32 %rsp" (4) at entry0:
                  ;; (push %rbx is now in init:, before the spill loop)
                  ((% '%%call-native) 0      ;tstart
                                      0      ;tend (no zero fill needed)
                                      bytes  ;code
                                      0      ;start
                                      (+ (lbl-off lbs 'spill:)
                                         spill-area-bytes)  ;end
                                      (lbl-off lbs 'entry:) ;entry
                                      (+ (lbl-off lbs 'entry0:) 4)
                                      (+ spill-area-bytes align-pad 40))))
              (cond [(%iarg-type? (caar args))
                     (if (< count 4)
                       (loop (cdr args) (+ count 1) scount
                             (cons `(,(~ '#(:iarg0 :iarg1 :iarg2 :iarg3) count)
                                     ,@(car args))
                                   named)
                             spill-params)
                       (loop (cdr args) (+ count 1) (+ scount 1)
                             named
                             (cons `(:spill ,@(car args)
                                     ,(* (- num-spills scount 1) 8))
                                   spill-params)))]
                    [(%farg-type? (caar args))
                     ;; We load both integer regs and flonum regs.
                     ;; It matters for variadic function call.
                     (if (< count 4)
                       (let ([fkey (~ '#(:farg0 :farg1 :farg2 :farg3) count)]
                             [ikey (~ '#(:iarg0 :iarg1 :iarg2 :iarg3) count)]
                             [vkey (~ '#(:farg0-variant :farg1-variant
                                         :farg2-variant :farg3-variant) count)])
                         (loop (cdr args) (+ count 1) scount
                               (if (eq? (caar args) <float>)
                                 (list* `(,vkey ,<uint8> movss)
                                        `(,fkey ,@(car args))
                                        `(,ikey ,@(car args)) named)
                                 (list* `(,fkey ,@(car args))
                                        `(,ikey ,@(car args)) named))
                               spill-params))
                       (loop (cdr args) (+ count 1) (+ scount 1)
                             named
                             (cons `(:spill ,@(car args)
                                     ,(* (- num-spills scount 1) 8))
                                   spill-params)))]
                    [else (error "bad arg entry:" (car args))])))))))
  )

;;;
;;; gosh ./gen-native.scm <dir>
;;;
(define (main args)
  (match (cdr args)
    [(dir) (call-with-temporary-file
            (^[port tmpname]
              (emit-header port)
              (gen-stub-amd64 port)
              (gen-stub-winx64 port)
              (close-output-port port)
              (sys-rename tmpname #"~|dir|/native-supp.scm"))
            :directory dir :prefix "native-supp.scm")]
    [else  (exit 1 "Usage: gosh ./gen-native.scm <directory>")])
  0)
