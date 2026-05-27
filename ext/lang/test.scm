(use gauche.test)
(use gauche.uvector)
(use lang.asm.linker)

(test-start "lang.asm.x86_64")

(use lang.asm.x86_64)
(test-module 'lang.asm.x86_64)

;;----------------------------------------------------------------------
(test-section "instructions")


;; Helper: assemble and return (bytes . label-alist) for comparison.
;; Label alist is sorted by address for deterministic comparison.
;; bytes is compared as a list for readability.
(define (t-asm name insns expected-bytes expected-labels)
  (test* name
         (cons expected-bytes expected-labels)
         (receive (bytes labels) (link-templates (list (x86_64-asm insns)) '())
           (cons (u8vector->list bytes)
                 (obj-template-labels->alist labels)))))

;; --- Trivial single-byte instructions ---

(t-asm "ret"     '((ret))     '(#xc3) '())
(t-asm "int3"    '((int3))    '(#xcc) '())
(t-asm "endbr64" '((endbr64)) '(#xf3 #x0f #x1e #xfa) '())

;; --- push / pop (low registers: no REX prefix) ---

(t-asm "push %rax" '((push %rax)) '(#x50) '())
(t-asm "push %rcx" '((push %rcx)) '(#x51) '())
(t-asm "push %rbp" '((push %rbp)) '(#x55) '())
(t-asm "push %rdi" '((push %rdi)) '(#x57) '())
(t-asm "pop  %rbp" '((pop  %rbp)) '(#x5d) '())
(t-asm "pop  %rbx" '((pop  %rbx)) '(#x5b) '())

;; --- push / pop (extended registers %r8-%r15: need REX.B = #x41) ---

(t-asm "push %r8"  '((push %r8))  '(#x41 #x50) '())
(t-asm "push %r15" '((push %r15)) '(#x41 #x57) '())
(t-asm "pop  %r12" '((pop  %r12)) '(#x41 #x5c) '())
(t-asm "pop  %r13" '((pop  %r13)) '(#x41 #x5d) '())

;; --- call / ret ---

;; call via register: FF /2 ModRM(11 010 reg)
(t-asm "call %rax" '((call %rax)) '(#xff #xd0) '())
(t-asm "call %rdx" '((call %rdx)) '(#xff #xd2) '())
;; call extended register: 41 FF /2
(t-asm "call %r9"  '((call %r9))  '(#x41 #xff #xd1) '())

;; --- movb (8-bit moves) ---

;; movb reg8→reg8: 88 /r
(t-asm "movb %al,%cl"  '((movb %al %cl))  '(#x88 #xc1) '())
(t-asm "movb %dl,%bl"  '((movb %dl %bl))  '(#x88 #xd3) '())

;; movb mem→reg8: 8A /r
(t-asm "movb (%rax),%dl" '((movb (%rax) %dl)) '(#x8a #x10) '())

;; movb imm8→reg8: B0+rd ib
(t-asm "movb $42,%bl"  '((movb 42 %bl))  '(#xb3 #x2a) '())
(t-asm "movb $0,%al"   '((movb 0 %al))   '(#xb0 #x00) '())

;; movb imm8→mem: C6 /0 ib
(t-asm "movb $42,(%rax)" '((movb 42 (%rax))) '(#xc6 #x00 #x2a) '())

;; --- movq (64-bit moves) ---

;; movq reg→reg: REX.W 89 /r  ModRM(11 src dst)
(t-asm "movq %rax,%rcx" '((movq %rax %rcx)) '(#x48 #x89 #xc1) '())
(t-asm "movq %rsp,%rbp" '((movq %rsp %rbp)) '(#x48 #x89 #xe5) '())
(t-asm "movq %rax,%rax" '((movq %rax %rax)) '(#x48 #x89 #xc0) '())

;; movq extended registers: REX.W + REX.R/B as needed
;; movq %r9,%r11: 4D 89 CB  (REX.W+R+B, 89, ModRM 11_001_011)
(t-asm "movq %r9,%r11"  '((movq %r9  %r11)) '(#x4d #x89 #xcb) '())
;; movq %rax,%r10: 49 89 C2  (REX.W+B, 89, ModRM 11_000_010)
(t-asm "movq %rax,%r10" '((movq %rax %r10)) '(#x49 #x89 #xc2) '())
;; movq %r8,%rcx: 4C 89 C1  (REX.W+R, 89, ModRM 11_000_001)
(t-asm "movq %r8,%rcx"  '((movq %r8  %rcx)) '(#x4c #x89 #xc1) '())

;; movq reg→mem: REX.W 89 /r
(t-asm "movq %rax,(%rax)"       '((movq %rax (%rax)))      '(#x48 #x89 #x00) '())
;; movq reg→mem with disp8 and non-SIB base: 48 89 43 10
(t-asm "movq %rax,16(%rbx)"     '((movq %rax (16 %rbx)))   '(#x48 #x89 #x43 #x10) '())
;; movq reg→neg-disp8(%rbp): 48 89 45 F8
(t-asm "movq %rax,-8(%rbp)"     '((movq %rax (-8 %rbp)))   '(#x48 #x89 #x45 #xf8) '())

;; movq mem→reg: REX.W 8B /r
(t-asm "movq (%rax),%rcx"       '((movq (%rax) %rcx))      '(#x48 #x8b #x08) '())
;; movq base+disp8 → reg: 48 8B 45 08
(t-asm "movq 8(%rbp),%rax"      '((movq (8 %rbp) %rax))    '(#x48 #x8b #x45 #x08) '())
;; movq (%rsp) needs SIB: 48 8B 04 24
(t-asm "movq (%rsp),%rax"       '((movq (%rsp) %rax))      '(#x48 #x8b #x04 #x24) '())
;; movq (%rbp) needs disp8=0: 48 8B 45 00
(t-asm "movq (%rbp),%rax"       '((movq (%rbp) %rax))      '(#x48 #x8b #x45 #x00) '())
;; movq with extended base: 4D 89 01  (movq %r8,(%r9))
(t-asm "movq %r8,(%r9)"         '((movq %r8 (%r9)))         '(#x4d #x89 #x01) '())

;; movq imm8→reg: REX.W C7 /0 id  (sign-extended 32-bit immediate)
(t-asm "movq $42,%rax"  '((movq 42 %rax))  '(#x48 #xc7 #xc0 #x2a #x00 #x00 #x00) '())
(t-asm "movq $0,%rbx"   '((movq 0 %rbx))   '(#x48 #xc7 #xc3 #x00 #x00 #x00 #x00) '())

;; movq imm32→reg: REX.W C7 /0 id
(t-asm "movq $1000,%rbx" '((movq 1000 %rbx))
       '(#x48 #xc7 #xc3 #xe8 #x03 #x00 #x00) '())

;; movq imm64→reg: REX.W B8+rd iq
(t-asm "movq $imm64,%rax"
       '((movq (imm64 #x0102030405060708) %rax))
       '(#x48 #xb8 #x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01) '())
;; movq imm64 to extended reg: REX.W+B B8+2 iq  (%r10)
(t-asm "movq $imm64,%r10"
       '((movq (imm64 #x0102030405060708) %r10))
       '(#x49 #xba #x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01) '())

;; movzbq / movzwq (zero-extend): REX.W 0F B6/B7 /r
(t-asm "movzbq (%rsi),%rdi" '((movzbq (%rsi) %rdi)) '(#x48 #x0f #xb6 #x3e) '())
(t-asm "movzwq (%rax),%rcx" '((movzwq (%rax) %rcx)) '(#x48 #x0f #xb7 #x08) '())

;; leaq: REX.W 8D /r
(t-asm "leaq 8(%rbx),%rax"  '((leaq (8 %rbx) %rax))  '(#x48 #x8d #x43 #x08) '())
(t-asm "leaq -16(%rbp),%rsi" '((leaq (-16 %rbp) %rsi)) '(#x48 #x8d #x75 #xf0) '())

;; --- SIB addressing ---

;; movq (%rax,%rcx,4),%rdx: 48 8B 14 88
(t-asm "movq (%rax,%rcx,4),%rdx"
       '((movq (%rax %rcx 4) %rdx)) '(#x48 #x8b #x14 #x88) '())
;; movq 8(%rax,%rcx,4),%rdx: 48 8B 54 88 08
(t-asm "movq 8(%rax,%rcx,4),%rdx"
       '((movq (8 %rax %rcx 4) %rdx)) '(#x48 #x8b #x54 #x88 #x08) '())

;; --- ALU instructions (addq/subq/andq/orq/xorq/cmpq) ---

;; addq reg→reg: REX.W 01 /r  ModRM(11 src dst)
(t-asm "addq %rcx,%rax" '((addq %rcx %rax)) '(#x48 #x01 #xc8) '())
;; addq imm8→reg: REX.W 83 /0 ib
(t-asm "addq $100,%rbx" '((addq 100 %rbx)) '(#x48 #x83 #xc3 #x64) '())
;; addq imm32→%rax uses short form: REX.W 05 id
(t-asm "addq $1000,%rax" '((addq 1000 %rax)) '(#x48 #x05 #xe8 #x03 #x00 #x00) '())
;; addq imm32→non-rax: REX.W 81 /0 id
(t-asm "addq $1000,%rbx" '((addq 1000 %rbx)) '(#x48 #x81 #xc3 #xe8 #x03 #x00 #x00) '())

;; subq reg→reg: REX.W 29 /r
(t-asm "subq %rcx,%rax"  '((subq %rcx %rax))  '(#x48 #x29 #xc8) '())
;; subq imm8→reg: REX.W 83 /5 ib
(t-asm "subq $16,%rsp"   '((subq 16 %rsp))    '(#x48 #x83 #xec #x10) '())
;; subq reg→mem (%rsp via SIB): REX.W 29 /r
(t-asm "subq %rcx,(%rsp)" '((subq %rcx (%rsp))) '(#x48 #x29 #x0c #x24) '())

;; orq: REX.W 09 /r
(t-asm "orq %rdx,%rsi"   '((orq %rdx %rsi))   '(#x48 #x09 #xd6) '())

;; andq with extended src: REX.W+R 21 /r
(t-asm "andq %r8,%rcx"   '((andq %r8 %rcx))   '(#x4c #x21 #xc1) '())

;; xorq reg→reg: REX.W 31 /r  (canonical zero-register idiom)
(t-asm "xorq %rax,%rax"  '((xorq %rax %rax))  '(#x48 #x31 #xc0) '())

;; cmpq imm8→reg: REX.W 83 /7 ib
(t-asm "cmpq $0,%rax"    '((cmpq 0 %rax))     '(#x48 #x83 #xf8 #x00) '())
;; cmpq imm32→%rax short form: REX.W 3D id
(t-asm "cmpq $256,%rax"  '((cmpq 256 %rax))   '(#x48 #x3d #x00 #x01 #x00 #x00) '())
;; cmpq reg→reg: REX.W 39 /r
(t-asm "cmpq %rbx,%rcx"  '((cmpq %rbx %rcx))  '(#x48 #x39 #xd9) '())

;; --- ALU instructions l/w/b variants ---

;; addl reg→reg: 01 /r  ModRM(11 src dst)
(t-asm "addl %ecx,%eax"  '((addl %ecx %eax))  '(#x01 #xc8) '())
;; addl imm8→reg: 83 /0 ib
(t-asm "addl $100,%ebx"  '((addl 100 %ebx))   '(#x83 #xc3 #x64) '())
;; addl imm32→%eax short form: 05 id
(t-asm "addl $1000,%eax" '((addl 1000 %eax))  '(#x05 #xe8 #x03 #x00 #x00) '())
;; addl imm32→non-eax: 81 /0 id
(t-asm "addl $1000,%ebx" '((addl 1000 %ebx))  '(#x81 #xc3 #xe8 #x03 #x00 #x00) '())
;; subl reg→reg: 29 /r
(t-asm "subl %ecx,%eax"  '((subl %ecx %eax))  '(#x29 #xc8) '())
;; xorl reg→reg: 31 /r
(t-asm "xorl %eax,%eax"  '((xorl %eax %eax))  '(#x31 #xc0) '())
;; cmpl imm8→reg: 83 /7 ib
(t-asm "cmpl $0,%eax"    '((cmpl 0 %eax))     '(#x83 #xf8 #x00) '())
;; cmpl imm32→%eax short form: 3D id
(t-asm "cmpl $256,%eax"  '((cmpl 256 %eax))   '(#x3d #x00 #x01 #x00 #x00) '())
;; cmpl reg→reg: 39 /r
(t-asm "cmpl %ebx,%ecx"  '((cmpl %ebx %ecx))  '(#x39 #xd9) '())

;; addw reg→reg: 66 01 /r
(t-asm "addw %cx,%ax"    '((addw %cx %ax))    '(#x66 #x01 #xc8) '())
;; addw imm8→reg: 66 83 /0 ib
(t-asm "addw $100,%bx"   '((addw 100 %bx))    '(#x66 #x83 #xc3 #x64) '())
;; addw imm16→%ax short form: 66 05 iw
(t-asm "addw $1000,%ax"  '((addw 1000 %ax))   '(#x66 #x05 #xe8 #x03) '())
;; addw imm16→non-ax: 66 81 /0 iw
(t-asm "addw $1000,%bx"  '((addw 1000 %bx))   '(#x66 #x81 #xc3 #xe8 #x03) '())
;; subw reg→reg: 66 29 /r
(t-asm "subw %cx,%ax"    '((subw %cx %ax))    '(#x66 #x29 #xc8) '())
;; cmpw imm8→reg: 66 83 /7 ib
(t-asm "cmpw $0,%ax"     '((cmpw 0 %ax))      '(#x66 #x83 #xf8 #x00) '())
;; cmpw imm16→%ax short form: 66 3D iw
(t-asm "cmpw $256,%ax"   '((cmpw 256 %ax))    '(#x66 #x3d #x00 #x01) '())
;; cmpw reg→reg: 66 39 /r
(t-asm "cmpw %bx,%cx"    '((cmpw %bx %cx))    '(#x66 #x39 #xd9) '())

;; addb reg→reg: 00 /r  ModRM(11 src dst)
(t-asm "addb %cl,%al"    '((addb %cl %al))    '(#x00 #xc8) '())
;; addb imm8→%al short form: 04 ib
(t-asm "addb $1,%al"     '((addb 1 %al))      '(#x04 #x01) '())
;; addb imm8→non-al: 80 /0 ib
(t-asm "addb $100,%bl"   '((addb 100 %bl))    '(#x80 #xc3 #x64) '())
;; subb reg→reg: 28 /r
(t-asm "subb %cl,%al"    '((subb %cl %al))    '(#x28 #xc8) '())
;; cmpb imm8→%al short form: 3C ib
(t-asm "cmpb $0,%al"     '((cmpb 0 %al))      '(#x3c #x00) '())
;; cmpb imm8→non-al: 80 /7 ib
(t-asm "cmpb $1,%bl"     '((cmpb 1 %bl))      '(#x80 #xfb #x01) '())
;; cmpb reg→reg: 38 /r
(t-asm "cmpb %bl,%cl"    '((cmpb %bl %cl))    '(#x38 #xd9) '())

;; --- Shift / rotate ---

;; shl by-1 form: REX.W D1 /4
(t-asm "shl $1,%rax"  '((shl 1 %rax))  '(#x48 #xd1 #xe0) '())
;; shl by-imm8 form: REX.W C1 /4 ib
(t-asm "shl $2,%rcx"  '((shl 2 %rcx))  '(#x48 #xc1 #xe1 #x02) '())
;; shr by-1: REX.W D1 /5
(t-asm "shr $1,%rax"  '((shr 1 %rax))  '(#x48 #xd1 #xe8) '())
;; sar by-imm8: REX.W C1 /7 ib
(t-asm "sar $3,%rdx"  '((sar 3 %rdx))  '(#x48 #xc1 #xfa #x03) '())
;; rol by-imm8: REX.W C1 /0 ib
(t-asm "rol $3,%rax"  '((rol 3 %rax))  '(#x48 #xc1 #xc0 #x03) '())
;; ror by-1: REX.W D1 /1
(t-asm "ror $1,%rcx"  '((ror 1 %rcx))  '(#x48 #xd1 #xc9) '())

;; --- incq / decq ---

;; incq: REX.W FF /0  ModRM(11 000 reg)
(t-asm "incq %rax"   '((incq %rax))   '(#x48 #xff #xc0) '())
(t-asm "incq %rdx"   '((incq %rdx))   '(#x48 #xff #xc2) '())
;; decq: REX.W FF /1  ModRM(11 001 reg)
(t-asm "decq %rax"   '((decq %rax))   '(#x48 #xff #xc8) '())
(t-asm "decq %rcx"   '((decq %rcx))   '(#x48 #xff #xc9) '())
;; incq mem: REX.W FF /0 with memory operand
(t-asm "incq (%rdi)" '((incq (%rdi))) '(#x48 #xff #x07) '())

;; --- incl / decl ---

;; incl: FF /0  ModRM(11 000 reg)
(t-asm "incl %eax"   '((incl %eax))   '(#xff #xc0) '())
(t-asm "incl %edx"   '((incl %edx))   '(#xff #xc2) '())
;; decl: FF /1  ModRM(11 001 reg)
(t-asm "decl %eax"   '((decl %eax))   '(#xff #xc8) '())
(t-asm "decl %ecx"   '((decl %ecx))   '(#xff #xc9) '())
;; incl/decl mem: FF /0 or /1 with memory operand
(t-asm "incl (%rdi)" '((incl (%rdi))) '(#xff #x07) '())
(t-asm "decl (%rdi)" '((decl (%rdi))) '(#xff #x0f) '())

;; --- incw / decw ---

;; incw: 66 FF /0  ModRM(11 000 reg)
(t-asm "incw %ax"    '((incw %ax))    '(#x66 #xff #xc0) '())
(t-asm "incw %dx"    '((incw %dx))    '(#x66 #xff #xc2) '())
;; decw: 66 FF /1  ModRM(11 001 reg)
(t-asm "decw %ax"    '((decw %ax))    '(#x66 #xff #xc8) '())
(t-asm "decw %cx"    '((decw %cx))    '(#x66 #xff #xc9) '())
;; incw/decw mem: 66 FF /0 or /1 with memory operand
(t-asm "incw (%rdi)" '((incw (%rdi))) '(#x66 #xff #x07) '())
(t-asm "decw (%rdi)" '((decw (%rdi))) '(#x66 #xff #x0f) '())

;; --- incb / decb ---

;; incb: FE /0  ModRM(11 000 reg)
(t-asm "incb %al"    '((incb %al))    '(#xfe #xc0) '())
(t-asm "incb %dl"    '((incb %dl))    '(#xfe #xc2) '())
;; decb: FE /1  ModRM(11 001 reg)
(t-asm "decb %al"    '((decb %al))    '(#xfe #xc8) '())
(t-asm "decb %cl"    '((decb %cl))    '(#xfe #xc9) '())
;; incb/decb mem: FE /0 or /1 with memory operand
(t-asm "incb (%rdi)" '((incb (%rdi))) '(#xfe #x07) '())
(t-asm "decb (%rdi)" '((decb (%rdi))) '(#xfe #x0f) '())

;; --- FP conversion ---

;; cvtss2sd xmm->xmm: F3 0F 5A /r  ModRM(11 dst src)
(t-asm "cvtss2sd %xmm0,%xmm1" '((cvtss2sd %xmm0 %xmm1)) '(#xf3 #x0f #x5a #xc8) '())
(t-asm "cvtss2sd %xmm2,%xmm0" '((cvtss2sd %xmm2 %xmm0)) '(#xf3 #x0f #x5a #xc2) '())

;; cvtss2sd mem->xmm: F3 0F 5A /r  ModRM(00 dst 100) SIB(00 100 101) disp32
(t-asm "cvtss2sd (%rax),%xmm0"
       '((cvtss2sd (%rax) %xmm0))
       '(#xf3 #x0f #x5a #x00) '())
(t-asm "cvtss2sd (#x1000),%xmm0"
       '((cvtss2sd (#x1000) %xmm0))
       '(#xf3 #x0f #x5a #x04 #x25 #x00 #x10 #x00 #x00) '())

;; cvtsd2ss xmm->xmm: F2 0F 5A /r  ModRM(11 dst src)
(t-asm "cvtsd2ss %xmm0,%xmm1" '((cvtsd2ss %xmm0 %xmm1)) '(#xf2 #x0f #x5a #xc8) '())
(t-asm "cvtsd2ss %xmm3,%xmm2" '((cvtsd2ss %xmm3 %xmm2)) '(#xf2 #x0f #x5a #xd3) '())

;; cvtsd2ss mem->xmm: F2 0F 5A /r
(t-asm "cvtsd2ss (%rax),%xmm1"
       '((cvtsd2ss (%rax) %xmm1))
       '(#xf2 #x0f #x5a #x08) '())
(t-asm "cvtsd2ss (#x2000),%xmm1"
       '((cvtsd2ss (#x2000) %xmm1))
       '(#xf2 #x0f #x5a #x0c #x25 #x00 #x20 #x00 #x00) '())

;; --- Embedded data directives ---

;; .datab: emit raw bytes
(t-asm ".datab 4 bytes"
       '((.datab (imm8 #xde) (imm8 #xad) (imm8 #xbe) (imm8 #xef)))
       '(#xde #xad #xbe #xef) '())

;; .datal: emit 32-bit little-endian value
(t-asm ".datal"
       '((.datal (imm32 #x12345678)))
       '(#x78 #x56 #x34 #x12) '())

;; .dataq: emit 64-bit little-endian value
(t-asm ".dataq"
       '((.dataq (imm64 #x0102030405060708)))
       '(#x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01) '())

;; --- Labels: backward reference ---

;; A short backward jump: EB rel8 where rel8 = -(size of loop body + 2)
;; layout: loop[0] incq(3) jmp[3](2) end[5]
;; jmp offset = 0 - 5 = -5 = #xfb
(t-asm "backward jmp"
       '(loop:
         (incq %rax)    ; 3 bytes  [addr 0..2]
         (jmp loop:))   ; 2 bytes  [addr 3..4], end addr=5
       '(#x48 #xff #xc0 #xeb #xfb)
       '((loop: . 0)))

;; --- Labels: forward reference ---

;; A short forward conditional jump: 75 rel8
;; layout: jne[0](2) movq[2](3) skip[5]
;; jne offset = 5 - 2 = 3
(t-asm "forward jne"
       '((jne skip:)      ; 2 bytes  [addr 0..1], end addr=2
         (movq %rax %rax) ; 3 bytes  [addr 2..4], end addr=5
         skip:)
       '(#x75 #x03 #x48 #x89 #xc0)
       '((skip: . 5)))

;; --- Labels: long (32-bit) conditional jump ---

;; jel (je long): 0F 84 rel32
;; layout: jel[0](6) movq[6](3) far[9]
;; jel offset = 9 - 6 = 3
(t-asm "forward jel (long je)"
       '((jel far:)       ; 6 bytes  [addr 0..5], end addr=6
         (movq %rax %rax) ; 3 bytes  [addr 6..8], end addr=9
         far:)
       '(#x0f #x84 #x03 #x00 #x00 #x00 #x48 #x89 #xc0)
       '((far: . 9)))

;; --- Labels: long unconditional jump ---

;; jmpl: E9 rel32
;; layout: jmpl[0](5) movq[5](3) far[8]
;; offset = 8 - 5 = 3
(t-asm "forward jmpl (long jmp)"
       '((jmpl far:)      ; 5 bytes  [addr 0..4], end addr=5
         (movq %rax %rax) ; 3 bytes  [addr 5..7], end addr=8
         far:)
       '(#xe9 #x03 #x00 #x00 #x00 #x48 #x89 #xc0)
       '((far: . 8)))

;; --- Labels: multiple labels, backward and forward ---

;; Typical function prologue/loop/epilogue:
;; entry[0] push(1) movq(3) loop[4] decq(3) jne(2) pop[9](1) ret[10](1)
;; jne loop: offset = 4 - 9 = -5 = #xfb
(t-asm "multiple labels"
       '(entry:
         (push %rbp)       ; 1 byte   [addr 0], end=1
         (movq %rsp %rbp)  ; 3 bytes  [addr 1..3], end=4
         loop:
         (decq %rcx)       ; 3 bytes  [addr 4..6], end=7
         (jne loop:)       ; 2 bytes  [addr 7..8], end=9
         (pop %rbp)        ; 1 byte   [addr 9], end=10
         (ret))            ; 1 byte   [addr 10], end=11
       '(#x55 #x48 #x89 #xe5 #x48 #xff #xc9 #x75 #xfb #x5d #xc3)
       '((entry: . 0) (loop: . 4)))

;; --- Labels: RIP-relative memory access ---

;; movq data(%rip),%rax  then the data label immediately follows.
;; movq is 7 bytes [addr 0..6], end addr=7; data label at addr=7.
;; RIP-relative offset = 7 - 7 = 0  → disp32 = 0
(t-asm "rip-relative load"
       '((movq (data: %rip) %rax) ; 7 bytes [addr 0..6], end=7
         data:
         (.dataq (imm64 42)))     ; 8 bytes [addr 7..14]
       '(#x48 #x8b #x05 #x00 #x00 #x00 #x00
         #x2a #x00 #x00 #x00 #x00 #x00 #x00 #x00)
       '((data: . 7)))

;; --- x86_64-asm / link-template round-trip (no placeholders) ---

(test-section "templates")

(let ()
  (define tmpl (x86_64-asm '((movq %rax %rcx))))
  (test* "x86_64-asm type" #t (is-a? tmpl <obj-template>))
  (receive (bytes labels) (link-templates (list tmpl)'())
    (test* "x86_64-asm round-trip bytes"  '(#x48 #x89 #xc1) (u8vector->list bytes))
    (test* "x86_64-asm round-trip labels" '() (obj-template-labels->alist labels))))

(let ()
  (define tmpl (x86_64-asm '(entry:
                             (push %rbp)
                             (movq %rsp %rbp)
                             (ret))))
  (receive (bytes labels) (link-templates (list tmpl)'())
    (test* "x86_64-asm with labels bytes"
           '(#x55 #x48 #x89 #xe5 #xc3)
           (u8vector->list bytes))
    (test* "x86_64-asm with labels alist"
           '((entry: . 0))
           (obj-template-labels->alist labels))))

;; Verify that link-template does not mutate the template's byte vector
;; (same template can be instantiated multiple times).
(let ()
  (define tmpl (x86_64-asm '((ret))))
  (receive (b1 _) (link-templates (list tmpl)'())
    (receive (b2 _) (link-templates (list tmpl)'())
      (test* "link-templates returns fresh vector"
             #f
             (eq? b1 b2)))))

;; --- immediate-value placeholders ---

;; imm64 placeholder: movq (imm64 :val), %rax
;; Encoding: REX.W(48) B8 <8 bytes immediate>  -- 10 bytes total
(let ()
  (define tmpl (x86_64-asm '((movq (imm64 :val) %rax))))
  ;; Default instantiation (zero)
  (receive (b _) (link-templates (list tmpl)'())
    (test* "imm64 placeholder default"
           '(#x48 #xb8 0 0 0 0 0 0 0 0)
           (u8vector->list b)))
  ;; Instantiate with a real 64-bit value
  (receive (b _) (link-templates (list tmpl)`((:val ,<uint64> #x0102030405060708)))
    (test* "imm64 placeholder filled"
           '(#x48 #xb8 #x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01)
           (u8vector->list b)))
  ;; Re-instantiate with a different value to confirm immutability of template
  (receive (b _) (link-templates (list tmpl)`((:val ,<uint64> 1)))
    (test* "imm64 placeholder re-instantiate"
           '(#x48 #xb8 #x01 0 0 0 0 0 0 0)
           (u8vector->list b))))

;; imm64 placeholder into an extended register: movq (imm64 :v), %r10
;; Encoding: REX.W+B(49) BA <8 bytes> -- 10 bytes
(let ()
  (define tmpl (x86_64-asm '((movq (imm64 :v) %r10))))
  (receive (b _) (link-templates (list tmpl)`((:v ,<uint64> #x0102030405060708)))
    (test* "imm64 placeholder %r10"
           '(#x49 #xba #x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01)
           (u8vector->list b))))

;; imm32 placeholder: movq (imm32 :x), %rbx
;; Encoding: REX.W(48) C7 /0 ModRM(C3) <4 bytes> -- 7 bytes total
(let ()
  (define tmpl (x86_64-asm '((movq (imm32 :x) %rbx))))
  (receive (b _) (link-templates (list tmpl)'())
    (test* "imm32 placeholder default"
           '(#x48 #xc7 #xc3 0 0 0 0)
           (u8vector->list b)))
  (receive (b _) (link-templates (list tmpl)`((:x ,<uint32> 1000)))
    (test* "imm32 placeholder filled"
           '(#x48 #xc7 #xc3 #xe8 #x03 #x00 #x00)
           (u8vector->list b))))

;; imm8 placeholder: movb (imm8 :n), %al
;; Encoding: B0 <1 byte> -- 2 bytes total
(let ()
  (define tmpl (x86_64-asm '((movb (imm8 :n) %al))))
  (receive (b _) (link-templates (list tmpl)'())
    (test* "imm8 placeholder default"
           '(#xb0 0)
           (u8vector->list b)))
  (receive (b _) (link-templates (list tmpl)`((:n ,<uint8> 42)))
    (test* "imm8 placeholder filled"
           '(#xb0 #x2a)
           (u8vector->list b))))

;; Multiple placeholders in one template, including a non-placeholder instruction
(let ()
  (define tmpl (x86_64-asm '((movq (imm64 :fn) %rax)
                             (movb (imm8  :nb) %cl)
                             (ret))))
  (receive (b _) (link-templates (list tmpl)`((:fn ,<uint64> #xdeadbeef00112233)
                                       (:nb ,<uint8> 7)))
    (test* "multiple placeholders"
           (append '(#x48 #xb8 #x33 #x22 #x11 #x00 #xef #xbe #xad #xde)
                   '(#xb1 #x07)
                   '(#xc3))
           (u8vector->list b)))
  ;; Partial instantiation: only :fn supplied, :nb stays 0
  (receive (b _) (link-templates (list tmpl)`((:fn ,<uint64> 1)))
    (test* "partial instantiation"
           (append '(#x48 #xb8 #x01 0 0 0 0 0 0 0)
                   '(#xb1 0)
                   '(#xc3))
           (u8vector->list b))))

;; Placeholder can appear multiple times
(let ()
  (define tmpl (x86_64-asm '((movq (imm64 :a) %rax)
                             (movb (imm8  :b) %cl)
                             (movq (imm64 :a) %rax)
                             (movb (imm8  :b) %cl)
                             (ret))))
  (receive (b _) (link-templates (list tmpl)`((:a ,<uint64> #xcafebabe01234567)
                                       (:b ,<uint8> #xfe)))
    (test* "placeholders appear multiple times"
           (append '(#x48 #xb8 #x67 #x45 #x23 #x01 #xbe #xba #xfe #xca)
                   '(#xb1 #xfe)
                   '(#x48 #xb8 #x67 #x45 #x23 #x01 #xbe #xba #xfe #xca)
                   '(#xb1 #x0fe)
                   '(#xc3))
           (u8vector->list b))))

;; Placeholder in a template that also has labels
(let ()
  (define tmpl (x86_64-asm '(start:
                             (movq (imm64 :ptr) %rax)
                             (ret))))
  (receive (b labels) (link-templates (list tmpl)`((:ptr ,<uint64> #xff)))
    (test* "placeholder with label bytes"
           '(#x48 #xb8 #xff 0 0 0 0 0 0 0 #xc3)
           (u8vector->list b))
    (test* "placeholder with label labels"
           '((start: . 0))
           (obj-template-labels->alist labels))))

;; --- data-directive placeholders ---

;; .dataq placeholder: 8-byte hole
(let ()
  (define tmpl (x86_64-asm '((.dataq :addr))))
  (receive (b _) (link-templates (list tmpl)'())
    (test* ".dataq placeholder default"
           '(0 0 0 0 0 0 0 0)
           (u8vector->list b)))
  (receive (b _) (link-templates (list tmpl)`((:addr ,<uint64> #x0102030405060708)))
    (test* ".dataq placeholder filled"
           '(#x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01)
           (u8vector->list b))))

;; .datal placeholder: 4-byte hole
(let ()
  (define tmpl (x86_64-asm '((.datal :v))))
  (receive (b _) (link-templates (list tmpl)`((:v ,<uint32> #xdeadbeef)))
    (test* ".datal placeholder filled"
           '(#xef #xbe #xad #xde)
           (u8vector->list b))))

;; .datab placeholder: 1-byte hole
(let ()
  (define tmpl (x86_64-asm '((.datab :b))))
  (receive (b _) (link-templates (list tmpl)'())
    (test* ".datab placeholder default" '(0) (u8vector->list b)))
  (receive (b _) (link-templates (list tmpl)`((:b ,<uint8> #xa5)))
    (test* ".datab placeholder filled" '(#xa5) (u8vector->list b))))

;; .datal placeholder with <float>
(let ()
  (define tmpl (x86_64-asm '((.datal :x))))
  (receive (b _) (link-templates (list tmpl)`((:x ,<float> 1.0)))
    (test* ".datal <float> 1.0"
           '(#x00 #x00 #x80 #x3f)
           (u8vector->list b)))
  (receive (b _) (link-templates (list tmpl)`((:x ,<float> -2.5)))
    (test* ".datal <float> -2.5"
           '(#x00 #x00 #x20 #xc0)
           (u8vector->list b))))

;; .dataq placeholder with <float> and <double>
(let ()
  (define tmpl (x86_64-asm '((.dataq :x))))
  (receive (b _) (link-templates (list tmpl)`((:x ,<float> 1.0)))
    (test* ".datal <float> 1.0"
           '(#x00 #x00 #x80 #x3f 0 0 0 0)
           (u8vector->list b)))
  (receive (b _) (link-templates (list tmpl)`((:x ,<float> -2.5)))
    (test* ".datal <float> -2.5"
           '(#x00 #x00 #x20 #xc0 0 0 0 0)
           (u8vector->list b)))
  (receive (b _) (link-templates (list tmpl)`((:x ,<double> 1.0)))
    (test* ".dataq <double> 1.0"
           '(#x00 #x00 #x00 #x00 #x00 #x00 #xf0 #x3f)
           (u8vector->list b)))
  (receive (b _) (link-templates (list tmpl)`((:x ,<double> 3.14)))
    (test* ".dataq <double> 3.14"
           '(#x1f #x85 #xeb #x51 #xb8 #x1e #x09 #x40)
           (u8vector->list b))))

;; Mix: placeholder data following a real instruction
(let ()
  (define tmpl (x86_64-asm '((ret) (.datal :v))))
  (receive (b _) (link-templates (list tmpl)`((:v ,<uint32> #xdeadbeef)))
    (test* ".datal placeholder after ret"
           '(#xc3 #xef #xbe #xad #xde)
           (u8vector->list b))))

;; Mix: instruction placeholder and data placeholder sharing one keyword
(let ()
  (define tmpl (x86_64-asm '((.dataq :fn-ptr)
                             (movq (imm64 :fn-ptr) %rax)
                             (ret))))
  (receive (b _) (link-templates (list tmpl)`((:fn-ptr ,<uint64> #x0102030405060708)))
    (test* "data and imm64 placeholder same keyword"
           (append '(#x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01) ; .dataq
                   '(#x48 #xb8 #x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01) ; movq
                   '(#xc3))                                               ; ret
           (u8vector->list b))))

;; Existing literal data forms still work unchanged
(let ()
  (define tmpl (x86_64-asm '((.dataq (imm64 #x0102030405060708)))))
  (receive (b _) (link-templates (list tmpl)'())
    (test* ".dataq literal unchanged"
           '(#x08 #x07 #x06 #x05 #x04 #x03 #x02 #x01)
           (u8vector->list b))))

;; --- link-template :postamble ---

;; :postamble 0: no change to output size.
(let ([tmpl (x86_64-asm '((ret)))])
  (receive (b _) (link-templates (list tmpl)'() :postamble 0)
    (test* "link-templates :postamble 0"
           '(#xc3)
           (u8vector->list b))))

;; :postamble 8: eight zero bytes appended after the linked bytes.
(let ([tmpl (x86_64-asm '((ret)))])
  (receive (b _) (link-templates (list tmpl)'() :postamble 8)
    (test* "link-templates :postamble 8 size"
           9
           (uvector-size b))
    (test* "link-templates :postamble 8 content"
           '(#xc3 0 0 0 0 0 0 0 0)
           (u8vector->list b))))

;; :postamble region can be filled via offset form.
;; Template: (.dataq :anchor) at offset 0 (8 bytes); postamble adds 8 more.
;; Offset 8 from :anchor reaches into the postamble.
(let ([tmpl (x86_64-asm '((.dataq :anchor)))])
  (receive (b _) (link-templates (list tmpl)`((:anchor ,<uint64> #xfeedface00000000 0)
                                       (:anchor ,<uint32> #xdeadbeef         8))
                                :postamble 8)
    (test* "link-templates :postamble filled via offset form"
           (append '(#x00 #x00 #x00 #x00 #xce #xfa #xed #xfe)
                   '(#xef #xbe #xad #xde 0 0 0 0))
           (u8vector->list b))))

;; labels are unchanged regardless of :postamble.
(let ([tmpl (x86_64-asm '(entry: (ret)))])
  (receive (_ labels) (link-templates (list tmpl)'() :postamble 4)
    (test* "link-templates :postamble labels unchanged"
           '((entry: . 0))
           (obj-template-labels->alist labels))))

;; --- offset parameter form (keyword native-type value extra-offset) ---

;; Template: 16 zero bytes, one 8-byte patch at offset 0 for :base.
(let ([tmpl (make-obj-template
              (list (make-obj-fragment (make-u8vector 16 0) '() '((:base 0 8)) 'text))
              'little-endian)])
  ;; extra-offset 0: fills at the patch's own offset (same as scalar form)
  (receive (b _) (link-templates (list tmpl)`((:base ,<uint32> #x01020304 0)))
    (test* "offset form extra=0"
           '(#x04 #x03 #x02 #x01 0 0 0 0 0 0 0 0 0 0 0 0)
           (u8vector->list b)))
  ;; extra-offset 4: fills 4 bytes starting at offset 4
  (receive (b _) (link-templates (list tmpl)`((:base ,<uint32> #x01020304 4)))
    (test* "offset form extra=4"
           '(0 0 0 0 #x04 #x03 #x02 #x01 0 0 0 0 0 0 0 0)
           (u8vector->list b)))
  ;; two offset params referencing the same keyword
  (receive (b _) (link-templates (list tmpl)`((:base ,<uint32> #xdeadbeef 0)
                                       (:base ,<uint32> #x12345678 8)))
    (test* "offset form two fills from same base"
           '(#xef #xbe #xad #xde 0 0 0 0 #x78 #x56 #x34 #x12 0 0 0 0)
           (u8vector->list b)))
  ;; out-of-bounds: extra-offset puts fill start at or past template end
  (test* "offset form out-of-bounds error"
         (test-error)
         (receive (b _)
           (link-templates (list tmpl)`((:base ,<uint32> 0 16)))
           b)))

;; --- c-array parameter ---

(autoload gauche.native-type make-c-array-type)

(let ([tmpl (make-obj-template
             (list (make-obj-fragment (make-u8vector 12 0) '() '((:data 0 0)) 'text))
             'little-endian)])
  (receive (b _) (link-templates (list tmpl)`((:data ,(make-c-array-type <uint8> '(4)) (10 20 30))))
    (test* "c-array <uint8>"
           '(10 20 30 0 0 0 0 0 0 0 0 0)
           (u8vector->list b)))
  (receive (b _) (link-templates (list tmpl)`((:data ,(make-c-array-type <int32> '(3)) (1 2 3))))
    (test* "c-array <int32>"
           '(1 0 0 0 2 0 0 0 3 0 0 0)
           (u8vector->list b)))
  ;; c-array with extra-offset: fill starting at base + 4
  (receive (b _) (link-templates (list tmpl)`((:data ,(make-c-array-type <uint8> '(4)) (10 20 30) 4)))
    (test* "c-array <uint8> with extra-offset"
           '(0 0 0 0 10 20 30 0 0 0 0 0)
           (u8vector->list b)))
  (test* "c-array non-list error"
         (test-error)
         (receive (b _)
           (link-templates (list tmpl)`((:data ,(make-c-array-type <uint8> '(4)) not-a-list)))
           b)))

;; --- movs_ instruction-variant placeholder ---

;; ((movs_ :op) %xmm0 %xmm1)  -- sse->sse form
;; Default encoding: F2 0F 10 ModRM
;; ModRM: mod=3(reg-reg), reg=1(xmm1/dst), r/m=1(xmm1/dst) -> 0b11_001_001 = #xc9
(let ()
  (define tmpl (x86_64-asm '(((movs_ :op) %xmm0 %xmm1))))
  ;; Default is movsd
  (receive (b _) (link-templates (list tmpl)'())
    (test* "movs_ sse->sse default (movsd)"
           '(#xf2 #x0f #x10 #xc9)
           (u8vector->list b)))
  ;; Explicit movsd
  (receive (b _) (link-templates (list tmpl)'((:op #f movsd)))
    (test* "movs_ sse->sse explicit movsd"
           '(#xf2 #x0f #x10 #xc9)
           (u8vector->list b)))
  ;; Switch to movss
  (receive (b _) (link-templates (list tmpl)'((:op #f movss)))
    (test* "movs_ sse->sse switched to movss"
           '(#xf3 #x0f #x10 #xc9)
           (u8vector->list b)))
  ;; Re-instantiate as movsd again (template not mutated)
  (receive (b _) (link-templates (list tmpl)'((:op #f movsd)))
    (test* "movs_ sse->sse back to movsd"
           '(#xf2 #x0f #x10 #xc9)
           (u8vector->list b))))

;; ((movs_ :op) (%rax) %xmm1)  -- mem->sse form
;; Default encoding: F2 0F 10 ModRM
;; ModRM: mod=0, reg=1(xmm1/dst), r/m=0(%rax) -> 0b00_001_000 = #x08
(let ()
  (define tmpl (x86_64-asm '(((movs_ :op) (%rax) %xmm1))))
  (receive (b _) (link-templates (list tmpl)'())
    (test* "movs_ mem->sse default (movsd)"
           '(#xf2 #x0f #x10 #x08)
           (u8vector->list b)))
  (receive (b _) (link-templates (list tmpl)'((:op #f movss)))
    (test* "movs_ mem->sse switched to movss"
           '(#xf3 #x0f #x10 #x08)
           (u8vector->list b))))

;; ((movs_ :op) %xmm0 (%rax))  -- sse->mem form
;; Default encoding: F2 0F 11 ModRM
;; ModRM: mod=0, reg=0(xmm0/src), r/m=0(%rax) -> #x00
(let ()
  (define tmpl (x86_64-asm '(((movs_ :op) %xmm0 (%rax)))))
  (receive (b _) (link-templates (list tmpl)'())
    (test* "movs_ sse->mem default (movsd)"
           '(#xf2 #x0f #x11 #x00)
           (u8vector->list b)))
  (receive (b _) (link-templates (list tmpl)'((:op #f movss)))
    (test* "movs_ sse->mem switched to movss"
           '(#xf3 #x0f #x11 #x00)
           (u8vector->list b))))

;; movs_ combined with other placeholders in a sequence
(let ()
  (define tmpl (x86_64-asm '((.dataq :fn-ptr)
                             ((movs_ :variant) (%rax) %xmm0)
                             (ret))))
  (receive (b _) (link-templates (list tmpl)`((:fn-ptr ,<uint64> #xdeadbeef) (:variant #f movss)))
    (test* "movs_ combined with .dataq placeholder"
           (append '(#xef #xbe #xad #xde 0 0 0 0) ; .dataq :fn-ptr
                   '(#xf3 #x0f #x10 #x00)          ; movss (%rax),%xmm0
                   '(#xc3))                         ; ret
           (u8vector->list b))))

;;----------------------------------------------------------------------
(test-section "link-templates")

;; Helper: make a simple single-fragment template from raw data.
(define (make-text-tmpl bytes labels patches)
  (make-obj-template (list (make-obj-fragment bytes labels patches 'text)) 'little-endian))

;; --- Two single-section templates concatenated ---

(let* ([t1 (make-text-tmpl #u8(1 2 3) '((a: . 0) (b: . 2)) '())]
       [t2 (make-text-tmpl #u8(4 5 6) '((c: . 0) (d: . 1)) '())])
  (receive (bytes labels) (link-templates (list t1 t2) '())
    (test* "link-templates two text: bytes"
           '(1 2 3 4 5 6)
           (u8vector->list bytes))
    (test* "link-templates two text: labels rebased"
           '((a: . 0) (b: . 2) (c: . 3) (d: . 4))
           (obj-template-labels->alist labels))))

;; --- Patch in the second template is correctly rebased and applied ---

(let* ([t1 (make-text-tmpl #u8(0 0 0 0) '() '())]
       ;; patch descriptor: (keyword byte-offset byte-width)
       [t2 (make-text-tmpl #u8(0 0 0 0 0 0 0 0) '() '((:val 0 8)))])
  (receive (bytes _) (link-templates (list t1 t2) `((:val ,<uint64> #xdeadbeef)))
    (test* "link-templates patch rebased into second template"
           (append '(0 0 0 0)
                   '(#xef #xbe #xad #xde 0 0 0 0))
           (u8vector->list bytes))))

;; --- Two templates each with text and data fragments: A+C+B+D ordering ---
;; We build templates with two fragments manually using make-obj-template's
;; compat wrapper for each fragment, then merge both.

(let* ([frag-t1-text (make-obj-fragment #u8(10 20 30) '((tx1: . 0)) '() 'text)]
       [frag-t1-data (make-obj-fragment #u8(40 50)    '((da1: . 0)) '() 'data)]
       [tmpl1        (make <obj-template>
                       :fragments (list frag-t1-text frag-t1-data)
                       :endian 'little-endian)]
       [frag-t2-text (make-obj-fragment #u8(60 70)     '((tx2: . 0)) '() 'text)]
       [frag-t2-data (make-obj-fragment #u8(80 90 100) '((da2: . 0)) '() 'data)]
       [tmpl2        (make <obj-template>
                       :fragments (list frag-t2-text frag-t2-data)
                       :endian 'little-endian)])
  (receive (bytes labels) (link-templates (list tmpl1 tmpl2) '())
    ;; Expected order: text1(3) + text2(2) + data1(2) + data2(3) = A+C+B+D
    (test* "link-templates A+C+B+D bytes"
           '(10 20 30 60 70 40 50 80 90 100)
           (u8vector->list bytes))
    ;; Labels: tx1:=0, tx2:=3, da1:=5, da2:=7
    (test* "link-templates A+C+B+D labels"
           '((tx1: . 0) (tx2: . 3) (da1: . 5) (da2: . 7))
           (obj-template-labels->alist labels))))

;; --- Param patch in a data fragment of the second template ---

(let* ([frag-text (make-obj-fragment #u8(1 2 3) '() '() 'text)]
       ;; patch at offset 0 in data fragment, byte-width 4
       [frag-data (make-obj-fragment #u8(0 0 0 0) '() '((:x 0 4)) 'data)]
       [tmpl      (make <obj-template>
                    :fragments (list frag-text frag-data)
                    :endian 'little-endian)])
  (receive (bytes _) (link-templates (list tmpl) `((:x ,<uint32> #x01020304)))
    ;; text(3 bytes) + data(4 bytes); patch at offset 0 in data = offset 3 overall
    (test* "link-templates data patch applied after text"
           '(1 2 3 4 3 2 1)
           (u8vector->list bytes))))

;; --- Mismatched endianness error ---

(let* ([t1 (make-obj-template (list (make-obj-fragment #u8(1) '() '() 'text)) 'little-endian)]
       [t2 (make-obj-template (list (make-obj-fragment #u8(1) '() '() 'text)) 'big-endian)])
  (test* "link-templates endian mismatch"
         #t
         (guard (e (#t #t)) (link-templates (list t1 t2) '()) #f)))

;;----------------------------------------------------------------------
(test-section ".section pseudo-instruction")

;; --- Two-section assembly: text + data fragments ---

(let* ([tmpl (x86_64-asm '((movq %rdi %rax)
                           (.section data)
                           (.dataq (imm64 #xdeadbeef))))]
       [frags (~ tmpl 'fragments)])
  (test* ".section produces two fragments" 2 (length frags))
  (test* ".section text fragment section" 'text (~ (car frags) 'section))
  (test* ".section data fragment section" 'data (~ (cadr frags) 'section))
  ;; movq %rdi,%rax = REX.W(48) 89 f8
  (test* ".section text bytes" '(#x48 #x89 #xf8)
         (u8vector->list (~ (car frags) 'bytes)))
  (test* ".section data bytes" '(#xef #xbe #xad #xde 0 0 0 0)
         (u8vector->list (~ (cadr frags) 'bytes))))

;; --- Labels are fragment-local after splitting ---

(let* ([tmpl (x86_64-asm '(entry:
                           (movq %rdi %rax)
                           (.section data)
                           val:
                           (.dataq (imm64 0))))]
       [frags (~ tmpl 'fragments)]
       [text-frag (car frags)]
       [data-frag (cadr frags)])
  (test* ".section text label offset" 0
         (assq-ref (~ text-frag 'labels) 'entry:))
  (test* ".section data label offset" 0
         (assq-ref (~ data-frag 'labels) 'val:)))

;; --- Param patch in data fragment is correctly rebased ---

(let* ([tmpl (x86_64-asm '((movq %rdi %rax)
                           (.section data)
                           (.dataq :myval)))]
       [frags  (~ tmpl 'fragments)]
       [dpatch (car (~ (cadr frags) 'patches))])
  ;; patch offset within data fragment should be 0
  (test* ".section data patch offset" 0 (cadr dpatch))
  ;; applying the param should fill the data word; text(3) + data(8) = 11 bytes
  (receive (bytes _) (link-templates (list tmpl)`((:myval ,<uint64> #xcafebabe)))
    (test* ".section data param fill"
           '(#x48 #x89 #xf8 #xbe #xba #xfe #xca 0 0 0 0)
           (u8vector->list bytes))))

;; --- Cross-section near jump produces a label-rel patch ---

(let* ([tmpl  (x86_64-asm '((jmpl data-target:)
                            (.section data)
                            data-target:
                            (.dataq (imm64 0))))]
       [text-frag (car (~ tmpl 'fragments))]
       [tpatch    (car (~ text-frag 'patches))])
  (test* "cross-section jmpl patch key"    'data-target: (car tpatch))
  (test* "cross-section jmpl patch offset" 1             (cadr tpatch))
  (test* "cross-section jmpl patch type"   'label-rel    (caddr tpatch))
  (test* "cross-section jmpl insn-end"     5             (cadddr tpatch)))

;; --- Cross-section short jump signals an error ---

(test* "cross-section short jump error"
       (test-error <error> #/cross-section short jump not allowed/)
       (x86_64-asm '((jmp data-target:)
                     (.section data)
                     data-target:
                     (.dataq (imm64 0)))))

;; --- label-rel patch resolution via link-template ---
;; Layout after link-template:
;;   text[0]   ret            1 byte  (#xc3)
;;   text[1]   jmpl data-target:  5 bytes  (#xe9 + 4-byte disp)
;;   text[6]   padding for alignment
;;   data[8]   .dataq 0       8 bytes  (pad before label)
;;   data[16]  data-target:   <- absolute offset 16
;;   data[16]  .dataq 0       8 bytes
;; insn-end = 6, target = 14, disp = 14 - 6 = 8 = #x00000008
(let* ([tmpl (x86_64-asm '((ret)
                           (jmpl data-target:)
                           (.section data)
                           (.align 8)
                           (.dataq (imm64 0))
                           data-target:
                           (.dataq (imm64 0))))])
  (receive (bytes _) (link-templates (list tmpl)'())
    (test* "label-rel resolved: full bytes"
           (append '(#xc3 #xe9 #x0a #x00 #x00 #x00)  ; text: ret + jmpl disp=8+2
                   '(#x00 #x00)                      ; alignment padding
                   (make-list 16 0))                 ; data: 16 zero bytes
           (u8vector->list bytes))))


;;----------------------------------------------------------------------
(test-section "locals and prolog/epilog sections")

;; Helper: make a single-fragment template for an arbitrary named section.
(define (make-section-frag-tmpl section bytes :optional (patches '()) (locals '()))
  (make-obj-template
   (list (make-obj-fragment bytes '() patches section locals))
   'little-endian
   8))                                  ; stack-word-size=8

;; --- Section ordering ---

;; Fragments supplied in reversed order; link-templates must reorder to
;; prolog -> text -> epilog -> data.
(let* ([prolog-t (make-section-frag-tmpl 'prolog #u8(10 20))]
       [text-t   (make-section-frag-tmpl 'text   #u8(30 40 50))]
       [epilog-t (make-section-frag-tmpl 'epilog #u8(60))]
       [data-t   (make-section-frag-tmpl 'data   #u8(70 80 90))])
  (receive (bytes _)
      (link-templates (list data-t epilog-t text-t prolog-t) '())
    (test* "section ordering: prolog→text→epilog→data"
           '(10 20  30 40 50  60  70 80 90)
           (u8vector->list bytes))))

;; Unknown sections land after the known four, preserving their first-seen order.
(let* ([tmpl (make-obj-template
              (list (make-obj-fragment #u8(1) '() '() 'text)
                    (make-obj-fragment #u8(2) '() '() 'custom)
                    (make-obj-fragment #u8(3) '() '() 'prolog)
                    (make-obj-fragment #u8(4) '() '() 'data))
              'little-endian)])
  (receive (bytes _) (link-templates (list tmpl) '())
    (test* "section ordering: unknown section after data"
           '(3  1  4  2)
           (u8vector->list bytes))))

;; --- :nbytes-local computed from locals ---

;; Two locals (:local-x, :local-y) declared in a text fragment.
;; A prolog fragment has a 4-byte :nbytes-local patch.
;; Expected: :nbytes-local = 2*8 = 16, :local-x = -8, :local-y = -16.
(let* ([prolog-frag
        (make-obj-fragment #u8(0 0 0 0) '() '((:nbytes-local 0 4)) 'prolog)]
       [text-frag
        (make-obj-fragment (make-u8vector 8 0) '()
                           '((:local-x 0 4) (:local-y 4 4))
                           'text
                           '(:local-x :local-y))]
       [tmpl (make-obj-template (list prolog-frag text-frag) 'little-endian 8)])
  (receive (bytes _) (link-templates (list tmpl) '())
    ;; prolog (4 bytes) comes first, then text (8 bytes)
    (test* ":nbytes-local = 16"
           '(#x10 #x00 #x00 #x00)
           (u8vector->list (u8vector-copy bytes 0 4)))
    (test* ":local-x offset = -8 (F8 FF FF FF)"
           '(#xf8 #xff #xff #xff)
           (u8vector->list (u8vector-copy bytes 4 8)))
    (test* ":local-y offset = -16 (F0 FF FF FF)"
           '(#xf0 #xff #xff #xff)
           (u8vector->list (u8vector-copy bytes 8 12)))))

;; --- :nbytes-local = 0 when no locals are declared ---

(let* ([prolog-frag
        (make-obj-fragment #u8(0 0 0 0) '() '((:nbytes-local 0 4)) 'prolog)]
       [text-frag
        (make-obj-fragment #u8(1 2 3) '() '() 'text)]
       [tmpl (make-obj-template (list prolog-frag text-frag) 'little-endian 8)])
  (receive (bytes _) (link-templates (list tmpl) '())
    (test* ":nbytes-local = 0 with no locals"
           '(#x00 #x00 #x00 #x00)
           (u8vector->list (u8vector-copy bytes 0 4)))))

;; --- Full prolog + body + epilog via x86_64-asm ---
;;
;; Prolog (.section prolog): subq $N, %rsp        allocate stack frame
;; Text:                     movq (:local-a %rsp), %rax    slot 0 = -8(%rsp)
;;                           movq (:local-b %rsp), %rcx    slot 1 = -16(%rsp)
;; Epilog (.section epilog): addq $N, %rsp ; ret  restore and return
;;
;; With 2 locals and stack-word-size=8: N = 16.
;;
;; Expected bytes after linking:
;;   prolog[0..6]   48 81 EC 10 00 00 00   subq $16, %rsp
;;   text[7..14]    48 8B 84 24 F8 FF FF FF  movq -8(%rsp), %rax
;;   text[15..22]   48 8B 8C 24 F0 FF FF FF  movq -16(%rsp), %rcx
;;   epilog[23..30] 48 81 C4 10 00 00 00 C3  addq $16, %rsp; ret

(let* ([prolog (x86_64-asm '((.section prolog)
                             (subq (imm32 :nbytes-local) %rsp)))]
       [body   (x86_64-asm '((movq (:local-a %rsp) %rax)
                             (movq (:local-b %rsp) %rcx))
                            :locals '(:local-a :local-b))]
       [epilog (x86_64-asm '((.section epilog)
                             (addq (imm32 :nbytes-local) %rsp)
                             (ret)))])
  (receive (bytes _) (link-templates (list prolog body epilog) '())
    (test* "prolog+body+epilog: total byte count" 31 (uvector-size bytes))
    (test* "prolog+body+epilog: prolog — subq $16,%rsp"
           '(#x48 #x81 #xec #x10 #x00 #x00 #x00)
           (u8vector->list (u8vector-copy bytes 0 7)))
    (test* "prolog+body+epilog: movq -8(%rsp),%rax"
           '(#x48 #x8b #x84 #x24 #xf8 #xff #xff #xff)
           (u8vector->list (u8vector-copy bytes 7 15)))
    (test* "prolog+body+epilog: movq -16(%rsp),%rcx"
           '(#x48 #x8b #x8c #x24 #xf0 #xff #xff #xff)
           (u8vector->list (u8vector-copy bytes 15 23)))
    (test* "prolog+body+epilog: epilog — addq $16,%rsp + ret"
           '(#x48 #x81 #xc4 #x10 #x00 #x00 #x00 #xc3)
           (u8vector->list (u8vector-copy bytes 23 31)))))

(test-end)
