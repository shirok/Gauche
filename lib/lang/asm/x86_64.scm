;;;
;;; x86_64 minimum assembler
;;;
;;;   Copyright (c) 2015-2019  Shiro Kawai  <shiro@acm.org>
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

;; EXPERIMENTAL - At this moment, this module is mainly "to play with" x86_64
;; instructions.  This is not a full set assembler; we implement features
;; on demand.
;; TODO: Eventually we might want to split machine-independent front-end
;; to lang.asm.  The API will very likely change then.

(define-module lang.asm.x86_64
  (use gauche.uvector)
  (use gauche.sequence)
  (use gauche.parameter)
  (use srfi-1)
  (use srfi-13)
  (use srfi-42)
  (use util.match)
  (export-all)                          ;for now
  )
(select-module lang.asm.x86_64)

;; Instruction notation
;;   (opcode)
;;   (opcode operand1)
;;   (opcode operand1 operand2)
;;
;; Operand
;;   %reg
;;   (%reg)
;;   imm
;;   (imm)
;;   (off %base)
;;   (%base %index)
;;   (%base %index scale)
;;   (off %base %index scale)

;;-----------------------------------------------------------------
;; Entry and x86 ISA definitions (subset)
;;

;; asm  :: [Insn] -> [Byte]
(define (asm insns)
  ;; first pass. create [(p,xaddr)] where p :: (Int,[(Symbol,Int)]) -> [Byte]
  ;; and xaddr is a value of PC after the code is fetched.
  (receive (abss _)
      (map-accum (match-lambda*
                   [((? symbol? label) addr) (values (cons label addr) addr)]
                   [(insn addr) (let* ((p (asm1 (parse-insn insn)))
                                       (dummy (p addr #f))
                                       (naddr (+ addr (length dummy))))
                                  (values (cons p naddr) naddr))])
                 0 insns)
    ;; second pass
    (let1 bss (fold (^[x seed]
                      (match-let1 (p . addr) x
                        (if (symbol? p)
                          seed          ;ignore labels
                          (cons (p addr abss) seed))))
                    '() abss)
      (concatenate (reverse bss)))))

;; asm1 :: ParsedInsn -> (Int, [Symbol,Int]) -> [Byte]
;;  First pass.  Returns a closure to generate the byte sequence of
;;  machine instructions.
;;  The closure will take (1) The address of *next* instruction, and
;;  (2) an assoc list of labels to addresses.
(define (asm1 pinsn)
  (define ! expand-spec)
  (define w rex.w)
  (match pinsn
    ;; call and ret.  we don't care "far" calls/returns.
    [`(call (imm32 ,i))            (! (opc #xe8) (imm32 i))]
    [`(call (reg ,r))              (! (opc #xff) (reg 2) (r/m-reg r))]
    [`(call (mem . ,x))            (! (opc #xff) (reg 2) (mem x))]
    [`(ret)                        (! (opc #xc3))]

    [`(push (reg ,n))              (! (rex.b n) (opc+rq #x50 n))]
    [`(pop  (reg ,n))              (! (rex.b n) (opc+rq #x58 n))]

    ;; jumps.  for the time being, we ask programmers to explicitly specify
    ;; whether the jump is short or long.
    [`(jmp  (reg ,t))              (! (opc #xff) (reg 4) (r/m-reg t))]
    [`(jmp  (mem . ,x))            (! (opc #xff) (reg 4) (mem x))]
    [`(jmp  (label ,t))            (op-jump t #t #xeb)]
    [`(jmpl (label ,t))            (op-jump t #f #xe9)]
    [`(jo   (label ,t))            (op-jump t #t #x70)]
    [`(jol  (label ,t))            (op-jump t #f '(#x0f #x80))]
    [`(jno  (label ,t))            (op-jump t #t #x71)]
    [`(jnol (label ,t))            (op-jump t #f '(#x0f #x81))]
    [`(jb   (label ,t))            (op-jump t #t #x72)]
    [`(jc   (label ,t))            (op-jump t #t #x72)]
    [`(jnae (label ,t))            (op-jump t #t #x72)]
    [`(jbl  (label ,t))            (op-jump t #f '(#x0f #x82))]
    [`(jcl  (label ,t))            (op-jump t #f '(#x0f #x82))]
    [`(jnael(label ,t))            (op-jump t #f '(#x0f #x82))]
    [`(jnb  (label ,t))            (op-jump t #t #x73)]
    [`(jnc  (label ,t))            (op-jump t #t #x73)]
    [`(jae  (label ,t))            (op-jump t #t #x73)]
    [`(jnbl (label ,t))            (op-jump t #f '(#x0f #x83))]
    [`(jncl (label ,t))            (op-jump t #f '(#x0f #x83))]
    [`(jael (label ,t))            (op-jump t #f '(#x0f #x83))]
    [`(jz   (label ,t))            (op-jump t #t #x74)]
    [`(je   (label ,t))            (op-jump t #t #x74)]
    [`(jzl  (label ,t))            (op-jump t #f '(#x0f #x84))]
    [`(jel  (label ,t))            (op-jump t #f '(#x0f #x84))]
    [`(jnz  (label ,t))            (op-jump t #t #x75)]
    [`(jne  (label ,t))            (op-jump t #t #x75)]
    [`(jnzl (label ,t))            (op-jump t #f '(#x0f #x85))]
    [`(jnel (label ,t))            (op-jump t #f '(#x0f #x85))]
    [`(jbe  (label ,t))            (op-jump t #t #x76)]
    [`(jna  (label ,t))            (op-jump t #t #x76)]
    [`(jbel (label ,t))            (op-jump t #f '(#x0f #x86))]
    [`(jnal (label ,t))            (op-jump t #f '(#x0f #x86))]
    [`(jnbe (label ,t))            (op-jump t #t #x77)]
    [`(ja   (label ,t))            (op-jump t #t #x77)]
    [`(jnbel(label ,t))            (op-jump t #f '(#x0f #x87))]
    [`(jal  (label ,t))            (op-jump t #f '(#x0f #x87))]
    [`(js   (label ,t))            (op-jump t #t #x78)]
    [`(jsl  (label ,t))            (op-jump t #f '(#x0f #x88))]
    [`(jns  (label ,t))            (op-jump t #t #x79)]
    [`(jnsl (label ,t))            (op-jump t #f '(#x0f #x89))]
    [((or 'jp 'jpe)    ('label t)) (op-jump t #t #x7a)]
    [((or 'jpl 'jpel)  ('label t)) (op-jump t #f '(#x0f #x8a))]
    [((or 'jnp 'jpo)   ('label t)) (op-jump t #t #x7b)]
    [((or 'jnpl 'jpol) ('label t)) (op-jump t #f '(#x0f #x8b))]
    [((or 'jl 'jnge)   ('label t)) (op-jump t #t #x7c)]
    [((or 'jll 'jngel) ('label t)) (op-jump t #f '(#x0f #x8c))]
    [((or 'jnl 'jge)   ('label t)) (op-jump t #t #x7d)]
    [((or 'jnll 'jgel) ('label t)) (op-jump t #f '(#x0f #x8d))]
    [((or 'jle 'jng)   ('label t)) (op-jump t #t #x7e)]
    [((or 'jlel 'jngl) ('label t)) (op-jump t #f '(#x0f #x8e))]
    [((or 'jnle 'jg)   ('label t)) (op-jump t #t #x7f)]
    [((or 'jnlel 'jgl) ('label t)) (op-jump t #f '(#x0f #x8f))]
    
    ;; moving data around
    [`(movq (imm8  ,i) (reg ,dst)) (! w (opc #xc7) (reg 0) (r/m-reg dst) (imm32 i))]
    [`(movq (imm32 ,i) (reg ,dst)) (! w (opc #xc7) (reg 0) (r/m-reg dst) (imm32 i))]
    [`(movq (imm8  ,i) (mem . ,x)) (! w (opc #xc7) (reg 0) (mem x) (imm32 i))]
    [`(movq (imm32 ,i) (mem . ,x)) (! w (opc #xc7) (reg 0) (mem x) (imm32 i))]
    [`(movq (imm64 ,i) (reg ,dst)) (! w (rex.b dst) (opc+rq #xb8 dst) (imm64 i))]
    [`(movq (label ,l) (reg ,dst)) (op-movlabel l dst)]
    [`(movq (reg ,src) (reg ,dst)) (! w (opc #x89) (reg src) (r/m-reg dst))]
    [`(movq (reg ,src) (mem . ,x)) (! w (opc #x89) (reg src) (mem x))]
    [`(movq (mem . ,x) (reg ,dst)) (! w (opc #x8b) (reg dst) (mem x))]

    [`(movzbq (mem . ,x) (reg ,dst)) (! w (opc'(#x0f #xb6)) (reg dst) (mem x))]
    [`(movzwq (mem . ,x) (reg ,dst)) (! w (opc'(#x0f #xb7)) (reg dst) (mem x))]

    [`(leaq (mem . ,x) (reg ,dst)) (! w (opc #x8d) (reg dst) (mem x))]

    ;; calculations
    [('addq _ _)                   (op-add pinsn 0)]
    [('orq _ _)                    (op-add pinsn 1)]
    [('adcq _ _)                   (op-add pinsn 2)]
    [('sbbq _ _)                   (op-add pinsn 3)]
    [('andq _ _)                   (op-add pinsn 4)]
    [('subq _ _)                   (op-add pinsn 5)]
    [('xorq _ _)                   (op-add pinsn 6)]
    [('cmpq _ _)                   (op-add pinsn 7)]

    ;; shifts and rotation
    [('rol  _ _)                   (op-shift pinsn 0)]
    [('ror  _ _)                   (op-shift pinsn 1)]
    [('rcl  _ _)                   (op-shift pinsn 2)]
    [('rcr  _ _)                   (op-shift pinsn 3)]
    [('shl  _ _)                   (op-shift pinsn 4)]
    [('shr  _ _)                   (op-shift pinsn 5)]
    [('sar  _ _)                   (op-shift pinsn 7)]

    ;; inc and dec
    [('incq _)                     (op-inc pinsn 0)]
    [('decq _)                     (op-inc pinsn 1)]

    ;; embedded data
    [`(datab ,(_ i) ...)           (^(a t) (append-map int8 i))]
    [`(datal ,(_ i) ...)           (^(a t) (append-map int32 i))]
    [`(dataq ,(_ i) ...)           (^(a t) (append-map int64 i))]
    [`(datas (str ,s))             (^(a t) (fold-right cons '(0) (string->u8vector s)))]
    ))

;; jump family
(define (op-jump target short? opcode)
  (define immX   (if short? imm8 imm32))
  (define immX?  (if short? imm8? imm32?))
  (^[addr label-alist]
    (cond [(not label-alist)
           ((expand-spec (opc opcode) (immX 0)) 0 0)] ; dummy
          [(assq-ref label-alist target)
           => (^[taddr]
                (unless (immX? (- taddr addr))
                  (error "jump target out of range:" target))
                ((expand-spec (opc opcode) (immX (- taddr addr))) 0 0))]
          [else (error "jump destination doesn't exist:" target)])))

;; special case to load label's address into a register
(define (op-movlabel label dst)
  (define ! expand-spec)
  (define w rex.w)
  (^[addr label-alist]
    (if label-alist
      (let1 laddr (assq-ref label-alist label)
        (unless laddr (error "undefined label:" label))
        ((! w (rex.b dst) (opc+rq #xb8 dst) (imm64 laddr)) 0 0))
      ((! w (rex.b dst) (opc+rq #xb8 dst) (imm64 0)) 0 0)))) ;; dummy

;; addq family
;;  opcode variations are derived from a single number, regc.
(define (op-add pinsn regc)
  (define ! expand-spec)
  (define w rex.w)
  (define basc (+ (ash regc 3) 1))
  (define raxc (+ (ash regc 3) 5))
  (match pinsn
    [`(,_ (imm8 ,i)  (reg ,dst)) (! w (opc #x83) (reg regc) (r/m-reg dst) (imm8 i))]
    [`(,_ (imm8 ,i)  (mem . ,x)) (! w (opc #x83) (reg regc) (mem x) (imm8 i))]
    [`(,_ (imm32 ,i) (reg ,dst)) (if (= dst 0) ; %rax
                                   (! w (opc raxc) (imm32 i))
                                   (! w (opc #x81) (reg regc) (r/m-reg dst) (imm32 i)))]
    [`(,_ (imm32 ,i) (mem . ,x)) (! w (opc #x81) (reg regc) (imm32 i))]
    [`(,_ (reg ,src) (reg ,dst)) (! w (opc basc) (reg src) (r/m-reg dst))]
    [`(,_ (reg ,src) (mem . ,x)) (! w (opc basc) (reg src) (mem x))]
    [`(,_ (mem . ,x) (reg ,dst)) (! w (opc (+ basc 2)) (reg dst) (mem x))]
    ))

;; shift family
(define (op-shift pinsn regc)
  (define ! expand-spec)
  (define w rex.w)
  (match pinsn
    [`(,_ (imm8 1)  (reg ,r))   (! w (opc #xd1) (reg regc) (r/m-reg r))]
    [`(,_ (imm8 1)  (mem . ,x)) (! w (opc #xd1) (reg regc) (mem x))]
    [`(,_ (imm8 ,i) (reg ,r))   (! w (opc #xc1) (reg regc) (r/m-reg r) (imm8 i))]
    [`(,_ (imm8 ,i) (mem . ,x)) (! w (opc #xc1) (reg regc) (mem x) (imm8 i))]
    [`(,_ (reg %cl) (reg ,r))   (! w (opc #xd3) (reg regc) (r/m-reg r))]
    [`(,_ (reg %cl) (mem . ,x)) (! w (opc #xd3) (reg regc) (mem x))]
    ))

;; inc and dec
(define (op-inc pinsn regc)
  (define ! expand-spec)
  (define w rex.w)
  (match pinsn
    [`(,_ (reg ,r))             (! w (opc #xff) (reg regc) (r/m-reg r))]
    [`(,_ (mem . ,x))           (! w (opc #xff) (reg regc) (mem x))]
    ))

;;-----------------------------------------------------------------
;; Asm parser & machine code generators
;;

(define (parse-insn insn)
  (match insn
    [(opc . oprs) `(,opc ,@(map parse-operand oprs))]
    [_ (error "invalid insn:" insn)]))

(define (parse-operand opr)
  (match opr
    [(? reg64?)                     `(reg ,(regnum opr))]
    [(? symbol?)                    `(label ,opr)]
    [(? string?)                    `(str ,opr)] ; C string literal
    [(? imm8?)                      `(imm8 ,opr)]
    [(? imm32?)                     `(imm32 ,opr)]
    [(? imm64?)                     `(imm64 ,opr)]
    [((? integer? a))               `(mem addr ,a)]
    [((? reg64? b))                 `(mem base ,(regnum b))]
    [((? symbol? l))                `(mem label ,l)]
    [((? integer? d) (? reg64? b))  `(mem base+disp ,(regnum b) ,d)]
    [((? reg64? b) (? reg64? i))    `(mem sib ,(regnum b) ,(regnum i) 1 0)]
    [((? integer? d) (? reg64? b) (? reg64? i))
                                    `(mem sib ,(regnum b) ,(regnum i) 1 ,d)]
    [((? reg64? b) (? reg64? i) (? scale? s))
                                    `(mem sib ,(regnum b) ,(regnum i) ,s 0)]
    [((? integer? d) (? reg64? b) (? reg64? i) (? scale? s))
                                    `(mem sib ,(regnum b) ,(regnum i) ,s ,d)]
    [_ (error "invalid operand:" opr)]))

(define (expand-spec . modifiers)
  (^[addr label-alist]
    (generate-bincode (fold (cut <> <> addr label-alist) '() modifiers))))

(define (generate-bincode spec)
  (let-keywords* spec ((rex.w #f) (rex.r #f) (rex.x #f) (rex.b #f) ;REX
                       (opcode #f)
                       (mode #f) (reg #f) (r/m #f) ;modrm
                       (scale 1) (index #f) (base #f) ;sib
                       (displacement #f)
                       (immediate    #f))
    (cond-list
     [(or rex.w rex.r rex.x rex.b) (pack-rex rex.w rex.r rex.x rex.b)]
     [#t  @ (if (list? opcode) opcode `(,opcode))]
     [(or mode reg r/m) (pack-modrm mode reg r/m)]
     [(or index base) (pack-sib scale index base)]
     [displacement @]
     [immediate @])))

;; REX prefix
;;    w - operand width.  #t - 64bit
;;    r - modrm reg register +8
;;    x - sib index register +8
;;    b - modrm r/m register +8
(define (pack-rex w r x b)
  (+ #x40 (if w 8 0) (if r 4 0) (if x 2 0) (if b 1 0)))

;; MODRM byte
;;    reg - %rax--%rdi (REX.r=0) or %r8--%r15 (REX.r = 1)
;;    case mod
;;     00 r/m - REX.b=0: [%rax %rcx %rdx %rbx SIB RIP+disp32 %rsi %rdi]
;;              REX.b=1: [%r8  %r9  %r10 %r11 SIB RIP+disp32 %r14 %r15]
;;     01 r/m - REX.b=0: [%rax %rcx %rdx %rbx SIB %rbp %rsi %rdi] + disp8
;;              REX.b=1: [%r8  %r9  %r10 %r11 SIB %r13 %r14 %r15] + disp8
;;     10 r/m - REX.b=0: [%rax %rcx %rdx %rbx SIB %rbp %rsi %rdi] + disp32
;;              REX.b=1: [%r8  %r9  %r10 %r11 SIB %r13 %r14 %r15] + disp32
;;     11 r/m - REX.b=0: %rax %rcx %rdx %rbx %rsp %rbp %rsi %rdi
;;                       %al  %cl  %dl  %bl  %ah  %ch  %dh  %bh
;;                       %al  %cl  %dl  %bl  %spl %bpl %sil %dil
;;              REX.b=1: %r8  %r9  %r10 %r11 %r12 %r13 %r14 %r15

(define (pack-modrm mod reg r/m)
  (pack-byte233 (or mod 0) (or reg 0) (or r/m 0)))

(define (pack-byte233 x y z)
  (+ (ash (logand x 3) 6) (ash (logand y 7) 3) (logand z 7)))

;; SIB byte
;;    scale - 1,2,4,8
;;    index - REX.x=0  [%rax %rcx %rdx %rbx none %rbp %rsi %rdi]
;;            REX.x=1  [%r8  %r9  %r10 %r11 %r12 %r13 %r14 %r15]
;;    base  - REX.b=0  [%rax %rcx %rdx %rbx %rsp **   %rsi %rdi]
;;            REX.b=1  [%r8  %r9  %r10 %r11 %r12 **   %r14 %r15]
;;      where ** depends on modrm's mod field:
;;            mod=00 => disp32
;;            mod=01 => %rbp/%r13+disp8
;;            mod=10 => %rbp/%r13+disp32
(define (pack-sib scale index base)
  (pack-byte233 (case scale [(#f 1) 0][(2) 1][(4) 2][(8) 3])
                (or index 0)
                (or base 0)))

;; Modifiers
;;   A modifier is a closure that modifies code sequences of an instruction.
;;   It is called once in each pass per instruction.  The first pass is
;;   used to determine the # of octets; there may be a label that's yet to
;;   be resolved.  The second pass fills the resolved address.
;;
;;   A modifier closure takes three arguments, s (spec), a (address) and
;;   t (table).  S is an opaque list to which the modifier prepends
;;   information.  T and A are #f in the first pass, and an alist of
;;   label to address and the address of the next instruction respectively
;;   in the second pass.

(define (opc c)      (^[s a t] `(:opcode ,c ,@s)))
(define (opc+rq c r) (^[s a t] `(:opcode ,(+ c (modulo r 8)) ,@s)))

(define rex.w        (^[s a t] `(:rex.w #t ,@s)))
(define (rex.b r)    (^[s a t] (if (>= r 8) `(:rex.b #t ,@s) s)))

(define (reg r)      (^[s a t] `(,@(if (>= r 8) `(:rex.r #t) '())
                                       :reg ,r ,@s)))

(define (mem params)
  (match params
    [`(addr ,a)          (mem-addr a)]
    [`(base ,b)          (mem-base b)]
    [`(label ,l)         (mem-label l)]
    [`(base+disp ,b ,d)  (mem-base+disp b d)]
    [`(sib ,b ,i ,s ,d)  (mem-sib b i s d)]))

(define (r/m-reg r)
  (^[s a t] `(,@(if (>= r 8) `(:rex.b #t) '()) :mode 3 :r/m ,r ,@s)))

(define (mem-addr a)
  (^[s a t] `(:mode 0 :r/m 4 :index 4 :base 5 :displacement ,(int8/32 a) ,@s)))

(define (mem-base b)
  (^[s a t]
    `(,@(if (>= b 8) `(:rex.b #t) '())
      ,@(case (modulo b 8)
          [(4) `(:mode 0 :r/m 4 :index 4 :base 4)]  ; we need to use SIB
          [(5) `(:mode 1 :r/m 5 :displacement (0))] ; we need to use disp8
          [else `(:mode 0 :r/m ,b)]                 ; we can use mode0
          )
      ,@s)))

;; we use RIP-relative addressing.
(define (mem-label l)
  (^[s a t]
    (if (and a t)
      (let1 laddr (assq-ref t l)
        (unless laddr (error "undefined label:" l))
        `(:mode 0 :r/m 5 :displacement ,(int32 (- laddr a)) ,@s))
      `(:mode 0 :r/m 5 :displacement ,(int32 0) ,@s))))

(define (mem-base+disp base disp)
  (^[s a t]
    `(,@(if (>= base 8) `(:rex.b #t) '())
      :mode ,(if (imm8? disp) 1 2)
      ,@(case (modulo base 8)
          [(4) `(:r/m 4 :index 4 :base 4 :displacement ,(int8/32 disp))]
          [else `(:r/m ,base :displacement ,(int8/32 disp))])
      ,@s)))

(define (mem-sib base index scale disp)
  (when (= index 4)
    (error "cannot use %rsp as an index register"))
  (^[s a t]
    `(,@(if (>= base 8) `(:rex.b #t) '())
      ,@(if (>= index 8) `(:rex.x #t) '())
      :mode ,(cond [(zero? disp) 0][(imm8? disp) 1][else 2])
      :r/m 4 :scale ,scale :index ,index :base ,base
      ,@(if (zero? disp) '() `(:displacement ,(int8/32 disp)))
      ,@s)))

(define (imm8 i)   (^[s a t] `(:immediate ,(int8 i) ,@s)))
(define (imm32 i)  (^[s a t] `(:immediate ,(int32 i) ,@s)))
(define (imm64 i)  (^[s a t] `(:immediate ,(int64 i) ,@s)))
      
(define *regs64*
  '(%rax %rcx %rdx %rbx %rsp %rbp %rsi %rdi
    %r8  %r9  %r10 %r11 %r12 %r13 %r14 %r15))

(define (reg64? opr) (memq opr *regs64*))
(define (regnum reg) (find-index (cut eq? reg <>) *regs64*))

(define (scale? n) (memv n '(1 2 4 8)))

(define (imm64? opr) (and (integer? opr) (<= (- (expt 2 63)) opr (- (expt 2 63) 1))))
(define (imm32? opr) (and (integer? opr) (<= (- (expt 2 31)) opr (- (expt 2 31) 1))))
(define (imm8? opr)  (and (integer? opr) (<= -128 opr 127)))


(define (int8 n)
  `(,(logand n #xff)))

(define (int32 n)
  (list (logand n #xff)
        (logand (ash n -8) #xff)
        (logand (ash n -16) #xff)
        (logand (ash n -24) #xff)))

(define (int64 n)
  (list (logand n #xff)
        (logand (ash n -8) #xff)
        (logand (ash n -16) #xff)
        (logand (ash n -24) #xff)
        (logand (ash n -32) #xff)
        (logand (ash n -40) #xff)
        (logand (ash n -48) #xff)
        (logand (ash n -56) #xff)))

(define (int8/32 n) (if (<= -128 n 127) (int8 n) (int32 n)))

