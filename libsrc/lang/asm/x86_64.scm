;;;
;;; x86_64 minimum assembler
;;;
;;;   Copyright (c) 2015-2025  Shiro Kawai  <shiro@acm.org>
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

;;
;; instructions.  This is not a full set assembler; we implement features
;; on demand.
;; TODO: Eventually we might want to split machine-independent front-end
;; to lang.asm.  The API will very likely change then.

(define-module lang.asm.x86_64
  (use gauche.uvector)
  (use gauche.sequence)
  (use binary.io)
  (use scheme.list)
  (use srfi.13)
  (use srfi.42)
  (use lang.asm.linker)
  (use lang.asm.regset)
  (use util.match)
  (export x86_64-asm x86_64-dump)
  )
(select-module lang.asm.x86_64)

;; Instruction notation
;;   (<opcode>)
;;   (<opcode> <operand1>)
;;   (<opcode> <operand1> <operand2>)
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
;;
;;  %reg, %base, %index : Symbol beginning with '%' to name a register.
;;  imm, off : An integer.  Some instructions have variations depending
;;     on the operand width, which is automatically handled.
;;  scale: 1, 2, 4, or 8
;;
;; For most operands, we require having data width suffix (e.g. movb for
;; byte, movq for quad).  Not all widths are supported.
;;
;; For jump instruction with label, %rip-relative addressing is used.
;; TODO: (mov LABEL %reg) may also use %rip-relative.  Since we don't
;; have linkers, the absolute address of LABEL will never be known.
;; (Or, we could add an on-memory linker...)
;;
;; Pseudo instructions
;;    (.section name)    begins named section.  By default code is
;;                       in 'text' section.  In the link phase,
;;                       sections with the same name are gathered.
;;    (.endsection name) ends the section.  This is for documenentation
;;                       purpose, so that section ending is clear
;;                       in the dumped assembly list.  An error is signaled
;;                       when name doesn't match the current section.
;;

;; Accumulates patch annotations during pass 2.  #f outside x86_64-asm;
;; bound to the growing patch list via parameterize inside x86_64-asm.
;; Updated by (push! (patch-collector) new-patch).
(define patch-collector (make-parameter #f))

;; Tracks the current section name (e.g. 'text, 'data) during pass 2.
;; #f outside x86_64-asm; set to 'text at entry and updated when a
;; (section sym) marker is encountered.
(define current-section-name (make-parameter #f))

;; xaddr :: section-name x addr -> xaddr
;;   An extended address that carries both the byte offset within the flat
;;   assembled byte vector and the section name it belongs to.  Used as
;;   the pass-1 accumulator and stored in label entries in the a-map.
;;   Deconstructed with match as (sec . addr).
(define (make-xaddr section addr) (cons section addr))

;;-----------------------------------------------------------------
;; Entry and x86 ISA definitions (subset)
;;

;; x86_64-asm :: [Insn] -> <obj-template>
;;   Assembles INSNS into an <obj-template>.  If the instruction list contains
;;   (.section sym) pseudo-instructions, the result has one <obj-fragment> per
;;   section region; otherwise a single 'text fragment is returned.
(define (x86_64-asm insns :key (locals '()))
  (let1 a-map (run-pass1 insns)
    (receive (bss patches)
        (parameterize ([patch-collector '()]
                       [current-section-name 'text])
          (let1 bss (run-pass2 a-map)
            (values bss (patch-collector))))
      (make-obj-template
       (build-fragments a-map (list->u8vector (concatenate bss)) patches locals)
       'little-endian
       8))))                                   ; stack-word-size for x86_64

;; run-pass1 :: [Insn] -> [(p, xaddr)]
;;   First pass. create an abstract mapping [(p, xaddr)], where
;;     p :: Symbol          ; label: (sym . xaddr)
;;        | (section sym)   ; .section marker: ((section sym) . xaddr)
;;        | (endsection sym)   ; .endsection marker: ((section sym) . xaddr)
;;        | (Int,[(Symbol,Int)]) -> [Byte] ; closure: (closure . xaddr)
;;   The accumulator is an xaddr (sec . addr).
;;   Labels are stored as (sym . xaddr) so pass2 can detect cross-section refs.
(define (run-pass1 insns)
  (filter
   values
   (values-ref
    (map-accum
     (match-lambda*
       [((? symbol? label) (and xaddr (sec . addr)))
        (values (cons label xaddr) xaddr)]
       [(('.section sym) (and xaddr (sec . addr)))
        (if (eq? sec sym)
          (values addr xaddr)            ;nop
          (values (cons (list 'section sym) addr) (make-xaddr sym addr)))]
       [(('.endsection sym) (and xaddr (sec . addr)))
        (unless (eq? sec sym)
          (error ".endsection doesn't match the current section: ~s"
                 `(.endsection ,sym)))
        (values (cons (list 'endsection sym) addr) (make-xaddr #f addr))]
       [(insn (sec . addr))
        (let* ([p     (asm1 (parse-insn insn))]
               [dummy (p addr #f)]
               [n     (length dummy)]
               [naddr (+ addr n)]
               ;; pass2 calls closures with end-addr, which is
               ;; already aligned for .align, yielding 0 bytes.
               ;; Freeze the pass-1 count so pass2 is consistent.
               [p2    (if (eq? (car insn) '.align)
                        (^[a t] (make-list n 0))
                        p)])
          (values (cons p2 naddr) (make-xaddr sec naddr)))])
     (make-xaddr 'text 0) insns)
    0)))

;; run-pass2 :: [(p, xaddr)] -> [[Byte]]
;;   Takes the result of first pass, calling the closure P to realize the
;;   actual machine codes.  Updates (current-section-name) when a (section sym)
;;   marker is encountered.
(define (run-pass2 a-map)
  (reverse (fold (^[p+addr seed]
                   (match-let1 (p . addr) p+addr
                     (match p
                       [(? symbol?) seed]  ; label: ignore
                       [('section s) (current-section-name s) seed]
                       [('endsection _) seed]
                       [_ (cons (p addr a-map) seed)])))
                 '() a-map)))

;; build-fragments :: a-map, u8vector, patches, declared-locals
;;                   -> [<obj-fragment>]
;;   Splits the flat assembled code into one fragment per section region.
;;   Section regions are derived from (section sym) markers in A-MAP; the
;;   implicit first region starts at offset 0 with section 'text.
;;   Labels and patches are filtered to their owning region and rebased to
;;   fragment-local offsets.
;;   declared-locals is a list of keywords (from x86_64-asm :locals argument);
;;   each fragment's locals slot is set to the intersection of declared-locals
;;   with the patch keywords present in that fragment (Step 3).
(define (build-fragments a-map code patches declared-locals)
  (let* ([total (uvector-size code)]
         ;; Collect section boundaries: ((sec-name . start-offset) ...)
         [sec-starts
          (cons (cons 'text 0)
                (filter-map (^p (and (pair? (car p))
                                     (eq? (caar p) 'section)
                                     (cons (cadar p) (cdr p))))
                            a-map))]
         ;; Build (sec-name start end) regions
         [regions
          (let loop ([ss sec-starts] [result '()])
            (if (null? (cdr ss))
              (reverse (cons (list (caar ss) (cdar ss) total) result))
              (loop (cdr ss)
                    (cons (list (caar ss) (cdar ss) (cdr (cadr ss)))
                          result))))])
    (map (^[region]
           (match-let1 (sec start end) region
             (let* ([frag-bytes (u8vector-copy code start end)]
                    [frag-labels
                     (filter-map
                      (^p (and (symbol? (car p))
                               (match (cdr p)
                                 [(fsec . addr)
                                  (and (eq? fsec sec)
                                       (cons (car p) (- addr start)))])))
                      a-map)]
                    [frag-patches
                     (filter-map
                      (^p (let1 off (cadr p)
                            (and (>= off start) (< off end)
                                 (match p
                                   [(kw o 'label-rel end-off)
                                    (list kw (- o start) 'label-rel
                                          (- end-off start))]
                                   [(kw o . rest)
                                    (cons* kw (- o start) rest)]))))
                      patches)]
                    ;; locals: declared-locals that appear as patch keywords
                    ;; in this fragment, preserving declared order.
                    [frag-locals
                     (filter (^[lkw] (any (^p (eq? (car p) lkw)) frag-patches))
                             declared-locals)])
               (make-obj-fragment frag-bytes frag-labels frag-patches sec
                                  frag-locals))))
         regions)))

;; x86_64-dump :: [Insn] -> ()
;;   For debugging.  Show the assembly results in human-readable way.
(define (x86_64-dump insns)
  (let* ([tmpl  (x86_64-asm insns)]
         [a-map (run-pass1 insns)]
         [bss   (parameterize ([current-section-name 'text])
                  (run-pass2 a-map))])
    (let loop ([insns insns] [a-map a-map] [bss bss])
      (unless (null? insns)
        (let* ([entry (car a-map)]
               [raw   (cdr entry)]
               ;; label entries have (addr . sec) as cdr; others have plain int
               [addr  (if (pair? raw) (car raw) raw)])
          (cond
           [(symbol? (car insns))   ; label
            (format #t "~4,'0x:~12a    ~a:\n" addr "" (car insns))
            (loop (cdr insns) (cdr a-map) bss)]
           [(and (pair? (car insns)) (eq? (caar insns) '.section))
            (format #t "     .section ~a\n" (cadar insns))
            (loop (cdr insns) (cdr a-map) bss)]  ; no bss entry for .section
           [(and (pair? (car insns)) (eq? (caar insns) '.endsection))
            (format #t "     .endsection ~a\n" (cadar insns))
            (loop (cdr insns) (cdr a-map) bss)]  ; no bss entry for .endsection
           [else
            (let1 byte-slices (slices (car bss) 4)
              (define (bytedump bytes)
                (with-output-to-string
                  (^[] (dolist [b bytes] (format #t " ~2,'0x" b)))))
              (format #t "~4,'0x:~12a          ~s\n"
                      addr
                      (bytedump (if (pair? byte-slices)
                                  (car byte-slices)
                                  '()))
                      (car insns))
              (when (pair? byte-slices)
                (dolist [bytes (cdr byte-slices)]
                  (format #t "    :~24a\n" (bytedump bytes))))
              (loop (cdr insns) (cdr a-map) (cdr bss)))]))))))

;; asm1 :: ParsedInsn -> (Int, [Symbol,Int]) -> [Byte]
;;  Called in pass 1.   The heart of instruction to machine code mapping.
;;  Returns a closure to generate the byte sequence of machine instructions.
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

    ;; trap
    [('int3)                       (! (opc #xcc))]
    [['endbr64]                    (! (opc '(#xf3 #x0f #x1e #xfa)))]

    ;; moving data around
    [`(movb (reg8 ,src)(reg8 ,dst))(! (opc #x88) (reg src) (r/m-reg dst))]
    [`(movb (reg8 ,src)(mem . ,x)) (! (opc #x88) (reg src) (mem x))]
    [`(movb (mem . ,x) (reg8 ,dst))(! (opc #x8a) (reg dst) (mem x))]
    [`(movb (imm8 ,i)  (reg8 ,dst))(! (opc+rq #xb0 dst) (imm8 i))]
    [`(movb (imm8 ,i)  (mem . ,x)) (! (opc #xc6) (reg 0) (mem x) (imm8 i))]

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

    [`(movsd (sse ,src) (sse ,dst)) (! (opc'(#xf2 #x0f #x10)) (reg dst) (r/m-reg dst))]
    [`(movsd (mem . ,x) (sse ,dst)) (! (opc'(#xf2 #x0f #x10)) (reg dst) (mem x))]
    [`(movsd (sse ,src) (mem . ,x)) (! (opc'(#xf2 #x0f #x11)) (reg src) (mem x))]

    ;; movs_ -- switchable movsd/movss: prefix byte (#xf2/#xf3) is patchable.
    ;; Default encoding uses movsd (#xf2); link-template flips to #xf3 for movss.
    [`((movs_ ,kw) (sse ,src) (sse ,dst))
     (op-movs_ kw (! (opc '(#xf2 #x0f #x10)) (reg dst) (r/m-reg dst)))]
    [`((movs_ ,kw) (mem . ,x) (sse ,dst))
     (op-movs_ kw (! (opc '(#xf2 #x0f #x10)) (reg dst) (mem x)))]
    [`((movs_ ,kw) (sse ,src) (mem . ,x))
     (op-movs_ kw (! (opc '(#xf2 #x0f #x11)) (reg src) (mem x)))]

    ;; calculations
    [('addq _ _)                   (op-addq pinsn 0)]
    [('orq _ _)                    (op-addq pinsn 1)]
    [('adcq _ _)                   (op-addq pinsn 2)]
    [('sbbq _ _)                   (op-addq pinsn 3)]
    [('andq _ _)                   (op-addq pinsn 4)]
    [('subq _ _)                   (op-addq pinsn 5)]
    [('xorq _ _)                   (op-addq pinsn 6)]
    [('cmpq _ _)                   (op-addq pinsn 7)]
    [('addl _ _)                   (op-addl pinsn 0)]
    [('orl _ _)                    (op-addl pinsn 1)]
    [('adcl _ _)                   (op-addl pinsn 2)]
    [('sbbl _ _)                   (op-addl pinsn 3)]
    [('andl _ _)                   (op-addl pinsn 4)]
    [('subl _ _)                   (op-addl pinsn 5)]
    [('xorl _ _)                   (op-addl pinsn 6)]
    [('cmpl _ _)                   (op-addl pinsn 7)]
    [('addw _ _)                   (op-addw pinsn 0)]
    [('orw _ _)                    (op-addw pinsn 1)]
    [('adcw _ _)                   (op-addw pinsn 2)]
    [('sbbw _ _)                   (op-addw pinsn 3)]
    [('andw _ _)                   (op-addw pinsn 4)]
    [('subw _ _)                   (op-addw pinsn 5)]
    [('xorw _ _)                   (op-addw pinsn 6)]
    [('cmpw _ _)                   (op-addw pinsn 7)]
    [('addb _ _)                   (op-addb pinsn 0)]
    [('orb _ _)                    (op-addb pinsn 1)]
    [('adcb _ _)                   (op-addb pinsn 2)]
    [('sbbb _ _)                   (op-addb pinsn 3)]
    [('andb _ _)                   (op-addb pinsn 4)]
    [('subb _ _)                   (op-addb pinsn 5)]
    [('xorb _ _)                   (op-addb pinsn 6)]
    [('cmpb _ _)                   (op-addb pinsn 7)]

    ;; shifts and rotation
    [('rol  _ _)                   (op-shift pinsn 0)]
    [('ror  _ _)                   (op-shift pinsn 1)]
    [('rcl  _ _)                   (op-shift pinsn 2)]
    [('rcr  _ _)                   (op-shift pinsn 3)]
    [('shl  _ _)                   (op-shift pinsn 4)]
    [('shr  _ _)                   (op-shift pinsn 5)]
    [('sar  _ _)                   (op-shift pinsn 7)]

    ;; inc and dec
    [('incq _)                     (op-incq pinsn 0)]
    [('decq _)                     (op-incq pinsn 1)]
    [('incl _)                     (op-incl pinsn 0)]
    [('decl _)                     (op-incl pinsn 1)]
    [('incw _)                     (op-incw pinsn 0)]
    [('decw _)                     (op-incw pinsn 1)]
    [('incb _)                     (op-incb pinsn 0)]
    [('decb _)                     (op-incb pinsn 1)]

    ;; FP conversion
    [`(cvtss2sd (sse ,src) (sse ,dst)) (! (opc '(#xf3 #x0f #x5a)) (reg dst) (r/m-reg src))]
    [`(cvtss2sd (mem . ,v) (sse ,dst)) (! (opc '(#xf3 #x0f #x5a)) (reg dst) (mem v))]
    [`(cvtsd2ss (sse ,src) (sse ,dst)) (! (opc '(#xf2 #x0f #x5a)) (reg dst) (r/m-reg src))]
    [`(cvtsd2ss (mem . ,v) (sse ,dst)) (! (opc '(#xf2 #x0f #x5a)) (reg dst) (mem v))]

    ;; embedded data -- placeholder forms (single keyword argument)
    [`(.datab (hole ,kw))          (data-hole kw 1)]
    [`(.datal (hole ,kw))          (data-hole kw 4)]
    [`(.dataq (hole ,kw))          (data-hole kw 8)]
    ;; embedded data -- literal forms
    [`(.datab ,(_ i) ...)          (^(a t) (append-map int8 i))]
    [`(.datal ,(_ i) ...)          (^(a t) (append-map int32 i))]
    [`(.dataq ,(_ i) ...)          (^(a t) (append-map int64 i))]
    [`(.datas (str ,s))            (^(a t) (fold-right cons '(0) (string->u8vector s)))]
    [`(.align ,(_ i))              (^(a t) (make-list (- (round-up a i) a) 0))]
    ))

;; jump family
(define (op-jump target short? opcode)
  (define immX   (if short? imm8 imm32))
  (define immX?  (if short? imm8? imm32?))
  (^[addr label-alist]
    (cond [(not label-alist)
           ((expand-spec (opc opcode) (immX 0)) 0 0)] ; dummy
          [(assq-ref label-alist target)
           => (^[v]
                (match-let1 (tsec . taddr) v
                 (let1 cur-sec (current-section-name)
                  (if (or (not cur-sec) (eq? tsec cur-sec))
                    ;; intra-section: bake in displacement
                    (begin
                      (unless (immX? (- taddr addr))
                        (error "jump target out of range:" target))
                      ((expand-spec (opc opcode) (immX (- taddr addr))) 0 0))
                    ;; cross-section
                    (if short?
                      (error "cross-section short jump not allowed:" target)
                      ;; near jump: emit zeros and record a label-rel patch
                      (begin
                        (when (patch-collector)
                          (push! (patch-collector)
                                 (list target (- addr 4) 'label-rel addr)))
                        ((expand-spec (opc opcode) (immX 0)) 0 0)))))))]
          [else (error "jump destination doesn't exist:" target)])))

;; special case to load label's address into a register.
;; NB: in most cases, absolute value of label address is irrelevant,
;; for we don't have linker that handles relocation.
(define (op-movlabel label dst)
  (define ! expand-spec)
  (define w rex.w)
  (^[addr label-alist]
    (if label-alist
      (let1 v (assq-ref label-alist label)
        (unless v (error "undefined label:" label))
        (match-let1 (_ . laddr) v
        (warn "Using absolute address of label may not what you want to do. \
               Consider using leaq label(%rip) instead.\n")
        ((! w (rex.b dst) (opc+rq #xb8 dst) (imm64 laddr)) 0 0))
      ((! w (rex.b dst) (opc+rq #xb8 dst) (imm64 0)) 0 0))))) ;; dummy

;; op-movs_ :: keyword, closure -> closure
;;   Wraps an inner expand-spec closure for a movsd instruction.  In pass 2,
;;   records a patch for 'x86_64-movs_ at the prefix byte offset.
;;   link-template dispatches to the registered x86_64-movs_ handler.
(define (op-movs_ kw inner)
  (^[a t]
    (let1 bytes (inner a t)
      (when (and t (patch-collector))
        (push! (patch-collector) (list kw (- a (length bytes)) 'x86_64-movs_)))
      bytes)))

;; addq family
;;  opcode variations are derived from a single number, regc.
(define (op-addq pinsn regc)
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

(define (op-addl pinsn regc)
  (define ! expand-spec)
  (define basc (+ (ash regc 3) 1))
  (define raxc (+ (ash regc 3) 5))
  (match pinsn
    [`(,_ (imm8 ,i)   (reg32 ,dst)) (! (opc #x83) (reg regc) (r/m-reg dst) (imm8 i))]
    [`(,_ (imm8 ,i)   (mem . ,x))   (! (opc #x83) (reg regc) (mem x) (imm8 i))]
    [`(,_ (imm32 ,i)  (reg32 ,dst)) (if (= dst 0) ; %eax
                                      (! (opc raxc) (imm32 i))
                                      (! (opc #x81) (reg regc) (r/m-reg dst) (imm32 i)))]
    [`(,_ (imm32 ,i)  (mem . ,x))   (! (opc #x81) (reg regc) (mem x) (imm32 i))]
    [`(,_ (reg32 ,src) (reg32 ,dst)) (! (opc basc) (reg src) (r/m-reg dst))]
    [`(,_ (reg32 ,src) (mem . ,x))   (! (opc basc) (reg src) (mem x))]
    [`(,_ (mem . ,x)  (reg32 ,dst))  (! (opc (+ basc 2)) (reg dst) (mem x))]
    ))

(define (op-addw pinsn regc)
  (define ! expand-spec)
  (define basc (+ (ash regc 3) 1))
  (define raxc (+ (ash regc 3) 5))
  (match pinsn
    [`(,_ (imm8 ,i)   (reg16 ,dst)) (! (opc '(#x66 #x83)) (reg regc) (r/m-reg dst) (imm8 i))]
    [`(,_ (imm8 ,i)   (mem . ,x))   (! (opc '(#x66 #x83)) (reg regc) (mem x) (imm8 i))]
    [`(,_ (imm32 ,i)  (reg16 ,dst)) (if (= dst 0) ; %ax
                                      (! (opc (list #x66 raxc)) (imm16 i))
                                      (! (opc '(#x66 #x81)) (reg regc) (r/m-reg dst) (imm16 i)))]
    [`(,_ (imm32 ,i)  (mem . ,x))   (! (opc '(#x66 #x81)) (reg regc) (mem x) (imm16 i))]
    [`(,_ (reg16 ,src) (reg16 ,dst)) (! (opc (list #x66 basc)) (reg src) (r/m-reg dst))]
    [`(,_ (reg16 ,src) (mem . ,x))   (! (opc (list #x66 basc)) (reg src) (mem x))]
    [`(,_ (mem . ,x)  (reg16 ,dst))  (! (opc (list #x66 (+ basc 2))) (reg dst) (mem x))]
    ))

(define (op-addb pinsn regc)
  (define ! expand-spec)
  (define b0c (ash regc 3))
  (define b2c (+ (ash regc 3) 2))
  (define b4c (+ (ash regc 3) 4))
  (match pinsn
    [`(,_ (imm8 ,i)  (reg8 ,dst))   (if (= dst 0) ; %al
                                      (! (opc b4c) (imm8 i))
                                      (! (opc #x80) (reg regc) (r/m-reg dst) (imm8 i)))]
    [`(,_ (imm8 ,i)  (mem . ,x))    (! (opc #x80) (reg regc) (mem x) (imm8 i))]
    [`(,_ (reg8 ,src) (reg8 ,dst))   (! (opc b0c) (reg src) (r/m-reg dst))]
    [`(,_ (reg8 ,src) (mem . ,x))    (! (opc b0c) (reg src) (mem x))]
    [`(,_ (mem . ,x) (reg8 ,dst))    (! (opc b2c) (reg dst) (mem x))]
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
(define (op-incq pinsn regc)
  (define ! expand-spec)
  (define w rex.w)
  (match pinsn
    [`(,_ (reg ,r))             (! w (opc #xff) (reg regc) (r/m-reg r))]
    [`(,_ (mem . ,x))           (! w (opc #xff) (reg regc) (mem x))]
    ))

(define (op-incl pinsn regc)
  (define ! expand-spec)
  (match pinsn
    [`(,_ (reg32 ,r))           (! (opc #xff) (reg regc) (r/m-reg r))]
    [`(,_ (mem . ,x))           (! (opc #xff) (reg regc) (mem x))]
    ))

(define (op-incw pinsn regc)
  (define ! expand-spec)
  (match pinsn
    [`(,_ (reg16 ,r))           (! (opc '(#x66 #xff)) (reg regc) (r/m-reg r))]
    [`(,_ (mem . ,x))           (! (opc '(#x66 #xff)) (reg regc) (mem x))]
    ))

(define (op-incb pinsn regc)
  (define ! expand-spec)
  (match pinsn
    [`(,_ (reg8 ,r))            (! (opc #xfe) (reg regc) (r/m-reg r))]
    [`(,_ (mem . ,x))           (! (opc #xfe) (reg regc) (mem x))]
    ))

;; data-hole :: keyword, int -> closure
;;   Returns a closure for a data-directive placeholder.  Emits WIDTH zero
;;   bytes and (in pass 2) records a patch annotation into patch-collector.
(define (data-hole kw width)
  (^[a t]
    (when (and t (patch-collector))
      (push! (patch-collector) (list kw (- a width) width)))
    (make-list width 0)))

;; for align
(define (round-up addr modu)
  (* (quotient (+ addr modu -1) modu) modu))

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
    [(? regsse?)                    `(sse ,(ssenum opr))]
    [(? reg8?)                      `(reg8 ,(regnum8 opr))]
    [(? reg16?)                     `(reg16 ,(regnum16 opr))]
    [(? reg32?)                     `(reg32 ,(regnum32 opr))]
    [(? keyword? kw)                `(hole ,kw)]
    [(? symbol?)                    `(label ,opr)]
    [(? string?)                    `(str ,opr)] ; C string literal
    [(? imm8?)                      `(imm8 ,opr)]
    [(? imm32?)                     `(imm32 ,opr)]
    [(? imm64?)                     `(imm64 ,opr)]
    [('imm8 val)                    `(imm8 ,val)]
    [('imm16 val)                   `(imm16 ,val)]
    [('imm32 val)                   `(imm32 ,val)]
    [('imm64 val)                   `(imm64 ,val)]
    [((? integer? a))               `(mem addr ,a)]
    [((? reg64? b))                 `(mem base ,(regnum b))]
    [((? symbol? l))                `(mem label ,l)]
    [((? keyword? kw) (? reg64? b)) `(mem base+kw ,(regnum b) ,kw)]
    [((? integer? d) (? reg64b? b)) `(mem base+disp ,(regnumb b) ,d)]
    [((? symbol? l) (? reg64b? b))  `(mem base+disp ,(regnumb b) label ,l)]
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
    ;; REX must come immediately before the opcode, after any
    ;; mandatory legacy prefix (0x66/0xF2/0xF3).  Peel those prefix
    ;; bytes off the front of the opcode list and emit them ahead of
    ;; REX.
    (let* ([opcs   (if (list? opcode) opcode `(,opcode))]
           [legacy (take-while (^b (memv b '(#x66 #xf2 #xf3))) opcs)]
           [body   (drop opcs (length legacy))])
      (cond-list
       [#t  @ legacy]
       [(or rex.w rex.r rex.x rex.b) (pack-rex rex.w rex.r rex.x rex.b)]
       [#t  @ body]
       [(or mode reg r/m) (pack-modrm mode reg r/m)]
       [(or index base) (pack-sib scale index base)]
       [displacement @]
       [immediate @]))))

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
    [`(addr ,a)               (mem-addr a)]
    [`(base ,b)               (mem-base b)]
    [`(label ,l)              (mem-label l)]
    [`(base+disp ,b ,d)       (mem-base+disp b d)]
    [`(base+disp ,b label ,l) (mem-base+disp-label b l)]
    [`(base+kw ,b ,kw)        (mem-base+kw b kw)]
    [`(sib ,b ,i ,s ,d)       (mem-sib b i s d)]))

(define (r/m-reg r)
  (^[s a t] `(,@(if (>= r 8) `(:rex.b #t) '()) :mode 3 :r/m ,r ,@s)))

(define (mem-addr memaddr)
  (^[s a t] `(:mode 0 :r/m 4 :index 4 :base 5 :displacement ,(int32 memaddr) ,@s)))

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
      (let* ([v       (assq-ref t l)]
             [cur-sec (current-section-name)])
        (unless v (error "undefined label:" l))
        (match-let1 (lsec . laddr) v
        (if (or (not cur-sec) (eq? lsec cur-sec))
          ;; intra-section: bake in RIP-relative displacement
          `(:mode 0 :r/m 5 :displacement ,(int32 (- laddr a)) ,@s)
          ;; cross-section: emit zero, record label-rel patch
          (begin
            (when (patch-collector)
              (push! (patch-collector) (list l (- a 4) 'label-rel a)))
            `(:mode 0 :r/m 5 :displacement ,(int32 0) ,@s)))))
      `(:mode 0 :r/m 5 :displacement ,(int32 0) ,@s))))

(define (mem-base+disp base disp)
  (^[s a t]
    `(,@(if (and (number? base) (>= base 8)) `(:rex.b #t) '())
      :mode ,(if (number? base) (if (imm8? disp) 1 2) 0)
      ,@(case base
          [(%rip) `(:r/m 5 :displacement ,(int32 disp))]
          ;; %rsp (regnum 4) and %r12 (regnum 12) require a SIB byte
          ;; even with no index — their r/m=4 encoding signals "SIB
          ;; follows".  Encode SIB as scale=1 / index=4 (no index)
          ;; / base=4 (low 3 bits of the regnum); REX.B above
          ;; distinguishes %rsp from %r12.
          [(4 12) `(:r/m 4 :scale 1 :index 4 :base 4
                         :displacement ,(int8/32 disp))]
          [else `(:r/m ,base :displacement ,(int8/32 disp))])
      ,@s)))

(define (mem-base+disp-label base l)
  (^[s a t]
    (if (and a t)
      (let1 v (assq-ref t l)
        (unless v (error "undefined label:" l))
        (match-let1 (_ . laddr) v
          ((mem-base+disp base (- laddr a)) s a t)))
      ((mem-base+disp base 0) s a t))))

;; mem-base+kw :: regnum, keyword -> modifier
;;   Emits mod=10 (32-bit displacement) addressing for (:kw %reg).
;;   Always uses SIB for %rsp/%r12 (base mod 8 = 4); plain r/m otherwise.
;;   Records a 4-byte patch at (- a 4), which is the displacement field when
;;   this instruction has no immediate operand following (the common case for
;;   load/store instructions).
(define (mem-base+kw base kw)
  (^[s a t]
    (when (and t (patch-collector))
      (push! (patch-collector) (list kw (- a 4) 4)))
    `(,@(if (>= base 8) `(:rex.b #t) '())
      :mode 2
      ,@(case (modulo base 8)
          [(4) `(:r/m 4 :index 4 :base ,base)]  ; %rsp/%r12 needs SIB
          [else `(:r/m ,base)])
      :displacement (0 0 0 0)
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

;; imm8, imm16, imm32, imm64 -- immediate-value modifiers.
;;   When I is a keyword (:name), it is a placeholder: emit zeros of the
;;   appropriate width and record a patch annotation in patch-collector.
;;   The patch is recorded only in pass 2 (T is non-#f there).
(define (imm8 i)
  (^[s a t]
    (when (and (keyword? i) t (patch-collector))
      (push! (patch-collector) (list i (- a 1) 1)))
    `(:immediate ,(if (keyword? i) '(0) (int8 i)) ,@s)))

(define (imm16 i)
  (^[s a t]
    (when (and (keyword? i) t (patch-collector))
      (push! (patch-collector) (list i (- a 2) 2)))
    `(:immediate ,(if (keyword? i) '(0 0) (int16 i)) ,@s)))

(define (imm32 i)
  (^[s a t]
    (when (and (keyword? i) t (patch-collector))
      (push! (patch-collector) (list i (- a 4) 4)))
    `(:immediate ,(if (keyword? i) '(0 0 0 0) (int32 i)) ,@s)))

(define (imm64 i)
  (^[s a t]
    (when (and (keyword? i) t (patch-collector))
      (push! (patch-collector) (list i (- a 8) 8)))
    `(:immediate ,(if (keyword? i) '(0 0 0 0 0 0 0 0) (int64 i)) ,@s)))


(define-constant *regs8*  '(%al %cl %dl %bl %ah %ch %dh %bh))
(define-constant *regs16* '(%ax %cx %dx %bx %sp %bp %si %di))
(define-constant *regs32* '(%eax %ecx %edx %ebx %esp %ebp %esi %edi))
(define-constant *regs64* '(%rax %rcx %rdx %rbx %rsp %rbp %rsi %rdi
                            %r8  %r9  %r10 %r11 %r12 %r13 %r14 %r15))

;; reg8 - legacy general 8bit registers
(define (reg8? opr) (memq opr *regs8*))
(define (regnum8 reg) (find-index (cut eq? reg <>) *regs8*))

;; reg16 - legacy general 16bit registers
(define (reg16? opr) (memq opr *regs16*))
(define (regnum16 reg) (find-index (cut eq? reg <>) *regs16*))

;; reg32 - general 32bit registers
(define (reg32? opr) (memq opr *regs32*))
(define (regnum32 reg) (find-index (cut eq? reg <>) *regs32*))

;; reg64 - general 64bit registers.  Can be source/dest registers.
;; reg64b - registers that can be used as the 'base' register of modrm.
;;          this includes %rip plus reg64.
;; regnum - arg must be reg64.  returns the register number.
;; regnumb - arg must be reg64b.  returns register number or '%rip.

(define (reg64? opr) (memq opr *regs64*))
(define (reg64b? opr) (or (reg64? opr) (eq? opr '%rip)))
(define (regnum reg) (find-index (cut eq? reg <>) *regs64*))
(define (regnumb reg) (or (regnum reg) reg))

(define-constant *regsse*
  '(%xmm0 %xmm1 %xmm2 %xmm3 %xmm4 %xmm5 %xmm6 %xmm7))

;; to pass to regset
(define-constant *x64-regvec* (list->vector (append *regs64* *regsse*)))

(define (regsse? opr) (memq opr *regsse*))
(define (ssenum reg) (find-index (cut eq? reg <>) *regsse*))

(define (scale? n) (memv n '(1 2 4 8)))

(define (imm64? opr) (and (integer? opr) (<= (- (expt 2 63)) opr (- (expt 2 63) 1))))
(define (imm32? opr) (and (integer? opr) (<= (- (expt 2 31)) opr (- (expt 2 31) 1))))
(define (imm16? opr) (and (integer? opr) (<= -32768 opr 32767)))
(define (imm8? opr)  (and (integer? opr) (<= -128 opr 127)))


(define (int8 n)
  `(,(logand n #xff)))

(define (int16 n)
  (list (logand n #xff)
        (logand (ash n -8) #xff)))

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
