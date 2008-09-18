(define-module gauche.vm.jit
  (use gauche.uvector)
  (use gauche.sequence)
  (use gauche.parameter)
  (use gauche.experimental.ref)
  (use gauche.experimental.lamb)
  (use srfi-1)
  (use srfi-13)
  (use srfi-42)
  (use util.list)
  (use util.match)
  (export-all)                          ;for now
  )
(select-module gauche.vm.jit)

(define vm-code->list
  (with-module gauche.internal vm-code->list))

;; If the procedure doesn't create a closure and/or local env,
;; we can keep the procedure's arguments in registers instead of
;; wrapping the arguments by a new env.
(define args-regs (make-parameter 0))

;; 'Leaf environment' is true if the procedure does not use LREF(dep,off)
;; where dep > 0,  does not create an new environment, and does not create
;; a closure.  In that case we can omit keeping ENV in the register.
(define leaf-env? (make-parameter #f))

(define (jit closure)
  (define (make-label addr)
    (string->symbol #`"label_,addr"))
  (define (finish jits)
    (let1 zz (concatenate (reverse jits))
      (print ">>>")
      (for-each (cut print "  " <>) zz)
      (rlet1 yy (list->u8vector (asm zz))
        (format #t "code = 0x~x\n" (%code-address yy)))))
  (define (compile jits code ends addr)
    (if (or (null? code) (memv addr ends))
      (finish jits)
      (match code
        [`((CONSTI ,n) . ,rest)
         (compile `(,(jit-consti n) (,(make-label addr)) ,@jits)
                  rest ends (+ addr 1))]
        [`((CONSTI-PUSH ,n) . ,rest)
         (compile `(,(jit-consti-push n) (,(make-label addr)) ,@jits)
                  rest ends (+ addr 1))]
        [`((PUSH) . ,rest)
         (compile `(,(jit-push-val0) (,(make-label addr)) ,@jits)
                  rest ends (+ addr 1))]
        [`((LREF-VAL0-BNLT ,depth ,off) ,dest . ,rest)
         (compile `(,(jit-lref-val0-bnlt depth off (make-label dest))
                    (,(make-label addr)) ,@jits)
                  rest ends (+ addr 2))]
        [`((RET) . ,rest)
         (compile `(,(jit-ret) (,(make-label addr)) ,@jits) rest ends (+ addr 1))]
        [`((PRE-CALL ,nargs) ,dest . ,rest)
         (let* ((tail (list-tail code (- dest addr 1)))
                (sub  (compile `(,(jit-cc-prologue)) (cdr tail) ends dest)))
           (compile `(,(jit-pre-call (%code-address sub))
                      (,(make-label addr)) ,@jits)
                    rest (cons dest ends) (+ addr 2)))]
        [`((LREF0) . ,rest)
         (compile `(,(jit-lref 0 0) (,(make-label addr)) ,@jits)
                  rest ends (+ addr 1))]
        [`((NUMADDI ,n) . ,rest)
         (compile `(,(jit-numaddi n) (,(make-label addr)) ,@jits)
                  rest ends (+ addr 1))]
        [`((GREF-CALL ,nargs) ,id/gloc . ,rest)
         (compile `(,(jit-call nargs) ,(jit-gref id/gloc)
                    (,(make-label addr)) ,@jits)
                  rest ends (+ addr 2))]
        [`((PUSH-GREF-CALL ,nargs) ,id/gloc . ,rest)
         (compile `(,(jit-call nargs) ,(jit-gref id/gloc) ,(jit-push-val0)
                    (,(make-label addr)) ,@jits)
                  rest ends (+ addr 2))]
        [`((PUSH-PRE-CALL ,nargs) ,dest . ,rest)
         (let* ((tail (list-tail code (- dest addr 1)))
                (sub  (compile `(,(jit-cc-prologue)) (cdr tail) ends dest)))
           (compile `(,(jit-pre-call (%code-address sub)) ,(jit-push-val0)
                      (,(make-label addr)) ,@jits)
                    rest (cons dest ends) (+ addr 2)))]
        [`((NUMADD2) . ,rest)
         (compile `(,(jit-numadd2) (,(make-label addr)) ,@jits)
                  rest ends (+ addr 1))]
        [(vminsn . rest)
         (error "JIT is not implemented yet for the VM Instruction:" vminsn)])
      ))
  
  (unless (zero? (%closure-env closure))
    (error "Closures with non-null environment are not supported yet."))
  (when (ref closure'optional)
    (error "Closures with optional args are not supported yet."))
  (%make-native (compile `(,(jit-prologue (ref closure'required))) 
                         (vm-code->list (closure-code closure))
                         '() 0)
                (ref closure'required)
                (ref closure'optional)
                `(jit-compiled ,(ref closure'info))))

;;;
;;;  JIT assembly segments
;;;


;; Memo:
;;                                  subr  ccont
;;   reg     x86-64 ABI    our JIT  args  args
;;  +-----+---------------+-------+------+---------
;;   %rax    return value  
;;   %rbx    callee saved    ENV
;;   %rcx    4th arg
;;   %rdx    3rd arg               *info
;;   %rsi    2nd arg                argc  **data
;;   %rdi    1st arg               *argv  result
;;   %rbp    callee saved    VM
;;   %rsp    stack ptr
;;   %r8     5th arg
;;   %r9     6th arg
;;   %r10    callee saved    SP
;;   %r11    linking
;;   %r12    static link?
;;   %r13    callee saved
;;   %r14    callee saved
;;   %r15    callee saved    arg0

(define-syntax &
  (syntax-rules ()
    [(_ name) (get-addr 'name)]))

;; Prologue
;;   * If the routine takes arguments, it is pushed above SP, so we
;;     have to turn them into a new env frame to protect them.
;;   * Save the callee saved regs and set up some fixed-purpose regs.
;; Note:
;;  In this experiment, the VM calling routine feeds a pointer to
;;  the VM in %rdx.  It might be changed later.
;;  The environment in which the original closure is defined should
;;  be provided somehow.  Probably we'd create a new procedure type
;;  so that VM pointer and ENV info can be passed efficiently.  For
;;  the time being, we assume the closure has empty environment.

(define (jit-prologue nargs)
  (cond
   [(zero? nargs)
    `(,@(cond-list [(not (leaf-env?)) '(push %rbx)])
      (push %rbp)
      (push %r10)
      (movq %rdx %rbp)                   ; rbp = VM
      ,@(jit-null-env)                   ; rbx = NULL (env)
      (movq (,(vm-reg "sp") %rbp) %r10)  ; r10 = SP
      )]
   [(= (args-regs) 1)
    `(,@(cond-list [(not (leaf-env?)) '(push %rbx)])
      (push %rbp) (push %r10) (push %r15)
      (movq %rdx %rbp)                   ; rbp = VM
      ,@(jit-null-env)                   ; rbx = NULL (env)
      (movq (,(vm-reg "sp") %rbp) %r10)  ; r10 = SP
      (movq (%rdi) %r15)                 ; save arg0
      )]
   [else
    `(;; SP = SP + nargs*sizeof(ScmObj) + ENV_HDR_SIZE
      ;; newenv = SP - ENV_HDR_SIZE
      ;; newenv->size = nargs
      ;; newenv->info = SCM_FALSE
      ;; newenv->up   = NULL
      ;; ENV = newenv
      (push %rbx)
      (push %rbp) (push %r10)
      (movq %rdx %rbp)                   ; rbp = VM
      (movq (,(vm-reg "sp") %rbp) %r10)  ; r10 = SP
      (leaq (%r10 %rsi 8) %rbx)          ; rbx = newenv
      (xorq %rax %rax)                   ;
      (movq %rax (%rbx))                 ; newenv->up = NULL
      (movq 6    (8 %rbx))               ; newenv->info = SCM_FALSE
      (movq %rsi (16 %rbx))              ; newenv->size = nargs
      (leaq (24 %rbx) %r10)              ; SP = newenv + ENV_HDR_SIZE
      )]
   ))

;; Prologue for C-continuation.
(define (jit-cc-prologue)
  `(,@(cond-list [(not (leaf-env?)) '(push %rbx)])
    (push %rbp) (push %r10)
    (movq %rdi %rax)                    ; VAL0
    (movq (%rsi) %rbp)                  ; VM
    ,@(jit-load-env)                    ; ENV
    (movq (,(vm-reg "sp") %rbp) %r10)   ; SP
    ,@(cond-list
       [(= (args-regs) 1) @ '((push %r15) (movq (8 %rsi) %r15))])
    ))

(define (jit-pre-call ccont)
  ;; expands VMPushCC inline
  `((movq (,(vm-reg"cont") %rbp) %rdi)  ; rdi = CONT
    (movq ,ccont %rax)
    ,(if (leaf-env?)                    ; cc->env = ENV
       '(movq 0 (8 %r10))
       '(movq %rbx (8 %r10)))
    (movq %rdi (%r10))                  ; cc->prev = CONT
    (movq (,(vm-reg"base") %rbp) %rdi)  ; rdi = BASE
    (movq 0 (16 %r10))                  ; cc->argp = NULL
    (movq ,(+ 1 (args-regs)) (24 %r10)) ; cc->size = 1 + (args-regs)
    (movq %rdi (40 %r10))               ; cc->base = base
    (movq %rax (32 %r10))               ; cc->pc = ccont
    (movq %rbp (48 %r10))               ; data[0] = VM
    ,@(cond-list
       [(= (args-regs) 1) '(movq %r15 (56 %r10))]) ;data[1] = arg0
    (movq %r10 (,(vm-reg"cont")%rbp))   ; CONT = cc
    (addq ,(+ 56 (* (args-regs) 8)) %r10)
    (movq %r10 (,(vm-reg"sp")%rbp))     ; SP += CONT_FRAME_SIZE + datasize
    (movq %r10 (,(vm-reg"argp")%rbp))   ; ARGP = SP
    ))

(define (jit-call nargs)
  (case nargs
    [(0) (jit-call0)]
    [(1) (jit-call1)]
    [else (error "call with more than two arguments are not supported yet")]))

(define (jit-call1)
  (let ((FALLBACK (gensym)))
    `((movq %rax %rcx)
      (andq 3 %rcx)
      (jnz ,FALLBACK)
      (movq (%rax) %r8)                   ; *VAL0
      (movq ,(+ (& Scm_ProcedureClass) 3) %r9) ;
      (cmpq %r8 %r9)                      ; SCM_XTYPEP(VAL0, SCM_CLASS_PROCEDURE)
      (jne ,FALLBACK)
      (movzbq (#x11 %rax) %r8)            ; ((ScmSubr*)VAL0)->common.type
      (shr 3 %r8)
      (andq 7 %r8)                        ;   == SCM_PROC_SUBR?
      (jnz ,FALLBACK)
      (movzwq (#x10 %rax) %r8)            ; ((ScmSubr*)VAL0)->common.required
      (andq #x3ff %r8)
      (cmpq 1 %r8)
      (jne ,FALLBACK)
      (movq (#x38 %rax) %rdx)             ; arg2 = ((ScmSubr*)VAL0)->data
      (movq 1 %rsi)                       ; arg1 = 1
      (addq -8 %r10)                      ; SP--
      (movq %r10 %rdi)                    ; arg0 = SP
      (movq %r10 (,(vm-reg "sp") %rbp))   ; save SP
      ,@(jit-save-env)                    ; save ENV
      ,@(jit-popregs)
      (jmp (#x30 %rax))                   ; ((ScmSubr*)VAL0)->func(...)
      ,FALLBACK
      (addq -8 %r10)                      ; SP--
      (movq %rax %rdi)                    ; arg0
      (movq (%r10) %rsi)                  ; arg1
      (movq %r10 (,(vm-reg "sp") %rbp))   ; save SP
      ,@(jit-save-env)                    ; save ENV
      (movq ,(& Scm_VMApply1) %rax)
      ,@(jit-popregs)
      (jmp %rax)                          ; Scm_VMApply1(VAL0, *SP--)
      )))

(define (jit-call0)
  `((movq %rax %rdi)                    ; arg0
    (movq %r10 (,(vm-reg "sp") %rbp))   ; save SP
    ,@(jit-save-env)                    ; save ENV
    (movq ,(& Scm_VMApply0) %rax)       ;
    ,@(jit-popregs)
    (jmp %rax)                         ; Scm_VMApply0(VAL0)
    ))

(define (jit-store-val0)
  `((movq %rax (,(vm-reg "val0") %rbp))))

(define (jit-load-val0)
  `((movq (,(vm-reg "val0") %rbp) %rax)))

(define (jit-push-val0)
  `((movq %rax (%r10))                  ; *SP = rax
    (addq 8 %r10)))                     ; SP++

(define (jit-consti n)
  `((movq ,(make-int n) %rax)))

(define (jit-consti-push n)
  `((movq ,(make-int n) (%r10))
    (addq 8 %r10)))

(define (jit-popregs)
  `(,@(cond-list [(= (args-regs) 1) '(pop %r15)])
    (pop %r10)
    (pop %rbp)
    ,@(cond-list [(not (leaf-env?)) '(pop %rbx)])))

(define (jit-load-env)
  (cond-list [(not (leaf-env?)) `(movq (,(vm-reg "env") %rbp) %rbx)]))

(define (jit-null-env)
  (cond-list [(not (leaf-env?)) `(xorq %rbx %rbx)]))

(define (jit-save-env)
  (cond-list [(not (leaf-env?)) `(movq %rbx (,(vm-reg "env") %rbp))]))

(define (jit-ret)
  `(,@(jit-popregs)
    (ret)))

(define (jit-lref depth offset)
  (cond
   [(= (args-regs) 1)
    (cond [(zero? depth)
           (unless (zero? offset) (error "bogus LREF"))
           '((movq %r15 %rax))]
          [else
           (when (leaf-env?) (error "bad LREF with leaf-env?"))
           `((movq %rbx %rax)               ; e = ENV
             ,@(list-ec (: i (- depth 1))
                        '(movq (%rax) %rax)) ; e = e->up
             (movq (,(* (- -1 offset) 8) %rax) %rax) ; %rax = ENV_DATA(e, off)
             )])]
   [else
    ;; normal way
    `((movq %rbx %rax)               ; e = ENV
      ,@(list-ec (: i depth)
                 '(movq (%rax) %rax)) ; e = e->up
      (movq (,(* (- -1 offset) 8) %rax) %rax) ; %rax = ENV_DATA(e, off)
      )]))

(define (jit-gref gloc)
  (let ((END (gensym))
        (MSG (gensym))
        (gloc (cond [(symbol? gloc)
                     ((with-module gauche.internal find-binding)
                      (current-module) gloc #f)] ;; for test
                    [(identifier? gloc)
                     ((with-module gauche.internal find-binding)
                      (ref gloc'module) (ref gloc'name) #f)]
                    [(is-a? gloc <gloc>)
                     gloc]
                    [else (error "gref requires identifier or gloc:" gloc)])))
    `((movq ,(%address-of gloc) %rcx)
      (movq (24 %rcx) %rax)             ; rax = gloc->value
      (cmpq #x56 %rax)                  ; if (SCM_UNBOUNDP(rax))
      (jne ,END)
      (movq ,MSG %rdi)
      (movq (8 %rcx) %rsi)
      (movq ,(& Scm_Error) %rax)        ; Scm_Error("...", gloc->name);
      (call %rax)
      (jmp ,END)
      ,MSG
      (datas "unbound variable: %S")
      ,END)))

(define (jit-numaddi n)
  (let ((NON-INT (gensym))
        (END     (gensym)))
    `(
      (movq %rax %rdi)                  ; rdi = rax = VAL0
      (andq 3 %rax)                     ; if (INTP(VAL0))
      (cmpq 1 %rax)                     ;
      (jne ,NON-INT)                    ;
      ;; We add fixnums unshifted; overflow can be detected by OF.
      (addq ,(ash n 2) %rdi)            ; rdi += n
      (movq %rdi %rax)                  ; result = rdi
      (jno ,END)                        ; if not overflow, nothing to do
      (rcr 1 %rdi)                      ; pull carry flag
      (sar 1 %rdi)                      ; one more shift
      (movq ,(& Scm_MakeInteger) %rax)  ; make bugnum.
      (call %rax)                       ;
      (jmp ,END)                        ;
      ,NON-INT                          ; if VAL0 is not a fixnum,
      (movq ,(make-int n) %rsi)         ;  rsi = MAKE_INT(n)
      (movq ,(& Scm_Add) %rax)          ;  Scm_Add(rdi, rsi)
      (call %rax)                       ;
      ,END
      )))

(define (jit-numadd2)
  (let ((NON-INT (gensym))
        (END (gensym)))
    `((movq %rax %rsi)                    ; rsi(arg1) = VAL0
      (movq (-8 %r10) %rdi)               ; rdi(arg0) = *SP
      (andq 3 %rax)
      (subq 8 %r10)                       ; SP--
      (cmpq 1 %rax)
      (jne ,NON-INT)
      (movq %rdi %rax)
      (andq 3 %rax)
      (cmpq 1 %rax)
      (jne ,NON-INT)
      ;; We add fixnums unshifted; overflow can be detected by OF.
      (subq 1 %rsi)
      (addq %rsi %rdi)
      (movq %rdi %rax)
      (jno ,END)
      (rcr 1 %rdi)
      (sar 1 %rdi)
      (movq ,(& Scm_MakeInteger) %rax)
      (call %rax)
      (jmp ,END)
      ,NON-INT
      (movq ,(& Scm_Add) %rax)            ; Scm_Add(*SP--, VAL0)
      (call %rax)
      ,END
      )))

(define (jit-lref-val0-bnlt depth off label)
  (let ((L1 (gensym))
        (L2 (gensym))
        (G  (gensym))
        (END (gensym)))
    `((movq %rax %r8)                   ; save VAL0
      ,@(jit-lref depth off)            ; rax = LREF(depth,off)
      (movq %rax %r9)                   ; save rax
      ;; Now what we do is "if (!(%r9 < %r8)) goto label".
      ;; we shortcut if both args are fixnums, or both are flonums.
      (andq 3 %rax)
      (jz ,L1)
      (cmpq 1 %rax)
      (jne ,G)
      (movq %r8 %rax)
      (andq 3 %rax)
      (cmpq 1 %rax)
      (jne ,G)
      ;; both are fixnums.  we don't bother to shift tag bits.
      (cmpq %r8 %r9)                    ;
      (movq 6 %rax)                     ; val0 = #f
      (jnl ,label)                      ; branch
      (jmp ,END)
      ;; %r9 is heap object.  Is it flonum?
      ,L1
      (movq (%r9) %rcx)                 ;
      (movq ,(+ (& Scm_RealClass) 3) %rdx)
      (cmpq %rdx %rcx)                  ; SCM_XTYPEP(%r9, SCM_CLASS_REAL)
      (jne ,G)
      ;; how about %r8?
      (movq %r8 %rax)
      (andq 3 %rax)
      (jnz ,G)
      (movq (%r8) %rcx)
      (cmpq %rdx %rcx)                  ; SCM_XTYPEP(%r8, SCM_CLASS_REAL)
      (jne ,G)
      ;; now both are flonums.
      ;;  for now, we fall back to generic
      ;; generic handling.
      ,G
      (movq %r9 %rdi)                   ; arg0 = r9
      (movq %r8 %rsi)                   ; arg1 = r8
      (movq ,(& Scm_NumCmp) %rax)       ; r = Scm_NumCmp(%r9, %r8)
      (call %rax)
      (cmpq 0 %rax)                     ; if (r >= 0) goto label
      (movq 6 %rax)                     ; val0 = #f
      (jge ,label)
      ,END
      (movq #x16 %rax)                  ; VAL0 = #t
      )))

(define (make-int n) (+ (ash n 2) 1))

;;;
;;;  Low-level info access
;;;

(define (vm-reg name) (values-ref (%vm-access-info name) 0))

(define (get-addr name)                        ; get address of the symbol
  (rlet1 r (%get-addr (string-drop (x->string name) 4))
    (when (zero? r) (error "Address is unknown for" name))))

(inline-stub
 "#include <gauche/uvector.h>"

 "extern ScmObj Scm_MakeNative(void*, int, int, ScmObj);"
 "extern ScmObj Scm_VMAccessInfo(const char*);"
 "extern intptr_t Scm_VMGetBoundaryFrameMark(void);"
 
 (define-type <u8vector> "ScmU8Vector*")

 (define-cproc %make-native (code::<u8vector>
                             reqargs::<fixnum>
                             optional::<boolean>
                             info)
   (expr (Scm_MakeNative (cast void* (SCM_U8VECTOR_ELEMENTS code))
                         reqargs (?: optional 1 0) info)))

 (define-cproc %vm-access-info (field-name::<const-cstring>)
   (call "Scm_VMAccessInfo"))

 (define-cproc %address-of (obj)        ;kludge
   (expr (Scm_MakeIntegerU (cast intptr_t obj))))

 (define-cproc %closure-env (obj::<closure>)
   (expr (Scm_MakeIntegerU (cast intptr_t (-> obj env)))))

 (define-cproc %code-address (v::<u8vector>) ;kludge
   (expr (Scm_MakeIntegerU (cast intptr_t (SCM_U8VECTOR_ELEMENTS v)))))

 (define-cproc %get-boundary-frame-mark ()
   (expr (Scm_MakeIntegerU (Scm_VMGetBoundaryFrameMark))))
 
 ;; dumb kluge
 (define-cise-stmt fntab
   [(_ arg fnname ...)
    `(cond ,@(map (lambda (fn)
                    `[(== (strcmp ,arg ,(string-drop (x->string fn) 4)) 0)
                      (result (cast intptr_t (& ,fn)))])
                  fnname)
           [else (result 0)])])

 (define-cproc %get-addr (name::<const-cstring>)
   (body <ulong>
         (fntab name
                Scm_MakeInteger
                Scm_Add
                Scm_NumCmp
                Scm_VMPushCC
                Scm_VMApply0
                Scm_VMApply1
                Scm_Error
                Scm_RealClass
                Scm_ProcedureClass
                )))
 )


;;;
;;;  x86_64 minimum assembly
;;;
;;;    We only support what we need for JIT experiment now.
;;;

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
    (let1 bss (fold (^*[((p . addr) seed)
                        (if (symbol? p)
                          seed          ;ignore labels
                          (cons (p addr abss) seed))])
                    '() abss)
      (concatenate (reverse bss)))))

;; asm1 :: ParsedInsn -> (Int, (Int, [Symbol,Int]) -> [Byte])
;;  First pass.  Returns two values : Number of bytes to be generated,
;;  and a closure to generate the final byte sequence in the second pass.
;;  The closure will take (1) The address of *next* instruction, and
;;  (2) an assoc list of labels to addresses.
(define (asm1 pinsn)
  (define $ expand-spec)
  (define w rex.w)
  (match pinsn
    ;; call and ret.  we don't care "far" calls/returns.
    [`(call (imm32 ,i))            ($ (opc #xe8) (imm32 i))]
    [`(call (reg ,r))              ($ (opc #xff) (reg 2) (r/m-reg r))]
    [`(call (mem . ,x))            ($ (opc #xff) (reg 2) (mem x))]
    [`(ret)                        ($ (opc #xc3))]

    [`(push (reg ,n))              ($ (rex.b n) (opc+rq #x50 n))]
    [`(pop  (reg ,n))              ($ (rex.b n) (opc+rq #x58 n))]

    ;; jumps.  for the time being, we ask programmers to explicitly specify
    ;; whether the jump is short or long.
    [`(jmp  (reg ,t))              ($ (opc #xff) (reg 4) (r/m-reg t))]
    [`(jmp  (mem . ,x))            ($ (opc #xff) (reg 4) (mem x))]
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
    [`(movq (imm8  ,i) (reg ,dst)) ($ w (opc #xc7) (reg 0) (r/m-reg dst) (imm32 i))]
    [`(movq (imm32 ,i) (reg ,dst)) ($ w (opc #xc7) (reg 0) (r/m-reg dst) (imm32 i))]
    [`(movq (imm8  ,i) (mem . ,x)) ($ w (opc #xc7) (reg 0) (mem x) (imm32 i))]
    [`(movq (imm32 ,i) (mem . ,x)) ($ w (opc #xc7) (reg 0) (mem x) (imm32 i))]
    [`(movq (imm64 ,i) (reg ,dst)) ($ w (rex.b dst) (opc+rq #xb8 dst) (imm64 i))]
    [`(movq (label ,l) (reg ,dst)) (op-movlabel l dst)]
    [`(movq (reg ,src) (reg ,dst)) ($ w (opc #x89) (reg src) (r/m-reg dst))]
    [`(movq (reg ,src) (mem . ,x)) ($ w (opc #x89) (reg src) (mem x))]
    [`(movq (mem . ,x) (reg ,dst)) ($ w (opc #x8b) (reg dst) (mem x))]

    [`(movzbq (mem . ,x) (reg ,dst)) ($ w (opc'(#x0f #xb6)) (reg dst) (mem x))]
    [`(movzwq (mem . ,x) (reg ,dst)) ($ w (opc'(#x0f #xb7)) (reg dst) (mem x))]

    [`(leaq (mem . ,x) (reg ,dst)) ($ w (opc #x8d) (reg dst) (mem x))]

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
  (lambda (addr label-alist)
    (cond [(not label-alist)
           ((expand-spec (opc opcode) (immX 0)) 0 0)] ; dummy
          [(assq-ref label-alist target)
           => (lambda (taddr)
                (unless (immX? (- taddr addr))
                  (error "jump target out of range:" target))
                ((expand-spec (opc opcode) (immX (- taddr addr))) 0 0))]
          [else (error "jump destination doesn't exist:" target)])))

;; special case to load label's address into a register
(define (op-movlabel label dst)
  (define $ expand-spec)
  (define w rex.w)
  (lambda (addr label-alist)
    (if label-alist
      (let1 laddr (assq-ref label-alist label)
        (unless laddr (error "undefined label:" label))
        (($ w (rex.b dst) (opc+rq #xb8 dst) (imm64 laddr)) 0 0))
      (($ w (rex.b dst) (opc+rq #xb8 dst) (imm64 0)) 0 0)))) ;; dummy

;; addq family
;;  opcode variations are derived from a single number, regc.
(define (op-add pinsn regc)
  (define $ expand-spec)
  (define w rex.w)
  (define basc (+ (ash regc 3) 1))
  (define raxc (+ (ash regc 3) 5))
  (match pinsn
    [`(,_ (imm8 ,i)  (reg ,dst)) ($ w (opc #x83) (reg regc) (r/m-reg dst) (imm8 i))]
    [`(,_ (imm8 ,i)  (mem . ,x)) ($ w (opc #x83) (reg regc) (mem x) (imm8 i))]
    [`(,_ (imm32 ,i) (reg ,dst)) (if (= dst 0) ; %rax
                                   ($ w (opc raxc) (imm32 i))
                                   ($ w (opc #x81) (reg regc) (r/m-reg dst) (imm32 i)))]
    [`(,_ (imm32 ,i) (mem . ,x)) ($ w (opc #x81) (reg regc) (imm32 i))]
    [`(,_ (reg ,src) (reg ,dst)) ($ w (opc basc) (reg src) (r/m-reg dst))]
    [`(,_ (reg ,src) (mem . ,x)) ($ w (opc basc) (reg src) (mem x))]
    [`(,_ (mem . ,x) (reg ,dst)) ($ w (opc (+ basc 2)) (reg dst) (mem x))]
    ))

;; shift family
(define (op-shift pinsn regc)
  (define $ expand-spec)
  (define w rex.w)
  (match pinsn
    [`(,_ (imm8 1)  (reg ,r))   ($ w (opc #xd1) (reg regc) (r/m-reg r))]
    [`(,_ (imm8 1)  (mem . ,x)) ($ w (opc #xd1) (reg regc) (mem x))]
    [`(,_ (imm8 ,i) (reg ,r))   ($ w (opc #xc1) (reg regc) (r/m-reg r) (imm8 i))]
    [`(,_ (imm8 ,i) (mem . ,x)) ($ w (opc #xc1) (reg regc) (mem x) (imm8 i))]
    [`(,_ (reg %cl) (reg ,r))   ($ w (opc #xd3) (reg regc) (r/m-reg r))]
    [`(,_ (reg %cl) (mem . ,x)) ($ w (opc #xd3) (reg regc) (mem x))]
    ))

;; inc and dec
(define (op-inc pinsn regc)
  (define $ expand-spec)
  (define w rex.w)
  (match pinsn
    [`(,_ (reg ,r))             ($ w (opc #xff) (reg regc) (r/m-reg r))]
    [`(,_ (mem . ,x))           ($ w (opc #xff) (reg regc) (mem x))]
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
  (lambda (addr label-alist)
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

(define (opc c)      (lambda (s a t) `(:opcode ,c ,@s)))
(define (opc+rq c r) (lambda (s a t) `(:opcode ,(+ c (modulo r 8)) ,@s)))

(define rex.w        (lambda (s a t) `(:rex.w #t ,@s)))
(define (rex.b r)    (lambda (s a t) (if (>= r 8) `(:rex.b #t ,@s) s)))

(define (reg r)      (lambda (s a t) `(,@(if (>= r 8) `(:rex.r #t) '())
                                       :reg ,r ,@s)))

(define (mem params)
  (match params
    [`(addr ,a)          (mem-addr a)]
    [`(base ,b)          (mem-base b)]
    [`(label ,l)         (mem-label l)]
    [`(base+disp ,b ,d)  (mem-base+disp b d)]
    [`(sib ,b ,i ,s ,d)  (mem-sib b i s d)]))

(define (r/m-reg r)
  (lambda (s a t) `(,@(if (>= r 8) `(:rex.b #t) '()) :mode 3 :r/m ,r ,@s)))

(define (mem-addr a)
  (lambda (s a t)
    `(:mode 0 :r/m 4 :index 4 :base 5 :displacement ,(int8/32 a) ,@s)))

(define (mem-base b)
  (lambda (s a t)
    `(,@(if (>= b 8) `(:rex.b #t) '())
      ,@(case (modulo b 8)
          [(4) `(:mode 0 :r/m 4 :index 4 :base 4)]  ; we need to use SIB
          [(5) `(:mode 1 :r/m 5 :displacement (0))] ; we need to use disp8
          [else `(:mode 0 :r/m ,b)]                 ; we can use mode0
          )
      ,@s)))

;; we use RIP-relative addressing.
(define (mem-label l)
  (lambda (s a t)
    (if (and a t)
      (let1 laddr (assq-ref t l)
        (unless laddr (error "undefined label:" l))
        `(:mode 0 :r/m 5 :displacement ,(int32 (- laddr a)) ,@s))
      `(:mode 0 :r/m 5 :displacement ,(int32 0) ,@s))))

(define (mem-base+disp base disp)
  (lambda (s a t)
    `(,@(if (>= base 8) `(:rex.b #t) '())
      :mode ,(if (imm8? disp) 1 2)
      ,@(case (modulo base 8)
          [(4) `(:r/m 4 :index 4 :base 4 :displacement ,(int8/32 disp))]
          [else `(:r/m ,base :displacement ,(int8/32 disp))])
      ,@s)))

(define (mem-sib base index scale disp)
  (when (= index 4)
    (error "cannot use %rsp as an index register"))
  (lambda (s a t)
    `(,@(if (>= base 8) `(:rex.b #t) '())
      ,@(if (>= index 8) `(:rex.x #t) '())
      :mode ,(cond [(zero? disp) 0][(imm8? disp) 1][else 2])
      :r/m 4 :scale ,scale :index ,index :base ,base
      ,@(if (zero? disp) '() `(:displacement ,(int8/32 disp)))
      ,@s)))

(define (imm8 i)   (lambda (s a t) `(:immediate ,(int8 i) ,@s)))
(define (imm32 i)  (lambda (s a t) `(:immediate ,(int32 i) ,@s)))
(define (imm64 i)  (lambda (s a t) `(:immediate ,(int64 i) ,@s)))
      
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

;;;
;;; Some test functions
;;;

(define (show-result asm-result)
  (print (map (pa$ format "~2,'0X") asm-result))
  (values))

(define (test-asm insns) (show-result (asm insns)))

(define (maknative nargs . jitted)
  (%make-native (list->u8vector (asm (concatenate jitted))) nargs #f #f))

(provide "gauche/vm/jit")
