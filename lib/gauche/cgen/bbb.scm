;;;
;;; gauche.cgen.bbb - Basic-blocks backend
;;;
;;;   Copyright (c) 2021  Shiro Kawai  <shiro@acm.org>
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

(define-module gauche.cgen.bbb
  ;; We need to access compiler internals.  This is ugly---eventually
  ;; we want to have a separate module that exposes some compiler internals.
  (extend gauche.internal)
  (use util.match)
  (use gauche.vm.insn)
  (export compile-b) ; just for now
  )
(select-module gauche.cgen.bbb)

;; This is an alternative compiler backend targetting at block-based
;; languages.  pass5b is called in place of pass5.
;;
;; This pass aims at AOT-compilation, so we're not too sensitve about
;; performance.

;; Abstract instructions:
;; - All local variables are assigned to registers.  LREF becomes just
;;   a register reference.
;; - All local operations are between regsiters.  GREF becomes LD and
;;   GSET becomes ST.
;; - Constant values are treated as preset constant registers.

;; Instructions:
;;
;; (MOV dreg sreg)     - dreg <- sreg
;; (MOV* r o (dreg ...) sreg) - mv bind (details TBD)
;; (BREF dreg sreg)    - dreg <- (box-ref sreg)
;; (BSET dreg sreg)    - (box-set! dreg sreg)
;; (LD reg identifier) - global load
;; (ST reg identifier) - global store
;; (CLOSE reg bb)      - make a closure with current env and a basic block BB,
;;                       leave it in REG.
;; (BR reg bb1 bb2)    - if the value of register REG is true, jump to
;;                       a basic block BB1.  Otherwise, jump to BB2.
;; (JP bb)             - jump to a basic block BB.
;; (CONT bb)           - push continuation frame, with BB to be the
;;                       'next' basic block.
;; (CALL proc arg ...) - PROC and ARGs are all registers.  Transfer control
;;                       to PROC.
;; (RET reg ...)       - Return, using values in REGs as the results.
;; (BUILTIN op reg ...) - Builtin operation
;; (ASM op reg ...)    - Inlined VM assembly operation
;;
;; (DEF id flags reg)  - insert global binding of ID in the current module
;;                       with the value in REG.

;; Basic blocks:
;;   First, we convert IForm to a DG of basic blocks (BBs).
;;   BB has one entry point and multiple exit point.

;; Block environment.  Keep track of allocated registers and blocks
;; per compile unit (usually each toplevel form)
(define-class <benv> ()
  ((registers :init-form '())                 ; (<List> (</> <reg> <const>))
   (regmap :init-form (make-hash-table 'eq?)) ; lvar -> <reg>
   (cstmap :init-form (make-hash-table 'equal?)) ; const -> <const>
   (blocks :init-value '())                   ; basic blocks
   (parent :init-keyword :parent)))           ; parent benv

(define (make-benv parent)
  (make <benv> :parent parent))

(define (benv-depth benv)               ; used for register naming
  (if-let1 p (~ benv'parent) (+ (benv-depth p) 1) 0))

;; Registers and constatns.
(define-class <reg> ()
  ((lvar :init-keyword :lvar)           ; source LVAR; can be #f
   (name :init-keyword :name)))         ; given temporary name
(define-method write-object ((reg <reg>) port)
  (if-let1 lvar (~ reg'lvar)
    (format port "#<reg ~a(~a)>" (~ reg'name) (lvar-name lvar))
    (format port "#<reg ~a>" (~ reg'name))))

(define (make-reg bb lvar)
  (define (lookup benv lvar)
    (or (hash-table-get (~ benv'regmap) lvar #f)
        (and-let1 parent (~ benv'parent)
          (lookup parent lvar))))
  (or (lookup (~ bb'benv) lvar)
      (let1 name (string->symbol (format "%~d.~d" (benv-depth (~ bb'benv))
                                         (length (~ bb'benv'registers))))
        (rlet1 r (make <reg> :name name :lvar lvar)
          (push! (~ bb'benv'registers) r)
          (when lvar (hash-table-put! (~ bb'benv'regmap) lvar r))))))

;; For constant, we box the value, in case if it is #<undef>.
(define-class <const> ()                ; constant register
  ((value :init-keyword :value)         ; BOXED constant value
   (name :init-keyword :name)))         ; given temporary name
(define-method write-object ((cst <const>) port)
  (format port "#<const ~a(~,,,,20:s)>" (~ cst'name) (unbox (~ cst'value))))

(define (make-const bb val)
  (or (hash-table-get (~ bb'benv'cstmap) val #f)
      (let1 name (symbol-append '% (length (~ bb'benv'registers)))
        (rlet1 r (make <const> :name name :value (box val))
          (push! (~ bb'benv'registers) r)
          (hash-table-put! (~ bb'benv'cstmap) val r)))))
(define (const-value cst) (unbox (~ cst'value)))

;; Basic blocks
(define-class <basic-block> ()
  ((insns      :init-value '())         ; list of instructions
   (upstream   :init-keyword :upstream) ; list of upstream blocks
   (downstream :init-value '())         ; list of downstream blocks
   (benv       :init-keyword :benv)
   ))

(define (make-bb benv . upstreams)
  (rlet1 bb (make <basic-block> :benv benv :upstream upstreams)
    (push! (~ benv'blocks) bb)))

(define (push-insn bb insn)
  (push! (~ bb'insns) insn))

;;
;; Conversion to basic blocks
;;

(define-inline (pass5b/rec iform bb benv ctx)
  ((vector-ref *pass5b-dispatch-table* (iform-tag iform))
   iform bb benv ctx))

;; Returns the entry basic block
(define (pass5b iform benv)
  (rlet1 entry-bb (make-bb benv)
    (pass5b/rec iform entry-bb benv 'tail)))

;; Wrap the tail expr with this, to add RET.
(define (pass5b/return bb ctx reg)
  (if (eq? ctx 'tail)
    (begin
      (push-insn bb `(RET ,reg))
      (values bb #f))
    (values bb reg)))

(define (pass5b/$DEFINE iform bb benv ctx)
  (receive (bb val0) (pass5b/rec ($define-expr iform) bb benv 'normal)
    (push-insn bb `(DEF ,($define-id iform)
                        ,($define-flags iform)
                        ,val0))
    (values bb #f)))

(define (pass5b/$LREF iform bb benv ctx)
  (pass5b/return bb ctx (make-reg bb ($lref-lvar iform))))

(define (pass5b/$LSET iform bb benv ctx)
  (receive (bb val0) (pass5b/rec ($lset-expr iform) bb benv 'normal)
    (let1 r (make-reg bb ($lset-lvar iform))
      (push-insn bb `(MOV ,r ,val0))
      (pass5b/return bb ctx r))))

(define (pass5b/$GREF iform bb benv ctx)
  (let1 r (make-reg bb #f)
    (push-insn bb `(LD ,r ,($gref-id iform)))
    (pass5b/return bb ctx r)))

(define (pass5b/$GSET iform bb benv ctx)
  (receive (bb val0) (pass5b/rec ($gset-expr iform) bb benv 'normal)
    (push-insn bb `(ST ,val0 ,($gref-id iform)))
    (pass5b/return bb ctx #f)))

(define (pass5b/$CONST iform bb benv ctx)
  (pass5b/return bb ctx (make-const bb ($const-value iform))))

(define (pass5b/$IF iform bb benv ctx)
  (receive (bb val0) (pass5b/rec ($if-test iform) bb benv 'normal)
    (let ([then-bb (make-bb benv bb)]
          [else-bb (make-bb benv bb)])
      (push-insn bb `(BR ,val0 ,then-bb ,else-bb))
      (receive (tbb tval0) (pass5b/rec ($if-then iform) then-bb benv 'normal)
        (receive (ebb eval0) (pass5b/rec ($if-else iform) else-bb benv 'normal)
          (case ctx
            [(tail)
             (push-insn tbb `(RET ,tval0))
             (push-insn ebb `(RET ,eval0))
             (values tbb tval0)] ; it doesn't really matter
            [(stmt)
             (let1 cbb (make-bb benv tbb ebb)
               (push-insn tbb `(JP ,cbb))
               (push-insn ebb `(JP ,cbb))
               (values cbb #f))]
            [(normal)
             (let* ([cbb (make-bb benv tbb ebb)]
                    [r (make-reg cbb #f)])
               (when tval0 (push-insn tbb `(MOV ,r ,tval0)))
               (push-insn tbb `(JP ,cbb))
               (when eval0 (push-insn ebb `(MOV ,r ,eval0)))
               (push-insn ebb `(JP ,cbb))
               (values cbb r))]))))))

(define (pass5b/$LET iform bb benv ctx)
  (let loop ([bb bb]
             [vars ($let-lvars iform)]
             [inits ($let-inits iform)])
    (if (null? vars)
      (pass5b/rec ($let-body iform) bb benv ctx)
      (let1 reg (make-reg bb (car vars))
        (receive (bb val0) (pass5b/rec (car inits) bb benv 'normal)
          (push-insn bb `(MOV ,reg ,val0))
          (loop bb (cdr vars) (cdr inits)))))))

(define (pass5b/$RECEIVE iform bb benv ctx)
  (let1 regs (map (cut make-reg bb <>) ($receive-lvars iform))
    (receive (bb val0) (pass5b/rec ($receive-expr iform) bb benv 'normal)
      (push-insn bb `(MOV* ,($receive-reqargs iform) ,($receive-optarg iform)
                           ,regs ,val0))
      (pass5b/rec ($receive-body iform) bb benv ctx))))

(define (pass5b/$LAMBDA iform bb benv ctx)
  (let* ([lbenv (make-benv benv)]
         [lbb (make-bb lbenv)]
         [reg (make-reg bb #f)])
    (receive (cbb val0) (pass5b/rec ($lambda-body iform) lbb lbenv 'tail)
      (push-insn bb `(CLOSE ,reg ,lbb))
      (pass5b/return bb ctx reg))))

(define (pass5b/$LABEL iform bb benv ctx)
  ;; NB: $label-label is #f at the end of pass4.  We assume that, and use
  ;; it to keep <basic-block> once we assigne one.
  (if-let1 cbb ($label-label iform)
    (begin (push-insn bb `(JP ,cbb))
           (values bb #f))
    (let1 cbb (make-bb benv bb)
      (push-insn bb `(JP ,cbb))
      ($label-label-set! iform cbb)
      (receive (ccbb res) (pass5b/rec ($label-body iform) cbb benv ctx)
        (values ccbb res)))))

(define (pass5b/$SEQ iform bb benv ctx)
  (if (null? ($seq-body iform))
    (pass5b/return bb ctx #f)
    (let loop ([bb bb] [iforms ($seq-body iform)])
      (if (null? (cdr iforms))
        (pass5b/rec (car iforms) bb benv ctx)
        (receive (bb _) (pass5b/rec (car iforms) bb benv 'stmt)
          (loop bb (cdr iforms)))))))

(define (pass5b/$CALL iform bb benv ctx)
  (define (call-common cont-bb cont-reg)
    (let loop ([bb bb] [args ($call-args iform)] [regs '()])
      (if (null? args)
        (receive (bb proc) (pass5b/rec ($call-proc iform) bb benv 'normal)
          (push-insn bb `(CALL ,proc ,@(reverse regs)))
          (values (or cont-bb bb) cont-reg))
        (receive (bb reg) (pass5b/rec (car args) bb benv 'normal)
          (loop bb (cdr args) (cons reg regs))))))
  (ecase ctx
    [(tail)   (call-common #f '%VAL0)]
    [(stmt)   (let1 cbb (make-bb benv bb)
                (push-insn bb `(CONT ,cbb))
                (call-common cbb #f))]
    [(normal) (let* ([cbb (make-bb benv bb)]
                     [valreg (make-reg cbb #f)])
                (push-insn bb `(CONT ,cbb))
                (push-insn cbb `(MOV ,valreg %VAL0))
                (call-common cbb valreg))]))

(define (pass5b/$ASM iform bb benv ctx)
  (let loop ([bb bb] [args ($asm-args iform)] [regs '()])
    (if (null? args)
      (begin
        (push-insn bb `(ASM ,($asm-insn iform) ,@(reverse regs)))
        (pass5b/return bb ctx '%VAL0))
      (receive (bb reg) (pass5b/rec (car args) bb benv 'normal)
        (loop bb (cdr args) (cons reg regs))))))

(define (pass5b/$CONS iform bb benv ctx)
  (pass5b/builtin-twoargs 'CONS iform bb benv ctx))
(define (pass5b/$APPEND iform bb benv ctx)
  (pass5b/builtin-twoargs 'APPEND iform bb benv ctx))
(define (pass5b/$MEMV iform bb benv ctx)
  (pass5b/builtin-twoargs 'MEMV iform bb benv ctx))
(define (pass5b/$EQ? iform bb benv ctx)
  (pass5b/builtin-twoargs 'EQ? iform bb benv ctx))
(define (pass5b/$EQV? iform bb benv ctx)
  (pass5b/builtin-twoargs 'EQV? iform bb benv ctx))

(define (pass5b/builtin-twoargs op iform bb benv ctx)
  (receive (bb reg0) (pass5b/rec ($*-arg0 iform) bb benv 'normal)
    (receive (bb reg1) (pass5b/rec ($*-arg1 iform) bb benv 'normal)
      (push-insn bb `(BUILTIN ,op ,reg0 ,reg1))
      (pass5b/return bb ctx '%VAL0))))

(define (pass5b/$LIST->VECTOR iform bb benv ctx)
  (receive (bb reg0) (pass5b/rec ($*-arg0 iform) bb benv 'normal)
    (push-insn bb `(BUILTIN LIST->VECTOR ,reg0))
    (pass5b/return bb ctx '%VAL0)))

(define (pass5b/$VECTOR iform bb benv ctx)
  (pass5b/builtin-nargs 'VECTOR iform bb benv ctx))
(define (pass5b/$LIST iform bb benv ctx)
  (pass5b/builtin-nargs 'LIST iform bb benv ctx))
(define (pass5b/$LIST* iform bb benv ctx)
  (pass5b/builtin-nargs 'LIST* iform bb benv ctx))

(define (pass5b/builtin-nargs op iform bb benv ctx)
  (let loop ([bb bb] [args ($*-args iform)] [regs '()])
    (if (null? args)
      (begin
        (push-insn bb `(BUILTIN ,op ,@(reverse regs)))
        (pass5b/return bb ctx '%VAL0))
      (receive (bb reg) (pass5b/rec (car args) bb benv ctx)
        (loop bb (cdr args) (cons reg regs))))))

(define (pass5b/$IT iform bb benv ctx)
  (pass5b/return bb ctx '%VAL0))

;; Dispatch table.
(define *pass5b-dispatch-table* (generate-dispatch-table pass5b))

;;
;; For debugging
;;

(define (dump-bb bb :optional (port (current-output-port)))
  (define bb-alist '())  ;; ((bb . N) ...)
  (define (bb-num bb)
    (if-let1 p (assq bb bb-alist)
      (cdr p)
      (rlet1 n (length bb-alist)
        (push! bb-alist (cons bb n)))))
  (define bb-shown '())
  (define bb-toshow (list bb))
  (define (dump-insn insn)
    (define (bbname bb)
      (unless (memq bb bb-shown) (push! bb-toshow bb))
      #"BB#~(bb-num bb)")
    (define (regname reg) (if (is-a? reg <reg>) (~ reg'name) reg))
    (match insn
      [((or 'MOV 'BREF 'BSET) d s)
       (format port "  ~s\n" `(,(car insn) ,(regname d) ,(regname s)))]
      [((or 'LD 'ST) reg id)
       (format port "  (~s ~s ~a#~a)\n" (car insn) (regname reg)
               (~ id'module'name) (~ id'name))]
      [('CLOSE reg bb)
       (format port "  (CLOSE ~s ~a)\n" (regname reg) (bbname bb))]
      [('BR reg bb1 bb2)
       (format port "  (BR ~s ~a ~a)\n" (regname reg) (bbname bb1) (bbname bb2))]
      [((or 'JP 'CONT) bb)  (format port "  (~s ~a)\n" (car insn) (bbname bb))]
      [((or 'CALL 'RET) . regs)
       (format port "  ~s\n" `(,(car insn) ,@(map regname regs)))]
      [('ASM insn . args)
       (let ((insn-name (~ (vm-find-insn-info (car insn))'name)))
         (format port "  ~s\n" `(ASM (,insn-name ,@(cdr insn))
                                     ,@(map regname args))))]
      [('DEF id flags reg)
       (format port "  (DEF ~a#~a ~s ~s)\n" (~ id'module'name) (~ id'name)
               flags (regname reg))]
      [_ (format port "??~s\n" insn)]))
  (define (dump-1 bb)
    (unless (memq bb bb-shown)
      (push! bb-shown bb)
      (format port "BB #~a\n" (bb-num bb))
      (format port "    Up: ~s\n" (map bb-num (~ bb'upstream)))
      (format port "    Dn: ~s\n" (map bb-num (~ bb'downstream)))
      (for-each dump-insn (reverse (~ bb'insns)))))
  (let loop ()
    (unless (null? bb-toshow)
      (dump-1 (pop! bb-toshow))
      (loop))))

;; For now
(define (compile-b program)
  (let* ([cenv (make-bottom-cenv (vm-current-module))]
         [iform (pass2-4 (pass1 program cenv) (cenv-module cenv))])
    (pp-iform iform)
    (let* ([benv (make-benv #f)]
           [bb (pass5b iform benv)])
      ;;(write (~ benv'registers)) (newline)
      (dump-bb bb))))
