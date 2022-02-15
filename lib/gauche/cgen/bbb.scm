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
;; (BUILTIN op receiver reg ...) - Builtin operation
;; (ASM op receiver reg ...)    - Inlined VM assembly operation
;;
;; (DEF id flags reg)  - insert global binding of ID in the current module
;;                       with the value in REG.
;;
;;  reg is either <reg>, <const> or %VAL0.  If it is <reg>, it can be 'boxed',
;;  meaning we have one indirection to the shared box.
;;  register is destination position is always <reg>.

;; Basic blocks:
;;   First, we convert IForm to a DG of basic blocks (BBs).
;;   BB has one entry point and multiple exit point.

;; Block environment.  Keep track of allocated registers and blocks
;; per compile unit (usually each toplevel form)
(define-class <benv> ()
  ((name :init-keyword :name)                 ; possible name (for info)
   (registers :init-form '())                 ; (<List> (</> <reg> <const>))
   (regmap :init-form (make-hash-table 'eq?)) ; lvar -> <reg>
   (cstmap :init-form (make-hash-table 'equal?)) ; const -> <const>
   (input-regs :init-value '())               ; <reg>s to receive args
   (input-reqargs :init-value #f)             ; # of required args.
                                              ;  #f if this is not a closure
                                              ;  (- (length input-regs) 1) if
                                              ;  closure takes rest args.
                                              ;  (length input-regs) otherwise.
   (entry :init-value #f)                     ; entry BB
   (blocks :init-value '())                   ; basic blocks
   (parent :init-keyword :parent)             ; parent benv
   (children :init-value '())))

(define (make-benv parent name)
  (rlet1 benv (make <benv> :parent parent :name name)
    (when parent
      (push! (~ parent'children) benv))
    (set! (~ benv'entry) (make-bb benv))))

(define (benv-depth benv)               ; used for register naming
  (if-let1 p (~ benv'parent) (+ (benv-depth p) 1) 0))

;; Registers and constatns.
(define-class <reg> ()
  ((lvar :init-keyword :lvar)           ; source LVAR; can be #f
   (name :init-keyword :name)           ; given temporary name
   (boxed :init-value #f)               ; #t if this reg should be in a box
   (blocks :init-value '())             ; BBs using this register.
   ))

(define-method write-object ((reg <reg>) port)
  (if-let1 lvar (~ reg'lvar)
    (format port "#<reg ~a(~a)>" (~ reg'name) (lvar-name lvar))
    (format port "#<reg ~a>" (~ reg'name))))

(define (make-reg bb lvar)
  (define (lookup benv lvar)
    (or (hash-table-get (~ benv'regmap) lvar #f)
        (and-let1 parent (~ benv'parent)
          (lookup parent lvar))))
  (if-let1 reg (lookup (~ bb'benv) lvar)
    (touch-reg! bb reg)
    (let* ([symname (and lvar (unwrap-syntax (lvar-name lvar)))]
           [name (string->symbol (format "%~d.~d~a" (benv-depth (~ bb'benv))
                                         (length (~ bb'benv'registers))
                                         (if symname #".~symname" "")))])
      (rlet1 reg (make <reg> :name name :lvar lvar)
        (push! (~ bb'benv'registers) reg)
        (push! (~ reg'blocks) bb)
        (when lvar (hash-table-put! (~ bb'benv'regmap) lvar reg))))))

(define (touch-reg! bb reg)
  (when (and (is-a? reg <reg>) (not (memq bb (~ reg'blocks))))
    (push! (~ reg'blocks) bb))
  reg)

(define (mark-reg-boxed! reg) (set! (~ reg'boxed) #t))
(define (reg-boxed? reg) (~ reg'boxed))


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
  ((id         :init-keyword :id)       ; unique id within benv
   (insns      :init-value '())         ; list of instructions (reversed)
   (upstream   :init-keyword :upstream) ; list of upstream blocks
   (downstream :init-value '())         ; list of downstream blocks
   (benv       :init-keyword :benv)
   (entry?     :init-value #f)          ; #t if this BB is entered from outside
   ))


(define (make-bb benv . upstreams)
  (rlet1 bb (make <basic-block>
              :benv benv :upstream upstreams :id (length (~ benv'blocks)))
    (when (null? upstreams)
      (set! (~ bb'entry?) #t))
    (dolist [ubb upstreams]
      (push! (~ ubb'downstream) bb))
    (push! (~ benv'blocks) bb)))

(define (link-bb from-bb to-bb)         ;if we emit JP from FROM-BB to TO-BB
  (push! (~ from-bb'downstream) to-bb)
  (push! (~ to-bb'upstream) from-bb))

(define (bb-name bb)                    ;for debug dump
  (format "BB#~s~a" (~ bb'id) (if (~ bb'entry?) "!" "")))

(define (push-insn bb insn)
  (push! (~ bb'insns) insn))

;; A 'complete' basic block is the one thats ends with either
;; CALL, BR, JP or RET insn.
(define (bb-complete? bb)
  (match (~ bb'insns)
    [(((or 'CALL 'BR 'JP 'RET) . _) . _) #t]
    [_ #f]))

;;
;; Conversion to basic blocks
;;
;;   iform - input iform to compile (after pass4)
;;   bb - 'current' basic block to accumulate insns
;;   benv - current benv
;;   ctx - expression context.
;;
;;  Returns two values.
;;   - A basic block the caller should continue to emit insns (it may be
;;     the same as the input bb, or a new one.
;;   - A register that holds the result of expression.  It may be #f (no
;;     result), or '%VAL0, instead of a <reg>.

(define-inline (pass5b/rec iform bb benv ctx)
  ((vector-ref *pass5b-dispatch-table* (iform-tag iform))
   iform bb benv ctx))

;; Returns the entry basic block
(define (pass5b iform benv)
  (rlet1 entry-bb (~ benv'entry)
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

;; $IF - We have these variations
;;   Context:
;;     - If it's tail, we just BR to then/else branches, not worrying about
;;       their result.  Return value is irrelevant.
;;     - If it's stmt, the control merges to a new BB and it is returned.
;;     - If it's normal, the control merges and the register contains
;;       the result.
;;   $IT:
;;     - Either one of the branch can be an $IT node.  We recognize
;;       the case and pass down the result of the test.

(define (pass5b/$IF iform bb benv ctx)
  (define (if-branch iform bb benv ctx test-result)
    (if (has-tag? iform $IT)
      (begin (touch-reg! bb test-result)
             (pass5b/return bb ctx test-result))
      (pass5b/rec iform bb benv ctx)))
  (receive (bb val0) (pass5b/rec ($if-test iform) bb benv 'normal)
    (let ([then-bb (make-bb benv bb)]
          [else-bb (make-bb benv bb)])
      (touch-reg! bb val0)
      (push-insn bb `(BR ,val0 ,then-bb ,else-bb))
      (receive (tbb tval0) (if-branch ($if-then iform) then-bb benv ctx val0)
        (receive (ebb eval0) (if-branch ($if-else iform) else-bb benv ctx val0)
          (case ctx
            [(tail)
             (values tbb tval0)] ; it doesn't really matter
            [(stmt normal)
             (if (bb-complete? tbb)
               (if (bb-complete? ebb)
                 (values tbb #f) ; it doesn't really matter
                 (let* ([cbb (make-bb benv ebb)]
                        [r (and (eq? ctx 'normal) eval0
                                (rlet1 r (make-reg cbb #f)
                                  (touch-reg! ebb eval0)
                                  (touch-reg! ebb r)
                                  (push-insn ebb `(MOV ,r ,eval0))))])
                   (push-insn ebb `(JP ,cbb))
                   (values cbb r)))
               (if (bb-complete? ebb)
                 (let* ([cbb (make-bb benv tbb)]
                        [r (and (eq? ctx 'normal) tval0
                                (rlet1 r (make-reg cbb #f)
                                  (touch-reg! tbb tval0)
                                  (touch-reg! tbb r)
                                  (push-insn tbb `(MOV ,r ,tval0))))])
                   (push-insn ebb `(JP ,cbb))
                   (values cbb r))
                 (let* ([cbb (make-bb benv tbb ebb)]
                        [r (and (eq? ctx 'normal)
                                (make-reg cbb #f))])
                   (when (and (eq? ctx 'normal) tval0)
                     (touch-reg! tbb tval0)
                     (touch-reg! tbb r)
                     (push-insn tbb `(MOV ,r ,tval0)))
                   (push-insn tbb `(JP ,cbb))
                   (when (and (eq? ctx 'normal) eval0)
                     (touch-reg! ebb eval0)
                     (touch-reg! ebb r)
                     (push-insn ebb `(MOV ,r ,eval0)))
                   (push-insn ebb `(JP ,cbb))
                   (values cbb r))))]))))))

(define (pass5b/$LET iform bb benv ctx)
  (let loop ([bb bb]
             [vars ($let-lvars iform)]
             [inits ($let-inits iform)])
    (if (null? vars)
      (pass5b/rec ($let-body iform) bb benv ctx)
      (let1 reg (make-reg bb (car vars))
        (when (memq ($let-type iform) '(rec rec*))
          (mark-reg-boxed! reg))
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
  (let* ([lbenv (make-benv benv ($lambda-name iform))]
         [lbb (~ lbenv'entry)]
         [reg (make-reg bb #f)])
    (set! (~ lbenv'input-regs)
          (map (cut make-reg lbb <>) ($lambda-lvars iform)))
    (set! (~ lbenv'input-reqargs) ($lambda-reqargs iform))
    (receive (cbb val0) (pass5b/rec ($lambda-body iform) lbb lbenv 'tail)
      (push-insn bb `(CLOSE ,reg ,lbenv))
      (pass5b/return bb ctx reg))))

(define (pass5b/$CLAMBDA iform bb benv ctx)
  (define (generate-closures bb lambdas benv)
    (let loop ([bb bb] [ls lambdas] [regs '()])
      (if (null? ls)
        (values bb (reverse regs))
        (receive (bb reg) (pass5b/rec (car ls) bb benv 'normal)
          (loop bb (cdr ls) (cons reg regs))))))
  (define (reqargs-min-max argcounts)   ;dupe from pass5/$CLAMBDA
    (let loop ([counts argcounts] [mi #f] [mx 0])
      (match counts
        [() (values mi mx)]
        [((req . _) . rest) (loop rest (if mi (min mi req) req) (max mx req))])))
  (define (create-clambda iform bb ctx)
    (receive (bb clo-regs) (generate-closures bb ($clambda-closures iform) benv)
      (for-each (cut touch-reg! bb <>) clo-regs)
      (receive (minarg maxarg) (reqargs-min-max ($clambda-argcounts iform))
        (let ([closure-list-reg (make-reg bb #f)]
              [minarg-const (make-const bb minarg)]
              [maxarg-const (make-const bb maxarg)]
              [formals-const (make-const bb #f)]
              [name-const (make-const bb ($clambda-name iform))]
              [make-case-lambda-reg (make-reg bb #f)])
          (push-insn bb `(BUILTIN LIST ,closure-list-reg ,@clo-regs))
          (push-insn bb `(LD ,make-case-lambda-reg ,make-case-lambda.))
          (push-insn bb `(CALL ,make-case-lambda-reg
                               ,minarg-const
                               ,maxarg-const
                               ,formals-const
                               ,closure-list-reg
                               ,name-const))
          (values bb #f)))))

  (if (eq? ctx 'tail)
    (create-clambda iform bb ctx)
    (let* ([cbb (make-bb benv bb)]
           [valreg (make-reg cbb #f)])
      (push-insn bb `(CONT ,cbb))
      (create-clambda iform bb ctx)      ;no need to receive result.
      (push-insn cbb `(MOV ,valreg %VAL0))
      (values cbb valreg))))

(define (pass5b/$LABEL iform bb benv ctx)
  ;; NB: $label-label is #f at the end of pass4.  We assume that, and use
  ;; it to keep <basic-block> once we assign one.
  (if-let1 cbb ($label-label iform)
    (begin (link-bb bb cbb)
           (push-insn bb `(JP ,cbb))
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

;; Used by $CALL, $ASM and $BUILTIN nodes.  Returns bb and list of regs.
;; If use-val0? is true, the last reg may be %VAL0.
(define (pass5b/prepare-args bb benv args :optional (use-val0? #f))
  (if (null? args)
    (values bb '())
    (let loop ([bb bb] [args args] [regs '()])
      (if (null? (cdr args))
        (receive (bb reg) (pass5b/rec (car args) bb benv 'normal)
          (let1 reg (if (eq? reg '%VAL0)
                      (rlet1 reg (make-reg bb #f)
                        (push-insn bb `(MOV ,reg %VAL0)))
                      reg)
            (values bb (reverse (cons reg regs)))))
        (receive (bb reg) (pass5b/rec (car args) bb benv 'normal)
          (if (eq? reg '%VAL0)
            (let1 reg (make-reg bb #f)
              (push-insn bb `(MOV ,reg %VAL0))
              (loop bb (cdr args) (cons reg regs)))
            (loop bb (cdr args) (cons reg regs))))))))

;; $CALL node is classfied by pass4; see compile-5.scm for the details.
;; We set <basic-block> to $label node's label.
(define (pass5b/$CALL iform bb benv ctx)
  (define (embedded-call)
    (receive (bb regs) (pass5b/prepare-args bb benv ($call-args iform))
      (let* ([proc ($call-proc iform)]    ; $LAMBDA node
             [label ($lambda-body proc)]  ; $LABEL mode
             [lbb (make-bb benv bb)])
        (for-each (^[reg lvar]
                    (touch-reg! bb reg)
                    (let1 lreg (make-reg bb lvar)
                      (unless (eq? lreg reg)
                        (push-insn bb `(MOV ,lreg ,reg)))))
                  regs ($lambda-lvars proc))
        (push-insn bb `(JP ,lbb))
        ($label-label-set! label lbb)
        (pass5b/rec ($label-body label) lbb benv ctx))))
  (define (jump-call)
    (receive (bb regs) (pass5b/prepare-args bb benv ($call-args iform))
      (let* ([embed-node ($call-proc iform)]  ; $CALL node
             [proc ($call-proc embed-node)]   ; $LAMBDA node
             [label ($lambda-body proc)]      ; $LABEL node
             [lbb ($label-label label)])
        (for-each (^[reg lvar]
                    (touch-reg! bb reg)
                    (let1 lreg (make-reg bb lvar)
                      (unless (eq? lreg reg)
                        (push-insn bb `(MOV ,lreg ,reg)))))
                  regs ($lambda-lvars proc))
        (link-bb bb lbb)
        (push-insn bb `(JP ,lbb))
        (values lbb #f))))                 ;dummy
  (define (normal-call cont-bb cont-reg)
    (when cont-bb (set! (~ cont-bb'entry?) #t))
    (receive (bb regs) (pass5b/prepare-args bb benv ($call-args iform))
      (receive (bb proc) (pass5b/rec ($call-proc iform) bb benv 'normal)
        (for-each (cut touch-reg! bb <>) regs)
        (push-insn bb `(CALL ,proc ,@regs))
        (values (or cont-bb bb) cont-reg))))
  (case ($call-flag iform)
    [(embed) (embedded-call)]
    [(jump) (jump-call)]
    [else
     (ecase ctx
       [(tail)   (normal-call #f '%VAL0)]
       [(stmt)   (let1 cbb (make-bb benv bb)
                   (push-insn bb `(CONT ,cbb))
                   (normal-call cbb #f))]
       [(normal) (let* ([cbb (make-bb benv bb)]
                        [valreg (make-reg cbb #f)])
                   (push-insn bb `(CONT ,cbb))
                   (push-insn cbb `(MOV ,valreg %VAL0))
                   (normal-call cbb valreg))])]))

(define (pass5b/$ASM iform bb benv ctx)
  (receive (bb regs) (pass5b/prepare-args bb benv ($asm-args iform) #t)
    (let1 receiver (make-reg bb #f)
      (push-insn bb `(ASM ,($asm-insn iform) ,receiver ,@regs))
      (pass5b/return bb ctx receiver))))

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
  (receive (bb regs)
      (pass5b/prepare-args bb benv (list ($*-arg0 iform) ($*-arg1 iform)) #t)
    (for-each (cut touch-reg! bb <>) regs)
    (let1 receiver (make-reg bb #f)
      (push-insn bb `(BUILTIN ,op ,receiver ,@regs))
      (pass5b/return bb ctx receiver))))

(define (pass5b/$LIST->VECTOR iform bb benv ctx)
  (receive (bb reg0) (pass5b/rec ($*-arg0 iform) bb benv 'normal)
    (touch-reg! bb reg0)
    (let1 receiver (make-reg bb #f)
      (push-insn bb `(BUILTIN LIST->VECTOR ,receiver ,reg0))
      (pass5b/return bb ctx receiver))))

(define (pass5b/$VECTOR iform bb benv ctx)
  (pass5b/builtin-nargs 'VECTOR iform bb benv ctx))
(define (pass5b/$LIST iform bb benv ctx)
  (pass5b/builtin-nargs 'LIST iform bb benv ctx))
(define (pass5b/$LIST* iform bb benv ctx)
  (pass5b/builtin-nargs 'LIST* iform bb benv ctx))

(define (pass5b/builtin-nargs op iform bb benv ctx)
  (receive (bb regs) (pass5b/prepare-args bb benv ($*-args iform) #t)
    (for-each (cut touch-reg! bb <>) regs)
    (let1 receiver (make-reg bb #f)
      (push-insn bb `(BUILTIN ,op ,receiver ,@regs))
      (pass5b/return bb ctx receiver))))

(define (pass5b/$IT iform bb benv ctx)
  (error "[Intenral] $IT node should be handled by the parent."))

;; Dispatch table.
(define *pass5b-dispatch-table* (generate-dispatch-table pass5b))

;;
;; Simplify bb chains
;;

(define (simplify-bbs! benv)
  (define (skip-jp-insn! bb intermediate dest)
    (match (car (~ bb'insns))
      [('JP _)
       (set-car! (~ bb'insns) `(JP ,dest))
       (link-bb bb dest)]
      [('BR t a b)
       (cond
        [(eq? a intermediate)
         (set-car! (~ bb'insns) `(BR ,t ,dest ,b))
         (link-bb bb dest)]
        [(eq? b intermediate)
         (set-car! (~ bb'insns) `(BR ,t ,a ,dest))
         (link-bb bb dest)]
        [else (error "[internal] Something wrong - can't swap:"
                     (list a b intermediate dest))])]
      [x (error "[internal] Something wrong - can't swap with " x)]))
  (define (swap-jp-to-ret! bb intermediate reg)
    (match (car (~ bb'insns))
      [('JP _) (set-car! (~ bb'insns) `(RET ,reg)) #t]
      [_ #f]))
  (define (erase-downstream! bb down)
    (update! (~ bb'downstream) (^[bbs] (delete down bbs))))
  (define (erase-upstream! bb up)
    (update! (~ bb'upstream) (^[bbs] (delete up bbs))))
  (define (remove-bb! bb)
    (update! (~ bb'benv'blocks) (^[blocks] (delete bb blocks))))
  (define (check-bb bb)
    (when (and (not (~ bb'entry?))
               (length=? (~ bb'insns) 1))
      (let1 i (car (~ bb'insns))
        (match i
          [('JP dest)
           (dolist [u (~ bb'upstream)]
             (erase-downstream! u bb)
             (skip-jp-insn! u bb dest))
           (dolist [d (~ bb'downstream)]
             (erase-upstream! d bb))
           (remove-bb! bb)]
          [('RET reg)
           (dolist [u (~ bb'upstream)]
             (when (swap-jp-to-ret! u bb reg)
               (erase-downstream! u bb)
               (erase-upstream! bb u)))
           (when (null? (~ bb'upstream))
             (remove-bb! bb))]
          [_ #f]))))
  (define (scan benv)
    (for-each check-bb (~ benv'blocks))
    (for-each scan (~ benv'children)))
  (scan benv))

;;
;; For debugging
;;

(define (dump-benv benv :optional (port (current-output-port)))
  (define benv-alist '()) ;; ((benv . N) ...)
  (define (benvname benv)
    #"~(assq-ref benv-alist benv).~(~ benv'name)")
  (define (regname reg)
    (if (is-a? reg <reg>)
      (if (length>? (~ reg'blocks) 1)
        (string->symbol #"~(~ reg'name)*")
        (~ reg'name))
      reg))
  (define (dump-insn insn)
    (match insn
      [('MOV d s)
       (format port "  ~s\n" `(,(car insn) ,(regname d) ,(regname s)))]
      [((or 'LD 'ST) reg id)
       (format port "  (~s ~s ~a#~a)\n" (car insn) (regname reg)
               (~ id'module'name) (~ id'name))]
      [('CLOSE reg benv)
       (format port "  (CLOSE ~s BENV#~a)\n" (regname reg) (benvname benv))]
      [('BR reg bb1 bb2)
       (format port "  (BR ~s ~a ~a)\n"
               (regname reg) (bb-name bb1) (bb-name bb2))]
      [((or 'JP 'CONT) bb)  (format port "  (~s ~a)\n" (car insn) (bb-name bb))]
      [((or 'CALL 'RET) . regs)
       (format port "  ~s\n" `(,(car insn) ,@(map regname regs)))]
      [('ASM insn . args)
       (let ((insn-name (~ (vm-find-insn-info (car insn))'name)))
         (format port "  ~s\n" `(ASM (,insn-name ,@(cdr insn))
                                     ,@(map regname args))))]
      [('BUILTIN op . args)
       (format port "  ~s\n" `(BUILTIN ,op ,@(map regname args)))]
      [('DEF id flags reg)
       (format port "  (DEF ~a#~a ~s ~s)\n" (~ id'module'name) (~ id'name)
               flags (regname reg))]
      [_ (format port "??~s\n" insn)]))
  (define (dump-bb bb)
    (format port "~a\n" (bb-name bb))
    (format port "    ~s → * → ~s\n"
            (map (cut ~ <> 'id) (reverse (~ bb'upstream)))
            (map (cut ~ <> 'id) (reverse (~ bb'downstream))))
    (for-each dump-insn (reverse (~ bb'insns))))
  (define (dump-1 benv)
    (let1 n (x->string (benvname benv))
      (format port "BENV ~a ~a\n"
              n (make-string (- 65 (string-length n)) #\=)))
    (format port "   args: ~s\n" (map regname (~ benv'input-regs)))
    (for-each dump-bb (reverse (~ benv'blocks)))
    (for-each dump-1 (reverse (~ benv'children))))
  (define (assign-benv-names benv)
    (push! benv-alist (cons benv (length benv-alist)))
    (for-each assign-benv-names (reverse (~ benv'children))))
  (assign-benv-names benv)
  (dump-1 benv))

;; For now
(define (compile-to-basic-blocks program :optional (mod (vm-current-module)))
  (let* ([cenv (make-bottom-cenv mod)]
         [iform (pass2-4 (pass1 program cenv) mod)])
    ;(pp-iform iform)
    (let* ([benv (make-benv #f '%toplevel)]
           [bb (pass5b iform benv)])
      (simplify-bbs! benv)
      benv)))

(define (compile-b program)
  (dump-benv (compile-to-basic-blocks program)))
