;;;
;;; gauche.cgen.bbb - Basic-blocks backend
;;;
;;;   Copyright (c) 2021-2022  Shiro Kawai  <shiro@acm.org>
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
  (use gauche.sequence)
  (use scheme.list)
  (use scheme.set)
  (use util.match)
  (use gauche.vm.insn)
  (export compile-b
          compile-b/dump  ; for debugging

          bb-name bb-incoming-regs

          cluster-regs
          cluster-needs-dispatch?

          dump-benv
          <reg>
          <const> const-value)
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
;;  REG is either <reg>, <const> or %VAL0.
;;  The register is destination position is always <reg>.
;;  If a <reg> can be set!, the register is 'boxed'.  The boxing/unboxing
;;  aren't explicit in the instruction.
;;
;; General instructions:
;;
;; (MOV dreg sreg)     - dreg <- sreg
;; (MOV* r o (dreg ...) sreg) - mv bind (details TBD)
;; (LD reg identifier) - global load
;; (ST reg identifier) - global store
;; (CLOSE reg bb)      - make a closure with current env and a basic block
;;                       BB, leave it in REG.
;; (BR reg bb1 bb2)    - if the value of register REG is true, jump to
;;                       a basic block BB1.  Otherwise, jump to BB2.
;; (JP bb)             - jump to a basic block BB.
;; (CONT bb)           - push continuation frame, with BB to be the
;;                       'next' basic block.
;; (CALL bb proc arg ...) - PROC and ARGs are all registers.
;;                       Transfer control to PROC. If the continuation of
;;                       this call is known, BB holds the basic block of
;;                       the continuation; it is #f if this is a tail call.
;; (RET reg ...)       - Return, using values in REGs as the result(s).
;; (DEF id flags reg)  - insert global binding of ID in the current module
;;                       with the value in REG.
;;
;; Bulitin operations:
;;  These are tied to the builtin procedures inlined to VM ASMs.
;;  The ones marked with (*) don't directly corresponds to VM insns, but
;;  have a related "immediate" version of insns.
;;
;; (CONS dreg reg1 reg1)
;; (CAR dreg reg)
;; (CDR dreg reg)
;; (CAAR dreg reg)
;; (CADR dreg reg)
;; (CDAR dreg reg)
;; (CDDR dreg reg)
;; (LIST dreg reg ...)
;; (LIST* dreg reg ...)
;; (LENGTH dreg reg)
;; (MEMQ dreg reg1 reg2)
;; (MEMV dreg reg1 reg2)
;; (ASSQ dreg reg1 reg2)
;; (ASSV dreg reg1 reg2)
;; (EQ dreg reg1 reg2)
;; (EQV dreg reg1 reg2)
;; (APPEND dreg reg ...)
;; (NOT dreg reg)
;; (REVERSE dreg reg)
;; (APPLY dreg reg ...)
;; (TAIL-APPLY dreg reg ...)
;; (IS-A reg1 dreg reg2)    - Called only the class arg isn't redefined
;; (NULLP dreg reg)
;; (PAIRP dreg reg)
;; (CHARP dreg reg)
;; (EOFP dreg reg)
;; (STRINGP dreg reg)
;; (SYMBOLP dreg reg)
;; (VECTORP dreg reg)
;; (NUMBERP dreg reg)
;; (REALP dreg reg)
;; (IDENTIFIERP dreg reg)
;; (SETTER dreg reg)
;; (VEC dreg reg ...)
;; (LIST->VEC dreg reg)
;; (APP-VEC dreg reg ...)
;; (VEC-LEN dreg reg)
;; (VEC-REF dreg reg1 reg2)
;; (VEC-SET dreg reg1 reg2 reg3)
;; (UVEC-REF dreg reg1 reg2 reg3)  - The first arg is uvector type
;; (NUMEQ2 dreg reg1 reg2)
;; (NUMLT2 dreg reg1 reg2)
;; (NUMLE2 dreg reg1 reg2)
;; (NUMGT2 dreg reg1 reg2)
;; (NUMGE2 dreg reg1 reg2)
;; (NUMADD2 dreg reg1 reg2)
;; (NUMSUB2 dreg reg1 reg2)
;; (NUMMUL2 dreg reg1 reg2)
;; (NUMDIV2 dreg reg1 reg2)
;; (NUMMOD2 dreg reg1 reg2)    - (*)
;; (NUMREM2 dreg reg1 reg2)    - (*)
;; (NEGATE dreg reg)
;; (ASH dreg reg1 reg2)        - (*)
;; (LOGAND dreg reg1 reg2)
;; (LOGIOR dreg reg1 reg2)
;; (LOGXOR dreg reg1 reg2)
;; ;; READ-CHAR
;; ;; PEEK-CHAR
;; ;; WRITE-CHAR
;; (CURIN dreg)
;; (CUROUT dreg)
;; (CURERR dreg)
;; ;; SLOT-REF - This may call back to VM
;; ;; SLOT-SET - This may call back to VM
;; (UNBOX dreg reg)

(define *builtin-ops*
  `(CONS CAR CDR CAAR CADR CDAR CDDR LIST LIST* LENGTH
    MEMQ MEMV ASSQ ASSV EQ EQV
    APPEND NOT REVERSE APPLY TAIL-APPLY IS-A
    NULLP PAIRP CHARP EOFP STRINGP VECTORP NUMBERP REALP IDENTIFIERP
    SETTER VEC LIST->VEC APP-VEC VEC-LEN VEC-REF VEC-SET UVEC-REF
    NUMEQ2 NUMLT2 NUMLE2 NUMGT2 NUMGE2 NUMADD2 NUMSUB2 NUMMUL2 NUMDIV2
    NUMMOD2 NUMREM2
    NEGATE ASH LOGAND LOGIOR LOGXOR CURIN CUROUT CURERR UNBOX))

;; Basic blocks:
;;   First, we convert IForm to a DG of basic blocks (BBs).
;;   BB has one entry point and multiple exit points.

;; Block environment.  Keep track of allocated registers and blocks,
;; per each closure or toplevel form.
;;
;; BBs are pushed in BLOCKS slot.  For closures, the first BB, BB#0,
;; is a dummy BB to track argument registers and has no insns.
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
   (input-optargs :init-value #f)             ; # of optional args.
   (entry :init-value #f)                     ; entry BB
   (blocks :init-value '())                   ; basic blocks
   (clusters :init-value '())                 ; clusters
   (globals :init-form (make-hash-table default-comparator))
                                              ; <identifier> -> usage
                                              ;  usage being a list of
                                              ;  def, read, write
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
   (introduced :init-keyword :introduced) ; BB that introduced this reg.
   (assigned :init-value '())           ; BBs that assigns this reg. Exclusive
                                        ;   with introduced.
   (used :init-value '())               ; BBs using this reg. Includes assigned.
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
    (use-reg! bb reg)
    (let* ([benv (~ bb'benv)]
           [symname (and lvar (unwrap-syntax (lvar-name lvar)))]
           [name (string->symbol (format "%~d.~d~a" (benv-depth benv)
                                         (length (~ benv'registers))
                                         (if symname #".~symname" "")))])
      (rlet1 reg (make <reg> :name name :lvar lvar :introduced bb)
        (push! (~ benv'registers) reg)
        (when bb
          (push! (~ reg'used) bb)
          (use-reg! bb reg #t))
        (when lvar (hash-table-put! (~ benv'regmap) lvar reg))))))

;; If reg is used within BB, record the fact.
(define (use-reg! bb reg :optional (assign? #f))
  (when (is-a? reg <reg>)
    (push-unique! (~ reg'used) bb)
    (when (and assign? (not (memq bb (~ reg'introduced))))
      (push-unique! (~ reg'assigned) bb))
    (unless (assq reg (~ bb'reg-use))
      (push! (~ bb'reg-use) (cons reg (if assign? 'w 'r)))))
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
   (reg-use    :init-value '())         ; register usage alist
   ;; reg-use keeps how a register is used in the BB first time; it can be
   ;; either w (written) or r (read).
   (cluster    :init-value #f)
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

(define (mark-entry-bb! bb)
  (set! (~ bb'entry?) #t))

(define (bb-name bb)                    ;for debug dump
  (format "BB_~a_~s~a"
          (and (~ bb'cluster) (~ bb'cluster'id))
          (~ bb'id)
          (if (~ bb'entry?) "!" "")))

(define (push-insn bb insn)
  (push! (~ bb'insns) insn))

(define (last-insn bb) (and (pair? (~ bb'insns)) (car (~ bb'insns))))

;; Returns a vetor of registers that needs to be received from a caller,
;; if this bb is an entry bb.
;; Need to be called after register lifetime analysis.
(define (bb-incoming-regs bb)
  (list->vector (filter-map (^p (and (eq? (cdr p) 'r) (car p)))
                            (reverse (~ bb'reg-use)))))

;; A 'complete' basic block is the one that ends with either
;; CALL, BR, JP or RET insn.
(define (bb-complete? bb)
  (match (last-insn bb)
    [(((or 'CALL 'BR 'JP 'RET) . _) . _) #t]
    [_ #f]))

;; A cluster is a DG of BBs that does not contain edges of CALL instructions.
;; A cluster can be compiled into a chunk of codes that share the same
;; stack frame.

(define-class <cluster> ()
  ((id :init-form (gensym "C")) ;for debugging
   (entry? :init-value #f)      ;#t if this cluster is procedure entry
   (benv :init-keyword :benv)
   (blocks :init-value '())     ;basic blocks
   (entry-blocks :init-value '()) ; BBs to be jumped from another cluster
   ;; Register classification.  Used internally.
   ;;  XREGS - Regs outlive this cluster
   ;;  LREGS - Regs local to this cluster
   (xregs :init-form (set eq-comparator))
   (lregs :init-form (set eq-comparator))
   ))

(define (make-cluster benv)
  (rlet1 c (make <cluster> :benv benv)
    (push! (~ benv'clusters) c)))

;; Does cluster needs dispatch on entry?
(define (cluster-needs-dispatch? c)
  (length>? (~ c'entry-blocks) 1))

;; Retunrs a list of cluster registers.  The order is undefined.
(define (cluster-regs c)
  (set-fold cons (set-fold cons '() (~ c'lregs)) (~ c'xregs)))

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
  (hash-table-push! (~ bb'benv'globals) ($define-id iform) 'def)
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
      (use-reg! bb val0)
      (use-reg! bb r #t)
      (push-insn bb `(MOV ,r ,val0))
      (pass5b/return bb ctx r))))

(define (pass5b/$GREF iform bb benv ctx)
  (let1 r (make-reg bb #f)
    (push-insn bb `(LD ,r ,($gref-id iform)))
    (hash-table-push! (~ bb'benv'globals) ($gref-id iform) 'read)
    (pass5b/return bb ctx r)))

(define (pass5b/$GSET iform bb benv ctx)
  (receive (bb val0) (pass5b/rec ($gset-expr iform) bb benv 'normal)
    (push-insn bb `(ST ,val0 ,($gref-id iform)))
    (hash-table-push! (~ bb'benv'globals) ($gref-id iform) 'write)
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
      (begin (use-reg! bb test-result)
             (pass5b/return bb ctx test-result))
      (pass5b/rec iform bb benv ctx)))
  (receive (bb val0) (pass5b/rec ($if-test iform) bb benv 'normal)
    (let ([then-bb (make-bb benv bb)]
          [else-bb (make-bb benv bb)])
      (use-reg! bb val0)
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
                                  (use-reg! ebb eval0)
                                  (use-reg! ebb r #t)
                                  (push-insn ebb `(MOV ,r ,eval0))))])
                   (push-insn ebb `(JP ,cbb))
                   (values cbb r)))
               (if (bb-complete? ebb)
                 (let* ([cbb (make-bb benv tbb)]
                        [r (and (eq? ctx 'normal) tval0
                                (rlet1 r (make-reg cbb #f)
                                  (use-reg! tbb tval0)
                                  (use-reg! tbb r #t)
                                  (push-insn tbb `(MOV ,r ,tval0))))])
                   (push-insn ebb `(JP ,cbb))
                   (values cbb r))
                 (let* ([cbb (make-bb benv tbb ebb)]
                        [r (and (eq? ctx 'normal)
                                (make-reg cbb #f))])
                   (when (and (eq? ctx 'normal) tval0)
                     (use-reg! tbb tval0)
                     (use-reg! tbb r #t)
                     (push-insn tbb `(MOV ,r ,tval0)))
                   (push-insn tbb `(JP ,cbb))
                   (when (and (eq? ctx 'normal) eval0)
                     (use-reg! ebb eval0)
                     (use-reg! ebb r #t)
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
          (use-reg! bb val0)
          (use-reg! bb reg #t)
          (push-insn bb `(MOV ,reg ,val0))
          (loop bb (cdr vars) (cdr inits)))))))

(define (pass5b/$RECEIVE iform bb benv ctx)
  (let1 regs (map (cut make-reg bb <>) ($receive-lvars iform))
    (receive (bb val0) (pass5b/rec ($receive-expr iform) bb benv 'normal)
      (push-insn bb `(MOV* ,($receive-reqargs iform) ,($receive-optarg iform)
                           ,regs ,val0))
      (for-each (cut use-reg! bb <> #t) regs)
      (use-reg! bb val0)
      (pass5b/rec ($receive-body iform) bb benv ctx))))

(define (pass5b/$LAMBDA iform bb benv ctx)
  (let* ([lbenv (make-benv benv ($lambda-name iform))]
         [lbb (~ lbenv'entry)]
         [lbb2 (make-bb lbenv lbb)])
    (set! (~ lbenv'entry) lbb2) ; skip the dummy block
    (set! (~ lbenv'input-regs)
          (map (cut make-reg lbb <>) ($lambda-lvars iform)))
    (set! (~ lbenv'input-reqargs) ($lambda-reqargs iform))
    (set! (~ lbenv'input-optargs) ($lambda-optarg iform))
    (push-insn lbb `(JP ,lbb2))
    (receive (cbb val0) (pass5b/rec ($lambda-body iform) lbb2 lbenv 'tail)
      (let1 reg (make-reg bb #f)
        (push-insn bb `(CLOSE ,reg ,lbenv))
        (pass5b/return bb ctx reg)))))

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
  (define (create-clambda iform bb cbb ctx)
    (receive (bb clo-regs) (generate-closures bb ($clambda-closures iform) benv)
      (for-each (cut use-reg! bb <>) clo-regs)
      (receive (minarg maxarg) (reqargs-min-max ($clambda-argcounts iform))
        (let ([closure-list-reg (make-reg bb #f)]
              [minarg-const (make-const bb minarg)]
              [maxarg-const (make-const bb maxarg)]
              [formals-const (make-const bb #f)]
              [name-const (make-const bb ($clambda-name iform))]
              [make-case-lambda-reg (make-reg bb #f)])
          (push-insn bb `(LIST ,closure-list-reg ,@clo-regs))
          (push-insn bb `(LD ,make-case-lambda-reg ,make-case-lambda.))
          (push-insn bb `(CALL ,cbb
                               ,make-case-lambda-reg
                               ,minarg-const
                               ,maxarg-const
                               ,formals-const
                               ,closure-list-reg
                               ,name-const))
          (values bb #f)))))

  (if (eq? ctx 'tail)
    (create-clambda iform bb #f ctx)
    (let* ([cbb (make-bb benv bb)]
           [valreg (make-reg cbb #f)])
      (push-insn bb `(CONT ,cbb))
      (create-clambda iform bb cbb ctx)      ;no need to receive result.
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
                        (use-reg! bb reg #t)
                        (push-insn bb `(MOV ,reg %VAL0)))
                      reg)
            (values bb (reverse (cons reg regs)))))
        (receive (bb reg) (pass5b/rec (car args) bb benv 'normal)
          (if (eq? reg '%VAL0)
            (let1 reg (make-reg bb #f)
              (use-reg! bb reg #t)
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
                    (use-reg! bb reg)
                    (let1 lreg (make-reg bb lvar)
                      (unless (eq? lreg reg)
                        (use-reg! bb reg)
                        (use-reg! bb lreg #t)
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
                    (use-reg! bb reg)
                    (let1 lreg (make-reg bb lvar)
                      (unless (eq? lreg reg)
                        (use-reg! bb reg)
                        (use-reg! bb lreg #t)
                        (push-insn bb `(MOV ,lreg ,reg)))))
                  regs ($lambda-lvars proc))
        (link-bb bb lbb)
        (push-insn bb `(JP ,lbb))
        (values lbb #f))))                 ;dummy
  (define (normal-call cont-bb cont-reg)
    (when cont-bb (set! (~ cont-bb'entry?) #t))
    (receive (bb regs) (pass5b/prepare-args bb benv ($call-args iform))
      (receive (bb proc) (pass5b/rec ($call-proc iform) bb benv 'normal)
        (for-each (cut use-reg! bb <>) regs)
        (push-insn bb `(CALL ,cont-bb ,proc ,@regs))
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
       [(normal) (let* ([cbb (make-bb benv bb)])
                   (push-insn bb `(CONT ,cbb))
                   (receive (cbb _) (normal-call cbb #f)
                     (let1 valreg (make-reg cbb #f)
                       (push-insn cbb `(MOV ,valreg %VAL0))
                       (values cbb valreg))))])]))

;; NB: Some ASM instructions require c-continuations internally,
;; so we should break them up.
(define (pass5b/$ASM iform bb benv ctx)
  (define (emit bb mnemonic regs)
    (let1 receiver (make-reg bb #f)
      (for-each (cut use-reg! bb <>) regs)
      (use-reg! bb receiver #t)
      (push-insn bb `(,mnemonic ,receiver ,@regs))
      (pass5b/return bb ctx receiver)))
  (receive (bb regs) (pass5b/prepare-args bb benv ($asm-args iform) #t)
    (let* ([opc ($asm-insn iform)]
           [mnemonic (~ (vm-find-insn-info (car opc))'name)])
      (case mnemonic
        [(IS-A)
         (pass5b/de-asm iform 'is-a? bb benv ctx)]
        [(READ-CHAR)
         (pass5b/de-asm iform 'read-char bb benv ctx)]
        [(PEEK-CHAR)
         (pass5b/de-asm iform 'peek-char bb benv ctx)]
        [(SLOT-REF)
         (pass5b/de-asm iform 'slot-ref bb benv ctx)]
        [(SLOT-SET)
         (pass5b/de-asm iform 'slot-set! bb benv ctx)]
        ;; Convert immediate insns to non-immediate ones
        ;; Be careful about the position of the immediate arg
        [(NUMADDI) (emit bb 'NUMADD2 (cons (make-const bb (cadr opc)) regs))]
        [(NUMSUBI) (emit bb 'NUMSUB2 (cons (make-const bb (cadr opc)) regs))]
        [(NUMMODI) (emit bb 'NUMMOD2 (list (car regs) (make-const bb (cadr opc))))]
        [(NUMREMI) (emit bb 'NUMREM2 (list (car regs) (make-const bb (cadr opc))))]
        [(ASHI) (emit bb 'ASH (list (car regs) (make-const bb (cadr opc))))]
        [else (emit bb mnemonic regs)]))))

;; If an ASM insn calls back to VM, we need to turn it back to a
;; normal call.
;; NAME is a symbol for Scheme procedure that does the job.
(define (pass5b/de-asm iform name bb benv ctx)
  (pass5b/rec ($call ($*-src iform)
                     ($gref (make-identifier name
                                             (find-module 'gauche)
                                             '()))
                     ($asm-args iform))
              bb benv ctx))

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
    (for-each (cut use-reg! bb <>) regs)
    (let1 receiver (make-reg bb #f)
      (use-reg! bb receiver #t)
      (push-insn bb `(,op ,receiver ,@regs))
      (pass5b/return bb ctx receiver))))

(define (pass5b/$LIST->VECTOR iform bb benv ctx)
  (receive (bb reg0) (pass5b/rec ($*-arg0 iform) bb benv 'normal)
    (use-reg! bb reg0)
    (let1 receiver (make-reg bb #f)
      (use-reg! bb receiver #t)
      (push-insn bb `(LIST->VECTOR ,receiver ,reg0))
      (pass5b/return bb ctx receiver))))

(define (pass5b/$VECTOR iform bb benv ctx)
  (pass5b/builtin-nargs 'VECTOR iform bb benv ctx))
(define (pass5b/$LIST iform bb benv ctx)
  (pass5b/builtin-nargs 'LIST iform bb benv ctx))
(define (pass5b/$LIST* iform bb benv ctx)
  (pass5b/builtin-nargs 'LIST* iform bb benv ctx))

(define (pass5b/builtin-nargs op iform bb benv ctx)
  (receive (bb regs) (pass5b/prepare-args bb benv ($*-args iform) #t)
    (for-each (cut use-reg! bb <>) regs)
    (let1 receiver (make-reg bb #f)
      (use-reg! bb receiver #t)
      (push-insn bb `(,op ,receiver ,@regs))
      (pass5b/return bb ctx receiver))))

(define (pass5b/$DYNENV iform bb benv ctx)
  ;; TODO: A provisional implementation, ignoring denv key&val.
  ;; A new compiler wraps definition expression with $DYNENV; it only
  ;; adds debug information, so we can ignore it now.
  (pass5b/rec ($dynenv-body iform) bb benv ctx)
  )

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
;; lifetime analysis
;;

;; Make sure a register REG is alive in all paths between its introduction
;; and its use.
;; At the beginning, only the BBs that explicitly use REG are marked so.
;; We have to mark the intermediate BBs of all possible paths.
;;
;; We start from a set of BBs that explicitly use REG (input set), and
;; going upstreams with recording intermediate BBs that are not marked.
;; Once the path hits a BB that introduces REG, mark all intermediate
;; BBs.

(define (mark-live-path-bbs! reg)
  ;; path - hash BB -> (BB1 ...).  Representing we followed BB1 to BB.
  ;;        If we hit 'using' BB, we mark all BBs chained in this path
  ;;        as 'using'.  Note that there can be cycles.
  (define root ($ alist->tree-map
                  (filter-map (^b (and (not (eq? b (~ reg'introduced)))
                                       (list b)))
                              (~ reg'used))
                  eq-comparator))
  (define path (make-hash-table eq-comparator))
  (define (mark-path! bb)
    (unless (memq bb (~ reg'used))
      (use-reg! bb reg)
      (for-each mark-path! (hash-table-get path bb '()))))

  (let repeat ()
    (and-let* ([p (tree-map-pop-min! root)]
               [bb0 (car p)])
      (let loop ([bbs (~ bb0'upstream)])
        (if (null? bbs)
          (repeat)
          (cond [(eq? (car bbs) (~ reg'introduced))
                 (mark-path! bb0)
                 (repeat)]
                [(memq (car bbs) (~ reg'used))
                 (mark-path! bb0)
                 (repeat)]
                [(tree-map-exists? root (car bbs)) (loop (cdr bbs))]
                [(hash-table-exists? path (car bbs)) (loop (cdr bbs))]
                [else
                 (tree-map-put! root (car bbs) '())
                 (hash-table-push! path (car bbs) bb0)
                 (loop (cdr bbs))]))))))

(define (mark-all-live-paths! benv)
  (define (rec benv)
    (dolist [reg (~ benv'registers)]
      (when (is-a? reg <reg>)
        (mark-live-path-bbs! reg)))
    (for-each rec (~ benv'children)))
  (rec benv))

;;
;; BB entry register setup
;;

(define (bb-assigning-registers bb)
  (filter (^r (and (is-a? r <reg>)
                   (memq bb (~ r'assigned))))
          (~ bb'benv'registers)))

(define (bb-using-registers bb)
  (filter (^r (and (is-a? r <reg>)
                   (memq bb (~ r'used))))
          (~ bb'benv'registers)))

(define (bb-receiving-registers bb)
  (lset-difference eq? (bb-using-registers bb) (bb-assigning-registers bb)))

;;
;; Basic block clustering
;;


(define (cluster-bbs! benv)
  (define (visit-1 c bb)
    (cond
     [(eq? (~ bb'cluster) c)]
     [(~ bb'cluster) (error "[internal] something wrong")]
     [else (set! (~ bb'cluster) c)
           (push! (~ c'blocks) bb)
           (visit* c (find-downstream-connections bb))
           (visit* c (find-upstream-connections bb))]))
  (define (visit* c bbs)
    (for-each (cut visit-1 c <>) bbs))
  (define (find-downstream-connections bb)
    (match (last-insn bb)
      [('JP bb1) (list bb1)]
      [('BR _ bb1 bb2) (list bb1 bb2)]
      [_ '()]))
  (define (find-upstream-connections bb)
    (filter (^[ubb] (memq bb (find-downstream-connections ubb)))
            (~ bb'upstream)))
  (define (pick-unvisited bbs)
    (find (^[bb] (not (~ bb'cluster))) bbs))
  (define (gen-clusters cs)
    (if-let1 bb (pick-unvisited (~ benv'blocks))
      (let1 c (make-cluster benv)
        (visit-1 c bb)
        (gen-clusters (cons c cs)))
      cs))
  (define (set-entry-blocks! c)
    (dolist [bb (~ c'blocks)]
      (when (or (eq? bb (~ benv'entry))
                (any (^[ubb] (not (memq bb (find-downstream-connections ubb))))
                     (~ bb'upstream)))
        (push! (~ c'entry-blocks) bb))))

  ;; 1st pass
  (let1 clusters (gen-clusters '())
    (for-each set-entry-blocks! clusters)
    (set! (~ benv'clusters) clusters))
  (for-each cluster-bbs! (~ benv'children))
  ;; 2nd pass (register classification)
  (dolist [c (~ benv'clusters)]
    (classify-cluster-regs! benv c))
  )

;; Called when jump from from-cluster to bb.  If bb is not in
;; from-cluster, we need to mark bb as an entry bb
(define (check-entry-bb! from-cluster bb)
  (when (and (~ bb'cluster) (not (eq? from-cluster (~ bb'cluster))))
    (mark-entry-bb! bb)
    (push-unique! (~ bb'cluster'entry-blocks) bb)))

(define (classify-cluster-regs! benv c)
  (dolist [reg (~ benv'registers)]
    (when (and (is-a? reg <reg>)
               (any (^b (eq? (~ b'cluster) c)) (~ reg'used)))
      (cond [(and (every (^b (eq? (~ b'cluster) c)) (~ reg'used))
                  (not (memq reg (~ benv'input-regs))))
             (update! (~ c'lregs) (cut set-adjoin! <> reg))]
            [else
             (update! (~ c'xregs) (cut set-adjoin! <> reg))]))))

;;
;; For debugging
;;

(define (dump-benv benv :optional (port (current-output-port)))
  (define benv-alist '()) ;; ((benv . N) ...)
  (define (benvname benv)
    #"~(assq-ref benv-alist benv).~(~ benv'name)")
  (define (regname reg)
    (if (is-a? reg <reg>)
      (if (length>? (~ reg'used) 1)
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
      [('CALL bb proc . regs)
       (format port "  (CALL ~a ~s ~s)\n"
               (and bb (bb-name bb)) (regname proc) (map regname regs))]
      [('RET . regs)
       (format port "  ~s\n" `(,(car insn) ,@(map regname regs)))]
      [('DEF id flags reg)
       (format port "  (DEF ~a#~a ~s ~s)\n" (~ id'module'name) (~ id'name)
               flags (regname reg))]
      [((? (cut memq <> *builtin-ops*) op) . regs)
       (format port "  ~s\n" `(,op ,@(map regname regs)))]
      [_ (format port "??~s\n" insn)]))
  (define (dump-bb bb benv)
    (format port " ~a   [~a> >~a]\n" (bb-name bb)
            (map (cut ~ <> 'id) (reverse (~ bb'upstream)))
            (map (cut ~ <> 'id) (reverse (~ bb'downstream))))
    (format port "   reg-use: ~s\n"
            (map (^p (cons (~ (car p)'name) (cdr p))) (~ bb'reg-use)))
    (for-each dump-insn (reverse (~ bb'insns))))
  (define (dump-1 benv)
    (let1 n (x->string (benvname benv))
      (format port "BENV ~a ~a\n"
              n (make-string (- 65 (string-length n)) #\=)))
    (format port "   args: ~s\n" (map regname (~ benv'input-regs)))
    (for-each (cut dump-bb <> benv) (reverse (~ benv'blocks)))
    (for-each dump-1 (reverse (~ benv'children))))
  (define (assign-benv-names benv)
    (push! benv-alist (cons benv (length benv-alist)))
    (for-each assign-benv-names (reverse (~ benv'children))))
  (define (dump-clusters)
    (format #t "~70,,,'=a\n" "Clusters ")
    (dolist [benv (reverse (map car benv-alist))]
      (dolist [cluster (~ benv'clusters)]
        (format #t " CLUSTER ~a" (~ cluster'id))
        (if (memq (~ benv'entry) (~ cluster'blocks) )
          (format #t "  ; Entry of ~a\n" (benvname benv))
          (print))
        (format #t "   BBs: ~s\n" (map bb-name (~ cluster'blocks)))
        (format #t "   Entries: ~s\n" (map bb-name (~ cluster'entry-blocks)))
        )))
  (define (dump-registers)
    (format #t "~70,,,'=a\n" "Registers ")
    (dolist [benv (reverse (map car benv-alist))]
      (format #t " BENV ~d\n" (benvname benv))
      (dolist [reg (reverse (~ benv'registers))]
        (when (is-a? reg <reg>)
          (format #t "  ~15,,,,15a ~s\n" (~ reg'name)
                  (map bb-name (~ reg'used)))))))
  (assign-benv-names benv)
  (dump-1 benv)
  (dump-clusters)
  (dump-registers)
  )

;; For now
(define (compile-b program :optional (mod (vm-current-module)))
  (let* ([cenv (make-bottom-cenv mod)]
         [iform (pass2-4 (pass1 program cenv) mod)])
    (let* ([benv (make-benv #f (gensym "toplevel"))]
           [bb (pass5b iform benv)])
      (simplify-bbs! benv)
      (mark-all-live-paths! benv)
      (cluster-bbs! benv)
      benv)))

(define (compile-b/dump program)
  (dump-benv (compile-b program)))
