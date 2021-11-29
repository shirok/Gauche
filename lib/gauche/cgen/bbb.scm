;;;
;;; bbb.scm - Basic-blocks backend
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
;;
;; (DEF id flags reg)  - insert global binding of ID in the current module
;;                       with the value in REG.

;; Basic blocks:
;;   First, we convert IForm to a DG of basic blocks (BBs).
;;   BB has one entry point and multiple exit point.

(define-class <context> ()
  ((registers :init-form '())                 ; (<List> (</> <reg> <const>))
   (regmap :init-form (make-hash-table 'eq?)) ; lvar -> <reg>
   (cstmap :init-form (make-hash-table 'equal?)) ; const -> <const>
   (blocks :init-value '())))                 ; basic blocks

(define-class <reg> ()
  ((lvar :init-keyword :lvar)           ; source LVAR; can be #f
   (name :init-keyword :name)))         ; given temporary name
(define-method write-object ((reg <reg>) port)
  (if-let1 lvar (~ reg'lvar)
    (format port "#<reg ~a(~a)>" (~ reg'name) (lvar-name lvar))
    (format port "#<reg ~a>" (~ reg'name))))

(define (make-reg ctx lvar)
  (or (hash-table-get (~ ctx'regmap) lvar #f)
      (let1 name (symbol-append '% (length (~ ctx'registers)))
        (rlet1 r (make <reg> :name name :lvar lvar)
          (push! (~ ctx'registers) r)
          (when lvar (hash-table-put! (~ ctx'regmap) lvar r))))))

(define-class <const> ()                ; constant register
  ((value :init-keyword :value)         ; constant value
   (name :init-keyword :name)))         ; given temporary name
(define-method write-object ((cst <const>) port)
  (format port "#<const ~a(~,,,,20:s)>" (~ cst'name) (~ cst'value)))

(define (make-const ctx val)
  (or (hash-table-get (~ ctx'cstmap) val #f)
      (let1 name (symbol-append '% (length (~ ctx'registers)))
        (rlet1 r (make <const> :name name :value val)
          (push! (~ ctx'registers) r)
          (hash-table-put! (~ ctx'cstmap) val r)))))


(define-class <basic-block> ()
  ((insns      :init-value '())   ; list of instructions
   (upstream   :init-value '())   ; list of upstream blocks
   (downstream :init-value '())   ; list of downstream blocks
   ))

(define (make-bb . upstreams)
  (make <basic-block> :upstream upstreams))

;;

(define (push-insn bb insn)
  (push! (~ bb'insns) insn))

;;

;; Main traverser.
(define (iform->bb ctx iform)
  ;; Returns two values, bb and the 'current value', which is either
  ;; <reg>, <const>, or #f.
  (define (rec bb iform)
    (case/unquote
     (iform-tag iform)
     [($DEFINE)(receive (bb val0) (rec bb ($define-expr iform))
                 (push-insn bb `(DEF ,($define-id iform)
                                     ,($define-flags iform)
                                     ,val0))
                 (values bb #f))]
     [($LREF)  (values bb (make-reg ctx ($lref-lvar iform)))]
     [($LSET)  (receive (bb val0) (rec bb ($lset-expr iform))
                 (let1 r (make-reg ctx ($lset-lvar iform))
                   (push-insn bb `(MOV ,r ,val0))
                   (values bb r)))]
     [($GREF)  (let1 r (make-reg ctx #f)
                 (push-insn bb `(LD ,r ($gref-id iform)))
                 (values bb r))]
     [($GSET)  (receive (bb val0) (rec bb ($gset-expr iform))
                 (push-insn bb `(ST ,val0 ($gref-id iform)))
                 (values bb #f))]
     [($CONST) (values bb (make-const ctx ($const-value iform)))]
     [($IF)    (receive (bb val0) (rec bb ($if-test iform))
                 (let ([then-bb (make-bb bb)]
                       [else-bb (make-bb bb)])
                   (push-insn bb `(BR ,val0 ,then-bb ,else-bb))
                   (receive (tbb tval0) (rec then-bb ($if-then iform))
                     (receive (ebb eval0) (rec else-bb ($if-eles iform))
                       (let* ([cbb (make-bb tbb ebb)]
                              [r (make-reg ctx #f)])
                         (when tval0 (push-insn tbb `(MOV ,r ,tval0)))
                         (push-insn tbb '(JP ,cbb))
                         (when eval0 (push-insn ebb `(MOV ,r ,eval0)))
                         (push-insn ebb `(JP ,cbb))
                         (values cbb r))))))]
     [($LET)   (values bb #f)]          ;WRITEME
     [($RECEIVE) (values bb #f)]        ;WRITEME
     [($LAMBDA) (let ([lbb (make-bb)]
                      [reg (make-reg ctx #f)])
                  (receive (lbb val0) (rec lbb ($lambda-body iform))
                    (push-insn lbb `(RET ,val0))
                    (push-insn bb `(CLOSE ,reg ,lbb))
                    (values bb reg)))]
     [($LABEL)  (values bb #f)]
     [($SEQ)   (if (null? ($seq-body iform))
                 (values bb #f)
                 (let loop ([bb bb] [iforms ($seg-body iform)])
                   (if (null? (cdr iforms))
                     (rec bb (car iforms))
                     (receive (bb _) (rec bb (car iforms))
                       (loop bb (cdr iforms))))))]
     [($CALL)  (let1 cbb (make-bb bb)
                 (push-insn bb `(CONT ,cbb))
                 (let loop ([bb bb] [args ($cont-args iform)] [regs '()])
                   (if (null? args)
                     (receive (bb proc) (rec bb ($cont-proc iform))
                       (push-insn bb `(CALL ,proc ,@(reverse regs)))
                       (values cbb 'VAL0))
                     (receive (bb arg) (rec bb (car args))
                       (loop bb (cdr args) (cons arg regs))))))]
     [($ASM)    (let loop ([bb bb] [args ($asm-args iform)] [regs '()])
                  (if (null? args)
                    (begin
                      (push-insn bb `(ASM ,($asm-insn iform) ,@(reverse regs)))
                      (values bb 'VAL0))
                    (receive (bb reg) (rec bb (car args))
                      (loop bb (cdr args) (cons reg regs)))))]
     [($CONS $APPEND $MEMV $EQ? $EQV?)
      ;; 2-arg builtin - temporary
      (receive (bb reg0) (rec bb ($*-arg0 iform))
        (receive (bb reg1) (rec bb ($*-arg1 iform))
          (push-insn bb `(BUILTIN ,(iform-tag iform) ,reg0 ,reg1))
          (values bb 'VAL0)))]
     [($LIST->VECTOR)
      ;; 1-arg bultin - temporary
      (receive (bb reg0) (rec bb ($*-arg0 iform))
        (push-insn bb `(BUILTIN ,(iform-tag iform) ,reg0))
        (values bb 'VAL0))]
     [($VECTOR $LIST $LIST*)
      ;; n-arg builtin - temporary
      (let loop ([bb bb] [args ($*-args iform)] [regs '()])
        (if (null? args)
          (begin
            (push-insn bb `(BUILTIN ,(iform-tag iform) ,@(reverse regs)))
            (values bb 'VAL0))
          (receive (bb r) (rec bb (car args))
            (loop bb (cdr args) (cons reg regs)))))]
     [else (values bb #f)]))
  (rec (make-bb) iform))

;; For debugging



(define (dump-bb bb :optional (port (current-output-port)))
  (define bb-alist '())
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
       (format port "  ~s\n" `(,(car insn) ,(~ d'name) ,(~ s'name)))]
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
      (format port "  Up: ~s\n" (map bb-num (~ bb'upstream)))
      (format port "  Dn: ~s\n" (map bb-num (~ bb'downstream)))
      (for-each dump-insn (reverse (~ bb'insns)))))
  (let loop ()
    (unless (null? bb-toshow)
      (dump-1 (pop! bb-toshow))
      (loop))))
        
(define (compile-b program)
  (let* ([cenv (make-bottom-cenv (vm-current-module))]
         [iform (pass2-4 (pass1 program cenv) (cenv-module cenv))])
    (pp-iform iform)
    (dump-bb (iform->bb (make <context>) iform))))

                      
