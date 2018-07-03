;;;
;;; comppile-i.scm - Inliners of builtin procedures
;;;
;;;   Copyright (c) 2004-2018  Shiro Kawai  <shiro@acm.org>
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

;; If the subr has a directly corresponding VM instruction, the
;; inlining direction is embedded within the subr definition in
;; the stub file.  The inliners below deal with more complex
;; situations.

;; Some operations (e.g. NUMADD2) has specialized instructions when
;; one of the operands has certain properties (e.g. if one of the operand
;; is a small exact integer, NUMADDI can be used).  Such choice of
;; instructions are done in Pass 5 $ASM handler, since they may have
;; more information.  The inliner can emit a generic instruction and
;; leave the choice of specialized instructions to the later stage.

;; Defines builtin inliner for the existing SUBRs.
;; The binding of NAME must be visible from gauche.internal.
(define-macro (define-builtin-inliner name proc)
  (let1 debug-name (string->symbol #"inliner/~name")
    `(let1 ,debug-name ,proc
       (set! (%procedure-inliner ,name) ,debug-name)
       (%mark-binding-inlinable! (find-module 'gauche.internal) ',name))))

;; Generate two argument call of assembler insn INSN unconditionally
(define (gen-inliner-arg2 insn)
  (^[src args]
    (match args
      [(x y) ($asm src (list insn) (list x y))]
      [else (undefined)])))

;; If the form has two arg and the latter arg is a constant exact integer
;; that fits insn arg, generate ***I type insn.  If both args are constant,
;; replace the form with ($const (proc x y)).  Otherwise give up.
(define (gen-inliner-argi insn proc)
  (^[src args]
    (match args
      [(n cnt)
       (let ([cnt-val (check-numeric-constant cnt)]
             [n-val   (check-numeric-constant n)])
         (cond [(and cnt-val n-val) ($const (proc n-val cnt-val))]
               [(and cnt-val (integer-fits-insn-arg? cnt-val))
                ($asm src `(,insn ,cnt-val) (list n))]
               [else (undefined)]))]
      [else (undefined)])))

(inline-stub
 (define-cproc %procedure-inliner (proc::<procedure>)
   (setter (proc::<procedure> inliner) ::<void>
           (set! (-> proc inliner) inliner))
   (return (?: (-> proc inliner) (-> proc inliner) '#f)))

 (define-cproc %mark-binding-inlinable! (mod::<module> name::<symbol>) ::<void>
   (let* ([g::ScmGloc* (Scm_FindBinding mod name 0)])
     (unless g
       (Scm_Error "[internal] %%mark-binding-inlinable!: \
                   no such binding for %S in %S"
                  (SCM_OBJ name) (SCM_OBJ mod)))
     (Scm_GlocMark g SCM_BINDING_INLINABLE)))
 )

;;--------------------------------------------------------
;; Inlining numeric operators
;;

;; (1) VM insturctions are usually binary where the corresponding
;;  Scheme operators are variable arity.  We analyze the arguments
;;  and generate a (possibly nested) $asm clause.
;;
;; (2) We try to fold constant operations.  Constant numbers may appear
;;  literally, or a result of constant-variable compilation or other
;;  constant folding.   Except the literal numbers we need to call
;;  pass1 first on the argument to see if we can get a constant.

;; Returns numeric value if iform is a constant number.
(define (check-numeric-constant iform)
  (and ($const? iform)
       (number? ($const-value iform))
       ($const-value iform)))

(define (ensure-inexact-const numconstval)
  ($const (inexact numconstval)))

(define-macro (fold-asm src op insn const x y more)
  `(let loop ([x ,x] [y ,y] [more ,more])
     (let ([xval (check-numeric-constant x)]
           [yval (check-numeric-constant y)])
       (let1 v (if (and xval yval)
                 (,const (,op xval yval))
                 ($asm ,src `(,,insn) `(,,'x ,,'y)))
         (if (null? more) v (loop v (car more) (cdr more)))))))

(define-macro (define-builtin-inliner-+* op unit insn const)
  `(define-builtin-inliner ,op
     (^[src args]
       (match args
         [()  (,const ,unit)]
         [(x) (if-let1 val (check-numeric-constant x)
                (,const val)
                (undefined))]  ; let it be handled at runtime
         [(x y . more) (fold-asm src ,op ,insn ,const x y more)]))))

(define-builtin-inliner-+* +  0 NUMADD2 $const)
(define-builtin-inliner-+* +. 0 NUMIADD2 ensure-inexact-const)
(define-builtin-inliner-+* *  1 NUMMUL2 $const)
(define-builtin-inliner-+* *. 1 NUMIMUL2 ensure-inexact-const)

(define-macro (define-builtin-inliner--/ op insn const)
  `(define-builtin-inliner ,op
     (^[src args]
       (match args
         [() (error "procedure requires at least one argument:" src)]
         [(x) (if-let1 val (check-numeric-constant x)
                (,const (,op val))
                ,(if (eq? op '-)
                   `($asm src `(,NEGATE) (list x))
                   (undefined)))] ; let it be handled at runtime
         [(x y . more) (fold-asm src ,op ,insn ,const x y more)]))))

(define-builtin-inliner--/ -  NUMSUB2  $const)
(define-builtin-inliner--/ -. NUMISUB2 ensure-inexact-const)
(define-builtin-inliner--/ /. NUMIDIV2 ensure-inexact-const)

;; NB: If we detect exact division-by-zero case, we shouldn't fold
;; the constant and let it be handled at runtime.  
(define-builtin-inliner /
  (^[src args]
    (match args
      [() (error "procedure requires at least one argument:" src)]
      [(x) (let1 val (check-numeric-constant x)
             (if (and val (not (eqv? val 0)))
               ($const (/ val))
               (undefined)))]
      [(x y . more)
       ;; can't use fold-asm here because of exact zero check
       (let loop ([x x] [y y] [more more])
         (let ([xval (check-numeric-constant x)]
               [yval (check-numeric-constant y)])
           (let1 v (if (and xval yval (not (and (eqv? yval 0) (exact? xval))))
                     ($const (/ xval yval))
                     ($asm src `(,NUMDIV2) `(,x ,y)))
             (if (null? more) v (loop v (car more) (cdr more))))))])))

(define-builtin-inliner =   (gen-inliner-arg2 NUMEQ2))
(define-builtin-inliner <   (gen-inliner-arg2 NUMLT2))
(define-builtin-inliner <=  (gen-inliner-arg2 NUMLE2))
(define-builtin-inliner >   (gen-inliner-arg2 NUMGT2))
(define-builtin-inliner >=  (gen-inliner-arg2 NUMGE2))

(define-builtin-inliner modulo (gen-inliner-argi NUMMODI modulo))
(define-builtin-inliner remainder (gen-inliner-argi NUMREMI remainder))
(define-builtin-inliner ash (gen-inliner-argi ASHI ash))

;; bitwise and, ior and xor.  we treat (op expr const) case specially.
(define (builtin-inliner-bitwise opname op opcode unit)
  ;; Classify the arguments to (integer) constants and non-constants.
  ;; Integer constants are folded.  Returns cons of the folded constant
  ;; (#f if no constant argument), and the list of iforms for the rest
  ;; of arguments.
  (define (classify-args args)
    (let loop ([args args] [constval #f] [iforms '()])
      (if (null? args)
        (cons constval iforms)
        (let1 val (check-numeric-constant (car args))
          (if (and val (exact-integer? val))
            (loop (cdr args) (if constval (op constval val) val) iforms)
            (loop (cdr args) constval (cons (car args) iforms)))))))

  (^[src args]
    (match (classify-args args)
      [(#f)         ($const unit)]
      [(constval)   ($const constval)]
      [(constval x) ($asm src `(,opcode) (list ($const constval) x))]
      [(#f x y)     ($asm src `(,opcode) (list x y))]
      [_ (undefined)])))

(define-builtin-inliner logand
  (builtin-inliner-bitwise 'logand logand LOGAND -1))
(define-builtin-inliner logior
  (builtin-inliner-bitwise 'logior logior LOGIOR 0))
(define-builtin-inliner logxor
  (builtin-inliner-bitwise 'logxor logxor LOGXOR 0))

;;--------------------------------------------------------
;; Inlining other operators
;;

(define-builtin-inliner vector-ref
  (^[src args]
    (match args
      [(vec ind) ($asm src `(,VEC-REF) (list vec ind))]
      [else (undefined)])))

(define-builtin-inliner vector-set!
  (^[src args]
    (match args
      [(vec ind val) ($asm src `(,VEC-SET) `(,vec ,ind ,val))]
      [else (error "wrong number of arguments for vector-set!:" src)])))

(define-macro (define-builtin-inliner-uvref tag TAG)
  (let ([%-ref (symbol-append tag 'vector-ref)]
        [%type (symbol-append 'SCM_UVECTOR_ TAG)])
    `(define-builtin-inliner ,%-ref
       (^[src args]
         (match args
           [(vec ind) ($asm src `(,UVEC-REF ,,%type) `(,vec ,ind))]
           [else (undefined)])))))
    
(define-builtin-inliner-uvref s8 S8)
(define-builtin-inliner-uvref u8 U8)
(define-builtin-inliner-uvref s16 S16)
(define-builtin-inliner-uvref u16 U16)
(define-builtin-inliner-uvref s32 S32)
(define-builtin-inliner-uvref u32 U32)
(define-builtin-inliner-uvref s64 S64)
(define-builtin-inliner-uvref u64 U64)
(define-builtin-inliner-uvref f16 F16)
(define-builtin-inliner-uvref f32 F32)
(define-builtin-inliner-uvref f64 F64)

(define-builtin-inliner zero?
  (^[src args]
    (match args
      [(arg) ($asm src `(,NUMEQ2) `(,arg ,($const 0)))]
      [else (error "wrong number of arguments for zero?:" src)])))

(define-builtin-inliner acons
  (^[src args]
    (match args
      [(a b c) ($asm src `(,CONS) `(,($asm #f `(,CONS) `(,a ,b)) ,c))]
      [else (error "wrong number of arguments for acons:" src)])))

(define-builtin-inliner reverse
  (^[src args]
    (match args
      [(a) ($asm src `(,REVERSE) `(,a))]
      [else (undefined)])))

(define-builtin-inliner current-input-port
  (^[src args]
     (match args
       [() ($asm src `(,CURIN) '())]
       [else (undefined)])))

(define-builtin-inliner current-output-port
  (^[src args]
    (match args
      [() ($asm src `(,CUROUT) '())]
      [else (undefined)])))

(define-builtin-inliner current-error-port
  (^[src args]
    (match args
      [() ($asm src `(,CURERR) '())]
      [else (undefined)])))

(define-builtin-inliner dynamic-wind
  (^[src args]
    (match args
      [(b t a)
       (let ([at (make-lvar 'after)]
             [bt (make-lvar 'before)]
             [tt (make-lvar 'thunk)]
             [r (make-lvar 'tmp)])
         (if (constant-lambda? a)
           ;; when after thunk is dummy, we don't bother to call it.
           ($let src 'let `(,at ,bt ,tt) `(,a ,b ,t)
                 ($seq
                  `(,($call ($*-src b) ($lref bt) '())
                    ,($asm src `(,PUSH-HANDLERS) `(,($lref bt) ,($lref at)))
                    ,($call ($*-src t) ($lref tt) '()))))
           ;; normal path
           ($let src 'let `(,at ,bt ,tt) `(,a ,b ,t)
                 ($seq
                  `(,($call ($*-src b) ($lref bt) '())
                    ,($asm src `(,PUSH-HANDLERS) `(,($lref bt) ,($lref at)))
                    ,($receive #f 0 1 (list r)
                               ($call ($*-src t) ($lref tt) '())
                               ($seq
                                `(,($asm src `(,POP-HANDLERS) '())
                                  ,($call ($*-src a) ($lref at) '())
                                  ,($asm #f `(,TAIL-APPLY 2)
                                         (list ($gref values.) ($lref r))))))
                    )))))]
      [_ (undefined)])))

