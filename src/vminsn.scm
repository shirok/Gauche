;;;
;;; vminsn.scm - Virtual machine instruction definition
;;;
;;;   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

;;; This file is processed by geninsn to produce a couple of C files:
;;; gauche/vminsn.h and vminsn.c, which are then included in vm.c.
;;; This file is also used by the compiler.
;;;
;;;
;;; (define-insn <name> <num-params> <operand-type>
;;;              :optional (<combination> '())
;;;                        (<body> #f)
;;;                        <flags> ...)
;;;
;;;   <name> - instruction name.  In C, an enum SCM_VM_<name> is defined.
;;;
;;;   <num-params> - # of parameters the instruction takes.
;;;                  Can be (N M ...) - in this case, N is the proper
;;;                  <num-params> of this insn, but vm-build-insn tolerates
;;;                  the passed insn to have M parameters as well.  If N < M,
;;;                  extra parameters are ignored.  If N > M, 0 is assumed
;;;                  for missing parameters.  This is mainly to compile
;;;                  Gauche itself with the previous version of Gauche.
;;;
;;;   <operand-type> - none : the insn doesn't take an operand.
;;;                    obj  : an ScmObj operand.
;;;                    addr : an address the next pc points.
;;;                    code : an ScmCompiledCode operand.
;;;                    codes: a list of ScmCompiledCodes.
;;;                    obj+addr : an ScmObj, followed by an address
;;;
;;;   <combination>  - If this is a comibined insn, list the ingredients
;;;                    here.  It is used in multiple purposes:
;;;                    * The instruction body is generated automatically,
;;;                      fusing the ingredients' body.
;;;                    * A DFA is set up in the instruction emitter
;;;                      that replaces this specific sequence of
;;;                      insns to the combined insn.  Because of this,
;;;                      pass5 (instruction generation) doesn't need to
;;;                      handle instruction combination explicitly.
;;;
;;;   <body>         - a CiSE expression that handles the instruction.
;;;                    can be #f for combined insn.
;;;
;;;   <flags>
;;;           :obsoleted  - mark obsoleted insn.  handled but won't be
;;;                         generated.
;;;           :fold-lref  - the insn must be a combination of LREF + something,
;;;                         with depth and offset parameters.
;;;                         this indicates that the combined insn should be
;;;                         emitted even if preceding insn is 'shortcut'
;;;                         LREFs such as LREF0 or LREF21; that is, instead
;;;                         of having LREF0-SOMETHING or LREF21-SOMETHING
;;;                         separately, we'll have LREF-SOMETHING(0,0) and
;;;                         LREF-SOMETHING(2,1), respectively.

;;;==============================================================
;;; Common Cise macros
;;;

;;
;; ($result <expr>)
;;   Emits code to place the value of <expr> as a result.
;;   Depending on the parameter 'result-type', the result will be
;;   either put in VAL0, pushed directly into the stack, or returned.
;;   The parameter result-type is defined in geninsn.

(define-cise-stmt $result
  [(_ expr) `(begin ,@(case (result-type)
                       [(reg)  `((set! VAL0 ,expr)
                                 (set! (-> vm numVals) 1)
                                 NEXT_PUSHCHECK)]
                       [(push) `((PUSH-ARG ,expr) NEXT)]
                       [(call) `((set! VAL0 ,expr))]
                       [(ret)  `((set! VAL0 ,expr)
                                 (set! (-> vm numVals) 1)
                                 (RETURN-OP)
                                 NEXT)]
                       ))])

;; variations of $result with type coercion
(define-cise-stmt $result:b
  [(_ expr) `($result (SCM_MAKE_BOOL ,expr))])
(define-cise-stmt $result:i
  [(_ expr) (let1 r (gensym "cise__")
              `(let* ([,r :: long ,expr])
                 ($result (SCM_MAKE_INT ,r))))])
(define-cise-stmt $result:n
  [(_ expr) (let ([r (gensym "cise__")]
                  [v (gensym "cise__")])
              `(let* ([,r :: long ,expr] [,v])
                 (if (SCM_SMALL_INT_FITS ,r)
                   (set! ,v (SCM_MAKE_INT ,r))
                   (set! ,v (Scm_MakeInteger ,r)))
                 ($result ,v)))])
(define-cise-stmt $result:u
  [(_ expr) (let ([r (gensym "cise__")]
                  [v (gensym "cise__")])
              `(let* ([,r :: u_long ,expr] [,v])
                 (if (SCM_SMALL_INT_FITS ,r)
                   (set! ,v (SCM_MAKE_INT ,r))
                   (set! ,v (Scm_MakeIntegerU ,r)))
                 ($result ,v)))])
(define-cise-stmt $result:f
  [(_ expr) (let1 r (gensym "cise__")
              `(let* ([,r :: double ,expr])
                 ($result (Scm_VMReturnFlonum ,r))))])

;; Extract local value.
;; If local var nees unboxing, necessary UNBOX insn will be generated
;; by the compiler, so we don't need to worry about it.
(define-cise-stmt $lref!
  [(_ var env off)
   `(set! ,var (ENV-DATA ,env ,off))])

;;
;; ($w/argr <val> <expr> ...)
;; ($w/argp <val> <expr> ...)
;;   Get one arg from various sources, depending on the parameter
;;   'arg-source'.  If 'arg-source' is #f, argr takes the value from
;;   VAL0 while argp pops the value from the stack.
;;   The parameter 'arg-source' is defined in geninsn.
;;

(define-cise-stmt $w/arg_               ; internal
  [(_ val default body)
   (match (or (arg-source) default)
     ['pop        `(let* ((,val)) (POP-ARG ,val) ,@body)]
     ['reg        `(let* ((,val VAL0)) ,@body)]
     ['lref     (let ((dep (gensym)) (off (gensym)) (e (gensym)))
                  `(let* ((,dep :: int (SCM_VM_INSN_ARG0 code))
                          (,off :: int (SCM_VM_INSN_ARG1 code))
                          (,e :: ScmEnvFrame* ENV)
                          (,val))
                     (case/fallthrough ,dep
                       [(4) (set! ,e (-> ,e up))]
                       [(3) (set! ,e (-> ,e up))]
                       [(2) (set! ,e (-> ,e up))]
                       [(1) (set! ,e (-> ,e up))]
                       [(0) ($lref! ,val ,e ,off) (break)]
                       [else (while (> (post-- ,dep) 0) (set! ,e (-> ,e up)))
                             ($lref! ,val ,e ,off) (break)])
                     ,@body))]
     [('lref d o) `(let* ((,val (ENV-DATA ,(let loop ((d d))
                                             (if (zero? d)
                                               'ENV
                                               `(-> ,(loop (- d 1)) up)))
                                          ,o)))
                     ,@body)])])

(define-cise-stmt $w/argr               ; use VAL0 default
  [(_ var . body) `($w/arg_ ,var reg ,body)])

(define-cise-stmt $w/argp               ; pop default
  [(_ var . body) `($w/arg_ ,var pop ,body)])

;;
;; ($arg-source src body ...)
;;   Switch arg-source to SRC in the BODY
;;
(define-cise-stmt $arg-source           ; override arg-source
  env
  [(_ src . body)
   (parameterize ((arg-source src))
     (cise-render-rec `(begin ,@body) 'stmt env))])

;;
;; ($insn-body insn)
;;   Returns the body of INSN.
;;
(define-cise-stmt $insn-body
  [(_ insn-name)
   (or (and-let* ([insn (assq-ref (insn-alist) insn-name)])
         (ref insn'body))
       (error "cannot take the body of the instruction:" insn-name))])

;;
;; ($vm-err fmt args ...)
;;   Report error.  This used to be a call to VM_ERR macro, but some
;;   compilers choke when #line directive is inserted between the macro
;;   arguments.
;;
(define-cise-stmt $vm-err
  [(_ . args) `(Scm_Error ,@args)])

;;
;; ($obsoleted name)
;;  Mark obsoleted instruction.
;;
(define-cise-stmt $obsoleted
  [(_ name)
   `($vm-err "%s instruction is obsoleted.  Using wrong compiler version?"
             ,(cgen-safe-name-friendly (x->string name)))])

;;
;; ($type-check VAR PRED WHAT)
;;   Common type checking.  Unless (PRED VAR), bail out.
;;
(define-cise-stmt $type-check
  [(_ var pred what)
   `(unless (,pred ,var)
      ($vm-err "%s required, but got %S" ,what ,var))])

;;
;; ($goto-insn INSN)
;;   Jump to the entry of the instruction INSN
;;
(define-cise-stmt $goto-insn
  [(_ insn)
   `(,(format "goto label_~a;" (cgen-safe-name-friendly (x->string insn))))])

;;
;; ($branch EXPR)
;; ($branch* EXPR)
;;   Branch.  $branch* leaves the result in VAL0.
;;
(define-cise-stmt $branch
  [(_ expr) `(begin (if ,expr (FETCH-LOCATION PC) INCR-PC) CHECK-INTR NEXT)])
(define-cise-stmt $branch*
  [(_ expr)
   `(begin
      (if ,expr
        (begin (set! VAL0 SCM_FALSE) (FETCH-LOCATION PC))
        (begin (set! VAL0 SCM_TRUE) INCR-PC))
      CHECK-INTR
      NEXT)])

;;
;; ($retc EXPR)
;; ($retc* EXPR)
;;   Conditional return.  $retc* leaves #f in VAL0.
;;
(define-cise-stmt $retc
  [(_ expr) `(begin (when ,expr (RETURN-OP)) NEXT)])
(define-cise-stmt $retc*
  [(_ expr)
   `(begin (when ,expr
             (set! VAL0 SCM_FALSE)
             (set! (-> vm numVals) 1)
             (RETURN-OP))
           NEXT)])

;;
;; ($w/numcmp r op . body)
;;   Compare arg (default stack top) and VAL0 with OP, and places the result
;;   in r.
(define-cise-stmt $w/numcmp
  [(_ r op . body)
   (let ([x (gensym)] [y (gensym)]
         [cmp (case op
                [(=) 'Scm_NumEq]
                [(<) 'Scm_NumLT] [(<=) 'Scm_NumLE]
                [(>) 'Scm_NumGT] [(>=) 'Scm_NumGE]
                [else (error "[internal] invalid op for $w/numcmp" op)])])
     `($w/argp ,x
        (let* ((,y VAL0) (,r :: int))
          (cond [(and (SCM_INTP ,x) (SCM_INTP ,y))
                 (set! ,r (,op (cast (signed long) (cast intptr_t ,x))
                               (cast (signed long) (cast intptr_t ,y))))]
                [(and (SCM_FLONUMP ,x) (SCM_FLONUMP ,y))
                 (set! ,r (,op (SCM_FLONUM_VALUE ,x) (SCM_FLONUM_VALUE ,y)))]
                [else
                 (set! ,r (,cmp ,x ,y))])
          ,@body)))])

;;
;; ($undef var)
;; ($define var)
;; ($include var)
;;   Preprocessor directives.
;;
(define-cise-stmt $undef
  [(_ var) `("\n" |#reset-line| "#undef " ,(x->string var) "\n")])
(define-cise-stmt $define
  [(_ var) `("\n" |#reset-line| "#define " ,(x->string var) "\n")])
(define-cise-stmt $include
  [(_ var) `("\n" |#reset-line| "#include " ,(write-to-string var) "\n")])

;;
;; ($lrefNN depth offset)
;;   Generate an expr to refer to the local variable, starting from ENV.
;;
(define-cise-stmt $lrefNN
  [(_ depth offset)
   (let1 v (gensym)
     `(let* ([,v :: ScmObj (ENV_DATA ,(let loop ((d depth))
                                        (case d
                                          [(0) 'ENV]
                                          [else `(-> ,(loop (- d 1)) up)]))
                                     ,offset)])
        ($result ,v)))])

;;
;; ($values)
;;    Generate common code for VALUES and VALUES-RET.
;;
(define-cise-stmt $values
  [(_)
   '(let* ([nargs::int (SCM_VM_INSN_ARG code)]
           [i::int (- nargs 1)]
           [v VAL0])
      (when (>= nargs SCM_VM_MAX_VALUES) ($vm-err "values got too many args"))
      (VM-ASSERT (<= (- nargs 1) (- SP (-> vm stackBase))))
      (when (> nargs 0)
        (for [() (> i 0) (post-- i)]
             (set! (aref (-> vm vals) (- i 1)) v)
             (POP-ARG v)))
      (set! VAL0 v)
      (set! (-> vm numVals) nargs))])

;;;==============================================================
;;; Instruction definitions
;;;

;; NOP
;;  Used for placeholder.   Won't appear in the final compiled code.
;;
(define-insn NOP   0 none #f
  NEXT)

;; CONST <obj>
;;  Set <obj> to val0.
;;
(define-insn CONST 0 obj #f
  (let* ([val])
    (FETCH-OPERAND val)
    INCR-PC
    ($result val)))

;; Some immediate constants
(define-insn CONSTI 1 none #f ($result:i (SCM_VM_INSN_ARG code))) ; small int
(define-insn CONSTN 0 none #f ($result SCM_NIL))                  ; ()
(define-insn CONSTF 0 none #f ($result SCM_FALSE))                ; #f
(define-insn CONSTU 0 none #f ($result SCM_UNDEFINED))            ; #<undef>

;; Combined insn
(define-insn CONST-PUSH  0 obj   (CONST PUSH))
(define-insn CONSTI-PUSH 1 none  (CONSTI PUSH))
(define-insn CONSTN-PUSH 0 none  (CONSTN PUSH))
(define-insn CONSTF-PUSH 0 none  (CONSTF PUSH))
(define-insn CONST-RET   0 obj   (CONST RET))
(define-insn CONSTF-RET  0 none  (CONSTF RET))
(define-insn CONSTU-RET  0 none  (CONSTU RET))

;; push
;;  Push value of val0 to the stack top
;;  *sp++ = val0
;;
(define-insn PUSH  0 none #f
  (begin (CHECK-STACK-PARANOIA 1) (PUSH-ARG VAL0) NEXT))

;; PRE-CALL(nargs) <cont>
;;  Prepare for a normal call.   Push a continuation that resumes
;;  execution from <cont>.
;;
(define-insn PRE-CALL 1 addr #f
  (let* ([next::ScmWord*])
    (CHECK-STACK-PARANOIA CONT_FRAME_SIZE)
    (FETCH-LOCATION next)
    (PUSH-CONT next)
    INCR-PC
    CHECK-INTR
    NEXT))

;; combined insn
(define-insn PUSH-PRE-CALL 1 addr (PUSH PRE-CALL))

;; CHECK-STACK(size)
;;  Check for stack overflow
;;
(define-insn CHECK-STACK 1 none #f
  (let* ([reqstack::int (SCM_VM_INSN_ARG code)])
    (CHECK-STACK reqstack)
    NEXT))

;; CALL(NARGS)
;;  Call procedure in val0.  The continuation of this call is already
;;  pushed by PRE-CALL, so this instruction is always the end of a graph.
;;
(define-insn CALL     1 none #f
  (let* ([nm] [argc::int] [proctype::int])
    (label call_entry)
    ($undef APPLY_CALL)
    ($include "./vmcall.c")
    (label tail_apply_entry)
    ($define APPLY_CALL)
    ($include "./vmcall.c")))

;; TAIL-CALL(NARGS)
;;  Call procedure in val0.  Same as CALL except this discards the
;;  caller's arugment frame and shift the callee's argument frame.
;;
(define-insn TAIL-CALL 1 none #f
  (begin (DISCARD-ENV) ($goto-insn CALL)))

;; JUMP <addr>
;;  Jump to <addr>.
;;
(define-insn JUMP      0 addr #f
  (begin (FETCH-LOCATION PC) CHECK-INTR NEXT))

;; RET
;;  Pop the continuation stack.
;;
(define-insn RET       0 none #f
  (begin (RETURN-OP) CHECK-INTR NEXT))

;; DEFINE(flag) <symbol>
;;  Defines global binding of SYMBOL in the current module.
;;  The value is taken from the input stack.
;;  This instruction only appears at the toplevel.  Internal defines
;;  are recognized and eliminated by the compiling process.
;;  Flag can be 0, SCM_BINDING_CONST(2) or SCM_BINDING_INLINABLE(4).
;;  (flag == 1 is also supported for the backward compatibility; remove
;;  it after 0.9.1 release!)
(define-insn DEFINE     1 obj #f
  (let* ([var] [val VAL0])
    (FETCH-OPERAND var)
    (VM_ASSERT (SCM_IDENTIFIERP var))
    (SCM_FLONUM_ENSURE_MEM val)
    INCR-PC
    (let* ([id::ScmIdentifier* (Scm_OutermostIdentifier (SCM_IDENTIFIER var))]
           [mod::ScmModule* (-> id module)]
           [name::ScmSymbol* (SCM_SYMBOL (-> id name))])
      (case (SCM_VM_INSN_ARG code)        ;flag
        [(0)                    (Scm_MakeBinding mod name val 0)]
        [(1 SCM_BINDING_CONST)  (Scm_MakeBinding mod name val SCM_BINDING_CONST)]
        [(SCM_BINDING_INLINABLE)(Scm_MakeBinding mod name val SCM_BINDING_INLINABLE)])
      ($result (SCM_OBJ name)))))

;; CLOSURE <code>
;;  Create a closure capturing current environment.
;;  CODE is the compiled code.   Leaves created closure in val0.
;;
(define-insn CLOSURE    0 code #f
  (let* ((body))
    (FETCH-OPERAND body)
    INCR-PC
    ($result (Scm_MakeClosure body (get_env vm)))))

;; LOCAL-ENV(NLOCALS)
;;  Create a new environment frame from the current arg frame.
;;  Used for let.
(define-insn LOCAL-ENV  1 none #f
  (begin (CHECK-STACK-PARANOIA (ENV-SIZE 0))
         (FINISH-ENV SCM_FALSE ENV)
         NEXT))

;; combined insn
(define-insn PUSH-LOCAL-ENV 1 none (PUSH LOCAL-ENV)
  (begin (CHECK-STACK-PARANOIA (ENV-SIZE 1))
         (PUSH-ARG VAL0)
         (FINISH-ENV SCM_FALSE ENV)
         NEXT))

;; LOCAL-ENV-CLOSURES(nlocals) <codelist>
;;  A special instruction for efficient handling of letrec.
;;  Similar to LOCAL-ENV, but this doesn't use the current arg frame.
;;  The operand contains a literal list of <compiled-code>s or <closure>s.
;;  This instruction creates a frame of NLOCALS size and initializes each
;;  slot with each element of <codelist>.   If the element is a
;;  <compiled-code>, a new closure is created with the environment
;;  including the frame just created.   If the element is a <closure>,
;;  it is just used (such <closure> does not refer to the outside environment).
;;  CODELIST can have 'holes', i.e. if it has #<undef>,
;;  the corresponding frame entry is left undefined.
;;  This instruction also leaves the last closure in VAL0.
(define-insn LOCAL-ENV-CLOSURES 1 codes #f
  (let* ([nlocals::int (SCM_VM_INSN_ARG code)]
         [z::ScmObj*] [cp] [clo SCM_UNDEFINED] [e::ScmEnvFrame*])
    (FETCH-OPERAND cp)
    INCR-PC
    (CHECK-STACK-PARANOIA (ENV-SIZE nlocals))
    (dotimes [i nlocals] (set! (* (post++ SP)) SCM_UNDEFINED))
    (FINISH-ENV SCM_FALSE ENV)
    (set! e (get_env vm))
    (set! z (- (cast ScmObj* e) nlocals))
    (dolist [c cp]
      (cond [(SCM_COMPILED_CODE_P c)
             (set! (* (post++ z)) (set! clo (Scm_MakeClosure c e)))]
            [(SCM_PROCEDUREP c) (set! (* (post++ z)) c) (set! clo c)]
            [else (set! (* (post++ z)) c)]))
    ($result clo)))

;; POP-LOCAL-ENV
;;  Pop one environment frame created by LOCAL-ENV.  In practice,
;;  this is only used if 'let' is in the non-tail bottom position
;;  (for other cases, the env frame is discarded along other frame
;;  operations).
(define-insn POP-LOCAL-ENV 0 none #f
  (begin (set! ENV (-> ENV up)) NEXT))

;; LOCAL-ENV-JUMP(DEPTH) <addr>
;;  Combination of LOCAL-ENV-SHIFT + JUMP.
;;  We can use this when none of the new environment needs boxing.
(define-insn LOCAL-ENV-JUMP 1 addr #f
  (begin
    (local_env_shift vm (SCM_VM_INSN_ARG code))
    (FETCH-LOCATION PC)
    CHECK-INTR
    NEXT))

;; LOCAL-ENV-CALL(DEPTH)
;; LOCAL-ENV-TAIL-CALL(DEPTH)
;;  This instruction appears when local function call is optimized.
;;  VAL0 has a closure to call, and the stack already has the arguments.
;;
;;  This instruction creates an env frame with the existing
;;  values (just like LOCAL-ENV), then jump to the entrance point of
;;  the closure in VAL0.  The point is that we can bypass the generic
;;  CALL sequence, since the arguments are already adjusted and we
;;  know the called closure is not a generic function.
;;  (# of arguments can be known by SP - ARGP).
(define-insn LOCAL-ENV-CALL  1 none #f
  (let* ([nargs::int (cast int (- SP ARGP))])
    (VM-ASSERT (SCM_CLOSUREP VAL0))
    (cond [(> nargs 0)
           (CHECK-STACK-PARANOIA (ENV-SIZE 0))
           (FINISH-ENV SCM_FALSE (-> (SCM_CLOSURE VAL0) env))]
          [else
           (set! ENV (-> (SCM_CLOSURE VAL0) env))
           (set! ARGP SP)])
    (set! (-> vm base) (SCM_COMPILED_CODE (-> (SCM_CLOSURE VAL0) code)))
    (set! PC (-> vm base code))
    (CHECK-STACK (-> vm base maxstack))
    CHECK-INTR
    (SCM_PROF_COUNT_CALL vm (SCM_OBJ (-> vm base)))
    NEXT))

(define-insn LOCAL-ENV-TAIL-CALL 1 none #f
  (let* ([nargs::int (cast int (- SP ARGP))] [to::ScmObj*])
    (VM-ASSERT (SCM_CLOSUREP VAL0))
    (if (IN-STACK-P (cast ScmObj* CONT))
      (set! to (CONT-FRAME-END CONT))
      (set! to (-> vm stackBase)))
    (when (and (> nargs 0) (!= to ARGP))
      (let* ([t::ScmObj* to] [a::ScmObj* ARGP])
        (dotimes [c nargs]
          (set! (* (post++ t)) (* (post++ a))))))
    (set! ARGP to)
    (set! SP (+ to nargs))
    ($goto-insn LOCAL-ENV-CALL)))

;; BF <else-offset>          ; branch if VAL0 is false
;; BT <else-offset>          ; branch if VAL0 is true
;; BNNULL <else-offset>      ; branch if VAL0 is not null
;; BNEQ <else-offset>        ; branch if VAL0 is not eq? to (POP)
;; BNEQV <else-offset>       ; branch if VAL0 is not eqv? to (POP)
;; BNUMNE  <else-offset>     ; branch if (VAL0 != (POP))
;; BNLT  <else-offset>     ; branch if !((POP) < VAL0)
;; BNLE  <else-offset>     ; branch if !((POP) <= VAL0)
;; BNGT  <else-offset>     ; branch if !((POP) > VAL0)
;; BNGE  <else-offset>     ; branch if !((POP) >= VAL0)
;;   Conditional branches.
;;   The combined operations leave the boolean value of the test result
;;   in VAL0.
(define-insn BF      0 addr #f ($branch (SCM_FALSEP VAL0)))
(define-insn BT      0 addr #f ($branch (not (SCM_FALSEP VAL0))))
(define-insn BNEQ    0 addr #f ($w/argp z ($branch* (not (SCM_EQ VAL0 z)))))
(define-insn BNEQV   0 addr #f ($w/argp z ($branch* (not (Scm_EqvP VAL0 z)))))
(define-insn BNNULL  0 addr #f ($branch* (not (SCM_NULLP VAL0))))

(define-insn BNUMNE  0 addr #f (let* ((y VAL0))
                                 ($w/argp x ($branch* (not (Scm_NumEq x y))))))
(define-insn BNLT    0 addr #f ($w/numcmp r <  ($branch* (not r))))
(define-insn BNLE    0 addr #f ($w/numcmp r <= ($branch* (not r))))
(define-insn BNGT    0 addr #f ($w/numcmp r >  ($branch* (not r))))
(define-insn BNGE    0 addr #f ($w/numcmp r >= ($branch* (not r))))

;; Compare LREF(n,m) and VAL0 and branch.  This is not a simple combination
;; of LREF + BNLT etc. (which would compare stack top and LREF).  These insns
;; save one stack operation.  The compiler recognizes the pattern and
;; emits these.  See pass5/if-numcmp and pass5/if-numeq.
(define-insn LREF-VAL0-BNUMNE 2 addr #f ($arg-source lref ($insn-body BNUMNE)))
(define-insn LREF-VAL0-BNLT 2 addr #f ($arg-source lref ($insn-body BNLT)))
(define-insn LREF-VAL0-BNLE 2 addr #f ($arg-source lref ($insn-body BNLE)))
(define-insn LREF-VAL0-BNGT 2 addr #f ($arg-source lref ($insn-body BNGT)))
(define-insn LREF-VAL0-BNGE 2 addr #f ($arg-source lref ($insn-body BNGE)))

;; BNUMNEI(i) <else-offset> ; combined CONSTI(i) + BNUMNE
;; BNEQC <else-offset>     ; branch if immediate constant is not eq? to VAL0
;; BNEQVC <else-offset>    ; branch if immediate constant is not eqv? to VAL0
;;   NB: we tried other variations of constant op + branch combination,
;;       notably BNEQVI, BNUMNEF, BNLTF etc, but they did't show any
;;       improvement.
(define-insn BNUMNEI     1 addr #f
  (let* ([imm::long (SCM_VM_INSN_ARG code)])
    ($w/argr v0
      ($type-check v0 SCM_NUMBERP "number")
      ($branch*
       (not (or (and (SCM_INTP v0)    (== (SCM_INT_VALUE v0) imm))
                (and (SCM_FLONUMP v0) (== (SCM_FLONUM_VALUE v0) imm))))))))
(define-insn BNEQC       0 obj+addr #f
  (let* ([z]) (FETCH-OPERAND z) INCR-PC ($branch* (not (SCM_EQ VAL0 z)))))
(define-insn BNEQVC      0 obj+addr #f
  (let* ([z]) (FETCH-OPERAND z) INCR-PC ($branch* (not (Scm_EqvP VAL0 z)))))

;; RF
;; RT
;; RNNULL
;; RNEQ
;; RNEQV
;;   Conditional returns.
(define-insn RF          0 none #f ($retc (SCM_FALSEP VAL0)))
(define-insn RT          0 none #f ($retc (not (SCM_FALSEP VAL0))))
(define-insn RNEQ        0 none #f ($w/argp v
                                     ($retc* (not (SCM_EQ VAL0 v)))))
(define-insn RNEQV       0 none #f ($w/argp v
                                     ($retc* (not (Scm_EqvP VAL0 v)))))
(define-insn RNNULL      0 none #f ($retc* (not (SCM_NULLP VAL0))))

;;
;; Common stuff for RECEIVE and TAIL-RECEIVE.
;;
(define-cise-stmt $receive
  [(_ . stmts)
   `(let* ([reqargs::int (SCM_VM_INSN_ARG0 code)]
           [restarg::int (SCM_VM_INSN_ARG1 code)]
           [size::int] [i::int 0] [argsize::int] [rest SCM_NIL] [tail SCM_NIL]
           [nextpc::ScmWord*])
      (when (< (-> vm numVals) reqargs)
        ($vm-err "received fewer values than expected"))
      (when (and (not restarg) (> (-> vm numVals) reqargs))
        ($vm-err "received more values than expected"))
      (set! argsize (+ reqargs (?: restarg 1 0)))
      ,@stmts
      (cond [(> reqargs 0) (PUSH-ARG VAL0) (post++ i)]
            [(and restarg (> (-> vm numVals) 0))
             (SCM_APPEND1 rest tail VAL0)
             (post++ i)])
      (for [() (< i reqargs) (post++ i)]
           (PUSH-ARG (aref (-> vm vals) (- i 1))))
      (when restarg
        (for [() (< i (-> vm numVals)) (post++ i)]
             (SCM_APPEND1 rest tail (aref (-> vm vals) (- i 1))))
        (PUSH-ARG rest))
      (FINISH-ENV SCM_FALSE ENV)
      (set! (-> vm numVals) 1) ; we already processed extra vals, so reset it
      NEXT)])

;; RECEIVE(nargs,restarg) <cont-offset>
;;  Primitive operation for receive and call-with-values.
;;  Turn the value(s) into an environment.
;;  Like LET, this pushes the continuation frame to resume the
;;  operation from CONT-OFFSET.
;;
(define-insn RECEIVE     2 addr #f
  ($receive (set! size (+ CONT_FRAME_SIZE (ENV_SIZE (+ reqargs restarg))))
            (CHECK-STACK-PARANOIA size)
            (FETCH-LOCATION nextpc)
            INCR-PC
            (PUSH-CONT nextpc)))

;; TAIL-RECEIVE(nargs,restarg)
;;  Tail position of receive.  No need to push the continuation.
;;  Actually, TAIL-RECEIVE can be used anywhere if there's no
;;  argument pushed after the last continuation frame.  See TAIL-LET.
;;
(define-insn TAIL-RECEIVE 2 none #f
  ($receive (set! size (ENV_SIZE (+ reqargs restarg)))))

;; RECEIVE-ALL <cont>
;;  A special version of RECEIVE that pushes all the current values
;;  into the stack and makes them into an environment.
;;  It is used primarily for the occasions that compiler knows it needs
;;  to save the values temporarily (e.g. for evaluate 'after' thunk of
;;  dynamic-wind, the results of its body needs to be saved).
;;  This must be twined with VALUES-N, which reverses the effects, i.e.
;;  turn the values in the env frame into values.
(define-insn RECEIVE-ALL 0 addr #f
  (let* ([nextpc::ScmWord*])
    (CHECK-STACK-PARANOIA CONT_FRAME_SIZE)
    (FETCH-LOCATION nextpc)
    INCR_PC
    (PUSH-CONT nextpc)
    ($goto-insn TAIL-RECEIVE-ALL)))

;; TAIL-RECEIVE-ALL
;;  Tail version of RECEIVE-ALL.
(define-insn TAIL-RECEIVE-ALL 0 none #f
  (begin (CHECK-STACK-PARANOIA (ENV-SIZE (+ (-> vm numVals) 1)))
         (PUSH-ARG VAL0)
         (dotimes [i (- (-> vm numVals) 1)]
           (PUSH-ARG (aref (-> vm vals) i)))
         (FINISH-ENV SCM_FALSE ENV)
         NEXT))

;; VALUES-N
;;  Inverse of RECEIVE-ALL.  Transfer the current environment content
;;  to the values register, and pop the env.
;;  Differ from VALUES since this one doesn't know # of values beforehand.
(define-insn VALUES-N 0 none #f
  (begin
    (VM-ASSERT ENV)
    (let* ([nvals::int (cast int (-> ENV size))] [v])
      (set! (-> vm numVals) nvals)
      (for [() (> nvals 1) (post-- nvals)]
           (POP-ARG (aref (-> vm vals) (- nvals 1))))
      (POP-ARG VAL0)
      NEXT)))

;; LSET(depth, offset)
;;  Local set
;;
(define-insn LSET        2 none #f
  (let* ([dep::int (SCM_VM_INSN_ARG0 code)]
         [off::int (SCM_VM_INSN_ARG1 code)]
         [e::ScmEnvFrame* ENV])
    (for [() (> dep 0) (post-- dep)]
         (VM-ASSERT (!= e NULL))
         (set! e (-> e up)))
    (VM-ASSERT (!= e NULL))
    (VM-ASSERT (> (-> e size) off))
    (SCM_FLONUM_ENSURE_MEM VAL0)
    (let* ([box (ENV-DATA e off)])
      (VM_ASSERT (SCM_BOXP box))
      (SCM_BOX_SET box VAL0))
    (set! (-> vm numVals) 1)
    NEXT))

;; GSET <location>
;;  LOCATION may be a symbol or gloc
;;
(define-insn GSET        0 obj #f
  (let* ((loc))
    (FETCH-OPERAND loc)
    (SCM_FLONUM_ENSURE_MEM VAL0)
    (cond
     [(SCM_GLOCP loc) (SCM_GLOC_SET (SCM_GLOC loc) VAL0)]
     [(SCM_IDENTIFIERP loc)
      ;; If runtime flag LIMIT_MODULE_MUTATION is set,
      ;; we search only for the id's module, so that set! won't
      ;; mutate bindings in the other module.
      (let* ([id::ScmIdentifier* (Scm_OutermostIdentifier (SCM_IDENTIFIER loc))]
             [name::ScmSymbol* (SCM_SYMBOL (-> id name))]
             [limit::int
              (SCM_VM_RUNTIME_FLAG_IS_SET vm SCM_LIMIT_MODULE_MUTATION)]
             [gloc::ScmGloc*
              (Scm_FindBinding (-> id module) name
                               (?: limit SCM_BINDING_STAY_IN_MODULE 0))])
        (when (== gloc NULL)
          ;; Do search again for meaningful error message
          (when limit
            (set! gloc (Scm_FindBinding (-> id module) name 0))
            (when (!= gloc NULL)
              ($vm-err "can't mutate binding of %S, \
                        which is in another module"
                       (-> id name)))
            ;; FALLTHROUGH
            )
          ($vm-err "symbol not defined: %S" loc))
        (SCM_GLOC_SET gloc VAL0)
        ;; memoize gloc.
        (set! (* PC) (SCM_WORD gloc)))]
     [else ($vm-err "GSET: can't be here")])
    INCR-PC
    (set! (-> vm numVals) 1)
    NEXT))

;; LREF(depth,offset)
;;  Retrieve local value.
;;
(define-insn LREF        2 none #f
  (let* ([dep::int (SCM_VM_INSN_ARG0 code)]
         [off::int (SCM_VM_INSN_ARG1 code)]
         [e::ScmEnvFrame* ENV])
    (for [() (> dep 0) (post-- dep)]
         (set! e (-> e up)))
    ($result (ENV-DATA e off))))

;; Shortcut for the frequent depth/offset.
;; From statistics, we found that the following depth/offset combinations
;; are quite frequent:
;;  (0,0) (0,1) (0,2) (0,3)
;;  (1,0) (1,1) (1,2)
;;  (2,0) (2,1)
;;  (3,0)
(define-insn LREF0  0 none #f ($lrefNN 0 0))
(define-insn LREF1  0 none #f ($lrefNN 0 1))
(define-insn LREF2  0 none #f ($lrefNN 0 2))
(define-insn LREF3  0 none #f ($lrefNN 0 3))
(define-insn LREF10 0 none #f ($lrefNN 1 0))
(define-insn LREF11 0 none #f ($lrefNN 1 1))
(define-insn LREF12 0 none #f ($lrefNN 1 2))
(define-insn LREF20 0 none #f ($lrefNN 2 0))
(define-insn LREF21 0 none #f ($lrefNN 2 1))
(define-insn LREF30 0 none #f ($lrefNN 3 0))

;; combined instrction
(define-insn LREF-PUSH   2 none  (LREF PUSH))
(define-insn LREF0-PUSH  0 none  (LREF0 PUSH))
(define-insn LREF1-PUSH  0 none  (LREF1 PUSH))
(define-insn LREF2-PUSH  0 none  (LREF2 PUSH))
(define-insn LREF3-PUSH  0 none  (LREF3 PUSH))
(define-insn LREF10-PUSH 0 none  (LREF10 PUSH))
(define-insn LREF11-PUSH 0 none  (LREF11 PUSH))
(define-insn LREF12-PUSH 0 none  (LREF12 PUSH))
(define-insn LREF20-PUSH 0 none  (LREF20 PUSH))
(define-insn LREF21-PUSH 0 none  (LREF21 PUSH))
(define-insn LREF30-PUSH 0 none  (LREF30 PUSH))

;; GREF <location>
;;  LOCATION may be a symbol or GLOC object.
;;  Retrieve global value in the current module.
;;
(define-insn GREF        0 obj #f
  (let* ((v)) (GLOBAL-REF v) ($result v)))

;; combined instructions
;;  NB: PUSH-GREF itself isn't a very useful form, but it works as a
;;  transient state to PUSH-GREF-CALL / PUSH-GREF-TAIL-CALL, which are
;;  very frequent operation, during instruction combining.

(define-insn GREF-PUSH   0 obj   (GREF PUSH))
(define-insn GREF-CALL   1 obj   (GREF CALL))
(define-insn GREF-TAIL-CALL 1 obj (GREF TAIL-CALL))

(define-insn PUSH-GREF   0 obj      (PUSH GREF))
(define-insn PUSH-GREF-CALL 1 obj   (PUSH GREF CALL))
(define-insn PUSH-GREF-TAIL-CALL 1 obj (PUSH GREF TAIL-CALL))

(define-insn LREF0-PUSH-GREF 0 obj  (LREF0 PUSH GREF) #f :obsoleted)
(define-insn LREF0-PUSH-GREF-CALL 1 obj (LREF0 PUSH GREF CALL) #f :obsoleted)
(define-insn LREF0-PUSH-GREF-TAIL-CALL 1 obj (LREF0 PUSH GREF TAIL-CALL) #f :obsoleted)

;; PROMISE
;;  Delay syntax emits this instruction.  Wrap a procedure into a promise
;;  object.
;;
(define-insn PROMISE  0 none #f
  ($result (Scm_MakePromise FALSE VAL0)))

;; VALUES_APPLY(nargs) <args>
;;  This instruction only appears in the code generated dynamically
;;  by Scm_Apply(Rec).  This is used to pass the application information
;;  across the boundary frame (see user_eval_inner() in vm.c).
;;  When the VM sees this instruciton, VAL0 contains the procedure to
;;  call, and VAL1... contains the arguments.
;;  If nargs >= SCM_VM_MAX_VALUES-1, args[SCM_VM_MAX_VALUES-1] through
;;  args[nargs-1] are made into a list and stored in VALS[SCM_VM_MAX_VALUES-1]
(define-insn VALUES-APPLY 0 none #f
  (let* ([nargs::int (SCM_VM_INSN_ARG code)])
    (CHECK-STACK (ENV-SIZE nargs))
    (dotimes [i nargs]
      (when (>= i (- SCM_VM_MAX_VALUES 1))
        (for-each (lambda (vv) (PUSH-ARG vv))
                  (aref (-> vm vals) (- SCM_VM_MAX_VALUES 1)))
        (break))
      (PUSH-ARG (aref (-> vm vals) i)))
    ($goto-insn TAIL-CALL)))

;; Inlined operators
;;  They work the same as corresponding Scheme primitives, but they are
;;  directly interpreted by VM, skipping argument processing part.
;;  Compiler may insert these in order to fulfill the operation (e.g.
;;  `case' needs MEMV).  If the optimization level is high, global
;;  reference of those primitive calls in the user code are replaced
;;  as well.
;;
(define-insn CONS        0 none #f
  (let* ([ca]) (POP-ARG ca) ($result (Scm_Cons ca VAL0))))
(define-insn CONS-PUSH   0 none   (CONS PUSH))

(define-insn CAR         0 none #f
  ($w/argr v ($type-check v SCM_PAIRP "pair") ($result (SCM_CAR v))))
(define-insn CAR-PUSH    0 none   (CAR PUSH))
(define-insn-lref+ LREF-CAR 0 none (LREF CAR))

(define-insn CDR         0 none #f
  ($w/argr v ($type-check v SCM_PAIRP "pair") ($result (SCM_CDR v))))
(define-insn CDR-PUSH    0 none   (CDR PUSH))
(define-insn-lref+ LREF-CDR 0 none (LREF CDR))

(define-cise-stmt $cxxr
  [(_ a b)
   `($w/argr obj
      ($type-check obj SCM_PAIRP "pair")
      (let* ([obj2 (,b obj)])
        ($type-check obj2 SCM_PAIRP "pair")
        ($result (,a obj2))))])

(define-insn CAAR        0 none #f ($cxxr SCM_CAR SCM_CAR))
(define-insn CAAR-PUSH   0 none (CAAR PUSH))
(define-insn CADR        0 none #f ($cxxr SCM_CAR SCM_CDR))
(define-insn CADR-PUSH   0 none (CADR PUSH))
(define-insn CDAR        0 none #f ($cxxr SCM_CDR SCM_CAR))
(define-insn CDAR-PUSH   0 none (CDAR PUSH))
(define-insn CDDR        0 none #f ($cxxr SCM_CDR SCM_CDR))
(define-insn CDDR-PUSH   0 none (CDDR PUSH))

(define-insn LIST        1 none #f
  (let* ([nargs::int (SCM_VM_INSN_ARG code)] [cp SCM_NIL] [arg])
    (when (> nargs 0)
      (SCM_FLONUM_ENSURE_MEM VAL0)
      (set! cp (Scm_Cons VAL0 cp))
      (while (> (pre-- nargs) 0)
        (POP-ARG arg)
        (set! cp (Scm_Cons arg cp))))
    ($result cp)))

(define-insn LIST-STAR   1 none #f      ; list*
  (let* ([nargs::int (SCM_VM_INSN_ARG code)] [cp SCM_NIL] [arg])
    (when (> nargs 0)
      (SCM_FLONUM_ENSURE_MEM VAL0)
      (set! cp VAL0)
      (while (> (pre-- nargs) 0)
        (POP-ARG arg)
        (set! cp (Scm_Cons arg cp))))
    ($result cp)))

(define-insn LENGTH      0 none #f      ; length
  (let* ([len::int (Scm_Length VAL0)])
    (when (< len 0) ($vm-err "proper list required, but got %S" VAL0))
    ($result:i len)))

(define-insn MEMQ 0 none #f ($w/argp v ($result (Scm_Memq v VAL0))))
(define-insn MEMV 0 none #f ($w/argp v ($result (Scm_Memv v VAL0))))
(define-insn ASSQ 0 none #f ($w/argp v ($result (Scm_Assq v VAL0))))
(define-insn ASSV 0 none #f ($w/argp v ($result (Scm_Assv v VAL0))))
(define-insn EQ   0 none #f ($w/argp v ($result:b (SCM_EQ v VAL0))))
(define-insn EQV  0 none #f ($w/argp v ($result:b (Scm_EqvP v VAL0))))

(define-insn APPEND      1 none #f
  (let* ([nargs::int (SCM_VM_INSN_ARG code)] [cp SCM_NIL] [args SCM_NIL] [a])
    (when (> nargs 0)
      (SCM_FLONUM_ENSURE_MEM VAL0)
      (set! cp VAL0)
      ;; We want to pop all args before doing works, for Scm_Length may cause
      ;; forcing lazy-pair.
      (while (> (pre-- nargs) 0)
        (POP-ARG a)
        (set! args (Scm_Cons a args)))
      ;; Now it' safe to work on args (note that we work from tail to head).
      (dolist [a (Scm_ReverseX args)]
        (when (< (Scm_Length a) 0) ($vm-err "list required, but got %S" a))
        (set! cp (Scm_Append2 a cp))))
    ($result cp)))

(define-insn NOT      0 none #f ($w/argr v ($result:b (SCM_FALSEP v))))
(define-insn REVERSE  0 none #f ($w/argr v ($result (Scm_Reverse v))))

(define-insn APPLY       1 none #f
  ;; this instruction will go away soon.  for now it only appears
  ;; as the result of 'cond' with srfi-61 extension.
  (let* ([nargs::int (SCM_VM_INSN_ARG code)] [cp])
    (SCM_FLONUM_ENSURE_MEM VAL0)
    (while (> (pre-- nargs) 1)
      (POP-ARG cp)
      (set! VAL0 (Scm_Cons cp VAL0)))
    (set! cp VAL0)                      ; now cp has arg list
    (POP-ARG VAL0)                      ; get proc
    (TAIL-CALL-INSTRUCTION)
    (set! VAL0 (Scm_VMApply VAL0 cp))
    NEXT))

(define-insn TAIL-APPLY  1 none #f
  ;; Inlined apply.  Assumes the call is at the tail position.
  ;; NB: As of 0.9, all 'apply' call is expanded into this instruction.
  ;; If the code is not at the tail position, compiler pass5 inserts
  ;; PRE-CALL instruction so that the call of apply becomes a tail call.
  ;;
  ;; Here, the stack should have the following layout.
  ;;
  ;;   SP  >|      |
  ;;        | argN |
  ;;        |   :  |
  ;;        | arg0 |
  ;;   ARGP>| proc |        VAL0=rest
  ;;
  ;; where N = SCMVM_INSN_ARG(code)-2.
  ;; rest contains a list of "all the rest arguments".
  ;; We "rotate" the stack and VAL0 and jump to the
  ;; TAIL-CALL processing.  (The unfolding of rest
  ;; argument will be done in ADJUST_ARGUMENT_FRAME later,
  ;; if necessary.)
  ;;
  ;;   SP  >|      |
  ;;        | rest |
  ;;        | argN |
  ;;        |   :  |
  ;;   ARGP>| arg0 |        VAL0=proc
  ;;
  (let* ([rest VAL0]
         [rargc::int (check_arglist_tail_for_apply vm rest)]
         [nargc::int (- (SCM_VM_INSN_ARG code) 2)]
         [proc (* (- SP nargc 1))])
    (when (< rargc 0) ($vm-err "improper list not allowed: %S" rest))
    (while (> nargc 0)
      (set! (* (- SP nargc 1)) (* (- SP nargc)))
      (post-- nargc))
    ;; a micro-optimization: if VAL0 is (), we just omit it and
    ;; pretend this is a normal TAIL-CALL.
    (when (== rargc 0)
      (post-- SP)
      (set! VAL0 proc)
      ($goto-insn TAIL-CALL))
    ;; normal path
    (set! (* (- SP 1)) rest)
    (set! VAL0 proc)
    (DISCARD-ENV)
    (goto tail_apply_entry)))

(define-insn IS-A        0 none #f      ; is-a?
  ($w/argp obj
    ($type-check VAL0 SCM_CLASSP "class")
    (let* ([c::ScmClass* (SCM_CLASS VAL0)])
      ;; be careful to handle class redifinition case
      (cond [(not (SCM_FALSEP (-> (Scm_ClassOf obj) redefined)))
             (Scm__VMProtectStack vm)
             ($result (Scm_VMIsA obj c))]
            [else ($result:b (SCM_ISA obj c))]))))

(define-insn NULLP       0 none #f ($w/argr v ($result:b (SCM_NULLP v))))
(define-insn PAIRP       0 none #f ($w/argr v ($result:b (SCM_PAIRP v))))
(define-insn CHARP       0 none #f ($w/argr v ($result:b (SCM_CHARP v))))
(define-insn EOFP        0 none #f ($w/argr v ($result:b (SCM_EOFP v))))
(define-insn STRINGP     0 none #f ($w/argr v ($result:b (SCM_STRINGP v))))
(define-insn SYMBOLP     0 none #f ($w/argr v ($result:b (SCM_SYMBOLP v))))
(define-insn VECTORP     0 none #f ($w/argr v ($result:b (SCM_VECTORP v))))
(define-insn IDENTIFIERP 0 none #f ($w/argr v ($result:b (SCM_IDENTIFIERP v))))
(define-insn NUMBERP     0 none #f ($w/argr v ($result:b (SCM_NUMBERP v))))
(define-insn REALP       0 none #f ($w/argr v ($result:b (SCM_REALP v))))

(define-insn SETTER      0 none #f ($w/argr v ($result (Scm_Setter v))))

(define-insn VALUES      1 none #f (begin ($values) NEXT))
(define-insn VALUES-RET  1 none (VALUES RET) (begin ($values) (RETURN-OP) NEXT))

(define-insn VEC         1 none #f      ; vector
  (let* ([nargs::int (SCM_VM_INSN_ARG code)]
         [i::int (- nargs 1)]
         [vec (Scm_MakeVector nargs SCM_UNDEFINED)])
    (when (> nargs 0)
      (let* ([arg VAL0])
        (for [() (> i 0) (post-- i)]
             (SCM_FLONUM_ENSURE_MEM arg)
             (set! (SCM_VECTOR_ELEMENT vec i) arg)
             (POP-ARG arg))
        (SCM_FLONUM_ENSURE_MEM arg)
        (set! (SCM_VECTOR_ELEMENT vec 0) arg)))
    ($result vec)))

(define-insn LIST2VEC    0 none #f      ; list->vector
  ($w/argr v ($result (Scm_ListToVector v 0 -1))))

(define-insn APP-VEC     1 none #f      ; (compose list->vector append)
  (let* ([nargs::int (SCM_VM_INSN_ARG code)] [cp SCM_NIL] [args SCM_NIL] [a])
    (when (> nargs 0)
      (SCM_FLONUM_ENSURE_MEM VAL0)
      (set! cp VAL0)
      (while (> (pre-- nargs) 0)
        (POP-ARG a)
        (set! args (Scm_Cons a args)))
      (dolist [a (Scm_ReverseX args)]
        (when (< (Scm_Length a) 0) ($vm-err "list required, but got %S" a))
        (set! cp (Scm_Append2 a cp))))
    ($result (Scm_ListToVector cp 0 -1))))

(define-insn VEC-LEN     0 none #f      ; vector-length
  ($w/argr v
    ($type-check v SCM_VECTORP "vector")
    ($result:i (SCM_VECTOR_SIZE v))))

(define-insn VEC-REF     0 none #f      ; vector-ref
  (let* ([k VAL0])
    ($w/argp vec
      ($type-check vec SCM_VECTORP "vector")
      ($type-check k SCM_INTP "fixnum")
      (when (or (< (SCM_INT_VALUE k) 0)
                (>= (SCM_INT_VALUE k) (SCM_VECTOR_SIZE vec)))
        ($vm-err "vector-ref index out of range: %S" k))
      ($result (SCM_VECTOR_ELEMENT vec (SCM_INT_VALUE k))))))

(define-insn VEC-SET     0 none #f      ; vector-set
  (let* ([vec] [ind])
    (POP-ARG ind)
    (POP-ARG vec)
    ($type-check vec SCM_VECTORP "vector")
    ($type-check ind SCM_INTP "fixnum")
    (let* ([k::int (SCM_INT_VALUE ind)] [v VAL0])
      (when (or (< k 0) (>= k (SCM_VECTOR_SIZE vec)))
        ($vm-err "vector-set! index out of range: %d" k))
      (SCM_FLONUM_ENSURE_MEM v)
      (set! (SCM_VECTOR_ELEMENT vec k) v)
      ($result SCM_UNDEFINED))))

;; VEC-REF and VEC-SET with immediate index.  VAL0 must be a vector.
(define-insn VEC-REFI    1 none #f
  ($w/argr vec
    ($type-check vec SCM_VECTORP "vector")
    (let* ([k::int (SCM_VM_INSN_ARG code)])
      (when (or (< k 0) (>= k (SCM_VECTOR_SIZE vec)))
        ($vm-err "vector-ref index out of range: %d" k))
      ($result (SCM_VECTOR_ELEMENT vec k)))))

(define-insn VEC-SETI    1 none #f
  ($w/argp vec
    ($type-check vec SCM_VECTORP "vector")
    (let* ([k::int (SCM_VM_INSN_ARG code)] [v VAL0])
      (when (or (< k 0) (>= k (SCM_VECTOR_SIZE vec)))
        ($vm-err "vector-set! index out of range: %d" k))
      (SCM_FLONUM_ENSURE_MEM v)
      (set! (SCM_VECTOR_ELEMENT vec k) v)
      ($result SCM_UNDEFINED))))

(define-insn UVEC-REF    1 none #f    ; uvector-ref
  (let* ([k VAL0]
         [utype::int (SCM_VM_INSN_ARG code)])
    ($w/argp vec
      (unless (SCM_UVECTOR_SUBTYPE_P vec utype)
        ($vm-err "%s required, but got %S" (Scm_UVectorTypeName utype) vec))
      ($type-check k SCM_INTP "fixnum")
      (when (or (< (SCM_INT_VALUE k) 0)
                (>= (SCM_INT_VALUE k) (SCM_UVECTOR_SIZE vec)))
        ($vm-err "uvector-ref index out of range: %S" k))
      ($result (Scm_VMUVectorRef (SCM_UVECTOR vec) utype (SCM_INT_VALUE k)
                                 SCM_UNBOUND)))))

;; not enough evidence yet to support this is worth
;; (define-insn UVEC-REFI   1 none #f    ; uvector-ref, index in arg.
;;   (let* ([arg::int (SCM_VM_INSN_ARG code)]
;;          [utype::uint (logand arg #x0f)]
;;          [k::uint (>> arg 4)])
;;     ($w/argr vec
;;       (unless (SCM_UVECTOR_SUBTYPE_P vec utype)
;;         ($vm-err "%s required, but got %S" (Scm_UVectorTypeName utype) vec))
;;       (when (>= k (SCM_UVECTOR_SIZE vec))
;;         ($vm-err "uvector-ref index out of range: %d" k))
;;       ($result (Scm_VMUVectorRef (SCM_UVECTOR vec) utype k SCM_UNBOUND)))))

(define-insn NUMEQ2      0 none #f      ; =
  ($w/argp arg
    (cond
     [(and (SCM_INTP VAL0) (SCM_INTP arg)) ($result:b (== VAL0 arg))]
     [(and (SCM_FLONUMP VAL0) (SCM_FLONUMP arg))
      ($result:b (== (SCM_FLONUM_VALUE VAL0) (SCM_FLONUM_VALUE arg)))]
     [else ($result:b (Scm_NumEq arg VAL0))])))

(define-insn NUMLT2  0 none #f ($w/numcmp r <  ($result:b r)))
(define-insn NUMLE2  0 none #f ($w/numcmp r <= ($result:b r)))
(define-insn NUMGT2  0 none #f ($w/numcmp r >  ($result:b r)))
(define-insn NUMGE2  0 none #f ($w/numcmp r >= ($result:b r)))

(define-insn NUMADD2 0 none #f          ; +
  ($w/argp arg
    (cond
     [(and (SCM_INTP arg) (SCM_INTP VAL0))
      ($result:n (+ (SCM_INT_VALUE arg) (SCM_INT_VALUE VAL0)))]
     [(and (SCM_FLONUMP arg) (SCM_FLONUMP VAL0))
      ($result:f (+ (SCM_FLONUM_VALUE arg) (SCM_FLONUM_VALUE VAL0)))]
     [else ($result (Scm_Add arg VAL0))])))

(define-insn NUMSUB2 0 none #f          ; -  (binary)
  ($w/argp arg
    (cond
     [(and (SCM_INTP arg) (SCM_INTP VAL0))
      ($result:n (- (SCM_INT_VALUE arg) (SCM_INT_VALUE VAL0)))]
     [(and (SCM_FLONUMP arg) (SCM_FLONUMP VAL0))
      ($result:f (- (SCM_FLONUM_VALUE arg) (SCM_FLONUM_VALUE VAL0)))]
     [else ($result (Scm_Sub arg VAL0))])))

(define-insn NUMMUL2 0 none #f          ; *
  ($w/argp arg
    ;; we take a shortcut if either one is flonum and the
    ;; other is real.  (if both are integers, the overflow check
    ;; would be cumbersome so we just call Scm_Mul).
    (if (or (and (SCM_FLONUMP arg) (SCM_REALP VAL0))
            (and (SCM_FLONUMP VAL0) (SCM_REALP arg)))
      ($result:f (* (Scm_GetDouble arg) (Scm_GetDouble VAL0)))
      ($result (Scm_Mul arg VAL0)))))

(define-insn NUMDIV2 0 none #f          ; / (binary)
  ($w/argp arg
    (if (or (and (SCM_FLONUMP arg) (SCM_REALP VAL0))
            (and (SCM_FLONUMP VAL0) (SCM_REALP arg)))
      ($result:f (/ (Scm_GetDouble arg) (Scm_GetDouble VAL0)))
      ($result (Scm_Div arg VAL0)))))

(define-insn LREF-VAL0-NUMADD2 2 none #f ($arg-source lref ($insn-body NUMADD2)))

(define-insn NEGATE  0 none #f          ; -  (unary)
  ($w/argr v
    (cond
     [(SCM_INTP v)    ($result:n (- (SCM_INT_VALUE v)))]
     [(SCM_FLONUMP v) ($result:f (- (Scm_GetDouble v)))]
     [else            ($result (Scm_Negate v))])))

(define-insn NUMIADD2    0 none #f      ; +.
  ($w/argp arg
    (if (and (SCM_REALP arg) (SCM_REALP VAL0))
      ($result:f (+ (Scm_GetDouble arg) (Scm_GetDouble VAL0)))
      ($result (Scm_Add (Scm_Inexact arg)
                        (Scm_Inexact VAL0))))))

(define-insn NUMISUB2    0 none #f      ; -. (binary)
  ($w/argp arg
    (if (and (SCM_REALP arg) (SCM_REALP VAL0))
      ($result:f (- (Scm_GetDouble arg) (Scm_GetDouble VAL0)))
      ($result (Scm_Sub (Scm_Inexact arg) (Scm_Inexact VAL0))))))

(define-insn NUMIMUL2    0 none #f      ; *.
  ($w/argp arg
    (if (and (SCM_REALP arg) (SCM_REALP VAL0))
      ($result:f (* (Scm_GetDouble arg) (Scm_GetDouble VAL0)))
      ($result (Scm_Mul (Scm_Inexact arg) (Scm_Inexact VAL0))))))

(define-insn NUMIDIV2    0 none #f      ; /. (binary)
  ($w/argp arg
    (if (and (SCM_FLONUMP arg) (SCM_FLONUMP VAL0))
      ($result:f (/ (Scm_GetDouble arg) (Scm_GetDouble VAL0)))
      ($result (Scm_VMDivInexact arg VAL0)))))

(define-insn NUMADDI     1 none #f      ; +, if one of op is small int
  (let* ([imm::long (SCM_VM_INSN_ARG code)])
    ($w/argr arg
      (cond [(SCM_INTP arg) ($result:n (+ imm (SCM_INT_VALUE arg)))]
            [(SCM_FLONUMP arg)
             ($result:f (+ (SCM_FLONUM_VALUE arg) (cast double imm)))]
            [else           ($result (Scm_Add (SCM_MAKE_INT imm) arg))]))))

(define-insn-lref+ LREF-NUMADDI 1 none (LREF NUMADDI))
(define-insn-lref+ LREF-NUMADDI-PUSH 1 none (LREF NUMADDI PUSH))

(define-insn NUMSUBI     1 none #f      ; -, if one of op is small int
  (let* ([imm::long (SCM_VM_INSN_ARG code)])
    ($w/argr arg
      (cond [(SCM_INTP arg) ($result:n (- imm (SCM_INT_VALUE arg)))]
            [(SCM_FLONUMP arg)
             ($result:f (- (cast double imm) (SCM_FLONUM_VALUE arg)))]
            [else           ($result (Scm_Sub (SCM_MAKE_INT imm) arg))]))))


;(define-insn NUMQUOT     0 none)        ; quotient
;(define-insn NUMMOD      0 none)        ; modulo
;(define-insn NUMREM      0 none)        ; remainder

(define-insn ASHI         1 none #f
  (let* ([cnt::ScmSmallInt (SCM_VM_INSN_ARG code)])
    ($w/argr arg ($result (Scm_Ash arg cnt)))))
(define-insn LOGAND       0 none #f
  ($w/argp x ($result (Scm_LogAnd x VAL0))))
(define-insn LOGIOR       0 none #f
  ($w/argp x ($result (Scm_LogIor x VAL0))))
(define-insn LOGXOR       0 none #f
  ($w/argp x ($result (Scm_LogXor x VAL0))))
(define-insn LOGANDC      0 obj #f
  (let* ([obj])
    ($w/argr x (FETCH-OPERAND obj) INCR-PC ($result (Scm_LogAnd x obj)))))
(define-insn LOGIORC      0 obj #f
  (let* ([obj])
    ($w/argr x (FETCH-OPERAND obj) INCR-PC ($result (Scm_LogIor x obj)))))
(define-insn LOGXORC      0 obj #f
  (let* ([obj])
    ($w/argr x (FETCH-OPERAND obj) INCR-PC ($result (Scm_LogXor x obj)))))

(define-insn READ-CHAR   1 none #f      ; read-char
  (let* ([nargs::int (SCM_VM_INSN_ARG code)] [ch::int 0] [port::ScmPort*])
    (cond [(== nargs 1)
           ($type-check VAL0 SCM_IPORTP "input port")
           (set! port (SCM_PORT VAL0))]
          [else
           (set! port SCM_CURIN)])
    (set! ch (Scm_Getc port))
    ($result (?: (< ch 0) SCM_EOF (SCM_MAKE_CHAR ch)))))

(define-insn PEEK-CHAR   1 none #f      ; peek-char
  (let* ([nargs::int (SCM_VM_INSN_ARG code)] [ch::int 0] [port::ScmPort*])
    (cond [(== nargs 1)
           ($type-check VAL0 SCM_IPORTP "input port")
           (set! port (SCM_PORT VAL0))]
          [else
           (set! port SCM_CURIN)])
    (set! ch (Scm_Peekc port))
    ($result (?: (< ch 0) SCM_EOF (SCM_MAKE_CHAR ch)))))

(define-insn WRITE-CHAR  1 none #f      ; write-char
  (let* ([nargs::int (SCM_VM_INSN_ARG code)] [ch] [port::ScmPort*])
    (cond [(== nargs 2)
           ($type-check VAL0 SCM_OPORTP "output port")
           (set! port (SCM_PORT VAL0))
           (POP-ARG ch)]
          [else (set! port SCM_CUROUT
                      ch VAL0)])
    ($type-check ch SCM_CHARP "character")
    (SCM_PUTC (SCM_CHAR_VALUE ch) port)
    ($result SCM_UNDEFINED)))

(define-insn CURIN       0 none #f      ; current-input-port
  ($result (SCM_OBJ (-> vm curin))))
(define-insn CUROUT      0 none #f      ; current-output-port
  ($result (SCM_OBJ (-> vm curout))))
(define-insn CURERR      0 none #f      ; current-error-port
  ($result (SCM_OBJ (-> vm curerr))))

(define-insn SLOT-REF    0 none #f      ; slot-ref
  ($w/argp obj
    (TAIL-CALL-INSTRUCTION)
    (SCM_FLONUM_ENSURE_MEM VAL0)
    ($result (Scm_VMSlotRef obj VAL0 FALSE))))

(define-insn SLOT-SET    0 none #f      ; slot-set!
  (let* ((slot))
    (POP-ARG slot)
    ($w/argp obj
      (TAIL-CALL-INSTRUCTION)
      (SCM_FLONUM_ENSURE_MEM slot)
      (SCM_FLONUM_ENSURE_MEM VAL0)
      ($result (Scm_VMSlotSet obj slot VAL0)))))

(define-insn SLOT-REFC   0 obj #f       ; slot-ref with constant slot name
  (let* ((slot))
    (FETCH-OPERAND slot)
    INCR-PC
    (TAIL-CALL-INSTRUCTION)
    (SCM_FLONUM_ENSURE_MEM VAL0)
    ($result (Scm_VMSlotRef VAL0 slot FALSE))))

(define-insn SLOT-SETC   0 obj #f       ; slot-set! with constant slot name
  (let* ((slot))
    (FETCH-OPERAND slot)
    INCR-PC
    ($w/argp obj
      (TAIL-CALL-INSTRUCTION)
      (SCM_FLONUM_ENSURE_MEM VAL0)
      ($result (Scm_VMSlotSet obj slot VAL0)))))

;;;
;;; Additional instructions
;;;

;; PUSH-HANDLERS
;; POP-HANDLERS
;;   Used for dynamic-wind and alike.

(define-insn PUSH-HANDLERS 0 none #f    ; push dynamic handlers
  (let* ((before) (after VAL0))
    (VM-ASSERT (>= (- SP (-> vm stackBase)) 1))
    (POP-ARG before)
    (SCM_FLONUM_ENSURE_MEM before)
    (SCM_FLONUM_ENSURE_MEM after)
    (set! (-> vm handlers) (Scm_Acons before after (-> vm handlers)))
    NEXT))

(define-insn POP-HANDLERS 0 none #f     ; pop dynamic handlers
  (begin
    (VM-ASSERT (SCM_PAIRP (-> vm handlers)))
    (set! (-> vm handlers) (SCM_CDR (-> vm handlers)))
    NEXT))

;;
;; Experimental
;;

(define-insn-lref* LREF-RET 0 none (LREF RET))

;; if param == 0, VAL0 <- box(VAL0)
;; else if param > 0,  ENV[param-1] <- box(ENV[param-1])
;; The second case is for arguments that are mutated.
(define-insn BOX 1 none #f
  (let* ([param::int (SCM_VM_INSN_ARG code)])
    (cond [(== param 0)
           (SCM_FLONUM_ENSURE_MEM VAL0)
           (let* ([b::ScmBox* (Scm_MakeBox VAL0)])
             (set! VAL0 (SCM_OBJ b)))]
          [(> param 0)
           (let* ([off::int (- param 1)])
             (VM-ASSERT (> (-> ENV size) off))
             (let* ([v (ENV-DATA ENV off)])
               (SCM_FLONUM_ENSURE_MEM v)
               (let* ([b::ScmBox* (Scm_MakeBox v)])
                 (set! (ENV-DATA ENV off) (SCM_OBJ b)))))])
    NEXT))

;; ENV-SET(offset)
;;  Mutate the top env's specified slot with VAL0
;;  This is used with LOCAL-ENV-CLOSURES to initialize non-procedure
;;  slots of the env.  We used to use LSET for this purpose, but now
;;  LSET counts on that all mutable lvars are boxed, so we need a separete
;;  insn that directly initialize the env frame.
(define-insn ENV-SET 1 none #f
  (let* ([off::int (SCM_VM_INSN_ARG code)])
    (VM-ASSERT (> (-> ENV size) off))
    (SCM_FLONUM_ENSURE_MEM VAL0)
    (set! (ENV-DATA ENV off) VAL0)
    NEXT))

;; UNBOX
;;  VAL0 <- unbox(VAL0)
(define-insn UNBOX 0 none #f
  ($w/argr v
    (set! VAL0 (SCM_BOX_VALUE v))
    NEXT))

(define-insn LREF-UNBOX 2 none (LREF UNBOX) #f :fold-lref)

;; LOCAL-ENV-SHIFT(DEPTH)
;;  This instruction appears when local function call is optimized.
;;  The stack already has NLOCALS values.  We discard DEPTH env frames,
;;  and creates a new local env from the stack value.
(define-insn LOCAL-ENV-SHIFT 1 none #f
  (begin
    (local_env_shift vm (SCM_VM_INSN_ARG code))
    NEXT))
