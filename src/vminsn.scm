;;;
;;; vminsn.scm - Virtual machine instruction definition
;;;
;;;   Copyright (c) 2000-2005 Shiro Kawai, All rights reserved.
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
;;;  $Id: vminsn.scm,v 1.3 2005-04-22 23:12:11 shirok Exp $
;;;

;;; This file is processed by geninsn to produce a couple of C files:
;;; gauche/vminsn.h and vminsn.c, which are then included in vm.c.
;;; This file is also used by the compiler.
;;;
;;; 
;;; (define-insn <name> <num-params> <operand-type>)
;;;
;;;   <name> - instruction name.  In C, an enum SCM_VM_<name> is defined.
;;;
;;;   <num-params> - # of parameters the instruction takes.
;;;
;;;   <operand-type> - none : the insn doesn't take an operand.
;;;                    obj  : an ScmObj operand.
;;;                    addr : an address the next pc points.
;;;                    code : an ScmCompiledCode operand.
;;;                    codes: a list of ScmCompiledCodes.


;; NOP
;;  Used for placeholder.   Won't appear in the final compiled code.
;;
(define-insn NOP   0 none)

;; CONST <obj>
;;  Set <obj> to val0.
;;
(define-insn CONST 0 obj)

;; CONSTI(i)
;; CONSTN
;; CONSTF
;; CONSTU
;;  
(define-insn CONSTI 1 none)             ; constant small integer
(define-insn CONSTN 0 none)             ; constant ()
(define-insn CONSTF 0 none)             ; constant #f
(define-insn CONSTU 0 none)             ; constant #<undef>

(define-insn CONST-PUSH  0 obj)         ; combined insn
(define-insn CONSTI-PUSH 1 none)        ; push small integer
(define-insn CONSTN-PUSH 0 none)        ; push ()
(define-insn CONSTF-PUSH 0 none)        ; push #f

(define-insn CONST-RET   0 obj)
(define-insn CONSTF-RET  0 none)
(define-insn CONSTU-RET  0 none)

;; push
;;  Push value of val0 to the stack top
;;  *sp++ = val0
;;
(define-insn PUSH  0 none)

;; PRE-CALL(nargs) <cont>
;;  Prepare for a normal call.   Push a continuation that resumes
;;  execution from <cont>.
;;
(define-insn PRE-CALL 1 addr)

;; combined insn
(define-insn PUSH-PRE-CALL 1 addr)

;; CHECK-STACK(size)
;;  Check for stack overflow
;;
(define-insn CHECK-STACK 1 none)

;; CALL(NARGS)
;;  Call procedure in val0.  The continuation of this call is already
;;  pushed by PRE-CALL, so this instruction is always the end of a graph.
;;
(define-insn CALL     1 none)

;; TAIL-CALL(NARGS)
;;  Call procedure in val0.  Same as CALL except this discards the 
;;  caller's arugment frame and shift the callee's argument frame.
;;
(define-insn TAIL-CALL 1 none)

;; JUMP <addr>
;;  Jump to <addr>.  
;;
(define-insn JUMP      0 addr)

;; RET
;;  Pop the continuation stack.
;;
(define-insn RET       0 none)

;; DEFINE(flag) <symbol>
;;  Defines global binding of SYMBOL in the current module.
;;  The value is taken from the input stack.
;;  This instruction only appears at the toplevel.  Internal defines
;;  are recognized and eliminated by the compiling process.
;;  If flag == 1, the definition becomes 'constant'.
;;
(define-insn DEFINE     1 obj)

;; CLOSURE <code>
;;  Create a closure capturing current environment.
;;  CODE is the compiled code.   Leaves created closure in val0.
;;
(define-insn CLOSURE    0 code)

;; LOCAL-ENV(NLOCALS)
;;  Create a new environment frame from the current arg frame.
;;  Used for let.
(define-insn LOCAL-ENV  1 none)

;; combined insn
(define-insn PUSH-LOCAL-ENV 1 none)

;; LOCAL-ENV-CLOSURES(nlocals) <codelist>
;;  Used for letrec.
;;  Similar to LOCAL-ENV, but this doesn't use the current arg frame.
;;  The operand contains a literal list of compiled codes.  This instruction
;;  creates a frame of NLOCALS size, then creates closures
;;  with the new environment and the given compiled codes, and fills the
;;  new frame with the created closures.   CODELIST can have 'holes', i.e.
;;  if it has #f instead of a compiled code, the corresponding frame entry
;;  is left undefined.
;;  This instruction also leaves the last closure in VAL0.
(define-insn LOCAL-ENV-CLOSURES 1 codes)

;; POP-LOCAL-ENV
;;  Pop one environment frame created by LOCAL-ENV.  In practice,
;;  this is only used if 'let' is in the non-tail bottom position
;;  (for other cases, the env frame is discarded along other frame
;;  operations).
(define-insn POP-LOCAL-ENV 0 none)

;; LOCAL-ENV-JUMP(DEPTH) <addr>
;;  This instruction appears when local function call is optimized.
;;  The stack already has NLOCALS values.  This instruction creates an
;;  env frame with them (just like LOCAL-ENV), then jump to <addr>.
;;  (# of arguments can be known by SP - ARGP).
(define-insn LOCAL-ENV-JUMP 1 addr)

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
(define-insn LOCAL-ENV-CALL  1 none)
(define-insn LOCAL-ENV-TAIL-CALL 1 none)

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
(define-insn BF          0 addr)
(define-insn BT          0 addr)
(define-insn BNEQ        0 addr)
(define-insn BNEQV       0 addr)
(define-insn BNNULL      0 addr)

(define-insn BNUMNE      0 addr)
(define-insn BNLT        0 addr)
(define-insn BNLE        0 addr)
(define-insn BNGT        0 addr)
(define-insn BNGE        0 addr)

;; BNUMNEI(i) <else-offset> ; combined CONSTI(i) + BNUMNE
;; BNEQC <else-offset>     ; branch if immediate constant is not eq? to VAL0
;; BNEQVC <else-offset>    ; branch if immediate constant is not eqv? to VAL0
;;   NB: we tried other variations of constant op + branch combination,
;;       notably BNEQVI, BNUMNEF, BNLTF etc, but they did't show any
;;       improvement.
(define-insn BNUMNEI    1 addr)
(define-insn BNEQC       0 obj+addr)
(define-insn BNEQVC      0 obj+addr)

;; RF
;; RT
;; RNNULL
;; RNEQ
;; RNEQV
;;   Conditional returns.
(define-insn RF          0 none)
(define-insn RT          0 none)
(define-insn RNEQ        0 none)
(define-insn RNEQV       0 none)
(define-insn RNNULL      0 none)

;; RECEIVE(nargs,restarg) <cont>
;;  Primitive operation for receive and call-with-values.
;;  Turn the value(s) into an environment.
;;  Like LET, this pushes the continuation frame to resume the
;;  operation from CONT-OFFSET.
;;
(define-insn RECEIVE     2 addr)

;; TAIL-RECEIVE(nargs,restarg)
;;  Tail position of receive.  No need to push the continuation.
;;  Actually, TAIL-RECEIVE can be used anywhere if there's no
;;  argument pushed after the last continuation frame.  See TAIL-LET.
;;
(define-insn TAIL-RECEIVE 2 none)

;; LSET(depth, offset)
;;  Local set
;;
(define-insn LSET        2 none)

;; shortcut for the first frame, small offset */
(define-insn LSET0       0 none)
(define-insn LSET1       0 none)
(define-insn LSET2       0 none)
(define-insn LSET3       0 none)
(define-insn LSET4       0 none)

;; GSET <location>
;;  LOCATION may be a symbol or gloc
;;
(define-insn GSET        0 obj)

;; LREF(depth,offset)
;;  Retrieve local value.
;;
(define-insn LREF        2 none)

;; shortcut for the first and second frame, small offset
(define-insn LREF0       0 none)
(define-insn LREF1       0 none)
(define-insn LREF2       0 none)
(define-insn LREF3       0 none)
(define-insn LREF4       0 none)

(define-insn LREF10      0 none)
(define-insn LREF11      0 none)
(define-insn LREF12      0 none)
(define-insn LREF13      0 none)
(define-insn LREF14      0 none)

;; combined instrction
(define-insn LREF-PUSH   2 none)
(define-insn LREF0-PUSH  0 none)
(define-insn LREF1-PUSH  0 none)
(define-insn LREF2-PUSH  0 none)
(define-insn LREF3-PUSH  0 none)
(define-insn LREF4-PUSH  0 none)
(define-insn LREF10-PUSH 0 none)
(define-insn LREF11-PUSH 0 none)
(define-insn LREF12-PUSH 0 none)
(define-insn LREF13-PUSH 0 none)
(define-insn LREF14-PUSH 0 none)

;; GREF <location>
;;  LOCATION may be a symbol or GLOC object.
;;  Retrieve global value in the current module.
;;
(define-insn GREF        0 obj)

;; combined instructions
;;  NB: PUSH-GREF itself isn't a very useful form, but it works as a
;;  transient state to PUSH-GREF-CALL / PUSH-GREF-TAIL-CALL, which are
;;  very frequent operation, during instruction combining.

(define-insn GREF-PUSH   0 obj)
(define-insn GREF-CALL   1 obj)
(define-insn GREF-TAIL-CALL 1 obj)

(define-insn PUSH-GREF   0 obj)
(define-insn PUSH-GREF-CALL 1 obj)
(define-insn PUSH-GREF-TAIL-CALL 1 obj)

;; PROMISE
;;  Delay syntax emits this instruction.  Wrap a procedure into a promise
;;  object.
;;
(define-insn PROMISE     0 none)

;; CONST_APPLY(nargs) <form>
;;  This instruction should only appear in the code generated dynamically
;;  by Scm_Apply.  This is used to pass the application information across
;;  the boundary frame (see user_eval_inner() in vm.c).
;;  <form> is a constant list (<proc> <arg> ...).  <proc> is called with
;;  <arg>s.
(define-insn CONST-APPLY 1 obj)

;; Inlined operators
;;  They work the same as corresponding Scheme primitives, but they are
;;  directly interpreted by VM, skipping argument processing part.
;;  Compiler may insert these in order to fulfill the operation (e.g.
;;  `case' needs MEMV).  If the optimization level is high, global
;;  reference of those primitive calls in the user code are replaced
;;  as well.
;;
(define-insn CONS        0 none)
(define-insn CONS-PUSH   0 none)
(define-insn CAR         0 none)
(define-insn CAR-PUSH    0 none)
(define-insn CDR         0 none)
(define-insn CDR-PUSH    0 none)
(define-insn CAAR        0 none)
(define-insn CAAR-PUSH   0 none)
(define-insn CADR        0 none)
(define-insn CADR-PUSH   0 none)
(define-insn CDAR        0 none)
(define-insn CDAR-PUSH   0 none)
(define-insn CDDR        0 none)
(define-insn CDDR-PUSH   0 none)

(define-insn LIST        1 none)
(define-insn LIST-STAR   1 none)        ; list*
(define-insn LENGTH      0 none)        ; length
(define-insn MEMQ        0 none)
(define-insn MEMV        0 none)
(define-insn ASSQ        0 none)
(define-insn ASSV        0 none)
(define-insn EQ          0 none)        ; eq?
(define-insn EQV         0 none)        ; eqv?
(define-insn APPEND      1 none)
(define-insn NOT         0 none)
(define-insn REVERSE     0 none)
(define-insn APPLY       1 none)
(define-insn TAIL-APPLY  1 none)        ; apply at the tail position
(define-insn IS-A        0 none)        ; is-a?

(define-insn NULLP       0 none)        ; null?
(define-insn PAIRP       0 none)        ; pair?
(define-insn CHARP       0 none)        ; char?
(define-insn EOFP        0 none)        ; eof?
(define-insn STRINGP     0 none)        ; string?
(define-insn SYMBOLP     0 none)        ; symbol?
(define-insn VECTORP     0 none)        ; vector?
(define-insn IDENTIFIERP 0 none)

(define-insn SETTER      0 none)

(define-insn VALUES      1 none)

(define-insn VEC         1 none)        ; vector
(define-insn LIST2VEC    0 none)        ; list->vector
(define-insn APP-VEC     1 none)        ; (compose list->vector append)
(define-insn VEC-LEN     0 none)        ; vector-length
(define-insn VEC-REF     0 none)        ; vector-ref
(define-insn VEC-SET     0 none)        ; vector-set
;; VEC-REF and VEC-SET with immediate index.  VAL0 must be a vector.
(define-insn VEC-REFI    1 none)
(define-insn VEC-SETI    1 none)

(define-insn NUMEQ2      0 none)        ; =
(define-insn NUMLT2      0 none)        ; <
(define-insn NUMLE2      0 none)        ; <=
(define-insn NUMGT2      0 none)        ; >
(define-insn NUMGE2      0 none)        ; >=
(define-insn NUMADD2     0 none)        ; +
(define-insn NUMSUB2     0 none)        ; -  (binary)
(define-insn NUMMUL2     0 none)        ; *
(define-insn NUMDIV2     0 none)        ; /
(define-insn NEGATE      0 none)        ; -  (unary)

(define-insn NUMADDI     1 none)        ; +, if one of op is small int
(define-insn NUMSUBI     1 none)        ; -, if one of op is small int

;(define-insn NUMQUOT     0 none)        ; quotient
;(define-insn NUMMOD      0 none)        ; modulo
;(define-insn NUMREM      0 none)        ; remainder
;(define-insn ASH         0 none)        ; ash
;(define-insn LOGAND      0 none)        ; logand
;(define-insn LOGIOR      0 none)        ; logior
;(define-insn LOGXOR      0 none)        ; logxor
;(define-insn LOGNOT      0 none)        ; lognot
;(define-insn LOGANDI     1 none)        ; logand w/ immediate small int
;(define-insn LOGIORI     1 none)        ; logior w/ immediate small int
;(define-insn LOGXORI     1 none)        ; logxor w/ immediate small int

(define-insn READ-CHAR   1 none)        ; read-char
(define-insn PEEK-CHAR   1 none)        ; peek-char
(define-insn WRITE-CHAR  1 none)        ; write-char

(define-insn CURIN       0 none)        ; current-input-port
(define-insn CUROUT      0 none)        ; current-output-port
(define-insn CURERR      0 none)        ; current-error-port

(define-insn SLOT-REF    0 none)        ; slot-ref
(define-insn SLOT-SET    0 none)        ; slot-set!
(define-insn SLOT-REFC   0 obj)         ; slot-ref with constant slot name
(define-insn SLOT-SETC   0 obj)         ; slot-set! with constant slot name


