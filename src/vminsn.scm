;;;
;;; vminsn.scm - Virtual machine instruction definition
;;;
;;;   Copyright (c) 2000-2004 Shiro Kawai, All rights reserved.
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
;;;  $Id: vminsn.scm,v 1.1.2.1 2004-12-24 12:50:36 shirok Exp $
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
;;;                    obj  : the insn takes an ScmObj operand.
;;;                    addr : the insn takes an address the next pc points.
;;;                    code : the insn takes an ScmCompiledCode operand.


;; NOP
;;  Used for placeholder.   Won't appear in the final compiled code.
;;
(define-insn NOP   0 none)

;; MNOP
;;  NOP, but appears at the point where instruction graph merges.
;;  This is used in the old compiler.  Will be removed in future.
;;
(define-insn MNOP  0 none)

;; CONST <obj>
;;  Set <obj> to val0.
;;
(define-insn CONST 0 obj)

;; PUSH
;;  Push value of val0 to the stack top
;;  *sp++ = val0
;;
(define-insn PUSH  0 none)

;; PUSHI(i)
;; PUSHNIL
;;  Compiled operators
(define-insn PUSHI    1 none)           ; push immediate integer
(define-insn PUSHNIL  0 none)           ; push '()
    
;; POP
;;  Pop arg.
;;  val0 = *--sp
(define-insn POP      0 none)

;; DUP
;;  Duplicate the value on top of the stack
;;  tmp = *(sp - 1)
;;  *sp++ = tmp
;;
(define-insn DUP      0 none)

;; PRE-CALL(nargs) <cont>
;;  Prepare for a normal call.   Push a continuation that resumes
;;  execution from <cont>.
;;
(define-insn PRE-CALL 1 addr)

;; PRE-TAIL(nargs) 
;;  Prepare for a tail call.   At this moment, this instruction only checks
;;  stack boundary.  Eventually this will be removed.
;;
(define-insn PRE-TAIL 1 none)

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

;; JUMP <CODE>
;;  Jump to <CODE>.  
;;
(define-insn JUMP      0 addr)

;; RET
;;  Pop the continuation stack.
;;
(define-insn RET       0 none)

;; DEFINE <symbol>
;;  Defines global binding of SYMBOL in the current module.
;;  The value is taken from the input stack.
;;  This instruction only appears at the toplevel.  Internal defines
;;  are recognized and eliminated by the compiling process.
;;
(define-insn DEFINE     0 obj)
(define-insn DEFINE-CONST 0 obj)

;; LAMBDA(NARGS,RESTARG) <code>
;;  Create a closure capturing current environment.
;;  CODE is the compiled code.   Leaves created closure in val0.
;;
(define-insn LAMBDA     2 code)

;; LET(nlocals) <cont>
;;  Create a new environment frame, size of NLOCALS.  let-families
;;  like let, let* and letrec yields this instruction.
;;
;;  This instruction needs to push the continuation frame to
;;  protect the values currently pushed on the stack after the last
;;  continuation frame.  <CONT> is the instruction offset
;;  after the let form resumes.
;;
(define-insn LET        1 addr)

;; TAIL-LET(NLOCALS)
;;  Let-family at the tail position.  No need to push the
;;  continuation.
;;  Actually, TAIL-LET can be used anywhere if there's no
;;  argument pushed after the last continuation frame.
;;  So the name is misleading---maybe LET-BOTTOM or something.
;;
(define-insn TAIL-LET   1 none)

;; IF <else-offset>
;;  If val0 is true, transfer control to next insn.  Otherwise,
;;  jump to <ELSE-OFFSET>.
;;  NB: this should be named like JF (jump if false) or something.
;;
(define-insn IF         0 addr)

;; RECEIVE(nargs,restarg) <cont>
;;  Primitive operation for receive and call-with-values.
;;  Turn the value(s) into an environment.
;;  Like LET, this pushes the continuation frame to resume the
;;  operation from CONT-OFFSET.
;;
(define-insn RECEIVE    2 addr)

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

;; shortcut for the first and second frame, small offset */
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

;; combined instrction */
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

;; PROMISE
;;  Delay syntax emits this instruction.  Wrap a procedure into a promise
;;  object.
;;
(define-insn PROMISE     0 none)

;; QUOTE-INSN <INSN>
;;  [Obsoleted]: will be replaced by CONST.
;;  Quote the next VM instruction.  Needs to load VM insn to val0.
;;  It occurs when VM insn is passed to apply.
;;
(define-insn QUOTE-INSN  0 obj)

;; HALT
;;  Code for debugging, to ensure the part of the code should
;;  never be executed.
;;
(define-insn HALT        0 none)

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
(define-insn LIST        1 none)
(define-insn LIST-STAR   1 none)        ; list*
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
;;(define-insn NOT-NULLP 0 none)

(define-insn NULLP       0 none)        ; null?
(define-insn PAIRP       0 none)        ; pair?
(define-insn CHARP       0 none)        ; char?
(define-insn EOFP        0 none)        ; eof?
(define-insn STRINGP     0 none)        ; string?
(define-insn SYMBOLP     0 none)        ; symbol?

(define-insn SETTER      0 none)

(define-insn VALUES      1 none)

(define-insn VEC         1 none)        ; vector
(define-insn APP-VEC     1 none)        ; (compose list->vector append)
(define-insn VEC-LEN     0 none)        ; vector-length
(define-insn VEC-REF     0 none)        ; vector-ref
(define-insn VEC-SET     0 none)        ; vector-set

(define-insn NUMEQ2      0 none)        ; =
(define-insn NUMLT2      0 none)        ; <
(define-insn NUMLE2      0 none)        ; <=
(define-insn NUMGT2      0 none)        ; >
(define-insn NUMGE2      0 none)        ; >=
(define-insn NUMADD2     0 none)        ; +
(define-insn NUMSUB2     0 none)        ; -

(define-insn NUMADDI     1 none)        ; +, if one of op is small int
(define-insn NUMSUBI     1 none)        ; -, if one of op is small int

(define-insn READ-CHAR   1 none)        ; read-char
(define-insn WRITE-CHAR  1 none)        ; write-char

(define-insn SLOT-REF    0 none)        ; slot-ref
(define-insn SLOT-SET    0 none)        ; slot-set!
