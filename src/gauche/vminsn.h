/*
 * vminsn.h - Virtual machine instruction definition
 *
 *   Copyright (c) 2000-2004 Shiro Kawai, All rights reserved.
 * 
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 * 
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   3. Neither the name of the authors nor the names of its contributors
 *      may be used to endorse or promote products derived from this
 *      software without specific prior written permission.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 *  $Id: vminsn.h,v 1.40.2.3 2004-12-23 10:01:12 shirok Exp $
 */

/* DEFINSN(symbol, name, # of parameters) */

/* NOP
 *  Used for placeholder.
 */
DEFINSN(SCM_VM_NOP, "NOP", 0)

/* MNOP
 *  NOP, but appears at the point where instruction graph merges.
 */
DEFINSN(SCM_VM_MNOP, "MNOP", 0)

/* CONST <obj>
 *  val0 <- <obj>
 */
DEFINSN(SCM_VM_CONST, "CONST", 0)

/* PUSH
 *
 *  Push value of val0 to the stack top
 */
DEFINSN(SCM_VM_PUSH, "PUSH", 0)

/* combined push */
DEFINSN(SCM_VM_PUSHI, "PUSHI", 1) /* push immediate integer */
DEFINSN(SCM_VM_PUSHNIL, "PUSHNIL", 0) /* push '() */
    
/* POP
 *
 *  Pop arg
 */
DEFINSN(SCM_VM_POP, "POP", 0)

/* DUP
 *
 *  Duplicate the value on top of the stack
 */
DEFINSN(SCM_VM_DUP, "DUP", 0)

/* PRE-CALL(nargs) <prep>
 *
 *  Prepare for a normal call.   <prep> is a list of code that ends with CALL.
 *  The next insn is the continuation.  This instruction pushes the
 *  continuation, then go executing <prep>.
 *
 *  [NVM] the next word of PRE-CALL is a continuation address
 *  (offset from base).
 */
DEFINSN(SCM_VM_PRE_CALL, "PRE-CALL", 1)

/* PRE-TAIL(nargs) 
 *
 *  Prepare for a tail call.   At this moment, this instruction only checks
 *  stack boundary.  Eventually this will be removed.
 */
DEFINSN(SCM_VM_PRE_TAIL, "PRE-TAIL", 1)

/* CHECK-STACK(size)
 *
 *  Check for stack overflow
 */
DEFINSN(SCM_VM_CHECK_STACK, "CHECK-STACK", 1)

/* CALL(NARGS)
 *
 *  Call procedure in val0.  The continuation of this call is already
 *  pushed by PRE_CALL, so this instruction is always the end of a graph.
 */
DEFINSN(SCM_VM_CALL, "CALL", 1)

/* TAIL-CALL(NARGS)
 *
 *  Call procedure in val0.  Same as CALL except this discards the 
 *  caller's arugment frame and shift the callee's argument frame.
 */
DEFINSN(SCM_VM_TAIL_CALL, "TAIL-CALL", 1)

/* JUMP <CODE>
 *
 *  [NVM] Jump to <CODE>.  
 */
DEFINSN(SCM_VM_JUMP, "JUMP", 0)

/* RET
 *
 *  [NVM] Pop the continuation stack.
 */
DEFINSN(SCM_VM_RET, "RET", 0)

/* DEFINE <SYMBOL>
 *
 *  Defines global binding of SYMBOL in the current module.
 *  The value is taken from the input stack.
 *  This instruction only appears at the toplevel.  Internal defines
 *  are recognized and eliminated by the compiling process.
 */
DEFINSN(SCM_VM_DEFINE, "DEFINE", 0)
DEFINSN(SCM_VM_DEFINE_CONST, "DEFINE-CONST", 0)

/* LAMBDA(NARGS,RESTARG) <COMPILED-CODE>
 *
 *  Create a closure capturing current environment.
 *  CODE is the compiled code.   Leaves created closure in the stack.
 */
DEFINSN(SCM_VM_LAMBDA, "LAMBDA", 2)

/* LET(NLOCALS) <CONT-OFFSET>
 *
 *  Create a new environment frame, size of NLOCALS.  let-families
 *  like let, let* and letrec yields this instruction.
 *  This instruction needs to push the continuation frame to
 *  protect the values currently pushed on the stack after the last
 *  continuation frame.  <CONT-OFFSET> is the instruction offset
 *  after the let form resumes.
 */
DEFINSN(SCM_VM_LET, "LET", 1)

/* TAIL-LET(NLOCALS)
 *
 *  Let-family at the tail position.  No need to push the
 *  continuation.
 *  Actually, TAIL-LET can be used anywhere if there's no
 *  argument pushed after the last continuation frame.
 *  So the name is misleading---maybe LET-BOTTOM or something.
 */
DEFINSN(SCM_VM_TAIL_LET, "TAIL-LET", 1)

/* IF <ELSE-OFFSET>
 *
 *  If val0 is true, transfer control to next insn.  Otherwise,
 *  jump to <ELSE-OFFSET>.
 */
DEFINSN(SCM_VM_IF, "IF", 0)

/* RECEIVE(NARGS,RESTARG) <CONT-OFFSET>
 *
 *  Primitive operation for receive and call-with-values.
 *  Like LET, this pushes the continuation frame to resume the
 *  operation from CONT-OFFSET.
 */
DEFINSN(SCM_VM_RECEIVE, "RECEIVE", 2)

/* TAIL-RECEIVE(NARGS,RESTARG)
 *
 *  Tail position of receive.  No need to push the continuation.
 *  Actually, TAIL-RECEIVE can be used anywhere if there's no
 *  argument pushed after the last continuation frame.  See TAIL-LET.
 */
DEFINSN(SCM_VM_TAIL_RECEIVE, "TAIL-RECEIVE", 2)

/* LSET(DEPTH, OFFSET)
 *
 *  Local set
 */
DEFINSN(SCM_VM_LSET, "LSET", 2)

/* shortcut for the first frame, small offset */
DEFINSN(SCM_VM_LSET0, "LSET0", 0)
DEFINSN(SCM_VM_LSET1, "LSET1", 0)
DEFINSN(SCM_VM_LSET2, "LSET2", 0)
DEFINSN(SCM_VM_LSET3, "LSET3", 0)
DEFINSN(SCM_VM_LSET4, "LSET4", 0)

/* GSET <LOCATION>
 *
 *  LOCATION may be a symbol or gloc
 */
DEFINSN(SCM_VM_GSET, "GSET", 0)

/* LREF(DEPTH,OFFSET)
 *
 *  Retrieve local value.
 */
DEFINSN(SCM_VM_LREF, "LREF", 2)

/* shortcut for the first and second frame, small offset */
DEFINSN(SCM_VM_LREF0, "LREF0", 0)
DEFINSN(SCM_VM_LREF1, "LREF1", 0)
DEFINSN(SCM_VM_LREF2, "LREF2", 0)
DEFINSN(SCM_VM_LREF3, "LREF3", 0)
DEFINSN(SCM_VM_LREF4, "LREF4", 0)

DEFINSN(SCM_VM_LREF10, "LREF10", 0)
DEFINSN(SCM_VM_LREF11, "LREF11", 0)
DEFINSN(SCM_VM_LREF12, "LREF12", 0)
DEFINSN(SCM_VM_LREF13, "LREF13", 0)
DEFINSN(SCM_VM_LREF14, "LREF14", 0)

/* combined instrction */
DEFINSN(SCM_VM_LREF_PUSH, "LREF-PUSH", 2)
DEFINSN(SCM_VM_LREF0_PUSH, "LREF0-PUSH", 0)
DEFINSN(SCM_VM_LREF1_PUSH, "LREF1-PUSH", 0)
DEFINSN(SCM_VM_LREF2_PUSH, "LREF2-PUSH", 0)
DEFINSN(SCM_VM_LREF3_PUSH, "LREF3-PUSH", 0)
DEFINSN(SCM_VM_LREF4_PUSH, "LREF4-PUSH", 0)
DEFINSN(SCM_VM_LREF10_PUSH, "LREF10-PUSH", 0)
DEFINSN(SCM_VM_LREF11_PUSH, "LREF11-PUSH", 0)
DEFINSN(SCM_VM_LREF12_PUSH, "LREF12-PUSH", 0)
DEFINSN(SCM_VM_LREF13_PUSH, "LREF13-PUSH", 0)
DEFINSN(SCM_VM_LREF14_PUSH, "LREF14-PUSH", 0)


/* GREF <LOCATION>
 *
 *  LOCATION may be a symbol or GLOC object.
 *  Retrieve global value in the current module.
 */
DEFINSN(SCM_VM_GREF, "GREF", 0)
/*DEFINSN(SCM_VM_GREF_PUSH, "GREF-PUSH", 0)*/

/* PROMISE
 *
 *  Delay syntax emits this instruction.  Wrap a procedure into a promise
 *  object.
 */
DEFINSN(SCM_VM_PROMISE, "PROMISE", 0)

/* QUOTE-INSN <INSN>
 *
 *  Quote the next VM instruction.  Needs to load VM insn to val0.
 *  It occurs when VM insn is passed to apply.
 */
DEFINSN(SCM_VM_QUOTE_INSN, "QUOTE-INSN", 0)

/* HALT
 *
 *  [NVM] code for debugging, to ensure the part of the code should
 *  never be executed.
 */
DEFINSN(SCM_VM_HALT, "HALT", 0)

/* Inlined operators
 *  They work the same as corresponding Scheme primitives, but they are
 *  directly interpreted by VM, skipping argument processing part.
 *  Compiler may insert these in order to fulfill the operation (e.g.
 *  `case' needs MEMV).  If the optimization level is high, global
 *  reference of those primitive calls in the user code are replaced
 *  as well.
 */
DEFINSN(SCM_VM_CONS, "CONS", 0)
DEFINSN(SCM_VM_CONS_PUSH, "CONS-PUSH", 0)
DEFINSN(SCM_VM_CAR, "CAR", 0)
DEFINSN(SCM_VM_CAR_PUSH, "CAR-PUSH", 0)
DEFINSN(SCM_VM_CDR, "CDR", 0)
DEFINSN(SCM_VM_CDR_PUSH, "CDR-PUSH", 0)
DEFINSN(SCM_VM_LIST, "LIST", 1)
/*DEFINSN(SCM_VM_LIST_PUSH, "LIST-PUSH", 0)*/
DEFINSN(SCM_VM_LIST_STAR, "LIST*", 1)
DEFINSN(SCM_VM_MEMQ, "MEMQ", 0)
DEFINSN(SCM_VM_MEMV, "MEMV", 0)
DEFINSN(SCM_VM_ASSQ, "ASSQ", 0)
DEFINSN(SCM_VM_ASSV, "ASSV", 0)
DEFINSN(SCM_VM_EQ, "EQ?", 0)
DEFINSN(SCM_VM_EQV, "EQV?", 0)
DEFINSN(SCM_VM_APPEND, "APPEND", 1)
DEFINSN(SCM_VM_NOT, "NOT", 0)
DEFINSN(SCM_VM_REVERSE, "REVERSE", 0)
DEFINSN(SCM_VM_APPLY, "APPLY", 1)
DEFINSN(SCM_VM_TAIL_APPLY, "TAIL-APPLY", 1)
/*DEFINSN(SCM_VM_NOT_NULLP, "NOT-NULL?", 0)*/
/*DEFINSN(SCM_VM_FOR_EACH, "FOR-EACH", 1)*/
/*DEFINSN(SCM_VM_MAP, "MAP", 1)*/

DEFINSN(SCM_VM_NULLP, "NULL?", 0)
DEFINSN(SCM_VM_PAIRP, "PAIR?", 0)
DEFINSN(SCM_VM_CHARP, "CHAR?", 0)
DEFINSN(SCM_VM_EOFP,  "EOF?", 0)
DEFINSN(SCM_VM_STRINGP, "STRING?", 0)
DEFINSN(SCM_VM_SYMBOLP, "SYMBOL?", 0)

DEFINSN(SCM_VM_SETTER, "SETTER", 0)

DEFINSN(SCM_VM_VALUES, "VALUES", 1)

DEFINSN(SCM_VM_VEC, "VEC", 1)
DEFINSN(SCM_VM_APP_VEC, "APP-VEC", 1)
DEFINSN(SCM_VM_VEC_LEN, "VEC-LEN", 0)
DEFINSN(SCM_VM_VEC_REF, "VEC-REF", 0)
DEFINSN(SCM_VM_VEC_SET, "VEC-SET", 0)

DEFINSN(SCM_VM_NUMEQ2, "NUMEQ2", 0)
DEFINSN(SCM_VM_NUMLT2, "NUMLT2", 0)
DEFINSN(SCM_VM_NUMLE2, "NUMLE2", 0)
DEFINSN(SCM_VM_NUMGT2, "NUMGT2", 0)
DEFINSN(SCM_VM_NUMGE2, "NUMGE2", 0)
DEFINSN(SCM_VM_NUMADD2, "NUMADD2", 0)
DEFINSN(SCM_VM_NUMSUB2, "NUMSUB2", 0)

DEFINSN(SCM_VM_NUMADDI, "NUMADDI", 1)
DEFINSN(SCM_VM_NUMSUBI, "NUMSUBI", 1)

DEFINSN(SCM_VM_READ_CHAR, "READ-CHAR", 1)
DEFINSN(SCM_VM_WRITE_CHAR, "WRITE-CHAR", 1)

DEFINSN(SCM_VM_SLOT_REF, "SLOT-REF", 0)
DEFINSN(SCM_VM_SLOT_SET, "SLOT-SET", 0)
