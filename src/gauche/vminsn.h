/*
 * vminsn.h - Virtual machine instruction definition
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, ditribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: vminsn.h,v 1.13 2001-02-15 10:29:56 shiro Exp $
 */

/* DEFINSN(symbol, name, # of parameters) */

/* NOP
 *   Input stack  : -
 *   Result stack : -
 *  Used for placeholder.
 */
DEFINSN(SCM_VM_NOP, "NOP", 0)

/* PUSH
 *
 *  Push value of val0 to the stack top
 */
DEFINSN(SCM_VM_PUSH, "PUSH", 0)

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

/* PRE-CALL <preparation>
 *
 *  Prepair for a normal call.
 */
DEFINSN(SCM_VM_PRE_CALL, "PRE-CALL", 1)

/* PRE-TAIL
 *
 *  Prepair for a tail call.
 */
DEFINSN(SCM_VM_PRE_TAIL, "PRE-TAIL", 0)

/* CALL(NARGS)
 *
 *  Call procedure in val0.
 */
DEFINSN(SCM_VM_CALL, "CALL", 1)

/* TAIL-CALL(NARGS)
 *
 *  Call procedure in val0.
 */
DEFINSN(SCM_VM_TAIL_CALL, "TAIL-CALL", 1)

/* DEFINE <SYMBOL>
 *   Input stack  : value
 *   Result stack : SYMBOL
 *
 *  Defines global binding of SYMBOL in the current module.
 *  The value is taken from the input stack.
 *  This instruction only appears at the toplevel.  Internal defines
 *  are recognized and eliminated by the compiling process.
 */
DEFINSN(SCM_VM_DEFINE, "DEFINE", 0)

/* LAMBDA(NARGS,RESTARG) <ARGLIST> <CODE>
 *   Input stack  : -
 *   Result stack : closure
 *
 *  Create a closure capturing current environment.  Two operands are
 *  taken: ARGLIST is a form of lambda list; it is just for debug.
 *  CODE is the compiled code.   Leaves created closure in the stack.
 */
DEFINSN(SCM_VM_LAMBDA, "LAMBDA", 2)

/* LET(NLOCALS)
 *   Input stack  : -
 *   Result stack : -
 *
 *  Create a new environment frame, size of NLOCALS.  let-families
 *  like let, let* and letrec yields this instruction.
 */
DEFINSN(SCM_VM_LET, "LET", 1)

/* POPENV
 *   Input stack  : -
 *   Result stack : -
 *
 *  Pop a local environment.  Executed on the end of let-family
 *  constructs.
 */
DEFINSN(SCM_VM_POPENV, "POPENV", 0)

/* IF  <THEN-CODE>
 *   Input stack  : test
 *   Result stack : -
 *
 *  If test is true, transfer control to THEN-CODE.  Otherwise
 *  it continues execution.   Test arg is popped.
 */
DEFINSN(SCM_VM_IF, "IF", 0)

/* TAILBIND(NARGS) <INFO>
 *   Input stack  : value0 ... valueN-1
 *   Result stack : -
 *
 *  Lightweight tail call.  This instruction appears in the loop body
 *  and the tail call to inlined procedures.
 *  Discards current environment, allocates NARGS size of environment,
 *  then sets values to the envionment.   It is equivalent to the following
 *  instruction sequence:
 *
 *    POPENV  LET(NARGS)  SET  LREF(N-1,0) ... SET LREF(0,0)
 */
DEFINSN(SCM_VM_TAILBIND, "TAILBIND", 1)

/* LSET(DEPTH, OFFSET)
 *   Input stack  : value
 *   Result stack : -
 *
 *  Local set
 */
DEFINSN(SCM_VM_LSET, "LSET", 2)

/* GSET <LOCATION>
 *   Input stack  : value
 *   Result stack : -
 *
 *  LOCATION may be a symbol or gloc
 */
DEFINSN(SCM_VM_GSET, "GSET", 0)

/* LREF(DEPTH,OFFSET)
 *   Input stack  : -
 *   Result stack : value
 *
 *  Retrieve local value.
 */
DEFINSN(SCM_VM_LREF, "LREF", 2)

/* GREF <LOCATION>
 *   Input stack  : -
 *   Result stack : value
 *
 *  LOCATION may be a symbol or GLOC object.
 *  Retrieve global value in the current module.
 */
DEFINSN(SCM_VM_GREF, "GREF", 0)

/* PROMISE
 *   Input stack  : procedure
 *   Result stack : promise
 *
 *  Delay syntax emits this instruction.  Wrap a procedure into promise
 *  object.
 */
DEFINSN(SCM_VM_PROMISE, "PROMISE", 0)

/* Inlined operators
 *  They work the same as corresponding Scheme primitives, but they are
 *  directly interpreted by VM, skipping argument processing part.
 *  Compiler may insert these in order to fulfill the operation (e.g.
 *  `case' needs MEMQ).  If the optimization level is high, global
 *  reference of those primitive calls in the user code are replaced
 *  as well.
 */
DEFINSN(SCM_VM_CONS, "CONS", 0)
DEFINSN(SCM_VM_CAR, "CAR", 0)
DEFINSN(SCM_VM_CDR, "CDR", 0)
DEFINSN(SCM_VM_LIST, "LIST", 1)
DEFINSN(SCM_VM_LIST_STAR, "LIST*", 1)
DEFINSN(SCM_VM_MEMV, "MEMV", 0)
DEFINSN(SCM_VM_EQ, "EQ?", 0)
DEFINSN(SCM_VM_EQV, "EQV?", 0)
DEFINSN(SCM_VM_APPEND, "APPEND", 1)
DEFINSN(SCM_VM_NOT, "NOT", 0)
DEFINSN(SCM_VM_NULLP, "NULL?", 0)
DEFINSN(SCM_VM_REVERSE, "REVERSE", 0)
/*DEFINSN(SCM_VM_NOT_NULLP, "NOT-NULL?", 0)*/
/*DEFINSN(SCM_VM_FOR_EACH, "FOR-EACH", 1)*/
/*DEFINSN(SCM_VM_MAP, "MAP", 1)*/

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
