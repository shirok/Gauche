/*
 * inline.c - pre-defined inliners
 *
 *  Copyright(C) 2000 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: inline.c,v 1.1 2001-01-15 04:44:54 shiro Exp $
 */

#include "gauche.h"

/*
 * Inliner procedure API
 *
 *  If SUBR is inlinable, it's inliner field has a ptr to the C-procedure.
 *  The inline is called at the compile time, with four arguments:
 *  The SUBR structure itself, the compiling form, the compiling environment
 *  and the compiling context.
 *
 *  It must return a compiled code, which is a list of VM instructions,
 *  or #f.  If it returns #f, the compiler emits the code of normal calling
 *  sequence.  It allows the inliner to optimize certain common case.
 */

/* TODO: fix API to access compiler enviornment and context */

/*----------------------------------------------------------------
 * Pair and lists
 */

ScmObj Scm_inline_cons(ScmSubr *subr, ScmObj form, ScmObj env, int ctx)
{
    int nargs = Scm_Length(SCM_CDR(form));
    ScmObj code = SCM_NIL, codetail;
    if (nargs != 2) Scm_Error("cons requires exactly 2 args: %S", form);
    SCM_GROW_LIST_SPLICING(code, codetail,
                           Scm_Compile(SCM_CADR(form), env, 1));
    SCM_GROW_LIST_SPLICING(code, codetail,
                           Scm_Compile(SCM_CAR(SCM_CDDR(form)), env, 1));
    SCM_GROW_LIST(code, codetail, SCM_VM_MAKE_INSN(SCM_VM_CONS));
    return code;
}


ScmObj Scm_inline_memv(ScmSubr *subr, ScmObj form, ScmObj env, int ctx)
{
    int nargs = Scm_Length(SCM_CDR(form));
    ScmObj code = SCM_NIL, codetail;
    if (nargs != 2) Scm_Error("memv requires exactly 2 args: %S", form);
    SCM_GROW_LIST_SPLICING(code, codetail,
                           Scm_Compile(SCM_CADR(form), env, 1));
    SCM_GROW_LIST_SPLICING(code, codetail,
                           Scm_Compile(SCM_CAR(SCM_CDDR(form)), env, 1));
    SCM_GROW_LIST(code, codetail, SCM_VM_MAKE_INSN(SCM_VM_MEMV));
    return code;
}
    
/*----------------------------------------------------------------
 * Vectors
 */




/*----------------------------------------------------------------
 * Numbers
 */
