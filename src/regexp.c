/*
 * regexp.c - regular expression
 *
 *  Copyright(C) 2000-2001 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, distribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: regexp.c,v 1.2 2001-04-11 06:20:13 shiro Exp $
 */

#include "gauche.h"

/* class stuff */
SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_RegexpClass, NULL);
SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_RegMatchClass, NULL);

/*
 * We use bytecode to interpret the regexp, as opposed to a DG
 * we're using in the main VM.  It's because the data we deal
 * with here is a stream of bytes, instead of a general Lisp structure,
 * and having raw byte data to match benefits performance.
 *
 * We match the string without decoding each character as much as
 * possible.
 *
 * Each bytecode is always consisted by one byte opcode and one byte
 * operand, so it's really a sequence of 16-bit code.
 *
 *     MATCH   <byte>         matches one byte.
 *     ASSERT  <code>         assertion.
 *     CHARSET <index>        matches to charset in index-th position of
 *                            the constant vector.
 *     JUMP    <next>         jump to the bytecode indexed by <next>*2.
 *     TRY     <next>         try the following sequence.  if fail, jump
 *                            to <next>*2-th bytecode.
 *     BEGIN   <n>            start <n>-th group.
 *     END     <n>            end <n>-th group.
 */

enum {
#define DEF_REG_INSN(insn)  insn,
#include "gauche/reginsn.h"
#undef DEF_REG_INSN(insn)
    RX_NUM_INSN
};

/*
 * Compilation
 */

static ScmObj re_compile(ScmString *expr)
{
    const char *rx = SCM_STRING_START(expr);
    const char *rxp = rx;
    int len = SCM_STRING_LENGTH(expr);

    return SCM_NIL;
}

/*
 * Initialization
 */
void Scm__InitRegexp(void)
{
    ScmModule *m = Scm_GaucheModule();
    Scm_InitBuiltinClass(&Scm_RegexpClass,   "<regexp>", m);
    Scm_InitBuiltinClass(&Scm_RegMatchClass, "<regexp-match>", m);
}
