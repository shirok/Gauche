/*
 * code.h - Virtual machine code
 *
 *   Copyright (c) 2005-2022  Shiro Kawai  <shiro@acm.org>
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
 */

#ifndef GAUCHE_CODE_H
#define GAUCHE_CODE_H

SCM_DECL_BEGIN

#include <gauche/vm.h>

/*
 * Compiled code packet
 */

struct ScmCompiledCodeRec {
    SCM_HEADER;
    ScmWord *code;              /* Code vector (*1). This is allocated as
                                   atomic, to prevent GC from scanning it.
                                   (*2) */
    ScmObj *constants;          /* Constant vector.  this isn't used during
                                   execution, but kept here so that the
                                   constants in the code vector won't be
                                   GC-ed. (*2) */
    int codeSize;               /* size of code vector */
    int constantSize;           /* size of constant vector (*2) */
    int maxstack;               /* maximum runtime stack depth */
    u_short requiredArgs;       /* # of required args, if this code is the
                                   body of a closure.  Otherwise 0. */
    u_short optionalArgs;       /* 1 if this code is the body of a closure.
                                   that takes rest arg.  Otherwise 0. */
    ScmObj name;                /* If this is the body of a closure, holds
                                   its name.  Otherwise #f. */
    ScmObj debugInfo;           /* debug info, that associates instructions
                                   and source code / other metainfo.  May be
                                   () if no info is available. (*3) */
    ScmObj signatureInfo;       /* signature info, a metainfo related to the
                                   interface of this closure.   Maybe #f
                                   if no info is available. (*4) */
    ScmObj parent;              /* ScmCompiledCode if this code is compiled
                                   within other code chunk.  #f otherwise. */
    ScmObj intermediateForm;    /* A packed IForm of the body (see compile.scm
                                   for the details of IForm).  It is used
                                   to inline this procedure.  Only set if
                                   the procedure is defined with define-inline.
                                   #f otherwise. (*5) */
    void *builder;              /* An opaque data used during consturcting
                                   the code vector.  Usually NULL. */
};

/* Footnotes on ScmCompiledCodeRec
 *
 *   *1) This may be NULL if this compiled code is "partially compiled"---
 *       that is, the compiler only runs pass1 and put the intermediate
 *       form in intermediateForm field.
 *   *2) For the C-dumped code, the code vector is located in a static data
 *       area, subject to GC scanning.  In that case, the constants pointer
 *       is NULL.
 *   *3) ((<offset> <info> ...) ...)
 *       <offset> is either an instruction offset or 'definition (for the
 *       entire closure).
 *       At this moment, only used <info> is (source-info . <source>).
 *   *4) (<signature> <info> ...)
 *       <signature> is (<procedure-name> <formal> ...)
 *       <procedure-name> may be just a symbol, or a list (in case of internal
 *       function).  There's no precise definition for the format yet---it's
 *       for debug information.  See Scm_CompiledCodeFullName().
 *       <formal> ... is the formal argument list, as appears in the original
 *       lambda form, including :key, :optional etc.
 *       <info> is a form of (<key> . <value>).  For now, we have 'type
 *       as a key, and <value> contains symbolic type signature such as
 *       '(<list> <integer> * -> *).  We may add more <key>s later.
 *   *5) This IForm is a direct result of Pass1, i.e. non-optimized form.
 *       Pass2 scans it when IForm is inlined into the caller site.
 */

SCM_CLASS_DECL(Scm_CompiledCodeClass);
#define SCM_CLASS_COMPILED_CODE   (&Scm_CompiledCodeClass)

#define SCM_COMPILED_CODE(obj)    ((ScmCompiledCode*)(obj))
#define SCM_COMPILED_CODE_P(obj)  SCM_XTYPEP(obj, SCM_CLASS_COMPILED_CODE)
#define SCM_COMPILED_CODE_ARG_INFO(obj) (SCM_COMPILED_CODE(obj)->argInfo)
#define SCM_COMPILED_CODE_REQUIRED_ARGS(obj) \
    (SCM_COMPILED_CODE(obj)->requiredArgs)
#define SCM_COMPILED_CODE_OPTIONAL_ARGS(obj) \
    (SCM_COMPILED_CODE(obj)->optionalArgs)

#define SCM_COMPILED_CODE_CONST_INITIALIZER(code, codesize, maxstack, reqargs, optargs, name, debuginfo, signatureinfo, parent, iform) \
    { { SCM_CLASS_STATIC_TAG(Scm_CompiledCodeClass) },   \
      (code), NULL, (codesize), 0, (maxstack),           \
      (reqargs), (optargs), (name), (debuginfo), (signatureinfo),   \
      (parent), (iform), NULL /*builder*/ }

SCM_EXTERN void   Scm_CompiledCodeCopyX(ScmCompiledCode *dest,
                                        const ScmCompiledCode *src);
SCM_EXTERN void   Scm_CompiledCodeDump(ScmCompiledCode *cc);
SCM_EXTERN ScmObj Scm_CompiledCodeToList(ScmCompiledCode *cc);
SCM_EXTERN ScmObj Scm_CompiledCodeFullName(ScmCompiledCode *cc);
SCM_EXTERN void   Scm_VMExecuteToplevels(ScmCompiledCode *cv[]);

/*
 * VM instructions
 */
#define SCM_VM_INSN_ARG_MAX          ((1L<<(32-13))-1)
#define SCM_VM_INSN_ARG_MIN          (-SCM_VM_INSN_ARG_MAX)
#define SCM_VM_INSN_ARG_FITS(k) \
    (((k)<=SCM_VM_INSN_ARG_MAX)&&((k)>=SCM_VM_INSN_ARG_MIN))

/* Macros for transition to the packed code vector of NVM.
   In the packed code vector, VM insns are stored untagged.
   It eliminates the shift in the dispatcher. */
#define SCM_VM_INSN_CODE(obj)       ((u_int)(SCM_WORD(obj)&0x0fff))
#define SCM_VM_INSN_ARG(obj)        ((signed long)SCM_WORD(obj) >> 12)
#define SCM_VM_INSN_ARG0(obj)       ((int)((SCM_WORD(obj) >> 12) & 0x03ff))
#define SCM_VM_INSN_ARG1(obj)       ((int)((SCM_WORD(obj) >> 22) & 0x03ff))

#define SCM_VM_INSN(code)           SCM_WORD(code)
#define SCM_VM_INSN1(code, arg)     SCM_WORD((long)((arg)<<12) | (code))
#define SCM_VM_INSN2(code, arg0, arg1)  \
    SCM_WORD((long)((arg1) << 22) | ((arg0) << 12) | (code))

/* insn flags.  see vminsn.scm for details. */
enum ScmVMInsnFlag {
    SCM_VM_INSN_OBSOLETED = (1L<<0),
    SCM_VM_INSN_FOLD_LREF = (1L<<1)
};

/* Operand type */
enum {
    SCM_VM_OPERAND_NONE,        /* take no operand */
    SCM_VM_OPERAND_OBJ,         /* take ScmObj */
    SCM_VM_OPERAND_CODE,        /* take ScmCompiledCode */
    SCM_VM_OPERAND_CODES,       /* take a list of ScmCompiledCodes */
    SCM_VM_OPERAND_ADDR,        /* take address of next code */
    SCM_VM_OPERAND_OBJ_ADDR     /* take an object and address of next code */
};

SCM_EXTERN const char *Scm_VMInsnName(u_int code);
SCM_EXTERN int Scm_VMInsnNumParams(u_int code);
SCM_EXTERN int Scm_VMInsnOperandType(u_int code);
SCM_EXTERN int Scm_VMInsnNameToCode(ScmObj name);
SCM_EXTERN ScmWord Scm_VMInsnBuild(ScmObj insn);

SCM_DECL_END

#endif /* GAUCHE_CODE_H */
