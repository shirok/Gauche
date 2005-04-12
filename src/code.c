/*
 * code.c - compiled code builder/handler
 *
 *   Copyright (c) 2005 Shiro Kawai, All rights reserved.
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
 *  $Id: code.c,v 1.2 2005-04-12 01:42:25 shirok Exp $
 */

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/class.h"
#include "gauche/code.h"
#include "gauche/vminsn.h"
#include "gauche/builtin-syms.h"

/*===============================================================
 * NVM related stuff
 */

/* Debug information:
 *
 *  debug info is kept as an assoc-list with insn offset
 *  as a key.
 */

ScmObj Scm_CompiledCodeFullName(ScmCompiledCode *cc)
{
    if (SCM_COMPILED_CODE_P(cc->parent)
        && !SCM_EQ(SCM_COMPILED_CODE(cc->parent)->name, SCM_SYM_TOPLEVEL)) {
        ScmObj h = SCM_NIL, t = SCM_NIL;
        for (;;) {
            SCM_APPEND1(h, t, cc->name);
            if (!SCM_COMPILED_CODE_P(cc->parent)) break;
            cc = SCM_COMPILED_CODE(cc->parent);
            if (SCM_EQ(cc->name, SCM_SYM_TOPLEVEL)) break;
        }
        return Scm_ReverseX(h);
    } else {
        return cc->name;
    }
}

static void compiled_code_print(ScmObj obj, ScmPort *out, ScmWriteContext *c)
{
    Scm_Printf(out, "#<compiled-code %S@%p>",
               Scm_CompiledCodeFullName(SCM_COMPILED_CODE(obj)), obj);
}

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_CompiledCodeClass, compiled_code_print);

static ScmCompiledCode *make_compiled_code(void)
{
    ScmCompiledCode *cc = SCM_NEW(ScmCompiledCode);
    SCM_SET_CLASS(cc, SCM_CLASS_COMPILED_CODE);
    cc->code = NULL;
    cc->constants = NULL;
    cc->maxstack = -1;
    cc->info = SCM_NIL;
    cc->argInfo = SCM_FALSE;
    cc->name = SCM_FALSE;
    cc->parent = SCM_FALSE;
    cc->builder = NULL;
    return cc;
}

/*----------------------------------------------------------------------
 * An API to execute statically compiled toplevel code.  *PROVISIONAL*
 */
static ScmObj execute_toplevels(ScmObj*, int, void*);

void Scm_VMExecuteToplevels(ScmCompiledCode *cs[])
{
    ScmObj proc = Scm_MakeSubr(execute_toplevels, cs, 0, 0, SCM_FALSE);
    Scm_Apply(proc, SCM_NIL);
}

static ScmObj execute_toplevels_cc(ScmObj result, void **data)
{
    ScmCompiledCode **cs = (ScmCompiledCode **)data[0];
    ScmVM *vm;

    if (cs[0] == NULL) return SCM_UNDEFINED;
    data[0] = cs+1;
    Scm_VMPushCC(execute_toplevels_cc, data, 1);
    vm = Scm_VM();
    vm->base = cs[0];
    vm->pc = vm->base->code;
    return SCM_UNDEFINED;
}

static ScmObj execute_toplevels(ScmObj *args, int nargs, void *cv)
{
    ScmCompiledCode **cs = (ScmCompiledCode **)cv;
    Scm_VMPushCC(execute_toplevels_cc, &cv, 1);
    return SCM_UNDEFINED;
}

/*----------------------------------------------------------------------
 * Disassembler
 */
void Scm_CompiledCodeDump(ScmCompiledCode *cc)
{
    int i;
    ScmWord *p;
    ScmObj closures = SCM_NIL, arginfo, cp;
    int clonum = 0;

    Scm_Printf(SCM_CUROUT, "main_code (name=%S, code=%p, size=%d, const=%d, stack=%d):\n",
               cc->name, cc->code, cc->codeSize, cc->constantSize,
               cc->maxstack);
    do {
      loop:
        p = cc->code;
        Scm_Printf(SCM_CUROUT, "args: %S\n", cc->argInfo);
        for (i=0; i < cc->codeSize; i++) {
            ScmWord insn = p[i];
            ScmObj info, s;
            ScmPort *out = SCM_PORT(Scm_MakeOutputStringPort(TRUE));
            u_int code;
            const char *insn_name;

            info = Scm_Assq(SCM_MAKE_INT(i), cc->info);
            code = SCM_VM_INSN_CODE(insn);
            insn_name = Scm_VMInsnName(code);
            
            switch (Scm_VMInsnNumParams(code)) {
            case 0:
                Scm_Printf(out, "  %4d %s ", i, insn_name);
                break;
            case 1:
                Scm_Printf(out, "  %4d %s(%d) ", i, insn_name,
                           SCM_VM_INSN_ARG(insn));
                break;
            case 2:
                Scm_Printf(out, "  %4d %s(%d,%d) ", i, insn_name,
                           SCM_VM_INSN_ARG0(insn),SCM_VM_INSN_ARG1(insn));
                break;
            }
            switch (Scm_VMInsnOperandType(code)) {
            case SCM_VM_OPERAND_ADDR:
                Scm_Printf(out, "%d", (ScmWord*)p[i+1] - cc->code);
                i++;
                break;
            case SCM_VM_OPERAND_OBJ:
                Scm_Printf(out, "%S", p[i+1]);
                i++;
                break;
            case SCM_VM_OPERAND_OBJ_ADDR:
                Scm_Printf(out, "%S, %d", p[i+1], (ScmWord*)p[i+2] - cc->code);
                i += 2;
                break;
            case SCM_VM_OPERAND_CODE:
                Scm_Printf(out, "#<lambda %d>", clonum);
                closures = Scm_Acons(SCM_OBJ(p[i+1]), SCM_MAKE_INT(clonum),
                                     closures);
                clonum++;
                i++;
                break;
            case SCM_VM_OPERAND_CODES:
                Scm_Printf(out, "(");
                SCM_FOR_EACH(cp, SCM_OBJ(p[i+1])) {
                    if (SCM_COMPILED_CODE_P(SCM_CAR(cp))) {
                        closures = Scm_Acons(SCM_CAR(cp),
                                             SCM_MAKE_INT(clonum),
                                             closures);
                        Scm_Printf(out, "#<lambda %d>", clonum);
                        clonum++;
                    }
                }
                Scm_Printf(out, ")");
                i++;
                break;
            default:
                /*nothing*/;
            }

            /* Show info */
            s = Scm_GetOutputStringUnsafe(out);
            if (!SCM_PAIRP(info)) {
                Scm_Puts(SCM_STRING(s), SCM_CUROUT);
                Scm_Putc('\n', SCM_CUROUT);
            } else {
                int len = SCM_STRING_SIZE(s);
                ScmObj srcinfo = Scm_Assq(SCM_SYM_SOURCE_INFO, info);
                ScmObj bindinfo = Scm_Assq(SCM_SYM_BIND_INFO, info);
                Scm_Puts(SCM_STRING(s), SCM_CUROUT);
                Scm_Flush(SCM_CUROUT);
                for (; len<32; len++) {
                    Scm_Putc(' ', SCM_CUROUT);
                }
                if (SCM_FALSEP(srcinfo)) {
                    Scm_Printf(SCM_CUROUT, "; lambda %#40.1S\n",
                               SCM_CDR(bindinfo));
                } else {
                    Scm_Printf(SCM_CUROUT, "; %#40.1S\n", SCM_CDR(srcinfo));
                }
            }
        }
        if (!SCM_NULLP(closures)) {
            cc = SCM_COMPILED_CODE(SCM_CAAR(closures));
            Scm_Printf(SCM_CUROUT, "internal_closure_%S (name=%S, code=%p, size=%d, const=%d stack=%d):\n",
                       SCM_CDAR(closures), cc->name, cc->code,
                       cc->codeSize, cc->constantSize, cc->maxstack);
            closures = SCM_CDR(closures);
            goto loop;
        }
    } while (0);
}

/*------------------------------------------------------------------
 * Builder - used by the new compiler
 */

#define CC_BUILDER_CHUNK_BITS  5
#define CC_BUILDER_CHUNK_SIZE  (1L<<CC_BUILDER_CHUNK_BITS)
#define CC_BUILDER_CHUNK_MASK  (CC_BUILDER_CHUNK_SIZE-1)

typedef struct cc_builder_chunk {
    struct cc_builder_chunk *prev;
    ScmWord code[CC_BUILDER_CHUNK_SIZE];
} cc_builder_chunk;

typedef struct cc_builder_rec {
    cc_builder_chunk *chunks;
    int numChunks;
    ScmObj constants;           /* list of constants */
    int currentIndex;
    ScmObj currentInsn;         /* buffer for instruction combining */
    ScmObj currentOperand;      /* ditto */
    ScmObj currentInfo;         /* ditto */
    ScmObj labelDefs;           /* alist of (name . offset) */
    ScmObj labelRefs;           /* alist of (name . offset-to-fill) */
    int labelCount;             /* counter to generate unique labels */
    ScmObj info;                /* alist of (offset (source-info obj)) */
} cc_builder;

/* Some internal stuff */

#define CC_BUILDER_GET(b, cc)                                           \
    do {                                                                \
        if (cc->builder == NULL) {                                      \
            Scm_Error("[internal error] CompiledCode is already frozen"); \
        }                                                               \
        (b) = (cc_builder*)cc->builder;                                 \
    } while (0)

static cc_builder *make_cc_builder(void)
{
    cc_builder *b;
    b = SCM_NEW(cc_builder);
    b->chunks = NULL;
    b->numChunks = 0;
    b->constants = SCM_NIL;
    b->currentIndex = 0;
    b->currentInsn = b->currentOperand = b->currentInfo = SCM_FALSE;
    b->labelDefs = b->labelRefs = SCM_NIL;
    b->labelCount = 0;
    b->info = SCM_NIL;
    return b;
}

static void cc_builder_add_word(cc_builder *b, ScmWord w)
{
    int ni = b->currentIndex & CC_BUILDER_CHUNK_MASK;
    if (ni == 0) {
        cc_builder_chunk *newchunk = SCM_NEW(cc_builder_chunk);
        newchunk->prev = b->chunks;
        b->chunks = newchunk;
        b->numChunks++;
    }
    b->chunks->code[ni] = w;
    b->currentIndex++;
}

static void cc_builder_add_constant(cc_builder *b, ScmObj obj)
{
    if (!SCM_PTRP(obj)) return;
    if (!SCM_FALSEP(Scm_Memq(obj, b->constants))) return;
    b->constants = Scm_Cons(obj, b->constants);
}

static void cc_builder_add_info(cc_builder *b)
{
    if (SCM_FALSEP(b->currentInfo)) return;
    b->info = Scm_Acons(SCM_MAKE_INT(b->currentIndex),
                        SCM_LIST1(Scm_Cons(SCM_SYM_SOURCE_INFO,
                                           b->currentInfo)),
                        b->info);
    b->currentInfo = SCM_FALSE;
}

static ScmWord *cc_builder_ref(cc_builder *b, int index)
{
    int nc, ni;
    cc_builder_chunk *chunk;
    if (index < 0 || index >= b->currentIndex){
        Scm_Error("compiled code builder: index out of range: %d", index);
    }
    nc = b->numChunks - (index >> CC_BUILDER_CHUNK_BITS) - 1;
    ni = index & CC_BUILDER_CHUNK_BITS;
    chunk = b->chunks;
    while (nc > 0) {
        chunk = chunk->prev;
        nc--;
    }
    return chunk->code + ni;
}

/* Returns label offset of the given label, if the label is already defined.
   Otherwise, returns -1. */
static int cc_builder_label_def(cc_builder *b, ScmObj label)
{
    ScmObj p = Scm_Assq(label, b->labelDefs);
    if (SCM_PAIRP(p)) {
        return SCM_INT_VALUE(SCM_CDR(p));
    } else {
        return -1;
    }
}

/* Flush the currentInsn buffer. */
static void cc_builder_flush(cc_builder *b)
{
    ScmObj args;
    u_int code;
    ScmWord word;
    int labelAddr;
    
    if (!SCM_PAIRP(b->currentInsn)) return;
    code = SCM_INT_VALUE(SCM_CAR(b->currentInsn));
    args = SCM_CDR(b->currentInsn);

    if (code >= SCM_VM_NUM_INSNS) goto badinsn;

    switch (Scm_Length(args)) {
    case 0:
        word = SCM_VM_INSN(code);
        break;
    case 1:
        word = SCM_VM_INSN1(code, SCM_INT_VALUE(SCM_CAR(args)));
        break;
    default:
        word = SCM_VM_INSN2(code,
                            SCM_INT_VALUE(SCM_CAR(args)),
                            SCM_INT_VALUE(SCM_CADR(args)));
        break;
    }
    cc_builder_add_info(b);
    cc_builder_add_word(b, word);

    switch (Scm_VMInsnOperandType(code)) {
    case SCM_VM_OPERAND_ADDR:
        /* Addr should be a label.  We just push the label reference
           into labelRefs, and emit a dummy address for the time being.
           (we can't emit the actual number even if we're referring to
           the label that has already appeared, since the number should
           be calculated after the code vector is allocated.) */
        b->labelRefs = Scm_Acons(b->currentOperand,
                                 SCM_MAKE_INT(b->currentIndex),
                                 b->labelRefs);
        cc_builder_add_word(b, SCM_WORD(0)); /* dummy */
        break;
    case SCM_VM_OPERAND_OBJ:;
    case SCM_VM_OPERAND_CODES:
        cc_builder_add_word(b, SCM_WORD(b->currentOperand));
        cc_builder_add_constant(b, b->currentOperand);
        break;
    case SCM_VM_OPERAND_OBJ_ADDR:
        /* operand would be given as a list of (OBJ LABEL). */
        SCM_ASSERT(SCM_PAIRP(b->currentOperand)
                   && SCM_PAIRP(SCM_CDR(b->currentOperand)));
        cc_builder_add_word(b, SCM_WORD(SCM_CAR(b->currentOperand)));
        cc_builder_add_constant(b, SCM_CAR(b->currentOperand));
        b->labelRefs = Scm_Acons(SCM_CADR(b->currentOperand),
                                 SCM_MAKE_INT(b->currentIndex),
                                 b->labelRefs);
        cc_builder_add_word(b, SCM_WORD(0)); /* dummy */
        break;
    case SCM_VM_OPERAND_CODE:
        if (!SCM_COMPILED_CODE_P(b->currentOperand)) goto badoperand;
        cc_builder_add_word(b, SCM_WORD(b->currentOperand));
        cc_builder_add_constant(b, b->currentOperand);
    default:
        break;
    }
    b->currentInsn = SCM_FALSE;
    return;
  badinsn:
    Scm_Error("[internal error] bad instruction in the compiler: %S",
              b->currentInsn);
  badoperand:
    b->currentInsn = SCM_FALSE;
    Scm_Error("[internal error] bad operand: %S", b->currentOperand);
    return;
}

/* a peephole optimization; rewrite jump destination for cascaded jump
 *
 * - if the destination of JUMP-like insn (including conditional jump
 *   and PRE-CALL) is another JUMP, rewrite the destination.
 * - if the destination of BF is another BF (this pattern appears frequently,
 *   e.g. 'or' is used in the test clause of 'cond'), rewrite the destination.
 */
static void cc_builder_jumpopt(ScmCompiledCode *cc)
{
    ScmWord *cp = cc->code;
    u_int code, i;
    ScmWord *target;

    for (i=0; i<cc->codeSize; i++) {
        code = SCM_VM_INSN_CODE(*cp); cp++;
        switch (Scm_VMInsnOperandType(code)) {
        case SCM_VM_OPERAND_OBJ:;
        case SCM_VM_OPERAND_CODE:;
        case SCM_VM_OPERAND_CODES:;
            i++; cp++;
            break;
        case SCM_VM_OPERAND_OBJ_ADDR:
            i++; cp++;
            /*FALLTHROUGH*/
        case SCM_VM_OPERAND_ADDR:
            target = (ScmWord*)*cp;
            while (SCM_VM_INSN_CODE(*target) == SCM_VM_JUMP
                   || (code == SCM_VM_BF
                       && SCM_VM_INSN_CODE(*target) == SCM_VM_BF)) {
                target = (ScmWord*)target[1];
            }
            if (target != (ScmWord*)*cp) {
                *cp = SCM_WORD(target);
            }
            i++; cp++;
            break;
        default:
            break;
        }
    }
}


/* Creates and returns a new empty compiled-code object for building
   new code chunk. */
ScmObj Scm_MakeCompiledCodeBuilder(int reqargs, int optargs,
                                   ScmObj name, ScmObj parent, ScmObj intForm)
{
    ScmCompiledCode *cc = make_compiled_code();
    cc->builder = make_cc_builder();
    cc->requiredArgs = reqargs;
    cc->optionalArgs = optargs;
    cc->name = name;
    cc->parent = parent;
    cc->intermediateForm = intForm;
    return SCM_OBJ(cc);
}

/* Returns current buffered insn/operand */
ScmObj Scm_CompiledCodeCurrentInsn(ScmCompiledCode *cc)
{
    cc_builder *b;
    CC_BUILDER_GET(b, cc);
    return Scm_Values2(b->currentInsn, b->currentOperand);
}

/* Replace current buffered insn/operand */
void Scm_CompiledCodeReplaceInsn(ScmCompiledCode *cc,
                                 ScmObj insn,
                                 ScmObj operand,
                                 ScmObj info)
{
    cc_builder *b;
    CC_BUILDER_GET(b, cc);
    b->currentInsn = insn;
    b->currentOperand = operand;
    if (!SCM_FALSEP(info)) {
        b->currentInfo = info;
    }
}

/* Flush the current buffered insn/operant to the chunk. */
void Scm_CompiledCodeFlushInsn(ScmCompiledCode *cc)
{
    cc_builder *b;
    CC_BUILDER_GET(b, cc);
    cc_builder_flush(b);
}


/* Put an insn.  If there's a buffered insn/operand, first they are moved
   to the chunk, then the given insn/operand are put in the buffer.
   We assume instruction combination is already done */
void Scm_CompiledCodePutInsn(ScmCompiledCode *cc, ScmObj insn, ScmObj operand,
                             ScmObj info)
{
    cc_builder *b;
    CC_BUILDER_GET(b, cc);
    cc_builder_flush(b);
    b->currentInsn = insn;
    b->currentOperand = operand;
    b->currentInfo = info;
}

/* Returns a label identifier (integer) unique to this code block */
ScmObj Scm_CompiledCodeNewLabel(ScmCompiledCode *cc)
{
    ScmObj label;
    cc_builder *b;
    CC_BUILDER_GET(b, cc);
    label = SCM_MAKE_INT(b->labelCount);
    b->labelCount++;
    return label;
}

/* Set label to the current instruction position. */
void Scm_CompiledCodeSetLabel(ScmCompiledCode *cc, ScmObj label)
{
    cc_builder *b;
    
    CC_BUILDER_GET(b, cc);

    /* Flush buffered insn first. */
    cc_builder_flush(b);

    /* NB: should check duplicate labels */
    b->labelDefs = Scm_Acons(label, SCM_MAKE_INT(b->currentIndex),
                             b->labelDefs);
}

/* Pack the code accumulated in the builder into a code vector.
   Perform label resolution and jump optimization. */
void Scm_CompiledCodeFinishBuilder(ScmCompiledCode *cc, int maxstack)
{
    ScmObj constants = SCM_NIL, cp;
    cc_builder *b;
    cc_builder_chunk *bc, *bcprev;
    int i, j, numConstants;

    CC_BUILDER_GET(b, cc);
    cc_builder_flush(b);
    cc->code = SCM_NEW_ATOMIC2(ScmWord *, b->currentIndex * sizeof(ScmWord));
    cc->codeSize = b->currentIndex;

    /* reverse chunks, leaving the first chunk in bcprev. */
    bcprev = NULL;
    for (bc = b->chunks; bc;) {
        cc_builder_chunk *next = bc->prev;
        bc->prev = bcprev;
        bcprev = bc;
        bc = next;
    }

    /* pack words */
    bc = bcprev;
    for (i=0, j=0; i<b->currentIndex; i++, j++) {
        if (j >= CC_BUILDER_CHUNK_SIZE) {
            bc = bc->prev;
            j = 0;
        }
        cc->code[i] = bc->code[j];
    }

    /* pack constants */
    numConstants = Scm_Length(b->constants);
    if (numConstants > 0) {
        ScmObj cp;
        cc->constants = SCM_NEW_ARRAY(ScmObj, numConstants);
        for (i=0, cp=b->constants; i<numConstants; i++, cp=SCM_CDR(cp)) {
            cc->constants[i] = SCM_CAR(cp);
        }
    }
    cc->constantSize = numConstants;

    /* resolve labels */
    SCM_FOR_EACH(cp, b->labelRefs) {
        int destAddr = cc_builder_label_def(b, SCM_CAAR(cp));
        int operandAddr;
        if (destAddr < 0) {
            Scm_Error("[internal error] undefined label in compiled code: %S",
                      SCM_CAAR(cp));
        }
        operandAddr = SCM_INT_VALUE(SCM_CDAR(cp));
        SCM_ASSERT(operandAddr >= 0 && operandAddr < cc->codeSize);
        cc->code[operandAddr] = SCM_WORD(cc->code + destAddr);
    }

    /* jump destination optimization */
    cc_builder_jumpopt(cc);

    /* record debug info */
    cc->info = b->info;

    /* set max stack depth */
    cc->maxstack = maxstack;
    
    /* make sure this code is 'fixed'---no more building */
    cc->builder = NULL;
}


/* CompiledCode - Scheme interface */

/* Converts the code vector into a list.
   Instruction -> (<insn-symbol> [<arg0> <arg1>])
   Obj/Code operand -> as is
   Addr operand -> integer offset from the beginning of the code */
ScmObj Scm_CompiledCodeToList(ScmCompiledCode *cc)
{
    int i, off;
    ScmObj h = SCM_NIL, t = SCM_NIL;
    
    for (i=0; i<cc->codeSize; i++) {
        ScmWord insn = cc->code[i];
        int code = SCM_VM_INSN_CODE(insn);
        const char *name = Scm_VMInsnName(code);
        
        switch (Scm_VMInsnNumParams(code)) {
        case 0: 
            SCM_APPEND1(h, t, SCM_LIST1(SCM_INTERN(name)));
            break;
        case 1:
            SCM_APPEND1(h, t, SCM_LIST2(SCM_INTERN(name),
                                        SCM_MAKE_INT(SCM_VM_INSN_ARG(insn))));
            break;
        case 2:
            SCM_APPEND1(h, t, SCM_LIST3(SCM_INTERN(name),
                                        SCM_MAKE_INT(SCM_VM_INSN_ARG0(insn)),
                                        SCM_MAKE_INT(SCM_VM_INSN_ARG1(insn))));
            break;
        }

        switch (Scm_VMInsnOperandType(code)) {
        case SCM_VM_OPERAND_OBJ:;
        case SCM_VM_OPERAND_CODE:;
        case SCM_VM_OPERAND_CODES:;
            SCM_APPEND1(h, t, SCM_OBJ(cc->code[++i]));
            break;
        case SCM_VM_OPERAND_ADDR:
            off = (ScmWord*)cc->code[++i] - cc->code;
            SCM_APPEND1(h, t, SCM_MAKE_INT(off));
            break;
        case SCM_VM_OPERAND_OBJ_ADDR:
            off = (ScmWord*)cc->code[i+2] - cc->code;
            SCM_APPEND(h, t, SCM_LIST2(SCM_OBJ(cc->code[i+1]),
                                       SCM_MAKE_INT(off)));
            i += 2;
            break;
        }
    }
    return h;
}

static ScmObj code_maxstack_get(ScmObj cc)
{
    return SCM_MAKE_INT(SCM_COMPILED_CODE(cc)->maxstack);
}

static ScmObj code_info_get(ScmObj cc)
{
    return SCM_COMPILED_CODE(cc)->info;
}

static ScmObj code_arginfo_get(ScmObj cc)
{
    return SCM_COMPILED_CODE(cc)->argInfo;
}

static ScmObj code_reqargs_get(ScmObj cc)
{
    return SCM_MAKE_INT(SCM_COMPILED_CODE(cc)->requiredArgs);
}

static ScmObj code_optargs_get(ScmObj cc)
{
    return SCM_MAKE_INT(SCM_COMPILED_CODE(cc)->optionalArgs);
}

static ScmObj code_name_get(ScmObj cc)
{
    return SCM_COMPILED_CODE(cc)->name;
}

static ScmObj code_parent_get(ScmObj cc)
{
    return SCM_OBJ(SCM_COMPILED_CODE(cc)->parent);
}

static ScmObj code_iform_get(ScmObj cc)
{
    return SCM_OBJ(SCM_COMPILED_CODE(cc)->intermediateForm);
}

static ScmClassStaticSlotSpec code_slots[] = {
    SCM_CLASS_SLOT_SPEC("parent", code_parent_get, NULL),
    SCM_CLASS_SLOT_SPEC("arg-info", code_arginfo_get, NULL),
    SCM_CLASS_SLOT_SPEC("info", code_info_get, NULL),
    SCM_CLASS_SLOT_SPEC("required-args", code_reqargs_get, NULL),
    SCM_CLASS_SLOT_SPEC("optional-args", code_optargs_get, NULL),
    SCM_CLASS_SLOT_SPEC("name", code_name_get, NULL),
    SCM_CLASS_SLOT_SPEC("full-name", Scm_CompiledCodeFullName, NULL),
    SCM_CLASS_SLOT_SPEC("max-stack", code_maxstack_get, NULL),
    SCM_CLASS_SLOT_SPEC("intermediate-form", code_iform_get, NULL),
    { NULL }
};

/*===========================================================
 * VM Instruction introspection
 */

static struct insn_info {
    const char *name;           /* name */
    int nparams;                /* # of parameters */
    int operandType;            /* operand type */
} insn_table[] = {
#define DEFINSN(sym, nam, np, type) \
    { nam, np, SCM_CPP_CAT(SCM_VM_OPERAND_, type) },
#include "vminsn.c"
#undef DEFINSN
};

#define CHECK_CODE(code)                                        \
    do {                                                        \
        if (code >= SCM_VM_NUM_INSNS) {                         \
            Scm_Error("invalid VM instruction code: %d", code); \
        }                                                       \
    } while (0)

const char *Scm_VMInsnName(u_int code)
{
    CHECK_CODE(code);
    return insn_table[code].name;
}

int Scm_VMInsnNumParams(u_int code)
{
    CHECK_CODE(code);
    return insn_table[code].nparams;
}

int Scm_VMInsnOperandType(u_int code)
{
    CHECK_CODE(code);
    return insn_table[code].operandType;
}

int Scm_VMInsnNameToCode(ScmObj name)
{
    const char *n;
    struct insn_info *info;
    int i;
    
    if (SCM_SYMBOLP(name))  name = SCM_OBJ(SCM_SYMBOL_NAME(name));
    else if (!SCM_STRINGP(name)) {
        Scm_Error("vm-insn-name->code: requires a symbol or a string, but got %S", name);
    }
    n = Scm_GetStringConst(SCM_STRING(name));
    info = insn_table;
    for (i=0; i<SCM_VM_NUM_INSNS; i++) {
        if (strcmp(insn_table[i].name, n) == 0) {
            return i;
        }
    }
    Scm_Error("vm-insn-name->code: no such instruction: %A", name);
    return -1;                  /* dummy */
}

/* (kind of) inversion of VMInsnInspect. */
ScmWord Scm_VMInsnBuild(ScmObj obj)
{
    struct insn_info *info;
    int len = Scm_Length(obj), code, arg0, arg1;
    const char *name;
    ScmWord insn = 0;
    
    if (len < 1 || len > 3 || !SCM_SYMBOLP(SCM_CAR(obj))) goto badspec;
    code = Scm_VMInsnNameToCode(SCM_CAR(obj));
    
    switch (Scm_VMInsnNumParams(code)) {
    case 0:
        if (len != 1) {
            Scm_Error("VM instruction %S takes no parameters, but got %S",
                      SCM_CAR(obj), obj);
        }
        return SCM_VM_INSN(code);
    case 1:
        if (len != 2) {
            Scm_Error("VM instruction %S takes one parameter, but got %S",
                      SCM_CAR(obj), obj);
        }
        if (!SCM_INTP(SCM_CADR(obj))) goto badspec;
        arg0 = SCM_INT_VALUE(SCM_CADR(obj));
        return SCM_VM_INSN1(code, arg0);
    case 2:
        if (len != 3) {
            Scm_Error("VM instruction %S takes two parameters, but got %S",
                      SCM_CAR(obj), obj);
        }
        if (!SCM_INTP(SCM_CADR(obj))) goto badspec;
        if (!SCM_INTP(SCM_CAR(SCM_CDDR(obj)))) goto badspec;
        arg0 = SCM_INT_VALUE(SCM_CADR(obj));
        arg1 = SCM_INT_VALUE(SCM_CAR(SCM_CDDR(obj)));
        return SCM_VM_INSN2(code, arg0, arg1);
    }
    /*FALLTHROUGH*/
  badspec:    
    Scm_Error("Bad VM insn spec: %S", obj);
    return 0;       /* dummy */
}

/*===========================================================
 * Initialization
 */
void Scm__InitCode(void)
{
    Scm_InitStaticClass(SCM_CLASS_COMPILED_CODE, "<compiled-code>",
                        Scm_GaucheModule(), code_slots, 0);
}
