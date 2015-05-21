/*
 * code.c - compiled code builder/handler
 *
 *   Copyright (c) 2005-2015  Shiro Kawai  <shiro@acm.org>
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

#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/class.h"
#include "gauche/code.h"
#include "gauche/vminsn.h"
#include "gauche/priv/builtin-syms.h"

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

/* Copy the source compiled-code into the destination.  This is used in
   the external optimizer to 'edit' code vector---such routines can
   create a new compiled-code, then copy it to the original so that
   the identity of compiled-code is kept. */
void Scm_CompiledCodeCopyX(ScmCompiledCode *dest,
                           const ScmCompiledCode *src)
{
    SCM_ASSERT(dest->builder == NULL);
    SCM_ASSERT(src->builder == NULL);

    memcpy(dest, src, sizeof(ScmCompiledCode));
}

/*----------------------------------------------------------------------
 * An API to execute statically compiled toplevel code.  *PROVISIONAL*
 */
static ScmSubrProc execute_toplevels;

void Scm_VMExecuteToplevels(ScmCompiledCode *cs[])
{
    ScmObj proc = Scm_MakeSubr(execute_toplevels, cs, 0, 0, SCM_FALSE);
    Scm_ApplyRec(proc, SCM_NIL);
}

static ScmObj execute_toplevels_cc(ScmObj result, void **data)
{
    ScmCompiledCode **cs = (ScmCompiledCode **)data[0];
    if (cs[0] == NULL) return SCM_UNDEFINED;
    data[0] = cs+1;
    ScmVM *vm = Scm_VM();
    Scm_VMPushCC(execute_toplevels_cc, data, 1);
    vm->base = cs[0];
    vm->pc = vm->base->code;
    return SCM_UNDEFINED;
}

static ScmObj execute_toplevels(ScmObj *args, int nargs, void *cv)
{
    Scm_VMPushCC(execute_toplevels_cc, &cv, 1);
    return SCM_UNDEFINED;
}

/*----------------------------------------------------------------------
 * Disassembler
 */
static ScmObj check_lifted_closure(ScmWord *p, ScmObj lifted);
static void print_header(const char *prefix, ScmObj name, ScmCompiledCode *cc);

void Scm_CompiledCodeDump(ScmCompiledCode *cc)
{
    ScmObj closures = SCM_NIL, lifted = SCM_NIL, shown_lifted = SCM_NIL;
    int clonum = 0, more = FALSE;

    print_header("main_code", SCM_MAKE_STR(""), cc);
    do {
        ScmWord *p = cc->code;
        Scm_Printf(SCM_CUROUT, "args: %S\n", cc->argInfo);
        for (int i=0; i < cc->codeSize; i++) {
            ScmWord insn = p[i];
            ScmPort *out = SCM_PORT(Scm_MakeOutputStringPort(TRUE));
            ScmObj info = Scm_Assq(SCM_MAKE_INT(i), cc->info);
            u_int code = SCM_VM_INSN_CODE(insn);
            const char *insn_name = Scm_VMInsnName(code);

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
                /* Check if we're referring to a lifted closure. */
                lifted = check_lifted_closure(p+i, lifted);
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
            case SCM_VM_OPERAND_CODES: {
                Scm_Printf(out, "(");
                ScmObj cp;
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
            }
            default:
                /*nothing*/;
            }

            /* Show info */
            ScmObj s = Scm_GetOutputStringUnsafe(out, 0);
            if (!SCM_PAIRP(info)) {
                Scm_Puts(SCM_STRING(s), SCM_CUROUT);
                Scm_Putc('\n', SCM_CUROUT);
            } else {
                int len = SCM_STRING_BODY_SIZE(SCM_STRING_BODY(s));
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
                    Scm_Printf(SCM_CUROUT, "; %#40.1S\n",
                               Scm_UnwrapSyntax(SCM_CDR(srcinfo)));
                }
            }
        }
        more = FALSE;
        if (!SCM_NULLP(closures)) {
            cc = SCM_COMPILED_CODE(SCM_CAAR(closures));
            print_header("closure:", SCM_CDAR(closures), cc);
            closures = SCM_CDR(closures);
            more = TRUE;
        } else if (!SCM_NULLP(lifted)) {
            while (!SCM_NULLP(lifted)) {
                if (SCM_FALSEP(Scm_Memq(SCM_CAAR(lifted), shown_lifted))) {
                    cc = SCM_COMPILED_CODE(SCM_CAAR(lifted));
                    print_header("lifted:", SCM_CDAR(lifted), cc);
                    shown_lifted = Scm_Cons(SCM_CAAR(lifted), shown_lifted);
                    lifted = SCM_CDR(lifted);
                    more = TRUE;
                    break;
                } else {
                    lifted = SCM_CDR(lifted);
                    continue;
                }
            }
        }
    } while (more);
}

static void print_header(const char *prefix, ScmObj name, ScmCompiledCode *cc)
{
    Scm_Printf(SCM_CUROUT, "=== %s%A (name=%S, code=%p, size=%d, const=%d stack=%d):\n",
               prefix, name, cc->name, cc->code,
               cc->codeSize, cc->constantSize, cc->maxstack);
}

/* The compiler may have lifted an internal closure to a global procedure.
   We can tell so if the opcode is GREF_x, and the operand is an identifier,
   whose name is an uninterned symbol and it is globally bound to a procedure.

   If we indeed have a lifted closure, we chain the closure's code and
   the identifier into the lifted list, returns the updated list.
   Otherwise, we return lifted list as is.
 */
static ScmObj check_lifted_closure(ScmWord *p, ScmObj lifted)
{
    ScmWord code = SCM_VM_INSN_CODE(p[0]);
    static ScmWord gref_insns[] = {
        SCM_VM_GREF,
        SCM_VM_GREF_PUSH,
        SCM_VM_GREF_CALL,
        SCM_VM_GREF_TAIL_CALL,
        SCM_VM_PUSH_GREF,
        SCM_VM_PUSH_GREF_CALL,
        SCM_VM_PUSH_GREF_TAIL_CALL
    };

    if (!SCM_IDENTIFIERP(p[1])) return lifted;
    ScmIdentifier *id = Scm_OutermostIdentifier(SCM_IDENTIFIER(p[1]));
    if (SCM_SYMBOL_INTERNED(id->name)) return lifted;

    for (int i=0; i < sizeof(gref_insns)/sizeof(ScmWord); i++) {
        if (code == gref_insns[i]) {
            ScmObj g = Scm_GlobalVariableRef(id->module, SCM_SYMBOL(id->name),
                                             SCM_BINDING_STAY_IN_MODULE);
            if (SCM_CLOSUREP(g)) {
                if (SCM_FALSEP(Scm_Assq(SCM_CLOSURE(g)->code, lifted))) {
                    return Scm_Acons(SCM_CLOSURE(g)->code,
                                     SCM_OBJ(id->name),
                                     lifted);
                } else {
                    return lifted;
                }
            }
        }
    }
    return lifted;
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

/* To perform instruction combination, the builder buffers one insn/operand.
 * currentInsn == SCM_WORD(-1) indicates there's no buffered insn.
 */
typedef struct cc_builder_rec {
    cc_builder_chunk *chunks;
    int numChunks;
    ScmObj constants;           /* list of constants */
    int currentIndex;
    ScmWord currentInsn;        /* buffer for instruction combining.
                                   this can be a special value either
                                   CC_BUILDER_BUFFER_EMPTY or
                                   CC_BUILDER_BUFFER_TRANS.  see below. */
    int    prevOpcode;          /* previous saved insn opcode */
    int    currentOpcode;       /* saved insn opcode */
    int    currentArg0;         /* ditto */
    int    currentArg1;         /* ditto */
    ScmObj currentOperand;      /* ditto */
    ScmObj currentInfo;         /* ditto */
    int    currentState;        /* index to the current state of
                                   combiner STN */
    ScmObj labelDefs;           /* alist of (name . offset) */
    ScmObj labelRefs;           /* alist of (name . offset-to-fill) */
    int labelCount;             /* counter to generate unique labels */
    ScmObj info;                /* alist of (offset (source-info obj)) */
} cc_builder;

/* Indicates that there's no pending instruction. */
#define CC_BUILDER_BUFFER_EMPTY       SCM_WORD(-1)

/* Indicates that the instruction combiner is in the transitional
   state.  In ordinary circumstances this state will be resolved
   as the code generation goes.  However, if the instruction combination
   is "cut off", for example by emitting a jump destination label, we
   have to complete the instruction.  It is done by seeking for
   the default arc of the current state.  */
#define CC_BUILDER_BUFFER_TRANS       SCM_WORD(-2)

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
    cc_builder *b = SCM_NEW(cc_builder);
    b->chunks = NULL;
    b->numChunks = 0;
    b->constants = SCM_NIL;
    b->currentIndex = 0;
    b->currentInsn = CC_BUILDER_BUFFER_EMPTY;
    b->currentOpcode = b->prevOpcode = -1;
    b->currentOperand = b->currentInfo = SCM_FALSE;
    b->currentState = -1;
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

static void finish_transition(cc_builder *b);

/* Flush the currentInsn buffer. */
static void cc_builder_flush(cc_builder *b)
{
    if ((b)->currentInsn == CC_BUILDER_BUFFER_EMPTY) return;
    if ((b)->currentInsn == CC_BUILDER_BUFFER_TRANS) {
        finish_transition(b);
    }
    cc_builder_add_info(b);
    cc_builder_add_word(b, b->currentInsn);

    u_int code = SCM_VM_INSN_CODE(b->currentInsn);
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
    b->currentInsn = CC_BUILDER_BUFFER_EMPTY;
    b->currentState = -1;
    b->currentOpcode = -1;
    return;
  badoperand:
    b->currentInsn = CC_BUILDER_BUFFER_EMPTY;
    b->currentState = -1;
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

    for (u_int i=0; i<(u_int)cc->codeSize; i++) {
        u_int code = SCM_VM_INSN_CODE(*cp); cp++;
        switch (Scm_VMInsnOperandType(code)) {
        case SCM_VM_OPERAND_OBJ:;
        case SCM_VM_OPERAND_CODE:;
        case SCM_VM_OPERAND_CODES:;
            i++; cp++;
            break;
        case SCM_VM_OPERAND_OBJ_ADDR:
            i++; cp++;
            /*FALLTHROUGH*/
        case SCM_VM_OPERAND_ADDR: {
            ScmWord *target = (ScmWord*)*cp;
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
        }
        default:
            break;
        }
    }
}

/* Creates and returns a new empty compiled-code object for building
   new code chunk. */
ScmObj Scm_MakeCompiledCodeBuilder(int reqargs, int optargs,
                                   ScmObj name, ScmObj arginfo,
                                   ScmObj parent, ScmObj intForm)
{
    ScmCompiledCode *cc = make_compiled_code();
    cc->builder = make_cc_builder();
    cc->requiredArgs = reqargs;
    cc->optionalArgs = optargs;
    cc->name = name;
    cc->argInfo = arginfo;
    cc->parent = parent;
    cc->intermediateForm = intForm;
    return SCM_OBJ(cc);
}

/* Returns a label identifier (integer) unique to this code block */
ScmObj Scm_CompiledCodeNewLabel(ScmCompiledCode *cc)
{
    cc_builder *b;
    CC_BUILDER_GET(b, cc);
    ScmObj label = SCM_MAKE_INT(b->labelCount);
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

/* Push arbitrary debug-info into the builder.
   <debug-info> : (<insn-offset> <item> ...)
   <insn-offset> : <integer> or 'definition
   Currently supported item:
   <item> : (<source-info> . source)
*/
void Scm_CompiledCodePushInfo(ScmCompiledCode *cc, ScmObj info)
{
    cc_builder *b;
    CC_BUILDER_GET(b, cc);
    b->info = Scm_Cons(info, b->info);
}

/* Pack the code accumulated in the builder into a code vector.
   Perform label resolution and jump optimization. */
void Scm_CompiledCodeFinishBuilder(ScmCompiledCode *cc, int maxstack)
{
    cc_builder *b;
    CC_BUILDER_GET(b, cc);
    cc_builder_flush(b);
    cc->code = SCM_NEW_ATOMIC2(ScmWord *, b->currentIndex * sizeof(ScmWord));
    cc->codeSize = b->currentIndex;

    /* reverse chunks, leaving the first chunk in bcprev. */
    cc_builder_chunk *bc, *bcprev = NULL;
    for (bc = b->chunks; bc;) {
        cc_builder_chunk *next = bc->prev;
        bc->prev = bcprev;
        bcprev = bc;
        bc = next;
    }

    /* pack words */
    bc = bcprev;
    for (int i=0, j=0; i<b->currentIndex; i++, j++) {
        if (j >= CC_BUILDER_CHUNK_SIZE) {
            bc = bc->prev;
            j = 0;
        }
        cc->code[i] = bc->code[j];
    }

    /* pack constants */
    int numConstants = Scm_Length(b->constants);
    if (numConstants > 0) {
        cc->constants = SCM_NEW_ARRAY(ScmObj, numConstants);
        ScmObj cp = b->constants;
        for (int i=0; i<numConstants; i++, cp=SCM_CDR(cp)) {
            cc->constants[i] = SCM_CAR(cp);
        }
    }
    cc->constantSize = numConstants;

    /* resolve labels */
    ScmObj cp;
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

/*----------------------------------------------------------------
 * Emitting instruction and operand, performing instruction combination
 */

/* This is originally implemented in Scheme, but moved here for efficiency,
 * since this routine is the most frequently called one during compilation.
 */

/* The state transition table */
struct stn_arc {
    int input;                  /* input insn, or -1 for wildcard */
    int action;                 /* EMIT, KEEP, NEXT */
    int next;                   /* emitting insn / next state */
};

/* State transition actions */
enum {
    NEXT,
    EMIT,
    KEEP
};

/* Include STN generated from vminsn.scm */
static struct stn_arc stn[] = {
#define STATE_TABLE
#include "vminsn.c"
#undef STATE_TABLE
};

/* Save the args/operand if necessary */
static inline void save_params(cc_builder *b, int code,
                               int arg0, int arg1, ScmObj operand,
                               ScmObj info)
{
    b->prevOpcode = b->currentOpcode;
    b->currentOpcode = code;
    switch (Scm_VMInsnNumParams(code)) {
    case 2: b->currentArg1 = arg1;
        /* FALLTHROUGH */
    case 1: b->currentArg0 = arg0;
        /* FALLTHROUGH */
    case 0:;
    }
    if (Scm_VMInsnOperandType(code) != SCM_VM_OPERAND_NONE) {
        b->currentOperand = operand;
    }
    if (!SCM_FALSEP(info)) {
        b->currentInfo = info;
    }
}

static int vm_insn_flags(u_int code);

/* Fill the current insn word */
static inline void fill_current_insn(cc_builder *b, int code)
{
    /* A special handling of fold-lref insn.
       Fold-lref insn is a combined insn LREF-XXXX(depth,offset).  What's
       special about it is that we 'fold' specialized LREF insn
       (e.g. LREF10) into generic LREF.
     */
#define SET_LREF_ARGS(dep, off) \
    b->currentArg0 = (dep); b->currentArg1 = (off); break
    if (vm_insn_flags(code) & SCM_VM_INSN_FOLD_LREF) {
        switch (b->prevOpcode) {
        case SCM_VM_LREF0:  SET_LREF_ARGS(0, 0);
        case SCM_VM_LREF1:  SET_LREF_ARGS(0, 1);
        case SCM_VM_LREF2:  SET_LREF_ARGS(0, 2);
        case SCM_VM_LREF3:  SET_LREF_ARGS(0, 3);
        case SCM_VM_LREF10: SET_LREF_ARGS(1, 0);
        case SCM_VM_LREF11: SET_LREF_ARGS(1, 1);
        case SCM_VM_LREF12: SET_LREF_ARGS(1, 2);
        case SCM_VM_LREF20: SET_LREF_ARGS(2, 0);
        case SCM_VM_LREF21: SET_LREF_ARGS(2, 1);
        case SCM_VM_LREF30: SET_LREF_ARGS(3, 0);
        case SCM_VM_LREF: /* args are already set */ break;
        default: Scm_Error("[internal] Compiler internal error: FOLD_LREF insn needs to be combined with LREF*, but prevOpcode = %d", b->prevOpcode);
        }
    }
#undef SET_LREF_ARGS
    
    /* Compose insn word */
    switch (Scm_VMInsnNumParams(code)) {
    case 0: b->currentInsn = SCM_VM_INSN(code); break;
    case 1: b->currentInsn = SCM_VM_INSN1(code, b->currentArg0); break;
    case 2: b->currentInsn = SCM_VM_INSN2(code,
                                          b->currentArg0,
                                          b->currentArg1); break;
    }
}

/* Called by cc_builder_flush to finish the current transition forcibly.
   We look for the default arc (-1) of the current state and use that
   insn to represent the current state.  We can assume parameters and
   operands are set properly in the cc_builder. */
static void finish_transition(cc_builder *b)
{
    int i = b->currentState;
    SCM_ASSERT(i >= 0 && i < sizeof(stn)/sizeof(struct stn_arc[1]));
    for (;; i++) {
        if (stn[i].input < 0) {
            fill_current_insn(b, stn[i].next);
            break;
        }
        SCM_ASSERT(i < sizeof(stn)/sizeof(struct stn_arc[1]));
    }
}

void Scm_CompiledCodeEmit(ScmCompiledCode *cc,
                          int code, /* instruction code number */
                          int arg0, /* instruction code parameter 0 */
                          int arg1, /* instruction code parameter 1 */
                          ScmObj operand,
                          ScmObj info) /* debug info */
{
    cc_builder *b;
    struct stn_arc *arc;
    CC_BUILDER_GET(b, cc);

    if (SCM_VM_COMPILER_FLAG_IS_SET(Scm_VM(), SCM_COMPILE_NOCOMBINE)) {
        save_params(b, code, arg0, arg1, operand, info);
        fill_current_insn(b, code);
        cc_builder_flush(b);
        return;
    }

  restart:
    /* Some insn needs special treatment. */
    if (code == SCM_VM_LREF) {
        static const int lrefs[4][4] = {
            { SCM_VM_LREF0,  SCM_VM_LREF1,  SCM_VM_LREF2,  SCM_VM_LREF3 },
            { SCM_VM_LREF10, SCM_VM_LREF11, SCM_VM_LREF12, -1 },
            { SCM_VM_LREF20, SCM_VM_LREF21, -1, -1 },
            { SCM_VM_LREF30, -1, -1, -1 }
        };
        if (arg0 < 4 && arg1 < 4) {
            int insn = lrefs[arg0][arg1];
            if (insn >= 0) code = insn;
        }
    } else if (code == SCM_VM_CONST) {
        if (SCM_NULLP(operand)) {
            code = SCM_VM_CONSTN;
        } else if (SCM_FALSEP(operand)) {
            code = SCM_VM_CONSTF;
        } else if (SCM_UNDEFINEDP(operand)) {
            code = SCM_VM_CONSTU;
        } else if (SCM_INTP(operand)) {
            long v = SCM_INT_VALUE(operand);
            if (SCM_VM_INSN_ARG_FITS(v)) {
                code = SCM_VM_CONSTI;
                arg0 = v;
            }
        }
    }

    /* Look up the state */
    if (b->currentState < 0) {
        arc = stn + code;
    } else {
        int i = b->currentState;
        for (;;i++) {
            if (stn[i].input == code || stn[i].input == -1) {
                arc = stn + i;
                break;
            }
        }
    }

    switch (arc->action) {
    case EMIT:
        save_params(b, code, arg0, arg1, operand, info);
        fill_current_insn(b, arc->next);
        cc_builder_flush(b);
        b->currentState = -1;
        break;
    case KEEP:
        fill_current_insn(b, arc->next);
        cc_builder_flush(b);
        b->currentState = -1;
        goto restart;
    case NEXT:
        save_params(b, code, arg0, arg1, operand, info);
        b->currentState = arc->next;
        b->currentInsn = CC_BUILDER_BUFFER_TRANS;
        break;
    }
}

/*----------------------------------------------------------------
 * CompiledCode - Scheme interface
 */

/* Converts the code vector into a list.
   Instruction -> (<insn-symbol> [<arg0> <arg1>])
   Obj/Code operand -> as is
   Addr operand -> integer offset from the beginning of the code */
ScmObj Scm_CompiledCodeToList(ScmCompiledCode *cc)
{
    ScmObj h = SCM_NIL, t = SCM_NIL;

    for (u_int i=0; i<(u_int)cc->codeSize; i++) {
        ScmWord insn = cc->code[i];
        u_int code = SCM_VM_INSN_CODE(insn);
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
        case SCM_VM_OPERAND_ADDR: {
            u_int off = (u_int)((ScmWord*)cc->code[++i] - cc->code);
            SCM_APPEND1(h, t, SCM_MAKE_INT(off));
            break;
        }
        case SCM_VM_OPERAND_OBJ_ADDR: {
            u_int off = (u_int)((ScmWord*)cc->code[i+2] - cc->code);
            SCM_APPEND(h, t, SCM_LIST2(SCM_OBJ(cc->code[i+1]),
                                       SCM_MAKE_INT(off)));
            i += 2;
            break;
        }
        }
    }
    return h;
}

static ScmObj code_size_get(ScmObj cc)
{
    return SCM_MAKE_INT(SCM_COMPILED_CODE(cc)->codeSize);
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
    SCM_CLASS_SLOT_SPEC("size", code_size_get, NULL),
    SCM_CLASS_SLOT_SPEC("max-stack", code_maxstack_get, NULL),
    SCM_CLASS_SLOT_SPEC("intermediate-form", code_iform_get, NULL),
    SCM_CLASS_SLOT_SPEC_END()
};

/*===========================================================
 * VM Instruction introspection
 */

static struct insn_info {
    const char *name;           /* name */
    int nparams;                /* # of parameters */
    int operandType;            /* operand type */
    int flags;                  /* flags */
} insn_table[] = {
#define DEFINSN(sym, nam, np, type, flags)                     \
    { nam, np, SCM_CPP_CAT(SCM_VM_OPERAND_, type), flags },
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

int vm_insn_flags(u_int code)   /* private for the time being */
{
    CHECK_CODE(code);
    return insn_table[code].flags;
}

int Scm_VMInsnOperandType(u_int code)
{
    CHECK_CODE(code);
    return insn_table[code].operandType;
}

int Scm_VMInsnNameToCode(ScmObj name)
{
    if (SCM_SYMBOLP(name))  name = SCM_OBJ(SCM_SYMBOL_NAME(name));
    else if (!SCM_STRINGP(name)) {
        Scm_Error("vm-insn-name->code: requires a symbol or a string, but got %S", name);
    }
    const char *n = Scm_GetStringConst(SCM_STRING(name));
    for (int i=0; i<SCM_VM_NUM_INSNS; i++) {
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
    int len = Scm_Length(obj);

    if (len < 1 || len > 3 || !SCM_SYMBOLP(SCM_CAR(obj))) goto badspec;
    int code = Scm_VMInsnNameToCode(SCM_CAR(obj));

    switch (Scm_VMInsnNumParams(code)) {
    case 0:
        if (len != 1) {
            Scm_Error("VM instruction %S takes no parameters, but got %S",
                      SCM_CAR(obj), obj);
        }
        return SCM_VM_INSN(code);
    case 1: {
        if (len != 2) {
            Scm_Error("VM instruction %S takes one parameter, but got %S",
                      SCM_CAR(obj), obj);
        }
        if (!SCM_INTP(SCM_CADR(obj))) goto badspec;
        int arg0 = SCM_INT_VALUE(SCM_CADR(obj));
        return SCM_VM_INSN1(code, arg0);
    }
    case 2: {
        if (len != 3) {
            Scm_Error("VM instruction %S takes two parameters, but got %S",
                      SCM_CAR(obj), obj);
        }
        if (!SCM_INTP(SCM_CADR(obj))) goto badspec;
        if (!SCM_INTP(SCM_CAR(SCM_CDDR(obj)))) goto badspec;
        int arg0 = SCM_INT_VALUE(SCM_CADR(obj));
        int arg1 = SCM_INT_VALUE(SCM_CAR(SCM_CDDR(obj)));
        return SCM_VM_INSN2(code, arg0, arg1);
    }
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
