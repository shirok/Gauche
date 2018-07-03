/*
 * write.c - writer
 *
 *   Copyright (c) 2000-2018  Shiro Kawai  <shiro@acm.org>
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
#include "gauche/writer.h"
#include "gauche/priv/builtin-syms.h"
#include "gauche/priv/macroP.h" /* PVREF stuff.  Will go in future */
#include "gauche/priv/writerP.h"
#include "gauche/priv/portP.h"
#include "gauche/char_attr.h"

#include <ctype.h>

static void write_walk(ScmObj obj, ScmPort *port);
static void write_ss(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);
static void write_rec(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);
static void write_object(ScmObj obj, ScmPort *out, ScmWriteContext *ctx);
static ScmObj write_object_fallback(ScmObj *args, int nargs, ScmGeneric *gf);
SCM_DEFINE_GENERIC(Scm_GenericWriteObject, write_object_fallback, NULL);

static const ScmWriteControls *defaultWriteControls;

/*============================================================
 * Writers
 */

/* Note: all internal routine (static functions) assumes the output
   port is properly locked. */

/* Note: the current internal structure is in the transient state.
   handling of writer mode and context should be much better.
   Do not count on these APIs! */

/* Note: in order to support write/ss, we need to pass down the context
   along the call tree.
   For the time being, we attach transient extra info to the port during
   the call.  It's a bit ugly, though, to rely on such mutation.  In
   future we might introduce a 'wrapper' port, which operates just like
   the inner (wrapped) port but can carry extra info.  We can do it with
   virtual port now, but it takes some overhead; we want it to be much
   lighter.
 */

#define SPBUFSIZ   50

/* Two bitmask used internally to indicate extra write mode */
#define WRITE_LIMITED   0x10    /* we're limiting the length of output. */

/* VM-default case mode */
#define DEFAULT_CASE \
   (SCM_VM_RUNTIME_FLAG_IS_SET(Scm_VM(), SCM_CASE_FOLD)? \
    SCM_WRITE_CASE_FOLD:SCM_WRITE_CASE_NOFOLD)

/* Whether we need 'walk' pass to find out shared and/or ciruclar
   structure.  Now we use two-pass writing by default, and use one-pass
   writing only when requested specifically. */
#define WRITER_NEED_2PASS(ctx) (SCM_WRITE_MODE(ctx) != SCM_WRITE_SIMPLE)

/*
 * WriteContext public API
 */
int Scm_WriteContextMode(const ScmWriteContext *ctx)
{
    return SCM_WRITE_MODE(ctx);
}

int Scm_WriteContextCase(const ScmWriteContext *ctx)
{
    return SCM_WRITE_CASE(ctx);
}

static void write_context_init(ScmWriteContext *ctx, int mode, int flags, int limit)
{
    ctx->mode = mode;
    /* if case mode is not specified, use default taken from VM default */
    if (SCM_WRITE_CASE(ctx) == 0) ctx->mode |= DEFAULT_CASE;
    ctx->flags = flags;
    ctx->limit = limit;
    ctx->controls = NULL;
    if (limit > 0) ctx->flags |= WRITE_LIMITED;
}

/*
 * WriteControls
 */
ScmWriteControls *Scm_MakeWriteControls(const ScmWriteControls *proto)
{
    ScmWriteControls *p = SCM_NEW(ScmWriteControls);
    SCM_SET_CLASS(p, SCM_CLASS_WRITE_CONTROLS);
    if (proto) {
        *p = *proto;
    } else {
        p->printLength = -1;
        p->printLevel = -1;
        p->printWidth = -1;
        p->printBase = 10;
        p->printRadix = FALSE;
        p->printPretty = FALSE;
    }
    return p;
}

const ScmWriteControls *Scm_DefaultWriteControls(void)
{
    return defaultWriteControls;
}

const ScmWriteControls *Scm_GetWriteControls(ScmWriteContext *ctx,
                                             ScmWriteState *st)
{
    if (ctx && ctx->controls) return ctx->controls;
    if (st && st->controls) return st->controls;
    return defaultWriteControls;
}

/*
 * WriteState
 */
/* The class definition is in libio.scm  */

/* NB: For the time being, proto argument is ignored. */
ScmWriteState *Scm_MakeWriteState(ScmWriteState *proto)
{
    ScmWriteState *z = SCM_NEW(ScmWriteState);
    SCM_SET_CLASS(z, SCM_CLASS_WRITE_STATE);
    z->sharedTable = NULL;
    z->sharedCounter = 0;
    z->currentLevel = 0;
    z->controls = NULL;
    return z;
}

/* Cleanup transient data attached to the port. */
static void cleanup_port_write_state(ScmPort *port)
{
    port->flags &= ~(SCM_PORT_WALKING|SCM_PORT_WRITESS);
    if (port->writeState != NULL) {
        ScmWriteState *s = port->writeState;
        port->writeState = NULL;
        /* The table for recursive/shared detection should be GC'ed after
           we drop the reference to it.  However, the table can be quite big
           after doing write-shared on a large graph, and our implementation of
           big hashtables isn't particularly friendly to GC---it is prone
           to be a victim of false pointers, especially on 32bit architecture.
           It becomes an issue if the app repeatedly use write-shared on
           large graph, for an incorrectly retained hashtable may have false
           pointers to other incorrectly retained hashtable, making the amount
           of retained garbage unbounded.  So, we take extra step to clear
           the table to avoid the risk.  In vast majority of the case, the
           table is used only for circle detection, in which case the table
           is small and it won't add much overhead.
        */
        if (s && s->sharedTable) {
            Scm_HashCoreClear(SCM_HASH_TABLE_CORE(s->sharedTable));
        }
    }
}

/*
 * Entry points
 *
 *  For shared/circular structure detection, we have to distinguish
 *  the "toplevel" call to write and the recursive calls.  The catch
 *  is that Scm_Write etc. can be called recursively, via write-object
 *  method, and we can't rely on its arguments to determine which is
 *  the case.  So we see the port to find out if we're in the recursive
 *  mode (see the above discussion about the context.)
 */

/*
 * Scm_Write - Standard Write, convenience version.  Using default controls.
 */
void Scm_Write(ScmObj obj, ScmObj p, int mode)
{
    Scm_WriteWithControls(obj, p, mode, NULL);
}

/*
 * Scm_WriteWithControls - the general entry
 */
void Scm_WriteWithControls(ScmObj obj, ScmObj p, int mode,
                           const ScmWriteControls *ctrl)
{
    if (!SCM_OPORTP(p)) Scm_Error("output port required, but got %S", p);

    ScmPort *port = SCM_PORT(p);
    ScmVM *vm = Scm_VM();
    if (ctrl == NULL) ctrl = Scm_DefaultWriteControls();

    if (PORT_LOCK_OWNER_P(port, vm) && PORT_RECURSIVE_P(port)) {
        /* We're in the recursive call, so we just recurse into write_walk
           or write_rec, according to the phase.   NB: The controls passed
           into the argument CTRL is ignored; the "root" control, passed
           to the toplevel write API, will be used.  */
        if (PORT_WALKER_P(port)) {
            /* Special treatment - if we're "display"-ing a string, we'll
               bypass walk path even if we're in the middle of write/ss.
               Using srfi-38 notation to show displayed strings doesn't
               make sense at all. */
            if (!((mode == SCM_WRITE_DISPLAY) && SCM_STRINGP(obj))) {
                write_walk(obj, port);
            }
        } else {
            ScmWriteContext ctx;
            write_context_init(&ctx, mode, 0, 0);
            write_rec(obj, port, &ctx);
        }

    } else {
        /* We're in the toplevel call.*/
        ScmWriteContext ctx;
        write_context_init(&ctx, mode, 0, 0);
        PORT_LOCK(port, vm);
        if (WRITER_NEED_2PASS(&ctx)) {
            ctx.controls = ctrl;
            PORT_SAFE_CALL(port, write_ss(obj, port, &ctx),
                           cleanup_port_write_state(port));
        } else {
            /* write-simple case.  CTRL is ignored. */
            PORT_SAFE_CALL(port, write_rec(obj, port, &ctx), /*no cleanup*/);
        }
        PORT_UNLOCK(port);
    }
}

/*
 * Scm_WriteLimited - Write to limited length.
 *
 *  Characters exceeding WIDTH are truncated.
 *  If the output fits within WIDTH, # of characters actually written
 *  is returned.  Othewise, -1 is returned.
 *
 *  Currently this API is only used from Scm_Printf, for 'format' has been
 *  moved to libfmt.scm.  I don't like the way this is implemented and would
 *  like to share this with libfmt.scm eventually.
 */
int Scm_WriteLimited(ScmObj obj, ScmObj p, int mode, int width)
{
    if (!SCM_OPORTP(p)) {
        Scm_Error("output port required, but got %S", p);
    }

    ScmPort *port = SCM_PORT(p);

    /* The walk pass does not produce any output, so we don't bother to
       create an intermediate string port. */
    if (PORT_LOCK_OWNER_P(port, Scm_VM()) && PORT_WALKER_P(port)) {
        SCM_ASSERT(PORT_RECURSIVE_P(port));
        write_walk(obj, port);
        return 0;               /* doesn't really matter */
    }

    ScmObj out = Scm_MakeOutputStringPort(TRUE);
    SCM_PORT(out)->writeState = SCM_PORT(port)->writeState;
    ScmWriteContext ctx;
    write_context_init(&ctx, mode, 0, width);

    /* We don't need to lock 'out', nor clean it up, for it is private. */
    /* This part is a bit confusing - we only need to call write_ss
       if we're at the toplevel call.  */
    if (PORT_RECURSIVE_P(SCM_PORT(port))) {
        write_rec(obj, SCM_PORT(out), &ctx);
    } else if (WRITER_NEED_2PASS(&ctx)) {
        write_ss(obj, SCM_PORT(out), &ctx);
    } else {
        write_rec(obj, SCM_PORT(out), &ctx);
    }
    
    ScmString *str = SCM_STRING(Scm_GetOutputString(SCM_PORT(out), 0));
    int nc = SCM_STRING_BODY_LENGTH(SCM_STRING_BODY(str));
    if (nc > width) {
        ScmObj sub = Scm_Substring(str, 0, width, FALSE);
        SCM_PUTS(sub, port);    /* this locks port */
        return -1;
    } else {
        SCM_PUTS(str, port);    /* this locks port */
        return nc;
    }
}

/* OBSOLETED: This is redundant.  Will be gone in 1.0 release. */
int Scm_WriteCircular(ScmObj obj, ScmObj port, int mode, int width)
{
    if (width <= 0) {
        Scm_Write(obj, port, mode);
        return 0;
    } else {
        return Scm_WriteLimited(obj, port, mode, width);
    }
}

/*===================================================================
 * Internal writer
 */

/* Obj is PTR, except pair and vector */
static void write_general(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    ScmClass *c = Scm_ClassOf(obj);
    if (c->print) c->print(obj, out, ctx);
    else          write_object(obj, out, ctx);
}

/* Default object printer delegates print action to generic function
   write-object.   We can't use VMApply here since this function can be
   called deep in the recursive stack of Scm_Write, so the control
   may not return to VM immediately. */
static void write_object(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    Scm_ApplyRec(SCM_OBJ(&Scm_GenericWriteObject),
                 SCM_LIST2(obj, SCM_OBJ(port)));
}

/* Default method for write-object */
static ScmObj write_object_fallback(ScmObj *args, int nargs, ScmGeneric *gf)
{
    if (nargs != 2 || (nargs == 2 && !SCM_OPORTP(args[1]))) {
        Scm_Error("No applicable method for write-object with %S",
                  Scm_ArrayToList(args, nargs));
    }
    ScmClass *klass = Scm_ClassOf(args[0]);
    Scm_Printf(SCM_PORT(args[1]), "#<%A%s%p>",
               klass->name,
               (SCM_FALSEP(klass->redefined)? " " : ":redefined "),
               args[0]);
    return SCM_TRUE;
}

/* character name table (first 33 chars of ASCII)*/
static const char *char_names[] = {
    "null",   "x01",   "x02",    "x03",   "x04",   "x05",   "x06",   "alarm",
    "backspace","tab", "newline","x0b",   "x0c",   "return","x0e",   "x0f",
    "x10",    "x11",   "x12",    "x13",   "x14",   "x15",   "x16",   "x17",
    "x18",    "x19",   "x1a",    "escape","x1c",   "x1d",   "x1e",   "x1f",
    "space"
};

/* Returns # of chars written.
   This can be better in char.c, but to do so, we'd better to clean up
   public interface for ScmWriteContext.
   TODO: It would be nice to have a mode to print character in unicode
   character name.
 */
static size_t write_char(ScmChar ch, ScmPort *port, ScmWriteContext *ctx)
{
    if (SCM_WRITE_MODE(ctx) == SCM_WRITE_DISPLAY) {
        Scm_PutcUnsafe(ch, port);
        return 1;
    } else {
        const char *cname = NULL;
        char buf[SPBUFSIZ];

        Scm_PutzUnsafe("#\\", -1, port);
        if (ch <= 0x20)       cname = char_names[ch];
        else if (ch == 0x7f)  cname = "del";
        else {
            switch (Scm_CharGeneralCategory(ch)) {
            case SCM_CHAR_CATEGORY_Mn:
            case SCM_CHAR_CATEGORY_Mc:
            case SCM_CHAR_CATEGORY_Me:
            case SCM_CHAR_CATEGORY_Zs:
            case SCM_CHAR_CATEGORY_Zl:
            case SCM_CHAR_CATEGORY_Zp:
            case SCM_CHAR_CATEGORY_Cc:
            case SCM_CHAR_CATEGORY_Cf:
            case SCM_CHAR_CATEGORY_Cs:
            case SCM_CHAR_CATEGORY_Co:
            case SCM_CHAR_CATEGORY_Cn:
                /* NB: Legacy Gauche uses native character code for #\xNNNN
                   notation, while R7RS uses Unicode codepoint.  We eventually
                   need a write mode (legacy or r7rs) and switch the output
                   accordingly---the safe bet is to use #\uNNNN for legacy
                   mode and #\xNNNN for R7RS mode.  */
                snprintf(buf, SPBUFSIZ, "x%04x", (unsigned int)ch);
                cname = buf;
                break;
            }
        }

        if (cname) {
            Scm_PutzUnsafe(cname, -1, port);
            return strlen(cname)+2; /* +2 for '#\' */
        } else {
            Scm_PutcUnsafe(ch, port);
            return 3;               /* +2 for '#\' */
        }
    }
}

/* If OBJ is a primitive object (roughly, immediate or number), write it to
   PORT.  Assumes the caller locks the PORT.
   Returns the # of characters written, or #f if OBJ is not a primitive object.
 */
ScmObj Scm__WritePrimitive(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    const ScmWriteControls *wp = Scm_GetWriteControls(ctx, port->writeState);
    
#define CASE_ITAG_RET(obj, str)                 \
    case SCM_ITAG(obj):                         \
        Scm_PutzUnsafe(str, -1, port);          \
        return SCM_MAKE_INT(sizeof(str)-1);

    if (SCM_IMMEDIATEP(obj)) {
        switch (SCM_ITAG(obj)) {
            CASE_ITAG_RET(SCM_FALSE,     "#f");
            CASE_ITAG_RET(SCM_TRUE,      "#t");
            CASE_ITAG_RET(SCM_NIL,       "()");
            CASE_ITAG_RET(SCM_EOF,       "#<eof>");
            CASE_ITAG_RET(SCM_UNDEFINED, "#<undef>");
            CASE_ITAG_RET(SCM_UNBOUND,   "#<unbound>");
        default:
            Scm_Panic("write: unknown itag object: %08x", SCM_WORD(obj));
        }
    }
    else if (SCM_INTP(obj) && wp->printBase == 10 && !wp->printRadix) {
        /* Shortcut to avoid allocation */
        char buf[SPBUFSIZ];
        int k = snprintf(buf, SPBUFSIZ, "%ld", SCM_INT_VALUE(obj));
        Scm_PutzUnsafe(buf, -1, port);
        return SCM_MAKE_INT(k);
    }
    else if (SCM_CHARP(obj)) {
        size_t k = write_char(SCM_CHAR_VALUE(obj), port, ctx);
        return SCM_MAKE_INT(k);
    }
    else if (SCM_NUMBERP(obj)) {
        ScmNumberFormat fmt;
        Scm_NumberFormatInit(&fmt);
        fmt.radix = wp->printBase;
        if (wp->printRadix) fmt.flags |= SCM_NUMBER_FORMAT_ALT_RADIX;
        return SCM_MAKE_INT(Scm_PrintNumber(port, obj, &fmt));
    }
    /* PVREF only appears in pattern temlate in the current macro expander.
       It will be go away once we rewrite the expander. */
    else if (SCM_PVREF_P(obj)) {
        char buf[SPBUFSIZ];
        int k = snprintf(buf, SPBUFSIZ, "#<pvar %" PRIdPTR ".%" PRIdPTR ">",
                         SCM_PVREF_LEVEL(obj), SCM_PVREF_COUNT(obj));
        Scm_PutzUnsafe(buf, -1, port);
        return SCM_MAKE_INT(k);
    }
    return SCM_FALSE;
}

/* We need two passes to realize write/ss.

   The first pass ("walk" pass) traverses the data and finds out
   all shared substructures and/or cyclic references.  It builds a
   hash table of objects that need special treatment.

   The second pass ("emit" pass) writes out the data.

   For the walk pass, we can't use generic traversal algorithm
   if the data contains user-defined structures.  In which case,
   we delegate the walk task to the user-defined print routine.
   In the walk pass, we set SCM_PORT_WALKING flag of the port.
   Port API recognizes this flag and just ignore any output to
   this port.  Writers recognize this flag and works as the
   walk pass.

   The walk pass sets up a hashtable that records how many times
   each aggregate datum has been seen.  If it's >1, emit pass
   uses #n# and #n= notation.

   NB: R7RS write-shared doesn't require datum labels on strings,
   but srfi-38 does.  We follow srfi-38.

   NB: The walk pass is now written in Scheme (libio.scm: write-walk),
   but the emit pass is in C (write_rec).  Using naive recursion in write_rec
   can bust the C stack when deep structure is passed, even if it is
   not circular.
   Thus we avoided recursion by managing traversal stack by our own
   ('stack' local variable).    It made the code ugly.  Oh well.

   We can't avoid recusion via class printer.  It would need a major
   overhaul to fix that.  However, just preventing blowup by lists
   and vectors is still useful.

   The stack is a list, whose element can be (#t count . list) or
   (index . vector).  In the first case, the count part keeps
   track of how many siblings we already printed, and the list part is the
   rest of the list we should process after the current item is
   written out.  In the second case, we're processing the vector,
   and the next item we should process is pointed by index.

   We also set a limit of stack depth in write_rec; in case
   car-circular list (e.g. #0=(#0#) ) is given to write-simple.
   It used to SEGV by busting C stack.  With the change of making it
   non-recursive, it would hog all the heap before crash, which is
   rather unpleasant, so we make it bail out before that.
 */

/* pass 1 */
/* Implemented in Scheme */
static void write_walk(ScmObj obj, ScmPort *port)
{
    static ScmObj proc = SCM_UNDEFINED;
    SCM_ASSERT(port->writeState);
    ScmHashTable *ht = port->writeState->sharedTable;
    SCM_ASSERT(ht != NULL);
    SCM_BIND_PROC(proc, "%write-walk-rec", Scm_GaucheInternalModule());
    Scm_ApplyRec3(proc, obj, SCM_OBJ(port), SCM_OBJ(ht));
}

/* pass 2 */

/* A limit of stack depth to detect (potential) car-circular structure
   when we're writing out without shared structure notation.  This is
   an arbitrary limit, but we used to SEGVed in such case, so it's better
   than that. */
#define STACK_LIMIT  0x1000000

/* Trick: The hashtable contains positive integer after the walk pass.
   If we emit a reference tag N, we replace the entry's value to -N,
   so that we can distinguish whether we've already emitted the object
   or not. */
static void write_rec(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    char numbuf[50];  /* enough to contain long number */
    ScmObj stack = SCM_NIL;
    ScmWriteState *st = port->writeState;
    ScmHashTable *ht = (st? st->sharedTable : NULL);
    const ScmWriteControls *wp = Scm_GetWriteControls(ctx, st);
    int stack_depth = 0;        /* only used when !ht */

#define PUSH(elt)                                       \
    do {                                                \
        stack = Scm_Cons(elt, stack);                   \
        if (!ht && ++stack_depth > STACK_LIMIT) {       \
            Scm_Error("write recursed too deeply; "     \
                      "maybe a circular structure?");   \
        }                                               \
    } while (0)
#define POP()                                   \
    do {                                        \
        stack = SCM_CDR(stack);                 \
        if (!ht) stack_depth--;                 \
    } while (0)
#define CHECK_LEVEL()                                                   \
    do {                                                                \
        if (st) {                                                       \
            if (wp->printLevel >= 0 && st->currentLevel >= wp->printLevel) { \
                Scm_PutcUnsafe('#', port);                              \
                goto next;                                              \
            } else {                                                    \
                if (st) st->currentLevel++;                             \
            }                                                           \
        }                                                               \
    } while (0)
    

    for (;;) {
    write1:
        if (ctx->flags & WRITE_LIMITED) {
            if (port->src.ostr.length >= ctx->limit) return;
        }

        /* number may be heap allocated, but we don't use srfi-38 notation. */
        if (!SCM_PTRP(obj) || SCM_NUMBERP(obj)) {
            if (SCM_FALSEP(Scm__WritePrimitive(obj, port, ctx))) {
                Scm_Panic("write: got a bogus object: %08x", SCM_WORD(obj));
            }
            goto next;
        }
        if ((SCM_STRINGP(obj) && SCM_STRING_SIZE(obj) == 0)
            || (SCM_VECTORP(obj) && SCM_VECTOR_SIZE(obj) == 0)) {
            /* we don't put a reference tag for these */
            write_general(obj, port, ctx);
            goto next;
        }

        /* obj is heap allocated and we may use label notation. */
        if (ht) {
            ScmObj e = Scm_HashTableRef(ht, obj, SCM_MAKE_INT(1));
            long k = SCM_INT_VALUE(e);
            if (k <= 0) {
                /* This object is already printed. */
                snprintf(numbuf, 50, "#%ld#", -k);
                Scm_PutzUnsafe(numbuf, -1, port);
                goto next;
            } else if (k > 1) {
                /* This object will be seen again. Put a reference tag. */
                ScmWriteState *s = port->writeState;
                snprintf(numbuf, 50, "#%d=", s->sharedCounter);
                Scm_HashTableSet(ht, obj, SCM_MAKE_INT(-s->sharedCounter), 0);
                s->sharedCounter++;
                Scm_PutzUnsafe(numbuf, -1, port);
            }
        }

        /* Writes aggregates */
        if (SCM_PAIRP(obj)) {

            CHECK_LEVEL();

            /* special case for quote etc.
               NB: we need to check if we've seen SCM_CDR(obj), otherwise we'll
               get infinite recursion for the case like (cdr '#1='#1#). */
            if (SCM_PAIRP(SCM_CDR(obj)) && SCM_NULLP(SCM_CDDR(obj))
                && (!ht
                    || SCM_FALSEP(Scm_HashTableRef(ht, SCM_CDR(obj), SCM_FALSE)))){
                const char *prefix = NULL;
                if (SCM_CAR(obj) == SCM_SYM_QUOTE) {
                    prefix = "'";
                } else if (SCM_CAR(obj) == SCM_SYM_QUASIQUOTE) {
                    prefix = "`";
                } else if (SCM_CAR(obj) == SCM_SYM_UNQUOTE) {
                    prefix = ",";
                } else if (SCM_CAR(obj) == SCM_SYM_UNQUOTE_SPLICING) {
                    prefix = ",@";
                }
                if (prefix) {
                    Scm_PutzUnsafe(prefix, -1, port);
                    obj = SCM_CADR(obj);
                    goto write1;
                }
            }

            if (wp->printLength == 0) {
                /* in this case we don't print the elements at all, so we need
                   to treat this specially. */
                Scm_PutzUnsafe("(...)", -1, port);
                if (st) st->currentLevel--;
                goto next;
            }

            /* normal case */
            Scm_PutcUnsafe('(', port);
            PUSH(Scm_Cons(SCM_TRUE, Scm_Cons(SCM_MAKE_INT(1), SCM_CDR(obj))));
            obj = SCM_CAR(obj);
            goto write1;
        } else if (SCM_VECTORP(obj)) {

            CHECK_LEVEL();

            if (wp->printLength == 0) {
                /* in this case we don't print the elements at all, so we need
                   to treat this specially. */
                Scm_PutzUnsafe("#(...)", -1, port);
                if (st) st->currentLevel--;
                goto next;
            }
            Scm_PutzUnsafe("#(", -1, port);
            PUSH(Scm_Cons(SCM_MAKE_INT(1), obj));
            obj = SCM_VECTOR_ELEMENT(obj, 0);
            goto write1;
        } else if (Scm_ClassOf(obj)->flags & SCM_CLASS_AGGREGATE) {
            CHECK_LEVEL();
            write_general(obj, port, ctx);
            if (st) st->currentLevel--;
            goto next;
        } else {
            write_general(obj, port, ctx);
            goto next;
        }

    next:
        while (SCM_PAIRP(stack)) {
            ScmObj top = SCM_CAR(stack);
            SCM_ASSERT(SCM_PAIRP(top));
            if (SCM_INTP(SCM_CAR(top))) {
                /* we're processing a vector */
                ScmObj v = SCM_CDR(top);
                int i = SCM_INT_VALUE(SCM_CAR(top));
                int len = SCM_VECTOR_SIZE(v);

                if (i == len) { /* we've done this vector */
                    Scm_PutcUnsafe(')', port);
                    POP();
                } else if (wp->printLength >= 0 && wp->printLength <= i) {
                    Scm_PutzUnsafe(" ...)", -1, port);
                    POP();
                } else {
                    Scm_PutcUnsafe(' ', port);
                    obj = SCM_VECTOR_ELEMENT(v, i);
                    SCM_SET_CAR(top, SCM_MAKE_INT(i+1));
                    goto write1;
                }
            } else {
                /* we're processing a list */
                SCM_ASSERT(SCM_PAIRP(SCM_CDR(top)));
                long count = SCM_INT_VALUE(SCM_CADR(top));
                ScmObj v = SCM_CDDR(top);
                if (SCM_NULLP(v)) { /* we've done with this list */
                    Scm_PutcUnsafe(')', port);
                    POP();
                } else if (!SCM_PAIRP(v)) {
                    /* Improper list.  We treat aggregate types specially,
                       since such object at this position shouldn't increment
                       "level" - its content is regarded as the same level of
                       the current list.
                     */
                    Scm_PutzUnsafe(" . ", -1, port);
                    if (Scm_ClassOf(v)->flags & SCM_CLASS_AGGREGATE) {
                        if (st) st->currentLevel--;
                        write_rec(v, port, ctx);
                        if (st) st->currentLevel++;
                        Scm_PutcUnsafe(')', port);
                        POP();
                    } else {
                        obj = v;
                        SCM_SET_CAR(SCM_CDR(top), SCM_MAKE_INT(count+1));
                        SCM_SET_CDR(SCM_CDR(top), SCM_NIL);
                        goto write1;
                    }
                } else if (wp->printLength >= 0 && wp->printLength <= count) {
                    /* print-length limit reached */
                    Scm_PutzUnsafe(" ...)", -1, port);
                    POP();
                } else if (ht && !SCM_EQ(Scm_HashTableRef(ht, v, SCM_MAKE_INT(1)), SCM_MAKE_INT(1)))  {
                    /* cdr part is shared */
                    Scm_PutzUnsafe(" . ", -1, port);
                    obj = v;
                    SCM_SET_CAR(SCM_CDR(top), SCM_MAKE_INT(count+1));
                    SCM_SET_CDR(SCM_CDR(top), SCM_NIL);
                    goto write1;
                } else {
                    Scm_PutcUnsafe(' ', port);
                    obj = SCM_CAR(v);
                    SCM_SET_CAR(SCM_CDR(top), SCM_MAKE_INT(count+1));
                    SCM_SET_CDR(SCM_CDR(top), SCM_CDR(v));
                    goto write1;
                }
            }
            if (st) st->currentLevel--;
        }
        break;
    }

#undef PUSH
#undef POP
#undef CHECK_DEPTH
}

/* Write/ss main driver
   This should never be called recursively.
   We modify port->flags and port->writeState; they are cleaned up
   by the caller even if we throw an error during write. */
static void write_ss(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    SCM_ASSERT(port->writeState == NULL);

    /* pass 1 */
    port->flags |= SCM_PORT_WALKING;
    if (SCM_WRITE_MODE(ctx)==SCM_WRITE_SHARED) port->flags |= SCM_PORT_WRITESS;
    ScmWriteState *s = Scm_MakeWriteState(NULL);
    s->sharedTable = SCM_HASH_TABLE(Scm_MakeHashTableSimple(SCM_HASH_EQ, 0));
    s->controls = ctx->controls;
    port->writeState = s;

    write_walk(obj, port);
    port->flags &= ~(SCM_PORT_WALKING|SCM_PORT_WRITESS);

    /* pass 2 */
    if (ctx->controls && ctx->controls->printPretty) {
        static ScmObj proc = SCM_UNDEFINED;
        SCM_BIND_PROC(proc, "%pretty-print", Scm_GaucheInternalModule());
        Scm_ApplyRec4(proc, obj, SCM_OBJ(port),
                      SCM_OBJ(s->sharedTable), SCM_OBJ(ctx->controls));
    } else {
        write_rec(obj, port, ctx);
    }
    cleanup_port_write_state(port);
}

/*OBSOLETED*/
/*format is now in Scheme (libfmt.scm).*/
void Scm_Format(ScmPort *out, ScmString *fmt, ScmObj args, int sharedp)
{
    Scm_Error("Scm_Format is obsoleted");
}

/*
 * Printf()-like formatters
 *
 *  These functions are familiar to C-programmers.   The differences
 *  from C's printf() family are:
 *
 *    - The first argument must be Scheme output port.
 *    - In the format string, the following conversion directives can
 *      be used, as well as the standard printf() directives:
 *
 *        %[width][.prec]S    - The corresponding argument must be
 *                              ScmObj, which is written out by WRITE
 *                              mode.  If width is specified and no
 *                              prec is given, the output is padded
 *                              if it is shorter than width.  If both
 *                              width and prec are given, the output
 *                              is truncated if it is wider than width.
 *
 *        %[width][.prec]A    - Same as %S, but use DISPLAY mode.
 *
 *        %C                  - Take ScmChar argument and outputs it.
 *
 *  Both functions return a number of characters written.
 *
 *  A pound flag '#' for the S directive causes circular-safe output.
 *
 *  NB: %A is taken by C99 for hexadecimal output of double numbers.
 *  We'll introduce a flag for S directive to use DISPLAY mode, and will
 *  move away from %A in future.
 */

struct vprintf_ctx {
    const char *fmt;
    ScmObj args;
};

/* NB: Scm_Vprintf scans format string twice.  In the first pass, arguments
 * are retrieved from va_list variable and pushed to a list.  In the second
 * pass, they are printed according to the format string.
 * It is necessary because we need to do the printing part within a closure
 * called by Scm_WithPortLocking.  On some architecture, we can't pass
 * va_list type of argument in a closure packet easily.
 */

/* Pass 1.  Pop vararg and make a list of arguments.
 * NB: If we're "walking" pass, and the argument is a Lisp object,
 * we recurse to it in this pass.
 */
static ScmObj vprintf_pass1(ScmPort *out, const char *fmt, va_list ap)
{
    ScmObj h = SCM_NIL, t = SCM_NIL;
    const char *fmtp = fmt;
    int c, longp;

    while ((c = *fmtp++) != 0) {
        if (c != '%') continue;
        longp = FALSE;
        while ((c = *fmtp++) != 0) {
            switch (c) {
            case 'd': case 'i': case 'c':
                if (longp) {
                    signed long val = va_arg(ap, signed long);
                    SCM_APPEND1(h, t, Scm_MakeInteger(val));
                } else {
                    signed int val = va_arg(ap, signed int);
                    SCM_APPEND1(h, t, Scm_MakeInteger(val));
                }
                break;
            case 'o': case 'u': case 'x': case 'X':
                if (longp) {
                    unsigned long val = va_arg(ap, unsigned long);
                    SCM_APPEND1(h, t, Scm_MakeIntegerU(val));
                } else {
                    unsigned int val = va_arg(ap, unsigned int);
                    SCM_APPEND1(h, t, Scm_MakeIntegerU(val));
                }
                break;
            case 'e': case 'E': case 'f': case 'g': case 'G':
                {
                    double val = va_arg(ap, double);
                    SCM_APPEND1(h, t, Scm_MakeFlonum(val));
                    break;
                }
            case 's':
                {
                    char *val = va_arg(ap, char *);
                    /* for safety */
                    if (val != NULL) SCM_APPEND1(h, t, SCM_MAKE_STR(val));
                    else SCM_APPEND1(h, t, SCM_MAKE_STR("(null)"));
                    break;
                }
            case '%': break;
            case 'p':
                {
                    void *val = va_arg(ap, void *);
                    SCM_APPEND1(h, t, Scm_MakeIntegerU((u_long)(intptr_t)val));
                    break;
                }
            case 'S': case 'A':
                {
                    ScmObj o = va_arg(ap, ScmObj);
                    SCM_APPEND1(h, t, o);
                    if (PORT_WALKER_P(out)) write_walk(o, out);
                    break;
                }
            case 'C':
                {
                    ScmChar c = va_arg(ap, ScmChar);
                    SCM_APPEND1(h, t, Scm_MakeInteger(c));
                    break;
                }
            case '*':
                {
                    int c = va_arg(ap, int);
                    SCM_APPEND1(h, t, Scm_MakeInteger(c));
                    continue;
                }
            case 'l':
                longp = TRUE;
                continue;
            default:
                continue;
            }
            break;
        }
        if (c == 0) {
            Scm_Error("incomplete %%-directive in format string: %s", fmt);
        }
    }
    return h;
}

/* Pass 2. */
static void vprintf_pass2(ScmPort *out, const char *fmt, ScmObj args)
{
    const char *fmtp = fmt;
    ScmDString argbuf;
    char buf[SPBUFSIZ];
    int c;

    while ((c = *fmtp++) != 0) {
        int width, prec, dot_appeared, pound_appeared;

        if (c != '%') {
            Scm_PutcUnsafe(c, out);
            continue;
        }

        Scm_DStringInit(&argbuf);
        SCM_DSTRING_PUTB(&argbuf, c);
        width = 0, prec = 0, dot_appeared = 0, pound_appeared = 0;
        while ((c = *fmtp++) != 0) {
            switch (c) {
            case 'l':
                SCM_DSTRING_PUTB(&argbuf, c);
                continue;
            case 'd': case 'i': case 'c':
                {
                    SCM_ASSERT(SCM_PAIRP(args));
                    ScmObj val = SCM_CAR(args);
                    args = SCM_CDR(args);
                    SCM_ASSERT(SCM_EXACTP(val));
                    SCM_DSTRING_PUTB(&argbuf, c);
                    SCM_DSTRING_PUTB(&argbuf, 0);
                    snprintf(buf, SPBUFSIZ, Scm_DStringGetz(&argbuf),
                             Scm_GetInteger(val));
                    Scm_PutzUnsafe(buf, -1, out);
                    break;
                }
            case 'o': case 'u': case 'x': case 'X':
                {
                    SCM_ASSERT(SCM_PAIRP(args));
                    ScmObj val = SCM_CAR(args);
                    args = SCM_CDR(args);
                    SCM_ASSERT(SCM_EXACTP(val));
                    SCM_DSTRING_PUTB(&argbuf, c);
                    SCM_DSTRING_PUTB(&argbuf, 0);
                    snprintf(buf, SPBUFSIZ, Scm_DStringGetz(&argbuf),
                             Scm_GetUInteger(val));
                    Scm_PutzUnsafe(buf, -1, out);
                    break;
                }
            case 'e': case 'E': case 'f': case 'g': case 'G':
                {
                    SCM_ASSERT(SCM_PAIRP(args));
                    ScmObj val = SCM_CAR(args);
                    args = SCM_CDR(args);
                    SCM_ASSERT(SCM_FLONUMP(val));
                    SCM_DSTRING_PUTB(&argbuf, c);
                    SCM_DSTRING_PUTB(&argbuf, 0);
                    snprintf(buf, SPBUFSIZ, Scm_DStringGetz(&argbuf),
                             Scm_GetDouble(val));
                    Scm_PutzUnsafe(buf, -1, out);
                    break;
                }
            case 's':
                {
                    SCM_ASSERT(SCM_PAIRP(args));
                    ScmObj val = SCM_CAR(args);
                    args = SCM_CDR(args);
                    SCM_ASSERT(SCM_STRINGP(val));
                    Scm_PutsUnsafe(SCM_STRING(val), out);

                    /* TODO: support right adjustment such as %-10s.
                       Currently we ignore minus sign and pad chars
                       on the right. */
                    for (int len = SCM_STRING_BODY_LENGTH(SCM_STRING_BODY(val));
                         len < width;
                         len++) {
                        Scm_PutcUnsafe(' ', out);
                    }
                    break;
                }
            case '%':
                {
                    Scm_PutcUnsafe('%', out);
                    break;
                }
            case 'p':
                {
                    SCM_ASSERT(SCM_PAIRP(args));
                    ScmObj val = SCM_CAR(args);
                    args = SCM_CDR(args);
                    SCM_ASSERT(SCM_EXACTP(val));
                    SCM_DSTRING_PUTB(&argbuf, c);
                    SCM_DSTRING_PUTB(&argbuf, 0);
                    snprintf(buf, SPBUFSIZ, Scm_DStringGetz(&argbuf),
                             (void*)(intptr_t)Scm_GetUInteger(val));
                    Scm_PutzUnsafe(buf, -1, out);
                    break;
                }
            case 'S': case 'A':
                {
                    SCM_ASSERT(SCM_PAIRP(args));
                    ScmObj val = SCM_CAR(args);
                    args = SCM_CDR(args);

                    int mode = (pound_appeared
                                ? SCM_WRITE_SHARED
                                : ((c == 'A')
                                   ? SCM_WRITE_DISPLAY
                                   : SCM_WRITE_WRITE));
                    int n = 0;
                    if (width <= 0) {
                        Scm_Write(val, SCM_OBJ(out), mode);
                    } else {
                        Scm_WriteLimited(val, SCM_OBJ(out), mode, width);
                    }
                    if (n < 0 && prec > 0) {
                        Scm_PutzUnsafe(" ...", -1, out);
                    }
                    if (n > 0) {
                        for (; n < prec; n++) Scm_PutcUnsafe(' ', out);
                    }
                    break;
                }
            case 'C':
                {
                    SCM_ASSERT(SCM_PAIRP(args));
                    ScmObj val = SCM_CAR(args);
                    args = SCM_CDR(args);
                    SCM_ASSERT(SCM_EXACTP(val));
                    Scm_PutcUnsafe(Scm_GetInteger(val), out);
                    break;
                }
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':
                if (dot_appeared) {
                    prec = prec*10 + (c - '0');
                } else {
                    width = width*10 + (c - '0');
                }
                goto fallback;
            case '.':
                dot_appeared++;
                goto fallback;
            case '#':
                pound_appeared++;
                goto fallback;
            case '*':
                SCM_ASSERT(SCM_PAIRP(args));
                if (dot_appeared) {
                    prec = Scm_GetInteger(SCM_CAR(args));
                } else {
                    width = Scm_GetInteger(SCM_CAR(args));
                }
                args = SCM_CDR(args);
                goto fallback;
            fallback:
            default:
                SCM_DSTRING_PUTB(&argbuf, c);
                continue;
            }
            break;
        }
        if (c == 0) {
            Scm_Error("incomplete %%-directive in format string: %s", fmt);
        }
    }
}

/* Public APIs */

void Scm_Vprintf(ScmPort *out, const char *fmt, va_list ap, int sharedp)
{
    if (!SCM_OPORTP(out)) Scm_Error("output port required, but got %S", out);

    /* TOOD: handle sharedp */
    ScmObj args = vprintf_pass1(out, fmt, ap);

    ScmVM *vm = Scm_VM();
    PORT_LOCK(out, vm);
    PORT_SAFE_CALL(out, vprintf_pass2(out, fmt, args), /*no cleanup*/);
    PORT_UNLOCK(out);
}

void Scm_Printf(ScmPort *out, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    Scm_Vprintf(out, fmt, ap, FALSE);
    va_end(ap);
}

void Scm_PrintfShared(ScmPort *out, const char *fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    Scm_Vprintf(out, fmt, ap, TRUE);
    va_end(ap);
}

ScmObj Scm_Sprintf(const char *fmt, ...)
{
    ScmObj r;
    va_list args;
    va_start(args, fmt);
    r = Scm_Vsprintf(fmt, args, FALSE);
    va_end(args);
    return r;
}

ScmObj Scm_SprintfShared(const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    ScmObj r = Scm_Vsprintf(fmt, args, TRUE);
    va_end(args);
    return r;
}

ScmObj Scm_Vsprintf(const char *fmt, va_list ap, int sharedp)
{
    ScmObj ostr = Scm_MakeOutputStringPort(TRUE);
    Scm_Vprintf(SCM_PORT(ostr), fmt, ap, sharedp);
    return Scm_GetOutputString(SCM_PORT(ostr), 0);
}

/*
 * Initialization
 */
void Scm__InitWrite(void)
{
    Scm_InitBuiltinGeneric(&Scm_GenericWriteObject, "write-object",
                           Scm_GaucheModule());
    defaultWriteControls = Scm_MakeWriteControls(NULL);
}
