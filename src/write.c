/*
 * write.c - writer
 *
 *   Copyright (c) 2000-2013  Shiro Kawai  <shiro@acm.org>
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
#include "gauche/port.h"
#include "gauche/builtin-syms.h"

#include <ctype.h>

static void write_walk(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);
static void write_ss(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);
static void write_ss_rec(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);
static void write_object(ScmObj obj, ScmPort *out, ScmWriteContext *ctx);
static ScmObj write_object_fallback(ScmObj *args, int nargs, ScmGeneric *gf);
static void format_write(ScmObj obj, ScmPort *port, ScmWriteContext *ctx,
                         int sharedp);
SCM_DEFINE_GENERIC(Scm_GenericWriteObject, write_object_fallback, NULL);

/*============================================================
 * Writers
 */

/* Note: all internal routine (static functions) assumes the output
   port is properly locked. */

/* Note: the current internal structure is in the transient state.
   handling of writer mode and context should be much better.
   Do not count on these APIs! */

/* Note: in order to support write/ss, we need to pass down the context
   along the call tree.  We can think of a few strategies:

  (a) Use separate context argument : this is logically the most natural way.
      The problem is that the legacy code didn't take the context into
      account (especially in the printer of user-defined objects).

  (b) Attach context information to the port : this isn't "right", because
      theoretically a user program may want to mix output of write/ss and
      other writes into a single port.  However, it isn't likely a problem,
      since (1) the outmost write() call locks the port, hence only one
      thread can write to the port during a single write/ss call, and
      (2) the purpose of write/ss is to produce an output which can be
      read back, so you don't want to mix up other output.

      Another possible drawback is the overhead of dynamic wind in the
      toplevel write() call (since we need to remove the context information
      from the port when write() exits non-locally).  If the port hasn't
      been locked, we need a C-level unwind-protect anyway, so it's not
      a problem.   If the port is already locked, extra dynamic wind may
      impact performance.

      Furthermore, I feel it isn't "right" to modify longer-living data
      (port) for the sake of local, dynamically-scoped information (context).

      The advantage of this method is that legacy code will work unchanged.

  (c) A variation of (b) is to "wrap" the port by a transient procedural
      port, which passes through output data to the original port, _and_
      keeps the context info.  This is clean in the sense that it doesn't
      contaminate the longer-living data (original port) by the transient
      info.  We don't need to worry about dynamic winding as well (we can
      leave the transient port to be GCed).

      The concern is the overhead of forwarding output via procedural
      port interface.

   I'm not sure which is the best way in long run; so, as a temporary
   solution, I use the strategy (b), since it is compatible to the current
   version.  Let's see how it works.
 */

#define SPBUFSIZ   50

/* Two bitmask used internally to indicate extra write mode */
#define WRITE_LIMITED   0x10    /* we're limiting the length of output. */
#define WRITE_CIRCULAR  0x20    /* circular-safe write.  info->table
                                   is set up to look up for circular
                                   objects. */

/* VM-default case mode */
#define DEFAULT_CASE \
   (SCM_VM_RUNTIME_FLAG_IS_SET(Scm_VM(), SCM_CASE_FOLD)? \
    SCM_WRITE_CASE_FOLD:SCM_WRITE_CASE_NOFOLD)

/*
 * Scm_Write - Standard Write.
 */
void Scm_Write(ScmObj obj, ScmObj p, int mode)
{
    ScmWriteContext ctx;
    ScmVM *vm;
    ScmPort *port;

    if (!SCM_OPORTP(p)) {
        Scm_Error("output port required, but got %S", p);
    }
    port = SCM_PORT(p);
    ctx.mode = mode;
    ctx.flags = 0;

    /* if this is a "walk" pass of write/ss, dispatch to the walker */
    if (port->flags & SCM_PORT_WALKING) {
        SCM_ASSERT(SCM_PAIRP(port->data)&&SCM_HASH_TABLE_P(SCM_CDR(port->data)));
        write_walk(obj, port, &ctx);
        return;
    }
    /* if this is a "output" pass of write/ss, call the recursive routine */
    if (port->flags & SCM_PORT_WRITESS) {
        SCM_ASSERT(SCM_PAIRP(port->data)&&SCM_HASH_TABLE_P(SCM_CDR(port->data)));
        write_ss_rec(obj, port, &ctx);
        return;
    }

    /* if case mode is not specified, use default taken from VM default */
    if (SCM_WRITE_CASE(&ctx) == 0) ctx.mode |= DEFAULT_CASE;

    vm = Scm_VM();
    PORT_LOCK(port, vm);
    if (SCM_WRITE_MODE(&ctx) == SCM_WRITE_SHARED) {
        PORT_SAFE_CALL(port, write_ss(obj, port, &ctx));
    } else {
        PORT_SAFE_CALL(port, write_ss_rec(obj, port, &ctx));
    }
    PORT_UNLOCK(port);
}

/*
 * Scm_WriteLimited - Write to limited length.
 *
 *  Characters exceeding WIDTH are truncated.
 *  If the output fits within WIDTH, # of characters actually written
 *  is returned.  Othewise, -1 is returned.
 *
 *  Current implementation is sloppy, potentially wasting time to write
 *  objects which will be just discarded.
 */
int Scm_WriteLimited(ScmObj obj, ScmObj port, int mode, int width)
{
    ScmWriteContext ctx;
    ScmString *str;
    ScmObj out;
    int nc, sharedp = FALSE;

    if (!SCM_OPORTP(port))
        Scm_Error("output port required, but got %S", port);
    out = Scm_MakeOutputStringPort(TRUE);
    SCM_PORT(out)->data = SCM_PORT(port)->data;
    ctx.mode = mode;
    ctx.flags = WRITE_LIMITED;
    ctx.limit = width;
    /* if case mode is not specified, use default taken from VM default */
    if (SCM_WRITE_CASE(&ctx) == 0) ctx.mode |= DEFAULT_CASE;
    /* the walk pass does not produce any output, so we return immediately. */
    if (SCM_PORT(port)->flags & SCM_PORT_WALKING) {
        SCM_ASSERT(SCM_PAIRP(SCM_PORT(port)->data)&&SCM_HASH_TABLE_P(SCM_CDR(SCM_PORT(port)->data)));
        write_walk(obj, SCM_PORT(port), &ctx);
        return 0;               /* doesn't really matter */
    }
    /* we don't need to lock out, for it is private. */
    sharedp = SCM_WRITE_MODE(&ctx) == SCM_WRITE_SHARED;
    format_write(obj, SCM_PORT(out), &ctx, sharedp);
    str = SCM_STRING(Scm_GetOutputString(SCM_PORT(out), 0));
    nc = SCM_STRING_BODY_LENGTH(SCM_STRING_BODY(str));
    if (nc > width) {
        ScmObj sub = Scm_Substring(str, 0, width, FALSE);
        SCM_PUTS(sub, port);    /* this locks port */
        return -1;
    } else {
        SCM_PUTS(str, port);    /* this locks port */
        return nc;
    }
}

/*
 * Scm_WriteCircular - circular-safe writer
 */

int Scm_WriteCircular(ScmObj obj, ScmObj port, int mode, int width)
{
    ScmObj out;
    ScmString *str;
    ScmWriteContext ctx;
    int nc;

    if (!SCM_OPORTP(port)) {
        Scm_Error("output port required, but got %S", port);
    }
    ctx.mode = mode;
    ctx.flags = WRITE_CIRCULAR;
    if (SCM_WRITE_CASE(&ctx) == 0) ctx.mode |= DEFAULT_CASE;
    if (width > 0) {
        ctx.flags |= WRITE_LIMITED;
        ctx.limit = width;
    }
    ctx.ncirc = 0;
    ctx.table = SCM_HASH_TABLE(Scm_MakeHashTableSimple(SCM_HASH_EQ, 8));

    if (width <= 0) {
        ScmVM *vm = Scm_VM();
        PORT_LOCK(SCM_PORT(port), vm);
        PORT_SAFE_CALL(SCM_PORT(port),
                       format_write(obj, SCM_PORT(port), &ctx, TRUE));
        PORT_UNLOCK(SCM_PORT(port));
        return 0;
    }

    if (SCM_PORT(port)->flags & SCM_PORT_WALKING) {
        SCM_ASSERT(SCM_PAIRP(SCM_PORT(port)->data)&&SCM_HASH_TABLE_P(SCM_CDR(SCM_PORT(port)->data)));
        write_walk(obj, SCM_PORT(port), &ctx);
        return 0;               /* doesn't really matter */
    }

    out = Scm_MakeOutputStringPort(TRUE);
    SCM_PORT(out)->data = SCM_PORT(port)->data;
    /* no need to lock out, for it is private */
    format_write(obj, SCM_PORT(out), &ctx, TRUE);
    str = SCM_STRING(Scm_GetOutputString(SCM_PORT(out),0));
    nc = SCM_STRING_BODY_LENGTH(SCM_STRING_BODY(str));
    if (nc > width) {
        ScmObj sub = Scm_Substring(str, 0, width, FALSE);
        SCM_PUTS(sub, port);    /* this locks port */
        return -1;
    } else {
        SCM_PUTS(str, port);    /* this locks port */
        return nc;
    }
}

/*===================================================================
 * Internal writer
 */

/* character name table (first 33 chars of ASCII)*/
static const char *char_names[] = {
    "null",   "x01",   "x02",    "x03",   "x04",   "x05",   "x06",   "x07",
    "x08",    "tab",   "newline","x0b",   "x0c",   "return","x0e",   "x0f",
    "x10",    "x11",   "x12",    "x13",   "x14",   "x15",   "x16",   "x17",
    "x18",    "x19",   "x1a",    "escape","x1c",   "x1d",   "x1e",   "x1f",
    "space"
};

#define CASE_ITAG(obj, str) \
    case SCM_ITAG(obj): Scm_PutzUnsafe(str, -1, port); break;

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
    ScmClass *klass;
    if (nargs != 2 || (nargs == 2 && !SCM_OPORTP(args[1]))) {
        Scm_Error("No applicable method for write-object with %S",
                  Scm_ArrayToList(args, nargs));
    }
    klass = Scm_ClassOf(args[0]);
    Scm_Printf(SCM_PORT(args[1]), "#<%A%s%p>",
               klass->name,
               (SCM_FALSEP(klass->redefined)? " " : ":redefined "),
               args[0]);
    return SCM_TRUE;
}

/* We need two passes to realize write/ss.

   The first pass ("walk" pass) traverses the data and finds out
   all shared substructures and/or cyclic references.  It builds a
   hash table of objects that need special treatment.

   The second pass ("output" pass) writes out the data.

   For the walk pass, we can't use generic traversal algorithm
   if the data contains user-defined structures.  In which case,
   we delegate the walk task to the user-defined print routine.
   In the walk pass, a special dummy port is created.  It is a
   procedural port to which all output is discarded.  If the
   user-defined routine needs to traverse substructure, it calls
   back system's writer routine such as Scm_Write, Scm_Printf,
   so we can effectively traverse entire data to be printed.

   NB: write_walk and write_ss_rec need to traverse recursive structure.
   A simple way is to recurse to each element (e.g. car of a list,
   and each element of a vector) and loop over the sequence.
   However it busts the C stack when deep structure is passed.
   Thus we avoided recursion by managing traversal stack by our own
   ('stack' local variable).    It made the code ugly.  Oh well.

   We can't avoid recusion via class printer.  It would need a major
   overhaul to fix that.  However, just preventing blowup by lists
   and vectors is still useful.

   The stack is a list, whose element can be (#t . list) or
   (index . vector).  In the first case, the list part is the
   rest of the list we should process after the current item is
   written out.  In the second case, we're processing the vector,
   and the next item we should process is pointed by index.
 
   We also set a limit of stack depth in write_ss_rec; in case
   car-circular list (e.g. #0=(#0#) ) is given to the plain write.
   It used to SEGV by busting C stack.  With the change of making it
   non-recursive, it would hog all the heap before crash, which is
   rather unpleasant, so we make it bail out before that.
 */


/* Dummy port for the walk pass */
static ScmPortVTable walker_port_vtable = {
    NULL, NULL, NULL, NULL, NULL,
    NULL, NULL, NULL, NULL, NULL,
    NULL, NULL
};

static ScmPort *make_walker_port(void)
{
    ScmPort *port;
    ScmObj ht;

    port = SCM_PORT(Scm_MakeVirtualPort(SCM_CLASS_PORT, SCM_PORT_OUTPUT,
                                        &walker_port_vtable));
    ht = Scm_MakeHashTableSimple(SCM_HASH_EQ, 0);
    port->data = Scm_Cons(SCM_MAKE_INT(0), ht);
    port->flags = SCM_PORT_WALKING;
    return port;
}

/* pass 1 */
static void write_walk(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmHashTable *ht = SCM_HASH_TABLE(SCM_CDR(port->data));
    ScmObj stack = SCM_NIL;

#define REGISTER(obj)                                           \
    do {                                                        \
        ScmObj e = Scm_HashTableRef(ht, (obj), SCM_UNBOUND);    \
        if (!SCM_UNBOUNDP(e)) {                                 \
            Scm_HashTableSet(ht, (obj), SCM_TRUE, 0);           \
            goto next;                                          \
        }                                                       \
        Scm_HashTableSet(ht, obj, SCM_FALSE, 0);                \
    } while (0)

    for (;;) {
    walk1:
        if (!SCM_PTRP(obj) || SCM_KEYWORDP(obj) || SCM_NUMBERP(obj)
            || (SCM_SYMBOLP(obj) && SCM_SYMBOL_INTERNED(obj))) {
            goto next;
        }

        if (SCM_STRINGP(obj)) {
            if (!SCM_STRING_NULL_P(obj)) REGISTER(obj);
            goto next;
        }
        if (SCM_SYMBOLP(obj)) {
            SCM_ASSERT(!SCM_SYMBOL_INTERNED(obj));
            REGISTER(obj);
            goto next;
        }

        if (SCM_PAIRP(obj)) {
            REGISTER(obj);
            stack = Scm_Acons(SCM_TRUE, SCM_CDR(obj), stack);
            obj = SCM_CAR(obj);
            goto walk1;
        }
        if (SCM_VECTORP(obj)) {
            if (SCM_VECTOR_SIZE(obj) == 0) goto next;
            REGISTER(obj);
            stack = Scm_Acons(SCM_MAKE_INT(1), obj, stack);
            obj = SCM_VECTOR_ELEMENT(obj, 0);
            goto walk1;
        }
        /* Now we have user-defined object.
           Call the user's print routine. */
        REGISTER(obj);
        write_general(obj, port, ctx);
        goto next;
    next:
        while (SCM_PAIRP(stack)) {
            ScmObj top = SCM_CAR(stack);
            SCM_ASSERT(top);
            if (SCM_INTP(SCM_CAR(top))) {
                ScmObj v = SCM_CDR(top);
                int i = SCM_INT_VALUE(SCM_CAR(top));
                if (i == SCM_VECTOR_SIZE(v)) {
                    stack = SCM_CDR(stack);
                } else {
                    obj = SCM_VECTOR_ELEMENT(v, i);
                    SCM_SET_CAR(top, SCM_MAKE_INT(i+1));
                    goto walk1;
                }
            } else {
                /* A simple way would be:
                 *   obj = SCM_CDR(top); stack = SCM_CDR(stack); goto walk1;
                 * However it will cause consing for every cdr traversal.
                 * We'd rather reuse the cell.
                 */
                ScmObj v = SCM_CDR(top);
                if (SCM_PAIRP(v)) {
                    ScmObj e = Scm_HashTableRef(ht, v, SCM_UNBOUND);
                    if (!SCM_UNBOUNDP(e)) {
                        Scm_HashTableSet(ht, v, SCM_TRUE, 0);
                        stack = SCM_CDR(stack);
                    } else {
                        Scm_HashTableSet(ht, v, SCM_FALSE, 0);
                        obj = SCM_CAR(v);
                        SCM_SET_CDR(top, SCM_CDR(v)); /*  reuse stack top */
                        goto walk1;
                    }
                } else {
                    obj = v;
                    stack = SCM_CDR(stack);
                    goto walk1;
                }
            }
        }
        break;
    }
}

/* pass 2 */

static void write_ss_nonptr(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    if (SCM_IMMEDIATEP(obj)) {
        switch (SCM_ITAG(obj)) {
            CASE_ITAG(SCM_FALSE,     "#f");
            CASE_ITAG(SCM_TRUE,      "#t");
            CASE_ITAG(SCM_NIL,       "()");
            CASE_ITAG(SCM_EOF,       "#<eof>");
            CASE_ITAG(SCM_UNDEFINED, "#<undef>");
            CASE_ITAG(SCM_UNBOUND,   "#<unbound>");
        default:
            Scm_Panic("write: unknown itag object: %08x", SCM_WORD(obj));
        }
    }
    else if (SCM_INTP(obj)) {
        char buf[SPBUFSIZ];
        snprintf(buf, SPBUFSIZ, "%ld", SCM_INT_VALUE(obj));
        Scm_PutzUnsafe(buf, -1, port);
    }
    else if (SCM_FLONUMP(obj)) {
        write_general(obj, port, ctx);
        return;
    }
    else if (SCM_CHARP(obj)) {
        ScmChar ch = SCM_CHAR_VALUE(obj);
        if (SCM_WRITE_MODE(ctx) == SCM_WRITE_DISPLAY) {
            Scm_PutcUnsafe(ch, port);
        } else {
            Scm_PutzUnsafe("#\\", -1, port);
            if (ch <= 0x20)       Scm_PutzUnsafe(char_names[ch], -1, port);
            else if (ch == 0x7f)  Scm_PutzUnsafe("del", -1, port);
            else                  Scm_PutcUnsafe(ch, port);
        }
    }
    else Scm_Panic("write: got a bogus object: %08x", SCM_WORD(obj));
    return;
}

/* A limit of stack depth to detect (potential) car-circular structure
   when we're writing out without shared structure notation.  This is
   an arbitrary limit, but we used to SEGVed in such case, so it's better
   than that. */
#define STACK_LIMIT  0x1000000

static void write_ss_rec(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmObj e;
    char numbuf[50];  /* enough to contain long number */
    ScmHashTable *ht = NULL;
    ScmObj stack = SCM_NIL;
    int stack_depth = 0;

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
        if (ht) stack_depth--;                  \
    } while (0)
    

    if (SCM_PAIRP(port->data) && SCM_HASH_TABLE_P(SCM_CDR(port->data))) {
        ht = SCM_HASH_TABLE(SCM_CDR(port->data));
    }

    for (;;) {
    write1:
        if (ctx->flags & WRITE_LIMITED) {
            if (port->src.ostr.length >= ctx->limit) return;
        }

        if (!SCM_PTRP(obj)) {
            write_ss_nonptr(obj, port, ctx);
            goto next;
        }
        if (SCM_NUMBERP(obj)) {
            /* number may be heap allocated, but we don't use srfi-38 notation. */
            write_general(obj, port, ctx);
            goto next;
        }
        if ((SCM_STRINGP(obj) && SCM_STRING_NULL_P(obj))
                 || (SCM_VECTORP(obj) && SCM_VECTOR_SIZE(obj) == 0)) {
            /* special case where we don't put a reference tag. */
            write_general(obj, port, ctx);
            goto next;
        }

        if (ht) {
            e = Scm_HashTableRef(ht, obj, SCM_FALSE);
            if (!SCM_FALSEP(e)) {
                if (SCM_INTP(e)) {
                    /* This object is already printed. */
                    snprintf(numbuf, 50, "#%ld#", SCM_INT_VALUE(e));
                    Scm_PutzUnsafe(numbuf, -1, port);
                    goto next;
                } else {
                    /* This object will be seen again. Put a reference tag. */
                    int count = SCM_INT_VALUE(SCM_CAR(port->data));
                    snprintf(numbuf, 50, "#%d=", count);
                    Scm_HashTableSet(ht, obj, SCM_MAKE_INT(count), 0);
                    SCM_SET_CAR(port->data, SCM_MAKE_INT(count+1));
                    Scm_PutzUnsafe(numbuf, -1, port);
                }
            }
        }

        /* Writes aggregates */
        if (SCM_PAIRP(obj)) {
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

            /* normal case */
            Scm_PutcUnsafe('(', port);
            PUSH(Scm_Cons(SCM_TRUE, SCM_CDR(obj)));
            obj = SCM_CAR(obj);
            goto write1;
        } else if (SCM_VECTORP(obj)) {
            Scm_PutzUnsafe("#(", -1, port);
            PUSH(Scm_Cons(SCM_MAKE_INT(1), obj));
            obj = SCM_VECTOR_ELEMENT(obj, 0);
            goto write1;
        } else {
            /* string or user-defined object */
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
                } else {
                    Scm_PutcUnsafe(' ', port);
                    obj = SCM_VECTOR_ELEMENT(v, i);
                    SCM_SET_CAR(top, SCM_MAKE_INT(i+1));
                    goto write1;
                }
            } else {
                /* we're processing a list */
                ScmObj v = SCM_CDR(top);
                if (SCM_NULLP(v)) { /* we've done with this list */
                    Scm_PutcUnsafe(')', port);
                    POP();
                } else if (!SCM_PAIRP(v)) {
                    Scm_PutzUnsafe(" . ", -1, port);
                    obj = v;
                    SCM_SET_CDR(top, SCM_NIL);
                    goto write1;
                } else if (ht && !SCM_FALSEP(e = Scm_HashTableRef(ht, v, SCM_FALSE))) {
                    /* cdr part is shared */
                    Scm_PutzUnsafe(" . ", -1, port);
                    obj = v;
                    SCM_SET_CDR(top, SCM_NIL);
                    goto write1;
                } else {
                    Scm_PutcUnsafe(' ', port);
                    obj = SCM_CAR(v);
                    SCM_SET_CDR(top, SCM_CDR(v));
                    goto write1;
                }
            }
        }
        break;
    }

#undef PUSH
#undef POP
}

/* Write/ss main driver
   NB: this should never be called recursively. */
static void write_ss(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmPort *walker_port = make_walker_port();

    /* pass 1 */
    write_walk(obj, walker_port, ctx);
    Scm_ClosePort(walker_port);

    /* pass 2 */
    /* TODO: we need to rewind port mode */
    port->data = walker_port->data;
    port->flags |= SCM_PORT_WRITESS;
    write_ss_rec(obj, port, ctx);
    port->data = SCM_FALSE;
    port->flags &= ~SCM_PORT_WRITESS;
}

/*===================================================================
 * Formatters
 */

/* TODO: provide option to compile format string. */

#define NEXT_ARG(arg, args)                                             \
    do {                                                                \
        if (!SCM_PAIRP(args))                                           \
            Scm_Error("too few arguments for format string: %S", fmt);  \
        arg = SCM_CAR(args);                                            \
        args = SCM_CDR(args);                                           \
        argcnt++;                                                       \
    } while (0)

/* max # of parameters for a format directive */
#define MAX_PARAMS 5

/* dispatch to proper writer */
static void format_write(ScmObj obj, ScmPort *port, ScmWriteContext *ctx,
                         int sharedp)
{
    if (port->flags & SCM_PORT_WALKING) {
        SCM_ASSERT(SCM_PAIRP(port->data)&&SCM_HASH_TABLE_P(SCM_CDR(port->data)));
        write_walk(obj, port, ctx);
        return;
    }
    if (port->flags & SCM_PORT_WRITESS) {
        SCM_ASSERT(SCM_PAIRP(port->data)&&SCM_HASH_TABLE_P(SCM_CDR(port->data)));
        write_ss_rec(obj, port, ctx);
        return;
    }
    if (sharedp) {

        write_ss(obj, port, ctx);
    } else {
        write_ss_rec(obj, port, ctx);
    }
}

/* output string with padding */
static void format_pad(ScmPort *out, ScmString *str,
                       int mincol, int colinc, ScmChar padchar,
                       int rightalign)
{
    int padcount = mincol- SCM_STRING_BODY_LENGTH(SCM_STRING_BODY(str));
    int i;

    if (padcount > 0) {
        if (colinc > 1) {
            padcount = ((padcount+colinc-1)/colinc)*colinc;
        }
        if (rightalign) {
            for (i=0; i<padcount; i++) Scm_PutcUnsafe(padchar, out);
        }
        Scm_PutsUnsafe(str, SCM_PORT(out));
        if (!rightalign) {
            for (i=0; i<padcount; i++) Scm_PutcUnsafe(padchar, out);
        }
    } else {
        Scm_PutsUnsafe(str, out);
    }
}

/* ~s and ~a writer */
static void format_sexp(ScmPort *out, ScmObj arg,
                        ScmObj *params, int nparams,
                        int rightalign, int dots, int mode)
{
    int mincol = 0, colinc = 1, minpad = 0, maxcol = -1, nwritten = 0, i;
    ScmChar padchar = ' ';
    ScmObj tmpout = Scm_MakeOutputStringPort(TRUE);
    ScmString *tmpstr;

    if (nparams>0 && SCM_INTP(params[0])) mincol = SCM_INT_VALUE(params[0]);
    if (nparams>1 && SCM_INTP(params[1])) colinc = SCM_INT_VALUE(params[1]);
    if (nparams>2 && SCM_INTP(params[2])) minpad = SCM_INT_VALUE(params[2]);
    if (nparams>3 && SCM_CHARP(params[3])) padchar = SCM_CHAR_VALUE(params[3]);
    if (nparams>4 && SCM_INTP(params[4])) maxcol = SCM_INT_VALUE(params[4]);

    if (minpad > 0 && rightalign) {
        for (i=0; i<minpad; i++) Scm_PutcUnsafe(padchar, SCM_PORT(tmpout));
    }
    if (maxcol > 0) {
        nwritten = Scm_WriteLimited(arg, tmpout, mode, maxcol);
    } else {
        Scm_Write(arg, tmpout, mode);
    }
    if (minpad > 0 && !rightalign) {
        for (i=0; i<minpad; i++) Scm_PutcUnsafe(padchar, SCM_PORT(tmpout));
    }
    tmpstr = SCM_STRING(Scm_GetOutputString(SCM_PORT(tmpout), 0));

    if (maxcol > 0 && nwritten < 0) {
        const char *s = Scm_GetStringContent(tmpstr, NULL, NULL, NULL), *e;
        if (dots && maxcol > 4) {
            e = Scm_StringBodyPosition(SCM_STRING_BODY(tmpstr), maxcol-4);
            Scm_PutzUnsafe(s, (int)(e-s), out);
            Scm_PutzUnsafe(" ...", 4, out);
        } else {
            e = Scm_StringBodyPosition(SCM_STRING_BODY(tmpstr), maxcol);
            Scm_PutzUnsafe(s, (int)(e-s), out);
        }
    } else {
        format_pad(out, tmpstr, mincol, colinc, padchar, rightalign);
    }
}

/* ~d, ~b, ~o, and ~x */
static void format_integer(ScmPort *out, ScmObj arg,
                           ScmObj *params, int nparams, int radix,
                           int delimited, int alwayssign, int use_upper)
{
    int mincol = 0, commainterval = 3;
    ScmChar padchar = ' ', commachar = ',';
    ScmObj str;
    if (!Scm_IntegerP(arg)) {
        /* if arg is not an integer, use ~a */
        ScmWriteContext ictx;
        ictx.mode = SCM_WRITE_DISPLAY;
        ictx.flags = 0;
        format_write(arg, out, &ictx, FALSE);
        return;
    }
    if (SCM_FLONUMP(arg)) arg = Scm_Exact(arg);
    if (nparams>0 && SCM_INTP(params[0])) mincol = SCM_INT_VALUE(params[0]);
    if (nparams>1 && SCM_CHARP(params[1])) padchar = SCM_CHAR_VALUE(params[1]);
    if (nparams>2 && SCM_CHARP(params[2])) commachar = SCM_CHAR_VALUE(params[2]);
    if (nparams>3 && SCM_INTP(params[3])) commainterval = SCM_INT_VALUE(params[3]);
    str = Scm_NumberToString(arg, radix, use_upper? SCM_NUMBER_FORMAT_USE_UPPER:0);
    if (alwayssign && SCM_STRING_BODY_START(SCM_STRING_BODY(str))[0] != '-') {
        str = Scm_StringAppend2(SCM_STRING(SCM_MAKE_STR("+")),
                                SCM_STRING(str));
    }
    if (delimited && commainterval) {
        /* Delimited output.  We use char*, for str never contains
           mbchar. */
        /* NB: I think the specification of delimited behavior in CLtL2
           contradicts its examples; it is ambiguous about what happens
           if the number is padded. */
        ScmDString tmpout;
        u_int num_digits, colcnt;
        const char *ptr = Scm_GetStringContent(SCM_STRING(str), &num_digits,
                                               NULL, NULL);

        Scm_DStringInit(&tmpout);
        if (*ptr == '-' || *ptr == '+') {
            Scm_DStringPutc(&tmpout, *ptr);
            ptr++;
            num_digits--;
        }
        colcnt = num_digits % commainterval;
        if (colcnt != 0) Scm_DStringPutz(&tmpout, ptr, colcnt);
        while (colcnt < num_digits) {
            if (colcnt != 0) Scm_DStringPutc(&tmpout, commachar);
            Scm_DStringPutz(&tmpout, ptr+colcnt, commainterval);
            colcnt += commainterval;
        }
        str = Scm_DStringGet(&tmpout, 0);
    }
    format_pad(out, SCM_STRING(str), mincol, 1, padchar, TRUE);
}

static void format_proc(ScmPort *out, ScmString *fmt, ScmObj args, int sharedp)
{
    ScmChar ch = 0;
    ScmObj arg, oargs = args;
    ScmPort *fmtstr = SCM_PORT(Scm_MakeInputStringPort(fmt, FALSE));
    int backtracked = FALSE;    /* true if ~:* is used */
    int arglen, argcnt;
    ScmWriteContext sctx, actx; /* context for ~s and ~a */

    arglen = Scm_Length(args);
    argcnt = 0;

    sctx.mode = SCM_WRITE_WRITE;
    sctx.flags = 0;
    actx.mode = SCM_WRITE_DISPLAY;
    actx.flags = 0;

    for (;;) {
        int atflag, colonflag;
        ScmObj params[MAX_PARAMS];
        int numParams;

        ch = Scm_GetcUnsafe(fmtstr);
        if (ch == EOF) {
            if (!backtracked && !SCM_NULLP(args)) {
                Scm_Error("too many arguments for format string: %S", fmt);
            }
            return;
        }

        if (ch != '~') {
            Scm_PutcUnsafe(ch, out);
            continue;
        }

        numParams = 0;
        atflag = colonflag = FALSE;

        for (;;) {
            ch = Scm_GetcUnsafe(fmtstr);
            switch (ch) {
            case EOF:
                Scm_Error("incomplete format string: %S", fmt);
                break;
            case '%':
                Scm_PutcUnsafe('\n', out);
                break;
            case 's':; case 'S':;
                NEXT_ARG(arg, args);
                if (numParams == 0) {
                    format_write(arg, out, &sctx, sharedp);
                } else {
                    format_sexp(out, arg, params, numParams, atflag, colonflag,
                                sharedp? SCM_WRITE_SHARED:SCM_WRITE_WRITE);
                }
                break;
            case 'a':; case 'A':;
                NEXT_ARG(arg, args);
                if (numParams == 0) {
                    /* short path */
                    format_write(arg, out, &actx, sharedp);
                } else {
                    /* TODO: Here we discard sharedp flag, since we don't yet
                       have 'display/ss' mode.  We should separate
                       write/display distinction and shared-write mode. */
                    format_sexp(out, arg, params, numParams, atflag, colonflag,
                                SCM_WRITE_DISPLAY);
                }
                break;
            case 'd':; case 'D':;
                NEXT_ARG(arg, args);
                if (numParams == 0 && !atflag && !colonflag) {
                    format_write(arg, out, &actx, FALSE);
                } else {
                    format_integer(out, arg, params, numParams, 10,
                                   colonflag, atflag, FALSE);
                }
                break;
            case 'b':; case 'B':;
                NEXT_ARG(arg, args);
                if (numParams == 0 && !atflag && !colonflag) {
                    if (Scm_IntegerP(arg)) {
                        format_write(Scm_NumberToString(arg, 2, 0), out,
                                     &actx, FALSE);
                    } else {
                        format_write(arg, out, &actx, FALSE);
                    }
                } else {
                    format_integer(out, arg, params, numParams, 2,
                                   colonflag, atflag, FALSE);
                }
                break;
            case 'o':; case 'O':;
                NEXT_ARG(arg, args);
                if (numParams == 0 && !atflag && !colonflag) {
                    if (Scm_IntegerP(arg)) {
                        format_write(Scm_NumberToString(arg, 8, 0), out,
                                     &actx, FALSE);
                    } else {
                        format_write(arg, out, &actx, FALSE);
                    }
                } else {
                    format_integer(out, arg, params, numParams, 8,
                                   colonflag, atflag, FALSE);
                }
                break;
            case 'x':; case 'X':;
                NEXT_ARG(arg, args);
                if (numParams == 0 && !atflag && !colonflag) {
                    if (Scm_IntegerP(arg)) {
                        int f = (ch == 'X')? SCM_NUMBER_FORMAT_USE_UPPER : 0;
                        format_write(Scm_NumberToString(arg, 16, f),
                                     out, &actx, FALSE);
                    } else {
                        format_write(arg, out, &actx, FALSE);
                    }
                } else {
                    format_integer(out, arg, params, numParams, 16,
                                   colonflag, atflag, ch == 'X');
                }
                break;
            case '*':
                {
                    int argindex;
                    if (numParams) {
                        if (!SCM_INTP(params[0])) goto badfmt;
                        argindex = SCM_INT_VALUE(params[0]);
                    } else {
                        argindex = 1;
                    }
                    if (colonflag) {
                        if (atflag) goto badfmt;
                        argindex = argcnt - argindex;
                        backtracked = TRUE;
                    } else if (!atflag) {
                        argindex = argcnt + argindex;
                    } else {
                        backtracked = TRUE;
                    }
                    if (argindex < 0 || argindex > arglen) {
                        Scm_Error("'~*' format directive refers outside of argument list in %S", fmt);
                    }
                    argcnt = argindex;
                    args = Scm_ListTail(oargs, argcnt, SCM_UNBOUND);
                    break;
                }
            case 'v':; case 'V':;
                if (atflag || colonflag || numParams >= MAX_PARAMS)
                    goto badfmt;
                NEXT_ARG(arg, args);
                if (!SCM_FALSEP(arg) && !SCM_INTP(arg) && !SCM_CHARP(arg)) {
                    Scm_Error("argument for 'v' format parameter in %S should be either an integer, a character or #f, but got %S",
                              fmt, arg);
                }
                params[numParams++] = arg;
                ch = Scm_GetcUnsafe(fmtstr);
                if (ch != ',') Scm_UngetcUnsafe(ch, fmtstr);
                continue;
            case '@':
                if (atflag) {
                    Scm_Error("too many @-flag for formatting directive: %S",
                              fmt);
                }
                atflag = TRUE;
                continue;
            case ':':
                if (colonflag) {
                    Scm_Error("too many :-flag for formatting directive: %S",
                              fmt);
                }
                colonflag = TRUE;
                continue;
            case '\'':
                if (atflag || colonflag) goto badfmt;
                if (numParams >= MAX_PARAMS) goto badfmt;
                ch = Scm_GetcUnsafe(fmtstr);
                if (ch == EOF) goto badfmt;
                params[numParams++] = SCM_MAKE_CHAR(ch);
                ch = Scm_GetcUnsafe(fmtstr);
                if (ch != ',') Scm_UngetcUnsafe(ch, fmtstr);
                continue;
            case '0':; case '1':; case '2':; case '3':; case '4':;
            case '5':; case '6':; case '7':; case '8':; case '9':;
            case '-':; case '+':;
                if (atflag || colonflag || numParams >= MAX_PARAMS) {
                    goto badfmt;
                } else {
                    int sign = (ch == '-')? -1 : 1;
                    unsigned long value = isdigit(ch)? (ch - '0') : 0;
                    for (;;) {
                        ch = Scm_GetcUnsafe(fmtstr);
                        /* TODO: check valid character */
                        if (!isdigit(ch)) {
                            if (ch != ',') Scm_UngetcUnsafe(ch, fmtstr);
                            params[numParams++] = Scm_MakeInteger(sign*value);
                            break;
                        }
                        /* TODO: check overflow */
                        value = value * 10 + (ch - '0');
                    }
                }
                continue;
            case ',':
                if (atflag || colonflag || numParams >= MAX_PARAMS) {
                    goto badfmt;
                } else {
                    params[numParams++] = SCM_FALSE;
                    continue;
                }
            default:
                Scm_PutcUnsafe(ch, out);
                break;
            }
            break;
        }
    }
  badfmt:
    Scm_Error("illegal format string: %S", fmt);
    return;       /* dummy */
}

void Scm_Format(ScmPort *out, ScmString *fmt, ScmObj args, int sharedp)
{
    ScmVM *vm;

    if (!SCM_OPORTP(out)) {
        Scm_Error("output port required, but got %S", out);
    }

    vm = Scm_VM();
    PORT_LOCK(out, vm);
    PORT_SAFE_CALL(out, format_proc(SCM_PORT(out), fmt, args, sharedp));
    PORT_UNLOCK(out);
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

static void vprintf_proc(ScmPort *out, const char *fmt, ScmObj args,
                         int sharedp)
{
    const char *fmtp = fmt;
    ScmObj val;
    ScmDString argbuf;
    char buf[SPBUFSIZ];
    int c, longp = 0, len, mode;

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
                longp++;
                SCM_DSTRING_PUTB(&argbuf, c);
                continue;
            case 'd':; case 'i':; case 'c':
                {
                    SCM_ASSERT(SCM_PAIRP(args));
                    val = SCM_CAR(args);
                    args = SCM_CDR(args);
                    SCM_ASSERT(SCM_EXACTP(val));
                    SCM_DSTRING_PUTB(&argbuf, c);
                    SCM_DSTRING_PUTB(&argbuf, 0);
                    snprintf(buf, SPBUFSIZ, Scm_DStringGetz(&argbuf),
                             Scm_GetInteger(val));
                    Scm_PutzUnsafe(buf, -1, out);
                    break;
                }
            case 'o':; case 'u':; case 'x':; case 'X':
                {
                    SCM_ASSERT(SCM_PAIRP(args));
                    val = SCM_CAR(args);
                    args = SCM_CDR(args);
                    SCM_ASSERT(SCM_EXACTP(val));
                    SCM_DSTRING_PUTB(&argbuf, c);
                    SCM_DSTRING_PUTB(&argbuf, 0);
                    snprintf(buf, SPBUFSIZ, Scm_DStringGetz(&argbuf),
                             Scm_GetUInteger(val));
                    Scm_PutzUnsafe(buf, -1, out);
                    break;
                }
            case 'e':; case 'E':; case 'f':; case 'g':; case 'G':
                {
                    SCM_ASSERT(SCM_PAIRP(args));
                    val = SCM_CAR(args);
                    args = SCM_CDR(args);
                    SCM_ASSERT(SCM_FLONUMP(val));
                    SCM_DSTRING_PUTB(&argbuf, c);
                    SCM_DSTRING_PUTB(&argbuf, 0);
                    snprintf(buf, SPBUFSIZ, Scm_DStringGetz(&argbuf),
                             Scm_GetDouble(val));
                    Scm_PutzUnsafe(buf, -1, out);
                    break;
                }
            case 's':;
                {
                    SCM_ASSERT(SCM_PAIRP(args));
                    val = SCM_CAR(args);
                    args = SCM_CDR(args);
                    SCM_ASSERT(SCM_STRINGP(val));
                    Scm_PutsUnsafe(SCM_STRING(val), out);

                    /* TODO: support right adjustment such as %-10s.
                       Currently we ignore minus sign and pad chars
                       on the right. */
                    for (len = SCM_STRING_BODY_LENGTH(SCM_STRING_BODY(val));
                         len < width;
                         len++) {
                        Scm_PutcUnsafe(' ', out);
                    }
                    break;
                }
            case '%':;
                {
                    Scm_PutcUnsafe('%', out);
                    break;
                }
            case 'p':
                {
                    SCM_ASSERT(SCM_PAIRP(args));
                    val = SCM_CAR(args);
                    args = SCM_CDR(args);
                    SCM_ASSERT(SCM_EXACTP(val));
                    SCM_DSTRING_PUTB(&argbuf, c);
                    SCM_DSTRING_PUTB(&argbuf, 0);
                    snprintf(buf, SPBUFSIZ, Scm_DStringGetz(&argbuf),
                             (void*)(intptr_t)Scm_GetUInteger(val));
                    Scm_PutzUnsafe(buf, -1, out);
                    break;
                }
            case 'S':; case 'A':
                {
                    ScmWriteContext wctx;

                    SCM_ASSERT(SCM_PAIRP(args));
                    val = SCM_CAR(args);
                    args = SCM_CDR(args);

                    mode = (c == 'A')? SCM_WRITE_DISPLAY : SCM_WRITE_WRITE;
                    wctx.mode = mode | DEFAULT_CASE;
                    wctx.flags = 0;

                    if (pound_appeared) {
                        int n = Scm_WriteCircular(val, SCM_OBJ(out), mode, width);
                        if (n < 0 && prec > 0) {
                            Scm_PutzUnsafe(" ...", -1, out);
                        }
                        if (n > 0) {
                            for (; n < prec; n++) Scm_PutcUnsafe(' ', out);
                        }
                    } else if (width == 0) {
                        format_write(val, out, &wctx, sharedp);
                    } else if (dot_appeared) {
                        int n = Scm_WriteLimited(val, SCM_OBJ(out), mode, width);
                        if (n < 0 && prec > 0) {
                            Scm_PutzUnsafe(" ...", -1, out);
                        }
                        if (n > 0) {
                            for (; n < prec; n++) Scm_PutcUnsafe(' ', out);
                        }
                    } else {
                        format_write(val, out, &wctx, sharedp);
                    }
                    break;
                }
            case 'C':
                {
                    SCM_ASSERT(SCM_PAIRP(args));
                    val = SCM_CAR(args);
                    args = SCM_CDR(args);
                    SCM_ASSERT(SCM_EXACTP(val));
                    Scm_PutcUnsafe(Scm_GetInteger(val), out);
                    break;
                }
            case '0':; case '1':; case '2':; case '3':; case '4':;
            case '5':; case '6':; case '7':; case '8':; case '9':
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

void Scm_Vprintf(ScmPort *out, const char *fmt, va_list ap, int sharedp)
{
    ScmObj h = SCM_NIL, t = SCM_NIL;
    const char *fmtp = fmt;
    ScmVM *vm;
    int c, longp;

    if (!SCM_OPORTP(out)) {
        Scm_Error("output port required, but got %S", out);
    }
    /*
     * First pass : pop vararg and make a list of arguments.
     */
    while ((c = *fmtp++) != 0) {
        if (c != '%') continue;
        longp = FALSE;
        while ((c = *fmtp++) != 0) {
            switch (c) {
            case 'd':; case 'i':; case 'c':
                if (longp) {
                    signed long val = va_arg(ap, signed long);
                    SCM_APPEND1(h, t, Scm_MakeInteger(val));
                } else {
                    signed int val = va_arg(ap, signed int);
                    SCM_APPEND1(h, t, Scm_MakeInteger(val));
                }
                break;
            case 'o':; case 'u':; case 'x':; case 'X':
                if (longp) {
                    unsigned long val = va_arg(ap, unsigned long);
                    SCM_APPEND1(h, t, Scm_MakeIntegerU(val));
                } else {
                    unsigned int val = va_arg(ap, unsigned int);
                    SCM_APPEND1(h, t, Scm_MakeIntegerU(val));
                }
                break;
            case 'e':; case 'E':; case 'f':; case 'g':; case 'G':
                {
                    double val = va_arg(ap, double);
                    SCM_APPEND1(h, t, Scm_MakeFlonum(val));
                    break;
                }
            case 's':;
                {
                    char *val = va_arg(ap, char *);
                    /* for safety */
                    if (val != NULL) SCM_APPEND1(h, t, SCM_MAKE_STR(val));
                    else SCM_APPEND1(h, t, SCM_MAKE_STR("(null)"));
                    break;
                }
            case '%':;
                {
                    break;
                }
            case 'p':
                {
                    void *val = va_arg(ap, void *);
                    SCM_APPEND1(h, t, Scm_MakeIntegerU((u_long)(intptr_t)val));
                    break;
                }
            case 'S':; case 'A':
                {
                    ScmObj o = va_arg(ap, ScmObj);
                    SCM_APPEND1(h, t, o);
                    break;
                }
            case 'C':
                {
                    int c = va_arg(ap, int);
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
    /*
     * Second pass is called while locking the port.
     */
    vm = Scm_VM();
    PORT_LOCK(out, vm);
    PORT_SAFE_CALL(out, vprintf_proc(out, fmt, h, sharedp));
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
    ScmObj r;
    va_list args;
    va_start(args, fmt);
    r = Scm_Vsprintf(fmt, args, TRUE);
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
}
