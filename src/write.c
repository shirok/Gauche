/*
 * write.c - writer
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
 *  $Id: write.c,v 1.14 2001-04-22 07:34:59 shiro Exp $
 */

#include <stdio.h>
#include <ctype.h>
#include "gauche.h"

static void write_object(ScmObj obj, ScmPort *out, ScmWriteContext *ctx);
static ScmObj write_object_fallback(ScmObj *args, int nargs, ScmGeneric *gf);
SCM_DEFINE_GENERIC(Scm_GenericWriteObject, write_object_fallback, NULL);

/*============================================================
 * Writers
 */

/* TODO: merge normal write and circular write natually, without
   sacrificing the performance of normal write. */

/* character name table (first 33 chars of ASCII)*/
static const char *char_names[] = {
    "null",   "x01",   "x02",    "x03",   "x04",   "x05",   "x06",   "x07",
    "x08",    "tab",   "newline","x0b",   "x0c",   "return","x0e",   "x0f",
    "x10",    "x11",   "x12",    "x13",   "x14",   "x15",   "x16",   "x17",
    "x18",    "x19",   "x1a",    "escape","x1c",   "x1d",   "x1e",   "x1f",
    "space"
};

#define CASE_ITAG(obj, str) \
    case SCM_ITAG(obj): SCM_PUTCSTR(str, out); break;

#define SPBUFSIZ   50

/* Two bitmask used internally in write_internal to indicate extra
   write mode */
#define WRITE_LIMITED   0x10    /* we're limiting the length of output.
                                 */
#define WRITE_CIRCULAR  0x20    /* circular-safe write.  info->table
                                   is set up to look up for circular
                                   objects.
                                 */

static inline int outlen(ScmPort *out)
{
    SCM_ASSERT(SCM_PORT_TYPE(out) == SCM_PORT_OSTR);
    if (out->src.ostr.length < 0) {
        return (int)(out->src.ostr.current - out->src.ostr.start);
    } else {
        return out->src.ostr.length;
    }
}


/* Common routine. */
static void write_internal(ScmObj obj, ScmPort *out, ScmWriteContext *ctx)
{
    if (ctx->flags & WRITE_LIMITED) {
        if (outlen(out) >= ctx->limit) return;
    }

    if (!SCM_PTRP(obj)) {
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
            SCM_PUTCSTR(buf, out);
        }
        else if (SCM_CHARP(obj)) {
            ScmChar ch = SCM_CHAR_VALUE(obj);
            if (SCM_WRITE_MODE(ctx) == SCM_WRITE_DISPLAY) {
                SCM_PUTC(ch, out);
            } else {
                SCM_PUTCSTR("#\\", out);
                if (ch <= 0x20)       SCM_PUTCSTR(char_names[ch], out);
                else if (ch == 0x7f)  SCM_PUTCSTR("del", out);
                else                  SCM_PUTC(ch, out);
            }
        }
        else if (SCM_VM_INSNP(obj)) {
            Scm__VMInsnWrite(obj, out, ctx);
        }
        else Scm_Panic("write: got a bogus object: %08x", SCM_WORD(obj));
    } else {
        if (SCM_PAIRP(obj)) {
            ScmObj p;
            SCM_PUTC('(', out);
            write_internal(SCM_CAR(obj), out, ctx);
            SCM_FOR_EACH(p, SCM_CDR(obj)) {
                SCM_PUTC(' ', out);
                write_internal(SCM_CAR(p), out, ctx);
            }
            if (!SCM_NULLP(p)) {
                SCM_PUTCSTR(" . ", out);
                write_internal(p, out, ctx);
            }
            SCM_PUTC(')', out);
        } else {
            ScmClass *c = Scm_ClassOf(obj);
            if (c->print) c->print(obj, out, ctx);
            else          write_object(obj, out, ctx);
        }
    }
}

/*
 * Scm_Write - Standard Write.  Returns # of written characters.
 */
void Scm_Write(ScmObj obj, ScmObj port, int mode)
{
    ScmWriteContext ctx;
    if (!SCM_OPORTP(port))
        Scm_Error("output port required, but got %S", port);
    ctx.mode = mode;
    ctx.flags = 0;
    write_internal(obj, SCM_PORT(port), &ctx);
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
    ScmObj out;
    int nc;
    
    if (!SCM_OPORTP(port))
        Scm_Error("output port required, but got %S", port);
    out = Scm_MakeOutputStringPort();
    ctx.mode = mode;
    ctx.flags = WRITE_LIMITED;
    ctx.limit = width;
    write_internal(obj, SCM_PORT(out), &ctx);
    nc = outlen(SCM_PORT(out));
    if (nc > width) {
        ScmObj sub = Scm_Substring(SCM_STRING(Scm_GetOutputString(SCM_PORT(out))),
                                   0, width);
        SCM_PUTS(sub, port);
        return -1;
    } else {
        SCM_PUTS(Scm_GetOutputString(SCM_PORT(out)), port);
        return nc;
    }
}

/*
 * Scm_WriteCircular - circular-safe writer
 */

/* The first pass of write*.  */
static void write_scan(ScmObj obj, ScmWriteContext *ctx)
{
    ScmHashEntry *e;

    for (;;) {
        if (!SCM_PTRP(obj)) return;
            
        if (SCM_PAIRP(obj)) {
            e = Scm_HashTableGet(ctx->table, obj);
            if (e) { e->value = SCM_TRUE; return; }
            Scm_HashTablePut(ctx->table, obj, SCM_FALSE);

            write_scan(SCM_CAR(obj), ctx);
            obj = SCM_CDR(obj);
            continue;
        }
        if (SCM_VECTORP(obj)) {
            int i, len = SCM_VECTOR_SIZE(obj);

            e = Scm_HashTableGet(ctx->table, obj);
            if (e) { e->value = SCM_TRUE; return; }
            Scm_HashTablePut(ctx->table, obj, SCM_FALSE);

            for (i=0; i<len; i++) {
                write_scan(SCM_VECTOR_ELEMENT(obj, i), ctx);
            }
            return;
        }
        /* TODO: need to call class-specific print method for scan. */
        return;
    }
}

/* pass 2 of circular list writer. */
static void write_circular(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);

static void write_circular_list(ScmObj obj, ScmPort *port,
                                ScmWriteContext *ctx)
{
    ScmHashEntry *e;
    for (;;) {
        write_circular(SCM_CAR(obj), port, ctx);

        obj = SCM_CDR(obj);
        if (SCM_NULLP(obj)) { SCM_PUTC(')', port); return; }
        if (!SCM_PAIRP(obj)) {
            SCM_PUTCSTR(" . ", port);
            write_circular(obj, port, ctx);
            SCM_PUTC(')', port);
            return;
        }
        e = Scm_HashTableGet(ctx->table, obj);
        if (e && e->value != SCM_FALSE) {
            SCM_PUTCSTR(" . ", port);
            write_circular(obj, port, ctx);
            SCM_PUTC(')', port);
            return;
        }
        SCM_PUTC(' ', port);
    }
}

static void write_circular_vector(ScmObj obj, ScmPort *port,
                                  ScmWriteContext *ctx)
{
    int len = SCM_VECTOR_SIZE(obj), i;
    ScmObj *elts = SCM_VECTOR_ELEMENTS(obj);
    for (i=0; i<len-1; i++) {
        write_circular(elts[i], port, ctx);
        SCM_PUTC(' ', port);
    }
    write_circular(elts[i], port, ctx);
    SCM_PUTC(')', port);
}

static void write_circular(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmHashEntry *e;
    if (!SCM_PTRP(obj)) {
        write_internal(obj, port, ctx);
        return;
    }

    if (!SCM_PAIRP(obj) && !SCM_VECTORP(obj)) {
        write_internal(obj, port, ctx);
        return;
    }
        
    e = Scm_HashTableGet(ctx->table, obj);
    if (e && e->value != SCM_FALSE) {
        char numbuf[50];
        if (SCM_INTP(e->value)) {
            /* This object is already printed. */
            snprintf(numbuf, 50, "#%ld#", SCM_INT_VALUE(e->value));
            SCM_PUTCSTR(numbuf, port);
            return;
        } else {
            /* This object will be seen again.
               Put a reference tag. */
            char numbuf[50];
            snprintf(numbuf, 50, "#%d=", ctx->ncirc);
            e->value = SCM_MAKE_INT(ctx->ncirc);
            ctx->ncirc++;
            SCM_PUTCSTR(numbuf, port);
        }
    }

    if (SCM_PAIRP(obj)) {
        SCM_PUTC('(', port);
        write_circular_list(obj, port, ctx);
    } else if (SCM_VECTORP(obj)) {
        SCM_PUTCSTR("#(", port);
        write_circular_vector(obj, port, ctx);
    }
}

int Scm_WriteCircular(ScmObj obj, ScmPort *port, int mode, int width)
{
    ScmWriteContext ctx;
    int nc;

    if (!SCM_OPORTP(port))
        Scm_Error("output port required, but got %S", port);
    ctx.mode = mode;
    ctx.flags = WRITE_CIRCULAR;
    if (width > 0) {
        ctx.flags |= WRITE_LIMITED;
        ctx.limit = width;
    }
    ctx.ncirc = 0;
    ctx.table = SCM_HASHTABLE(Scm_MakeHashTable(SCM_HASH_ADDRESS, NULL, 8));
    write_scan(obj, &ctx);

    if (width > 0) {
        ScmObj out = Scm_MakeOutputStringPort();
        write_circular(obj, SCM_PORT(out), &ctx);
        nc = outlen(SCM_PORT(out));
        if (nc > width) {
            ScmObj sub = Scm_Substring(SCM_STRING(Scm_GetOutputString(SCM_PORT(out))),
                                       0, width);
            SCM_PUTS(sub, port);
            return -1;
        } else {
            SCM_PUTS(Scm_GetOutputString(SCM_PORT(out)), port);
            return nc;
        }
    } else {
        write_circular(obj, port, &ctx);
    }
    return 0;
}

/* Default object printer delegates print action to generic function
   write-object.   We can't use VMApply here since this function can be
   called deep in the recursive stack of Scm_Write, so the control
   may not return to VM immediately. */
static void write_object(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    Scm_Apply(SCM_OBJ(&Scm_GenericWriteObject), SCM_LIST2(obj, SCM_OBJ(port)));
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
    Scm_Printf(SCM_PORT(args[1]), "#<%A %p>", klass->name, args[0]);
    return SCM_TRUE;
}

/*===================================================================
 * Formatters
 */

/* Very simple formatter, for now.
 * TODO: provide option to compile format string.
 */
#define NEXT_ARG(arg, args)                                             \
    do {                                                                \
        if (!SCM_PAIRP(args))                                           \
            Scm_Error("too few arguments for format string: %S", fmt);  \
        arg = SCM_CAR(args);                                            \
        args = SCM_CDR(args);                                           \
    } while (0)

ScmObj Scm_Format(ScmObj out, ScmString *fmt, ScmObj args)
{
    ScmObj fmtstr = Scm_MakeInputStringPort(fmt);
    ScmChar ch = 0;
    ScmObj arg;
    int out_to_str = 0;

    if (out == SCM_FALSE) {
        out = Scm_MakeOutputStringPort();
        out_to_str = 1;
    } else if (out == SCM_TRUE) {
        out = SCM_OBJ(SCM_VM_CURRENT_OUTPUT_PORT(Scm_VM()));
    } else if (!SCM_OPORTP(out)) {
        Scm_Error("output port required, but got %S", out);
    }
    
    for (;;) {
        SCM_GETC(ch, fmtstr);
        if (ch == EOF) {
            if (!SCM_NULLP(args))
                Scm_Error("too many arguments for format string: %S", fmt);
            if (out_to_str) {
                return Scm_GetOutputString(SCM_PORT(out));
            } else {
                return SCM_UNDEFINED;
            }
        }

        if (ch != '~') {
            SCM_PUTC(ch, out);
            continue;
        }

        SCM_GETC(ch, fmtstr);
        switch (ch) {
        case '%':
            SCM_PUTC('\n', out);
            continue;
        case 's':; case 'S':;
            NEXT_ARG(arg, args);
            Scm_Write(arg, out, SCM_WRITE_WRITE);
            continue;
        case 'a':; case 'A':;
            NEXT_ARG(arg, args);
            Scm_Write(arg, out, SCM_WRITE_DISPLAY);
            continue;
        default:
            SCM_PUTC(ch, out);
            continue;
        }
    }
}

/* C version of format for convenience */
ScmObj Scm_Cformat(ScmObj port, const char *fmt, ...)
{
    va_list ap;
    ScmString *fmtstr = SCM_STRING(SCM_MAKE_STR(fmt));
    ScmObj fmtport = Scm_MakeInputStringPort(fmtstr);
    int nargs = 0;
    ScmChar ch = 0;
    ScmObj start = SCM_NIL, end = SCM_NIL;

    /* Count # of args */
    for (;;) {
        SCM_GETC(ch, fmtport);
        if (ch == EOF) break;
        if (ch != '~') continue;

        SCM_GETC(ch, fmtport);
        switch (ch) {
        case 'a':; case 'A':; case 's':; case 'S':
            nargs++;
            break;
        }
    }

    va_start(ap, fmt);
    while (nargs--) {
        ScmObj o = va_arg(ap, ScmObj);
        SCM_APPEND1(start, end, o);
    }
    va_end(ap);
    return Scm_Format(port, fmtstr, start);
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

/* Known bugs:
 *  - If fmt string contains multi-byte chars, the result count may
 *    not be correct.
 */

void Scm_Vprintf(ScmPort *out, const char *fmt, va_list ap)
{
    const char *fmtp = fmt;
    ScmDString argbuf;
    char buf[SPBUFSIZ];
    int c, longp = 0;

    if (!SCM_OPORTP(out)) {
        Scm_Error("output port required, but got %S", out);
    }
    
    while ((c = *fmtp++) != 0) {
        int width, prec, dot_appeared, pound_appeared;

        if (c != '%') {
            SCM_PUTC(c, out);
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
                    signed int val = va_arg(ap, signed int);

                    SCM_DSTRING_PUTB(&argbuf, c);
                    SCM_DSTRING_PUTB(&argbuf, 0);
                    snprintf(buf, SPBUFSIZ, Scm_DStringGetCstr(&argbuf), val);
                    SCM_PUTCSTR(buf, out);
                    break;
                }
            case 'o':; case 'u':; case 'x':; case 'X':
                {
                    unsigned long val = va_arg(ap, unsigned long);
                    SCM_DSTRING_PUTB(&argbuf, c);
                    SCM_DSTRING_PUTB(&argbuf, 0);
                    snprintf(buf, SPBUFSIZ, Scm_DStringGetCstr(&argbuf), val);
                    SCM_PUTCSTR(buf, out);
                    break;
                }
            case 'e':; case 'E':; case 'f':; case 'g':; case 'G':
                {
                    double val = va_arg(ap, double);
                    SCM_DSTRING_PUTB(&argbuf, c);
                    SCM_DSTRING_PUTB(&argbuf, 0);
                    snprintf(buf, SPBUFSIZ, Scm_DStringGetCstr(&argbuf), val);
                    SCM_PUTCSTR(buf, out);
                    break;
                }
            case 's':;
                {
                    char *val = va_arg(ap, char *);
                    int len;
                    SCM_PUTCSTR(val, out);

                    /* TODO: support right adjustment such as %-10s.
                       Currently we ignore minus sign and pad chars
                       on the right. */
                    for (len = strlen(val); len < width; len++) {
                        SCM_PUTC(' ', out);
                    }
                    break;
                }
            case '%':;
                {
                    SCM_PUTC('%', out);
                    break;
                }
            case 'p':
                {
                    void *val = va_arg(ap, void *);
                    SCM_DSTRING_PUTB(&argbuf, c);
                    SCM_DSTRING_PUTB(&argbuf, 0);
                    snprintf(buf, SPBUFSIZ, Scm_DStringGetCstr(&argbuf), val);
                    SCM_PUTCSTR(buf, out);
                    break;
                }
            case 'S':; case 'A':
                {
                    ScmObj o = va_arg(ap, ScmObj);
                    int mode =
                        (c == 'A')? SCM_WRITE_DISPLAY : SCM_WRITE_WRITE;
                    ScmWriteContext ctx;
                    ctx.mode = mode;
                    ctx.flags = 0;

                    if (pound_appeared) {
                        int n = Scm_WriteCircular(o, out, mode, width);
                        if (n < 0 && prec > 0) {
                            SCM_PUTCSTR(" ...", out);
                        }
                        if (n > 0) {
                            for (; n < width; n++) SCM_PUTC(' ', out);
                        }
                    } else if (width == 0) {
                        write_internal(o, out, &ctx);
                    } else if (dot_appeared) {
                        int n = Scm_WriteLimited(o, SCM_OBJ(out), mode, width);
                        if (n < 0 && prec > 0) {
                            SCM_PUTCSTR(" ...", out);
                        }
                        if (n > 0) {
                            for (; n < width; n++) SCM_PUTC(' ', out);
                        }
                    } else {
                        write_internal(o, out, &ctx);
                    }
                    break;
                }
            case 'C':
                {
                    int c = va_arg(ap, int);
                    SCM_PUTC(c, out);
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
            fallback:
            default:
                SCM_DSTRING_PUTB(&argbuf, c);
                continue;
            }
            break;
        }
        if (c == 0)
            Scm_Error("incomplete %-directive in format string: %s", fmt);
    }
}

void Scm_Printf(ScmPort *out, const char *fmt, ...)
{
    va_list ap;

    va_start(ap, fmt);
    Scm_Vprintf(out, fmt, ap);
    va_end(ap);
}

/*
 * Initialization
 */
void Scm__InitWrite(void)
{
    Scm_InitBuiltinGeneric(&Scm_GenericWriteObject, "write-object",
                           Scm_GaucheModule());
}
