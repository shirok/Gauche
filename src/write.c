/*
 * write.c - writer
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
 *  $Id: write.c,v 1.4 2001-01-15 01:28:28 shiro Exp $
 */

#include <stdio.h>
#include <ctype.h>
#include "gauche.h"

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

#define CASE_ITAG(obj, str, nchars) \
    case SCM_ITAG(obj): SCM_PUTCSTR(str, out); nc = nchars; break;

#define SPBUFSIZ   50

/* Two bitmask used internally in write_internal to indicate extra
   write mode */
#define WRITE_LIMITED   0x10    /* we're limiting the length of output.
                                 */
#define WRITE_CIRCULAR  0x20    /* circular-safe write.  info->table
                                   is set up to look up for circular
                                   objects.
                                 */

/* Extra context for fancier operation.  Valid if one of the above
   flags are on. */
struct ScmWriteInfoRec {
    int ncirc;                  /* # of circular references */
    ScmHashTable *table;        /* reference table */
};

/* Common routine. */
static int write_internal(ScmObj obj, ScmPort *out, int mode,
                          int room, ScmWriteInfo *info)
{
    int nc = 0;

    if (mode & WRITE_LIMITED && room < 0) return 0;

    if (!SCM_PTRP(obj)) {
        if (SCM_IMMEDIATEP(obj)) {
            switch (SCM_ITAG(obj)) {
                CASE_ITAG(SCM_FALSE,     "#f", 2);
                CASE_ITAG(SCM_TRUE,      "#t", 2);
                CASE_ITAG(SCM_NIL,       "()", 2);
                CASE_ITAG(SCM_EOF,       "#<eof>", 6);
                CASE_ITAG(SCM_UNDEFINED, "#<undef>", 8);
                CASE_ITAG(SCM_UNBOUND,   "#<unbound>", 10);
            default:
                Scm_Panic("write: unknown itag object: %08x", SCM_WORD(obj));
            }
        }
        else if (SCM_INTP(obj)) {
            char buf[SPBUFSIZ];
            nc = snprintf(buf, SPBUFSIZ, "%d", SCM_INT_VALUE(obj));
            SCM_PUTCSTR(buf, out);
        }
        else if (SCM_CHARP(obj)) {
            ScmChar ch = SCM_CHAR_VALUE(obj);
            SCM_PUTCSTR("#\\", out);  nc += 2;
            if (ch <= 0x20) {
                SCM_PUTCSTR(char_names[ch], out);
                nc += strlen(char_names[ch]);
            } else if (ch == 0x7f) {
                SCM_PUTCSTR("del", out);
                nc += 3;
            } else {
                SCM_PUTC(ch, out);
                nc++;
            }
        }
        else if (SCM_VM_INSNP(obj)) {
            nc = Scm__VMInsnWrite(obj, out, mode);
        }
        else Scm_Panic("write: got a bogus object: %08x", SCM_WORD(obj));
    } else {
        if (SCM_PAIRP(obj)) {
            ScmObj p;
            SCM_PUTC('(', out); nc++;
            nc += write_internal(SCM_CAR(obj), out, mode, room-nc, info);
            SCM_FOR_EACH(p, SCM_CDR(obj)) {
                SCM_PUTC(' ', out); nc++;
                nc += write_internal(SCM_CAR(p), out, mode, room-nc, info);
            }
            if (!SCM_NULLP(p)) {
                SCM_PUTCSTR(" . ", out); nc += 3;
                nc += write_internal(p, out, mode, room-nc, info);
            }
            SCM_PUTC(')', out); nc++;
        } else {
            nc = Scm_ClassOf(obj)->print(obj, out, mode);
        }
    }
    return nc;
}

/*
 * Scm_Write - Standard Write.  Returns # of written characters.
 */
int Scm_Write(ScmObj obj, ScmObj port, int mode)
{
    if (!SCM_OPORTP(port))
        Scm_Error("output port required, but got %S", port);
    return write_internal(obj, SCM_PORT(port), mode, 0, NULL);
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
    ScmObj out = Scm_MakeOutputStringPort();
    int nc;
    if (!SCM_OPORTP(port))
        Scm_Error("output port required, but got %S", port);
    nc = write_internal(obj, SCM_PORT(out), mode, width, NULL);
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
static void write_scan(ScmObj obj, ScmWriteInfo *info)
{
    ScmHashEntry *e;

    for (;;) {
        if (!SCM_PTRP(obj)) return;
            
        if (SCM_PAIRP(obj)) {
            e = Scm_HashTableGet(info->table, obj);
            if (e) { e->value = SCM_TRUE; return; }
            Scm_HashTablePut(info->table, obj, SCM_FALSE);

            write_scan(SCM_CAR(obj), info);
            obj = SCM_CDR(obj);
            continue;
        }
        if (SCM_VECTORP(obj)) {
            int i, len = SCM_VECTOR_SIZE(obj);

            e = Scm_HashTableGet(info->table, obj);
            if (e) { e->value = SCM_TRUE; return; }
            Scm_HashTablePut(info->table, obj, SCM_FALSE);

            for (i=0; i<len; i++) {
                write_scan(SCM_VECTOR_ELEMENT(obj, i), info);
            }
            return;
        }
        /* TODO: need to call class-specific print method for scan. */
        return;
    }
}

/* pass 2 of circular list writer. */
static int write_circular(ScmObj obj, ScmPort *port, int mode,
                          int room, ScmWriteInfo *info);

static int write_circular_list(ScmObj obj, ScmPort *port, int mode,
                               int room, ScmWriteInfo *info)
{
    int nc = 0;
    ScmHashEntry *e;
    for (;;) {
        nc += write_circular(SCM_CAR(obj), port, mode, room-nc, info);

        obj = SCM_CDR(obj);
        if (SCM_NULLP(obj)) { SCM_PUTC(')', port); return ++nc; }
        if (!SCM_PAIRP(obj)) {
            SCM_PUTCSTR(" . ", port); nc+=3;
            nc += write_circular(obj, port, mode, room-nc, info);
            SCM_PUTC(')', port); nc++;
            return nc;
        }
        e = Scm_HashTableGet(info->table, obj);
        if (e && e->value != SCM_FALSE) {
            SCM_PUTCSTR(" . ", port); nc+=3;
            nc += write_circular(obj, port, mode, room-nc, info);
            SCM_PUTC(')', port); nc++;
            return nc;
        }
        SCM_PUTC(' ', port); nc++;
    }
}

static int write_circular_vector(ScmObj obj, ScmPort *port, int mode,
                                 int room, ScmWriteInfo *info)
{
    int nc = 0, len = SCM_VECTOR_SIZE(obj), i;
    ScmObj *elts = SCM_VECTOR_ELEMENTS(obj);
    for (i=0; i<len-1; i++) {
        nc += write_circular(elts[i], port, mode, room-nc, info);
        SCM_PUTC(' ', port); nc++;
    }
    nc += write_circular(elts[i], port, mode, room-nc, info);
    SCM_PUTC(')', port); nc++;
    return nc;
}

static int write_circular(ScmObj obj, ScmPort *port, int mode,
                          int room, ScmWriteInfo *info)
{
    ScmHashEntry *e;
    int nc = 0;
    if (!SCM_PTRP(obj))
        return write_internal(obj, port, mode, room, info);

    if (!SCM_PAIRP(obj) && !SCM_VECTORP(obj))
        return write_internal(obj, port, mode, room, info);
        
    e = Scm_HashTableGet(info->table, obj);
    if (e && e->value != SCM_FALSE) {
        char numbuf[50];
        if (SCM_INTP(e->value)) {
            /* This object is already printed. */
            nc += snprintf(numbuf, 50, "#%d#", SCM_INT_VALUE(e->value));
            SCM_PUTCSTR(numbuf, port);
            return nc;
        } else {
            /* This object will be seen again.
               Put a reference tag. */
            char numbuf[50];
            nc += snprintf(numbuf, 50, "#%d=", info->ncirc);
            e->value = SCM_MAKE_INT(info->ncirc);
            info->ncirc++;
            SCM_PUTCSTR(numbuf, port);
        }
    }

    if (SCM_PAIRP(obj)) {
        SCM_PUTC('(', port); nc++;
        nc += write_circular_list(obj, port, mode, room-nc, info);
    } else if (SCM_VECTORP(obj)) {
        SCM_PUTCSTR("#(", port); nc+=2;
        nc += write_circular_vector(obj, port, mode, room-nc, info);
    }
    return nc;
}

int Scm_WriteCircular(ScmObj obj, ScmPort *port, int mode)
{
    ScmWriteInfo info;

    if (!SCM_OPORTP(port))
        Scm_Error("output port required, but got %S", port);
    info.ncirc = 0;
    info.table = SCM_HASHTABLE(Scm_MakeHashTable(SCM_HASH_ADDRESS, NULL, 8));
    write_scan(obj, &info);
    return write_circular(obj, port, mode|WRITE_CIRCULAR, 0, &info);
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
    ScmChar ch;
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
            Scm_Write(arg, out, SCM_PRINT_WRITE);
            continue;
        case 'a':; case 'A':;
            NEXT_ARG(arg, args);
            Scm_Write(arg, out, SCM_PRINT_DISPLAY);
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
    int nargs;
    ScmChar ch;
    ScmObj start = SCM_NIL, end;

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
        SCM_GROW_LIST(start, end, o);
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

int Scm_Vprintf(ScmPort *out, const char *fmt, va_list ap)
{
    const char *fmtp = fmt;
    ScmDString argbuf;
    char buf[SPBUFSIZ];
    int c, longp = 0, nc = 0;

    if (!SCM_OPORTP(out)) {
        Scm_Error("output port required, but got %S", out);
    }
    
    while ((c = *fmtp++) != 0) {
        int width, prec, dot_appeared;

        if (c != '%') {
            SCM_PUTC(c, out); nc++;
            continue;
        }

        Scm_DStringInit(&argbuf);
        SCM_DSTRING_PUTB(&argbuf, c);
        width = 0, prec = 0, dot_appeared = 0;
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
                    nc += snprintf(buf, SPBUFSIZ,
                                   Scm_DStringGetCstr(&argbuf), val);
                    SCM_PUTCSTR(buf, out);
                    break;
                }
            case 'o':; case 'u':; case 'x':; case 'X':
                {
                    unsigned long val = va_arg(ap, unsigned long);
                    SCM_DSTRING_PUTB(&argbuf, c);
                    SCM_DSTRING_PUTB(&argbuf, 0);
                    nc += snprintf(buf, SPBUFSIZ,
                                   Scm_DStringGetCstr(&argbuf), val);
                    SCM_PUTCSTR(buf, out);
                    break;
                }
            case 'e':; case 'E':; case 'f':; case 'g':; case 'G':
                {
                    double val = va_arg(ap, double);
                    SCM_DSTRING_PUTB(&argbuf, c);
                    SCM_DSTRING_PUTB(&argbuf, 0);
                    nc += snprintf(buf, SPBUFSIZ,
                                   Scm_DStringGetCstr(&argbuf), val);
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
                    nc += len;
                    break;
                }
            case '%':;
                {
                    SCM_PUTC('%', out); nc++;
                    break;
                }
            case 'p':
                {
                    void *val = va_arg(ap, void *);
                    SCM_DSTRING_PUTB(&argbuf, c);
                    SCM_DSTRING_PUTB(&argbuf, 0);
                    nc += snprintf(buf, SPBUFSIZ,
                                   Scm_DStringGetCstr(&argbuf), val);
                    SCM_PUTCSTR(buf, out);
                    break;
                }
            case 'S':; case 'A':
                {
                    ScmObj o = va_arg(ap, ScmObj);
                    int mode =
                        (c == 'A')? SCM_PRINT_DISPLAY : SCM_PRINT_WRITE;

                    if (width == 0) {
                        nc += write_internal(o, out, mode, 0, NULL);
                    } else if (dot_appeared) {
                        int n = Scm_WriteLimited(o, SCM_OBJ(out),
                                                 mode, width);
                        if (n < 0 && prec > 0) {
                            SCM_PUTCSTR(" ...", out);
                            nc += 4;
                        }
                        if (n > 0) {
                            for (; n < width; n++) SCM_PUTC(' ', out);
                        }
                        nc += width;
                    } else {
                        int n = write_internal(o, out, mode, 0, NULL);
                        if (n < width) {
                            for (; n < width; n++) SCM_PUTC(' ', out);
                        }
                        nc += width;
                    }
                    break;
                }
            case 'C':
                {
                    int c = va_arg(ap, int);
                    SCM_PUTC(c, out); nc++;
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

    return nc;
}

int Scm_Printf(ScmPort *out, const char *fmt, ...)
{
    int r;
    va_list ap;

    va_start(ap, fmt);
    r = Scm_Vprintf(out, fmt, ap);
    va_end(ap);
    return r;
}

