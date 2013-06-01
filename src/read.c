/*
 * read.c - reader
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

#include <stdio.h>
#include <ctype.h>
#include <math.h>
#define LIBGAUCHE_BODY
#include "gauche.h"
#include "gauche/vm.h"
#include "gauche/port.h"
#include "gauche/builtin-syms.h"

/*
 * READ
 */

static void   read_context_flush(ScmReadContext *ctx);
static ScmObj read_internal(ScmPort *port, ScmReadContext *ctx);
static ScmObj read_item(ScmPort *port, ScmReadContext *ctx);
static ScmObj read_list(ScmPort *port, ScmChar closer, ScmReadContext *ctx);
static ScmObj read_vector(ScmPort *port, ScmChar closer, ScmReadContext *ctx);
static ScmObj read_string(ScmPort *port, int incompletep, ScmReadContext *ctx);
static ScmObj read_quoted(ScmPort *port, ScmObj quoter, ScmReadContext *ctx);
static ScmObj read_char(ScmPort *port, ScmReadContext *ctx);
static ScmObj read_word(ScmPort *port, ScmChar initial, ScmReadContext *ctx,
                        int temp_case_fold, int include_hash_sign);
static ScmObj read_symbol(ScmPort *port, ScmChar initial, ScmReadContext *ctx);
static ScmObj read_number(ScmPort *port, ScmChar initial, ScmReadContext *ctx);
static ScmObj read_symbol_or_number(ScmPort *port, ScmChar initial, ScmReadContext *ctx);
static ScmObj read_escaped_symbol(ScmPort *port, ScmChar delim, int interned);
static ScmObj read_keyword(ScmPort *port, ScmReadContext *ctx);
static ScmObj read_regexp(ScmPort *port);
static ScmObj read_charset(ScmPort *port);
static ScmObj read_sharp_comma(ScmPort *port, ScmReadContext *ctx);
static ScmObj process_sharp_comma(ScmPort *port, ScmObj key, ScmObj args,
                                  ScmReadContext *ctx, int has_ref);
static ScmObj read_shebang(ScmPort *port, ScmReadContext *ctx);
static ScmObj read_reference(ScmPort *port, ScmChar ch, ScmReadContext *ctx);
static ScmObj read_sharp_word(ScmPort *port, char c, ScmReadContext *ctx);

/* Special hook for SRFI-4 syntax */
static ScmObj (*read_uvector_hook)(ScmPort *, const char*, ScmReadContext *) = NULL;

/* Table of 'read-time constructor' in SRFI-10 */
static struct {
    ScmHashTable *table;
    ScmInternalMutex mutex;
} readCtorData = { NULL };

/* Table of hash-bang directive */
static struct {
    ScmHashTable *table;
    ScmInternalMutex mutex;
} hashBangData = { NULL };

/*----------------------------------------------------------------
 * Entry points
 *   Note: Entire read operation are done while locking the input port.
 *   So we can use 'unsafe' version of port operations inside this file.
 *   The lock is removed if reader routine signals an error.  It is OK
 *   to call read routine recursively.
 */
ScmObj Scm_ReadWithContext(ScmObj port, ScmReadContext *ctx)
{
    ScmVM *vm = Scm_VM();
    volatile ScmObj r = SCM_NIL;
    if (!SCM_PORTP(port) || SCM_PORT_DIR(port) != SCM_PORT_INPUT) {
        Scm_Error("input port required: %S", port);
    }
    if (!(ctx->flags & SCM_READ_RECURSIVELY)) {
        ctx->table = NULL;
        ctx->pending = SCM_NIL;
    }
    if (PORT_LOCKED(SCM_PORT(port), vm)) {
        r = read_item(SCM_PORT(port), ctx);
    } else {
        PORT_LOCK(SCM_PORT(port), vm);
        PORT_SAFE_CALL(SCM_PORT(port), r = read_item(SCM_PORT(port), ctx));
        PORT_UNLOCK(SCM_PORT(port));
    }
    if (!(ctx->flags & SCM_READ_RECURSIVELY)) {
        read_context_flush(ctx);
    }
    return r;
}

ScmObj Scm_Read(ScmObj port)
{
    return Scm_ReadWithContext(port, Scm_MakeReadContext(NULL));
}

/* Convenience functions */
ScmObj Scm_ReadFromString(ScmString *str)
{
    ScmObj inp = Scm_MakeInputStringPort(str, TRUE), r;
    ScmReadContext *ctx = Scm_MakeReadContext(NULL);
    r = read_item(SCM_PORT(inp), ctx);
    read_context_flush(ctx);
    return r;
}

ScmObj Scm_ReadFromCString(const char *cstr)
{
    ScmObj s = SCM_MAKE_STR_IMMUTABLE(cstr);
    ScmObj inp = Scm_MakeInputStringPort(SCM_STRING(s), TRUE);
    ScmObj r;
    ScmReadContext *ctx = Scm_MakeReadContext(NULL);
    r = read_item(SCM_PORT(inp), ctx);
    read_context_flush(ctx);
    return r;
}

ScmObj Scm_ReadListWithContext(ScmObj port, ScmChar closer, ScmReadContext *ctx)
{
    ScmVM *vm = Scm_VM();
    volatile ScmObj r = SCM_NIL;
    if (!SCM_PORTP(port) || SCM_PORT_DIR(port) != SCM_PORT_INPUT) {
        Scm_Error("input port required: %S", port);
    }
    if (!(ctx->flags & SCM_READ_RECURSIVELY)) {
        ctx->table = NULL;
        ctx->pending = SCM_NIL;
    }
    if (PORT_LOCKED(SCM_PORT(port), vm)) {
        r = read_list(SCM_PORT(port), closer, ctx);
    } else {
        PORT_LOCK(SCM_PORT(port), vm);
        PORT_SAFE_CALL(SCM_PORT(port), r = read_list(SCM_PORT(port), closer, ctx));
        PORT_UNLOCK(SCM_PORT(port));
    }
    if (!(ctx->flags & SCM_READ_RECURSIVELY)) {
        read_context_flush(ctx);
    }
    return r;
}

ScmObj Scm_ReadList(ScmObj port, ScmChar closer)
{
    ScmReadContext *ctx = Scm_MakeReadContext(NULL);
    return Scm_ReadListWithContext(port, closer, ctx);
}

/*----------------------------------------------------------------
 * Read context
 */

ScmReadContext *Scm_MakeReadContext(ScmReadContext *proto)
{
    ScmReadContext *ctx = SCM_NEW(ScmReadContext);
    SCM_SET_CLASS(ctx, SCM_CLASS_READ_CONTEXT);
    if (proto == NULL) {
        ctx->flags = SCM_READ_SOURCE_INFO;
        ctx->table = NULL;
        ctx->pending = SCM_NIL;
    } else {
        /* TODO: we haven't thought well of the case when prototype
           is given.  Should we copy the #n= table as well?  How should
           we handle pending read references? */
        ctx->flags = proto->flags;
        ctx->table = NULL;
        ctx->pending = SCM_NIL;
    }
    return ctx;
}

static void read_context_print(ScmObj obj, ScmPort *port,
                               ScmWriteContext *ctx)
{
    Scm_Printf(port, "#<read-context %p>", obj);
}

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_ReadContextClass, read_context_print);

/*----------------------------------------------------------------
 * Error
 */

void Scm_ReadError(ScmPort *port, const char *msg, ...)
{
    ScmObj ostr = Scm_MakeOutputStringPort(TRUE);
    ScmObj name = Scm_PortName(port);
    ScmObj rerr;
    int line = Scm_PortLine(port);
    va_list ap;

    Scm_Printf(SCM_PORT(ostr), "Read error at %S:",
               SCM_STRINGP(name)? name : SCM_OBJ(SCM_MAKE_STR("??")));
    if (line >= 0) {
        Scm_Printf(SCM_PORT(ostr), "line %d: ", line);
    }
    va_start(ap, msg);
    Scm_Vprintf(SCM_PORT(ostr), msg, ap, TRUE);
    va_end(ap);

    rerr = Scm_MakeReadError(Scm_GetOutputString(SCM_PORT(ostr), 0),
                             port, line);
    Scm_Raise(rerr);
}

/*----------------------------------------------------------------
 * Read reference
 */

/* Read reference is a proxy object to for referenced object (#N=).
 */

static void read_reference_print(ScmObj obj, ScmPort *port,
                                 ScmWriteContext *ctx);
SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_ReadReferenceClass, read_reference_print);

ScmObj Scm_MakeReadReference(void)
{
    ScmReadReference *a;
    a = SCM_NEW(ScmReadReference);
    SCM_SET_CLASS(a, SCM_CLASS_READ_REFERENCE);
    a->value = SCM_UNBOUND;
    return SCM_OBJ(a);
}

static void read_reference_print(ScmObj obj, ScmPort *port,
                                 ScmWriteContext *ctx)
{
    Scm_Printf(port, "#<read-reference>");
}

static void ref_push(ScmReadContext *ctx, ScmObj obj, ScmObj finisher)
{
    ctx->pending = Scm_Acons(obj, finisher, ctx->pending);
}

static ScmObj ref_val(ScmObj ref)
{
    if (!SCM_READ_REFERENCE_REALIZED(ref)) {
        Scm_Error("reader encontered unresolved read reference.  Implementation error?");
    }
    return SCM_READ_REFERENCE(ref)->value;
}

static ScmObj ref_register(ScmReadContext *ctx, ScmObj obj, int refnum)
{
    SCM_ASSERT(ctx->table);
    Scm_HashTableSet(ctx->table, SCM_MAKE_INT(refnum), obj, 0);
    return obj;
}

/* ctx->pending contains an assoc list of objects who contains read reference
   which should be resolved.
   The car of each entry is the object that needs to be fixed, and the
   cdr of eacy entry may contain a finisher procedure (if the object is
   created by read-time constructor.
*/
static void read_context_flush(ScmReadContext *ctx)
{
    ScmObj cp, ep, entry, obj, finisher;

    SCM_FOR_EACH(cp, ctx->pending) {
        entry = SCM_CAR(cp);
        SCM_ASSERT(SCM_PAIRP(entry));
        obj = SCM_CAR(entry);
        finisher = SCM_CDR(entry);

        if (!SCM_FALSEP(finisher)) {
            Scm_ApplyRec(finisher, SCM_LIST1(obj));
        } else if (SCM_PAIRP(obj)) {
            SCM_FOR_EACH(ep, obj) {
                if (SCM_READ_REFERENCE_P(SCM_CAR(ep))) {
                    SCM_SET_CAR(ep, ref_val(SCM_CAR(ep)));
                }
                if (SCM_READ_REFERENCE_P(SCM_CDR(ep))) {
                    /* in case we have (... . #N#) */
                    SCM_SET_CDR(ep, ref_val(SCM_CDR(ep)));
                    break;
                }
            }
        } else if (SCM_VECTORP(obj)) {
            int i, len = SCM_VECTOR_SIZE(obj);
            for (i=0; i<len; i++) {
                ep = SCM_VECTOR_ELEMENT(obj, i);
                if (SCM_READ_REFERENCE_P(ep)) {
                    SCM_VECTOR_ELEMENTS(obj)[i] = ref_val(ep);
                }
            }
        } else {
            Scm_Error("read_context_flush: recursive reference only supported with vector and lists");
        }
    }
}

/*----------------------------------------------------------------
 * Miscellaneous routines
 */

/* Table of initial 128 bytes of ASCII characters to dispatch for
   special meanings.
    bit 0 : a valid constituent char of words
    bit 1 : candidate of case folding
*/
static const unsigned char ctypes[] = {
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
 /*     !   "   #   $   %   &   '   (   )   *   +   ,   -   .   /  */
    0,  1,  0,  0,  1,  1,  1,  0,  0,  0,  1,  1,  0,  1,  1,  1,
 /* 0   1   2   3   4   5   6   7   8   9   :   ;   <   =   >   ?  */
    1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  0,  1,  1,  1,  1,
 /* @   A   B   C   D   E   F   G   H   I   J   K   L   M   N   O  */
    1,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,
 /* P   Q   R   S   T   U   V   W   X   Y   Z   [   \   ]   ^   _  */
    3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,  0,  0,  0,  1,  1,
 /* `   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o  */
    0,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
 /* p   q   r   s   t   u   v   w   x   y   z   {   |   }   ~   ^? */
    1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  0,  0,  0,  1,  0,
};

inline static int char_word_constituent(int c, int include_hash_sign)
{
    return (c >= 128 || (c >= 0 && (ctypes[(unsigned char)c]&1))
            || (c == '#' && include_hash_sign));
}

inline static int char_word_case_fold(int c)
{
    return (c >= 0 && c < 128 && (ctypes[(unsigned char)c]&2));
}

static void read_nested_comment(ScmPort *port, ScmReadContext *ctx)
{
    int nesting = 0;
    int line = Scm_PortLine(port);
    ScmChar c, c1;

    for (c = Scm_GetcUnsafe(port);;) {
        switch (c) {
        case '#':
            c1 = Scm_GetcUnsafe(port);
            if (c1 == '|')   { nesting++; break; }
            else if (c1 == EOF) goto eof;
            else c = c1;
            continue;
        case '|':
            c1 = Scm_GetcUnsafe(port);
            if (c1 == '#') {
                if (nesting-- == 0) {
                    return;
                }
                break;
            }
            else if (c1 == EOF) goto eof;
            else c = c1;
            continue;
        case EOF:
          eof:
            Scm_ReadError(port, "encountered EOF inside nested multi-line comment (comment begins at line %d)", line);
        default:
            break;
        }
        c = Scm_GetcUnsafe(port);
    }
}

static void read_comment(ScmPort *port) /* leading semicolon is already read */
{
    for (;;) {
        /* NB: comment may contain unexpected character code.
           for the safety, we read bytes here. */
        int c = Scm_GetbUnsafe(port);
        if (c == '\n') {
            /* oops.  ugly. */
            port->line++;
            break;
        }
        if (c == EOF) break;
    }
}

static int skipws(ScmPort *port, ScmReadContext *ctx)
{
    for (;;) {
        int c = Scm_GetcUnsafe(port);
        if (c == EOF) return c;
        if (c <= 127) {
            if (isspace(c)) continue;
            if (c == ';') {
                read_comment(port);
                continue;
            }
            return c;
        }
        else if (!SCM_CHAR_EXTRA_WHITESPACE(c)) return c;
    }
}

static ScmObj read_internal(ScmPort *port, ScmReadContext *ctx)
{
    int c = skipws(port, ctx);
    switch (c) {
    case '(':
        return read_list(port, ')', ctx);
    case '"':
        return read_string(port, FALSE, ctx);
    case '#':
        {
            int c1 = Scm_GetcUnsafe(port);
            switch (c1) {
            case EOF:
                Scm_ReadError(port, "premature #-sequence at EOF");
            case 't':; case 'T': return read_sharp_word(port, 't', ctx);
            case 'f':; case 'F': return read_sharp_word(port, 'f', ctx);
            case 's':; case 'S': return read_sharp_word(port, 's', ctx);
            case 'u':; case 'U': return read_sharp_word(port, 'u', ctx);
            case '(':
                return read_vector(port, ')', ctx);
            case '\\':
                return read_char(port, ctx);
            case 'x':; case 'X':; case 'o':; case 'O':;
            case 'b':; case 'B':; case 'd':; case 'D':;
            case 'e':; case 'E':; case 'i':; case 'I':;
                Scm_UngetcUnsafe(c1, port);
                return read_number(port, c, ctx);
            case '!':
                /* #! is either a script shebang or a reader directive */
                return read_shebang(port, ctx);
            case '/':
                /* #/.../ literal regexp */
                return read_regexp(port);
            case '[':
                /* #[...] literal charset */
                return read_charset(port);
            case ',':
                /* #,(form) - SRFI-10 read-time macro */
                return read_sharp_comma(port, ctx);
            case '|':
                /* #| - block comment (SRFI-30)
                   it is equivalent to whitespace, so we return #<undef> */
                read_nested_comment(port, ctx);
                return SCM_UNDEFINED;
            case '`':
                /* #`"..." is a special syntax of #,(string-interpolate "...") */
                {
                    ScmObj form = read_item(port, ctx);
                    return process_sharp_comma(port,
                                               SCM_SYM_STRING_INTERPOLATE,
                                               SCM_LIST1(form), ctx, FALSE);
                }
            case '?':
                /* #? - debug directives */
                {
                    int c2;
                    ScmObj form;

                    c2 = Scm_GetcUnsafe(port);
                    switch (c2) {
                    case '=':
                        /* #?=form - debug print */
                        form = read_item(port, ctx);
                        return SCM_LIST2(SCM_SYM_DEBUG_PRINT, form);
                    case EOF:
                        return SCM_EOF;
                    default:
                        Scm_ReadError(port, "unsupported #?-syntax: #?%C", c2);
                    }
                }
            case '0': case '1': case '2': case '3': case '4':
            case '5': case '6': case '7': case '8': case '9':
                /* #N# or #N= form */
                return read_reference(port, c1, ctx);
            case '*':
                /* #*"...." byte string
                   #*01001001 for bit vector, maybe in future. */
                {
                    int c2;
                    c2 = Scm_GetcUnsafe(port);
                    if (c2 == '"') return read_string(port, TRUE, ctx);
                    Scm_ReadError(port, "unsupported #*-syntax: #*%C", c2);
                }
            case ':':
                /* #:name - uninterned symbol */
                {
                    int c2 = Scm_GetcUnsafe(port);
                    if (c2 == '|') {
                        return read_escaped_symbol(port, c2, FALSE);
                    } else {
                        ScmObj name = read_word(port, c2, ctx, FALSE, FALSE);
                        return Scm_MakeSymbol(SCM_STRING(name), FALSE);
                    }
                }
            case ';':
                /* #;expr - comment out sexpr */
                {
                    int orig = ctx->flags;
                    ctx->flags |= SCM_READ_DISABLE_CTOR;
                    read_item(port, ctx); /* read and discard */
                    ctx->flags = orig;
                    return SCM_UNDEFINED; /* indicate this is a comment */
                }
            default:
                Scm_ReadError(port, "unsupported #-syntax: #%C", c1);
            }
        }
    case '\'': return read_quoted(port, SCM_SYM_QUOTE, ctx);
    case '`': return read_quoted(port, SCM_SYM_QUASIQUOTE, ctx);
    case ':':
        return read_keyword(port, ctx);
    case ',':
        {
            int c1 = Scm_GetcUnsafe(port);
            if (c1 == EOF) {
                Scm_ReadError(port, "unterminated unquote");
            } else if (c1 == '@') {
                return read_quoted(port, SCM_SYM_UNQUOTE_SPLICING, ctx);
            } else {
                Scm_UngetcUnsafe(c1, port);
                return read_quoted(port, SCM_SYM_UNQUOTE, ctx);
            }
        }
    case '|':
        return read_escaped_symbol(port, '|', TRUE);
    case '[':
        /* TODO: make it customizable */
        return read_list(port, ']', ctx);
    case '{':
        /* srfi-105 experimental support */
        {
            ScmObj r = read_list(port, '}', ctx);
            if (SCM_VM_COMPILER_FLAG_IS_SET(Scm_VM(),SCM_COMPILE_ENABLE_CEXPR)){
                static ScmObj xform_cexpr = SCM_UNDEFINED;
                SCM_BIND_PROC(xform_cexpr, "%xform-cexpr",
                              Scm_GaucheInternalModule());
                return Scm_ApplyRec1(xform_cexpr, r);
            } else {
                return r;
            }
        }
    case '+':; case '-':
        /* Note: R5RS doesn't permit identifiers beginning with '+' or '-',
           but some Scheme programs use such identifiers. */
        return read_symbol_or_number(port, c, ctx);
    case '.':;
        {
            int c1 = Scm_GetcUnsafe(port);
            if (!char_word_constituent(c1, FALSE)) {
                Scm_ReadError(port, "dot in wrong context");
            }
            Scm_UngetcUnsafe(c1, port);
            return read_symbol_or_number(port, c, ctx);
        }
    case '0':; case '1':; case '2':; case '3':; case '4':;
    case '5':; case '6':; case '7':; case '8':; case '9':;
        /* Note: R5RS doesn't permit identifiers beginning with digits,
           but some Scheme programs use such identifiers. */
        return read_symbol_or_number(port, c, ctx);
    case ')':; case ']':; case '}':;
        Scm_ReadError(port, "extra close parenthesis");
    case EOF:
        return SCM_EOF;
    default:
        return read_symbol(port, c, ctx);
    }
}

static ScmObj read_item(ScmPort *port, ScmReadContext *ctx)
{
    for (;;) {
        ScmObj obj = read_internal(port, ctx);
        if (!SCM_UNDEFINEDP(obj)) return obj;
    }
}

/*----------------------------------------------------------------
 * List
 */

/* Internal read_list.  returns whether the list contains unresolved
   reference or not within the flag has_ref */
static ScmObj read_list_int(ScmPort *port, ScmChar closer,
                            ScmReadContext *ctx, int *has_ref, int start_line)
{
    ScmObj start = SCM_NIL, last = SCM_NIL, item;
    int c, dot_seen = FALSE, ref_seen = FALSE;

    for (;;) {
        c = skipws(port, ctx);
        if (c == EOF) goto eoferr;
        if (c == closer) {
            *has_ref = !!ref_seen;
            return start;
        }

        if (dot_seen) goto baddot;

        if (c == '.') {
            int c2 = Scm_GetcUnsafe(port);
            if (c2 == closer) {
                goto baddot;
            } else if (c2 == EOF) {
                goto eoferr;
            } else if (!char_word_constituent(c2, FALSE)) {
                /* can be a dot pair at the end */
                if (start == SCM_NIL) goto baddot;
                Scm_UngetcUnsafe(c2, port);
                item = read_item(port, ctx);
                if (SCM_READ_REFERENCE_P(item)) ref_seen = TRUE;
                SCM_SET_CDR(last, item);
                dot_seen = TRUE;
                continue;
            }
            Scm_UngetcUnsafe(c2, port);
            item = read_symbol_or_number(port, c, ctx);
        } else {
            Scm_UngetcUnsafe(c, port);
            item = read_internal(port, ctx);
            if (SCM_UNDEFINEDP(item)) continue;
            if (SCM_READ_REFERENCE_P(item)) ref_seen = TRUE;
        }
        SCM_APPEND1(start, last, item);
    }
  eoferr:
    if (start_line >= 0) {
        Scm_ReadError(port, "EOF inside a list (starting from line %d)",
                      start_line);
    } else {
        Scm_ReadError(port, "EOF inside a list");
    }
  baddot:
    Scm_ReadError(port, "bad dot syntax");
    return SCM_NIL;             /* dummy */
}

static ScmObj read_list(ScmPort *port, ScmChar closer, ScmReadContext *ctx)
{
    int has_ref;
    int line = -1;
    ScmObj r;

    if (ctx->flags & SCM_READ_SOURCE_INFO) line = Scm_PortLine(port);

    r = read_list_int(port, closer, ctx, &has_ref, line);

    if (SCM_PAIRP(r) && (ctx->flags & SCM_READ_SOURCE_INFO) && line >= 0) {
        /* Swap the head of the list for an extended pair to record
           source-code info.*/
        r = Scm_ExtendedCons(SCM_CAR(r), SCM_CDR(r));
        Scm_PairAttrSet(SCM_PAIR(r), SCM_SYM_SOURCE_INFO,
                        SCM_LIST2(Scm_PortName(port), SCM_MAKE_INT(line)));
    }

    if (has_ref) ref_push(ctx, r, SCM_FALSE);
    return r;
}

static ScmObj read_vector(ScmPort *port, ScmChar closer, ScmReadContext *ctx)
{
    int has_ref;
    int line = -1;
    ScmObj r;

    if (ctx->flags & SCM_READ_SOURCE_INFO) line = Scm_PortLine(port);
    r = read_list_int(port, closer, ctx, &has_ref, line);
    r = Scm_ListToVector(r, 0, -1);
    if (has_ref) ref_push(ctx, r, SCM_FALSE);
    return r;
}

static ScmObj read_quoted(ScmPort *port, ScmObj quoter, ScmReadContext *ctx)
{
    int line = -1;
    ScmObj item, r;

    if (ctx->flags & SCM_READ_SOURCE_INFO) line = Scm_PortLine(port);
    item = read_item(port, ctx);
    if (SCM_EOFP(item)) Scm_ReadError(port, "unterminated quote");
    if (line >= 0) {
        r = Scm_ExtendedCons(quoter, Scm_Cons(item, SCM_NIL));
        Scm_PairAttrSet(SCM_PAIR(r), SCM_SYM_SOURCE_INFO,
                        SCM_LIST2(Scm_PortName(port), SCM_MAKE_INT(line)));
    } else {
        r = Scm_Cons(quoter, Scm_Cons(item, SCM_NIL));
    }
    if (SCM_READ_REFERENCE_P(item)) ref_push(ctx, SCM_CDR(r), SCM_FALSE);
    return r;
}

/*----------------------------------------------------------------
 * String
 */

static ScmChar read_string_xdigits(ScmPort *port, int ndigs, int key,
                                   int incompletep)
{
    char buf[8];
    int nread;
    ScmChar r;
    SCM_ASSERT(ndigs <= 8);
    r = Scm_ReadXdigitsFromPort(port, ndigs, buf, &nread);
    if (r == SCM_CHAR_INVALID) {
        ScmDString ds;
        int c, i;
        /* skip chars to the end of string, so that the reader will read
           after the erroneous string */
        for (;;) {
            if (incompletep) c = Scm_GetbUnsafe(port);
            else c = Scm_GetcUnsafe(port);
            if (c == EOF || c == '"') break;
            if (c == '\\') {
                if (incompletep) c = Scm_GetbUnsafe(port);
                else c = Scm_GetcUnsafe(port);
            }
        }
        /* construct an error message */
        Scm_DStringInit(&ds);
        Scm_DStringPutc(&ds, '\\');
        Scm_DStringPutc(&ds, key);
        for (i=0; i<nread; i++) Scm_DStringPutc(&ds, (unsigned char)buf[i]);
        Scm_ReadError(port,
                      "Bad '\\%c' escape sequence in a string literal: %s",
                      key, Scm_DStringGetz(&ds));
    }
    return r;
}

static ScmObj read_string(ScmPort *port, int incompletep,
                          ScmReadContext *ctx)
{
    int c = 0;
    ScmDString ds;
    Scm_DStringInit(&ds);

#define FETCH(var)                                      \
    if (incompletep) { var = Scm_GetbUnsafe(port); }    \
    else             { var = Scm_GetcUnsafe(port); }
#define ACCUMULATE(var)                                 \
    if (incompletep) { SCM_DSTRING_PUTB(&ds, var); }    \
    else             { SCM_DSTRING_PUTC(&ds, var); }
#define INTRALINE_WS(var)                               \
    ((var)==' ' || (var)=='\t' || SCM_CHAR_EXTRA_WHITESPACE_INTRALINE(var))

    for (;;) {
        FETCH(c);
        switch (c) {
        case EOF: goto eof_exit;
        case '"': {
            int flags = ((incompletep? SCM_STRING_INCOMPLETE : 0)
                         | SCM_STRING_IMMUTABLE);
            return Scm_DStringGet(&ds, flags);
        }
        backslash:
        case '\\': {
            FETCH(c);
            switch (c) {
            case EOF: goto eof_exit;
            case 'n': ACCUMULATE('\n'); break;
            case 'r': ACCUMULATE('\r'); break;
            case 'f': ACCUMULATE('\f'); break;
            case 't': ACCUMULATE('\t'); break;
            case '\\': ACCUMULATE('\\'); break;
            case '0': ACCUMULATE(0); break;
            case 'x': {
                int cc = read_string_xdigits(port, 2, 'x', incompletep);
                ACCUMULATE(cc);
                break;
            }
            case 'u': {
                int cc = read_string_xdigits(port, 4, 'u', incompletep);
                ACCUMULATE(Scm_UcsToChar(cc));
                break;
            }
            case 'U': {
                int cc = read_string_xdigits(port, 8, 'U', incompletep);
                ACCUMULATE(Scm_UcsToChar(cc));
                break;
            }
                /* R6RS-style line continuation handling*/
            line_continuation:
            case ' ':
            case '\t': {
                for (;;) {
                    FETCH(c);
                    if (c == EOF) goto eof_exit;
                    if (c == '\r') goto cont_cr;
                    if (c == '\n') goto cont_lf;
                    if (!INTRALINE_WS(c)) goto cont_err;
                }
            }
                /*FALLTHROUGH*/
            cont_cr:
            case '\r': {
                FETCH(c);
                if (c == EOF)  goto eof_exit;
                if (c == '\\') goto backslash;
                if (c != '\n' && !INTRALINE_WS(c)) {
                    ACCUMULATE(c);
                    break;
                }
                /*FALLTHROUGH*/
            }
            cont_lf:
            case '\n': {
                for (;;) {
                    FETCH(c);
                    if (c == EOF) goto eof_exit;
                    if (c == '\\') goto backslash;
                    if (!INTRALINE_WS(c)) {
                        ACCUMULATE(c);
                        break;
                    }
                }
                break;
            }
            default:
                if (SCM_CHAR_EXTRA_WHITESPACE_INTRALINE(c)) goto line_continuation;
                else ACCUMULATE(c);
            }
            break;
        }
        default: ACCUMULATE(c); break;
        }
    }
 eof_exit:
    Scm_ReadError(port, "EOF encountered in a string literal: %S",
                  Scm_DStringGet(&ds, 0));
 cont_err:
    Scm_ReadError(port, "Invalid line continuation sequence in a string literal: %S...",
                  Scm_DStringGet(&ds, 0));
    /* NOTREACHED */
    return SCM_FALSE;
}

/*----------------------------------------------------------------
 * Character
 */

static struct char_name {
    const char *name;
    int size;
    ScmObj ch;
} char_names[] = {
#define DEFCHAR(name, char) \
    { #name, sizeof(#name)-1, SCM_MAKE_CHAR(char) }
    DEFCHAR(space,   ' '),
    DEFCHAR(newline, '\n'),
    DEFCHAR(nl,      '\n'),
    DEFCHAR(lf,      '\n'),
    DEFCHAR(return,  '\r'),
    DEFCHAR(cr,      '\r'),
    DEFCHAR(tab,     '\t'),
    DEFCHAR(ht,      '\t'),
    DEFCHAR(page,    '\f'),
    DEFCHAR(escape,  0x1b),
    DEFCHAR(esc,     0x1b),
    DEFCHAR(delete,  0x7f),
    DEFCHAR(del,     0x7f),
    DEFCHAR(null,    0),
    { NULL, 0, 0 }
};

static ScmObj read_char(ScmPort *port, ScmReadContext *ctx)
{
    int c;
    ScmString *name;
    const char *cname;
    u_int namelen, namesize;
    struct char_name *cntab = char_names;

    c = Scm_GetcUnsafe(port);
    switch (c) {
    case EOF: Scm_ReadError(port, "EOF encountered in character literal");
    case '(':; case ')':; case '[':; case ']':; case '{':; case '}':;
    case '"':; case ' ':; case '\\':; case '|':; case ';':;
    case '#':;
        return SCM_MAKE_CHAR(c);
    default:
        /* need to read word to see if it is a character name */
        name = SCM_STRING(read_word(port, c, ctx, TRUE, FALSE));
        cname = Scm_GetStringContent(name, &namesize, &namelen, NULL);
        if (namelen == 1) {
            return SCM_MAKE_CHAR(c);
        }
        if (namelen != namesize) {
            /* no character name contains multibyte chars */
            goto unknown;
        }

        /* handle #\x1f etc. */
        if (cname[0] == 'x' && isxdigit(cname[1])) {
            int code = Scm_ReadXdigitsFromString(cname+1, namesize-1, NULL);
            if (code < 0) goto unknown;
            return SCM_MAKE_CHAR(code);
        }
        /* handle #\uxxxx or #\uxxxxxxxx*/
        if ((cname[0] == 'u') && isxdigit(cname[1])) {
            int code;
            if (namesize >= 5 && namesize <= 9) {
                code = Scm_ReadXdigitsFromString(cname+1, namesize-1, NULL);
                if (code >= 0) return SCM_MAKE_CHAR(Scm_UcsToChar(code));
            }
            /* if we come here, it's an error. */
            Scm_ReadError(port, "Bad UCS character code: #\\%s", cname);
        }

        while (cntab->name) {
            if (cntab->size == namesize
                && strncmp(cntab->name, cname, namesize) == 0) {
                return cntab->ch;
            }
            cntab++;
        }
      unknown:
        Scm_ReadError(port, "Unknown character name: #\\%A", name);
    }
    return SCM_UNDEFINED;       /* dummy */
}

/*----------------------------------------------------------------
 * Symbols and Numbers
 */

/* Reads a sequence of word-constituent characters from PORT, and returns
   ScmString.  INITIAL may be a readahead character, or SCM_CHAR_INVALID
   if there's none.  TEMP_CASE_FOLD turns on case-fold mode regardless of
   the read context setting.  INCLUDE_HASH_SIGN allows '#' to appear in
   the word.
*/
static ScmObj read_word(ScmPort *port, ScmChar initial, ScmReadContext *ctx,
                        int temp_case_fold, int include_hash_sign)
{
    int c = 0;
    int case_fold = temp_case_fold || SCM_PORT_CASE_FOLD(port);
    ScmDString ds;
    Scm_DStringInit(&ds);
    if (initial != SCM_CHAR_INVALID) {
        if (case_fold && char_word_case_fold(initial)) initial = tolower(initial);
        SCM_DSTRING_PUTC(&ds, initial);
    }

    for (;;) {
        c = Scm_GetcUnsafe(port);
        if (c == EOF || !char_word_constituent(c, include_hash_sign)) {
            Scm_UngetcUnsafe(c, port);
            return Scm_DStringGet(&ds, 0);
        }
        if (case_fold && char_word_case_fold(c)) c = tolower(c);
        SCM_DSTRING_PUTC(&ds, c);
    }
}

/* Kaveat: We don't allow '#' in symbols, but we need to read '#'
   for numbers.  To allow weird identifers like '1+', we need to read the
   word as a number fist and convert it to a symbol when the read word
   can't be interpreted as a number.  For the consistency, we read
   with '#' and then check it in read_symbol, too. */
static void check_valid_symbol(ScmString *s)
{
    ScmObj r = Scm_StringScanChar(s, SCM_CHAR('#'), SCM_STRING_SCAN_INDEX);
    if (!SCM_FALSEP(r)) {
        Scm_Error("invalid symbol name: %S", SCM_OBJ(s));
    }
}

static ScmObj read_symbol(ScmPort *port, ScmChar initial, ScmReadContext *ctx)
{
    ScmString *s = SCM_STRING(read_word(port, initial, ctx, FALSE, TRUE));
    check_valid_symbol(s);
    return Scm_Intern(s);
}

static ScmObj read_number(ScmPort *port, ScmChar initial, ScmReadContext *ctx)
{
    ScmString *s = SCM_STRING(read_word(port, initial, ctx, FALSE, TRUE));
    ScmObj num = Scm_StringToNumber(s, 10, 0);
    if (num == SCM_FALSE)
        Scm_ReadError(port, "bad numeric format: %S", s);
    return num;
}

static ScmObj read_symbol_or_number(ScmPort *port, ScmChar initial, ScmReadContext *ctx)
{
    ScmString *s = SCM_STRING(read_word(port, initial, ctx, FALSE, TRUE));
    ScmObj num = Scm_StringToNumber(s, 10, 0);
    if (num != SCM_FALSE) return num;
    check_valid_symbol(s);
    return Scm_Intern(s);    
}

static ScmObj read_keyword(ScmPort *port, ScmReadContext *ctx)
{
    int c2 = Scm_GetcUnsafe(port);
    ScmObj name;

    if (c2 == '|') {
        name = read_escaped_symbol(port, c2, FALSE); /* read as uninterned */
        return Scm_MakeKeyword(SCM_SYMBOL_NAME(name));
    } else {
        Scm_UngetcUnsafe(c2, port);
        name = read_word(port, SCM_CHAR_INVALID, ctx, FALSE, FALSE);
        return Scm_MakeKeyword(SCM_STRING(name));
    }
}

static ScmObj read_escaped_symbol(ScmPort *port, ScmChar delim, int interned)
{
    int c = 0;
    ScmDString ds;
    Scm_DStringInit(&ds);

    for (;;) {
        c = Scm_GetcUnsafe(port);
        if (c == EOF) {
            goto err;
        } else if (c == delim) {
            ScmString *s = SCM_STRING(Scm_DStringGet(&ds, 0));
            return Scm_MakeSymbol(s, interned);
        } else if (c == '\\') {
            /* CL-style single escape */
            c = Scm_GetcUnsafe(port);
            /* TODO: we should recognize \xNN, since the symbol writer
               prints a symbol name in that syntax. */
            if (c == EOF) goto err;
            SCM_DSTRING_PUTC(&ds, c);
        } else {
            SCM_DSTRING_PUTC(&ds, c);
        }
    }
  err:
    Scm_ReadError(port, "unterminated escaped symbol: |%s ...",
                  Scm_DStringGetz(&ds));
    return SCM_UNDEFINED; /* dummy */
}

/*----------------------------------------------------------------
 * Regexp & charset
 */

/* gauche extension :  #/regexp/ */
static ScmObj read_regexp(ScmPort *port)
{
    ScmChar c = 0, c1 = 0;
    ScmDString ds;
    Scm_DStringInit(&ds);
    for (;;) {
        c = Scm_GetcUnsafe(port);
        if (c == SCM_CHAR_INVALID) {
            Scm_ReadError(port, "unterminated literal regexp");
        }
        if (c == '\\') {
            /* NB: We "eat" a backslash before '/', since it is only dealt
               with the reader and nothing to do with regexp parser itself. */
            c1 = Scm_GetcUnsafe(port);
            if (c1 == SCM_CHAR_INVALID) {
                Scm_ReadError(port, "unterminated literal regexp");
            }
            if (c1 != '/') SCM_DSTRING_PUTC(&ds, c);
            SCM_DSTRING_PUTC(&ds, c1);
        } else if (c == '/') {
            /* Read one more char to see if we have a flag */
            int flags = 0;
            c = Scm_GetcUnsafe(port);
            if (c == 'i') flags |= SCM_REGEXP_CASE_FOLD;
            else          Scm_UngetcUnsafe(c, port);
            return Scm_RegComp(SCM_STRING(Scm_DStringGet(&ds, 0)), flags);
        } else {
            SCM_DSTRING_PUTC(&ds, c);
        }
    }
}

/* gauche extension :  #[charset] */
static ScmObj read_charset(ScmPort *port)
{
    return Scm_CharSetRead(port, NULL, TRUE, FALSE);
}

/*----------------------------------------------------------------
 * Back reference (#N# and #N=)
 */

static ScmObj read_reference(ScmPort *port, ScmChar ch, ScmReadContext *ctx)
{
    ScmObj e = SCM_UNBOUND;
    int refnum = Scm_DigitToInt(ch, 10);

    for (;;) {
        ch = Scm_GetcUnsafe(port);
        if (ch == EOF) {
            Scm_ReadError(port, "unterminated reference form (#digits)");
        }
        if (SCM_CHAR_ASCII_P(ch) && isdigit(ch)) {
            refnum = refnum*10+Scm_DigitToInt(ch, 10);
            if (refnum < 0) Scm_ReadError(port, "reference number overflow");
            continue;
        }
        if (ch != '#' && ch != '=') {
            Scm_ReadError(port, "invalid reference form (must be either #digits# or #digits=) : #%d%A", refnum, SCM_MAKE_CHAR(ch));
        }
        break;
    }
    if (ch == '#') {
        /* #digit# - back reference */
        if (ctx->table == NULL
            || SCM_UNBOUNDP(e = Scm_HashTableRef(ctx->table,
                                                 Scm_MakeInteger(refnum),
                                                 SCM_UNBOUND))) {
            Scm_ReadError(port, "invalid reference number in #%d#", refnum);
        }
        if (SCM_READ_REFERENCE_P(e) && SCM_READ_REFERENCE_REALIZED(e)) {
            return SCM_READ_REFERENCE(e)->value;
        } else {
            return e;
        }
    } else {
        /* #digit= - register */
        ScmObj val;
        ScmObj ref = Scm_MakeReadReference();

        if (ctx->table == NULL) {
            ctx->table =
                SCM_HASH_TABLE(Scm_MakeHashTableSimple(SCM_HASH_EQV, 0));
        }
        if (!SCM_UNBOUNDP(Scm_HashTableRef(ctx->table,
                                           Scm_MakeInteger(refnum),
                                           SCM_UNBOUND))) {
            Scm_ReadError(port, "duplicate back-reference number in #%d=",
                          refnum);
        }
        ref_register(ctx, ref, refnum);
        val = read_item(port, ctx);
        if (ref == val) {
            /* an edge case: #0=#0# */
            Scm_ReadError(port, "indeterminate read reference: #%d=#%d#",
                          refnum, refnum);
        }
        SCM_READ_REFERENCE(ref)->value = val;
        return val;
    }
}

/*----------------------------------------------------------------
 * SRFI-10 support
 */

/* NB: The 'module' argument of DefineReaderCtor and GetReaderCtor may
   be used in future to make reader-ctor binding associated with modules.
   For now, it is not used and the caller should pass SCM_FALSE. */

ScmObj Scm_DefineReaderCtor(ScmObj symbol, ScmObj proc, ScmObj finisher,
                            ScmObj module /*reserved*/)
{
    ScmObj pair;
    if (!SCM_PROCEDUREP(proc)) {
        Scm_Error("procedure required, but got %S\n", proc);
    }
    pair = Scm_Cons(proc, finisher);
    (void)SCM_INTERNAL_MUTEX_LOCK(readCtorData.mutex);
    Scm_HashTableSet(readCtorData.table, symbol, pair, 0);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(readCtorData.mutex);
    return SCM_UNDEFINED;
}

ScmObj Scm_GetReaderCtor(ScmObj symbol, ScmObj module /*reserved*/)
{
    ScmObj r;
    (void)SCM_INTERNAL_MUTEX_LOCK(readCtorData.mutex);
    r = Scm_HashTableRef(readCtorData.table, symbol, SCM_FALSE);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(readCtorData.mutex);
    return r;
}

static ScmObj read_sharp_comma(ScmPort *port, ScmReadContext *ctx)
{
    int len, has_ref, line=-1;
    ScmChar next;
    ScmObj form, r;

    next = Scm_GetcUnsafe(port);
    if (next != '(') {
        Scm_ReadError(port, "bad #,-form: '(' should be followed, but got %C",
                      next);
    }

    if (ctx->flags & SCM_READ_SOURCE_INFO) line = Scm_PortLine(port);

    form = read_list_int(port, ')', ctx, &has_ref, line);
    len = Scm_Length(form);
    if (len <= 0) {
        Scm_ReadError(port, "bad #,-form: #,%S", form);
    }
    r = process_sharp_comma(port, SCM_CAR(form), SCM_CDR(form), ctx, has_ref);
    return r;
}

static ScmObj process_sharp_comma(ScmPort *port, ScmObj key, ScmObj args,
                                  ScmReadContext *ctx, int has_ref)
{
    ScmObj r, e;

    if (ctx->flags & SCM_READ_DISABLE_CTOR) return SCM_FALSE;

    (void)SCM_INTERNAL_MUTEX_LOCK(readCtorData.mutex);
    e = Scm_HashTableRef(readCtorData.table, key, SCM_FALSE);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(readCtorData.mutex);

    if (!SCM_PAIRP(e)) Scm_ReadError(port, "unknown #,-key: %S", key);
    r = Scm_ApplyRec(SCM_CAR(e), args);
    if (has_ref) ref_push(ctx, r, SCM_CDR(e));
    return r;
}

static ScmObj reader_ctor(ScmObj *args, int nargs, void *data)
{
    ScmObj optarg = (nargs > 2? args[2] : SCM_FALSE);
    return Scm_DefineReaderCtor(args[0], args[1], optarg, SCM_FALSE);
}

/*----------------------------------------------------------------
 * #!-support
 */

ScmObj Scm_DefineReaderDirective(ScmObj symbol, ScmObj proc)
{
    (void)SCM_INTERNAL_MUTEX_LOCK(hashBangData.mutex);
    Scm_HashTableSet(hashBangData.table, symbol, proc, 0);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(hashBangData.mutex);
    return SCM_UNDEFINED;
}

static ScmObj read_shebang(ScmPort *port, ScmReadContext *ctx)
{
    /* If '#!' appears in the beginning of the input port, and it is
       followed by '/' or a space, then we consider it as the
       beginning of shebang and discards entire line.  Otherwise, we
       take this as #!<identifier> directive as specified in R6RS, and
       calls appropriate handler.

       R6RS is actually not very clean at this corner.  It requires
       distinct modes to parse a script (which always begins with
       shebang) and to parse R6RS program (in which '#!'  always
       introduces #!<identifier> token).  There's no way for just one
       parser that strictly covers both situation.
    */
    int c2 = Scm_GetcUnsafe(port);
    if (port->bytes == 3 && (c2 == '/' || c2 == ' ')) {
        /* shebang */
        for (;;) {
            c2 = Scm_GetcUnsafe(port);
            if (c2 == '\n') return SCM_UNDEFINED;
            if (c2 == EOF) return SCM_EOF;
        }
        /*NOTREACHED*/
    } else {
        ScmObj id = read_symbol(port, c2, ctx), e, r;
        SCM_ASSERT(SCM_SYMBOLP(id));
        (void)SCM_INTERNAL_MUTEX_LOCK(hashBangData.mutex);
        e = Scm_HashTableRef(hashBangData.table, id, SCM_FALSE);
        (void)SCM_INTERNAL_MUTEX_UNLOCK(hashBangData.mutex);
        if (SCM_FALSEP(e)) {
            Scm_Warn("Ignoring unrecognized hash-bang directive: #!%S", id);
            return SCM_UNDEFINED;
        }
        /* Reader directive may return zero or one value.  When it returns
           no values, we call Scm_VMSetResult to adjust the number of values.
         */
        r = Scm_ApplyRec3(e, id, SCM_OBJ(port), SCM_OBJ(ctx));
        if (Scm_VMGetNumResults(Scm_VM()) == 1) return r;
        else { Scm_VMSetResult(SCM_UNDEFINED); return SCM_UNDEFINED; }
    }
}

/*----------------------------------------------------------------
 * #f, #t, #false, #true, and UVector literals
 */

/* Pre-0.9.4 reader.  #t and #f delimit themselves (except '1', '3' or '6'
   follows '#f'.)  I doubt any code breaks if we change that, but there
   may be a data files around that somehow relies on this behavior.  So
   we keep this in 'legacy' reader mode.  */
static ScmObj read_sharp_word_legacy(ScmPort *port, char ch, ScmReadContext *ctx)
{
    ScmChar c1, c2 = SCM_CHAR_INVALID;
    char *tag = NULL;

    if (ch == 't') return SCM_TRUE;

    c1 = Scm_GetcUnsafe(port);
    if (ch == 'f') {
        if (c1 != '1' && c1 != '3' && c1 != '6') {
            Scm_UngetcUnsafe(c1, port);
            return SCM_FALSE;
        }
        c2 = Scm_GetcUnsafe(port);
        if (c1 == '3' && c2 == '2') tag = "f32";
        else if (c1 == '6' && c2 == '4') tag = "f64";
        else if (c1 == '1' && c2 == '6') tag = "f16";
    } else {
        if (c1 == '8') tag = (ch == 's')? "s8" : "u8";
        else if (c1 == '1') {
            c2 = Scm_GetcUnsafe(port);
            if (c2 == '6') tag = (ch == 's')? "s16" : "u16";
        }
        else if (c1 == '3') {
            c2 = Scm_GetcUnsafe(port);
            if (c2 == '2') tag = (ch == 's')? "s32" : "u32";
        }
        else if (c1 == '6') {
            c2 = Scm_GetcUnsafe(port);
            if (c2 == '4') tag = (ch == 's')? "s64" : "u64";
        }
    }
    if (tag == NULL) {
        char buf[SCM_CHAR_MAX_BYTES*4], *bufp = buf;
        *bufp++ = ch;
        SCM_CHAR_PUT(bufp, c1);
        bufp += SCM_CHAR_NBYTES(c1);
        if (c2 != SCM_CHAR_INVALID) {
            SCM_CHAR_PUT(bufp, c2);
            bufp += SCM_CHAR_NBYTES(c2);
        }
        *bufp = '\0';
        Scm_ReadError(port, "invalid uniform vector tag: %s", buf);
    }
    if (read_uvector_hook == NULL) {
        /* Require srfi-4 (gauche/uvector)
           NB: we don't need mutex here, for the loading of srfi-4 is
           serialized in Scm_Require. */
        Scm_Require(SCM_MAKE_STR("gauche/uvector"),
                    SCM_LOAD_PROPAGATE_ERROR, NULL);
        if (read_uvector_hook == NULL)
            Scm_ReadError(port, "couldn't load srfi-4 module");
    }
    return read_uvector_hook(port, tag, ctx);
}

/* A 'new' version, friendly to R7RS */
static ScmObj read_sharp_word_1(ScmPort *port, char ch, ScmReadContext *ctx)
{
    ScmString *s = SCM_STRING(read_word(port, ch, ctx, TRUE, FALSE));
    const char *w = Scm_GetStringConst(s);
    const char *tag = NULL;

    switch (ch) {
    case 'f':
        if (strcmp(w, "f16") == 0
            || strcmp(w, "f32") == 0
            || strcmp(w, "f64") == 0) {
            tag = w;
        } else if (w[1] == '\0' || strcmp(w, "false") == 0) {
            return SCM_FALSE;
        }
        break;
    case 's':
        if (strcmp(w, "s8") == 0
            || strcmp(w, "s16") == 0
            || strcmp(w, "s32") == 0
            || strcmp(w, "s64") == 0) {
            tag = w;
        }
        break;
    case 'u':
        if (strcmp(w, "u8") == 0
            || strcmp(w, "u16") == 0
            || strcmp(w, "u32") == 0
            || strcmp(w, "u64") == 0) {
            tag = w;
        }
        break;
    case 't':
        if (w[1] == '\0' || strcmp(w, "true") == 0) {
            return SCM_TRUE;
        }
    }
    if (tag == NULL) {
        Scm_ReadError(port, "invalid #-token: #%s", w);
    }
    /* Uvector support is implemented by gauche.uvector.  When it
       is loaded, it sets up the pointer read_uvector_hook. */
    if (read_uvector_hook == NULL) {
        /* Require srfi-4 (gauche/uvector)
           NB: we don't need mutex here, for the loading of srfi-4 is
           serialized in Scm_Require. */
        Scm_Require(SCM_MAKE_STR("gauche/uvector"),
                    SCM_LOAD_PROPAGATE_ERROR, NULL);
        if (read_uvector_hook == NULL)
            Scm_ReadError(port, "couldn't load srfi-4 module");
    }
    return read_uvector_hook(port, tag, ctx);
}

static ScmObj read_sharp_word(ScmPort *port, char ch, ScmReadContext *ctx)
{
    if (ctx->flags & SCM_READ_LEGACY) {
        return read_sharp_word_legacy(port, ch, ctx);
    } else {
        return read_sharp_word_1(port, ch, ctx);
    }
}

void Scm__InstallReadUvectorHook(ScmObj (*f)(ScmPort*, const char*, ScmReadContext *))
{
    read_uvector_hook = f;
}


/*----------------------------------------------------------------
 * Initialization
 */

void Scm__InitRead(void)
{
    readCtorData.table =
        SCM_HASH_TABLE(Scm_MakeHashTableSimple(SCM_HASH_EQ, 0));
    (void)SCM_INTERNAL_MUTEX_INIT(readCtorData.mutex);
    Scm_DefineReaderCtor(SCM_SYM_DEFINE_READER_CTOR,
                         Scm_MakeSubr(reader_ctor, NULL, 2, 1,
                                      SCM_SYM_DEFINE_READER_CTOR),
                         SCM_FALSE, SCM_FALSE);

    hashBangData.table =
        SCM_HASH_TABLE(Scm_MakeHashTableSimple(SCM_HASH_EQ, 0));
    (void)SCM_INTERNAL_MUTEX_INIT(hashBangData.mutex);
}

