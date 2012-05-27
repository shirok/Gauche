/*
 * string.c - string implementation
 *
 *   Copyright (c) 2000-2012  Shiro Kawai  <shiro@acm.org>
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

#include <ctype.h>
#include <string.h>

void Scm_DStringDump(FILE *out, ScmDString *dstr);

static void string_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);
SCM_DEFINE_BUILTIN_CLASS(Scm_StringClass, string_print, NULL, NULL, NULL,
                         SCM_CLASS_SEQUENCE_CPL);

/* Internal primitive constructor.   LEN can be negative if the string
   is incomplete. */
static ScmString *make_str(int len, int siz, const char *p, int flags)
{
    ScmString *s = SCM_NEW(ScmString);
    SCM_SET_CLASS(s, SCM_CLASS_STRING);

    if (len < 0) flags |= SCM_STRING_INCOMPLETE;
    if (flags & SCM_STRING_INCOMPLETE) len = siz;

    s->body = NULL;
    s->initialBody.flags = flags & SCM_STRING_FLAG_MASK;
    s->initialBody.length = len;
    s->initialBody.size = siz;
    s->initialBody.start = p;
    return s;
}

#if 0
static ScmStringBody *make_str_body(int len, int siz, const char *p, int flags)
{
    ScmStringBody *b = SCM_NEW(ScmStringBody);
    b->length = (len < 0)? siz : len;
    b->size = siz;
    b->start = p;
    b->flags = flags;
    return b;
}
#endif

#define DUMP_LENGTH   50

/* for debug */
void Scm_StringDump(FILE *out, ScmObj str)
{
    int i;
    const ScmStringBody *b = SCM_STRING_BODY(str);
    int s = SCM_STRING_BODY_SIZE(b);
    const char *p = SCM_STRING_BODY_START(b);

    fprintf(out, "STR(len=%d,siz=%d) \"", SCM_STRING_BODY_LENGTH(b), s);
    for (i=0; i < DUMP_LENGTH && s > 0;) {
        int n = SCM_CHAR_NFOLLOWS(*p) + 1;
        for (; n > 0 && s > 0; p++, n--, s--, i++) {
            putc(*p, out);
        }
    }
    if (s > 0) {
        fputs("...\"\n", out);
    } else {
        fputs("\"\n", out);
    }
}

/*
 * Multibyte length calculation
 */

/* We have multiple similar functions, due to performance reasons. */

/* Calculate both length and size of C-string str.
   If str is incomplete, *plen gets -1. */
static inline int count_size_and_length(const char *str, int *psize, int *plen)
{
    char c;
    const char *p = str;
    int size = 0, len = 0;
    while ((c = *p++) != 0) {
        int i = SCM_CHAR_NFOLLOWS(c);
        len++;
        size++;
        while (i-- > 0) {
            if (!*p++) { len = -1; goto eos; }
            size++;
        }
    }
  eos:
    *psize = size;
    *plen = len;
    return len;
}

/* Calculate length of known size string.  str can contain NUL character. */
static inline int count_length(const char *str, int size)
{
    int count = 0;

    while (size-- > 0) {
        ScmChar ch;
        unsigned char c = (unsigned char)*str;
        int i = SCM_CHAR_NFOLLOWS(c);
        if (i < 0 || i > size) return -1;
        SCM_CHAR_GET(str, ch);
        if (ch == SCM_CHAR_INVALID) return -1;
        count++;
        str += i+1;
        size -= i;
    }
    return count;
}

/* Returns length of string, starts from str and end at stop.
   If stop is NULL, str is regarded as C-string (NUL terminated).
   If the string is incomplete, returns -1. */
int Scm_MBLen(const char *str, const char *stop)
{
    int size = (stop == NULL)? (int)strlen(str) : (stop - str);
    return count_length(str, size);
}

/*----------------------------------------------------------------
 * Constructors
 */

/* General constructor. */
ScmObj Scm_MakeString(const char *str, int size, int len, int flags)
{
    ScmString *s;

    flags &= ~SCM_STRING_TERMINATED;

    if (size < 0) {
        count_size_and_length(str, &size, &len);
        flags |= SCM_STRING_TERMINATED;
    } else {
        if (len < 0) len = count_length(str, size);
    }

    if (flags & SCM_STRING_COPYING) {
        char *nstr = SCM_NEW_ATOMIC2(char *, size + 1);
        memcpy(nstr, str, size);
        nstr[size] = '\0';          /* be kind to C */
        flags |= SCM_STRING_TERMINATED;
        s = make_str(len, size, nstr, flags);
    } else {
        s = make_str(len, size, str, flags);
    }
    return SCM_OBJ(s);
}

ScmObj Scm_MakeFillString(int len, ScmChar fill)
{
    int size = SCM_CHAR_NBYTES(fill), i;
    char *ptr = SCM_NEW_ATOMIC2(char *, size*len+1);
    char *p;

    if (len < 0) Scm_Error("length out of range: %d", len);
    for (i=0, p=ptr; i<len; i++, p+=size) {
        SCM_CHAR_PUT(p, fill);
    }
    ptr[size*len] = '\0';
    return SCM_OBJ(make_str(len, size*len, ptr, SCM_STRING_TERMINATED));
}

ScmObj Scm_ListToString(ScmObj chars)
{
    ScmObj cp;
    int size = 0, len = 0;
    ScmChar ch;
    char *buf, *bufp;

    SCM_FOR_EACH(cp, chars) {
        if (!SCM_CHARP(SCM_CAR(cp)))
            Scm_Error("character required, but got %S", SCM_CAR(cp));
        ch = SCM_CHAR_VALUE(SCM_CAR(cp));
        size += SCM_CHAR_NBYTES(ch);
        len++;
    }
    bufp = buf = SCM_NEW_ATOMIC2(char *, size+1);
    SCM_FOR_EACH(cp, chars) {
        ch = SCM_CHAR_VALUE(SCM_CAR(cp));
        SCM_CHAR_PUT(bufp, ch);
        bufp += SCM_CHAR_NBYTES(ch);
    }
    *bufp = '\0';
    return Scm_MakeString(buf, size, len, 0);
}

/* Extract string as C-string.  This one guarantees to return
   mutable string (we always copy) */
char *Scm_GetString(ScmString *str)
{
    int size;
    char *p;
    const ScmStringBody *b = SCM_STRING_BODY(str);

    size = SCM_STRING_BODY_SIZE(b);
    p = SCM_NEW_ATOMIC2(char *, size+1);
    memcpy(p, SCM_STRING_BODY_START(b), size);
    p[size] = '\0';
    return p;
}

/* Common routine for Scm_GetStringConst and Scm_GetStringContent */
static const char *get_string_from_body(const ScmStringBody *b)
{
    int size = SCM_STRING_BODY_SIZE(b);
    if (SCM_STRING_BODY_HAS_FLAG(b, SCM_STRING_TERMINATED)) {
        /* we can use string data as C-string */
        return SCM_STRING_BODY_START(b);
    } else {
        char *p = SCM_NEW_ATOMIC2(char *, size+1);
        memcpy(p, SCM_STRING_BODY_START(b), size);
        p[size] = '\0';
        /* kludge! This breaks 'const' qualification, but we know
           this is an idempotent operation from the outside.  Note that
           this is safe even multiple threads execute this part
           simultaneously. */
        ((ScmStringBody*)b)->start = p; /* discard const qualifier */
        ((ScmStringBody*)b)->flags |= SCM_STRING_TERMINATED;
        return p;
    }
}


/* Extract string as C-string.  Returned string is immutable,
   so we can directly return the body of the string. */
const char *Scm_GetStringConst(ScmString *str)
{
    return get_string_from_body(SCM_STRING_BODY(str));
}

/* Atomically extracts C-string, length, size, and incomplete flag.
   MT-safe. */
const char *Scm_GetStringContent(ScmString *str,
                                 unsigned int *psize,   /* out */
                                 unsigned int *plength, /* out */
                                 unsigned int *pflags)  /* out */
{
    const ScmStringBody *b = SCM_STRING_BODY(str);
    if (psize)   *psize = SCM_STRING_BODY_SIZE(b);
    if (plength) *plength = SCM_STRING_BODY_LENGTH(b);
    if (pflags) *pflags = SCM_STRING_BODY_FLAGS(b);
    return get_string_from_body(b);
}


/* Copy string.  You can modify the flags of the newly created string
   by FLAGS and MASK arguments; for the bits set in MASK, corresponding
   bits in FLAGS are copied to the new string, and for other bits, the
   original flags are copied.

   The typical semantics of copy-string is achieved by passing 0 to
   FLAGS and SCM_STRING_IMMUTABLE to MASK (i.e. reset IMMUTABLE flag,
   and keep other flags intact.

   NB: This routine doesn't check whether specified flag is valid
   with the string content, i.e. you can drop INCOMPLETE flag with
   copying, while the string content won't be checked if it consists
   valid complete string. */
ScmObj Scm_CopyStringWithFlags(ScmString *x, int flags, int mask)
{
    const ScmStringBody *b = SCM_STRING_BODY(x);
    int size = SCM_STRING_BODY_SIZE(b);
    int len  = SCM_STRING_BODY_LENGTH(b);
    const char *start = SCM_STRING_BODY_START(b);
    int newflags = ((SCM_STRING_BODY_FLAGS(b) & ~mask)
                    | (flags & mask));

    return SCM_OBJ(make_str(len, size, start, newflags));
}

ScmObj Scm_StringCompleteToIncomplete(ScmString *x)
{
    return Scm_CopyStringWithFlags(x, SCM_STRING_INCOMPLETE,
                                   SCM_STRING_INCOMPLETE);
}

ScmObj Scm_StringIncompleteToComplete(ScmString *x,
                                      int handling,
                                      ScmChar substitute)
{
    const ScmStringBody *b;
    ScmObj r = SCM_FALSE;

    switch (handling) {
    case SCM_ILLEGAL_CHAR_REJECT:
    case SCM_ILLEGAL_CHAR_OMIT:
    case SCM_ILLEGAL_CHAR_REPLACE:
        break;
    default:
        Scm_Error("invalid 'handling' argument: %d", handling);
        return SCM_UNDEFINED; /* dummy */
    }

    b = SCM_STRING_BODY(x);
    if (!SCM_STRING_BODY_INCOMPLETE_P(b)) {
        /* we do simple copy */
        r = Scm_CopyString(x);
    } else {
        const char *s = SCM_STRING_BODY_START(b);
        int siz = SCM_STRING_BODY_SIZE(b);
        int len = count_length(s, siz);
        if (len >= 0) {
            r = Scm_MakeString(s, siz, len, 0);
        } else if (handling == SCM_ILLEGAL_CHAR_REJECT) {
            r = SCM_FALSE;
        } else {
            ScmDString ds;
            const char *p = s;
            ScmChar ch;

            Scm_DStringInit(&ds);

            while (p < s+siz) {
                if (p + SCM_CHAR_NFOLLOWS(*p) >= s + siz) {
                    ch = SCM_CHAR_INVALID;
                } else {
                    SCM_CHAR_GET(p, ch);
                }

                if (ch != SCM_CHAR_INVALID) {
                    Scm_DStringPutc(&ds, ch);
                    p += SCM_CHAR_NBYTES(ch);
                } else if (handling == SCM_ILLEGAL_CHAR_OMIT) {
                    p++;
                } else {        /* SCM_ILLEGAL_CHAR_REPLACE */
                    Scm_DStringPutc(&ds, substitute);
                    p++;
                }
            }
            r = Scm_DStringGet(&ds, 0);
        }
    }

    return r;
}

/*----------------------------------------------------------------
 * Comparison
 */

/* TODO: merge Equal and Cmp API; required generic comparison protocol */
int Scm_StringEqual(ScmString *x, ScmString *y)
{
    const ScmStringBody *xb = SCM_STRING_BODY(x);
    const ScmStringBody *yb = SCM_STRING_BODY(y);
    if ((SCM_STRING_BODY_FLAGS(xb)^SCM_STRING_BODY_FLAGS(yb))&SCM_STRING_INCOMPLETE) {
        return FALSE;
    }
    if (SCM_STRING_BODY_SIZE(xb) != SCM_STRING_BODY_SIZE(yb)) {
        return FALSE;
    }
    return (memcmp(SCM_STRING_BODY_START(xb),
                   SCM_STRING_BODY_START(yb),
                   SCM_STRING_BODY_SIZE(xb)) == 0? TRUE : FALSE);
}

int Scm_StringCmp(ScmString *x, ScmString *y)
{
    int sizx, sizy, siz, r;
    const ScmStringBody *xb = SCM_STRING_BODY(x);
    const ScmStringBody *yb = SCM_STRING_BODY(y);
    if ((SCM_STRING_BODY_FLAGS(xb)^SCM_STRING_BODY_FLAGS(yb))&SCM_STRING_INCOMPLETE) {
        Scm_Error("cannot compare incomplete vs complete string: %S, %S",
                  SCM_OBJ(x), SCM_OBJ(y));
    }
    sizx = SCM_STRING_BODY_SIZE(xb);
    sizy = SCM_STRING_BODY_SIZE(yb);
    siz = (sizx < sizy)? sizx : sizy;
    r = memcmp(SCM_STRING_BODY_START(xb), SCM_STRING_BODY_START(yb), siz);
    if (r == 0) {
        if (sizx == sizy) return 0;
        if (sizx < sizy)  return -1;
        else              return 1;
    } else if (r < 0) {
        return -1;
    } else {
        return 1;
    }
}

/* single-byte case insensitive comparison */
static int sb_strcasecmp(const char *px, int sizx,
                         const char *py, int sizy)
{
    char cx, cy;
    for (; sizx > 0 && sizy > 0; sizx--, sizy--, px++, py++) {
        cx = tolower((u_char)*px);
        cy = tolower((u_char)*py);
        if (cx == cy) continue;
        return (cx - cy);
    }
    if (sizx > 0) return 1;
    if (sizy > 0) return -1;
    return 0;
}

/* multi-byte case insensitive comparison */
static int mb_strcasecmp(const char *px, int lenx,
                         const char *py, int leny)
{
    int cx, cy, ccx, ccy, ix, iy;
    for (; lenx > 0 && leny > 0; lenx--, leny--, px+=ix, py+=iy) {
        SCM_CHAR_GET(px, cx);
        SCM_CHAR_GET(py, cy);
        ccx = SCM_CHAR_UPCASE(cx);
        ccy = SCM_CHAR_UPCASE(cy);
        if (ccx != ccy) return (ccx - ccy);
        ix = SCM_CHAR_NBYTES(cx);
        iy = SCM_CHAR_NBYTES(cy);
    }
    if (lenx > 0) return 1;
    if (leny > 0) return -1;
    return 0;
}

int Scm_StringCiCmp(ScmString *x, ScmString *y)
{
    int sizx, lenx, sizy, leny;
    const char *px, *py;
    const ScmStringBody *xb = SCM_STRING_BODY(x);
    const ScmStringBody *yb = SCM_STRING_BODY(y);

    if ((SCM_STRING_BODY_FLAGS(xb)^SCM_STRING_BODY_FLAGS(yb))&SCM_STRING_INCOMPLETE) {
        Scm_Error("cannot compare incomplete strings in case-insensitive way: %S, %S",
                  SCM_OBJ(x), SCM_OBJ(y));
    }
    sizx = SCM_STRING_BODY_SIZE(xb); lenx = SCM_STRING_BODY_LENGTH(xb);
    sizy = SCM_STRING_BODY_SIZE(yb); leny = SCM_STRING_BODY_LENGTH(yb);
    px = SCM_STRING_BODY_START(xb);
    py = SCM_STRING_BODY_START(yb);

    if (sizx == lenx && sizy == leny) {
        return sb_strcasecmp(px, sizx, py, sizy);
    } else {
        return mb_strcasecmp(px, lenx, py, leny);
    }
}

/*----------------------------------------------------------------
 * Reference
 */

/* Internal fn for index -> position.  Args assumed in boundary. */
static const char *forward_pos(const char *current, int offset)
{
    int n;

    while (offset--) {
        n = SCM_CHAR_NFOLLOWS(*current);
        current += n + 1;
    }
    return current;
}

/* string-ref.
 * If POS is out of range,
 *   - returns SCM_CHAR_INVALID if range_error is FALSE
 *   - raise error otherwise.
 * This differs from Scheme version, which takes an optional 'fallback'
 * argument which will be returned when POS is out-of-range.  We can't
 * have the same semantics since the return type is limited.
 */
ScmChar Scm_StringRef(ScmString *str, int pos, int range_error)
{
    const ScmStringBody *b = SCM_STRING_BODY(str);
    int len = SCM_STRING_BODY_LENGTH(b);

    /* we can't allow string-ref on incomplete strings, since it may yield
       invalid character object. */
    if (SCM_STRING_BODY_INCOMPLETE_P(b)) {
        Scm_Error("incomplete string not allowed : %S", str);
    }
    if (pos < 0 || pos >= len) {
        if (range_error) {
            Scm_Error("argument out of range: %d", pos);
        } else {
            return SCM_CHAR_INVALID;
        }
    }
    if (SCM_STRING_BODY_SINGLE_BYTE_P(b)) {
        return (ScmChar)(((unsigned char *)SCM_STRING_BODY_START(b))[pos]);
    } else {
        const char *p = forward_pos(SCM_STRING_BODY_START(b), pos);
        ScmChar c;
        SCM_CHAR_GET(p, c);
        return c;
    }
}

/* The meaning and rationale of range_error is the same as Scm_StringRef.
 * Returns -1 if OFFSET is out-of-range and RANGE_ERROR is FALSE.
 * (Because of this, the return type is not ScmByte but int.
 */
int Scm_StringByteRef(ScmString *str, int offset, int range_error)
{
    const ScmStringBody *b = SCM_STRING_BODY(str);
    if (offset < 0 || offset >= (int)SCM_STRING_BODY_SIZE(b)) {
        if (range_error) {
            Scm_Error("argument out of range: %d", offset);
        } else {
            return -1;
        }
    }
    return (ScmByte)SCM_STRING_BODY_START(b)[offset];
}

/* External interface of forward_pos.  Returns the pointer to the
   offset-th character in str. */
/* NB: this function allows offset == length of the string; in that
   case, the return value points the location past the string body,
   but it is necessary sometimes to do a pointer arithmetic with the
   returned values. */
const char *Scm_StringBodyPosition(const ScmStringBody *b, int offset)
{
    if (offset < 0 || offset > (int)SCM_STRING_BODY_LENGTH(b)) {
        Scm_Error("argument out of range: %d", offset);
    }
    if (SCM_STRING_BODY_INCOMPLETE_P(b)) {
        return (SCM_STRING_BODY_START(b)+offset);
    } else {
        return (forward_pos(SCM_STRING_BODY_START(b), offset));
    }
}

/* This is old API and now DEPRECATED.  It's difficult to use this safely,
   since you don't have a way to get the string length consistent at the
   moment you call this function.   Use Scm_StringBodyPosition instead. */
const char *Scm_StringPosition(ScmString *str, int offset)
{
    return Scm_StringBodyPosition(SCM_STRING_BODY(str), offset);
}

/*----------------------------------------------------------------
 * Concatenation
 */

ScmObj Scm_StringAppend2(ScmString *x, ScmString *y)
{
    const ScmStringBody *xb = SCM_STRING_BODY(x);
    const ScmStringBody *yb = SCM_STRING_BODY(y);
    int sizex = SCM_STRING_BODY_SIZE(xb), lenx = SCM_STRING_BODY_LENGTH(xb);
    int sizey = SCM_STRING_BODY_SIZE(yb), leny = SCM_STRING_BODY_LENGTH(yb);
    int flags = 0;
    char *p = SCM_NEW_ATOMIC2(char *,sizex + sizey + 1);

    memcpy(p, xb->start, sizex);
    memcpy(p+sizex, yb->start, sizey);
    p[sizex + sizey] = '\0';
    flags |= SCM_STRING_TERMINATED;

    if (SCM_STRING_BODY_INCOMPLETE_P(xb) || SCM_STRING_BODY_INCOMPLETE_P(yb)) {
        flags |= SCM_STRING_INCOMPLETE; /* yields incomplete string */
    }
    return SCM_OBJ(make_str(lenx+leny, sizex+sizey, p, flags));
}

ScmObj Scm_StringAppendC(ScmString *x, const char *str, int sizey, int leny)
{
    const ScmStringBody *xb = SCM_STRING_BODY(x);
    int sizex = SCM_STRING_BODY_SIZE(xb), lenx = SCM_STRING_BODY_LENGTH(xb);
    int flags = 0;
    char *p;

    if (sizey < 0) count_size_and_length(str, &sizey, &leny);
    else if (leny < 0) leny = count_length(str, sizey);

    p = SCM_NEW_ATOMIC2(char *, sizex + sizey + 1);
    memcpy(p, xb->start, sizex);
    memcpy(p+sizex, str, sizey);
    p[sizex+sizey] = '\0';
    flags |= SCM_STRING_TERMINATED;

    if (SCM_STRING_BODY_INCOMPLETE_P(xb) || leny < 0) {
        flags |= SCM_STRING_INCOMPLETE;
    }
    return SCM_OBJ(make_str(lenx + leny, sizex + sizey, p, flags));
}

ScmObj Scm_StringAppend(ScmObj strs)
{
#define BODY_ARRAY_SIZE 32
    ScmObj cp;
    int size = 0, len = 0, flags = 0, numstrs, i;
    char *buf, *bufp;
    const ScmStringBody *bodies_s[BODY_ARRAY_SIZE], **bodies;

    /* It is trickier than it appears, since the strings may be modified
       by another thread during we're dealing with it.  So in the first
       pass to sum up the lenghts of strings, we extract the string bodies
       and save it.  */
    numstrs = Scm_Length(strs);
    if (numstrs < 0) Scm_Error("improper list not allowed: %S", strs);
    if (numstrs > BODY_ARRAY_SIZE) {
        bodies = SCM_NEW_ARRAY(const ScmStringBody*, numstrs);
    } else {
        bodies = bodies_s;
    }

    i=0;
    SCM_FOR_EACH(cp, strs) {
        const ScmStringBody *b;
        if (!SCM_STRINGP(SCM_CAR(cp))) {
            Scm_Error("string required, but got %S\n", SCM_CAR(cp));
        }
        b = SCM_STRING_BODY(SCM_CAR(cp));
        size += SCM_STRING_BODY_SIZE(b);
        len += SCM_STRING_BODY_LENGTH(b);
        if (SCM_STRING_BODY_INCOMPLETE_P(b)) {
            flags |= SCM_STRING_INCOMPLETE;
        }
        bodies[i++] = b;
    }

    bufp = buf = SCM_NEW_ATOMIC2(char *, size+1);
    for (i=0; i<numstrs; i++) {
        const ScmStringBody *b = bodies[i];
        memcpy(bufp, SCM_STRING_BODY_START(b), SCM_STRING_BODY_SIZE(b));
        bufp += SCM_STRING_BODY_SIZE(b);
    }
    *bufp = '\0';
    bodies = NULL;              /* to help GC */
    flags |= SCM_STRING_TERMINATED;
    return SCM_OBJ(make_str(len, size, buf, flags));
#undef BODY_ARRAY_SIZE
}

ScmObj Scm_StringJoin(ScmObj strs, ScmString *delim, int grammer)
{
#define BODY_ARRAY_SIZE 32
    ScmObj cp;
    int size = 0, len = 0, nstrs, ndelim, i, flags = 0;
    int dsize, dlen;            /* for delimiter string */
    const ScmStringBody *bodies_s[BODY_ARRAY_SIZE], **bodies;
    const ScmStringBody *dbody;
    char *buf, *bufp;

    nstrs = Scm_Length(strs);
    if (nstrs < 0) Scm_Error("improper list not allowed: %S", strs);
    if (nstrs == 0) {
        if (grammer == SCM_STRING_JOIN_STRICT_INFIX) {
            Scm_Error("can't join empty list of strings with strict-infix grammer");
        }
        return SCM_MAKE_STR("");
    }

    if (nstrs > BODY_ARRAY_SIZE) {
        bodies = SCM_NEW_ARRAY(const ScmStringBody *, nstrs);
    } else {
        bodies = bodies_s;
    }

    dbody = SCM_STRING_BODY(delim);
    dsize = SCM_STRING_BODY_SIZE(dbody);
    dlen  = SCM_STRING_BODY_LENGTH(dbody);
    if (SCM_STRING_BODY_INCOMPLETE_P(dbody)) {
        flags |= SCM_STRING_INCOMPLETE;
    }

    i = 0;
    SCM_FOR_EACH(cp, strs) {
        const ScmStringBody *b;
        if (!SCM_STRINGP(SCM_CAR(cp))) {
            Scm_Error("string required, but got %S\n", SCM_CAR(cp));
        }
        b = SCM_STRING_BODY(SCM_CAR(cp));
        size += SCM_STRING_BODY_SIZE(b);
        len  += SCM_STRING_BODY_LENGTH(b);
        if (SCM_STRING_BODY_INCOMPLETE_P(b)) {
            flags |= SCM_STRING_INCOMPLETE;
        }
        bodies[i++] = b;
    }
    if (grammer == SCM_STRING_JOIN_INFIX
        || grammer == SCM_STRING_JOIN_STRICT_INFIX) {
        ndelim = nstrs - 1;
    } else {
        ndelim = nstrs;
    }
    size += dsize * ndelim;
    len += dlen * ndelim;

    bufp = buf = SCM_NEW_ATOMIC2(char *, size+1);
    if (grammer == SCM_STRING_JOIN_PREFIX) {
        memcpy(bufp, SCM_STRING_BODY_START(dbody), dsize);
        bufp += dsize;
    }
    for (i=0; i<nstrs; i++) {
        const ScmStringBody *b = bodies[i];
        memcpy(bufp, SCM_STRING_BODY_START(b), SCM_STRING_BODY_SIZE(b));
        bufp += SCM_STRING_BODY_SIZE(b);
        if (i < nstrs-1) {
            memcpy(bufp, SCM_STRING_BODY_START(dbody), dsize);
            bufp += dsize;
        }
    }
    if (grammer == SCM_STRING_JOIN_SUFFIX) {
        memcpy(bufp, SCM_STRING_BODY_START(dbody), dsize);
        bufp += dsize;
    }
    *bufp = '\0';
    bodies = NULL;              /* to help GC */
    flags |= SCM_STRING_TERMINATED;
    return SCM_OBJ(make_str(len, size, buf, flags));
#undef BODY_ARRAY_SIZE
}

/*----------------------------------------------------------------
 * Mutation
 */

/*
 * String mutation is extremely heavy operation in Gauche,
 * and only provided for compatibility to RnRS.  At C API level
 * there's no point in using string mutation at all.  A single
 * API, which replaces the string body, is provided at C level.
 */

ScmObj Scm_StringReplaceBody(ScmString *str, const ScmStringBody *newbody)
{
    if (SCM_STRING_IMMUTABLE_P(str)) {
        Scm_Error("attempted to modify an immutable string: %S", str);
    }

    /* Atomically replaces the str's body (no MT hazard) */
    str->body = newbody;

    /* TODO: If the initialBody of str isn't shared,
       nullify str->initialBody.start so that the original string is
       GCed.  It should be done after implementing 'shared' flag
       into the string body. */
    return SCM_OBJ(str);
}

/*----------------------------------------------------------------
 * Substring
 */

static ScmObj substring(const ScmStringBody *xb, int start, int end,
                        int byterange)
{
    int len = byterange? SCM_STRING_BODY_SIZE(xb) : SCM_STRING_BODY_LENGTH(xb);
    int flags = SCM_STRING_BODY_FLAGS(xb) & ~SCM_STRING_IMMUTABLE;
    SCM_CHECK_START_END(start, end, len);

    if (SCM_STRING_BODY_SINGLE_BYTE_P(xb) || byterange) {
        if (end != len) flags &= ~SCM_STRING_TERMINATED;
        if (byterange)  flags |= SCM_STRING_INCOMPLETE;
        return SCM_OBJ(make_str(end-start,
                                end-start,
                                SCM_STRING_BODY_START(xb) + start,
                                flags));
    } else {
        const char *s, *e;
        if (start) s = forward_pos(SCM_STRING_BODY_START(xb), start);
        else s = SCM_STRING_BODY_START(xb);
        if (len == end) {
            e = SCM_STRING_BODY_START(xb) + SCM_STRING_BODY_SIZE(xb);
        } else {
            e = forward_pos(s, end - start);
            flags &= ~SCM_STRING_TERMINATED;
        }
        return SCM_OBJ(make_str((int)(end - start), (int)(e - s), s, flags));
    }
}

ScmObj Scm_Substring(ScmString *x, int start, int end, int byterange)
{
    return substring(SCM_STRING_BODY(x), start, end, byterange);
}

/* Auxiliary procedure to support optional start/end parameter specified
   in lots of SRFI-13 functions.   If start and end is specified and restricts
   string range, call substring.  Otherwise returns x itself. */
ScmObj Scm_MaybeSubstring(ScmString *x, ScmObj start, ScmObj end)
{
    int istart, iend;
    const ScmStringBody *xb = SCM_STRING_BODY(x);
    if (SCM_UNBOUNDP(start) || SCM_UNDEFINEDP(start) || SCM_FALSEP(start)) {
        istart = 0;
    } else {
        if (!SCM_INTP(start))
            Scm_Error("exact integer required for start, but got %S", start);
        istart = SCM_INT_VALUE(start);
    }

    if (SCM_UNBOUNDP(end) || SCM_UNDEFINEDP(end) || SCM_FALSEP(end)) {
        if (istart == 0) return SCM_OBJ(x);
        iend = SCM_STRING_BODY_LENGTH(xb);
    } else {
        if (!SCM_INTP(end))
            Scm_Error("exact integer required for start, but got %S", end);
        iend = SCM_INT_VALUE(end);
    }
    return substring(xb, istart, iend, FALSE);
}

/*----------------------------------------------------------------
 * Search & parse
 */

/* Split string by char.  Char itself is not included in the result. */
/* TODO: fix semantics.  What should be returned for (string-split "" #\.)? */
ScmObj Scm_StringSplitByChar(ScmString *str, ScmChar ch)
{
    const ScmStringBody *strb = SCM_STRING_BODY(str);
    int size = SCM_STRING_BODY_SIZE(strb), sizecnt = 0;
    int lencnt = 0;
    const char *s = SCM_STRING_BODY_START(strb), *p = s, *e = s + size;
    ScmObj head = SCM_NIL, tail = SCM_NIL;

    if (SCM_STRING_BODY_INCOMPLETE_P(strb)) {
        /* TODO: fix the policy of handling incomplete string */
        Scm_Error("incomplete string not accepted: %S", str);
    }

    while (p < e) {
        ScmChar cc;
        int ncc;

        SCM_CHAR_GET(p, cc);
        ncc = SCM_CHAR_NBYTES(cc);
        if (ch == cc) {
            SCM_APPEND1(head, tail, Scm_MakeString(s, sizecnt, lencnt, 0));
            sizecnt = lencnt = 0;
            p += ncc;
            s = p;
        } else {
            p += ncc;
            sizecnt += ncc;
            lencnt ++;
        }
    }
    SCM_APPEND1(head, tail, Scm_MakeString(s, sizecnt, lencnt, 0));
    return head;
}

/* Boyer-Moore string search.  assuming siz1 > siz2, siz2 < 256. */
static int boyer_moore(const char *ss1, int siz1,
                       const char *ss2, int siz2)
{
    unsigned char shift[256];
    int i, j, k;
    for (i=0; i<256; i++) { shift[i] = siz2; }
    for (j=0; j<siz2-1; j++) {
        shift[(unsigned char)ss2[j]] = siz2-j-1;
    }
    for (i=siz2-1; i<siz1; i+=shift[(unsigned char)ss1[i]]) {
        for (j=siz2-1, k = i; j>=0 && ss1[k] == ss2[j]; j--, k--)
            ;
        if (j == -1) return k+1;
    }
    return -1;
}

static int boyer_moore_reverse(const char *ss1, int siz1,
                               const char *ss2, int siz2)
{
    unsigned char shift[256];
    int i, j, k;
    for (i=0; i<256; i++) { shift[i] = siz2; }
    for (j=siz2-1; j>0; j--) {
        shift[(unsigned char)ss2[j]] = j;
    }
    for (i=siz1-siz2+1; i>=0; i-=shift[(unsigned char)ss1[i]]) {
        for (j=0, k = i; j<siz2 && ss1[k] == ss2[j]; j++, k++)
            ;
        if (j == siz2) return i;
    }
    return -1;
}

/* A primitive routine to search a substring s2 in s1.
   Returns TRUE iff found.  Calculates both byte index and character index.
   If both strings are single-byte and s1 is long, we use Boyer-Moore.
 */
static int string_search(const char *s1, int siz1, int len1,
                         const char *s2, int siz2, int len2,
                         int *bi /* out */,
                         int *ci /* out */)
{
    int i;

    if (siz2 == 0) {
        *bi = *ci = 0;
        return TRUE;
    }

    if (siz1 == len1) {
        if (siz2 == len2) {
            /* short cut for single-byte strings */
            if (siz1 < siz2) return FALSE;
            if (siz1 < 256 || siz2 >= 256) {
                /* brute-force search */
                for (i=0; i<=siz1-siz2; i++) {
                    if (memcmp(s2, s1+i, siz2) == 0) break;
                }
                if (i == siz1-siz2+1) return FALSE;
            } else {
                i = boyer_moore(s1, siz1, s2, siz2);
                if (i < 0) return FALSE;
            }
            *bi = *ci = i;
            return TRUE;
        } else {
            return FALSE;       /* sbstring can't contain mbstring. */
        }
    }
    if (len1 >= len2) {
        const char *sp = s1;
        for (i=0; i<=len1-len2; i++) {
            if (memcmp(sp, s2, siz2) == 0) {
                *bi = (int)(sp - s1);
                *ci = i;
                return TRUE;
            }
            sp += SCM_CHAR_NFOLLOWS(*sp) + 1;
        }
    }
    return FALSE;
}

static int string_search_reverse(const char *s1, int siz1, int len1,
                                 const char *s2, int siz2, int len2,
                                 int *bi /* out */,
                                 int *ci /* out */)
{
    int i;

    if (siz2 == 0) {
        *bi = siz1;
        *ci = len1;
        return TRUE;
    }

    if (siz1 == len1) {
        if (siz2 == len2) {
            /* short cut for single-byte strings */
            if (siz1 < siz2) return FALSE;
            if (siz1 < 256 || siz2 >= 256) {
                /* brute-force search */
                for (i=siz1-siz2-1; i>=0; i--) {
                    if (memcmp(s2, s1+i, siz2) == 0) break;
                }
                if (i == 0) return FALSE;
            } else {
                i = boyer_moore_reverse(s1, siz1, s2, siz2);
                if (i < 0) return FALSE;
            }
            *bi = *ci = i;
            return TRUE;
        } else {
            return FALSE;       /* sbstring can't contain mbstring. */
        }
    }
    if (len1 >= len2) {
        const char *sp = s1 + siz1, *p;
        for (i=0; i<len2; i++) {
            SCM_CHAR_BACKWARD(sp, s1, p);
            SCM_ASSERT(*p);
            sp = p;
        }
        for (i=len1-len2; i>=0; i--) {
            if (memcmp(sp, s2, siz2) == 0) {
                *bi = (int)(sp - s1);
                *ci = i;
                return TRUE;
            }
            SCM_CHAR_BACKWARD(sp, s1, p);
            sp = p;
        }
    }
    return FALSE;
}

/* Scan s2 in s1, and calculates appropriate return value(s) according to       
   retmode.

   SCM_STRING_SCAN_INDEX  : return the index of s1
        s1 = "abcde" and s2 = "cd" => 2
   SCM_STRING_SCAN_BEFORE : return substring of s1 before s2
        s1 = "abcde" and s2 = "cd" => "ab"
   SCM_STRING_SCAN_AFTER  : return substring of s1 after s2
        s1 = "abcde" and s2 = "cd" => "e"
   SCM_STRING_SCAN_BEFORE2 : return substring of s1 before s2, and rest
       s1 = "abcde" and s2 = "cd" => "ab" and "cde"
   SCM_STRING_SCAN_AFTER2 : return substring of s1 up to s2 and rest
       s1 = "abcde" and s2 = "cd" => "abcd" and "e"
   SCM_STRING_SCAN_BOTH   : return substring of s1 before and after s2
       s1 = "abcde" and s2 = "cd" => "ab" and "e"
*/
static ScmObj string_scan(ScmString *ss1, const char *s2,
                          int siz2, int len2, int incomplete2,
                          int retmode,
                          int (*searcher)(const char*, int, int,
                                          const char*, int, int, int*, int*))
{
    int bi, ci, incomplete;
    const ScmStringBody *sb = SCM_STRING_BODY(ss1);
    const char *s1 = SCM_STRING_BODY_START(sb);
    int siz1 = SCM_STRING_BODY_SIZE(sb);
    int len1 = SCM_STRING_BODY_LENGTH(sb);

    if (retmode < 0 || retmode > SCM_STRING_SCAN_BOTH) {
        Scm_Error("return mode out fo range: %d", retmode);
    }

    if (!searcher(s1, siz1, len1, s2, siz2, len2, &bi, &ci)) {
        if (retmode <= SCM_STRING_SCAN_AFTER) {
            return SCM_FALSE;
        } else {
            return Scm_Values2(SCM_FALSE, SCM_FALSE);
        }
    }

    incomplete =
        (SCM_STRING_BODY_INCOMPLETE_P(sb) || incomplete2)?
        SCM_STRING_INCOMPLETE : 0;
    
    switch (retmode) {
    case SCM_STRING_SCAN_INDEX:
        return Scm_MakeInteger(ci);
    case SCM_STRING_SCAN_BEFORE:
        return Scm_MakeString(s1, bi, ci, incomplete);
    case SCM_STRING_SCAN_AFTER:
        return Scm_MakeString(s1+bi+siz2, siz1-bi-siz2,
                              len1-ci-len2, incomplete);
    case SCM_STRING_SCAN_BEFORE2:
        return Scm_Values2(Scm_MakeString(s1, bi, ci, incomplete),
                           Scm_MakeString(s1+bi, siz1-bi, len1-ci, incomplete));
    case SCM_STRING_SCAN_AFTER2:
        return Scm_Values2(Scm_MakeString(s1, bi+siz2, ci+len2, incomplete),
                           Scm_MakeString(s1+bi+siz2, siz1-bi-siz2,
                                          len1-ci-len2, incomplete));
    case SCM_STRING_SCAN_BOTH:
        return Scm_Values2(Scm_MakeString(s1, bi, ci, incomplete),
                           Scm_MakeString(s1+bi+siz2, siz1-bi-siz2,
                                          len1-ci-len2, incomplete));
    }
}

ScmObj Scm_StringScan(ScmString *s1, ScmString *s2, int retmode)
{
    const ScmStringBody *s2b = SCM_STRING_BODY(s2);
    return string_scan(s1,
                       SCM_STRING_BODY_START(s2b),
                       SCM_STRING_BODY_SIZE(s2b),
                       SCM_STRING_BODY_LENGTH(s2b),
                       SCM_STRING_BODY_INCOMPLETE_P(s2b),
                       retmode, string_search);
}

ScmObj Scm_StringScanChar(ScmString *s1, ScmChar ch, int retmode)
{
    char buf[SCM_CHAR_MAX_BYTES];
    SCM_CHAR_PUT(buf, ch);
    return string_scan(s1, buf, SCM_CHAR_NBYTES(ch), 1, FALSE, retmode,
                       string_search);
}

ScmObj Scm_StringScanRight(ScmString *s1, ScmString *s2, int retmode)
{
    const ScmStringBody *s2b = SCM_STRING_BODY(s2);
    return string_scan(s1,
                       SCM_STRING_BODY_START(s2b),
                       SCM_STRING_BODY_SIZE(s2b),
                       SCM_STRING_BODY_LENGTH(s2b),
                       SCM_STRING_BODY_INCOMPLETE_P(s2b),
                       retmode, string_search_reverse);
}

ScmObj Scm_StringScanCharRight(ScmString *s1, ScmChar ch, int retmode)
{
    char buf[SCM_CHAR_MAX_BYTES];
    SCM_CHAR_PUT(buf, ch);
    return string_scan(s1, buf, SCM_CHAR_NBYTES(ch), 1, FALSE, retmode,
                       string_search_reverse);
}

/*----------------------------------------------------------------
 * Miscellaneous functions
 */

ScmObj Scm_StringToList(ScmString *str)
{
    const ScmStringBody *b = SCM_STRING_BODY(str);
    ScmObj start = SCM_NIL, end = SCM_NIL;
    const char *bufp = SCM_STRING_BODY_START(b);
    int len = SCM_STRING_BODY_LENGTH(b);
    ScmChar ch;

    if (SCM_STRING_BODY_INCOMPLETE_P(b))
        Scm_Error("incomplete string not supported: %S", str);
    while (len-- > 0) {
        SCM_CHAR_GET(bufp, ch);
        bufp += SCM_CHAR_NBYTES(ch);
        SCM_APPEND1(start, end, SCM_MAKE_CHAR(ch));
    }
    return start;
}

/* Convert cstring array to a list of Scheme strings.  Cstring array
   can be NULL terminated (in case size < 0) or its size is explicitly
   specified (size >= 0).  FLAGS is passed to Scm_MakeString. */
ScmObj Scm_CStringArrayToList(const char **array, int size, int flags)
{
    int i;
    ScmObj h = SCM_NIL, t = SCM_NIL;
    if (size < 0) {
        for (;*array; array++) {
            ScmObj s = Scm_MakeString(*array, -1, -1, flags);
            SCM_APPEND1(h, t, s);
        }
    } else {
        for (i=0; i<size; i++, array++) {
            ScmObj s = Scm_MakeString(*array, -1, -1, flags);
            SCM_APPEND1(h, t, s);
        }
    }
    return h;
}

/* common routine for Scm_ListTo[Const]CStringArray */
static int list_to_cstring_array_check(ScmObj lis, int errp)
{
    ScmObj lp;
    int len = 0;
    SCM_FOR_EACH(lp, lis) {
        if (!SCM_STRINGP(SCM_CAR(lp))) {
            if (errp) Scm_Error("a proper list of strings is required, but the list contains non-string element: %S", SCM_CAR(lp));
            else return -1;
        }
        len++;
    }
    return len;
}

/* Convert list of Scheme strings into C const char* string array, NULL
   terminated.
   If errp == FALSE, returns NULL on error.
   otherwise, signals an error. */
const char **Scm_ListToConstCStringArray(ScmObj lis, int errp)
{
    ScmObj lp;
    const char **array, **p;
    int len = list_to_cstring_array_check(lis, errp);
    if (len < 0) return NULL;
    p = array = SCM_NEW_ARRAY(const char*, len+1);
    SCM_FOR_EACH(lp, lis) {
        *p++ = Scm_GetStringConst(SCM_STRING(SCM_CAR(lp)));
    }
    *p = NULL;                  /* termination */
    return array;
}

/* Convert list of Scheme strings into C char* string array, NULL
   terminated.
   If errp == FALSE, returns NULL on error.
   otherwise, signals an error.
   If provided, alloc is used to allocate both a pointer array and char
   arrays.  Otherwise, SCM_ALLOC is used. */
char **Scm_ListToCStringArray(ScmObj lis, int errp, void *(*alloc)(size_t))
{
    ScmObj lp;
    char **array, **p;
    int len = list_to_cstring_array_check(lis, errp);
    if (len < 0) return NULL;

    if (alloc) {
        p = array = (char **)alloc((len+1) * sizeof(char *));
        SCM_FOR_EACH(lp, lis) {
            const char *s = Scm_GetStringConst(SCM_STRING(SCM_CAR(lp)));
            *p = (char *)alloc(strlen(s) + 1);
            strcpy(*p, s);
            p++;
        }
    } else {
        p = array = SCM_NEW_ARRAY(char*, len+1);
        SCM_FOR_EACH(lp, lis) {
            *p++ = Scm_GetString(SCM_STRING(SCM_CAR(lp)));
        }
    }
    *p = NULL;                  /* termination */
    return array;
}

/*----------------------------------------------------------------
 * printer
 */
static inline void string_putc(ScmChar ch, ScmPort *port, int bytemode)
{
    char buf[5];
    switch (ch) {
    case '\\': SCM_PUTZ("\\\\", -1, port); break;
    case '"':  SCM_PUTZ("\\\"", -1, port); break;
    case '\n': SCM_PUTZ("\\n", -1, port); break;
    case '\t': SCM_PUTZ("\\t", -1, port); break;
    case '\r': SCM_PUTZ("\\r", -1, port); break;
    case '\f': SCM_PUTZ("\\f", -1, port); break;
    case '\0': SCM_PUTZ("\\0", -1, port); break;
    default:
        if (ch < ' ' || ch == 0x7f || (bytemode && ch >= 0x80)) {
            snprintf(buf, 5, "\\x%02x", (unsigned char)ch);
            SCM_PUTZ(buf, -1, port);
        } else {
            SCM_PUTC(ch, port);
        }
    }
}

static void string_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmString *str = SCM_STRING(obj);
    if (SCM_WRITE_MODE(ctx) == SCM_WRITE_DISPLAY) {
        SCM_PUTS(str, port);
    } else {
        const ScmStringBody *b = SCM_STRING_BODY(str);
        if (SCM_STRING_BODY_SINGLE_BYTE_P(b)) {
            const char *cp = SCM_STRING_BODY_START(b);
            int size = SCM_STRING_BODY_SIZE(b);
            if (SCM_STRING_BODY_INCOMPLETE_P(b)) {
                SCM_PUTZ("#*\"", -1, port);
            } else {
                SCM_PUTC('"', port);
            }
            while (size--) {
                string_putc(*cp++, port, SCM_STRING_BODY_INCOMPLETE_P(b));
            }
        } else {
            ScmChar ch;
            const char *cp = SCM_STRING_BODY_START(b);
            int len = SCM_STRING_BODY_LENGTH(b);

            SCM_PUTC('"', port);
            while (len--) {
                SCM_CHAR_GET(cp, ch);
                string_putc(ch, port, FALSE);
                cp += SCM_CHAR_NBYTES(ch);
            }
        }
        SCM_PUTC('"', port);
    }
}

/*==================================================================
 *
 * String pointer
 *
 */

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_StringPointerClass, NULL);

ScmObj Scm_MakeStringPointer(ScmString *src, int index, int start, int end)
{
    const ScmStringBody *srcb = SCM_STRING_BODY(src);
    int len = SCM_STRING_BODY_LENGTH(srcb);
    int effective_size;
    const char *sptr, *ptr, *eptr;
    ScmStringPointer *sp;

    SCM_CHECK_START_END(start, end, len);
    while (index < 0) index += (end - start) + 1;
    if (index > (end - start)) goto badindex;

    if (SCM_STRING_BODY_SINGLE_BYTE_P(srcb)) {
        sptr = SCM_STRING_BODY_START(srcb) + start;
        ptr = sptr + index;
        effective_size = end - start;
    } else {
        sptr = forward_pos(SCM_STRING_BODY_START(srcb), start);
        ptr = forward_pos(sptr, index);
        if (end == len) {
            eptr = SCM_STRING_BODY_START(srcb) + SCM_STRING_BODY_SIZE(srcb);
        } else {
            eptr = forward_pos(sptr, end - start);
        }
        effective_size = (int)(eptr - ptr);
    }
    sp = SCM_NEW(ScmStringPointer);
    SCM_SET_CLASS(sp, SCM_CLASS_STRING_POINTER);
    sp->length = (SCM_STRING_BODY_INCOMPLETE_P(srcb)? -1 : (end-start));
    sp->size = effective_size;
    sp->start = sptr;
    sp->index = index;
    sp->current = ptr;
    return SCM_OBJ(sp);
  badindex:
    Scm_Error("index out of range: %d", index);
    return SCM_UNDEFINED;
}

ScmObj Scm_StringPointerRef(ScmStringPointer *sp)
{
    ScmChar ch;
    if (sp->length < 0 || sp->size == sp->length) {
        if (sp->index >= sp->size) return SCM_EOF;
        ch = *sp->current;
    } else {
        if (sp->index >= sp->length) return SCM_EOF;
        SCM_CHAR_GET(sp->current, ch);
    }
    return SCM_MAKE_CHAR(ch);
}

ScmObj Scm_StringPointerNext(ScmStringPointer *sp)
{
    ScmChar ch;
    if (sp->length < 0 || sp->size == sp->length) {
        if (sp->index >= sp->size) return SCM_EOF;
        sp->index++;
        ch = *sp->current++;
    } else {
        if (sp->index >= sp->length) return SCM_EOF;
        SCM_CHAR_GET(sp->current, ch);
        sp->index++;
        sp->current += SCM_CHAR_NFOLLOWS(*sp->current) + 1;
    }
    return SCM_MAKE_CHAR(ch);
}

ScmObj Scm_StringPointerPrev(ScmStringPointer *sp)
{
    ScmChar ch;
    if (sp->index <= 0) return SCM_EOF;
    if (sp->length < 0 || sp->size == sp->length) {
        sp->index--;
        ch = *--sp->current;
    } else {
        const char *prev;
        SCM_CHAR_BACKWARD(sp->current, sp->start, prev);
        SCM_ASSERT(prev != NULL);
        SCM_CHAR_GET(prev, ch);
        sp->index--;
        sp->current = prev;
    }
    return SCM_MAKE_CHAR(ch);
}

ScmObj Scm_StringPointerSet(ScmStringPointer *sp, int index)
{
    if (index < 0) goto badindex;
    if (sp->length < 0 || sp->size == sp->length) {
        if (index > sp->size) goto badindex;
        sp->index = index;
        sp->current = sp->start + index;
    } else {
        if (index > sp->length) goto badindex;
        sp->index = index;
        sp->current = forward_pos(sp->start, index);
    }
    return SCM_OBJ(sp);
  badindex:
    Scm_Error("index out of range: %d", index);
    return SCM_UNDEFINED;
}

ScmObj Scm_StringPointerSubstring(ScmStringPointer *sp, int afterp)
{
    /* TODO: set SCM_STRING_TERMINATED if applicable. */
    if (sp->length < 0) {
        if (afterp)
            return SCM_OBJ(make_str(-1, sp->size - sp->index, sp->current, 0));
        else
            return SCM_OBJ(make_str(-1, sp->index, sp->start, 0));
    } else {
        if (afterp)
            return SCM_OBJ(make_str(sp->length - sp->index,
                                    (int)(sp->start + sp->size - sp->current),
                                    sp->current, 0));
        else
            return SCM_OBJ(make_str(sp->index,
                                    (int)(sp->current - sp->start),
                                    sp->start, 0));
    }
}

/* Copy string pointer.
   Thanks to Alex Shinn (foof@synthcode.com) */
ScmObj Scm_StringPointerCopy(ScmStringPointer *sp1)
{
    ScmStringPointer *sp2 = SCM_NEW(ScmStringPointer);
    SCM_SET_CLASS(sp2, SCM_CLASS_STRING_POINTER);
    sp2->length  = sp1->length;
    sp2->size    = sp1->size;
    sp2->start   = sp1->start;
    sp2->index   = sp1->index;
    sp2->current = sp1->current;
    return SCM_OBJ(sp2);
}

/* Dump string pointer info for debugging.
   Thanks to Alex Shinn (foof@synthcode.com) */
void Scm_StringPointerDump(ScmStringPointer *sp1)
{
    Scm_Printf(SCM_CUROUT,
               "<sp addr: %p len: %d size: %d start: %p index: %d cur: %d>\n",
               sp1, sp1->length, sp1->size, sp1->start, sp1->index,
               sp1->current);
}

/*==================================================================
 *
 * Dynamic strings
 *
 */

/* I used to use realloc() to grow the storage; now I avoid it, for
   Boehm GC's realloc almost always copies the original content and
   we don't get any benefit.
   The growing string is kept in the chained chunks.  The size of
   chunk getting bigger as the string grows, until a certain threshold.
   The memory for actual chunks and the chain is allocated separately,
   in order to use SCM_NEW_ATOMIC.
 */

/* NB: it is important that DString functions don't call any
 * time-consuming procedures except memory allocation.   Some of
 * mutex code in other parts relies on that fact.
 */

/* maximum chunk size */
#define DSTRING_MAX_CHUNK_SIZE  8180

void Scm_DStringInit(ScmDString *dstr)
{
    dstr->init.bytes = 0;
    dstr->anchor = dstr->tail = NULL;
    dstr->current = dstr->init.data;
    dstr->end = dstr->current + SCM_DSTRING_INIT_CHUNK_SIZE;
    dstr->lastChunkSize = SCM_DSTRING_INIT_CHUNK_SIZE;
    dstr->length = 0;
}

int Scm_DStringSize(ScmDString *dstr)
{
    ScmDStringChain *chain;
    int size;
    if (dstr->tail) {
        size = dstr->init.bytes;
        dstr->tail->chunk->bytes = (int)(dstr->current - dstr->tail->chunk->data);
        for (chain = dstr->anchor; chain; chain = chain->next) {
            size += chain->chunk->bytes;
        }
    } else {
        size = (int)(dstr->current - dstr->init.data);
    }
    return size;
}

void Scm__DStringRealloc(ScmDString *dstr, int minincr)
{
    ScmDStringChunk *newchunk;
    ScmDStringChain *newchain;
    int newsize;

    /* sets the byte count of the last chunk */
    if (dstr->tail) {
        dstr->tail->chunk->bytes = (int)(dstr->current - dstr->tail->chunk->data);
    } else {
        dstr->init.bytes = (int)(dstr->current - dstr->init.data);
    }

    /* determine the size of the new chunk.  the increase factor 3 is
       somewhat arbitrary, determined by rudimental benchmarking. */
    newsize = dstr->lastChunkSize * 3;
    if (newsize > DSTRING_MAX_CHUNK_SIZE) {
        newsize = DSTRING_MAX_CHUNK_SIZE;
    }
    if (newsize < minincr) {
        newsize = minincr;
    }

    newchunk = SCM_NEW_ATOMIC2(ScmDStringChunk*,
                               sizeof(ScmDStringChunk)+newsize-SCM_DSTRING_INIT_CHUNK_SIZE);
    newchunk->bytes = 0;

    newchain = SCM_NEW(ScmDStringChain);

    newchain->next = NULL;
    newchain->chunk = newchunk;
    if (dstr->tail) {
        dstr->tail->next = newchain;
        dstr->tail = newchain;
    } else {
        dstr->anchor = dstr->tail = newchain;
    }
    dstr->current = newchunk->data;
    dstr->end = newchunk->data + newsize;
    dstr->lastChunkSize = newsize;
}

/* Retrieve accumulated string. */
static const char *dstring_getz(ScmDString *dstr, int *plen, int *psiz)
{
    int size, len;
    char *buf;
    if (dstr->anchor == NULL) {
        /* we only have one chunk */
        size = (int)(dstr->current - dstr->init.data);
        len = dstr->length;
        buf = SCM_NEW_ATOMIC2(char*, size+1);
        memcpy(buf, dstr->init.data, size);
        buf[size] = '\0';
    } else {
        ScmDStringChain *chain = dstr->anchor;
        char *bptr;

        size = Scm_DStringSize(dstr);
        len = dstr->length;
        bptr = buf = SCM_NEW_ATOMIC2(char*, size+1);

        memcpy(bptr, dstr->init.data, dstr->init.bytes);
        bptr += dstr->init.bytes;
        for (; chain; chain = chain->next) {
            memcpy(bptr, chain->chunk->data, chain->chunk->bytes);
            bptr += chain->chunk->bytes;
        }
        *bptr = '\0';
    }
    if (len < 0) len = count_length(buf, size);
    *plen = len;
    *psiz = size;
    return buf;
}

ScmObj Scm_DStringGet(ScmDString *dstr, int flags)
{
    int len, size;
    const char *str = dstring_getz(dstr, &len, &size);
    return SCM_OBJ(make_str(len, size, str, flags|SCM_STRING_TERMINATED));
}

/* For conveninence.   Note that dstr may already contain NUL byte in it,
   in that case you'll get chopped string. */
const char *Scm_DStringGetz(ScmDString *dstr)
{
    int len, size;
    return dstring_getz(dstr, &len, &size);
}

void Scm_DStringPutz(ScmDString *dstr, const char *str, int size)
{
    if (size < 0) size = (int)strlen(str);
    if (dstr->current + size > dstr->end) {
        Scm__DStringRealloc(dstr, size);
    }
    memcpy(dstr->current, str, size);
    dstr->current += size;
    if (dstr->length >= 0) {
        int len = count_length(str, size);
        if (len >= 0) dstr->length += len;
        else dstr->length = -1;
    }
}

void Scm_DStringAdd(ScmDString *dstr, ScmString *str)
{
    const ScmStringBody *b = SCM_STRING_BODY(str);
    int size = SCM_STRING_BODY_SIZE(b);
    if (size == 0) return;
    if (dstr->current + size > dstr->end) {
        Scm__DStringRealloc(dstr, size);
    }
    memcpy(dstr->current, SCM_STRING_BODY_START(b), size);
    dstr->current += size;
    if (dstr->length >= 0 && !SCM_STRING_BODY_INCOMPLETE_P(b)) {
        dstr->length += SCM_STRING_BODY_LENGTH(b);
    } else {
        dstr->length = -1;
    }
}

void Scm_DStringPutb(ScmDString *ds, char byte)
{
    SCM_DSTRING_PUTB(ds, byte);
}

void Scm_DStringPutc(ScmDString *ds, ScmChar ch)
{
    SCM_DSTRING_PUTC(ds, ch);
}


/* for debug */
void Scm_DStringDump(FILE *out, ScmDString *dstr)
{
    fprintf(out, "DString %p\n", dstr);
    if (dstr->anchor) {
        ScmDStringChain *chain; int i;
        fprintf(out, "  chunk0[%3d] = \"", dstr->init.bytes);
        SCM_IGNORE_RESULT(fwrite(dstr->init.data, 1, dstr->init.bytes, out));
        fprintf(out, "\"\n");
        for (i=1, chain = dstr->anchor; chain; chain = chain->next, i++) {
            int size = (chain->next? chain->chunk->bytes : (int)(dstr->current - dstr->tail->chunk->data));
            fprintf(out, "  chunk%d[%3d] = \"", i, size);
            SCM_IGNORE_RESULT(fwrite(chain->chunk->data, 1, size, out));
            fprintf(out, "\"\n");
        }
    } else {
        int size = (int)(dstr->current - dstr->init.data);
        fprintf(out, "  chunk0[%3d] = \"", size);
        SCM_IGNORE_RESULT(fwrite(dstr->init.data, 1, size, out));
        fprintf(out, "\"\n");
    }
}
