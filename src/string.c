/*
 * string.c - string implementation
 *
 *   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

#include <string.h>
#include <ctype.h>

void Scm_DStringDump(FILE *out, ScmDString *dstr);

static void string_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);
SCM_DEFINE_BUILTIN_CLASS(Scm_StringClass, string_print, NULL, NULL, NULL,
                         SCM_CLASS_SEQUENCE_CPL);

#define CHECK_SIZE(siz)                                         \
    do {                                                        \
        if ((siz) > SCM_STRING_MAX_SIZE) {                      \
            Scm_Error("string size too big: %ld", (siz));       \
        }                                                       \
    } while (0)

/* Internal primitive constructor.   LEN can be negative if the string
   is incomplete. */
static ScmString *make_str(ScmSmallInt len, ScmSmallInt siz,
                           const char *p, int flags)
{
    if (len < 0) flags |= SCM_STRING_INCOMPLETE;
    if (flags & SCM_STRING_INCOMPLETE) len = siz;

    if (siz > SCM_STRING_MAX_SIZE) {
        Scm_Error("string size too big: %ld", siz);
    }
    if (len > siz) {
        Scm_Error("string length (%ld) exceeds size (%ld)", len, siz);
    }

    ScmString *s = SCM_NEW(ScmString);
    SCM_SET_CLASS(s, SCM_CLASS_STRING);
    s->body = NULL;
    s->initialBody.flags = flags & SCM_STRING_FLAG_MASK;
    s->initialBody.length = len;
    s->initialBody.size = siz;
    s->initialBody.start = p;
    return s;
}

#define DUMP_LENGTH   50

/* for debug */
void Scm_StringDump(FILE *out, ScmObj str)
{
    const ScmStringBody *b = SCM_STRING_BODY(str);
    ScmSmallInt s = SCM_STRING_BODY_SIZE(b);
    const char *p = SCM_STRING_BODY_START(b);

    fprintf(out, "STR(len=%d,siz=%ld) \"", SCM_STRING_BODY_LENGTH(b), s);
    for (int i=0; i < DUMP_LENGTH && s > 0;) {
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

/* Like GC_strndup, but we don't require the source string to be
   NUL-terminated (instead, we trust the caller that the size
   argument is in valid range.) */
char *Scm_StrdupPartial(const char *src, size_t size)
{
    char *dst = SCM_NEW_ATOMIC_ARRAY(char, size+1);
    memcpy(dst, src, size);
    dst[size] = '\0';
    return dst;
}

/*
 * Multibyte length calculation
 */

/* We have multiple similar functions, due to performance reasons. */

/* Calculate both length and size of C-string str.
   If str is incomplete, *plen gets -1. */
static inline ScmSmallInt count_size_and_length(const char *str,
                                                ScmSmallInt *psize, /* out */
                                                ScmSmallInt *plen)  /* out */
{
    char c;
    const char *p = str;
    ScmSmallInt size = 0, len = 0;
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
static inline ScmSmallInt count_length(const char *str, ScmSmallInt size)
{
    ScmSmallInt count = 0;
    while (size-- > 0) {
        unsigned char c = (unsigned char)*str;
        int i = SCM_CHAR_NFOLLOWS(c);
        if (i < 0 || i > size) return -1;
        ScmChar ch;
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
    ScmSmallInt size = (stop == NULL)? strlen(str) : (stop - str);
    ScmSmallInt len = count_length(str, size);
    if (len > SCM_STRING_MAX_LENGTH) {
        Scm_Error("Scm_MBLen: length too big: %ld", len);
    }
    return (int)len; /* we keep the result int for the backward compatibility */
}

/*----------------------------------------------------------------
 * Constructors
 */

/* General constructor. */
ScmObj Scm_MakeString(const char *str, ScmSmallInt size, ScmSmallInt len,
                      int flags)
{
    flags &= ~SCM_STRING_TERMINATED;

    if (size < 0) {
        count_size_and_length(str, &size, &len);
        flags |= SCM_STRING_TERMINATED;
    } else {
        if (len < 0) len = count_length(str, size);
    }
    /* Range of size and len will be checked in make_str */

    ScmString *s;
    if (flags & SCM_STRING_COPYING) {
        flags |= SCM_STRING_TERMINATED; /* SCM_STRDUP_PARTIAL terminates the result str */
        s = make_str(len, size, SCM_STRDUP_PARTIAL(str, size), flags);
    } else {
        s = make_str(len, size, str, flags);
    }
    return SCM_OBJ(s);
}

ScmObj Scm_MakeFillString(ScmSmallInt len, ScmChar fill)
{
    if (len < 0) Scm_Error("length out of range: %d", len);
    ScmSmallInt csize = SCM_CHAR_NBYTES(fill);
    CHECK_SIZE(csize*len);
    char *ptr = SCM_NEW_ATOMIC2(char *, csize*len+1);
    char *p = ptr;
    for (int i=0; i<len; i++, p+=csize) {
        SCM_CHAR_PUT(p, fill);
    }
    ptr[csize*len] = '\0';
    return SCM_OBJ(make_str(len, csize*len, ptr, SCM_STRING_TERMINATED));
}

ScmObj Scm_ListToString(ScmObj chars)
{
    ScmSmallInt size = 0, len = 0;

    ScmObj cp;
    SCM_FOR_EACH(cp, chars) {
        if (!SCM_CHARP(SCM_CAR(cp)))
            Scm_Error("character required, but got %S", SCM_CAR(cp));
        ScmChar ch = SCM_CHAR_VALUE(SCM_CAR(cp));
        size += SCM_CHAR_NBYTES(ch);
        len++;
        CHECK_SIZE(size);
    }
    char *buf = SCM_NEW_ATOMIC2(char *, size+1);
    char *bufp = buf;
    SCM_FOR_EACH(cp, chars) {
        ScmChar ch = SCM_CHAR_VALUE(SCM_CAR(cp));
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
    const ScmStringBody *b = SCM_STRING_BODY(str);
    return SCM_STRDUP_PARTIAL(SCM_STRING_BODY_START(b), SCM_STRING_BODY_SIZE(b));
}

/* Common routine for Scm_GetStringConst and Scm_GetStringContent */
static const char *get_string_from_body(const ScmStringBody *b)
{
    ScmSmallInt size = SCM_STRING_BODY_SIZE(b);
    if (SCM_STRING_BODY_HAS_FLAG(b, SCM_STRING_TERMINATED)) {
        /* we can use string data as C-string */
        return SCM_STRING_BODY_START(b);
    } else {
        char *p = SCM_STRDUP_PARTIAL(SCM_STRING_BODY_START(b), size);
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
   so we can directly return the body of the string.  We do not
   allow string containing NUL to be passed to C world, for it
   would be a security risk.
   TODO: Let the string body have a flag so that we don't need
   to scan the string every time.
*/
const char *Scm_GetStringConst(ScmString *str)
{
    const ScmStringBody *b = SCM_STRING_BODY(str);
    if (memchr(SCM_STRING_BODY_START(b), 0, SCM_STRING_BODY_SIZE(b))) {
        Scm_Error("A string containing NUL character is not allowed: %S",
                  SCM_OBJ(str));
    }
    return get_string_from_body(b);
}

/* Atomically extracts C-string, length, size, and incomplete flag.
   MT-safe. */
/* NB: Output parameters are int's for the ABI compatibility. */
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
    ScmSmallInt size = SCM_STRING_BODY_SIZE(b);
    ScmSmallInt len  = SCM_STRING_BODY_LENGTH(b);
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

    const ScmStringBody *b = SCM_STRING_BODY(x);
    if (!SCM_STRING_BODY_INCOMPLETE_P(b)) {
        /* we do simple copy */
        r = Scm_CopyString(x);
    } else {
        const char *s = SCM_STRING_BODY_START(b);
        ScmSmallInt siz = SCM_STRING_BODY_SIZE(b);
        ScmSmallInt len = count_length(s, siz);
        if (len >= 0) {
            r = Scm_MakeString(s, siz, len, 0);
        } else if (handling == SCM_ILLEGAL_CHAR_REJECT) {
            r = SCM_FALSE;
        } else {
            const char *p = s;

            ScmDString ds;
            Scm_DStringInit(&ds);

            while (p < s+siz) {
                ScmChar ch;
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
    const ScmStringBody *xb = SCM_STRING_BODY(x);
    const ScmStringBody *yb = SCM_STRING_BODY(y);
    if ((SCM_STRING_BODY_FLAGS(xb)^SCM_STRING_BODY_FLAGS(yb))&SCM_STRING_INCOMPLETE) {
        Scm_Error("cannot compare incomplete vs complete string: %S, %S",
                  SCM_OBJ(x), SCM_OBJ(y));
    }
    ScmSmallInt sizx = SCM_STRING_BODY_SIZE(xb);
    ScmSmallInt sizy = SCM_STRING_BODY_SIZE(yb);
    ScmSmallInt siz = (sizx < sizy)? sizx : sizy;
    int r = memcmp(SCM_STRING_BODY_START(xb), SCM_STRING_BODY_START(yb), siz);
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
static int sb_strcasecmp(const char *px, ScmSmallInt sizx,
                         const char *py, ScmSmallInt sizy)
{
    for (; sizx > 0 && sizy > 0; sizx--, sizy--, px++, py++) {
        char cx = tolower((u_char)*px);
        char cy = tolower((u_char)*py);
        if (cx == cy) continue;
        return (cx - cy);
    }
    if (sizx > 0) return 1;
    if (sizy > 0) return -1;
    return 0;
}

/* multi-byte case insensitive comparison */
static int mb_strcasecmp(const char *px, ScmSmallInt lenx,
                         const char *py, ScmSmallInt leny)
{
    int ix, iy;
    for (; lenx > 0 && leny > 0; lenx--, leny--, px+=ix, py+=iy) {
        int cx, cy;
        SCM_CHAR_GET(px, cx);
        SCM_CHAR_GET(py, cy);
        int ccx = SCM_CHAR_UPCASE(cx);
        int ccy = SCM_CHAR_UPCASE(cy);
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
    const ScmStringBody *xb = SCM_STRING_BODY(x);
    const ScmStringBody *yb = SCM_STRING_BODY(y);

    if ((SCM_STRING_BODY_FLAGS(xb)^SCM_STRING_BODY_FLAGS(yb))&SCM_STRING_INCOMPLETE) {
        Scm_Error("cannot compare incomplete strings in case-insensitive way: %S, %S",
                  SCM_OBJ(x), SCM_OBJ(y));
    }
    ScmSmallInt sizx = SCM_STRING_BODY_SIZE(xb);
    ScmSmallInt lenx = SCM_STRING_BODY_LENGTH(xb);
    ScmSmallInt sizy = SCM_STRING_BODY_SIZE(yb);
    ScmSmallInt leny = SCM_STRING_BODY_LENGTH(yb);
    const char *px = SCM_STRING_BODY_START(xb);
    const char *py = SCM_STRING_BODY_START(yb);

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
static const char *forward_pos(const char *current, ScmSmallInt offset)
{
    while (offset--) {
        int n = SCM_CHAR_NFOLLOWS(*current);
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
ScmChar Scm_StringRef(ScmString *str, ScmSmallInt pos, int range_error)
{
    const ScmStringBody *b = SCM_STRING_BODY(str);
    ScmSmallInt len = SCM_STRING_BODY_LENGTH(b);

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
int Scm_StringByteRef(ScmString *str, ScmSmallInt offset, int range_error)
{
    const ScmStringBody *b = SCM_STRING_BODY(str);
    if (offset < 0 || offset >= SCM_STRING_BODY_SIZE(b)) {
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
const char *Scm_StringBodyPosition(const ScmStringBody *b, ScmSmallInt offset)
{
    if (offset < 0 || offset > SCM_STRING_BODY_LENGTH(b)) {
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
const char *Scm_StringPosition(ScmString *str, ScmSmallInt offset)
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
    ScmSmallInt sizex = SCM_STRING_BODY_SIZE(xb);
    ScmSmallInt lenx = SCM_STRING_BODY_LENGTH(xb);
    ScmSmallInt sizey = SCM_STRING_BODY_SIZE(yb);
    ScmSmallInt leny = SCM_STRING_BODY_LENGTH(yb);
    CHECK_SIZE(sizex+sizey);
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

ScmObj Scm_StringAppendC(ScmString *x, const char *str,
                         ScmSmallInt sizey, ScmSmallInt leny)
{
    const ScmStringBody *xb = SCM_STRING_BODY(x);
    ScmSmallInt sizex = SCM_STRING_BODY_SIZE(xb);
    ScmSmallInt lenx = SCM_STRING_BODY_LENGTH(xb);
    int flags = 0;

    if (sizey < 0) count_size_and_length(str, &sizey, &leny);
    else if (leny < 0) leny = count_length(str, sizey);
    CHECK_SIZE(sizex+sizey);

    char *p = SCM_NEW_ATOMIC2(char *, sizex + sizey + 1);
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
    ScmSmallInt size = 0, len = 0;
    int flags = 0;
    const ScmStringBody *bodies_s[BODY_ARRAY_SIZE], **bodies;

    /* It is trickier than it appears, since the strings may be modified
       by another thread during we're dealing with it.  So in the first
       pass to sum up the lengths of strings, we extract the string bodies
       and save it.  */
    int numstrs = Scm_Length(strs);
    if (numstrs < 0) Scm_Error("improper list not allowed: %S", strs);
    if (numstrs > BODY_ARRAY_SIZE) {
        bodies = SCM_NEW_ARRAY(const ScmStringBody*, numstrs);
    } else {
        bodies = bodies_s;
    }

    int i = 0;
    ScmObj cp;
    SCM_FOR_EACH(cp, strs) {
        const ScmStringBody *b;
        if (!SCM_STRINGP(SCM_CAR(cp))) {
            Scm_Error("string required, but got %S\n", SCM_CAR(cp));
        }
        b = SCM_STRING_BODY(SCM_CAR(cp));
        size += SCM_STRING_BODY_SIZE(b);
        len += SCM_STRING_BODY_LENGTH(b);
        CHECK_SIZE(size);
        if (SCM_STRING_BODY_INCOMPLETE_P(b)) {
            flags |= SCM_STRING_INCOMPLETE;
        }
        bodies[i++] = b;
    }

    char *buf = SCM_NEW_ATOMIC2(char *, size+1);
    char *bufp = buf;
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
    ScmSmallInt size = 0, len = 0;
    int flags = 0;
    const ScmStringBody *bodies_s[BODY_ARRAY_SIZE], **bodies;

    int nstrs = Scm_Length(strs);
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

    const ScmStringBody *dbody = SCM_STRING_BODY(delim);
    ScmSmallInt dsize = SCM_STRING_BODY_SIZE(dbody);
    ScmSmallInt dlen  = SCM_STRING_BODY_LENGTH(dbody);
    if (SCM_STRING_BODY_INCOMPLETE_P(dbody)) {
        flags |= SCM_STRING_INCOMPLETE;
    }

    int i = 0, ndelim;
    ScmObj cp;
    SCM_FOR_EACH(cp, strs) {
        const ScmStringBody *b;
        if (!SCM_STRINGP(SCM_CAR(cp))) {
            Scm_Error("string required, but got %S\n", SCM_CAR(cp));
        }
        b = SCM_STRING_BODY(SCM_CAR(cp));
        size += SCM_STRING_BODY_SIZE(b);
        len  += SCM_STRING_BODY_LENGTH(b);
        CHECK_SIZE(size);
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
    CHECK_SIZE(size);

    char *buf = SCM_NEW_ATOMIC2(char *, size+1);
    char *bufp = buf;
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

static ScmObj substring(const ScmStringBody *xb,
                        ScmSmallInt start, ScmSmallInt end,
                        int byterange)
{
    ScmSmallInt len = byterange? SCM_STRING_BODY_SIZE(xb) : SCM_STRING_BODY_LENGTH(xb);
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

ScmObj Scm_Substring(ScmString *x, ScmSmallInt start, ScmSmallInt end,
                     int byterangep)
{
    return substring(SCM_STRING_BODY(x), start, end, byterangep);
}

/* Auxiliary procedure to support optional start/end parameter specified
   in lots of SRFI-13 functions.   If start and end is specified and restricts
   string range, call substring.  Otherwise returns x itself. */
ScmObj Scm_MaybeSubstring(ScmString *x, ScmObj start, ScmObj end)
{
    ScmSmallInt istart, iend;
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

/* Boyer-Moore string search.  assuming siz1 > siz2, siz2 < 256. */
static ScmSmallInt boyer_moore(const char *ss1, ScmSmallInt siz1,
                               const char *ss2, ScmSmallInt siz2)
{
    unsigned char shift[256];
    for (ScmSmallInt i=0; i<256; i++) { shift[i] = siz2; }
    for (ScmSmallInt j=0; j<siz2-1; j++) {
        shift[(unsigned char)ss2[j]] = siz2-j-1;
    }
    for (ScmSmallInt i=siz2-1; i<siz1; i+=shift[(unsigned char)ss1[i]]) {
        ScmSmallInt j, k;
        for (j=siz2-1, k = i; j>=0 && ss1[k] == ss2[j]; j--, k--)
            ;
        if (j == -1) return k+1;
    }
    return -1;
}

static ScmSmallInt boyer_moore_reverse(const char *ss1, ScmSmallInt siz1,
                                       const char *ss2, ScmSmallInt siz2)
{
    unsigned char shift[256];
    for (ScmSmallInt i=0; i<256; i++) { shift[i] = siz2; }
    for (ScmSmallInt j=siz2-1; j>0; j--) {
        shift[(unsigned char)ss2[j]] = j;
    }
    for (ScmSmallInt i=siz1-siz2+1; i>=0; i-=shift[(unsigned char)ss1[i]]) {
        ScmSmallInt j, k;
        for (j=0, k = i; j<siz2 && ss1[k] == ss2[j]; j++, k++)
            ;
        if (j == siz2) return i;
    }
    return -1;
}

/* Primitive routines to search a substring s2 within s1.
   Returns NOT_FOUND if not fonud, FOUND_BOTH_INDEX if both byte index
   (*bi) and character index (*ci) is calculted, FOUND_BYTE_INDEX
   if only byte index is calculated.

   When the encoding is utf-8 or none, we can scan a string as if it is just
   a bytestring.   The only caveat is that, with utf-8, we need to caclulate
   character index after we find the match.  It is still a total win, for
   finding out non-matches using Boyer-Moore is a lot faster than naive way.

   If the encoding is EUC-JP or SJIS, we can only use Boyer-Moore when we
   know the strings have single-byte only.  Multibyte strings of those
   encodings can have spurious matches when compared bytewise.
 */

/* return value of string_scan */
#define NOT_FOUND 0         /* string not found */
#define FOUND_BOTH_INDEX 1  /* string found, and both indexes are calculated */
#define FOUND_BYTE_INDEX 2  /* string found, and only byte index is calc'd */

/* In utf-8 multibyte case, we only count byte index and let the caller
   figure out the character index.  In other encodings we can always find
   both index. */
#if defined(GAUCHE_CHAR_ENCODING_UTF_8)
#define FOUND_MAYBE_BOTH FOUND_BYTE_INDEX
#else
#define FOUND_MAYBE_BOTH FOUND_BOTH_INDEX
#endif

/* In euc-jp and sjis case, we use faster method only when (size == len). */
#if defined(GAUCHE_CHAR_ENCODING_EUC_JP) || defined(GAUCHE_CHAR_ENCODING_SJIS)
#define BYTEWISE_SEARCHABLE(siz, len)  ((siz) == (len))
#define MULTIBYTE_NAIVE_SEARCH_NEEDED 1
#else
#define BYTEWISE_SEARCHABLE(siz, len)  TRUE
#define MULTIBYTE_NAIVE_SEARCH_NEEDED 0
#endif

/* glibc has memrchr, but we need to provide fallback anyway and
   we don't need it to be highly tuned, so we just roll our own. */
static const void *my_memrchr(const void *s, int c, size_t n)
{
    const char *p = (const char*)s + n - 1;
    for (;p >= (const char*)s; p--) {
        if ((int)*p == c) return p;
    }
    return NULL;
}

static int string_search(const char *s1, ScmSmallInt siz1, ScmSmallInt len1,
                         const char *s2, ScmSmallInt siz2, ScmSmallInt len2,
                         ScmSmallInt *bi /* out */,
                         ScmSmallInt *ci /* out */)
{
    if (siz2 == 0) {
        *bi = *ci = 0;
        return FOUND_BOTH_INDEX;
    }

    /* Single-byte case. */
    if (BYTEWISE_SEARCHABLE(siz1, len1)) {
        if (siz2 == 1) {
            /* Single ASCII character search case.  This is a huge win. */
            const char *z = memchr(s1, s2[0], siz1);
            if (z) { *bi = *ci = z - s1; return FOUND_MAYBE_BOTH; }
            else return NOT_FOUND;
        }
        if (BYTEWISE_SEARCHABLE(siz2, len2)) {
            ScmSmallInt i;
            /* Shortcut for single-byte strings */
            if (siz1 < siz2) return NOT_FOUND;
            if (siz1 < 256 || siz2 >= 256) {
                /* brute-force search */
                for (i=0; i<=siz1-siz2; i++) {
                    if (memcmp(s2, s1+i, siz2) == 0) break;
                }
                if (i == siz1-siz2+1) return NOT_FOUND;
            } else {
                i = boyer_moore(s1, siz1, s2, siz2);
                if (i < 0) return NOT_FOUND;
            }
            *bi = *ci = i;
            return FOUND_MAYBE_BOTH;
        }
        /* FALLTHROUGH */
    }

#if MULTIBYTE_NAIVE_SEARCH_NEEDED
    /* Multibyte case. */
    if (len1 >= len2) {
        const char *sp = s1;
        for (ScmSmallInt i=0; i<=len1-len2; i++) {
            if (memcmp(sp, s2, siz2) == 0) {
                *bi = (int)(sp - s1);
                *ci = i;
                return FOUND_BOTH_INDEX;
            }
            sp += SCM_CHAR_NFOLLOWS(*sp) + 1;
        }
    }
#endif /*MULTIBYTE_NAIVE_SEARCH_NEEDED*/
    return NOT_FOUND;
}

static int string_search_reverse(const char *s1, ScmSmallInt siz1, ScmSmallInt len1,
                                 const char *s2, ScmSmallInt siz2, ScmSmallInt len2,
                                 ScmSmallInt *bi /* out */,
                                 ScmSmallInt *ci /* out */)
{
    if (siz2 == 0) {
        *bi = siz1;
        *ci = len1;
        return FOUND_BOTH_INDEX;
    }

    /* Single-byte case. */
    if (BYTEWISE_SEARCHABLE(siz1, len1)) {
        if (siz2 == 1) {
            /* Single ASCII character search case.  This is a huge win. */
            const char *z = my_memrchr(s1, s2[0], siz1);
            if (z) { *bi = *ci = z - s1; return FOUND_MAYBE_BOTH; }
            else return NOT_FOUND;
        }
        if (BYTEWISE_SEARCHABLE(siz2, len2)) {
            ScmSmallInt i;
            /* short cut for single-byte strings */
            if (siz1 < siz2) return NOT_FOUND;
            if (siz1 < 256 || siz2 >= 256) {
                /* brute-force search */
                for (i=siz1-siz2; i>=0; i--) {
                    if (memcmp(s2, s1+i, siz2) == 0) break;
                }
                if (i < 0) return NOT_FOUND;
            } else {
                i = boyer_moore_reverse(s1, siz1, s2, siz2);
                if (i < 0) return NOT_FOUND;
            }
            *bi = *ci = i;
            return FOUND_MAYBE_BOTH;
        } else {
            return NOT_FOUND;   /* sbstring can't contain mbstring. */
        }
    }

#if MULTIBYTE_NAIVE_SEARCH_NEEDED
    /* Multibyte case. */
    if (len1 >= len2) {
        const char *sp = s1 + siz1, *p;
        for (ScmSmallInt i=0; i<len2; i++) {
            SCM_CHAR_BACKWARD(sp, s1, p);
            SCM_ASSERT(*p);
            sp = p;
        }
        for (ScmSmallInt i=len1-len2; i>=0; i--) {
            if (memcmp(sp, s2, siz2) == 0) {
                *bi = (int)(sp - s1);
                *ci = i;
                return FOUND_BOTH_INDEX;
            }
            SCM_CHAR_BACKWARD(sp, s1, p);
            sp = p;
        }
    }
#endif /*MULTIBYTE_NAIVE_SEARCH_NEEDED*/
    return NOT_FOUND;
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
                          ScmSmallInt siz2, ScmSmallInt len2,
                          int incomplete2,
                          int retmode,
                          int (*searcher)(const char*, ScmSmallInt, ScmSmallInt,
                                          const char*, ScmSmallInt, ScmSmallInt,
                                          ScmSmallInt*, ScmSmallInt*),
                          ScmObj *secondval) /* out */
{
    ScmSmallInt bi, ci;
    const ScmStringBody *sb = SCM_STRING_BODY(ss1);
    const char *s1 = SCM_STRING_BODY_START(sb);
    ScmSmallInt siz1 = SCM_STRING_BODY_SIZE(sb);
    ScmSmallInt len1 = SCM_STRING_BODY_LENGTH(sb);

    if (retmode < 0 || retmode > SCM_STRING_SCAN_BOTH) {
        Scm_Error("return mode out fo range: %d", retmode);
    }

    int incomplete =
        (SCM_STRING_BODY_INCOMPLETE_P(sb) || incomplete2)
        ? SCM_STRING_INCOMPLETE : 0;

    /* prefiltering - if both string is complete, and s1 is sbstring
       and s2 is mbstring, we know there's no match.  */
    int retcode =
        (!incomplete && (siz1 == len1) && (siz2 != len2))
        ? NOT_FOUND
        : searcher(s1, siz1, len1, s2, siz2, len2, &bi, &ci);

    if (retcode == NOT_FOUND) {
        if (retmode > SCM_STRING_SCAN_AFTER) *secondval = SCM_FALSE;
        return SCM_FALSE;
    }

    if (retcode == FOUND_BYTE_INDEX && !incomplete) {
        ci = count_length(s1, bi);
    }

    switch (retmode) {
    case SCM_STRING_SCAN_INDEX:
        return Scm_MakeInteger(ci);
    case SCM_STRING_SCAN_BEFORE:
        return Scm_MakeString(s1, bi, ci, incomplete);
    case SCM_STRING_SCAN_AFTER:
        return Scm_MakeString(s1+bi+siz2, siz1-bi-siz2,
                              len1-ci-len2, incomplete);
    case SCM_STRING_SCAN_BEFORE2:
        *secondval = Scm_MakeString(s1+bi, siz1-bi, len1-ci, incomplete);
        return Scm_MakeString(s1, bi, ci, incomplete);
    case SCM_STRING_SCAN_AFTER2:
        *secondval = Scm_MakeString(s1+bi+siz2, siz1-bi-siz2,
                                    len1-ci-len2, incomplete);
        return Scm_MakeString(s1, bi+siz2, ci+len2, incomplete);
    case SCM_STRING_SCAN_BOTH:
        *secondval = Scm_MakeString(s1+bi+siz2, siz1-bi-siz2,
                                    len1-ci-len2, incomplete);
        return Scm_MakeString(s1, bi, ci, incomplete);
    }
    return SCM_UNDEFINED;       /* dummy */
}

ScmObj Scm_StringScan(ScmString *s1, ScmString *s2, int retmode)
{
    ScmObj v1, v2;
    const ScmStringBody *s2b = SCM_STRING_BODY(s2);
    v1 = string_scan(s1,
                     SCM_STRING_BODY_START(s2b),
                     SCM_STRING_BODY_SIZE(s2b),
                     SCM_STRING_BODY_LENGTH(s2b),
                     SCM_STRING_BODY_INCOMPLETE_P(s2b),
                     retmode, string_search, &v2);
    if (retmode <= SCM_STRING_SCAN_AFTER) return v1;
    else return Scm_Values2(v1, v2);
}

ScmObj Scm_StringScanChar(ScmString *s1, ScmChar ch, int retmode)
{
    ScmObj v1, v2;
    char buf[SCM_CHAR_MAX_BYTES];
    SCM_CHAR_PUT(buf, ch);
    v1 = string_scan(s1, buf, SCM_CHAR_NBYTES(ch), 1, FALSE, retmode,
                     string_search, &v2);
    if (retmode <= SCM_STRING_SCAN_AFTER) return v1;
    else return Scm_Values2(v1, v2);
}

ScmObj Scm_StringScanRight(ScmString *s1, ScmString *s2, int retmode)
{
    ScmObj v1, v2;
    const ScmStringBody *s2b = SCM_STRING_BODY(s2);
    v1 = string_scan(s1,
                     SCM_STRING_BODY_START(s2b),
                     SCM_STRING_BODY_SIZE(s2b),
                     SCM_STRING_BODY_LENGTH(s2b),
                     SCM_STRING_BODY_INCOMPLETE_P(s2b),
                     retmode, string_search_reverse, &v2);
    if (retmode <= SCM_STRING_SCAN_AFTER) return v1;
    else return Scm_Values2(v1, v2);
}

ScmObj Scm_StringScanCharRight(ScmString *s1, ScmChar ch, int retmode)
{
    ScmObj v1, v2;
    char buf[SCM_CHAR_MAX_BYTES];
    SCM_CHAR_PUT(buf, ch);
    v1 = string_scan(s1, buf, SCM_CHAR_NBYTES(ch), 1, FALSE, retmode,
                     string_search_reverse, &v2);
    if (retmode <= SCM_STRING_SCAN_AFTER) return v1;
    else return Scm_Values2(v1, v2);
}

#undef NOT_FOUND
#undef FOUND_BOTH_INDEX
#undef FOUND_BYTE_INDEX
#undef FOUND_MAYBE_BOTH
#undef BYTEWISE_SEARCHABLE
#undef MULTIBYTE_NAIVE_SEARCH_NEEDED

/* Split string by char.  Char itself is not included in the result.
   If LIMIT >= 0, up to that number of matches are considered (i.e.
   up to LIMIT+1 strings are returned).   LIMIT < 0 makes the number
   of matches unlimited.
   TODO: If CH is a utf-8 multi-byte char, Boyer-Moore skip table is
   calculated every time we call string_scan, which is a waste.  Some
   mechanism to cache the skip table would be nice.
*/
ScmObj Scm_StringSplitByCharWithLimit(ScmString *str, ScmChar ch, int limit)
{
    char buf[SCM_CHAR_MAX_BYTES];
    int nb = SCM_CHAR_NBYTES(ch);
    ScmObj head = SCM_NIL, tail = SCM_NIL;

    if (limit == 0) return SCM_LIST1(SCM_OBJ(str)); /* trivial case */

    SCM_CHAR_PUT(buf, ch);

    for (;;) {
        ScmObj v1, v2;
        v1 = string_scan(str, buf, nb, 1, FALSE, SCM_STRING_SCAN_BOTH,
                         string_search, &v2);
        if (SCM_FALSEP(v1)) {
            SCM_APPEND1(head, tail, SCM_OBJ(str));
            break;
        } else {
            SCM_APPEND1(head, tail, v1);
            if (--limit == 0) { SCM_APPEND1(head, tail, v2); break; }
        }
        str = SCM_STRING(v2);
    }
    return head;
}

/* For ABI compatibility - On 1.0, let's make this have limit arg and
   drop Scm_StringSplitByCharWithLimit.  */
ScmObj Scm_StringSplitByChar(ScmString *str, ScmChar ch)
{
    return Scm_StringSplitByCharWithLimit(str, ch, -1);
}

/*----------------------------------------------------------------
 * Miscellaneous functions
 */

ScmObj Scm_StringToList(ScmString *str)
{
    const ScmStringBody *b = SCM_STRING_BODY(str);
    ScmObj start = SCM_NIL, end = SCM_NIL;
    const char *bufp = SCM_STRING_BODY_START(b);
    ScmSmallInt len = SCM_STRING_BODY_LENGTH(b);

    if (SCM_STRING_BODY_INCOMPLETE_P(b))
        Scm_Error("incomplete string not supported: %S", str);
    while (len-- > 0) {
        ScmChar ch;
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
    ScmObj h = SCM_NIL, t = SCM_NIL;
    if (size < 0) {
        for (;*array; array++) {
            ScmObj s = Scm_MakeString(*array, -1, -1, flags);
            SCM_APPEND1(h, t, s);
        }
    } else {
        for (int i=0; i<size; i++, array++) {
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
    int len = list_to_cstring_array_check(lis, errp);
    if (len < 0) return NULL;
    const char **array = SCM_NEW_ARRAY(const char*, len+1);
    const char **p = array;
    ScmObj lp;
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
    char **array, **p;
    int len = list_to_cstring_array_check(lis, errp);
    if (len < 0) return NULL;

    if (alloc) {
        p = array = (char **)alloc((len+1) * sizeof(char *));
        ScmObj lp;
        SCM_FOR_EACH(lp, lis) {
            const char *s = Scm_GetStringConst(SCM_STRING(SCM_CAR(lp)));
            *p = (char *)alloc(strlen(s) + 1);
            strcpy(*p, s);
            p++;
        }
    } else {
        p = array = SCM_NEW_ARRAY(char*, len+1);
        ScmObj lp;
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
    char buf[6];
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
            /* TODO: Should we provide 'legacy-compatible writer mode,
               which does not use ';' terminator? */
            snprintf(buf, 6, "\\x%02x;", (unsigned char)ch);
            SCM_PUTZ(buf, -1, port);
        } else {
            SCM_PUTC(ch, port);
        }
    }
}

static void string_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmString *str = SCM_STRING(obj);
    if (Scm_WriteContextMode(ctx) == SCM_WRITE_DISPLAY) {
        SCM_PUTS(str, port);
    } else {
        const ScmStringBody *b = SCM_STRING_BODY(str);
        if (SCM_STRING_BODY_SINGLE_BYTE_P(b)) {
            const char *cp = SCM_STRING_BODY_START(b);
            ScmSmallInt size = SCM_STRING_BODY_SIZE(b);
            if (SCM_STRING_BODY_INCOMPLETE_P(b)) {
                SCM_PUTZ("#*\"", -1, port);
            } else {
                SCM_PUTC('"', port);
            }
            while (size--) {
                string_putc(*cp++, port, SCM_STRING_BODY_INCOMPLETE_P(b));
            }
        } else {
            const char *cp = SCM_STRING_BODY_START(b);
            ScmSmallInt len = SCM_STRING_BODY_LENGTH(b);

            SCM_PUTC('"', port);
            while (len--) {
                ScmChar ch;
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

ScmObj Scm_MakeStringPointer(ScmString *src, ScmSmallInt index,
                             ScmSmallInt start, ScmSmallInt end)
{
    const ScmStringBody *srcb = SCM_STRING_BODY(src);
    ScmSmallInt len = SCM_STRING_BODY_LENGTH(srcb);
    ScmSmallInt effective_size;
    const char *sptr, *ptr, *eptr;

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
    ScmStringPointer *sp = SCM_NEW(ScmStringPointer);
    SCM_SET_CLASS(sp, SCM_CLASS_STRING_POINTER);
    sp->length = (SCM_STRING_BODY_INCOMPLETE_P(srcb)? -1 : (end-start));
    sp->size = effective_size;
    sp->start = sptr;
    sp->index = index;
    sp->current = ptr;
    return SCM_OBJ(sp);
  badindex:
    Scm_Error("index out of range: %ld", index);
    return SCM_UNDEFINED;
}

ScmObj Scm_StringPointerRef(ScmStringPointer *sp)
{
    ScmChar ch;
    if (sp->length < 0 || sp->size == sp->length) {
        if (sp->index >= sp->size) return SCM_EOF;
        ch = *(const unsigned char*)sp->current;
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
        ch = *(const unsigned char*)sp->current++;
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
        ch = *(const unsigned char*)--sp->current;
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

ScmObj Scm_StringPointerSet(ScmStringPointer *sp, ScmSmallInt index)
{
    if (index < 0) goto badindex;
    /* NB: Safe to cast index, for too large index would be rejected
       by the check. */
    if (sp->length < 0 || sp->size == sp->length) {
        if (index > sp->size) goto badindex;
        sp->index = (int)index;
        sp->current = sp->start + (int)index;
    } else {
        if (index > sp->length) goto badindex;
        sp->index = (int)index;
        sp->current = forward_pos(sp->start, index);
    }
    return SCM_OBJ(sp);
  badindex:
    Scm_Error("index out of range: %ld", index);
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
    ScmSmallInt size;
    if (dstr->tail) {
        size = dstr->init.bytes;
        dstr->tail->chunk->bytes = (int)(dstr->current - dstr->tail->chunk->data);
        for (ScmDStringChain *chain = dstr->anchor; chain; chain = chain->next) {
            size += chain->chunk->bytes;
        }
    } else {
        size = dstr->current - dstr->init.data;
    }
    if (size > SCM_STRING_MAX_SIZE) {
        Scm_Error("Scm_DStringSize: size exceeded the range: %ld", size);
    }
    return (int)size;
}

void Scm__DStringRealloc(ScmDString *dstr, int minincr)
{
    /* sets the byte count of the last chunk */
    if (dstr->tail) {
        dstr->tail->chunk->bytes = (int)(dstr->current - dstr->tail->chunk->data);
    } else {
        dstr->init.bytes = (int)(dstr->current - dstr->init.data);
    }

    /* determine the size of the new chunk.  the increase factor 3 is
       somewhat arbitrary, determined by rudimental benchmarking. */
    ScmSmallInt newsize = dstr->lastChunkSize * 3;
    if (newsize > DSTRING_MAX_CHUNK_SIZE) {
        newsize = DSTRING_MAX_CHUNK_SIZE;
    }
    if (newsize < minincr) {
        newsize = minincr;
    }

    ScmDStringChunk *newchunk = SCM_NEW_ATOMIC2(
        ScmDStringChunk*,
        sizeof(ScmDStringChunk)+newsize-SCM_DSTRING_INIT_CHUNK_SIZE);
    newchunk->bytes = 0;

    ScmDStringChain *newchain = SCM_NEW(ScmDStringChain);

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
static const char *dstring_getz(ScmDString *dstr, int *psiz, int *plen, int noalloc)
{
    ScmSmallInt size, len;
    char *buf;
    if (dstr->anchor == NULL) {
        /* we only have one chunk */
        size = dstr->current - dstr->init.data;
        CHECK_SIZE(size);
        len = dstr->length;
        if (noalloc) {
            buf = dstr->init.data;
        } else {
            buf = SCM_STRDUP_PARTIAL(dstr->init.data, size);
        }
    } else {
        ScmDStringChain *chain = dstr->anchor;
        char *bptr;

        size = Scm_DStringSize(dstr);
        CHECK_SIZE(size);
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
    if (plen) *plen = (int)len;
    if (psiz) *psiz = (int)size;
    return buf;
}

ScmObj Scm_DStringGet(ScmDString *dstr, int flags)
{
    int len, size;
    const char *str = dstring_getz(dstr, &size, &len, FALSE);
    return SCM_OBJ(make_str(len, size, str, flags|SCM_STRING_TERMINATED));
}

/* For conveninence.   Note that dstr may already contain NUL byte in it,
   in that case you'll get chopped string. */
const char *Scm_DStringGetz(ScmDString *dstr)
{
    int len, size;
    return dstring_getz(dstr, &size, &len, FALSE);
}

/* Returns the current content of DString, along with byte size and character
   length. The returned pointer may not be NUL-terminated.

   Unlike Scm_DStringGet[z], returned pointer can directly points into
   the internal buffer of Scm_DString; especially, this never allocates
   if DString only uses initial buffer.  The caller should be aware that
   the returned content may be altered by further DString operation. */
const char *Scm_DStringPeek(ScmDString *dstr, int *size, int *len)
{
    return dstring_getz(dstr, size, len, TRUE);
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
        int len = (int)count_length(str, size);
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
        fprintf(out, "  chunk0[%3d] = \"", dstr->init.bytes);
        SCM_IGNORE_RESULT(fwrite(dstr->init.data, 1, dstr->init.bytes, out));
        fprintf(out, "\"\n");
        ScmDStringChain *chain = dstr->anchor;
        for (int i=1; chain; chain = chain->next, i++) {
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
