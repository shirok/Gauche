/*
 * string.c - string implementation
 *
 *   Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
 *  $Id: string.c,v 1.72 2003-07-05 03:29:12 shirok Exp $
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <string.h>
#define LIBGAUCHE_BODY
#include "gauche.h"

void Scm_DStringDump(FILE *out, ScmDString *dstr);

static void string_print(ScmObj obj, ScmPort *port, ScmWriteContext *);
SCM_DEFINE_BUILTIN_CLASS(Scm_StringClass, string_print, NULL, NULL, NULL,
                         SCM_CLASS_SEQUENCE_CPL);

#define CHECK_MUTABLE(str)                                              \
    do {                                                                \
        if (SCM_STRING_IMMUTABLE_P(str))                                \
            Scm_Error("attempted to modify immutable string: %S", str); \
    } while (0)

/* internal primitive constructor.   the created string is mutable
   by default.  len can be negative value, indicating the string
   is incomplete. */
static ScmString *make_str(int len, int siz, const char *p)
{
    ScmString *s = SCM_NEW(ScmString);
    SCM_SET_CLASS(s, SCM_CLASS_STRING);
    s->immutable = 0;
    s->incomplete = (len < 0)? 1 : 0;
    s->length = (len < 0)? siz : len;
    s->size = siz;
    s->start = p;
    return s;
}

#define DUMP_LENGTH   50

/* for debug */
#if SCM_DEBUG_HELPER
void Scm_StringDump(FILE *out, ScmObj str)
{
    int i;
    int s = SCM_STRING_SIZE(str);
    const char *p = SCM_STRING_START(str);

    fprintf(out, "STR(len=%d,siz=%d) \"", SCM_STRING_LENGTH(str), s);
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
#endif /*SCM_DEBUG_HELPER*/

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
            if (!*p++) len = -1;
            size++;
        }
    }
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
    int size = (stop == NULL)? strlen(str) : (stop - str);
    return count_length(str, size);
}

/*----------------------------------------------------------------
 * Constructors
 */

/* General constructor. */
ScmObj Scm_MakeString(const char *str, int size, int len, int flags)
{
    ScmString *s;
    
    if (size < 0) count_size_and_length(str, &size, &len);
    else if (len < 0) len = count_length(str, size);
    if (flags & SCM_MAKSTR_COPYING) {
        char *nstr = SCM_NEW_ATOMIC2(char *, size + 1);
        memcpy(nstr, str, size);
        nstr[size] = '\0';          /* be kind to C */
        s = make_str(len, size, nstr);
    } else {
        s = make_str(len, size, str);
    }
    s->immutable = (flags & SCM_MAKSTR_IMMUTABLE);
    if ((flags&SCM_MAKSTR_INCOMPLETE) || len < 0) {
        s->incomplete = TRUE;
        s->length = s->size;
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
    return SCM_OBJ(make_str(len, size*len, ptr));
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

char *Scm_GetString(ScmString *str)
{
    int size;
    char *p;

    size = SCM_STRING_SIZE(str);
    p = SCM_NEW_ATOMIC2(char *, size+1);
    memcpy(p, SCM_STRING_START(str), size);
    p[size] = '\0';
    return p;
}

const char *Scm_GetStringConst(ScmString *str)
{
    int size;
    
    size = SCM_STRING_SIZE(str);
    if (SCM_STRING_START(str)[size] == '\0') {
        /* we can use string data as C-string */
        return SCM_STRING_START(str);
    } else {
        char *p = SCM_NEW_ATOMIC2(char *, size+1);
        memcpy(p, SCM_STRING_START(str), size);
        p[size] = '\0';
        return p;
    }
}

/* returned string is always mutable. */
ScmObj Scm_CopyString(ScmString *x)
{
    if (SCM_STRING_INCOMPLETE_P(x)) {
        return SCM_OBJ(make_str(-1, SCM_STRING_SIZE(x), SCM_STRING_START(x)));
    } else {
        return SCM_OBJ(make_str(SCM_STRING_LENGTH(x), SCM_STRING_SIZE(x),
                                SCM_STRING_START(x)));
    }
}

/* mark the string immutable */
ScmObj Scm_StringMakeImmutable(ScmString *x)
{
    x->immutable = TRUE;
    return SCM_OBJ(x);
}

/* string-complete->incomplete! */
ScmObj Scm_StringCompleteToIncompleteX(ScmString *x)
{
    CHECK_MUTABLE(x);
    x->incomplete = TRUE;
    x->length = x->size;
    return SCM_OBJ(x);
}

ScmObj Scm_StringCompleteToIncomplete(ScmString *x)
{
    return Scm_StringCompleteToIncompleteX(SCM_STRING(Scm_CopyString(x)));
}

ScmObj Scm_StringIncompleteToCompleteX(ScmString *x)
{
    CHECK_MUTABLE(x);
    if (SCM_STRING_INCOMPLETE_P(x)) {
        int len = count_length(SCM_STRING_START(x), SCM_STRING_SIZE(x));
        if (len < 0) return SCM_FALSE;
        x->incomplete = FALSE;
        x->length = len;
    }
    return SCM_OBJ(x);
}

ScmObj Scm_StringIncompleteToComplete(ScmString *x)
{
    return Scm_StringIncompleteToCompleteX(SCM_STRING(Scm_CopyString(x)));
}

/*----------------------------------------------------------------
 * Comparison
 */

/* TODO: merge Equal and Cmp API; required generic comparison protocol */
int Scm_StringEqual(ScmString *x, ScmString *y)
{
    if ((SCM_STRING_INCOMPLETE_P(x) && !SCM_STRING_INCOMPLETE_P(y))
        || (!SCM_STRING_INCOMPLETE_P(x) && SCM_STRING_INCOMPLETE_P(y))) {
        return FALSE;
    }
    if (SCM_STRING_SIZE(x) != SCM_STRING_SIZE(y)) {
        return FALSE;
    }
    return (memcmp(SCM_STRING_START(x), SCM_STRING_START(y), SCM_STRING_SIZE(x)) == 0? TRUE : FALSE);
}

int Scm_StringCmp(ScmString *x, ScmString *y)
{
    int sizx, sizy, siz, r;
    if ((SCM_STRING_INCOMPLETE_P(x) && !SCM_STRING_INCOMPLETE_P(y))
        || (!SCM_STRING_INCOMPLETE_P(x) && SCM_STRING_INCOMPLETE_P(y))){
        Scm_Error("cannot compare incomplete vs complete string: %S, %S",
                  SCM_OBJ(x), SCM_OBJ(y));
    }
    sizx = SCM_STRING_SIZE(x);
    sizy = SCM_STRING_SIZE(y);
    siz = (sizx < sizy)? sizx : sizy;
    r = memcmp(SCM_STRING_START(x), SCM_STRING_START(y), siz);
    if (r == 0) return (sizx - sizy);
    else return r;
}

/* single-byte case insensitive comparison */
static int sb_strcasecmp(const char *px, int sizx,
                         const char *py, int sizy)
{
    char cx, cy;
    for (; sizx > 0 && sizy > 0; sizx--, sizy--, px++, py++) {
        cx = tolower(*px);
        cy = tolower(*py);
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
    
    if (SCM_STRING_INCOMPLETE_P(x) || SCM_STRING_INCOMPLETE_P(y)) {
        Scm_Error("cannot compare incomplete strings in case-insensitive way: %S, %S",
                  SCM_OBJ(x), SCM_OBJ(y));
    }
    sizx = SCM_STRING_SIZE(x); lenx = SCM_STRING_SIZE(x);
    sizy = SCM_STRING_SIZE(y); leny = SCM_STRING_SIZE(y);
    px = SCM_STRING_START(x);
    py = SCM_STRING_START(y);
    
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

ScmChar Scm_StringRef(ScmString *str, int pos)
{
    int len = SCM_STRING_LENGTH(str);

    /* we can't allow string-ref on incomplete strings, since it may yield
       invalid character object. */
    if (SCM_STRING_INCOMPLETE_P(str)) 
        Scm_Error("incomplete string not allowed : %S", str);
    if (pos < 0 || pos >= len) Scm_Error("argument out of range: %d", pos);
    if (SCM_STRING_SINGLE_BYTE_P(str)) {
        return (ScmChar)(SCM_STRING_START(str)[pos]);
    } else {
        const char *p = forward_pos(SCM_STRING_START(str), pos);
        ScmChar c;
        SCM_CHAR_GET(p, c);
        return c;
    }
}

int Scm_StringByteRef(ScmString *str, int offset)
{
    if (offset < 0 || offset >= SCM_STRING_SIZE(str)) {
        Scm_Error("argument out of range: %d", offset);
    }
    return (ScmByte)SCM_STRING_START(str)[offset];
}

/* External interface of forward_pos.  Returns the pointer to the
   offset-th character in str. */
/* NB: this function allows offset == length of the string; in that
   case, the return value points the location past the string body,
   but it is necessary sometimes to do a pointer arithmetic with the
   returned values. */
const char *Scm_StringPosition(ScmString *str, int offset)
{
    if (offset < 0 || offset > SCM_STRING_LENGTH(str)) {
        Scm_Error("argument out of range: %d", offset);
    }
    if (SCM_STRING_INCOMPLETE_P(str)) {
        return (SCM_STRING_START(str)+offset);
    } else {
        return (forward_pos(SCM_STRING_START(str), offset));
    }
}

/*----------------------------------------------------------------
 * Concatenation
 */

ScmObj Scm_StringAppend2(ScmString *x, ScmString *y)
{
    int sizex = SCM_STRING_SIZE(x), lenx = SCM_STRING_LENGTH(x);
    int sizey = SCM_STRING_SIZE(y), leny = SCM_STRING_LENGTH(y);
    int lenz;
    char *p = SCM_NEW_ATOMIC2(char *,sizex + sizey + 1);

    memcpy(p, x->start, sizex);
    memcpy(p+sizex, y->start, sizey);
    p[sizex + sizey] = '\0';

    if (SCM_STRING_INCOMPLETE_P(x) || SCM_STRING_INCOMPLETE_P(y)) {
        lenz = -1;              /* yields incomplete string */
    } else {
        lenz = lenx + leny;
    }
    return SCM_OBJ(make_str(lenz, sizex+sizey, p));
}

ScmObj Scm_StringAppendC(ScmString *x, const char *str, int sizey, int leny)
{
    int sizex = SCM_STRING_SIZE(x), lenx = SCM_STRING_LENGTH(x);
    int lenz;
    char *p;

    if (sizey < 0) count_size_and_length(str, &sizey, &leny);
    else if (leny < 0) leny = count_length(str, sizey);
    
    p = SCM_NEW_ATOMIC2(char *, sizex + sizey + 1);
    memcpy(p, x->start, sizex);
    memcpy(p+sizex, str, sizey);
    p[sizex+sizey] = '\0';

    if (SCM_STRING_INCOMPLETE_P(x) || leny < 0) {
        lenz = -1;
    } else {
        lenz = lenx + leny;
    }
    return SCM_OBJ(make_str(lenz, sizex + sizey, p));
}

ScmObj Scm_StringAppend(ScmObj strs)
{
    ScmObj cp;
    int size = 0, len = 0, completep = TRUE;
    char *buf, *bufp;
    SCM_FOR_EACH(cp, strs) {
        ScmObj str = SCM_CAR(cp);
        if (!SCM_STRINGP(str)) Scm_Error("string required, but got %S\n", str);
        size += SCM_STRING_SIZE(str);
        if (completep) {
            if (SCM_STRING_INCOMPLETE_P(str)) completep = FALSE;
            len += SCM_STRING_LENGTH(str);
        }
    }

    bufp = buf = SCM_NEW_ATOMIC2(char *, size+1);
    SCM_FOR_EACH(cp, strs) {
        ScmObj str = SCM_CAR(cp);
        memcpy(bufp, SCM_STRING_START(str), SCM_STRING_SIZE(str));
        bufp += SCM_STRING_SIZE(str);
    }
    *bufp = '\0';
    return SCM_OBJ(make_str(completep? len : -1, size, buf));
}

ScmObj Scm_StringJoin(ScmObj strs, ScmString *delim, int grammer)
{
    ScmObj cp;
    int size = 0, len = 0, nstrs = 0, ndelim = 0;
    int dsize = SCM_STRING_SIZE(delim), dlen = SCM_STRING_LENGTH(delim);
    char *buf, *bufp;

    if (SCM_NULLP(strs)) {
        if (grammer == SCM_STRING_JOIN_STRICT_INFIX)
            Scm_Error("can't join empty list of strings with strict-infix grammer");
        return SCM_MAKE_STR("");
    }

    SCM_FOR_EACH(cp, strs) {
        ScmObj str = SCM_CAR(cp);
        if (!SCM_STRINGP(str)) Scm_Error("string required, but got %S\n", str);
        size += SCM_STRING_SIZE(str);
        if (SCM_STRING_INCOMPLETE_P(str) || len < 0) {
            len = -1;
        } else {
            len += SCM_STRING_LENGTH(str);
        }
        nstrs++;
    }
    if (grammer == SCM_STRING_JOIN_INFIX
        || grammer == SCM_STRING_JOIN_STRICT_INFIX) {
        ndelim = nstrs - 1;
    } else {
        ndelim = nstrs;
    }
    size += dsize * ndelim;
    if (len >= 0 && !SCM_STRING_INCOMPLETE_P(delim)) len += dlen * ndelim;
    else len = -1;

    bufp = buf = SCM_NEW_ATOMIC2(char *, size+1);
    if (grammer == SCM_STRING_JOIN_PREFIX) {
        memcpy(bufp, SCM_STRING_START(delim), dsize);
        bufp += dsize;
    }
    SCM_FOR_EACH(cp, strs) {
        ScmObj str = SCM_CAR(cp);
        memcpy(bufp, SCM_STRING_START(str), SCM_STRING_SIZE(str));
        bufp += SCM_STRING_SIZE(str);
        if (SCM_PAIRP(SCM_CDR(cp))) {
            memcpy(bufp, SCM_STRING_START(delim), dsize);
            bufp += dsize;
        }
    }
    if (grammer == SCM_STRING_JOIN_SUFFIX) {
        memcpy(bufp, SCM_STRING_START(delim), dsize);
        bufp += dsize;
    }
    *bufp = '\0';
    return SCM_OBJ(make_str(len, size, buf));
}

/*----------------------------------------------------------------
 * Substitution
 */

static ScmObj string_substitute(ScmString *x, int start,
                                const char *str, int sizey, int leny,
                                int incompletep)
{
    int sizex = SCM_STRING_SIZE(x), lenx = SCM_STRING_LENGTH(x);
    int end = start + leny, sizez;
    char *p;

    CHECK_MUTABLE(x);
    if (start < 0) Scm_Error("start index out of range: %d", start);
    if (end > lenx) {
        Scm_Error("substitution string too long: %S", make_str(leny, sizey, str));
    }

    if (SCM_STRING_SINGLE_BYTE_P(x)) {
        /* x is sbstring */
        sizez = sizex - leny + sizey;
        p = SCM_NEW_ATOMIC2(char *, sizez+1);
        if (start > 0) memcpy(p, SCM_STRING_START(x), start);
        memcpy(p+start, str, sizey);
        memcpy(p+start+sizey, SCM_STRING_START(x)+end, sizex-end);
        p[sizez] = '\0';
    } else {
        /* x is mbstring */
        const char *s, *e;
        s = forward_pos(x->start, start);
        e = forward_pos(s, end - start);
        sizez = sizex + sizey - (e - s);
        p = SCM_NEW_ATOMIC2(char *, sizez+1);
        if (start > 0) memcpy(p, x->start, s - x->start);
        memcpy(p + (s - x->start), str, sizey);
        memcpy(p + (s - x->start) + sizey, e, x->start + sizex - e);
        p[sizez] = '\0';
    }
    /* modify x */
    x->incomplete = SCM_STRING_INCOMPLETE_P(x) || incompletep;
    x->length = x->incomplete? sizez : lenx;
    x->size = sizez;
    x->start = p;
    return SCM_OBJ(x);
}

ScmObj Scm_StringSubstitute(ScmString *x, int start, ScmString *y)
{
    return string_substitute(x, start,
                             SCM_STRING_START(y), SCM_STRING_SIZE(y),
                             SCM_STRING_LENGTH(y),
                             SCM_STRING_INCOMPLETE_P(y));
}

ScmObj Scm_StringSet(ScmString *x, int k, ScmChar ch)
{
    if (SCM_STRING_INCOMPLETE_P(x)) {
        char byte = (char)ch;
        return string_substitute(x, k, &byte, 1, 1, TRUE);
    } else {
        char buf[SCM_CHAR_MAX_BYTES+1];
        int size = SCM_CHAR_NBYTES(ch);
        SCM_CHAR_PUT(buf, ch);
        return string_substitute(x, k, buf, size, 1, FALSE);
    }
}

ScmObj Scm_StringByteSet(ScmString *x, int k, ScmByte b)
{
    int size = SCM_STRING_SIZE(x);
    char *p;
    CHECK_MUTABLE(x);
    if (k < 0 || k >= size) Scm_Error("argument out of range: %d", k);
    p = SCM_NEW_ATOMIC2(char *, size+1);
    memcpy(p, x->start, size);
    p[size] = '\0';
    p[k] = (char)b;
    x->start = p;
    x->incomplete = TRUE;
    x->length = x->size;
    return SCM_OBJ(x);
}

/*----------------------------------------------------------------
 * Substring
 */

ScmObj Scm_Substring(ScmString *x, int start, int end)
{
    if (start < 0)
        Scm_Error("start argument needs to be positive: %d", start);
    if (end > SCM_STRING_LENGTH(x))
        Scm_Error("end argument is out of range: %d", end);
    if (end < start)
        Scm_Error("end argument must be equal to or greater than the start argument: start=%d, end=%d", start, end);
    if (SCM_STRING_SINGLE_BYTE_P(x)) {
        return SCM_OBJ(make_str(SCM_STRING_INCOMPLETE_P(x)? -1 : (end-start),
                                end-start,
                                SCM_STRING_START(x) + start));
    } else {
        const char *s, *e;
        if (start) s = forward_pos(x->start, start); else s = x->start;
        e = forward_pos(s, end - start);
        return SCM_OBJ(make_str(end - start, e - s, s));
    }
}

/* Auxiliary procedure to support optional start/end parameter specified
   in lots of SRFI-13 functions.   If start and end is specified and restricts
   string range, call substring.  Otherwise returns x itself. */
ScmObj Scm_MaybeSubstring(ScmString *x, ScmObj start, ScmObj end)
{
    int istart, iend;
    if (SCM_UNBOUNDP(start) || SCM_UNDEFINEDP(start)) {
        istart = 0;
    } else {
        if (!SCM_INTP(start))
            Scm_Error("exact integer required for start, but got %S", start);
        istart = SCM_INT_VALUE(start);
    }

    if (SCM_UNBOUNDP(end) || SCM_UNDEFINEDP(end)) {
        if (istart == 0) return SCM_OBJ(x);
        iend = SCM_STRING_LENGTH(x);
    } else {
        if (!SCM_INTP(end))
            Scm_Error("exact integer required for start, but got %S", end);
        iend = SCM_INT_VALUE(end);
    }
    return Scm_Substring(x, istart, iend);
}

/*----------------------------------------------------------------
 * Search & parse
 */

/* Split string by char.  Char itself is not included in the result. */
/* TODO: fix semantics.  What should be returned for (string-split "" #\.)? */
ScmObj Scm_StringSplitByChar(ScmString *str, ScmChar ch)
{
    int size = SCM_STRING_SIZE(str), sizecnt = 0;
    int lencnt = 0;
    const char *s = SCM_STRING_START(str), *p = s, *e = s + size;
    ScmObj head = SCM_NIL, tail = SCM_NIL;

    if (SCM_STRING_INCOMPLETE_P(str)) {
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
static inline int boyer_moore(const char *ss1, int siz1,
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

/* Scan s2 in s1.  If both strings are single-byte, and s1 is long,
   we use Boyer-Moore.
   
   To avoid rescanning of the string, this function can return
   various information, depends on retmode argument.

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
ScmObj Scm_StringScan(ScmString *s1, ScmString *s2, int retmode)
{
    int i, incomplete;
    const char *ss1 = SCM_STRING_START(s1);
    const char *ss2 = SCM_STRING_START(s2);
    int siz1 = SCM_STRING_SIZE(s1), len1 = SCM_STRING_LENGTH(s1);
    int siz2 = SCM_STRING_SIZE(s2), len2 = SCM_STRING_LENGTH(s2);

    if (retmode < 0 || retmode > SCM_STRING_SCAN_BOTH) {
        Scm_Error("return mode out fo range: %d", retmode);
    }

    if (siz2 == 0) {
        /* shortcut */
        switch (retmode) {
        case SCM_STRING_SCAN_INDEX: return SCM_MAKE_INT(0);
        case SCM_STRING_SCAN_BEFORE: return SCM_MAKE_STR("");
        case SCM_STRING_SCAN_AFTER:  return Scm_CopyString(s1);
        case SCM_STRING_SCAN_BEFORE2:;
        case SCM_STRING_SCAN_AFTER2:;
        case SCM_STRING_SCAN_BOTH:
            return Scm_Values2(SCM_MAKE_STR(""), Scm_CopyString(s1));
        }
    }
    
    if (siz1 == len1) {
        if (siz2 == len2) goto sbstring;
        goto failed;            /* sbstring can't contain mbstring. */   
    }
    if (len1 >= len2) {
        const char *ssp = ss1;
        for (i=0; i<=len1-len2; i++) {
            if (memcmp(ssp, ss2, siz2) == 0) {
                switch (retmode) {
                case SCM_STRING_SCAN_INDEX:
                    return Scm_MakeInteger(i);
                case SCM_STRING_SCAN_BEFORE:
                    return Scm_MakeString(ss1, ssp-ss1, i, 0);
                case SCM_STRING_SCAN_AFTER:
                    return Scm_MakeString(ssp+siz2, siz1-(ssp-ss1+siz2),
                                          len1-i-len2, 0);
                case SCM_STRING_SCAN_BEFORE2:
                    return Scm_Values2(Scm_MakeString(ss1, ssp-ss1, i, 0),
                                       Scm_MakeString(ssp, siz1-(ssp-ss1),
                                                      len1-i, 0));
                case SCM_STRING_SCAN_AFTER2:
                    return Scm_Values2(Scm_MakeString(ss1, ssp-ss1+siz2,
                                                      i+len2, 0),
                                       Scm_MakeString(ssp+siz2,
                                                      siz1-(ssp-ss1+siz2),
                                                      len1-i-len2, 0));
                case SCM_STRING_SCAN_BOTH:
                    return Scm_Values2(Scm_MakeString(ss1, ssp-ss1, i, 0),
                                       Scm_MakeString(ssp+siz2,
                                                      siz1-(ssp-ss1+siz2),
                                                      len1-i-len2, 0));
                }
            }
            ssp += SCM_CHAR_NFOLLOWS(*ssp) + 1;
        }
    }
    goto failed;

  sbstring: /* short cut for single-byte strings */
    if (siz1 < siz2) goto failed;
    if (siz1 < 256 || siz2 >= 256) {
        /* brute-force search */
        for (i=0; i<=siz1-siz2; i++) {
            if (memcmp(ss2, ss1+i, siz2) == 0) break;
        }
        if (i == siz1-siz2+1) goto failed;
    } else {
        i = boyer_moore(ss1, siz1, ss2, siz2);
        if (i < 0) goto failed;
    }
    incomplete =
        (SCM_STRING_INCOMPLETE_P(s1) || SCM_STRING_INCOMPLETE_P(s2))?
        SCM_MAKSTR_INCOMPLETE : 0;
    switch (retmode) {
    case SCM_STRING_SCAN_INDEX:
        return Scm_MakeInteger(i);
    case SCM_STRING_SCAN_BEFORE:
        return Scm_MakeString(ss1, i, i, incomplete);
    case SCM_STRING_SCAN_AFTER:
        return Scm_MakeString(ss1+i+siz2, siz1-(i+siz2), siz1-(i+siz2),
                              incomplete);
    case SCM_STRING_SCAN_BEFORE2:
        return Scm_Values2(Scm_MakeString(ss1, i, i, incomplete),
                           Scm_MakeString(ss1+i, siz1-i, siz1-i, incomplete));
    case SCM_STRING_SCAN_AFTER2:
        return Scm_Values2(Scm_MakeString(ss1, i+siz2, i+siz2, incomplete),
                           Scm_MakeString(ss1+i+siz2, siz1-(i+siz2),
                                          siz1-(i+siz2), incomplete));
    case SCM_STRING_SCAN_BOTH:
        return Scm_Values2(Scm_MakeString(ss1, i, i, incomplete),
                           Scm_MakeString(ss1+i+siz2, siz1-(i+siz2),
                                          siz1-(i+siz2), incomplete));
    }
  failed:
    if (retmode <= SCM_STRING_SCAN_AFTER) {
        return SCM_FALSE;
    } else {
        return Scm_Values2(SCM_FALSE, SCM_FALSE);
    }
}

ScmObj Scm_StringScanChar(ScmString *s1, ScmChar ch, int retmode)
{
    ScmString s2;
    char buf[SCM_CHAR_MAX_BYTES];
    SCM_CHAR_PUT(buf, ch);
    SCM_SET_CLASS(&s2, SCM_CLASS_STRING);
    s2.incomplete = FALSE;
    s2.immutable = TRUE;
    s2.length = 1;
    s2.size = SCM_CHAR_NBYTES(ch);
    s2.start = buf;
    return Scm_StringScan(s1, &s2, retmode);
}

/*----------------------------------------------------------------
 * Miscellaneous functions
 */

ScmObj Scm_StringP(ScmObj obj)
{
    return SCM_STRINGP(obj)? SCM_TRUE : SCM_FALSE;
}

int Scm_StringLength(ScmString *str)
{
    return SCM_STRING_LENGTH(str);
}

ScmObj Scm_StringToList(ScmString *str)
{
    ScmObj start = SCM_NIL, end = SCM_NIL;
    const char *bufp = SCM_STRING_START(str);
    int len = SCM_STRING_LENGTH(str);
    ScmChar ch;

    if (SCM_STRING_INCOMPLETE_P(str))
        Scm_Error("incomplete string not supported: %S", str);
    while (len-- > 0) {
        SCM_CHAR_GET(bufp, ch);
        bufp += SCM_CHAR_NBYTES(ch);
        SCM_APPEND1(start, end, SCM_MAKE_CHAR(ch));
    }
    return start;
}

ScmObj Scm_StringFill(ScmString *str, ScmChar ch,
                      ScmObj maybe_start, ScmObj maybe_end)
{
    int len, i, start, end, prelen, midlen, postlen;
    int chlen = SCM_CHAR_NBYTES(ch);
    char *newstr, *p;
    const unsigned char *s, *r;

    CHECK_MUTABLE(str);
    if (SCM_STRING_INCOMPLETE_P(str)) {
        Scm_Error("incomplete string not allowed: %S", str);
    }
    len = SCM_STRING_LENGTH(str);

    if (SCM_UNBOUNDP(maybe_start) || SCM_UNDEFINEDP(maybe_start)) {
        start = 0;
    } else {
        if (!SCM_INTP(maybe_start))
            Scm_Error("exact integer required for start, but got %S",
                      maybe_start);
        start = SCM_INT_VALUE(maybe_start);
    }
    if (SCM_UNBOUNDP(maybe_end) || SCM_UNDEFINEDP(maybe_end)) {
        end = len;
    } else {
        if (!SCM_INTP(maybe_end))
            Scm_Error("exact integer required for end, but got %S",
                      maybe_end);
        end = SCM_INT_VALUE(maybe_end);
    }
    if (start < 0 || start > end || end > len) {
        Scm_Error("start/end pair is out of range: (%d %d)", start, end);
    }
    if (start == end) return SCM_OBJ(str);
    
    s = SCM_STRING_START(str);
    for (i = 0; i < start; i++) s += SCM_CHAR_NFOLLOWS(*s)+1;
    prelen = s - SCM_STRING_START(str);
    r = s;
    for (; i < end; i++)        s += SCM_CHAR_NFOLLOWS(*s)+1;
    midlen = s - r;
    postlen = SCM_STRING_SIZE(str) - midlen - prelen;

    p = newstr = SCM_NEW_ATOMIC2(char *,
                                 prelen + (end-start)*chlen + postlen + 1);
    memcpy(p, SCM_STRING_START(str), prelen);
    p += prelen;
    for (i=0; i < end-start; i++) {
        SCM_CHAR_PUT(p, ch);
        p += chlen;
    }
    memcpy(p, SCM_STRING_START(str) + prelen + midlen, postlen);
    p[postlen] = '\0';          /* be friendly to C */
    /* modify str */
    str->size = prelen + (end-start)*chlen + postlen;
    str->start = newstr;
    return SCM_OBJ(str);
}

ScmObj Scm_ConstCStringArrayToList(const char **array, int size)
{
    int i;
    ScmObj h = SCM_NIL, t = SCM_NIL;
    if (size < 0) {
        for (;*array; array++) SCM_APPEND1(h, t, SCM_MAKE_STR(*array));
    } else {
        for (i=0; i<size; i++) SCM_APPEND1(h, t, SCM_MAKE_STR(*array));
    }
    return h;
}

ScmObj Scm_CStringArrayToList(char **array, int size)
{
    int i;
    ScmObj h = SCM_NIL, t = SCM_NIL;
    if (size < 0) {
        for (;*array; array++)
            SCM_APPEND1(h, t, SCM_MAKE_STR_COPYING(*array));
    } else {
        for (i=0; i<size; i++)
            SCM_APPEND1(h, t, SCM_MAKE_STR_COPYING(*array));
    }
    return h;
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
        if (SCM_STRING_SINGLE_BYTE_P(str)) {
            const char *cp = SCM_STRING_START(str);
            int size = SCM_STRING_SIZE(str);
            if (SCM_STRING_INCOMPLETE_P(str)) {
                SCM_PUTZ("#*\"", -1, port);
            } else {
                SCM_PUTC('"', port);
            }
            while (size--) {
                string_putc(*cp++, port, SCM_STRING_INCOMPLETE_P(str));
            }
        } else {
            ScmChar ch;
            const char *cp = SCM_STRING_START(str);
            int len = SCM_STRING_LENGTH(str);

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
    int len = SCM_STRING_LENGTH(src);
    int effective_size;
    const char *sptr, *ptr, *eptr;
    ScmStringPointer *sp;

    SCM_CHECK_START_END(start, end, len);
    while (index < 0) index += (end - start) + 1;
    if (index > (end - start)) goto badindex;
    
    if (SCM_STRING_SINGLE_BYTE_P(src)) {
        sptr = src->start + start;
        ptr = sptr + index;
        effective_size = end - start;
    } else {
        sptr = forward_pos(src->start, start);
        ptr = forward_pos(sptr, index);
        eptr = forward_pos(sptr, end - start);
        effective_size = eptr - ptr;
    }
    sp = SCM_NEW(ScmStringPointer);
    SCM_SET_CLASS(sp, SCM_CLASS_STRING_POINTER);
    sp->length = (SCM_STRING_INCOMPLETE_P(src)? -1 : (end-start));
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
    if (sp->length < 0) {
        if (afterp)
            return SCM_OBJ(make_str(-1, sp->size - sp->index, sp->current));
        else
            return SCM_OBJ(make_str(-1, sp->index, sp->start));
    } else {
        if (afterp)
            return SCM_OBJ(make_str(sp->length - sp->index,
                                    sp->start + sp->size - sp->current,
                                    sp->current));
        else
            return SCM_OBJ(make_str(sp->index,
                                    sp->current - sp->start,
                                    sp->start));
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
#if SCM_DEBUG_HELPER
void Scm_StringPointerDump(ScmStringPointer *sp1)
{
    Scm_Printf(SCM_CUROUT,
               "<sp addr: %p len: %d size: %d start: %p index: %d cur: %d>\n",
               sp1, sp1->length, sp1->size, sp1->start, sp1->index,
               sp1->current);
}
#endif /*SCM_DEBUG_HELPER*/

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

inline int Scm_DStringSize(ScmDString *dstr)
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

ScmObj Scm_DStringGet(ScmDString *dstr)
{
    int len, size;
    const char *str = dstring_getz(dstr, &len, &size);
    return SCM_OBJ(make_str(len, size, str));
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
    if (size < 0) size = strlen(str);
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
    int size = SCM_STRING_SIZE(str);
    if (size == 0) return;
    if (dstr->current + size > dstr->end) {
        Scm__DStringRealloc(dstr, size);
    }
    memcpy(dstr->current, SCM_STRING_START(str), size);
    dstr->current += size;
    if (dstr->length >= 0 && !SCM_STRING_INCOMPLETE_P(str)) {
        dstr->length += SCM_STRING_LENGTH(str);
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
#if SCM_DEBUG_HELPER
void Scm_DStringDump(FILE *out, ScmDString *dstr)
{
    fprintf(out, "DString %p\n", dstr);
    if (dstr->anchor) {
        ScmDStringChain *chain; int i;
        fprintf(out, "  chunk0[%3d] = \"", dstr->init.bytes);
        fwrite(dstr->init.data, 1, dstr->init.bytes, out);
        fprintf(out, "\"\n");
        for (i=1, chain = dstr->anchor; chain; chain = chain->next, i++) {
            int size = (chain->next? chain->chunk->bytes : (int)(dstr->current - dstr->tail->chunk->data));
            fprintf(out, "  chunk%d[%3d] = \"", i, size);
            fwrite(chain->chunk->data, 1, size, out);
            fprintf(out, "\"\n");
        }
    } else {
        int size = (int)(dstr->current - dstr->init.data);
        fprintf(out, "  chunk0[%3d] = \"", size);
        fwrite(dstr->init.data, 1, size, out);
        fprintf(out, "\"\n");
    }
}
#endif /*SCM_DEBUG_HELPER*/

