/*
 * string.c - string implementation
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
 *  $Id: string.c,v 1.45 2001-06-02 09:01:46 shirok Exp $
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include "gauche.h"

static void string_print(ScmObj obj, ScmPort *port, ScmWriteContext *);
SCM_DEFINE_BUILTIN_CLASS(Scm_StringClass, string_print, NULL, NULL,
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
void Scm_StringDump(FILE *out, ScmObj str)
{
    int i;
    int s = SCM_STRING_SIZE(str);
    const char *p = SCM_STRING_START(str);

    fprintf(out, "STR(len=%ld,siz=%d) \"", SCM_STRING_LENGTH(str), s);
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
        unsigned char c = (unsigned char)*str++;
        int i = SCM_CHAR_NFOLLOWS(c);
        if (i < 0 || i > size) return -1;
        count++;
        str += i;
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
        p[sizez+1] = '\0';
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
        p[sizez+1] = '\0';
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
    int len = SCM_STRING_LENGTH(str), lencnt = 0;
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
static inline ScmObj boyer_moore(const char *ss1, int siz1,
                                 const char *ss2, int siz2)
{
    unsigned char shift[256];
    int i, j;
    for (i=0; i<256; i++) { shift[i] = siz2; }
    for (i=0; i<siz2; i++) {
        if (shift[(unsigned char)ss2[i]] > i)
            shift[(unsigned char)ss2[i]] = i;
    }
    for (i=j=siz2-1; i<siz1; i++) {
        if (ss2[j] == ss1[i]) {
            for (; j > 0; i--, j--) {
                if (ss2[j] != ss1[i]) return SCM_FALSE;
            }
            return Scm_MakeInteger(i);
        } else {
            i += shift[(unsigned char)ss2[j]];
        }
    }
    return SCM_FALSE;
}

/* See if s2 appears in s1.  If both strings are single-byte, and s1
   is long, we use Boyer-Moore. */
ScmObj Scm_StringContains(ScmString *s1, ScmString *s2)
{
    int i;
    const char *ss1 = SCM_STRING_START(s1);
    const char *ss2 = SCM_STRING_START(s2);
    int siz1 = SCM_STRING_SIZE(s1), len1 = SCM_STRING_LENGTH(s1);
    int siz2 = SCM_STRING_SIZE(s2), len2 = SCM_STRING_LENGTH(s2);

    if (SCM_STRING_INCOMPLETE_P(s1)) {
        Scm_Error("incomplete string not supported: %S", s1);
    }
    if (SCM_STRING_INCOMPLETE_P(s2)) {
        Scm_Error("incomplete string not supported: %S", s2);
    }
    if (siz1 == len1) {
        if (len2 < 0 || siz2 == len2) goto sbstring;
        return SCM_FALSE;       /* sbstring can't contain mbstring. */
    }
    if (len1 < len2) return SCM_FALSE;
    else {
        const char *ssp = ss1;
        for (i=0; i<=len1-len2; i++) {
            if (memcmp(ssp, ss2, siz2) == 0) return Scm_MakeInteger(i);
            ssp += SCM_CHAR_NFOLLOWS(*ssp);
        }
        return SCM_FALSE;
    }
    return SCM_FALSE;
  sbstring: /* short cut for special case */
    if (siz1 < siz2) return SCM_FALSE;
    if (siz1 < 256 || siz2 >= 256) {
        /* brute-force search */
        for (i=0; i<=siz1-siz2; i++) {
            if (memcmp(ss2, ss1+i, siz2) == 0) return Scm_MakeInteger(i);
        }
    } else {
        return boyer_moore(ss1, siz1, ss2, siz2);
    }
    return SCM_FALSE;
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
    const char *s, *r;

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
                SCM_PUTZ("#\"", -1, port);
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

ScmObj Scm_MakeStringPointer(ScmString *src, int index)
{
    const char *ptr;
    ScmStringPointer *sp;
    if (SCM_STRING_SINGLE_BYTE_P(src)) {
        while (index < 0) index += SCM_STRING_SIZE(src)+1;
        if (index > SCM_STRING_SIZE(src)) goto badindex;
        else           ptr = src->start + index;
    } else {
        while (index < 0) index += SCM_STRING_LENGTH(src)+1;
        if (index > SCM_STRING_LENGTH(src)) goto badindex;
        ptr = forward_pos(src->start, index);
    }
    sp = SCM_NEW(ScmStringPointer);
    SCM_SET_CLASS(sp, SCM_CLASS_STRING_POINTER);
    sp->length = (SCM_STRING_INCOMPLETE_P(src)? -1 : SCM_STRING_LENGTH(src));
    sp->size = SCM_STRING_SIZE(src);
    sp->start = SCM_STRING_START(src);
    sp->index = index;
    sp->current = ptr;
    return SCM_OBJ(sp);
  badindex:
    Scm_Error("index out of range: %d", index);
    return SCM_UNDEFINED;
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

/*==================================================================
 *
 * Dynamic strings
 *
 */

#define DSTRING_CHUNK_SIZE 16
#define DSTRING_CHUNK_ROUND_UP(siz) \
    ((siz+DSTRING_CHUNK_SIZE-1)&~(DSTRING_CHUNK_SIZE-1))

void Scm_DStringInit(ScmDString *dstr)
{
    dstr->start = SCM_NEW_ATOMIC2(char *, DSTRING_CHUNK_SIZE);
    dstr->end = dstr->start + DSTRING_CHUNK_SIZE;
    dstr->current = dstr->start;
    dstr->length = 0;
}

void Scm__DStringRealloc(ScmDString *dstr, int minincr)
{
    char *p;
    int newsize = dstr->end - dstr->start + DSTRING_CHUNK_ROUND_UP(minincr);
    int cursize = dstr->current - dstr->start;

    p = (char *)SCM_REALLOC(dstr->start, newsize);
    dstr->start = p;
    dstr->end = p + newsize;
    dstr->current = p + cursize;
}

/* We don't need to copy the string, thanks to GC.
 * If the dynamic string is not reused, unused part of allocated buffer
 * (between dstr->current and dstr->end) will remain unused, but it
 * won't matter since it is small (< DSTRING_CHUNK_SIZE) and such unused
 * hole happens anyway in memory allocation level.
 */
ScmObj Scm_DStringGet(ScmDString *dstr)
{
    int len = dstr->length;
    int size = dstr->current - dstr->start;
    
    if (len < 0) {
        len = count_length(dstr->start, size);
    }
    return SCM_OBJ(make_str(len, size, dstr->start));
}

/* For conveninence.   Note that dstr may already contain NUL byte in it,
   in that case you'll get chopped string. */
const char *Scm_DStringGetz(ScmDString *dstr)
{
    SCM_DSTRING_PUTB(dstr, '\0');
    return dstr->start;
}

void Scm_DStringPutz(ScmDString *dstr, const char *str, int size)
{
    if (size < 0) size = strlen(str);
    while (dstr->current + size >= dstr->end) {
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
    while (dstr->current + size >= dstr->end) {
        Scm__DStringRealloc(dstr, size);
    }
    memcpy(dstr->current, SCM_STRING_START(str), size);
    dstr->current += size;
    if (dstr->length >= 0 && SCM_STRING_INCOMPLETE_P(str)) {
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
void Scm_DStringDump(FILE *out, ScmDString *dstr)
{
    fprintf(out, "DSTR %p-%p (%p)  len=%d\n",
            dstr->start, dstr->end, dstr->current, dstr->length);
}

