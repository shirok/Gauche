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
 *  $Id: string.c,v 1.28 2001-04-26 08:23:00 shiro Exp $
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include "gauche.h"

static void string_print(ScmObj obj, ScmPort *port, ScmWriteContext *);
SCM_DEFINE_BUILTIN_CLASS(Scm_StringClass, string_print, NULL, NULL,
                         SCM_CLASS_SEQUENCE_CPL);

static ScmString *make_str(int len, int siz, const char *p)
{
    ScmString *s = SCM_NEW(ScmString);
    SCM_SET_CLASS(s, SCM_CLASS_STRING);
    s->length = len;
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
        if (i < 0) return -1;
        count++;
        while (i-- > 0) {
            str++;
            if (size-- < 0) return -1;
        }
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

ScmObj Scm_MakeStringConst(const char *str, int size, int len)
{
    if (size < 0) count_size_and_length(str, &size, &len);
    else if (len < 0) len = count_length(str, size);
    return SCM_OBJ(make_str(len, size, str));
}

ScmObj Scm_MakeString(const char *str, int size, int len)
{
    char *nstr;

    if (size < 0) count_size_and_length(str, &size, &len);
    else if (len < 0) len = count_length(str, size);
    nstr = SCM_NEW_ATOMIC2(char *, size + 1);
    memcpy(nstr, str, size+1);  /* includes \0 */
    return SCM_OBJ(make_str(len, size, nstr));
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

static ScmObj makestring_from_list(ScmObj chars)
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
    return Scm_MakeStringConst(buf, size, len);
}

ScmObj Scm_MakeStringFromList(ScmObj chars)
{
    return makestring_from_list(chars);
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

ScmObj Scm_CopyString(ScmString *x)
{
    return SCM_OBJ(make_str(SCM_STRING_LENGTH(x), SCM_STRING_SIZE(x),
                            SCM_STRING_START(x)));
}

/*----------------------------------------------------------------
 * Comparison
 */

int Scm_StringCmp(ScmString *x, ScmString *y)
{
    int sizx = SCM_STRING_SIZE(x);
    int sizy = SCM_STRING_SIZE(y);
    int siz = (sizx < sizy)? sizx : sizy;
    int r = memcmp(SCM_STRING_START(x), SCM_STRING_START(y), siz);
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
    int sizx = SCM_STRING_SIZE(x), lenx = SCM_STRING_SIZE(x);
    int sizy = SCM_STRING_SIZE(y), leny = SCM_STRING_SIZE(y);
    const char *px = SCM_STRING_START(x);
    const char *py = SCM_STRING_START(y);
    
    if ((sizx == lenx && sizy == leny)|| lenx < 0 || leny < 0) {
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
    int size = SCM_STRING_SIZE(str);
    
    if (pos >= 0) {
        if (len > 0 && len != size) {
            if (pos < len) {
                const char *p = forward_pos(SCM_STRING_START(str), pos);
                ScmChar c;
                SCM_CHAR_GET(p, c);
                return c;
            }
        } else {
            if (pos < size) {
                return (ScmChar)(SCM_STRING_START(str)[pos]);
            }
        }
    }
    Scm_Error("argument out of range: %d", pos);
    /* NOTREACHED */
    return (ScmChar)-1;
}

int Scm_StringByteRef(ScmString *str, int offset)
{
    if (offset < 0 || offset >= SCM_STRING_SIZE(str)) {
        Scm_Error("argument out of range: %d", offset);
    }
    return (ScmByte)SCM_STRING(str)->start[offset];
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

    if (lenx < 0 || leny < 0) {
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

    if (lenx < 0 || leny < 0) {
        lenz = -1;
    } else {
        lenz = lenx + leny;
    }
    return SCM_OBJ(make_str(lenz, sizex + sizey, p));
}

ScmObj Scm_StringAppend(ScmObj strs)
{
    ScmObj cp;
    int size = 0, len = 0;
    char *buf, *bufp;
    SCM_FOR_EACH(cp, strs) {
        ScmObj str = SCM_CAR(cp);
        if (!SCM_STRINGP(str)) Scm_Error("string required, but got %S\n", str);
        size += SCM_STRING_SIZE(str);
        if (len >= 0 && SCM_STRING_LENGTH(str) >= 0) {
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
    return SCM_OBJ(make_str(len, size, buf));
}

ScmObj Scm_StringJoin(ScmObj strs, ScmString *delim)
{
    ScmObj cp;
    int size = 0, len = 0, nstrs = 0;
    int dsize = SCM_STRING_SIZE(delim), dlen = SCM_STRING_LENGTH(delim);
    char *buf, *bufp;

    if (SCM_NULLP(strs)) return SCM_MAKE_STR("");
    
    SCM_FOR_EACH(cp, strs) {
        ScmObj str = SCM_CAR(cp);
        if (!SCM_STRINGP(str)) Scm_Error("string required, but got %S\n", str);
        size += SCM_STRING_SIZE(str);
        if (len >= 0 && SCM_STRING_LENGTH(str) >= 0 && dlen > 0) {
            len += SCM_STRING_LENGTH(str);
        }
        nstrs++;
    }
    size += dsize * (nstrs-1);
    if (len >= 0) len += dlen * (nstrs-1);

    bufp = buf = SCM_NEW_ATOMIC2(char *, size+1);
    SCM_FOR_EACH(cp, strs) {
        ScmObj str = SCM_CAR(cp);
        memcpy(bufp, SCM_STRING_START(str), SCM_STRING_SIZE(str));
        bufp += SCM_STRING_SIZE(str);
        if (SCM_PAIRP(SCM_CDR(cp))) {
            memcpy(bufp, SCM_STRING_START(delim), dsize);
            bufp += dsize;
        }
    }
    *bufp = '\0';
    return SCM_OBJ(make_str(len, size, buf));
}

/*----------------------------------------------------------------
 * Substitution
 */

ScmObj Scm_StringSubstituteCstr(ScmString *x, int start, int end,
                                const char *str, int sizey, int leny)
{
    int sizex = SCM_STRING_SIZE(x), lenx = SCM_STRING_LENGTH(x);
    int sizez, lenz;
    char *p;

    if (start < 0 || end < start) return SCM_FALSE;
    if (lenx < 0 || sizex == lenx) {
        /* x is sbstring */
        if (lenx < 0) {         /* x is incomplete.  length doesn't matter. */
            if (sizey < 0) sizey = strlen(str);
            lenz = -1;
        } else {                /* x is complete sbstring */
            if (sizey < 0) count_size_and_length(str, &sizey, &leny);
            else if (leny < 0) leny = count_length(str, sizey);
            if (leny >= 0) lenz = lenx - (end - start) + leny;
            else lenz = -1;
        }
        if (end > sizex) return SCM_FALSE;
        sizez = sizex - (end - start) + sizey;

        p = SCM_NEW_ATOMIC2(char *, sizez+1);
        if (start > 0) memcpy(p, SCM_STRING_START(x), start);
        memcpy(p+start, str, sizey);
        memcpy(p+start+sizey, SCM_STRING_START(x)+end, sizex-end);
        p[sizez+1] = '\0';
    } else {
        /* x is mbstring */
        const char *s, *e;
        if (sizey < 0) count_size_and_length(str, &sizey, &leny);
        else if (leny < 0) leny = count_length(str, sizey);
        s = forward_pos(x->start, start);
        e = forward_pos(s, end - start);
        sizez = sizex + sizey - (e - s);
        if (leny >= 0) lenz = lenx + leny - (end - start);
        else lenz = -1;

        p = SCM_NEW_ATOMIC2(char *, sizez+1);
        if (start > 0) memcpy(p, x->start, s - x->start);
        memcpy(p + (s - x->start), str, sizey);
        memcpy(p + (s - x->start) + sizey, e, x->start + sizex - e);
        p[sizez+1] = '\0';
    }
    /* modify x */
    x->length = lenz;
    x->size = sizez;
    x->start = p;
    return SCM_OBJ(x);
}

ScmObj Scm_StringSubstitute(ScmString *x, int start, int end, ScmString *y)
{
    int sizey = SCM_STRING_SIZE(y), leny = SCM_STRING_LENGTH(y);

    return Scm_StringSubstituteCstr(x, start, end, y->start, sizey, leny);
}

ScmObj Scm_StringSet(ScmString *x, int k, ScmChar ch)
{
    char buf[SCM_CHAR_MAX_BYTES+1];
    int size = SCM_CHAR_NBYTES(ch);
    SCM_CHAR_PUT(buf, ch);
    return Scm_StringSubstituteCstr(x, k, k+1, buf, size, 1);
}

ScmObj Scm_StringByteSet(ScmString *x, int k, ScmByte b)
{
    int size = SCM_STRING_SIZE(x);
    char *p;
    
    if (k < 0 || k >= size) Scm_Error("argument out of range: %d", k);
    p = SCM_NEW_ATOMIC2(char *, size+1);
    memcpy(p, x->start, size);
    p[size] = '\0';
    p[k] = (char)b;
    /* needs to rescan the string */
    x->length = count_length(p, size);
    x->start = p;
    return SCM_OBJ(x);
}

/*----------------------------------------------------------------
 * Substring
 */

ScmObj Scm_Substring(ScmString *x, int start, int end)
{
    const char *s, *e;
    int lenx = SCM_STRING_LENGTH(x);
    
    if (start < 0)
        Scm_Error("start argument needs to be positive: %d", start);
    if (end > lenx)
        Scm_Error("end argument is out of range: %d", end);
    if (end < start)
        Scm_Error("end argument must be equal to or greater than the start argument: start=%d, end=%d", start, end);
    /* TODO: incomplete string case? */
    if (start) s = forward_pos(x->start, start); else s = x->start;
    e = forward_pos(s, end - start);
    return SCM_OBJ(make_str(end - start, e - s, s));
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
        iend = (SCM_STRING_LENGTH(x) < 0) ?
            SCM_STRING_SIZE(x) : SCM_STRING_LENGTH(x);
    } else {
        if (!SCM_INTP(end))
            Scm_Error("exact integer required for start, but got %S", end);
        iend = SCM_INT_VALUE(end);
    }
    return Scm_Substring(x, istart, iend);
}

/* SRFI-13 string-take and string-drop */
ScmObj Scm_StringTake(ScmString *x, int nchars, int takefirst, int fromright)
{
    int len = SCM_STRING_LENGTH(x);
    if (nchars < 0 || nchars >= len)
        Scm_Error("nchars argument out of range: %d", nchars);
    if (fromright) nchars = len - nchars;
    if (takefirst)
        return Scm_Substring(x, 0, nchars);
    else
        return Scm_Substring(x, nchars, len);
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

    if (len < 0) {
        /* TODO: fix the policy of handling incomplete string */
        Scm_Error("incomplete string not accepted: %S", str);
    }
    
    while (p < e) {
        ScmChar cc;
        int ncc;

        SCM_CHAR_GET(p, cc);
        ncc = SCM_CHAR_NBYTES(cc);
        if (ch == cc) {
            SCM_APPEND1(head, tail, Scm_MakeString(s, sizecnt, lencnt));
            sizecnt = lencnt = 0;
            p += ncc;
            s = p;
        } else {
            p += ncc;
            sizecnt += ncc;
            lencnt ++;
        }
    }
    SCM_APPEND1(head, tail, Scm_MakeString(s, sizecnt, lencnt));
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

    if (len1 < 0) {
        if (len2 < 0 || siz2 == len2) goto sbstring;
        Scm_Error("can't handle incomplete string %S with complete string %S",
                  s1, s2);
    }
    if (siz1 == len1) {
        if (len2 < 0 || siz2 == len2) goto sbstring;
        return SCM_FALSE;       /* sbstring can't contain mbstring. */
    }
    if (len2 < 0)
        Scm_Error("can't handle complete string %S with incomplete stirng %S",
                  s1, s2);
    
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
 * Prefix/suffix
 */




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
    
    while (len-- > 0) {
        SCM_CHAR_GET(bufp, ch);
        bufp += SCM_CHAR_NBYTES(ch);
        SCM_APPEND1(start, end, SCM_MAKE_CHAR(ch));
    }
    return start;
}

ScmObj Scm_ListToString(ScmObj chars)
{
    return makestring_from_list(chars);
}

ScmObj Scm_StringFill(ScmString *str, ScmChar ch,
                      ScmObj maybe_start, ScmObj maybe_end)
{
    int completep, len, i, start, end, prelen, midlen, postlen;
    int chlen = SCM_CHAR_NBYTES(ch);
    char *newstr, *p;
    const char *s, *r;

    completep = (SCM_STRING_LENGTH(str) >= 0);
    len = completep ? SCM_STRING_SIZE(str) : SCM_STRING_LENGTH(str);

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
    for (i = 0; i < start; i++) {
        s += (completep? SCM_CHAR_NFOLLOWS(*s)+1 : 1);
    }
    prelen = s - SCM_STRING_START(str);
    r = s;
    for (; i < end; i++) {
        s += (completep? SCM_CHAR_NFOLLOWS(*s)+1 : 1);
    }
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

static void string_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmString *str = SCM_STRING(obj);
    
    if (SCM_WRITE_MODE(ctx) == SCM_WRITE_DISPLAY) {
        SCM_PUTS(str, port);
    } else {
        SCM_PUTC('"', port);
        if (SCM_STRING_COMPLETE_P(str)) {
            ScmChar ch;
            const char *cp = SCM_STRING_START(str);
            int len = SCM_STRING_LENGTH(str);

            while (len--) {
                SCM_CHAR_GET(cp, ch);
                switch (ch) {
                case '\\': SCM_PUTCSTR("\\\\", port); break;
                case '"':  SCM_PUTCSTR("\\\"", port); break;
                case '\n': SCM_PUTCSTR("\\n", port); break;
                case '\t': SCM_PUTCSTR("\\t", port); break;
                case '\r': SCM_PUTCSTR("\\r", port); break;
                case '\f': SCM_PUTCSTR("\\f", port); break;
                case '\0': SCM_PUTCSTR("\\0", port); break;
                default:
                    /* TODO: need to escape control chars */
                    SCM_PUTC(ch, port);
                }
                cp += SCM_CHAR_NBYTES(ch);
            }
        } else {
            const char *cp = SCM_STRING_START(str);
            int size = SCM_STRING_SIZE(str);
            int c;
            while (size--) {
                switch (c = *cp) {
                case '\\': SCM_PUTCSTR("\\\\", port); break;
                case '"':  SCM_PUTCSTR("\\\"", port); break;
                case '\n': SCM_PUTCSTR("\\n", port); break;
                case '\t': SCM_PUTCSTR("\\t", port); break;
                case '\r': SCM_PUTCSTR("\\r", port); break;
                case '\f': SCM_PUTCSTR("\\f", port); break;
                case '\0': SCM_PUTCSTR("\\0", port); break;
                default:
                    /* TODO: need to escape control chars */
                    SCM_PUTC(c, port);
                }
                cp++;
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
    return SCM_NIL;             /* writeme */
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
const char *Scm_DStringGetCstr(ScmDString *dstr)
{
    SCM_DSTRING_PUTB(dstr, '\0');
    return dstr->start;
}

void Scm_DStringPutCstr(ScmDString *dstr, const char *str)
{
    int size = strlen(str);
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
    if (dstr->length >= 0 && SCM_STRING_LENGTH(str) >= 0) {
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

