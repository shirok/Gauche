/*
 * string.c - string implementation
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
 *  $Id: string.c,v 1.5 2001-01-16 04:44:40 shiro Exp $
 */

#include <stdio.h>
#include <sys/types.h>
#include "gauche.h"

#define INITSTR(var, len, siz, p)               \
    do {                                        \
        (var) = SCM_NEW(ScmString);             \
        (var)->hdr.klass = SCM_CLASS_STRING;    \
        (var)->length = (len);                  \
        (var)->size = (siz);                    \
        (var)->start = (p);                     \
    } while (0)

#define DUMP_LENGTH   50

static int string_print(ScmObj obj, ScmPort *port, int mode);

static ScmClass *sequence_cpl[] = {
    SCM_CLASS_SEQUENCE, SCM_CLASS_COLLECTION, SCM_CLASS_TOP, NULL
};

ScmClass Scm_StringClass = {
    SCM_CLASS_CLASS,
    "<string>",
    string_print,
    sequence_cpl
};

/* for debug */
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

/*
 * Multibyte length calculation
 */

/* We have multiple similar functions, due to performance reasons. */

/* Calculate both length and size of C-string str.
   If str is incomplete, *plen gets -1. */
#ifdef __GNUC__
inline
#endif
static int count_size_and_length(const char *str, int *psize, int *plen)
{
    char c;
    const char *p = str;
    int size = 0, len = 0;
    while (c = *p++) {
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
#ifdef __GNUC__
inline
#endif
static int count_length(const char *str, int size)
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

/* Returns length of C-string.   Returns -1 if str is incomplete */
int Scm_MBLen(const char *str)
{
    char c;
    int count = 0;
    
    while (c = *str++) {
        int i = SCM_CHAR_NFOLLOWS(c);
        if (i < 0) return -1;
        count++;
        while (i-- > 0) {
            if (!*str++) return -1;
        }
    }
    return count;
}

/*
 * Constructors
 */

ScmObj Scm_MakeStringConst(const char *str, int size, int len)
{
    ScmString *z;

    if (size < 0 || len < 0) count_size_and_length(str, &size, &len);
    INITSTR(z, len, size, str);
    return SCM_OBJ(z);
}

ScmObj Scm_MakeString(const char *str, int size, int len)
{
    ScmString *z;
    char *nstr;

    if (size < 0 || len < 0) count_size_and_length(str, &size, &len);
    nstr = SCM_NEW_ATOMIC2(char *, size + 1);
    memcpy(nstr, str, size+1);  /* includes \0 */
    INITSTR(z, len, size, nstr);
    return SCM_OBJ(z);
}

ScmObj Scm_MakeFillString(int len, ScmChar fill)
{
    ScmString *z;
    int size = SCM_CHAR_NBYTES(fill), i;
    char *ptr = SCM_NEW_ATOMIC2(char *, size*len+1);
    char *p;
    
    for (i=0, p=ptr; i<len; i++, p+=size) {
        SCM_STR_PUTC(p, fill);
    }
    ptr[size*len] = '\0';
    INITSTR(z, len, size*len, ptr);
    return SCM_OBJ(z);
}

static ScmObj makestring_from_list(ScmObj chars)
{
    ScmObj cp, s;
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
        SCM_STR_PUTC(bufp, ch);
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
    ScmString *z;
    INITSTR(z, SCM_STRING_LENGTH(x), SCM_STRING_SIZE(x), SCM_STRING_START(x));
    return SCM_OBJ(z);
}

/*
 * Comparison
 */

ScmObj Scm_StringEqual(ScmString *x, ScmString *y)
{
    int sizx = SCM_STRING_SIZE(x);
    int sizy = SCM_STRING_SIZE(y);
    if (sizx == sizy) {
        if (memcmp(SCM_STRING_START(x), SCM_STRING_START(y), sizx) == 0) {
            return SCM_TRUE;
        }
    }
    return SCM_FALSE;
}

#define STRCMP(fn, op1, op2)                                            \
ScmObj fn(ScmString *x, ScmString *y)                                   \
{                                                                       \
    int sizx = SCM_STRING_SIZE(x);                                      \
    int sizy = SCM_STRING_SIZE(y);                                      \
    int siz = (sizx < sizy)? sizx : sizy;                               \
    int r = memcmp(SCM_STRING_START(x), SCM_STRING_START(y), siz);      \
    if (r op1 0) return SCM_TRUE;                                       \
    if (r == 0 && sizx op2 sizy) return SCM_TRUE;                       \
    else return SCM_FALSE;                                              \
}

STRCMP(Scm_StringLt, <, <)
STRCMP(Scm_StringLe, <, <=)
STRCMP(Scm_StringGt, >, >)
STRCMP(Scm_StringGe, >, >=)

/*
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
                SCM_STR_GETC(p, c);
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

/*
 * Concatenation
 */

ScmObj Scm_StringAppend2(ScmString *x, ScmString *y)
{
    int sizex = SCM_STRING_SIZE(x), lenx = SCM_STRING_LENGTH(x);
    int sizey = SCM_STRING_SIZE(y), leny = SCM_STRING_LENGTH(y);
    int lenz;
    ScmString *z;
    char *p = SCM_NEW_ATOMIC2(char *,sizex + sizey + 1);

    memcpy(p, x->start, sizex);
    memcpy(p+sizex, y->start, sizey);
    p[sizex + sizey] = '\0';

    if (lenx < 0 || leny < 0) {
        lenz = -1;              /* yields incomplete string */
    } else {
        lenz = lenx + leny;
    }
    INITSTR(z, lenz, sizex + sizey, p);
    return SCM_OBJ(z);
}

ScmObj Scm_StringAppendC(ScmString *x, const char *str, int sizey, int leny)
{
    int sizex = SCM_STRING_SIZE(x), lenx = SCM_STRING_LENGTH(x);
    int lenz;
    char *p;
    ScmString *z;

    if (sizey < 0 || leny < 0) count_size_and_length(str, &sizey, &leny);
    
    p = SCM_NEW_ATOMIC2(char *, sizex + sizey + 1);
    memcpy(p, x->start, sizex);
    memcpy(p+sizex, str, sizey);
    p[sizex+sizey] = '\0';

    if (lenx < 0 || leny < 0) {
        lenz = -1;
    } else {
        lenz = lenx + leny;
    }
    INITSTR(z, lenz, sizex + sizey, p);
    return SCM_OBJ(z);
}

/*
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
            if (sizey < 0 || leny < 0)
                count_size_and_length(str, &sizey, &leny);
            lenz = lenx - (end - start) + leny;
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
        if (sizey < 0 || leny < 0) count_size_and_length(str, &sizey, &leny);
        s = forward_pos(x->start, start);
        e = forward_pos(s, end - start);
        sizez = sizex + sizey - (e - s);
        lenz = lenx + leny - (end - start);

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
    int sizex = SCM_STRING_SIZE(x), lenx = SCM_STRING_LENGTH(x);
    int sizey = SCM_STRING_SIZE(y), leny = SCM_STRING_LENGTH(y);
    int sizez, lenz;
    ScmString *z;
    char *p, *s, *e;

    return Scm_StringSubstituteCstr(x, start, end, y->start, sizey, leny);
}

ScmObj Scm_StringSet(ScmString *x, int k, ScmChar ch)
{
    char buf[SCM_CHAR_MAX_BYTES+1];
    int size = SCM_CHAR_NBYTES(ch);
    SCM_STR_PUTC(buf, ch);
    return Scm_StringSubstituteCstr(x, k, k+1, buf, size, 1);
}

ScmObj Scm_StringByteSet(ScmString *x, int k, ScmByte b)
{
    int size = SCM_STRING_SIZE(x), len = SCM_STRING_LENGTH(x);
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

/*
 * Substring
 */

ScmObj Scm_Substring(ScmString *x, int start, int end)
{
    ScmString *z;
    const char *s, *e;
    int sizex = SCM_STRING_SIZE(x), lenx = SCM_STRING_LENGTH(x);
    
    if (start < 0)
        Scm_Error("start argument needs to be positive: %d", start);
    if (end > lenx)
        Scm_Error("end argument is out of range: %d", end);
    if (end < start)
        Scm_Error("end argument must be equal to or greater than the start argument: start=%d, end=%d", start, end);
    if (start) s = forward_pos(x->start, start); else s = x->start;
    e = forward_pos(s, end - start);

    INITSTR(z, end - start, e - s, s);
    return SCM_OBJ(z);
}

/*
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
    ScmObj start = SCM_NIL, end;
    const char *bufp = SCM_STRING_START(str);
    int chsize, len = SCM_STRING_LENGTH(str);
    ScmChar ch;
    
    while (len-- > 0) {
        SCM_STR_GETC(bufp, ch);
        bufp += SCM_CHAR_NBYTES(ch);
        SCM_GROW_LIST(start, end, SCM_MAKE_CHAR(ch));
    }
    return start;
}

ScmObj Scm_ListToString(ScmObj chars)
{
    return makestring_from_list(chars);
}

ScmObj Scm_StringFill(ScmString *str, ScmChar ch)
{
    int len = SCM_STRING_LENGTH(str), i;
    int chlen = SCM_CHAR_NBYTES(ch);
    char *newstr = SCM_NEW_ATOMIC2(char *, len * chlen + 1);
    char *p = newstr;

    for (i=0; i<len; i++) {
        SCM_STR_PUTC(p, ch);
        p += chlen;
    }
    p[len*chlen] = '\0';
    /* modify str */
    str->size = len * chlen;
    str->start = newstr;
    return SCM_OBJ(str);
}

static int string_print(ScmObj obj, ScmPort *port, int mode)
{
    ScmString *str = SCM_STRING(obj);
    int nc = 0;
    
    if (mode == SCM_PRINT_DISPLAY) {
        SCM_PUTS(str, port);
        nc = SCM_STRING_LENGTH(str);
    } else {
        SCM_PUTC('"', port); nc++;
        if (SCM_STRING_COMPLETE_P(str)) {
            ScmChar ch;
            const char *cp = SCM_STRING_START(str);
            int chsize, len = SCM_STRING_LENGTH(str);

            while (len--) {
                SCM_STR_GETC(cp, ch);
                switch (ch) {
                case '\\': SCM_PUTCSTR("\\\\", port); nc += 2; break;
                case '"':  SCM_PUTCSTR("\\\"", port); nc += 2; break;
                case '\n': SCM_PUTCSTR("\\n", port); nc += 2; break;
                case '\t': SCM_PUTCSTR("\\t", port); nc += 2; break;
                case '\r': SCM_PUTCSTR("\\r", port); nc += 2; break;
                case '\f': SCM_PUTCSTR("\\f", port); nc += 2; break;
                case '\0': SCM_PUTCSTR("\\0", port); nc += 2; break;
                default:
                    /* TODO: need to escape control chars */
                    SCM_PUTC(ch, port); nc++;
                }
                cp += SCM_CHAR_NBYTES(ch);
            }
        } else {
            const char *cp = SCM_STRING_START(str);
            int size = SCM_STRING_SIZE(str);
            int c;
            while (size--) {
                switch (c = *cp) {
                case '\\': SCM_PUTCSTR("\\\\", port); nc += 2; break;
                case '"':  SCM_PUTCSTR("\\\"", port); nc += 2; break;
                case '\n': SCM_PUTCSTR("\\n", port); nc += 2; break;
                case '\t': SCM_PUTCSTR("\\t", port); nc += 2; break;
                case '\r': SCM_PUTCSTR("\\r", port); nc += 2; break;
                case '\f': SCM_PUTCSTR("\\f", port); nc += 2; break;
                case '\0': SCM_PUTCSTR("\\0", port); nc += 2; break;
                default:
                    /* TODO: need to escape control chars */
                    SCM_PUTC(c, port);
                }
                cp++;
            }
        }
        SCM_PUTC('"', port); nc++;
    }
    return nc;
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
    /* TODO: Maybe we should avoid realloc.  Maybe we should use chained
       segments of str.  So far, what I know is the program crashes
       if we use GC_realloc here. */
#if 0
    p = (char *)Scm_Malloc(newsize);
    memcpy(p, dstr->start, GC_size(dstr->start));
#else
    p = (char *)Scm_Realloc(dstr->start, newsize);
#endif

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
    ScmString *z;
    int len = dstr->length;
    int size = dstr->current - dstr->start;
    
    if (len < 0) {
        len = count_length(dstr->start, size);
    }
    INITSTR(z, len, size, dstr->start);
    return SCM_OBJ(z);
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

