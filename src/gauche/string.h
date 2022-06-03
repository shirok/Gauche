/*
 * string.h - Public API for Scheme strings
 *
 *   Copyright (c) 2000-2022  Shiro Kawai  <shiro@acm.org>
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

/* This file is included from gauche.h */

#ifndef GAUCHE_STRING_H
#define GAUCHE_STRING_H

/* [String mutation and MT safety]
 * Scheme String is mutable (unfortunately).  The fields of a string
 * may be altered by another thread while you're reading it.  For MT
 * safety, it is important that we ensure the atomicity in retrieving
 * string length/size and content.
 *
 * It isn't practical to use mutex for every string access, so we use
 * atomicity of pointer dereference and assignments.  The actual string
 * fields are stored in immutable structure, ScmStringBody, and a Scheme
 * string, ScmString, has a pointer to it.  When mutation occurs, a new
 * ScmStringBody is allocated, and the pointer is altered.  So, as far
 * as the client retrieves ScmStringBody first, then use its field values,
 * the client won't see inconsistent state.
 * Alternatively, the client can use Scm_GetStringContent(), which
 * retrieves length, size and char array atomically.
 *
 * We further use an assumption that mutation of strings is rare.
 * So ScmString is allocated with initial body.   The 'body' pointer
 * of ScmString is NULL when it is created, which indicates the initial
 * body is used (this convention allows static definition of ScmString).
 * Once the string is mutated, the 'body' pointer points to a
 * fresh ScmStringBody.  Note that it means the initial content of the
 * string, pointed by initialBody.start, won't be GC-ed as far as the
 * ScmString is alive, even if its content is mutated and the initial
 * content isn't used.   Another reason to avoid string mutations.
 */
/* NB: We used to have lengh and size as 'u_int'.  It effectively limited
   the length and size of strings up to max(INT_MAX, SCM_SMALL_INT_MAX),
   for we accept fixnum range in Scheme level.  For 32bit architecture
   it was OK, but for 64bit architecture it became an issue.
   As of 0.9.7, we switch to use ScmSmallInt for length and size, breaking
   the backward compatibility.
*/
/* The 'index' slot may contain an index vector to realize O(1) random-access
 * of the string.  Building index costs time and space, so it is only
 * constructed when explicitly asked.  Srfi-135 (Immutable Texts) is
 * really an immutable string with an indexed body.
 * The user should treat index field as a opaque pointer.
 * See priv/stringP.h for the details.
 */

typedef struct ScmStringBodyRec {
    u_long flags;
    ScmSmallInt length;         /* in characters */
    ScmSmallInt size;           /* in bytes */
    const char *start;
    const void *index;
} ScmStringBody;

#if SIZEOF_LONG == 4
#define SCM_STRING_MAX_SIZE    SCM_SMALL_INT_MAX
#define SCM_STRING_MAX_LENGTH  SCM_SMALL_INT_MAX
#else /*SIZEOF_LONG > 4*/
#define SCM_STRING_MAX_SIZE    INT_MAX
#define SCM_STRING_MAX_LENGTH  INT_MAX
#endif

struct ScmStringRec {
    SCM_HEADER;
    const ScmStringBody *body;  /* may be NULL if we use initial body. */
    ScmStringBody initialBody;  /* initial body */
};

/* The flag value.  Some of them can be specified at construction time
   (marked 'C').  Some of them are kept in "flags" field of the string body
   (marked 'R). */
enum ScmStringFlags {
    SCM_STRING_IMMUTABLE  = (1L<<0),     /* [C,R] The string is immutable. */
    SCM_STRING_INCOMPLETE = (1L<<1),     /* [C,R] The string is incomplete. */
    SCM_STRING_TERMINATED = (1L<<2),     /* [R] The string content is
                                            NUL-terminated.  This flag is used
                                            internally. */
    SCM_STRING_COPYING = (1L<<16),       /* [C]   Need to copy the content
                                            given to the constructor. */
};
#define SCM_STRING_FLAG_MASK  (0xffff)

SCM_CLASS_DECL(Scm_StringClass);
#define SCM_CLASS_STRING        (&Scm_StringClass)

#define SCM_STRINGP(obj)        SCM_XTYPEP(obj, SCM_CLASS_STRING)
#define SCM_STRING(obj)         ((ScmString*)(obj))
#define SCM_STRING_BODY(obj) \
    ((const ScmStringBody*)(SCM_STRING(obj)->body?SCM_STRING(obj)->body:&SCM_STRING(obj)->initialBody))

/* Accessor macros for string body */
#define SCM_STRING_BODY_LENGTH(body)       ((body)->length)
#define SCM_STRING_BODY_SIZE(body)         ((body)->size)
#define SCM_STRING_BODY_START(body)        ((body)->start)
#define SCM_STRING_BODY_END(body)          ((body)->start + (body)->size)
#define SCM_STRING_BODY_FLAGS(body)        ((body)->flags)

#define SCM_STRING_BODY_HAS_FLAG(body, flag) \
    (SCM_STRING_BODY_FLAGS(body)&(flag))
#define SCM_STRING_BODY_INCOMPLETE_P(body)   \
    SCM_STRING_BODY_HAS_FLAG(body, SCM_STRING_INCOMPLETE)
#define SCM_STRING_BODY_IMMUTABLE_P(body)    \
    SCM_STRING_BODY_HAS_FLAG(body, SCM_STRING_IMMUTABLE)
#define SCM_STRING_BODY_SINGLE_BYTE_P(body) \
    (SCM_STRING_BODY_SIZE(body)==SCM_STRING_BODY_LENGTH(body))

/* This is MT-safe, for string immutability won't change */
#define SCM_STRING_IMMUTABLE_P(obj)  \
    SCM_STRING_BODY_IMMUTABLE_P(SCM_STRING_BODY(obj))

#define SCM_STRING_NULL_P(obj) \
    (SCM_STRING_BODY_SIZE(SCM_STRING_BODY(obj)) == 0)

/* Macros for backward compatibility.  Use of these are deprecated,
   since they are not MT-safe.  Use SCM_STRING_BODY_* macros or
   Scm_GetString* API. */
#define SCM_STRING_LENGTH(obj)  (SCM_STRING_BODY(obj)->length)
#define SCM_STRING_SIZE(obj)    (SCM_STRING_BODY(obj)->size)
#define SCM_STRING_START(obj)   (SCM_STRING_BODY(obj)->start)
#define SCM_STRING_INCOMPLETE_P(obj)  \
    (SCM_STRING_BODY_INCOMPLETE_P(SCM_STRING_BODY(obj)))
#define SCM_STRING_SINGLE_BYTE_P(obj) \
    (SCM_STRING_SIZE(obj)==SCM_STRING_LENGTH(obj))


/* OBSOLETED.  Kept for backward compatibility */
#define SCM_MAKSTR_INCOMPLETE  SCM_STRING_INCOMPLETE
#define SCM_MAKSTR_IMMUTABLE   SCM_STRING_IMMUTABLE
#define SCM_MAKSTR_COPYING     SCM_STRING_COPYING

/*
 * String cursors (srfi-130)
 */

/* If offset is small, we use immediate value (see gauche.h).
 * When the offset doesn't fit, we fall back to ScmStringCursorLarge.
 * The two types of cursors are handled transparently in string.c and
 * users shouldn't worry about the distinction.
 * The actual definition is in priv/stringP.h.
 */
typedef struct ScmStringCursorLargeRec ScmStringCursorLarge;

SCM_CLASS_DECL(Scm_StringCursorClass);
SCM_CLASS_DECL(Scm_StringCursorLargeClass);
#define SCM_CLASS_STRING_CURSOR            (&Scm_StringCursorClass)
#define SCM_CLASS_STRING_CURSOR_LARGE      (&Scm_StringCursorLargeClass)

SCM_EXTERN int    Scm_StringCursorP(ScmObj obj);
SCM_EXTERN ScmObj Scm_MakeStringCursorFromIndex(ScmString *src, ScmSmallInt index);
SCM_EXTERN ScmObj Scm_MakeStringCursorEnd(ScmString *src);
SCM_EXTERN ScmObj Scm_StringCursorIndex(ScmString *s, ScmObj sc);
SCM_EXTERN ScmObj Scm_StringCursorStart(ScmString* s);
SCM_EXTERN ScmObj Scm_StringCursorEnd(ScmString* s);
SCM_EXTERN ScmObj Scm_StringCursorForward(ScmString* s, ScmObj cursor, int nchars);
SCM_EXTERN ScmObj Scm_StringCursorBack(ScmString* s, ScmObj cursor, int nchars);
SCM_EXTERN ScmChar Scm_StringRefCursor(ScmString* s, ScmObj sc, int range_error);
SCM_EXTERN ScmObj Scm_SubstringCursor(ScmString *str, ScmObj start, ScmObj end);
SCM_EXTERN int Scm_StringCursorCompare(ScmObj sc1, ScmObj sc2, int (*numcmp)(ScmObj, ScmObj));


/*
 * Constructors
 */

SCM_EXTERN ScmObj  Scm_MakeString(const char *str,
                                  ScmSmallInt size, ScmSmallInt len,
                                  u_long flags);
SCM_EXTERN ScmObj  Scm_MakeFillString(ScmSmallInt len, ScmChar fill);
SCM_EXTERN ScmObj  Scm_CopyStringWithFlags(ScmString *str,
                                           u_long flags, u_long mask);

#define SCM_MAKE_STR(cstr) \
    Scm_MakeString(cstr, -1, -1, 0)
#define SCM_MAKE_STR_COPYING(cstr) \
    Scm_MakeString(cstr, -1, -1, SCM_STRING_COPYING)
#define SCM_MAKE_STR_IMMUTABLE(cstr) \
    Scm_MakeString(cstr, -1, -1, SCM_STRING_IMMUTABLE)

#define Scm_CopyString(str) \
    Scm_CopyStringWithFlags(str, 0, SCM_STRING_IMMUTABLE)

/*
 * C String extraction
 */

SCM_EXTERN char*   Scm_GetString(ScmString *str);
SCM_EXTERN const char* Scm_GetStringConst(ScmString *str);
SCM_EXTERN const char* Scm_GetStringConstUnsafe(ScmString *str);
SCM_EXTERN const char* Scm_GetStringContent(ScmString *str,
                                            ScmSmallInt *psize,
                                            ScmSmallInt *plen,
                                            u_long *pflags);

#define SCM_STRING_CONST_CSTRING(obj) Scm_GetStringConst(SCM_STRING(obj))
#define SCM_STRING_CONST_CSTRING_SAFE(obj) Scm_GetStringConstSafe(SCM_STRING(obj))

/*
 * Conversions
 */

SCM_EXTERN ScmObj Scm_CStringArrayToList(const char **array,
                                         ScmSmallInt size, u_long flags);
SCM_EXTERN const char **Scm_ListToConstCStringArray(ScmObj lis,
                                                    int errp);
SCM_EXTERN char **Scm_ListToCStringArray(ScmObj lis, int errp,
                                         void *(*alloc)(size_t));
SCM_EXTERN ScmObj  Scm_StringToList(ScmString *str);
SCM_EXTERN ScmObj  Scm_ListToString(ScmObj chars);

/* OBSOLETED */
SCM_EXTERN ScmObj  Scm_StringCompleteToIncomplete(ScmString *);
SCM_EXTERN ScmObj  Scm_StringIncompleteToComplete(ScmString *, int, ScmChar);

/*
 * Comparisons
 */

SCM_EXTERN int     Scm_StringEqual(ScmString *x, ScmString *y);
SCM_EXTERN int     Scm_StringCmp(ScmString *x, ScmString *y);
SCM_EXTERN int     Scm_StringCiCmp(ScmString *x, ScmString *y);

/*
 * Accessors and modifiers
 */

SCM_EXTERN ScmChar Scm_StringRef(ScmString *str,
                                 ScmSmallInt k,
                                 int range_error);
SCM_EXTERN int     Scm_StringByteRef(ScmString *str,
                                     ScmSmallInt k,
                                     int range_error);
SCM_EXTERN ScmObj  Scm_Substring(ScmString *x,
                                 ScmSmallInt start,
                                 ScmSmallInt end,
                                 int byterange);
SCM_EXTERN ScmObj  Scm_StringReplaceBody(ScmString *x, const ScmStringBody *b);

/*
 * Concatenation
 */

SCM_EXTERN ScmObj  Scm_StringAppend2(ScmString *x, ScmString *y);
SCM_EXTERN ScmObj  Scm_StringAppendC(ScmString *x, const char *s,
                                     ScmSmallInt size, ScmSmallInt len);
SCM_EXTERN ScmObj  Scm_StringAppend(ScmObj strs);
SCM_EXTERN ScmObj  Scm_StringJoin(ScmObj strs, ScmString *delim, int grammar);


/* grammar spec for StringJoin (see SRFI-13) */
enum {
    SCM_STRING_JOIN_INFIX,
    SCM_STRING_JOIN_STRICT_INFIX,
    SCM_STRING_JOIN_SUFFIX,
    SCM_STRING_JOIN_PREFIX
};

/*
 * Searching
 */

/* Note: On 1.0 release, let StringSplitByChar have limit arg.  For 0.9.x
   series we use a separate Scm_StringSplitByCharWithLimit in order to keep
   ABI compatibility. */
SCM_EXTERN ScmObj  Scm_StringSplitByChar(ScmString *str, ScmChar ch);
SCM_EXTERN ScmObj  Scm_StringSplitByCharWithLimit(ScmString *str, ScmChar ch,
                                                  int limit);
SCM_EXTERN ScmObj  Scm_StringScan(ScmString *s1, ScmString *s2, int retmode);
SCM_EXTERN ScmObj  Scm_StringScanChar(ScmString *s1, ScmChar ch, int retmode);
SCM_EXTERN ScmObj  Scm_StringScanRight(ScmString *s1, ScmString *s2, int retmode);
SCM_EXTERN ScmObj  Scm_StringScanCharRight(ScmString *s1, ScmChar ch, int retmode);

/* "retmode" argument for string scan */
enum {
    SCM_STRING_SCAN_INDEX,      /* return index */
    SCM_STRING_SCAN_BEFORE,     /* return substring of s1 before s2 */
    SCM_STRING_SCAN_AFTER,      /* return substring of s1 after s2 */
    SCM_STRING_SCAN_BEFORE2,    /* return substr of s1 before s2 and rest */
    SCM_STRING_SCAN_AFTER2,     /* return substr of s1 up to s2 and rest */
    SCM_STRING_SCAN_BOTH,       /* return substr of s1 before and after s2 */
    SCM_STRING_SCAN_CURSOR,     /* return <string-cursor>
                                   TRANSIENT: This may be more reasonable
                                   to be after INDEX, but we have it here
                                   until 1.0 to keep ABI compatibility */
    SCM_STRING_SCAN_NUM_RETMODES
};

/*
 * Miscellaneous
 */
SCM_EXTERN ScmSmallInt Scm_MBLen(const char *str, const char *stop);

/* INTERNAL */
SCM_EXTERN const char *Scm_StringPosition(ScmString *str, ScmSmallInt k); /*DEPRECATED*/
SCM_EXTERN const char *Scm_StringBodyPosition(const ScmStringBody *str, ScmSmallInt k);
SCM_EXTERN ScmObj  Scm_MaybeSubstring(ScmString *x, ScmObj start, ScmObj end);
SCM_EXTERN int     Scm_StringBodyFastIndexableP(const ScmStringBody *sb);

/*
 * Static initializer
 */
/* You can allocate a constant string statically, if you calculate
   the length by yourself.  These macros are mainly used in machine-
   generated code.
   SCM_DEFINE_STRING_CONST can be used to define a static string,
   and SCM_STRING_CONST_INITIALIZER can be used inside static array
   of strings. */

#define SCM_STRING_CONST_INITIALIZER(str, len, siz)             \
    { { SCM_CLASS_STATIC_TAG(Scm_StringClass) }, NULL,          \
    { SCM_STRING_IMMUTABLE|SCM_STRING_TERMINATED, (len), (siz), (str), NULL } }

#define SCM_DEFINE_STRING_CONST(name, str, len, siz)            \
    ScmString name = SCM_STRING_CONST_INITIALIZER(str, len, siz)

/*
 * DStrings
 *   Auxiliary structure to construct a string of unknown length.
 *   This is not an ScmObj.   See string.c for details.
 */
#define SCM_DSTRING_INIT_CHUNK_SIZE 32

typedef struct ScmDStringChunkRec {
    ScmSmallInt bytes;          /* actual bytes stored in this chunk.
                                   Note that this is set when the next
                                   chunk is allocated, or by Scm_DStringSize.*/
    char data[SCM_DSTRING_INIT_CHUNK_SIZE]; /* variable length, indeed. */
} ScmDStringChunk;

typedef struct ScmDStringChainRec {
    struct ScmDStringChainRec *next;
    ScmDStringChunk *chunk;
} ScmDStringChain;

struct ScmDStringRec {
    ScmDStringChunk init;       /* initial chunk */
    ScmDStringChain *anchor;    /* chain of extra chunks */
    ScmDStringChain *tail;      /* current chunk */
    char *current;              /* current ptr */
    char *end;                  /* end of current chunk */
    ScmSmallInt lastChunkSize;  /* size of the last chunk */
    ScmSmallInt length;         /* # of chars written */
};

SCM_EXTERN void        Scm_DStringInit(ScmDString *dstr);
SCM_EXTERN ScmSmallInt Scm_DStringSize(ScmDString *dstr);
SCM_EXTERN ScmObj      Scm_DStringGet(ScmDString *dstr, u_long flags);
SCM_EXTERN const char *Scm_DStringGetz(ScmDString *dstr);
SCM_EXTERN void        Scm_DStringWeld(ScmDString *dstr);
SCM_EXTERN const char *Scm_DStringPeek(ScmDString *dstr,
                                       ScmSmallInt *size,
                                       ScmSmallInt *len);
SCM_EXTERN void        Scm_DStringPutz(ScmDString *dstr, const char *str,
                                       ScmSmallInt siz);
SCM_EXTERN void        Scm_DStringAdd(ScmDString *dstr, ScmString *str);
SCM_EXTERN void        Scm_DStringPutb(ScmDString *dstr, char byte);
SCM_EXTERN void        Scm_DStringPutc(ScmDString *dstr, ScmChar ch);
SCM_EXTERN ScmSmallInt Scm_DStringTruncate(ScmDString *dstr,
                                           ScmSmallInt newsize);

#define SCM_DSTRING_SIZE(dstr)    Scm_DStringSize(dstr);

#define SCM_DSTRING_PUTB(dstr, byte)                                     \
    do {                                                                 \
        if ((dstr)->current >= (dstr)->end) Scm__DStringRealloc(dstr, 1);\
        *(dstr)->current++ = (char)(byte);                               \
        (dstr)->length = -1;    /* may be incomplete */                  \
    } while (0)

#define SCM_DSTRING_PUTC(dstr, ch)                      \
    do {                                                \
        ScmChar ch_DSTR = (ch);                         \
        ScmDString *d_DSTR = (dstr);                    \
        ScmSmallInt siz_DSTR = SCM_CHAR_NBYTES(ch_DSTR);\
        if (d_DSTR->current + siz_DSTR > d_DSTR->end)   \
            Scm__DStringRealloc(d_DSTR, siz_DSTR);      \
        SCM_CHAR_PUT(d_DSTR->current, ch_DSTR);         \
        d_DSTR->current += siz_DSTR;                    \
        if (d_DSTR->length >= 0) d_DSTR->length++;      \
    } while (0)

SCM_EXTERN void Scm__DStringRealloc(ScmDString *dstr, ScmSmallInt min_incr);

/*
 * Utility.  Returns NUL-terminated string (SRC doesn't need to be
 * NUL-terminated, but must be longer than SIZE).
 */
SCM_EXTERN char *Scm_StrdupPartial(const char *src, size_t size);

#endif /* GAUCHE_STRING_H */
