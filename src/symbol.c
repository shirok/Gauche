/*
 * symbol.c - symbol implementation
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
#include "gauche/priv/builtin-syms.h"
#include "gauche/priv/moduleP.h"

/*-----------------------------------------------------------
 * Symbols
 */

static void symbol_print(ScmObj obj, ScmPort *port, ScmWriteContext *);
static int symbol_compare(ScmObj x, ScmObj y, int equalp);

SCM_DEFINE_BUILTIN_CLASS(Scm_SymbolClass, symbol_print, symbol_compare,
                         NULL, NULL, NULL);

static ScmClass *keyword_cpl[] = {
    SCM_CLASS_STATIC_PTR(Scm_SymbolClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

SCM_DEFINE_BUILTIN_CLASS(Scm_KeywordClass, symbol_print, symbol_compare,
                         NULL, NULL, keyword_cpl);

/* name -> symbol mapper */
static ScmInternalMutex obtable_mutex = SCM_INTERNAL_MUTEX_INITIALIZER;
static ScmHashTable *obtable = NULL;

#if GAUCHE_KEEP_DISJOINT_KEYWORD_OPTION
/* Global keyword table. */
static struct {
    ScmHashTable *table;
    ScmInternalMutex mutex;
} keywords = { NULL };

static int keyword_disjoint_p = FALSE;
#endif /*!GAUCHE_KEEP_DISJOINT_KEYWORD_OPTION*/

/* internal constructor.  NAME must be an immutable string. */
static ScmSymbol *make_sym(ScmClass *klass, ScmString *name, int interned)
{
    if (interned) {
        /* fast path */
        SCM_INTERNAL_MUTEX_LOCK(obtable_mutex);
        ScmObj e = Scm_HashTableRef(obtable, SCM_OBJ(name), SCM_FALSE);
        SCM_INTERNAL_MUTEX_UNLOCK(obtable_mutex);
        if (!SCM_FALSEP(e)) return SCM_SYMBOL(e);
    }

    ScmSymbol *sym = SCM_NEW(ScmSymbol);
    SCM_SET_CLASS(sym, klass);
    sym->name = name;
    sym->flags = interned? SCM_SYMBOL_FLAG_INTERNED : 0;

    if (!interned) {
        return sym;
    } else {
        /* Using SCM_DICT_NO_OVERWRITE ensures that if another thread interns
           the same name symbol between above HashTableRef and here, we'll
           get the already interned symbol. */
        SCM_INTERNAL_MUTEX_LOCK(obtable_mutex);
        ScmObj e = Scm_HashTableSet(obtable, SCM_OBJ(name), SCM_OBJ(sym),
                                    SCM_DICT_NO_OVERWRITE);
        SCM_INTERNAL_MUTEX_UNLOCK(obtable_mutex);
        return SCM_SYMBOL(e);
    }
}

/* Intern */
ScmObj Scm_MakeSymbol(ScmString *name, int interned)
{
    ScmObj sname = Scm_CopyStringWithFlags(name, SCM_STRING_IMMUTABLE,
                                           SCM_STRING_IMMUTABLE);
    return SCM_OBJ(make_sym(SCM_CLASS_SYMBOL, SCM_STRING(sname), interned));
}

/* Keyword prefix. */
static SCM_DEFINE_STRING_CONST(keyword_prefix, ":", 1, 1);

/* In unified keyword, we include preceding ':' to the name. */
ScmObj Scm_MakeKeyword(ScmString *name)
{
#if GAUCHE_KEEP_DISJOINT_KEYWORD_OPTION
    if (keyword_disjoint_p) {
        (void)SCM_INTERNAL_MUTEX_LOCK(keywords.mutex);
        ScmObj r = Scm_HashTableRef(keywords.table, SCM_OBJ(name), SCM_FALSE);
        (void)SCM_INTERNAL_MUTEX_UNLOCK(keywords.mutex);

        if (SCM_KEYWORDP(r)) return r;

        ScmKeyword *k = SCM_NEW(ScmKeyword);
        SCM_SET_CLASS(k, SCM_CLASS_KEYWORD);
        k->name = SCM_STRING(Scm_CopyString(name));
        (void)SCM_INTERNAL_MUTEX_LOCK(keywords.mutex);
        r = Scm_HashTableSet(keywords.table, SCM_OBJ(name), SCM_OBJ(k),
                             SCM_DICT_NO_OVERWRITE);
        (void)SCM_INTERNAL_MUTEX_UNLOCK(keywords.mutex);
        return r;
    }
#endif /*GAUCHE_KEEP_DISJOINT_KEYWORD_OPTION*/
    ScmObj sname = Scm_StringAppend2(&keyword_prefix, name);
    ScmSymbol *s = make_sym(SCM_CLASS_KEYWORD, SCM_STRING(sname), TRUE);
    Scm_DefineConst(Scm__GaucheKeywordModule(), s, SCM_OBJ(s));
    return SCM_OBJ(s);
}

ScmObj Scm_KeywordToString(ScmKeyword *k)
{
#if GAUCHE_KEEP_DISJOINT_KEYWORD_OPTION
    if (keyword_disjoint_p) {
        return SCM_OBJ(k->name);
    } else {
        return Scm_Substring(k->name, 1, -1, FALSE);
    }
#else  /*!GAUCHE_KEEP_DISJOINT_KEYWORD_OPTION*/
    return Scm_Substring(k->name, 1, -1, FALSE);
#endif /*!GAUCHE_KEEP_DISJOINT_KEYWORD_OPTION*/
}

/* Default prefix string. */
static SCM_DEFINE_STRING_CONST(default_prefix, "G", 1, 1);

/* Returns uninterned symbol.   PREFIX can be NULL */
ScmObj Scm_Gensym(ScmString *prefix)
{
    char numbuf[50];
    /* We don't need mutex for this variable, since a race on it is
       tolerated---multiple threads may be get the same name symbols,
       but they are uninterned and never be eq? to each other. */
    static intptr_t gensym_count = 0;

    if (prefix == NULL) prefix = &default_prefix;
    int nc = snprintf(numbuf, 49, "%"PRIdPTR, gensym_count++);
    numbuf[49] = '\0';
    ScmObj name = Scm_StringAppendC(prefix, numbuf, nc, nc);
    ScmSymbol *sym = make_sym(SCM_CLASS_SYMBOL, SCM_STRING(name), FALSE);
    return SCM_OBJ(sym);
}

/* If symbol S has a prefix P, returns a symbol without the prefix.
   Otherwise, returns #f. */
ScmObj Scm_SymbolSansPrefix(ScmSymbol *s, ScmSymbol *p)
{
    const ScmStringBody *bp = SCM_STRING_BODY(SCM_SYMBOL_NAME(p));
    const ScmStringBody *bs = SCM_STRING_BODY(SCM_SYMBOL_NAME(s));
    int zp = SCM_STRING_BODY_SIZE(bp);
    int zs = SCM_STRING_BODY_SIZE(bs);
    const char *cp = SCM_STRING_BODY_START(bp);
    const char *cs = SCM_STRING_BODY_START(bs);

    if (zp > zs || memcmp(cp, cs, zp) != 0) return SCM_FALSE;
    return Scm_Intern(SCM_STRING(Scm_MakeString(cs + zp, zs - zp, -1,
                                                SCM_STRING_IMMUTABLE)));
}


/* Print */

/* table of special chars.
   bit 0: bad char for symbol to begin with
   bit 1: bad char for symbol to contain
   bit 2: bad char for symbol, and should be written as \nnn
   bit 3: bad char for symbol, and should be written as \c
   bit 4: may be escaped when case fold mode
 */
static const char special[] = {
 /* NUL .... */
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
 /* .... */
    7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7,
 /*    !  "  #  $  %  &  '  (  )  *  +  ,  -  .  /  */
    3, 0, 3, 3, 0, 0, 0, 3, 3, 3, 0, 1, 3, 1, 1, 0,
 /* 0  1  2  3  4  5  6  7  8  9  :  ;  <  =  >  ?  */
    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 3, 0, 0, 0, 0,
 /* @  A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  */
    1, 16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,
 /* P  Q  R  S  T  U  V  W  X  Y  Z  [  \  ]  ^  _  */
    16,16,16,16,16,16,16,16,16,16,16,3, 11,3, 0, 0,
 /* `  a  b  c  d  e  f  g  h  i  j  k  l  m  n  o  */
    3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
 /* p  q  r  s  t  u  v  w  x  y  z  {  |  }  ~  ^? */
    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 11,3, 0, 7
};

/* internal function to write symbol name, with proper escaping */
void Scm_WriteSymbolName(ScmString *snam, ScmPort *port, ScmWriteContext *ctx,
                         u_int flags)
{
    /* See if we have special characters, and use |-escape if necessary. */
    /* TODO: For now, we regard chars over 0x80 is all "printable".
       Need a more consistent mechanism. */
    const ScmStringBody *b = SCM_STRING_BODY(snam);
    const char *p = SCM_STRING_BODY_START(b);
    int siz = SCM_STRING_BODY_SIZE(b);
    int escape = FALSE;
    int spmask = (Scm_WriteContextCase(ctx) == SCM_WRITE_CASE_FOLD)? 0x12 : 0x02;

    if (siz == 0) {         /* special case */
        if (!(flags & SCM_SYMBOL_WRITER_NOESCAPE_EMPTY)) {
            SCM_PUTZ("||", -1, port);
        }
        return;
    }
    if (siz == 1 && (*p == '+' || *p == '-')) {
        SCM_PUTC((unsigned)*p, port);
        return;
    }
    if ((unsigned int)*p < 128
        && ((special[(unsigned int)*p]&1)
#if GAUCHE_KEEP_DISJOINT_KEYWORD_OPTION
            || (keyword_disjoint_p && (*p == ':'))
#endif /*GAUCHE_KEEP_DISJOINT_KEYWORD_OPTION*/
            )
        && (!(flags & SCM_SYMBOL_WRITER_NOESCAPE_INITIAL))) {
        escape = TRUE;
    } else {
        const char *q = p;
        for (int i=0; i<siz; i++, q++) {
            if ((unsigned int)*q < 128
                && (special[(unsigned int)*q]&spmask)) {
                escape = TRUE;
                break;
            }
        }
    }
    if (escape) {
        SCM_PUTC('|', port);
        for (const char *q=p; q<p+siz; ) {
            unsigned int ch;
            SCM_CHAR_GET(q, ch);
            q += SCM_CHAR_NBYTES(ch);
            if (ch < 128) {
                if (special[ch] & 8) {
                    SCM_PUTC('\\', port);
                    SCM_PUTC(ch, port);
                } else if (special[ch] & 4) {
                    Scm_Printf(port, "\\x%02x;", ch);
                } else {
                    SCM_PUTC(ch, port);
                }
            } else {
                SCM_PUTC(ch, port);
            }
        }
        SCM_PUTC('|', port);
        return;
    } else {
        SCM_PUTS(snam, port);
    }
}

/* Symbol printer.
   NB: Uninterned symbols are treated as sharable objects (can be written
   with #n= syntax).  It is handled by upper layer (write.c) so we don't
   worry about it in this routine.
 */
static void symbol_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    if (Scm_WriteContextMode(ctx) == SCM_WRITE_DISPLAY) {
        SCM_PUTS(SCM_SYMBOL_NAME(obj), port);
    } else {
#if GAUCHE_KEEP_DISJOINT_KEYWORD_OPTION
        if (SCM_KEYWORDP(obj) && keyword_disjoint_p) {
            Scm_Putc(':', port);
            /* We basically print keyword names in the same way as symbols
               (i.e. using |-escape if necessary).  However, as a convention,
               two things are different from the default symbol writer.
               (1) We don't check the noninitials; :1 is unambiguously a
               keyword, so we don't need to print :|1|.
               (2) A keyword with an empty name can be printed just as :,
               instead of :||.
               These conventions are useful if we pass the S-expression with
               these keywords to other Scheme implementations that don't support
               CL-style keywords; they would just read those ones as symbols.
            */
            Scm_WriteSymbolName(SCM_KEYWORD(obj)->name, port, ctx,
                                (SCM_SYMBOL_WRITER_NOESCAPE_INITIAL
                                 |SCM_SYMBOL_WRITER_NOESCAPE_EMPTY));
            return;
        }
#endif /*GAUCHE_KEEP_DISJOINT_KEYWORD_OPTION*/
        if (!SCM_SYMBOL_INTERNED(obj)) SCM_PUTZ("#:", -1, port);
        Scm_WriteSymbolName(SCM_SYMBOL_NAME(obj), port, ctx, 0);
    }
}

/* Symbol comparison procedure.
   Will be used via 'compare' procedure.  Following srfi-114, we compare
   by name, but takes extra care of intern/unintern distinction; if the
   names are the same, interned symbol is less, and if both are
   uninterned, we compare addresses.
 */
static int symbol_compare(ScmObj x, ScmObj y, int equalp)
{
    if (equalp) {
        /* Symbol equality test is handled in Scm_Eq* and will never come
           here, but just in case.  */
        return SCM_EQ(x, y)? 0:1;
    } else if (SCM_EQ(x, y)) {
        return 0;
    } else {
        int r = Scm_StringCmp(SCM_SYMBOL_NAME(x), SCM_SYMBOL_NAME(y));
        if (r != 0) return r;
        if (SCM_SYMBOL_INTERNED(x)) return -1; /* y must be uninterned */
        if (SCM_SYMBOL_INTERNED(y)) return  1; /* x must be uninterned */
        return (x < y)? -1 : 1;                /* both are uninterned */
    }
}

/*
 * Keyword Utilities
 *   The names are historical; KEY doesn't need to be a keyword at all;
 *   anything that can be compared by eq? do.
 */

ScmObj Scm_GetKeyword(ScmObj key, ScmObj list, ScmObj fallback)
{
    ScmObj cp;
    SCM_FOR_EACH(cp, list) {
        if (!SCM_PAIRP(SCM_CDR(cp))) {
            Scm_Error("incomplete key list: %S", list);
        }
        if (key == SCM_CAR(cp)) return SCM_CADR(cp);
        cp = SCM_CDR(cp);
    }
    if (SCM_UNBOUNDP(fallback)) {
        Scm_Error("value for key %S is not provided: %S", key, list);
    }
    return fallback;
}

ScmObj Scm_DeleteKeyword(ScmObj key, ScmObj list)
{
    ScmObj cp;
    SCM_FOR_EACH(cp, list) {
        if (!SCM_PAIRP(SCM_CDR(cp))) {
            Scm_Error("incomplete key list: %S", list);
        }
        if (key == SCM_CAR(cp)) {
            /* found */
            ScmObj h = SCM_NIL, t = SCM_NIL;
            ScmObj tail = Scm_DeleteKeyword(key, SCM_CDR(SCM_CDR(cp)));
            ScmObj cp2;
            SCM_FOR_EACH(cp2, list) {
                if (cp2 == cp) {
                    SCM_APPEND(h, t, tail);
                    return h;
                } else {
                    SCM_APPEND1(h, t, SCM_CAR(cp2));
                }
            }
        }
        cp = SCM_CDR(cp);
    }
    return list;
}

ScmObj Scm_DeleteKeywordX(ScmObj key, ScmObj list)
{
    ScmObj cp, prev = SCM_FALSE;
    SCM_FOR_EACH(cp, list) {
        if (!SCM_PAIRP(SCM_CDR(cp))) {
            Scm_Error("incomplete key list: %S", list);
        }
        if (key == SCM_CAR(cp)) {
            /* found */
            if (SCM_FALSEP(prev)) {
                /* we're at the head of list */
                return Scm_DeleteKeywordX(key, SCM_CDR(SCM_CDR(cp)));
            } else {
                ScmObj tail = Scm_DeleteKeywordX(key, SCM_CDR(SCM_CDR(cp)));
                SCM_SET_CDR(prev, tail);
                return list;
            }
        }
        cp = SCM_CDR(cp);
        prev = cp;
    }
    return list;
}

/*
 * Initialization
 */

#include "builtin-syms.c"

void Scm__InitSymbol(void)
{
    SCM_INTERNAL_MUTEX_INIT(obtable_mutex);
    obtable = SCM_HASH_TABLE(Scm_MakeHashTableSimple(SCM_HASH_STRING, 4096));
    init_builtin_syms();
#if GAUCHE_KEEP_DISJOINT_KEYWORD_OPTION
    (void)SCM_INTERNAL_MUTEX_INIT(keywords.mutex);
    keywords.table = SCM_HASH_TABLE(Scm_MakeHashTableSimple(SCM_HASH_STRING, 256));
    /* Preset keyword class precedence list, depending on the value of
       GAUCHE_KEYWORD_DISJOINT or GAUCHE_KEYWORD_IS_SYMBOL */
    const char *disjoint = Scm_GetEnv("GAUCHE_KEYWORD_DISJOINT");
    const char *issymbol = Scm_GetEnv("GAUCHE_KEYWORD_IS_SYMBOL");
    if (disjoint != NULL) {
        keyword_disjoint_p = TRUE;
    } else if (issymbol != NULL) {
        keyword_disjoint_p = FALSE;
    } else {
        keyword_disjoint_p = TRUE; /* This determines the default */
    }
    if (keyword_disjoint_p) {
        Scm_KeywordClass.cpa = &(keyword_cpl[1]);
        /* The class is initialized later in class.c */
    }
#endif /*!GAUCHE_KEEP_DISJOINT_KEYWORD_OPTION*/
}
