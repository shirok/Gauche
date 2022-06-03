/*
 * symbol.h - Public API for Scheme symbols
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

#ifndef GAUCHE_SYMBOL_H
#define GAUCHE_SYMBOL_H

struct ScmSymbolRec {
    SCM_HEADER;
    ScmString *name;
    int        flags;
};

SCM_CLASS_DECL(Scm_SymbolClass);
#define SCM_CLASS_SYMBOL       (&Scm_SymbolClass)

typedef enum {
    SCM_SYMBOL_FLAG_INTERNED = 1L<<0
} ScmSymbolFlags;

#define SCM_SYMBOL(obj)          ((ScmSymbol*)(obj))
#define SCM_SYMBOLP(obj)         SCM_ISA(obj, SCM_CLASS_SYMBOL)
#define SCM_SYMBOL_NAME(obj)     (SCM_SYMBOL(obj)->name)
#define SCM_SYMBOL_INTERNED(obj) \
    (SCM_SYMBOL(obj)->flags&SCM_SYMBOL_FLAG_INTERNED)

/* Compatibility note: The 'flags' slot in ScmSymbol used to be an
 * 'interned', just to keep whether the symbol is interned or not.
 * We changed it in 0.9.2 to allow future extensions.
 * All statically defined ScmSymbol has initialized this slot by
 * either 0 or 1, so we can make use of the rest of bits; however,
 * the existing code checks SCM_SYMBOL_INTERNED without the mask,
 * so, in order to keep the binary compatibility, we can set other
 * bits of flags only if the symbol is interened.   This restriction
 * can be removed upon 1.0 release.
 */

SCM_EXTERN ScmObj Scm_MakeSymbol(ScmString *name, int interned);
SCM_EXTERN ScmObj Scm_Gensym(ScmString *prefix);
SCM_EXTERN ScmObj Scm_SymbolSansPrefix(ScmSymbol *s, ScmSymbol *p);

#define Scm_Intern(name)  Scm_MakeSymbol(name, TRUE)
#define SCM_INTERN(cstr)  Scm_Intern(SCM_STRING(SCM_MAKE_STR_IMMUTABLE(cstr)))

SCM_EXTERN void Scm_WriteSymbolName(ScmString *snam, ScmPort *port,
                                    ScmWriteContext *ctx, u_int flags);

/* flags for Scm_WriteSymbolName */
#define SCM_SYMBOL_WRITER_NOESCAPE_INITIAL  1u
#define SCM_SYMBOL_WRITER_NOESCAPE_EMPTY    2u

typedef ScmSymbol ScmKeyword;

SCM_CLASS_DECL(Scm_KeywordClass);
#define SCM_CLASS_KEYWORD       (&Scm_KeywordClass)

#define SCM_KEYWORD(obj)        ((ScmKeyword*)(obj))
#define SCM_KEYWORDP(obj)       SCM_XTYPEP(obj, SCM_CLASS_KEYWORD)
#define SCM_KEYWORD_NAME(obj)   (SCM_KEYWORD(obj)->name)

SCM_EXTERN ScmObj Scm_MakeKeyword(ScmString *name);
SCM_EXTERN ScmObj Scm_GetKeyword(ScmObj key, ScmObj list, ScmObj fallback);
SCM_EXTERN ScmObj Scm_DeleteKeyword(ScmObj key, ScmObj list);
SCM_EXTERN ScmObj Scm_DeleteKeywordX(ScmObj key, ScmObj list);
SCM_EXTERN ScmObj Scm_KeywordToString(ScmKeyword *k);

#define SCM_MAKE_KEYWORD(cstr) \
    Scm_MakeKeyword(SCM_STRING(SCM_MAKE_STR_IMMUTABLE(cstr)))
#define SCM_GET_KEYWORD(cstr, list, fallback) \
    Scm_GetKeyword(SCM_MAKE_KEYWORD(cstr), list, fallback)

#endif /* GAUCHE_SYMBOL_H */
