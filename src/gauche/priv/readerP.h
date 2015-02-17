/*
 * readerP.h - Reader private header file
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

#ifndef GAUCHE_PRIV_READERP_H
#define GAUCHE_PRIV_READERP_H

/*
 * ReadContext
 */
struct ScmReadContextRec {
    SCM_HEADER;
    int flags;                  /* see below */
    ScmHashTable *table;        /* used internally. */
    ScmObj pending;             /* used internally. */
};

enum ScmReadContextFlags {
    RCTX_SOURCE_INFO = (1L<<0),  /* preserving souce file information */
    RCTX_LITERAL_IMMUTABLE = (1L<<1), /* literal should be read as immutable */
    RCTX_DISABLE_CTOR = (1L<<2), /* disable #,() */
    RCTX_RECURSIVELY = (1L<<3),  /* used internally. */

    RCTX_LEXICAL_MODE_MASK = 0xf0 /* >>4 and get ScmReadLexicalMode */
};

#define RCTX_LEXICAL_MODE_SHIFT  4
#define RCTX_LEXICAL_MODE(ctx) \
    ((((ctx)->flags)&RCTX_LEXICAL_MODE_MASK)>>RCTX_LEXICAL_MODE_SHIFT)
#define RCTX_LEXICAL_MODE_SET(ctx, mode)                                \
    (((ctx)->flags)                                                     \
     = (((ctx)->flags & ~RCTX_LEXICAL_MODE_MASK)                        \
        | (((mode)<<RCTX_LEXICAL_MODE_SHIFT)&RCTX_LEXICAL_MODE_MASK)))

#endif /*GAUCHE_PRIV_READERP_H*/
