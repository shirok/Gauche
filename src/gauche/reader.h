/*
 * reader.h - Reader API
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

/* This file is included from gauche.h */

#ifndef GAUCHE_READER_H
#define GAUCHE_READER_H

SCM_CLASS_DECL(Scm_ReadContextClass);
#define SCM_CLASS_READ_CONTEXT   (&Scm_ReadContextClass)
#define SCM_READ_CONTEXT(obj)    ((ScmReadContext*)(obj))
#define SCM_READ_CONTEXT_P(obj)  SCM_XTYPEP(obj, SCM_CLASS_READ_CONTEXT)

SCM_EXTERN ScmReadContext *Scm_MakeReadContext(ScmReadContext *proto);
SCM_EXTERN int             Scm_ReadContextLiteralImmutable(ScmReadContext *);
SCM_EXTERN ScmReadContext *Scm_CurrentReadContext();
SCM_EXTERN ScmReadContext *Scm_SetCurrentReadContext(ScmReadContext *ctx);

/* Reader-lexical-mode
    permissive
    legacy
    warn-legacy
    strict-r7
   This is kept in a parameter, so it's thread-local.

   NB: The actual implementation is in port.c, because ports require
   this working at initialization, but reader is initialized after ports.
   It is debatable which module these should belong to.  Reader *always*
   reference port's reader lexical mode, and the global reader lexical
   mode parameter is only used to initialize individual ports.  So these
   can be in port's API.  But in that case, we may want other name
   (e.g. DefaultReaderLexicalMode).
 */
SCM_EXTERN ScmObj Scm_ReaderLexicalMode(void);
/* returns previous setting */
SCM_EXTERN ScmObj Scm_SetReaderLexicalMode(ScmObj);

/* An object to keep unrealized circular reference (e.g. #N=) during
 * 'read'.  It is replaced by the reference value before exitting 'read',
 * and it shouldn't leak out to the normal Scheme program, except the
 * code that handles it explicitly (like read-time constructor).
 */
typedef struct ScmReadReferenceRec {
    SCM_HEADER;
    ScmObj value;               /* realized reference.  initially UNBOUND */
} ScmReadReference;

SCM_CLASS_DECL(Scm_ReadReferenceClass);
#define SCM_CLASS_READ_REFERENCE  (&Scm_ReadReferenceClass)
#define SCM_READ_REFERENCE(obj)   ((ScmReadReference*)(obj))
#define SCM_READ_REFERENCE_P(obj) SCM_XTYPEP(obj, SCM_CLASS_READ_REFERENCE)
#define SCM_READ_REFERENCE_REALIZED(obj) \
   (!SCM_EQ(SCM_READ_REFERENCE(obj)->value, SCM_UNBOUND))

/*
 * Standard Read entries
 */
SCM_EXTERN ScmObj Scm_Read(ScmObj port);
SCM_EXTERN ScmObj Scm_ReadWithContext(ScmObj port, ScmReadContext *ctx);
SCM_EXTERN ScmObj Scm_ReadList(ScmObj port, ScmChar closer);
SCM_EXTERN ScmObj Scm_ReadListWithContext(ScmObj port, ScmChar closer,
                                          ScmReadContext *ctx);
SCM_EXTERN ScmObj Scm_ReadFromString(ScmString *string);
SCM_EXTERN ScmObj Scm_ReadFromCString(const char *string);

SCM_EXTERN void   Scm_ReadError(ScmPort *port, const char *fmt, ...);

/*
 * Common utilities to handle hex-digit escapes
 */
SCM_EXTERN ScmChar Scm_ReadXdigitsFromString(const char *buf,
                                             int buflen,
                                             ScmChar key,
                                             ScmObj mode,
                                             int terminator,
                                             const char**);
SCM_EXTERN ScmObj  Scm_ReadXdigitsFromPort(ScmPort *port, int key, ScmObj mode,
                                           int incompletep, ScmDString *buf);

SCM_EXTERN long Scm_ReadDigitsAsLong(ScmPort *port, ScmChar ch, int radix,
                                     ScmChar *next,
                                     int *numread);
SCM_EXTERN long Scm_ParseDigitsAsLong(const char *buf, size_t len, int radix,
                                      int *numread);

/*
 * SRFI-10 hash-comma syntax
 */
SCM_EXTERN ScmObj Scm_DefineReaderCtor(ScmObj symbol, ScmObj proc,
                                       ScmObj finisher, ScmObj module);
SCM_EXTERN ScmObj Scm_GetReaderCtor(ScmObj symbol, ScmObj module);

/*
 * Hash-bang reader directive handlers
 */
SCM_EXTERN ScmObj Scm_DefineReaderDirective(ScmObj symbol, ScmObj proc);

#endif  /*GAUCHE_READER_H*/

