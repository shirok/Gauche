/*
 * writer.h - Writer API
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

#ifndef GAUCHE_WRITER_H
#define GAUCHE_WRITER_H

/* Print mode flags */
/* R7RS write defaults to "circular write" mode.  We follow that, so
   by default we go with two-pass circular-only write mode.  Setting
   SCM_WRITE_SIMPLE makes write use one-pass mode.  Settings
   SCM_WRITE_SHARED makes write write-shared (srfi-38 write/ss) mode. */
enum ScmWriteModeFlags {
    SCM_WRITE_WRITE = 0,        /* write mode   */
    SCM_WRITE_DISPLAY = 1,      /* display mode */
    SCM_WRITE_SHARED = 2,       /* write/ss mode */
    SCM_WRITE_SIMPLE = 3        /* write-simple mode */
};

/* Case folding mode flags */
enum ScmWriteCaseFlags {
    SCM_WRITE_CASE_FOLD = 4,    /* case-fold mode.  need to escape capital
                                   letters. */
    SCM_WRITE_CASE_NOFOLD = 8,  /* case-sensitive mode.  no need to escape
                                   capital letters */
};

SCM_EXTERN ScmWriteControls *Scm_MakeWriteControls(const ScmWriteControls *proto);
SCM_EXTERN const ScmWriteControls *Scm_DefaultWriteControls(void);
SCM_EXTERN const ScmWriteControls *Scm_GetWriteControls(ScmWriteContext *ctx,
                                                        ScmWriteState *st);

SCM_EXTERN int Scm_WriteContextMode(const ScmWriteContext *ctx);
SCM_EXTERN int Scm_WriteContextCase(const ScmWriteContext *ctx);

SCM_EXTERN void Scm_Write(ScmObj obj, ScmObj port, int mode);
SCM_EXTERN void Scm_WriteWithControls(ScmObj obj, ScmObj port, int mode,
                                      const ScmWriteControls *ctrl);
SCM_EXTERN int Scm_WriteCircular(ScmObj obj, ScmObj port, int mode, int width);
SCM_EXTERN int Scm_WriteLimited(ScmObj obj, ScmObj port, int mode, int width);
SCM_EXTERN void Scm_Format(ScmPort *port, ScmString *fmt, ScmObj args, int ss);
SCM_EXTERN void Scm_Printf(ScmPort *port, const char *fmt, ...);
SCM_EXTERN void Scm_PrintfShared(ScmPort *port, const char *fmt, ...);
SCM_EXTERN void Scm_Vprintf(ScmPort *port, const char *fmt, va_list args,
                            int sharedp);
SCM_EXTERN ScmObj Scm_Sprintf(const char *fmt, ...);
SCM_EXTERN ScmObj Scm_SprintfShared(const char *fmt, ...);
SCM_EXTERN ScmObj Scm_Vsprintf(const char *fmt, va_list args, int sharedp);


#endif  /*GAUCHE_WRITER_H*/
