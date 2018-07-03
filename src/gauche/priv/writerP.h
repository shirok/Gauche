/*
 * writerP.h - Writer private API
 *
 *   Copyright (c) 2013-2018  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_PRIV_WRITERP_H
#define GAUCHE_PRIV_WRITERP_H

/* Writer control parameters */
struct ScmWriteControlsRec {
    SCM_HEADER;
    int printLength;            /* -1 for no limit */
    int printLevel;             /* -1 for no limit */
    int printWidth;             /* -1 for no limit */
    int printBase;              /* 2-36 */
    int printRadix;             /* boolean, #t to print radix for all numbers */
    int printPretty;            /* boolean, #t to use pretty printer */
};

SCM_CLASS_DECL(Scm_WriteControlsClass);
#define SCM_CLASS_WRITE_CONTROLS  (&Scm_WriteControlsClass)
#define SCM_WRITE_CONTROLS(obj)   ((ScmWriteControls*)(obj))
#define SCM_WRITE_CONTROLS_P(obj) SCM_XTYPEP(obj, SCM_CLASS_WRITE_CONTROLS)

/* WriteContext and WriteState

   WriteContext affects write operation below the current subtree.
   WriteState is created at the root of write-family call and carried
   around during the entire write operation.

   WriteState is and ScmObj and will be accessed from Scheme world as well.
 */

struct ScmWriteContextRec {
    short mode;                 /* print mode */
    short flags;                /* internal */
    int limit;                  /* used in WriteLimited */
    const ScmWriteControls *controls;
};

#define SCM_WRITE_CONTEXT(obj)    ((ScmWriteContext*)(obj))

struct ScmWriteStateRec {
    SCM_HEADER;
    ScmHashTable *sharedTable;  /* track shared structure.  can be NULL */
    const ScmWriteControls *controls; /* saving writecontext->controls
                                         for recursive call */
    int sharedCounter;          /* counter to emit #n= and #n# */
    int currentLevel;
};

SCM_CLASS_DECL(Scm_WriteStateClass);
#define SCM_CLASS_WRITE_STATE  (&Scm_WriteStateClass)
#define SCM_WRITE_STATE(obj)   ((ScmWriteState*)(obj))
#define SCM_WRITE_STATE_P(obj) SCM_XTYPEP(obj, SCM_CLASS_WRITE_STATE)

SCM_EXTERN ScmWriteState *Scm_MakeWriteState(ScmWriteState *proto);


#define SCM_WRITE_MODE_MASK  0x03
#define SCM_WRITE_CASE_MASK  0x0c

#define SCM_WRITE_MODE(ctx)   ((ctx)->mode & SCM_WRITE_MODE_MASK)
#define SCM_WRITE_CASE(ctx)   ((ctx)->mode & SCM_WRITE_CASE_MASK)

SCM_EXTERN ScmObj Scm__WritePrimitive(ScmObj obj, ScmPort *port,
                                      ScmWriteContext *ctx);

#endif /*GAUCHE_PRIV_WRITERP_H*/
