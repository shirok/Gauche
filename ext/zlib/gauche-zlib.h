/*
 * Gauche-zlib - zlib module
 *
 *    Copyright (c) 2006 Rui Ueyama, All rights reserved.
 *
 *    Redistribution and use in source and binary forms, with or without
 *    modification, are permitted provided that the following conditions
 *    are met:
 *
 *    1. Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *
 *    2. Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 *    3. Neither the name of the authors nor the names of its contributors
 *       may be used to endorse or promote products derived from this
 *       software without specific prior written permission.
 *
 *    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *    A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *    OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *    SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *    TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *    PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *    LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *    NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *    SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/* Prologue */
#ifndef GAUCHE_ZLIB_H
#define GAUCHE_ZLIB_H

#include <gauche.h>
#include <gauche/extend.h>
#include <zlib.h>

#if defined(EXTZLIB_EXPORTS)
#define LIBGAUCHE_EXT_BODY
#endif
#include <gauche/extern.h>      /* redefine SCM_EXTERN */


SCM_DECL_BEGIN

typedef struct ScmZlibInfoRec {
    z_streamp strm;
    ScmPort *remote;            /* source or drain port */
    int ownerp;
    int flush;
    int stream_endp;
    int bufsiz;
    char *buf;
    char *ptr;
    unsigned char *dict;
    int dictlen;
    int level;
    int strategy;
    ScmObj dict_adler;
} ScmZlibInfo;

#define SCM_PORT_ZLIB_INFO(p) ((ScmZlibInfo*)(p)->src.buf.data)
#define SCM_PORT_ZSTREAM(p)   (SCM_PORT_ZLIB_INFO(p)->strm)

SCM_CLASS_DECL(Scm_DeflatingPortClass);
#define SCM_CLASS_DEFLATING_PORT  (&Scm_DeflatingPortClass)
#define SCM_DEFLATING_PORT_P(obj) SCM_ISA(obj, SCM_CLASS_DEFLATING_PORT)
SCM_CLASS_DECL(Scm_InflatingPortClass);
#define SCM_CLASS_INFLATING_PORT  (&Scm_InflatingPortClass)
#define SCM_INFLATING_PORT_P(obj) SCM_ISA(obj, SCM_CLASS_INFLATING_PORT)

extern ScmObj Scm_MakeDeflatingPort(ScmPort *source, int level,
                                    int window_bits, int memlevel,
                                    int strategy, ScmObj dict,
                                    int bufsiz, int ownerp);
extern ScmObj Scm_MakeInflatingPort(ScmPort *sink, int bufsiz,
                                    int window_bits, ScmObj dict,
                                    int ownerp);

/*================================================================
 * Conditions
 */

typedef ScmError ScmZlibError;

SCM_CLASS_DECL(Scm_ZlibErrorClass);
#define SCM_CLASS_ZLIB_ERROR  (&Scm_ZlibErrorClass)
#define SCM_ZLIB_ERROR(obj)   ((ScmZlibError*)(obj))
#define SCM_ZLIB_ERRORP(obj)  SCM_ISA(obj, SCM_CLASS_ZLIB_ERROR)

typedef ScmZlibError ScmZlibNeedDictError;
SCM_CLASS_DECL(Scm_ZlibNeedDictErrorClass);
#define SCM_CLASS_ZLIB_NEED_DICT_ERROR  (&Scm_ZlibNeedDictErrorClass)
#define SCM_ZLIB_NEED_DICT_ERROR(obj)   ((ScmZlibNeedDictError*)(obj))
#define SCM_ZLIB_NEED_DICT_ERRORP(obj)  SCM_ISA(obj, SCM_CLASS_ZLIB_NEED_DICT_ERROR)

typedef ScmZlibError ScmZlibStreamError;
SCM_CLASS_DECL(Scm_ZlibStreamErrorClass);
#define SCM_CLASS_ZLIB_STREAM_ERROR  (&Scm_ZlibStreamErrorClass)
#define SCM_ZLIB_STREAM_ERROR(obj)   ((ScmZlibStreamError*)(obj))
#define SCM_ZLIB_STREAM_ERRORP(obj)  SCM_ISA(obj, SCM_CLASS_ZLIB_STREAM_ERROR)

typedef ScmZlibError ScmZlibDataError;
SCM_CLASS_DECL(Scm_ZlibDataErrorClass);
#define SCM_CLASS_ZLIB_DATA_ERROR       (&Scm_ZlibDataErrorClass)
#define SCM_ZLIB_DATA_ERROR(obj)        ((ScmZlibDataError*)(obj))
#define SCM_ZLIB_DATA_ERRORP(obj)       SCM_ISA(obj, SCM_CLASS_ZLIB_DATA_ERROR)

typedef ScmZlibError ScmZlibMemoryError;
SCM_CLASS_DECL(Scm_ZlibMemoryErrorClass);
#define SCM_CLASS_ZLIB_MEMORY_ERROR     (&Scm_ZlibMemoryErrorClass)
#define SCM_ZLIB_MEMORY_ERROR(obj)      ((ScmZlibMemoryError*)(obj))
#define SCM_ZLIB_MEMORY_ERRORP(obj)     SCM_ISA(obj, SCM_CLASS_ZLIB_MEMORY_ERROR)

typedef ScmZlibError ScmZlibVersionError;
SCM_CLASS_DECL(Scm_ZlibVersionErrorClass);
#define SCM_CLASS_ZLIB_VERSION_ERROR    (&Scm_ZlibVersionErrorClass)
#define SCM_ZLIB_VERSION_ERROR(obj)     ((ScmZlibVersionError*)(obj))
#define SCM_ZLIB_VERSION_ERRORP(obj)    SCM_ISA(obj, SCM_CLASS_ZLIB_VERSION_ERROR)

extern ScmObj Scm_MakeZlibError(ScmObj message, int error_code);
extern void Scm_ZlibError(int error_code, const char *msg, ...);
extern ScmObj Scm_InflateSync(ScmPort *port);

extern void Scm_Init_zlib(void);

/* Epilogue */
SCM_DECL_END

#endif  /* GAUCHE_ZLIB_H */
