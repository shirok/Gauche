/*
 * charconv.h - character code conversion library
 *
 *   Copyright (c) 2000-2020  Shiro Kawai  <shiro@acm.org>
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

#ifndef GAUCHE_CHARCONV_H
#define GAUCHE_CHARCONV_H

#include <gauche.h>
#include "iconv-adapter.h"

SCM_DECL_BEGIN

struct ScmConvInfoRec;

typedef ScmSize ScmConvProc(struct ScmConvInfoRec*, const char*, ScmSize,
                            char*, ScmSize, ScmSize*);
typedef ScmSize ScmConvReset(struct ScmConvInfoRec*, char*, ScmSize);
typedef ScmSize ScmConvHandler(struct ScmConvInfoRec*, const char **,
                               ScmSize*, char**, ScmSize*);

/* Packaging conversion context info.*/
typedef struct ScmConvInfoRec {
    ScmConvHandler *jconv;      /* jconv handler */
    ScmConvProc *convert;       /* 1-character conversion routine */
    ScmConvReset *reset;        /* reset routine */
    iconv_t handle;             /* iconv handle, if the conversion is
                                   handled by iconv */
    const char *fromCode;       /* convert from ... */
    const char *toCode;         /* conver to ... */
    int istate;                 /* current input state */
    int ostate;                 /* current output state */
    ScmPort *remote;            /* source or drain port */
    int ownerp;                 /* do I own remote port? */
    int remoteClosed;           /* true if remore port is closed */
    int replacep;               /* true if we replace unrecognized input
                                   with replacement sequence */
    ScmSize replaceSize;        /* size of replaceSeq */
    const char *replaceSeq;     /* the replacement sequence, NULL terminated */
    ScmSize bufsiz;             /* size of conversion buffer */
    char *buf;                  /* internal conversion buffer */
    char *ptr;                  /* current ptr in the internal conv buf */
} ScmConvInfo;

/* Bitmask for 'flags' argument.
   Scm_ConversionSupportedP only recognizes CVPORT_ICONV. */
enum {
    CVPORT_OWNER = (1L<<0),     /* Close the inner port if the conversion port
                                   is closed. */
    CVPORT_REPLACE = (1L<<1),   /* Use replacement character for illegal 
                                   sequences instead of signaling an error */
};

extern ScmObj Scm_MakeInputConversionPort(ScmPort *source,
                                          const char *fromCode,
                                          const char *toCode,
                                          ScmSize bufsiz,
                                          u_long flags);
extern ScmObj Scm_MakeOutputConversionPort(ScmPort *sink,
                                           const char *toCode,
                                           const char *fromCode,
                                           ScmSize bufsiz,
                                           u_long flags);

typedef const char *(*ScmCodeGuessingProc)(const char *buf,
                                           ScmSize bufsiz,
                                           void *data);

extern const char *Scm_GetCESName(ScmObj code, const char *argname);
extern int Scm_ConversionSupportedP(const char *from, const char *to,
                                    u_long flags);

extern void Scm_RegisterCodeGuessingProc(const char *code,
                                         ScmCodeGuessingProc proc,
                                         void *data);

extern const char *Scm_GuessCES(const char *code,
                                const char *buf,
                                ScmSize buflen);

/* 
 * jconv interface
 *
 *   jconv is a lower-level layer
 */

/* jconv error code */
#define ILLEGAL_SEQUENCE  (-1)  /* input contains illegal sequence */
#define INPUT_NOT_ENOUGH  (-2)  /* input terminates prematurely */
#define OUTPUT_NOT_ENOUGH (-3)  /* output buffer is too small */
#define NO_OUTPUT_CHAR    (-4)  /* char can't be represented in output CES */

extern ScmConvInfo *jconv_open(const char *toCode, 
                               const char *fromCode,
                               int useIconv);
extern int jconv_close(ScmConvInfo*);
extern ScmSize jconv(ScmConvInfo*, const char **inptr, ScmSize *inroom,
                     char **outptr, ScmSize *outroom);
extern ScmSize jconv_reset(ScmConvInfo *, char *outptr, ScmSize outroom);
extern void jconv_set_replacement(ScmConvInfo *info);

/* Given UCS char, return # of bytes required for UTF8 encoding.
   We have these in char_utf8.h, but it is only available when the
   native encoding is utf-8.   Eventually we need to factor these out.
 */
#define UCS2UTF_NBYTES(ucs)                      \
    (((ucs) < 0x80) ? 1 :                        \
     (((ucs) < 0x800) ? 2 :                      \
      (((ucs) < 0x10000) ? 3 :                   \
       (((ucs) < 0x200000) ? 4 :                 \
        (((ucs) < 0x4000000) ? 5 : 6)))))

extern void jconv_ucs4_to_utf8(unsigned int ucs, char *cp);
extern int  jconv_utf8_to_ucs4(const char *cp,
                               ScmSize size,
                               ScmChar *ucs);   /* out */

SCM_DECL_END

#endif /*GAUCHE_CHARCONV_H*/

/*
 * Local variables:
 * mode: c
 * end:
 */
