/*
 * charconv.c - character code conversion library
 *
 *  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, distribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: charconv.c,v 1.16 2001-06-08 09:36:09 shirok Exp $
 */

#include <string.h>
#include <errno.h>
#include <gauche.h>
#include "charconv.h"

#ifdef HAVE_ICONV_H
#include <iconv.h>
#else
typedef void *iconv_t;
iconv_t iconv_open(const char *tocode, const char *fromcode);
int iconv_close(iconv_t handle);
int iconv(iconv_t handle, char **inbuf, size_t *inroom, char **outbuf, size_t *outroom);
#endif /* !HAVE_ICONV_H */

#ifdef ICONV_CONST_INPUT
#define INBUFCAST /*none*/
#else
#define INBUFCAST (char **)
#endif

#define DEFAULT_CONVERSION_BUFFER_SIZE 1024

typedef struct conv_info_rec {
    iconv_t handle;
    ScmPort *remote;
    int ownerp;
    int bufsiz;
    char *inbuf;
    char *inptr;
    char *outbuf;
    char *outptr;
} conv_info;

typedef struct conv_guess_rec {
    const char *codeName;
    ScmCodeGuessingProc proc;
    void *data;
    struct conv_guess_rec *next;
} conv_guess;

/* TODO: MT safeness! */
static conv_guess *guess_procs = NULL;

/*------------------------------------------------------------
 * Query
 */

/* Auxiliary function */
const char* Scm_GetCESName(ScmObj code, const char *argname)
{
    const char *c = NULL;
    if (SCM_UNBOUNDP(code) || SCM_FALSEP(code)) {
        c = Scm_SupportedCharacterEncodings()[0];
    } else if (!SCM_STRINGP(code)) {
        Scm_Error("string or #f is required for %s, but got %S",
                  argname, code);
    } else {
        c = Scm_GetStringConst(SCM_STRING(code));
    }
    return c;
}

int Scm_ConversionSupportedP(const char *from, const char *to)
{
    iconv_t cd = iconv_open(to, from);
    if (cd == (iconv_t)-1) return FALSE;
    iconv_close(cd);
    return TRUE;
}

void Scm_RegisterCodeGuessingProc(const char *code,
                                  ScmCodeGuessingProc proc,
                                  void *data)
{
    conv_guess *rec = SCM_NEW(conv_guess);
    /* TODO: MT safeness */
    rec->next = guess_procs;
    rec->codeName = code;
    rec->proc = proc;
    rec->data = data;
    guess_procs = rec;
}

static conv_guess *findGuessingProc(const char *code)
{
    conv_guess *rec = guess_procs;
    for (; rec; rec = rec->next) {
        if (strcasecmp(rec->codeName, code) == 0) return rec;
    }
    return NULL;
}

/*------------------------------------------------------------
 * Input conversion
 *
 *  <-- Bufferd port <--outbuf-- filler <--inbuf--- getz(remote)
 */

static int conv_input_filler(char *buf, int len, void *data)
{
    conv_info *info = (conv_info*)data;
    size_t insize, inroom, outroom, result;
    int nread;
    const char *inbuf = info->inbuf;
    char *outbuf = info->outbuf;

    /* Fill the input buffer.  There are some remaining bytes in the
       inbuf from the last conversion (insize), so we try to fill the
       rest. */
    insize = info->inptr - info->inbuf, 
    nread = Scm_Getz(info->inptr, info->bufsiz - insize, info->remote);
    if (nread <= 0) {
        if (info->ownerp) Scm_ClosePort(info->remote);
        if (insize == 0) return 0; /* All done. */
    } else {
        insize += nread;
    }

    /* Conversion. */
    inroom = insize;
    outroom = info->bufsiz;
#ifdef DEBUG
    fprintf(stderr, "=> in(%p,%p)%d out(%p,%p)%d\n",
            info->inbuf, info->inptr, insize,
            info->outbuf, info->outptr, outroom);
#endif
    result = iconv(info->handle, INBUFCAST &inbuf, &inroom, &outbuf, &outroom);
#ifdef DEBUG
    fprintf(stderr, "<= r=%d, in(%p)%d out(%p)%d\n",
            result, inbuf, inroom, outbuf, outroom);
#endif
    if (result == (size_t)-1) {
        if (errno == EINVAL || errno == E2BIG) {
            /* Conversion stopped due to an incomplete character at the
               end of the input buffer, or the output buffer is full.
               We shift the unconverted bytes to the beginning of input
               buffer. */
            memmove(info->inbuf, info->inbuf+insize-inroom, inroom);
            info->inptr = info->inbuf + inroom;
            return info->bufsiz - outroom;
        } else {
            /* it's likely that input contains invalid sequence.
               TODO: we should handle this case gracefully. */
            int cnt = inroom >= 6 ? 6 : inroom;
            ScmObj s = Scm_MakeString(info->inbuf+insize-inroom, cnt, cnt,
                                      SCM_MAKSTR_COPYING|SCM_MAKSTR_INCOMPLETE);
            Scm_Error("invalid character sequence in the input stream: %S ...",
                      s);
            return 0;           /* dummy */
        }
    } else {
        /* Conversion is done completely. */
        /* NB: There are cases that some bytes are left in the input buffer
           even iconv returns positive value.  We need to shift those bytes. */
        if (inroom > 0) {
            memmove(info->inbuf, info->inbuf+insize-inroom, inroom);
            info->inptr = info->inbuf + inroom;
            return info->bufsiz - outroom;
        } else {
            info->inptr = info->inbuf;
            return info->bufsiz - outroom;
        }
    }
}

ScmObj Scm_MakeInputConversionPort(ScmPort *fromPort,
                                   const char *fromCode,
                                   const char *toCode,
                                   int bufsiz,
                                   int ownerp)
{
    conv_info *cinfo;
    iconv_t handle;
    ScmPort *newport;
    conv_guess *guess;
    char *inbuf = NULL;
    int preread = 0;

    if (!SCM_IPORTP(fromPort))
        Scm_Error("input port required, but got %S", fromPort);

    if (bufsiz <= 0) bufsiz = DEFAULT_CONVERSION_BUFFER_SIZE;
    guess = findGuessingProc(fromCode);
    if (guess) {
        const char *guessed;
        
        inbuf = SCM_NEW_ATOMIC2(char *, bufsiz);
        preread = Scm_Getz(inbuf, bufsiz, fromPort);
        if (preread <= 0)
            Scm_Error("input port is already empty: %S", fromPort);
        guessed = guess->proc(inbuf, preread, guess->data);
        if (guessed == NULL)
            Scm_Error("%s: failed to guess input encoding", fromCode);
        fromCode = guessed;
    }

    handle = iconv_open(toCode, fromCode);
    if (handle == (iconv_t)-1) {
        if (errno == EINVAL) {
            Scm_Error("conversion from code %s to code %s is not supported",
                      fromCode, toCode);
        } else {
            /* TODO: try to call gc to collect unused file descriptors */
            Scm_SysError("iconv_open failed");
        }
    }
    cinfo = SCM_NEW(conv_info);
    cinfo->handle = handle;
    cinfo->remote = fromPort;
    cinfo->ownerp = ownerp;
    cinfo->bufsiz = bufsiz;
    if (preread > 0) {
        cinfo->inbuf = inbuf;
        cinfo->inptr = inbuf + preread;
    } else {
        cinfo->inbuf = SCM_NEW_ATOMIC2(char *, cinfo->bufsiz);
        cinfo->inptr = cinfo->inbuf;
    }
    cinfo->outbuf = SCM_NEW_ATOMIC2(char *, cinfo->bufsiz);
    cinfo->outptr = cinfo->outbuf;
    
    return Scm_MakeBufferedPort(SCM_PORT_INPUT, cinfo->bufsiz, 0,
                                cinfo->outbuf, conv_input_filler,
                                (void*)cinfo);
}

/*------------------------------------------------------------
 * Output conversion
 *
 *   Bufferd port -->inbuf--> flusher -->outbuf--> putz(remote)
 */

/* NB: Glibc-2.1.2's iconv() has a bug in SJIS handling.  If output
 * is in SJIS and output buffer overflows in the middle of two-byte
 * sequence, it leaves the first byte in the output buffer as if
 * it were valid converted character, while the input buffer pointer
 * stops just before the unconverted character, as supposed.
 * There's no way to detect that unless I scan the output by myself
 * to see the last byte of conversion is invalid or not.
 *
 * As a walkaround, I flush the output buffer more frequently than
 * needed, avoiding the situation that the output buffer overflow.
 * Hoping the bugs are fixed in the future release of glibc.
 */

#define GLIBC_2_1_ICONV_BUG

static int conv_output_flusher(char *buf, int len, void *data)
{
    conv_info *info = (conv_info*)data;
    size_t outsize, inroom, outroom, result;
    int nread;
    const char *inbuf;
    char *outbuf;

    if (buf == NULL) {
        /* the port is closed.  flush outbuf */
        Scm_Putz(info->outbuf, info->outptr - info->outbuf, info->remote);
        Scm_Flush(info->remote);
        if (info->ownerp) Scm_ClosePort(info->remote);
        return 0;
    }

    inbuf = info->inbuf;
    inroom = len;
    for (;;) {
        /* Conversion. */
        outbuf = info->outptr;
        outsize = info->bufsiz - (info->outptr - info->outbuf);
        outroom = outsize;
#ifdef DEBUG
        fprintf(stderr, "=> in(%p,%p)%d out(%p,%p)%d\n",
                info->inbuf, len, inroom,
                info->outbuf, info->outptr, outroom);
#endif
        result = iconv(info->handle, INBUFCAST &inbuf, &inroom, &outbuf, &outroom);
#ifdef DEBUG
        fprintf(stderr, "<= r=%d, in(%p)%d out(%p)%d\n",
                result, inbuf, inroom, outbuf, outroom);
#endif
        if (result == (size_t)-1) {
            if (errno == EINVAL) {
#ifndef GLIBC_2_1_ICONV_BUG
                /* Conversion stopped due to an incomplete character at the
                   end of the input buffer.  We just return # of bytes
                   flushed.  (Shifting unconverted characters is done by
                   buffered port routine) */
                info->outptr = outbuf;
                return len - inroom;
#else
                /* See the above notes.  We always flush the output buffer
                   here, so that we can avoid output buffer overrun. */
                Scm_Putz(info->outbuf, outbuf - info->outbuf, info->remote);
                info->outptr = info->outbuf;
                return len - inroom;
#endif
            } else if (errno == E2BIG) {
                /* Output buffer got full.  Flush it, and continue
                   conversion. */
                Scm_Putz(info->outbuf, outbuf - info->outbuf, info->remote);
                info->outptr = info->outbuf;
                continue;
            } else {
                /* it's likely that input contains invalid sequence.
                   TODO: we should handle this case gracefully. */
                Scm_Error("invalid character sequence in the input stream");
                return 0;           /* dummy */
            }
        } else {
#ifndef GLIBC_2_1_ICONV_BUG
            /* Conversion is done completely.  Update outptr. */
            info->outptr = outbuf;
            return len - inroom;
#else
            /* See the above notes.  We always flush the output buffer here,
               so that we can avoid output buffer overrun. */
            Scm_Putz(info->outbuf, outbuf - info->outbuf, info->remote);
            info->outptr = info->outbuf;
            return len - inroom;
#endif
        }
    }
}

ScmObj Scm_MakeOutputConversionPort(ScmPort *toPort,
                                    const char *toCode,
                                    const char *fromCode,
                                    int bufsiz, int ownerp)
{
    conv_info *cinfo;
    iconv_t handle;
    ScmPort *newport;
    
    if (!SCM_OPORTP(toPort))
        Scm_Error("output port required, but got %S", toPort);

    handle = iconv_open(toCode, fromCode);
    if (handle == (iconv_t)-1) {
        if (errno == EINVAL) {
            Scm_Error("conversion from code %s to code %s is not supported",
                      fromCode, toCode);
        } else {
            /* TODO: try to call gc to collect unused file descriptors */
            Scm_SysError("iconv_open failed");
        }
    }
    cinfo = SCM_NEW(conv_info);
    cinfo->handle = handle;
    cinfo->remote = toPort;
    cinfo->ownerp = ownerp;
    cinfo->bufsiz = (bufsiz > 0)? bufsiz : DEFAULT_CONVERSION_BUFFER_SIZE;
    cinfo->inbuf = SCM_NEW_ATOMIC2(char *, cinfo->bufsiz);
    cinfo->inptr = cinfo->inbuf;
    cinfo->outbuf = SCM_NEW_ATOMIC2(char *, cinfo->bufsiz);
    cinfo->outptr = cinfo->outbuf;
    
    return Scm_MakeBufferedPort(SCM_PORT_OUTPUT, cinfo->bufsiz, 0,
                                cinfo->inbuf, conv_output_flusher,
                                (void*)cinfo);
}

/*------------------------------------------------------------
 * Direct interface for code guessing
 */
const char *Scm_GuessCES(const char *code, const char *buf, int buflen)
{
    conv_guess *guess = findGuessingProc(code);
    if (guess == NULL)
        Scm_Error("unknown code guessing scheme: %s", code);
    return guess->proc(buf, buflen, guess->data);
}

/*====================================================================
 * Builtin code guessing functions
 */

/* Auto-detect japanese character encoding, among ISO2022JP, EUC, SJIS
 * or UTF-8.  If the input is ambiguous, it is in favor of the native
 * encoding.  The algorithm is a hardcoded DFA.
 */

static const signed char eucjp[][256] = {
    /* EUC-JP first byte dispatch */
    {/* 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*0*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*1*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*2*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*3*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*4*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*5*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*6*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*7*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, 2,-1, /*8*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*9*/
       -1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /*A*/
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /*B*/
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /*C*/
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /*D*/
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /*E*/
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,-1, /*F*/
    },
    /* EUC-JP second byte dispatch */
    {/* 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*0*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*1*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*2*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*3*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*4*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*5*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*6*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*7*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*8*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*9*/
       -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*A*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*B*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*C*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*D*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*E*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, /*F*/
    },
    /* EUC-JP hankaku-kana (CS1) */
    {/* 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*0*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*1*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*2*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*3*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*4*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*5*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*6*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*7*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*8*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*9*/
       -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*A*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*B*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*C*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*D*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*E*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*F*/
    },
};

static const signed char sjis[][256] = {
    /* SJIS first byte dispatch */
    {/* 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*0*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*1*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*2*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*3*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*4*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*5*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*6*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*7*/
        0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /*8*/
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /*9*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*A*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*B*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*C*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*D*/
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /*E*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*F*/
    },
    /* SJIS second byte dispatch */
    {/* 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*0*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*1*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*2*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*3*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*4*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*5*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*6*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1, /*7*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*8*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*9*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*A*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*B*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*C*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*D*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*E*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1,-1, /*F*/
    },
};
    
static const signed char utf8[][256] = {
    /* UTF-8 first byte dispatch */
    {/* 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*0*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*1*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*2*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*3*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*4*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*5*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*6*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*7*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*8*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*9*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*A*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*B*/
       -1,-1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /*C*/
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /*D*/
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, /*E*/
        3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 5,-1,-1, /*F*/
    },
    /* UTF-8 the last byte */
    {/* 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*0*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*1*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*2*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*3*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*4*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*5*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*6*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*7*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*8*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*9*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*A*/
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, /*B*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*C*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*D*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*E*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*F*/
    },
    /* UTF-8 the last two byte */
    {/* 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*0*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*1*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*2*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*3*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*4*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*5*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*6*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*7*/
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /*8*/
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /*9*/
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /*A*/
        1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, /*B*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*C*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*D*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*E*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*F*/
    },
    /* UTF-8 the last three byte */
    {/* 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*0*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*1*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*2*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*3*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*4*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*5*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*6*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*7*/
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, /*8*/
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, /*9*/
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, /*A*/
        2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, /*B*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*C*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*D*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*E*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*F*/
    },
    /* UTF-8 the last four byte */
    {/* 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*0*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*1*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*2*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*3*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*4*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*5*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*6*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*7*/
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, /*8*/
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, /*9*/
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, /*A*/
        3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, /*B*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*C*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*D*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*E*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*F*/
    },
    /* UTF-8 the last five byte */
    {/* 0  1  2  3  4  5  6  7  8  9  A  B  C  D  E  F*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*0*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*1*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*2*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*3*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*4*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*5*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*6*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*7*/
        4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, /*8*/
        4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, /*9*/
        4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, /*A*/
        4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 4, /*B*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*C*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*D*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*E*/
       -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1, /*F*/
    },
};
    
static const char *guess_jp(const char *buf, int buflen, void *data)
{
    int i;
    int eucstat = 0;
    int sjisstat = 0;
    int utf8stat = 0;
    
    for (i=0; i<buflen; i++) {
        int c = (unsigned char)buf[i];
        /* special treatment of jis escape sequence */
        if (c == 0x1b) {
            if (i < buflen-1) {
                c = (unsigned char)buf[++i];
                if (c == '$' || c == '(') return "CSISO2022JP";
            }
        }
        if (eucstat >= 0) {
            if (sjisstat < 0 && utf8stat < 0) return "EUCJP";
            eucstat = eucjp[eucstat][c];
        }
        if (sjisstat >= 0) {
            if (eucstat < 0 && utf8stat < 0) return "SJIS";
            sjisstat = sjis[sjisstat][c];
        }
        if (utf8stat >= 0) {
            if (eucstat < 0 && sjisstat < 0) return "UTF-8";
            utf8stat = utf8[utf8stat][c];
        }
        if (eucstat < 0 && sjisstat < 0 && utf8stat < 0) {
            /* we ran out the possibilities */
            return NULL;
        }
    }
    /* Now, we have ambigous code. */
#if defined GAUCHE_CHAR_ENCODING_EUC_JP
    if (eucstat >= 0) return "EUCJP";
    if (sjisstat >= 0) return "SJIS";
    if (utf8stat >= 0) return "UTF-8";
    return NULL;
#elif defined GAUCHE_CHAR_ENCODING_UTF_8
    if (utf8stat >= 0) return "UTF-8";
    if (eucstat >= 0) return "EUCJP";
    if (sjisstat >= 0) return "SJIS";
    return NULL;
#elif defined GAUCHE_CHAR_ENCODING_SJIS
    if (sjisstat >= 0) return "SJIS";
    if (eucstat >= 0) return "EUCJP";
    if (utf8stat >= 0) return "UTF-8";
    return NULL;
#else
    return NULL;
#endif
}


/*====================================================================
 * Initialization
 */
extern void Scm_Init_convlib(ScmModule *module);

void Scm_Init_libcharconv(void)
{
    ScmModule *mod = SCM_MODULE(SCM_FIND_MODULE("gauche.charconv", TRUE));
    Scm_RegisterCodeGuessingProc("*JP", guess_jp, NULL);
    Scm_Init_convlib(mod);
}

#ifndef HAVE_ICONV_H
/* substitution of iconv for the platforms where it is not available */

/* for now, we only have dummy functions */
iconv_t iconv_open(const char *tocode, const char *fromcode)
{
    return (iconv_t)-1;
}

int iconv_close(iconv_t handle)
{
    return 0;
}

int iconv(iconv_t handle, char **inbuf, size_t *inroom, char **outbuf, size_t *outroom)
{
    return 0;
}
#endif /* !HAVE_ICONV_H */

