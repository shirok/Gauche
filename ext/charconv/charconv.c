/*
 * charconv.c - character code conversion library
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

#include <string.h>
#include <errno.h>
#include <gauche.h>
#include <gauche/extend.h>
#include "gauche/priv/portP.h"
#include "charconv.h"

#define DEFAULT_CONVERSION_BUFFER_SIZE 1024
#define MINIMUM_CONVERSION_BUFFER_SIZE 16

typedef struct conv_guess_rec {
    const char *codeName;
    ScmCodeGuessingProc proc;
    void *data;
    struct conv_guess_rec *next;
} conv_guess;

/* anchor of the chain of conversion guessing procedures */
static struct {
    int dummy;                  /* trick to place this in .data section */
    conv_guess *procs;
    ScmInternalMutex mutex;
} guess = { 1 };

/* anchor of the conversion context used for UCS -> internal char routine */
static struct {
    int dummy;                  /* trick to place this in .data section */
    ScmConvInfo *ucs2char;
    ScmConvInfo *char2ucs;
    ScmInternalMutex mutex;
} ucsconv = { 1 };

/*------------------------------------------------------------
 * Query
 */

/* Auxiliary function */
const char* Scm_GetCESName(ScmObj code, const char *argname)
{
    const char *c = NULL;
    if (SCM_UNBOUNDP(code) || SCM_FALSEP(code)) {
        c = Scm_SupportedCharacterEncodings()[0];
    } else if (SCM_STRINGP(code)) {
        c = Scm_GetStringConst(SCM_STRING(code));
    } else if (SCM_SYMBOLP(code)) {
        c = Scm_GetStringConst(SCM_SYMBOL_NAME(code));
    } else {
        Scm_Error("string, symbol or #f is required for %s, but got %S",
                  argname, code);
    }
    return c;
}

int Scm_ConversionSupportedP(const char *from, const char *to)
{
    ScmConvInfo *info = jconv_open(to, from);
    if (info == NULL) return FALSE;
    jconv_close(info);
    return TRUE;
}

void Scm_RegisterCodeGuessingProc(const char *code,
                                  ScmCodeGuessingProc proc,
                                  void *data)
{
    conv_guess *rec = SCM_NEW(conv_guess);
    rec->codeName = code;
    rec->proc = proc;
    rec->data = data;
    (void)SCM_INTERNAL_MUTEX_LOCK(guess.mutex);
    rec->next = guess.procs;
    guess.procs = rec;
    (void)SCM_INTERNAL_MUTEX_UNLOCK(guess.mutex);
}

static conv_guess *findGuessingProc(const char *code)
{
    conv_guess *rec;
    (void)SCM_INTERNAL_MUTEX_LOCK(guess.mutex);
    for (rec = guess.procs; rec; rec = rec->next) {
        if (strcasecmp(rec->codeName, code) == 0) break;
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(guess.mutex);
    return rec;
}

static int conv_fileno(ScmPort *port)
{
    ScmConvInfo *info = (ScmConvInfo*)port->src.buf.data;
    return Scm_PortFileNo(info->remote);
}

static int conv_ready(ScmPort *port)
{
    ScmConvInfo *info = (ScmConvInfo*)port->src.buf.data;
    /* This isn't accurate, but for now ... */
    return Scm_CharReady(info->remote);
}

static ScmObj conv_name(int dir, ScmPort *remote, const char *from, const char *to)
{
    ScmObj out = Scm_MakeOutputStringPort(TRUE);
    Scm_Printf(SCM_PORT(out), "[conv(%s->%s) %s %S]",
               from, to, (dir == SCM_PORT_INPUT? "from" : "to"),
               Scm_PortName(remote));
    return Scm_GetOutputStringUnsafe(SCM_PORT(out), 0);
}

/*------------------------------------------------------------
 * Input conversion
 *
 *  <-- Buffered port <--- filler <--(info->buf)--- getz(remote)
 */

static int conv_input_filler(ScmPort *port, int mincnt)
{
    ScmConvInfo *info = (ScmConvInfo*)port->src.buf.data;
    const char *inbuf = info->buf;
    char *outbuf = port->src.buf.end;

    if (info->remoteClosed) return 0;

    /* Fill the input buffer.  There may be some remaining bytes in the
       inbuf from the last conversion (insize), so we try to fill the
       rest. */
    size_t insize = info->ptr - info->buf;
    int nread = Scm_Getz(info->ptr, info->bufsiz - (int)insize, info->remote);
    if (nread <= 0) {
        /* input reached EOF.  finish the output state */
        if (insize == 0) {
            size_t outroom = SCM_PORT_BUFFER_ROOM(port);
            size_t result = jconv_reset(info, outbuf, outroom);
            if (result == OUTPUT_NOT_ENOUGH) {
                /* The port buffer doesn't have enough space to contain the
                   finishing sequence.  Its unusual, for the port buffer
                   must be almost empty at this time, and the finishing
                   sequence is usually just a few bytes.
                   We signal an error. */
                Scm_Error("couldn't flush the ending escape sequence in the character encoding conversion port (%s -> %s).  possibly an implementation error",
                          info->fromCode, info->toCode);
                }
            if (info->ownerp) {
                Scm_ClosePort(info->remote);
                info->remoteClosed = TRUE;
            }
#ifdef JCONV_DEBUG
            fprintf(stderr, "<= r=%d (reset), out(%p)%d\n",
                    result, outbuf, outroom);
#endif
            return (int)result;
        }
    } else {
        insize += nread;
    }

    /* Conversion. */
    size_t inroom = insize;
    size_t outroom = SCM_PORT_BUFFER_ROOM(port);

#ifdef JCONV_DEBUG
    fprintf(stderr, "=> in(%p)%d out(%p)%d\n", inbuf, insize, outbuf, outroom);
#endif
    size_t result = jconv(info, &inbuf, &inroom, &outbuf, &outroom);
#ifdef JCONV_DEBUG
    fprintf(stderr, "<= r=%d, in(%p)%d out(%p)%d\n",
            result, inbuf, inroom, outbuf, outroom);
#endif
    /* we've got an error. */
    if (result == INPUT_NOT_ENOUGH || result == OUTPUT_NOT_ENOUGH) {
        /* Conversion stopped due to an incomplete character at the
           end of the input buffer, or the output buffer is full.
           We shift the unconverted bytes to the beginning of input
           buffer. */
        memmove(info->buf, info->buf+insize-inroom, inroom);
        info->ptr = info->buf + inroom;
        return info->bufsiz - (int)outroom;
    } else if (result == ILLEGAL_SEQUENCE) {
        /* it's likely that the input contains invalid sequence. */
        int cnt = inroom >= 6 ? 6 : (int)inroom;
        ScmObj s = Scm_MakeString(info->buf+insize-inroom, cnt, cnt,
                                  SCM_STRING_COPYING|SCM_STRING_INCOMPLETE);
        Scm_Error("invalid character sequence in the input stream: %S ...", s);
    }

    /* Conversion is done completely. */
    /* NB: There are cases that some bytes are left in the input buffer
       even iconv returns positive value.  We need to shift those bytes. */
    if (inroom > 0) {
        memmove(info->buf, info->buf+insize-inroom, inroom);
        info->ptr = info->buf + inroom;
        return info->bufsiz - (int)outroom;
    } else {
        info->ptr = info->buf;
        return info->bufsiz - (int)outroom;
    }
}

static void conv_input_closer(ScmPort *p)
{
    ScmConvInfo *info = (ScmConvInfo*)p->src.buf.data;
    jconv_close(info);
    if (info->ownerp) {
        Scm_ClosePort(info->remote);
        info->remoteClosed = TRUE;
    }
}

ScmObj Scm_MakeInputConversionPort(ScmPort *fromPort,
                                   const char *fromCode,
                                   const char *toCode,
                                   ScmObj handler,
                                   int bufsiz,
                                   int ownerp)
{
    char *inbuf = NULL;
    int preread = 0;

    if (!SCM_IPORTP(fromPort))
        Scm_Error("input port required, but got %S", fromPort);

    if (bufsiz <= 0) bufsiz = DEFAULT_CONVERSION_BUFFER_SIZE;
    if (bufsiz <= MINIMUM_CONVERSION_BUFFER_SIZE) {
        bufsiz = MINIMUM_CONVERSION_BUFFER_SIZE;
    }
    conv_guess *guess = findGuessingProc(fromCode);
    if (guess) {
        const char *guessed;

        inbuf = SCM_NEW_ATOMIC2(char *, bufsiz);
        preread = Scm_Getz(inbuf, bufsiz, fromPort);
        if (preread <= 0) {
            /* Input buffer is already empty or unreadable.
               Determining character code is not necessary.
               We just return a dummy empty port. */
            return Scm_MakeInputStringPort(SCM_STRING(SCM_MAKE_STR("")), FALSE);
        }
        guessed = guess->proc(inbuf, preread, guess->data);
        if (guessed == NULL)
            Scm_Error("%s: failed to guess input encoding", fromCode);
        fromCode = guessed;
    }

    ScmConvInfo *cinfo = jconv_open(toCode, fromCode);
    if (cinfo == NULL) {
        Scm_Error("conversion from code %s to code %s is not supported",
                  fromCode, toCode);
    }
    cinfo->remote = fromPort;
    cinfo->ownerp = ownerp;
    cinfo->bufsiz = bufsiz;
    cinfo->remoteClosed = FALSE;
    if (preread > 0) {
        cinfo->buf = inbuf;
        cinfo->ptr = inbuf + preread;
    } else {
        cinfo->buf = SCM_NEW_ATOMIC2(char *, cinfo->bufsiz);
        cinfo->ptr = cinfo->buf;
    }

    ScmPortBuffer bufrec;
    memset(&bufrec, 0, sizeof(bufrec));
    bufrec.size = cinfo->bufsiz;
    bufrec.buffer = SCM_NEW_ATOMIC2(char *, cinfo->bufsiz);
    bufrec.mode = SCM_PORT_BUFFER_FULL;
    bufrec.filler = conv_input_filler;
    bufrec.flusher = NULL;
    bufrec.closer = conv_input_closer;
    bufrec.ready = conv_ready;
    bufrec.filenum = conv_fileno;
    bufrec.data = (void*)cinfo;

    ScmObj name = conv_name(SCM_PORT_INPUT, fromPort, fromCode, toCode);
    return Scm_MakeBufferedPort(SCM_CLASS_PORT, name, SCM_PORT_INPUT, TRUE, &bufrec);
}

/* a special case of input conversion port --- coding-aware port coversion.
   this function is called via Scm_CodingAwarePortHook from
   src/port.c */

static ScmPort *coding_aware_conv(ScmPort *src, const char *encoding)
{
    return SCM_PORT(Scm_MakeInputConversionPort(src,
                                                encoding,
                                                Scm_SupportedCharacterEncodings()[0],
                                                SCM_FALSE,
                                                0, TRUE));
}

/*------------------------------------------------------------
 * Output conversion
 *
 *   Buffered port ----> flusher -->(info->buf)--> putz(remote)
 */

/* NB: Glibc-2.1.2's iconv() has a bug in SJIS handling.  If output
 * is in SJIS and output buffer overflows in the middle of two-byte
 * sequence, it leaves the first byte in the output buffer as if
 * it were valid converted character, while the input buffer pointer
 * stops just before the unconverted character, as supposed.
 * There's no way to detect that unless I scan the output by myself
 * to see the last byte of conversion is invalid or not.
 *
 * As a workaround, I flush the output buffer more frequently than
 * needed, avoiding the situation that the output buffer overflow.
 * Hoping the bugs are fixed in the future release of glibc.
 */

#define GLIBC_2_1_ICONV_BUG

static void conv_output_closer(ScmPort *port)
{
    ScmConvInfo *info = (ScmConvInfo*)port->src.buf.data;

    /* if there's remaining bytes in buf, send them to the remote port. */
    if (info->ptr > info->buf) {
        Scm_Putz(info->buf, (int)(info->ptr - info->buf), info->remote);
        info->ptr = info->buf;
    }
    /* sends out the closing sequence, if any */
    int r = (int)jconv_reset(info, info->buf, info->bufsiz);
#ifdef JCONV_DEBUG
    fprintf(stderr, "<= r=%d(reset), buf(%p)\n",
            r, info->buf);
#endif
    if (r < 0) {
        Scm_Error("something wrong in resetting output character encoding conversion (%s -> %s).  possibly an implementation error.",
                  info->fromCode, info->toCode);
    }
    if (r > 0) {
        Scm_Putz(info->buf, r, info->remote);
    }
    /* flush remove port */
    Scm_Flush(info->remote);
    if (info->ownerp) {
        Scm_ClosePort(info->remote);
        info->remoteClosed = TRUE;
    }
    jconv_close(info);
}

static int conv_output_flusher(ScmPort *port, int cnt, int forcep)
{
    ScmConvInfo *info = (ScmConvInfo*)port->src.buf.data;
    size_t inroom = SCM_PORT_BUFFER_AVAIL(port);
    size_t len = inroom;
    const char *inbuf = port->src.buf.buffer;

    for (;;) {
        /* Conversion. */
        char *outbuf = info->ptr;
        size_t outroom = info->bufsiz - (info->ptr - info->buf);
#ifdef JCONV_DEBUG
        fprintf(stderr, "=> in(%p,%p)%d out(%p,%p)%d\n",
                inbuf, len, inroom,
                info->buf, info->ptr, outroom);
#endif
        size_t result = jconv(info, &inbuf, &inroom, &outbuf, &outroom);
#ifdef JCONV_DEBUG
        fprintf(stderr, "<= r=%d, in(%p)%d out(%p)%d\n",
                result, inbuf, inroom, outbuf, outroom);
#endif
        if (result == INPUT_NOT_ENOUGH) {
#ifndef GLIBC_2_1_ICONV_BUG
            /* Conversion stopped due to an incomplete character at the
               end of the input buffer.  We just return # of bytes
               flushed.  (Shifting unconverted characters is done by
               buffered port routine) */
            info->ptr = outbuf;
#else
            /* See the above notes.  We always flush the output buffer
               here, so that we can avoid output buffer overrun. */
            Scm_Putz(info->buf, (int)(outbuf - info->buf), info->remote);
            info->ptr = info->buf;
#endif
            return (int)(len - inroom);
        } else if (result == OUTPUT_NOT_ENOUGH) {
            /* Output buffer got full.  Flush it, and continue
               conversion. */
            Scm_Putz(info->buf, (int)(outbuf - info->buf), info->remote);
            info->ptr = info->buf;
            continue;
        } else if (result == ILLEGAL_SEQUENCE) {
            /* it's likely that input contains invalid sequence.
               TODO: we should handle this case gracefully. */
            Scm_Error("invalid character sequence in the input stream");
            return 0;           /* dummy */
        } else {
#ifndef GLIBC_2_1_ICONV_BUG
            /* Conversion is done completely.  Update outptr. */
            info->ptr = outbuf;
#else
            /* See the above notes.  We always flush the output buffer here,
               so that we can avoid output buffer overrun. */
            Scm_Putz(info->buf, (int)(outbuf - info->buf), info->remote);
            info->ptr = info->buf;
#endif
            if (forcep && len - inroom != cnt) continue;
            return (int)(len - inroom);
        }
    }
}

ScmObj Scm_MakeOutputConversionPort(ScmPort *toPort,
                                    const char *toCode,
                                    const char *fromCode,
                                    int bufsiz, int ownerp)
{
    if (!SCM_OPORTP(toPort))
        Scm_Error("output port required, but got %S", toPort);

    if (bufsiz <= 0) bufsiz = DEFAULT_CONVERSION_BUFFER_SIZE;
    if (bufsiz <= MINIMUM_CONVERSION_BUFFER_SIZE) {
        bufsiz = MINIMUM_CONVERSION_BUFFER_SIZE;
    }

    ScmConvInfo *cinfo = jconv_open(toCode, fromCode);
    if (cinfo == NULL) {
        Scm_Error("conversion from code %s to code %s is not supported",
                  fromCode, toCode);
    }
    cinfo->remote = toPort;
    cinfo->ownerp = ownerp;
    cinfo->bufsiz = (bufsiz > 0)? bufsiz : DEFAULT_CONVERSION_BUFFER_SIZE;
    cinfo->remoteClosed = FALSE;
    cinfo->buf = SCM_NEW_ATOMIC2(char *, cinfo->bufsiz);
    cinfo->ptr = cinfo->buf;

    ScmPortBuffer bufrec;
    memset(&bufrec, 0, sizeof(bufrec));
    bufrec.size = cinfo->bufsiz;
    bufrec.buffer = SCM_NEW_ATOMIC2(char *, cinfo->bufsiz);
    bufrec.mode = SCM_PORT_BUFFER_FULL;
    bufrec.filler = NULL;
    bufrec.flusher = conv_output_flusher;
    bufrec.closer = conv_output_closer;
    bufrec.ready = conv_ready;
    bufrec.filenum = conv_fileno;
    bufrec.data = (void*)cinfo;

    ScmObj name = conv_name(SCM_PORT_OUTPUT, toPort, fromCode, toCode);
    return Scm_MakeBufferedPort(SCM_CLASS_PORT, name, SCM_PORT_OUTPUT, TRUE, &bufrec);
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

/*------------------------------------------------------------
 * UCS4 <-> internal character routine
 *
 * These routines are called when the literal character is given by
 * unicode notation (#\uXXXx, #\UXXXXXXXX or \uXXXX, \UXXXXXXXX inside
 * string), or unicode->char routine is called.
 * For this purpose, we keep two global conversion context.
 * Since internal encodings are stateless, we can reuse those
 * context, instead of calling jconv_open every time.
 */

static ScmChar ucstochar(int ucs4)
{
#if defined(GAUCHE_CHAR_ENCODING_UTF_8)
    return (ScmChar)ucs4;
#else  /*!GAUCHE_CHAR_ENCODING_UTF_8*/
    char inbuf[6], outbuf[6];
    const char *inb = inbuf;
    char *outb = outbuf;

    if (ucsconv.ucs2char == NULL) return SCM_CHAR_INVALID;
    size_t inroom = UCS2UTF_NBYTES(ucs4);
    size_t outroom = 6;
    jconv_ucs4_to_utf8(ucs4, inbuf);
    (void)SCM_INTERNAL_MUTEX_LOCK(ucsconv.mutex);
    size_t r = jconv(ucsconv.ucs2char, &inb, &inroom, &outb, &outroom);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ucsconv.mutex);
    if (r == INPUT_NOT_ENOUGH || r == OUTPUT_NOT_ENOUGH) {
        Scm_Error("can't convert UCS4 code %d to a character: implementation problem?", ucs4);
    }
    if (r == ILLEGAL_SEQUENCE) {
        return SCM_CHAR_INVALID;
    } else {
        ScmChar out;
        SCM_CHAR_GET(outbuf, out);
        return out;
    }
#endif /*!GAUCHE_CHAR_ENCODING_UTF_8*/
}

static int chartoucs(ScmChar ch)
{
#if defined(GAUCHE_CHAR_ENCODING_UTF_8)
    if (ch == SCM_CHAR_INVALID) return -1;
    return (int)ch;
#else  /*!GAUCHE_CHAR_ENCODING_UTF_8*/
    char inbuf[6], outbuf[6];
    const char *inb = inbuf;
    char *outb = outbuf;

    if (ch == SCM_CHAR_INVALID) return -1;
    if (ucsconv.char2ucs == NULL) return -1;
    size_t inroom = SCM_CHAR_NBYTES(ch);
    size_t outroom = 6;
    SCM_CHAR_PUT(inbuf, ch);
    (void)SCM_INTERNAL_MUTEX_LOCK(ucsconv.mutex);
    size_t r = jconv(ucsconv.char2ucs, &inb, &inroom, &outb, &outroom);
    (void)SCM_INTERNAL_MUTEX_UNLOCK(ucsconv.mutex);
    if (r == INPUT_NOT_ENOUGH || r == OUTPUT_NOT_ENOUGH) {
        Scm_Error("can't convert character %u to UCS4 code: implementation problem?", ch);
    }
    if (r == ILLEGAL_SEQUENCE) {
        return -1;
    } else {
        unsigned char *ucp = (unsigned char*)outbuf;
        if (ucp[0] < 0x80) return (int)ucp[0];
        if (ucp[0] < 0xe0) {
            return ((ucp[0]&0x1f)<<6) + (ucp[1]&0x3f);
        }
        if (ucp[0] < 0xf0) {
            return ((ucp[0]&0x0f)<<12)
                   + ((ucp[1]&0x3f)<<6)
                   + (ucp[2]&0x3f);
        }
        if (ucp[0] < 0xf8) {
            return ((ucp[0]&0x07)<<18)
                   + ((ucp[1]&0x3f)<<12)
                   + ((ucp[2]&0x3f)<<6)
                   + (ucp[3]&0x3f);
        }
        if (ucp[0] < 0xfc) {
            return ((ucp[0]&0x03)<<24)
                   + ((ucp[1]&0x3f)<<18)
                   + ((ucp[2]&0x3f)<<12)
                   + ((ucp[3]&0x3f)<<6)
                   + (ucp[4]&0x3f);
        }
        if (ucp[0] < 0xfe) {
            return ((ucp[0]&0x01)<<30)
                   + ((ucp[1]&0x3f)<<24)
                   + ((ucp[2]&0x3f)<<18)
                   + ((ucp[3]&0x3f)<<12)
                   + ((ucp[4]&0x3f)<<6)
                   + (ucp[5]&0x3f);
        }
        return -1;
    }
#endif /*!GAUCHE_CHAR_ENCODING_UTF_8*/
}

/*====================================================================
 * Initialization
 */
extern void Scm_Init_convaux(void);
extern void Scm_Init_convguess(void);

SCM_EXTENSION_ENTRY void Scm_Init_gauche__charconv(void)
{
    SCM_INIT_EXTENSION(gauche__charconv);
    ScmModule *mod = SCM_FIND_MODULE("gauche.charconv", SCM_FIND_MODULE_CREATE);
    guess.procs = NULL;
    (void)SCM_INTERNAL_MUTEX_INIT(guess.mutex);
#if   defined(GAUCHE_CHAR_ENCODING_UTF_8)
    ucsconv.ucs2char = ucsconv.char2ucs = NULL;
#elif defined(GAUCHE_CHAR_ENCODING_EUC_JP)
    ucsconv.ucs2char = jconv_open("EUCJP", "UTF-8");
    ucsconv.char2ucs = jconv_open("UTF-8", "EUCJP");
#elif defined(GAUCHE_CHAR_ENCODING_SJIS)
    ucsconv.ucs2char = jconv_open("SJIS", "UTF-8");
    ucsconv.char2ucs = jconv_open("UTF-8", "SJIS");
#else
    ucsconv.ucs2char = ucsconv.char2ucs = NULL;
#endif
    (void)SCM_INTERNAL_MUTEX_INIT(ucsconv.mutex);
    Scm_Init_convguess();
    Scm_Init_convaux();
    Scm__InstallCharconvHooks(ucstochar, chartoucs);
    Scm__InstallCodingAwarePortHook(coding_aware_conv);
}
