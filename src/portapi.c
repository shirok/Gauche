/*
 * portapi.c - port common API
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

/* This file is included _twice_ by port.c to define safe- and unsafe-
 * variant of port common APIs.  It is to minimize the overhead of
 * locking operations.
 *
 * The macro SHORTCUT allows 'safe' version to bypass lock/unlock
 * stuff by calling 'unsafe' version when the port is already locked by
 * the calling thread.
 */

/* [scratch and ungottern buffer]
 *   It is always possible to mix binary and character i/o for Gauche's
 *   ports.  To support peek operations in Scheme and 'unget' operations
 *   in C, we need to buffer at most one character or its equivalent
 *   byte sequence.   The 'ungotten' and 'scratch' fields are used for
 *   character and binary buffering, respectively.
 *   (This level of buffering is common to all input port types, and
 *   distinct from the buffering of 'buffered' (file) port type.)
 *
 *   'Ungotten' field keeps SCM_CHAR_INVALID if there's no buffered
 *   character.  Otherwise, its value is the buffered character.
 *   The number of bytes in the 'scratch' array is kept in 'scrcnt'
 *   field.  If 'scrcnt' field is not zero, there's data in the
 *   'scratch' array.
 *
 *   In no cases there should be data in both ungotten and scratch
 *   field.  The consistency is taken care of the routines defined here;
 *   no other routine should touch these buffering field.
 */

#ifdef SAFE_PORT_OP
#define VMDECL        ScmVM *vm = Scm_VM()
#define LOCK(p)       PORT_LOCK(p, vm)
#define UNLOCK(p)     PORT_UNLOCK(p)
#define SAFE_CALL(p, exp) PORT_SAFE_CALL(p, exp, /*no cleanup*/)
#define SHORTCUT(p, unsafe) \
  do { if (PORT_LOCKED(p, vm)) { unsafe; }} while (0)
#else
#define VMDECL        /*none*/
#define LOCK(p)       /*none*/
#define UNLOCK(p)     /*none*/
#define SAFE_CALL(p, exp) (exp)
#define SHORTCUT(p, unsafe) /* none */
#endif

/* Convenience macro */
#ifndef CLOSE_CHECK
#define CLOSE_CHECK(port)                                               \
    do {                                                                \
        if (SCM_PORT_CLOSED_P(port)) {                                  \
            UNLOCK(p);                                                  \
            Scm_PortError((port), SCM_PORT_ERROR_CLOSED,                \
                          "I/O attempted on closed port: %S", (port));  \
        }                                                               \
    } while (0)
#endif /* CLOSE_CHECK */

/* In the walk pass of multi-pass writing (see write.c), we set
   SCM_PORT_WALKING flag of the port.  Usually Scm_Write family recognizes
   the flag and suppress output.  However, in case if low-level port API
   is directly called during the walk pass, we just check the flag again.
*/
#ifndef WALKER_CHECK
#define WALKER_CHECK(port)                      \
    do {                                        \
        if (PORT_WALKER_P(port)) return;        \
    } while (0)
#endif /* WALKER_CHECK */

/*=================================================================
 * Putb
 */

#ifdef SAFE_PORT_OP
void Scm_Putb(ScmByte b, ScmPort *p)
#else
void Scm_PutbUnsafe(ScmByte b, ScmPort *p)
#endif
{
    VMDECL;
    SHORTCUT(p, Scm_PutbUnsafe(b, p); return);
    WALKER_CHECK(p);
    LOCK(p);
    CLOSE_CHECK(p);

    switch (SCM_PORT_TYPE(p)) {
    case SCM_PORT_FILE:
        if (p->src.buf.current >= p->src.buf.end) {
            SAFE_CALL(p, bufport_flush(p, (int)(p->src.buf.current - p->src.buf.buffer), FALSE));
        }
        SCM_ASSERT(p->src.buf.current < p->src.buf.end);
        *p->src.buf.current++ = b;
        if (SCM_PORT_BUFFER_MODE(p) == SCM_PORT_BUFFER_NONE) {
            SAFE_CALL(p, bufport_flush(p, 1, FALSE));
        }
        UNLOCK(p);
        break;
    case SCM_PORT_OSTR:
        SCM_DSTRING_PUTB(&p->src.ostr, b);
        UNLOCK(p);
        break;
    case SCM_PORT_PROC:
        SAFE_CALL(p, p->src.vt.Putb(b, p));
        UNLOCK(p);
        break;
    default:
        UNLOCK(p);
        Scm_PortError(p, SCM_PORT_ERROR_OUTPUT,
                      "bad port type for output: %S", p);
    }
}

/*=================================================================
 * Putc
 */

#ifdef SAFE_PORT_OP
void Scm_Putc(ScmChar c, ScmPort *p)
#else
void Scm_PutcUnsafe(ScmChar c, ScmPort *p)
#endif
{
    VMDECL;
    SHORTCUT(p, Scm_PutcUnsafe(c, p); return);
    WALKER_CHECK(p);
    LOCK(p);
    CLOSE_CHECK(p);

    switch (SCM_PORT_TYPE(p)) {
    case SCM_PORT_FILE: {
        int nb = SCM_CHAR_NBYTES(c);
        if (p->src.buf.current+nb > p->src.buf.end) {
            SAFE_CALL(p, bufport_flush(p, (int)(p->src.buf.current - p->src.buf.buffer), FALSE));
        }
        SCM_ASSERT(p->src.buf.current+nb <= p->src.buf.end);
        SCM_CHAR_PUT(p->src.buf.current, c);
        p->src.buf.current += nb;
        if (SCM_PORT_BUFFER_MODE(p) == SCM_PORT_BUFFER_LINE) {
            if (c == '\n') {
                SAFE_CALL(p, bufport_flush(p, nb, FALSE));
            }
        } else if (SCM_PORT_BUFFER_MODE(p) == SCM_PORT_BUFFER_NONE) {
            SAFE_CALL(p, bufport_flush(p, nb, FALSE));
        }
        UNLOCK(p);
        break;
    }
    case SCM_PORT_OSTR:
        SCM_DSTRING_PUTC(&p->src.ostr, c);
        UNLOCK(p);
        break;
    case SCM_PORT_PROC:
        SAFE_CALL(p, p->src.vt.Putc(c, p));
        UNLOCK(p);
        break;
    default:
        UNLOCK(p);
        Scm_PortError(p, SCM_PORT_ERROR_OUTPUT,
                      "bad port type for output: %S", p);
    }
}

/*=================================================================
 * Puts
 */

#ifdef SAFE_PORT_OP
void Scm_Puts(ScmString *s, ScmPort *p)
#else
void Scm_PutsUnsafe(ScmString *s, ScmPort *p)
#endif
{
    VMDECL;
    SHORTCUT(p, Scm_PutsUnsafe(s, p); return);
    WALKER_CHECK(p);
    LOCK(p);
    CLOSE_CHECK(p);

    switch (SCM_PORT_TYPE(p)) {
    case SCM_PORT_FILE: {
        u_int size;
        const char *ss = Scm_GetStringContent(s, &size, NULL, NULL);
        SAFE_CALL(p, bufport_write(p, ss, size));

        if (SCM_PORT_BUFFER_MODE(p) == SCM_PORT_BUFFER_LINE) {
            const char *cp = p->src.buf.current;
            while (cp-- > p->src.buf.buffer) {
                if (*cp == '\n') {
                    SAFE_CALL(p, bufport_flush(p, (int)(cp - p->src.buf.current), FALSE));
                    break;
                }
            }
        } else if (SCM_PORT_BUFFER_MODE(p) == SCM_PORT_BUFFER_NONE) {
            SAFE_CALL(p, bufport_flush(p, 0, TRUE));
        }
        UNLOCK(p);
        break;
    }
    case SCM_PORT_OSTR:
        Scm_DStringAdd(&p->src.ostr, s);
        UNLOCK(p);
        break;
    case SCM_PORT_PROC:
        SAFE_CALL(p, p->src.vt.Puts(s, p));
        UNLOCK(p);
        break;
    default:
        UNLOCK(p);
        Scm_PortError(p, SCM_PORT_ERROR_OUTPUT,
                      "bad port type for output: %S", p);
    }
}

/*=================================================================
 * Putz
 */

#ifdef SAFE_PORT_OP
void Scm_Putz(const char *s, int siz, ScmPort *p)
#else
void Scm_PutzUnsafe(const char *s, int siz, ScmPort *p)
#endif
{
    VMDECL;
    SHORTCUT(p, Scm_PutzUnsafe(s, siz, p); return);
    WALKER_CHECK(p);
    LOCK(p);
    CLOSE_CHECK(p);
    if (siz < 0) siz = (int)strlen(s);
    switch (SCM_PORT_TYPE(p)) {
    case SCM_PORT_FILE:
        SAFE_CALL(p, bufport_write(p, s, siz));
        if (SCM_PORT_BUFFER_MODE(p) == SCM_PORT_BUFFER_LINE) {
            const char *cp = p->src.buf.current;
            while (cp-- > p->src.buf.buffer) {
                if (*cp == '\n') {
                    SAFE_CALL(p, bufport_flush(p, (int)(cp - p->src.buf.current), FALSE));
                    break;
                }
            }
        } else if (SCM_PORT_BUFFER_MODE(p) == SCM_PORT_BUFFER_NONE) {
            SAFE_CALL(p, bufport_flush(p, 0, TRUE));
        }
        UNLOCK(p);
        break;
    case SCM_PORT_OSTR:
        Scm_DStringPutz(&p->src.ostr, s, siz);
        UNLOCK(p);
        break;
    case SCM_PORT_PROC:
        SAFE_CALL(p, p->src.vt.Putz(s, siz, p));
        UNLOCK(p);
        break;
    default:
        UNLOCK(p);
        Scm_PortError(p, SCM_PORT_ERROR_OUTPUT,
                      "bad port type for output: %S", p);
    }
}

/*=================================================================
 * Flush
 */

#ifdef SAFE_PORT_OP
void Scm_Flush(ScmPort *p)
#else
void Scm_FlushUnsafe(ScmPort *p)
#endif
{
    VMDECL;
    SHORTCUT(p, Scm_FlushUnsafe(p); return);
    WALKER_CHECK(p);
    LOCK(p);
    CLOSE_CHECK(p);
    switch (SCM_PORT_TYPE(p)) {
    case SCM_PORT_FILE:
        SAFE_CALL(p, bufport_flush(p, 0, TRUE));
        UNLOCK(p);
        break;
    case SCM_PORT_OSTR:
        UNLOCK(p);
        break;
    case SCM_PORT_PROC:
        SAFE_CALL(p, p->src.vt.Flush(p));
        UNLOCK(p);
        break;
    default:
        UNLOCK(p);
        Scm_PortError(p, SCM_PORT_ERROR_OUTPUT,
                      "bad port type for output: %S", p);
    }
}

/*=================================================================
 * Ungetc & PeekChar
 */

#ifdef SAFE_PORT_OP
void Scm_Ungetc(ScmChar c, ScmPort *p)
#else
void Scm_UngetcUnsafe(ScmChar c, ScmPort *p)
#endif
{
    VMDECL;
    SHORTCUT(p, Scm_UngetcUnsafe(c, p); return);
    LOCK(p);
    if (p->ungotten != SCM_CHAR_INVALID
        || p->scrcnt != 0) {
        Scm_PortError(p, SCM_PORT_ERROR_INPUT,
                      "pushback buffer overflow on port %S", p);
    }
    p->ungotten = c;
    UNLOCK(p);
}

#ifdef SAFE_PORT_OP
ScmChar Scm_Peekc(ScmPort *p)
#else
ScmChar Scm_PeekcUnsafe(ScmPort *p)
#endif
{
    VMDECL;
    SHORTCUT(p, return Scm_PeekcUnsafe(p));
    LOCK(p);
    ScmChar ch = p->ungotten;
    if (ch == SCM_CHAR_INVALID) {
        ch = Scm_GetcUnsafe(p);
        p->ungotten = ch;
    }
    UNLOCK(p);
    return ch;
}

/* At this moment we only allow one character to be 'ungotten',
   but we might change it in future, so this one returns a list. */
#ifdef SAFE_PORT_OP
ScmObj Scm_UngottenChars(ScmPort *p)
#else
ScmObj Scm_UngottenCharsUnsafe(ScmPort *p)
#endif
{
    VMDECL;
    SHORTCUT(p, return Scm_UngottenCharsUnsafe(p));
    LOCK(p);
    ScmChar ch = p->ungotten;
    UNLOCK(p);
    if (ch == SCM_CHAR_INVALID) {
        return SCM_NIL;
    } else {
        return SCM_LIST1(SCM_MAKE_CHAR(ch));
    }
}

/*=================================================================
 * Ungetb & PeekByte
 */

#ifdef SAFE_PORT_OP
void Scm_Ungetb(int b, ScmPort *p)
#else
void Scm_UngetbUnsafe(int b, ScmPort *p)
#endif
{
    VMDECL;
    SHORTCUT(p, Scm_UngetbUnsafe(b, p); return);
    LOCK(p);
    if (p->ungotten != SCM_CHAR_INVALID
        || p->scrcnt >= SCM_CHAR_MAX_BYTES) {
        Scm_PortError(p, SCM_PORT_ERROR_INPUT,
                      "pushback buffer overflow on port %S", p);
    }
    p->scratch[p->scrcnt++] = b;
    UNLOCK(p);
}

#ifdef SAFE_PORT_OP
int Scm_Peekb(ScmPort *p)
#else
int Scm_PeekbUnsafe(ScmPort *p)
#endif
{
    int b;
    VMDECL;
    SHORTCUT(p, return Scm_PeekbUnsafe(p));
    LOCK(p);
    if (p->scrcnt > 0) {
        b = (unsigned char)p->scratch[0];
    } else {
        SCM_GETB(b, p);
        if (b >= 0) {
            if (p->scrcnt > 0) {
                /* unshift scratch buffer */
                SCM_ASSERT(p->scrcnt < SCM_CHAR_MAX_BYTES);
                for (int i=p->scrcnt; i>0; i--) {
                    p->scratch[i] = p->scratch[i-1];
                }
                p->scratch[0] = b;
                p->scrcnt++;
            } else {
                p->scratch[0] = b;
                p->scrcnt = 1;
            }
        }
    }
    UNLOCK(p);
    return b;
}

#ifdef SAFE_PORT_OP
ScmObj Scm_UngottenBytes(ScmPort *p)
#else
ScmObj Scm_UngottenBytesUnsafe(ScmPort *p)
#endif
{
    VMDECL;
    SHORTCUT(p, return Scm_UngottenBytesUnsafe(p));
    char buf[SCM_CHAR_MAX_BYTES];
    LOCK(p);
    for (int i=0; i<p->scrcnt; i++) buf[i] = p->scratch[i];
    int n = p->scrcnt;
    UNLOCK(p);
    ScmObj h = SCM_NIL, t = SCM_NIL;
    for (int i=0; i<n; i++) {
        SCM_APPEND1(h, t, SCM_MAKE_INT((unsigned char)buf[i]));
    }
    return h;
}

/*=================================================================
 * Getb
 */

#ifndef SHIFT_SCRATCH  /* we need to define this only once */
#define SHIFT_SCRATCH

/* shift scratch buffer content */
static inline void shift_scratch(ScmPort *p, int off)
{
    for (u_int i=0; i<p->scrcnt; i++) {
        p->scratch[i] = p->scratch[i+off];
    }
}

/* handle the case that there's remaining data in the scratch buffer */
static int getb_scratch(ScmPort *p)
{
    int b = (unsigned char)p->scratch[0];
    p->scrcnt--;
    shift_scratch(p, 1);
    return b;
}

/* handle the case that there's an ungotten char */
static int getb_ungotten(ScmPort *p)
{
    SCM_CHAR_PUT(p->scratch, p->ungotten);
    p->scrcnt = SCM_CHAR_NBYTES(p->ungotten);
    p->ungotten = SCM_CHAR_INVALID;
    return getb_scratch(p);
}
#endif /*SHIFT_SCRATCH*/

/* Getb body */
#ifdef SAFE_PORT_OP
int Scm_Getb(ScmPort *p)
#else
int Scm_GetbUnsafe(ScmPort *p)
#endif
{
    int b = 0;
    VMDECL;
    SHORTCUT(p, return Scm_GetbUnsafe(p));
    LOCK(p);
    CLOSE_CHECK(p);

    /* check if there's "pushed back" stuff */
    if (p->scrcnt) {
        b = getb_scratch(p);
    } else if (p->ungotten != SCM_CHAR_INVALID) {
        b = getb_ungotten(p);
    } else {
        switch (SCM_PORT_TYPE(p)) {
        case SCM_PORT_FILE:
            if (p->src.buf.current >= p->src.buf.end) {
                int r = 0;
                SAFE_CALL(p, r = bufport_fill(p, 1, FALSE));
                if (r == 0) {
                    UNLOCK(p);
                    return EOF;
                }
            }
            b = (unsigned char)*p->src.buf.current++;
            break;
        case SCM_PORT_ISTR:
            if (p->src.istr.current >= p->src.istr.end) b = EOF;
            else b = (unsigned char)*p->src.istr.current++;
            break;
        case SCM_PORT_PROC:
            SAFE_CALL(p, b = p->src.vt.Getb(p));
            break;
        default:
            UNLOCK(p);
            Scm_PortError(p, SCM_PORT_ERROR_INPUT,
                          "bad port type for input: %S", p);
        }
        p->bytes++;
    }
    UNLOCK(p);
    return b;
}

/*=================================================================
 * Getc
 */

/* handle the case that there's data in scratch area */
#ifdef SAFE_PORT_OP
#define GETC_SCRATCH getc_scratch
static int getc_scratch(ScmPort *p)
#else
#define GETC_SCRATCH getc_scratch_unsafe
static int getc_scratch_unsafe(ScmPort *p)
#endif
{
    char tbuf[SCM_CHAR_MAX_BYTES];
    int nb = SCM_CHAR_NFOLLOWS(p->scratch[0]);
    int curr = p->scrcnt;

    memcpy(tbuf, p->scratch, curr);
    p->scrcnt = 0;
    for (int i=curr; i<=nb; i++) {
        int r = EOF;
        SAFE_CALL(p, r = Scm_Getb(p));
        if (r == EOF) {
            UNLOCK(p);
            Scm_PortError(p, SCM_PORT_ERROR_INPUT,
                          "encountered EOF in middle of a multibyte character from port %S", p);
        }
        tbuf[i] = (char)r;
    }
    int ch;
    SCM_CHAR_GET(tbuf, ch);
    return ch;
}

/* Getc body */
#ifdef SAFE_PORT_OP
int Scm_Getc(ScmPort *p)
#else
int Scm_GetcUnsafe(ScmPort *p)
#endif
{
    VMDECL;
    SHORTCUT(p, return Scm_GetcUnsafe(p));
    LOCK(p);
    CLOSE_CHECK(p);
    if (p->scrcnt > 0) {
        int r = GETC_SCRATCH(p);
        UNLOCK(p);
        return r;
    }
    if (p->ungotten != SCM_CHAR_INVALID) {
        int c = p->ungotten;
        p->ungotten = SCM_CHAR_INVALID;
        UNLOCK(p);
        return c;
    }

    switch (SCM_PORT_TYPE(p)) {
    case SCM_PORT_FILE: {
        int c = 0;
        if (p->src.buf.current >= p->src.buf.end) {
            int r = 0;
            SAFE_CALL(p, r = bufport_fill(p, 1, FALSE));
            if (r == 0) {
                UNLOCK(p);
                return EOF;
            }
        }
        int first = (unsigned char)*p->src.buf.current++;
        int nb = SCM_CHAR_NFOLLOWS(first);
        p->bytes++;
        if (nb > 0) {
            if (p->src.buf.current + nb > p->src.buf.end) {
                /* The buffer doesn't have enough bytes to consist a char.
                   move the incomplete char to the scratch buffer and try
                   to fetch the rest of the char. */
                int rest, filled = 0;
                p->scrcnt = (unsigned char)(p->src.buf.end - p->src.buf.current + 1);
                memcpy(p->scratch, p->src.buf.current-1, p->scrcnt);
                p->src.buf.current = p->src.buf.end;
                rest = nb + 1 - p->scrcnt;
                for (;;) {
                    SAFE_CALL(p, filled = bufport_fill(p, rest, FALSE));
                    if (filled <= 0) {
                        /* TODO: make this behavior customizable */
                        UNLOCK(p);
                        Scm_PortError(p, SCM_PORT_ERROR_INPUT,
                                      "encountered EOF in middle of a multibyte character from port %S", p);
                    }
                    if (filled >= rest) {
                        memcpy(p->scratch+p->scrcnt, p->src.buf.current, rest);
                        p->scrcnt += rest;
                        p->src.buf.current += rest;
                        break;
                    } else {
                        memcpy(p->scratch+p->scrcnt, p->src.buf.current, filled);
                        p->scrcnt += filled;
                        p->src.buf.current = p->src.buf.end;
                        rest -= filled;
                    }
                }
                SCM_CHAR_GET(p->scratch, c);
                p->scrcnt = 0;
            } else {
                SCM_CHAR_GET(p->src.buf.current-1, c);
                p->src.buf.current += nb;
            }
            p->bytes += nb;
        } else {
            c = first;
            if (c == '\n') p->line++;
        }
        UNLOCK(p);
        return c;
    }
    case SCM_PORT_ISTR: {
        if (p->src.istr.current >= p->src.istr.end) {
            UNLOCK(p);
            return EOF;
        }
        int c = 0;
        int first = (unsigned char)*p->src.istr.current++;
        int nb = SCM_CHAR_NFOLLOWS(first);
        p->bytes++;
        if (nb > 0) {
            if (p->src.istr.current + nb > p->src.istr.end) {
                /* TODO: make this behavior customizable */
                UNLOCK(p);
                Scm_PortError(p, SCM_PORT_ERROR_INPUT,
                              "encountered EOF in middle of a multibyte character from port %S", p);
            }
            SCM_CHAR_GET(p->src.istr.current-1, c);
            p->src.istr.current += nb;
            p->bytes += nb;
        } else {
            c = first;
            if (c == '\n') p->line++;
        }
        UNLOCK(p);
        return c;
    }
    case SCM_PORT_PROC: {
        int c = 0;
        SAFE_CALL(p, c = p->src.vt.Getc(p));
        if (c == '\n') p->line++;
        UNLOCK(p);
        return c;
    }
    default:
        UNLOCK(p);
        Scm_PortError(p, SCM_PORT_ERROR_INPUT, "bad port type for input: %S", p);
    }
    return 0;/*dummy*/
}

#undef GETC_SCRATCH

/*=================================================================
 * Getz - block read.
 *   If the buffering mode is BUFFER_FULL, this reads BUFLEN bytes
 *   unless it reaches EOF.  Otherwise, this reads less than BUFLEN
 *   if the data is not immediately available.
 */

#ifdef SAFE_PORT_OP
#define GETZ_SCRATCH getz_scratch
static int getz_scratch(char *buf, int buflen, ScmPort *p)
#else
#define GETZ_SCRATCH getz_scratch_unsafe
static int getz_scratch_unsafe(char *buf, int buflen, ScmPort *p)
#endif
{
    if (p->scrcnt >= (u_int)buflen) {
        memcpy(buf, p->scratch, buflen);
        p->scrcnt -= buflen;
        shift_scratch(p, buflen);
        return buflen;
    } else {
        memcpy(buf, p->scratch, p->scrcnt);
        int i = p->scrcnt;
        p->scrcnt = 0;
        int n = 0;
        SAFE_CALL(p, n = Scm_Getz(buf+i, buflen-i, p));
        return i + n;
    }
}

#ifndef GETZ_ISTR               /* common part */
#define GETZ_ISTR getz_istr
static int getz_istr(ScmPort *p, char *buf, int buflen)
{
    if (p->src.istr.current + buflen >= p->src.istr.end) {
        if (p->src.istr.current >= p->src.istr.end) return EOF;
        int siz = (int)(p->src.istr.end - p->src.istr.current);
        memcpy(buf, p->src.istr.current, siz);
        p->src.istr.current = p->src.istr.end;
        return siz;
    } else {
        memcpy(buf, p->src.istr.current, buflen);
        p->src.istr.current += buflen;
        return buflen;
    }
}
#endif /*!GETZ_ISTR*/

#ifdef SAFE_PORT_OP
int Scm_Getz(char *buf, int buflen, ScmPort *p)
#else
int Scm_GetzUnsafe(char *buf, int buflen, ScmPort *p)
#endif
{
    VMDECL;
    SHORTCUT(p, return Scm_GetzUnsafe(buf, buflen, p));
    LOCK(p);
    CLOSE_CHECK(p);

    if (p->scrcnt) {
        int r = GETZ_SCRATCH(buf, buflen, p);
        UNLOCK(p);
        return r;
    }
    if (p->ungotten != SCM_CHAR_INVALID) {
        p->scrcnt = SCM_CHAR_NBYTES(p->ungotten);
        SCM_CHAR_PUT(p->scratch, p->ungotten);
        p->ungotten = SCM_CHAR_INVALID;
        int r = GETZ_SCRATCH(buf, buflen, p);
        UNLOCK(p);
        return r;
    }

    switch (SCM_PORT_TYPE(p)) {
    case SCM_PORT_FILE: {
        int siz = 0;
        SAFE_CALL(p, siz = bufport_read(p, buf, buflen));
        p->bytes += siz;
        UNLOCK(p);
        if (siz == 0) return EOF;
        else return siz;
    }
    case SCM_PORT_ISTR: {
        int r = GETZ_ISTR(p, buf, buflen);
        p->bytes += r;
        UNLOCK(p);
        return r;
    }
    case SCM_PORT_PROC: {
        int r = 0;
        SAFE_CALL(p, r = p->src.vt.Getz(buf, buflen, p));
        p->bytes += r;
        UNLOCK(p);
        return r;
    }
    default:
        UNLOCK(p);
        Scm_PortError(p, SCM_PORT_ERROR_INPUT, "bad port type for input: %S", p);
    }
    return -1;                  /* dummy */
}

#undef GETZ_SCRATCH

/*=================================================================
 * ReadLine
 *   Reads up to EOL or EOF.
 */

/* Auxiliary procedures */

/* NB: it may be further optimized by scanning the contents of buffer
   when the port is a buffered port or an input string, which allows
   us to avoid mb->wc->mb conversion.   See port.c, v 1.69 for some
   attempt to do so.  The problem there is that if I have to take
   into account the cases of the ungotten char and the scratch buffer,
   code becomes ugly.  There might be some better approach. */

#ifndef READLINE_AUX
#define READLINE_AUX
/* Assumes the port is locked, and the caller takes care of unlocking
   even if an error is signalled within this body */
/* NB: this routine reads bytes, not chars.  It allows to readline
   from a port in unknown character encoding (e.g. reading the first
   line of xml doc to find out charset parameter). */
ScmObj readline_body(ScmPort *p)
{
    ScmDString ds;

    Scm_DStringInit(&ds);
    int b1 = Scm_GetbUnsafe(p);
    if (b1 == EOF) return SCM_EOF;
    for (;;) {
        if (b1 == EOF) return Scm_DStringGet(&ds, 0);
        if (b1 == '\n') break;
        if (b1 == '\r') {
            int b2 = Scm_GetbUnsafe(p);
            if (b2 == EOF || b2 == '\n') break;
            Scm_UngetbUnsafe(b2, p);
            break;
        }
        SCM_DSTRING_PUTB(&ds, b1);
        b1 = Scm_GetbUnsafe(p);
    }
    p->line++;
    return Scm_DStringGet(&ds, 0);
}
#endif /* READLINE_AUX */

#ifdef SAFE_PORT_OP
ScmObj Scm_ReadLine(ScmPort *p)
#else
ScmObj Scm_ReadLineUnsafe(ScmPort *p)
#endif
{
    ScmObj r = SCM_UNDEFINED;
    VMDECL;
    SHORTCUT(p, return Scm_ReadLineUnsafe(p));

    LOCK(p);
    SAFE_CALL(p, r = readline_body(p));
    UNLOCK(p);
    return r;
}

/*=================================================================
 * ByteReady
 */

#ifdef SAFE_PORT_OP
int Scm_ByteReady(ScmPort *p)
#else
int Scm_ByteReadyUnsafe(ScmPort *p)
#endif
{
    int r = 0;
    VMDECL;
    SHORTCUT(p, return Scm_ByteReadyUnsafe(p));
    if (!SCM_IPORTP(p)) Scm_Error("input port required, but got %S", p);
    LOCK(p);
    if (p->ungotten != SCM_CHAR_INVALID
        || p->scrcnt > 0) {
        r = TRUE;
    } else {
        switch (SCM_PORT_TYPE(p)) {
        case SCM_PORT_FILE:
            if (p->src.buf.current < p->src.buf.end) r = TRUE;
            else if (p->src.buf.ready == NULL) r = TRUE;
            else {
                SAFE_CALL(p, r = (p->src.buf.ready(p) != SCM_FD_WOULDBLOCK));
            }
            break;
        case SCM_PORT_PROC:
            SAFE_CALL(p, r = p->src.vt.Ready(p, FALSE));
            break;
        default:
            r = TRUE;
        }
    }
    UNLOCK(p);
    return r;
}

/*=================================================================
 * CharReady
 */

#ifdef SAFE_PORT_OP
int Scm_CharReady(ScmPort *p)
#else
int Scm_CharReadyUnsafe(ScmPort *p)
#endif
{
    int r = 0;
    VMDECL;
    SHORTCUT(p, return Scm_CharReadyUnsafe(p));
    if (!SCM_IPORTP(p)) Scm_Error("input port required, but got %S", p);
    LOCK(p);
    if (p->ungotten != SCM_CHAR_INVALID) r = TRUE;
    else {
        switch (SCM_PORT_TYPE(p)) {
        case SCM_PORT_FILE:
            if (p->src.buf.current < p->src.buf.end) r = TRUE;
            else if (p->src.buf.ready == NULL) r = TRUE;
            else {
                SAFE_CALL(p, r = (p->src.buf.ready(p) != SCM_FD_WOULDBLOCK));
            }
            break;
        case SCM_PORT_PROC:
            SAFE_CALL(p, r = p->src.vt.Ready(p, TRUE));
            break;
        default:
            r = TRUE;
        }
    }
    UNLOCK(p);
    return r;
}

/*=================================================================
 * PortSeek
 */

#ifndef SEEK_ISTR               /* common part */
#define SEEK_ISTR seek_istr
static off_t seek_istr(ScmPort *p, off_t o, int whence, int nomove)
{
    off_t r;
    if (nomove) {
        r = (off_t)(p->src.istr.current - p->src.istr.start);
    } else {
        long z = (long)o;
        if (whence == SEEK_CUR) {
            z += (long)(p->src.istr.current - p->src.istr.start);
        } else if (whence == SEEK_END) {
            z += (long)(p->src.istr.end - p->src.istr.start);
        }
        if (z < 0 || z > (long)(p->src.istr.end - p->src.istr.start)) {
            r = (off_t)-1;
        } else {
            p->src.istr.current = p->src.istr.start + z;
            r = (off_t)(p->src.istr.current - p->src.istr.start);
        }
        p->ungotten = SCM_CHAR_INVALID;
    }
    return r;
}
#endif /*SEEK_ISTR*/

#ifdef SAFE_PORT_OP
ScmObj Scm_PortSeek(ScmPort *p, ScmObj off, int whence)
#else
ScmObj Scm_PortSeekUnsafe(ScmPort *p, ScmObj off, int whence)
#endif
{
    off_t r = (off_t)-1, o = Scm_IntegerToOffset(off);
    int nomove = (whence == SEEK_CUR && o == 0);
    VMDECL;
    SHORTCUT(p, return Scm_PortSeekUnsafe(p, off, whence));
    if (SCM_PORT_CLOSED_P(p)) {
        Scm_PortError(p, SCM_PORT_ERROR_CLOSED,
                      "attempt to seek on closed port: %S", p);
    }
    LOCK(p);
    switch (SCM_PORT_TYPE(p)) {
    case SCM_PORT_FILE:
        /* NB: we might be able to skip calling seeker if we keep the
           # of bytes read or write so far, but such count may be off
           when the port has been experienced an error condition. */
        /* NB: the following doesn't work if we have bidirectional port.
           In such case we need to keep whether the last call of buffer
           handling routine was input or output. */
        if (!p->src.buf.seeker) break;
        if (nomove) {
            SAFE_CALL(p, r = p->src.buf.seeker(p, 0, SEEK_CUR));
            if (SCM_PORT_DIR(p)&SCM_PORT_INPUT) {
                r -= (off_t)(p->src.buf.end - p->src.buf.current);
            } else {
                r += (off_t)(p->src.buf.current - p->src.buf.buffer);
            }
        } else {
            /* NB: possible optimization: the specified position is within
               the current buffer, we can avoid calling seeker. */
            if (SCM_PORT_DIR(p)&SCM_PORT_INPUT) {
                char *c = p->src.buf.current; /* save current ptr */
                if (whence == SEEK_CUR) {
                    o -= (off_t)(p->src.buf.end - c);
                }
                p->src.buf.current = p->src.buf.end; /* invalidate buffer */
                SAFE_CALL(p, r = p->src.buf.seeker(p, o, whence));
                if (r == (off_t)-1) {
                    /* This may happend if seeker somehow gave up */
                    p->src.buf.current = c;
                }
            } else {
                SAFE_CALL(p, bufport_flush(p, 0, TRUE));
                SAFE_CALL(p, r = p->src.buf.seeker(p, o, whence));
            }
            /* Invalidate ungotten char */
            p->ungotten = SCM_CHAR_INVALID;
        }
        break;
    case SCM_PORT_ISTR:
        r = SEEK_ISTR(p, o, whence, nomove);
        break;
    case SCM_PORT_OSTR:
        if (nomove) {
            r = (off_t)Scm_DStringSize(&(p->src.ostr));
        } else {
            /* Not supported yet */
            r = (off_t)-1;
        }
        break;
    case SCM_PORT_PROC:
        if (p->src.vt.Seek) {
            SAFE_CALL(p, r = p->src.vt.Seek(p, o, whence));
        }
        break;
    }
    UNLOCK(p);
    if (r == (off_t)-1) return SCM_FALSE;
    else return Scm_OffsetToInteger(r);
}

/*=================================================================
 * Port Attributes
 *
 * Port attributes are stored in alist.  Each entry is either one
 * of the following form:
 *  (key value)          Just a value
 *  (key getter setter)  Procedurally handled value.  Getter will be
 *                       called as (getter port [fallback]), and Setter
 *                       will be called as (setter port value).
 *                       Setter can be #f if the attr is read-only.
 *                       Port is locked while getter and setter is called.
 *
 * The latter type of attribute can be created by Scm_PortAttrCreate.
 */

#ifdef SAFE_PORT_OP
ScmObj Scm_PortAttrGet(ScmPort *p, ScmObj key, ScmObj fallback)
#else
ScmObj Scm_PortAttrGetUnsafe(ScmPort *p, ScmObj key, ScmObj fallback)
#endif
{
    ScmObj r = SCM_UNBOUND;
    VMDECL;
    SHORTCUT(p, return Scm_PortAttrGetUnsafe(p, key, fallback););
    LOCK(p);
    ScmObj v = Scm_Assq(key, p->attrs);
    if (SCM_PAIRP(v)) {
        SCM_ASSERT(SCM_PAIRP(SCM_CDR(v)));
        if (SCM_PAIRP(SCM_CDDR(v))) {
            /* procedural */
            ScmObj getter = SCM_CADR(v);
            if (SCM_UNBOUNDP(fallback)) {
                SAFE_CALL(p, r = Scm_ApplyRec1(getter, SCM_OBJ(p)));
            } else {
                SAFE_CALL(p, r = Scm_ApplyRec2(getter, SCM_OBJ(p), fallback));
            }
        } else {
            r = SCM_CADR(v);
        }
    } else {
        r = fallback;
    }
    UNLOCK(p);

    if (SCM_UNBOUNDP(r)) {
        Scm_Error("No port attribute for key %S in port %S", key, SCM_OBJ(p));
    }
    return r;
}


#ifdef SAFE_PORT_OP
ScmObj Scm_PortAttrSet(ScmPort *p, ScmObj key, ScmObj val)
#else
ScmObj Scm_PortAttrSetUnsafe(ScmPort *p, ScmObj key, ScmObj val)
#endif
{
    int err_readonly = FALSE;
    int exists = FALSE;
    VMDECL;
    SHORTCUT(p, return Scm_PortAttrSetUnsafe(p, key, val););
    LOCK(p);
    ScmObj v = Scm_Assq(key, p->attrs);
    if (SCM_PAIRP(v)) {
        SCM_ASSERT(SCM_PAIRP(SCM_CDR(v)));
        exists = TRUE;
        if (SCM_PAIRP(SCM_CDDR(v))) {
            /* procedural */
            ScmObj setter = SCM_CAR(SCM_CDDR(v));
            if (SCM_FALSEP(setter)) {
                err_readonly = TRUE;
            } else {
                SAFE_CALL(p, Scm_ApplyRec2(setter, SCM_OBJ(p), val));
            }
        } else {
            SCM_SET_CAR(SCM_CDR(v), val);
        }
    } else {
        p->attrs = Scm_Cons(SCM_LIST2(key, val), p->attrs);
    }
    UNLOCK(p);
    if (err_readonly) {
        Scm_Error("Port attribute %A is read-only in port: %S",
                  key, SCM_OBJ(p));
    }
    return SCM_MAKE_BOOL(exists);
}

#ifdef SAFE_PORT_OP
ScmObj Scm_PortAttrCreate(ScmPort *p, ScmObj key, ScmObj get, ScmObj set)
#else
ScmObj Scm_PortAttrCreateUnsafe(ScmPort *p, ScmObj key, ScmObj get, ScmObj set)
#endif
{
    int err_exists = FALSE;
    VMDECL;
    SHORTCUT(p, return Scm_PortAttrCreateUnsafe(p, key, get, set););

    /* If get == #f, we create an ordinary attr entry.  Otherwise,
       we create a procedural entry. */
    ScmObj entry = (SCM_FALSEP(get)
                    ? SCM_LIST2(key, SCM_FALSE)
                    : SCM_LIST3(key, get, set));
    LOCK(p);
    ScmObj v = Scm_Assq(key, p->attrs);
    if (SCM_FALSEP(v)) {
        p->attrs = Scm_Cons(entry, p->attrs);
    } else {
        err_exists = TRUE;
    }
    UNLOCK(p);
    if (err_exists) {
        Scm_Error("Couldn't create port attribute %A in %S: Named attribute already exists.",
                  key, SCM_OBJ(p));
    }
    return SCM_UNDEFINED;       /* we may return more useful info in future */
}

#ifdef SAFE_PORT_OP
ScmObj Scm_PortAttrDelete(ScmPort *p, ScmObj key)
#else
ScmObj Scm_PortAttrDeleteUnsafe(ScmPort *p, ScmObj key)
#endif
{
    VMDECL;
    SHORTCUT(p, return Scm_PortAttrDeleteUnsafe(p, key););
    LOCK(p);
    p->attrs = Scm_AssocDelete(key, p->attrs, SCM_CMP_EQ);
    UNLOCK(p);
    return SCM_UNDEFINED;       /* we may return more useful info in future */
}

#ifdef SAFE_PORT_OP
ScmObj Scm_PortAttrs(ScmPort *p)
#else
ScmObj Scm_PortAttrsUnsafe(ScmPort *p)
#endif
{
    ScmObj h = SCM_NIL, t = SCM_NIL;
    VMDECL;
    SHORTCUT(p, return Scm_PortAttrsUnsafe(p););
    LOCK(p);
    ScmObj cp;
    SCM_FOR_EACH(cp, p->attrs) {
        ScmObj k = SCM_CAAR(cp);
        ScmObj v = Scm_PortAttrGetUnsafe(p, k, SCM_UNBOUND);
        SCM_APPEND1(h, t, Scm_Cons(k, v));
    }
    UNLOCK(p);
    return h;
}


#undef VMDECL
#undef LOCK
#undef UNLOCK
#undef SAFE_CALL
#undef SHORTCUT
#undef CLOSE_CHECK
