/*
 * vport.c - 'virtual port'
 *
 *   Copyright (c) 2004 Shiro Kawai, All rights reserved.
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
 *
 *  $Id: vport.c,v 1.4 2004-10-22 05:59:14 shirok Exp $
 */

#include "gauche/vport.h"
#include <gauche/class.h>
#include <gauche/extend.h>

/*================================================================
 * <virtual-port>
 */

static ScmObj vport_allocate(ScmClass *klass, ScmObj initargs);
static void   vport_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);

SCM_DEFINE_BUILTIN_CLASS(Scm_VirtualPortClass,
                         vport_print, NULL, NULL,
                         vport_allocate, NULL);

/*
 * Scheme handlers.  They are visible from Scheme as instance slots.
 * Any of these slots can be #f - if possible, the vport tries to fulfill
 * the feature by using alternative methods.  If not possible, it raises
 * 'not supported' error.
 */
typedef struct vport_rec {
    ScmObj getb_proc;           /* () -> Maybe Byte   */
    ScmObj getc_proc;           /* () -> Maybe Char   */
    ScmObj gets_proc;           /* (Size) -> Maybe String */
    ScmObj getline_proc;        /* () -> Maybe String */
    ScmObj ready_proc;          /* (Bool) -> Bool */
    ScmObj putb_proc;           /* (Byte) -> () */
    ScmObj putc_proc;           /* (Char) -> () */
    ScmObj puts_proc;           /* (String) -> () */
    ScmObj flush_proc;          /* () -> () */
    ScmObj close_proc;          /* () -> () */
    ScmObj seek_proc;           /* (Offset, Whence) -> Offset */
} vport;

/*------------------------------------------------------------
 * Vport Getb
 */
static int vport_getb(ScmPort *p)
{
    vport *data = (vport*)p->src.vt.data;
    SCM_ASSERT(data != NULL);

    if (SCM_FALSEP(data->getb_proc)) {
        /* If the port doesn't have get-byte method, use get-char
           if possible. */
        ScmObj ch;
        ScmChar c;
        char buf[SCM_CHAR_MAX_BYTES];
        int nb, i;

        if (SCM_FALSEP(data->getc_proc)) return EOF;
        ch = Scm_Apply(data->getc_proc, SCM_NIL);
        if (!SCM_CHARP(ch)) return EOF;

        c = SCM_CHAR_VALUE(ch);
        nb = SCM_CHAR_NBYTES(c);
        SCM_CHAR_PUT(buf, c);
        
        for (i=1; i<nb; i++) {
            /* pushback for later use.  this isn't very efficient;
               if efficiency becomes a problem, we need another API
               to pushback multiple bytes. */
            Scm_UngetbUnsafe(buf[i], p); 
        }
        return buf[0];
    } else {
        ScmObj b = Scm_Apply(data->getb_proc, SCM_NIL);
        if (!SCM_INTP(b)) return EOF;
        return (SCM_INT_VALUE(b) & 0xff);
    }
}

/*------------------------------------------------------------
 * Vport Getc
 */
static int vport_getc(ScmPort *p)
{
    vport *data = (vport*)p->src.vt.data;
    SCM_ASSERT(data != NULL);

    if (SCM_FALSEP(data->getc_proc)) {
        /* If the port doesn't have get-char method, try get-byte */
        ScmObj b, n, i;
        ScmChar ch;
        char buf[SCM_CHAR_MAX_BYTES];

        if (SCM_FALSEP(data->getb_proc)) return EOF;
        b = Scm_Apply(data->getb_proc, SCM_NIL);
        if (!SCM_INTP(b)) return EOF;
        buf[0] = SCM_INT_VALUE(b);
        n = SCM_CHAR_NFOLLOWS(p->scratch[0]);
        for (i=0; i<n; i++) {
            b = Scm_Apply(data->getb_proc, SCM_NIL);
            if (!SCM_INTP(b)) {
                /* TODO: should raise an exception? */
                return EOF;
            }
            buf[i+1] = SCM_INT_VALUE(b);
        }
        SCM_CHAR_GET(buf, ch);
        return ch;
    } else {
        ScmObj ch = Scm_Apply(data->getc_proc, SCM_NIL);
        if (!SCM_CHARP(ch)) return EOF;
        return SCM_CHAR_VALUE(ch);
    }
}

/*------------------------------------------------------------
 * Vport Gets
 */
static int vport_getz(char *buf, int buflen, ScmPort *p)
{
    vport *data = (vport*)p->src.vt.data;
    SCM_ASSERT(data != NULL);

    if (!SCM_FALSEP(data->gets_proc)) {
        int size;
        ScmObj s = Scm_Apply(data->gets_proc, SCM_LIST1(SCM_MAKE_INT(buflen)));
        if (!SCM_STRINGP(s)) return EOF;
        size = SCM_STRING_SIZE(s);
        if (size > buflen) {
            /* NB: should raise an exception? */
            memcpy(buf, SCM_STRING_START(s), buflen);
            return buflen;
        } else {
            memcpy(buf, SCM_STRIng_START(s), size);
            return size;
        }
    } else {
        int byte, i;
        for (i=0; i<buflen; i++) {
            byte = vport_getb(p);
            if (byte == EOF) break;
            buf[i] = byte;
        }
        if (i==0) return EOF;
        else return i;
    }
}

/*------------------------------------------------------------
 * Vport Getline
 */
static ScmObj vport_getline(ScmPort *p)
{
    vport *data = (vport*)p->src.vt.data;
    SCM_ASSERT(data != NULL);

    if (!SCM_FALSEP(data->getline_proc)) {
        int size;
        ScmObj s = Scm_Apply(data->getline_proc, SCM_NIL);
        if (!SCM_STRINGP(s)) return EOF;
        else return s;
    } else {
        Scm_ReadLineUnsafe(p);
    }
}

/*------------------------------------------------------------
 * Vport Ready
 */
static int vport_ready(ScmPort *p, int charp)
{
    vport *data = (vport*)p->src.vt.data;
    SCM_ASSERT(data != NULL);

    if (!SCM_FALSEP(data->ready_proc)) {
        int size;
        ScmObj s = Scm_Apply(data->ready_proc,
                             SCM_LIST1(SCM_MAKE_BOOL(charp)));
        return !SCM_FALSEP(s);
    } else {
        /* if no method is given, always return #t */
        return TRUE;
    }
}

/*------------------------------------------------------------
 * Vport putb
 */
static int vport_putb(ScmByte b, ScmPort *p)
{
    vport *data = (vport*)p->src.vt.data;
    ScmObj r;
    SCM_ASSERT(data != NULL);

    if (!SCM_FALSEP(data->putb_proc)) {
        Scm_PortError(p, SCM_PORT_ERROR_UNIT,
                      "cannot perform binary output to the port %S", p);
    }
    r = Scm_Apply(data->putb_proc, SCM_LIST1(SCM_MAKE_INT(b)));
    if (!SCM_INTP(r)) return 0;
    else return SCM_INT_VALUE(r);
}

/*------------------------------------------------------------
 * Vport putc
 */
static int vport_putc(ScmChar c, ScmPort *p)
{
    vport *data = (vport*)p->src.vt.data;
    ScmObj r;
    SCM_ASSERT(data != NULL);

    if (!SCM_FALSEP(data->putc_proc)) {
        if (!SCM_FALSEP(data->putb_proc)) {
            unsigned char buf[SCM_CHAR_MAX_BYTES];
            int i, n=SCM_CHAR_NBYTES(c);
            SCM_CHAR_PUT(buf, c);
            for (i=0; i<n; i++) {
                r = Scm_Apply(data->putb_proc, SCM_LIST1(SCM_MAKE_INT(buf[i])));
            }
        }
    } else {
        r = Scm_Apply(data->putc_proc, SCM_LIST1(SCM_MAKE_CHAR(c)));
    }
    if (!SCM_INTP(r)) return 0;
    else return SCM_INT_VALUE(r);
}



/*================================================================
 * <buffered-port>
 */

static ScmObj bport_allocate(ScmClass *klass, ScmObj initargs);
static void   bport_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);

SCM_DEFINE_BUILTIN_CLASS(Scm_BufferedPortClass,
                         bport_print, NULL, NULL,
                         bport_allocate, NULL);



