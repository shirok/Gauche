/*
 * tls.c - tls secure connection interface
 *
 *   Copyright (c) 2011 Kirill Zorin <k.zorin@me.com>
 *   Copyright (c) 2018 YOKOTA Hiroshi
 *   Copyright (c) 2018  Shiro Kawai  <shiro@acm.org>
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

#include "gauche-tls.h"
#include <gauche/extend.h>

/*
 * Class
 */

static void tls_print(ScmObj obj, ScmPort* port, ScmWriteContext* ctx);

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_TLSClass, tls_print);

static ScmClass *tlsclass_cpa[] = {
    SCM_CLASS_STATIC_PTR(Scm_TLSClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

#if defined(GAUCHE_USE_AXTLS)
static ScmObj ax_allocate(ScmClass *klass, ScmObj initargs);

SCM_DEFINE_BUILTIN_CLASS(Scm_AxTLSClass, tls_print, NULL,
                         NULL, ax_allocate, tlsclass_cpa);
#endif /*GAUCHE_USE_AXTLS*/

static void tls_print(ScmObj obj, ScmPort* port, ScmWriteContext* ctx)
{
    Scm_Printf(port, "#<%A", Scm_ShortClassName(SCM_CLASS_OF(obj)));
    /* at the moment there's not much to print, so we leave this hole
       for future development. */
    Scm_Printf(port, ">");
}

/*
 * Global stuff
 */

static ScmParameterLoc ca_bundle_path;
static ScmParameterLoc default_tls_class;
static ScmObj k_options;
static ScmObj k_num_sessions;

/*
 * Common operations
 */

ScmObj Scm_MakeTLS(ScmObj initargs)
{
    ScmObj klass = Scm_ParameterRef(Scm_VM(), &default_tls_class);
    if (!SCM_CLASSP(klass) || !Scm_SubtypeP(SCM_CLASS(klass), &Scm_TLSClass)) {
        Scm_Error("default-tls-class needs to be a subclass of <tls>, "
                  "but got: %S", klass);
    }
    return Scm_Allocate(SCM_CLASS(klass), initargs);
}


/* Explicitly destroys the context.  The axtls context holds open fd for
   /dev/urandom, and sometimes gc isn't called early enough before we use
   up all fds, so explicit destruction is recommended whenever possible. */
ScmObj Scm_TLSDestroy(ScmTLS* t)
{
    if (t->finalize) t->finalize(SCM_OBJ(t), NULL);
    return SCM_TRUE;
}

ScmObj Scm_TLSClose(ScmTLS* t)
{
    /* Extra parenthesis to keep close() from being macro-expanded by
       windows compatibility macro. */
    return (t->close)(t);
}

ScmObj Scm_TLSLoadObject(ScmTLS* t, ScmObj obj_type,
                         const char *filename, const char *password)
{
    return t->loadObject(t, obj_type, filename, password);
}

ScmObj Scm_TLSConnect(ScmTLS* t, int fd)
{
    return t->connect(t, fd);
}

ScmObj Scm_TLSAccept(ScmTLS* t, int fd)
{
    return t->accept(t, fd);
}

ScmObj Scm_TLSRead(ScmTLS* t)
{
    /* Extra parenthesis to keep read() from being macro-expanded by
       windows compatibility macro. */
    return (t->read)(t);
}

ScmObj Scm_TLSWrite(ScmTLS* t, ScmObj msg)
{
    /* Extra parenthesis to keep write() from being macro-expanded by
       windows compatibility macro. */
    return (t->write)(t, msg);
}

ScmObj Scm_TLSInputPort(ScmTLS* t)
{
    return t->in_port;
}

ScmObj Scm_TLSOutputPort(ScmTLS* t)
{
    return t->out_port;
}

ScmObj Scm_TLSInputPortSet(ScmTLS* t, ScmObj port)
{
    t->in_port = port;
}

ScmObj Scm_TLSOutputPortSet(ScmTLS* t, ScmObj port)
{
    t->out_port = port;
}

void Scm_Init_tls(ScmModule *mod)
{
    Scm_InitStaticClass(&Scm_TLSClass, "<tls>", mod, NULL, 0);
#if defined(GAUCHE_USE_AXTLS)
    Scm_InitStaticClass(&Scm_AxTLSClass, "<ax-tls>", mod, NULL, 0);
#endif
    Scm_DefinePrimitiveParameter(mod, "default-tls-class",
#if defined(GAUCHE_USE_AXTLS)
                                 SCM_OBJ(&Scm_AxTLSClass),
#else
                                 SCM_FALSE,
#endif
                                 &default_tls_class);
    Scm_DefinePrimitiveParameter(mod, "tls-ca-bundle-path",
                                 SCM_MAKE_STR(GAUCHE_CA_BUNDLE),
                                 &ca_bundle_path);
    k_options = SCM_MAKE_KEYWORD("options");
    k_num_sessions = SCM_MAKE_KEYWORD("num-sessions");
}

static const uint8_t* get_message_body(ScmObj msg, size_t *size)
{
    if (SCM_UVECTORP(msg) || SCM_STRINGP(msg)) {
    }
    return Scm_GetBytes(msg, size);
}

/*=========================================================
 * axTLS
 */

#if defined(GAUCHE_USE_AXTLS)

typedef struct ScmAxTLSRec {
    ScmTLS common;
    SSL_CTX* ctx;
    SSL* conn;
} ScmAxTLS;

static void ax_context_check(ScmAxTLS* t, const char* op)
{
    if (!t->ctx) Scm_Error("attempt to %s destroyed TLS: %S", op, t);
}

static void ax_close_check(ScmAxTLS* t, const char *op)
{
    if (!t->conn) Scm_Error("attempt to %s closed TLS: %S", op, t);
}
    
static ScmObj ax_connect(ScmTLS* tls, int fd)
{
    ScmAxTLS *t = (ScmAxTLS*)tls;
    ax_context_check(t, "connect");
    if (t->conn) Scm_SysError("attempt to connect already-connected TLS %S", t);
    t->conn = ssl_client_new(t->ctx, fd, 0, 0, NULL);
    int r = ssl_handshake_status(t->conn);
    if (r != SSL_OK) {
        Scm_Error("TLS handshake failed: %d", r);
    }
    return SCM_OBJ(t);
}

static ScmObj ax_accept(ScmTLS* tls, int fd)
{
    ScmAxTLS *t = (ScmAxTLS*)tls;
    ax_context_check(t, "accept");
    if (t->conn) Scm_SysError("attempt to connect already-connected TLS %S", t);
    t->conn = ssl_server_new(t->ctx, fd);
}

static ScmObj ax_read(ScmTLS* tls)
{
    ScmAxTLS *t = (ScmAxTLS*)tls;
    ax_context_check(t, "read");
    ax_close_check(t, "read");
    int r; uint8_t* buf;
    while ((r = ssl_read(t->conn, &buf)) == SSL_OK);
    if (r < 0) Scm_SysError("ssl_read() failed");
    return Scm_MakeString((char*) buf, r, r, SCM_STRING_INCOMPLETE);
}

static ScmObj ax_write(ScmTLS* tls, ScmObj msg)
{
    ScmAxTLS *t = (ScmAxTLS*)tls;
    ax_context_check(t, "write");
    ax_close_check(t, "write");

    size_t size;
    const uint8_t* cmsg = Scm_GetBytes(msg, &size);
    if (cmsg == NULL) {
        Scm_TypeError("TLS message", "uniform vector or string", msg);
    }

    int r = ssl_write(t->conn, cmsg, size);
    if (r  < 0) {
        Scm_SysError("ssl_write() failed");
    }
    return SCM_MAKE_INT(r);
}

static ScmObj ax_close(ScmTLS *tls)
{
    ScmAxTLS *t = (ScmAxTLS*)tls;
    if (t->ctx && t->conn) {
        ssl_free(t->conn);
        t->conn = 0;
        t->common.in_port = t->common.out_port = SCM_UNDEFINED;
    }
}

static ScmObj ax_loadObject(ScmTLS* tls, ScmObj obj_type,
                            const char *filename, const char *password)
{
    ScmAxTLS *t = (ScmAxTLS*)tls;
    uint32_t type = Scm_GetIntegerU32Clamp(obj_type, SCM_CLAMP_ERROR, NULL);
    if (ssl_obj_load(t->ctx, type, filename, password) == SSL_OK)
        return SCM_TRUE;
    else
        return SCM_FALSE;
}

static void ax_finalize(ScmObj obj, void *data)
{
    ScmAxTLS *t = (ScmAxTLS*)obj;
    if (t->ctx) {
        ax_close((ScmTLS*)t);
        ssl_ctx_free(t->ctx);
        t->ctx = NULL;
    }
}

static ScmObj ax_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmAxTLS* t = SCM_NEW_INSTANCE(ScmAxTLS, klass);

    uint32_t options = SSL_SERVER_VERIFY_LATER;
    ScmObj s_options = Scm_GetKeyword(k_options, initargs, SCM_UNDEFINED);
    if (SCM_INTEGERP(s_options)) {
        options = Scm_GetIntegerU32Clamp(s_options, SCM_CLAMP_ERROR, NULL);
    }
    int num_sessions = 0;
    ScmObj s_num_sessions = Scm_GetKeyword(k_num_sessions, initargs,
                                           SCM_UNDEFINED);
    if (SCM_INTP(s_num_sessions)) {
        num_sessions = SCM_INT_VALUE(s_num_sessions);
    }

    t->ctx = ssl_ctx_new(options, num_sessions);
    t->conn = NULL;
    t->common.in_port = t->common.out_port = SCM_UNDEFINED;

    t->common.connect = ax_connect;
    t->common.accept = ax_accept;
    t->common.read = ax_read;
    t->common.write = ax_write;
    t->common.close = ax_close;
    t->common.loadObject = ax_loadObject;
    t->common.finalize = ax_finalize;
    Scm_RegisterFinalizer(SCM_OBJ(t), ax_finalize, NULL);
    return SCM_OBJ(t);
}

#endif /*GAUCHE_USE_AXTLS*/

