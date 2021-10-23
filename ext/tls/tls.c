/*
 * tls.c - tls secure connection interface
 *
 *   Copyright (c) 2011 Kirill Zorin <k.zorin@me.com>
 *   Copyright (c) 2018 YOKOTA Hiroshi
 *   Copyright (c) 2018-2021  Shiro Kawai  <shiro@acm.org>
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

#include "load_system_cert.c"

/*
 * Class
 */

static void tls_print(ScmObj obj, ScmPort* port, ScmWriteContext* ctx);

static ScmClass *tlsclass_cpa[] = {
    SCM_CLASS_STATIC_PTR(Scm_TLSClass),
    SCM_CLASS_STATIC_PTR(Scm_ConnectionClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

SCM_DEFINE_BUILTIN_CLASS(Scm_TLSClass, tls_print, NULL, NULL, NULL,
                         tlsclass_cpa+1);

#if defined(GAUCHE_USE_AXTLS)
static ScmObj ax_allocate(ScmClass *klass, ScmObj initargs);

SCM_DEFINE_BUILTIN_CLASS(Scm_AxTLSClass, tls_print, NULL,
                         NULL, ax_allocate, tlsclass_cpa);
#endif /*GAUCHE_USE_AXTLS*/

static void tls_print(ScmObj obj, ScmPort* port,
                      ScmWriteContext* ctx SCM_UNUSED)
{
    Scm_Printf(port, "#<%A", Scm_ShortClassName(SCM_CLASS_OF(obj)));
    /* at the moment there's not much to print, so we leave this hole
       for future development. */
    Scm_Printf(port, ">");
}

/*
 * Global stuff
 */

static ScmPrimitiveParameter *ca_bundle_path;
static ScmPrimitiveParameter *default_tls_class;
static ScmObj s_system;
static ScmObj s_check;
static ScmObj s_tcp;
static ScmObj s_udp;
static ScmObj k_options;
static ScmObj k_num_sessions;
#if defined(GAUCHE_USE_AXTLS)
static ScmObj k_server_name;
#endif

/*
 * Common operations
 */

ScmObj Scm_MakeTLS(ScmObj initargs)
{
    ScmObj klass = Scm_PrimitiveParameterRef(Scm_VM(), default_tls_class);
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

static int parse_proto(ScmObj proto)
{
    if (SCM_EQ(proto, s_tcp)) return SCM_TLS_PROTO_TCP;
    if (SCM_EQ(proto, s_udp)) return SCM_TLS_PROTO_UDP;
    Scm_Error("proto must be either 'tcp or 'udp, but got: %S", proto);
    return 0;                   /* dummy */
}

ScmObj Scm_TLSConnect(ScmTLS *t, const char *host, const char *port,
                      ScmObj proto) /* symbol */
{
    return t->connect(t, host, port, parse_proto(proto));
}

ScmObj Scm_TLSConnectWithSocket(ScmTLS* t, ScmObj sock, int fd)
{
    t->sock = sock;
    return t->connectSock(t, fd);
}

ScmObj Scm_TLSAcceptWithSocket(ScmTLS* t, ScmObj sock, int fd)
{
    t->sock = sock;
    return t->acceptSock(t, fd);
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
    return SCM_UNDEFINED;
}

ScmObj Scm_TLSOutputPortSet(ScmTLS* t, ScmObj port)
{
    t->out_port = port;
    return SCM_UNDEFINED;
}

ScmObj Scm_TLSSocket(ScmTLS* t)
{
    return t->sock;
}

int Scm_TLSSystemCABundleAvailable(void)
{
#if HAVE_WINCRYPT_H
    /* On Windows, we can count on system's cert store. */
    return TRUE;
#else   /* !HAVE_WINCRYPT_H */
    static ScmObj available = SCM_UNDEFINED;

    if (SCM_UNDEFINEDP(available)) {
        const char *cacert_paths[] = {
            SYSTEM_CA_CERT_PATHS,
            in_gauche_cacert_path(),
            NULL
        };

        const char **p = cacert_paths;
        for (;*p != NULL; p++) {
            if (access(*p, R_OK) == 0) {
                available = SCM_TRUE;
                break;
            }
        }
        if (*p == NULL) available = SCM_FALSE;
    }
    return SCM_BOOL_VALUE(available);
#endif  /* !HAVE_WINCRYPT_H */
}

static inline ScmObj default_ca_bundle(void)
{
#if defined(GAUCHE_CA_BUNDLE_FILE)
    return SCM_MAKE_STR(GAUCHE_CA_BUNDLE);
#elif defined(GAUCHE_CA_BUNDLE_SYSTEM)
    return s_system;
#elif defined(GAUCHE_CA_BUNDLE_CHECK)
    return s_check;
#else
    return SCM_FALSE;
#endif
}

void Scm_Init_tls(ScmModule *mod)
{
    Scm_InitStaticClass(&Scm_TLSClass, "<tls>", mod, NULL, 0);
#if defined(GAUCHE_USE_AXTLS)
    Scm_InitStaticClass(&Scm_AxTLSClass, "<ax-tls>", mod, NULL, 0);
#endif

    s_system = SCM_INTERN("system");
    s_check = SCM_INTERN("check");
    s_tcp = SCM_INTERN("tcp");
    s_udp = SCM_INTERN("udp");

    /* Set default-tls-class to be lazy (see tls.scm for the reason) */
    default_tls_class =
        Scm_BindPrimitiveParameter(mod, "default-tls-class",
                                   SCM_FALSE,
                                   SCM_PARAMETER_LAZY);

    /* tls-ca-bundle-path paramter.  It can be 'check, 'system, #f,
       or a string pathname.
       If it is 'check, we check the system ca-bundle at the initialization
       time (see tls.scm) and switch it to either 'system or #f.  So the
       user will never see 'check. */
    ca_bundle_path =
        Scm_BindPrimitiveParameter(mod, "tls-ca-bundle-path",
                                   default_ca_bundle(), 0);
    k_options = SCM_MAKE_KEYWORD("options");
    k_num_sessions = SCM_MAKE_KEYWORD("num-sessions");
#if defined(GAUCHE_USE_AXTLS)
    k_server_name = SCM_MAKE_KEYWORD("server-name");
#endif
}

/*=========================================================
 * axTLS
 */

#if defined(GAUCHE_USE_AXTLS)

#ifdef HAVE_WINCRYPT_H
#include <wincrypt.h>
#endif

typedef struct ScmAxTLSRec {
    ScmTLS common;
    SSL_CTX* ctx;
    SSL* conn;
    SSL_EXTENSIONS* extensions;
    ScmString *server_name;
} ScmAxTLS;

/* axTLS has ssl_display_error but it emits directly to stdout. */
static const char *tls_strerror(int code)
{
    if (code < SSL_X509_OFFSET) {
        return x509_display_error(code - SSL_X509_OFFSET);
    }
    switch (code) {
    case SSL_OK:
        return "no error";
    case SSL_NOT_OK:
        return "not ok (internal error)";
    case SSL_ERROR_DEAD:
        return "connection dead";
    case SSL_CLOSE_NOTIFY:
        return "connection is being closed";
    case SSL_ERROR_CONN_LOST:
        return "connection lost";
    case SSL_ERROR_RECORD_OVERFLOW:
        return "record overflow";
    case SSL_ERROR_SOCK_SETUP_FAILURE:
        return "socket setup failure";
    case SSL_ERROR_INVALID_HANDSHAKE:
        return "invalid handshake";
    case SSL_ERROR_INVALID_PROT_MSG:
        return "invalid protocol message";
    case SSL_ERROR_INVALID_HMAC:
        return "invalid mac";
    case SSL_ERROR_INVALID_VERSION:
        return "invalid version";
    case SSL_ERROR_UNSUPPORTED_EXTENSION:
        return "unsupported extension";
    case SSL_ERROR_INVALID_SESSION:
        return "invalid session";
    case SSL_ERROR_NO_CIPHER:
        return "no cipher";
    case SSL_ERROR_INVALID_CERT_HASH_ALG:
        return "invalid cert hash algorithm";
    case SSL_ERROR_BAD_CERTIFICATE:
        return "bad certificate";
    case SSL_ERROR_INVALID_KEY:
        return "invalid key";
    case SSL_ERROR_FINISHED_INVALID:
        return "finished invalid";
    case SSL_ERROR_NO_CERT_DEFINED:
        return "no certificate defined";
    case SSL_ERROR_NO_CLIENT_RENOG:
        return "client renegotiation not supported";
    case SSL_ERROR_NOT_SUPPORTED:
        return "option not supported";
    default:
        break;
    }
    ScmObj z = Scm_Sprintf("SSL error %d", -code);
    return Scm_GetStringConst(SCM_STRING(z));
}

/*
 * 'system ca-bundle support
 */
#ifdef HAVE_WINCRYPT_H
static int mem_loader(ScmTLS *t, BYTE *pbCertEncoded, DWORD cbCertEncoded)
{
    return ssl_obj_memory_load(((ScmAxTLS*)t)->ctx, SSL_OBJ_X509_CACERT,
                               pbCertEncoded, cbCertEncoded, NULL);
}

static ScmObj load_system_cert(ScmAxTLS *t)
{
    return system_cert_loader((ScmTLS*)t, mem_loader);
}
#else  /*! HAVE_WINCRYPT_H */

static int file_loader(ScmTLS *t, const char *path)
{
    return ssl_obj_load(((ScmAxTLS*)t)->ctx, SSL_OBJ_X509_CACERT, path, NULL);
}

static inline ScmObj load_system_cert(ScmAxTLS *t) {
    return system_cert_loader((ScmTLS*)t, file_loader);
}
#endif /*! HAVE_WINCRYPT_H */


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
    if (t->conn) Scm_Error("attempt to connect already-connected TLS %S", t);

    ScmObj ca_bundle_path = SCM_UNDEFINED;
    SCM_BIND_PROC(ca_bundle_path, "tls-ca-bundle-path",
                  SCM_FIND_MODULE("rfc.tls", 0));
    ScmObj s_ca_file = Scm_ApplyRec0(ca_bundle_path);
    if (SCM_FALSEP(s_ca_file)) {
        if (!(t->ctx->options & SSL_SERVER_VERIFY_LATER)) {
            Scm_Error("axTLS: tls-ca-bundle-path must be set to validate server certs.");
        }
    } else {
      if(Scm_EqP(s_ca_file, SCM_INTERN(GAUCHE_CA_SYSTEM))) {
          if(SCM_FALSEP(load_system_cert(t))) {
              Scm_Error("Can't load certificates from system certificate store");
          }
      } else if (SCM_STRINGP(s_ca_file)) {
          const char *ca_file = Scm_GetStringConst(SCM_STRING(s_ca_file));
          if (!(t->common.loadObject(SCM_TLS(t), SCM_MAKE_INT(SSL_OBJ_X509_CACERT),
                                     ca_file, NULL))) {
              Scm_Error("CA bundle can't load: file=%S", s_ca_file);
          }
      } else {
          Scm_Error("Parameter tls-ca-bundle-path must have a string value,"
                    " but got: %S", s_ca_file);
      }
    }

    const char* hostname = t->server_name ? Scm_GetStringConst(t->server_name) : NULL;
    t->extensions->host_name = hostname;

    t->conn = ssl_client_new(t->ctx, fd, 0, 0, t->extensions);
    int r = ssl_handshake_status(t->conn);
    if (r != SSL_OK) {
        Scm_Error("TLS handshake failed. Possibly no supported ciphers. (code=%d)", r);
    }
    return SCM_OBJ(t);
}

static ScmObj ax_accept(ScmTLS* tls, int fd)
{
    ScmAxTLS *t = (ScmAxTLS*)tls;
    ax_context_check(t, "accept");
    if (t->conn) Scm_Error("attempt to connect already-connected TLS %S", t);
    t->conn = ssl_server_new(t->ctx, fd);

    return SCM_UNDEFINED;
}

static ScmObj ax_read(ScmTLS* tls)
{
    ScmAxTLS *t = (ScmAxTLS*)tls;
    ax_context_check(t, "read");
    ax_close_check(t, "read");
    int r; uint8_t* buf;
    while ((r = ssl_read(t->conn, &buf)) == SSL_OK);
    if (r < 0) Scm_Error("ssl_read() failed: %s", tls_strerror(r));
    return Scm_MakeString((char*) buf, r, r, SCM_STRING_INCOMPLETE);
}

static ScmObj ax_write(ScmTLS* tls, ScmObj msg)
{
    ScmAxTLS *t = (ScmAxTLS*)tls;
    ax_context_check(t, "write");
    ax_close_check(t, "write");

    ScmSize size;
    const uint8_t* cmsg = Scm_GetBytes(msg, &size);
    if (cmsg == NULL) {
        Scm_TypeError("TLS message", "uniform vector or string", msg);
    }

    int r = ssl_write(t->conn, cmsg, size);
    if (r < 0) Scm_Error("ssl_write() failed: %s", tls_strerror(r));
    return SCM_MAKE_INT(r);
}

static ScmObj ax_close(ScmTLS *tls)
{
    ScmAxTLS *t = (ScmAxTLS*)tls;
    if (t->ctx && t->conn) {
        ssl_free(t->conn);
        t->conn = 0;
        t->extensions = NULL;
        t->server_name = NULL;
        t->common.in_port = t->common.out_port = SCM_FALSE;
    }

    return SCM_UNDEFINED;
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

static void ax_finalize(ScmObj obj, void *data SCM_UNUSED)
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

    uint32_t options = 0;
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
    ScmObj server_name = Scm_GetKeyword(k_server_name, initargs, SCM_UNBOUND);
    if (!SCM_STRINGP(server_name) && !SCM_FALSEP(server_name)) {
        Scm_TypeError("ax-tls server-name", "string or #f", server_name);
    }

    t->ctx = ssl_ctx_new(options, num_sessions);
    t->conn = NULL;
    t->extensions = ssl_ext_new();
    t->server_name = SCM_STRING(server_name);
    t->common.in_port = t->common.out_port = SCM_FALSE;

    t->common.connectSock = ax_connect;
    t->common.acceptSock = ax_accept;
    t->common.read = ax_read;
    t->common.write = ax_write;
    t->common.close = ax_close;
    t->common.loadObject = ax_loadObject;
    t->common.finalize = ax_finalize;
    Scm_RegisterFinalizer(SCM_OBJ(t), ax_finalize, NULL);
    return SCM_OBJ(t);
}

#endif /*GAUCHE_USE_AXTLS*/
