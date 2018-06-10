/*
 * tls.c - tls secure connection interface
 *
 *   Copyright (c) 2011 Kirill Zorin <k.zorin@me.com>
 *                 2018 YOKOTA Hiroshi
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
} ;

#if defined(GAUCHE_USE_AXTLS)
static ScmObj ax_allocate(ScmClass *klass, ScmObj initargs);

SCM_DEFINE_BUILTIN_CLASS(Scm_AxTLSClass, tls_print, NULL,
                         NULL, ax_allocate, tlsclass_cpa);
#endif /*GAUCHE_USE_AXTLS*/


#if defined(GAUCHE_USE_MBEDTLS)
static ScmObj mbed_allocate(ScmClass *klass, ScmObj initargs);

SCM_DEFINE_BUILTIN_CLASS(Scm_MbedTLSClass, tls_print, NULL,
                         NULL, mbed_allocate, tlsclass_cpa);
#endif /*GAUCHE_USE_MBEDTLS*/

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

static ScmParameterLoc ca_certificate_path;
static ScmObj k_options;
static ScmObj k_num_sessions;
static ScmObj k_server_name;

/*
 * Common operations
 */
static void tls_finalize(ScmObj obj, void* data)
{
    ScmTLS* t = SCM_TLS(obj);
    if (t->finalize) t->finalize(t);
}

ScmObj Scm_MakeTLS(ScmObj initargs)
{
#if defined(GAUCHE_USE_AXTLS)
    return Scm_Allocate(&Scm_AxTLSClass, initargs);
#elif defined(GAUCHE_USE_MBEDTLS)
    return Scm_Allocate(&Scm_MbedTLSClass, initargs);
#endif
}


/* Explicitly destroys the context.  The axtls context holds open fd for
   /dev/urandom, and sometimes gc isn't called early enough before we use
   up all fds, so explicit destruction is recommended whenever possible. */
ScmObj Scm_TLSDestroy(ScmTLS* t)
{
    if (t->finalize) t->finalize(t);
    return SCM_TRUE;
}

ScmObj Scm_TLSClose(ScmTLS* t)
{
    return t->close(t);
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
    return t->read(t);
}

ScmObj Scm_TLSWrite(ScmTLS* t, ScmObj msg)
{
    return t->write(t, msg);
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
#if defined(GAUCHE_USE_MBEDTLS)
    Scm_InitStaticClass(&Scm_MbedTLSClass, "<mbed-tls>", mod, NULL, 0);
#endif
    Scm_DefinePrimitiveParameter(mod, "tls-ca-certificate-path",
                                 SCM_MAKE_STR(X509_CA_FILE),
                                 &ca_certificate_path);
    k_options = SCM_MAKE_KEYWORD("options");
    k_num_sessions = SCM_MAKE_KEYWORD("num-sessions");
    k_server_name = SCM_MAKE_KEYWORD("server-name");
}

static const uint8_t* get_message_body(ScmObj msg, u_int *size)
{
    if (SCM_UVECTORP(msg)) {
        *size = Scm_UVectorSizeInBytes(SCM_UVECTOR(msg));
        return (const uint8_t*) SCM_UVECTOR_ELEMENTS(msg);
    } else if (SCM_STRINGP(msg)) {
        return (const uint8_t*)Scm_GetStringContent(SCM_STRING(msg), size, 0, 0);
    } else {
        Scm_TypeError("TLS message", "uniform vector or string", msg);
        *size = 0;
        return 0;
    }
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
    int r;
    u_int size;
    const uint8_t* cmsg = get_message_body(msg, &size);
    if ((r = ssl_write(t->conn, cmsg, size)) < 0) {
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

static void ax_finalize(ScmTLS* tls)
{
    ScmAxTLS *t = (ScmAxTLS*)tls;
    if (t->ctx) {
        Scm_TLSClose(tls);
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
    Scm_RegisterFinalizer(SCM_OBJ(t), tls_finalize, NULL);
    return SCM_OBJ(t);
}

#endif /*GAUCHE_USE_AXTLS*/

/*=========================================================
 * mbedTLS
 */

#if defined(GAUCHE_USE_MBEDTLS)

typedef struct ScmMbedTLSRec {
    ScmTLS common;
    mbedtls_ssl_context ctx;
    mbedtls_net_context conn;
    mbedtls_entropy_context entropy;
    mbedtls_ctr_drbg_context ctr_drbg;
    mbedtls_ssl_config conf;
    mbedtls_x509_crt ca;
    ScmString *server_name;
} ScmMbedTLS;

static void mbed_context_check(ScmMbedTLS* t, const char* op)
{
    /* nothing to do (for now) */
}

static void mbed_close_check(ScmMbedTLS* t, const char *op)
{
    if (t->conn.fd < 0) Scm_Error("attempt to %s closed TLS: %S", op, t);
}
    
static ScmObj mbed_connect(ScmTLS* tls, int fd)
{
    ScmMbedTLS* t = (ScmMbedTLS*)tls;
    
    mbed_context_check(t, "connect");
    const char* pers = "Gauche";
    if(mbedtls_ctr_drbg_seed(&t->ctr_drbg, mbedtls_entropy_func, &t->entropy,
			     (const unsigned char *)pers, strlen(pers)) != 0) {
        Scm_SysError("mbedtls_ctr_drbg_seed() failed");
    }

    if (t->conn.fd >= 0) {
        Scm_SysError("attempt to connect already-connected TLS %S", t);
    }
    t->conn.fd = fd;

    if (mbedtls_ssl_config_defaults(&t->conf,
				    MBEDTLS_SSL_IS_CLIENT,
				    MBEDTLS_SSL_TRANSPORT_STREAM,
				    MBEDTLS_SSL_PRESET_DEFAULT) != 0) {
        Scm_SysError("mbedtls_ssl_config_defaults() failed");
    }
    mbedtls_ssl_conf_rng(&t->conf, mbedtls_ctr_drbg_random, &t->ctr_drbg);

    ScmObj s_ca_file = Scm_ParameterRef(Scm_VM(), &ca_certificate_path);
    if (!SCM_STRINGP(s_ca_file)) {
        Scm_Error("Parameter tls-ca-certificate-path must have a string value,"
                  " but got: %S", s_ca_file);
    }
    const char *ca_file = Scm_GetStringConst(SCM_STRING(s_ca_file));
    if(mbedtls_x509_crt_parse_file(&t->ca, ca_file) != 0) {
        Scm_SysError("mbedtls_x509_crt_parse_file() failed: file=%S", s_ca_file);
    }
    mbedtls_ssl_conf_ca_chain(&t->conf, &t->ca, NULL);
    mbedtls_ssl_conf_authmode(&t->conf, MBEDTLS_SSL_VERIFY_REQUIRED);

    if(mbedtls_ssl_setup(&t->ctx, &t->conf) != 0) {
        Scm_SysError("mbedtls_ssl_setup() failed");
    }

    const char* hostname = t->server_name ? Scm_GetStringConst(t->server_name) : NULL;
    if(mbedtls_ssl_set_hostname(&t->ctx, hostname) != 0) {
        Scm_SysError("mbedtls_ssl_set_hostname() failed");
    }

    mbedtls_ssl_set_bio(&t->ctx, &t->conn, mbedtls_net_send, mbedtls_net_recv, NULL);

    int r = mbedtls_ssl_handshake(&t->ctx);
    if (r != 0) {
        Scm_Error("TLS handshake failed: %d", r);
    }
    return SCM_OBJ(t);
}

static ScmObj mbed_accept(ScmTLS* tls, int fd)
{
    ScmMbedTLS *t = (ScmMbedTLS*)tls;
    mbed_context_check(t, "accept");

    const char* pers = "Gauche";
    if(mbedtls_ctr_drbg_seed(&t->ctr_drbg, mbedtls_entropy_func, &t->entropy,
			     (const unsigned char *)pers, strlen(pers)) != 0) {
        Scm_SysError("mbedtls_ctr_drbg_seed() failed");
    }

    if (t->conn.fd >= 0) {
        Scm_SysError("attempt to connect already-connected TLS %S", t);
    }
    t->conn.fd = fd;

    if (mbedtls_ssl_config_defaults(&t->conf,
				    MBEDTLS_SSL_IS_SERVER,
				    MBEDTLS_SSL_TRANSPORT_STREAM,
				    MBEDTLS_SSL_PRESET_DEFAULT) != 0) {
        Scm_SysError("mbedtls_ssl_config_defaults() failed");
    }
    mbedtls_ssl_conf_rng(&t->conf, mbedtls_ctr_drbg_random, &t->ctr_drbg);


    if(mbedtls_ssl_setup(&t->ctx, &t->conf) != 0) {
        Scm_SysError("mbedtls_ssl_setup() failed");
    }

    mbedtls_net_context client_fd;
    mbedtls_net_free(&client_fd);

    mbedtls_ssl_session_reset(&t->ctx);

    if(mbedtls_net_accept(&t->conn, &client_fd, NULL, 0, NULL) != 0) {
        Scm_SysError("mbedtls_net_accept() failed");
    }
    mbedtls_ssl_set_bio(&t->ctx, &client_fd, mbedtls_net_send, mbedtls_net_recv, NULL);

    int r = mbedtls_ssl_handshake(&t->ctx);
    if (r != 0) {
        Scm_Error("TLS handshake failed: %d", r);
    }
    return SCM_OBJ(t);
}

static ScmObj mbed_read(ScmTLS* tls)
{
    ScmMbedTLS *t = (ScmMbedTLS*)tls;
    mbed_context_check(t, "read");
    mbed_close_check(t, "read");
    uint8_t buf[1024] = {};
    int r;
    r = mbedtls_ssl_read(&t->ctx, buf, sizeof(buf));

    if (r < 0) { Scm_SysError("mbedtls_ssl_read() failed"); }

    return Scm_MakeString((char *)buf, r, r, 
                          SCM_STRING_INCOMPLETE | SCM_STRING_COPYING);
}

static ScmObj mbed_write(ScmTLS* tls, ScmObj msg)
{
    ScmMbedTLS *t = (ScmMbedTLS*)tls;
    mbed_context_check(t, "write");
    mbed_close_check(t, "write");

    u_int size;
    const uint8_t* cmsg = get_message_body(msg, &size);

    int r;
    r = mbedtls_ssl_write(&t->ctx, cmsg, size);
    if (r < 0) {
        Scm_SysError("mbedtls_ssl_write() failed");
    }

    return SCM_MAKE_INT(r);
}

static ScmObj mbed_close(ScmTLS *tls)
{
    ScmMbedTLS *t = (ScmMbedTLS*)tls;
    mbedtls_ssl_close_notify(&t->ctx);
    mbedtls_net_free(&t->conn);
    t->server_name = NULL;
    t->common.in_port = t->common.out_port = SCM_UNDEFINED;
    return SCM_TRUE;
}

static ScmObj mbed_loadObject(ScmTLS* tls, ScmObj obj_type,
                            const char *filename, const char *password)
{
    /* irrelevant */
    return SCM_FALSE;
}

static void mbed_finalize(ScmTLS* tls)
{
    mbed_close(tls);
}

static ScmObj mbed_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmMbedTLS* t = SCM_NEW_INSTANCE(ScmMbedTLS, klass);

    ScmObj server_name = Scm_GetKeyword(k_server_name, initargs, SCM_UNBOUND);
    if (!SCM_STRINGP(server_name) && !SCM_FALSEP(server_name)) {
        Scm_TypeError("mbed-tls server-name", "string or #f", server_name);
    }

    mbedtls_ctr_drbg_init(&t->ctr_drbg);
    mbedtls_net_init(&t->conn);
    mbedtls_ssl_init(&t->ctx);
    mbedtls_ssl_config_init(&t->conf);
    mbedtls_x509_crt_init(&t->ca);

    mbedtls_entropy_init(&t->entropy);

    t->server_name = SCM_STRING(server_name);
    t->common.in_port = t->common.out_port = SCM_UNDEFINED;

    t->common.connect = mbed_connect;
    t->common.accept = mbed_accept;
    t->common.read = mbed_read;
    t->common.write = mbed_write;
    t->common.close = mbed_close;
    t->common.loadObject = mbed_loadObject;
    t->common.finalize = mbed_finalize;
    Scm_RegisterFinalizer(SCM_OBJ(t), tls_finalize, NULL);
    return SCM_OBJ(t);
}

#endif /*GAUCHE_USE_MBEDTLS*/

