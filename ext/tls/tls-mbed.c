/*
 * tls-mbed.c - tls layer using mbedTLS
 *
 *   Copyright (c) 2018 YOKOTA Hiroshi
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

#if defined(GAUCHE_USE_MBEDTLS)

#ifdef HAVE_WINCRYPT_H
#include <wincrypt.h>
#endif


/*
 * Class
 */

static ScmObj mbed_allocate(ScmClass *klass, ScmObj initargs);

static void mbedtls_print(ScmObj obj, ScmPort* port,
                          ScmWriteContext* ctx SCM_UNUSED)
{
    Scm_Printf(port, "#<%A", Scm_ShortClassName(SCM_CLASS_OF(obj)));
    /* at the moment there's not much to print, so we leave this hole
       for future development. */
    Scm_Printf(port, ">");
}

/* NB: We avoid referring Scm_TLSClass statically, since it is in another
   DSO module and some OS doesn't resolve inter-DSO static data.  We
   set the CPA field in init routine. */
SCM_DEFINE_BUILTIN_CLASS(Scm_MbedTLSClass, mbedtls_print, NULL,
                         NULL, mbed_allocate, NULL);

static ScmObj k_server_name;

/*
 * Instance
 */
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


static inline ScmObj load_system_cert(ScmMbedTLS *t)
{
#ifdef HAVE_WINCRYPT_H
    const HCERTSTORE h = CertOpenStore(CERT_STORE_PROV_SYSTEM,
                                       X509_ASN_ENCODING,
                                       0,
                                       (CERT_STORE_SHARE_STORE_FLAG |
                                        CERT_STORE_SHARE_CONTEXT_FLAG |
                                        CERT_STORE_OPEN_EXISTING_FLAG |
                                        CERT_STORE_READONLY_FLAG |
                                        CERT_SYSTEM_STORE_LOCAL_MACHINE),
                                       TEXT("Root"));
    if (h == NULL) {
        Scm_Warn("Can't open certificate store");
        return SCM_FALSE;
    }

    if(!CertControlStore(h, 0, CERT_STORE_CTRL_AUTO_RESYNC, NULL)) {
        Scm_Warn("Can't resync certificate store");
        CertCloseStore(h, 0);
        return SCM_FALSE;
    }


    PCCERT_CONTEXT ctx = NULL;
    while(1) {
        ctx = CertEnumCertificatesInStore(h, ctx);

        if (ctx == NULL) { break; }

        int st = mbedtls_x509_crt_parse_der(&t->ca, ctx->pbCertEncoded, ctx->cbCertEncoded);
        if(st != 0) {
            Scm_Warn("Certificate is not accepted: %d", st);
        }
    }

    CertCloseStore(h, 0);
    return SCM_TRUE;
#else
    (void)t;
    return SCM_FALSE;
#endif
}


static void mbed_context_check(ScmMbedTLS* t SCM_UNUSED,
                               const char* op SCM_UNUSED)
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

    ScmObj ca_bundle_path = SCM_UNDEFINED;
    SCM_BIND_PROC(ca_bundle_path, "tls-ca-bundle-path",
                  SCM_FIND_MODULE("rfc.tls", 0));
    ScmObj s_ca_file = Scm_ApplyRec0(ca_bundle_path);
    if (SCM_FALSEP(s_ca_file)) {
        Scm_Error("mbedTLS: tls-ca-bundle-path isn't set. It is required to"
                  " validate server certs.");
    } else if(Scm_EqP(s_ca_file, Scm_MakeSymbol(SCM_STRING(SCM_MAKE_STR("system")), TRUE))) {
        if(SCM_FALSEP(load_system_cert(t))) {
            Scm_Error("Can't load certificates from system certificate store");
        }
    } else if(SCM_STRINGP(s_ca_file)) {
        const char *ca_file = Scm_GetStringConst(SCM_STRING(s_ca_file));
        if(mbedtls_x509_crt_parse_file(&t->ca, ca_file) != 0) {
            Scm_SysError("mbedtls_x509_crt_parse_file() failed: file=%S", s_ca_file);
        }
    } else {
        Scm_Error("Parameter tls-ca-bundle-path must have a string value or 'system,"
                  " but got: %S", s_ca_file);
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

    ScmSize size;
    const uint8_t* cmsg = Scm_GetBytes(msg, &size);

    if (cmsg == NULL) {
        Scm_TypeError("TLS message", "uniform vector or string", msg);
    }
    int r = mbedtls_ssl_write(&t->ctx, cmsg, size);
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

static ScmObj mbed_loadObject(ScmTLS* tls SCM_UNUSED,
                              ScmObj obj_type SCM_UNUSED,
                              const char *filename SCM_UNUSED,
                              const char *password SCM_UNUSED)
{
    /* irrelevant */
    return SCM_FALSE;
}

static void mbed_finalize(ScmObj obj, void *data SCM_UNUSED)
{
    ScmTLS *t = (ScmTLS*)obj;
    mbed_close(t);
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
    Scm_RegisterFinalizer(SCM_OBJ(t), mbed_finalize, NULL);
    return SCM_OBJ(t);
}

#endif /* defined(GAUCHE_USE_MBEDTLS) */

/*
 * Initialization
 */

void Scm_Init_rfc__tls__mbed()
{
    ScmModule *mod = SCM_MODULE(SCM_FIND_MODULE("rfc.tls.mbed", 0));
#if defined(GAUCHE_USE_MBEDTLS)
    ScmClass **cpa = SCM_NEW_ARRAY(ScmClass*, 4);
    cpa[0] = (ScmClass*)Scm_GlobalVariableRef(SCM_MODULE(SCM_FIND_MODULE("rfc.tls", 0)),
                                              SCM_SYMBOL(SCM_INTERN("<tls>")),
                                              0);
    cpa[1] = SCM_CLASS_CONNECTION;
    cpa[2] = SCM_CLASS_TOP;
    cpa[3] = NULL;
    Scm_MbedTLSClass.cpa = cpa;
    Scm_InitStaticClass(&Scm_MbedTLSClass, "<mbed-tls>", mod, NULL, 0);
    k_server_name = SCM_MAKE_KEYWORD("server-name");
#else
    /* insert dummy binding */
    SCM_DEFINE(mod, "<mbed-tls>", SCM_FALSE);
#endif /* defined(GAUCHE_USE_MBEDTLS) */
}
