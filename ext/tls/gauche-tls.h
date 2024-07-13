/*
 * gauche-tls.h - TLS secure connection interface
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

#ifndef GAUCHE_TLS_H
#define GAUCHE_TLS_H

#include <gauche.h>
#include <gauche/net.h>

#if defined(EXTTLS_EXPORTS)
#define LIBGAUCHE_EXT_BODY
#endif
#include <gauche/extern.h>
#include <gauche/class.h>

#define GAUCHE_CA_SYSTEM "system"

/* We try these paths to find ca certs on Unix-like platforms */
#define SYSTEM_CA_CERT_PATHS                                            \
    "/etc/ssl/certs/ca-certificates.crt", /* ubnutu */                  \
    "/usr/share/pki/ca-trust-source/ca-bundle.trust.crt", /* fedora */  \
    "/etc/pki/tls/certs/ca-budle.crt",    /* fedora (compat) */         \
    "/usr/local/etc/openssl/cert.pem"     /* osx homebrew openssl */

/* This is a 'success' code of AxTLS.  mbedtls x509 routine returns 0
   on success, which happens to be the same as SSL_OK.
   We just need this in case we're configured with mbedtls and without axtls.
 */
#ifndef SSL_OK
#define SSL_OK 0
#endif

#ifndef GAUCHE_USE_AXTLS
/* dummy symbols */
#define SSL_CLIENT_AUTHENTICATION               0x00010000
#define SSL_SERVER_VERIFY_LATER                 0x00020000
#define SSL_NO_DEFAULT_KEY                      0x00040000
#define SSL_DISPLAY_STATES                      0x00080000
#define SSL_DISPLAY_BYTES                       0x00100000
#define SSL_DISPLAY_CERTS                       0x00200000
#define SSL_DISPLAY_RSA                         0x00400000
#define SSL_CONNECT_IN_PARTS                    0x00800000
#define SSL_OBJ_X509_CERT                       1
#define SSL_OBJ_X509_CACERT                     2
#define SSL_OBJ_RSA_KEY                         3
#define SSL_OBJ_PKCS8                           4
#define SSL_OBJ_PKCS12                          5
#endif

/* 'proto' parameter */
enum {
    SCM_TLS_PROTO_TCP,
    SCM_TLS_PROTO_UDP
};

SCM_DECL_BEGIN

/* Common structure */

typedef struct ScmTLSRec ScmTLS;

struct ScmTLSRec {
    SCM_HEADER;
    ScmObj in_port;
    ScmObj out_port;

    ScmObj (*connect)(ScmTLS*, const char*, const char*, int);
    ScmObj (*bind)(ScmTLS*, const char*, const char*, int);
    ScmObj (*accept)(ScmTLS*);
    ScmObj (*read)(ScmTLS*);
    ScmObj (*write)(ScmTLS*, ScmObj);
    ScmObj (*close)(ScmTLS*);
    u_long (*poll)(ScmTLS*, u_long, ScmTimeSpec*);
    ScmObj (*loadCertificate)(ScmTLS*, const char*);
    ScmObj (*loadPrivateKey)(ScmTLS*, const char*, const char*);
    ScmObj (*getConnectionAddress)(ScmTLS*, int);
    void   (*finalize)(ScmObj, void*);
};

SCM_CLASS_DECL(Scm_TLSClass);
#if defined(GAUCHE_USE_AXTLS)
SCM_CLASS_DECL(Scm_AxTLSClass);
#endif /*GAUCHE_USE_AXTLS*/

#define SCM_CLASS_TLS   (&Scm_TLSClass)
#define SCM_TLS(obj)    ((ScmTLS*)obj)
#define SCM_TLSP(obj)   SCM_ISA(obj, SCM_CLASS_TLS)

extern ScmObj Scm_MakeTLS(ScmObj);
extern ScmObj Scm_TLSLoadCertificate(ScmTLS *t, const char *path);
extern ScmObj Scm_TLSLoadPrivateKey(ScmTLS *t, const char *path,
                                    const char *password);
extern ScmObj Scm_TLSConnect(ScmTLS *t,
                             const char *host,
                             const char *port, /* number or service name */
                             ScmObj proto);

extern ScmObj Scm_TLSBind(ScmTLS *t,
                          const char *ip,
                          const char *port, /* numeric or service name */
                          ScmObj proto);
extern ScmObj Scm_TLSAccept(ScmTLS *t); /* returns connected <tls> */
extern ScmObj Scm_TLSClose(ScmTLS* t);

/* Set global debug trace level.  0 = no trace, 9 = maximum trace.
   This affect all submodules. */
extern void   Scm_TLSSetDebugLevel(int level);

/* Each submodule registers debug level setter through this. */
extern void   Scm_TLSRegisterDebugLevelCallback(ScmObj setter);

extern int    Scm_TLSSystemCABundleAvailable(void);

extern ScmObj Scm_TLSGetConnectionAddress(ScmTLS *t, int who);
enum {
    TLS_SELF_ADDRESS = 0,
    TLS_PEER_ADDRESS = 1
};

extern u_long Scm_TLSPoll(ScmTLS *t, u_long rwflags, ScmTimeSpec *timeout);
enum {
    TLS_POLL_READ  = 1,
    TLS_POLL_WRITE = 2
};

/*
   KZ: presumably due to block sizes imposed by the crypto algorithms
   used, TLSRead() doesn't take a desired size and instead returns
   whatever the underlying TLS layer was able to read and
   decrypt. size accommodation is implemented in tls.scm.
 */
extern ScmObj Scm_TLSRead(ScmTLS* t);
extern ScmObj Scm_TLSWrite(ScmTLS* t, ScmObj msg);

extern ScmObj Scm_TLSInputPort(ScmTLS* t);
extern ScmObj Scm_TLSOutputPort(ScmTLS* t);

/* internal, for tls.scm implementation convenience */
extern ScmObj Scm_TLSInputPortSet(ScmTLS* t, ScmObj port);
extern ScmObj Scm_TLSOutputPortSet(ScmTLS* t, ScmObj port);


SCM_DECL_END

#endif /*GAUCHE_TLS_H */
