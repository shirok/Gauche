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

#if defined(GAUCHE_USE_AXTLS)
#  if defined(GAUCHE_WINDOWS)
#    include <ws2tcpip.h>
#  endif /*GAUCHE_WINDOWS*/
#include "axTLS/ssl/ssl.h"
#endif  /* GAUCHE_USE_AXTLS */

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

    /* <socket> - underlying socket.  This is only for reflection,
       and it is managed by the TLS implementation layer.
       Not always available -- MbedTLS 3.0 hides underlying socket,
       so this is #f.  We'll eventually drop this.  */
    ScmObj sock;

    ScmObj (*connect)(ScmTLS*, const char*, const char*, int);
    ScmObj (*connectSock)(ScmTLS*, int);
    ScmObj (*bind)(ScmTLS*, const char*, const char*, int);
    ScmObj (*accept)(ScmTLS*);
    ScmObj (*acceptSock)(ScmTLS*, int);
    ScmObj (*read)(ScmTLS*);
    ScmObj (*write)(ScmTLS*, ScmObj);
    ScmObj (*close)(ScmTLS*);
    ScmObj (*loadObject)(ScmTLS*, ScmObj, const char*, const char*);
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
extern ScmObj Scm_TLSDestroy(ScmTLS* t);
extern ScmObj Scm_TLSLoadObject(ScmTLS* t, ScmObj obj_type,
                                const char *filename,
                                const char *password);
extern ScmObj Scm_TLSConnect(ScmTLS *t,
                             const char *host,
                             const char *port, /* number or service name */
                             ScmObj proto);
extern ScmObj Scm_TLSConnectWithSocket(ScmTLS* t, ScmObj sock, int fd);
extern ScmObj Scm_TLSBind(ScmTLS *t,
                          const char *ip,
                          const char *port, /* numeric or service name */
                          int proto);
extern ScmObj Scm_TLSAccept(ScmTLS *t); /* returns connected <tls> */
extern ScmObj Scm_TLSAcceptWithSocket(ScmTLS* t, ScmObj sock, int fd);
extern ScmObj Scm_TLSClose(ScmTLS* t);
extern ScmObj Scm_TLSSocket(ScmTLS *t); /* DEPRECATED */

extern int    Scm_TLSSystemCABundleAvailable(void);



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
