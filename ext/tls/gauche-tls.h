/*
 * gauche-tls.h - TLS secure connection interface
 *
 *   Copyright (c) 2011 Kirill Zorin <k.zorin@me.com>
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

#if defined(GAUCHE_USE_AXTLS)
#include "axTLS/ssl/ssl.h"
#else /*!GAUCHE_USE_AXTLS*/
#define SSL_CLIENT_AUTHENTICATION               0x00010000
#define SSL_SERVER_VERIFY_LATER                 0x00020000
#define SSL_NO_DEFAULT_KEY                      0x00040000
#define SSL_DISPLAY_STATES                      0x00080000
#define SSL_DISPLAY_BYTES                       0x00100000
#define SSL_DISPLAY_CERTS                       0x00200000
#define SSL_DISPLAY_RSA                         0x00400000
#define SSL_CONNECT_IN_PARTS                    0x00800000
#endif /*!GAUCHE_USE_AXTLS*/

SCM_DECL_BEGIN

typedef struct ScmTLSRec {
  SCM_HEADER;
#if defined(GAUCHE_USE_AXTLS)
  SSL_CTX* ctx;
  SSL* conn;
  ScmPort* in_port, * out_port;
#endif /*GAUCHE_USE_AXTLS*/
} ScmTLS;

SCM_CLASS_DECL(Scm_TLSClass);

#define SCM_CLASS_TLS   (&Scm_TLSClass)
#define SCM_TLS(obj)    ((ScmTLS*)obj)
#define SCM_TLSP(obj)   SCM_XTYPEP(obj, SCM_CLASS_TLS)

extern ScmObj Scm_MakeTLS(uint32_t options, int num_sessions);
extern ScmObj Scm_TLSDestroy(ScmTLS* t);
extern ScmObj Scm_TLSConnect(ScmTLS* t, int fd);
extern ScmObj Scm_TLSClose(ScmTLS* t);

/*
   KZ: presumably due to block sizes imposed by the crypto algorithms
   used, TLSRead() doesn't take a desired size and instead returns
   whatever the underlying TLS layer was able to read and
   decrypt. size accomodation is implemented in tls.scm.
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
