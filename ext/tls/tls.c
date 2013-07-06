/*
 * tls.c - tls secure connection interface
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

#include "gauche-tls.h"
#include <gauche/extend.h>

static void tls_print(ScmObj obj, ScmPort* port, ScmWriteContext* ctx);

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_TLSClass, tls_print);

static void tls_print(ScmObj obj, ScmPort* port, ScmWriteContext* ctx)
{
  ScmTLS* t = SCM_TLS(obj);
  Scm_Printf(port, "#<TLS");
  /* at the moment there's not much to print, so we leave this hole
     for future development. */
  Scm_Printf(port, ">");
}

static void tls_finalize(ScmObj obj, void* data)
{
  ScmTLS* t = SCM_TLS(obj);
#if defined(GAUCHE_USE_AXTLS)
  Scm_TLSClose(t);
  ssl_ctx_free(t->ctx);
  t->ctx = 0;
#endif /*GAUCHE_USE_AXTLS*/
}

static void close_check(ScmTLS* tls, const char* op)
{
#if defined(GAUCHE_USE_AXTLS)
  if (!tls->conn)
    Scm_Error("attempt to %s closed TLS: %S", op, tls);
#endif /*GAUCHE_USE_AXTLS*/
}

ScmObj Scm_MakeTLS(void)
{
  ScmTLS* t = SCM_NEW(ScmTLS);
  SCM_SET_CLASS(t, SCM_CLASS_TLS);
#if defined(GAUCHE_USE_AXTLS)
  /* NB: we don't support certificate validation/trust. future work
     will have to take care of this if anyone cares about it at the
     policy level. (it should be noted that axTLS does support this;
     we just don't use it at the moment) */
  t->ctx = ssl_ctx_new(SSL_SERVER_VERIFY_LATER, 0);
  t->conn = 0;
  t->in_port = t->out_port = 0;
#endif /*GAUCHE_USE_AXTLS*/
  Scm_RegisterFinalizer(SCM_OBJ(t), tls_finalize, 0);
  return SCM_OBJ(t);
}

ScmObj Scm_TLSClose(ScmTLS* t)
{
#if defined(GAUCHE_USE_AXTLS)
  if (t->conn)
  {
    ssl_free(t->conn);
    t->conn = 0;
    t->in_port = t->out_port = 0;
  }
#endif /*GAUCHE_USE_AXTLS*/
  return SCM_TRUE;
}

ScmObj Scm_TLSConnect(ScmTLS* t, int fd)
{
#if defined(GAUCHE_USE_AXTLS)
  if (t->conn)
    Scm_SysError("attempt to connect already-connected TLS %S", t);
  t->conn = ssl_client_new(t->ctx, fd, 0, 0);
  if (SSL_OK != ssl_handshake_status(t->conn))
    Scm_SysError("TLS handshake failed");
#endif /*GAUCHE_USE_AXTLS*/
  return SCM_OBJ(t);
}

ScmObj Scm_TLSRead(ScmTLS* t)
{
#if defined(GAUCHE_USE_AXTLS)
  int r; uint8_t* buf;
  close_check(t, "read");
  while ((r = ssl_read(t->conn, &buf)) == SSL_OK);
  if (r < 0)
    Scm_SysError("ssl_read() failed");
  return Scm_MakeString((char*) buf, r, r, SCM_STRING_INCOMPLETE);
#else  /*!GAUCHE_USE_AXTLS*/
  return SCM_FALSE;
#endif /*!GAUCHE_USE_AXTLS*/
}

#if defined(GAUCHE_USE_AXTLS)
static const uint8_t* get_message_body(ScmObj msg, u_int *size)
{
  if (SCM_UVECTORP(msg))
  {
    *size = Scm_UVectorSizeInBytes(SCM_UVECTOR(msg));
    return (const uint8_t*) SCM_UVECTOR_ELEMENTS(msg);
  }
  else if (SCM_STRINGP(msg))
    return (const uint8_t*) Scm_GetStringContent(SCM_STRING(msg), size, 0, 0);
  else
  {
    Scm_TypeError("TLS message", "uniform vector or string", msg);
    *size = 0;
    return 0;
  }
}
#endif /*GAUCHE_USE_AXTLS*/

ScmObj Scm_TLSWrite(ScmTLS* t, ScmObj msg)
{
#if defined(GAUCHE_USE_AXTLS)
  int r; u_int size; const uint8_t* cmsg;
  close_check(t, "write");
  cmsg = get_message_body(msg, &size);
  if ((r = ssl_write(t->conn, cmsg, size)) < 0)
    Scm_SysError("ssl_write() failed");
  return SCM_MAKE_INT(r);
#else  /*!GAUCHE_USE_AXTLS*/
  return SCM_FALSE;
#endif /*!GAUCHE_USE_AXTLS*/
}

ScmObj Scm_TLSInputPort(ScmTLS* t)
{
#if defined(GAUCHE_USE_AXTLS)
  return SCM_OBJ(t->in_port);
#endif /*GAUCHE_USE_AXTLS*/
}

ScmObj Scm_TLSOutputPort(ScmTLS* t)
{
#if defined(GAUCHE_USE_AXTLS)
  return SCM_OBJ(t->out_port);
#endif /*GAUCHE_USE_AXTLS*/
}

ScmObj Scm_TLSInputPortSet(ScmTLS* t, ScmObj port)
{
#if defined(GAUCHE_USE_AXTLS)
  t->in_port = SCM_PORT(port);
#endif /*GAUCHE_USE_AXTLS*/
  return port;
}

ScmObj Scm_TLSOutputPortSet(ScmTLS* t, ScmObj port)
{
#if defined(GAUCHE_USE_AXTLS)
  t->out_port = SCM_PORT(port);
#endif /*GAUCHE_USE_AXTLS*/
  return port;
}

void Scm_Init_tls(ScmModule *mod)
{
  Scm_InitStaticClass(&Scm_TLSClass, "<tls>", mod, NULL, 0);
}
