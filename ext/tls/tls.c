/*
 * tls.c - tls secure connection interface
 *
 *   Copyright (c) 2011 Kirill Zorin <k.zorin@me.com>
 *   Copyright (c) 2018 YOKOTA Hiroshi
 *   Copyright (c) 2018-2024  Shiro Kawai  <shiro@acm.org>
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

static ScmClass *tlsclass_cpa[] = {
    SCM_CLASS_STATIC_PTR(Scm_TLSClass),
    SCM_CLASS_STATIC_PTR(Scm_ConnectionClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

SCM_DEFINE_BUILTIN_CLASS(Scm_TLSClass, tls_print, NULL, NULL, NULL,
                         tlsclass_cpa+1);

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

ScmObj Scm_TLSClose(ScmTLS* t)
{
    /* NB: pots are already closed */

    /* Extra parenthesis to keep close() from being macro-expanded by
       windows compatibility macro. */
    return (t->close)(t);
}

ScmObj Scm_TLSLoadCertificate(ScmTLS* t, const char *filename)
{
    if (!t->loadCertificate) {
        Scm_Error("tls-load-certificate is not supported on %S", t);
    }
    return t->loadCertificate(t, filename);
}

ScmObj Scm_TLSLoadPrivateKey(ScmTLS* t,
                             const char *filename,
                             const char *password) /* can be NULL */
{
    if (!t->loadPrivateKey) {
        Scm_Error("tls-load-private-key is not supported on %S", t);
    }
    return t->loadPrivateKey(t, filename, password);
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

ScmObj Scm_TLSBind(ScmTLS *t, const char *ip, const char *port,
                   ScmObj proto) /* symbol */
{
    return t->bind(t, ip, port, parse_proto(proto));
}

ScmObj Scm_TLSAccept(ScmTLS *t)
{
    return t->accept(t);
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

u_long Scm_TLSPoll(ScmTLS *t, u_long rwflags, ScmTimeSpec *timeout)
{
    return t->poll(t, rwflags, timeout);
}

ScmObj Scm_TLSGetConnectionAddress(ScmTLS *t, int who)
{
    if (t->getConnectionAddress) {
        return t->getConnectionAddress(t, who);
    } else {
        return SCM_FALSE;
    }
}

/*
 * Debug level management
 */
#define MAX_DEBUG_LEVEL_SETTERS 4
static ScmObj debug_level_setters[MAX_DEBUG_LEVEL_SETTERS];
static int debug_level_setter_count = 0;
static int debug_level = 0;
static ScmInternalMutex debug_level_setter_mutex;

void Scm_TLSSetDebugLevel(int level)
{
    if (level < 0) level = 0;
    if (level > 9) level = 9;
    (void)SCM_INTERNAL_MUTEX_LOCK(debug_level_setter_mutex);
    debug_level = level;
    (void)SCM_INTERNAL_MUTEX_UNLOCK(debug_level_setter_mutex);
    for (int i=0; i < debug_level_setter_count; i++) {
        Scm_ApplyRec1(debug_level_setters[i], SCM_MAKE_INT(level));
    }
}

void Scm_TLSRegisterDebugLevelCallback(ScmObj setter)
{
    int overflow = FALSE;

    (void)SCM_INTERNAL_MUTEX_LOCK(debug_level_setter_mutex);
    if (debug_level_setter_count >= MAX_DEBUG_LEVEL_SETTERS) {
        overflow = TRUE;
    } else {
        debug_level_setters[debug_level_setter_count++] = setter;
    }
    (void)SCM_INTERNAL_MUTEX_UNLOCK(debug_level_setter_mutex);
    if (overflow) {
        Scm_Error("[internal] Too many TLS debug level callbacks");
    }
    Scm_ApplyRec1(setter, SCM_MAKE_INT(debug_level));
}

#if !HAVE_WINCRYPT_H
#include "in_gauche_cacert_path.c"
#endif

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

    SCM_INTERNAL_MUTEX_INIT(debug_level_setter_mutex);
}
