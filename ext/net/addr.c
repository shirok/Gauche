/*
 * addr.c - socket address
 *
 *  Copyright(C) 2001-2004 by Shiro Kawai (shiro@acm.org)
 *
 *  Permission to use, copy, modify, distribute this software and
 *  accompanying documentation for any purpose is hereby granted,
 *  provided that existing copyright notices are retained in all
 *  copies and that this notice is included verbatim in all
 *  distributions.
 *  This software is provided as is, without express or implied
 *  warranty.  In no circumstances the author(s) shall be liable
 *  for any damages arising out of the use of this software.
 *
 *  $Id: addr.c,v 1.20 2004-08-02 12:20:54 shirok Exp $
 */

#include "gauche/net.h"
#include <string.h>

static ScmObj key_path = SCM_FALSE;
static ScmObj key_host = SCM_FALSE;
static ScmObj key_port = SCM_FALSE;
static ScmObj key_any = SCM_FALSE;
static ScmObj key_broadcast = SCM_FALSE;
static ScmObj key_loopback = SCM_FALSE;

/*==================================================================
 * Generic Socket Address
 */

static void sockaddr_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);
static ScmObj sockaddr_allocate(ScmClass *, ScmObj);

ScmClass *Scm_SockAddrCPL[] = {
    SCM_CLASS_STATIC_PTR(Scm_SockAddrClass),
    SCM_CLASS_STATIC_PTR(Scm_TopClass),
    NULL
};

SCM_DEFINE_BUILTIN_CLASS(Scm_SockAddrClass, sockaddr_print,
                         NULL, NULL, sockaddr_allocate,
                         NULL);

void sockaddr_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    Scm_Printf(port, "#<sockaddr %S %S>",
               Scm_SockAddrFamily(SCM_SOCKADDR(obj)),
               Scm_SockAddrName(SCM_SOCKADDR(obj)));
}

int Scm_SockAddrP(ScmObj obj)
{
    return Scm_SubtypeP(Scm_ClassOf(obj), SCM_CLASS_SOCKADDR);
}

/* C interface of sockaddr-name and sockaddr-family */
ScmObj Scm_SockAddrName(ScmSockAddr *addr)
{
    return Scm_Apply(SCM_OBJ(&Scm_GenericSockAddrName),
                     SCM_LIST1(SCM_OBJ(addr)));
}

ScmObj Scm_SockAddrFamily(ScmSockAddr *addr)
{
    return Scm_Apply(SCM_OBJ(&Scm_GenericSockAddrFamily),
                     SCM_LIST1(SCM_OBJ(addr)));
}

/* Fallback of allocation method */
static ScmObj sockaddr_allocate(ScmClass *klass, ScmObj initargs)
{
    Scm_Error("you can't directly instantiate the abstract class <sockaddr>");
    return SCM_UNDEFINED;       /* dummy */
}

/* creates sockaddr from struct sockaddr. */
ScmObj Scm_MakeSockAddr(ScmClass *klass, struct sockaddr *saddr, int len)
{
    ScmSockAddr *addr;
    addr = SCM_NEW2(ScmSockAddr*,
                    sizeof(ScmSockAddr) - sizeof(struct sockaddr) + len);
    if (klass == NULL) {
        switch (saddr->sa_family) {
        case AF_UNIX:
            klass = SCM_CLASS_SOCKADDR_UN;
            break;
        case AF_INET:
            klass = SCM_CLASS_SOCKADDR_IN;
            break;
#ifdef HAVE_IPV6
        case AF_INET6:
            klass = SCM_CLASS_SOCKADDR_IN6;
            break;
#endif
        default:
            Scm_Error("unknown address type (%d)", saddr->sa_family);
            break;
        }
    }
    SCM_SET_CLASS(addr, klass);
    addr->addrlen = len;
    memset(&addr->addr, 0, len);
    memcpy(&addr->addr, saddr, len);
    return SCM_OBJ(addr);
}

/*==================================================================
 * Unix domain socket
 */

#define UNIX_ADDRESS_PATH_MAX  108
static ScmObj sockaddr_un_allocate(ScmClass *klass, ScmObj initargs);

SCM_DEFINE_BUILTIN_CLASS(Scm_SockAddrUnClass, sockaddr_print,
                         NULL, NULL, sockaddr_un_allocate, Scm_SockAddrCPL);

static ScmObj sockaddr_un_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmObj path = Scm_GetKeyword(key_path, initargs, SCM_FALSE);
    ScmSockAddrUn *addr;
    
    if (!SCM_FALSEP(path) && !SCM_STRINGP(path)) {
        Scm_Error(":path parameter must be a string, but got %S", path);
    }
    addr = SCM_NEW(ScmSockAddrUn);
    SCM_SET_CLASS(addr, &Scm_SockAddrUnClass);
    memset(&addr->addr, 0, sizeof(struct sockaddr_un));
    addr->addr.sun_family = AF_UNIX;
    if (SCM_STRINGP(path)) {
        int size = SCM_STRING_SIZE(path);
        if (size >= UNIX_ADDRESS_PATH_MAX-1) {
            Scm_Error("path too long: %S", path);
        }
        memcpy(addr->addr.sun_path, SCM_STRING_START(path), size);
        addr->addr.sun_path[size] = '\0';
    }
    addr->addrlen = sizeof(struct sockaddr_un);
    return SCM_OBJ(addr);
}

/*==================================================================
 * Inet domain socket
 */

static ScmObj sockaddr_in_allocate(ScmClass *klass, ScmObj initargs);

SCM_DEFINE_BUILTIN_CLASS(Scm_SockAddrInClass, sockaddr_print,
                         NULL, NULL, sockaddr_in_allocate, Scm_SockAddrCPL);

static ScmObj sockaddr_in_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmObj host = Scm_GetKeyword(key_host, initargs, key_any);
    ScmObj port = Scm_GetKeyword(key_port, initargs, SCM_MAKE_INT(0));
    ScmSockAddrIn *addr;

    if (!SCM_INTP(port)) {
        Scm_Error(":port parameter must be a small exact integer, but got %S",
                  port);
    }
    addr = SCM_NEW(ScmSockAddrIn);
    SCM_SET_CLASS(addr, &Scm_SockAddrInClass);
    memset(&addr->addr, 0, sizeof(struct sockaddr_in));
#ifdef HAVE_SIN_LEN
    addr->addr.sin_len = sizeof(struct sockaddr_in);
#endif
    addr->addr.sin_family = AF_INET;
    addr->addr.sin_port = htons(SCM_INT_VALUE(port));
    if (SCM_STRINGP(host)) {
        const char *hname = Scm_GetStringConst(SCM_STRING(host));
        /* First, see if host is dotted number notation. */
        if (inet_aton(hname, &addr->addr.sin_addr) == 0) {
            /* Now, we need to look up the host name.
               Call MT-safe Scm_GetHostByName */
            ScmObj ap, hent = Scm_GetHostByName(hname);
            if (!SCM_SYS_HOSTENT_P(hent)) {
                Scm_Error("unknown host: %S", host);
            }
            ap = SCM_SYS_HOSTENT(hent)->addresses;
            if (SCM_NULLP(ap) || !SCM_STRINGP(SCM_CAR(ap))) {
                Scm_Error("host have unknown address type: %S", host);
            }
            hname = Scm_GetStringConst(SCM_STRING(SCM_CAR(ap)));
            if (inet_aton(hname, &addr->addr.sin_addr) == 0) {
                Scm_Error("host name lookup failure: %S", host);
            }
        }
    } else if (host == key_any) {
        addr->addr.sin_addr.s_addr = htonl(INADDR_ANY);
    } else if (host == key_broadcast) {
        addr->addr.sin_addr.s_addr = htonl(INADDR_BROADCAST);
    } else if (host == key_loopback) {
        addr->addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    } else {
        Scm_Error("bad :host parameter: %S", host);
    }
    addr->addrlen = sizeof(struct sockaddr_in);
    return SCM_OBJ(addr);
}

/*==================================================================
 * Inet6 domain socket
 */

#ifdef HAVE_IPV6

static ScmObj sockaddr_in6_allocate(ScmClass *klass, ScmObj initargs);

SCM_DEFINE_BUILTIN_CLASS(Scm_SockAddrIn6Class, sockaddr_print,
                         NULL, NULL, sockaddr_in6_allocate, Scm_SockAddrCPL);

static ScmObj sockaddr_in6_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmObj host = Scm_GetKeyword(key_host, initargs, key_any);
    ScmObj port = Scm_GetKeyword(key_port, initargs, SCM_MAKE_INT(0));
    ScmSockAddrIn6 *addr;

    if (!SCM_INTP(port)) {
        Scm_Error(":port parameter must be a small exact integer, but got %S",
                  port);
    }
    addr = SCM_NEW(ScmSockAddrIn6);
    SCM_SET_CLASS(addr, &Scm_SockAddrIn6Class);
    memset(&addr->addr, 0, sizeof(struct sockaddr_in6));
#ifdef HAVE_SIN6_LEN
    addr->addr.sin6_len = sizeof(struct sockaddr_in6);
#endif
    addr->addr.sin6_family = AF_INET6;
    addr->addr.sin6_port = htons(SCM_INT_VALUE(port));
    if (SCM_STRINGP(host)) {
        const char *hname = Scm_GetStringConst(SCM_STRING(host));
        struct addrinfo hints, *res;
        int r;
        memset(&hints, 0, sizeof(hints));
        hints.ai_family = AF_INET6;
        hints.ai_socktype = SOCK_STREAM;
        r = getaddrinfo(hname, NULL, &hints, &res);
        if (r) Scm_Error("getaddrinfo: %s", gai_strerror(r));
        addr->addr.sin6_addr = ((struct sockaddr_in6*)res->ai_addr)->sin6_addr;
    } else if (host == key_any) {
        addr->addr.sin6_addr = in6addr_any;
    } else if (host == key_loopback) {
        addr->addr.sin6_addr = in6addr_loopback;
    } else {
        Scm_Error("bad :host parameter: %S", host);
    }
    addr->addrlen = sizeof(struct sockaddr_in6);
    return SCM_OBJ(addr);
}

#endif /* HAVE_IPV6 */

/*==================================================================
 * Initialization stuff
 */

void Scm_Init_NetAddr(ScmModule *mod)
{
    key_path      = SCM_MAKE_KEYWORD("path");
    key_host      = SCM_MAKE_KEYWORD("host");
    key_port      = SCM_MAKE_KEYWORD("port");
    key_any       = SCM_MAKE_KEYWORD("any");
    key_broadcast = SCM_MAKE_KEYWORD("broadcast");
    key_loopback  = SCM_MAKE_KEYWORD("loopback");

    Scm_InitBuiltinClass(&Scm_SockAddrClass, "<sockaddr>", NULL,
                         sizeof(ScmSockAddr), mod);
    Scm_InitBuiltinClass(&Scm_SockAddrUnClass, "<sockaddr-un>", NULL,
                         sizeof(ScmSockAddrUn), mod);
    Scm_InitBuiltinClass(&Scm_SockAddrInClass, "<sockaddr-in>", NULL,
                         sizeof(ScmSockAddrIn), mod);
#ifdef HAVE_IPV6
    Scm_InitBuiltinClass(&Scm_SockAddrIn6Class, "<sockaddr-in6>", NULL,
                         sizeof(ScmSockAddrIn6), mod);
#endif /* HAVE_IPV6 */
}

