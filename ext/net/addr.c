/*
 * addr.c - socket address
 *
 *  Copyright (c) 2001-2015  Shiro Kawai  <shiro@acm.org>
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

#include "gauche-net.h"
#include <string.h>

static ScmObj key_path = SCM_FALSE;
static ScmObj key_host = SCM_FALSE;
static ScmObj key_port = SCM_FALSE;
static ScmObj key_any = SCM_FALSE;
static ScmObj key_broadcast = SCM_FALSE;
static ScmObj key_loopback = SCM_FALSE;

/* NB: built-in socket address structures are allocated as ATOMIC---when
   you want to extend them, be careful not to introduce sole pointers to
   allocated objects; GC will collect them prematurely.  The only
   pointer, the tagged pointer to the class, is protected since they're
   bound to global variables. */

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
    return Scm_ApplyRec(SCM_OBJ(&Scm_GenericSockAddrName),
                        SCM_LIST1(SCM_OBJ(addr)));
}

ScmObj Scm_SockAddrFamily(ScmSockAddr *addr)
{
    return Scm_ApplyRec(SCM_OBJ(&Scm_GenericSockAddrFamily),
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
    ScmSockAddr *addr = SCM_NEW_ATOMIC2(
        ScmSockAddr*, sizeof(ScmSockAddr) - sizeof(struct sockaddr) + len);
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
            Scm_Error("unknown address family (%d)", saddr->sa_family);
            break;
        }
    }
    SCM_SET_CLASS(addr, klass);
    addr->addrlen = len;
    memcpy(&addr->addr, saddr, len);
    return SCM_OBJ(addr);
}

/*==================================================================
 * Unix domain socket
 */

static ScmObj sockaddr_un_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmObj path = Scm_GetKeyword(key_path, initargs, SCM_FALSE);
    if (!SCM_FALSEP(path) && !SCM_STRINGP(path)) {
        Scm_Error(":path parameter must be a string, but got %S", path);
    }

    ScmSockAddrUn *addr = SCM_NEW_ATOMIC(ScmSockAddrUn);
    SCM_SET_CLASS(addr, SCM_CLASS_SOCKADDR_UN);
    memset(&addr->addr, 0, sizeof(struct sockaddr_un));
#ifdef HAVE_STRUCT_SOCKADDR_UN_SUN_LEN
    addr->addr.sun_len = sizeof(struct sockaddr_un);
#endif
    addr->addr.sun_family = AF_UNIX;
    if (SCM_STRINGP(path)) {
        u_int size;
        const char *cpath = Scm_GetStringContent(SCM_STRING(path), &size,
                                                 NULL, NULL);
        if (size >= sizeof(addr->addr.sun_path)-1) {
            Scm_Error("path too long: %S", path);
        }
        memcpy(addr->addr.sun_path, cpath, size);
        addr->addr.sun_path[size] = '\0';
    }
    addr->addrlen = sizeof(struct sockaddr_un);
    return SCM_OBJ(addr);
}

static int sockaddr_un_compare(ScmObj x, ScmObj y, int equalp)
{
    ScmSockAddrUn *xx = (ScmSockAddrUn*)x;
    ScmSockAddrUn *yy = (ScmSockAddrUn*)y;
    if (!equalp) Scm_Error("object %S and %S can't be ordered", x, y);

    if (xx->addrlen == yy->addrlen
        && memcmp(xx->addr.sun_path, yy->addr.sun_path,
                  sizeof(xx->addr.sun_path)) == 0) {
        return 0;               /* (equal? x y) => #t */
    } else {
        return -1;              /* (equal? x y) => #f */
    }
}

SCM_DEFINE_BUILTIN_CLASS(Scm_SockAddrUnClass, sockaddr_print,
                         sockaddr_un_compare, NULL,
                         sockaddr_un_allocate, Scm_SockAddrCPL);

/*==================================================================
 * Inet domain socket
 */

static ScmObj sockaddr_in_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmObj host = Scm_GetKeyword(key_host, initargs, key_any);
    ScmObj port = Scm_GetKeyword(key_port, initargs, SCM_MAKE_INT(0));

    if (!SCM_INTP(port)) {
        Scm_Error(":port parameter must be a small exact integer, but got %S",
                  port);
    }
    ScmSockAddrIn *addr = SCM_NEW_ATOMIC(ScmSockAddrIn);
    SCM_SET_CLASS(addr, SCM_CLASS_SOCKADDR_IN);
    memset(&addr->addr, 0, sizeof(struct sockaddr_in));
#ifdef HAVE_STRUCT_SOCKADDR_IN_SIN_LEN
    addr->addr.sin_len = sizeof(struct sockaddr_in);
#endif
    addr->addr.sin_family = AF_INET;
    addr->addr.sin_port = htons((short)SCM_INT_VALUE(port));
    if (SCM_STRINGP(host)) {
        const char *hname = Scm_GetStringConst(SCM_STRING(host));
        /* First, see if host is dotted number notation. */
        if (inet_pton(AF_INET, hname, &addr->addr.sin_addr) <= 0) {
            /* Now, we need to look up the host name.
               Call MT-safe Scm_GetHostByName */
            ScmObj hent = Scm_GetHostByName(hname);
            if (!SCM_SYS_HOSTENT_P(hent)) {
                Scm_Error("unknown host: %S", host);
            }
            ScmObj ap = SCM_SYS_HOSTENT(hent)->addresses;
            if (SCM_NULLP(ap) || !SCM_STRINGP(SCM_CAR(ap))) {
                Scm_Error("host have unknown address type: %S", host);
            }
            hname = Scm_GetStringConst(SCM_STRING(SCM_CAR(ap)));
            if (inet_pton(AF_INET, hname, &addr->addr.sin_addr) == 0) {
                Scm_Error("host name lookup failure: %S", host);
            }
        }
    } else if (host == key_any) {
        addr->addr.sin_addr.s_addr = htonl(INADDR_ANY);
    } else if (host == key_broadcast) {
        addr->addr.sin_addr.s_addr = htonl(INADDR_BROADCAST);
    } else if (host == key_loopback) {
        addr->addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    } else if (SCM_INTEGERP(host)) {
        int ov;
        unsigned long a = Scm_GetIntegerUClamp(host, SCM_CLAMP_NONE, &ov);
        if (ov) Scm_Error("host address is out of range: %S", host);
        addr->addr.sin_addr.s_addr = htonl(a);
    } else if (SCM_U8VECTORP(host)) {
        if (SCM_U8VECTOR_SIZE(host) < 4) {
            Scm_Error("host address is too short: %S", host);
        }
        const unsigned char *p = SCM_U8VECTOR_ELEMENTS(host);
        unsigned long a = (p[0]<<24)+(p[1]<<16)+(p[2]<<8)+p[3];
        addr->addr.sin_addr.s_addr = htonl(a);
    } else {
        Scm_Error("bad :host parameter: %S", host);
    }
    addr->addrlen = sizeof(struct sockaddr_in);
    return SCM_OBJ(addr);
}

static int sockaddr_in_compare(ScmObj x, ScmObj y, int equalp)
{
    ScmSockAddrIn *xx = (ScmSockAddrIn*)x;
    ScmSockAddrIn *yy = (ScmSockAddrIn*)y;
    if (!equalp) Scm_Error("object %S and %S can't be ordered", x, y);

    if (xx->addrlen == yy->addrlen
        && xx->addr.sin_family == yy->addr.sin_family
        && xx->addr.sin_port == yy->addr.sin_port
        && xx->addr.sin_addr.s_addr == yy->addr.sin_addr.s_addr) {
        return 0;               /* (equal? x y) => #t */
    } else {
        return -1;              /* (equal? x y) => #f */
    }
}

SCM_DEFINE_BUILTIN_CLASS(Scm_SockAddrInClass, sockaddr_print,
                         sockaddr_in_compare, NULL,
                         sockaddr_in_allocate, Scm_SockAddrCPL);

/*==================================================================
 * Inet6 domain socket
 */

#ifdef HAVE_IPV6

static ScmObj sockaddr_in6_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmObj host = Scm_GetKeyword(key_host, initargs, key_any);
    ScmObj port = Scm_GetKeyword(key_port, initargs, SCM_MAKE_INT(0));

    if (!SCM_INTP(port)) {
        Scm_Error(":port parameter must be a small exact integer, but got %S",
                  port);
    }
    ScmSockAddrIn6 *addr = SCM_NEW_ATOMIC(ScmSockAddrIn6);
    SCM_SET_CLASS(addr, SCM_CLASS_SOCKADDR_IN6);
    memset(&addr->addr, 0, sizeof(struct sockaddr_in6));
#ifdef HAVE_STRUCT_SOCKADDR_IN6_SIN6_LEN
    addr->addr.sin6_len = sizeof(struct sockaddr_in6);
#endif
    addr->addr.sin6_family = AF_INET6;
    addr->addr.sin6_port = htons(SCM_INT_VALUE(port));
    if (SCM_STRINGP(host)) {
        const char *hname = Scm_GetStringConst(SCM_STRING(host));
        struct addrinfo hints, *res;
        memset(&hints, 0, sizeof(hints));
        hints.ai_family = AF_INET6;
        hints.ai_socktype = SOCK_STREAM;
        int r = getaddrinfo(hname, NULL, &hints, &res);
#if !defined(GAUCHE_WINDOWS)
        if (r) Scm_Error("getaddrinfo failed: %s", gai_strerror(r));
#else  /*GAUCHE_WINDOWS*/
        if (r) Scm_SysError("getaddrinfo failed");
#endif /*GAUCHE_WINDOWS*/
        addr->addr.sin6_addr = ((struct sockaddr_in6*)res->ai_addr)->sin6_addr;
        freeaddrinfo(res);
    } else if (host == key_any) {
        addr->addr.sin6_addr = in6addr_any;
    } else if (host == key_loopback) {
        addr->addr.sin6_addr = in6addr_loopback;
    } else if (SCM_INTEGERP(host)) {
        /* NB: Can we have more efficient way? */
        for (int i=15; i>=0; i--) {
            ScmObj u8 = Scm_LogAnd(host, SCM_MAKE_INT(0xff));
            addr->addr.sin6_addr.s6_addr[i] = SCM_INT_VALUE(u8);
            host = Scm_Ash(host, -8);
        }
    } else if (SCM_U8VECTORP(host)) {
        if (SCM_U8VECTOR_SIZE(host) < 16) {
            Scm_Error("host address is too short: %S", host);
        }
        const unsigned char *p = SCM_U8VECTOR_ELEMENTS(host);
        for (int i=0; i<16; i++) {
            addr->addr.sin6_addr.s6_addr[i] = p[i];
        }
    } else {
        Scm_Error("bad :host parameter: %S", host);
    }
    addr->addrlen = sizeof(struct sockaddr_in6);
    return SCM_OBJ(addr);
}

static int sockaddr_in6_compare(ScmObj x, ScmObj y, int equalp)
{
    ScmSockAddrIn6 *xx = (ScmSockAddrIn6*)x;
    ScmSockAddrIn6 *yy = (ScmSockAddrIn6*)y;
    if (!equalp) Scm_Error("object %S and %S can't be ordered", x, y);

    if (xx->addrlen == yy->addrlen
        && xx->addr.sin6_family == yy->addr.sin6_family
        && xx->addr.sin6_port == yy->addr.sin6_port
        && (memcmp(&xx->addr.sin6_addr, &yy->addr.sin6_addr,
                   sizeof(xx->addr.sin6_addr)) == 0)) {
        return 0;               /* (equal? x y) => #t */
    } else {
        return -1;              /* (equal? x y) => #f */
    }
}

SCM_DEFINE_BUILTIN_CLASS(Scm_SockAddrIn6Class, sockaddr_print,
                         sockaddr_in6_compare, NULL,
                         sockaddr_in6_allocate, Scm_SockAddrCPL);

#endif /* HAVE_IPV6 */

/*==================================================================
 * Parse internet addresses
 */

#define S6_ADDR32(in6, offset) (*(uint32_t*)&(((in6).s6_addr)[(offset)*4]))

ScmObj Scm_InetStringToAddress(const char *s,
                               int *proto,     /*out*/
                               ScmUVector *buf /*out*/)
{
    struct in_addr in4;
#ifdef HAVE_IPV6
    struct in6_addr in6;
#endif
    if (inet_pton(AF_INET, s, &in4) > 0) {
        *proto = AF_INET;
        if (buf) {
            if (Scm_UVectorSizeInBytes(buf) < 4) {
                Scm_Error("uniform vector buffer isn't big enough to hold IPv4 address: %S", buf);
            }
            SCM_U32VECTOR_ELEMENTS(buf)[0] = in4.s_addr;
            return SCM_TRUE;
        } else {
            return Scm_MakeIntegerU(ntohl(in4.s_addr));
        }
    }

#ifdef HAVE_IPV6
    if (inet_pton(AF_INET6, s, &in6) > 0) {
        *proto = AF_INET6;
        if (buf) {
            if (Scm_UVectorSizeInBytes(buf) < 16) {
                Scm_Error("uniform vector buffer isn't big enough to hold IPv6 address: %S", buf);
            }
            SCM_U32VECTOR_ELEMENTS(buf)[0] = S6_ADDR32(in6, 0);
            SCM_U32VECTOR_ELEMENTS(buf)[1] = S6_ADDR32(in6, 1);
            SCM_U32VECTOR_ELEMENTS(buf)[2] = S6_ADDR32(in6, 2);
            SCM_U32VECTOR_ELEMENTS(buf)[3] = S6_ADDR32(in6, 3);
            return SCM_TRUE;
        } else {
            ScmObj s = SCM_MAKE_INT(0);
            for (int i=0; i<4; i++) {
                s = Scm_Add(Scm_Ash(s, 32),
                            Scm_MakeIntegerU(ntohl(S6_ADDR32(in6, i))));
            }
            return s;
        }
    }
#endif
    return SCM_FALSE;
}

ScmObj Scm_InetAddressToString(ScmObj addr,  /* integer or uvector */
                               int proto)
{
    if (proto == AF_INET) {
        char buf[INET_ADDRSTRLEN];
        struct in_addr in4;
        if (SCM_INTEGERP(addr)) {
            u_long a = Scm_GetIntegerU(addr);
            in4.s_addr = htonl(a);
        } else if (SCM_UVECTORP(addr)) {
            if (Scm_UVectorSizeInBytes(SCM_UVECTOR(addr)) < 4) {
                Scm_Error("uvector too short for IPv4 address: %S", addr);
            }
            in4.s_addr = SCM_U32VECTOR_ELEMENTS(addr)[0];
        } else {
            Scm_TypeError("address", "integer or uvector", addr);
        }
        if (inet_ntop(AF_INET, &in4, buf, INET_ADDRSTRLEN) != NULL) {
            return Scm_MakeString(buf, -1, -1, SCM_STRING_COPYING);
        } else {
            Scm_SysError("inet_ntop failed for address %S", addr);
        }
    }
#ifdef HAVE_IPV6
    if (proto == AF_INET6) {
        char buf[INET6_ADDRSTRLEN];
        struct in6_addr in6;
        if (SCM_INTEGERP(addr)) {
            ScmObj mask = Scm_MakeIntegerU(0xffffffffUL);
            for (int i=0; i<4; i++) {
                u_long a = Scm_GetIntegerU(Scm_LogAnd(addr, mask));
                S6_ADDR32(in6, 3-i) = htonl(a);
                addr = Scm_Ash(addr, -32);
            }
        } else if (SCM_UVECTORP(addr)) {
            if (Scm_UVectorSizeInBytes(SCM_UVECTOR(addr)) < 16) {
                Scm_Error("uvector too short for IPv6 address: %S", addr);
            }
            S6_ADDR32(in6, 0) = SCM_U32VECTOR_ELEMENTS(addr)[0];
            S6_ADDR32(in6, 1) = SCM_U32VECTOR_ELEMENTS(addr)[1];
            S6_ADDR32(in6, 2) = SCM_U32VECTOR_ELEMENTS(addr)[2];
            S6_ADDR32(in6, 3) = SCM_U32VECTOR_ELEMENTS(addr)[3];
        } else {
            Scm_TypeError("address", "integer or uvector", addr);
        }
        if (inet_ntop(AF_INET6, &in6, buf, INET6_ADDRSTRLEN) != NULL) {
            return Scm_MakeString(buf, -1, -1, SCM_STRING_COPYING);
        } else {
            Scm_SysError("inet_ntop failed for address %S", addr);
        }
    }
#endif
    Scm_Error("unsupported protocol for inet-address->string: %d", proto);
    return SCM_UNDEFINED;       /* dummy */
#undef BUFLEN
}

/*==================================================================
 * initialization stuff
 */

void Scm_Init_NetAddr(ScmModule *mod)
{
    key_path      = SCM_MAKE_KEYWORD("path");
    key_host      = SCM_MAKE_KEYWORD("host");
    key_port      = SCM_MAKE_KEYWORD("port");
    key_any       = SCM_MAKE_KEYWORD("any");
    key_broadcast = SCM_MAKE_KEYWORD("broadcast");
    key_loopback  = SCM_MAKE_KEYWORD("loopback");

    Scm_InitStaticClass(&Scm_SockAddrClass, "<sockaddr>", mod, NULL, 0);
    Scm_InitStaticClass(&Scm_SockAddrUnClass, "<sockaddr-un>", mod, NULL, 0);
    Scm_InitStaticClass(&Scm_SockAddrInClass, "<sockaddr-in>", mod, NULL, 0);
#ifdef HAVE_IPV6
    Scm_InitStaticClass(&Scm_SockAddrIn6Class, "<sockaddr-in6>", mod, NULL, 0);
#endif /* HAVE_IPV6 */
}
