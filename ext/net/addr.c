/*
 * addr.c - socket address
 *
 *  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
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
 *  $Id: addr.c,v 1.5 2001-06-14 09:07:14 shirok Exp $
 */

#include "net.h"
#include <string.h>

static ScmObj key_unknown;
static ScmObj key_inet;
static ScmObj key_unix;
static ScmObj key_path;
static ScmObj key_host;
static ScmObj key_port;
static ScmObj key_any;
static ScmObj key_broadcast;
static ScmObj key_loopback;

/*==================================================================
 * Generic Socket Address
 */

static void sockaddr_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);
static ScmObj sockaddr_allocate(ScmClass *, ScmObj);
static ScmObj sockaddr_family(ScmNextMethod *, ScmObj *, int, void *);

ScmClass *Scm_SockAddrCPL[] = { &Scm_SockAddrClass, &Scm_TopClass, NULL };

SCM_DEFINE_BUILTIN_CLASS(Scm_SockAddrClass, sockaddr_print,
                         NULL, NULL, sockaddr_allocate,
                         SCM_CLASS_DEFAULT_CPL);

SCM_DEFINE_GENERIC(Scm_GenericSockAddrName, Scm_NoNextMethod, NULL);
SCM_DEFINE_GENERIC(Scm_GenericSockAddrFamily, Scm_NoNextMethod, NULL);

void sockaddr_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    Scm_Printf(port, "#<sockaddr%S %S>",
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

/* Fallback of sockaddr-name */
static ScmObj sockaddr_name(ScmNextMethod *nm, ScmObj *args, int nargs,
                            void *d)
{
    return SCM_MAKE_STR("unknown");
    
}

static ScmClass *sockaddr_name_SPEC[] = { &Scm_SockAddrClass };
static SCM_DEFINE_METHOD(sockaddr_name_rec, &Scm_GenericSockAddrName,
                         1, 0, sockaddr_name_SPEC,
                         sockaddr_name, NULL);

/* Fallback of sockaddr-family */
static ScmObj sockaddr_family(ScmNextMethod *nm, ScmObj *args, int nargs,
                              void *d)
{
    return key_unknown;
}

static ScmClass *sockaddr_family_SPEC[] = { &Scm_SockAddrClass };
static SCM_DEFINE_METHOD(sockaddr_family_rec, &Scm_GenericSockAddrFamily,
                         1, 0, sockaddr_family_SPEC,
                         sockaddr_family, NULL);

/* Fallback of allocation method */
static ScmObj sockaddr_allocate(ScmClass *klass, ScmObj initargs)
{
    Scm_Error("you can't directly instantiate the abstract class <sockaddr>");
    return SCM_UNDEFINED;       /* dummy */
}

/* creates sockaddr from struct sockaddr. */
ScmObj Scm_MakeSockAddr(ScmClass *klass, struct sockaddr *saddr, int len)
{
    ScmSockAddr *addr = SCM_NEW2(ScmSockAddr*, sizeof(ScmObj)+len);
    SCM_SET_CLASS(addr, klass);
    memset(&addr->addr, len, 0);
    memcpy(&addr->addr, saddr, len);
    return SCM_OBJ(addr);
}

/*==================================================================
 * Unix domain socket
 */

typedef struct {
    SCM_HEADER;
    int addrlen;
    struct sockaddr_un addr;
} scm_sockaddr_un;

#define UNIX_ADDRESS_PATH_MAX  108
static ScmObj sockaddr_un_allocate(ScmClass *klass, ScmObj initargs);

SCM_DEFINE_BUILTIN_CLASS(Scm_SockAddrUnixClass, sockaddr_print,
                         NULL, NULL, sockaddr_un_allocate, Scm_SockAddrCPL);

static ScmObj sockaddr_un_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmObj path = Scm_GetKeyword(key_path, initargs, SCM_FALSE);
    scm_sockaddr_un *addr;
    
    if (!SCM_FALSEP(path) && !SCM_STRINGP(path)) {
        Scm_Error(":path parameter must be a string, but got %S", path);
    }
    addr = SCM_NEW(scm_sockaddr_un);
    SCM_SET_CLASS(addr, &Scm_SockAddrUnixClass);
    memset(&addr->addr, sizeof(struct sockaddr_un), 0);
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

/* (define-method sockaddr-name ((self <sockaddr-un>)) */
static ScmObj sockaddr_un_name(ScmNextMethod *nm, ScmObj *args, int nargs,
                                 void *d)
{
    scm_sockaddr_un *addr = (scm_sockaddr_un*)args[0];
    return SCM_MAKE_STR(addr->addr.sun_path);
}

static ScmClass *sockaddr_un_name_SPEC[] = { &Scm_SockAddrUnixClass };
static SCM_DEFINE_METHOD(sockaddr_un_name_rec, &Scm_GenericSockAddrName,
                         1, 0, sockaddr_un_name_SPEC,
                         sockaddr_un_name, NULL);

/* (define-method sockaddr-family ((self <sockaddr-un>)) */
static ScmObj sockaddr_un_family(ScmNextMethod *nm, ScmObj *args, int nargs,
                                 void *d)
{
    return key_unix;
}

static ScmClass *sockaddr_un_family_SPEC[] = { &Scm_SockAddrUnixClass };
static SCM_DEFINE_METHOD(sockaddr_un_family_rec, &Scm_GenericSockAddrFamily,
                         1, 0, sockaddr_un_family_SPEC,
                         sockaddr_un_family, NULL);

/*==================================================================
 * Inet domain socket
 */

typedef struct {
    SCM_HEADER;
    int addrlen;
    struct sockaddr_in addr;
} scm_sockaddr_in;

static ScmObj sockaddr_in_allocate(ScmClass *klass, ScmObj initargs);

SCM_DEFINE_BUILTIN_CLASS(Scm_SockAddrInetClass, sockaddr_print,
                         NULL, NULL, sockaddr_in_allocate, Scm_SockAddrCPL);

static ScmObj sockaddr_in_allocate(ScmClass *klass, ScmObj initargs)
{
    ScmObj host = Scm_GetKeyword(key_host, initargs, key_any);
    ScmObj port = Scm_GetKeyword(key_port, initargs, SCM_MAKE_INT(0));
    scm_sockaddr_in *addr;
    
    if (!SCM_INTP(port)) {
        Scm_Error(":port parameter must be a small exact integer, but got %S",
                  port);
    }
    addr = SCM_NEW(scm_sockaddr_in);
    SCM_SET_CLASS(addr, &Scm_SockAddrInetClass);
    memset(&addr->addr, sizeof(struct sockaddr_in), 0);
    addr->addr.sin_family = AF_INET;
    addr->addr.sin_port = htons(SCM_INT_VALUE(port));
    if (SCM_STRINGP(host)) {
        /* TODO: MT safeness */
        struct hostent *hent = gethostbyname(Scm_GetStringConst(SCM_STRING(host)));
        if (hent == NULL) Scm_Error("unknown host: %S", host);
        if (hent->h_addrtype != AF_INET)
            Scm_Error("host have unknown address type: %S", host);
        memcpy(&addr->addr.sin_addr, hent->h_addr_list[0],
               sizeof(struct in_addr));
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

/* (define-method sockaddr-name ((self <sockaddr-in>)) */
static ScmObj sockaddr_in_name(ScmNextMethod *nm, ScmObj *args, int nargs,
                               void *d)
{
    scm_sockaddr_in *addr = (scm_sockaddr_in*)args[0];
    ScmPort *out = SCM_PORT(Scm_MakeOutputStringPort());
    uint32_t inaddr = htonl(addr->addr.sin_addr.s_addr);

    /* NB: avoid using inet_ntoa(), which uses static buffer */
    Scm_Printf(out, "%d.%d.%d.%d:%d",
               ((inaddr>>24)&0xff),
               ((inaddr>>16)&0xff),
               ((inaddr>>8)&0xff),
               ((inaddr>>0)&0xff),
               ntohs(addr->addr.sin_port));
    return Scm_GetOutputString(out);
}

static ScmClass *sockaddr_in_name_SPEC[] = { &Scm_SockAddrInetClass };
static SCM_DEFINE_METHOD(sockaddr_in_name_rec, &Scm_GenericSockAddrName,
                         1, 0, sockaddr_in_name_SPEC,
                         sockaddr_in_name, NULL);

/* (define-method sockaddr-family ((self <sockaddr-in>)) */
static ScmObj sockaddr_in_family(ScmNextMethod *nm, ScmObj *args, int nargs,
                                 void *d)
{
    return key_inet;
}

static ScmClass *sockaddr_in_family_SPEC[] = { &Scm_SockAddrInetClass };
static SCM_DEFINE_METHOD(sockaddr_in_family_rec, &Scm_GenericSockAddrFamily,
                         1, 0, sockaddr_in_family_SPEC,
                         sockaddr_in_family, NULL);

/*==================================================================
 * Initialization stuff
 */

void Scm_Init_NetAddr(ScmModule *mod)
{
    key_unknown   = SCM_MAKE_KEYWORD("unknown");
    key_inet      = SCM_MAKE_KEYWORD("inet");
    key_unix      = SCM_MAKE_KEYWORD("unix");
    key_path      = SCM_MAKE_KEYWORD("path");
    key_host      = SCM_MAKE_KEYWORD("host");
    key_port      = SCM_MAKE_KEYWORD("port");
    key_any       = SCM_MAKE_KEYWORD("any");
    key_broadcast = SCM_MAKE_KEYWORD("broadcast");
    key_loopback  = SCM_MAKE_KEYWORD("loopback");

    Scm_InitBuiltinClass(&Scm_SockAddrClass, "<sockaddr>", mod);
    Scm_InitBuiltinGeneric(&Scm_GenericSockAddrName,
                           "sockaddr-name", mod);
    Scm_InitBuiltinGeneric(&Scm_GenericSockAddrFamily,
                           "sockaddr-family", mod);
    Scm_InitBuiltinMethod(&sockaddr_name_rec);
    Scm_InitBuiltinMethod(&sockaddr_family_rec);
    
    Scm_InitBuiltinClass(&Scm_SockAddrUnixClass, "<sockaddr-un>", mod);
    Scm_InitBuiltinMethod(&sockaddr_un_name_rec);
    Scm_InitBuiltinMethod(&sockaddr_un_family_rec);
    
    Scm_InitBuiltinClass(&Scm_SockAddrInetClass, "<sockaddr-in>", mod);
    Scm_InitBuiltinMethod(&sockaddr_in_name_rec);
    Scm_InitBuiltinMethod(&sockaddr_in_family_rec);
}

