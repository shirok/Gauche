/*
 * net.c - network interface
 *
 *   Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
 *
 *  $Id: net.c,v 1.26 2003-07-05 03:29:10 shirok Exp $
 */

#include "net.h"
#include <gauche/extend.h>

/*==================================================================
 * Socket
 */

static void socket_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_SocketClass, socket_print);

static void socket_cleanup(ScmSocket *sock)
{
    if (sock->status != SCM_SOCKET_STATUS_CLOSED && sock->fd >= 0) {
        if (sock->status == SCM_SOCKET_STATUS_CONNECTED) {
            shutdown(sock->fd, 2);  /* intentionally ignore errors */
            sock->status = SCM_SOCKET_STATUS_SHUTDOWN;
        }
        if (sock->inPort)  Scm_ClosePort(sock->inPort);  /* ignore errors */
        if (sock->outPort) Scm_ClosePort(sock->outPort); /* ignore errors */
        close(sock->fd);
        sock->fd = -1;
        sock->status = SCM_SOCKET_STATUS_CLOSED;
    }
}

static void socket_finalize(ScmObj obj, void *data)
{
    socket_cleanup((ScmSocket *)obj);
}

static void socket_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx)
{
    ScmSocket *sock = SCM_SOCKET(obj);
    Scm_Printf(port, "#<socket");
    switch (sock->status) {
    case SCM_SOCKET_STATUS_NONE:
        break;
    case SCM_SOCKET_STATUS_BOUND:
        Scm_Printf(port, " (bound %S)", Scm_SockAddrName(sock->address));
        break;
    case SCM_SOCKET_STATUS_LISTENING:
        Scm_Printf(port, " (listen %S)", Scm_SockAddrName(sock->address));
        break;
    case SCM_SOCKET_STATUS_CONNECTED:
        Scm_Printf(port, " (connect %S)", Scm_SockAddrName(sock->address));
        break;
    case SCM_SOCKET_STATUS_SHUTDOWN:
        Scm_Printf(port, " (shutdown)");
        break;
    case SCM_SOCKET_STATUS_CLOSED:
        Scm_Printf(port, " (closed)");
        break;
    default:
        Scm_Printf(port, " (unknown status)");
        break;
    }
    Scm_Printf(port, ">");
}

ScmSocket *make_socket(int fd, int type)
{
    ScmSocket *s = SCM_NEW(ScmSocket);
    SCM_SET_CLASS(s, SCM_CLASS_SOCKET);
    s->fd = fd;
    s->status = SCM_SOCKET_STATUS_NONE;
    s->inPort = s->outPort = NULL;
    s->address = NULL;
    s->name = NULL;
    s->type = type;
    Scm_RegisterFinalizer(SCM_OBJ(s), socket_finalize, NULL);
    return s;
}

ScmObj Scm_MakeSocket(int domain, int type, int protocol)
{
    ScmSocket *s;
    int sock = Scm_SysCall(socket(domain, type, protocol));
    if (sock < 0) Scm_SysError("couldn't create socket");
    return SCM_OBJ(make_socket(sock, type));
}

ScmObj Scm_SocketShutdown(ScmSocket *s, int how)
{
    if (s->status != SCM_SOCKET_STATUS_CONNECTED) {
        return SCM_FALSE;
    }
    if (Scm_SysCall(shutdown(s->fd, how)) < 0) {
        Scm_SysError("socket shutdown failed for %S", SCM_OBJ(s));
    }
    s->status = SCM_SOCKET_STATUS_SHUTDOWN;
    return SCM_TRUE;
}

ScmObj Scm_SocketClose(ScmSocket *s)
{
    if (s->status == SCM_SOCKET_STATUS_CLOSED) {
        return SCM_FALSE;
    }
    socket_cleanup(s);
    return SCM_TRUE;
}

ScmObj Scm_SocketInputPort(ScmSocket *sock, int buffering)
{
    if (sock->inPort == NULL) {
        if (sock->type != SOCK_DGRAM &&
            sock->status < SCM_SOCKET_STATUS_CONNECTED) {
            Scm_Error("attempt to obtain an input port from unconnected socket: %S",
                      SCM_OBJ(sock));
        }
        /* NB: I keep the socket itself in the port name, in order to avoid
           the socket from GCed prematurely if application doesn't keep
           pointer to the socket. */
        sock->inPort =
            SCM_PORT(Scm_MakePortWithFd(SCM_LIST2(SCM_MAKE_STR("socket input"),
                                                  SCM_OBJ(sock)),
                                        SCM_PORT_INPUT,
                                        sock->fd, buffering, FALSE));
    }
    return SCM_OBJ(sock->inPort);
}

ScmObj Scm_SocketOutputPort(ScmSocket *sock, int buffering)
{
    if (sock->outPort == NULL) {
        if (sock->type != SOCK_DGRAM &&
            sock->status < SCM_SOCKET_STATUS_CONNECTED) {
            Scm_Error("attempt to obtain an output port from an unconnected socket: %S",
                      SCM_OBJ(sock));
        }
        /* NB: I keep the socket itself in the port name, in order to avoid
           the socket from GCed prematurely if application doesn't keep
           pointer to the socket. */
        sock->outPort =
            SCM_PORT(Scm_MakePortWithFd(SCM_LIST2(SCM_MAKE_STR("socket output"),
                                                  SCM_OBJ(sock)),
                                        SCM_PORT_OUTPUT,
                                        sock->fd, buffering, FALSE));
    }
    return SCM_OBJ(sock->outPort);
}

/*==================================================================
 * Low-level library
 */

ScmObj Scm_SocketBind(ScmSocket *sock, ScmSockAddr *addr)
{
    ScmSockAddr *naddr;
    
    if (sock->fd < 0) {
        Scm_Error("attempt to bind a closed socket: %S", sock);
    }
    if (Scm_SysCall(bind(sock->fd, &addr->addr, addr->addrlen)) < 0) {
        Scm_SysError("bind failed to %S", addr);
    }
    /* The system may assign different address than <addr>, especially when
       <addr> contains some 'wild card' (e.g. port=0).  We call getsockname
       to obtain the exact address.   Patch provided by ODA Hideo */
    naddr = SCM_SOCKADDR(Scm_MakeSockAddr(SCM_CLASS_OF(addr),
                                          &addr->addr, addr->addrlen));
    if (Scm_SysCall(getsockname(sock->fd, &naddr->addr, &naddr->addrlen)) < 0) {
        Scm_SysError("getsockname failed to %S", addr);
    }
    sock->address = naddr;
    sock->status = SCM_SOCKET_STATUS_BOUND;
    return SCM_OBJ(sock);
}

ScmObj Scm_SocketListen(ScmSocket *sock, int backlog)
{
    if (sock->fd < 0) {
        Scm_Error("attempt to listen a closed socket: %S", sock);
    }
    if (Scm_SysCall(listen(sock->fd, backlog)) < 0) {
        Scm_SysError("listen(2) failed");
    }
    sock->status = SCM_SOCKET_STATUS_LISTENING;
    return SCM_OBJ(sock);
}

ScmObj Scm_SocketAccept(ScmSocket *sock)
{
    const char addrbuf[SCM_SOCKADDR_MAXLEN];
    int newfd, addrlen = SCM_SOCKADDR_MAXLEN;
    ScmSocket *newsock;
    ScmClass *addrClass = Scm_ClassOf(SCM_OBJ(sock->address));
    
    if (sock->fd < 0) {
        Scm_Error("attempt to accept a closed socket: %S", sock);
    }
    newfd = Scm_SysCall(accept(sock->fd, (struct sockaddr *)addrbuf, &addrlen));
    if (newfd < 0) {
        if (errno == EAGAIN) {
            return SCM_FALSE;
        } else {
            Scm_SysError("accept(2) failed");
        }
    }
    newsock = make_socket(newfd, sock->type);
    newsock->address =
        SCM_SOCKADDR(Scm_MakeSockAddr(addrClass,
                                      (struct sockaddr *)addrbuf,
                                      addrlen));
    newsock->status = SCM_SOCKET_STATUS_CONNECTED;
    return SCM_OBJ(newsock);
}

ScmObj Scm_SocketConnect(ScmSocket *sock, ScmSockAddr *addr)
{
    if (sock->fd < 0) {
        Scm_Error("attempt to connect a closed socket: %S", sock);
    }
    if (Scm_SysCall(connect(sock->fd, &addr->addr, addr->addrlen)) < 0) {
        Scm_SysError("connect failed to %S", addr);
    }
    sock->address = addr;
    sock->status = SCM_SOCKET_STATUS_CONNECTED;
    return SCM_OBJ(sock);
}

/* Low-level setsockopt() and getsockopt() interface. */
/* for getsockopt(), we need to know the size of the result.
   if rtype > 0, it is used as the size of result buffer and
   a string value is returned.  if rtype == 0, the result value
   assumed to be an integer. */

ScmObj Scm_SocketSetOpt(ScmSocket *s, int level, int option, ScmObj value)
{
    int r = 0;
    if (s->fd < 0) {
        Scm_Error("attempt to set a socket option of a closed socket: %S", s);
    }
    if (SCM_STRINGP(value)) {
        r = Scm_SysCall(setsockopt(s->fd, level, option, SCM_STRING_START(value),
                                   SCM_STRING_SIZE(value)));
    } else if (SCM_INTP(value) || SCM_BIGNUMP(value)) {
        int v = Scm_GetInteger(value);
        r = Scm_SysCall(setsockopt(s->fd, level, option, &v, sizeof(int)));
    } else {
        Scm_Error("socket option must be a string or an integer: %S", value);
    }
    if (r < 0) Scm_SysError("setsockopt failed");
    return SCM_TRUE;
}

ScmObj Scm_SocketGetOpt(ScmSocket *s, int level, int option, int rsize)
{
    int r = 0;
    if (s->fd < 0) {
        Scm_Error("attempt to get a socket option of a closed socket: %S", s);
    }
    if (rsize > 0) {
        char *buf = SCM_NEW_ATOMIC2(char *, rsize);
        r = Scm_SysCall(getsockopt(s->fd, level, option, buf, &rsize));
        if (r < 0) Scm_SysError("getsockopt failed");
        return Scm_MakeString(buf, rsize, rsize, SCM_MAKSTR_INCOMPLETE);
    } else {
        int val;
        rsize = sizeof(int);
        r = Scm_SysCall(getsockopt(s->fd, level, option, &val, &rsize));
        if (r < 0) Scm_SysError("getsockopt failed");
        return Scm_MakeInteger(val);
    }
}
                          
/*==================================================================
 * Initialization
 */

extern void Scm_Init_NetAddr(ScmModule *mod);
extern void Scm_Init_NetDB(ScmModule *mod);
extern void Scm_Init_netlib(ScmModule *mod);

void Scm_Init_libnet(void)
{
    ScmModule *mod;

    SCM_INIT_EXTENSION(net);
    mod = SCM_MODULE(SCM_FIND_MODULE("gauche.net", TRUE));
    Scm_InitBuiltinClass(&Scm_SocketClass, "<socket>", NULL,
                         sizeof(ScmSocket), mod);
    Scm_Init_NetAddr(mod);
    Scm_Init_NetDB(mod);
    Scm_Init_netlib(mod);

    /* Constants for socket option operation.
       I define them here, instead of netlib.stub,  so that I can check
       if the symbol is defined */
#define DEFSYM(sym, val) \
    SCM_DEFINE(mod, sym, Scm_MakeInteger(val))

    DEFSYM("SOL_SOCKET", SOL_SOCKET);
#ifdef SO_KEEPALIVE
    DEFSYM("SO_KEEPALIVE", SO_KEEPALIVE);
#endif
#ifdef SO_OOBINLINE
    DEFSYM("SO_OOBINLINE", SO_OOBINLINE);
#endif
#ifdef SO_REUSEADDR
    DEFSYM("SO_REUSEADDR", SO_REUSEADDR);
#endif
#ifdef SO_TYPE
    DEFSYM("SO_TYPE",      SO_TYPE);
#endif
#ifdef SO_BROADCAST
    DEFSYM("SO_BROADCAST", SO_BROADCAST);
#endif
#ifdef SO_SNDBUF
    DEFSYM("SO_SNDBUF",    SO_SNDBUF);
#endif
#ifdef SO_RCVBUF
    DEFSYM("SO_RCVBUF",    SO_RCVBUF);
#endif
#ifdef SO_PRIORITY
    DEFSYM("SO_PRIORITY",  SO_PRIORITY);
#endif
    DEFSYM("SO_ERROR",     SO_ERROR);

#ifdef SOL_TCP
    DEFSYM("SOL_TCP", SOL_TCP);
#ifdef TCP_NODELAY
    DEFSYM("TCP_NODELAY",  TCP_NODELAY);
#endif
#ifdef TCP_MAXSEG
    DEFSYM("TCP_MAXSEG",   TCP_MAXSEG);
#endif
#ifdef TCP_CORK
    DEFSYM("TCP_CORK",     TCP_CORK);
#endif
#endif /* SOL_TCP */

#ifdef SOL_IP
    DEFSYM("SOL_IP", SOL_IP);
#ifdef IP_OPTIONS
    DEFSYM("IP_OPTIONS",   IP_OPTIONS);
#endif
#endif /* SOL_IP */
}
