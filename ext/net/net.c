/*
 * net.c - network interface
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
 *  $Id: net.c,v 1.3 2001-06-12 10:20:45 shirok Exp $
 */

#include "net.h"

/*==================================================================
 * Socket
 */

static void socket_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);
static void socket_finalize(GC_PTR obj, GC_PTR data);

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

static void socket_finalize(GC_PTR obj, GC_PTR data)
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

ScmSocket *make_socket(int fd)
{
    ScmSocket *s = SCM_NEW(ScmSocket);
    GC_finalization_proc ofn; GC_PTR ocd;
    SCM_SET_CLASS(s, SCM_CLASS_SOCKET);
    s->fd = fd;
    s->status = SCM_SOCKET_STATUS_NONE;
    s->inPort = s->outPort = NULL;
    s->address = NULL;
    s->name = NULL;
    GC_REGISTER_FINALIZER(s, socket_finalize, NULL, &ofn, &ocd);
    return s;
}

ScmObj Scm_MakeSocket(int domain, int type, int protocol)
{
    ScmSocket *s;
    int sock = socket(domain, type, protocol);
    if (sock < 0) Scm_SysError("couldn't create socket");
    return SCM_OBJ(make_socket(sock));
}

ScmObj Scm_SocketShutdown(ScmSocket *s, int how)
{
    if (s->status != SCM_SOCKET_STATUS_CONNECTED) {
        return SCM_FALSE;
    }
    if (shutdown(s->fd, how) < 0) {
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

ScmObj Scm_SocketInputPort(ScmSocket *sock)
{
    if (sock->inPort == NULL) {
        if (sock->status < SCM_SOCKET_STATUS_CONNECTED) {
            Scm_Error("attempt to obtain an input port from unconnected socket: %S",
                      SCM_OBJ(sock));
        }
        sock->inPort = SCM_PORT(Scm_MakePortWithFd(SCM_FALSE,
                                                   SCM_PORT_INPUT,
                                                   sock->fd,
                                                   TRUE, FALSE));
    }
    return SCM_OBJ(sock->inPort);
}

ScmObj Scm_SocketOutputPort(ScmSocket *sock)
{
    if (sock->outPort == NULL) {
        if (sock->status < SCM_SOCKET_STATUS_CONNECTED) {
            Scm_Error("attempt to obtain an output port from an unconnected socket: %S",
                      SCM_OBJ(sock));
        }
        sock->outPort = SCM_PORT(Scm_MakePortWithFd(SCM_FALSE,
                                                    SCM_PORT_OUTPUT,
                                                    sock->fd,
                                                    TRUE, FALSE));
    }
    return SCM_OBJ(sock->outPort);
}

/*==================================================================
 * Low-level library
 */

ScmObj Scm_SocketBind(ScmSocket *sock, ScmSockAddr *addr)
{
    if (sock->fd < 0) {
        Scm_Error("attempt to bind a closed socket: %S", sock);
    }
    if (bind(sock->fd, &addr->addr, addr->addrlen) < 0) {
        Scm_SysError("bind failed to %S", addr);
    }
    sock->address = addr;
    sock->status = SCM_SOCKET_STATUS_BOUND;
    return SCM_OBJ(sock);
}

ScmObj Scm_SocketListen(ScmSocket *sock, int backlog)
{
    if (sock->fd < 0) {
        Scm_Error("attempt to listen a closed socket: %S", sock);
    }
    if (listen(sock->fd, backlog) < 0) {
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
    newfd = accept(sock->fd, (struct sockaddr *)addrbuf, &addrlen);
    if (newfd < 0) {
        if (errno == EAGAIN) {
            return SCM_FALSE;
        } else {
            Scm_SysError("accept(2) failed");
        }
    }
    newsock = make_socket(newfd);
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
    if (connect(sock->fd, &addr->addr, addr->addrlen) < 0) {
        Scm_SysError("connect failed to %S", addr);
    }
    sock->address = addr;
    sock->status = SCM_SOCKET_STATUS_CONNECTED;
    return SCM_OBJ(sock);
}
                          
/*==================================================================
 * Initialization
 */

extern void Scm_Init_NetAddr(ScmModule *mod);
extern void Scm_Init_netlib(ScmModule *mod);

void Scm_Init_net(void)
{
    ScmModule *mod = SCM_MODULE(SCM_FIND_MODULE("gauche.net", TRUE));

    Scm_InitBuiltinClass(&Scm_SocketClass, "<socket>", mod);
    Scm_Init_NetAddr(mod);
    Scm_Init_netlib(mod);
}
