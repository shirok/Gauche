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
 *  $Id: net.c,v 1.2 2001-05-25 09:07:24 shirok Exp $
 */

#include "net.h"

/*==================================================================
 * Socket
 */

static void socket_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);
static void socket_finalize(GC_PTR obj, GC_PTR data);

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_SocketClass, NULL);

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

ScmObj Scm_MakeSocket(int domain, int type, int protocol)
{
    ScmSocket *s;
    GC_finalization_proc ofn; GC_PTR ocd;
    int sock = socket(domain, type, protocol);
    
    if (sock < 0) Scm_SysError("couldn't create socket");
    s = SCM_NEW(ScmSocket);
    SCM_SET_CLASS(s, SCM_CLASS_SOCKET);
    s->fd = sock;
    s->status = SCM_SOCKET_STATUS_NONE;
    s->inPort = s->outPort = NULL;
    s->address = NULL;
    s->name = NULL;
    GC_REGISTER_FINALIZER(s, socket_finalize, NULL, &ofn, &ocd);
    return SCM_OBJ(s);
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
 * High-level library
 */





                          
