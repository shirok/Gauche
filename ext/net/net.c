/*
 * net.c - network interface
 *
 *   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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
#include <fcntl.h>
#include <gauche/extend.h>

/*==================================================================
 * Socket
 */

static void socket_print(ScmObj obj, ScmPort *port, ScmWriteContext *ctx);

SCM_DEFINE_BUILTIN_CLASS_SIMPLE(Scm_SocketClass, socket_print);

static void socket_finalize(ScmObj obj, void *data)
{
    ScmSocket *sock = (ScmSocket*)obj;
    /* NB: at this point, sock->inPort and sock->outPort may already
       be GC-ed and finalized, so we don't flush them here.
       Clearing them would help them to be collected earlier, though. */
    if (!(SOCKET_CLOSED(sock->fd))) {
#if defined(GAUCHE_WINDOWS)
        if (sock->cfd >= 0) { close(sock->cfd); sock->cfd = -1; }
#endif /*GAUCHE_WINDOWS*/
        closeSocket(sock->fd);
        sock->fd = INVALID_SOCKET;
        sock->status = SCM_SOCKET_STATUS_CLOSED;
        sock->inPort = NULL;
        sock->outPort = NULL;
    }
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

ScmSocket *make_socket(Socket fd, int type)
{
    ScmSocket *s = SCM_NEW(ScmSocket);
    SCM_SET_CLASS(s, SCM_CLASS_SOCKET);
    s->fd = fd;
    s->status = SCM_SOCKET_STATUS_NONE;
    s->inPort = s->outPort = NULL;
    s->address = NULL;
    s->name = NULL;
    s->type = type;
#if defined(GAUCHE_WINDOWS)
    s->cfd = -1;
#endif /*GAUCHE_WINDOWS*/
    Scm_RegisterFinalizer(SCM_OBJ(s), socket_finalize, NULL);
    return s;
}

ScmObj Scm_MakeSocket(int domain, int type, int protocol)
{
    intptr_t sock;
#if GAUCHE_WINDOWS
    /* On Windows, sockets created by socket() call sets
       WSA_FLAG_OVERLAPPED flag.  When used in threads other than
       primordial thread, I/O to/from such socket fails, since it
       requires extra OVERLAPPED struct in win32 call (which can't
       be done with POSIX calls).   Directly using WSASocket allows
       us to not set WSA_FLAG_OVERLAPPED flag. */
    SCM_SYSCALL(sock, WSASocket(domain, type, protocol, NULL, 0, 0));
#else  /*!GAUCHE_WINDOWS*/
    SCM_SYSCALL(sock, socket(domain, type, protocol));
#endif /*!GAUCHE_WINDOWS*/
    if (SOCKET_INVALID(sock)) Scm_SysError("couldn't create socket");
    return SCM_OBJ(make_socket((Socket)sock, type));
}

ScmObj Scm_SocketShutdown(ScmSocket *s, int how)
{
    int r;
    if (s->status != SCM_SOCKET_STATUS_CONNECTED) {
        return SCM_FALSE;
    }
    SCM_SYSCALL(r, shutdown(s->fd, how));
    if (r < 0) {
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
    /* We don't shutdown the connection; forked process may have
       reference to the same socket. */
    /* Clearing inPort/outPort helps them to be collected earlier. */
    if (s->inPort)  {
        Scm_ClosePort(s->inPort);  /* ignore errors */
        s->inPort = NULL;
    }
    if (s->outPort) {
        Scm_ClosePort(s->outPort); /* ignore errors */
        s->outPort = NULL;
    }
#if defined(GAUCHE_WINDOWS)
    if (s->cfd >= 0) { close(s->cfd); s->cfd = -1; }
#endif /*GAUCHE_WINDOWS*/
    closeSocket(s->fd);
    s->fd = INVALID_SOCKET;
    s->status = SCM_SOCKET_STATUS_CLOSED;
    return SCM_TRUE;
}

static void sockport_err(ScmSocket *sock, const char *io)
{
    Scm_Error("attempt to obtain an %s port from unconnected or closed socket: %S",
              io, sock);
}


ScmObj Scm_SocketInputPort(ScmSocket *sock, int buffering)
{
    if (sock->inPort == NULL) {
        int infd;
        if (sock->type != SOCK_DGRAM &&
            sock->status < SCM_SOCKET_STATUS_CONNECTED) {
            sockport_err(sock, "input");
        }
#ifndef GAUCHE_WINDOWS
        infd = sock->fd;
#else  /*GAUCHE_WINDOWS*/
        /* cfd will be closed when this socket is closed. */
        if (sock->cfd < 0) {
            sock->cfd = _open_osfhandle(sock->fd, 0);
        }
        infd = sock->cfd;
#endif /*GAUCHE_WINDOWS*/
        if (infd == INVALID_SOCKET) sockport_err(sock, "input");

        /* NB: I keep the socket itself in the port name, in order to avoid
           the socket from GCed prematurely if application doesn't keep
           pointer to the socket. */
        ScmObj sockname = SCM_LIST2(SCM_MAKE_STR("socket input"),
                                    SCM_OBJ(sock));
        sock->inPort = SCM_PORT(Scm_MakePortWithFd(sockname, SCM_PORT_INPUT,
                                                   infd, buffering, FALSE));
    }
    return SCM_OBJ(sock->inPort);
}

ScmObj Scm_SocketOutputPort(ScmSocket *sock, int buffering)
{
    if (sock->outPort == NULL) {
        int outfd;
        if (sock->type != SOCK_DGRAM &&
            sock->status < SCM_SOCKET_STATUS_CONNECTED) {
            sockport_err(sock, "output");
        }
#ifndef GAUCHE_WINDOWS
        outfd = sock->fd;
#else  /*GAUCHE_WINDOWS*/
        /* cfd will be closed when this socket is closed. */
        if (sock->cfd < 0) {
            sock->cfd = _open_osfhandle(sock->fd, 0);
        }
        outfd = sock->cfd;
#endif /*GAUCHE_WINDOWS*/
        if (outfd == INVALID_SOCKET) sockport_err(sock, "output");

        /* NB: I keep the socket itself in the port name, in order to avoid
           the socket from GCed prematurely if application doesn't keep
           pointer to the socket. */
        ScmObj sockname = SCM_LIST2(SCM_MAKE_STR("socket output"),
                                    SCM_OBJ(sock));
        sock->outPort = SCM_PORT(Scm_MakePortWithFd(sockname, SCM_PORT_OUTPUT,
                                                    outfd, buffering, FALSE));
    }
    return SCM_OBJ(sock->outPort);
}

/*==================================================================
 * Low-level library
 */

#define CLOSE_CHECK(fd, op, s)                                          \
    do {                                                                \
        if (SOCKET_CLOSED(fd)) {                                        \
            Scm_Error("attempt to %s a closed socket: %S", op, s);      \
        }                                                               \
    } while (0)

ScmObj Scm_SocketBind(ScmSocket *sock, ScmSockAddr *addr)
{
    int r;
    CLOSE_CHECK(sock->fd, "bind", sock);
    SCM_SYSCALL(r, bind(sock->fd, &addr->addr, addr->addrlen));
    if (r < 0) {
        Scm_SysError("bind failed to %S", addr);
    }
    /* The system may assign different address than <addr>, especially when
       <addr> contains some 'wild card' (e.g. port=0).  We call getsockname
       to obtain the exact address.   Patch provided by ODA Hideo */
    ScmSockAddr *naddr = SCM_SOCKADDR(
        Scm_MakeSockAddr(SCM_CLASS_OF(addr), &addr->addr, addr->addrlen));
    SCM_SYSCALL(r, getsockname(sock->fd, &naddr->addr, &naddr->addrlen));
    if (r < 0) {
        Scm_SysError("getsockname failed to %S", addr);
    }
    sock->address = naddr;
    sock->status = SCM_SOCKET_STATUS_BOUND;
    return SCM_OBJ(sock);
}

ScmObj Scm_SocketListen(ScmSocket *sock, int backlog)
{
    int r;
    CLOSE_CHECK(sock->fd, "listen to", sock);
    SCM_SYSCALL(r, listen(sock->fd, backlog));
    if (r < 0) {
        Scm_SysError("listen(2) failed");
    }
    sock->status = SCM_SOCKET_STATUS_LISTENING;
    return SCM_OBJ(sock);
}

ScmObj Scm_SocketAccept(ScmSocket *sock)
{
    Socket newfd;
    struct sockaddr_storage addrbuf;
    socklen_t addrlen = sizeof(addrbuf);
    ScmClass *addrClass = Scm_ClassOf(SCM_OBJ(sock->address));

    CLOSE_CHECK(sock->fd, "accept from", sock);
    SCM_SYSCALL(newfd, accept(sock->fd, (struct sockaddr*)&addrbuf, &addrlen));
    if (SOCKET_INVALID(newfd)) {
        if (errno == EAGAIN) {
            return SCM_FALSE;
        } else {
            Scm_SysError("accept(2) failed");
        }
    }
    ScmSocket *newsock = make_socket(newfd, sock->type);
    newsock->address =
        SCM_SOCKADDR(Scm_MakeSockAddr(addrClass,
                                      (struct sockaddr*)&addrbuf,
                                      addrlen));
    newsock->status = SCM_SOCKET_STATUS_CONNECTED;
    return SCM_OBJ(newsock);
}

ScmObj Scm_SocketConnect(ScmSocket *sock, ScmSockAddr *addr)
{
    int r;
    CLOSE_CHECK(sock->fd, "connect to", sock);
    SCM_SYSCALL(r, connect(sock->fd, &addr->addr, addr->addrlen));
    if (r < 0) {
        Scm_SysError("connect failed to %S", addr);
    }
    sock->address = addr;
    sock->status = SCM_SOCKET_STATUS_CONNECTED;
    return SCM_OBJ(sock);
}

ScmObj Scm_SocketGetSockName(ScmSocket *sock)
{
    int r;
    struct sockaddr_storage addrbuf;
    socklen_t addrlen = sizeof(addrbuf);

    CLOSE_CHECK(sock->fd, "get the name of", sock);
    SCM_SYSCALL(r, getsockname(sock->fd, (struct sockaddr*)&addrbuf, &addrlen));
    if (r < 0) {
        Scm_SysError("getsockname(2) failed");
    }
    return SCM_OBJ(Scm_MakeSockAddr(NULL, (struct sockaddr*)&addrbuf, addrlen));
}

ScmObj Scm_SocketGetPeerName(ScmSocket *sock)
{
    int r;
    struct sockaddr_storage addrbuf;
    socklen_t addrlen = sizeof(addrbuf);

    CLOSE_CHECK(sock->fd, "get the name of", sock);
    SCM_SYSCALL(r, getpeername(sock->fd, (struct sockaddr*)&addrbuf, &addrlen));
    if (r < 0) {
        Scm_SysError("getpeername(2) failed");
    }
    return SCM_OBJ(Scm_MakeSockAddr(NULL, (struct sockaddr*)&addrbuf, addrlen));
}

static const char *get_message_body(ScmObj msg, u_int *size)
{
    if (SCM_UVECTORP(msg)) {
        *size = Scm_UVectorSizeInBytes(SCM_UVECTOR(msg));
        return (const char*)SCM_UVECTOR_ELEMENTS(msg);
    } else if (SCM_STRINGP(msg)) {
        return Scm_GetStringContent(SCM_STRING(msg), size, NULL, NULL);
    } else {
        Scm_TypeError("socket message", "uniform vector or string", msg);
        *size = 0;              /* dummy */
        return NULL;
    }
}

ScmObj Scm_SocketSend(ScmSocket *sock, ScmObj msg, int flags)
{
    int r;
    u_int size;
    CLOSE_CHECK(sock->fd, "send to", sock);
    const char *cmsg = get_message_body(msg, &size);
    SCM_SYSCALL(r, send(sock->fd, cmsg, size, flags));
    if (r < 0) Scm_SysError("send(2) failed");
    return SCM_MAKE_INT(r);
}

ScmObj Scm_SocketSendTo(ScmSocket *sock, ScmObj msg, ScmSockAddr *to,
                        int flags)
{
    int r;
    u_int size;
    CLOSE_CHECK(sock->fd, "send to", sock);
    const char *cmsg = get_message_body(msg, &size);
    SCM_SYSCALL(r, sendto(sock->fd, cmsg, size, flags,
                          &SCM_SOCKADDR(to)->addr, SCM_SOCKADDR(to)->addrlen));
    if (r < 0) Scm_SysError("sendto(2) failed");
    return SCM_MAKE_INT(r);
}

ScmObj Scm_SocketSendMsg(ScmSocket *sock, ScmObj msg, int flags)
{
#if !GAUCHE_WINDOWS
    int r;
    u_int size;
    CLOSE_CHECK(sock->fd, "send to", sock);
    const char *cmsg = get_message_body(msg, &size);
    SCM_SYSCALL(r, sendmsg(sock->fd, (struct msghdr*)cmsg, flags));
    if (r < 0) Scm_SysError("sendmsg(2) failed");
    return SCM_MAKE_INT(r);
#else  /*GAUCHE_WINDOWS*/
    Scm_Error("sendmsg is not implemented on this platform.");
    return SCM_UNDEFINED;       /* dummy */
#endif /*GAUCHE_WINDOWS*/
}

ScmObj Scm_SocketRecv(ScmSocket *sock, int bytes, int flags)
{
    int r;
    CLOSE_CHECK(sock->fd, "recv from", sock);
    char *buf = SCM_NEW_ATOMIC2(char*, bytes);
    SCM_SYSCALL(r, recv(sock->fd, buf, bytes, flags));
    if (r < 0) {
        Scm_SysError("recv(2) failed");
    }
    return Scm_MakeString(buf, r, r, SCM_STRING_INCOMPLETE);
}

static char *get_message_buffer(ScmUVector *v, u_int *size)
{
    if (SCM_UVECTOR_IMMUTABLE_P(v)) {
        Scm_Error("attempted to use an immutable uniform vector as a buffer");
    }
    *size = Scm_UVectorSizeInBytes(v);
    return (char *)SCM_UVECTOR_ELEMENTS(v);
}

ScmObj Scm_SocketRecvX(ScmSocket *sock, ScmUVector *buf, int flags)
{
    int r;
    u_int size;
    CLOSE_CHECK(sock->fd, "recv from", sock);
    char *z = get_message_buffer(buf, &size);
    SCM_SYSCALL(r, recv(sock->fd, z, size, flags));
    if (r < 0) {
        Scm_SysError("recv(2) failed");
    }
    return Scm_MakeInteger(r);
}

ScmObj Scm_SocketRecvFrom(ScmSocket *sock, int bytes, int flags)
{
    int r;
    struct sockaddr_storage from;
    socklen_t fromlen = sizeof(from);
    CLOSE_CHECK(sock->fd, "recv from", sock);
    char *buf = SCM_NEW_ATOMIC2(char*, bytes);
    SCM_SYSCALL(r, recvfrom(sock->fd, buf, bytes, flags,
                            (struct sockaddr*)&from, &fromlen));
    if (r < 0) {
        Scm_SysError("recvfrom(2) failed");
    }
    return Scm_Values2(Scm_MakeString(buf, r, r, SCM_STRING_INCOMPLETE),
                       Scm_MakeSockAddr(NULL, (struct sockaddr*)&from, fromlen));
}

/* ADDRS is a list of socket addresses; if 'from' address type matches
   one of them, it is used to store the information so that we can avoid
   allocation.  If no addresses match the incoming type, and ADDRS is
   a complete list, the information of 'from' is discarded.  If no addresses
   match the incoming type, and the last cdr of ADDRS is #t (this case
   includes ADDRS == #t), a new sockaddr is allocated and returned. */
ScmObj Scm_SocketRecvFromX(ScmSocket *sock, ScmUVector *buf,
                           ScmObj addrs, int flags)
{
    int r;
    u_int size;
    struct sockaddr_storage from;
    socklen_t fromlen = sizeof(from);
    ScmObj addr = SCM_FALSE;

    CLOSE_CHECK(sock->fd, "recv from", sock);
    char *z = get_message_buffer(buf, &size);
    SCM_SYSCALL(r, recvfrom(sock->fd, z, size, flags,
                            (struct sockaddr*)&from, &fromlen));
    if (r < 0) {
        Scm_SysError("recvfrom(2) failed");
    }
    ScmObj cp;
    SCM_FOR_EACH(cp, addrs) {
        ScmObj a = SCM_CAR(cp);
        if (Scm_SockAddrP(a)) {
            if (SCM_SOCKADDR_FAMILY(a) == from.ss_family) {
                memcpy(&SCM_SOCKADDR(a)->addr, &from, SCM_SOCKADDR(a)->addrlen);
                addr = a;
                break;
            }
        }
    }
    if (SCM_FALSEP(addr) && SCM_EQ(cp, SCM_TRUE)) {
        /* Allocate sockaddr */
        addr = Scm_MakeSockAddr(NULL, (struct sockaddr*)&from, fromlen);
    }
    return Scm_Values2(Scm_MakeInteger(r), addr);
}

/* Low level message builder */
ScmObj Scm_SocketBuildMsg(ScmSockAddr *name, ScmVector *iov,
                          ScmObj control, int flags,
                          ScmUVector *buf)
{
#if !GAUCHE_WINDOWS
    struct msghdr *msg;
    int bufsiz = 0;
    char *bufptr = 0;

    if (buf != NULL) {
        bufsiz = Scm_UVectorSizeInBytes(buf);
        bufptr = (char*)SCM_UVECTOR_ELEMENTS(buf);
    }

    if (bufsiz >= sizeof(struct msghdr)) {
        msg = (struct msghdr*)bufptr;
        bufptr += sizeof(struct msghdr); bufsiz -= sizeof(struct msghdr);
    } else {
        msg = SCM_NEW(struct msghdr);
    }

    if (name != NULL) {
        msg->msg_name = &name->addr;
        msg->msg_namelen = name->addrlen;
    } else {
        msg->msg_name = NULL;
        msg->msg_namelen = 0;
    }

    if (iov != NULL) {
        int iovsiz = SCM_VECTOR_SIZE(iov) * sizeof(struct iovec);
        msg->msg_iovlen = SCM_VECTOR_SIZE(iov);
        if (bufsiz >= iovsiz) {
            msg->msg_iov = (struct iovec*)bufptr;
            bufptr += iovsiz; bufsiz -= iovsiz;
        } else {
            msg->msg_iov = SCM_NEW_ARRAY(struct iovec, msg->msg_iovlen);
        }
        for (int i=0; i < msg->msg_iovlen; i++) {
            ScmObj elt = SCM_VECTOR_ELEMENT(iov, i);
            u_int iovlen;
            msg->msg_iov[i].iov_base = (char*)get_message_body(elt, &iovlen);
            msg->msg_iov[i].iov_len  = iovlen;
        }
    } else {
        msg->msg_iov = NULL;
        msg->msg_iovlen = 0;
    }

    if (SCM_PAIRP(control)) {
        ScmObj cp;
        int ctrllen = 0;

        SCM_FOR_EACH(cp, control) {
            u_int clen;
            ScmObj c = SCM_CAR(cp);
            if (!(Scm_Length(c) == 3
                  && SCM_INTP(SCM_CAR(c))
                  && SCM_INTP(SCM_CADR(c))
                  && (SCM_STRINGP(SCM_CAR(SCM_CDDR(c)))
                      || SCM_U8VECTORP(SCM_CAR(SCM_CDDR(c)))))) {
                Scm_Error("socket-buildmsg: invalid control message spec: %S", c);
            }
            (void)get_message_body(SCM_CAR(SCM_CDDR(c)), &clen);
            ctrllen += CMSG_SPACE(clen);
        }
        msg->msg_controllen = ctrllen;
        if (bufsiz >= ctrllen) {
            msg->msg_control = bufptr;
        } else {
            msg->msg_control = SCM_NEW_ATOMIC_ARRAY(char, ctrllen);
        }
        struct cmsghdr *cmsg = CMSG_FIRSTHDR(msg);
        SCM_FOR_EACH(cp, control) {
            u_int clen;
            ScmObj c = SCM_CAR(cp);
            const char *cdata = get_message_body(SCM_CAR(SCM_CDDR(c)), &clen);
            cmsg->cmsg_level = SCM_INT_VALUE(SCM_CAR(c));
            cmsg->cmsg_type = SCM_INT_VALUE(SCM_CADR(c));
            cmsg->cmsg_len = CMSG_LEN(clen);
            memcpy(CMSG_DATA(cmsg), cdata, clen);
            cmsg = CMSG_NXTHDR(msg, cmsg);
        }
    } else {
        msg->msg_control = NULL;
        msg->msg_controllen = 0;
    }
    msg->msg_flags = flags;

    if (buf != NULL) return SCM_OBJ(buf);
    else return Scm_MakeUVector(SCM_CLASS_U8VECTOR, sizeof(struct msghdr), msg);
#else  /*GAUCHE_WINDOWS*/
    Scm_Error("buildmsg is not implemented on this platform.");
    return SCM_UNDEFINED;
#endif /*GAUCHE_WINDOWS*/
}

/* Low-level setsockopt() and getsockopt() interface. */
/* for getsockopt(), we need to know the size of the result.
   if rtype > 0, it is used as the size of result buffer and
   a string value is returned.  if rtype == 0, the result value
   assumed to be an integer. */

ScmObj Scm_SocketSetOpt(ScmSocket *s, int level, int option, ScmObj value)
{
    int r = 0;
    CLOSE_CHECK(s->fd, "set a socket option of", s);
    if (SCM_STRINGP(value)) {
        u_int size;
        const char *cvalue = Scm_GetStringContent(SCM_STRING(value), &size,
                                                  NULL, NULL);
        SCM_SYSCALL(r, setsockopt(s->fd, level, option, cvalue, size));
    } else if (SCM_UVECTORP(value)) {
        u_int size = Scm_UVectorSizeInBytes(SCM_UVECTOR(value));
        const char *cvalue = (const char*)SCM_UVECTOR_ELEMENTS(value);
        SCM_SYSCALL(r, setsockopt(s->fd, level, option, cvalue, size));
    } else if (SCM_INTP(value) || SCM_BIGNUMP(value)) {
        int v = Scm_GetInteger(value);
        SCM_SYSCALL(r, setsockopt(s->fd, level, option, (void*)&v, sizeof(int)));
    } else {
        Scm_TypeError("socket option value",
                      "an integer, a uvector or a string",
                      value);
    }
    if (r < 0) Scm_SysError("setsockopt failed");
    return SCM_TRUE;
}

ScmObj Scm_SocketGetOpt(ScmSocket *s, int level, int option, int rsize)
{
    int r = 0;
    socklen_t rrsize = rsize;
    CLOSE_CHECK(s->fd, "get a socket option of", s);
    if (rsize > 0) {
        char *buf = SCM_NEW_ATOMIC2(char *, rrsize);
        SCM_SYSCALL(r, getsockopt(s->fd, level, option, buf, &rrsize));
        if (r < 0) Scm_SysError("getsockopt failed");
        return Scm_MakeString(buf, rrsize, rrsize, SCM_STRING_INCOMPLETE);
    } else {
        int val;
        rrsize = sizeof(int);
        SCM_SYSCALL(r, getsockopt(s->fd, level, option, (void*)&val, &rrsize));
        if (r < 0) Scm_SysError("getsockopt failed");
        return Scm_MakeInteger(val);
    }
}

/* Low-level ioctl. */
ScmObj Scm_SocketIoctl(ScmSocket *s, int request, ScmObj data)
{
    int r = 0;
#if HAVE_STRUCT_IFREQ
    struct ifreq ifreq_pkt;

    CLOSE_CHECK(s->fd, "ioctl on", s);
    memset(&ifreq_pkt, 0, sizeof(ifreq_pkt));
    switch (request) {
#if defined(SIOCGIFINDEX)
    case SIOCGIFINDEX:
        if (!SCM_STRINGP(data)) {
            Scm_TypeError("SIOCGIFINDEX ioctl argument", "string", data);
        }
        strncpy(ifreq_pkt.ifr_name, Scm_GetStringConst(SCM_STRING(data)),
                IFNAMSIZ-1);
        SCM_SYSCALL(r, ioctl(s->fd, SIOCGIFINDEX, &ifreq_pkt));
        if (r < 0) Scm_SysError("ioctl(SIOCGIFINDEX) failed");
#if HAVE_STRUCT_IFREQ_IFR_IFINDEX
        return Scm_MakeInteger(ifreq_pkt.ifr_ifindex);
#elif HAVE_STRUCT_IFREQ_IFR_INDEX
       return Scm_MakeInteger(ifreq_pkt.ifr_index);
#endif /*HAVE_STRUCT_IFREQ_IFR_INDEX*/
#endif /*SIOCGIFINDEX*/
    default:
        Scm_Error("unsupported ioctl operation: %d", request);
    }
#else  /*!HAVE_STRUCT_IFREQ*/
    Scm_Error("unsupported ioctl operation: %d", request);
#endif /*!HAVE_STRUCT_IFREQ*/
    return SCM_UNDEFINED;       /* dummy */
}

/*==================================================================
 * Windows/MinGW compatibility layer
 */
#if defined(GAUCHE_WINDOWS)

int inet_pton(int af, const char *src, void *dst)
{
    TCHAR *str = SCM_MBS2WCS(src);

    switch (af) {
    case AF_INET: {
        struct sockaddr_in sa;
        INT addrsize = (INT)sizeof(sa);
        int r = WSAStringToAddress(str, af, NULL, (LPSOCKADDR)&sa, &addrsize);
        if (r != 0) return -1;
        memcpy(dst, &sa.sin_addr, sizeof(struct in_addr));
        return 1;
    }
    case AF_INET6: {
        struct sockaddr_in6 sa6;
        INT addrsize = (INT)sizeof(sa6);
        int r = WSAStringToAddress(str, af, NULL, (LPSOCKADDR)&sa6, &addrsize);
        if (r != 0) return -1;
        memcpy(dst, &sa6.sin6_addr, sizeof(struct in6_addr));
        return 1;
    }
    }
    return -1;
}

const char *inet_ntop(int af, const void *src, char *dst, socklen_t size)
{
#define ADDR_MAXLEN 64
    TCHAR buf[ADDR_MAXLEN];
    DWORD tressize = ADDR_MAXLEN-1;
    const char *res;
    int r = 0;

    switch (af) {
    case AF_INET: {
        struct sockaddr_in sa;
        memset(&sa, 0, sizeof(sa));
        sa.sin_family = AF_INET;
        memcpy(&sa.sin_addr, src, sizeof(struct in_addr));
        r = WSAAddressToString((LPSOCKADDR)&sa, (DWORD)sizeof(sa), NULL,
                               buf, &tressize);
        break;
    }
    case AF_INET6: {
        struct sockaddr_in6 sa6;
        memset(&sa6, 0, sizeof(sa6));
        sa6.sin6_family = AF_INET6;
        memcpy(&sa6.sin6_addr, src, sizeof(struct in6_addr));
        r = WSAAddressToString((LPSOCKADDR)&sa6, (DWORD)sizeof(sa6), NULL,
                               buf, &tressize);
        break;
    }
    default:
        return NULL;
    }

    if (r != 0) return NULL;
    buf[tressize] = 0;
    res = SCM_WCS2MBS(buf);
    size_t ressize = strlen(res);
    if (size <= (int)ressize) return NULL;
    memcpy(dst, res, ressize+1);
    return dst;
#undef ADDR_MAXLEN
}
#endif /*GAUCHE_WINDOWS*/

/*==================================================================
 * Initialization
 */

extern void Scm_Init_NetAddr(ScmModule *mod);
extern void Scm_Init_NetDB(ScmModule *mod);
extern void Scm_Init_netlib(ScmModule *mod);
extern void Scm_Init_netaux(void);




SCM_EXTENSION_ENTRY void Scm_Init_gauche__net(void)
{
    SCM_INIT_EXTENSION(gauche__net);
    ScmModule *mod = SCM_FIND_MODULE("gauche.net", SCM_FIND_MODULE_CREATE);
#ifdef HAVE_IPV6
    Scm_AddFeature("gauche.net.ipv6", NULL);
#endif
    Scm_InitStaticClass(&Scm_SocketClass, "<socket>", mod, NULL, 0);
    Scm_Init_NetAddr(mod);
    Scm_Init_NetDB(mod);
    Scm_Init_netlib(mod);
    Scm_Init_netaux();
}
