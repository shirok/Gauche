/*
 * net.h - network interface
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

#ifndef GAUCHE_NET_H
#define GAUCHE_NET_H

#include <sys/types.h>
#include <gauche.h>
#include <errno.h>

#if !defined(GAUCHE_WINDOWS)
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <sys/un.h>
#include <sys/ioctl.h>
typedef int Socket;
#define closeSocket close
#define INVALID_SOCKET  ((Socket)-1)
#else  /*GAUCHE_WINDOWS*/
#include <winsock2.h>
#include <ws2tcpip.h>
#include <mswsock.h>
typedef SOCKET Socket;
#define closeSocket closesocket
#endif /*GAUCHE_WINDOWS*/

#ifdef HAVE_RPC_TYPES_H
#include <rpc/types.h>
#endif

#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

#ifdef HAVE_NET_IF_H
#include <net/if.h>
#endif

#if defined(EXTNET_EXPORTS)
#define LIBGAUCHE_EXT_BODY
#endif
#include <gauche/extern.h>

SCM_DECL_BEGIN

/* windows stuff */
#if defined(GAUCHE_WINDOWS)
struct sockaddr_un {
    unsigned short sun_family;
    char sun_path[108];
};
int inet_pton(int af, const char *src, void *dst);
const char *inet_ntop(int af, const void *src, char *dst, socklen_t size);
#define MSG_WAITALL   0x8
#ifdef HAVE_IPV6
#define IPV6_V6ONLY   27
#define AI_ALL        0x00000100
#define AI_ADDRCONFIG 0x00000400
#define AI_V4MAPPED   0x00000800
#if !defined(__MINGW64_VERSION_MAJOR)
void WSAAPI freeaddrinfo(struct addrinfo*);
int  WSAAPI getaddrinfo(const char*, const char*, const struct addrinfo*, struct addrinfo**);
int  WSAAPI getnameinfo(const struct sockaddr*, socklen_t, char*, DWORD, char*, DWORD, int);
#endif /*!defined(__MINGW64_VERSION_MAJOR)*/
#endif /* HAVE_IPV6 */
#endif /*GAUCHE_WINDOWS*/

/*==================================================================
 * Socket
 */

/*------------------------------------------------------------------
 * Sockaddr_storage
 */

#if !defined(HAVE_STRUCT_SOCKADDR_STORAGE) && !defined(_MSC_VER) \
    && !defined(_SS_MAXSIZE)
/* Alternative implementation in case the system doesn't provide
   sockaddr_storage.  The code is based on the reference implementation
   provided in RFC3493.
   We don't consider ss_len, for we don't care the internal structure
   of sockaddr_storage.  Only the size and alignment matters. */

#define _SS_MAXSIZE    128
#define _SS_ALIGNSIZE  (sizeof (ScmInt64))

#define _SS_PAD1SIZE   (_SS_ALIGNSIZE - sizeof(sa_family_t))
#define _SS_PAD2SIZE   (_SS_MAXSIZE - (sizeof(sa_family_t) + \
                                      _SS_PAD1SIZE + _SS_ALIGNSIZE))

struct sockaddr_storage {
    sa_family_t  ss_family;
    char      __ss_pad1[_SS_PAD1SIZE];
    ScmInt64  __ss_align;     /* force alignment */
    char      __ss_pad2[_SS_PAD2SIZE];
};

#endif /*HAVE_STRUCT_SOCKADDR_STORAGE*/


/*------------------------------------------------------------------
 * Socket address
 */

/* NB: built-in socket address structures are allocated as ATOMIC---when
   you want to extend them, be careful not to introduce sole pointers to
   allocated objects; GC will collect them prematurely.  The only
   pointer, the tagged pointer to the class, is protected since they're
   bound to global variables. */

typedef struct ScmSockAddrRec {
    SCM_HEADER;
    socklen_t addrlen;
    struct sockaddr addr;
} ScmSockAddr;

SCM_CLASS_DECL(Scm_SockAddrClass);
#define SCM_CLASS_SOCKADDR    (&Scm_SockAddrClass)
#define SCM_SOCKADDR(obj)     ((ScmSockAddr*)(obj))
#define SCM_SOCKADDRP(obj)    SCM_XTYPEP(obj, SCM_CLASS_SOCKADDR)

#define SCM_SOCKADDR_FAMILY(obj)   SCM_SOCKADDR(obj)->addr.sa_family

int    Scm_SockAddrP(ScmObj obj);
ScmObj Scm_SockAddrName(ScmSockAddr *addr);
ScmObj Scm_SockAddrFamily(ScmSockAddr *addr);
ScmObj Scm_MakeSockAddr(ScmClass *klass, struct sockaddr *addr, int len);

extern ScmGeneric Scm_GenericSockAddrName;
extern ScmGeneric Scm_GenericSockAddrFamily;
extern ScmGeneric Scm_GenericSockAddrAddr;
extern ScmGeneric Scm_GenericSockAddrPort;

typedef struct ScmSockAddrUnRec {
    SCM_HEADER;
    int addrlen;
    struct sockaddr_un addr;
} ScmSockAddrUn;

SCM_CLASS_DECL(Scm_SockAddrUnClass);
#define SCM_CLASS_SOCKADDR_UN   (&Scm_SockAddrUnClass)

typedef struct ScmSockAddrInRec {
    SCM_HEADER;
    int addrlen;
    struct sockaddr_in addr;
} ScmSockAddrIn;

SCM_CLASS_DECL(Scm_SockAddrInClass);
#define SCM_CLASS_SOCKADDR_IN   (&Scm_SockAddrInClass)

#ifdef HAVE_IPV6

typedef struct ScmSockAddrIn6Rec {
    SCM_HEADER;
    int addrlen;
    struct sockaddr_in6 addr;
} ScmSockAddrIn6;

SCM_CLASS_DECL(Scm_SockAddrIn6Class);
#define SCM_CLASS_SOCKADDR_IN6   (&Scm_SockAddrIn6Class)

#endif /* HAVE_IPV6 */

#define SCM_SOCKADDR_MAXLEN    128

extern ScmObj Scm_InetStringToAddress(const char *s, int *proto,
                                      ScmUVector *buf);
extern ScmObj Scm_InetAddressToString(ScmObj addr, int proto);


/*------------------------------------------------------------------
 * Socket
 */

typedef struct ScmSocketRec {
    SCM_HEADER;
    Socket fd;                     /* INVALID_SOCKET if closed */
    int status;
    int type;
    ScmSockAddr *address;
    ScmPort *inPort;
    ScmPort *outPort;
    ScmString *name;
#if defined(GAUCHE_WINDOWS)
    /* Save a C run-time file descriptor so that we can close it
       when the socket is closed. */
    int cfd;
#endif /*GAUCHE_WINDOWS*/
} ScmSocket;

#define SOCKET_CLOSED(fd)  ((fd) == INVALID_SOCKET)
#define SOCKET_INVALID(fd) ((fd) == INVALID_SOCKET)

enum {
    SCM_SOCKET_STATUS_NONE,
    SCM_SOCKET_STATUS_BOUND,
    SCM_SOCKET_STATUS_LISTENING,
    SCM_SOCKET_STATUS_CONNECTED,
    SCM_SOCKET_STATUS_SHUTDOWN,
    SCM_SOCKET_STATUS_CLOSED
};

SCM_CLASS_DECL(Scm_SocketClass);
#define SCM_CLASS_SOCKET   (&Scm_SocketClass)
#define SCM_SOCKET(obj)    ((ScmSocket*)obj)
#define SCM_SOCKETP(obj)   SCM_XTYPEP(obj, SCM_CLASS_SOCKET)

extern ScmObj Scm_MakeSocket(int domain, int type, int protocol);
extern ScmObj Scm_SocketShutdown(ScmSocket *s, int how);
extern ScmObj Scm_SocketClose(ScmSocket *s);

extern ScmObj Scm_SocketInputPort(ScmSocket *s, int buffered);
extern ScmObj Scm_SocketOutputPort(ScmSocket *s, int buffered);

extern ScmObj Scm_SocketBind(ScmSocket *s, ScmSockAddr *addr);
extern ScmObj Scm_SocketConnect(ScmSocket *s, ScmSockAddr *addr);
extern ScmObj Scm_SocketListen(ScmSocket *s, int backlog);
extern ScmObj Scm_SocketAccept(ScmSocket *s);

extern ScmObj Scm_SocketGetSockName(ScmSocket *s);
extern ScmObj Scm_SocketGetPeerName(ScmSocket *s);

extern ScmObj Scm_SocketSend(ScmSocket *s, ScmObj msg, int flags);
extern ScmObj Scm_SocketSendTo(ScmSocket *s, ScmObj msg, ScmSockAddr *to, int flags);
extern ScmObj Scm_SocketSendMsg(ScmSocket *s, ScmObj msg, int flags);
extern ScmObj Scm_SocketRecv(ScmSocket *s, int bytes, int flags);
extern ScmObj Scm_SocketRecvX(ScmSocket *s, ScmUVector *buf, int flags);
extern ScmObj Scm_SocketRecvFrom(ScmSocket *s, int bytes, int flags);
extern ScmObj Scm_SocketRecvFromX(ScmSocket *s, ScmUVector *buf,
                                  ScmObj addrs, int flags);

extern ScmObj Scm_SocketBuildMsg(ScmSockAddr *name, ScmVector *iov,
                                 ScmObj control, int flags,
                                 ScmUVector *buf);

extern ScmObj Scm_SocketSetOpt(ScmSocket *s, int level,
                               int option, ScmObj value);
extern ScmObj Scm_SocketGetOpt(ScmSocket *s, int level,
                               int option, int resulttype);
extern ScmObj Scm_SocketIoctl(ScmSocket *s, int requiest, ScmObj data);

/*==================================================================
 * Netdb interface
 */

/*
 * Host entry
 */
typedef struct ScmSysHostentRec {
    SCM_HEADER;
    ScmObj name;                /* Scheme string of the host */
    ScmObj aliases;             /* list of aliases */
    ScmObj addresses;           /* list of addresses */
} ScmSysHostent;

SCM_CLASS_DECL(Scm_SysHostentClass);
#define SCM_CLASS_SYS_HOSTENT  (&Scm_SysHostentClass)
#define SCM_SYS_HOSTENT(obj)   ((ScmSysHostent*)obj)
#define SCM_SYS_HOSTENT_P(obj) SCM_XTYPEP(obj, SCM_CLASS_SYS_HOSTENT)

extern ScmObj Scm_GetHostByName(const char *name);
extern ScmObj Scm_GetHostByAddr(const char *addr, int type);
extern int Scm_HostNameToAddr(const char *name, char *addrbuf, int *addrlen);

/*
 * Protocol Entry
 */

typedef struct ScmSysProtoentRec {
    SCM_HEADER;
    ScmObj name;
    ScmObj aliases;
    ScmObj proto;
} ScmSysProtoent;

SCM_CLASS_DECL(Scm_SysProtoentClass);
#define SCM_CLASS_SYS_PROTOENT  (&Scm_SysProtoentClass)
#define SCM_SYS_PROTOENT(obj)   ((ScmSysProtoent*)obj)
#define SCM_SYS_PROTOENT_P(obj) SCM_XTYPEP(obj, SCM_CLASS_SYS_PROTOENT)

extern ScmObj Scm_GetProtoByName(const char *name);
extern ScmObj Scm_GetProtoByNumber(int proto);

/*
 * Service Entry
 */

typedef struct ScmSysServentRec {
    SCM_HEADER;
    ScmObj name;
    ScmObj aliases;
    ScmObj port;
    ScmObj proto;
} ScmSysServent;

SCM_CLASS_DECL(Scm_SysServentClass);
#define SCM_CLASS_SYS_SERVENT  (&Scm_SysServentClass)
#define SCM_SYS_SERVENT(obj)   ((ScmSysServent*)obj)
#define SCM_SYS_SERVENT_P(obj) SCM_XTYPEP(obj, SCM_CLASS_SYS_SERVENT)

extern ScmObj Scm_GetServByName(const char *name, const char *proto);
extern ScmObj Scm_GetServByPort(int port, const char *proto);

/*
 * Address information
 */

#ifdef HAVE_IPV6

typedef struct ScmSysAddrinfoRec {
    SCM_HEADER;
    int flags;
    int family;
    int socktype;
    int protocol;
    socklen_t addrlen;
    ScmString *canonname;
    ScmSockAddr *addr;
} ScmSysAddrinfo;

SCM_CLASS_DECL(Scm_SysAddrinfoClass);
#define SCM_CLASS_SYS_ADDRINFO  (&Scm_SysAddrinfoClass)
#define SCM_SYS_ADDRINFO(obj)   ((ScmSysAddrinfo*)obj)
#define SCM_SYS_ADDRINFO_P(obj) SCM_XTYPEP(obj, SCM_CLASS_SYS_ADDRINFO)

extern ScmObj Scm_GetAddrinfo(const char *nodename,
                              const char *servname,
                              struct addrinfo *hints);
extern ScmObj Scm_GetNameinfo(ScmSockAddr *addr, int flags);

#define NI_MAXHOST  1025
#define NI_MAXSERV    32

#endif /* HAVE_IPV6 */

SCM_DECL_END

#endif /*GAUCHE_NET_H */
