/*
 * net.h - network interface
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
 *  $Id: net.h,v 1.12 2003-01-04 23:51:48 shirok Exp $
 */

#ifndef GAUCHE_NET_H
#define GAUCHE_NET_H

#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <sys/un.h>
#include <gauche.h>
#include <errno.h>
#include "netconfig.h"

#ifdef HAVE_RPC_TYPES_H
#include <rpc/types.h>
#endif

#ifdef HAVE_STDINT_H
#include <stdint.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

/*==================================================================
 * Socket
 */

/*------------------------------------------------------------------
 * Socket address
 */

typedef struct ScmSockAddrRec {
    SCM_HEADER;
    int addrlen;
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

#define SCM_SOCKADDR_MAXLEN    128

/*------------------------------------------------------------------
 * Socket
 */

typedef struct ScmSocketRec {
    SCM_HEADER;
    int fd;                     /* -1 if closed */
    int status;
    int type;
    ScmSockAddr *address;
    ScmPort *inPort;
    ScmPort *outPort;
    ScmString *name;
} ScmSocket;

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

extern ScmObj Scm_SocketSetOpt(ScmSocket *s, int level,
                               int option, ScmObj value);
extern ScmObj Scm_SocketGetOpt(ScmSocket *s, int level,
                               int option, int resulttype);
    
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
#define SCM_SYS_HOSTENT        ((ScmHostent*)obj)
#define SCM_SYS_HOSTENT_P      SCM_XTYPEP(obj, SCM_CLASS_SYS_HOSTENT)

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

#ifdef __cplusplus
}
#endif

#endif /*GAUCHE_NET_H */
