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
 *  $Id: net.h,v 1.5 2001-06-13 19:56:53 shirok Exp $
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

extern ScmClass Scm_SockAddrClass;
#define SCM_CLASS_SOCKADDR    (&Scm_SockAddrClass)
#define SCM_SOCKADDR(obj)     ((ScmSockAddr*)(obj))
#define SCM_SOCKADDRP(obj)    SCM_XTYPEP(obj, SCM_CLASS_SOCKADDR)

#define SCM_SOCKADDR_FAMILY(obj)   SCM_SOCKADDR(obj)->addr.sa_family

int    Scm_SockAddrP(ScmObj obj);
ScmObj Scm_SockAddrName(ScmSockAddr *addr);
ScmObj Scm_SockAddrFamily(ScmSockAddr *addr);
ScmObj Scm_MakeSockAddr(ScmClass *klass, struct sockaddr *addr, int len);

#define SCM_SOCKADDR_MAXLEN    128

/*------------------------------------------------------------------
 * Socket
 */

typedef struct ScmSocketRec {
    SCM_HEADER;
    int fd;                     /* -1 if closed */
    int status;
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

extern ScmClass Scm_SocketClass;
#define SCM_CLASS_SOCKET   (&Scm_SocketClass)
#define SCM_SOCKET(obj)    ((ScmSocket*)obj)
#define SCM_SOCKETP(obj)   SCM_XTYPEP(obj, SCM_CLASS_SOCKET)

extern ScmObj Scm_MakeSocket(int domain, int type, int protocol);
extern ScmObj Scm_SocketShutdown(ScmSocket *s, int how);
extern ScmObj Scm_SocketClose(ScmSocket *s);

extern ScmObj Scm_SocketInputPort(ScmSocket *s);
extern ScmObj Scm_SocketOutputPort(ScmSocket *s);

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
 * Protcol Entry
 */

typedef struct ScmSysProtoEntRec {
    SCM_HEADER;
    struct protoent entry;
} ScmSysProtoEnt;

extern ScmClass Scm_ProtoEntClass;
#define SCM_CLASS_PROTOENT  (&Scm_ProtoEntClass)
#define SCM_PROTOENT(obj)   ((ScmProtoEnt*)obj)
#define SCM_PROTOENTP(obj)  SCM_XTYPEP(obj, SCM_CLASS_PROTOENT)

#ifdef __cplusplus
}
#endif

#endif /*GAUCHE_NET_H */
