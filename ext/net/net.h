#include <sys/types.h>
#include <sys/socket.h>
#include <netdb.h>
#include <netinet/in.h>
#include <sys/un.h>
#include <gauche.h>

/*==================================================================
 * Socket
 */

/*------------------------------------------------------------------
 * Socket address
 */



/*------------------------------------------------------------------
 * Socket
 */

typedef struct ScmSocketRec {
    SCM_HEADER;
    int fd;                     /* -1 if closed */
    int status;
    struct sockaddr *address;
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

extern ScmObj Scm_SocketBind(ScmSocket *s, ScmObj address);
extern ScmObj Scm_SocketConnect(ScmSocket *s);
extern ScmObj Scm_SocketListen(ScmSocket *s, int backlog);
extern ScmObj Scm_SocketAccept(ScmSocket *s);

extern ScmObj Scm_MakeClientSocket(ScmString *host, int port);
extern ScmObj Scm_MakeServerSocket(int port);


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

