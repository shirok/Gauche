;;;
;;; libnet.scm - network interface
;;;
;;;   Copyright (c) 2000-2021  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(declare) ;; a dummy form to suppress generation of "sci" file
(define-module gauche.net
  (use scheme.list)
  (use gauche.uvector)
  (use gauche.sequence)
  (use gauche.lazy)
  (use gauche.connection)
  (export <socket> make-socket

          ;; NB: Many enum symbols are conditionally exported.  They're
          ;; handled specially in init routine.  Here's the ones
          ;; that are always exported.
          PF_UNSPEC PF_UNIX PF_INET AF_UNSPEC AF_UNIX AF_INET
          SOCK_STREAM SOCK_DGRAM SOCK_RAW
          SHUT_RD SHUT_WR SHUT_RDWR

          socket-address socket-status socket-input-port socket-output-port
          socket-shutdown socket-close socket-bind socket-connect socket-fd
          socket-listen socket-accept socket-setsockopt socket-getsockopt
          socket-getsockname socket-getpeername socket-ioctl
          socket-send socket-sendto socket-sendmsg socket-buildmsg
          socket-recv socket-recv! socket-recvfrom socket-recvfrom!
          <sockaddr> <sockaddr-in> <sockaddr-un> make-sockaddrs
          sockaddr-name sockaddr-family sockaddr-addr sockaddr-port
          make-client-socket make-server-socket make-server-sockets
          call-with-client-socket
          <sys-hostent> sys-gethostbyname sys-gethostbyaddr
          <sys-protoent> sys-getprotobyname sys-getprotobynumber
          <sys-servent> sys-getservbyname sys-getservbyport
          sys-htonl sys-htons sys-ntohl sys-ntohs
          inet-checksum
          inet-string->address inet-string->address! inet-address->string

          ;; connection protocol - autoloaded
          connection-self-address connection-peer-address
          connection-input-port connection-output-port
          connection-shutdown connection-close
          connection-address-name
          )
  )
(select-module gauche.net)

;; Some generic functions needs to be defined after libobj initialization,
;; so we delay its loading
(autoload "gauche/netutil"
          connection-self-address
          connection-peer-address
          connection-input-port
          connection-output-port
          connection-shutdown
          connection-close
          connection-address-name)

(inline-stub

 (declcode
  (.include "gauche/net.h"))

 (declare-stub-type <socket-address> "ScmSockAddr*" "socket address"
   "Scm_SockAddrP" "SCM_SOCKADDR")

 (declare-stub-type <socket> "ScmSocket*")
 )

;;----------------------------------------------------------
;; Socket address methods

(inline-stub

 (define-cgeneric sockaddr-name "Scm_GenericSockAddrName" (extern))
 (define-cgeneric sockaddr-family "Scm_GenericSockAddrFamily" (extern))
 (define-cgeneric sockaddr-addr "Scm_GenericSockAddrAddr" (extern))
 (define-cgeneric sockaddr-port "Scm_GenericSockAddrPort" (extern))

 (define-cmethod sockaddr-name ((addr "Scm_SockAddrClass"))
   (cast void addr)                     ; suppress unused var warning
   (return '"unknown"))

 (define-cmethod sockaddr-family ((addr "Scm_SockAddrClass"))
   (cast void addr)                     ; suppress unused var warning
   (return 'unknown))

 (define-cmethod sockaddr-name ((addr "Scm_SockAddrUnClass"))
   (return
    (?: (> (cast unsigned (-> (cast ScmSockAddr* addr) addrlen))
           (sizeof (struct sockaddr)))
        (SCM_MAKE_STR (ref (-> (cast ScmSockAddrUn* addr) addr) sun_path))
        (SCM_MAKE_STR ""))))

 ;; NB: We might just use inet_ntop or WSAAddressToString, but we don't bother
 ;; checking their availability and #ifdefs.
 (define-cmethod sockaddr-name ((addr "Scm_SockAddrInClass"))
   (let* ([a::ScmSockAddrIn* (cast ScmSockAddrIn* addr)]
          [addr::ulong (ntohl (ref (-> a addr) sin_addr s_addr))]
          [port::ushort (ntohs (ref (-> a addr) sin_port))]
          [buf::(.array char (10))])
     (snprintf buf 10 ":%d" port)
     (return
      (Scm_StringAppendC
       (SCM_STRING (Scm_InetAddressToString (Scm_MakeIntegerU addr) AF_INET))
       buf -1 -1))))

 (define-cmethod sockaddr-family ((addr "Scm_SockAddrUnClass"))
   (cast void addr)                     ; suppress unused var warning
   (return 'unix))

 (define-cmethod sockaddr-family ((addr "Scm_SockAddrInClass"))
   (cast void addr)                     ; suppress unused var warning
   (return 'inet))

 (define-cmethod sockaddr-addr ((addr "Scm_SockAddrInClass")) ::<ulong>
   (let* ([a::ScmSockAddrIn* (cast ScmSockAddrIn* addr)])
     (return (ntohl (ref (-> a addr) sin_addr s_addr)))))

 (define-cmethod sockaddr-port ((addr "Scm_SockAddrInClass")) ::<ushort>
   (let* ([a::ScmSockAddrIn* (cast ScmSockAddrIn* addr)])
     (return (ntohs (ref (-> a addr) sin_port)))))

 (.when (defined HAVE_IPV6)
   (define-cmethod sockaddr-family ((addr "Scm_SockAddrIn6Class"))
     (cast void addr)                     ; suppress unused var warning
     (return 'inet6))

   (define-cfn in6-addr (a::ScmSockAddrIn6*) :static
     (let* ([p::uint32_t*
             (cast uint32_t* (ref (-> a addr) sin6_addr s6_addr))]
            [n (Scm_MakeIntegerFromUI (ntohl (* (post++ p))))])
       (dotimes [i 3]
         (set! n (Scm_LogIor (Scm_Ash n 32)
                             (Scm_MakeIntegerFromUI (ntohl (* (post++ p)))))))
       (return n)))

   (define-cmethod sockaddr-addr ((addr "Scm_SockAddrIn6Class"))
     (return (in6-addr (cast ScmSockAddrIn6* addr))))

   (define-cmethod sockaddr-port ((addr "Scm_SockAddrIn6Class")) ::<ushort>
     (let* ([a::ScmSockAddrIn6* (cast ScmSockAddrIn6* addr)])
       (return (ntohs (ref (-> a addr) sin6_port)))))

   (define-cmethod sockaddr-name ((addr "Scm_SockAddrIn6Class"))
     (let* ([a::ScmSockAddrIn6* (cast ScmSockAddrIn6* addr)]
            [addr (in6-addr a)]
            [port::ushort (ntohs (ref (-> a addr) sin6_port))]
            [buf::(.array char (10))])
     (snprintf buf 10 ":%d" port)
     (return
      (Scm_StringAppendC
       (SCM_STRING (Scm_InetAddressToString addr AF_INET6))
       buf -1 -1))))
   )
 )

;;----------------------------------------------------------
;; low-level socket routines

(define-cproc make-socket (domain::<fixnum> type::<fixnum>
                                            :optional (protocol::<fixnum> 0))
  Scm_MakeSocket)

(define-enum PF_UNSPEC)
(define-enum PF_UNIX)
(define-enum PF_INET)

(define-enum AF_UNSPEC)
(define-enum AF_UNIX)
(define-enum AF_INET)

(define-enum SOCK_STREAM)
(define-enum SOCK_DGRAM)
(define-enum SOCK_RAW)

(define-enum-conditionally MSG_CTRUNC)
(define-enum-conditionally MSG_DONTROUTE)
(define-enum-conditionally MSG_EOR)
(define-enum-conditionally MSG_OOB)
(define-enum-conditionally MSG_PEEK)
(define-enum-conditionally MSG_TRUNC)
(define-enum-conditionally MSG_WAITALL)

(define-enum-conditionally IPPROTO_IP)
(define-enum-conditionally IPPROTO_ICMP)
(define-enum-conditionally IPPROTO_ICMPV6)
(define-enum-conditionally IPPROTO_TCP)
(define-enum-conditionally IPPROTO_UDP)

(define-enum-conditionally SOMAXCONN)

(define-cproc socket-address (sock::<socket>)
  (if (-> sock address)
    (return (SCM_OBJ (-> sock address)))
    (return '#f)))

(define-cproc socket-status (sock::<socket>)
  (case (-> sock status)
    [(SCM_SOCKET_STATUS_NONE)      (return 'none)]
    [(SCM_SOCKET_STATUS_BOUND)     (return 'bound)]
    [(SCM_SOCKET_STATUS_LISTENING) (return 'listening)]
    [(SCM_SOCKET_STATUS_CONNECTED) (return 'connected)]
    [(SCM_SOCKET_STATUS_SHUTDOWN)  (return 'shutdonw)]
    [(SCM_SOCKET_STATUS_CLOSED)    (return 'closed)]
    [else
     (Scm_Error "invalid state of socket %S: implementation bugs?" sock)
     (return SCM_UNDEFINED)]))

(define-cproc socket-fd (sock::<socket>) ::<long>
  (return (cast long (-> sock fd))))

;; NB: buffered? keyword args in the following two procedures are
;; deprecated; use buffering arg.
(define-cproc socket-input-port (sock::<socket>
                                 :key (buffering #f) (buffered? #f))
  (let* ([bufmode::int])
    (cond [(not (SCM_FALSEP buffered?)) ;for backward compatibility
           (set! bufmode SCM_PORT_BUFFER_FULL)]
          [else
           (set! bufmode (Scm_BufferingMode buffering
                                            SCM_PORT_INPUT
                                            SCM_PORT_BUFFER_LINE))])
    (return (Scm_SocketInputPort sock bufmode))))

(define-cproc socket-output-port (sock::<socket>
                                  :key (buffering #f) (buffered? #f))
  (let* ([bufmode::int])
    (cond [(not (SCM_FALSEP buffered?)) ;for backward compatibility
           (set! bufmode SCM_PORT_BUFFER_FULL)]
          [else
           (set! bufmode (Scm_BufferingMode buffering
                                            SCM_PORT_OUTPUT
                                            SCM_PORT_BUFFER_LINE))])
    (return (Scm_SocketOutputPort sock bufmode))))

(inline-stub
 (.if (and (defined SHUT_RD)
           (defined SHUT_WR)
           (defined SHUT_RDWR))
   (begin
     (define-enum SHUT_RD)
     (define-enum SHUT_WR)
     (define-enum SHUT_RDWR))
   (begin
     (define-constant SHUT_RD 0)
     (define-constant SHUT_WR 1)
     (define-constant SHUT_RDWR 2)))
 )

(define-cproc socket-shutdown (sock::<socket> :optional (how::<fixnum> 2))
  Scm_SocketShutdown)

(define-cproc socket-close (sock::<socket>)
  Scm_SocketClose)

(define-cproc socket-bind (sock::<socket> addr::<socket-address>)
  Scm_SocketBind)

(define-cproc socket-listen (sock::<socket> backlog::<fixnum>)
  Scm_SocketListen)

(define-cproc socket-accept (sock::<socket>)
  Scm_SocketAccept)

(define-cproc socket-connect (sock::<socket> addr::<socket-address>)
  Scm_SocketConnect)

(define-cproc socket-getsockname (sock::<socket>)
  Scm_SocketGetSockName)

(define-cproc socket-getpeername (sock::<socket>)
  Scm_SocketGetPeerName)

(define-cproc socket-send (sock::<socket> msg
                           :optional (flags::<fixnum> 0))
  Scm_SocketSend)

(define-cproc socket-sendto (sock::<socket> msg to::<socket-address>
                             :optional (flags::<fixnum> 0))
  Scm_SocketSendTo)

(define-cproc socket-sendmsg (sock::<socket> msg :optional (flags::<fixnum> 0))
  Scm_SocketSendMsg)

(define-cproc socket-recv (sock::<socket> bytes::<fixnum>
                           :optional (flags::<fixnum> 0))
  Scm_SocketRecv)

(define-cproc socket-recv! (sock::<socket> buf::<uvector>
                           :optional (flags::<fixnum> 0))
  Scm_SocketRecvX)

(define-cproc socket-recvfrom (sock::<socket> bytes::<fixnum>
                               :optional (flags::<fixnum> 0))
  Scm_SocketRecvFrom)

(define-cproc socket-recvfrom! (sock::<socket> buf::<uvector> addrs
                                :optional (flags::<fixnum> 0))
  Scm_SocketRecvFromX)

;; struct msghdr builder
(define-cproc socket-buildmsg (name::<socket-address>?
                               iov::<vector>?
                               control  ;list of (level type data)
                               flags::<int>
                               :optional (buf::<uvector>? #f))
  Scm_SocketBuildMsg)

;; socket option interface
(define-cproc socket-setsockopt (sock::<socket>
                                 level::<fixnum> option::<fixnum> value)
  Scm_SocketSetOpt)

(define-cproc socket-getsockopt (sock::<socket>
                                 level::<fixnum> option::<fixnum>
                                 rsize::<fixnum>)
  Scm_SocketGetOpt)

(define-enum-conditionally SOL_SOCKET)
(define-enum-conditionally SO_ACCEPTCONN)
(define-enum-conditionally SO_BINDTODEVICE)
(define-enum-conditionally SO_BROADCAST)
(define-enum-conditionally SO_DEBUG)
(define-enum-conditionally SO_DONTROUTE)
(define-enum-conditionally SO_ERROR)
(define-enum-conditionally SO_KEEPALIVE)
(define-enum-conditionally SO_LINGER)
(define-enum-conditionally SO_OOBINLINE)
(define-enum-conditionally SO_PASSCRED)
(define-enum-conditionally SO_PEERCRED)
(define-enum-conditionally SO_PRIORITY)
(define-enum-conditionally SO_RCVBUF)
(define-enum-conditionally SO_RCVLOWAT)
(define-enum-conditionally SO_RCVTIMEO)
(define-enum-conditionally SO_REUSEADDR)
(define-enum-conditionally SO_REUSEPORT)
(define-enum-conditionally SO_SNDBUF)
(define-enum-conditionally SO_SNDLOWAT)
(define-enum-conditionally SO_SNDTIMEO)
(define-enum-conditionally SO_TIMESTAMP)
(define-enum-conditionally SO_TYPE)

(define-enum-conditionally SOL_TCP)
(define-enum-conditionally TCP_NODELAY)
(define-enum-conditionally TCP_MAXSEG)
(define-enum-conditionally TCP_CORK)

(define-enum-conditionally SOL_IP)
(define-enum-conditionally IP_OPTIONS)
(define-enum-conditionally IP_PKTINFO)
(define-enum-conditionally IP_RECVTOS)
(define-enum-conditionally IP_RECVTTL)
(define-enum-conditionally IP_RECVOPTS)
(define-enum-conditionally IP_TOS)
(define-enum-conditionally IP_TTL)
(define-enum-conditionally IP_HDRINCL)
(define-enum-conditionally IP_RECVERR)
(define-enum-conditionally IP_MTU_DISCOVER)
(define-enum-conditionally IP_MTU)
(define-enum-conditionally IP_ROUTER_ALERT)
(define-enum-conditionally IP_MULTICAST_TTL)
(define-enum-conditionally IP_MULTICAST_LOOP)
(define-enum-conditionally IP_ADD_MEMBERSHIP)
(define-enum-conditionally IP_DROP_MEMBERSHIP)
(define-enum-conditionally IP_MULTICAST_IF)

;; network device access
;; this is pretty much Linux-specific.

(define-cproc socket-ioctl (sock::<socket> request::<integer> data)
  (return (Scm_SocketIoctl sock (Scm_GetIntegerU request) data)))

(define-enum-conditionally SIOCGIFNAME)
(define-enum-conditionally SIOCSIFNAME)
(define-enum-conditionally SIOCGIFINDEX)
(define-enum-conditionally SIOCGIFADDR)
(define-enum-conditionally SIOCSIFADDR)
(define-enum-conditionally SIOCGIFDSTADDR)
(define-enum-conditionally SIOCSIFDSTADDR)
(define-enum-conditionally SIOCGIFBRDADDR)
(define-enum-conditionally SIOCSIFBRDADDR)
(define-enum-conditionally SIOCGIFNETMASK)
(define-enum-conditionally SIOCSIFNETMASK)
(define-enum-conditionally SIOCGIFFLAGS)
(define-enum-conditionally SIOCSIFFLAGS)
(define-enum-conditionally SIOCGIFMETRIC)
(define-enum-conditionally SIOCSIFMETRIC)
(define-enum-conditionally SIOCGIFMTU)
(define-enum-conditionally SIOCSIFMTU)
(define-enum-conditionally SIOCGIFHWADDR)
(define-enum-conditionally SIOCSIFHWADDR)
(define-enum-conditionally SIOCSIFHWBROADCAST)
(define-enum-conditionally SIOCGIFMAP)
(define-enum-conditionally SIOCSIFMAP)
(define-enum-conditionally SIOCADDMULTI)
(define-enum-conditionally SIOCDELMULTI)
(define-enum-conditionally SIOGIFTXQLEN)
(define-enum-conditionally SIOSIFTXQLEN)
(define-enum-conditionally SIOCGIFCONF)

(define-enum-conditionally IFF_UP)
(define-enum-conditionally IFF_BROADCAST)
(define-enum-conditionally IFF_DEBUG)
(define-enum-conditionally IFF_LOOPBACK)
(define-enum-conditionally IFF_POINTTOPOINT)
(define-enum-conditionally IFF_RUNNING)
(define-enum-conditionally IFF_NOARP)
(define-enum-conditionally IFF_PROMISC)
(define-enum-conditionally IFF_NOTRAILERS)
(define-enum-conditionally IFF_ALLMULTI)
(define-enum-conditionally IFF_MASTER)
(define-enum-conditionally IFF_SLAVE)
(define-enum-conditionally IFF_MULTICAST)
(define-enum-conditionally IFF_PORTSEL)
(define-enum-conditionally IFF_AUTOMEDIA)
(define-enum-conditionally IFF_DYNAMIC)

;;----------------------------------------------------------
;; netdb routines

(define-cproc sys-gethostbyname (name::<const-cstring>)
  Scm_GetHostByName)

(define-cproc sys-gethostbyaddr (addr::<const-cstring> type::<fixnum>)
  Scm_GetHostByAddr)

(define-cproc sys-getprotobyname (name::<const-cstring>)
  Scm_GetProtoByName)

(define-cproc sys-getprotobynumber (number::<fixnum>)
  Scm_GetProtoByNumber)

(define-cproc sys-getservbyname (name::<const-cstring> proto::<const-cstring>)
  Scm_GetServByName)

(define-cproc sys-getservbyport (port::<fixnum> proto::<const-cstring>)
  Scm_GetServByPort)

(define-cproc sys-ntohl (x::<uint32>) ::<uint32> ntohl)
(define-cproc sys-ntohs (x::<uint16>) ::<uint16> ntohs)
(define-cproc sys-htonl (x::<uint32>) ::<uint32> htonl)
(define-cproc sys-htons (x::<uint16>) ::<uint16> htons)

;;----------------------------------------------------------
;; IPv6 routines

(inline-stub
 (declare-cfn addrinfo_allocate (klass::ScmClass* intargs))

 (.if (defined HAVE_IPV6)
    (begin
      (define-cclass <sys-addrinfo>
        "ScmSysAddrinfo*" "Scm_SysAddrinfoClass"
        ()
        ((flags :type <int>)
         (family :type <int>)
         (socktype :type <int>)
         (protocol :type <int>)
         (addrlen :type <uint32>)
         (canonname :type <string>)
         (addr :setter "  if (!SCM_SOCKADDRP(value)) Scm_Error(\"ScmSockAddr* required, but got %S\", value);
  obj->addr = SCM_SOCKADDR(value);"))
        (allocator (c "addrinfo_allocate")))

      (define-cproc sys-getaddrinfo (nodename::<const-cstring>?
                                     servname::<const-cstring>?
                                     hints)
        (let* ([ai::(struct addrinfo)])
          (unless (or (SCM_SYS_ADDRINFO_P hints) (SCM_FALSEP hints))
            (SCM_TYPE_ERROR hints "<sys-addrinfo> or #f"))
          (unless (SCM_FALSEP hints)
            (memset (& ai) 0 (sizeof ai))
            (set! (ref ai ai_flags)  (-> (SCM_SYS_ADDRINFO hints) flags)
                  (ref ai ai_family) (-> (SCM_SYS_ADDRINFO hints) family)
                  (ref ai ai_socktype) (-> (SCM_SYS_ADDRINFO hints) socktype)
                  (ref ai ai_protocol) (-> (SCM_SYS_ADDRINFO hints) protocol)))
          (return (Scm_GetAddrinfo nodename servname
                                   (?: (SCM_FALSEP hints) NULL (& ai))))))

      (define-cproc sys-getnameinfo
        (addr::<socket-address> :optional flags::<fixnum>)
        Scm_GetNameinfo)
      ))
)

(define-enum-conditionally AF_INET6)
(define-enum-conditionally PF_INET6)

(define-enum-conditionally IPPROTO_IPV6)
(define-enum-conditionally IPV6_UNICAST_HOPS)
(define-enum-conditionally IPV6_MULTICAST_IF)
(define-enum-conditionally IPV6_MULTICAST_HOPS)
(define-enum-conditionally IPV6_MULTICAST_LOOP)
(define-enum-conditionally IPV6_JOIN_GROUP)
(define-enum-conditionally IPV6_LEAVE_GROUP)
(define-enum-conditionally IPV6_V6ONLY)

(define-enum-conditionally AI_PASSIVE)
(define-enum-conditionally AI_CANONNAME)
(define-enum-conditionally AI_NUMERICHOST)
(define-enum-conditionally AI_NUMERICSERV)
(define-enum-conditionally AI_V4MAPPED)
(define-enum-conditionally AI_ALL)
(define-enum-conditionally AI_ADDRCONFIG)

(define-enum-conditionally NI_NOFQDN)
(define-enum-conditionally NI_NUMERICHOST)
(define-enum-conditionally NI_NAMEREQD)
(define-enum-conditionally NI_NUMERICSERV)
(define-enum-conditionally NI_DGRAM)

;;----------------------------------------------------------
;; Conditionally export symbols
;;
;; We can't simply use export-if-defined, for it checks binding at
;; macro-expansion time.  In our case, symbols are defined by the
;; C initialization routine, which is after macros are expanded.

(inline-stub
 (define-cise-stmt export-conditionally
   [(_ . syms)
    `(let* ([mod::ScmModule* (Scm_CurrentModule)])
       ,@(map (^[sym]
                `(unless (SCM_UNBOUNDP (Scm_GlobalVariableRef mod
                                                              (SCM_SYMBOL ',sym)
                                                              0))
                   (Scm_ExportSymbols mod (list ',sym))))
              syms))])

 (define-cfn export-bindings () ::void :static
   (export-conditionally
    IPPROTO_IP IPPROTO_ICMP IPPROTO_TCP
    IPPROTO_UDP IPPROTO_IPV6 IPPROTO_ICMPV6 SOL_SOCKET SOMAXCONN
    SO_ACCEPTCONN SO_BINDTODEVICE SO_BROADCAST SO_DEBUG
    SO_DONTROUTE SO_ERROR SO_KEEPALIVE SO_LINGER SO_OOBINLINE
    SO_PASSCRED SO_PEERCRED SO_PRIORITY SO_RCVBUF SO_RCVLOWAT
    SO_RCVTIMEO SO_REUSEADDR SO_REUSEPORT SO_SNDBUF SO_SNDLOWAT
    SO_SNDTIMEO SO_TIMESTAMP SO_TYPE
    SOL_TCP TCP_NODELAY TCP_MAXSEG TCP_CORK
    SOL_IP IP_OPTIONS
    IP_PKTINFO IP_RECVTOS IP_RECVTTL IP_RECVOPTS IP_TOS
    IP_TTL IP_HDRINCL IP_RECVERR IP_MTU_DISCOVER IP_MTU
    IP_ROUTER_ALERT IP_MULTICAST_TTL IP_MULTICAST_LOOP
    IP_ADD_MEMBERSHIP IP_DROP_MEMBERSHIP IP_MULTICAST_IF
    MSG_CTRUNC MSG_DONTROUTE MSG_EOR MSG_OOB MSG_PEEK MSG_TRUNC
    MSG_WAITALL

    ;; Netdevice control.  OS specific.
    SIOCGIFNAME SIOCSIFNAME SIOCGIFINDEX SIOCGIFFLAGS SIOCSIFFLAGS
    SIOCGIFMETRIC SIOCSIFMETRIC SIOCGIFMTU SIOCSIFMTU
    SIOCGIFHWADDR SIOCSIFHWADDR SIOCSIFHWBROADCAST
    SIOCGIFMAP SIOCSIFMAP SIOCADDMULTI SIOCDELMULTI
    SIOGIFTXQLEN SIOSIFTXQLEN SIOCGIFCONF
    SIOCGIFADDR SIOCSIFADDR SIOCGIFDSTADDR SIOCSIFDSTADDR
    SIOCGIFBRDADDR SIOCSIFBRDADDR SIOCGIFNETMASK SIOCSIFNETMASK

    IFF_UP IFF_BROADCAST IFF_DEBUG IFF_LOOPBACK IFF_POINTTOPOINT
    IFF_RUNNING IFF_NOARP IFF_PROMISC IFF_NOTRAILERS IFF_ALLMULTI
    IFF_MASTER IFF_SLAVE IFF_MULTICAST IFF_PORTSEL IFF_AUTOMEDIA
    IFF_DYNAMIC

    ;; if ipv6 is supported, these symbols are defined in the C routine.
    PF_INET6 AF_INET6
    <sockaddr-in6> <sys-addrinfo> sys-getaddrinfo make-sys-addrinfo
    AI_PASSIVE AI_CANONNAME AI_NUMERICHOST AI_NUMERICSERV
    AI_V4MAPPED AI_ALL AI_ADDRCONFIG
    IPV6_UNICAST_HOPS IPV6_MULTICAST_IF IPV6_MULTICAST_HOPS
    IPV6_MULTICAST_LOOP IPV6_JOIN_GROUP IPV6_LEAVE_GROUP IPV6_V6ONLY
    sys-getnameinfo
    NI_NOFQDN NI_NUMERICHOST NI_NAMEREQD NI_NUMERICSERV NI_DGRAM)
   )

 (initcode "export_bindings();"))

;;----------------------------------------------------------
;; Internet checksum (RFC1071)

;; NB: This checksum routine, a bit of modification from RFC1071,
;; seems widely used.  In ordinary C programs you don't need to worry
;; about endianness, since even if the machine is little-endian,
;; the swapping happens twice (once when you fetch each 16-bit word,
;; and once when you store the result) so the result becomes
;; the same as big-endian calculation.  In our case we take over
;; the result into Scheme world, so we adjust the result to the
;; network byte order (big-endian).
(define-cproc inet-checksum (buf::<uvector> size::<int>) ::<uint>
  (let* ([wp::uint16_t* (cast uint16_t* (SCM_UVECTOR_ELEMENTS buf))]
         [sum::u_long 0]
         [result::uint16_t])
    (when (> size (Scm_UVectorSizeInBytes buf))
      (Scm_Error "uniform vector buffer too short: %S" buf))
    (for [() (> size 0) (-= size 2)]
         (when (== size 1)
           (.if WORDS_BIGENDIAN
                (+= sum (<< (* (cast u_char* wp)) 8))
                (+= sum (* (cast u_char* wp))))
           (break))
         (+= sum (* (post++ wp))))
    (set! sum (+ (>> sum 16) (logand sum #xffff)))
    (+= sum (>> sum 16))
    (set! result (cast uint16_t (lognot sum)))  ;truncate to 16bit
    (.unless WORDS_BIGENDIAN
         (set! result (logior (>> result 8) (<< result 8))))
    (return result)))

;; socket addresses
(define-cproc inet-string->address (s::<const-cstring>) ::(<top> <top>)
  (let* ([proto::int] [r (Scm_InetStringToAddress s (& proto) NULL)])
    (if (SCM_FALSEP r)
      (return SCM_FALSE SCM_FALSE)
      (return r (SCM_MAKE_INT proto)))))

(define-cproc inet-string->address! (s::<const-cstring> buf::<uvector>)
  (let* ([proto::int] [r (Scm_InetStringToAddress s (& proto) buf)])
    (if (SCM_FALSEP r) (return SCM_FALSE) (return (SCM_MAKE_INT proto)))))

(define-cproc inet-address->string (addr proto::<int>) Scm_InetAddressToString)

;; default backlog value for socket-listen
(define-constant DEFAULT_BACKLOG 5)

;; NB: we can't use (cond-expand (gauche.net.ipv6 ...) ) here, since
;; cond-expand is expanded when netaux.scm is compiled, but at that time
;; the feature 'gauche.net.ipv6' is not available since the gauche.net module
;; is not yet built.  So we use a bit of kludge here.
(define ipv6-capable (global-variable-bound? 'gauche.net 'sys-getaddrinfo))

;; NB: ipv4 preference setting for the compatibility to old windows installer.
;; if #t, make-sockaddrs returns ipv4 socket addresses before ipv6 ones.
(define ipv4-preferred (cond-expand [gauche.os.windows #t] [else #f]))

;; API
(define (make-sys-addrinfo :key (flags 0) (family AF_UNSPEC)
                                (socktype 0) (protocol 0))
  (if ipv6-capable
    (make <sys-addrinfo>
      :flags (if (list? flags) (apply logior flags) flags)
      :family family :socktype socktype :protocol protocol)
    (error "make-sys-addrinfo is available on IPv6-enabled platform")))

;; Utility
(define (address->protocol-family addr)
  (case (sockaddr-family addr)
    [(unix)  PF_UNIX]
    [(inet)  PF_INET]
    [(inet6) PF_INET6] ;;this can't happen if !ipv6-capable
    [else (error "unknown family of socket address" addr)]))

;; API
;; High-level interface.  We need some hardcoded heuristics here.
(define (make-client-socket proto . args)
  (cond [(eq? proto 'unix)
         (let-optionals* args ([path #f])
           (unless (string? path)
             (error "unix socket requires pathname, but got" path))
           (make-client-socket-unix path))]
        [(eq? proto 'inet)
         (let-optionals* args ([host #f] [port #f])
           (unless (and (string? host) (or (integer? port) (string? port)))
             (errorf "inet socket requires host name and port, but got ~s and ~s"
                     host port))
           (make-client-socket-inet host port))]
        [(is-a? proto <sockaddr>)
         ;; caller provided sockaddr
         (make-client-socket-from-addr proto)]
        [(and (string? proto)
              (pair? args)
              (integer? (car args)))
         ;; STk compatibility
         (make-client-socket-inet proto (car args))]
        [else
         (error "unsupported protocol:" proto)]))

(define (make-client-socket-from-addr addr)
  (rlet1 socket (make-socket (address->protocol-family addr) SOCK_STREAM)
    (socket-connect socket addr)))

(define (make-client-socket-unix path)
  (rlet1 socket (make-socket PF_UNIX SOCK_STREAM)
    (socket-connect socket (make <sockaddr-un> :path path))))

(define (make-client-socket-inet host port)
  (let1 err #f
    (define (try-connect address)
      (guard (e [else (set! err e) #f])
        (rlet1 socket (make-socket (address->protocol-family address)
                                  SOCK_STREAM)
          (socket-connect socket address))))
    (rlet1 socket (any try-connect (make-sockaddrs host port))
      (unless socket (raise err)))))

;; API
(define (make-server-socket proto . args)
  (cond [(eq? proto 'unix)
         (let-optionals* args ([path #f])
           (unless (string? path)
             (error "unix socket requires pathname, but got" path))
           (apply make-server-socket-unix path (cdr args)))]
        [(eq? proto 'inet)
         (let-optionals* args ([port #f])
           (define (err)
             (error "inet socket requires integer port number or \
                     string service name, or a list of them, but got:" port))
           (cond [(list? port)
                  (unless (every (any-pred integer? string?) port) (err))
                  (apply make-server-socket-inet* port (cdr args))]
                 [(or (integer? port) (string? port))
                  (apply make-server-socket-inet port (cdr args))]
                 [else (err)]))]
        [(is-a? proto <sockaddr>)
         ;; caller provided sockaddr
         (apply make-server-socket-from-addr proto args)]
        [(integer? proto)
         ;; STk compatibility
         (apply make-server-socket-inet proto args)]
        [else
         (error "unsupported protocol:" proto)]))

(define (make-server-socket-from-addr addr :key (reuse-addr? #f)
                                                (sock-init #f)
                                                (backlog DEFAULT_BACKLOG))
  (rlet1 socket (make-socket (address->protocol-family addr) SOCK_STREAM)
    (when (procedure? sock-init)
      (sock-init socket addr))
    (when reuse-addr?
      (socket-setsockopt socket SOL_SOCKET SO_REUSEADDR 1))
    (socket-bind socket addr)
    (socket-listen socket backlog)))


(define (make-server-socket-unix path :key (backlog DEFAULT_BACKLOG))
  (rlet1 socket (make-socket PF_UNIX SOCK_STREAM)
    (socket-bind socket (make <sockaddr-un> :path path))
    (socket-listen socket backlog)))

(define (make-server-socket-inet port . args)
  (apply make-server-socket-from-addr (car (make-sockaddrs #f port)) args))

(define (make-server-socket-inet* ports . args) ; taking multiple ports
  (let1 err #f
    (define (try-bind address)
      (guard (e [else (set! err e) #f])
        (apply make-server-socket-from-addr address args)))
    (rlet1 socket (any try-bind ($ lconcatenate $ lmap
                                   (cut make-sockaddrs #f <>)
                                   ports))
      (unless socket (raise err)))))

;; API
;; Listen both v4 and v6 sockets, unless the system supports dual-stack
;; socket.
;; Heuristics - if we have both v4 and v6 sockets, we *may* need
;; only v6 sockets if the system defaults to dual-stack socket.
;; Unfortunately the behavior is system dependent.  So we try to
;; open both (first v6, then v4) and if the latter fails to bind
;; we assume v6 socket listens both.
(define (make-server-sockets host port . args)
  (define (v4addrs ss)
    (filter (^s (eq? (sockaddr-family s) 'inet)) ss))
  (define (v6addrs ss)
    (filter (^s (eq? (sockaddr-family s) 'inet6)) ss))

  ;; Kludge: These may not be bound on certain platforms,
  ;; so we look them up at runtime.
  (define EADDRINUSE
    (global-variable-ref (find-module 'gauche) 'EADDRINUSE #f))
  (define EADDRNOTAVAIL
    (global-variable-ref (find-module 'gauche) 'EADDRNOTAVAIL #f))
  (define <sockaddr-in6>
    (global-variable-ref (find-module 'gauche.net) '<sockaddr-in6> #f))

  (define (bind-failed? e)
    (and (<system-error> e)
         (memv (~ e'errno) `(,EADDRINUSE ,EADDRNOTAVAIL))))

  ;; try binding v4 socket with the same port of opened v6 socket S6.
  ;; Returns (S6 S4) on success, or (S6) on failure.
  ;; NB: It is possible that v4's port is taken by another process,
  ;; instead of dual-stack S6 socket.
  (define (try-v4 s6 addrs)
    ;; If the original port argument is 0, we take port number from
    ;; the opened v6 socket.
    (let1 a4s (if (zero? port)
                ($ v4addrs $ make-sockaddrs host
                   $ sockaddr-port $ socket-address s6)
                (v4addrs addrs))
      (guard (e [(bind-failed? e) (list s6)]
                [else (raise e)])
        (cons s6 (filter-map (cut apply make-server-socket <> args) a4s)))))

  ;; try binding v6 socket on addr.  If actual-port is not #f, reallocate
  ;; addr with the given port.  It is for the case that the given port is 0.
  (define (try-v6 addr actual-port)
    (let1 addr (if actual-port
                 (make <sockaddr-in6>
                   :host (sockaddr-addr addr) :port actual-port)
                 addr)
      (guard (e [(bind-failed? e) (values #f actual-port)])
        (let1 s6 (apply make-server-socket addr args)
          (values s6
                  (if (zero? port)
                    (sockaddr-port (socket-address s6))
                    #f))))))

  ;; Bind multiple v6 sockaddrs.
  ;; If PORT is zero, we have to use the actual port number of the first
  ;; socket we can bind.  So it's more involved than simply mapping
  ;; make-server-socket.
  (define (make-v6socks a6s)
    (receive (socks _) (map-accum try-v6 #f a6s)
      (filter identity socks)))

  (let* ([ss (make-sockaddrs host port)]
         [a6s (v6addrs ss)])
    ;; NB: Mingw doesn't have EADDRINUSE.  it's likely not to have ipv6 either,
    ;; so we just use the default.  NB: we can't switch here with
    ;; gauche.sys.ipv6; see the comment on ipv6-capable definition above.
    (if (or (null? a6s)
            (not EADDRINUSE)
            (not <sockaddr-in6>))
      (map (cut apply make-server-socket <> args) ss)
      (append-map (cut try-v4 <> ss) (make-v6socks (v6addrs ss))))))

;; API
(define (make-sockaddrs host port :optional (proto 'tcp))
  (if ipv6-capable
    (let* ([socktype (case proto
                       [(tcp) SOCK_STREAM]
                       [(udp) SOCK_DGRAM]
                       [else (error "unsupported protocol:" proto)])]
           [port (x->string port)]
           [hints (make-sys-addrinfo :flags AI_PASSIVE :socktype socktype)]
           [ss (map (cut slot-ref <> 'addr) (sys-getaddrinfo host port hints))])
      (if ipv4-preferred
        (append (filter (^s (eq? (sockaddr-family s) 'inet)) ss)
                (remove (^s (eq? (sockaddr-family s) 'inet)) ss))
        ss))
    (let1 port (cond [(number? port) port]
                     [(sys-getservbyname port (symbol->string proto))
                      => (cut slot-ref <> 'port)]
                     [else
                      (error "couldn't find a port number of service:" port)])
      (if host
        (let1 hh (sys-gethostbyname host)
          (unless hh (error "couldn't find host: " host))
          (map (cut make <sockaddr-in> :host <> :port port)
               (slot-ref hh 'addresses)))
        (list (make <sockaddr-in> :host :any :port port))))))

;; API
(define (call-with-client-socket socket proc
                                 :key (input-buffering #f) (output-buffering #f))
  (unwind-protect
      (proc (if input-buffering
              (socket-input-port socket :buffering input-buffering)
              (socket-input-port socket))
            (if output-buffering
              (socket-output-port socket :buffering output-buffering)
              (socket-output-port socket)))
    (socket-close socket)))

;; Local variables:
;; mode: scheme
;; end:
