;;;
;;; netlib.stub - network interface
;;;
;;;   Copyright (c) 2000-2015  Shiro Kawai  <shiro@acm.org>
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

(select-module gauche.net)

(inline-stub

 (declcode "#include \"gauche-net.h\"")
 (declcode "#include <gauche/class.h>")
 (declcode "#include <gauche/bignum.h>")

 (define-type <socket-address> "ScmSockAddr*" "socket address"
   "Scm_SockAddrP" "SCM_SOCKADDR")

 (define-type <socket> "ScmSocket*")
 )

;;----------------------------------------------------------
;; Socket address methods

(inline-stub

 (define-cgeneric sockaddr-name "Scm_GenericSockAddrName" (extern))
 (define-cgeneric sockaddr-family "Scm_GenericSockAddrFamily" (extern))
 (define-cgeneric sockaddr-addr "Scm_GenericSockAddrAddr" (extern))
 (define-cgeneric sockaddr-port "Scm_GenericSockAddrPort" (extern))

 (define-cmethod sockaddr-name ((addr "Scm_SockAddrClass"))
   (return '"unknown"))

 (define-cmethod sockaddr-family ((addr "Scm_SockAddrClass"))
   (return 'unknown))

 (define-cmethod sockaddr-name ((addr "Scm_SockAddrUnClass"))
   (return
    (?: (> (-> (cast ScmSockAddr* addr) addrlen) (sizeof (struct sockaddr)))
        (SCM_MAKE_STR (ref (-> (cast ScmSockAddrUn* addr) addr) sun_path))
        (SCM_MAKE_STR ""))))

 (define-cmethod sockaddr-family ((addr "Scm_SockAddrUnClass")) (return 'unix))

 (define-cmethod sockaddr-family ((addr "Scm_SockAddrInClass")) (return 'inet))

 (define-cmethod sockaddr-addr ((addr "Scm_SockAddrInClass")) ::<ulong>
   (let* ([a::ScmSockAddrIn* (cast ScmSockAddrIn* addr)])
     (return (ntohl (ref (-> a addr) sin_addr s_addr)))))

 (define-cmethod sockaddr-port ((addr "Scm_SockAddrInClass")) ::<ushort>
   (let* ([a::ScmSockAddrIn* (cast ScmSockAddrIn* addr)])
     (return (ntohs (ref (-> a addr) sin_port)))))

 (when "defined HAVE_IPV6"
   (define-cmethod sockaddr-family ((addr "Scm_SockAddrIn6Class"))
     (return 'inet6))

   (define-cmethod sockaddr-addr ((addr "Scm_SockAddrIn6Class"))
     (let* ([a::ScmSockAddrIn6* (cast ScmSockAddrIn6* addr)]
            [p::ScmUInt32*
             (cast ScmUInt32* (ref (-> a addr) sin6_addr s6_addr))]
            [n (Scm_MakeIntegerFromUI (ntohl (* (post++ p))))])
       (dotimes [i 3]
         (set! n (Scm_LogIor (Scm_Ash n 32)
                             (Scm_MakeIntegerFromUI (ntohl (* (post++ p)))))))
       (return n)))

   (define-cmethod sockaddr-port ((addr "Scm_SockAddrIn6Class")) ::<ushort>
     (let* ([a::ScmSockAddrIn6* (cast ScmSockAddrIn6* addr)])
       (return (ntohs (ref (-> a addr) sin6_port)))))
   )
 )

;; Although many systems support this feature (e.g. inet_ntop/inet_pton
;; or WSAAdressToString/WSAStringToAddress), it would be too cumbersome
;; to check availability of those and switch the implementation.  So we
;; provide them in Scheme.

;; API
(define-method sockaddr-name ((addr <sockaddr-in>))
  #"~(inet-address->string (sockaddr-addr addr) AF_INET):~(sockaddr-port addr)")

;; NB: this should be conditionally defined by cond-expand at compile-time,
;; instead of load-time dispatch.  We need to clean up cond feature management
;; more to do so.
(if (global-variable-bound? (find-module 'gauche.net) '<sockaddr-in6>)
  ;; API
  (define-method sockaddr-name ((addr <sockaddr-in6>))
    #"[~(inet-address->string (sockaddr-addr addr) AF_INET6)]:~(sockaddr-port addr)"))

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
 (if "defined(SHUT_RD) && defined(SHUD_WR) && defined(SHUT_RDWR)"
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

(define-cproc socket-ioctl (sock::<socket> request::<int> data)
  Scm_SocketIoctl)

(define-enum-conditionally SIOCGIFNAME)
(define-enum-conditionally SIOCSIFNAME)
(define-enum-conditionally SIOCGIFINDEX)
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
(declcode "extern ScmObj addrinfo_allocate(ScmClass *klass, ScmObj initargs);")

(if "defined HAVE_IPV6"
    (begin
      (define-type <sys-addrinfo> "ScmSysAddrinfo*" #f
        "SCM_SYS_ADDRINFO_P" "SCM_SYS_ADDRINFO")

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
           (.if "WORDS_BIGENDIAN"
                (+= sum (<< (* (cast u_char* wp)) 8))
                (+= sum (* (cast u_char* wp))))
           (break))
         (+= sum (* (post++ wp))))
    (set! sum (+ (>> sum 16) (logand sum #xffff)))
    (+= sum (>> sum 16))
    (set! result (cast uint16_t (lognot sum)))  ;truncate to 16bit
    (.if "!WORDS_BIGENDIAN"
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


;; Local variables:
;; mode: scheme
;; end:
