;;;
;;; net - network interface
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

#!no-fold-case

(define-module gauche.net
  (use srfi-1)
  (use gauche.uvector)
  (use gauche.sequence)
  (use gauche.lazy)
  (export <socket> make-socket
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
          )
  )

(select-module gauche.net)

(dynamic-load "gauche--net")

(export-if-defined
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
 MSG_WAITALL)

;; Netdevice control.  OS specific.
(export-if-defined
 SIOCGIFNAME SIOCSIFNAME SIOCGIFINDEX SIOCGIFFLAGS SIOCSIFFLAGS
 SIOCGIFMETRIC SIOCSIFMETRIC SIOCGIFMTU SIOCSIFMTU
 SIOCGIFHWADDR SIOCSIFHWADDR SIOCSIFHWBROADCAST
 SIOCGIFMAP SIOCSIFMAP SIOCADDMULTI SIOCDELMULTI
 SIOGIFTXQLEN SIOSIFTXQLEN SIOCGIFCONF

 IFF_UP IFF_BROADCAST IFF_DEBUG IFF_LOOPBACK IFF_POINTTOPOINT
 IFF_RUNNING IFF_NOARP IFF_PROMISC IFF_NOTRAILERS IFF_ALLMULTI
 IFF_MASTER IFF_SLAVE IFF_MULTICAST IFF_PORTSEL IFF_AUTOMEDIA
 IFF_DYNAMIC)

;; if ipv6 is supported, these symbols are defined in the C routine.

(export-if-defined
 PF_INET6 AF_INET6
 <sockaddr-in6> <sys-addrinfo> sys-getaddrinfo make-sys-addrinfo
 AI_PASSIVE AI_CANONNAME AI_NUMERICHOST AI_NUMERICSERV
 AI_V4MAPPED AI_ALL AI_ADDRCONFIG
 IPV6_UNICAST_HOPS IPV6_MULTICAST_IF IPV6_MULTICAST_HOPS
 IPV6_MULTICAST_LOOP IPV6_JOIN_GROUP IPV6_LEAVE_GROUP IPV6_V6ONLY
 sys-getnameinfo
 NI_NOFQDN NI_NUMERICHOST NI_NAMEREQD NI_NUMERICSERV NI_DGRAM)

