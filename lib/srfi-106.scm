;;;
;;; srfi-106 - socket interface
;;;

;; NB: Test for srfi-106 is in ext/net.

(define-module srfi-106
  (use gauche.uvector)
  (use gauche.net :prefix net:)
  (export make-client-socket make-server-socket socket? call-with-socket
          socket-input-port socket-output-port
          socket-merge-flags socket-purge-flags
          socket-accept socket-send socket-recv socket-shutdown socket-close
          *af-unspec* *af-inet* *af-inet6*
          *sock-stream* *sock-dgram*
          *ai-canonname* *ai-numerichost*
          *ai-v4mapped* *ai-all* *ai-addrconfig*
          *ipproto-ip* *ipproto-tcp* *ipproto-udp*
          *msg-none* *msg-peek* *msg-oob* *msg-waitall*
          *shut-rd* *shut-wr* *shut-rdwr*
          address-family socket-domain address-info
          ip-protocol message-type shutdown-method))
(select-module srfi-106)

;;
;; Constants and flag operations
;;
(define-constant *af-inet*        net:AF_INET)
(define-constant *af-inet6*       net:AF_INET6)
(define-constant *af-unspec*      net:AF_UNSPEC)

(define-macro (address-family name)
  (ecase (unwrap-syntax name)
    [(inet)   *af-inet*]
    [(inet6)  *af-inet6*]
    [(unspec) *af-unspec*]))

(define-constant *sock-stream*    net:SOCK_STREAM)
(define-constant *sock-dgram*     net:SOCK_DGRAM)

(define-macro (socket-domain name)
  (ecase (unwrap-syntax name)
    [(stream)   *sock-stream*]
    [(datagram) *sock-dgram*]))

(define-constant *ai-canonname*   net:AI_CANONNAME)
(define-constant *ai-numerichost* net:AI_NUMERICHOST)
;; NB: AI_V4MAPPED, AI_ALL and AI_ADDRCONFIG may not be defined
;;     even if ipv6 is available (e.g. NetBSD, MinGW32).
(define-constant *ai-v4mapped*
  (global-variable-ref 'gauche.net 'AI_V4MAPPED 0))
(define-constant *ai-all*
  (global-variable-ref 'gauche.net 'AI_ALL 0))
(define-constant *ai-addrconfig*
  (global-variable-ref 'gauche.net 'AI_ADDRCONFIG 0))

(define-macro (address-info . names)
  (define (lookup name)
    (ecase (unwrap-syntax name)
      [(canoname)    *ai-canonname*]
      [(numerichost) *ai-numerichost*]
      [(v4mapped)    *ai-v4mapped*]
      [(all)         *ai-all*]
      [(addrconfig)  *ai-addrconfig*]))
  (apply logior (map lookup names)))

(define-constant *ipproto-ip*     net:IPPROTO_IP)
(define-constant *ipproto-tcp*    net:IPPROTO_TCP)
(define-constant *ipproto-udp*    net:IPPROTO_UDP)

(define-macro (ip-protocol name)
  (ecase (unwrap-syntax name)
    [(ip)  *ipproto-ip*]
    [(tcp) *ipproto-tcp*]
    [(udp) *ipproto-udp*]))

(define-constant *msg-none*       0)
(define-constant *msg-peek*       net:MSG_PEEK)
(define-constant *msg-oob*        net:MSG_OOB)
;; NB: MSG_WAITALL may not be defined (e.g. MinGW32).
(define-constant *msg-waitall*
  (global-variable-ref 'gauche.net 'MSG_WAITALL 0))

(define-macro (message-type . names)
  (define (lookup name)
    (ecase (unwrap-syntax name)
      [(none)      *msg-none*]
      [(peek)      *msg-peek*]
      [(oob)       *msg-oob*]
      [(wait-all)  *msg-waitall*]))
  (apply logior (map lookup names)))

(define-constant *shut-rd*       net:SHUT_RD)
(define-constant *shut-wr*       net:SHUT_WR)
(define-constant *shut-rdwr*     net:SHUT_RDWR)

(define-macro (shutdown-method . names)
  (define (lookup name)
    (ecase (unwrap-syntax name)
      [(read)      *shut-rd*]
      [(write)     *shut-wr*]))
  (apply logior (map lookup names)))

(define (socket-merge-flags . flags) (apply logior flags))
(define (socket-purge-flags base-flag . flags)
  (logand base-flag (lognot (apply logior flags))))

;;
;; Constructors
;;
(define (make-client-socket node service
                            :optional
                            (ai-family *af-inet*)
                            (ai-socktype *sock-stream*)
                            (ai-flags (socket-merge-flags *ai-v4mapped*
                                                          *ai-addrconfig*))
                            (ai-protocol *ipproto-ip*))
  (check-arg string? node)
  (check-arg string? service)
  (cond-expand
   [gauche.net.ipv6
    (let1 ais (net:sys-getaddrinfo node service
                                   (make net:<sys-addrinfo>
                                     :flags ai-flags
                                     :family ai-family
                                     :socktype ai-socktype
                                     :protocol ai-protocol))
      (or (any (^[ai] (guard (e [else #f])
                        (net:make-client-socket (~ ai'addr))))
               ais)
          ;; If we're here, attempts to connect to every ais failed.
          ;; TODO: We might want to keep one of the errors for better msg
          (error "couldn't connect to ~a:~a" node service)))]
   [else
    ;; We should handle other options, but this path is rarely used,
    ;; only for ipv6-less platforms.
    (let1 addrs (net:make-sockaddrs node service ai-protocol)
      (or (any (^[addr] (guard (e [else #f])
                          (net:make-client-socket addr)))
               addrs)
          (error "couldn't connect to ~a:~a" node service)))]))

(define (make-server-socket service
                            :optional
                            (ai-family *af-inet*)
                            (ai-socktype *sock-stream*)
                            (ai-protocol *ipproto-ip*))
  (check-arg string? service)
  (cond-expand
   [gauche.net.ipv6
    (let1 ais (net:sys-getaddrinfo #f service
                                   (make net:<sys-addrinfo>
                                     :flags 0
                                     :family ai-family
                                     :socktype ai-socktype
                                     :protocol ai-protocol))
      (or (any (^[ai] (guard (e [else #f])
                        (net:make-server-socket (~ ai'addr))))
               ais)
          ;; If we're here, attempts to bind every ais failed.
          ;; TODO: We might want to keep one of the errors for better msg
          (error "couldn't create a server socket at service `~a'" service)))]
   [else
     ;; We should handle other options, but this path is rarely used,
     ;; only for ipv6-less platforms.
     (let1 addrs (net:make-sockaddrs #f service ai-protocol)
       (or (any (^[addr] (guard (e [else #f])
                           (net:make-server-socket addr)))
                addrs)
          (error "couldn't create a server socket at service `~a'" service)))]
   ))

;;
;; Communication
;;

(define (socket-send socket u8v :optional (flags *msg-none*))
  (check-arg u8vector? u8v)
  (net:socket-send socket u8v flags))

(define (socket-recv socket size :optional (flags *msg-none*))
  (let1 buf (make-u8vector size)
    (let1 n (net:socket-recv! socket buf flags)
      (if (< n size)
        (uvector-alias <u8vector> buf 0 n) ; returns #u8() when conn. closed
        buf))))

;;
;; Miscellaneous
;;

(define (socket? x) (is-a? x net:<socket>))
(define socket-accept net:socket-accept)
(define socket-shutdown net:socket-shutdown)
(define socket-input-port net:socket-input-port)
(define socket-output-port net:socket-output-port)
(define socket-close net:socket-close)

(define (call-with-socket socket proc)
  (unwind-protect (proc socket)
    (socket-close socket)))
                                


