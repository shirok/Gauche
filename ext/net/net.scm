;;;
;;; net - network interface
;;;
;;;  Copyright(C) 2001-2003 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: net.scm,v 1.19 2003-05-18 06:57:30 shirok Exp $
;;;

(define-module gauche.net
  (use srfi-1)
  (export <socket> make-socket
          |PF_UNSPEC| |PF_UNIX| |PF_INET| |AF_UNSPEC| |AF_UNIX| |AF_INET|
          |SOCK_STREAM| |SOCK_DGRAM| |SOCK_RAW|
          socket-address socket-status socket-input-port socket-output-port
          socket-shutdown socket-close socket-bind socket-connect socket-fd
          socket-listen socket-accept socket-setsockopt socket-getsockopt
          <sockaddr> <sockaddr-in> <sockaddr-un>
          sockaddr-name sockaddr-family
          make-client-socket make-server-socket
          call-with-client-socket
          <sys-hostent> sys-gethostbyname sys-gethostbyaddr
          <sys-protoent> sys-getprotobyname sys-getprotobynumber
          <sys-servent> sys-getservbyname sys-getservbyport
          )
  )
  
(select-module gauche.net)

(dynamic-load "libnet" :export-symbols #t)

(define ipv6-capable (symbol-bound? 'sys-getaddrinfo))

(export-if-defined
 |IPPROTO_IP| |IPPROTO_ICMP| |IPPROTO_TCP| |IPPROTO_UDP| |IPPROTO_IPV6|
 |SOL_SOCKET| |SO_KEEPALIVE| |SO_OOBINLINE| |SO_REUSEADDR| |SO_TYPE|
 |SO_BROADCAST| |SO_SNDBUF| |SO_RCVBUF| |SO_PRIORITY| |SO_ERROR|
 |SOL_TCP| |TCP_NODELAY| |TCP_MAXSEG| |TCP_CORK|
 |SOL_IP| |IP_OPTIONS|)

(if ipv6-capable
    (define (make-sys-addrinfo . args)
      (let-keywords* args ((flags :flags 0)
                           (family :family |AF_UNSPEC|)
                           (socktype :socktype 0)
                           (protocol :protocol 0))
        (let1 hints (make <sys-addrinfo>)
          (slot-set! hints 'flags (if (list? flags) (apply logior flags) flags))
          (slot-set! hints 'family family)
          (slot-set! hints 'socktype socktype)
          (slot-set! hints 'protocol protocol)
          hints))))

;; if ipv6 is supported, these symbols are defiend in the C routine.

(export-if-defined
 |PF_INET6| |AF_INET6|
 <sockaddr-in6> <sys-addrinfo> sys-getaddrinfo make-sys-addrinfo
 |AI_PASSIVE| |AI_CANONNAME| |AI_NUMERICHOST| |AI_NUMERICSERV|
 |AI_V4MAPPED| |AI_ALL| |AI_ADDRCONFIG|
 sys-getnameinfo
 |NI_NOFQDN| |NI_NUMERICHOST| |NI_NAMEREQD| |NI_NUMERICSERV| |NI_DGRAM|)

;; High-level interface.  We need some hardcoded heuristics here.

(define (make-client-socket proto . args)
  (cond ((eq? proto 'unix)
         (let-optionals* args ((path #f))
           (unless (string? path)
             (error "unix socket requires pathname, but got" path))
           (make-client-socket-unix path)))
        ((eq? proto 'inet)
         (let-optionals* args ((host #f) (port #f))
           (unless (and (string? host) (integer? port))
             (errorf "inet socket requires host name and port, but got ~s and ~s"
                     host port))
           (make-client-socket-inet host port)))
        ((and (string? proto)
              (pair? args)
              (integer? (car args)))
         ;; STk compatibility
         (make-client-socket-inet proto (car args)))
        (else
         (error "unsupported protocol:" proto))))

(define (make-client-socket-unix path)
  (let ((address (make <sockaddr-un> :path path))
        (socket  (make-socket |PF_UNIX| |SOCK_STREAM|)))
    (socket-connect socket address)
    socket))

(define (make-client-socket-inet host port)
  (cond (ipv6-capable
         (let1 err #f
           (define (try-connect ai)
             (with-error-handler (lambda (e) (set! err e) #f)
               (lambda ()
                 (let ((address (slot-ref ai 'addr))
                       (socket (make-socket (slot-ref ai 'family)
                                            (slot-ref ai 'socktype))))
                   (socket-connect socket address)))))
           (let* ((hints (make-sys-addrinfo :socktype |SOCK_STREAM|))
                  (socket (any try-connect
                               (sys-getaddrinfo host (number->string port)
                                                hints))))
             (unless socket (raise err))
             socket)))
        (else
         (let ((address (make <sockaddr-in> :host host :port port))
               (socket (make-socket |PF_INET| |SOCK_STREAM|)))
           (socket-connect socket address)
           socket))
        ))

(define (make-server-socket proto . args)
  (cond ((eq? proto 'unix)
         (let-optionals* args ((path #f))
           (unless (string? path)
             (error "unix socket requires pathname, but got" path))
           (make-server-socket-unix path)))
        ((eq? proto 'inet)
         (let-optionals* args ((port #f))
           (unless (integer? port)
             (error "inet socket requires port number, but got" port))
           (apply make-server-socket-inet port (cdr args))))
        ((integer? proto)
         ;; STk compatibility
         (apply make-server-socket-inet proto args))
        (else
         (error "unsupported protocol:" proto))))

(define (make-server-socket-unix path)
  (let ((address (make <sockaddr-un> :path path))
        (socket (make-socket |PF_UNIX| |SOCK_STREAM|)))
    (socket-bind socket address)
    (socket-listen socket 5)))

(define (make-server-socket-inet port . args)
  (let1 reuse-addr? (get-keyword :reuse-addr? args #f)
    (define (bind-listen socket address)
      (when reuse-addr?
        (socket-setsockopt socket |SOL_SOCKET| |SO_REUSEADDR| 1))
      (socket-bind socket address)
      (socket-listen socket 5))
    (cond (ipv6-capable
           (let1 err #f
             (define (try-bind-listen ai)
               (with-error-handler (lambda (e) (set! err e) #f)
                 (lambda ()
                   (let ((address (slot-ref ai 'addr))
                         (socket (make-socket (slot-ref ai 'family)
                                             (slot-ref ai 'socktype))))
                   (bind-listen socket address)))))
             (let* ((hints (make-sys-addrinfo :flags |AI_PASSIVE|
                                       :socktype |SOCK_STREAM|))
                    (socket (any try-bind-listen
                                 (sys-getaddrinfo #f (number->string port)
                                                  hints))))
               (unless socket (raise err))
               socket)))
          (else
           (let ((address (make <sockaddr-in> :host :any :port port))
                 (socket (make-socket |PF_INET| |SOCK_STREAM|)))
             (bind-listen socket address))))
    ))

(define (call-with-client-socket socket proc)
  (with-error-handler
      (lambda (e)
        (socket-close socket)
        (raise e))
    (lambda ()
      (begin0
       (proc (socket-input-port socket) (socket-output-port socket))
       (socket-close socket)))))

;; backward compatibility -- will be removed!
(define pf_inet |PF_INET|)
(define pf_unix |PF_UNIX|)

(define af_inet |AF_INET|)
(define af_unix |AF_UNIX|)

(define sock_stream |SOCK_STREAM|)
(define sock_dgram  |SOCK_DGRAM|)

(define sol_socket |SOL_SOCKET|)
(define so_reuseaddr |SO_REUSEADDR|)

(provide "gauche/net")
