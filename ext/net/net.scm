;;;
;;; net - network interface
;;;  
;;;   Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
;;;  $Id: net.scm,v 1.21 2003-07-05 03:29:10 shirok Exp $
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

;; Utility
(define (address->protocol-family addr)
  (case (sockaddr-family addr)
    ((unix)  |PF_UNIX|)
    ((inet)  |PF_INET|)
    ((inet6) |PF_INET6|) ;;this can't happen if !ipv6-capable
    (else (error "unknown family of socket address" addr))))

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
        ((is-a? proto <sockaddr>)
         ;; caller provided sockaddr
         (make-client-socket-from-addr proto))
        ((and (string? proto)
              (pair? args)
              (integer? (car args)))
         ;; STk compatibility
         (make-client-socket-inet proto (car args)))
        (else
         (error "unsupported protocol:" proto))))

(define (make-client-socket-from-addr addr)
  (let1 socket (make-socket (address->protocol-family addr) |SOCK_STREAM|)
    (socket-connect socket addr)
    socket))

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
        ((is-a? proto <sockaddr>)
         ;; caller provided sockaddr
         (apply make-server-socket-from-addr proto args))
        ((integer? proto)
         ;; STk compatibility
         (apply make-server-socket-inet proto args))
        (else
         (error "unsupported protocol:" proto))))

(define (make-server-socket-from-addr addr . args)
  (let ((reuse-addr? (get-keyword :reuse-addr? args #f))
        (socket (make-socket (address->protocol-family addr) |SOCK_STREAM|)))
    (when reuse-addr?
      (socket-setsockopt socket |SOL_SOCKET| |SO_REUSEADDR| 1))
    (socket-bind socket address)
    (socket-listen socket 5)))

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
