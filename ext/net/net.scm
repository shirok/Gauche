;;;
;;; net - network interface
;;;
;;;  Copyright(C) 2001 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: net.scm,v 1.13 2001-09-29 09:27:45 shirok Exp $
;;;

(define-module gauche.net
  (use gauche.let-opt)
  (export <socket> make-socket
          |PF_UNIX| |PF_INET| |AF_UNIX| |AF_INET|
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
          ))
          
(select-module gauche.net)

(dynamic-load "libnet" :export-symbols #t)

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
  (let ((address (make <sockaddr-in> :host host :port port))
        (socket (make-socket |PF_INET| |SOCK_STREAM|)))
    (socket-connect socket address)
    socket))

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
  (let ((reuse-addr? (get-keyword :reuse-addr? args #f))
        (address (make <sockaddr-in> :host :any :port port))
        (socket (make-socket |PF_INET| |SOCK_STREAM|)))
    (when reuse-addr?
      (socket-setsockopt socket |SOL_SOCKET| |SO_REUSEADDR| 1))
    (socket-bind socket address)
    (socket-listen socket 5)))

(define (call-with-client-socket socket proc)
  (dynamic-wind
   (lambda () #f)
   (lambda () (proc (socket-input-port socket) (socket-output-port socket)))
   (lambda () (socket-close socket))))

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
