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
;;;  $Id: net.scm,v 1.5 2001-06-23 07:04:10 shirok Exp $
;;;

(define-module gauche.net
  (export <socket> make-socket
          pf_unix pf_inet af_unix af_inet sock_stream sock_dgram sock_raw
          socket-address socket-status socket-input-port socket-output-port
          socket-shutdown socket-close socket-bind socket-connect
          socket-listen socket-accept socket-setsockopt socket-getsockopt
          <sockaddr> <sockaddr-in> <sockaddr-un>
          sockaddr-name sockaddr-family
          make-client-socket make-server-socket
          <sys-hostent> sys-gethostbyname sys-gethostbyaddr
          <sys-protoent> sys-getprotobyname sys-getprotobynumber
          <sys-servent> sys-getservbyname sys-getservbyport
          ))
          
(select-module gauche.net)

(dynamic-load "libnet")

;; High-level interface.  We need some hardcoded heuristics here.

(define (make-client-socket proto . args)
  (cond ((eq? proto 'unix)
         (let-optional* args ((path #f))
           (unless (string? path)
             (error "unix socket requires pathname, but got ~s" path))
           (make-client-socket-unix path)))
        ((eq? proto 'inet)
         (let-optional* args ((host #f) (port #f))
           (unless (and (string? host) (integer? port))
             (error "inet socket requires host name and port, but got ~s and ~s"
                    host port))
           (make-client-socket-inet host port)))
        ((and (string? proto)
              (pair? args)
              (integer? (car args)))
         ;; STk compatibility
         (make-client-socket-inet proto (car args)))
        (else
         (error "unsupported protocol: ~s" proto))))

(define (make-client-socket-unix path)
  (let ((address (make <sockaddr-un> :path path))
        (socket  (make-socket pf_unix sock_stream)))
    (socket-connect socket address)
    socket))
  
(define (make-client-socket-inet host port)
  (let ((address (make <sockaddr-in> :host host :port port))
        (socket (make-socket pf_inet sock_stream)))
    (socket-connect socket address)
    socket))

(define (make-server-socket proto . args)
  (cond ((eq? proto 'unix)
         (let-optional* args ((path #f))
           (unless (string? path)
             (error "unix socket requires pathname, but got ~s" path))
           (make-server-socket-unix path)))
        ((eq? proto 'inet)
         (let-optional* args ((port #f))
           (unless (integer? port)
             (error "inet socket requires port number, but got ~s" port))
           (apply make-server-socket-inet port args)))
        ((integer? proto)
         ;; STk compatibility
         (apply make-server-socket-inet proto args))
        (else
         (error "unsupported protocol: ~s" proto))))

(define (make-server-socket-unix path)
  (let ((address (make <sockaddr-un> :path path))
        (socket (make-socket pf_unix sock_stream)))
    (socket-bind socket address)
    (socket-listen socket 5)))

(define (make-server-socket-inet port . args)
  (let ((reuse-addr? (get-keyword :reuse-addr args #f))
        (address (make <sockaddr-un> :host :any :port port))
        (socket (make-socket pf_inet sock_stream)))
    (when reuse-addr?
      (socket-setsockopt socket sol_socket so_reuseaddr 1))
    (socket-bind socket address)
    (socket-listen socket 5)))

(provide "gauche/net")
