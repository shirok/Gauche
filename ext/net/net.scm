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
;;;  $Id: net.scm,v 1.3 2001-06-13 19:56:53 shirok Exp $
;;;

(define-module gauche.net
  (use srfi-2)
  (use srfi-13)
  (export <socket> make-socket
          pf_unix pf_inet sock_stream sock_dgram sock_raw
          socket-address socket-status socket-input-port socket-output-port
          socket-shutdown socket-close socket-bind socket-connect
          socket-listen socket-accept socket-setsockopt socket-getsockopt
          <sockaddr> <sockaddr-in> <sockaddr-un>
          sockaddr-name sockaddr-family
          make-client-socket make-server-socket
          ))
          
(select-module gauche.net)

(dynamic-load "libnet")

;; High-level interface.  We need some hardcoded heuristics here.

(define (addr-spec->address addr-spec port server?)
  (cond ((is-a? addr-spec <sockaddr>) addr-spec)
        ((not (string? addr-spec))
         (error "illegal address-spec: ~s" addr-spec))
        ((string-prefix? "unix:" addr-spec)
         (make <sockaddr-un> :path (string-drop addr-spec 5)))
        ((string-prefix? "inet:" addr-spec)
         (if server?
             (make <sockaddr-in>
               :host :any
               :port (or (string->number (string-drop addr-spec 5))
                         (error "bad inet server address spec: ~s" addr-spec)))
             (let ((host-n-port (string-split addr-spec #\:)))
               (or (and-let* (((= (length host-n-port) 3))
                              (port (string->number (caddr host-n-port))))
                     (make <sockaddr-in>
                       :host (cadr host-n-port) :port port))
                   (error "bad inet client address: ~s" addr-spec)))
             ))
        (else
         (unless (and (integer? port) (>= port 0))
           (error "bad port number for inet client address: ~s" port))
         (make <sockaddr-in> :host addr-spec :port port))
        ))

(define (address->protocol-family address)
  (cond ((is-a? address <sockaddr-in>) pf_inet)
        ((is-a? address <sockaddr-un>) pf_unix)
        (else (error "this socket address is unknown to the high-level socket library" address))))

(define (make-client-socket addr-spec . args)
  (let-optional* args ((port #f))
    (let* ((address (addr-spec->address addr-spec port #f))
           (family  (address->protocol-family address))
           (socket  (make-socket family sock_stream)))
      (socket-connect socket address)
      socket)))

(define (make-server-socket addr-spec . args)
  #f
  )

(provide "gauche/net")
