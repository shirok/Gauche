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
;;;  $Id: net.scm,v 1.1 2001-06-12 10:25:09 shirok Exp $
;;;

(define-module gauche.net
  (export <socket> make-socket
          pf_unix pf_inet sock_stream sock_dgram sock_raw
          socket-shutdown socket-close socket-bind socket-connect
          socket-listen socket-accept
          <sockaddr> <sockaddr-in> <sockaddr-un>
          sockaddr-name sockaddr-family
          ))
          
(select-module gauche.net)

(dynamic-load "libnet")


(provide "gauche/net")
