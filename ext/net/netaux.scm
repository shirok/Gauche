;;;
;;; netaux.scm - network interface
;;;
;;;   Copyright (c) 2000-2013  Shiro Kawai  <shiro@acm.org>
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
(use srfi-1)
(use gauche.sequence)
(use util.match)

;; default backlog value for socket-listen
(define-constant DEFAULT_BACKLOG 5)

;; NB: we can't use (cond-expand (gauche.net.ipv6 ...) ) here, since
;; cond-expand is expanded when netaux.scm is compiled, but at that time
;; the feature 'gauche.net.ipv6' is not available since the gauche.net module
;; is not yet built.  So we use a bit of kludge here.
(define ipv6-capable (global-variable-bound? 'gauche.net 'sys-getaddrinfo))

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

(define (make-server-socket proto . args)
  (cond [(eq? proto 'unix)
         (let-optionals* args ((path #f))
           (unless (string? path)
             (error "unix socket requires pathname, but got" path))
           (apply make-server-socket-unix path (cdr args)))]
        [(eq? proto 'inet)
         (let-optionals* args ((port #f))
           (unless (or (integer? port) (string? port))
             (error "inet socket requires port, but got" port))
           (apply make-server-socket-inet port (cdr args)))]
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

(define (make-server-sockets host port . args)
  (let1 ss (make-sockaddrs host port)
    ;; Heuristics - if we have both v4 and v6 sockets, we *may* need
    ;; only v6 sockets if the system defaults to dual-stack socket.
    ;; Unfortunately the behavior is system dependent.  So we try to
    ;; open both (first v6, then v4) and if the latter fails to bind
    ;; we assume v6 socket listens both.
    (cond-expand
     [gauche.os.windows
      ;; mingw doesn't have EADDRINUSE.  it's likely not to have ipv6 either,
      ;; so we just use the default.  NB: we can't switch here with
      ;; gauche.sys.ipv6; see the comment on ipv6-capable definition above.
      (map (cut apply make-server-socket <> args) ss)]
     [else
      (let ([v4s (filter (^s (eq? (sockaddr-family s) 'inet)) ss)]
            [v6s (filter (^s (eq? (sockaddr-family s) 'inet6)) ss)])
        (if (and (not (null? v4s)) (not (null? v6s)))
          (fold (^(s ss)
                  (guard (e [(and (<system-error> e)
                                  (eqv? (~ e'errno) EADDRINUSE))
                             ss])         ;ignore s
                    (cons (apply make-server-socket s args) ss)))
                (map (cut apply make-server-socket <> args) v6s)
                v4s)
          (map (cut apply make-server-socket <> args) ss)))])))

(define (make-sockaddrs host port :optional (proto 'tcp))
  (if ipv6-capable
    (let* ([socktype (case proto
                       [(tcp) SOCK_STREAM]
                       [(udp) SOCK_DGRAM]
                       [else (error "unsupported protocol:" proto)])]
           [port (x->string port)]
           [hints (make-sys-addrinfo :flags AI_PASSIVE :socktype socktype)])
      (map (cut slot-ref <> 'addr) (sys-getaddrinfo host port hints)))
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

;;=================================================================
;; IP address <-> string converter
;; Although many systems support this feature (e.g. inet_ntop/inet_pton
;; or WSAAdressToString/WSAStringToAddress), it would be too cumbersome
;; to check availability of those and switch the implementation.  So we
;; provide them in Scheme.

;; accessor methods

(define-method sockaddr-name ((addr <sockaddr-in>))
  #`",(inet-address->string (sockaddr-addr addr) AF_INET):,(sockaddr-port addr)")

;; NB: this should be conditionally defined by cond-expand at compile-time,
;; instead of load-time dispatch.  We need to clean up cond feature management
;; more to do so.
(if ipv6-capable
  (define-method sockaddr-name ((addr <sockaddr-in6>))
    #`"[,(inet-address->string (sockaddr-addr addr) AF_INET6)]:,(sockaddr-port addr)"))
