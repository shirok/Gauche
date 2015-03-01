;;;
;;; netaux.scm - network interface
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
(use srfi-1)
(use gauche.sequence)
(use gauche.lazy)
(use util.match)

;; default backlog value for socket-listen
(define-constant DEFAULT_BACKLOG 5)

;; NB: we can't use (cond-expand (gauche.net.ipv6 ...) ) here, since
;; cond-expand is expanded when netaux.scm is compiled, but at that time
;; the feature 'gauche.net.ipv6' is not available since the gauche.net module
;; is not yet built.  So we use a bit of kludge here.
(define ipv6-capable (global-variable-bound? 'gauche.net 'sys-getaddrinfo))

;; API
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

;; API
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

;; API
(define (make-server-socket proto . args)
  (cond [(eq? proto 'unix)
         (let-optionals* args ([path #f])
           (unless (string? path)
             (error "unix socket requires pathname, but got" path))
           (apply make-server-socket-unix path (cdr args)))]
        [(eq? proto 'inet)
         (let-optionals* args ([port #f])
           (define (err)
             (error "inet socket requires integer port number or \
                     string service name, or a list of them, but got:" port))
           (cond [(list? port)
                  (unless (every (any-pred integer? string?) port) (err))
                  (apply make-server-socket-inet* port (cdr args))]
                 [(or (integer? port) (string? port))
                  (apply make-server-socket-inet port (cdr args))]
                 [else (err)]))]
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

(define (make-server-socket-inet* ports . args) ; taking multiple ports
  (let1 err #f
    (define (try-bind address)
      (guard (e [else (set! err e) #f])
        (apply make-server-socket-from-addr address args)))
    (rlet1 socket (any try-bind ($ lconcatenate $ lmap
                                   (cut make-sockaddrs #f <>)
                                   ports))
      (unless socket (raise err)))))

;; API
;; Listen both v4 and v6 sockets, unless the system supports dual-stack
;; socket.
;; Heuristics - if we have both v4 and v6 sockets, we *may* need
;; only v6 sockets if the system defaults to dual-stack socket.
;; Unfortunately the behavior is system dependent.  So we try to
;; open both (first v6, then v4) and if the latter fails to bind
;; we assume v6 socket listens both.
(define (make-server-sockets host port . args)
  (define (v4addrs ss)
    (filter (^s (eq? (sockaddr-family s) 'inet)) ss))
  (define (v6addrs ss)
    (filter (^s (eq? (sockaddr-family s) 'inet6)) ss))

  ;; Kludge: These may not be bound on certain platforms,
  ;; so we look them up at runtime.
  (define EADDRINUSE
    (global-variable-ref (find-module 'gauche) 'EADDRINUSE #f))
  (define EADDRNOTAVAIL
    (global-variable-ref (find-module 'gauche) 'EADDRNOTAVAIL #f))
  (define <sockaddr-in6>
    (global-variable-ref (find-module 'gauche.net) '<sockaddr-in6> #f))

  (define (bind-failed? e)
    (and (<system-error> e)
         (memv (~ e'errno) `(,EADDRINUSE ,EADDRNOTAVAIL))))

  ;; try binding v4 socket with the same port of opened v6 socket S6.
  ;; Returns (S6 S4) on success, or (S6) on failure.
  ;; NB: It is possible that v4's port is taken by another process,
  ;; instead of dual-stack S6 socket.
  (define (try-v4 s6 addrs)
    ;; If the original port argument is 0, we take port number from
    ;; the opened v6 socket.
    (let1 a4s (if (zero? port)
                ($ v4addrs $ make-sockaddrs host
                   $ sockaddr-port $ socket-address s6)
                (v4addrs addrs))
      (guard (e [(bind-failed? e) (list s6)]
                [else (raise e)])
        (cons s6 (filter-map (cut apply make-server-socket <> args) a4s)))))

  ;; try binding v6 socket on addr.  If actual-port is not #f, reallocate
  ;; addr with the given port.  It is for the case that the given port is 0.
  (define (try-v6 addr actual-port)
    (let1 addr (if actual-port
                 (make <sockaddr-in6>
                   :host (sockaddr-addr addr) :port actual-port)
                 addr)
      (guard (e [(bind-failed? e) (values #f actual-port)])
        (let1 s6 (apply make-server-socket addr args)
          (values s6
                  (if (zero? port)
                    (sockaddr-port (socket-address s6))
                    #f))))))

  ;; Bind multiple v6 sockaddrs.
  ;; If PORT is zero, we have to use the actual port number of the first
  ;; socket we can bind.  So it's more involved than simply mapping
  ;; make-server-socket.
  (define (make-v6socks a6s)
    (receive (socks _) (map-accum try-v6 #f a6s)
      (filter identity socks)))
  
  (let* ([ss (make-sockaddrs host port)]
         [a6s (v6addrs ss)])
    ;; NB: Mingw doesn't have EADDRINUSE.  it's likely not to have ipv6 either,
    ;; so we just use the default.  NB: we can't switch here with
    ;; gauche.sys.ipv6; see the comment on ipv6-capable definition above.
    (if (or (null? a6s)
            (not EADDRINUSE)
            (not <sockaddr-in6>))
      (map (cut apply make-server-socket <> args) ss)
      (append-map (cut try-v4 <> ss) (make-v6socks (v6addrs ss))))))

;; API
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

;; API
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
