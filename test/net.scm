;;
;; testing net
;;

(use gauche.test)

(test-start "net")

(use gauche.net)
(test-module 'gauche.net
             :allow-undefined '(sys-getaddrinfo <sys-addrinfo>
                                AI_PASSIVE PF_INET6))

;;-----------------------------------------------------------------
(test-section "socket address")

(test* "sockaddr_un" #t
        (let1 addr (make <sockaddr-un> :path "/tmp/xxx")
          (and (eq? (sockaddr-family addr) 'unix)
               (equal? (sockaddr-name addr) "/tmp/xxx")
               #t)))

(test* "sockaddr_in" #t
        (let1 addr (make <sockaddr-in> :host "127.0.0.1" :port 80)
          (and (eq? (sockaddr-family addr) 'inet)
               (equal? (sockaddr-name addr) "127.0.0.1:80")
               (= (sockaddr-addr addr) #x7f000001)
               (= (sockaddr-port addr) 80)
               #t)))

(test* "sockaddr_in" #t
       (let1 addr (make <sockaddr-in> :host "localhost" :port 23)
         (and (eq? (sockaddr-family addr) 'inet)
              (equal? (sockaddr-name addr) "127.0.0.1:23")
              #t)))

(test* "sockaddr_in" #t
       (let1 addr (make <sockaddr-in> :host #x7f000001 :port 23)
         (and (eq? (sockaddr-family addr) 'inet)
              (equal? (sockaddr-name addr) "127.0.0.1:23")
              #t)))

(test* "sockaddr_in" #t
       (let1 addr (make <sockaddr-in> :host '#u8(127 0 0 1) :port 23)
         (and (eq? (sockaddr-family addr) 'inet)
              (equal? (sockaddr-name addr) "127.0.0.1:23")
              #t)))

(test* "sockaddr_in" #t
       (let1 addr (make <sockaddr-in> :host :any :port 7777)
         (and (eq? (sockaddr-family addr) 'inet)
              (equal? (sockaddr-name addr) "0.0.0.0:7777")
              #t)))

(test* "sockaddr_in" #t
       (let1 addr (make <sockaddr-in> :host :broadcast)
         (and (eq? (sockaddr-family addr) 'inet)
              (equal? (sockaddr-name addr) "255.255.255.255:0")
              #t)))

(let ([un1 (make <sockaddr-un> :path "/tmp/a")]
      [un2 (make <sockaddr-un> :path "/tmp/aa")]
      [un3 (make <sockaddr-un> :path "/tmp/a")]
      [in1 (make <sockaddr-in> :host "127.0.0.1" :port 2030)]
      [in2 (make <sockaddr-in> :host "127.0.0.2" :port 2030)]
      [in3 (make <sockaddr-in> :host "127.0.0.1" :port 2031)]
      [in4 (make <sockaddr-in> :host "127.0.0.1" :port 2030)])
  (let-syntax ([t (syntax-rules ()
                    [(_ exp a b)
                     (test* (format "sockaddr equal? ~s" '(equal? a b))
                            exp (equal? a b))])])
    (t #f un1 un2)
    (t #t un1 un3)
    (t #f in1 in2)
    (t #f in1 in3)
    (t #t in1 in4)
    (t #f un1 in1)))

(cond-expand
 [gauche.net.ipv6
  (test* "sockaddr_in6" #t
         (let1 addr (make <sockaddr-in6> :host "2001:200::8002:203:47ff:fea5:3085" :port 23)
           (and (eq? (sockaddr-family addr) 'inet6)
                (#/\[2001:200:0?:8002:203:47ff:fea5:3085\]:23/ (sockaddr-name addr))
                (= (sockaddr-addr addr) #x2001020000008002020347fffea53085)
                (= (sockaddr-port addr) 23)
                #t)))
  (test* "sockaddr_in6" "[::1]:0"
         (sockaddr-name (make <sockaddr-in6> :host 1)))
  (test* "sockaddr_in6" "[1:2:3:4:5:6:7:8]:0"
         (sockaddr-name (make <sockaddr-in6> :host '#u8(0 1 0 2 0 3 0 4 0 5 0 6 0 7 0 8))))

  (let ([4n1 (make <sockaddr-in6> :host 1 :port 80)]
        [4n2 (make <sockaddr-in6> :host 1 :port 81)]
        [4n3 (make <sockaddr-in6> :host "1:2::8" :port 80)]
        [4n4 (make <sockaddr-in6> :host 1 :port 80)])
    (let-syntax ([t (syntax-rules ()
                      [(_ exp a b)
                       (test* (format "sockaddr equal? ~s" '(equal? a b))
                              exp (equal? a b))])])
      (t #f 4n1 4n2)
      (t #f 4n1 4n3)
      (t #t 4n1 4n4)))]
 [else])

(let ()
  (define (addr-test desc input exp-val exp-vers)
    (let1 uvresult (make-u8vector 16 0)
      (and exp-vers
           (let loop ([val exp-val]
                      [dig (if (= exp-vers AF_INET) 3 15)])
             (when (>= dig 0)
               (u8vector-set! uvresult dig (logand val #xff))
               (loop (ash val -8) (- dig 1)))))
      (test* #"inet-string->address  (~desc)" `(,exp-val ,exp-vers)
             (values->list (inet-string->address input)))
      (test* #"inet-string->address! (~desc)" `(,uvresult ,exp-vers)
             (let* ([buf (make-u8vector 16 0)]
                    [ver (inet-string->address! input buf)])
               (list buf ver)))))

  (addr-test "v4-1" "127.0.0.1" #x7f000001 AF_INET)
  (addr-test "v4-2" "192.168.1.2" #xc0a80102 AF_INET)
  (addr-test "v4-3" "255.255.252.0" #xfffffc00 AF_INET)
  ;(addr-test "v4-err1" "192.168.1" #f #f) ;; NB: this is allowed in IPv4 spec, although prohibited in inet_pton.  Cygwin's inet_pton does recognize this.
  (addr-test "v4-err2" "192.168.1.2.3" #f #f)
  (addr-test "v4-err3" "172.256.4.2" #f #f)

  (cond-expand
   [gauche.net.ipv6
    (addr-test "v6-1" "1:2:3:4:fffc:fffd:fffe:ffff"
               #x0001000200030004fffcfffdfffeffff AF_INET6)
    (addr-test "v6-2" "::1"
               #x00000000000000000000000000000001 AF_INET6)
    (addr-test "v6-3" "::0.1.0.2"
               #x00000000000000000000000000010002 AF_INET6)
    (addr-test "v6-4" "0:1:2:3:4:5:6:7"
               #x00000001000200030004000500060007 AF_INET6)
    (addr-test "v6-5" "ffe0::"
               #xffe00000000000000000000000000000 AF_INET6)
    (addr-test "v6-6" "ffe0:1::"
               #xffe00001000000000000000000000000 AF_INET6)
    (addr-test "v6-7" "ffe0::234:567"
               #xffe00000000000000000000002340567 AF_INET6)
    (addr-test "v6-8" "::192.168.1.2"
               #x000000000000000000000000c0a80102 AF_INET6)
    (addr-test "v6-9" "1::2:0:0:192.168.1.2"
               #x000100000000000200000000c0a80102 AF_INET6)
    (addr-test "v6-err1" ":1:2:3:4:5:6:7:8" #f #f)
    (addr-test "v6-err2" "ffe0::234::567" #f #f)
    ;; These two should be an error, but cygwin's inet_pton allows them.
    (cond-expand
     [(not gauche.os.cygwin)
      (addr-test "v6-err3" "::1:2:3:4:5:6:7:8" #f #f)
      (addr-test "v6-err4" "ffe0::234:567:" #f #f)]
     [else])]
   [else])
  )

(let ()
  (define (addr-test desc input vers expected)
    (let* ([size (cond [(= vers AF_INET) 4] [(= vers AF_INET6) 16])]
           [uv (make-u8vector size 0)])
      (do ([k (- size 1) (- k 1)]
           [val input (ash val -8)])
          [(< k 0)]
        (u8vector-set! uv k (logand val #xff)))
      (test* #"inet-address->string (~desc, int)" expected
             (inet-address->string input vers)
             (^[xs y] (member y xs)))
      (test* #"inet-address->string (~desc, uv)" expected
             (inet-address->string uv vers)
             (^[xs y] (member y xs)))))

  (addr-test "v4-1" #x7f000001 AF_INET '("127.0.0.1"))
  (addr-test "v4-2" #xc0a80102 AF_INET '("192.168.1.2"))
  (addr-test "v4-3" #xfffffc00 AF_INET '("255.255.252.0"))

  (cond-expand
   [gauche.net.ipv6
    (addr-test "v6-1" #x0001000200030004fffcfffdfffeffff AF_INET6
               '("1:2:3:4:fffc:fffd:fffe:ffff"))
    (addr-test "v6-2" #x00000000000000000000000000000001 AF_INET6
               '("::1"))
    (addr-test "v6-3" #x00000000000000000000000000010002 AF_INET6
               '("::0.1.0.2" "::1:2"))
    (addr-test "v6-4" #x00080001000200030004000500060007 AF_INET6
               '("8:1:2:3:4:5:6:7"))
    (addr-test "v6-5" #xffe00000000000000000000000000000 AF_INET6
               '("ffe0::"))
    (addr-test "v6-6" #xffe00001000000000000000000000000 AF_INET6
               '("ffe0:1::"))
    (addr-test "v6-7" #xffe00000000000000000000002340567 AF_INET6
               '("ffe0::234:567"))
    (addr-test "v6-8" #xffe00000000002230001000200030004 AF_INET6
               '("ffe0::223:1:2:3:4"))
    (addr-test "v6-9" #xffe00000000002230000000000030004 AF_INET6
               '("ffe0::223:0:0:3:4"))
    (addr-test "v6-10" #xffe00000000002230000000000000004 AF_INET6
               '("ffe0:0:0:223::4"))
    (addr-test "v6-11" #xffe00000000000000001000000000004 AF_INET6
               '("ffe0::1:0:0:4"))
    ]
   [else])
  )


;;-----------------------------------------------------------------
(test-section "netdb")

(test* "gethostbyname" #t
       (let1 host (sys-gethostbyname "localhost")
         (and host
              (or (equal? (slot-ref host 'name) "localhost")
                  (member "localhost" (slot-ref host 'aliases))
                  ;; cygwin and mingw usually doesn't define "localhost", and
                  ;; returns hostname.
                  (cond-expand
                   [(or gauche.os.cygwin gauche.os.windows) #t]
                   [else #f]))
              (member "127.0.0.1" (slot-ref host 'addresses))
              #t)))

(test* "gethostbyaddr" #t
       (let1 host (sys-gethostbyaddr "127.0.0.1" AF_INET)
         (and host
              (or (equal? (slot-ref host 'name) "localhost")
                  (member "localhost" (slot-ref host 'aliases))
                  ;; cygwin and mingw usually doesn't define "localhost", and
                  ;; returns hostname.  For now we skip this test.
                  (cond-expand
                   [(or gauche.os.cygwin gauche.os.windows) #t]
                   [else #f]))
              (member "127.0.0.1" (slot-ref host 'addresses))
              #t)))

(test* "getprotobyname" '(("icmp" 1) ("tcp" 6) ("udp" 17))
       (let ([tcp (sys-getprotobyname "tcp")]
             [udp (sys-getprotobyname "udp")]
             [icmp (sys-getprotobyname "icmp")])
         (map (^[proto] (list (slot-ref proto 'name)
                              (slot-ref proto 'proto)))
              (list icmp tcp udp))))

(test* "getprotobynumber" '(#t #t #t)
       (map (^[proto name] (or (member name (slot-ref proto 'aliases))
                               (equal? name (slot-ref proto 'name))))
            (map sys-getprotobynumber '(1 6 17))
            '("icmp" "tcp" "udp")))

(test* "getservbyname" '(("telnet" 23 "tcp") ("ftp" 21 "tcp"))
       (map (^[name proto]
              (let1 x (sys-getservbyname name proto)
                (and x (map (^s (slot-ref x s)) '(name port proto)))))
            '("telnet" "ftp")
            '("tcp"    "tcp")))

(test* "getservbyport" '(("telnet" 23 "tcp") ("ftp" 21 "tcp"))
       (map (^[port proto]
              (let1 x (sys-getservbyport port proto)
                (and x (map (^s (slot-ref x s)) '(name port proto)))))
            '(23       21)
            '("tcp"    "tcp")))

;;-----------------------------------------------------------------
(test-section "Packet utility")

;; checksum example taken from RFC1071
(test* "inet-checksum" (logand (lognot #xddf2) #xffff)
       (inet-checksum '#u8(#x00 #x01 #xf2 #x03 #xf4 #xf5 #xf6 #xf7) 8))
(test* "inet-checksum" (logand (lognot #xf201) #xffff)
       (inet-checksum '#u8(#x00 #x01 #xf2) 3))

;;-----------------------------------------------------------------
(test-section "socket")

;; max size of the packet.  increase this to test robustness for
;; buffer overrun attack.  right now, Gauche can bear fairly large
;; packet, but got extremely inefficient.
(define *chunk-size* 65537)

;; port number to test inet socket connection.  Gauche needs some
;; mechanism to check if the port is in use or not.
(define *inet-port* 6726)

(sys-unlink "sock.o")                   ; unix socket

(cond-expand
 [gauche.os.windows #f]
 [else
  (define server-sock #f)
  (define accept-sock #f)

  (define (do-accept)
    (set! accept-sock (socket-accept server-sock)))

  (define (do-echo close?)
    (let1 line (read-line (socket-input-port accept-sock))
      (format (socket-output-port accept-sock)
              "GOT:~a\n" line)
      (flush (socket-output-port accept-sock)))
    (when close?
      (socket-shutdown accept-sock SHUT_RDWR)
      (socket-close accept-sock)))

  (test* "unix server socket" #f
         (begin
           (set! server-sock (make-server-socket 'unix "sock.o"))
           (let1 stat (sys-stat "sock.o")
             (not (memq (sys-stat->file-type stat) '(socket fifo))))))

  (test* "unix client socket" '("GOT:abc" "GOT:xyz")
         (call-with-client-socket (make-client-socket 'unix "sock.o")
           (^[in out]
             (do-accept)
             (display "abc\n" out) (flush out)
             (do-echo #f)
             (let1 abc (read-line in)
               (display "xyz\n" out) (flush out)
               (do-echo #t)
               (list abc (read-line in))))))

  (test* "unix client socket" #t
         (call-with-client-socket (make-client-socket 'unix "sock.o")
           (^[in out]
             (do-accept)
             (display (make-string *chunk-size* #\a) out)
             (newline out)
             (flush out)
             (do-echo #t)
             (string=? (read-line in)
                       (string-append "GOT:" (make-string *chunk-size* #\a))))))

  (test* "unix client socket" #t
         (call-with-client-socket
          (make-client-socket (make <sockaddr-un> :path "sock.o"))
          (^[in out]
            (do-accept)
            (display (make-string *chunk-size* #\a) out)
            (newline out)
            (flush out)
            (do-echo #t)
            (string=? (read-line in)
                      (string-append "GOT:"(make-string *chunk-size* #\a))))))

  (socket-close server-sock)
  ])

(sys-unlink "sock.o")

;; We may have more than one server socket (e.g. one for v4 and one for v6)
(let ([server-socks #f]
      [accept-sock #f])

  (define (do-accept)
    (let ([rfds (list->sys-fdset (map socket-fd server-socks))])
      (receive (nfds rs ws xs) (sys-select rfds #f #f)
        (set! accept-sock
              (socket-accept (find (^s (sys-fdset-ref rs (socket-fd s)))
                                   server-socks))))))

  (define (do-echo close?)
    (let1 line (read-line (socket-input-port accept-sock))
      (format (socket-output-port accept-sock)
              "GOT:~a\n" line)
      (flush (socket-output-port accept-sock)))
    (when close?
      (socket-shutdown accept-sock SHUT_RDWR)
      (socket-close accept-sock)))

  (test* "inet server socket" #t
         (begin
           (set! server-socks
                 (make-server-sockets #f *inet-port* :reuse-addr? #t))
           (length>=? server-socks 1)))

  (test* "inet client socket" '("GOT:abc" "GOT:xyz")
         (call-with-client-socket (make-client-socket 'inet
                                                      "localhost" *inet-port*)
           (^[in out]
             (do-accept)
             (display "abc\n" out) (flush out)
             (do-echo #f)
             (let1 abc (read-line in)
               (display "xyz\n" out) (flush out)
               (do-echo #t)
               (list abc (read-line in))))))

  (test* "inet client socket (host,port)" #t
         (call-with-client-socket (make-client-socket "localhost" *inet-port*)
           (^[in out]
             (do-accept)
             (display (make-string *chunk-size* #\a) out)
             (newline out)
             (flush out)
             (do-echo #t)
             (string=? (read-line in)
                       (string-append "GOT:" (make-string *chunk-size* #\a))))))

  (test* "inet client socket (sockaddr)" #t
         (call-with-client-socket (make-client-socket
                                   (make <sockaddr-in>
                                     :host "localhost" :port *inet-port*))
           (^[in out]
             (do-accept)
             (display (make-string *chunk-size* #\a) out)
             (newline out)
             (flush out)
             (do-echo #t)
             (string=? (read-line in)
                       (string-append "GOT:" (make-string *chunk-size* #\a))))))

  (for-each socket-close server-socks)
  )

(cond-expand
 [gauche.net.ipv6
  (let ([server-sock (guard [e (else #f)]
                       (make-server-socket (make <sockaddr-in6>
                                             :host :any :port *inet-port*)
                                           :reuse-addr? #t))]
        [accept-sock #f])

    ;; On IPv6 system, the loopback may have different name than "localhost".
    ;; We apply some heuristics here.
    (define (get-ipv6-sock)
      (any (^[name]
             (guard (e [else #f])
               (make-client-socket
                (make <sockaddr-in6> :host name :port *inet-port*))))
           '("localhost" "ip6-localhost" "ipv6-localhost" "::1")))

    (define (do-accept)
      (set! accept-sock (socket-accept server-sock)))

    (define (do-echo close?)
      (let1 line (read-line (socket-input-port accept-sock))
        (format (socket-output-port accept-sock)
                "GOT:~a\n" line)
        (flush (socket-output-port accept-sock)))
      (when close?
        (socket-shutdown accept-sock SHUT_RDWR)
        (socket-close accept-sock)))

    ;; ipv6 may be turned off by runtime flag.  we skip in that case.
    (when server-sock
      (test* "inet client socket (ipv6)" #t
             (and-let* ((sock (get-ipv6-sock)))
               (call-with-client-socket sock
                 (^[in out]
                   (do-accept)
                   (display (make-string *chunk-size* #\a) out)
                   (newline out)
                   (flush out)
                   (do-echo #t)
                   (string=? (read-line in)
                             (string-append "GOT:"
                                            (make-string *chunk-size* #\a)))))))
      (socket-shutdown server-sock)
      (socket-close server-sock)))
  ]
 [else])

(test* "socket w/ port error handling" (test-error)
       (let1 s (make-client-socket 'inet "localhost" *inet-port*)
         (close-socket s)
         (socket-input-port s)))

(test* "socket w/ port error handling" (test-error)
       (let1 s (make-client-socket 'inet "localhost" *inet-port*)
         (close-socket s)
         (socket-output-port s)))

(test* "getsockname/getpeername" #t
       (let* ([addr (make <sockaddr-in> :host :loopback :port 0)]
              [serv (make-server-socket addr :reuse-addr? #t)]
              [clnt (make-client-socket
                     (make <sockaddr-in>
                       :host :loopback
                       :port (sockaddr-port (socket-address serv))))])
         (begin0 (every (^[addr]
                          (and (= (sockaddr-addr addr) #x7f000001)
                               (= (sockaddr-port addr)
                                  (sockaddr-port (socket-address serv)))))
                        (list (socket-getsockname serv)
                              (socket-getpeername clnt)))
                 (socket-close clnt)
                 (socket-close serv))))


;; UDP

(let ([server-sock (make-socket PF_INET SOCK_DGRAM)]
      [server-addr (make <sockaddr-in> :host :any :port *inet-port*)])

  (socket-setsockopt server-sock SOL_SOCKET SO_REUSEADDR 1)
  (socket-bind server-sock server-addr)

  (test* "udp client socket" "GOT:abc"
         (let ([sock (make-socket PF_INET SOCK_DGRAM)]
               [addr (make <sockaddr-in> :host :loopback :port *inet-port*)])
           (socket-connect sock addr)
           (socket-send sock "abc")
           (receive (msg from) (socket-recvfrom server-sock 1024)
             (socket-sendto server-sock (string-append "GOT:" msg) from))
           (begin0 (string-incomplete->complete (socket-recv sock 1024))
             (socket-close sock)
             (socket-close server-sock))))
  )

(define (with-sr-udp proc)
  (let ([s-sock (make-socket PF_INET SOCK_DGRAM)]
        [r-sock (make-socket PF_INET SOCK_DGRAM)]
        [s-addr (make <sockaddr-in> :host :loopback :port *inet-port*)]
        [r-addr (make <sockaddr-in> :host :any :port *inet-port*)])
    (socket-setsockopt r-sock SOL_SOCKET SO_REUSEADDR 1)
    (socket-bind r-sock r-addr)
    (unwind-protect
        (proc s-sock s-addr r-sock r-addr)
      (begin (socket-close r-sock)
             (socket-close s-sock)))))

(with-sr-udp
 (^[s-sock s-addr r-sock r-addr]
   (let ([from (make <sockaddr-in>)]
         [data (make-u8vector 1024 #x3c)]
         [buf  (make-u8vector 1024 #xa5)])
     (test* "udp uvector API" '(#t #t)
            (begin
              (socket-sendto s-sock data s-addr)
              (receive (size f-addr) (socket-recvfrom! r-sock buf (list from))
                (list (eq? f-addr from)
                      (equal? buf data))))))))
(with-sr-udp
 (^[s-sock s-addr r-sock r-addr]
   (let* ([from   (make <sockaddr-in>)]
          [s-data (make-string 255 #\a)]
          [v-data (make-u8vector 255 48)]
          [data   `#(,s-data ,v-data)]
          [sbuf   (make-u8vector 1024)]
          [rbuf   (make-u8vector 1024 0)])

     (define (xtest sbuf)
       (socket-sendmsg s-sock (socket-buildmsg s-addr data '() 0 sbuf))
       (receive (size f-addr) (socket-recvfrom! r-sock rbuf (list from))
         (list (eq? f-addr from)
               (let1 expect (make-u8vector (+ (string-length s-data)
                                              (uvector-length v-data)))
                 (dotimes [n (string-length s-data)]
                   (u8vector-set! expect n (char->integer (string-ref s-data n))))
                 (dotimes [n (uvector-length v-data)]
                   (u8vector-set! expect (+ n (string-length s-data))
                                  (u8vector-ref v-data n)))
                 (every (^i (= (u8vector-ref rbuf i)
                               (u8vector-ref expect i)))
                        (iota size))))))

     (test* "udp sendmsg w/sendbuf" '(#t #t) (xtest sbuf))
     (test* "udp sendmsg w/o sendbuf" '(#t #t) (xtest #f)))))

;;-----------------------------------------------------------------------
;; srfi-106
(test-section "srfi-106")
(use srfi-106)
(test-module 'srfi-106)

(test-end)
