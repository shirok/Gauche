;;
;; testing net
;;

(use gauche.test)

(load "net")
(import gauche.net)

(test-start "net")

;;-----------------------------------------------------------------
(test-section "socket address")

(test "sockaddr_un" #t
      (lambda ()
        (let ((addr (make <sockaddr-un> :path "/tmp/xxx")))
          (and (eq? (sockaddr-family addr) :unix)
               (equal? (sockaddr-name addr) "/tmp/xxx")
               #t))))

(test "sockaddr_in" #t
      (lambda ()
        (let ((addr (make <sockaddr-in> :host "127.0.0.1" :port 80)))
          (and (eq? (sockaddr-family addr) :inet)
               (equal? (sockaddr-name addr) "127.0.0.1:80")
               #t))))

(test "sockaddr_in" #t
      (lambda ()
        (let ((addr (make <sockaddr-in> :host "localhost" :port 23)))
          (and (eq? (sockaddr-family addr) :inet)
               (equal? (sockaddr-name addr) "127.0.0.1:23")
               #t))))

(test "sockaddr_in" #t
      (lambda ()
        (let ((addr (make <sockaddr-in> :host :any :port 7777)))
          (and (eq? (sockaddr-family addr) :inet)
               (equal? (sockaddr-name addr) "0.0.0.0:7777")
               #t))))

(test "sockaddr_in" #t
      (lambda ()
        (let ((addr (make <sockaddr-in> :host :broadcast)))
          (and (eq? (sockaddr-family addr) :inet)
               (equal? (sockaddr-name addr) "255.255.255.255:0")
               #t))))

;;-----------------------------------------------------------------
(test-section "socket")


;;-----------------------------------------------------------------
(test-section "netdb")

(test "gethostbyname" #t
      (lambda ()
        (let ((host (sys-gethostbyname "localhost")))
          (and (equal? (slot-ref host 'name) "localhost")
               (member "127.0.0.1" (slot-ref host 'addresses))
               #t))))

(test "gethostbyaddr" #t
      (lambda ()
        (let ((host (sys-gethostbyaddr "127.0.0.1" af_inet)))
          (and (or (member "localhost" (slot-ref host 'aliases))
                   (equal? (slot-ref host 'name) "localhost"))
               (member "127.0.0.1" (slot-ref host 'addresses))
               #t))))

(test "getprotobyname" '(("ip" 0) ("tcp" 6) ("udp" 17))
      (lambda ()
        (let ((tcp (sys-getprotobyname "tcp"))
              (udp (sys-getprotobyname "udp"))
              (ip  (sys-getprotobyname "ip")))
          (map (lambda (proto)
                 (list (slot-ref proto 'name)
                       (slot-ref proto 'proto)))
               (list ip tcp udp)))))

(test "getprotobynumber" '(#t #t #t)
      (lambda ()
        (map (lambda (proto name)
               (or (member name (slot-ref proto 'aliases))
                   (equal? name (slot-ref proto 'name))))
             (map sys-getprotobynumber '(0 6 17))
             '("ip" "tcp" "udp"))))

(test "getservbyname" '(("telnet" 23 "tcp") ("ftp" 21 "tcp"))
      (lambda ()
        (map (lambda (name proto)
               (let ((x (sys-getservbyname name proto)))
                 (and x
                      (map (lambda (s) (slot-ref x s))
                           '(name port proto)))))
             '("telnet" "ftp")
             '("tcp"    "tcp"))))

(test "getservbyport" '(("telnet" 23 "tcp") ("ftp" 21 "tcp"))
      (lambda ()
        (map (lambda (port proto)
               (let ((x (sys-getservbyport port proto)))
                 (and x
                      (map (lambda (s) (slot-ref x s))
                           '(name port proto)))))
             '(23       21)
             '("tcp"    "tcp"))))

(test-end)
