;;; ftp.scm - FTP library
;;;
;;;  Copyright (c) 2005 OOHASHI Daichi, All rights reserved.
;;;  Copyright (c) 2007-2019  Shiro Kawai  <shiro@acm.org>
;;;
;;;  Redistribution and use in source and binary forms, with or without
;;;  modification, are permitted provided that the following conditions
;;;  are met:
;;;
;;;  1. Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.
;;;
;;;  2. Redistributions in binary form must reproduce the above copyright
;;;     notice, this list of conditions and the following disclaimer in the
;;;     documentation and/or other materials provided with the distribution.
;;;
;;;  3. Neither the name of the authors nor the names of its contributors
;;;     may be used to endorse or promote products derived from this
;;;     software without specific prior written permission.
;;;
;;;  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;  TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;  PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;  LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;  NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;  SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;; RFC  959 FILE TRANSFER PROTOCOL (FTP)
;; RFC 1123 Requirements for Internet Hosts -- Application and Support
;; RFC 2428 FTP Extensions for IPv6 and NATs

(define-module rfc.ftp
  (use srfi-13)
  (use srfi-19)
  (use rfc.uri)
  (use gauche.net)
  (use gauche.logger)
  (export <ftp-connection> <ftp-error> call-with-ftp-connection
          ftp-chdir ftp-current-directory ftp-get ftp-size
          ftp-help ftp-list ftp-login ftp-ls ftp-mdtm ftp-mtime
          ftp-mkdir ftp-name-list ftp-noop ftp-passive?  ftp-put
          ftp-put-unique ftp-quit ftp-remove ftp-rename ftp-rmdir
          ftp-site ftp-stat ftp-system ftp-transfer-type
          ))
(select-module rfc.ftp)

(define-constant *default-ftp-port* 21)

(define-constant *anon-user* "anonymous")

(define-constant *anon-pass* "anonymous@")

(define-class <ftp-connection> ()
  ((transfer-type :init-value 'binary
                  :init-keyword :transfer-type
                  :accessor ftp-transfer-type)
   (passive       :init-keyword :passive
                  :getter ftp-passive?)
   (log-drain     :init-keyword :log-drain
                  :init-value #f)
   (socket        :init-keyword :socket)))

(define (call-with-ftp-connection uri proc . keys)
  (receive (user host port) (uri-decompose-authority uri)
    (let1 conn (apply ftp-login host
                      (cond-list
                       [user @ (list :username user)]
                       [port @ (list :port port)]
                       [#t   @ keys]))
      (unwind-protect (proc conn)
        (ftp-quit conn)))))

;; rfc 959 6(5) type commands

;; USER <SP> <username> <CRLF>
;; PASS <SP> <password> <CRLF>
;; ACCT <SP> <account-information> <CRLF>
(define (ftp-login host :key (account "") (passive #f)
                   (port *default-ftp-port*) (username *anon-user*)
                   (password (if (string=? username *anon-user*) *anon-pass* ""))
                   (log-drain #f))
  (define (do-login conn)
    (let retry ()
      (if (string-prefix? "120" (get-response conn))
        (begin (sys-sleep 1) (retry))
        (let1 r1 (send-command conn "USER" username)
          (if (not (string-prefix? "3" r1))
            r1
            (let1 r2 (send-command conn "PASS" password)
              (if (not (string-prefix? "3" r2))
                r2
                (send-command conn "ACCT" account))))))))
  (let* ((conn (make <ftp-connection>
                 :passive passive
                 :socket (make-client-socket 'inet host port)
                 :log-drain log-drain))
         (res (do-login conn)))
    (if (not (string-prefix? "2" res))
      (ftp-error res)
      conn)))

;; rfc 959 6(1) type commands

;; QUIT <CRLF>
(define (ftp-quit conn)
  (unwind-protect
      (simple-command conn "QUIT")
    (if-let1 s (ref conn 'socket)
      (begin (socket-shutdown s) (set! (ref conn 'socket) #f)))))

;; *ABOR <CRLF>

;; *ALLO <SP> <decimal-integer> [<SP> R <SP> <decimal-integer>] <CRLF>


;; CWD  <SP> <pathname> <CRLF>
;; CDUP <CRLF>
(define (ftp-chdir conn dirname)
  (if (string=? dirname "..")
    (simple-command conn "CDUP")
    (simple-command conn "CWD" dirname)))

;; DELE <SP> <pathname> <CRLF>
(define (ftp-remove conn path) (simple-command conn "DELE" path))

;; HELP [<SP> <string>] <CRLF>
(define (ftp-help conn . opt) (apply simple-command conn "HELP" opt))

;; MKD  <SP> <pathname> <CRLF>
;; PWD  <CRLF>
(define (ftp-mkdir conn dirname)(parse-257 (simple-command conn "MKD" dirname)))
(define (ftp-current-directory conn) (parse-257 (simple-command conn "PWD")))

(define (parse-257 res)
  (rxmatch-if (#/^257 \"((?:[^\"]|\"\")+)\"/ res) (_ dirname)
    (regexp-replace-all #/""/ dirname "\"")
    (ftp-error res)))

;; *MODE <SP> <mode-code> <CRLF>
;; *SMNT <SP> <pathname> <CRLF>


;; SITE <SP> <string> <CRLF>
(define (ftp-site conn arg) (simple-command conn "SITE" arg))


;; *STRU <SP> <structure-code> <CRLF>


;; RMD  <SP> <pathname> <CRLF>
(define (ftp-rmdir conn dirname) (simple-command conn "RMD" dirname))

;; STAT [<SP> <pathname>] <CRLF>
(define (ftp-stat conn . opt) (apply simple-command conn "STAT" opt))

;; SYST <CRLF>
(define (ftp-system conn) (string-drop (simple-command conn "SYST") 4))

;; SIZE
(define (ftp-size conn path)
  (ftp-set-type conn)                   ; SIZE is affected by transfer type
  (x->integer (string-drop (simple-command conn "SIZE" path) 4)))

;; MDTM
(define (ftp-mdtm conn path) (simple-command conn "MDTM" path))

(define (ftp-mtime conn path :optional (local-time? #f))
  (rxmatch-let (#/(\d{4})(\d\d)(\d\d)(\d\d)(\d\d)(\d\d)/ (ftp-mdtm conn path))
      (#f year month day hour min sec)
    (date->time-utc
     (make-date 0 (x->integer sec) (x->integer min) (x->integer hour)
                (x->integer day) (x->integer month) (x->integer year)
                (if local-time? (date-zone-offset (current-date)) 0)))))

;; NOOP <CRLF>
(define (ftp-noop conn) (simple-command conn "NOOP"))


;;; rfc 959 6(2) type command

;; *APPE <SP> <pathname> <CRLF>

;; LIST [<SP> <pathname>] <CRLF>
;; NLST [<SP> <pathname>] <CRLF>
(define (make-lister cmd)
  (lambda (conn . opt)
    (req&recv conn
              (cut apply send-command conn cmd opt)
              (cut port->string-list <>)
              'ascii)))

(define ftp-list (make-lister "LIST"))
(define ftp-name-list (make-lister "NLST"))
(define ftp-ls ftp-name-list)

;; *REIN <CRLF>

;; *REST <SP> <marker> <CRLF>

;; RETR <SP> <pathname> <CRLF>
(define (ftp-get conn path :key (sink (open-output-string))
                                (flusher get-output-string))
  (req&recv conn
            (cut send-command conn "RETR" path)
            (lambda (in) (copy-port in sink) (flusher sink))))

;; STOR <SP> <pathname> <CRLF>
(define (ftp-put conn from-file :optional (to-file (sys-basename from-file)))
  (values-ref
   (call-with-input-file from-file
     (cute req&send conn (cut send-command conn "STOR" to-file) <>))
   0))

;; STOU <CRLF>
(define (ftp-put-unique conn from-file)
  (call-with-input-file from-file
    (cute req&send conn (cut send-command conn "STOU") <>)))

;;; rfc 959 6(3) type commands

;; RNFR <SP> <pathname> <CRLF>
;; RNTO <SP> <pathname> <CRLF>
(define (ftp-rename conn from to)
  (let1 res1 (send-command conn "RNFR" from)
    (if (not (string-prefix? "3" res1))
      (ftp-error res1)
      (let1 res2 (send-command conn "RNTO" to)
        (if (not (string-prefix? "2" res2))
          (ftp-error res2)
          res2)))))


;; private utilities

;; PORT <SP> <host-port> <CRLF>
;; EPRT <SP> <d><net-prt><d><net-addr><d><tcp-port><d> <CRLF>
;; PASV <CRLF>
;; EPSV <SP> <net-prt> <CRLF>
;; open data connection to server and pass it to proc.
(define (call-with-data-connection conn proc)
  ;; returns a thunk to create data connection socket, and a thunk to
  ;; clean up.
  (define (dcsock)
    (if (ftp-passive? conn)
      (if (ipv4? conn)
        (parse-227 (simple-command conn "PASV"))
        (parse-229 (simple-command conn "EPSV")))
      (make-asock)))
  (define (ipv4? conn)
    (is-a? (socket-address (ref conn 'socket)) <sockaddr-in>))
  (define (parse-227 res)
    (if (not (string-prefix? "227" res))
      (ftp-error res)
      (rxmatch-let (#/\((\d+),(\d+),(\d+),(\d+),(\d+),(\d+)\)/ res)
          (#f h1 h2 h3 h4 p1 p2)
        (let1 ds (make-client-socket 'inet
                                     (string-join (list h1 h2 h3 h4) ".")
                                     (+ (* 256 (x->integer p1))
                                        (x->integer p2)))
          (values (lambda () ds)
                  (lambda () (socket-shutdown ds 2)))))))
  (define (parse-229 res)
    (if (not (string-prefix? "229" res))
      (ftp-error res)
      (rxmatch-let (#/\((.+?)\)/ res) (#f s)
        (let1 d (regexp-quote (substring s 0 1))
          (rxmatch-let ((string->regexp #"~|d|~|d|~|d|([^~|d|]+)~|d|") s) (#f port)
            (let1 ds (make-client-socket
                      'inet
                      (sockaddr-addr (socket-address (ref conn 'socket)))
                      (string->number port))
              (values (lambda () ds)
                      (lambda () (socket-shutdown ds 2)))))))))
  (define (make-asock)
    (let* ((asock (make-server-socket (make (class-of
                                             (socket-address (ref conn 'socket)))
                                        :host "localhost")))
           (psname (sockaddr-name (socket-address asock))))
      (if (ipv4? conn)
        (rxmatch-let (#/(\d+)\.(\d+)\.(\d+)\.(\d+):(\d+)/ psname)
            (#f a b c d p)
          (receive (q r) (quotient&remainder (string->number p) 256)
            (simple-command conn
                            "PORT"
                            (format #f "~A,~A,~A,~A,~A,~A" a b c d q r))))
        (rxmatch-let (#/\[(.+?)\]:(\d+)/ psname) (#f host port)
          (simple-command conn
                          "EPRT"
                          (format #f "|2|~A|~A|" host port))))
      (let ((ds #f))
        (values (lambda () (set! ds (socket-accept asock)) ds)
                (lambda ()
                  (when ds (socket-shutdown ds 2))
                  (socket-shutdown asock 2))))))
  (receive (sock-thunk cleanup-thunk) (dcsock)
    (unwind-protect (proc sock-thunk) (cleanup-thunk))))

;; error reporting routine.
;; it should be raise conditon with respect to the response code.
(define-condition-type <ftp-error> <error> #f)

(define (ftp-error res . args)
  (apply error <ftp-error> res args))

;; send ftp command
(define (send-command conn cmd . args)
  (let ((out (socket-output-port (ref conn 'socket)))
        (log (ref conn 'log-drain))
        (msg (with-output-to-string
               (lambda ()
                 (display cmd)
                 (for-each (lambda (arg) (display " ") (display arg)) args)))))
    (display msg out) (display "\r\n" out) (flush out)
    (when log
      (if (equal? cmd "PASS")
        (log-format log ">>> PASS *****") ;; do not log password
        (log-format log ">>> ~a" msg)))
    (get-response conn)))

;; send rfc-959 6(2) type command
(define (simple-command conn cmd . args)
  (let1 res (apply send-command conn cmd args)
    (if (string-prefix? "2" res)
      res
      (ftp-error res))))

;; receive response from server
(define (get-response conn)
  (define (get-resp in)
    (let1 l (read-line in)
      (rxmatch-if (#/^(\d\d\d)-/ l) (#f code)
        (let loop ((rs (list l))
                   (line (read-line in)))
          (if (and (string-prefix? code line)
                   (char=? (string-ref line 3) #\space))
            (string-concatenate-reverse rs)
            (loop (cons* line "\n" rs) (read-line in))))
        l)))
  (let1 res (get-resp (socket-input-port (ref conn 'socket)))
    (cond ((ref conn 'log-drain) => (cut log-format <> "<<< ~a" res)))
    (if (#/^[45]/ res)
      (ftp-error res)
      res)))


;; TYPE <SP> <type-code> <CRLF>
(define (ftp-set-type conn :optional (type (ftp-transfer-type conn)))
  (simple-command conn "TYPE"
                  (case type
                    [(ascii) "A"]
                    [(binary image) "I"]
                    [else (error "Invalid transfer type:" type)])))

;; requrest server to send data and receive it
(define (req&recv conn cmdproc reader . opt)
  (apply ftp-set-type conn opt)
  (call-with-data-connection conn
    (lambda (get-data-socket)
      (let1 res (cmdproc)
        (if (not (string-prefix? "1" res))
          (ftp-error res)
          (begin0
           (let1 s (get-data-socket)
             (unwind-protect (reader (socket-input-port s))
               (socket-close s)))
           (let1 res2 (get-response conn)
             (when (not (string-prefix? "2" res2))
               (ftp-error res2)))))))))

;; request server to receive data and send data
(define (req&send conn cmdproc port)

  (define (copy-data get-data-socket)
    (copy-port port (socket-output-port (get-data-socket))))
  (define (send-data)
    (call-with-data-connection conn
      (lambda (get-data-socket)
        (let1 res (cmdproc)
          (rxmatch-case res
            [#/^1\d\d FILE: (.+)$/ (#f dst-path)
             ;; RFC 1123 - 4.1.2.9  STOU Command: RFC-959 Section 4.1.3
             (copy-data get-data-socket) dst-path]
            [#/^1/ ()
             ;; in case if the server doesn't conform RFC1123
             (copy-data get-data-socket) #f]
            [else (ftp-error res)])))))
  (define (retrieve-response res dst-path)
    (rxmatch-case res
      [#/^2/ () (values res dst-path)]
      ; vsftpd duplicates the 1XX reply for STOU.
      [#/^1/ () (retrieve-response (get-response conn) dst-path)]
      [else (ftp-error res)]))

  (ftp-set-type conn)
  (let1 dst-path (send-data)
    (retrieve-response (get-response conn) dst-path)))

