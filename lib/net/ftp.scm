;;;; net.ftp -- simple FTP module

(define-module net.ftp
  (use scheme.list) ;; list library
  (use srfi-13)     ;; string library
  (use srfi-19)     ;; time and date library
  (use gauche.net)
  (use gauche.charconv)
  (use net.client)
  (export-all))
(select-module net.ftp)

;; inherit <net-client> and add an additional info alist
(define-class <ftp-conn> (<net-client>)
  ((info :initform '() :accessor ftp-conn-info)))

(define (sys-getusername) (sys-uid->user-name (sys-getuid)))

(define (open-ftp host . keys)
  (and-let* ((net-conn (apply open-net-client host
                              (append '(:port 21 :status 220) keys)))
             (sock (slot-ref net-conn 'socket))
             (out (socket-output-port sock))
             (resp (slot-ref net-conn 'response))
             (ftp (make <ftp-conn> :socket sock :response resp))
             (serv (car (string-split (cadr resp) #\space)))
             ;; auth info
             (user (get-keyword* :user keys "anonymous"))
             (pass (get-keyword* :password keys
                                 (string-append (sys-getusername)
                                                "@" (sys-gethostname)))))
    ;; register the initial domain info
    (slot-set! ftp 'info `((server . ,d)))
    ;; authenticate
    ;; return the ftp connection
    ftp))

;; USER <SP> <username> <CRLF>
;; PASS <SP> <password> <CRLF>
;; ACCT <SP> <account-information> <CRLF>
;; CWD  <SP> <pathname> <CRLF>
;; CDUP <CRLF>
;; SMNT <SP> <pathname> <CRLF>
;; QUIT <CRLF>
;; REIN <CRLF>
;; PORT <SP> <host-port> <CRLF>
;; PASV <CRLF>
;; TYPE <SP> <type-code> <CRLF>
;; STRU <SP> <structure-code> <CRLF>
;; MODE <SP> <mode-code> <CRLF>
;; RETR <SP> <pathname> <CRLF>
;; STOR <SP> <pathname> <CRLF>
;; STOU <CRLF>
;; APPE <SP> <pathname> <CRLF>
;; ALLO <SP> <decimal-integer>
;;     [<SP> R <SP> <decimal-integer>] <CRLF>
;; REST <SP> <marker> <CRLF>
;; RNFR <SP> <pathname> <CRLF>
;; RNTO <SP> <pathname> <CRLF>
;; ABOR <CRLF>
;; DELE <SP> <pathname> <CRLF>
;; RMD  <SP> <pathname> <CRLF>
;; MKD  <SP> <pathname> <CRLF>
;; PWD  <CRLF>
;; LIST [<SP> <pathname>] <CRLF>
;; NLST [<SP> <pathname>] <CRLF>
;; SITE <SP> <string> <CRLF>
;; SYST <CRLF>
;; STAT [<SP> <pathname>] <CRLF>
;; HELP [<SP> <string>] <CRLF>
;; NOOP <CRLF>

(define ftp-user (net-command 'USER))
(define ftp-pass (net-command 'PASS))
(define ftp-acct (net-command 'ACCT))
(define ftp-cwd  (net-command 'CWD))
(define ftp-cdup (net-command 'CDUP))
(define ftp-smnt (net-command 'SMNT))
(define ftp-rein (net-command 'REIN))
(define ftp-quit (net-command 'QUIT))
(define ftp-port (net-command 'PORT))
(define ftp-pasv (net-command 'PASV))
(define ftp-type (net-command 'TYPE))
(define ftp-stru (net-command 'STRU))
(define ftp-mode (net-command 'MODE))
(define ftp-retr (net-command 'RETR))
(define ftp-stor (net-command 'STOR))
(define ftp-stou (net-command 'STOU))
(define ftp-appe (net-command 'APPE))
(define ftp-allo (net-command 'ALLO))
(define ftp-rest (net-command 'REST))
(define ftp-rnfr (net-command 'RNFR))
(define ftp-rnto (net-command 'RNTO))
(define ftp-abor (net-command 'ABOR))
(define ftp-dele (net-command 'DELE))
(define ftp-rmd  (net-command 'RMD))
(define ftp-mkd  (net-command 'MKD))
(define ftp-list (net-command 'LIST))
(define ftp-nlst (net-command 'NLST))
(define ftp-site (net-command 'SITE))
(define ftp-syst (net-command 'SYST))
(define ftp-stat (net-command 'STAT))
(define ftp-help (net-command 'HELP))
(define ftp-noop (net-command 'NOOP))
