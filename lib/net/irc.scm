;;;; net.irc -- simple IRC client module

(define-module net.irc
  (use srfi-1)      ;; list library
  (use srfi-2)      ;; and-let*
  (use srfi-13)     ;; string library
  (use srfi-19)     ;; time and date library
  (use gauche.net)
  (use gauche.charconv)
  (use net.client)
  (export-all))
(select-module net.irc)

;; inherit <net-client> and add some additional slots to keep track of
;; an irc session
(define-class <irc-conn> (<net-client>)
  ((info :initform '() :accessor irc-conn-info)
   (channels :initform '() :accessor irc-channels)
   (log  :inifform #f  :accessor irc-log)
   ))

(define (open-irc host . keys)
  (and-let* ((net-conn (apply open-net-connection host
                              (append '(:port 6667 :status 220) keys)))
             (sock (slot-ref net-conn 'socket))
             (out (socket-output-port sock))
             (resp (slot-ref net-conn 'response))
             (irc (make <irc-conn> :socket sock :response resp))
             (serv (car (string-split (cadr resp) #\space))))
    ;; register the initial domain info
    (slot-set! irc 'info `((server . ,d)))
    ;; authenticate
    ;; return the ftp connection
    irc))
