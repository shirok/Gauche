;;;; net.nntp -- simple NNTP module

(define-module net.nntp
  (use srfi-1)      ;; list library
  (use srfi-2)      ;; and-let*
  (use srfi-13)     ;; string library
  (use gauche.net)
  (use net.client)
  (use net.smtp)
  (export-all))
(select-module net.nntp)

(define-class <nntp-client> (<smtp-client>) ())

(define nntp-port
  (or (and-let* ((serv (sys-getservbyname "nntp" "tcp")))
        (slot-ref serv 'port))
      119))

(define (open-nntp host . keys)
  (and-let*
      ((net-client (apply open-net-client host
                          :port nntp-port keys))
       (sock (slot-ref net-client 'socket))
       (out (socket-output-port sock))
       (resp (slot-ref net-client 'response))
       (nntp (make <nntp-client> :socket sock :response resp)))
    nntp))

(define nntp-article   (net-command 'ARTICLE))
(define nntp-body      (net-command 'BODY))
(define nntp-group     (net-command 'GROUP))
(define nntp-head      (net-command 'HEAD))
(define nntp-ihave     (net-command 'IHAVE))
(define nntp-last      (net-command 'LAST))
(define nntp-list      (net-list-command 'LIST))
(define nntp-newgroups (net-command 'NEWGROUPS))
(define nntp-newnews   (net-command 'NEWNEWS))
(define nntp-next      (net-command 'NEXT))
(define nntp-post      (net-command 'POST))
(define nntp-quit      (net-command 'QUIT))
(define nntp-slave     (net-command 'SLAVE))
(define nntp-stat      (net-command 'STAT))

;; convenience routine to post a single article
(define (post-article . keys)
  (let ((message (apply smtp-build-message keys)))
    ;; optionally open new port
    (let ((nntp (get-keyword :client keys))
          (close-when-done #f))
      (unless nntp
        (set! nntp (apply open-nntp host keys))
        (set! close-when-done #t))
      (unless nntp
        (error "couldn't open smtp connection"))
      (unless (net-response-ok? (nntp-post nntp from-addr))
        (error "couldn't initiate message"))
      (unless (net-response-ok? (smtp-send-body nntp message))
        (error "couldn't finishing message"))
      (when close-when-done
        (nntp-quit nntp ""))
      )))
