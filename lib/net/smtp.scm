;;;; net.smtp -- simple SMTP module

(define-module net.smtp
  (use srfi-1)      ;; list library
  (use srfi-2)      ;; and-let*
  (use srfi-13)     ;; string library
  (use srfi-19)     ;; time and date library
  (use gauche.charconv)
  (use gauche.net)
  (use net.client)
  (export-all))
(select-module net.smtp)

;; not used unless you're working with binary attachments, so we
;; autoload these
(autoload rfc.base64 base64-encode base64-encode-string)
(autoload rfc.quoted-printable quoted-printable-encode
          quoted-printable-encode-string)

(define (make-io-encoder encoder)
  (lambda (obj)
    (cond ((string? obj)
           (with-input-from-string obj encoder))
          ((port? obj)
           (with-input-from-port obj encoder))
          (else
           (error "don't know how to encode:" obj)))))

(define base64-encoder (make-io-encoder base64-encode))
(define qp-encoder (make-io-encoder quoted-printable-encode))
(define (smtp-identity-encoder obj)
  (cond ((string? obj) (display obj))
        ((port? obj)   (copy-port obj (current-output-port) :unit 1024))
        (else (error "don't know hot to encode:" obj))))

(define (string-lines str)
  (with-input-from-string str
    (lambda ()
      (let loop ((i (read-byte)) (count 0))
        (cond ((eof-object? i) (number->string count))
              ((eq? i 10) (loop (read-byte) (+ count 1)))
              (else (loop (read-byte) count)))))))

;; inherit <net-client> and add an additional info alist
(define-class <smtp-client> (<net-client>)
  ((info :initform '() :accessor smtp-client-info)))

;; a convenience class to make attachments
(define-class <smtp-attach> ()
  ((file :init-value #f :init-keyword :file)
   (text :init-value #f :init-keyword :text)
   (content-type :init-value #f :init-keyword :content-type)
   (charset :init-value #f :init-keyword :charset)
   ;; Content-Transfer-Encoding
   (encoding :init-value #f :init-keyword :encoding)
   ))

(define smtp-mime-version "1.0")

(define smtp-encoders
  (hash-table 'equal?
              `("base64" . ,base64-encoder)
              `("quoted-printable" . ,qp-encoder)))

(define smtp-port
  (or (and-let* ((serv (sys-getservbyname "smtp" "tcp")))
        (slot-ref serv 'port))
      25))

(define smtp-connect-status 220)

(define (open-smtp host . keys)
  (and-let* ((net-client (apply open-net-client host
                                :port smtp-port
                                :greeting? #t
                                :status smtp-connect-status
                                keys))
             (sock (slot-ref net-client 'socket))
             (out (socket-output-port sock))
             (resp (slot-ref net-client 'response))
             (smtp (make <smtp-client> :socket sock :response resp))
             (d (car (string-split (cadr resp) #\space))))
    ;; register the initial domain info
    (slot-set! smtp 'info `((domain . ,d)))
    ;; send our greeting
    (format out "EHLO ~A\r\n" (sys-gethostname))
    ;; get confirmation
    (and (net-response-check (net-get-response smtp) 250)
         smtp)))

(define (smtp-header name value . keys)
  (let* ((val (if (list? value) (string-join value "\r\n  ") value))
         (str (apply mime-header-encode val keys)))
    ;; value should be broken at 72 chars, but we'll be lazy for now
    (cond ((and (> (+ (string-size name) (string-size str)) 70)
                (string-scan str "; "))
           =>
           (lambda (i)
             (set! str (string-append (substring str 0 (+ i 1))
                                      "\r\n\t"
                                      (substring str (+ i 2)
                                                 (string-length str)))))))
    (string-crlf (format #f "~A: ~A" name str))))

(define (smtp-send-header smtp name value . keys)
  (format (socket-output-port (slot-ref smtp 'socket))
          (apply smtp-header name value keys)))

(define smtp-mail   (net-command 'MAIL))
(define smtp-rcpt   (net-command 'RCPT))
(define smtp-data   (net-command 'DATA))
(define smtp-reset  (net-command 'RSET))
(define smtp-verify (net-command 'VRFY))
(define smtp-expand (net-command 'EXPN))
(define smtp-help   (net-command 'HELP))
(define smtp-quit   (net-command 'QUIT))
(define smtp-noop   (net-command 'NOOP))

(define (smtp-mail-from smtp address . keys)
  (smtp-mail smtp "FROM:<~A>" address))

(define (smtp-recipient smtp addrs)
  (smtp-rcpt smtp "TO:<~A>" (car addrs)))

(define smtp-header-order
  '("From" "To" "Cc" "Subject" "Date"))

(define (smtp-build-headers headers . keys)
  (with-output-to-string
    (lambda ()
      (let ((ls (map car headers)))
        ;; write fixed headers first
        (for-each
         (lambda (h)
           (and-let* ((v (cdr (or (assq h headers) '(#f . #f)))))
             (unless (or (equal? v "") (null? v))
               (display (apply smtp-header h v keys))
               (set! ls (delete h ls)))))
         (get-keyword :header-order keys smtp-header-order))
        (for-each
         (lambda (h)
           (and-let* ((v (cdr (assq h headers))))
             (unless (or (equal? v "") (null? v))
               (display (apply smtp-header h v keys)))))
         ls)))))

(define (smtp-send-headers smtp headers . keys)
  (display (apply smtp-build-headers headers keys)
           (socket-output-port (slot-ref smtp 'socket))))

(define (smtp-send-body smtp body)
  (format (socket-output-port (slot-ref smtp 'socket))
          ;;"\r\n~A\r\n.\r\n" body)
          "~A\r\n.\r\n" body)
  (net-get-response smtp))

(define (listify obj)
  (if (or (pair? obj) (null? obj)) obj (list obj)))

(define (smtp-parse-boundary ctype)
  (cond ((and (string? ctype)
              (rxmatch #/(\;|\s)boundary=\"([^\"])\"/
                       ctype))
         => (^m (rxmatch-substring m)))
        (else #f)))

(define (smtp-generate-boundary)
  (string-append "----------------Multipart_"
                 (number->string (sys-time))
                 ))

(define (smtp-generate-msgid)
  (string-append "<" (number->string (sys-time))
                 "." (number->string (sys-getpid))
                 "." (symbol->string (gensym))
                 "@" (sys-gethostname) ">"))

(define (smtp-choose-encoding text ctype)
  (rxmatch-cond
    ((rxmatch #/charset=([^\s\"\;]*)/ ctype) (#f charset)
     charset)
    ((rxmatch #/charset=\"([^\"]*)\"/ ctype) (#f charset)
     charset)
    ((rxmatch #/^[\x01-\x7e]/ text) (#f)
     "utf-7")  ;; how to detect and default to iso-2022-jp?
    (else
     "us-ascii")))

(define (parse-mail-address addr)
  (rxmatch-case addr
    (#/\s*([^\"\s][^<\"]*[^\"\s])\s*<([^\"@]+@[^@]+)>/ (#f name base)
      (list base name))
    (#/\s*"([^\"]*)"\s*<([^\"@]+@[^@]+)>/ (#f name base)
      (list base name))
    (#/<([^\"@]+@[^@]+)>/ (#f base)
      (list base ""))
    (else
     (list addr ""))))

(define (smtp-build-message . keys)
  (let*
      (;; from info
       (user (get-keyword* :user keys (sys-uid->user-name (sys-getuid))))
       (localhost (get-keyword* :localhost keys (sys-gethostname)))
       (from (get-keyword* :from keys (string-append user "@" localhost)))
       (from-addr (get-keyword :from-addr keys from))
       ;; to info
       (host (get-keyword :host keys "localhost"))
       (to (listify (get-keyword :to keys '())))
       (cc (listify (get-keyword :cc keys '())))
       (bcc (listify (get-keyword :bcc keys '())))
       (rcpt (append to cc bcc))
       ;; body
       (body (get-keyword :body keys ""))
       ;; meta-info
       (ctype (get-keyword :content-type keys "text/plain"))
       (charset (get-keyword* :charset keys
                              (smtp-choose-encoding body ctype)))
       (cencode (get-keyword :encoding keys "7bit"))
       (boundary (get-keyword* :boundary keys
                               (or (smtp-parse-boundary ctype)
                                   (smtp-generate-boundary))))
       (msgid (get-keyword* :message-id keys (smtp-generate-msgid)))
       ;; other headers
       (subject (get-keyword :subject keys ""))
       (date (get-keyword* :date keys (date->string (current-date)
                                                    "~a, ~e ~b ~Y ~T ~z")))
       (misc (get-keyword :headers keys '()))
       (headers (append `(("From"     . ,from)
                          ("To"       . ,to)
                          ("Cc"       . ,cc)
                          ("Subject"  . ,subject)
                          ("Date"     . ,date)
                          )
                        misc))
       ;; attachments
       (attachments (map (lambda (a)
                           (if (pair? a)
                             (apply make (cons <smtp-attach> a))
                             a))
                         (get-keyword :attachments keys '())))
       )
    ;; make sure we're sending this to someone
    (unless (pair? rcpt)
      (error "no recipients of message: " keys))
    ;; add charset to content-type if needed
    (when (rxmatch #/^text\//i ctype)
      (unless (rxmatch #/charset=/i ctype)
        (set! ctype (format #f "~A; charset=~A" ctype charset))))
    ;; merge attachments
    (cond
      ((pair? attachments)
       (unless (equal? body "")
         (push! attachments (make <smtp-attach>
                              :text body
                              :content-type ctype
                              :charset charset
                              :encoding cencode)))
       ;; should allow override of multipart/alternative, etc.
       (set! ctype (format #f "multipart/mixed; boundary=\"~A\"" boundary))
       (set! cencode #f)
       ;; merge parts into a single body
       (set!
        body
        (with-output-to-string
          (lambda ()
            (display "This is a multi-part message in MIME format.\r\n")
            (for-each
             (lambda (a)
               (let ((a/ctype (or (slot-ref a 'content-type) "text/plain"))
                     (a/charset (slot-ref a 'charset))
                     (a/text (or (slot-ref a 'text)
                                 (open-input-file (slot-ref a 'file))))
                     (a/encoding (string-downcase
                                  (x->string (slot-ref a 'encoding)))))
                 ;; convert if different charset
                 (when (and a/charset
                            (rxmatch #/^text\//i a/ctype)
                            (not (string-ci=? a/charset
                                              (symbol->string
                                               (gauche-character-encoding)))))
                   (set! a/text (ces-convert (if (port? a/text)
                                               (port->string a/text)
                                               a/text)
                                             (gauche-character-encoding)
                                             a/charset)))
                 ;; force a subtype
                 (unless (string-index a/ctype #\;)
                   (set! a/ctype (format #f "~A; filename=\"~A\""
                                         a/ctype (or (slot-ref a 'file) ""))))
                ;; display the boundary and content-type
                 (format #t "--~A\r\nContent-Type: ~A\r\n" boundary a/ctype)
                 ;; display optional encoding
                 (when (and a/encoding (not (equal? a/encoding "")))
                   (format #t "Content-Transfer-Encoding: ~A\r\n" a/encoding))
                 (display "\r\n")
                 ;; display the text
                 (let ((enc (hash-table-get smtp-encoders a/encoding
                                            smtp-identity-encoder)))
                   (enc a/text))))
             attachments)
            (format #t "\r\n--~A--\r\n" boundary)))))
      (else
       ;; no attachments, just convert body charset if needed
       (when (and charset
                  (rxmatch #/^text\//i ctype)
                  (not (string-ci=? charset
                                    (symbol->string
                                     (gauche-character-encoding)))))
         (set! body (ces-convert body
                                 (gauche-character-encoding)
                                 charset)))))
    ;; add content headers
    (set! headers
          (append headers `(("Mime-Version" . ,smtp-mime-version)
                            ("Message-Id"   . ,msgid)
                            ("Content-Type" . ,ctype)
                            ,@(if (and cencode (not (equal? cencode ""))
                                       (not (pair? attachments)))
                                `(("Content-Transfer-Encoding" . ,cencode))
                                '())
                            ("Content-Length" . ,(number->string (string-size body)))
                            ("Lines" . ,(string-lines body))
                            )))
    ;; return the headers and body concatenated
    (string-append (apply smtp-build-headers headers
                          :charset charset keys)
                   body)))

;; convenience routine to send a single message
(define (send-mail . keys)
  (let* ((message (apply smtp-build-message keys))
         (host (get-keyword :host keys "localhost"))
         (user (get-keyword* :user keys (sys-uid->user-name (sys-getuid))))
         (localhost (get-keyword* :localhost keys (sys-gethostname)))
         (from (get-keyword* :from keys (string-append user "@" localhost)))
         (from-base (car (parse-mail-address from)))
         (from-addr (get-keyword :from-addr keys from-base))
         (to (listify (get-keyword :to keys '())))
         (cc (listify (get-keyword :cc keys '())))
         (bcc (listify (get-keyword :bcc keys '())))
         (rcpt (append to cc bcc)))
    ;; open port and send data
    (let ((smtp (apply open-smtp host keys)))
      (unless smtp
        (error "couldn't open smtp connection"))
      (unless (and (net-response-ok? (smtp-mail-from smtp from-addr))
                   (net-response-ok? (smtp-recipient smtp rcpt))
                   (smtp-data smtp ""))
        (error "couldn't initiate message"))
      (unless (and (net-response-ok? (smtp-send-body smtp message))
                   (net-response-ok? (smtp-quit smtp "")))
        (error "couldn't finishing message"))
      )))


