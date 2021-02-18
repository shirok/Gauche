;;;; net.http -- simple HTTP module

(define-module net.http
  (use srfi-1)      ;; list library
  (use srfi-2)      ;; and-let*
  (use srfi-13)     ;; string library
  (use srfi-19)     ;; time and date library
  (use gauche.net)
  (use gauche.charconv)
  (use net.client)
  (export-all))
(select-module net.http)

;; inherit <net-client> and add an additional info alist
(define-class <http-conn> (<net-client>)
  ((cookies :initform '() :accessor http-conn-cookies)
   (info :initform '() :accessor http-conn-info)))

(define (parse-url url)
  (rxmatch-let (rxmatch #/^http:\/\/([-A-Za-z\d.]+)(:(\d+))?(\/.*)?/ url)
     (#f host #f port path)
   (values host port path)))

(define (open-http host . keys)
  (and-let* ((port (or (get-keyword :port keys #f) 80))
             (net-conn (apply open-net-client host
                              (append `(:port ,port) keys)))
             (sock (slot-ref net-conn 'socket))
             (out (socket-output-port sock))
             (http (make <http-conn> :socket sock))
             ;; auth info
             (user (get-keyword* :user keys (sys-getusername)))
             (pass (get-keyword* :password keys "")))
    ;; authenticate if needed
    ;; return the http connection
    http))

;; performs an http GET and skips the headers, returning a port which
;; can be used to simply read the remote file
(define (open-input-http url)
  (receive (host port path) (parse-url url)
    (let ((http (open-http host :port port)))
      (when (http-get http path :host host)
        (and-let* ((in (socket-input-port (slot-ref http 'socket))))
          (let loop ((skip (read-line in)))
            (if (or (eof-object? skip) (equal? skip ""))
              in
              (loop (read-line in)))))))))

;; shortcut procedure to fetch an http uri as a string
(define (fetch-http url)
  (and-let* ((p (open-input-http url))
             (str (port->string p)))
    (close-input-port p)
    str))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; protocol-level interface (needs work)

(define (http-get http path . keys)
  (let ((out (socket-output-port (slot-ref http 'socket))))
    (format out "GET ~A HTTP/1.0\r\n\r\n" path)))

(define http-head    (net-command 'HEAD))
(define http-post    (net-command 'POST))
(define http-put     (net-command 'PUT))
(define http-delete  (net-command 'DELETE))
(define http-trace   (net-command 'TRACE))
(define http-connect (net-command 'CONNECT))
