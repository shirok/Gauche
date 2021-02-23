;;;; net.client -- network client abstract class

(define-module net.client
  (use scheme.list) ;; list library
  (use srfi-13)     ;; string library
  (use rfc.base64)
  (use gauche.net)
  (use gauche.threads)
  (use gauche.charconv)
  (export-all))
(select-module net.client)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; classes

(define-class <net-client> ()
  ((socket   :init-keyword :socket :initform #f
             :accessor net-client-socket)
   (response :init-keyword :response :initform #f
             :accessor net-client-response)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general utilities

;; we often want to use the current username in network protocols
(define (sys-getusername) (sys-uid->user-name (sys-getuid)))

;; use threads when we don't want to hang on network operations
(define (with-timeout timeout thunk . opt-handler)
  (let ((handler (if (pair? opt-handler) (car opt-handler) (lambda () #f))))
    (if (and timeout (> timeout 0))
      (let ((thread (make-thread thunk)))
        (thread-start! thread)
        (with-exception-handler
            (lambda (err)
              (cond ((join-timeout-exception? err)
                     (thread-terminate! thread)
                     (handler))
                    (else
                     (raise err))))
          (cut thread-join! thread timeout)))
      ;; no timeout, just run thunk normally
      (thunk))))

;; force network crlf in a string
(define (string-crlf str)
  (rxmatch-case str
    [#/\r\n$/ (#f) str]
    [#/(.*)(?:\r|\n)$/ (#f head) (string-append head "\r\n")]
    [else (string-append str "\r\n")]))

;; convert a header to =?<charset>?b?<encoded>?= style 7-bit encoded
;; format (see scmail for the inverse operation)
(define (mime-header-encode str :key
                            (8bit #f)
                            ((:from-charset from) (gauche-character-encoding))
                            ((:charset to) (gauche-character-encoding)))
  (cond [8bit str]
        [(rxmatch #/[^\x01-\x7e]/ str)
         (let1 buf (if (equal? from to) str (ces-convert str from to))
           ;; convert to base64 if not 7bit safe
           (format #f "=?~A?b?~A?=" to (base64-encode-string buf)) )]
        [else str]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; common net command format utilities

;; get a "<number> <message>" style network response
(define (net-get-response client)
  (let ((port (socket-input-port (net-client-socket client))))
    (let loop ((line (read-line port))
               (msg '()))
      (rxmatch-case line
        (#/^(\d+) (.*)$/
            (#f code message)
          (let ((resp (cons (string->number code)
                            (reverse (cons message msg)))))
            (slot-set! client 'response resp)
            resp))
        (#/^(\d+)\-(.*)$/
            (#f code message)
          (loop (read-line port) (cons message msg)))
        (else
         #f)))))

;; read a "." delimited message as a list of lines
(define (net-get-list client)
  (let ((port (socket-input-port (net-client-socket client))))
    (let loop ((line (read-line port))
               (msg '()))
      (if (or (eof-object? line) (equal? line "."))
        (reverse msg)
        (loop (read-line port) (cons line msg))))))

;; read a "." delimited message as a single string
(define (net-get-data client)
  (fold string-append "" (net-get-list client)))

;; test an exact response code
(define (net-response-check resp code)
  (and (pair? resp) (= (car resp) code)))

;; test a non-error response code
(define (net-response-ok? resp)
  (and (pair? resp) (<= 2 (quotient (car resp) 100) 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; building net commands

;; open a network connection
(define (open-net-client host :key
                         (port #f)
                         (timeout #f)
                         (greeting? #f)   ; wait for a greeting?
                         (status #f))     ; ... with what status?
  (with-timeout timeout
                (lambda ()
                  (and-let* ((sock (make-client-socket 'inet host port))
                             (out (socket-output-port sock))
                             (client (make <net-client> :socket sock)))
                    (if greeting?
                      (and-let* ((resp (net-get-response client)))
                        (if (if status
                              (net-response-check resp status)
                              (net-response-ok? resp))
                          client
                          #f))
                      client)))))

;; a basic network command
(define (net-command command)
  (lambda (client . args)
    (let ((out (socket-output-port (slot-ref client 'socket))))
      (if (pair? args)
        (let ((str (string-crlf (apply format #f args))))
          (format out "~A ~A" command str))
        (format out "~A" command))
      (net-get-response client))))

;; returns (resp list)
(define (net-list-command command)
  (let ((proc (net-command command)))
    (lambda (client . args)
      (and-let* ((resp (apply proc client args)))
        (if (net-response-ok? resp)
          (list resp (net-get-list client))
          (list resp #f))))))

;; returns (resp data)
(define (net-data-command command)
  (let ((proc (net-command command)))
    (lambda (client . args)
      (and-let* ((resp (apply proc client args)))
        (if (net-response-ok? resp)
          (list resp (net-get-data client))
          (list resp #f))))))
