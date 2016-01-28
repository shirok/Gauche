;;;
;;; http.scm - HTTP 1.1
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

;; HTTP handling routines.

;; RFC2616 Hypertext Transfer Protocol -- HTTP/1.1
;;  http://www.ietf.org/rfc/rfc2616.txt
;; RFC2617 HTTP Authentication: Basic and Digest Access Authentication
;;  http://www.ietf.org/rfc/rfc2617.txt
;; RFC2388 Returning Values from Forms:  multipart/form-data
;;  http://www.ietf.org/rfc/rfc2388.txt

;; HTTP 1.1 has lots of features.  This module doesn't yet cover all of them.
;; The features required for typical client usage are implemented first.

(define-module rfc.http
  (use srfi-11)
  (use srfi-13)
  (use rfc.822)
  (use rfc.uri)
  (use rfc.base64)
  (use gauche.net)
  (use gauche.parameter)
  (use gauche.charconv)
  (use gauche.sequence)
  (use gauche.uvector)
  (use util.match)
  (use text.tree)
  (export <http-error>
          http-user-agent make-http-connection reset-http-connection
          http-compose-query http-compose-form-data
          http-status-code->description

          http-proxy http-request
          http-null-receiver http-string-receiver http-oport-receiver
          http-file-receiver http-cond-receiver
          http-null-sender http-string-sender http-blob-sender
          http-file-sender http-multipart-sender

          http-get http-head http-post http-put http-delete
          http-default-auth-handler
          http-default-redirect-handler

          http-secure-connection-available?
          )
  )
(select-module rfc.http)

;;
(autoload rfc.mime
          mime-compose-message
          mime-compose-message-string
          mime-compose-parameters
          mime-parse-content-type)
(autoload rfc.tls
          make-tls tls-destroy tls-connect tls-input-port tls-output-port tls-close)

(autoload file.util file-size find-file-in-paths null-device)

;;==============================================================
;; Conditions
;;

(define-condition-type <http-error> <error> #f)

;;==============================================================
;; Global parameters
;;

;; default string to be used for user-agent.
(define http-user-agent
  (make-parameter #"gauche.http/~(gauche-version)"))

;; global proxy settings.  can be overridden by :proxy keyword
;; argument.
(define http-proxy (make-parameter #f))

;; The default redirect handler
;;
(define http-default-redirect-handler
  (make-parameter
   (^[method code headers body]
     (and-let* ([loc (rfc822-header-ref headers "location")])
       (case (x->integer code)
         [(300 301 305 307)
          (case method [(GET HEAD) `(,method . ,loc)] [else #f])]
         [(302 303)                     ;found / see other
          ;; See rfc2616 notes - the agent isn't supposed to automatically
          ;; redirect 302 response of POST, but redirect with GET for it is
          ;; the de-facto behavior.
          (case method
            [(GET HEAD) `(,method . ,loc)]
            [else `(GET . ,loc)])]
         [else #f])))))

;;==============================================================
;; Higher-level API
;;

;; Higher-level API is for conventional call-return API.
;;
;; The METHOD argument specifies the http request method by a symbol.
;; GET, HEAD, POST, PUT and DELETE is currently supported.
;;
;; The SERVER argument maybe a string naming the server (and
;; optionally a port number by the format of "server:port"), or
;; an <http-connection> object.  Using a server name is suitable
;; for easy one-shot http access; the connection and related states
;; are discarded once the procedure returns.
;; On the other hand, a connection object can keep the states such
;; as persistent connection and authentication tokens, suitable for
;; a series of communications to a server.
;;
;; The REQUEST-URI argument can be a string as specified in RFC2616,
;; or a list in the form of (<path> (<name> <value>) ...).  In the
;; latter form, (<name> <value>) assoc list is converted into a
;; url query form as defined in HTML4 (application/x-www-form-urlencoded)
;; and appended to <path>.
;;
;; The keyword arguments are handled by various low-level routines,
;; and here's the summary:
;;
;;   request-body - gives the body.
;;   receiver - A procedure that handles the response.
;;             It takes four arguments: response-code, response headers,
;;             total response size if known (#f otherwise), and
;;             a retriever thunk.   The retriever thunk is, when called,
;;             returns two values: an input port and an integer size.
;;             The receiver must loop (1) to read as many bytes as
;;             specified in the size from the input port, and (2)
;;             call the retriver again, while it returns positive integer.
;;             The retriever procedure returns zero when body is exhausted.
;;             In that case the value(s) the receiver returns will be
;;             the return value of http-request.
;;             The retriever thunk can return #f as size, if we don't know the
;;             chunk of response size.  In that case, the receiver may read
;;             up to EOF, or as much data as it wants, then call the
;;             retriever.  The second time the retriever returns size=0,
;;             so you can do cleanup work.
;;             The retriever procedure returns -1 if an error occurs
;;             during comminucation.  In that case, the receiver can do
;;             whatever cleanup work.  After the receiver returns, an
;;             appropriate error is thrown from http-request.
;;
;;   sender  - A procedure that handles sending the request body.
;;             It takes three arguments: tantative list of headers,
;;             the value given to :request-encoding argument,
;;             and a procedure HEADER-SINK.
;;
;;             HEADER-SINK takes one argument, a list of http headers
;;             in the form of (("field-name" "field-value") ...).
;;             In general, besides the given tentative headers, the sender
;;             needs to add either Content-Length header if it sends entire
;;             contents at once, or ("transfer-encoding" "chunked") if
;;             it uses chunked transfer.  If neither headers exist in the
;;             given header list, an error is raised and request is aborted.
;;             Sender can add other headers, e.g. content-type or mime-type.
;;             Sender can also remove or modify given tentative headers.
;;
;;             HEADER-SINK returns a procedure BODY-SINK on success.
;;             BODY-SINK takes an integer SIZE argument and returns an
;;             output PORT.  What the sender needs to do is to call
;;             BODY-SINK with the size of data chunk, then to write out
;;             the data into the returned PORT, and repeat it until
;;             all the data is sent.  To indicate the end of data,
;;             the sender needs to call BODY-SINK with argument 0.
;;
;;   host    - the host name passed to the 'host' header field.
;;   secure  - if true, using secure connection (via gauche.tls).
;;   auth-user, auth-password, auth-handler - authentication parameters.
;;   request-encoding - when http-* is to construct request-uri and/or
;;             request body, this argument specifies the character encoding
;;             to be used as the external encoding.
;;   redirect-handler - Called if the server responds with 3xx status.
;;             The argument is the request method, a status code, list of
;;             headers and response body (can be #f).  It may return a
;;             (METHOD . URL) or #f.   For the first case,
;;             http-request re-attempts to fetch the URL with the given method.
;;             (unless we're not looping).  If it returns #f, the original
;;             code, headers and body are returned from http-request.
;;             If given #t (default), the procedure bound to the parameter
;;             http-default-redirect-handler is called.
;;   no-redirect - If true, we ignore the value of redirect-handler and
;;             returns without attempting retrying.
;;             This is provided for the backward compatibility; newer code
;;             should use :redirect-handler #f
;;
;; Other unrecognized options are passed as request headers.

(define (http-request method server request-uri
                      :key (host #f)
                           (redirect-handler #t)
                           (no-redirect #f)
                           auth-handler
                           auth-user
                           auth-password
                           (proxy (http-proxy))
                           extra-headers
                           (user-agent (http-user-agent))
                           (secure #f)
                           (receiver (http-string-receiver))
                           (sender #f)
                           ((:request-encoding enc) (gauche-character-encoding))
                      :allow-other-keys opts)

  (define conn (ensure-connection server auth-handler auth-user auth-password
                                  proxy secure extra-headers))
  (define redirector (if no-redirect
                       #f
                       (case redirect-handler
                         [(#t) (http-default-redirect-handler)]
                         [(#f) #f]
                         [else => identity])))
  (define options `(:user-agent ,user-agent ,@(http-auth-headers conn) ,@opts))
  (define no-body-replies '("204" "304"))

  (define (get-body iport method code headers receiver)
    (and (not (eq? method 'HEAD))
         (not (member code no-body-replies))
         (receive-body iport code headers receiver)))

  ;; final touch of request headers
  (define (req-headers host)
    (cond-list [(~ conn'persistent) @ (if (~ conn'proxy)
                                        '(:proxy-connection keep-alive)
                                        '(:connection keep-alive))]
               [#t @ `(:host ,host :user-agent ,user-agent
                       ,@(http-auth-headers conn) ,@opts)]))

  ;; If we decide to give up redirection, we read from already-retrieved
  ;; body of 3xx reply.  This modifies reply headers if necessary.
  (define (redirect-headers body rep-headers)
    (if body
      `(:content-length ,(string-size body)
                        ,@(delete-keywords '(:content-length
                                             :content-transfer-encoding)
                                           rep-headers))
      rep-headers))

  ;; returns either one of:
  ;;   (reply <code> <headers> <body>)
  ;;   (redirect-to <method> <location>)
  (define (request-response in out method uri host sender)
    (send-request out method uri sender (req-headers host) enc)
    (receive (code rep-headers) (receive-header in)
      (if-let1 consider-redirect (and (string-prefix? "3" code) redirector)
        ;; we retrieve body as string, not using caller-provided receiver
        (let* ([body (get-body in method code rep-headers
                               (http-string-receiver))]
               [verdict (consider-redirect method code rep-headers body)])
          (if verdict
            `(redirect-to ,(car verdict) ,(cdr verdict))
            (let1 hdrs (redirect-headers body rep-headers)
              `(reply ,code ,hdrs
                      ,(and body
                            (receive-body (open-input-string body) code
                                          hdrs receiver))))))
        ;; no redirection
        `(reply ,code ,rep-headers
                ,(get-body in method code rep-headers receiver)))))

  ;; main loop
  (let loop ([history '()]
             [host host]
             [method method]
             [request-uri (ensure-request-uri request-uri enc)])
    (receive (host uri)
        (consider-proxy conn (or host (~ conn'server)) request-uri)
      (let1 result
          (with-connection
           conn
           (^[i o] (request-response i o method uri host sender)))
        (match result
          [('reply code rep-headers body) (values code rep-headers body)]
          [('redirect-to method location)
           (receive (uri proto new-server path*)
               (canonical-uri conn location (ref conn'server))
             (when (or (member uri history)
                       (> (length history) 20))
               (errorf <http-error> "redirection is looping via ~a" uri))
             (loop (cons uri history)
                   (~ (redirect-connection! conn proto new-server)'server)
                   method path*))])))))
;;
;; Pre-defined receivers
;;
(define (http-string-receiver)
  (^[code hdrs total retr]
    ;; TODO: check headers for encoding
    (let loop ([sink (open-output-string)])
      (receive (remote size) (retr)
        (cond [(eqv? size 0) (get-output-string sink)]
              [(or (not size) (> size 0))
               (copy-port remote sink :size size) (loop sink)])))))

(define (http-null-receiver)
  (^[code hdrs total retr]
    (let loop ([sink (open-output-file (null-device))])
      (receive (remote size) (retr)
        (cond [(and size (<= size 0)) (close-output-port sink)]
              [else (copy-port remote sink :size size) (loop sink)])))))

(define (http-oport-receiver sink flusher)
  (^[code hdrs total retr]
    (let loop ()
      (receive (remote size) (retr)
        (cond [(and size (<= size 0)) (flusher sink hdrs)]
              [else (copy-port remote sink :size size) (loop)])))))

(define (http-file-receiver filename :key (temporary #f))
  (^[code hdrs total retr]
    (receive (port tmpname) (sys-mkstemp filename)
      (let loop ()
        (receive (remote size) (retr)
          (cond [(or (not size) (> size 0))
                 (copy-port remote port :size size) (loop)]
                [(= size 0)
                 (close-output-port port)
                 (if temporary
                   tmpname
                   (begin (sys-rename tmpname filename) filename))]
                [else (close-output-port port) (sys-unlink tmpname)]))))))

(define-syntax http-cond-receiver
  (syntax-rules (else =>)
    [(_) (http-null-receiver)]
    [(_ [else . exprs]) (begin . exprs)]
    [(_ [cc => proc] . rest)
     (^[code hdrs total retr]
       ((if (match-status-code? cc code)
          proc
          (http-cond-receiver . rest))
        code hdrs total retr))]
    [(_ [cc . exprs] . rest)
     (^[code hdrs total retr]
       ((if (match-status-code? cc code '(cc . exprs))
          (begin . exprs)
          (http-cond-receiver . rest))
        code hdrs total retr))]
    [(_ other . rest)
     (syntax-error "invalid clause in http-cond-receiver" other)]))

(define (match-status-code? pattern code clause)
  (cond [(string? pattern) (equal? pattern code)]
        [(regexp? pattern) (rxmatch pattern code)]
        [else (error "Invalid pattern in a clause of http-cond-receiver:"
                     clause)]))

;;
;; Senders
;;

(define (http-null-sender)
  (^[hdrs encoding header-sink]
    (let1 body-sink (header-sink `(("content-length" "0") ,@hdrs))
      (body-sink 0))))

(define (http-string-sender string)     ;honors encoding
  (^[hdrs encoding header-sink]
    (let* ([body (if (ces-equivalent? encoding (gauche-character-encoding))
                   string
                   (ces-convert string (gauche-character-encoding) encoding))]
           [size (string-size body)]
           [body-sink (header-sink `(("content-length" ,(x->string size))
                                     ,@hdrs))]
           [oport (body-sink size)])
      (display body oport)
      (body-sink 0))))

(define (http-blob-sender blob)        ;blob may be a string or uvector
  (^[hdrs encoding header-sink]
    (let* ([size (if (string? blob) (string-size blob) (uvector-size blob))]
           [body-sink (header-sink `(("content-length" ,(x->string size))
                                     ,@hdrs))]
           [port (body-sink size)])
      (if (string? blob)
        (display blob port)
        (write-block blob port))
      (body-sink 0))))

;; Send contents directly from the file.  Encoding is ignored.
;; TODO: The file size may be changed while sending out.  If it gets
;; bigger, we can just ignore the rest, but what if it gets shorter?
(define (http-file-sender filename)
  (^[hdrs encoding header-sink]
    (let* ([size (file-size filename)]
           [body-sink (header-sink `(("content-length" ,(x->string size))
                                     ,@hdrs))]
           [port (body-sink size)])
      (call-with-input-file filename (cut copy-port <> port :size size))
      (body-sink 0))))

;; See http-compose-form-data definition for params spec.
;; TODO: support chunked sending, instead of building entire body at once.
(define (http-multipart-sender params)
  (^[hdrs encoding header-sink]
    (receive (body boundary) (http-compose-form-data params #f encoding)
      (let* ([size (string-size body)]
             [hdrs `(("content-length" ,(x->string size))
                     ("mime-version" "1.0")
                     ("content-type" ,#"multipart/form-data; boundary=\"~|boundary|\"")
                     ,@(alist-delete "content-type" hdrs equal?))]
             [body-sink (header-sink hdrs)]
             [port (body-sink size)])
        (display body port)
        (body-sink 0)))))

;;
;; Shortcuts for specific requests.
;;
(define (http-get server request-uri . options)
  (apply %http-request-adaptor 'GET server request-uri #f options))

(define (http-head server request-uri . options)
  (apply %http-request-adaptor 'HEAD server request-uri #f options))

(define (http-post server request-uri body . options)
  (apply %http-request-adaptor 'POST server request-uri body options))

(define (http-put server request-uri body . options)
  (apply %http-request-adaptor 'PUT server request-uri body options))

(define (http-delete server request-uri . options)
  (apply %http-request-adaptor 'DELETE server request-uri #f options))

;; Adaptor to the new API.  Converts :sink and :flusher arguments,
;; which are superseded by :receiver arguments.
(define (%http-request-adaptor method server request-uri body
                               :key (receiver #f) (sink #f) (flusher #f)
                               :allow-other-keys opts)
  (define recvr
    (cond [(and sink flusher)
           (http-oport-receiver sink flusher)]
          [(or sink flusher)
           (errorf "You need to provide :sink and :flusher together to http-~a"
                   (string-downcase (symbol->string method)))]
          [receiver]
          ;; fallback
          [else (http-oport-receiver (open-output-string)
                                     (^[s h] (get-output-string s)))]))
  (apply http-request method server request-uri
         :sender (cond [(not body) (http-null-sender)]
                       [(list? body) (http-multipart-sender body)]
                       [else (http-blob-sender body)])
         :receiver recvr opts))

;;==============================================================
;; HTTP connection context
;;

;; <http-connection> object can be used to have conversations with
;; (usually) a specific server.  A typical usage is to emulate a
;; browser to perform a certain transaction spanning to several
;; webpages.  (For the simple "one-shot" request-response access,
;; the rfc.http APIs create a temporary connection under the hood.)

(define-class <http-connection> ()
  ;; All slots are private.
  ((server :init-keyword :server)       ; server[:port]
   (socket :init-value #f)              ; A <socket> for persistent connection.
                                        ; If it is shutdown by the server,
                                        ; the APIs attempt to reconnect.
   (secure-agent  :init-value #f)       ; When using secure connection via
                                        ; tls, this slot holds its handle.
   (persistent    :init-keyword :persistent)   ; true for persistent connection.
   (auth-handler  :init-keyword :auth-handler  :init-value #f)
   (auth-user     :init-keyword :auth-user     :init-value #f)
   (auth-password :init-keyword :auth-password :init-value #f)
   (proxy         :init-keyword :proxy)
   (extra-headers :init-keyword :extra-headers)
   (secure        :init-keyword :secure) ; boolean
   ))

(define (make-http-connection server :key
                              (persistent #t)
                              (auth-handler  #f)
                              (auth-user     #f)
                              (auth-password #f)
                              (proxy #f)
                              (extra-headers '()))
  (make <http-connection>
    :persistent persistent
    :server server
    :auth-handler (or auth-handler (http-default-auth-handler))
    :auth-user auth-user
    :auth-password auth-password
    :proxy proxy
    :extra-headers extra-headers))

;; This modifies CONN.
(define (redirect-connection! conn proto new-server)
  (let1 orig-server (~ conn'server)
    (unless (and (string=? orig-server new-server)
                 (eq? (~ conn'secure) (equal? proto "https")))
      (shutdown-secure-agent conn)
      (and-let* ([s (~ conn'socket)])
        (socket-shutdown s)
        (socket-close s)
        (set! (~ conn'socket) #f))
      (set! (~ conn'server) new-server)
      (set! (~ conn'secure) (equal? proto "https"))))
  conn)

;;==============================================================
;; query and request body composition
;;

;; Query string composition.
;; NB: Query string syntax (aka application/x-www-form-urlencoded) is
;; not defined in RFC2616.  In fact, it is not clearly defined at all
;; in RFC.  The most definitive source might be the HTML4 specification,
;; section 17.13.4 "Form content types",
;; <http://www.w3.org/TR/html4/interact/forms.html#h-17.13.4.1>.
;; In reality it is used very frequently with http, so we put it here.
(define (http-compose-query path params
                            :optional (encoding (gauche-character-encoding)))
  (define (esc s) (uri-encode-string (x->string s) :encoding encoding))
  (define (query-1 n&v)
    (match n&v
      [(name value) #"~(esc name)=~(esc value)"]
      [_ (error "Invalid request-uri form:" params)]))
  (define (query) (string-concatenate (intersperse "&" (map query-1 params))))
  (cond [(not path) (query)]
        [(null? params) path]
        [else #"~|path|?~(query)"]))

;; multipart/form-data composition [RFC2388]
;; <params> : (<param> ...)
;; <param>  : (<name> <value>)  ; same as http-compose-query
;;          | (<name> <key> <value> <key2> <value2> ...)
;; <key>    : :value | :file | :content-type | :content-transfer-encoding
;;          | other keywords (used as a header name)
;; composed message is put to the current output port.
;; returns the boundary string.
(define (http-compose-form-data params port
                                :optional (encoding (gauche-character-encoding)))
  (define (translate-param param)
    (match param
      [(name value) (translate-param `(,name :value ,value))]
      [(name . kvs)
       (unless (even? (length kvs))
         (error "Invalid parameter format to create multipart/form-data:" param))
       (let-keywords kvs ([value ""]
                          [file  #f]
                          [content-type #f]
                          [content-transfer-encoding #f] . other-keys)
         `(,(canonical-content-type (mime-parse-content-type content-type)
                                    value file)
           (("content-transfer-encoding" ,(or content-transfer-encoding "binary"))
            ("content-disposition" ,(make-content-disposition name file))
            ,@(map (cut map x->string <>) (slices other-keys 2)))
           ,(if file `(file ,file) (x->string value))))]))
  (define (canonical-content-type ct value file)
    (match ct
      [#f (if (or file (string-incomplete? value))
            '("application" "octet-stream")
            `("text" "plain" ("charset" . ,(x->string encoding))))]
      [(type subtype . options)
       (if (assoc "charset" options)
         ct
         `(,type ,subtype ("charset" . ,(x->string encoding)) ,@options))]))
  (define (make-content-disposition name file)
    (with-output-to-string
      (^[]
        (display "form-data")
        (mime-compose-parameters
         `(("name" . ,name)
           ,@(cond-list [file `("filename" . ,file)]))))))
  (if (not port)
    (mime-compose-message-string (map translate-param params))
    (mime-compose-message (map translate-param params) port)))

;;==============================================================
;; status codes
;;

(define *status-code-map*
  (hash-table 'eqv?
              '(100 . "Continue")
              '(101 . "Switching Protocols")
              '(200 . "OK")
              '(201 . "Created")
              '(202 . "Accepted")
              '(203 . "Non-Authoritative Information")
              '(204 . "No Content")
              '(205 . "Reset Content")
              '(206 . "Partial Content")
              '(300 . "Multiple Choices")
              '(301 . "Moved Permanently")
              '(302 . "Found")
              '(303 . "See Other")
              '(304 . "Not Modified")
              '(305 . "Use Proxy")
              '(306 . "(Unused)")
              '(307 . "Temporary Redirect")
              '(400 . "Bad Request")
              '(401 . "Unauthorized")
              '(402 . "Payment Required")
              '(403 . "Forbidden")
              '(404 . "Not Found")
              '(405 . "Method Not Allowed")
              '(406 . "Not Acceptable")
              '(407 . "Proxy Authentication Required")
              '(408 . "Request Timeout")
              '(409 . "Conflict")
              '(410 . "Gone")
              '(411 . "Length Required")
              '(412 . "Precondition Failed")
              '(413 . "Request Entity Too Large")
              '(414 . "Request-URI Too Long")
              '(415 . "Unsupported Media Type")
              '(416 . "Requested Range Not Satisfiable")
              '(417 . "Expectation Failed")
              '(500 . "Internal Server Error")
              '(501 . "Not Implemented")
              '(502 . "Bad Gateway")
              '(503 . "Service Unavailable")
              '(504 . "Gateway Timeout")
              '(505 . "HTTP Version Not Supported")
              ))

;; API
;; code can be an integer or a string, e.g. "200"
;; returns #f for unknown code
(define (http-status-code->description code)
  (hash-table-get *status-code-map* (x->integer code) #f))

;;==============================================================
;; internal utilities
;;

(define (ensure-request-uri request-uri enc)
  (match request-uri
    [(? string?) request-uri]
    [(path n&v ...) (http-compose-query path n&v enc)]
    [_ (error "Invalid request-uri form for http request API:" request-uri)]))

(define (canonical-body request-body extra-headers enc)
  (cond [(not request-body) (values #f extra-headers)]
        [(string? request-body) (values request-body extra-headers)]
        [(list? request-body)
         (receive (body boundary) (http-compose-form-data request-body #f enc)
           (values body
                   `(:mime-version "1.0"
                     :content-type
                     ,#"multipart/form-data; boundary=~boundary"
                     ,@(delete-keyword! :content-type extra-headers))))]
        [else (error "Invalid request-body format:" request-body)]))

;; Always returns a connection object.
(define (ensure-connection server auth-handler auth-user auth-password
                           proxy secure extra-headers)
  (rlet1 conn (cond
               [(is-a? server <http-connection>) server]
               [(string? server) (make-http-connection server :persistent #f)]
               [else (error "bad type of argument for server: must be an <http-connection> object or a string of the server's name, but got:" server)])
    ;; TODO: Might need to reset connections if parameters are changed
    (let-syntax ([check-override
                  (syntax-rules ()
                    [(_ id)
                     (unless (undefined? id) (set! (ref conn'id) id))])])
      (check-override auth-handler)
      (check-override auth-user)
      (check-override auth-password)
      (check-override proxy)
      (check-override extra-headers)
      (check-override secure))))

(define (reset-http-connection conn)
  (shutdown-socket-connection conn)
  (shutdown-secure-agent conn))

(define (start-socket-connection conn)
  ;; If address is given ipv6 format such as "[::1]:port", we have to
  ;; use "::1" part as the hostname, excluding [].
  (define (parse-address addr)
    (rxmatch-case addr
      [#/^\[([a-fA-F\d:]+)\](?::(\d+))?$/ (_ host port) (values host port)]
      [#/^([^:]+)(?::(\d+))?$/ (_ host port) (values host port)]))
  (receive (host port) (parse-address (or (~ conn'proxy) (~ conn'server)))
    (set! (~ conn'socket)
          (make-client-socket host (if port
                                     (x->integer port)
                                    (if (~ conn'secure) 443 80))))))

(define (shutdown-socket-connection conn)
  (when (~ conn'socket)
    (guard (e [(<system-error> e) #f])
      (socket-shutdown (~ conn'socket)))
    (socket-close (~ conn'socket))
    (set! (~ conn'socket) #f)))

(define (with-connection conn proc)
  (unless (~ conn'persistent)
    (unless (~ conn'socket) (start-socket-connection conn))
    (when (~ conn'secure) (start-secure-agent conn)))
  (unwind-protect
      (apply proc (if (~ conn'secure)
                    `(,(tls-input-port (~ conn'secure-agent))
                      ,(tls-output-port (~ conn'secure-agent)))
                    `(,(socket-input-port (~ conn'socket))
                      ,(socket-output-port (~ conn'socket)))))
    (unless (~ conn'persistent)
      (when (~ conn'secure) (shutdown-secure-agent conn))
      (shutdown-socket-connection conn))))

;; canonicalize uri for the sake of redirection.
;; URI is a request-uri given to the API, or the redirect location specified
;; in 3xx response.  It can be a full URI or just a path w.r.t. the current
;; accessing server, so we pass the current server name as HOST in order to
;; fill the URI if necessary.
;; Returns three values; the full URI to access (it is used to detect a loop
;; in redirections), the server name, and the new request uri.
(define (canonical-uri conn uri host)
  (let*-values ([(scheme specific) (uri-scheme&specific uri)]
                [(h p q f) (uri-decompose-hierarchical specific)])
    (let ([scheme (or scheme (if (~ conn'secure) "https" "http"))]
          [host (or h host)])
      (values (uri-compose :scheme scheme :host host
                           :path p :query q :fragment f)
              scheme
              (or h host)
              ;; drop "//"
              (string-drop (uri-compose :path p :query q :fragment f) 2)))))

;; canonicalize host and uri w.r.t. proxy
(define (consider-proxy conn host uri)
  (if (ref conn'proxy)
    (values host (uri-compose :scheme "http" :host (ref conn'server) :path* uri))
    (values host uri)))

;; send
(define (send-request out method uri sender headers enc)
  (define request-line #"~method ~uri HTTP/1.1\r\n")
  (define request-headers
    ($ map (cut map (^s (if (keyword? s) (keyword->string s) (x->string s))) <>)
       $ slices headers 2))
  (case method
    [(POST PUT)
     (sender request-headers enc
             (^[hdrs]
               (send-headers request-line hdrs out)
               (let ([chunked?
                      (equal? (rfc822-header-ref hdrs "transfer-encoding")
                              "chunked")]
                     [first-time #t])
                 (^[size]
                   (when chunked?
                     (unless first-time (display "\r\n" out))
                     (format out "~x\r\n" size))
                   (flush out)
                   out))))]
    [else (send-headers request-line request-headers out)]))

;; NB: We try to send the request line and headers in one packet if possible,
;; since some http servers assumes important headers can be read in single
;; read() call.
(define (send-headers request-line hdrs out)
  (display (tree->string
            `(,request-line
              ,@(map (^h `(,(car h)": ",(cadr h)"\r\n")) hdrs)
              "\r\n"))
           out)
  (flush out))

;; receive
(define (receive-header remote)
  (receive (code reason) (parse-status-line (read-line remote))
    (values code (rfc822-header->list remote))))

(define (parse-status-line line)
  (cond [(eof-object? line)
         (error <http-error> "http reply contains no data")]
        [(#/\w+\s+(\d\d\d)\s+(.*)/ line) => (^m (values (m 1) (m 2)))]
        [else (error <http-error> "bad reply from server" line)]))

(define (receive-body remote code headers receiver)
  (let1 total (and-let* ([p (assoc "content-length" headers)])
                (x->integer (cadr p)))
    (if-let1 enc (assoc "transfer-encoding" headers)
      (if (equal? (cadr enc) "chunked")
        (receive-body-chunked remote code headers total receiver)
        (error <http-error> "unsupported transfer-encoding:" (cadr enc)))
      (receive-body-once remote code headers total receiver))))

(define (receive-body-once remote code headers total receiver)
  ;; Callback will be called twice (unless total is 0).  The first
  ;; time we return # of total bytes, the second time zero.
  (let1 rest total
    (define (callback)
      (if (equal? rest 0)
        (values remote 0)
        (begin (set! rest 0) (values remote total))))
    (receiver code headers total callback)))

;; NB: chunk extension and trailer are ignored for now.
(define (receive-body-chunked remote code headers total receiver)
  (define chunk-size #f)
  (define condition #f)
  (define (callback)
    (if (equal? chunk-size 0)
      (values remote 0) ;; finalize
      ;; If we get an error during receiving from the server, we need
      ;; to return -1 to give the chance to the receiver to clean up
      ;; things.  After the receiver returns we reraise the condition.
      (guard (e [else (set! condition e) (values remote -1)])
        ;; If we've already handled some chunks, we need to skip
        ;; the trailing CRLF of the previous chunk.
        (when chunk-size
          (read-line remote))
        (let1 line (read-line remote)
          (when (eof-object? line)
            (error <http-error> "chunked body ended prematurely"))
          (rxmatch-if (#/^([[:xdigit:]]+)/ line) (#f digits)
            (begin
              (set! chunk-size (string->number digits 16))
              (if (zero? chunk-size)
                ;; finish reading trailer
                (do ([line (read-line remote) (read-line remote)])
                    [(or (eof-object? line) (string-null? line))
                     (values remote 0)])
                (values remote chunk-size)))
            ;; something's wrong
            (error <http-error> "bad line in chunked data:" line))))))
  (begin0 (receiver code headers total callback)
    (when condition (raise condition))))

;;==============================================================
;; secure agent handling
;;

(define (shutdown-secure-agent conn)
  (when (~ conn'secure-agent)
    (tls-close (~ conn'secure-agent))
    (tls-destroy (~ conn'secure-agent))
    (set! (~ conn'secure-agent) #f)))

(define (start-secure-agent conn)
  (unless (http-secure-connection-available?)
    (error "Secure connection is not available on this platform"))
  (when (~ conn'secure-agent) (shutdown-secure-agent conn))
  (let1 tls (make-tls)
    (tls-connect tls (socket-fd (~ conn'socket)))
    (set! (~ conn'secure-agent) tls)))

;; for external api
(define (http-secure-connection-available?)
  (cond-expand
   [gauche.net.tls #t]
   [else #f]))

;;==============================================================
;; authentication handling
;;

(define (http-auth-headers conn)
  (or (and-let* ([auth-handler (~ conn'auth-handler)])
        (auth-handler conn))
      '()))

(define (http-basic-auth-handler conn)
  (and-let* ([user (~ conn 'auth-user)]
             [pass (or (~ conn 'auth-password) "")])
    `(:authorization ,($ format "Basic ~a"
                         $ base64-encode-string #"~|user|:~|pass|"))))

(define http-default-auth-handler
  (make-parameter http-basic-auth-handler))
