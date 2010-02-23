;;;
;;; http.scm - HTTP 1.1
;;;  
;;;   Copyright (c) 2000-2010  Shiro Kawai  <shiro@acm.org>
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
  (use srfi-1)
  (use srfi-2)
  (use srfi-11)
  (use srfi-13)
  (use rfc.822)
  (use rfc.uri)
  (use gauche.net)
  (use gauche.parameter)
  (use util.match)
  (use util.list)
  (export <http-error>
          http-user-agent make-http-connection http-compose-query
          http-compose-form-data
          http-get http-head http-post http-put http-delete
          http-default-auth-handler
          )
  )
(select-module rfc.http)

(autoload rfc.mime
          mime-compose-message
          mime-compose-message-string
          mime-compose-parameters
          mime-parse-content-type)

(autoload gauche.process
          run-process process-input process-output
          process-wait process-kill)

;;==============================================================
;; Conditions
;;

(define-condition-type <http-error> <error> #f)

;;==============================================================
;; Global parameters
;;

;; default string to be used for user-agent.  
(define http-user-agent
  (make-parameter #`"gauche.http/,(gauche-version)"))

;;==============================================================
;; Higher-level API
;;

;; Higher-level API is for conventional call-return API.
;; The client program call for request, and gets the response.
;; This family of APIs takes mandatory server and request-uri
;; arguments, and optional keyword arguments.
;;
;; The "server" argument maybe a string naming the server (and
;; optionally a port number by the format of "server:port"), or
;; an <http-connection> object.  Using a server name is suitable
;; for easy one-shot http access; the connection and related states
;; are discarded once the procedure returns.
;; On the other hand, a connection object can keep the states such
;; as persistent connection and authentication tokens, suitable for
;; a series of communications to a server.
;;
;; The request-uri argument can be a string as specified in RFC2616,
;; or a list in the form of (<path> (<name> <value>) ...).  In the
;; latter form, (<name> <value>) assoc list is converted into a
;; url query form as defined in HTML4 (application/x-www-form-urlencoded)
;; and appended to <path>.
;;
;; The body argument must be a string representing the request body.
;;
;; The options are handled by various low-level routines, and here's
;; the summary:
;;
;;   sink    - a port where the response body is written to.
;;   flusher - a procedure called once a response body is fully retrieved
;;             and written to the sink.  The value returned by flusher
;;             becomes the return value of http-get and http-post.
;;   host    - the host name passed to the 'host' header field.
;;   secure  - if true, using secure connection (via external stunnel
;;             process).
;;   no-redirect - if true, the procedures won't attempt to issue
;;             the request specified by 3xx response headers.
;;   auth-user, auth-password, auth-handler - authentication parameters.
;;   request-encoding - when http-* is to construct request-uri and/or
;;             request body, this argument specifies the character encoding
;;             to be used as the external encoding.
;;
;; Other unrecognized options are passed as request headers.

(define (http-get server request-uri . options)
  (http-generic 'GET server request-uri #f options))

(define (http-head server request-uri . options)
  (http-generic 'HEAD server request-uri #f options))

(define (http-post server request-uri body . options)
  (http-generic 'POST server request-uri body options))

(define (http-put server request-uri body . options)
  (http-generic 'PUT server request-uri body options))

(define (http-delete server request-uri . options)
  (http-generic 'DELETE server request-uri #f options))

;;==============================================================
;; HTTP connection context
;;

;; <http-connection> object can be used to have conversations with
;; (usually) a specific server.  A typical usage is to emulate a
;; browser to perform a certain transaction spanning to several
;; webpages.  (For the simple "one-shot" request-response access,
;; the rfc.http APIs create a temporary connection under the hood.)

(define-class <http-connection> ()
  ;; All the slots are private.
  ((server :init-keyword :server)       ; server[:port]
   (socket :init-value #f)              ; A <socket> for persistent connection.
                                        ; If it is shutdown by the server,
                                        ; the APIs attempt to reconnect.
   (secure-agent  :init-value #f)       ; When using secure connection via
                                        ; external process, this slot holds
                                        ; its handle.
   (auth-handler  :init-keyword :auth-handler) ; unused yet
   (auth-user     :init-keyword :auth-user)    ; unused yet
   (auth-password :init-keyword :auth-password); unused yet
   (proxy         :init-keyword :proxy)
   (extra-headers :init-keyword :extra-headers)
   (secure        :init-keyword :secure)
   ))

(define (make-http-connection server :key
                              (auth-handler  http-default-auth-handler)
                              (auth-user     #f)
                              (auth-password #f)
                              (proxy #f)
                              (extra-headers '()))
  (make <http-connection>
    :server server :auth-handler auth-handler :auth-user auth-user
    :auth-password auth-password :proxy proxy
    :extra-headers extra-headers))

(define (redirect conn proto new-server)
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
      [(name value) #`",(esc name)=,(esc value)"]
      [_ (error "invalid request-uri form: ~s" params)]))
  (define (query) (string-concatenate (intersperse "&" (map query-1 params))))
  (cond [(not path) (query)]
        [(null? params) path]
        [else #`",|path|?,(query)"]))

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
         (error "invalid parameter format to create multipart/form-data:" param))
       (let-keywords kvs ([value ""]
                          [file  #f]
                          [content-type #f]
                          [content-transfer-encoding #f] . other-keys)
         `(,(canonical-content-type (mime-parse-content-type content-type)
                                    value file)
           (("content-transfer-encoding" "binary")
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
      (lambda ()
        (display "form-data")
        (mime-compose-parameters `(("name" . ,name)
                                   ,@(cond-list [file `("filename" . ,file)]))))))
  (if (not port)
    (mime-compose-message-string (map translate-param params))
    (mime-compose-message (map translate-param params) port)))
  
;;==============================================================
;; internal utilities
;;

(define (http-generic request server request-uri request-body options)
  (let-keywords options ([host #f]
                         [no-redirect #f]
                         [auth-handler  (undefined)]
                         [auth-user     (undefined)]
                         [auth-password (undefined)]
                         [proxy         (undefined)]
                         [extra-headers (undefined)]
                         [user-agent (http-user-agent)]
                         [secure #f]
                         [enc :request-encoding (gauche-character-encoding)]
                         . opts)
    (let1 conn (ensure-connection server auth-handler auth-user auth-password
                                  proxy secure extra-headers)
      (receive (body opts) (canonical-body request-body opts enc)
        (let loop ([history '()]
                   [host host]
                   [request-uri (ensure-request-uri request-uri enc)])
          (receive (code headers body)
              (request-response request conn host request-uri body
                                `(:user-agent ,user-agent ,@opts))
            (or (and-let* ([ (not no-redirect) ]
                           [ (string-prefix? "3" code) ]
                           [loc (assoc "location" headers)])
                  (receive (uri proto new-server path*)
                      (canonical-uri conn (cadr loc) (ref conn'server))
                    (when (or (member uri history)
                              (> (length history) 20))
                      (errorf <http-error> "redirection is looping via ~a" uri))
                    (loop (cons uri history)
                          (ref (redirect conn proto new-server)'server)
                          path*)))
                (values code headers body))))))))

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
                     ,#`"multipart/form-data; boundary=,boundary"
                     ,@(delete-keyword! :content-type extra-headers))))]
        [else (error "Invalid request-body format:" request-body)]))

;; Always returns a connection object.
(define (ensure-connection server auth-handler auth-user auth-password
                           proxy secure extra-headers)
  (rlet1 conn (cond [(is-a? server <http-connection>) server]
                    [(string? server) (make-http-connection server)]
                    [else (error "bad type of argument for server: must be an <http-connection> object or a string of the server's name, but got:" server)])
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

(define (server->socket server)
  (cond ((#/([^:]+):(\d+)/ server)
         => (lambda (m) (make-client-socket (m 1) (x->integer (m 2)))))
        (else (make-client-socket server 80))))

(define (with-connection conn proc)
  (cond [(~ conn'secure)
         (start-secure-agent conn)
         (unwind-protect
             (proc (process-output (~ conn'secure-agent))
                   (process-input (~ conn'secure-agent)))
           (shutdown-secure-agent conn))]
        [else
         (let1 s (server->socket (or (ref conn'proxy) (ref conn'server)))
           (unwind-protect
               (proc (socket-input-port s) (socket-output-port s))
             (socket-close s)))]))

(define (request-response request conn host request-uri request-body options)
  (define no-body-replies '("204" "304"))
  (receive (host uri)
      (consider-proxy conn (or host (ref conn'server)) request-uri)
    (with-connection
     conn
     (lambda (in out)
       ;; NB: we need better way to sort out keyword options that shouldn't
       ;; be passed to the request header.
       (send-request out request host uri request-body
                     (delete-keywords '(:sink :flusher) options))
       (receive (code headers) (receive-header in)
         (values code
                 headers
                 (and (not (eq? request 'HEAD))
                      (not (member code no-body-replies))
                      (let-keywords options
                          ((sink    (open-output-string))
                           (flusher (lambda (sink _) (get-output-string sink)))
                           . #f)
                        (receive-body in headers sink flusher)))))))))

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
(define (send-request out request host uri body options)
  (display #`",request ,uri HTTP/1.1\r\n" out)
  (display #`"Host: ,|host|\r\n" out)
  (case request
    ((POST PUT)
     ;; for now, we don't support chunked encoding in POST method.
     (send-request-headers (list* :content-length (string-size body) options)
                           out)
     (display "\r\n" out)
     (display body out))
    (else
     ;; requests w/o body
     (send-request-headers options out)
     (display "\r\n" out)))
  (flush out))

(define (send-request-headers options out)
  (let loop ((options options))
    (unless (or (null? options) (null? (cdr options)))
      (format out "~a: ~a\r\n" (car options) (cadr options))
      (loop (cddr options)))))

;; receive
(define (receive-header remote)
  (receive (code reason) (parse-status-line (read-line remote))
    (values code (rfc822-header->list remote))))

(define (parse-status-line line)
  (cond ((eof-object? line)
         (error <http-error> "http reply contains no data"))
        ((#/\w+\s+(\d\d\d)\s+(.*)/ line)
         => (lambda (m) (values (m 1) (m 2))))
        (else
         (error <http-error> "bad reply from server" line))))

(define (receive-body remote headers sink flusher)
  (cond ((assoc "content-length" headers)
         => (lambda (p)
              (receive-body-nochunked (x->integer (cadr p)) remote sink)))
        ((assoc "transfer-encoding" headers)
         => (lambda (p)
              (if (equal? (cadr p) "chunked")
                  (receive-body-chunked remote sink)
                  (error <http-error> "unsupported transfer-encoding" (cadr p)))))
        (else (copy-port remote sink)))
  (flusher sink headers))

(define (receive-body-nochunked size remote sink)
  (when (positive? size) (copy-port remote sink :size size)))

;; NB: chunk extension and trailer are ignored for now.
(define (receive-body-chunked remote sink)
  (let loop ((line (read-line remote)))
    (when (eof-object? line)
      (error <http-error> "chunked body ended prematurely"))
    (rxmatch-if (#/^([[:xdigit:]]+)/ line) (#f digits)
      (let1 chunk-size (string->number digits 16)
        (if (zero? chunk-size)
          ;; finish reading trailer
          (do ((line (read-line remote) (read-line remote)))
              ((or (eof-object? line) (string-null? line)))
            #f)
          (begin
            (copy-port remote sink :size chunk-size)
            (read-line remote) ;skip the following CRLF
            (loop (read-line remote)))))
      ;; something wrong
      (error <http-error> "bad line in chunked data:" line))
    ))

;;==============================================================
;; secure agent handling
;;

(define (shutdown-secure-agent conn)
  (when (~ conn'secure-agent)
    (close-output-port (process-input (~ conn'secure-agent)))
    (or (process-wait (~ conn'secure-agent) #t)
        (begin (sys-nanosleep #e1e8)    ;0.1s
               (process-wait (~ conn'secure-agent) #t))
        (begin (sys-nanosleep #e1e8)    ;0.1s
               (process-kill (~ conn'secure-agent))))
    (set! (~ conn'secure-agent) #f)))

(define (start-secure-agent conn)
  (when (~ conn'secure-agent) (shutdown-secure-agent conn))
  (let* ([rhost      (or (~ conn'proxy) (~ conn'server))]
         [rhost:port (if (string-index rhost #\:) rhost #`",|rhost|:https")]
         [proc (run-process `("stunnel" "-c" "-r" ,rhost:port)
                            :input :pipe :output :pipe
                            :error "/dev/null" :wait #f)])
    (set! (~ conn'secure-agent) proc)))
     
;;==============================================================
;; authentication handling
;;

;; dummy - to be written
(define (http-default-auth-handler . _) #f)

