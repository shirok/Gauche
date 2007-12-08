;;;
;;; http.scm - HTTP 1.1
;;;  
;;;   Copyright (c) 2000-2007  Shiro Kawai  <shiro@acm.org>
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
;;;  $Id: http.scm,v 1.14 2007-12-08 03:24:30 shirok Exp $
;;;

;; HTTP handling routines.

;; RFC2616 Hypertext Transfer Protocol -- HTTP/1.1
;;  http://www.ietf.org/rfc/rfc2616txt
;; RFC2617 HTTP Authentication: Basic and Digest Access Authentication
;;  http://www.ietf.org/rfc/rfc2617.txt

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
  (export <http-error>
          http-user-agent make-http-connection
          http-get http-head http-post
          )
  )
(select-module rfc.http)

;;==============================================================
;; Conditions
;;

(define-condition-type <http-error> <error> #f)

;;==============================================================
;; Global parameters
;;

;; default string to be used for user-agent.  
(define http-user-agent
  (make-parameter "gauche.http/0.1"))

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
;; The request-uri argument is as specified in RFC2616.
;;
;; The options are handled by various low-level routines, and here's
;; the summary:
;;
;;   sink    - a port where the response body is written to.
;;   flusher - a procedure called once a response body is fully retrieved
;;             and written to the sink.  The value returned by flusher
;;             becomes the return value of http-get and http-post.
;;   host    - the host name passed to the 'host' header field.
;;   no-redirect - if true, the procedures won't attempt to issue
;;             the request specified by 3xx response headers.
;;   auth-user, auth-password, auth-handler - authentication parameters.
;;
;; Other unrecognized options are passed as request headers.

(define (http-get server request-uri . options)
  (http-generic 'GET server request-uri #f options))

(define (http-head server request-uri . options)
  (http-generic 'HEAD server request-uri #f options))

(define (http-post server request-uri body . options)
  (http-generic 'POST server request-uri body options))

;;==============================================================
;; HTTP connection context
;;

;; The connection object.  This class isn't exported; the users should
;; use make-http-connection.
(define-class <http-connection> ()
  ;; All the slots are private.
  ((server :init-keyword :server)       ; Original server[:port], given to
                                        ; make-http-connection.
   (effective-server :init-value #f)    ; The actual server[:port] we are
                                        ; talking.  This may different from
                                        ; the server slot if the request has
                                        ; been redirected by 3xx response.
   (socket :init-value #f)              ; A <socket> for persistent connection.
                                        ; If it is shutdown by the server,
                                        ; the APIs attempt to reconnect.
   (auth-handler  :init-keyword :auth-handler)
   (auth-user     :init-keyword :auth-user)
   (auth-password :init-keyword :auth-password)
   (extra-headers :init-keyword :extra-headers)
   ))

(define (make-http-connection server . opts)
  (let-keywords opts ((auth-handler  #f)
                      (auth-user     #f)
                      (auth-password #f)
                      (extra-headers '()))
    (make <http-connection>
      :server server :auth-handler auth-handler :auth-user auth-user
      :auth-password auth-password :extra-headers extra-headers)))

;;==============================================================
;; internal utilities
;;

(define (http-generic request server request-uri request-body options)
  (let-keywords options ((host server)
                         (no-redirect #f)
                         . opts)
    (let loop ((history '())
               (server server)
               (host host)
               (request-uri request-uri))
      (receive (code headers body)
          (request-response request server host request-uri request-body opts)
        (or (and-let* ([ (not no-redirect) ]
                       [ (string-prefix? "3" code) ]
                       [loc (assoc "location" headers)])
              (receive (uri server path*) (canonical-uri (cadr loc) server)
                (when (or (member uri history)
                          (> (length history) 20))
                  (errorf <http-error> "redirection is looping via ~a" uri))
                (loop (cons uri history) server server path*)))
            (values code headers body))))))

(define (server->socket server)
  (cond ((#/([^:]+):(\d+)/ server)
         => (lambda (m) (make-client-socket (m 1) (x->integer (m 2)))))
        (else (make-client-socket server 80))))

(define (with-server server proc)
  (let1 s (server->socket server)
    (unwind-protect
      (proc (socket-input-port s) (socket-output-port s))
      (socket-close s))))

(define (request-response request server host request-uri request-body options)
  (with-server
   server
   (lambda (in out)
     (send-request out request host request-uri request-body options)
     (receive (code headers) (receive-header in)
       (values code
               headers
               (and (not (eq? request 'HEAD))
                    (let-keywords options
                        ((sink    (open-output-string))
                         (flusher (lambda (sink _) (get-output-string sink)))
                         . #f)
                      (receive-body in headers sink flusher))))))))

;; canonicalize uri for the sake of redirection.
;; URI is a request-uri given to the API, or the redirect location specified
;; in 3xx response.  It can be a full URI or just a path w.r.t. the current
;; accessing server, so we pass the current server name as HOST in order to
;; fill the URI if necessary.
;; Returns three values; the full URI to access (it is used to detect a loop
;; in redirections), the server name, and the new request uri.
(define (canonical-uri uri host)
  (let*-values (((scheme specific) (uri-scheme&specific uri))
                ((h p q f) (uri-decompose-hierarchical specific)))
    (let ((scheme (or scheme "http")) ;; NB: consider https
          (host (or h host)))
      (values (uri-compose :scheme scheme :host host
                           :path p :query q :fragment f)
              (or h host)
              ;; drop "//"
              (string-drop (uri-compose :path p :query q :fragment f) 2)))))

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
  (copy-port remote sink :size size))

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
    )
  )

(provide "rfc/http")
