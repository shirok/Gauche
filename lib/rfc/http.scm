;;;
;;; http.scm - HTTP 1.1
;;;
;;;  Copyright(C) 2002 by Shiro Kawai (shiro@acm.org)
;;;
;;;  Permission to use, copy, modify, distribute this software and
;;;  accompanying documentation for any purpose is hereby granted,
;;;  provided that existing copyright notices are retained in all
;;;  copies and that this notice is included verbatim in all
;;;  distributions.
;;;  This software is provided as is, without express or implied
;;;  warranty.  In no circumstances the author(s) shall be liable
;;;  for any damages arising out of the use of this software.
;;;
;;;  $Id: http.scm,v 1.3 2002-11-14 05:05:41 shirok Exp $
;;;

;; HTTP handling routines.

;; RFC2616 Hypertext Transfer Protocol -- HTTP/1.1
;;  http://www.w3.org/Protocols/rfc2616/rfc2616.txt

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
  (export http-user-agent
          http-get http-head http-post
          )
  )
(select-module rfc.http)

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
;;
;; This family of APIs takes mandatory server and request-uri
;; arguments, and optional keyword arguments.
;;
;; The server argument specifies the server name (and optionally
;; port number by the format of "server:port").  The future extention
;; allow some sort of connection object for the persistent connection.
;;
;; The request-uri argument is as specified in RFC2616.

(define (http-get server request-uri . options)
  (request-response 'GET server request-uri options))

(define (http-head server request-uri . options)
  (request-response 'HEAD server request-uri options))

(define (http-post server request-uri body . options)
  (request-response 'POST server request-uri
                    (list* :request-body body options)))

;;==============================================================
;; internal utilities
;;

(define (server->socket server)
  (cond ((#/([^:]+):(\d+)/ server)
         => (lambda (m) (make-client-socket (m 1) (x->integer (m 2)))))
        (else (make-client-socket server 80))))

(define (server->host server)
  (car (string-split server #\:)))

(define (with-server server proc)
  (let1 s (server->socket server)
    (dynamic-wind
     (lambda () #f)
     (lambda ()
       (proc (socket-input-port s) (socket-output-port s)))
     (lambda () (socket-close s)))))

(define (request-response request server request-uri options)
  (let* ((sink    (get-keyword* :sink options (open-output-string)))
         (flusher (get-keyword  :flusher options get-output-string))
         (host    (get-keyword* :host options (server->host server)))
         (follow  (not (get-keyword  :no-redirect options #f)))
         (has-content? (not (memq request '(HEAD)))))
    (if follow
        (let loop ((history (list (canonical-uri request-uri host)))
                   (uri request-uri))
          (receive (code headers body redirect)
              (with-server server
                (lambda (in out)
                  (send-request out request host uri options)
                  (receive (code headers) (receive-header in)
                    (cond
                     ((and (string-prefix? "3" code)
                           (assoc "location" headers))
                      => (lambda (loc)
                           (let1 uri (canonical-uri (cadr loc) host)
                             (when (member uri history)
                               (errorf "redirection is looping via ~a" uri))
                             (values code headers #f uri))))
                     (else
                      (values code headers
                              (and has-content?
                                   (receive-body in headers sink flusher))
                              #f))))))
            (if redirect
                (loop (cons redirect history) redirect)
                (values code headers body))))
        (with-server server
          (lambda (in out)
            (send-request out request host request-uri options)
            (receive (code headers) (receive-header in)
              (values code headers
                      (and has-content?
                           (receive-body in headers sink flusher))))))
        )
    ))

;; canonicalize uri
(define (canonical-uri uri host)
  (let*-values (((scheme specific) (uri-scheme&specific uri))
                ((h p q f) (uri-decompose-hierarchical specific)))
    (uri-compose :scheme (or scheme "http")
                 :authority (or h host)
                 :path p
                 :query q
                 :fragment f)))
        
;; send
(define (send-request out request host uri options)
  (display #`",request ,uri HTTP/1.1\r\n" out)
  (display #`"Host: ,|host|\r\n" out)
  (if (memq request '(POST PUT))
      ;; for now, we don't support chunked encoding in POST method.
      (let ((body (get-keyword :request-body options ""))
            (content-length (get-keyword :content-length options #f)))
        (send-request-headers (if content-length
                                  options
                                  (list* :content-length (string-size body)
                                         options))
                              out)
        (display "\r\n" out)
        (display body out)
        (display "\r\n" out))
      ;; requests w/o body
      (begin
        (send-request-headers options out)
        (display "\r\n" out))))

(define (send-request-headers options out)
  ;; options with those keywords are internal-use.  Other options
  ;; will be sent in part of the header.  NB: host header is sent
  ;; by send-request, so we exclude it here.
  (define internal-keywords
    '(:sink :flusher :follow-redirect :host :request-body))

  (let loop ((options options))
    (cond ((null? options))
          ((null? (cdr options)))
          ((memv (car options) internal-keywords)
           (loop (cddr options)))
          (else
           (format out "~a: ~a\r\n" (car options) (cadr options))
           (loop (cddr options)))))
  )

;; receive
(define (receive-header remote)
  (receive (code reason) (parse-status-line (read-line remote))
    (values code (rfc822-header->list remote))))

(define (parse-status-line line)
  (cond ((eof-object? line) 
         (error "http reply contains no data"))
        ((#/\w+\s+(\d\d\d)\s+(.*)/ line)
         => (lambda (m) (values (m 1) (m 2))))
        (else
         (error "bad reply from server" line))))

(define (receive-body remote headers sink flusher)
  (cond ((assoc "content-length" headers)
         => (lambda (p)
              (receive-body-nochunked (x->integer (cadr p)) remote sink)))
        ((assoc "transfer-encoding" headers)
         => (lambda (p)
              (if (equal? (cadr p) "chunked")
                  (receive-body-chunked remote sink)
                  (error "unsupported transfer-encoding" (cadr p))))))
  (flusher sink))

(define (receive-body-nochunked size remote sink)
  (copy-port remote sink :size size))

;; NB: chunk extension and trailer are ignored for now.
(define (receive-body-chunked remote sink)
  (let loop ((line (read-line remote)))
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
      (error "bad line in chunked data" initial-line))
    )
  )

(provide "rfc/http")




