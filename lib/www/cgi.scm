;;;
;;; cgi.scm - CGI utility
;;;
;;;  Copyright(C) 2001-2002 by Shiro Kawai (shiro@acm.org)
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
;;;  $Id: cgi.scm,v 1.7 2002-04-22 03:32:25 shirok Exp $
;;;

;; Surprisingly, there's no ``formal'' definition of CGI.
;; The most reliable document I found is in <http://CGI-Spec.Golux.Com/>

;; TODO: a method should provided to return HTTP error condition to
;; the httpd.

(define-module www.cgi
  (use srfi-1)
  (use srfi-2)
  (use srfi-13)
  (use rfc.uri)
  (use rfc.cookie)
  (use gauche.parameter)
  (use text.tree)
  (use text.html-lite)
  (use text.parse)
  (export cgi-metavariables
          cgi-parse-parameters
          cgi-get-parameter
          cgi-header
          cgi-main)
  )
(select-module www.cgi)

;; A parameter cgi-metavariables can be used to supply metavariables,
;; instead of via environment variables.  It is handy for debugging,
;; or call CGI routine from other Scheme module.
(define cgi-metavariables (make-parameter #f))

;; Internal routine to get metavariable
(define (get-meta name)
  (or (and-let* ((mv (cgi-metavariables))
                 (p  (assoc name mv)))
        (cadr p))
      (sys-getenv name)))

;; Get query string.
(define (cgi-get-query)
  (let ((type   (get-meta "CONTENT_TYPE"))
        (method (get-meta "REQUEST_METHOD")))
    (when (and type (not (string-ci=? type "application/x-www-form-urlencoded")))
      (error "unsupported content-type" type))
    (cond ((not method)  ;; interactive use.
           (if (sys-isatty (current-input-port))
               (begin
                 (display "Enter parameters (name=value).  ^D to stop.\n")
                 (flush)
                 (let loop ((line (read-line))
                            (params '()))
                   (if (eof-object? line)
                       (string-join (reverse params) "&")
                       (loop (read-line) (cons line params)))))
               (error "REQUEST_METHOD not defined")))
          ((or (string-ci=? method "GET")
               (string-ci=? method "HEAD"))
           (or (get-meta "QUERY_STRING") ""))
          ((string-ci=? method "POST")
           ;; TODO: mutipart message
           (or (and-let* ((lenp (get-meta "CONTENT_LENGTH"))
                          (len  (x->integer lenp)))
                 (read-string len (current-input-port)))
               (port->string (current-input-port))))
          (else (error "unknown REQUEST_METHOD" method)))))

;; API: cgi-parse-parameters &keyword query-string merge-cookies
(define (cgi-parse-parameters . args)
  (let ((input   (or (get-keyword :query-string args #f)
                     (cgi-get-query)))
        (cookies (cond ((and (get-keyword :merge-cookies args #f)
                             (get-meta "HTTP_COOKIE"))
                        => parse-cookie-string)
                       (else '()))))
    (append
     (cond
      ((string-null? input) '())
      (else
       (fold-right (lambda (elt params)
                     (let* ((ss (string-split elt #\=))
                            (n  (uri-decode-string (car ss) :cgi-decode #t))
                            (p  (assoc n params))
                            (v  (if (null? (cdr ss))
                                    #t
                                    (uri-decode-string (string-join (cdr ss) "=")
                                                       :cgi-decode #t))))
                       (if p
                           (begin (set! (cdr p) (cons v (cdr p))) params)
                           (cons (list n v) params))))
                   '()
                   (string-split input #\&))))
     (map (lambda (cookie) (list (car cookie) (cadr cookie))) cookies))))

;; API: cgi-get-parameter key params &keyword list default convert
(define (cgi-get-parameter key params . args)
  (let* ((list?   (get-keyword :list args #f))
         (default (get-keyword :default args (if list? '() #f)))
         (convert (get-keyword :convert args identity)))
    (cond ((assoc key params)
           => (lambda (p)
                (if list?
                    (map convert (cdr p))
                    (convert (cadr p)))))
          (else default))))

;; API: cgi-header &keyword content-type location cookies
(define (cgi-header . args)
  (let ((content-type (get-keyword :content-type args "text/html"))
        (location     (get-keyword :location args #f))
        (cookies      (get-keyword :cookies args '())))
    (if location
        (list "Location: " (x->string location) "\n\n")
        (list "Content-type: " (x->string content-type) "\n"
              (map (lambda (cookie)
                     (list "Set-cookie: " cookie "\n"))
                   cookies)
              "\n"))))

;; API: cgi-main proc &keyword on-error merge-cookies
(define (cgi-main proc . args)
  (let ((eproc (get-keyword :on-error args
                            (lambda (e)
                              `(,(cgi-header)
                                ,(html-doctype)
                                ,(html:html
                                  (html:head (html:title "Error"))
                                  (html:body (html:h1 "Error")
                                             (html:p (html-escape-string
                                                      (slot-ref e 'message))))
                                  ))))))
    (with-error-handler
     (lambda (e) (write-tree (eproc e)))
     (lambda ()
       (let ((params (cgi-parse-parameters :merge-cookies
                                           (get-keyword :merge-cookies args #f))))
         (write-tree (proc params)))))
    ))

(provide "www/cgi")
