;;;
;;; cgi.scm - CGI utility
;;;  
;;;   Copyright (c) 2000-2003 Shiro Kawai, All rights reserved.
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
;;;  $Id: cgi.scm,v 1.12 2003-07-05 20:40:49 shirok Exp $
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
  (use gauche.charconv)
  (use text.tree)
  (use text.html-lite)
  (export cgi-metavariables
          cgi-output-character-encoding
          cgi-parse-parameters
          cgi-get-parameter
          cgi-header
          cgi-main)
  )
(select-module www.cgi)

;;----------------------------------------------------------------
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

;;----------------------------------------------------------------
;; The output character encoding.  Used to generate the default
;; output generator.

(define cgi-output-character-encoding
  (make-parameter (gauche-character-encoding)))

;;----------------------------------------------------------------
;; Get query string. (internal)
;;
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
                 (string-incomplete->complete (read-block len)))
               (port->string (current-input-port))))
          (else (error "unknown REQUEST_METHOD" method)))))

;;----------------------------------------------------------------
;; API: cgi-parse-parameters &keyword query-string merge-cookies
;;
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

;;----------------------------------------------------------------
;; API: cgi-get-parameter key params &keyword list default convert
;;
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

;;----------------------------------------------------------------
;; API: cgi-header &keyword content-type cookies location &allow-other-keys
;;
(define (cgi-header . args)
  (let-keywords* args ((content-type #f)
                       (location     #f)
                       (cookies      '()))
    (let ((ct (or content-type
                  (and (not location) "text/html")))
          (r '()))
      (when ct       (push! r #`"Content-type: ,ct\n"))
      (when location (push! r #`"Location: ,location\n"))
      (for-each (lambda (cookie)
                  (push! r #`"Set-cookie: ,cookie\n"))
                cookies)
      (let loop ((args args))
        (cond ((null? args))
              ((null? (cdr args)))
              ((memv (car args) '(:content-type :location :cookies))
               (loop (cddr args)))
              (else
               (push! r #`",(car args): ,(cadr args)\n")
               (loop (cddr args)))))
      (push! r "\n")
      (reverse r))))
      
;;----------------------------------------------------------------
;; API: cgi-main proc &keyword on-error merge-cookies
;;
(define (cgi-main proc . args)
  (let-keywords* args ((on-error cgi-default-error-proc)
                       (output-proc cgi-default-output))
    (with-error-handler
     (lambda (e) (output-proc (on-error e)))
     (lambda ()
       (let1 params
           (cgi-parse-parameters :merge-cookies
                                 (get-keyword :merge-cookies args #f))
         (output-proc (proc params)))))
    ))

;; aux fns
(define (cgi-default-error-proc e)
  `(,(cgi-header)
    ,(html-doctype)
    ,(html:html
      (html:head (html:title "Error"))
      (html:body (html:h1 "Error")
                 (html:p (html-escape-string (slot-ref e 'message)))))))

(define (cgi-default-output tree)
  (write-tree tree (wrap-with-output-conversion
                    (current-output-port)
                    (cgi-output-character-encoding))))

(provide "www/cgi")
