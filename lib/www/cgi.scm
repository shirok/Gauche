;;;
;;; cgi.scm - CGI utility
;;;  
;;;   Copyright (c) 2000-2004 Shiro Kawai, All rights reserved.
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
;;;  $Id: cgi.scm,v 1.18 2004-09-14 21:18:05 shirok Exp $
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
  (use rfc.mime)
  (use rfc.822)
  (use gauche.parameter)
  (use gauche.charconv)
  (use text.tree)
  (use text.html-lite)
  (use file.util)
  (export cgi-metavariables
          cgi-get-metavariable
          cgi-output-character-encoding
          cgi-parse-parameters
          cgi-get-parameter
          cgi-header
          cgi-temporary-files
          cgi-add-temporary-file 
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

;; rename to export
(define cgi-get-metavariable get-meta)

;;----------------------------------------------------------------
;; The output character encoding.  Used to generate the default
;; output generator.

(define cgi-output-character-encoding
  (make-parameter (gauche-character-encoding)))

;;----------------------------------------------------------------
;; The list of temporary files created to save uploaded files.
;; The files will be unlinked when cgi-main exits.

(define cgi-temporary-files
  (make-parameter '()))

(define (cgi-add-temporary-file path)
  (cgi-temporary-files (cons path (cgi-temporary-files))))

;;----------------------------------------------------------------
;; Get query string. (internal)
;;
(define (cgi-get-query)
  (let* ((type   (mime-parse-content-type (get-meta "CONTENT_TYPE")))
         (typesig (and type (list (car type) (cadr type))))
         (method (get-meta "REQUEST_METHOD")))
    (unless (or (not type)
                (member typesig
                        '(("application" "x-www-form-urlencoded")
                          ("multipart" "form-data"))))
      (error "unsupported content-type" typesig))
    (cond ((not method)  ;; interactive use.
           (if (sys-isatty (current-input-port))
             (begin
               (display "Enter parameters (name=value).  ^D to stop.\n")
               (flush)
               (let loop ((line (read-line))
                          (params '()))
                 (if (eof-object? line)
                   (string-join (reverse params) "&")
                   (loop (read-line)
                         (if (string-null? line)
                           params
                           (cons line params))))))
             (error "REQUEST_METHOD not defined")))
          ((or (string-ci=? method "GET")
               (string-ci=? method "HEAD"))
           (or (get-meta "QUERY_STRING") ""))
          ((string-ci=? method "POST")
           (if (equal? typesig '("multipart" "form-data"))
             'mime
             (or (and-let* ((lenp (get-meta "CONTENT_LENGTH"))
                            (len  (x->integer lenp)))
                   (string-incomplete->complete (read-block len)))
                 (port->string (current-input-port)))))
          (else (error "unknown REQUEST_METHOD" method)))))

;;----------------------------------------------------------------
;; API: cgi-parse-parameters &keyword query-string merge-cookies
;;                                    part-handlers
(define (cgi-parse-parameters . args)
  (let ((input   (or (get-keyword :query-string args #f)
                     (cgi-get-query)))
        (part-handlers (get-keyword :part-handlers args '()))
        (cookies (cond ((and (get-keyword :merge-cookies args #f)
                             (get-meta "HTTP_COOKIE"))
                        => parse-cookie-string)
                       (else '()))))
    (append
     (cond
      ((eq? input 'mime) ;; cgi-get-query returns this if content-type is mime
       (get-mime-parts part-handlers))
      ((string-null? input) '())
      (else (split-query-string input)))
     (map (lambda (cookie) (list (car cookie) (cadr cookie))) cookies))))

(define (split-query-string input)
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
              (string-split input #[&\;])))

;; part-handlers: ((<name-list> <action>) ...)
;;  <name-list> : list of field names (string or symbol)
;;  <action>    : #f                ;; return as string
;;              : file [<prefix>]   ;; save content to a tmpfile,
;;                                  ;;   returns filename.
;;              : ignore            ;; discard the value.
;;              : <procedure>       ;; calls <procedure> with
;;                                  ;;   name, part-info, reader, input-port.
;;
(define (get-mime-parts part-handlers)
  (define (part-ref info name)
    (rfc822-header-ref (ref info 'headers) name))

  (define (make-file-handler prefix)
    (lambda (name filename part-info reader inp)
      (receive (outp tmpfile) (sys-mkstemp prefix)
        (cgi-add-temporary-file tmpfile)
        (mime-retrieve-body part-info reader inp outp)
        (close-output-port outp)
        tmpfile)))

  (define (string-handler name filename part-info reader inp)
    (mime-body->string part-info reader inp))

  (define (ignore-handler name filename part-info reader inp)
    (let loop ((ignore (reader inp)))
      (if (eof-object? ignore) #f (loop (reader inp)))))

  (define (get-handler part-name)
    (let* ((handler (find (lambda (entry)
                            (or (eq? (car entry) #t)
                                (and (regexp? (car entry))
                                     (rxmatch (car entry) part-name))
                                (and (list? (car entry))
                                     (member part-name
                                             (map x->string (car entry))))
                                (string=? (x->string (car entry)) part-name)))
                          part-handlers))
           (action  (if handler (cdr handler) '(#f))))
      (cond
       ((not (pair? action))
        (error "malformed part-handler clause:" handler))
       ((eq? (car action) #f)
        string-handler)
       ((eq? (car action) 'file)
        (make-file-handler (if (null? (cdr action))
                             (build-path (temporary-directory) "gauche-cgi")
                             (cadr action))))
       ((eq? (car action) 'ignore)
        ignore-handler)
       (else
        (car action)))))

  (define (handle-part part-info reader)
    (let* ((cd   (part-ref part-info "content-disposition"))
           (opts (rfc822-field->tokens cd))
           (name (cond ((member "name=" opts) => cadr) (else #f)))
           (filename (cond ((member "filename=" opts) => cadr) (else #f)))
           (handler (cond ((not name) ignore-handler)
                          ((not filename) string-handler)
                          ((string-null? filename) ignore-handler)
                          (else  (get-handler name))))
           (result (handler name filename part-info reader
                            (current-input-port))))
      (if name (list name result) #f)))

  (let* ((ctype (get-meta "CONTENT_TYPE"))
         (inp   (current-input-port))
         (result (mime-parse-message inp `(("content-type" ,ctype))
                                     handle-part)))
    (filter-map (cut ref <> 'content) (ref result 'content)))
  )

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
                       (status       #f)
                       (cookies      '()))
    (let ((ct (or content-type
                  (and (not location) "text/html")))
          (r '()))
      (when status   (push! r #`"Status: ,status\n"))
      (when ct       (push! r #`"Content-type: ,ct\n"))
      (when location (push! r #`"Location: ,location\n"))
      (for-each (lambda (cookie)
                  (push! r #`"Set-cookie: ,cookie\n"))
                cookies)
      (let loop ((args args))
        (cond ((null? args))
              ((null? (cdr args)))
              ((memv (car args) '(:content-type :location :status :cookies))
               (loop (cddr args)))
              (else
               (push! r #`",(car args): ,(cadr args)\n")
               (loop (cddr args)))))
      (push! r "\n")
      (reverse r))))
      
;;----------------------------------------------------------------
;; API: cgi-main proc &keyword on-error merge-cookies
;;                             output-proc part-handlers
(define (cgi-main proc . args)
  (let-keywords* args ((on-error cgi-default-error-proc)
                       (output-proc cgi-default-output)
                       (merge-cookies #f)
                       (part-handlers '()))
    (with-error-handler
     (lambda (e) (output-proc (on-error e)))
     (lambda ()
       (let1 params
           (cgi-parse-parameters :merge-cookies merge-cookies
                                 :part-handlers part-handlers)
         (output-proc (proc params)))))
    ;; remove any temporary files
    (for-each sys-unlink (cgi-temporary-files))
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
