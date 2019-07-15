;;;
;;; cgi.scm - CGI utility
;;;
;;;   Copyright (c) 2000-2019  Shiro Kawai  <shiro@acm.org>
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

;; Surprisingly, there's no ``formal'' definition of CGI.
;; The most reliable document I found is in <http://CGI-Spec.Golux.Com/>

;; TODO: a method should provided to return HTTP error condition to
;; the httpd.

(define-module www.cgi
  (use srfi-1)
  (use srfi-13)
  (use rfc.uri)
  (use rfc.mime)
  (use rfc.822)
  (use gauche.parameter)
  (use gauche.charconv)
  (use gauche.uvector)
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
          cgi-main
          <cgi-error>
          <cgi-request-size-error>
          <cgi-content-type-error>
          <cgi-request-method-error>)
  )
(select-module www.cgi)

(autoload rfc.cookie parse-cookie-string)
(autoload gauche.vport open-input-limited-length-port)

;;----------------------------------------------------------------
;; A parameter cgi-metavariables can be used to supply metavariables,
;; instead of via environment variables.  It is handy for debugging,
;; or call CGI routine from other Scheme module.
(define cgi-metavariables (make-parameter #f))

;; Internal routine to get metavariable
(define (get-meta name)
  (or (and-let* ([mv (cgi-metavariables)]
                 [p  (assoc name mv)])
        (cadr p))
      (sys-getenv name)))

;; rename to export
(define cgi-get-metavariable get-meta)

;;----------------------------------------------------------------
;; Error conditions
;;

(define-condition-type <cgi-error> <error>
  #f)

(define-condition-type <cgi-request-size-error> <cgi-error>
  #f
  (name) ;; part name or #f
  (size))

(define-condition-type <cgi-content-type-error> <cgi-error>
  #f
  (content-type))

(define-condition-type <cgi-request-method-error> <cgi-error>
  #f
  (request-method))

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
;; If the request is multipart/mime, it just returns a symbol 'mime
;; without retrieving input.   cgi-parse-parameters handles the mime
;; message.
(define (cgi-get-query content-length)
  (let* ([type    (mime-parse-content-type (get-meta "CONTENT_TYPE"))]
         [typesig (and type (list (car type) (cadr type)))]
         [method  (get-meta "REQUEST_METHOD")])
    (unless (or (not type)
                (member typesig
                        '(("application" "x-www-form-urlencoded")
                          ("multipart" "form-data"))))
      (errorf <cgi-content-type-error>
              :content-type type
              "Unsupported CONTENT_TYPE: ~a" type))
    (cond [(not method)  ;; interactive use.
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
             (error <cgi-request-method-error>
                    :request-method #f
                    "REQUEST_METHOD not defined"))]
          [(or (string-ci=? method "GET")
               (string-ci=? method "HEAD"))
           (or (get-meta "QUERY_STRING") "")]
          [(string-ci=? method "POST")
           (if (equal? typesig '("multipart" "form-data"))
             'mime
             (or (and-let* ([lenp (or content-length
                                      (get-meta "CONTENT_LENGTH"))]
                            [len  (x->integer lenp)]
                            [ (<= 0 len) ]
                            [data (read-complete-block len)])
                   ;; NB: When we're here, we know content-type is
                   ;; application/x-www-form-urlencoded, so buf should consist
                   ;; of ASCII characters.
                   (u8vector->string data))
                 (port->string (current-input-port))))]
          [else
           (errorf <cgi-request-method-error>
                   :request-method method
                   "Unknown REQUEST_METHOD: ~a" method)]
          )))

;; Read LEN octets from the current input port and returns u8vector.
;; The port buffering may not be :full, so we should keep reading
;; until all data is retrieved.
(define (read-complete-block len)
  (if (zero? len)
    '#u8()
    (let ([buf (make-u8vector len)]
          [inp (current-input-port)])
      (let loop ([i 0])
        (let1 nread (read-block! buf inp i)
          (cond [(eof-object? nread)
                 (errorf <cgi-error> "POST request ends prematurely.  Expected content-length: ~a, but read only ~a octets." len i)]
                [(< (+ nread i) len) (loop (+ nread i))]
                [else buf]))))))

;;----------------------------------------------------------------
;; API: cgi-parse-parameters &keyword query-string merge-cookies
;;                                    part-handlers content-type
;;                                    mime-input
(define (cgi-parse-parameters :key
                              (query-string #f)
                              (merge-cookies #f)
                              (part-handlers '())
                              (content-type #f)
                              (content-length #f)
                              (mime-input #f))
  (let* ([input (cond [ query-string ]
                      [(or mime-input content-type) 'mime]
                      [else (cgi-get-query content-length)])]
         [cookies (if-let1 s (and merge-cookies (get-meta "HTTP_COOKIE"))
                    (parse-cookie-string s)
                    '())])
    (fold-params-by-name
     (cond
      [(eq? input 'mime)
       (get-mime-parts part-handlers
                       (or content-type (get-meta "CONTENT_TYPE"))
                       (or content-length (get-meta "CONTENT_LENGTH"))
                       (or mime-input (current-input-port)))]
      [(string-null? input) '()]
      [else (split-query-string input)])
     (map (^[cookie] (list (car cookie) (cadr cookie))) cookies))))

(define (fold-params-by-name alist tail)
  (fold-right (^[kv params]
                (if-let1 p (assoc (car kv) params)
                  (begin (push! (cdr p) (cdr kv)) params)
                  (cons (list (car kv) (cdr kv)) params)))
              tail alist))

(define (split-query-string input)
  (map (^[elt] (let1 ss (string-split elt #\=)
                 (cons (uri-decode-string (car ss) :cgi-decode #t)
                       (if (null? (cdr ss))
                         #t
                         (uri-decode-string (string-join (cdr ss) "=")
                                            :cgi-decode #t)))))
       (string-split input #[&\;])))

;; part-handlers: ((<name-list> <action> <options> ...) ...)
;;  <name-list> : list of field names (string, symbol, or regexp)
;;  <action>    : #f           ;; return as string (default)
;;              : file         ;; save content to a tmpfile,
;;                             ;;   returns filename.
;;              : file+name    ;; save content to a tmpfile,
;;                             ;;   returns a list (tmpfile origfile)
;;                             ;;   where origfile is the filename given
;;                             ;;   from the client.
;;              : ignore       ;; discard the value.
;;              : <procedure>  ;; calls <procedure> with
;;                             ;;   name, part-info, input-port.
;;  <options>   keyword-value list.
;;               :prefix "prefix"  - use "prefix" for temporary file
;;                                   (file and file+name only)
;;               :mode   <mode>    - set tmpfile's mode to <mode>
;;                                   (file and file+name only)
;;               :max-length integer - limit the content-length of the
;;                                   part.  if it is exceeded,
;;                                   <cgi-request-size-error> is
;;                                   thrown.  (NOT SUPPORTED)
(define (get-mime-parts part-handlers ctype clength inp)
  (define (part-ref info name)
    (rfc822-header-ref (ref info 'headers) name))

  (define (make-file-handler prefix honor-origfile? mode)
    (^[name filename part-info inp]
      (receive (outp tmpfile) (sys-mkstemp prefix)
        (cgi-add-temporary-file tmpfile)
        (mime-retrieve-body part-info inp outp)
        (close-output-port outp)
        (when mode (sys-chmod tmpfile mode))
        (if honor-origfile?
          (list tmpfile filename)
          tmpfile))))

  (define (string-handler name filename part-info inp)
    (mime-body->string part-info inp))

  (define (ignore-handler name filename part-info inp)
    (let loop ([ignore (read-line inp #t)])
      (if (eof-object? ignore) #f (loop (read-line inp #t)))))

  (define (get-action&opts part-name)
    (let1 clause (find (^[entry]
                         (or (eq? (car entry) #t)
                             (and (regexp? (car entry))
                                  (rxmatch (car entry) part-name))
                             (and (list? (car entry))
                                  (member part-name (map x->string (car entry))))
                             (string=? (x->string (car entry)) part-name)))
                       part-handlers)
      (cond
       [(or (not clause)
            (not (pair? (cdr clause))))
        (list string-handler)] ;; default action
       [(and (= (length clause) 3)
             (memq (cadr clause) '(file file+name))
             (string? (caddr clause)))
        ;; backward compatibility - will be deleted soon
        (list (cadr clause) :prefix (caddr clause))]
       [else (cdr clause)])))

  (define (get-handler action . opts)
    (cond
     [(not action) string-handler]
     [(memq action '(file file+name))
      (make-file-handler (get-keyword* :prefix opts
                                       (build-path (temporary-directory)
                                                   "gauche-cgi-"))
                         (eq? action 'file+name)
                         (get-keyword :mode opts #f))]
     [(eq? action 'ignore) ignore-handler]
     [else action]))

  ;; The value of content-disposition must be a properly quoted string
  ;; according to RFC2183 and RFC2045.  However, IE sends a pathname including
  ;; backslashes without quoting them.  As a compromise, we only consider
  ;; backslash escape if the following character is either #\" or #\\.
  ;; (This fix is provided by Tatsuya BIZENN).
  (define (content-disposition-string input)
    (read-char input)                   ; discard beginning DQUOTE
    (let1 r (open-output-string :private? #t)
      (define (finish) (get-output-string r))
      (let loop ([c (read-char input)])
        (cond [(eof-object? c) (finish)] ; tolerate missing closing DQUOTE
              [(char=? c #\")  (finish)] ; discard ending DQUOTE
              [(char=? c #\\)
               (let1 c (read-char input)
                 (cond [(eof-object? c) (finish)] ;; tolerate stray backslash
                       [else (unless (char-set-contains? #[\\\"] c)
                               (write-char #\\ r))
                             (write-char c r)
                             (loop (read-char input))]))]
              [else (write-char c r) (loop (read-char input))]))))

  (define (parse-content-disposition field)
    (if field
      (rfc822-field->tokens field
                            `((#[\"] . ,content-disposition-string)
                              (,*rfc822-atext-chars* . ,rfc822-dot-atom)))
      '()))

  ;; NB: This is not an ideal solution.  There could be a malformed
  ;; option string (e.g. filename=c:\foo\bar\baz, in which the option
  ;; value should be quoted but we have to accept those non-conforming
  ;; clients).
  (define (get-option optname optregex opts)
    (cond [(member optname opts) => cadr]
          [(any (^t (and (string? t) (rxmatch optregex t))) opts)
           => (cut rxmatch-after <>)]
          [else #f]))

  (define (handle-part part-info inp)
    (let* ([cd   (part-ref part-info "content-disposition")]
           [opts (parse-content-disposition cd)]
           [name (get-option "name=" #/^name=/ opts)]
           [filename (get-option "filename=" #/^filename=/ opts)])
      (cond
       [(not name)      ;; if no name is given, just ignore this part.
        (ignore-handler name filename part-info inp)
        #f]
       [(not filename)  ;; this is not a file uploading field.
        (cons name (string-handler name filename part-info inp))]
       [(string-null? filename) ;; file field is empty
        (cons name (ignore-handler name filename part-info inp))]
       [else
        (let* ([action&opts (get-action&opts name)]
               [handler (apply get-handler action&opts)]
               [result (handler name filename part-info inp)])
          (cons name result))])))

  (let* ([inp (if (and clength (<= 0 (x->integer clength)))
                (open-input-limited-length-port inp (x->integer clength))
                inp)]
         [result (mime-parse-message inp `(("content-type" ,ctype))
                                     handle-part)])
    (filter-map (cut ~ <>'content) (~ result'content))))

;;----------------------------------------------------------------
;; API: cgi-get-parameter key params &keyword list default convert
;;
(define (cgi-get-parameter key params :key ((:list lis) #f)
                                           (default (if lis '() #f))
                                           ((:convert cv) identity))
  (cond [(assoc key params) => (^p (if lis (map cv (cdr p)) (cv (cadr p))))]
        [else default]))

;;----------------------------------------------------------------
;; API: cgi-header &keyword content-type cookies location &allow-other-keys
;;
(define (cgi-header :key
                    (content-type #f)
                    (location     #f)
                    (status       #f)
                    (cookies      '())
                    :allow-other-keys rest)
  (let ([ct (or content-type
                (and (not location) "text/html"))]
        [r '()])
    (when status   (push! r #"Status: ~status\r\n"))
    (when ct       (push! r #"Content-type: ~ct\r\n"))
    (when location (push! r #"Location: ~location\r\n"))
    (dolist [cookie cookies]
      (push! r #"Set-cookie: ~cookie\r\n"))
    (dolist [p (slices rest 2)]
      (when (pair? (cdr p))
        (let1 hdrname (if (keyword? (car p)) (keyword->string (car p)) (car p))
          (push! r #"~|hdrname|: ~(cadr p)\r\n"))))
    (push! r "\r\n")
    (reverse r)))

;;----------------------------------------------------------------
;; API: cgi-main proc &keyword on-error merge-cookies
;;                             output-proc part-handlers
(define (cgi-main proc :key
                  (on-error cgi-default-error-proc)
                  (output-proc cgi-default-output)
                  (merge-cookies #f)
                  (part-handlers '()))
  (set! (port-buffering (current-error-port)) :line)
  (guard (e [else (output-proc (on-error e))])
    (let1 params (cgi-parse-parameters :merge-cookies merge-cookies
                                       :part-handlers part-handlers)
      (output-proc (proc params))))
  ;; remove any temporary files
  (for-each sys-unlink (cgi-temporary-files))
  0)

;; aux fns
(define (cgi-default-error-proc e)
  `(,(cgi-header)
    ,(html-doctype)
    ,(html:html
      (html:head (html:title "Error"))
      (html:body (html:h1 "Error")
                 (html:p (html-escape-string (slot-ref e 'message)))))))

(define (cgi-default-output tree)
  (if (ces-equivalent? (gauche-character-encoding)
                       (cgi-output-character-encoding))
    (write-tree tree)
    ;; NB: we avoid using wrap-with-output-conversion, for we want
    ;; to make sure output is flushed each time in case cgi-main
    ;; is used in a persistent process.
    (with-output-conversion
     (current-output-port)
     (cut write-tree tree)
     :encoding (cgi-output-character-encoding))))

