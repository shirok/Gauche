;;;
;;; www.cgi.test - framework to test CGI scripts
;;;
;;;   Copyright (c) 2003-2019  Shiro Kawai  <shiro@acm.org>
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

(define-module www.cgi.test
  (use gauche.process)
  (use rfc.uri)
  (use rfc.822)
  (use sxml.ssax)
  (export cgi-test-environment-ref
          call-with-cgi-script
          run-cgi-script->header&body
          run-cgi-script->sxml
          run-cgi-script->string
          run-cgi-script->string-list
          )
  )
(select-module www.cgi.test)

;; Default environments
(define *cgi-test-env*
  (hash-table 'string=?
              '("SERVER_SOFTWARE" . "cgitest/1.0")
              '("SERVER_NAME" . "localhost")
              '("GATEWAY_INTERFACE" . "CGI/1.1")
              '("SERVER_PROTOCOL" . "HTTP/1.1")
              '("SERVER_PORT" . "80")
              '("REQUEST_METHOD" . "GET")
              '("REMOTE_HOST" . "remote")
              '("REMOTE_ADDR" . "127.0.0.1")
              ))

(define cgi-test-environment-ref
  (getter-with-setter
   (^[key . maybe-default]
     (apply hash-table-get *cgi-test-env* (x->string key) maybe-default))
   (^[key value]
     (hash-table-put! *cgi-test-env* (x->string key) (x->string value)))))

;; Runs CGI script under specified environments.
;; Calls proc with a port connected to cgi process's stdout.
(define (call-with-cgi-script script proc :key (environment '()) (parameters #f))
  ;; set up environment
  (let1 envtab (make-hash-table 'string=?)
    (hash-table-for-each *cgi-test-env*
                         (cut hash-table-put! envtab <> <>))
    (dolist [p environment]
      (hash-table-put! envtab (x->string (car p)) (x->string (cdr p))))
    (let ([method (hash-table-get envtab "REQUEST_METHOD" "GET")]
          [query  (and parameters (build-query-string parameters))])
      (cond
       [(and parameters (member method '("GET" "HEAD")))
        (hash-table-put! envtab "QUERY_STRING" query)]
       [(and parameters (equal? method "POST"))
        ;; TODO: support multipart/form-data
        (hash-table-put! envtab "CONTENT_TYPE"
                         "application/x-www-form-urlencoded")
        (hash-table-put! envtab "CONTENT_LENGTH"
                         (x->string (string-size query)))])
      (call-with-process-io `("env" ,@(build-env envtab) ,script)
        (^[inp outp]
          (when (and query (equal? method "POST"))
            (display query outp)
            (newline outp)
            (close-output-port outp))
          (proc inp))
        :on-abnormal-exit :ignore)
      )))

;; Convenience procedurs
(define (run-cgi-script->header&body script reader . args)
  (apply call-with-cgi-script script
         (^p (let* ([header (rfc822-header->list p)]
                    [body   (reader p)])
               (values header body)))
         args))

(define (run-cgi-script->sxml script . args)
  (apply run-cgi-script->header&body script (cut ssax:xml->sxml <> '()) args))

(define (run-cgi-script->string script . args)
  (apply run-cgi-script->header&body script port->string args))

(define (run-cgi-script->string-list script . args)
  (apply run-cgi-script->header&body script port->string-list args))

;; Utils

(define (build-query-string param-alist)
  (string-join (map (^p (string-join
                         (map (^x (uri-encode-string (x->string x)))
                              (list (car p) (cdr p)))
                         "="))
                    param-alist)
               "&"))

(define (build-env env-table)
  (hash-table-map env-table (^[k v] #"~|k|=~|v|")))

