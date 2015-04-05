;;
;; test www.* modules
;;

(use gauche.test)
(use gauche.parameter)
(use gauche.charconv)
(use text.tree)
(use rfc.822)
(use file.util)
(test-start "www.* modules")

;;------------------------------------------------
(test-section "www.cgi")
(use www.cgi)
(test-module 'www.cgi)

(define params #f)
(define qs1 "a=foo+bar&boo=baz=doo&z%3Dz=%21%26&a=+%20&#=#&z=z=8&r&r=2")
(define qr1 '(("boo" "baz=doo") ("z=z" "!&") ("a" "foo bar" "  ") ("#" "#") ("z" "z=8") ("r" #t "2")))
(define qs1b "a=foo+bar;boo=baz=doo;z%3Dz=%21%26;a=+%20;#=#;z=z=8;r;r=2")

(define qs2 "zz=aa&aa=zz")
(define qr2 '(("zz" "aa") ("aa" "zz")))

(test* "cgi-parse-parameters" qr1
       (cgi-parse-parameters :query-string qs1))
(test* "cgi-parse-parameters" qr1
       (cgi-parse-parameters :query-string qs1b))

(define ps1 "--boundary
Content-Disposition: form-data; name=\"aaa\"

111
--boundary
Content-Disposition: form-data; name=\"bbb\"; filename=\"x.txt\"
Content-Type: text/plain

abc
def
ghi

--boundary
Content-Disposition: form-data; name=\"ccc\"; filename=\"\"

--boundary
Content-Disposition: form-data: name=\"ddd\"; filename=\"ttt\\bbb\"
Content-Type: application/octet-stream
Content-Transfer-Encoding: base64

VGhpcyBpcyBhIHRlc3Qgc2VudGVuY2Uu
--boundary--
")

(define ps2 "--boundary
Content-Disposition: form-data; name=aaa

111
--boundary
Content-Disposition: form-data; name=bbb; filename=x.txt
Content-Type: text/plain

abc
def
ghi

--boundary
Content-Disposition: form-data; name=ccc ; filename=\"\"

--boundary
Content-Disposition: form-data;name=ddd; filename=\"ttt\\bbb\"
Content-Type: application/octet-stream
Content-Transfer-Encoding: base64

VGhpcyBpcyBhIHRlc3Qgc2VudGVuY2Uu
--boundary--
")

(define pr1 '(("aaa" "111") ("bbb" "abc\ndef\nghi\n") ("ccc" #f) ("ddd" "This is a test sentence.")))

(define pr2 '(("aaa" "111") ("bbb" "x.txt") ("ccc" #f) ("ddd" "ttt\\bbb")))

(define (multipart-parse-test src)
  (test* "cgi-parse-parameters (multipart)" pr1
         (parameterize ((cgi-metavariables
                         `(("REQUEST_METHOD" "POST")
                           ("CONTENT_TYPE" "multipart/form-data; boundary=boundary")
                           ("CONTENT_LENGTH" ,(string-size src)))))
           (with-input-from-string src
             (lambda () (cgi-parse-parameters)))))

  (test* "cgi-parse-parameters (multipart, custom handler)" pr2
         (parameterize ((cgi-metavariables
                         `(("REQUEST_METHOD" "POST")
                           ("CONTENT_TYPE" "multipart/form-data; boundary=boundary")
                           ("CONTENT_LENGTH" ,(string-size src)))))
           (with-input-from-string src
             (lambda ()
               (cgi-parse-parameters
                :part-handlers
                `((#t ,(lambda (name filename info inp)
                         (let loop ((line (read-line inp)))
                           (if (eof-object? line)
                             filename
                             (loop (read-line inp))))))))))
           ))

  (test* "cgi-parse-parameters (multipart, custom handler 2)" "abc\ndef\nghi\n"
         (parameterize ((cgi-metavariables
                         `(("REQUEST_METHOD" "POST")
                           ("CONTENT_TYPE" "multipart/form-data; boundary=boundary")
                           ("CONTENT_LENGTH" ,(string-size src)))))
           (let1 r
               (with-input-from-string src
                 (lambda ()
                   (cgi-parse-parameters
                    :part-handlers `(("bbb" file :prefix "./bbb")))))
             (let* ((tmpfile (cgi-get-parameter "bbb" r))
                    (content (file->string tmpfile)))
               (sys-unlink tmpfile)
               content))))

  (test* "cgi-parse-parameters (multipart, custom handler 3)" "abc\ndef\nghi\n"
         (parameterize ((cgi-metavariables
                         `(("REQUEST_METHOD" "POST")
                           ("CONTENT_TYPE" "multipart/form-data; boundary=boundary")
                           ("CONTENT_LENGTH" ,(string-size src)))))
           (let1 r
               (with-input-from-string src
                 (lambda ()
                   (cgi-parse-parameters
                    :part-handlers `((#/b{3}/ file :prefix "./bbb")))))
             (let* ((tmpfile (cgi-get-parameter "bbb" r))
                    (content (file->string tmpfile)))
               (sys-unlink tmpfile)
               content))))
  )

(multipart-parse-test ps1)
(multipart-parse-test ps2)

(define ps3 "--boundary
Content-Disposition: form-data; name=aaa

111
--boundary
Content-Disposition: form-data; name=bbb

000
--boundary
Content-Disposition: form-data; name=aaa

222
--boundary
Content-Disposition: form-data; name=\"aaa\"

333
--boundary
Content-Disposition: form-data; name=bbb

999
--boundary--")

(define pr3 '(("aaa" "111" "222" "333") ("bbb" "000" "999")))

(test* "cgi-parse-parameter (multipart, multivalue)" pr3
       (parameterize ((cgi-metavariables
                       `(("REQUEST_METHOD" "POST")
                         ("CONTENT_TYPE" "multipart/form-data; boundary=boundary")
                         ("CONTENT_LENGTH" ,(string-size ps3)))))
         (with-input-from-string ps3 cgi-parse-parameters)))

(test* "cgi-get-parameter" "foo bar"
       (cgi-get-parameter "a" qr1))
(test* "cgi-get-parameter" '("foo bar" "  ")
       (cgi-get-parameter "a" qr1 :list #t))
(test* "cgi-get-parameter" #t
       (cgi-get-parameter "r" qr1))
(test* "cgi-get-parameter" '(#t "2")
       (cgi-get-parameter "r" qr1 :list #t))
(test* "cgi-get-parameter" '("baz=doo")
       (cgi-get-parameter "boo" qr1 :list #t))
(test* "cgi-get-parameter" 'none
       (cgi-get-parameter "booz" qr1 :default 'none))
(test* "cgi-get-parameter" #f
       (cgi-get-parameter "booz" qr1))
(test* "cgi-get-parameter" '()
       (cgi-get-parameter "booz" qr1 :list #t))
(test* "cgi-get-parameter" '(0 2)
       (cgi-get-parameter "r" qr1 :convert x->integer :list #t))

(test* "cgi-get-query (GET)" qr1
       (parameterize ((cgi-metavariables `(("REQUEST_METHOD" "GET")
                                           ("QUERY_STRING" ,qs1))))
         (with-input-from-string qs2
           cgi-parse-parameters)))
(test* "cgi-get-query (HEAD)" qr1
       (parameterize ((cgi-metavariables `(("REQUEST_METHOD" "HEAD")
                                           ("QUERY_STRING" ,qs1))))
         (with-input-from-string qs2
           cgi-parse-parameters)))
(test* "cgi-get-query (POST)" qr2
       (parameterize ((cgi-metavariables `(("REQUEST_METHOD" "POST")
                                           ("QUERY_STRING" ,qs1))))
         (with-input-from-string qs2
           cgi-parse-parameters)))
(test* "cgi-get-query (POST)" qr2
       (parameterize ((cgi-metavariables `(("REQUEST_METHOD" "POST")
                                           ("CONTENT_LENGTH" ,(string-length qs2)))))
         (with-input-from-string qs2
           cgi-parse-parameters)))
(test* "cgi-get-query (POST)" '(("zz" "aa"))
       (parameterize ((cgi-metavariables `(("REQUEST_METHOD" "POST")
                                           ("CONTENT_LENGTH" 5))))
         (with-input-from-string qs2
           cgi-parse-parameters)))

(test* "cgi-header" "Content-type: text/html\r\n\r\n"
       (tree->string (cgi-header)))

(test* "cgi-header" "Location: http://foo.bar/\r\n\r\n"
       (tree->string (cgi-header :location "http://foo.bar/")))

(test* "cgi-header" "Content-type: hoge\r\nLocation: http://foo.bar/\r\n\r\n"
       (tree->string
        (cgi-header :location "http://foo.bar/" :content-type "hoge")))

(test* "cgi-header" "Content-type: text/plain; charset=utf-8\r\n\r\n"
       (tree->string
        (cgi-header :content-type "text/plain; charset=utf-8")))

(test* "cgi-header"
       "Content-type: text/html\r\nSet-cookie: hoge\r\nSet-cookie: poge\r\n\r\n"
       (tree->string
        (cgi-header :cookies '("hoge" "poge"))))

(test* "cgi-header"
       "Content-type: text/html\r\nSet-cookie: hoge\r\nSet-cookie: poge\r\nx-foo: foo\r\n\r\n"
       (tree->string
        (cgi-header :x-foo "foo" :cookies '("hoge" "poge"))))

(test* "cgi-main" "Content-type: text/plain\r\n\r\na=foo bar"
       (parameterize ((cgi-metavariables `(("REQUEST_METHOD" "GET")
                                           ("QUERY_STRING" ,qs1))))
         (with-output-to-string
           (lambda ()
             (cgi-main
              (lambda (params)
                `(,(cgi-header :content-type "text/plain")
                  "a="
                  ,(cgi-get-parameter "a" params))))))))

(unless (eq? (gauche-character-encoding) 'none)
  (test* "cgi-output-character-encoding" #*"\xe3\x81\x82"
         (string-complete->incomplete
          (parameterize ((cgi-metavariables `(("REQUEST_METHOD" "GET")
                                              ("QUERY_STRING" "")))
                         (cgi-output-character-encoding 'utf8))
            (with-output-to-string
              (lambda ()
                (cgi-main
                 (lambda (params)
                   (string #\u3042)))))))))

;;------------------------------------------------
(test-section "www.cgi.test")
(use www.cgi.test)
(test-module 'www.cgi.test)

(test* "cgi-test-environment-ref" "remote"
       (cgi-test-environment-ref "REMOTE_HOST"))
(test* "cgi-test-environment-ref" "zzz"
       (cgi-test-environment-ref 'ZZZ "zzz"))
(test* "cgi-test-environment-set!" "foo.com"
       (begin
         (set! (cgi-test-environment-ref 'REMOTE_HOST) "foo.com")
         (cgi-test-environment-ref "REMOTE_HOST")))

(cond-expand
 [gauche.os.windows
  ;; windows can't support #! magic anyways.
  ]
 [else
  (sys-system "rm -rf test.o")
  (sys-mkdir "test.o" #o755)

  (with-output-to-file "test.o/cgitest.cgi"
    (lambda ()
      (print "#!/bin/sh")
      (print "echo Content-type: text/plain")
      (print "echo")
      (print "echo \"SERVER_NAME = $SERVER_NAME\"")
      (print "echo \"REMOTE_HOST = $REMOTE_HOST\"")
      (print "echo \"REQUEST_METHOD = $REQUEST_METHOD\"")
      (print "echo \"CONTENT_TYPE = $CONTENT_TYPE\"")
      (print "echo \"QUERY_STRING = $QUERY_STRING\"")))

  (sys-chmod "test.o/cgitest.cgi" #o755)

  (test* "call-with-cgi-script" '(("content-type" "text/plain"))
         (call-with-cgi-script "test.o/cgitest.cgi"
           (lambda (p)
             (rfc822-header->list p)))
         )

  (test* "run-cgi-script->string-list"
         '((("content-type" "text/plain"))
           ("SERVER_NAME = localhost"
            "REMOTE_HOST = foo.com"
            "REQUEST_METHOD = GET"
            "CONTENT_TYPE = "
            "QUERY_STRING = "))
         (receive r (run-cgi-script->string-list "test.o/cgitest.cgi")
           r)
         )

  (test* "run-cgi-script->string-list (using parameters/GET)"
         '("SERVER_NAME = localhost"
           "REMOTE_HOST = foo.com"
           "REQUEST_METHOD = GET"
           "CONTENT_TYPE = "
           "QUERY_STRING = a=b&%26%26%24%26=%21%40%21%40")
         (receive (_ body)
           (run-cgi-script->string-list "test.o/cgitest.cgi"
            :parameters '((a . b) (&&$& . !@!@)))
           body))

  (test* "run-cgi-script->string-list (using parameters/HEAD)"
         '("SERVER_NAME = localhost"
           "REMOTE_HOST = foo.com"
           "REQUEST_METHOD = HEAD"
           "CONTENT_TYPE = "
           "QUERY_STRING = a=b&%26%26%24%26=%21%40%21%40")
         (receive (_ body)
           (run-cgi-script->string-list "test.o/cgitest.cgi"
            :environment '((REQUEST_METHOD . HEAD))
            :parameters '((a . b) (&&$& . !@!@)))
           body))

  (with-output-to-file "test.o/cgitest.cgi"
    (lambda ()
      (print "#!/bin/sh")
      (print "echo Content-type: text/plain")
      (print "echo")
      (print "echo \"REQUEST_METHOD = $REQUEST_METHOD\"")
      (print "echo \"CONTENT_TYPE = $CONTENT_TYPE\"")
      (print "echo \"CONTENT_LENGTH = $CONTENT_LENGTH\"")
      (print "echo \"QUERY_STRING = $QUERY_STRING\"")
      (print "cat")))
       
  (test* "run-cgi-script->string-list (using parameters)"
         '("REQUEST_METHOD = POST"
           "CONTENT_TYPE = application/x-www-form-urlencoded"
           "CONTENT_LENGTH = 29"
           "QUERY_STRING = "
           "a=b&%26%26%24%26=%21%40%21%40")
         (receive (_ body)
           (run-cgi-script->string-list "test.o/cgitest.cgi"
            :environment '((REQUEST_METHOD . POST))
            :parameters '((a . b) (&&$& . !@!@)))
           body))

  (sys-system "rm -rf test.o")])

;;------------------------------------------------
(test-section "www.css")
(use www.css)
(test-module 'www.css)

;; NB: this assumes the test is run either under src/ or test/
(define (run-css-parser-test)
  (dolist [infile (glob "../test/data/css-*.css")]
    (test* #"css parser ~infile"
           (call-with-input-file (path-swap-extension infile "sxcss") read)
           (css-parse-file infile)
           equal?)))

(run-css-parser-test)

(test-end)


