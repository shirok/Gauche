;;
;; test www.* modules
;;

(use gauche.test)
(use gauche.parameter)
(use gauche.charconv)
(use text.tree)
(use rfc.822)
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

(test "cgi-parse-parameters" qr1
      (lambda ()
        (set! params (cgi-parse-parameters :query-string qs1))
        params))
(test "cgi-parse-parameters" qr1
      (lambda ()
        (set! params (cgi-parse-parameters :query-string qs1b))
        params))

(test* "cgi-get-parameter" "foo bar"
       (cgi-get-parameter "a" params))
(test* "cgi-get-parameter" '("foo bar" "  ")
       (cgi-get-parameter "a" params :list #t))
(test* "cgi-get-parameter" #t
       (cgi-get-parameter "r" params))
(test* "cgi-get-parameter" '(#t "2")
       (cgi-get-parameter "r" params :list #t))
(test* "cgi-get-parameter" '("baz=doo")
       (cgi-get-parameter "boo" params :list #t))
(test* "cgi-get-parameter" 'none
       (cgi-get-parameter "booz" params :default 'none))
(test* "cgi-get-parameter" #f
       (cgi-get-parameter "booz" params))
(test* "cgi-get-parameter" '()
       (cgi-get-parameter "booz" params :list #t))
(test* "cgi-get-parameter" '(0 2)
       (cgi-get-parameter "r" params :convert x->integer :list #t))

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

(test* "cgi-header" "Content-type: text/html\n\n"
       (tree->string (cgi-header)))

(test* "cgi-header" "Location: http://foo.bar/\n\n"
       (tree->string (cgi-header :location "http://foo.bar/")))

(test* "cgi-header" "Content-type: hoge\nLocation: http://foo.bar/\n\n"
       (tree->string
        (cgi-header :location "http://foo.bar/" :content-type "hoge")))

(test* "cgi-header" "Content-type: text/plain; charset=utf-8\n\n"
       (tree->string
        (cgi-header :content-type "text/plain; charset=utf-8")))

(test* "cgi-header"
       "Content-type: text/html\nSet-cookie: hoge\nSet-cookie: poge\n\n"
       (tree->string
        (cgi-header :cookies '("hoge" "poge"))))

(test* "cgi-header"
       "Content-type: text/html\nSet-cookie: hoge\nSet-cookie: poge\nx-foo: foo\n\n"
       (tree->string
        (cgi-header :x-foo "foo" :cookies '("hoge" "poge"))))

(test* "cgi-main" "Content-type: text/plain\n\na=foo bar"
       (parameterize ((cgi-metavariables `(("REQUEST_METHOD" "GET")
                                           ("QUERY_STRING" ,qs1))))
         (with-output-to-string
           (lambda ()
             (cgi-main
              (lambda (params)
                `(,(cgi-header :content-type "text/plain")
                  "a="
                  ,(cgi-get-parameter "a" params))))))))

;;------------------------------------------------
(test-section "www.cgi-test")
(use www.cgi-test)
(test-module 'www.cgi-test)

(test* "cgi-test-environment-ref" "remote"
       (cgi-test-environment-ref "REMOTE_HOST"))
(test* "cgi-test-environment-ref" "zzz"
       (cgi-test-environment-ref 'ZZZ "zzz"))
(test* "cgi-test-environment-set!" "foo.com"
       (begin
         (set! (cgi-test-environment-ref 'REMOTE_HOST) "foo.com")
         (cgi-test-environment-ref "REMOTE_HOST")))

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
         "QUERY_STRING = a=b&%26%26%24%26=!%40!%40")
       (receive (_ body)
           (run-cgi-script->string-list "test.o/cgitest.cgi"
                                        :parameters '((a . b) (&&$& . !@!@)))
         body))

(test* "run-cgi-script->string-list (using parameters/HEAD)"
       '("SERVER_NAME = localhost"
         "REMOTE_HOST = foo.com"
         "REQUEST_METHOD = HEAD"
         "CONTENT_TYPE = "
         "QUERY_STRING = a=b&%26%26%24%26=!%40!%40")
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
         "CONTENT_LENGTH = 25"
         "QUERY_STRING = "
         "a=b&%26%26%24%26=!%40!%40")
       (receive (_ body)
           (run-cgi-script->string-list "test.o/cgitest.cgi"
                                        :environment '((REQUEST_METHOD . POST))
                                        :parameters '((a . b) (&&$& . !@!@)))
         body))

(sys-system "rm -rf test.o")


(test-end)


