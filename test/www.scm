;;
;; test www.* modules
;;

(use gauche.test)
(use gauche.parameter)
(use text.tree)
(test-start "www.* modules")

;; need to pre-load charconv if test without installation
(when (file-exists? "../ext/syslog/syslog.scm")
  (eval
   '(begin (add-load-path "../ext/charconv")
           (load "../ext/charconv/charconv"))
   (interaction-environment)))

;;------------------------------------------------
(test-section "www.cgi")
(use www.cgi)
(test-module 'www.cgi)

(define params #f)
(define qs1 "a=foo+bar&boo=baz=doo&z%3Dz=%21%26&a=+%20&#=#&z=z=8&r&r=2")
(define qr1 '(("boo" "baz=doo") ("z=z" "!&") ("a" "foo bar" "  ") ("#" "#") ("z" "z=8") ("r" #t "2")))

(define qs2 "zz=aa&aa=zz")
(define qr2 '(("zz" "aa") ("aa" "zz")))

(test "cgi-parse-parameters" qr1
      (lambda ()
        (set! params (cgi-parse-parameters :query-string qs1))
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

(test-end)


