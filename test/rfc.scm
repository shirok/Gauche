;;
;; testing rfc.* modules
;;

(use gauche.test)
(use gauche.sequence)
(use gauche.process)
(use util.match)
(use srfi-19)
(test-start "rfc")

;; rfc.822 test is in ext/rfc

;;--------------------------------------------------------------------
(test-section "rfc.base64")
(use rfc.base64)
(test-module 'rfc.base64)

(test* "encode" "" (base64-encode-string ""))
(test* "encode" "YQ==" (base64-encode-string "a"))
(test* "encode" "MA==" (base64-encode-string "0"))
(test* "encode" "Cg==" (base64-encode-string "\n"))
(test* "encode" "YTA=" (base64-encode-string "a0"))
(test* "encode" "YTAK" (base64-encode-string "a0\n"))
(test* "encode" "PQk0" (base64-encode-string "=\t4"))
(test* "encode" "eTQ5YQ==" (base64-encode-string "y49a"))
(test* "encode" "RWdqYWk=" (base64-encode-string "Egjai"))
(test* "encode" "OTNiamFl" (base64-encode-string "93bjae"))
(test* "encode" "QkFSMGVyOQ==" (base64-encode-string "BAR0er9"))

(test* "encode w/ line width (default)"
       "MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2Nzg5MDEyMzQ1Njc4OTAxMjM0NTY3ODkwMTIzNDU2\n"
       (base64-encode-string "012345678901234567890123456789012345678901234567890123456"))
(test* "encode w/ line width 10, e1"
       "MDEyMzQ1Ng\n=="
       (base64-encode-string "0123456" :line-width 10))
(test* "encode w/ line width 11, e1"
       "MDEyMzQ1Ng=\n="
       (base64-encode-string "0123456" :line-width 11))
(test* "encode w/ line width 12, e1"
       "MDEyMzQ1Ng==\n"
       (base64-encode-string "0123456" :line-width 12))
(test* "encode w/ line width 11, e2"
       "MDEyMzQ1Njc\n="
       (base64-encode-string "01234567" :line-width 11))
(test* "encode w/ line width 12, e2"
       "MDEyMzQ1Njc=\n"
       (base64-encode-string "01234567" :line-width 12))
(test* "encode w/ line width 4"
       "MDEy\nMzQ=\n"
       (base64-encode-string "01234" :line-width 4))
(test* "encode w/ line width 3"
       "MDE\nyMz\nQ="
       (base64-encode-string "01234" :line-width 3))
(test* "encode w/ line width 2"
       "MD\nEy\nMz\nQ=\n"
       (base64-encode-string "01234" :line-width 2))
(test* "encode w/ line width 1"
       "M\nD\nE\ny\nM\nz\nQ\n=\n"
       (base64-encode-string "01234" :line-width 1))
(test* "encode w/ line width 0"
       "MDEyMzQ="
       (base64-encode-string "01234" :line-width 0))

(test* "decode" "" (base64-decode-string ""))
(test* "decode" "a" (base64-decode-string "YQ=="))
(test* "decode" "a" (base64-decode-string "YQ="))
(test* "decode" "a" (base64-decode-string "YQ"))
(test* "decode" "a0" (base64-decode-string "YTA="))
(test* "decode" "a0" (base64-decode-string "YTA"))
(test* "decode" "a0\n" (base64-decode-string "YTAK"))
(test* "decode" "y49a" (base64-decode-string "eTQ5YQ=="))
(test* "decode" "Egjai" (base64-decode-string "RWdqYWk="))
(test* "decode" "93bjae" (base64-decode-string "OTNiamFl"))
(test* "decode" "BAR0er9" (base64-decode-string "QkFSMGVyOQ=="))
(test* "decode" "BAR0er9" (base64-decode-string "QkFS\r\nMGVyOQ\r\n=="))

(test* "standard encode" "YTA+YTA/" (base64-encode-string "a0>a0?"))
(test* "standard decode" "a0>a0?" (base64-decode-string "YTA+YTA/"))
(test* "url-safe encode" "YTA-YTA_" (base64-encode-string "a0>a0?" :url-safe #t))
(test* "url-safe decode" "a0>a0?" (base64-decode-string "YTA-YTA_" :url-safe #t))

;;--------------------------------------------------------------------
(test-section "rfc.quoted-printable")
(use rfc.quoted-printable)
(test-module 'rfc.quoted-printable)

(test* "encode" "abcd=0Cefg"
       (quoted-printable-encode-string "abcd\x0cefg"))
(test* "encode"
       "abcd\r\nefg"
       (quoted-printable-encode-string "abcd\r\nefg"))
(test* "encode (tab/space at eol)"
       "abcd=09\r\nefg=20\r\n"
       (quoted-printable-encode-string "abcd\t\r\nefg \r\n"))
(test* "encode (soft line break)"
       "0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abc=\r\ndefghij0123456789abcdefghij"
       (quoted-printable-encode-string "0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij"))
(test* "encode (soft line break w/line-width)"
       "0123456789abcdefg=\r\nhij0123456789abcd=\r\nefghij"
       (quoted-printable-encode-string
        "0123456789abcdefghij0123456789abcdefghij"
        :line-width 20))
(test* "encode (soft line break w/line-width)"
       "0123456789abcdef=3D=\r\nghij0123456789a=3D=\r\n=3Dbcdefghij"
       (quoted-printable-encode-string
        "0123456789abcdef=ghij0123456789a==bcdefghij"
        :line-width 20))
(test* "encode (soft line break w/line-width lower bound)"
       "a=\r\n=3F=\r\nb"
       (quoted-printable-encode-string "a?b" :line-width 4))
(test* "encode (no line break)"
       "0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij"
       (quoted-printable-encode-string "0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij"
                                       :line-width #f))
(test* "encode (hard line break)"
       "a\r\nb\r\nc\r\n"
       (quoted-printable-encode-string "a\rb\nc\r\n"))
(test* "encode (binary)"
       "a=0Db=0Ac=0D=0A"
       (quoted-printable-encode-string "a\rb\nc\r\n" :binary #t))

(test* "decode" "\x01\x08abcde=\r\n"
       (quoted-printable-decode-string "=01=08abc=64=65=3D\r\n"))
(test* "decode (soft line break)"
       "Now's the time for all folk to come to the aid of their country."
       (quoted-printable-decode-string "Now's the time =\r\nfor all folk to come=   \r\n to the aid of their country."))
(test* "decode (robustness)"
       "foo=1qr =  j\r\n"
       (quoted-printable-decode-string "foo=1qr =  j\r\n="))


;;--------------------------------------------------------------------
(test-section "rfc.cookie")
(use rfc.cookie)
(use srfi-19)
(test-module 'rfc.cookie)

(test* "parse, old" '(("foo" "bar")
                      ("aaa" "bbb" :path "/a/b" :domain "a.b.com")
                      ("x12" "Yy \"yY\" ;; Zz" :port "100, 200, 300")
                      ("zzz" #f)
                      ("_n_" "")
                      ("mmm" "ppp"))
       (parse-cookie-string " foo=bar; aaa = bbb ; $Path=/a/b;$Domain =a.b.com;x12=\"Yy \\\"yY\\\" ;; Zz\"; $Port=\"100, 200, 300\";zzz ;_n_=;mmm=ppp"))

(test* "parse, new" '(("$Version" "1")
                      ("foo" "bar")
                      ("aaa" "bbb" :path "/a/b" :domain "a.b.com")
                      ("x12" "Yy \"yY\" ;; Zz" :port "100, 200, 300")
                      ("zzz" #f)
                      ("_n_" "")
                      ("mmm" "ppp"))
       (parse-cookie-string "$Version=1; foo=bar, aaa = bbb ; $Path=/a/b;$Domain =a.b.com,x12=\"Yy \\\"yY\\\" ;; Zz\"; $Port=\"100, 200, 300\",zzz ,_n_=,mmm=ppp"))

(test* "parse, new" '(("foo" "bar")
                      ("aaa" "bbb" :path "/a/b" :domain "a.b.com")
                      ("x12" "Yy \"yY\" ;; Zz" :port "100, 200, 300")
                      ("zzz" #f)
                      ("_n_" "")
                      ("mmm" "ppp"))
       (parse-cookie-string " foo=bar, aaa = bbb ; $Path=/a/b;$Domain =a.b.com,x12=\"Yy \\\"yY\\\" ;; Zz\"; $Port=\"100, 200, 300\",zzz ,_n_=,mmm=ppp"
                            1))

(define *cookie-spec*
  '(("guest-id" "foo123"
     :domain "foo.com" :path "/abc"
     :expires 1000000000 :max-age 864000
     :discard #t :comment "hogehoge"
     :comment-url "http://foo.com/hogehoge"
     :port "80, 8080" :version 1)
    ("guest-account" "87975348"
     :domain "zzz.com" :path "/zzz"
     :discard #f :secure #t :comment "ZzzZzz, OooOoo"
     :comment-url "http://foo.com/hogehoge")))

(test* "cookie, old"
       '("guest-id=foo123;Domain=foo.com;Path=/abc;Expires=Sun, 09-Sep-2001 01:46:40 GMT"
         "guest-account=87975348;Domain=zzz.com;Path=/zzz;Secure")
       (construct-cookie-string *cookie-spec* 0))

(test* "cookie, new"
       '("guest-id=foo123;Domain=foo.com;Path=/abc;Max-Age=864000;Discard;Comment=hogehoge;CommentURL=\"http://foo.com/hogehoge\";Port=\"80, 8080\";Version=1"
         "guest-account=87975348;Domain=zzz.com;Path=/zzz;Secure;Comment=\"ZzzZzz, OooOoo\";CommentURL=\"http://foo.com/hogehoge\"")
       (construct-cookie-string *cookie-spec* 1))

;; test for formatting srfi-19 time/date
(test* "cookie, old, srfi-19 date"
       '("foo=bar;Expires=Sun, 09-Sep-2001 01:46:40 GMT"
         "foo=baz;Expires=Sun, 09-Sep-2001 01:46:40 GMT")
       (construct-cookie-string
        `(("foo" "bar" :expires ,(make-time time-utc 0 1000000000))
          ("foo" "baz" :expires ,(make-date 0 40 46 1 9 9 2001 0)))
        0))

;;--------------------------------------------------------------------
(test-section "rfc.ftp")
(use rfc.ftp)
(test-module 'rfc.ftp)

;;--------------------------------------------------------------------
(test-section "rfc.icmp")
(use rfc.icmp)
(use gauche.uvector)
(test-module 'rfc.icmp)

;; WRITEME

;;--------------------------------------------------------------------
(test-section "rfc.ip")
(use rfc.ip)
(use gauche.uvector)
(test-module 'rfc.ip)

(test* "ip-version" 4
       (ip-version '#u8(69 0 0 36 139 12 0 0 64 1 241 202
                        127 0 0 1 127 0 0 1 0 0 205 245 50 10 0 0)
                   0))
(test* "ip-version" 6
       (ip-version '#u8(#x60 0 0 0 0 0 17 0
                        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
                        0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
                   0))

(test* "ip-protocol" 1
       (ip-protocol '#u8(69 0 0 36 139 12 0 0 64 1 241 202
                         127 0 0 1 127 0 0 1 0 0 205 245 50 10 0 0)
                    0))
(test* "ip-protocol" 17
       (ip-protocol '#u8(#x60 0 0 0 0 0 17 0
                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1)
                    0))
(test* "ip-protocol" 17
       (ip-protocol '#u8(#x60 0 0 0 0 0 0 0
                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
                         0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1
                         60 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                         43 2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
                         17 0 0 0 0 0 0 0)
                    0))

;;--------------------------------------------------------------------
;; NB: rfc.json test is moved to under ext/peg, since it totally
;; depends on parser.peg.

;;--------------------------------------------------------------------
;; NB: rfc.mime test is in ext/mime

;;--------------------------------------------------------------------
(test-section "rfc.uri")
(use rfc.uri)
(test-module 'rfc.uri)

(test* "encode" "abc%3C%20%3E%20%22%20%23%25%7B%7C%7D%5C%5E"
       (uri-encode-string "abc< > \" #%{|}\\^"))
(test* "encode (noescape)" ".a%21%2Ap"
       (uri-encode-string ".a!*p" :noescape *rfc3986-unreserved-char-set*))
(test* "decode" "abc< > \" #%?{|}\\^"
       (uri-decode-string "abc%3c%20%3E%20%22%20%23%25%3f%7B%7C%7d%5c%5e"))
(test* "decode" "abc<+>+\"+#%?{|}\\^"
       (uri-decode-string "abc%3C+%3e+%22+%23%25%3F%7b%7c%7D%5C%5E"))
(test* "decode" "abc< > \" #%?{|}\\^"
       (uri-decode-string "abc%3c+%3e+%22+%23%25%3f%7b%7c%7d%5c%5e"
                          :cgi-decode #t))
(test* "decode" "%"    (uri-decode-string "%"))
(test* "decode" "a%"   (uri-decode-string "a%"))
(test* "decode" "a%y"  (uri-decode-string "a%y"))
(test* "decode" "a%ay" (uri-decode-string "a%ay"))
(test* "decode" ""     (uri-decode-string ""))

(test* "uri-scheme&specific" '("http" "//practical-scheme.net/gauche/")
       (receive r
           (uri-scheme&specific "http://practical-scheme.net/gauche/")
         r))

(test* "uri-scheme&specific" '(#f "/dev/tty")
       (receive r
           (uri-scheme&specific "/dev/tty")
         r))

(test* "uri-decompose-hierarchical" '("www.example.com:8080"
                                      "/about/company"
                                      "abc=def&ghi%20"
                                      "zzz")
       (receive r
           (uri-decompose-hierarchical
            "//www.example.com:8080/about/company?abc=def&ghi%20#zzz")
         r))

(test* "uri-decompose-hierarchical" '("www.example.com:8080"
                                      "/about/company"
                                      #f
                                      "zzz")
       (receive r
           (uri-decompose-hierarchical
            "//www.example.com:8080/about/company#zzz")
         r))

(test* "uri-decompose-hierarchical" '("www.example.com:8080"
                                      "/"
                                      "abc"
                                      #f)
       (receive r
           (uri-decompose-hierarchical
            "//www.example.com:8080/?abc")
         r))

(test* "uri-decompose-hierarchical" '("www.example.com:8080"
                                      #f
                                      #f
                                      #f)
       (receive r (uri-decompose-hierarchical "//www.example.com:8080") r))

(test* "uri-decompose-hierarchical" '((#f #f  #f #f)
                                      ("" #f  #f #f)
                                      (#f "/" #f #f)
                                      ("" "/" #f #f))
       (map (lambda (specific)
              (receive r (uri-decompose-hierarchical specific) r))
            '("" "//" "/" "///")))

(test* "uri-decompose-authority" '(#f "www.example.com" #f)
       (receive r (uri-decompose-authority "www.example.com") r))
(test* "uri-decompose-authority" '(#f "www.example.com" "8080")
       (receive r (uri-decompose-authority "www.example.com:8080") r))
(test* "uri-decompose-authority" '("foo:bar" "www.example.com" #f)
       (receive r (uri-decompose-authority "foo:bar@www.example.com") r))

(test* "uri-parse" '("https" "shiro" "www.example.com" 443 "/login" "abc" "def")
       (receive r (uri-parse "https://shiro@www.example.com:443/login?abc#def")
         r))
(test* "uri-parse" '("ftp" "anonymous:anonymous" "ftp.example.com" #f
                     "/pub/foo" #f #f)
       (receive r (uri-parse "ftp://anonymous:anonymous@ftp.example.com/pub/foo")
         r))
(test* "uri-parse" '("file" #f #f #f "/usr/local/lib/abc" #f #f)
       (receive r (uri-parse "file:/usr/local/lib/abc")
         r))
(test* "uri-parse" '(#f #f #f #f "/usr/local/lib" #f #f)
       (receive r (uri-parse "/usr/local/lib") r))
(test* "uri-parse" '("mailto" #f #f #f "shiro@example.com" #f #f)
       (receive r (uri-parse "mailto:shiro@example.com") r))

(let ([base0 "http://a/b/c/d;p?q"])
  (define (t base rel expect) 
    (test* (format "merging ~s onto ~s" rel base)
           expect (uri-merge base rel)))
  (define t0 (pa$ t base0))
  ;; examples given in RFC3986 section 5.4
  ;; normal path
  (t0 "g:h" "g:h")
  (t0 "g" "http://a/b/c/g")
  (t0 "./g" "http://a/b/c/g")
  (t0 "g/" "http://a/b/c/g/")
  (t0 "/g" "http://a/g")
  (t0 "//g" "http://g")
  (t0 "?y" "http://a/b/c/d;p?y")
  (t0 "g?y" "http://a/b/c/g?y")
  (t0 "#s" "http://a/b/c/d;p?q#s")
  (t0 "g#s" "http://a/b/c/g#s")
  (t0 "g?y#s" "http://a/b/c/g?y#s")
  (t0 ";x" "http://a/b/c/;x")
  (t0 "g;x" "http://a/b/c/g;x")
  (t0 "g;x?y#s" "http://a/b/c/g;x?y#s")
  (t0 "" "http://a/b/c/d;p?q")
  (t0 "." "http://a/b/c/")
  (t0 "./" "http://a/b/c/")
  (t0 ".." "http://a/b/")
  (t0 "../" "http://a/b/")
  (t0 "../g" "http://a/b/g")
  (t0 "../.." "http://a/")
  (t0 "../../" "http://a/")
  (t0 "../../g" "http://a/g")
  ;; failure path
  (t0 "../../../g" "http://a/g")
  (t0 "../../../../g" "http://a/g")
  (t0 "/./g" "http://a/g")
  (t0 "/../g" "http://a/g")
  (t0 "g." "http://a/b/c/g.")
  (t0 ".g" "http://a/b/c/.g")
  (t0 "g.." "http://a/b/c/g..")
  (t0 "..g" "http://a/b/c/..g")
  (t0 "./../g" "http://a/b/g")
  (t0 "./g/." "http://a/b/c/g/")
  (t0 "g/./h" "http://a/b/c/g/h")
  (t0 "g/../h" "http://a/b/c/h")
  (t0 "g;x=1/./y" "http://a/b/c/g;x=1/y")
  (t0 "g;x=1/../y" "http://a/b/c/y")
  (t0 "g?y/./x" "http://a/b/c/g?y/./x")
  (t0 "g?y/../x" "http://a/b/c/g?y/../x")
  (t0 "g#s/./x" "http://a/b/c/g#s/./x")
  (t0 "g#s/../x" "http://a/b/c/g#s/../x")
  (t0 "http:g" "http:g") ;; for strict parser

  ;; some edge cases.  the first case works since we do pre-normalization
  ;; of the base URI (RFC3986 5.2.1), which is optional.
  (t "http://example.com/foo/.." "./" "http://example.com/")
  (t "http://example.com/" "./foo/bar/.." "http://example.com/foo/")

  ;; empty base-path case
  (t "http://example.com"  "foo" "http://example.com/foo")
  (t "http://example.com"  "./foo" "http://example.com/foo")
  (t "http://example.com"  "../foo" "http://example.com/foo")
  )

;; data uri scheme.  NB: this uses gauche.vport internally (for now),
;; so it depends on ext/vport.

(let ([d0 "data:text/plain;charset=utf-8,%21%40%23%24%25%5E%26%2A%28%29_abc"]
      [d1 "data:application/octed-stream;base64,IUAjJCVeJiooKV9hYmM="])
  (test* "data uri decode->encode (text)" d0
         (receive (ct data) (uri-decompose-data d0)
           (cond-expand
            [gauche.ces.utf8 (uri-compose-data data)]
            [else (uri-compose-data data :content-type ct)])))
  (test* "data uri decode->encode (binary)" d1
         (receive (ct data) (uri-decompose-data d1)
           (uri-compose-data data :content-type ct)))
  )

;; uri-ref
;; test-data : ((input (parts expected) ...) ...)
(let ([data '(("http://user@foo.bar.baz:8080/path/to/resource?query#frag"
               (scheme "http")
               (userinfo "user")
               (host "foo.bar.baz")
               (port 8080)
               (authority "//user@foo.bar.baz:8080/")
               (scheme+authority "http://user@foo.bar.baz:8080/")
               (host+port "foo.bar.baz:8080")
               (userinfo+host+port "user@foo.bar.baz:8080")
               (path "/path/to/resource")
               (path+query "/path/to/resource?query")
               (query "query")
               (path+query+fragment "/path/to/resource?query#frag")
               (fragment "frag")
               ((host+port path+query)
                ("foo.bar.baz:8080" "/path/to/resource?query")))
              ("mailto:foo@example.com?subject=Hello"
               (scheme "mailto")
               (userinfo #f)
               (host #f)
               (port #f)
               (authority "///")
               (scheme+authority "mailto:///")
               (host+port "")
               (userinfo+host+port "")
               (path "foo@example.com")
               (path+query "foo@example.com?subject=Hello")
               (query "subject=Hello")
               (path+query+fragment "foo@example.com?subject=Hello")
               (fragment #f)))])
  (define (test-uri-ref datum)
    (test* #"test uri-ref ~(car datum)" '()
           (let ([input (car datum)]
                 [testers (cdr datum)]
                 [bad '()])
             (dolist [tester testers]
               (let1 r (uri-ref input (car tester))
                 (unless (equal? r (cadr tester))
                   (push! bad `(,(car tester) :expeced ,(cadr tester)
                                :got ,r)))))
             bad)))

  (for-each test-uri-ref data))

;;--------------------------------------------------------------------
(test-section "rfc.http")
(use rfc.http)
(test-module 'rfc.http)

(use rfc.822)
(use rfc.mime)
(use gauche.parameter)

(test* "http-user-agent" #"gauche.http/~(gauche-version)"
       (and (is-a? http-user-agent <parameter>)
            (http-user-agent)))

(test* "http-compose-query" "/search?q=foo%20bar&n=20"
       (http-compose-query "/search" '((q "foo bar") (n 20))))

(define *http-port* 6726)

(define (alist-equal? alis1 alis2)
  (define (%sort alis)
    (sort alis (lambda (a b) (string<? (car a) (car b)))))
  (and (list? alis1)
       (list? alis2)
       (equal? (%sort alis1) (%sort alis2))))

(define *simple-httpd*
  '(
    (use gauche.net)
    (use rfc.822)
    (define *http-port* 6726)

    (define %predefined-contents
      (let1 ht (make-hash-table 'string=?)
        (hash-table-put! ht "/redirect01"
                         `("HTTP/1.x 302 Moved Temporarily\n"
                           ,#"Location: http://localhost:~|*http-port*|/redirect02\n\n"))
        (hash-table-put! ht "/redirect11"
                         '("HTTP/1.x 302 Moved Temporarily\n"
                           "Location: /redirect12\n\n"))
        (hash-table-put! ht "/loop1"
                         '("HTTP/1.x 302 Moved Temporarily\n"
                           "Location: /loop2\n\n"))
        (hash-table-put! ht "/loop2"
                         '("HTTP/1.x 302 Moved Temporarily\n"
                           "Location: /loop1\n\n"))
        (hash-table-put! ht "/chunked"
                         '("HTTP/1.x 200 OK\nTransfer-Encoding: chunked\n\n"
                           "2\r\nOK\n0\r\n\r\n"))
        (hash-table-put! ht "/void"
                         '("HTTP/1.x 404 Not found\nContent-type: text/plain\n"
                           "Content-length: 9\n\nNot found"))
        ht))

    (define (http-server socket)
      (let loop ()
        (let* ([client (socket-accept socket)]
               [in  (socket-input-port client)]
               [out (socket-output-port client)]
               [request-line (read-line in)])
          (rxmatch-if (#/^(\S+) (\S+) HTTP\/1\.1$/ request-line)
              (#f method request-uri)
            (let* ([headers (rfc822-read-headers in)]
                   [bodylen
                    (cond [(assoc-ref headers "content-length")
                           => (^e (string->number (car e)))]
                          [else 0])]
                   [body (read-block bodylen in)])
              (cond
               [(equal? request-uri "/exit")
                (socket-close client)
                (sys-exit 0)]
               [(hash-table-get %predefined-contents request-uri #f)
                => (cut for-each (cut display <> out) <>)]
               [else
                (display "HTTP/1.x 200 OK\nContent-Type: text/plain\n\n" out)
                (write `(("method" ,method)
                         ("request-uri" ,request-uri)
                         ("request-body" ,(string-incomplete->complete body))
                         ,@headers)
                       out)])
              (socket-close client)
              (loop))
            (error "malformed request line:" request-line)))))

    (define (main args)
      (let1 socket (make-server-socket 'inet *http-port* :reuse-addr? #t)
        (print "ready") (flush) ; handshake
        (http-server socket)
        0)))
  )

;; sys-fork causes problems on windows platform (both cygwin and mingw), so
;; we go with run-process.  We assume ./gosh is THE gosh we want to run.
(with-output-to-file "testsrv.o" (lambda () (for-each write *simple-httpd*)))
(let1 p (run-process '("./gosh" "-ftest" "./testsrv.o") :output :pipe)
  (read-line (process-output p)) ; handshake
  )

(let ([expected `(("method" "GET")
                  ("request-uri" "/get")
                  ("request-body" "")
                  ("host" ,#"localhost:~*http-port*")
                  ("user-agent" ,#"gauche.http/~(gauche-version)")
                  ("my-header" "foo"))]
      [host #"localhost:~*http-port*"])
  (define (req-body . args)
    (values-ref (apply http-request args) 2))

  (test* "http-get, default string receiver" expected
         (receive (code headers body)
             (http-request 'GET host "/get" :my-header "foo")
           (and (equal? code "200")
                (equal? headers '(("content-type" "text/plain")))
                (read-from-string body)))
         alist-equal?)

  (test* "http-get, custom receiver" expected
         (req-body 'GET host "/get"
                   :receiver (lambda (code hdrs total retr)
                               (let loop ((result '()))
                                 (receive (port size) (retr)
                                   (if (and size (= size 0))
                                     result
                                     (loop (append result (read port)))))))
                   :my-header "foo")
         alist-equal?)

  (sys-unlink "test.o")
  (test* "http-get, file receiver" expected
         (let1 f (req-body 'GET host "/get"
                           :receiver (http-file-receiver "test.o")
                           :my-header :foo)
           (and (equal? f "test.o")
                (with-input-from-file "test.o" read)))
         alist-equal?)
  (sys-unlink "test.o")

  (test* "http-get, file receiver (tmp)" expected
         (let1 f (req-body 'GET host "/get"
                           :receiver (http-file-receiver "test.o"
                                                         :temporary #t)
                           :my-header :foo)
           (and (not (equal? f "test.o"))
                (begin0 (with-input-from-file f read)
                  (sys-unlink f))))
         alist-equal?)

  (let ()
    (define cond-receiver
      (http-cond-receiver
       ["404" (lambda (code hdrs total retr)
                (let1 sink (open-output-file
                            (cond-expand
                             [gauche.os.windows "NUL"]
                             [else "/dev/null"]))
                  (let loop ()
                    (receive (port size) (retr)
                      (cond
                       [(and size (= size 0)) (close-output-port sink) 404]
                       [else (copy-port port sink :size size) (loop)])))))]            ["200" (lambda (code hdrs total retr)
                (let loop ((result '()))
                  (receive (port size) (retr)
                    (if (and size (= size 0))
                      result
                      (loop (append result (read port)))))))]
       ))

    (test* "http-get, cond-receiver" expected
           (req-body 'GET host "/get"
                     :receiver cond-receiver
                     :my-header :foo))

    (test* "http-get, cond-receiver" 404
           (req-body 'GET host "/void"
                     :receiver cond-receiver
                     :my-header :foo)))
  )

(test* "http-get (redirect)" "/redirect02"
       (receive (code headers body)
           (http-request 'GET #"localhost:~*http-port*" "/redirect01")
         (cond ((assoc-ref (read-from-string body) "request-uri")
                => car))))

(test* "http-get (redirect)" "/redirect12"
       (receive (code headers body)
           (http-request 'GET #"localhost:~*http-port*" "/redirect11")
         (cond ((assoc-ref (read-from-string body) "request-uri")
                => car))))

(test* "http-get (no redirect)" "/redirect12"
       (receive (code headers body)
           (http-request 'GET #"localhost:~*http-port*" "/redirect11"
                         :redirect-handler #f)
         (rfc822-header-ref headers "location")))

(test* "http-get (custom redirect)" "/foofoo"
       (receive (code headers body)
           (http-request 'GET #"localhost:~*http-port*" "/redirect11"
                         :redirect-handler (^[meth code hdrs body]
                                             `(GET . "/foofoo")))
         (cond ((assoc-ref (read-from-string body) "request-uri")
                => car))))

(test* "http-get (custom redirect to HEAD)" #f
       (receive (code headers body)
           (http-request 'GET #"localhost:~*http-port*" "/redirect11"
                         :redirect-handler (^[meth code hdrs body]
                                             `(HEAD . "/foofoo")))
         body))

(test* "http-get (loop)" (test-error <http-error>)
       (http-request 'GET #"localhost:~*http-port*" "/loop1"))

(test* "http-get (chunked body)" "OK"
       (receive (code headers body)
           (http-request 'GET #"localhost:~*http-port*" "/chunked")
         body))

(test* "http-head" #t
       (receive (code headers body)
           (http-request 'HEAD #"localhost:~*http-port*" "/")
         (and (equal? code "200")
              (equal? headers '(("content-type" "text/plain")))
              (not body))))

(let1 expected `(("method" "POST")
                 ("request-uri" "/post")
                 ("content-length" "4")
                 ("host" ,#"localhost:~*http-port*")
                 ("user-agent" ,#"gauche.http/~(gauche-version)")
                 ("request-body" "data"))
  (define (tester msg thunk)
    (test* #"http-post ~msg" expected
           (receive (code headers body) (thunk)
             (and (equal? code "200")
                  (equal? headers '(("content-type" "text/plain")))
                  (read-from-string body)))
           alist-equal?))

  (tester "(new API)"
          (lambda ()
            (http-request 'POST #"localhost:~*http-port*" "/post"
                          :sender (http-string-sender "data"))))

  (tester "(old API)"
          (lambda ()
            (http-post #"localhost:~*http-port*" "/post" "data")))
  )

(let1 expected '(("a" "b") ("c" "d"))
  (define (tester msg thunk)
    (test* #"http-post (multipart/form-data) ~msg" expected
           (receive (code headers body) (thunk)
             
             (and-let* ([ (equal? code "200") ]
                        [ (equal? headers '(("content-type" "text/plain"))) ]
                        [r (read-from-string body)]
                        [body (assoc "request-body" r)]
                        [part (call-with-input-string (cadr body)
                                (cut mime-parse-message <> r mime-body->string))]
                        [ (is-a? part <mime-part>) ]
                        [ (list? (ref part'content)) ])
               (map (lambda (p)
                      (match (mime-parse-content-disposition
                              (rfc822-header-ref (ref p'headers)
                                                 "content-disposition"))
                        [("form-data" ("name" . name))
                         (list name (ref p'content))]
                        [else
                         (list (ref p'headers) (ref p'content))]))
                    (ref part'content))))))

  (tester "(new API)"
          (lambda ()
            (http-request 'POST #"localhost:~*http-port*" "/post"
                          :sender
                          (http-multipart-sender '(("a" "b") ("c" "d"))))))
  (tester "(old API)"
          (lambda ()
            (http-post #"localhost:~*http-port*" "/post"
                       '(("a" "b") ("c" "d")))))
  )

(test* "<http-error>" #t
       (guard (e (else (is-a? e <http-error>)))
         (http-request 'GET #"localhost:~*http-port*" "/exit")))

(sys-waitpid -1)

(test-end)
