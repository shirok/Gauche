;;
;; testing precompiled rfc.* modules
;;

(use gauche.test)
(use util.match)
(use srfi-19)
(use gauche.sequence)

(test-start "precompiled rfc modules")

;;--------------------------------------------------------------------
(test-section "rfc.822")
(use rfc.822)
(test-module 'rfc.822)

(define rfc822-header1
  "Received: by foo.bar.com id ZZZ55555; Thu, 31 May 2001 16:38:04 -1000 (HST)
Received: from ooo.ooo.com (ooo.ooo.com [1.2.3.4])
\tby foo.bar.com (9.9.9+3.2W/3.7W-) with ESMTP id ZZZ55555
\tfor <yoo@bar.com>; Thu, 31 May 2001 16:38:02 -1000 (HST)
Received: from zzz ([1.2.3.5]) by ooo.ooo.com  with Maccrosoft SMTPSVC(5.5.1877.197.19);
\t Thu, 31 May 2001 22:33:16 -0400
Message-ID: <beefbeefbeefbeef@ooo.ooo.com>
Subject: Bogus Tester
From: Bogus Sender <bogus@ooo.com>
To: You <you@bar.com>, Another <another@ooo.com>
Date: Fri, 01 Jun 2001 02:37:31 (GMT)
Mime-Version: 1.0
Content-Type: text/html
Content-Transfer-Encoding: quoted-printable
X-MSMail-Priority: Normal
X-mailer: FooMail 4.0 4.03 (SMT460B92F)
Content-Length: 4349

")

(define rfc822-header1-list
  '(("received" "by foo.bar.com id ZZZ55555; Thu, 31 May 2001 16:38:04 -1000 (HST)")
    ("received" "from ooo.ooo.com (ooo.ooo.com [1.2.3.4])\tby foo.bar.com (9.9.9+3.2W/3.7W-) with ESMTP id ZZZ55555\tfor <yoo@bar.com>; Thu, 31 May 2001 16:38:02 -1000 (HST)")
    ("received" "from zzz ([1.2.3.5]) by ooo.ooo.com  with Maccrosoft SMTPSVC(5.5.1877.197.19);\t Thu, 31 May 2001 22:33:16 -0400")
    ("message-id" "<beefbeefbeefbeef@ooo.ooo.com>")
    ("subject" "Bogus Tester")
    ("from" "Bogus Sender <bogus@ooo.com>")
    ("to" "You <you@bar.com>, Another <another@ooo.com>")
    ("date" "Fri, 01 Jun 2001 02:37:31 (GMT)")
    ("mime-version" "1.0")
    ("content-type" "text/html")
    ("content-transfer-encoding" "quoted-printable")
    ("x-msmail-priority" "Normal")
    ("x-mailer" "FooMail 4.0 4.03 (SMT460B92F)")
    ("content-length" "4349")
    ))

(test* "rfc822-read-headers" #t
       (equal? rfc822-header1-list
               (rfc822-read-headers (open-input-string rfc822-header1))))

;; token parsers
(test* "rfc822-field->tokens (basic)"
       '(("aa") ("bb") ("cc") ("dd") ("ee") (" a\"aa\\aa (a)"))
       (map rfc822-field->tokens
            '("aa"
              "  bb   "
              " (comment) cc(comment)"
              " (co\\mm$$*##&$%ent) dd(com (me) nt)"
              "\"ee\""
              "  \" a\\\"aa\\\\aa (a)\" (comment\\))")))

(test* "rfc822-field->tokens"
       '("from" "aaaaa.aaa.org" "by" "ggg.gggg.net" "with" "ESMTP" "id" "24D50175C8")
       (rfc822-field->tokens
        "from aaaaa.aaa.org (aaaaa.aaa.org [192.168.0.9]) by ggg.gggg.net (Postfix) with ESMTP id 24D50175C8"))


(test* "rfc822-parse-date" '(2003 3 4 12 34 56 -3600 2)
       (receive r (rfc822-parse-date "Tue,  4 Mar 2003 12:34:56 -3600") r))

(test* "rfc822-parse-date" '(2003 3 4 12 34 56 0 2)
       (receive r (rfc822-parse-date "Tue,  4 Mar 2003 12:34:56 UT") r))

(test* "rfc822-parse-date (no weekday)" '(2003 3 4 12 34 56 -3600 #f)
       (receive r (rfc822-parse-date "4 Mar 2003 12:34:56 -3600") r))

(test* "rfc822-parse-date (no timezone)" '(2003 3 4 12 34 56 #f #f)
       (receive r (rfc822-parse-date "4 Mar 2003 12:34:56") r))

(test* "rfc822-parse-date (old tz)" '(2003 3 4 12 34 56 #f #f)
       (receive r (rfc822-parse-date "4 Mar 2003 12:34:56 jst") r))

(test* "rfc822-parse-date (no seconds)" '(2003 3 4 12 34 #f 900 #f)
       (receive r (rfc822-parse-date "4 Mar 2003 12:34 +0900") r))

(test* "rfc822-parse-date (no seconds)" '(2003 3 4 12 34 #f 900 2)
       (receive r (rfc822-parse-date "Tue, 04 Mar 2003 12:34 +0900") r))

(test* "rfc822-parse-date (2digit year)" '(2003 3 4 12 34 56 -3600 2)
       (receive r (rfc822-parse-date "Tue,  4 Mar 03 12:34:56 -3600") r))

(test* "rfc822-parse-date (2digit year)" '(1987 3 4 12 34 56 -3600 2)
       (receive r (rfc822-parse-date "Tue,  4 Mar 87 12:34:56 -3600") r))

(test* "rfc822-parse-date (Weekday, exhausive)" '(0 1 2 3 4 5 6 #f)
       (map-with-index
        (lambda (ind wday)
          (receive (y m d H M S tz wd)
              (rfc822-parse-date
               #"~|wday|, ~(+ 2 ind) Jan 2000 00:00:00 +0000")
            wd))
        '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Znn")))

(test* "rfc822-parse-date (Months, exhausive)"
       '(1 2 3 4 5 6 7 8 9 10 11 12 #f)
       (map (lambda (mon)
              (receive (y m d H M S tz wd)
                  (rfc822-parse-date
                   #"1 ~mon 1999 00:00:00 +0000")
                m))
            '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug"
              "Sep" "Oct" "Nov" "Dec" "Zzz")))

(test* "rfc822-parse-date (invalid)" '(#f #f #f #f #f #f #f #f)
       (receive r (rfc822-parse-date "Sun 2 Mar 2002") r))

(test* "date->rfc822-date" "Sun, 29 Nov 2009 01:23:45 +0000"
       (date->rfc822-date (make-date 0 45 23 01 29 11 2009 0)))
(test* "date->rfc822-date" "Sun, 29 Nov 2009 01:23:45 +0900"
       (date->rfc822-date (make-date 0 45 23 01 29 11 2009 32400)))
(test* "date->rfc822-date" "Sun, 29 Nov 2009 01:23:45 -0830"
       (date->rfc822-date (make-date 0 45 23 01 29 11 2009 -30600)))
(test* "date->rfc822-date" "Sun, 29 Nov 2009 01:23:45 +0030"
       (date->rfc822-date (make-date 0 45 23 01 29 11 2009 1800)))


(test* "rfc822-invalid-header-field" #f
       (rfc822-invalid-header-field "abcde"))
(test* "rfc822-invalid-header-field" 'incomplete-string
       (rfc822-invalid-header-field #*"abcde"))
;; unicode literal doesn't work with none encoding
(unless (eq? (gauche-character-encoding) 'none)
  (test* "rfc822-invalid-header-field" 'bad-character
         (rfc822-invalid-header-field "abc\u3030 def"))
  )
(test* "rfc822-invalid-header-field" 'bad-character
       (rfc822-invalid-header-field "abc\x00 def"))
(test* "rfc822-invalid-header-field" 'line-too-long
       (rfc822-invalid-header-field (make-string 1000 #\a)))
(test* "rfc822-invalid-header-field" 'line-too-long
       (rfc822-invalid-header-field
        (string-append (string-join (make-list 5 (make-string 78 #\a))
                                    "\r\n ")
                       (make-string 1000 #\a))))
(test* "rfc822-invalid-header-field" 'stray-crlf
       (rfc822-invalid-header-field
        (string-join (make-list 5 (make-string 78 #\a)) "\r\n")))
(test* "rfc822-invalid-header-field" 'stray-crlf
       (rfc822-invalid-header-field "abc\ndef"))
(test* "rfc822-invalid-header-field" 'stray-crlf
       (rfc822-invalid-header-field "abc\rdef"))

(test* "rfc822-write-headers"
       "name: Shiro Kawai\r\n\
        address: 1234 Lambda St.\r\n \
        Higher Order Functions, HI, 99899\r\n\
        registration-date: 2007-12-10\r\n\r\n"
       (with-output-to-string
         (cut rfc822-write-headers
              '(("name" "Shiro Kawai")
                ("address" "1234 Lambda St.\r\n Higher Order Functions, HI, 99899")
                ("registration-date" "2007-12-10")))))

(test* "rfc822-write-headers (ignore error)"
       (make-list 2 "name: Shiro\x00Kawai\r\n\r\n")
       (map (lambda (x)
              (with-output-to-string
                (cut rfc822-write-headers '(("name" "Shiro\x00Kawai"))
                     :check x)))
            '(#f :ignore)))

(test* "rfc822-write-headers (continue)"
       "x: A\r\nx: B\r\nx: C\r\n\r\n"
       (call-with-output-string
         (lambda (p)
           (rfc822-write-headers '(("x" "A") ("x" "B")) :output p :continue #t)
           (rfc822-write-headers '(("x" "C")) :output p))))

(let-syntax ([test-reason
              (syntax-rules ()
                [(_ expect hdrs)
                 (begin
                   (test* (format "rfc822-write-headers error (~a)" expect)
                          expect
                          (guard (e (else (rxmatch-case (ref e'message)
                                            [#/\(([\w-]+)\)/ (_ m) m]
                                            [else e])))
                            (with-output-to-string
                              (cut rfc822-write-headers hdrs))))
                   (test* (format "rfc822-write-headers handle (~a)" expect)
                          (format "x-name: ~a\r\n\r\n" expect)
                          (with-output-to-string
                            (cut rfc822-write-headers hdrs
                                 :check (lambda (name body reason)
                                          (values #"x-~name"
                                                  (x->string reason))))))
                   )])])

  (test-reason "bad-character" '(("name" "Shiro\x00Kawai")))
  (test-reason "incomplete-string" '(("name" #*"Shiro Kawai")))
  (test-reason "stray-crlf" '(("name" "Shiro\nKawai")))
  (test-reason "line-too-long" `(("name" ,(make-string 1000 #\a))))
  )
        
;;--------------------------------------------------------------------
(test-section "rfc.mime")
(use rfc.mime)
(test-module 'rfc.mime)
(use rfc.mime-port)
(test-module 'rfc.mime-port)

(test* "mime-parse-version" '((1 0) (1 0) (1 0) (1 0) #f)
       (map mime-parse-version
            '(" 1.0"
              " 1.0 (produced by MetaSend Vx.x) "
              " (produced by MetaSend Vx.x) 1.0"
              " 1.(produced by MetaSend Vx.x (beta))0"
              " none ")))

(test* "mime-parse-content-type" '("text" "plain")
       (mime-parse-content-type " text/plain (client: foo bar)"))
(test* "mime-parse-content-type" '("text" "plain" ("charset" . "us-ascii"))
       (mime-parse-content-type " text/plain ;charset=\"us-ascii\""))
(test* "mime-parse-content-type" '("text" "plain" ("charset" . "us-ascii"))
       (mime-parse-content-type " text/plain; charset=us-ascii (Plain Text)"))
(test* "mime-parse-content-type" '("text" "plain" ("charset" . "iso-2022-jp"))
       (mime-parse-content-type " text/(Plain Text)plain ; (Japanese) charset=iso-2022-jp"))

(test* "mime-parse-content-type"
       '("text" "plain" ("zzz" . "yyy") ("xxx" . "www"))
       (mime-parse-content-type " text/plain ;zzz=\"yyy\"; xxx = www (AAA)"))
(test* "mime-parse-content-type"
       '("multipart" "alternative"
         ("boundary" . "=_alternative 006EBAA488256DF0_="))
       (mime-parse-content-type
        "multipart/alternative; boundary=\"=_alternative 006EBAA488256DF0_=\"")
       )

#| ; TODO: enable these after rfc2231 support
(test* "mime-parse-content-type (rfc2231 encode)"
       '("application" "x-stuff"
         ("x" . "y")
         ("title" . "This is ***fun***")
         ("url" . "http://www.example.com/"))
       (mime-parse-content-type
        "application/x-stuff; x=y; title*=us-ascii'en-us'This%20is%20%2A%2A%2Afun%2A%2A%2A; url=\"http://www.example.com/\""))

(test* "mime-parse-content-type (rfc2231 concatenation)"
       '("message" "external-body"
         ("access-type" . "URL")
         ("url" . "ftp://cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar"))
       (mime-parse-content-type
        "Content-Type: message/external-body; access-type=URL; \
         URL*0=\"ftp://\"; \
         URL*1=\"cs.utk.edu/pub/moore/bulk-mailer/bulk-mailer.tar\""))

(test* "mime-parse-content-type (rfc2231 concatenation and encode)"
       '("application" "x-stuff"
         ("title" . "This is even more ***fun*** isn't it!")
         ("a" . "b"))
       (mime-parse-content-type
        "Content-Type: application/x-stuff; \
           title*0*=us-ascii'en'This%20is%20even%20more%20 \
           title*1*=%2A%2A%2Afun%2A%2A%2A%20 \
           title*2=\"isn't it!\"; \
           a=b"))
|#

(test* "mime-compose-parameters (simple)"
       "; ab=cd; ef=gh"
       (mime-compose-parameters '((ab . cd) (ef . gh)) #f))
(test* "mime-compose-parameters (quote)"
       "; ab=\"c d\"; ef=\"\\\"\\\\\""
       (mime-compose-parameters '((ab . "c d") (ef . "\"\\")) #f))
(test* "mime-compose-parameters (long)"
       "; ab=cd;\r\n foo=012345678901234567890123456789012345679012345678901234567890123456789"
       (mime-compose-parameters
        '((ab . cd) (foo . "012345678901234567890123456789012345679012345678901234567890123456789"))
        #f))

(test* "mime-encode-text (pass-through)" "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod\r\n tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim\r\n veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea\r\n commodo consequat. Duis aute irure dolor in reprehenderit in voluptate\r\n velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat\r\n cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id\r\n est laborum."
       (mime-encode-text "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."))
(test* "mime-encode-text (pass-through, nonbreak)" "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
       (mime-encode-text "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum."
                         :line-width #f))
(test* "mime-encode-text (pass-through, forced line break)" "Loremipsumdolorsitamet,consecteturadipisicingelit,seddoeiusmodtemporincididu\r\n ntutlaboreetdoloremagnaaliqua."
       (mime-encode-text "Loremipsumdolorsitamet,consecteturadipisicingelit,seddoeiusmodtemporincididuntutlaboreetdoloremagnaaliqua."))

;; unicode literal doesn't work with none encoding
(unless (eq? (gauche-character-encoding) 'none)
  (test* "mime-encode-text" "=?utf-8?B?zrvjga7lroflrpnjgbjjgojjgYbjgZPjgZ0=?=\r\n =?utf-8?B?44CCV2VsY29tZSB0byDOuy1zcGFjZQ==?="
         (mime-encode-text "\u03bb\u306e\u5b87\u5b99\u3078\u3088\u3046\u3053\u305d\u3002\u0057\u0065\u006c\u0063\u006f\u006d\u0065\u0020\u0074\u006f\u0020\u03bb\u002d\u0073\u0070\u0061\u0063\u0065" :line-width 50))
  (test* "mime-encode-text (nobreak)" "=?utf-8?B?zrvjga7lroflrpnjgbjjgojjgYbjgZPjgZ3jgIJXZWxjb21lIHRvIM67LXNwYWNl?="
         (mime-encode-text "\u03bb\u306e\u5b87\u5b99\u3078\u3088\u3046\u3053\u305d\u3002\u0057\u0065\u006c\u0063\u006f\u006d\u0065\u0020\u0074\u006f\u0020\u03bb\u002d\u0073\u0070\u0061\u0063\u0065" :line-width #f))
  )

(use gauche.charconv)

(when (ces-conversion-supported? "iso-8859-1" #f)
  (test* "mime-decode-word" "this is some text"
         (mime-decode-word "=?iso-8859-1?q?this=20is=20some=20text?="))
  (test* "mime-decode-text" "this is some text"
         (mime-decode-text
          "=?iso-8859-1?q?this=20is?= =?iso-8859-1?q?some=20text?="))
  (test* "mime-decode-text" "this is some text"
         (mime-decode-text
          "=?iso-8859-1?q?this=20is?= some=?iso-8859-1?q?=20text?="))

  (test* "mime-encode-word" "=?iso-8859-1?Q?this=20is=20some=20text?="
         (mime-encode-word "this is some text" :charset 'iso-8859-1
                           :transfer-encoding 'quoted-printable))
  (test* "mime-encode-text" "=?iso-8859-1?B?VGhlIHF1aWNr?=\r\n =?iso-8859-1?B?IGJyb3duIGZv?=\r\n =?iso-8859-1?B?eCBqdW1wcyBv?=\r\n =?iso-8859-1?B?dmVyIHRoZSBs?=\r\n =?iso-8859-1?B?YXp5IGRvZw==?="
         (mime-encode-text "The quick brown fox jumps over the lazy dog"
                           :charset 'iso-8859-1 :force #t
                           :line-width 30))
  (test* "mime-encode-text" "\r\n =?iso-8859-1?B?VGhlIHF1aWNrIGJyb3du?=\r\n =?iso-8859-1?B?IGZveCBqdW1wcyBvdmVy?=\r\n =?iso-8859-1?B?IHRoZSBsYXp5IGRvZw==?="
         (mime-encode-text "The quick brown fox jumps over the lazy dog"
                           :charset 'iso-8859-1 :force #t
                           :line-width 40
                           :start-column 20))
  )

(when (ces-conversion-supported? "us-ascii" #f)
  (test* "mime-decode-word" "Keith_Moore"
         (mime-decode-word "=?US-ASCII?Q?Keith_Moore?="))
  (test* "mime-decode-word" "Keith_Moore"
         (mime-decode-word "=?US-ASCII?B?S2VpdGhfTW9vcmU=?="))
  (test* "mime-decode-text" "Keith/Moore"
         (mime-decode-text "=?US-ASCII?B?S2VpdGg=?=/=?US-ASCII?Q?Moore?="))

  (test* "mime-encode-text" "=?us-ascii?B?VGhlIHF1aWNr?=\r\n =?us-ascii?B?IGJyb3duIGZv?=\r\n =?us-ascii?B?eCBqdW1wcyBv?=\r\n =?us-ascii?B?dmVyIHRoZSBs?=\r\n =?us-ascii?B?YXp5IGRvZw==?="
         (mime-encode-text "The quick brown fox jumps over the lazy dog"
                           :charset 'us-ascii :force #t
                           :line-width 30))
  )

(when (and (memq (gauche-character-encoding) '(euc-jp sjis utf-8))
           (ces-conversion-supported? "iso-2022-jp" #f))
  (test* "mime-decode-word" "\u5ddd\u5408 \u53f2\u6717"
         (mime-decode-word "=?ISO-2022-JP?B?GyRCQG45ZxsoQiAbJEI7S08vGyhC?="))
  (test* "mime-decode-text" "(\u5ddd\u5408 \u53f2\u6717)"
         (mime-decode-text "(=?iso-2022-jp?b?GyRCQG45ZxsoQg==?= =?iso-2022-jp?b?GyRCO0tPLxsoQg==?=)"))
  (test* "mime-encode-word" "=?iso-2022-jp?B?GyRCQG45ZxsoQiAbJEI7S08vGyhC?="
         (mime-encode-word "\u5ddd\u5408 \u53f2\u6717"
                           :charset 'iso-2022-jp))
  )

;; this tests whether illegal input sequence is handled gracefully
(when (memq (gauche-character-encoding) '(euc-jp sjis utf-8))
  (test* "mime-decode-word" "=?ISO-2022-JP?B?GyRCJDkbKBsoQg==?="
         (mime-decode-word "=?ISO-2022-JP?B?GyRCJDkbKBsoQg==?="))
  (test* "mime-decode-text" "(=?ISO-2022-JP?B?GyRCJDkbKBsoQg==?=)"
         (mime-decode-text "(=?ISO-2022-JP?B?GyRCJDkbKBsoQg==?=)"))
  )

;; NB: this assumes the test is run in-place
(define (mime-message-tester num headers)
  (let ((src #"../../test/data/rfc-mime-~|num|.txt")
        (res (call-with-input-file #"../../test/data/rfc-mime-~|num|.res.txt"
               read)))
    (call-with-input-file src
      (lambda (inp)
        (let* ((title (read-line inp)) ;; test title
               (expl  (read-line inp)) ;; explanation (ignored)
               (headers (or headers (rfc822-read-headers inp))))
          (test* #"mime-parse-message (~|num| - ~|title|)"
                 res
                 (and (equal? (mime-parse-version
                               (rfc822-header-ref headers "mime-version"))
                              '(1 0))
                      (mime-message-resolver
                       (mime-parse-message inp headers
                                           (cut mime-body->string <> <>))
                       #f)
                      )))))
    ))

(define (mime-message-resolver mesg parent)
  (unless (eqv? (ref mesg 'parent) parent) (error "parent link broken"))
  (list* (string-append (ref mesg 'type) "/" (ref mesg 'subtype))
         (ref mesg 'index)
         (if (string? (ref mesg 'content))
           (list (ref mesg 'content))
           (map (cut mime-message-resolver <> mesg) (ref mesg 'content)))))

(dotimes (n 8)
  (mime-message-tester
   n
   (and (= n 6)
        '(("mime-version" " 1.0")
          ("content-type" "multipart/form-data; boundary=\"---------------------------6578815652962098482130719379\"")))))

(let1 b (mime-make-boundary)
  (test* "mime-compose-message (simple)"
         (string-append "\r\n--"b"\r\n"
                        "Content-type: text/plain\r\n"
                        "Content-transfer-encoding: 7bit\r\n\r\n"
                        "This is a pen."
                        "\r\n--"b"\r\n"
                        "Content-type: text/html; charset=us-ascii\r\n\r\n"
                        "<html><head></head><body></body></html>"
                        "\r\n--"b"\r\n"
                        "Content-type: application/octet-stream\r\n"
                        "Content-transfer-encoding: base64\r\n\r\n"
                        "YWJjZGVmZw=="
                        "\r\n--"b"--\r\n")
         (receive (s bb)
           (mime-compose-message-string
            '((("text" "plain")
               (("content-transfer-encoding" "7bit"))
               "This is a pen.")
              (("text" "html" ("charset" . "us-ascii"))
               ()
               "<html><head></head><body></body></html>")
              (("application" "octet-stream")
               (("content-transfer-encoding" "base64"))
               "abcdefg"))
            :boundary b)
           (and (equal? b bb) s))))

(define (mime-roundtrip-tester num)
  (define (gen-parts mesg)
    (match mesg
      [(ctype _ (? string? body))
       `(,(mime-parse-content-type ctype) '() ,body)]
      [("message/rfc822" _ ("text/plain" _ body))
       `(("message" "rfc822") '()
         ,#"content-type: text/plain\r\n\r\n~body")]
      [(ctype _ children ...)
       `(,(mime-parse-content-type ctype) '()
         (subparts ,@(map gen-parts children)))]))
  (let1 src (call-with-input-file #"../../test/data/rfc-mime-~|num|.res.txt" read)
    (receive (composed boundary)
        (mime-compose-message-string (list (gen-parts src)))
      (test* #"mime-roundtrip (~num)"
             `("multipart/mixed" 0 ,src)
             (mime-message-resolver
              (call-with-input-string composed
                (cut mime-parse-message <>
                     `(("mime-version" "1.0")
                       ("content-type" ,#"multipart/mixed; boundary=\"~|boundary|\""))
                     (cut mime-body->string <> <>)))
              #f)))))
                     
(dotimes (n 8) (mime-roundtrip-tester n))
    
(test-end)
