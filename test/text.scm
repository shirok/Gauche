;;
;; testing text.* module
;;

(use gauche.test)
(test-start "text utilities")

;;-------------------------------------------------------------------
(test-section "csv")
(use text.csv)
(test-module 'text.csv)

(test* "csv-reader" '("abc" "def" "" "ghi")
       (call-with-input-string "abc  ,  def  ,, ghi  "
         (make-csv-reader #\,)))

(test* "csv-reader" '("abc" "def" "" ", ghi")
       (call-with-input-string "abc  :  def  :: , ghi  "
         (make-csv-reader #\:)))

(test* "csv-reader" '("abc" "def" "ghi")
       (call-with-input-string "abc  ,  \"def\"  , \"ghi\"  "
         (make-csv-reader #\,)))

(test* "csv-reader" '("abc" " de,f " "gh\ni" "jkl")
       (call-with-input-string "   abc,  \" de,f \"  , \"gh\ni\", \"jkl\""
         (make-csv-reader #\,)))

(test* "csv-reader" '("ab\nc" "de \n\n \nf " "" "" "gh\"\n\"i")
       (call-with-input-string "   \"ab\nc\" ,  \"de \n\n \nf \"  ,  , \"\" , \"gh\"\"\n\"\"i\""
         (make-csv-reader #\,)))

(test* "csv-reader" '(("" "") ("a" "") ("" "b"))
       (let1 r (make-csv-reader #\,)
         (call-with-input-string ",\na,  \n  ,b"
           (^p (let* ([a (r p)] [b (r p)] [c (r p)] [d (r p)])
                 (and (eof-object? d)
                      (list a b c)))))))

(test* "csv-reader" (test-error)
       (call-with-input-string " abc,  def , \"ghi\"\"\n\n"
         (make-csv-reader #\,)))

(test* "csv-reader" #t
       (eof-object?
        (call-with-input-string "" (make-csv-reader #\,))))

(test* "csv-writer"
       "abc,def,123,\"what's up?\",\"he said, \"\"nothing new.\"\"\"\n"
       (call-with-output-string
         (lambda (out)
           ((make-csv-writer #\,)
            out
            '("abc" "def" "123" "what's up?" "he said, \"nothing new.\""))))
       )

(test* "csv-writer"
       "abc,def,123,\"what's up?\",\"he said, \"\"nothing new.\"\"\"\r\n"
       (call-with-output-string
         (lambda (out)
           ((make-csv-writer #\, "\r\n")
            out
            '("abc" "def" "123" "what's up?" "he said, \"nothing new.\""))))
       )

(test* "csv-writer" "\n"
       (call-with-output-string
         (lambda (out)
           ((make-csv-writer #\,) out '()))))

;;-------------------------------------------------------------------
(test-section "diff")
(use text.diff)
(test-module 'text.diff)

(define diff-a "foo
bar
bar
baz
baz
hoge
")
(define diff-b "foo
bar
baz
fuga
hoge
fuga
")

(test* "diff-report"
       "  foo\n  bar\n- bar\n  baz\n- baz\n+ fuga\n  hoge\n+ fuga\n"
       (with-output-to-string
         (lambda () (diff-report diff-a diff-b))))

;;-------------------------------------------------------------------
(test-section "html-lite")
(use text.html-lite)
(use srfi-13)
(test-module 'text.html-lite)

(test* "html-escape-string"
       "&lt;a href=&quot;http://abc/def?ghi&amp;jkl&quot;&gt;"
       (html-escape-string "<a href=\"http://abc/def?ghi&jkl\">"))

(test* "html-escape-string"
       "&lt;class&gt;"
       (html-escape-string '<class>))

(test* "html-doctype"
       '("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\""
         "\"http://www.w3.org/TR/html4/strict.dtd\">" "")
       (map string-trim-both (string-split (html-doctype) #\newline)))

(test* "html-doctype"
       '("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\""
         "\"http://www.w3.org/TR/html4/loose.dtd\">" "")
       (map string-trim-both
            (string-split (html-doctype :type :transitional) #\newline)))

(test* "html-doctype"
       '("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Frameset//EN\""
         "\"http://www.w3.org/TR/html4/frameset.dtd\">" "")
       (map string-trim-both
            (string-split (html-doctype :type :frameset) #\newline)))

(test* "html-doctype"
       '("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\""
         "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">" "")
       (map string-trim-both
            (string-split (html-doctype :type :xhtml-1.0) #\newline)))

(use srfi-13)

(let ()
  ;; NB: avoid using tree->string, for we haven't tested it yet.
  (define (tos x) (string-delete (string-downcase (x->string x)) #\newline))
  (define (flatten-rec x r)
    (cond ((null? x) r)
          ((not (pair? x)) (cons (tos x) r))
          ((pair? (car x))
           (flatten-rec (cdr x) (flatten-rec (car x) r)))
          ((null? (car x)) (flatten-rec (cdr x) r))
          (else (flatten-rec (cdr x) (cons (tos (car x)) r)))))
  (define (flatten x) (string-concatenate-reverse (flatten-rec x '())))
  
  (test* "html, head, body"
         "<html><head><title>foo</title></head><body>foo</body></html>"
         (flatten (html:html (html:head (html:title "foo"))
                             (html:body "foo"))))
  (test* "attributes"
         "<a href=\"http://foo/bar?a&amp;b\" id=\"aabb\">zzdd</a>"
         (flatten (html:a :href "http://foo/bar?a&b" :id "aabb" "zzdd")))

  (test* "empty element"
         "<img src=\"foo\" alt=\"bar baz\" />"
         (flatten (html:img :src "foo" :alt "bar baz")))
  )

;;-------------------------------------------------------------------
(test-section "parse")
(use text.parse)
(test-module 'text.parse)

;; a part of text data is taken from Oleg's vinput-parse.scm
;;  http://pobox.com/~oleg/ftp/Scheme/parsing.html

(define (test-find-string input pattern . max-chars)
  (call-with-input-string input
    (lambda (p)
      (let* ((n (apply find-string-from-port? pattern p max-chars))
             (c (read-char p)))
        (list n (if (eof-object? c) 'eof c))))))

(test* "find-string-from-port?" '(7 #\d)
       (test-find-string "bacacabd" "acab"))
(test* "find-string-from-port?" '(7 #\d)
       (test-find-string "bacacabd" "acab" 100))
(test* "find-string-from-port?" '(#f eof)
       (test-find-string "bacacabd" "acad"))
(test* "find-string-from-port?" '(#f eof)
       (test-find-string "bacacabd" "acad" 100))
(test* "find-string-from-port?" '(#f #\a)
       (test-find-string "bacacabd" "bd" 5))
(test* "find-string-from-port?" '(8 eof)
       (test-find-string "bacacabd" "bd" 9))
(test* "find-string-from-port?" '(8 eof)
       (test-find-string "bacacabd" "bd"))
(test* "find-string-from-port?" '(8 eof)
       (test-find-string "bacacabd" "bd" 8))
(test* "find-string-from-port?" '(#f eof)
       (test-find-string "bacacabd" "be" 20))


(define (test-parseutil proc input . args)
  (call-with-input-string input
    (lambda (p)
      (let* ((c (apply proc (append args (list p))))
             (n (read-char p)))
        (list (if (eof-object? c) 'eof c)
              (if (eof-object? n) 'eof n))))))

(define (test-assert-curr-char str clist)
  (test-parseutil assert-curr-char str clist "zz"))

(test* "assert-curr-char" '(#\space #\a)
       (test-assert-curr-char " abcd" '(#\a #\space)))
(test* "assert-curr-char" '(#\space #\a)
       (test-assert-curr-char " abcd" #[a ]))
(test* "assert-curr-char" '(#\space #\a)
       (test-assert-curr-char " abcd" #[a\s]))
(test* "assert-curr-char" '(#\space #\a)
       (test-assert-curr-char " abcd" '(#\a #[\s])))
(test* "assert-curr-char" '(#\a #\space)
       (test-assert-curr-char "a bcd" '(#\a #\space)))
(test* "assert-curr-char" '(#\a #\space)
       (test-assert-curr-char "a bcd" #[a ]))
(test* "assert-curr-char" (test-error)
       (test-assert-curr-char "bcd" #[a ]))
(test* "assert-curr-char" (test-error)
       (test-assert-curr-char "" #[a ]))
(test* "assert-curr-char" '(eof eof)
       (test-assert-curr-char "" '(#\a #\space *eof*)))

(test* "skip-until number" '(#f #\a)
       (test-parseutil skip-until " abcd" 1))
(test* "skip-until number" (test-error)
       (test-parseutil skip-until " abcd" 10))
(test* "skip-until number" '(#f eof)
       (test-parseutil skip-until " abcd" 5))
(test* "skip-until cset" '(#\space #\a)
       (test-parseutil skip-until " abcd" '(#\a #\space)))
(test* "skip-until cset" '(#\space #\a)
       (test-parseutil skip-until " abcd" #[a ]))
(test* "skip-until cset" '(#\c #\space)
       (test-parseutil skip-until "xxxc bcd" #[abc ]))
(test* "skip-until cset" '(#\c eof)
       (test-parseutil skip-until "xxxc" #[abc ]))
(test* "skip-until cset" (test-error)
       (test-parseutil skip-until "xxxc" #[def]))
(test* "skip-until cset" '(eof eof)
       (test-parseutil skip-until "xxxc" '(#[def] *eof*)))
(test* "skip-until cset" '(#\c eof)
       (test-parseutil skip-until "xxxc" '(#[c-f] *eof*)))
(test* "skip-until proc" '(#\c #\space)
       (test-parseutil skip-until "xxxc bcd"
                       (lambda (x) (not (eqv? x #\x)))))
(test* "skip-until proc" '(eof eof)
       (test-parseutil skip-until "xxx"
                       (lambda (x) (not (eqv? x #\x)))))
(test* "skip-until proc" (test-error)
       (test-parseutil skip-until "yyyy"
                       (lambda (x) (eqv? x #\x))))
(test* "skip-while" '(#\d #\d)
       (test-parseutil skip-while "xxxd" '(#\a #\space #\x)))
(test* "skip-while" '(#\d #\d)
       (test-parseutil skip-while "xxxd" #[ax ]))
(test* "skip-while" '(#\y #\y)
       (test-parseutil skip-while "yxxxd" #[ax ]))
(test* "skip-while" '(eof eof)
       (test-parseutil skip-while "xxxa" #[ax ]))
(test* "skip-while" '(#\d #\d)
       (test-parseutil skip-while "xxxd"
                       (lambda (x) (eqv? x #\x))))
(test* "skip-while" '(#\y #\y)
       (test-parseutil skip-while "yxxxd"
                       (lambda (x) (eqv? x #\x))))
(test* "skip-while" '(eof eof)
       (test-parseutil skip-while "yxxxd"
                       (lambda (x) (and (char? x)
                                        (char-alphabetic? x)))))

(test* "next-token" '("" #\d)
       (test-parseutil next-token "xxxd" #[ax ] #[d] "next token"))
(test* "next-token" '("bc" #\d)
       (test-parseutil next-token "xxxabcd" #[ax ] #[d] "next token"))
(test* "next-token" '("aeio" #\tab)
       (test-parseutil next-token "   aeio\tnjj" #[\s] #[\s] "next token"))
(test* "next-token" (test-error)
       (test-parseutil next-token "   aeio" #[\s] #[\s] "next token"))
(test* "next-token" '("aeio" eof)
       (test-parseutil next-token "   aeio" #[\s] '(#[\s] *eof*) "next token"))
(test* "next-token" '("aeio" #\tab)
       (test-parseutil next-token "   aeio\tnjj"
                       (lambda (x) (and (char? x)
                                        (char-whitespace? x)))
                       (lambda (x) (or (eof-object? x)
                                       (char-whitespace? x)))
                       "next token"
                       ))

(test* "next-token-of" '("" #\x)
       (test-parseutil next-token-of "xxxd" #[a-c]))
(test* "next-token-of" '("" #\x)
       (test-parseutil next-token-of "xxxd" #[a-d]))
(test* "next-token-of" '("xxx" #\d)
       (test-parseutil next-token-of "xxxd" #[ax]))
(test* "next-token-of" '("anmb" #\-)
       (test-parseutil next-token-of "anmb-runge" #[\w]))
(test* "next-token-of" '("rnge!rg0#$@" #\space)
       (test-parseutil next-token-of "rnge!rg0#$@ bag" #[\S]))
(test* "next-token-of" '("xxx" #\d)
       (test-parseutil next-token-of "xxxd"
                       (lambda (x) (eqv? x #\x))))
(test* "next-token-of" '("xxxx" eof)
       (test-parseutil next-token-of "xxxx"
                       (lambda (x) (eqv? x #\x))))

(test* "read-string" '("aaaa" #\a)
       (test-parseutil read-string "aaaaa" 4))
(test* "read-string" '("aaaaa" eof)
       (test-parseutil read-string "aaaaa" 5))
(test* "read-string" '("aaaaa" eof)
       (test-parseutil read-string "aaaaa" 6))
(test* "read-string" '("" #\a)
       (test-parseutil read-string "aaaaa" 0))
(test* "read-string" '("" #\a)
       (test-parseutil read-string "aaaaa" -1))
(test* "read-string" '("" eof)
       (test-parseutil read-string "" 7))

;;-------------------------------------------------------------------
(test-section "progress")
(use text.progress)
(test-module 'text.progress)

;; WRITEME

;;-------------------------------------------------------------------
(test-section "sql")
(use text.sql)
(test-module 'text.sql)

(test* "sql-tokenize" '("select" "tab" #\. "x" #\, "tab" #\. "y" "as" "foo"
                        "from" "tab" "where" "tab" #\. "z" < (number "30"))
       (sql-tokenize "select tab.x, tab.y as foo from tab\nwhere tab.z<30"))

(test* "sql-tokenize (literal numberes)" '((number "0")
                                           (number "-12")
                                           (number "+12")
                                           (number ".123")
                                           (number "123.")
                                           (number "123.45")
                                           (number "-.123")
                                           (number "-123.")
                                           (number "-123.45")
                                           (number "+.123")
                                           (number "+123.")
                                           (number "+123.45")
                                           (number "0E0")
                                           (number "-1E3")
                                           (number "-1.E3")
                                           (number "-.1E3")
                                           (number "-1.2E3")
                                           (number "1E-3")
                                           (number "1.E-3")
                                           (number ".1E-3")
                                           - #\. "E" (number "-3")
                                           (number "1.2") (number ".3")
                                           )
                                           
       (sql-tokenize "0 -12 +12 .123 123. 123.45 -.123 -123. -123.45
                      +.123 +123. +123.45 0E0 -1E3 -1.E3 -.1E3
                      -1.2E3 1E-3 1.E-3 .1E-3 -.E-3 1.2.3"))

(test* "sql-tokenize (literal strings)" '((string "abc")
                                          (string "ab'c")
                                          (string "'abc")
                                          (string "abc'")
                                          (string "")
                                          (string "'")
                                          (string "a'b'c'"))
       (sql-tokenize "'abc' 'ab''c' '''abc' 'abc''' '' '''' 'a''b''c'''"))

(test* "sql-tokenize (unterminated literal)" (test-error <sql-parse-error>)
       (sql-tokenize "'abc def"))

(test* "sql-tokenize (unterminated literal)" (test-error <sql-parse-error>)
       (sql-tokenize "'abc''def"))

(test* "sql-tokenize (other stuff)" '((bitstring "0")
                                      (bitstring "010101")
                                      (hexstring "0")
                                      (hexstring "1aBc9")
                                      (delimited "run \"run\" run"))
       (sql-tokenize "B'0' B'010101' X'0' X'1aBc9' \"run \"\"run\"\" run\""))

(test* "sql-tokenize (parameters)" '((parameter 0) #\,
                                     (parameter 1) #\,
                                     (parameter "foo") #\,
                                     (parameter "bar") #\,
                                     (parameter 2))
       (sql-tokenize "?,?,:foo, :bar , ?"))

;;-------------------------------------------------------------------
(test-section "tree")
(use text.tree)
(test-module 'text.tree)

(test* "tree->string" "" (tree->string '()))
(test* "tree->string" "" (tree->string ""))
(test* "tree->string" "ab" (tree->string "ab"))
(test* "tree->string" "ab" (tree->string 'ab))
(test* "tree->string" "ab" (tree->string '(a . b)))
(test* "tree->string" "ab" (tree->string '(a b)))
(test* "tree->string" "Ab" (tree->string '(|A| . :b)))
(test* "tree->string" "ab" (tree->string '((((() ())) . a) ((((b)))))))

(test-end)
