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

;; middle-level API

(let ([data '(("" "" "" "" "" "" "" "" "")
              ("Exported data" "" "" "" "" "" "" "" "")
              ("" "" "" "" "" "" "" "" "")
              ("" "" "Year" "Country" "" "Population" "GDP" "" "Note")
              ("" "" "1958" "Land of Lisp" "" "39994" "551,435,453" "" "")
              ("" "" "1957" "United States of Formula Translators" "" "115333"
               "4,343,225,434" "" "Estimated")
              ("" "" "1959" "People's Republic of COBOL" ""
               "82524" "3,357,551,143" "" "")
              ("" "" "1970" "Kingdom of Pascal" "" "3785" "" "" "GDP missing")
              ("" "" "" "" "" "" "" "" "")
              ("" "" "1962" "APL Republic" "" "1545" "342,335,151" "" ""))]
      [header-slots1  '("Country" "Year" "GDP" "Population")]
      [header-slots2 '(#/country/i #/year/i #/gdp/i #/popu/i)])
  (test* "make-csv-header-parser (strings)" '#(3 2 6 5)
         (any (make-csv-header-parser header-slots1) data))

  (test* "make-csv-header-parser (regexps)" '#(3 2 6 5)
         (any (make-csv-header-parser header-slots2) data))
  
  (test* "make-csv-record-parser (strings)"
         '(("Land of Lisp" "1958" "551,435,453" "39994")
           ("United States of Formula Translators" "1957" "4,343,225,434"
            "115333")
           ("People's Republic of COBOL" "1959" "3,357,551,143" "82524")
           ("APL Republic" "1962" "342,335,151" "1545"))
         (filter-map (make-csv-record-parser header-slots1 '#(3 2 6 5)
                                             '(("Year" #/^\d+$/)
                                               "Country" "Population" "GDP"))
                     data))

  (test* "make-csv-record-parser (regexps)"
         '(("Land of Lisp" "1958" "551,435,453" "39994")
           ("United States of Formula Translators" "1957" "4,343,225,434"
            "115333")
           ("People's Republic of COBOL" "1959" "3,357,551,143" "82524")
           ("APL Republic" "1962" "342,335,151" "1545"))
         (filter-map (make-csv-record-parser header-slots2 '#(3 2 6 5)
                                             '((#/year/i #/^\d+$/)
                                               #/country/i #/popu/i #/gdp/i))
                     data))
  
  (test* "csv-rows->tuples (allow-gap? #f)"
         '(("Land of Lisp" "1958" "551,435,453" "39994")
           ("United States of Formula Translators" "1957" "4,343,225,434"
            "115333")
           ("People's Republic of COBOL" "1959" "3,357,551,143" "82524")
           ("Kingdom of Pascal" "1970" "" "3785"))
         (csv-rows->tuples data header-slots1))

  (test* "csv-rows->tuples (allow-gap? #t)"
         '(("Land of Lisp" "1958" "551,435,453" "39994")
           ("United States of Formula Translators" "1957" "4,343,225,434"
            "115333")
           ("People's Republic of COBOL" "1959" "3,357,551,143" "82524")
           ("Kingdom of Pascal" "1970" "" "3785")
           ("APL Republic" "1962" "342,335,151" "1545"))
         (csv-rows->tuples data header-slots1 :allow-gap? #t))
  )

;;-------------------------------------------------------------------
(test-section "diff")
(use text.diff)
(use srfi-13)
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

(test* "diff"
       '(((- 2 "bar")) ((- 4 "baz") (+ 3 "fuga")) ((+ 5 "fuga")))
       (diff diff-a diff-b))

(test* "diff"
       '(((- 2 "bar")) ((- 4 "baz") (+ 3 "FUGA")) ((+ 5 "FUGA")))
       (diff diff-a (string-upcase diff-b) :equal string-ci=?))

(test* "diff-report"
       "  foo\n  bar\n- bar\n  baz\n- baz\n+ fuga\n  hoge\n+ fuga\n"
       (with-output-to-string
         (lambda () (diff-report diff-a diff-b))))

;;-------------------------------------------------------------------
(test-section "gap-buffer")
(use text.gap-buffer)
(use gauche.uvector)

(test-module 'text.gap-buffer)

;; stringify gap-buffer for test purpose
(define (gap-buffer-visualize gbuf)
  (with-output-to-string
    (^[]
      (let1 buf (~ gbuf'buffer) ;accessing internal
        (dotimes [i (gap-buffer-gap-start gbuf)]
          (display (integer->char (~ buf i))))
        (dotimes [i (- (gap-buffer-gap-end gbuf) (gap-buffer-gap-start gbuf))]
          (display #\_))
        (dotimes [i (- (gap-buffer-capacity gbuf) (gap-buffer-gap-end gbuf))]
          (display (integer->char (~ buf (+ i (gap-buffer-gap-end gbuf))))))))))

(test* "constuct" "abcde___"
       (gap-buffer-visualize (string->gap-buffer "abcde")))
(test* "constuct" "___abcde"
       (gap-buffer-visualize (string->gap-buffer "abcde" 0 'beginning)))
(test* "constuct" "bcd_"
       (gap-buffer-visualize (string->gap-buffer "abcde" 0 'end 1 4)))
  
(let1 gbuf (string->gap-buffer "abcde")
  (test* "move" "abcd___e"
         (gap-buffer-visualize (gap-buffer-move! gbuf -1 'current)))
  (test* "move" "a___bcde"
         (gap-buffer-visualize (gap-buffer-move! gbuf 1)))
  (test* "move" "abc___de"
         (gap-buffer-visualize (gap-buffer-move! gbuf -2 'end)))
  (test* "insert" "abcZ__de"
         (gap-buffer-visualize (gap-buffer-insert! gbuf #\Z)))
  (test* "insert" "abcZxyde"
         (gap-buffer-visualize (gap-buffer-insert! gbuf "xy")))
  (test* "insert" "abcZxyw_______de"
         (gap-buffer-visualize (gap-buffer-insert! gbuf #\w)))
  (test* "insert"
         "abcZxyw012345678901234567890123456789_________________________de"
         (gap-buffer-visualize
          (gap-buffer-insert! gbuf "012345678901234567890123456789")))
  (test* "delete"
         "abcZxyw012345678901234567890123456789__________________________e"
         (gap-buffer-visualize
          (gap-buffer-delete! gbuf 1)))
  (test* "delete"
         "abc____________________________________678901234567890123456789e"
         (begin (gap-buffer-move! gbuf 3)
                (gap-buffer-visualize
                 (gap-buffer-delete! gbuf 10))))
  (test* "delete"
         (test-error)
         (gap-buffer-visualize (gap-buffer-delete! gbuf 100)))
  (test* "change"
         "abcdef______________________________________1234567890123456789e"
         (gap-buffer-visualize (gap-buffer-change! gbuf 5 "def")))
  )

(let1 gbuf (string->gap-buffer "abcde")
  (test* "->string (gap at end)" "abcde"
         (gap-buffer->string gbuf))
  (test* "->string (gap at beginning)" "abcde"
         (begin (gap-buffer-move! gbuf 0)
                (gap-buffer->string gbuf)))
  (test* "->string (gap at middle)" "abcde"
         (begin (gap-buffer-move! gbuf 3 'current)
                (gap-buffer->string gbuf)))
  )

(let1 gbuf (gap-buffer-copy (string->gap-buffer "abcde"))
  (test* "undo and redo"
         '("abXYZcde" "abXYde" "aQRSYde" "abXYde" "abXYZcde"
           "abcde" "aQRSYde" "abcde")
         (let* ([u0 (begin (gap-buffer-move! gbuf 2)
                           (gap-buffer-edit! gbuf '(i #f "XYZ")))]
                [s0 (gap-buffer->string gbuf)] ; abXYZcde
                [u1 (begin (gap-buffer-move! gbuf -1 'current)
                           (gap-buffer-edit! gbuf '(d #f 2)))]
                [s1 (gap-buffer->string gbuf)] ; abXYde
                [u2 (begin (gap-buffer-move! gbuf -3 'current)
                           (gap-buffer-edit! gbuf '(c #f 2 "QRS")))]
                [s2 (gap-buffer->string gbuf)] ; aQRSYde
                [r2 (begin (gap-buffer-move! gbuf 0)
                           (gap-buffer-edit! gbuf u2))] ; undo change!
                [s3 (gap-buffer->string gbuf)] ; abXYde
                [r1 (gap-buffer-edit! gbuf u1)] ; undo delete!
                [s4 (gap-buffer->string gbuf)] ; abXYZcde
                [r0 (begin (gap-buffer-move! gbuf -1 'end)
                           (gap-buffer-edit! gbuf u0))] ; undo insert!
                [s5 (gap-buffer->string gbuf)] ; abcde
                [u0 (gap-buffer-edit! gbuf r0)] ; redo insert!
                [u1 (gap-buffer-edit! gbuf r1)] ; redo delete!
                [u2 (gap-buffer-edit! gbuf r2)] ; redo change!
                [s6 (gap-buffer->string gbuf)] ; aQRSYde
                [s7 (begin (gap-buffer-edit! gbuf u2)
                           (gap-buffer-edit! gbuf u1)
                           (gap-buffer-edit! gbuf u0)
                           (gap-buffer->string gbuf))] ; abcde
                )
           (list s0 s1 s2 s3 s4 s5 s6 s7)))
  )

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
                       (^x (not (eqv? x #\x)))))
(test* "skip-until proc" '(eof eof)
       (test-parseutil skip-until "xxx"
                       (^x (not (eqv? x #\x)))))
(test* "skip-until proc" (test-error)
       (test-parseutil skip-until "yyyy"
                       (^x (eqv? x #\x))))
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
                       (^x (eqv? x #\x))))
(test* "skip-while" '(#\y #\y)
       (test-parseutil skip-while "yxxxd"
                       (^x (eqv? x #\x))))
(test* "skip-while" '(eof eof)
       (test-parseutil skip-while "yxxxd"
                       (^x (and (char? x)
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
                       (^x (and (char? x)
				(char-whitespace? x)))
                       (^x (or (eof-object? x)
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
                       (^x (eqv? x #\x))))
(test* "next-token-of" '("xxxx" eof)
       (test-parseutil next-token-of "xxxx"
                       (^x (eqv? x #\x))))

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
(test-section "line-edit")
(use text.line-edit)
(test-module 'text.line-edit)


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
(test* "tree->string" "ab" (tree->string '((((() ())) . a) ((((b)))))))
(test* "tree->string"
       (if (symbol? :b) "A:b" "Ab") ; transient during symbol-keyword integration
       (tree->string '(|A| . :b)))

;;-------------------------------------------------------------------
(test-section "unicode.ucd")
(use text.unicode.ucd)
(test-module 'text.unicode.ucd)

(test-end)
