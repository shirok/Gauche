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

(test* "csv-reader" *test-error*
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

(test* "diff" '("abc" "ghi")
       (map car (cadr (caddr (diff "abc\ndef\nghi" "abc\nghi")))))

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
(test* "assert-curr-char" *test-error*
       (test-assert-curr-char "bcd" #[a ]))
(test* "assert-curr-char" *test-error*
       (test-assert-curr-char "" #[a ]))
(test* "assert-curr-char" '(eof eof)
       (test-assert-curr-char "" '(#\a #\space *eof*)))

(test* "skip-until number" '(#f #\a)
       (test-parseutil skip-until " abcd" 1))
(test* "skip-until number" *test-error*
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
(test* "skip-until cset" *test-error*
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
(test* "skip-until proc" *test-error*
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
(test* "next-token" *test-error*
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
(test-section "tr")
(use text.tr)
(test-module 'text.tr)

(test* "basic" "hELLO, wORLD!"
       (string-tr "Hello, World!" "A-Za-z" "a-zA-Z"))
(test* "repeat" "h????, w????!"
       (string-tr "Hello, World!" "A-Za-z" "a-z?*"))
(test* "repeat" "h????, w????!"
       (string-tr "Hello, World!" "A-Za-z" "a-z?*0"))
(test* "repeat" "h???!, w!!??!"
       (string-tr "Hello, World!" "A-Za-z" "a-z?*13!*13"))
(test* "repeat - error" *test-error*
       (string-tr "Hello, World!" "A*10" "a-z?*13!*13"))
(test* "delete" ", !"
       (string-tr "Hello, World!" "A-Za-z" "" :delete #t))
(test* "delete" "H, W!"
       (string-tr "Hello, World!" "a-z" "" :delete #t))
(test* "delete" "h, w!"
       (string-tr "Hello, World!" "A-Za-z" "a-z" :delete #t))
(test* "complement" "Hello??World?"
       (string-tr "Hello, World!" "A-Za-z" "?*" :complement #t))
(test* "complement" "H??????W?????"
       (string-tr "Hello, World!" "A-Z" "?*" :complement #t))
(test* "complement & delete" "HelloWorld"
       (string-tr "Hello, World!" "A-Za-z" ""
                  :complement #t :delete #t))
(test* "squeeze" "helo,   world!!!!"
       (string-tr "Hello,   World!!!!" "A-Za-z" "a-z" :squeeze #t))
(test* "squeeze & complement" "Hello, World!"
       (string-tr "Hello,   World!!!!" "A-Za-z" ""
                  :squeeze #t :complement #t))

;; whole test over smaller table size
(test* "basic, table-size" "hELLO, wORLD!"
       (string-tr "Hello, World!" "A-Za-z" "a-zA-Z" :table-size 65))
(test* "repeat, table-size" "h????, w????!"
       (string-tr "Hello, World!" "A-Za-z" "a-z?*" :table-size 66))
(test* "repeat, table-size" "h????, w????!"
       (string-tr "Hello, World!" "A-Za-z" "a-z?*0" :table-size 98))
(test* "repeat, table-size" "h???!, w!!??!"
       (string-tr "Hello, World!" "A-Za-z" "a-z?*13!*13" :table-size 99))
(test* "delete, table-size" ", !"
       (string-tr "Hello, World!" "A-Za-z" ""
                  :delete #t :table-size 32))
(test* "delete, table-size" "H, W!"
       (string-tr "Hello, World!" "a-z" ""
                  :delete #t :table-size 64))
(test* "delete, table-size" "h, w!"
       (string-tr "Hello, World!" "A-Za-z" "a-z"
                  :delete #t :table-size 68))
(test* "complement, table-size" "Hello??World?"
       (string-tr "Hello, World!" "A-Za-z" "?*"
                  :complement #t :table-size 87))
(test* "complement, table-size" "H??????W?????"
       (string-tr "Hello, World!" "A-Z" "?*"
                  :complement #t :table-size 2))
(test* "complement & delete, table-size" "HelloWorld"
       (string-tr "Hello, World!" "A-Za-z" ""
                  :complement #t :delete #t :table-size 70))
(test* "squeeze, table-size" "helo,   world!!!!"
       (string-tr "Hello,   World!!!!" "A-Za-z" "a-z"
                  :squeeze #t :table-size 65))
(test* "squeeze & complement, table-size" "Hello, World!"
       (string-tr "Hello,   World!!!!" "A-Za-z" ""
                  :squeeze #t :complement #t :table-size 103))

(test* "escape in spec" "*ello, World!"
       (string-tr "Hello,-World!" "A\\-H" "_ \\*"))

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
