;;
;; testing text.* module
;;

(use gauche.test)
(test-start "text utilities")

;;-------------------------------------------------------------------
(test-section "csv")
(use text.csv)

(test "csv-reader" '("abc" "def" "" "ghi")
      (lambda ()
        (call-with-input-string "abc  ,  def  ,, ghi  "
          (make-csv-reader #\,))))

(test "csv-reader" '("abc" "def" "" ", ghi")
      (lambda ()
        (call-with-input-string "abc  :  def  :: , ghi  "
          (make-csv-reader #\:))))

(test "csv-reader" '("abc" "def" "ghi")
      (lambda ()
        (call-with-input-string "abc  ,  \"def\"  , \"ghi\"  "
          (make-csv-reader #\,))))

(test "csv-reader" '("abc" " de,f " "gh\ni" "jkl")
      (lambda ()
        (call-with-input-string "   abc,  \" de,f \"  , \"gh\ni\", \"jkl\""
          (make-csv-reader #\,))))

(test "csv-reader" '("ab\nc" "de \n\n \nf " "" "" "gh\"\n\"i")
      (lambda ()
        (call-with-input-string "   \"ab\nc\" ,  \"de \n\n \nf \"  ,  , \"\" , \"gh\"\"\n\"\"i\""
          (make-csv-reader #\,))))

(test "csv-reader" #t
      (lambda ()
        (with-error-handler
         (lambda (e) #t)
         (lambda ()
           (call-with-input-string " abc,  def , \"ghi\"\"\n\n"
             (make-csv-reader #\,))))))

(test "csv-reader" #t
      (lambda ()
        (eof-object?
         (call-with-input-string "" (make-csv-reader #\,)))))

(test "csv-writer"
      "abc,def,123,\"what's up?\",\"he said, \"\"nothing new.\"\"\"\n"
      (lambda ()
        (call-with-output-string
          (lambda (out)
            ((make-csv-writer #\,)
             out
             '("abc" "def" "123" "what's up?" "he said, \"nothing new.\"")))))
      )

(test "csv-writer"
      "abc,def,123,\"what's up?\",\"he said, \"\"nothing new.\"\"\"\r\n"
      (lambda ()
        (call-with-output-string
          (lambda (out)
            ((make-csv-writer #\, "\r\n")
             out
             '("abc" "def" "123" "what's up?" "he said, \"nothing new.\"")))))
      )

(test "csv-writer" "\n"
      (lambda () (call-with-output-string
                   (lambda (out)
                     ((make-csv-writer #\,) out '())))))

;;-------------------------------------------------------------------
(test-section "parse")
(use text.parse)

;; a part of text data is taken from Oleg's vinput-parse.scm
;;  http://pobox.com/~oleg/ftp/Scheme/parsing.html

(define (test-find-string input pattern . max-chars)
  (call-with-input-string input
    (lambda (p)
      (let* ((n (apply find-string-from-port? pattern p max-chars))
             (c (read-char p)))
        (list n (if (eof-object? c) 'eof c))))))

(test "find-string-from-port?" '(7 #\d)
      (lambda () (test-find-string "bacacabd" "acab")))
(test "find-string-from-port?" '(7 #\d)
      (lambda () (test-find-string "bacacabd" "acab" 100)))
(test "find-string-from-port?" '(#f eof)
      (lambda () (test-find-string "bacacabd" "acad")))
(test "find-string-from-port?" '(#f eof)
      (lambda () (test-find-string "bacacabd" "acad" 100)))
(test "find-string-from-port?" '(#f #\a)
      (lambda () (test-find-string "bacacabd" "bd" 5)))
(test "find-string-from-port?" '(8 eof)
      (lambda () (test-find-string "bacacabd" "bd" 9)))
(test "find-string-from-port?" '(8 eof)
      (lambda () (test-find-string "bacacabd" "bd")))
(test "find-string-from-port?" '(8 eof)
      (lambda () (test-find-string "bacacabd" "bd" 8)))
(test "find-string-from-port?" '(#f eof)
      (lambda () (test-find-string "bacacabd" "be" 20)))


(define (test-parseutil proc input . args)
  (with-error-handler
   (lambda (e) 'error)
   (lambda ()
     (call-with-input-string input
       (lambda (p)
         (let* ((c (apply proc (append args (list p))))
                (n (read-char p)))
           (list (if (eof-object? c) 'eof c)
                 (if (eof-object? n) 'eof n))))))))

(define (test-assert-curr-char str clist)
  (test-parseutil assert-curr-char str clist "zz"))

(test "assert-curr-char" '(#\space #\a)
      (lambda () (test-assert-curr-char " abcd" '(#\a #\space))))
(test "assert-curr-char" '(#\space #\a)
      (lambda () (test-assert-curr-char " abcd" #[a ])))
(test "assert-curr-char" '(#\space #\a)
      (lambda () (test-assert-curr-char " abcd" #[a\s])))
(test "assert-curr-char" '(#\space #\a)
      (lambda () (test-assert-curr-char " abcd" '(#\a #[\s]))))
(test "assert-curr-char" '(#\a #\space)
      (lambda () (test-assert-curr-char "a bcd" '(#\a #\space))))
(test "assert-curr-char" '(#\a #\space)
      (lambda () (test-assert-curr-char "a bcd" #[a ])))
(test "assert-curr-char" 'error
      (lambda () (test-assert-curr-char "bcd" #[a ])))
(test "assert-curr-char" 'error
      (lambda () (test-assert-curr-char "" #[a ])))
(test "assert-curr-char" '(eof eof)
      (lambda () (test-assert-curr-char "" '(#\a #\space *eof*))))

(test "skip-until number" '(#f #\a)
      (lambda () (test-parseutil skip-until " abcd" 1)))
(test "skip-until number" 'error
      (lambda () (test-parseutil skip-until " abcd" 10)))
(test "skip-until number" '(#f eof)
      (lambda () (test-parseutil skip-until " abcd" 5)))
(test "skip-until cset" '(#\space #\a)
      (lambda () (test-parseutil skip-until " abcd" '(#\a #\space))))
(test "skip-until cset" '(#\space #\a)
      (lambda () (test-parseutil skip-until " abcd" #[a ])))
(test "skip-until cset" '(#\c #\space)
      (lambda () (test-parseutil skip-until "xxxc bcd" #[abc ])))
(test "skip-until cset" '(#\c eof)
      (lambda () (test-parseutil skip-until "xxxc" #[abc ])))
(test "skip-until cset" 'error
      (lambda () (test-parseutil skip-until "xxxc" #[def])))
(test "skip-until cset" '(eof eof)
      (lambda () (test-parseutil skip-until "xxxc" '(#[def] *eof*))))
(test "skip-until cset" '(#\c eof)
      (lambda () (test-parseutil skip-until "xxxc" '(#[c-f] *eof*))))
(test "skip-while" '(#\d #\d)
      (lambda () (test-parseutil skip-while "xxxd" '(#\a #\space #\x))))
(test "skip-while" '(#\d #\d)
      (lambda () (test-parseutil skip-while "xxxd" #[ax ])))
(test "skip-while" '(#\y #\y)
      (lambda () (test-parseutil skip-while "yxxxd" #[ax ])))
(test "skip-while" '(eof eof)
      (lambda () (test-parseutil skip-while "xxxa" #[ax ])))

(test "next-token" '("" #\d)
      (lambda () (test-parseutil next-token "xxxd" #[ax ] #[d] "next token")))
(test "next-token" '("bc" #\d)
      (lambda () (test-parseutil next-token "xxxabcd" #[ax ] #[d] "next token")))
(test "next-token" '("aeio" #\tab)
      (lambda () (test-parseutil next-token "   aeio\tnjj" #[\s] #[\s] "next token")))
(test "next-token" 'error
      (lambda () (test-parseutil next-token "   aeio" #[\s] #[\s] "next token")))
(test "next-token" '("aeio" eof)
      (lambda () (test-parseutil next-token "   aeio" #[\s] '(#[\s] *eof*) "next token")))
(test "next-token-of" '("" #\x)
      (lambda () (test-parseutil next-token-of "xxxd" #[a-c])))
(test "next-token-of" '("" #\x)
      (lambda () (test-parseutil next-token-of "xxxd" #[a-d])))
(test "next-token-of" '("xxx" #\d)
      (lambda () (test-parseutil next-token-of "xxxd" #[ax])))
(test "next-token-of" '("anmb" #\-)
      (lambda () (test-parseutil next-token-of "anmb-runge" #[\w])))
(test "next-token-of" '("rnge!rg0#$@" #\space)
      (lambda () (test-parseutil next-token-of "rnge!rg0#$@ bag" #[\S])))

(test "read-string" '("aaaa" #\a)
      (lambda () (test-parseutil read-string "aaaaa" 4)))
(test "read-string" '("aaaaa" eof)
      (lambda () (test-parseutil read-string "aaaaa" 5)))
(test "read-string" '("aaaaa" eof)
      (lambda () (test-parseutil read-string "aaaaa" 6)))
(test "read-string" '("" #\a)
      (lambda () (test-parseutil read-string "aaaaa" 0)))
(test "read-string" '("" #\a)
      (lambda () (test-parseutil read-string "aaaaa" -1)))
(test "read-string" '("" eof)
      (lambda () (test-parseutil read-string "" 7)))

;;-------------------------------------------------------------------
(test-section "tr")
(use text.tr)

(test "basic" "hELLO, wORLD!"
      (lambda () (string-tr "Hello, World!" "A-Za-z" "a-zA-Z")))
(test "repeat" "h????, w????!"
      (lambda () (string-tr "Hello, World!" "A-Za-z" "a-z?*")))
(test "repeat" "h????, w????!"
      (lambda () (string-tr "Hello, World!" "A-Za-z" "a-z?*0")))
(test "repeat" "h???!, w!!??!"
      (lambda () (string-tr "Hello, World!" "A-Za-z" "a-z?*13!*13")))
(test "repeat - error" #t
      (lambda ()
        (with-error-handler
         (lambda (e) #t)
         (lambda () (string-tr "Hello, World!" "A*10" "a-z?*13!*13")))))
(test "delete" ", !"
      (lambda ()
        (string-tr "Hello, World!" "A-Za-z" "" :delete #t)))
(test "delete" "H, W!"
      (lambda ()
        (string-tr "Hello, World!" "a-z" "" :delete #t)))
(test "delete" "h, w!"
      (lambda ()
        (string-tr "Hello, World!" "A-Za-z" "a-z" :delete #t)))
(test "complement" "Hello??World?"
      (lambda ()
        (string-tr "Hello, World!" "A-Za-z" "?*" :complement #t)))
(test "complement" "H??????W?????"
      (lambda ()
        (string-tr "Hello, World!" "A-Z" "?*" :complement #t)))
(test "complement & delete" "HelloWorld"
      (lambda ()
        (string-tr "Hello, World!" "A-Za-z" ""
                   :complement #t :delete #t)))
(test "squeeze" "helo,   world!!!!"
      (lambda ()
        (string-tr "Hello,   World!!!!" "A-Za-z" "a-z" :squeeze #t)))
(test "squeeze & complement" "Hello, World!"
      (lambda ()
        (string-tr "Hello,   World!!!!" "A-Za-z" ""
                   :squeeze #t :complement #t)))

;; whole test over smaller table size
(test "basic, table-size" "hELLO, wORLD!"
      (lambda ()
        (string-tr "Hello, World!" "A-Za-z" "a-zA-Z" :table-size 65)))
(test "repeat, table-size" "h????, w????!"
      (lambda ()
        (string-tr "Hello, World!" "A-Za-z" "a-z?*" :table-size 66)))
(test "repeat, table-size" "h????, w????!"
      (lambda ()
        (string-tr "Hello, World!" "A-Za-z" "a-z?*0" :table-size 98)))
(test "repeat, table-size" "h???!, w!!??!"
      (lambda ()
        (string-tr "Hello, World!" "A-Za-z" "a-z?*13!*13" :table-size 99)))
(test "delete, table-size" ", !"
      (lambda ()
        (string-tr "Hello, World!" "A-Za-z" ""
                   :delete #t :table-size 32)))
(test "delete, table-size" "H, W!"
      (lambda ()
        (string-tr "Hello, World!" "a-z" ""
                   :delete #t :table-size 64)))
(test "delete, table-size" "h, w!"
      (lambda ()
        (string-tr "Hello, World!" "A-Za-z" "a-z"
                   :delete #t :table-size 68)))
(test "complement, table-size" "Hello??World?"
      (lambda ()
        (string-tr "Hello, World!" "A-Za-z" "?*"
                   :complement #t :table-size 87)))
(test "complement, table-size" "H??????W?????"
      (lambda ()
        (string-tr "Hello, World!" "A-Z" "?*"
                   :complement #t :table-size 2)))
(test "complement & delete, table-size" "HelloWorld"
      (lambda ()
        (string-tr "Hello, World!" "A-Za-z" ""
                   :complement #t :delete #t :table-size 70)))
(test "squeeze, table-size" "helo,   world!!!!"
      (lambda ()
        (string-tr "Hello,   World!!!!" "A-Za-z" "a-z"
                   :squeeze #t :table-size 65)))
(test "squeeze & complement, table-size" "Hello, World!"
      (lambda ()
        (string-tr "Hello,   World!!!!" "A-Za-z" ""
                   :squeeze #t :complement #t :table-size 103)))

(test "escape in spec" "*ello, World!"
      (lambda ()
        (string-tr "Hello,-World!" "A\\-H" "_ \\*")))

(test-end)
