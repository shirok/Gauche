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
