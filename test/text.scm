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

(test-end)
