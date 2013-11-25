;;
;; testing text.tr
;;

(use gauche.test)
(test-start "text.tr")

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
(test* "complement" "Hello??World?"
       (string-tr "Hello, World!" "a-zA-Z" "?*" :complement #t))
(test* "complement" "H??????W?????"
       (string-tr "Hello, World!" "A-Z" "?*" :complement #t))
(test* "complement & delete" "HelloWorld"
       (string-tr "Hello, World!" "A-Za-z" ""
                  :complement #t :delete #t))
(test* "complement & delete" "HelloWorld"
       (string-tr "Hello, World!" "a-zA-Z" ""
                  :complement #t :delete #t))
(test* "squeeze" "helo,   world!!!!"
       (string-tr "Hello,   World!!!!" "A-Za-z" "a-z" :squeeze #t))
(test* "squeeze & complement" "Hello, World!"
       (string-tr "Hello,   World!!!!" "A-Za-z" ""
                  :squeeze #t :complement #t))

(test* "spec edge case (-)" "-bacdaef"
       (string-tr "Ab-cd-ef" "A-" "-a"))
(test* "spec edge case (\\)" "a/b"
       (string-tr "a\\b" "\\\\" "/"))
(test* "spec edge case (\\)" (test-error)
       (string-tr "a\\b" "\\" "/"))


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

(test-end)
