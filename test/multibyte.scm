;; Encode-specific character/string tests
(use gauche.test)
(add-load-path "../test")

(test-start "multibyte operations")

(case (gauche-character-encoding)
  [(euc-jp) (load "euc-jp")]
  [(sjis)   (load "sjis")]
  [(utf-8)  (load "utf-8")]
  [(none)   #f]
  [else (format #t "No test provided for the character encoding ~s"
                (gauche-character-encoding))])

(test-end)
