;;
;; test for multibyte characters
;;

(case (gauche-character-encoding)
  ((euc-jp) (load "../test/euc-jp"))
  ((sjis)   (load "../test/sjis"))
  ((utf-8)  (load "../test/utf-8"))
  (else (format #t "No test provided for the character encoding '~s"
                (gauche-character-encoding))))
