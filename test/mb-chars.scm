;;
;; test for multibyte characters
;;

(case (gauche-character-encoding)
  ((euc-jp) (load "../test/euc-jp"))
  (else (format #t "No test provided for the character encoding '~s"
                (gauche-character-encoding))))
