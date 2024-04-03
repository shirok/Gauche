;;;
;;; test SRFI-13
;;;

(use gauche.test)
(test-start "SRFI-13")
(test-section "SRFI-13")

(define-module srfi-13-tests
  (use gauche.test)
  (use srfi.13)
  (test-module 'srfi.13)

  (test* "string-null?" #f (string-null? "abc"))
  (test* "string-null?" #t (string-null? ""))
  (test* "string-every" #t (string-every #\a ""))
  (test* "string-every" #t (string-every #\a "aaaa"))
  (test* "string-every" #f (string-every #\a "aaba"))
  (test* "string-every" #t (string-every #[a-z] "aaba"))
  (test* "string-every" #f (string-every #[a-z] "aAba"))
  (test* "string-every" #t (string-every #[a-z] ""))
  (test* "string-every" #t (string-every (^x (char-ci=? x #\a)) "aAaA"))
  (test* "string-every" #f (string-every (^x (char-ci=? x #\a)) "aAbA"))
  (test* "string-every" (char->integer #\A)
         (string-every (^x (char->integer x)) "aAbA"))
  (test* "string-every" #t
         (string-every (^x (error "hoge")) ""))
  (test* "string-any" #t (string-any #\a "aaaa"))
  (test* "string-any" #f (string-any #\a "Abcd"))
  (test* "string-any" #f (string-any #\a ""))
  (test* "string-any" #t (string-any #[a-z] "ABcD"))
  (test* "string-any" #f (string-any #[a-z] "ABCD"))
  (test* "string-any" #f (string-any #[a-z] ""))
  (test* "string-any" #t (string-any (^x (char-ci=? x #\a)) "CAaA"))
  (test* "string-any" #f (string-any (^x (char-ci=? x #\a)) "ZBRC"))
  (test* "string-any" #f (string-any (^x (char-ci=? x #\a)) ""))
  (test* "string-any" (char->integer #\a)
         (string-any (^x (char->integer x)) "aAbA"))
  (test* "string-tabulate" "0123456789"
         (string-tabulate (^[code] (integer->char (+ code (char->integer #\0))))
                          10))
  (test* "string-tabulate" ""
         (string-tabulate (^[code] (integer->char (+ code (char->integer #\0))))
                          0))
  (test* "reverse-list->string" "cBa"
         (reverse-list->string '(#\a #\B #\c)))
  (test* "reverse-list->string" ""
         (reverse-list->string '()))
                                        ; string-join : Gauche builtin.
  (test* "substring/shared" "cde" (substring/shared "abcde" 2))
  (test* "substring/shared" "cd"  (substring/shared "abcde" 2 4))
  (test* "string-copy!" "abCDEfg"
         (rlet1 x (string-copy "abcdefg")
           (string-copy! x 2 "CDE")))
  (test* "string-copy!" "abCDEfg"
         (rlet1 x (string-copy "abcdefg")
           (string-copy! x 2 "ZABCDE" 3)))
  (test* "string-copy!" "abCDEfg"
         (rlet1 x (string-copy "abcdefg")
           (string-copy! x 2 "ZABCDEFG" 3 6)))
  (test* "string-copy!" "CDEFGfg"
         (rlet1 x (string-copy "abcdefg")
           (string-copy! x 0 "ZABCDEFG" 3)))
  (test* "string-take" "Pete S"  (string-take "Pete Szilagyi" 6))
  (test* "string-take" ""        (string-take "Pete Szilagyi" 0))
  (test* "string-take" "Pete Szilagyi" (string-take "Pete Szilagyi" 13))
  (test* "string-drop" "zilagyi" (string-drop "Pete Szilagyi" 6))
  (test* "string-drop" "Pete Szilagyi" (string-drop "Pete Szilagyi" 0))
  (test* "string-drop" ""        (string-drop "Pete Szilagyi" 13))

  (test* "string-take-right" "rules" (string-take-right "Beta rules" 5))
  (test* "string-take-right" ""      (string-take-right "Beta rules" 0))
  (test* "string-take-right" "Beta rules" (string-take-right "Beta rules" 10))
  (test* "string-drop-right" "Beta " (string-drop-right "Beta rules" 5))
  (test* "string-drop-right" "Beta rules" (string-drop-right "Beta rules" 0))
  (test* "string-drop-right" ""      (string-drop-right "Beta rules" 10))

  (test* "string-pad" "  325" (string-pad "325" 5))
  (test* "string-pad" "71325" (string-pad "71325" 5))
  (test* "string-pad" "71325" (string-pad "8871325" 5))
  (test* "string-pad" "~~325" (string-pad "325" 5 #\~))
  (test* "string-pad" "~~~25" (string-pad "325" 5 #\~ 1))
  (test* "string-pad" "~~~~2" (string-pad "325" 5 #\~ 1 2))
  (test* "string-pad-right" "325  " (string-pad-right "325" 5))
  (test* "string-pad-right" "71325" (string-pad-right "71325" 5))
  (test* "string-pad-right" "88713" (string-pad-right "8871325" 5))
  (test* "string-pad-right" "325~~" (string-pad-right "325" 5 #\~))
  (test* "string-pad-right" "25~~~" (string-pad-right "325" 5 #\~ 1))
  (test* "string-pad-right" "2~~~~" (string-pad-right "325" 5 #\~ 1 2))

  (test* "string-trim"  "a b c d  \r\n"
         (string-trim "  \t  a b c d  \r\n"))
  (test* "string-trim"  "\t  a b c d  \r\n"
         (string-trim "  \t  a b c d  \r\n" #\space))
  (test* "string-trim"  "a b c d  \r\n"
         (string-trim "4358948a b c d  \r\n" #[\d]))

  (test* "string-trim-right"  "  \t  a b c d"
         (string-trim-right "  \t  a b c d  \r\n"))
  (test* "string-trim-right"  "  \t  a b c d  "
         (string-trim-right "  \t  a b c d  \r\n" #[\r\n]))
  (test* "string-trim-right"  "349853a b c d"
         (string-trim-right "349853a b c d03490" #[\d]))

  (test* "string-trim-both"  "a b c d"
         (string-trim-both "  \t  a b c d  \r\n"))
  (test* "string-trim-both"  "  \t  a b c d  "
         (string-trim-both "  \t  a b c d  \r\n" #[\r\n]))
  (test* "string-trim-both"  "a b c d"
         (string-trim-both "349853a b c d03490" #[\d]))

  ;; string-fill - in string.scm

  (test* "string-compare" 5
         (string-compare "The cat in the hat" "abcdefgh"
                         values values values
                         4 6 2 4))
  (test* "string-compare-ci" 5
         (string-compare-ci "The cat in the hat" "ABCDEFGH"
                            values values values
                            4 6 2 4))

  ;; TODO: bunch of string= families

  (test* "string-prefix-length" 5
         (string-prefix-length "cancaNCAM" "cancancan"))
  (test* "string-prefix-length" 1
         (string-prefix-length "abc" "a"))
  (test* "string-prefix-length" 1
         (string-prefix-length "a" "abc"))
  (test* "string-prefix-length" 0
         (string-prefix-length "abc" ""))
  (test* "string-prefix-length" 0
         (string-prefix-length "" "abc"))
  (test* "string-prefix-length" 0
         (string-prefix-length "" ""))
  (test* "string-prefix-length-ci" 8
         (string-prefix-length-ci "cancaNCAM" "cancancan"))
  (test* "string-suffix-length" 2
         (string-suffix-length "CanCan" "cankancan"))
  (test* "string-suffix-length-ci" 5
         (string-suffix-length-ci "CanCan" "cankancan"))
  (test* "string-prefix-length-ci" 1
         (string-prefix-length-ci "abc" "A"))
  (test* "string-prefix-length-ci" 1
         (string-prefix-length-ci "A" "abc"))
  (test* "string-prefix-length-ci" 0
         (string-prefix-length-ci "abc" ""))
  (test* "string-prefix-length-ci" 0
         (string-prefix-length-ci "" "abc"))
  (test* "string-prefix-length-ci" 0
         (string-prefix-length-ci "" ""))

  (test* "string-prefix?" #t    (string-prefix? "abcd" "abcdefg"))
  (test* "string-prefix?" #f    (string-prefix? "abcf" "abcdefg"))
  (test* "string-prefix-ci?" #t (string-prefix-ci? "abcd" "aBCDEfg"))
  (test* "string-prefix-ci?" #f (string-prefix-ci? "abcf" "aBCDEfg"))
  (test* "string-suffix?" #t    (string-suffix? "defg" "abcdefg"))
  (test* "string-suffix?" #f    (string-suffix? "aefg" "abcdefg"))
  (test* "string-suffix-ci?" #t (string-suffix-ci? "defg" "aBCDEfg"))
  (test* "string-suffix-ci?" #f (string-suffix-ci? "aefg" "aBCDEfg"))

  (test* "string-index" 4
         (string-index "abcd:efgh:ijkl" #\:))
  (test* "string-index" 4
         (string-index "abcd:efgh;ijkl" #[\W]))
  (test* "string-index" #f
         (string-index "abcd:efgh;ijkl" #[\d]))
  (test* "string-index" 9
         (string-index "abcd:efgh:ijkl" #\: 5))
  (test* "string-index-right" 4
         (string-index-right "abcd:efgh;ijkl" #\:))
  (test* "string-index-right" 9
         (string-index-right "abcd:efgh;ijkl" #[\W]))
  (test* "string-index-right" #f
         (string-index-right "abcd:efgh;ijkl" #[\d]))
  (test* "string-index-right" 4
         (string-index-right "abcd:efgh;ijkl" #[\W] 2 5))

  (test* "string-count" 2
         (string-count "abc def\tghi jkl" #\space))
  (test* "string-count" 3
         (string-count "abc def\tghi jkl" #[\s]))
  (test* "string-count" 2
         (string-count "abc def\tghi jkl" #[\s] 4))
  (test* "string-count" 1
         (string-count "abc def\tghi jkl" #[\s] 4 9))
  (test* "string-contains" 3
         (string-contains "Ma mere l'oye" "mer"))
  (test* "string-contains" #f
         (string-contains "Ma mere l'oye" "Mer"))
  (test* "string-contains-ci" 3
         (string-contains-ci "Ma mere l'oye" "Mer"))
  (test* "string-contains-ci" #f
         (string-contains-ci "Ma mere l'oye" "Meer"))
  (test* "string-contains" 15
         (string-contains "eek -- what a geek." "ee" 12 18))
  (test* "string-contains-ci" 15
         (string-contains-ci "Eek -- what a geek." "EE" 12 18))

  (test* "string-titlecase" "--Capitalize This Sentence."
         (string-titlecase "--capitalize tHIS sentence."))
  (test* "string-titlecase" "3Com Makes Routers."
         (string-titlecase "3com makes routers."))
  (test* "string-titlecase!" "alSo Whatever"
         (rlet1 s (string-copy "also whatever")
           (string-titlecase! s 2 9)))

  (test* "string-upcase" "SPEAK LOUDLY"
         (string-upcase "speak loudly"))
  (test* "string-upcase" "PEAK"
         (string-upcase "speak loudly" 1 5))
  (test* "string-upcase!" "sPEAK loudly"
         (rlet1 s (string-copy "speak loudly")
           (string-upcase! s 1 5)))

  (test* "string-downcase" "speak softly"
         (string-downcase "SPEAK SOFTLY"))
  (test* "string-downcase" "peak"
         (string-downcase "SPEAK SOFTLY" 1 5))
  (test* "string-downcase!" "speak softly"
         (rlet1 s (string-copy "SPEAK SOFTLY")
           (string-downcase! s)))
  (test* "string-downcase!" "Speak softly"
         (rlet1 s (string-copy "SPEAK SOFTLY")
           (string-downcase! s 1)))
  (test* "string-downcase!" "Speak SOFTLY"
         (rlet1 s (string-copy "SPEAK SOFTLY")
           (string-downcase! s 1 5)))

  (test* "string-reverse" "nomel on nolem on"
         (string-reverse "no melon no lemon"))
  (test* "string-reverse" "nomel on"
         (string-reverse "no melon no lemon" 9))
  (test* "string-reverse" "on"
         (string-reverse "no melon no lemon" 9 11))
  (test* "string-reverse!" "nomel on nolem on"
         (rlet1 s (string-copy "no melon no lemon")
           (string-reverse! s)))
  (test* "string-reverse!" "no melon nomel on"
         (rlet1 s (string-copy "no melon no lemon")
           (string-reverse! s 9)))
  (test* "string-reverse!" "no melon on lemon"
         (rlet1 s (string-copy "no melon no lemon")
           (string-reverse! s 9 11)))

  (test* "string-append" #f
         (let1 s "test" (eq? s (string-append s))))
  (test* "string-concatenate" "" (string-concatenate '()))
  (test* "string-concatenate" #f
         (let1 s "test" (eq? s (string-concatenate (list s)))))
  (test* "string-concatenate" "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
         (string-concatenate
          '("A" "B" "C" "D" "E" "F" "G" "H"
            "I" "J" "K" "L" "M" "N" "O" "P"
            "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
            "a" "b" "c" "d" "e" "f" "g" "h"
            "i" "j" "k" "l" "m" "n" "o" "p"
            "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))
  (test* "string-concatenate (long list)" #t
         (string=? (make-string 20000 #\a)
                   (string-concatenate (make-list 20000 "a"))))
  (test* "string-concatenate/shared" "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"
         (string-concatenate/shared
          '("A" "B" "C" "D" "E" "F" "G" "H"
            "I" "J" "K" "L" "M" "N" "O" "P"
            "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
            "a" "b" "c" "d" "e" "f" "g" "h"
            "i" "j" "k" "l" "m" "n" "o" "p"
            "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))
  (test* "string-concatenate-reverse" "zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLKJIHGFEDCBA"
         (string-concatenate-reverse
          '("A" "B" "C" "D" "E" "F" "G" "H"
            "I" "J" "K" "L" "M" "N" "O" "P"
            "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
            "a" "b" "c" "d" "e" "f" "g" "h"
            "i" "j" "k" "l" "m" "n" "o" "p"
            "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))
  (test* "string-concatenate-reverse" #f
         (let1 s "test"
           (eq? s (string-concatenate-reverse (list s)))))
  (test* "string-concatenate-reverse (optarg)"
         "Hello, I must be going.XXXXX"
         (string-concatenate-reverse '(" must be" "Hello, I") " going.XXXXX"))
  (test* "string-concatenate-reverse (optarg)"
         "Hello, I must be going."
         (string-concatenate-reverse '(" must be" "Hello, I") " going.XXXXX" 7))

  (test* "string-concatenate-reverse/shared" "zyxwvutsrqponmlkjihgfedcbaZYXWVUTSRQPONMLKJIHGFEDCBA"
         (string-concatenate-reverse/shared
          '("A" "B" "C" "D" "E" "F" "G" "H"
            "I" "J" "K" "L" "M" "N" "O" "P"
            "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
            "a" "b" "c" "d" "e" "f" "g" "h"
            "i" "j" "k" "l" "m" "n" "o" "p"
            "q" "r" "s" "t" "u" "v" "w" "x" "y" "z")))

  (test* "string-map" "svool"
         (string-map (^c (integer->char (- 219 (char->integer c))))
                     "hello"))
  (test* "string-map" "vool"
         (string-map (^c (integer->char (- 219 (char->integer c))))
                     "hello" 1))
  (test* "string-map" "vo"
         (string-map (^c (integer->char (- 219 (char->integer c))))
                     "hello" 1 3))
  (test* "string-map!" "svool"
         (rlet1 s (string-copy "hello")
           (string-map! (^c (integer->char (- 219 (char->integer c))))
                        s)))
  (test* "string-map!" "hvool"
         (rlet1 s (string-copy "hello")
           (string-map! (^c (integer->char (- 219 (char->integer c))))
                        s 1)))
  (test* "string-map!" "hvolo"
         (rlet1 s (string-copy "hello")
           (string-map! (^c (integer->char (- 219 (char->integer c))))
                        s 1 3)))

  (test* "string-fold" '(#\o #\l #\l #\e #\h . #t)
         (string-fold cons #t "hello"))
  (test* "string-fold" '(#\l #\e . #t)
         (string-fold cons #t "hello" 1 3))
  (test* "string-fold-right" '(#\h #\e #\l #\l #\o . #t)
         (string-fold-right cons #t "hello"))
  (test* "string-fold-right" '(#\e #\l . #t)
         (string-fold-right cons #t "hello" 1 3))

  (test* "string-unfold" "hello"
         (string-unfold null? car cdr '(#\h #\e #\l #\l #\o)))
  (test* "string-unfold" "hi hello"
         (string-unfold null? car cdr '(#\h #\e #\l #\l #\o) "hi "))
  (test* "string-unfold" "hi hello ho"
         (string-unfold null? car cdr
                        '(#\h #\e #\l #\l #\o) "hi "
                        (^x " ho")))

  (test* "string-unfold-right" "olleh"
         (string-unfold-right null? car cdr '(#\h #\e #\l #\l #\o)))
  (test* "string-unfold-right" "olleh hi"
         (string-unfold-right null? car cdr '(#\h #\e #\l #\l #\o) " hi"))
  (test* "string-unfold-right" "ho olleh hi"
         (string-unfold-right null? car cdr
                              '(#\h #\e #\l #\l #\o) " hi"
                              (^x "ho ")))

  (test* "string-for-each" "CLtL"
         (let ([out (open-output-string)]
               [prev #f])
           (string-for-each (^c (when (or (not prev)
                                          (char-whitespace? prev))
                                  (write-char c out))
                                (set! prev c))
                            "Common Lisp, the Language")
           (get-output-string out)))
  (test* "string-for-each" "oLtL"
         (let ([out (open-output-string)]
               [prev #f])
           (string-for-each (^c (when (or (not prev)
                                          (char-whitespace? prev))
                                  (write-char c out))
                                (set! prev c))
                            "Common Lisp, the Language" 1)
           (get-output-string out)))
  (test* "string-for-each" "oL"
         (let ([out (open-output-string)]
               [prev #f])
           (string-for-each (^c (when (or (not prev)
                                          (char-whitespace? prev))
                                  (write-char c out))
                                (set! prev c))
                            "Common Lisp, the Language" 1 10)
           (get-output-string out)))
  (test* "string-for-each-index" '(4 3 2 1 0)
         (rlet1 r '()
           (string-for-each-index (^i (set! r (cons i r))) "hello")))
  (test* "string-for-each-index" '(4 3 2 1)
         (rlet1 r '()
           (string-for-each-index (^i (set! r (cons i r))) "hello" 1)))
  (test* "string-for-each-index" '(2 1)
         (rlet1 r '()
           (string-for-each-index (^i (set! r (cons i r))) "hello" 1 3)))

  (test* "xsubstring" "cdefab"
         (xsubstring "abcdef" 2))
  (test* "xsubstring" "efabcd"
         (xsubstring "abcdef" -2))
  (test* "xsubstring" "abcabca"
         (xsubstring "abc" 0 7))
  (test* "xsubstring" "abcabca"
         (xsubstring "abc"
                     30000000000000000000000000000000
                     30000000000000000000000000000007))
  (test* "xsubstring" "abcde"
         (xsubstring "abcde" -10 -5))
  (test* "xsubstring" "eabcdea"
         (xsubstring "abcde" -11 -4))
  (test* "xsubstring" "eabcdeabcdeabc"
         (xsubstring "abcde" -11 3))
  (test* "xsubstring" "defdefd"
         (xsubstring "abcdefg" 0 7 3 6))
  (test* "xsubstring" ""
         (xsubstring "abcdefg" 9 9 3 6))

  (test* "string-xcopy!" "ZZcdefabZZ"
         (rlet1 s (make-string 10 #\Z)
           (string-xcopy! s 2 "abcdef" 2)))
  (test* "string-xcopy!" "ZZdefdefZZ"
         (rlet1 s (make-string 10 #\Z)
           (string-xcopy! s 2 "abcdef" 0 6 3)))

  (test* "string-replace" "abcdXYZghi"
         (string-replace "abcdefghi" "XYZ" 4 6))
  (test* "string-replace" "abcdZghi"
         (string-replace "abcdefghi" "XYZ" 4 6 2))
  (test* "string-replace" "abcdZefghi"
         (string-replace "abcdefghi" "XYZ" 4 4 2))
  (test* "string-replace" "abcdefghi"
         (string-replace "abcdefghi" "XYZ" 4 4 1 1))
  (test* "string-replace" "abcdhi"
         (string-replace "abcdefghi" "" 4 7))

  (test* "string-tokenize" '("Help" "make" "programs" "run," "run," "RUN!")
         (string-tokenize "Help make programs run, run, RUN!"))
  (test* "string-tokenize" '("Help" "make" "programs" "run" "run" "RUN")
         (string-tokenize "Help make programs run, run, RUN!"
                          #[a-zA-Z]))
  (test* "string-tokenize" '("programs" "run" "run" "RUN")
         (string-tokenize "Help make programs run, run, RUN!"
                          #[a-zA-Z] 10))
  (test* "string-tokenize" '("elp" "make" "programs" "run" "run")
         (string-tokenize "Help make programs run, run, RUN!"
                          #[a-z]))

  (test* "string-filter" "rrrr"
         (string-filter #\r "Help make programs run, run, RUN!"))
  (test* "string-filter" "HelpmakeprogramsrunrunRUN"
         (string-filter #[a-zA-Z]
                        "Help make programs run, run, RUN!"))
  (test* "string-filter" "programsrunrun"
         (string-filter (^c (char-lower-case? c))
                        "Help make programs run, run, RUN!" 10))
  (test* "string-filter" ""
         (string-filter (^c (char-lower-case? c)) ""))
  (test* "string-delete" "Help make pogams un, un, RUN!"
         (string-delete #\r "Help make programs run, run, RUN!"))
  (test* "string-delete" "   , , !"
         (string-delete #[a-zA-Z]
                        "Help make programs run, run, RUN!"))
  (test* "string-delete" " , , RUN!"
         (string-delete (^c (char-lower-case? c))
                        "Help make programs run, run, RUN!" 10))
  (test* "string-delete" ""
         (string-delete (^c (char-lower-case? c)) ""))
  )

(test-end)
