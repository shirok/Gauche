;;
;; test for string related functions
;;

(use gauche.test)

(test-start "strings (basic)")

;;-------------------------------------------------------------------
(test-section "builtins")

(test* "string" "abcdefg" (string #\a #\b #\c #\d #\e #\f #\g))
(test* "string" "" (string))
(test* "list->string" "abcdefg"
       (list->string '(#\a #\b #\c #\d #\e #\f #\g)))
(test* "list->string" "" (list->string '()))
(test* "make-string" "aaaaa" (make-string 5 #\a))
(test* "make-string" "" (make-string 0 #\a))

(test* "immutable" #t (string-immutable? "abcde"))
(test* "immutable" #t (string-immutable? ""))
(test* "immutable" #f (string-immutable? (string-copy "abcde")))
(test* "immutable" #f (string-immutable? (string #\a #\b)))
(test* "immutable" #f (string-immutable? (string)))

(test* "string->list" '(#\a #\b #\c #\d #\e #\f #\g)
       (string->list "abcdefg"))
(test* "string->list" '(#\c #\d #\e #\f #\g)
       (string->list "abcdefg" 2)) ;srfi-13 extension
(test* "string->list" '(#\c #\d #\e)
       (string->list "abcdefg" 2 5)) ;srfi-13 extension
(test* "string->list" '(#\a)
       (string->list "abcdefg" 0 1)) ;srfi-13 extension
(test* "string->list" '() (string->list ""))

(test* "string-copy" '("abcde" #f)
       (let* ((x "abcde") (y (string-copy x)))
         (list y (eq? x y))))
(test* "string-copy" "cde" (string-copy "abcde" 2))
(test* "string-copy" "cd"  (string-copy "abcde" 2 4))

(test* "string-ref" #\b (string-ref "abc" 1))
(define x (string-copy "abcde"))
(test* "string-set!" "abZde" (begin (string-set! x 2 #\Z) x))

(test* "string w/ char >= \\x80" #\u00a1
       (string-ref (string #\u00a1) 0))

(test* "string-reader hex-escape" '(1 2 3)
       (let1 s "\x1;\x2;\x00003;"
         (map (^i (char->integer (string-ref s i))) '(0 1 2))))
(test* "string-reader hex-escape legacy support" '(1 2 0 48 48 51)
       (let1 s "\x01\x02\x00003"
         (map (^i (char->integer (string-ref s i))) '(0 1 2 3 4 5))))
(test* "string-reader hex-escape error cases 1 (no hexdigits)"
       (test-error)
       (read-from-string "\"\\x;\""))
(test* "string-reader hex-escape error cases 2 (integer overflow)"
       (test-error)
       (read-from-string "\"\\x11111111111111111111111111111111;\""))
(test* "string-reader hex-escape error cases 3 (out of unicode range)"
       (test-error)
       (read-from-string "\"\\x111111;\""))
(test* "string-reader hex-escape legacy fallback" "\x11;1111"
       (read-from-string "\"\\x111111\""))


(test* "string-fill!" "ZZZZZZ"
       (string-fill! (string-copy "000000") #\Z))
(test* "string-fill!" "000ZZZ"
       (string-fill! (string-copy "000000") #\Z 3))
(test* "string-fill!" "000ZZ0"
       (string-fill! (string-copy "000000") #\Z 3 5))

(test* "string-join" "foo bar baz"
       (string-join '("foo" "bar" "baz")))
(test* "string-join" "foo::bar::baz"
       (string-join '("foo" "bar" "baz") "::"))
(test* "string-join" "foo::bar::baz"
       (string-join '("foo" "bar" "baz") "::" 'infix))
(test* "string-join" ""
       (string-join '() "::"))
(test* "string-join" "foo::bar::baz::"
       (string-join '("foo" "bar" "baz") "::" 'suffix))
(test* "string-join" ""
       (string-join '() "::" 'suffix))
(test* "string-join" "::foo::bar::baz"
       (string-join '("foo" "bar" "baz") "::" 'prefix))
(test* "string-join" ""
       (string-join '() "::" 'prefix))
(test* "string-join" "foo::bar::baz"
       (string-join '("foo" "bar" "baz") "::" 'strict-infix))

(let ()
  (define (test-string-scan out s1 s2 . opt)
    (if (pair? out)
      (begin
        (test* "string-scan" (car out) (apply string-scan s1 s2 opt))
        (test* "string-scan-right" (cadr out)
               (apply string-scan-right s1 s2 opt)))
      (apply test-string-scan (list out out) s1 s2 opt)))
  (define (test-string-scan2 out1 out2 s1 s2 . opt)
    (if (pair? out1)
      (begin
        (test* "string-scan" (list (car out1) (car out2))
               (receive r (apply string-scan s1 s2 opt) r))
        (test* "string-scan-right" (list (cadr out1) (cadr out2))
               (receive r (apply string-scan-right s1 s2 opt) r)))
      (apply test-string-scan2 (list out1 out1) (list out2 out2) s1 s2 opt)))

  (test-string-scan 3 "abcdefghi" "def")
  (test-string-scan 3 "abcdefghi" "def" 'index)
  (test-string-scan 6 "abcdefghi" "ghi")
  (test-string-scan 0 "abcdefghi" "abc")
  (test-string-scan '(0 6) "abcabcabc" "abc")
  (test-string-scan "abc" "abcdefghi" "def" 'before)
  (test-string-scan "ghi" "abcdefghi" "def" 'after)
  (test-string-scan2 "abc" "defghi" "abcdefghi" "def" 'before*)
  (test-string-scan2 "abcdef" "ghi" "abcdefghi" "def" 'after*)
  (test-string-scan2 "abc" "ghi" "abcdefghi" "def" 'both)

  (test-string-scan 4 "abcdefghi" #\e)
  (test-string-scan 8 "abcdefghi" #\i)
  (test-string-scan 0 "abcdefghi" #\a)
  (test-string-scan '(0 8) "abcdefgha" #\a)
  (test-string-scan "abcd" "abcdefghi" #\e 'before)
  (test-string-scan "fghi" "abcdefghi" #\e 'after)
  (test-string-scan2 "abcd" "efghi" "abcdefghi" #\e 'before*)
  (test-string-scan2 "abcde" "fghi" "abcdefghi" #\e 'after*)
  (test-string-scan2 "abcd" "fghi" "abcdefghi" #\e 'both)

  ;; this tests boyer-moore 
  (test-string-scan 216
                    "abracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabr"
                    "abracadabra")
  (test-string-scan '(0 365)
                    "axaxcadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababaxaxax"
                    "axax")

  ;; results differ between leftmost and rightmost
  (test-string-scan '(1 8) "abracadabra" "br")
  (test-string-scan '("acadabra" "a") "abracadabra" "br" 'after)
  (test-string-scan '("a" "abracada") "abracadabra" "br" 'before)
  (test-string-scan2 '("abr" "abracadabr") '("acadabra" "a")
                     "abracadabra" "br" 'after*)
  (test-string-scan2 '("a" "abracada") '("bracadabra" "bra")
                     "abracadabra" "br" 'before*)
  (test-string-scan2 '("a" "abracada") '("acadabra" "a")
                     "abracadabra" "br" 'both)

  ;; edge case
  (test-string-scan '(0 11) "abakjrgaker" "")
  (test-string-scan '("abakjrgaker" "") "abakjrgaker" "" 'after)
  (test-string-scan '("" "abakjrgaker") "abakjrgaker" "" 'before)
  (test-string-scan2 '("" "abakjrgaker") '("abakjrgaker" "")
                     "abakjrgaker" "" 'after*)
  (test-string-scan2 '("" "abakjrgaker") '("abakjrgaker" "")
                     "abakjrgaker" "" 'before*)
  (test-string-scan2 '("" "abakjrgaker") '("abakjrgaker" "")
                     "abakjrgaker" "" 'both)

  ;; incomplete strings
  (test-string-scan 3 #*"abcdefghi" "def")
  (test-string-scan 3 "abcdefghi" #*"def")
  (test-string-scan #*"ghi" #*"abcdefghi" "def" 'after)
  (test-string-scan #*"abc" #*"abcdefghi" "def" 'before)
  (test-string-scan2 #*"abcdef" #*"ghi" #*"abcdefghi" "def" 'after*)
  (test-string-scan2 #*"abc" #*"defghi" #*"abcdefghi" "def" 'before*)
  (test-string-scan2 #*"abc" #*"ghi" #*"abcdefghi" "def" 'both)
  (test-string-scan2 #*"abc" #*"ghi" "abcdefghi" #*"def" 'both)
  (test-string-scan2 #*"abcd" #*"fghi" #*"abcdefghi" #\e 'both)
  )

;;-------------------------------------------------------------------
(test-section "string-split")

(test* "string-split (char)" '("aa" "bbb" "c")
       (string-split "aa*bbb*c" #\*))
(test* "string-split (char)" '("aa" "bbb" "c" "")
       (string-split "aa*bbb*c*" #\*))
(test* "string-split (char)" '("aa" "bbb" "c" "" "")
       (string-split "aa*bbb*c**" #\*))
(test* "string-split (char)" '("aa")
       (string-split "aa" #\*))
(test* "string-split (char)" '("")
       (string-split "" #\*))
(test* "string-split (char)" '("" "")
       (string-split "*" #\*))

(test* "string-split (1-char string)" '("aa" "bbb" "c")
       (string-split "aa*bbb*c" "*"))

(test* "string-split (string)" '("aa" "bbb" "c*c")
       (string-split "aa**bbb**c*c" "**"))
(test* "string-split (string)" '("aa**bbb**c*c")
       (string-split "aa**bbb**c*c" "--"))
(test* "string-split (string)" '("aa" "bbb" "c*c" "")
       (string-split "aa**bbb**c*c**" "**"))
(test* "string-split (string)" '("")
       (string-split "" "**"))
(test* "string-split (string)" '("" "")
       (string-split "**" "**"))

(test* "string-split (regexp)" '("aa" "bbb" "c" "c")
       (string-split "aa--bbb--c-c" #/-+/))
(test* "string-split (regexp)" '("aa" "bbb" "-c-c")
       (string-split "aa--bbb---c-c" #/--/))
(test* "string-split (regexp)" '("" "aa" "bbb" "c" "c" "")
       (string-split "--aa--bbb---c-c-" #/-+/))
(test* "string-split (regexp)" '("--" "--" "---" "-" "-")
       (string-split "--aa--bbb---c-c-" #/\w+/))
(test* "string-split (regexp)" '("--aa--bbb---c-c-")
       (string-split "--aa--bbb---c-c-" #/z+/))
(test* "string-split (regexp)" (test-error) ;; test detection of infinite loop
       (string-split "--aa--bbb---c-c-" #/-*/))

(test* "string-split (charset)" '("aa" "bbb" "c" "d")
       (string-split "aa---bbb***c&d" #[\W]))
(test* "string-split (charset)" '("" "---" "***" "&" "")
       (string-split "aa---bbb***c&d" #[\w]))
(test* "string-split (charset)" '("")
       (string-split "" #[\w]))
(test* "string-split (charset)" '("" "")
       (string-split "a" #[\w]))

(test* "string-split (predicate)" '("" "---" "***" "&" "")
       (string-split "aa---bbb***c&d" char-alphabetic?))

(test-section "string-split(with limit)")

(test* "string-split (char)" '("aa*bbb*c**")
       (string-split "aa*bbb*c**" #\* 0))
(test* "string-split (char)" '("aa" "bbb*c**")
       (string-split "aa*bbb*c**" #\* 1))
(test* "string-split (char)" '("aa" "bbb" "c**")
       (string-split "aa*bbb*c**" #\* 2))

(test* "string-split (char)" '("aa")
       (string-split "aa" #\* 1))
(test* "string-split (char)" '("")
       (string-split "" #\* 2))
(test* "string-split (char)" '("" "")
       (string-split "*" #\* 3))
(test* "string-split (char)" '("*")
       (string-split "*" #\* 0))

(test* "string-split (1-char string)" '("aa" "bbb*c")
       (string-split "aa*bbb*c" "*" 1))
(test* "string-split (1-char string)" '("aa" "bbb" "c")
       (string-split "aa*bbb*c" "*" 2))

(test* "string-split (string)" '("aa**bbb**c*c")
       (string-split "aa**bbb**c*c" "**" 0))
(test* "string-split (string)" '("aa" "bbb**c*c")
       (string-split "aa**bbb**c*c" "**" 1))
(test* "string-split (string)" '("aa" "bbb" "c*c")
       (string-split "aa**bbb**c*c" "**" 2))
(test* "string-split (string)" '("aa" "bbb" "c*c")
       (string-split "aa**bbb**c*c" "**" 3))
(test* "string-split (string)" '("aa**bbb**c*c")
       (string-split "aa**bbb**c*c" "--" 2))
(test* "string-split (string)" '("aa**bbb**c*c**")
       (string-split "aa**bbb**c*c**" "**" 0))
(test* "string-split (string)" '("aa" "bbb**c*c**")
       (string-split "aa**bbb**c*c**" "**" 1))
(test* "string-split (string)" '("aa" "bbb" "c*c**")
       (string-split "aa**bbb**c*c**" "**" 2))
(test* "string-split (string)" '("aa" "bbb" "c*c" "")
       (string-split "aa**bbb**c*c**" "**" 3))
(test* "string-split (string)" '("")
       (string-split "" "**" 0))
(test* "string-split (string)" '("")
       (string-split "" "**" 1))
(test* "string-split (string)" '("**")
       (string-split "**" "**" 0))
(test* "string-split (string)" '("" "")
       (string-split "**" "**" 1))

(test* "string-split (regexp)" '("aa--bbb--c-c")
       (string-split "aa--bbb--c-c" #/-+/ 0))
(test* "string-split (regexp)" '("aa" "bbb--c-c")
       (string-split "aa--bbb--c-c" #/-+/ 1))
(test* "string-split (regexp)" '("aa" "bbb" "c-c")
       (string-split "aa--bbb--c-c" #/-+/ 2))
(test* "string-split (regexp)" '("aa" "bbb" "c" "c")
       (string-split "aa--bbb--c-c" #/-+/ 3))
(test* "string-split (regexp)" '("aa" "bbb" "c" "c")
       (string-split "aa--bbb--c-c" #/-+/ 4))
(test* "string-split (regexp)" '("aa--bbb---c-c")
       (string-split "aa--bbb---c-c" #/--/ 0))
(test* "string-split (regexp)" '("aa" "bbb---c-c")
       (string-split "aa--bbb---c-c" #/--/ 1))
(test* "string-split (regexp)" '("aa" "bbb" "-c-c")
       (string-split "aa--bbb---c-c" #/--/ 2))
(test* "string-split (regexp)" '("aa" "bbb" "-c-c")
       (string-split "aa--bbb---c-c" #/--/ 3))
(test* "string-split (regexp)" '("--aa--bbb---c-c-")
       (string-split "--aa--bbb---c-c-" #/-+/ 0))
(test* "string-split (regexp)" '("" "aa--bbb---c-c-")
       (string-split "--aa--bbb---c-c-" #/-+/ 1))
(test* "string-split (regexp)" '("" "aa" "bbb---c-c-")
       (string-split "--aa--bbb---c-c-" #/-+/ 2))
(test* "string-split (regexp)" '("" "aa" "bbb" "c-c-")
       (string-split "--aa--bbb---c-c-" #/-+/ 3))
(test* "string-split (regexp)" '("" "aa" "bbb" "c" "c-")
       (string-split "--aa--bbb---c-c-" #/-+/ 4))
(test* "string-split (regexp)" '("--aa--bbb---c-c-")
       (string-split "--aa--bbb---c-c-" #/\w+/ 0))
(test* "string-split (regexp)" '("--" "--bbb---c-c-")
       (string-split "--aa--bbb---c-c-" #/\w+/ 1))
(test* "string-split (regexp)" '("--" "--" "---c-c-")
       (string-split "--aa--bbb---c-c-" #/\w+/ 2))
(test* "string-split (regexp)" '("--" "--" "---" "-c-")
       (string-split "--aa--bbb---c-c-" #/\w+/ 3))
(test* "string-split (regexp)" '("--aa--bbb---c-c-")
       (string-split "--aa--bbb---c-c-" #/z+/ 2))
(test* "string-split (regexp)" (test-error) ;; test detection of infinite loop
       (string-split "--aa--bbb---c-c-" #/-*/ 2))
(test* "string-split (bad limit)" (test-error)
       (string-split "--aa--bbb---c-c-" #/-+/ 'a))

;;-------------------------------------------------------------------
(test-section "incomplete strings")

;; Real test for incomplete string requires multibyte strings.
;; Here I only check consistency of combination between complete
;; and incomplete strings.

(test* "string-incomplete?" #f (string-incomplete? "abc"))
(test* "string-incomplete?" #t (string-incomplete? #*"abc"))
(test* "string-incomplete?" #f (string-incomplete? ""))
(test* "string-incomplete?" #t (string-incomplete? #*""))

(test* "string-complete->incomplete" #*"xyz"
       (string-complete->incomplete "xyz"))
(test* "string-complete->incomplete" #*"xyz"
       (string-complete->incomplete #*"xyz"))
(test* "string-incomplete->complete" "xyz"
       (string-incomplete->complete #*"xyz"))
(test* "string-incomplete->complete" "xyz"
       (string-incomplete->complete "xyz"))

(test* "string=?" #t (string=? #*"abc" #*"abc"))

(test* "string-byte-ref" (char->integer #\b)
       (string-byte-ref #*"abc" 1))
(test* "string-byte-ref" 0
       (string-byte-ref #*"\0\0\0" 1))

(test* "string-append" #*"abcdef"
       (string-append "abc" #*"def"))
(test* "string-append" #*"abcdef"
       (string-append #*"abc" "def"))
(test* "string-append" #*"abcdef"
       (string-append #*"abc" #*"def"))
(test* "string-append" #*"abcdef"
       (string-append "a" #*"b" "c" "d" "e" #*"f"))

(test* "string-join" #*"a:b:c"
       (string-join '("a" #*"b" "c") ":"))
(test* "string-join" #*"a:b:c"
       (string-join '("a" "b" "c") #*":"))

;; NB: should we allow this?
(test* "string-set!" #*"abQde"
       (let ((s (string-copy #*"abcde")))
         (string-set! s 2 #\Q)
         s))
(test* "string-byte-set!" #*"abQde"
       (let ((s (string-copy "abcde")))
         (string-byte-set! s 2 (char->integer #\Q))
         s))
(test* "string-byte-set!" #*"abQde"
       (let ((s (string-copy #*"abcde")))
         (string-byte-set! s 2 (char->integer #\Q))
         s))

;; In 0.9.1 this gets an error
(unless (eq? (gauche-character-encoding) 'none)
  (test* "string-byte-set!" (case (gauche-character-encoding)
                              [(utf-8)  #*"\x01\x81\x82"]
                              [(euc-jp) #*"\x01\xa2"]
                              [(sjis)   #*"\x01\xa0"])
         (string-byte-set! (string-copy "\u3042") 0 1)))

(test* "substring" #*"ab"
       (substring #*"abcde" 0 2))

;;-------------------------------------------------------------------
(test-section "string-pointer")

(define sp #f)
(test* "make-string-pointer" #t
       (begin
         (set! sp (make-string-pointer "abcdefg"))
         (string-pointer? sp)))
(test* "string-pointer-next!" #\a
       (string-pointer-next! sp))
(test* "string-pointer-next!" #\b
       (string-pointer-next! sp))
(test* "string-pointer-prev!" #\b
       (string-pointer-prev! sp))
(test* "string-pointer-prev!" #\a
       (string-pointer-prev! sp))
(test* "string-pointer-prev!" #t
       (eof-object? (string-pointer-prev! sp)))
(test* "string-pointer-index" 0
       (string-pointer-index sp))
(test* "string-pointer-index" 7
       (do ((x (string-pointer-next! sp) (string-pointer-next! sp)))
           ((eof-object? x) (string-pointer-index sp))))
(test* "string-pointer-substring" '("abcdefg" "")
       (list (string-pointer-substring sp)
             (string-pointer-substring sp :after #t)))
(test* "string-pointer-substring" '("abcd" "efg")
       (begin
         (string-pointer-set! sp 4)
         (list (string-pointer-substring sp)
               (string-pointer-substring sp :after #t))))
(test* "string-pointer-substring" '("" "abcdefg")
       (begin
         (string-pointer-set! sp 0)
         (list (string-pointer-substring sp)
               (string-pointer-substring sp :after #t))))
(test* "string-pointer-substring" '("" "")
       (let ((sp (make-string-pointer "")))
         (list (string-pointer-substring sp)
               (string-pointer-substring sp :after #t))))

(test* "make-string-pointer (bound)" #t
       (begin
         (set! sp (make-string-pointer "abcdefg" 1 2 5))
         (string-pointer? sp)))
(test* "string-pointer-next! (bound)" #\d
       (string-pointer-next! sp))
(test* "string-pointer-next! (bound)" #\e
       (string-pointer-next! sp))
(test* "string-pointer-next! (bound)" #t
       (eof-object? (string-pointer-next! sp)))
(test* "string-pointer-prev! (bound)" #\e
       (string-pointer-prev! sp))
(test* "string-pointer-prev! (bound)" #\d
       (string-pointer-prev! sp))
(test* "string-pointer-prev! (bound)" #\c
       (string-pointer-prev! sp))
(test* "string-pointer-prev! (bound)" #t
       (eof-object? (string-pointer-prev! sp)))
(test* "string-pointer-next! (bound)" #\c
       (string-pointer-next! sp))
(test* "string-pointer-substring (bound)" '("c" "de")
       (list (string-pointer-substring sp)
             (string-pointer-substring sp :after #t)))

;;-------------------------------------------------------------------
(test-section "input string port")

;; These also tests port's ungetc and scratch buffer, and
;; some special string syntax.

(define istr (open-input-string "abcdefg"))

(test* "read-char" #\a (read-char istr))
(test* "peek-char" #\b (peek-char istr))
(test* "read-byte" 98  (read-byte istr))
(test* "read-byte from ungotten buffer" 99
       (begin (peek-char istr) (read-byte istr)))
(test* "read-block using ungotten buffer" #*"d"
       (begin (peek-char istr) (read-block 1 istr)))
(test* "read-block using ungotten buffer" #*"efg"
       (begin (peek-char istr) (read-block 10 istr)))
(test* "termination" #t
       (eof-object? (read-char istr)))
(test* "termination" #t
       (eof-object? (read-byte istr)))
(test* "termination" #t
       (eof-object? (read-block 3 istr)))

(test* "get-remaining-input-string" "defg"
       (let ((istr (open-input-string "abcdefg")))
         (read-char istr)
         (read-char istr)
         (read-char istr)
         (get-remaining-input-string istr)))
(test* "get-remaining-input-string" ""
       (let ((istr (open-input-string "abcdefg")))
         (read-line istr)
         (get-remaining-input-string istr)))
(test* "get-remaining-input-string" "cdefg"
       (let ((istr (open-input-string "abcdefg")))
         (read-char istr)
         (read-char istr)
         (peek-char istr)
         (get-remaining-input-string istr)))
(test* "get-remaining-input-string" "cdefg"
       (let ((istr (open-input-string "abcdefg")))
         (read-char istr)
         (read-char istr)
         (peek-byte istr)
         (get-remaining-input-string istr)))

(test* "line continuation" "abcdefgh"
       (read (open-input-string "\"abcd\\\n   efgh\"")))
(test* "line continuation" "abcdefgh2"
       (read (open-input-string "\"abcd\\\nefgh2\"")))
(test* "line continuation" "abcdefgh3"
       (read (open-input-string "\"abcd\\\n\t \tefgh3\"")))
(test* "line continuation" "ABCDEFGH"
       (read (open-input-string "\"ABCD\\\r\n   EFGH\"")))
(test* "line continuation" "ABCDEFGH2"
       (read (open-input-string "\"ABCD\\\r\nEFGH2\"")))
(test* "line continuation" "ABCDEFGH3"
       (read (open-input-string "\"ABCD\\\r\n \t EFGH3\"")))
(test* "line continuation" "ABCDefgh"
       (read (open-input-string "\"ABCD\\\r   efgh\"")))
(test* "line continuation" "ABCDefgh2"
       (read (open-input-string "\"ABCD\\\refgh2\"")))
(test* "line continuation" "ABCDefgh3"
       (read (open-input-string "\"ABCD\\ \t \refgh3\"")))
(test* "line continuation" "0123 4567"
       (read (open-input-string "\"0123 \\\n   4567\"")))
(test* "line continuation" "0123 4567"
       (read (open-input-string "\"0123 \\   \n   4567\"")))
(test* "line continuation" "0123-4567"
       (read (open-input-string "\"0123\\\n \\  \n -4567\"")))
(test* "line continuation (invalid)" (test-error)
       (read (open-input-string "\"1234\\ x\"")))

(unless (eq? (gauche-character-encoding) 'none)
  (test* "line continuation (extended chars)" "abc def1"
         (read (open-input-string "\"abc \\\u3000\ndef1\"")))
  (test* "line continuation (extended chars)" "abc def2"
         (read (open-input-string "\"abc \\ \u3000\ndef2\"")))
  (test* "line continuation (extended chars)" "abc def3"
         (read (open-input-string "\"abc \\\u3000 \ndef3\"")))
  (test* "line continuation (extended chars)" "ABC DEF1"
         (read (open-input-string "\"ABC \\\n\u3000 DEF1\"")))
  (test* "line continuation (extended chars)" "ABC DEF2"
         (read (open-input-string "\"ABC \\\n \u3000DEF2\"")))
  (test* "line continuation (extended chars)" "uvw xyz1"
         (read (open-input-string "\"uvw \\\u3000\n\u3000\\\nxyz1\"")))
  )

(define (read-line-tester str)
  (let1 s (open-input-string str)
    (let loop ((l (read-line s))
               (r '()))
      (if (eof-object? l) (reverse r) (loop (read-line s) (cons l r))))))

(test* "read-line (nullstr)" '()
       (read-line-tester ""))
(test* "read-line (NL)" '("")
       (read-line-tester "\n"))
(test* "read-line (CR)" '("")
       (read-line-tester "\r"))
(test* "read-line (CRNL)" '("")
       (read-line-tester "\r\n"))
(test* "read-line (mix)" '("ab" "cd" "" "ef" "g")
       (read-line-tester "ab\rcd\r\r\nef\ng"))
(test* "read-line (ungotten)" '("ab" "cd")
       (let1 s (open-input-string "ab\ncd")
         (let loop ((l (begin (peek-char s) (read-line s)))
                    (r '()))
           (if (eof-object? l) (reverse r) (loop (read-line s) (cons l r))))))

;;-------------------------------------------------------------------
(test-section "output string port")

;; This effectively tests the dynamic string implemenatation.
;; The parameter dstr-init-size and dstr-incr-factor have to
;; match to test boundary conditions.

(define *dstr-init-size* 32)
(define *dstr-incr-factor* 3)

(define (string-port-tester . args)
  (let ((out (open-output-string)))
    (for-each (^s (display s out)) args)
    (get-output-string out)))

(define (test-string-port signature total seg)
  (let* ((repeat (inexact->exact (ceiling (/ total seg))))
         (actual (* seg repeat))
         (result (make-string actual #\?)))
    (test (string-append "string-port " signature)
          #t
          (lambda ()
            (string=? result
                      (apply string-port-tester (make-list repeat (make-string seg #\?))))))))

(define (test-string-ports signature total . segs)
  (test-string-port signature total total)
  (for-each (lambda (seg) (test-string-port signature total seg)) segs))

(test* "string-port (0)" ""
       (string-port-tester))
(test* "string-port (0)" ""
       (string-port-tester "" "" ""))

(test-string-ports "(small-1)" (- *dstr-init-size* 1) 3 2 1)
(test-string-ports "(small)" *dstr-init-size* 3 2 1)
(test-string-ports "(small+1)" (+ *dstr-init-size* 1) 3 2 1)
(test-string-ports "(mid-1)"
                   (- (* *dstr-init-size* (+ *dstr-incr-factor* 1)) 1)
                   (- *dstr-init-size* 1) *dstr-init-size* 3)
(test-string-ports "(mid)"
                   (* *dstr-init-size* (+ *dstr-incr-factor* 1))
                   (- *dstr-init-size* 1) *dstr-init-size* 3)
(test-string-ports "(mid+1)" 
                   (+ (* *dstr-init-size* (+ *dstr-incr-factor* 1)) 1)
                   (- *dstr-init-size* 1) *dstr-init-size* 3)
(test-string-ports "(large)" 10000
                   (- *dstr-init-size* 1) *dstr-init-size*
                   (+ *dstr-init-size* 1)
                   (- (* *dstr-init-size* (+ *dstr-incr-factor* 1)) 1)
                   (* *dstr-init-size* (+ *dstr-incr-factor* 1))
                   )

;;-------------------------------------------------------------------
(test-section "string interpolation")

(test* "string interpolation" "string interpolation"
       (let ((x "inter") (y "polation"))
         #"string ~|x|~|y|"))
(test "string interpolation" "string interpolation"
      (lambda ()
        (define (x) "inter")
        (define (y) "polation")
        #"string ~(x)~(y)"))
(test "string interpolation" "string interpolation"
      (lambda ()
        (define (x a)
          (if a "inter" "polation"))
        #"string ~(x #t)~(x #f)"))

(test-end)
