;;
;; test for string related functions
;;

(use gauche.test)

(test-start "string")

;;-------------------------------------------------------------------
(test-section "builtins")

(test "string" "abcdefg" (lambda () (string #\a #\b #\c #\d #\e #\f #\g)))
(test "string" "" (lambda () (string)))
(test "list->string" "abcdefg"
      (lambda () (list->string '(#\a #\b #\c #\d #\e #\f #\g))))
(test "list->string" "" (lambda () (list->string '())))
(test "make-string" "aaaaa" (lambda () (make-string 5 #\a)))
(test "make-string" "" (lambda () (make-string 0 #\a)))

(test "immutable" #t (lambda () (string-immutable? "abcde")))
(test "immutable" #t (lambda () (string-immutable? "")))
(test "immutable" #f (lambda () (string-immutable? (string-copy "abcde"))))
(test "immutable" #f (lambda () (string-immutable? (string #\a #\b))))
(test "immutable" #f (lambda () (string-immutable? (string))))

(test "string->list" '(#\a #\b #\c #\d #\e #\f #\g)
      (lambda () (string->list "abcdefg")))
(test "string->list" '(#\c #\d #\e #\f #\g)
      (lambda () (string->list "abcdefg" 2))) ;srfi-13 extension
(test "string->list" '(#\c #\d #\e)
      (lambda () (string->list "abcdefg" 2 5))) ;srfi-13 extension
(test "string->list" '(#\a)
      (lambda () (string->list "abcdefg" 0 1))) ;srfi-13 extension
(test "string->list" '() (lambda () (string->list "")))

(test "string-copy" '("abcde" #f)
      (lambda () (let* ((x "abcde") (y (string-copy x)))
                   (list y (eq? x y)))))
(test "string-copy" "cde" (lambda () (string-copy "abcde" 2)))
(test "string-copy" "cd"  (lambda () (string-copy "abcde" 2 4)))

(test "string-ref" #\b (lambda () (string-ref "abc" 1)))
(define x (string-copy "abcde"))
(test "string-set!" "abZde" (lambda () (string-set! x 2 #\Z) x))

(test "string-fill!" "ZZZZZZ"
      (lambda () (string-fill! (string-copy "000000") #\Z)))
(test "string-fill!" "000ZZZ"
      (lambda () (string-fill! (string-copy "000000") #\Z 3)))
(test "string-fill!" "000ZZ0"
      (lambda () (string-fill! (string-copy "000000") #\Z 3 5)))

(test "string-join" "foo bar baz"
      (lambda () (string-join '("foo" "bar" "baz"))))
(test "string-join" "foo::bar::baz"
      (lambda () (string-join '("foo" "bar" "baz") "::")))
(test "string-join" "foo::bar::baz"
      (lambda () (string-join '("foo" "bar" "baz") "::" 'infix)))
(test "string-join" ""
      (lambda () (string-join '() "::")))
(test "string-join" "foo::bar::baz::"
      (lambda () (string-join '("foo" "bar" "baz") "::" 'suffix)))
(test "string-join" ""
      (lambda () (string-join '() "::" 'suffix)))
(test "string-join" "::foo::bar::baz"
      (lambda () (string-join '("foo" "bar" "baz") "::" 'prefix)))
(test "string-join" ""
      (lambda () (string-join '() "::" 'prefix)))
(test "string-join" "foo::bar::baz"
      (lambda () (string-join '("foo" "bar" "baz") "::" 'strict-infix)))

(test "string-scan" 3 (lambda () (string-scan "abcdefghi" "def")))
(test "string-scan" 3 (lambda () (string-scan "abcdefghi" "def" 'index)))
(test "string-scan" "abc" (lambda () (string-scan "abcdefghi" "def" 'before)))
(test "string-scan" "ghi" (lambda () (string-scan "abcdefghi" "def" 'after)))
(test "string-scan" '("abc" "defghi")
      (lambda ()
        (receive r (string-scan "abcdefghi" "def" 'before*) r)))
(test "string-scan" '("abcdef" "ghi")
      (lambda ()
        (receive r (string-scan "abcdefghi" "def" 'after*) r)))
(test "string-scan" '("abc" "ghi")
      (lambda ()
        (receive r (string-scan "abcdefghi" "def" 'both) r)))

(test "string-scan" 4 (lambda () (string-scan "abcdefghi" #\e)))
(test "string-scan" "abcd" (lambda () (string-scan "abcdefghi" #\e 'before)))
(test "string-scan" "fghi" (lambda () (string-scan "abcdefghi" #\e 'after)))
(test "string-scan" '("abcd" "efghi")
      (lambda ()
        (receive r (string-scan "abcdefghi" #\e 'before*) r)))
(test "string-scan" '("abcde" "fghi")
      (lambda ()
        (receive r (string-scan "abcdefghi" #\e 'after*) r)))
(test "string-scan" '("abcd" "fghi")
      (lambda ()
        (receive r (string-scan "abcdefghi" #\e 'both) r)))

(test "string-scan (boyer-moore)" 216
      (lambda ()
        (string-scan "abracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabrabracadababrabrabr"
                     "abracadabra")))

(test "string-scan (special case)" 0
      (lambda ()
        (string-scan "abakjrgaker" "")))

;;-------------------------------------------------------------------
(test-section "incomplete strings")

;; Real test for incomplete string requires multibyte strings.
;; Here I only check consistency of combination between complete
;; and incomplete strings.

(test "string-incomplete?" #f (lambda () (string-incomplete? "abc")))
(test "string-incomplete?" #t (lambda () (string-incomplete? #"abc")))
(test "string-incomplete?" #f (lambda () (string-incomplete? "")))
(test "string-incomplete?" #t (lambda () (string-incomplete? #"")))

(test "string-complete->incomplete" #"xyz"
      (lambda () (string-complete->incomplete "xyz")))
(test "string-complete->incomplete" #"xyz"
      (lambda () (string-complete->incomplete #"xyz")))
(test "string-incomplete->complete" "xyz"
      (lambda () (string-incomplete->complete #"xyz")))
(test "string-incomplete->complete" "xyz"
      (lambda () (string-incomplete->complete "xyz")))

(test "string=?" #t (lambda () (string=? #"abc" #"abc")))

(test "string-byte-ref" (char->integer #\b)
      (lambda () (string-byte-ref #"abc" 1)))
(test "string-byte-ref" 0
      (lambda () (string-byte-ref #"\0\0\0" 1)))

(test "string-append" #"abcdef"
      (lambda () (string-append "abc" #"def")))
(test "string-append" #"abcdef"
      (lambda () (string-append #"abc" "def")))
(test "string-append" #"abcdef"
      (lambda () (string-append #"abc" #"def")))
(test "string-append" #"abcdef"
      (lambda () (string-append "a" #"b" "c" "d" "e" #"f")))

(test "string-join" #"a:b:c"
      (lambda () (string-join '("a" #"b" "c") ":")))
(test "string-join" #"a:b:c"
      (lambda () (string-join '("a" "b" "c") #":")))

(test "string-scan" 3
      (lambda () (string-scan #"abcdefghi" "def")))
(test "string-scan" 3
      (lambda () (string-scan "abcdefghi" #"def")))
(test "string-scan" '(#"abc" #"ghi")
      (lambda () (receive r (string-scan #"abcdefghi" "def" 'both) r)))
(test "string-scan" '(#"abc" #"ghi")
      (lambda () (receive r (string-scan "abcdefghi" #"def" 'both) r)))
(test "string-scan" '(#"abcd" #"fghi")
      (lambda () (receive r (string-scan #"abcdefghi" #\e 'both) r)))


(test "string-substitute!" #"abCDe"
      (lambda () (string-substitute! (string-copy "abcde") 2 #"CD")))
(test "string-substitute!" #"abCDe"
      (lambda () (string-substitute! (string-copy #"abcde") 2 "CD")))
(test "string-substitute!" #"abCDe"
      (lambda () (string-substitute! (string-copy #"abcde") 2 #"CD")))

;; NB: should we allow this?
(test "string-set!" #"abQde"
      (lambda ()
        (let ((s (string-copy #"abcde")))
          (string-set! s 2 #\Q)
          s)))
(test "string-byte-set!" #"abQde"
      (lambda ()
        (let ((s (string-copy "abcde")))
          (string-byte-set! s 2 (char->integer #\Q))
          s)))
(test "string-byte-set!" #"abQde"
      (lambda ()
        (let ((s (string-copy #"abcde")))
          (string-byte-set! s 2 (char->integer #\Q))
          s)))

(test "substring" #"ab"
      (lambda () (substring #"abcde" 0 2)))

;;-------------------------------------------------------------------
(test-section "string-pointer")

(define sp #f)
(test "make-string-pointer" #t
      (lambda ()
        (set! sp (make-string-pointer "abcdefg"))
        (string-pointer? sp)))
(test "string-pointer-next!" #\a
      (lambda () (string-pointer-next! sp)))
(test "string-pointer-next!" #\b
      (lambda () (string-pointer-next! sp)))
(test "string-pointer-prev!" #\b
      (lambda () (string-pointer-prev! sp)))
(test "string-pointer-prev!" #\a
      (lambda () (string-pointer-prev! sp)))
(test "string-pointer-prev!" #t
      (lambda () (eof-object? (string-pointer-prev! sp))))
(test "string-pointer-index" 0
      (lambda () (string-pointer-index sp)))
(test "string-pointer-index" 7
      (lambda () (do ((x (string-pointer-next! sp) (string-pointer-next! sp)))
                     ((eof-object? x) (string-pointer-index sp)))))
(test "string-pointer-substring" '("abcdefg" "")
      (lambda () (list (string-pointer-substring sp)
                       (string-pointer-substring sp :after #t))))
(test "string-pointer-substring" '("abcd" "efg")
      (lambda ()
        (string-pointer-set! sp 4)
        (list (string-pointer-substring sp)
              (string-pointer-substring sp :after #t))))
(test "string-pointer-substring" '("" "abcdefg")
      (lambda ()
        (string-pointer-set! sp 0)
        (list (string-pointer-substring sp)
              (string-pointer-substring sp :after #t))))
(test "string-pointer-substring" '("" "")
      (lambda ()
        (let ((sp (make-string-pointer "")))
          (list (string-pointer-substring sp)
                (string-pointer-substring sp :after #t)))))

(test "make-string-pointer (bound)" #t
      (lambda ()
        (set! sp (make-string-pointer "abcdefg" 1 2 5))
        (string-pointer? sp)))
(test "string-pointer-next! (bound)" #\d
      (lambda () (string-pointer-next! sp)))
(test "string-pointer-next! (bound)" #\e
      (lambda () (string-pointer-next! sp)))
(test "string-pointer-next! (bound)" #t
      (lambda () (eof-object? (string-pointer-next! sp))))
(test "string-pointer-prev! (bound)" #\e
      (lambda () (string-pointer-prev! sp)))
(test "string-pointer-prev! (bound)" #\d
      (lambda () (string-pointer-prev! sp)))
(test "string-pointer-prev! (bound)" #\c
      (lambda () (string-pointer-prev! sp)))
(test "string-pointer-prev! (bound)" #t
      (lambda () (eof-object? (string-pointer-prev! sp))))
(test "string-pointer-next! (bound)" #\c
      (lambda () (string-pointer-next! sp)))
(test "string-pointer-substring (bound)" '("c" "de")
      (lambda () (list (string-pointer-substring sp)
                       (string-pointer-substring sp :after #t))))

(test-end)
