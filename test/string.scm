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

(test-end)
