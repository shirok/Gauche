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

(test "string->list" '(#\a #\b #\c #\d #\e #\f #\g)
      (lambda () (string->list "abcdefg")))
(test "string->list" '() (lambda () (string->list "")))

(test "string-ref" #\b (lambda () (string-ref "abc" 1)))
(define x (string-copy "abcde"))
(test "string-set!" "abZde" (lambda () (string-set! x 2 #\Z) x))

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
