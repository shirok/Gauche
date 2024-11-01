;;
;; test for parseopt
;;

(use gauche.test)

(test-start "parseopt")
(use gauche.parseopt)
(test-module 'gauche.parseopt)

(define (help) (display "Help message"))

(define parser
  (make-option-parser
   (("help" => help)
    ("a"        ()          (format #t "a,"))
    ("bb=s"     (arg)       (format #t "bb=~a," arg))
    ("b=s"      (arg)       (format #t "b=~a," arg))
    ("cc=ss"    (arg1 arg2) (format #t "cc=(~a ~a)," arg1 arg2))
    ("ddd=sss"  args        (format #t "ddd=~a," args))
    ("eee=i"    (arg)       (format #t "eee=~s," arg))
    ("ffff=si"  (arg1 arg2) (format #t "ffff=(~a ~s)," arg1 arg2))
    ("ggggg=fff" args       (format #t "ggggg=~s," args))
    ("h|hh|hhh" ()          (format #t "h*,"))
    ("j:s"      (arg)       (format #t "j=~a," arg))
    ("k:sss"    args        (format #t "k=~a," args))
    (else (option args looper)
          (format #t "?=~a," option)
          (looper args))
    )))

(define (tester . cmdargs)
  (lambda ()
    (let* ((restarg '())
           (output (with-output-to-string
                     (lambda ()
                       (set! restarg (parser cmdargs))))))
      (cons output restarg))))

(test "help" '("Help message")
      (tester "-help"))

(test "help" '("Help message")
      (tester "--help"))

(test "help" '("Help message" "x" "y")
      (tester "-help" "x" "y"))

(test "-a" '("a," "x" "y")     (tester "-a" "x" "y"))
(test "--a" '("" "x" "-a" "y")  (tester "x" "-a" "y"))
(test "-bb" '("bb=x," "y")  (tester "-bb" "x" "y"))
(test "--bb" '("bb=x," "y")  (tester "--bb" "x" "y"))
(test "-bb" '("bb=x," "y")  (tester "-bb=x" "y"))
(test "-bb" '("bb=x," "y")  (tester "-bb=x" "y"))
(test "-a -bb" '("a,bb=x," "y") (tester "-a" "-bb" "x" "y"))
(test "-bb -a" '("bb=x,a," "y") (tester "-bb" "x" "-a" "y"))
(test "-bb -a" '("bb=-a," "x" "y") (tester "-bb" "-a" "x" "y"))

(test "-cc" '("bb=x,cc=(y z),") (tester "-bb" "x" "-cc" "y" "z"))
(test "-cc" '("cc=(y -bb)," "x" "z") (tester "-cc" "y" "-bb" "x" "z"))
(test "-cc" '("cc=(y -bb)," "x" "z") (tester "-cc=y" "-bb" "x" "z"))

(test "-ddd" '("ddd=(x y z),") (tester "-ddd" "x" "y" "z"))
(test "-ddd" '("ddd=(x y z),") (tester "--ddd=x" "y" "z"))

(test "-eee" '("eee=23," "x") (tester "-eee" "23" "x"))
(test "-eee" '("eee=23," "x") (tester "-eee" "023" "x"))
(test "-eee" '("eee=-23," "x") (tester "-eee" "-23" "x"))
(test "-eee" '("eee=23," "x") (tester "-eee" "#x17" "x"))
(test "-eee" '("eee=23," "x") (tester "-eee" "#o27" "x"))
(test "-eee" '("eee=23," "x") (tester "-eee" "#b10111" "x"))

(test "-ffff" '("ffff=(-a -3),") (tester "--ffff" "-a" "-3"))
(test "-ffff" '("ffff=(-a -3),") (tester "-ffff" "-a" "-03"))
(test "-ffff" '("ffff=(-03 -3),") (tester "--ffff" "-03" "-03"))

(test "-ggggg" '("ggggg=(1.0 2.0 3.0),") (tester "-ggggg" "1.0" "2.0" "3.0"))

(test "-h"   '("h*,") (tester "-h"))
(test "-hh"  '("h*,") (tester "-hh"))
(test "-hhh" '("h*,") (tester "-hhh"))

(test "-j"   '("j=jj,a,") (tester "-j" "jj" "-a"))
(test "-j"   '("j=#f,a,") (tester "-j" "-a"))
(test "-j"   '("j=#f,")  (tester "-j"))
(test "-k"   '("k=(1 2 3),a,") (tester "-k" "1" "2" "3" "-a"))
(test "-k"   '("k=(#f #f #f),a,") (tester "-k" "-a"))

(test "-b 1"   '("b=abc,") (tester "-b" "abc"))
(test "-b 2"   '("b=abc,") (tester "-b=abc"))
(test "-b 3"   '("b=abc,") (tester "-babc"))
(test "-b (long option match)"   '("bb=abc,") (tester "-bb" "abc"))

(test "--" '("bb=x," "-a" "-cc") (tester "-bb" "x" "--" "-a" "-cc"))
(test "--" '("" "-bb" "x" "--" "-a" "-cc") (tester "--" "-bb" "x" "--" "-a" "-cc"))

(test "else" '("bb=x,?=what," "x" "y") (tester "-bb" "x" "-what" "x" "y"))
(test "else" '("bb=x,?=what,eee=3," "x" "y")
      (tester "-bb" "x" "-what" "-eee" "3" "x" "y"))
(test "else" '("bb=x,?=what," "q" "-eee" "3" "x" "y")
      (tester "-bb" "x" "-what=q" "-eee" "3" "x" "y"))

(test* "let-args (foo)" 9
       (let-args '() ((foo "foo=n" 9)) foo))

(test* "let-args (foo)" #f
       (let-args '() ((foo "foo=n")) foo))

(test* "let-args (foo)" 3
       (let-args '("--foo" "3") ((foo "foo=n" 9)) foo))

(test* "let-args (foo)" (test-error <parseopt-error>)
       (let-args '("--foof" "3") ((foo "foo=n" 9)) foo))

(test* "let-args (foo)" 3
       (let-args '("--foo" "3") ((bar "bar") (foo "foo=n" 9)) foo))

(test* "let-args (bar)" #t
       (let-args '("--bar") ((bar "bar") (foo "foo=n" 9)) bar))

(test* "let-args (bar)" #t
       (let-args '("--bar") ((foo "foo=n" 9) (bar "bar")) bar))

(test* "let-args (bar)" #f
       (let-args '("--foo" "3") ((foo "foo=n" 9) (bar "bar")) bar))

(test* "let-args (baz)" '("4" 2)
       (let-args '("--foo" "3" "--baz" "4" "2")
           ((foo "foo=n" 9)
            (bar "bar")
            (baz "baz=si"))
         baz))

(test* "let-args (baz)" (test-error <parseopt-error>)
       (let-args '("--foo" "3" "--baz" "4")
           ((foo "foo=n" 9)
            (bar "bar")
            (baz "baz=si"))
         baz))

(test* "let-args (baz)" #f
       (let-args '("--foo" "3" "--bar")
           ((foo "foo=n" 9)
            (bar "bar")
            (baz "baz=si"))
         baz))

(test* "let-args (rest)" '("bunga" "bonga")
       (let-args '("--foo" "3" "--bar" "bunga" "bonga")
           ((foo "foo=n" 9)
            (bar "bar")
            (baz "baz=si")
            . rest)
         rest))

(test* "let-args (rest)" '("bunga" "bonga")
       (let-args '("bunga" "bonga")
           ((foo "foo=n" 9)
            (bar "bar")
            (baz "baz=si")
            . rest)
         rest))

(test* "let-args (rest)" '()
       (let-args '()
           ((foo "foo=n" 9)
            (bar "bar")
            (baz "baz=si")
            . rest)
         rest))

(test* "let-args (else)" '("foo" ("5"))
       (call/cc
        (lambda (ret)
          (let-args '("-foo" "5")
              ((else (opt args cont) (ret (list opt args))))
            #f))))

(test* "let-args (else)" 5
       (call/cc
        (lambda (ret)
          (let-args '("-foo" "5")
              ((bar  "bar=i")
               (else (opt args cont) (cont (cons "-bar" args))))
            bar))))

(test* "let-args (callback)" 25
       (let-args '("-foo" "5")
           ((foo "foo=n" => (^x (* x x)))
            (bar "bar"))
         foo))

(test* "let-args (callback)" #f
       (let-args '()
           ((foo "foo=n" => (^x (* x x)))
            (bar "bar"))
         foo))

(test* "let-args (callback)" 8
       (let-args '()
           ((foo "foo=n" 8 => (^x (* x x)))
            (bar "bar"))
         foo))

(test* "let-args (side-effect)" 5
       (let ((boo 0))
         (let-args '("-foo" "5")
             ((#f "foo=n" => (^x (set! boo x)))
              (bar "bar"))
           boo)))

(test* "let-args (side-effect)" 0
       (let ((boo 0))
         (let-args '("-bar")
             ((#f "foo=n" => (^x (set! boo x)))
              (#f "bar"))
           boo)))

(test* "let-args (scope)" 7
       (let ((foo 7))
         (let-args '("-foo" "6")
             ((foo "foo=n")
              (bar "bar=n" foo))
           bar)))

(test* "let-args (scope)" 8
       (let ((x 8))
         (let-args '("-foo")
             ((x   "x")
              (foo "foo" => (lambda () x)))
           foo)))

(test* "let-args (scope)" 9
       (let ((x 9))
         (call/cc
          (lambda (ret)
            (let-args '("-foo")
                ((x  "x")
                 (else _ (ret x)))
              11)))))

(let ()
  (define (run args)
    (let-args args
        ((a "a*")
         (b "b*=s"))
      (list a b)))

  (test* "plural args" '(() ())
         (run '()))
  (test* "plural args" '((#t) ("foo"))
         (run '("-a" "-b" "foo")))
  (test* "plural args" '((#t #t) ("foo" "bar"))
         (run '("-a" "-b" "foo" "-a" "-b" "bar")))
  )

(let ()
  (define (run args help-string-args)
    (let/cc ret
      (let-args args
          ((a "a" ? "An option with no arg")
           (b "b=s{PARAM_B}" ? "Taking string argument {PARAM_B}.")
           (c "c=s{PARAM_C}" "wow" ? "Taking string argument and default. \
                             And this is a long help string that will span \
                             more than one line.")
           (d "d" => (^[] 'yo) ? "Callback, no default")
           (e "e" 'boo => (^[] 'yay) ? "Callback, default")
           (f "f|floccinaucinihilipilification=s" ? "Long option name")
           (g "g=i{PARAM_G1}i{PARAM_G2}" => (^[g1 g2] (list g1 g2))
              ? "Taking two integer arguments, {PARAM_G1} and {PARAM_G2}.")
           (h "h")                      ;no help string
           (else => (^[badopt rest loop]
                      (ret (string-append "Unknown option: " badopt "\n"
                                          "Options:\n"
                                          (apply option-parser-help-string
                                                 help-string-args))))))
        (list a b c d e f g))))

  (define (t args expect)
    (test* #"help string ~|args|" expect (run args '())))

  ;; Just to make sure help string doesn't affect normal operations.
  (t '()                     '(#f #f "wow" #f boo #f #f))
  (t '("-a")                 '(#t #f "wow" #f boo #f #f))
  (t '("-b" "bee")           '(#f "bee" "wow" #f boo #f #f))
  (t '("-c" "sea")           '(#f #f "sea" #f boo #f #f))
  (t '("-d")                 '(#f #f "wow" yo boo #f #f))
  (t '("-e")                 '(#f #f "wow" #f yay #f #f))
  (t '("-f" "foo")           '(#f #f "wow" #f boo "foo" #f))

  ;; And now, help string generation
  ($ test*/diff "help string generation 1"
     '("Unknown option: zzz"
       "Options:"
       "  -a           An option with no arg"
       "  -b PARAM_B   Taking string argument PARAM_B."
       "  -c PARAM_C   Taking string argument and default. And this is a long help"
       "               string that will span more than one line."
       "  -d           Callback, no default"
       "  -e           Callback, default"
       "  -f, --floccinaucinihilipilification s"
       "               Long option name"
       "  -g PARAM_G1 PARAM_G2"
       "               Taking two integer arguments, PARAM_G1 and PARAM_G2."
       "  -h           (No help available)")
     (run '("--zzz") '()))

  ($ test*/diff "help string generation 2"
     '("Unknown option: zzz"
       "Options:"
       "    -a                   An option with no arg"
       "    -b PARAM_B           Taking string argument PARAM_B."
       "    -c PARAM_C           Taking string argument and default."
       "                         And this is a long help string that"
       "                         will span more than one line."
       "    -d                   Callback, no default"
       "    -e                   Callback, default"
       "    -f, --floccinaucinihilipilification s"
       "                         Long option name"
       "    -g PARAM_G1 PARAM_G2 Taking two integer arguments,"
       "                         PARAM_G1 and PARAM_G2.")
     (run '("--zzz") '(:omit-options-without-help #t
                       :option-indent 4
                       :description-indent 25
                       :width 60)))
  )

(test-end)
