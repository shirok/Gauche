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
    ("cc=ss"    (arg1 arg2) (format #t "cc=(~a ~a)," arg1 arg2))
    ("ddd=sss"  args       (format #t "ddd=~a," args))
    ("eee=i"    (arg)       (format #t "eee=~s," arg))
    ("ffff=si"  (arg1 arg2) (format #t "ffff=(~a ~s)," arg1 arg2))
    ("ggggg=fff" args        (format #t "ggggg=~s," args))
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

(test "--" '("bb=x," "-a" "-cc") (tester "-bb" "x" "--" "-a" "-cc"))
(test "--" '("" "-bb" "x" "--" "-a" "-cc") (tester "--" "-bb" "x" "--" "-a" "-cc"))

(test "else" '("bb=x,?=what," "x" "y") (tester "-bb" "x" "-what" "x" "y"))
(test "else" '("bb=x,?=what,eee=3," "x" "y")
      (tester "-bb" "x" "-what" "-eee" "3" "x" "y"))
(test "else" '("bb=x,?=what," "q" "-eee" "3" "x" "y")
      (tester "-bb" "x" "-what=q" "-eee" "3" "x" "y"))

(test-end)
