;;
;; testing file.* modules
;;

(use gauche.test)
(test-start "file utilities")
(use srfi-13)

;;------------------------------------------------------------------
(test-section "file.util")
(use file.util)

(test "current-directory" '("/" "/" #t #t)
      (lambda ()
        (let* ((cur   (sys-getcwd))
               (root  (begin (current-directory "/")
                             (sys-getcwd)))
               (root* (current-directory))
               (cur*  (begin (current-directory cur)
                             (sys-getcwd)))
               (cur** (current-directory)))
          (list root root* (string=? cur cur*) (string=? cur* cur**)))))

;; prepare test data set
(sys-system "rm -rf test.out")
(sys-system "mkdir test.out")
(with-output-to-file "test.out/test1.o"
  (lambda () (display (make-string 100 #\o))))
(with-output-to-file "test.out/test2.o"
  (lambda () (display (make-string 100 #\o))))
(with-output-to-file "test.out/test3.o"
  (lambda () (display (make-string 100 #\i))))
(with-output-to-file "test.out/test4.o"
  (lambda () (display (make-string 20000 #\i))))
(with-output-to-file "test.out/test5.o"
  (lambda () (display (make-string 20000 #\i))))

(if (symbol-bound? 'sys-symlink)
    (begin
      (sys-symlink "test.out/test1.o" "test.out/test6.o")
      (sys-symlink "test.out/test6.o" "test.out/test7.o"))
    (begin
      (with-output-to-file "test.out/test6.o" (lambda () (newline)))
      (with-output-to-file "test.out/test7.o" (lambda () (newline)))))

(sys-system "mkdir test.out/test.d")
(with-output-to-file "test.out/test.d/test10.o"
  (lambda () (display (make-string 100 #\o))))
(if (symbol-bound? 'sys-symlink)
    (sys-symlink "test.out/test1.o" "test.out/test.d/test11.o")
    (with-output-to-file "test.out/test.d/test11.o" (lambda () (newline))))

(test "directory-list"
      '("." ".." "test.d" "test1.o" "test2.o"
        "test3.o" "test4.o" "test5.o" "test6.o" "test7.o" )
      (lambda () (directory-list "test.out")))

(test "directory-list :children?"
      '("test.d" "test1.o" "test2.o"
        "test3.o" "test4.o" "test5.o" "test6.o" "test7.o" )
      (lambda () (directory-list "test.out" :children? #t)))

(test "directory-list :add-path?"
      '("test.out/." "test.out/.." "test.out/test.d" "test.out/test1.o"
        "test.out/test2.o"  "test.out/test3.o" "test.out/test4.o"
        "test.out/test5.o" "test.out/test6.o" "test.out/test7.o" )
      (lambda () (directory-list "test.out/" :add-path? #t)))

(test "directory-list :filter"
      '("test.out/test1.o"
        "test.out/test2.o"  "test.out/test3.o" "test.out/test4.o"
        "test.out/test5.o" "test.out/test6.o" "test.out/test7.o" )
      (lambda ()
        (directory-list "test.out" :add-path? #t
                        :filter (lambda (p) (string-suffix? "o" p)))))

(test "directory-list2"
      '(("." ".." "test.d")
        ("test1.o" "test2.o" "test3.o" "test4.o"
         "test5.o" "test6.o" "test7.o" ))
      (lambda () (receive x (directory-list2 "test.out") x)))

(test "directory-list2 :add-path"
      '(("test.out/." "test.out/.." "test.out/test.d")
        ("test.out/test1.o" "test.out/test2.o"  "test.out/test3.o"
         "test.out/test4.o" "test.out/test5.o" "test.out/test6.o"
         "test.out/test7.o"))
      (lambda () (receive x (directory-list2 "test.out" :add-path? #t) x)))
        
(test "directory-list2 :children"
      '(("test.out/test.d")
        ("test.out/test1.o" "test.out/test2.o"  "test.out/test3.o"
         "test.out/test4.o" "test.out/test5.o" "test.out/test6.o"
         "test.out/test7.o"))
      (lambda () (receive x (directory-list2 "test.out" :add-path? #t :children? #t) x)))
        
(test "directory-list2 :filter"
      '(("test.d")
        ("test1.o" "test2.o" "test3.o" "test4.o"
         "test5.o" "test6.o" "test7.o" ))
      (lambda ()
        (receive x 
            (directory-list2 "test.out"
                             :filter (lambda (p) (string-contains p "test")))
          x)))

(sys-system "rm -rf test.out")

;;------------------------------------------------------------------
(test-section "file.filter")
(use file.filter)

(sys-unlink "tmp1.o")
(sys-unlink "tmp2.o")
(with-output-to-file "tmp1.o"
  (lambda () (display "aaa bbb ccc ddd\neee fff ggg hhh\n")))

(test "file.filter tmp1.o -> string"
      "AAA BBB CCC DDDEEE FFF GGG HHH"
      (lambda ()
        (with-output-to-string
          (lambda ()
            (file-filter (lambda (in out)
                           (port-for-each (lambda (line)
                                            (display (string-upcase line) out))
                                          (lambda () (read-line in))))
                         :input "tmp1.o")))))

(test "file.filter string -> tmp2.o"
      "AAA BBB CCC DDDEEE FFF GGG HHH"
      (lambda ()
        (with-input-from-string "aaa bbb ccc ddd\neee fff ggg hhh\n"
          (lambda ()
            (file-filter (lambda (in out)
                           (port-for-each (lambda (line)
                                            (display (string-upcase line) out))
                                          (lambda () (read-line in))))
                         :output "tmp2.o")))
        (call-with-input-file "tmp2.o" port->string)))

(sys-unlink "tmp2.o")

(test "file.filter cleanup" #f
      (lambda ()
        (with-error-handler
         (lambda (e)
           (file-exists? "tmp2.o"))
         (lambda ()
           (with-input-from-string "zzz"
             (lambda ()
               (file-filter (lambda (in out) (error "yyy"))
                            :output "tmp2.o")))))))

(sys-unlink "tmp2.o")

(test "file.filter cleanup" #t
      (lambda ()
        (with-error-handler
         (lambda (e)
           (file-exists? "tmp2.o"))
         (lambda ()
           (with-input-from-string "zzz"
             (lambda ()
               (file-filter (lambda (in out) (error "yyy"))
                            :output "tmp2.o"
                            :keep-output? #t)))))))

(sys-unlink "tmp2.o")

(test "file.filter temporary"
      '(#f "AAA BBB CCC DDDEEE FFF GGG HHH")
      (lambda ()
        (let* ((r1
                (with-input-from-string "aaa bbb ccc ddd\neee fff ggg hhh\n"
                  (lambda ()
                    (file-filter
                     (lambda (in out)
                       (port-for-each (lambda (line)
                                        (display (string-upcase line) out))
                                      (lambda () (read-line in)))
                       (file-exists? "tmp2.o"))
                     :output "tmp2.o"
                     :temporary-file "foo"))))
               (r2
                (call-with-input-file "tmp2.o" port->string)))
          (list r1 r2))))

(sys-unlink "tmp1.o")
(sys-unlink "tmp2.o")

(test-end)
