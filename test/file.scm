;;
;; testing file.* modules
;;

(use gauche.test)
(test-start "file utilities")
(use srfi-1)
(use srfi-13)

;;------------------------------------------------------------------
(test-section "file.filter")
(use file.filter)
(test-module 'file.filter)

(sys-unlink "tmp1.o")
(sys-unlink "tmp2.o")
(with-output-to-file "tmp1.o"
  (lambda () (display "aaa bbb ccc ddd\neee fff ggg hhh\n")))

(test* "file.filter tmp1.o -> string"
       "AAA BBB CCC DDDEEE FFF GGG HHH"
       (with-output-to-string
         (lambda ()
           (file-filter (lambda (in out)
                          (port-for-each (lambda (line)
                                           (display (string-upcase line) out))
                                         (lambda () (read-line in))))
                        :input "tmp1.o"))))

(test* "file.filter string -> tmp2.o"
       "AAA BBB CCC DDDEEE FFF GGG HHH"
       (begin
         (with-input-from-string "aaa bbb ccc ddd\neee fff ggg hhh\n"
           (lambda ()
             (file-filter (lambda (in out)
                            (port-for-each (lambda (line)
                                             (display (string-upcase line) out))
                                           (lambda () (read-line in))))
                          :output "tmp2.o")))
         (call-with-input-file "tmp2.o" port->string)))

(sys-unlink "tmp2.o")

(test* "file.filter cleanup" #f
       (with-error-handler
           (lambda (e) (file-exists? "tmp2.o"))
         (lambda ()
           (with-input-from-string "zzz"
             (lambda ()
               (file-filter (lambda (in out) (error "yyy"))
                            :output "tmp2.o"))))))

(sys-unlink "tmp2.o")

(test* "file.filter cleanup" #t
       (with-error-handler
           (lambda (e) (file-exists? "tmp2.o"))
         (lambda ()
           (with-input-from-string "zzz"
             (lambda ()
               (file-filter (lambda (in out) (error "yyy"))
                            :output "tmp2.o"
                            :keep-output? #t))))))

(sys-unlink "tmp2.o")

(test* "file.filter temporary"
       '(#f "AAA BBB CCC DDDEEE FFF GGG HHH")
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
         (list r1 r2)))

(sys-unlink "tmp1.o")
(sys-unlink "tmp2.o")

(test-end)
