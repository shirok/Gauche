;; Test utility scripts

(use gauche.test)
(use gauche.process)
(use file.util)

(test-start "utility scripts")

(define *nulldev*
  (cond-expand
   [gauche.os.windows "NUL"]
   [else "/dev/null"]))

;;=======================================================================
(test-section "gauche-install")

(define (run-install . args)
  (run-process `("./gosh" "-ftest" "gauche-install.in" ,@args)
               :output *nulldev* :wait #t))

(remove-files "test.o" "test1.o")

(test* "-d" #t
       (begin (run-install "-d" "test1.o/dest")
              (file-is-directory? "test1.o/dest")))

(create-directory-tree "."
                       `(test.o ((bin ((command1 ,(make-string 20000))
                                       (command2 ,(make-string 20000))))
                                 (lib ((lib1 ,(make-string 1000000))
                                       (lib2 ,(make-string 500000))))
                                 (etc ((conf1 ,(make-string 1000)))))))

(test* "file -> file" #t
       (begin (run-install "-m" "444" "test.o/lib/lib1" "test1.o/dest/lib1")
              (and (file-is-regular? "test1.o/dest/lib1")
                   (= (file-perm "test1.o/dest/lib1") #o444)
                   (file-equal? "test.o/lib/lib1" "test1.o/dest/lib1"))))

(remove-files "test1.o/dest/lib1")

(test* "files -> dir" #t
       (begin (run-install "-m" "444" "test.o/lib/lib1" "test.o/lib/lib2"
                           "test1.o/dest")
              (and (file-is-regular? "test1.o/dest/lib1")
                   (file-is-regular? "test1.o/dest/lib2")
                   (= (file-perm "test1.o/dest/lib1")
                      (file-perm "test1.o/dest/lib1")
                      #o444)
                   (file-equal? "test.o/lib/lib1" "test1.o/dest/lib1")
                   (file-equal? "test.o/lib/lib2" "test1.o/dest/lib2"))))

(remove-files "test1.o/dest/lib1" "test1.o/dest/lib2")

(test* "-T" #t
       (begin (run-install "-T" "test1.o/dest" "-m" "555"
                           "test.o/bin/command1"
                           "test.o/bin/command2")
              (and (= (file-perm "test1.o/dest/test.o/bin/command1")
                      (file-perm "test1.o/dest/test.o/bin/command2")
		      (cond-expand
		       [gauche.os.windows #o444]
		       [else #o555]))
                   (file-equal? "test.o/bin/command1"
                                "test1.o/dest/test.o/bin/command1")
                   (file-equal? "test.o/bin/command2"
                                "test1.o/dest/test.o/bin/command2"))))

(test* "-U" #t
       (begin (run-install "-U" "test1.o/dest" "-m" "555"
                           "test.o/bin/command1"
                           "test.o/bin/command2")
              (and (not (file-exists? "test1.o/dest/test.o/bin/command1"))
                   (not (file-exists? "test1.o/dest/test.o/bin/command2")))))

(test* "-T -p" #t
       (begin (run-install "-T" "test1.o/dest" "-m" "555" "-p" "test.o"
                           "test.o/bin/command1"
                           "test.o/bin/command2")
              (and (= (file-perm "test1.o/dest/bin/command1")
                      (file-perm "test1.o/dest/bin/command2")
		      (cond-expand
		       [gauche.os.windows #o444]
		       [else #o555]))
                   (file-equal? "test.o/bin/command1"
                                "test1.o/dest/bin/command1")
                   (file-equal? "test.o/bin/command2"
                                "test1.o/dest/bin/command2"))))

(test* "-U -p" #t
       (begin (run-install "-U" "test1.o/dest" "-m" "555" "-p" "test.o"
                           "test.o/bin/command1"
                           "test.o/bin/command2")
              (and (not (file-exists? "test1.o/dest/bin/command1"))
                   (not (file-exists? "test1.o/dest/bin/command2")))))

(remove-files "test.o" "test1.o")




(test-end)
