;;
;; testing file.* modules
;;

(use gauche.test)
(test-start "file utilities")
(use srfi-1)
(use srfi-13)

;; NB: copy-file in file.util uses copy-port, which requires gauche.uvector.
;; To allow in-place test, we pre-loads gauche.uvector.
(when (file-exists? "../ext/uvector/uvector.scm")
  (eval '(begin (add-load-path "../ext/uvector/")
                (load "uvector"))
        (interaction-environment))
  (eval '(import gauche.uvector) (interaction-environment)))

;;------------------------------------------------------------------
(test-section "file.util")
(use file.util)
(test-module 'file.util)

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
      (sys-symlink "test1.o" "test.out/test6.o")
      (sys-symlink "test6.o" "test.out/test7.o")
      (sys-symlink "test.d" "test.out/test2.d"))
    (begin
      (with-output-to-file "test.out/test6.o" (lambda () (newline)))
      (with-output-to-file "test.out/test7.o" (lambda () (newline)))
      (sys-mkdir "test.out/test2.d" #o777)))

(sys-system "mkdir test.out/test.d")
(with-output-to-file "test.out/test.d/test10.o"
  (lambda () (display (make-string 100 #\o))))
(if (symbol-bound? 'sys-symlink)
    (sys-symlink "../test1.o" "test.out/test.d/test11.o")
    (with-output-to-file "test.out/test.d/test11.o" (lambda () (newline))))

(test* "directory-list"
       '("." ".." "test.d" "test1.o" "test2.d" "test2.o"
         "test3.o" "test4.o" "test5.o" "test6.o" "test7.o" )
       (directory-list "test.out"))

(test* "directory-list :children?"
       '("test.d" "test1.o" "test2.d" "test2.o"
         "test3.o" "test4.o" "test5.o" "test6.o" "test7.o" )
       (directory-list "test.out" :children? #t))

(test* "directory-list :add-path?"
       '("test.out/." "test.out/.." "test.out/test.d" "test.out/test1.o"
         "test.out/test2.d" "test.out/test2.o"  "test.out/test3.o"
         "test.out/test4.o" "test.out/test5.o" "test.out/test6.o"
         "test.out/test7.o" )
       (directory-list "test.out/" :add-path? #t))

(test* "directory-list :filter"
       '("test.out/test1.o"
         "test.out/test2.o"  "test.out/test3.o" "test.out/test4.o"
         "test.out/test5.o" "test.out/test6.o" "test.out/test7.o" )
       (directory-list "test.out" :add-path? #t
                       :filter (lambda (p) (string-suffix? "o" p))))

(test* "directory-list :filter"
       '("test.out/test1.o"
         "test.out/test2.o"  "test.out/test3.o" "test.out/test4.o"
         "test.out/test5.o" "test.out/test6.o" "test.out/test7.o" )
       (directory-list "test.out" :add-path? #t :filter-add-path? #t
                       :filter file-is-regular?))

(test* "directory-list2"
       '(("." ".." "test.d" "test2.d")
         ("test1.o" "test2.o" "test3.o" "test4.o"
          "test5.o" "test6.o" "test7.o" ))
       (receive x (directory-list2 "test.out") x))

(test* "directory-list2 :add-path"
       '(("test.out/." "test.out/.." "test.out/test.d" "test.out/test2.d")
         ("test.out/test1.o" "test.out/test2.o"  "test.out/test3.o"
          "test.out/test4.o" "test.out/test5.o" "test.out/test6.o"
          "test.out/test7.o"))
       (receive x (directory-list2 "test.out" :add-path? #t) x))

(test* "directory-list2 :children"
       '(("test.out/test.d" "test.out/test2.d")
         ("test.out/test1.o" "test.out/test2.o"  "test.out/test3.o"
          "test.out/test4.o" "test.out/test5.o" "test.out/test6.o"
          "test.out/test7.o"))
       (receive x (directory-list2 "test.out" :add-path? #t :children? #t) x))

(test* "directory-list2 :filter"
       '(("test.d" "test2.d")
         ("test1.o" "test2.o" "test3.o" "test4.o"
          "test5.o" "test6.o" "test7.o" ))
       (receive x 
           (directory-list2 "test.out"
                            :filter (lambda (p) (string-contains p "test")))
         x))

(when (symbol-bound? 'sys-symlink)
  (test* "directory-list2 :follow-link? #f"
         '(("test.d")
           ("test1.o" "test2.d" "test2.o" "test3.o" "test4.o"
            "test5.o" "test6.o" "test7.o" ))
         (receive x (directory-list2 "test.out" :follow-link? #f :children? #t)
           x)
         ))

(test* "directory-fold"
       (if (symbol-bound? 'sys-symlink)
           '("test.out/test.d/test10.o" "test.out/test.d/test11.o"
             "test.out/test1.o"
             "test.out/test2.d/test10.o" "test.out/test2.d/test11.o"
             "test.out/test2.o" "test.out/test3.o"
             "test.out/test6.o" "test.out/test7.o")
           '("test.out/test.d/test10.o" "test.out/test.d/test11.o"
             "test.out/test1.o"
             "test.out/test2.o" "test.out/test3.o"
             "test.out/test6.o" "test.out/test7.o"))
       (reverse
        (directory-fold "test.out"
                        (lambda (path result)
                          (if (= (file-size path) 100)
                              (cons path result)
                              result))
                        '()))
       )

(when (symbol-bound? 'sys-symlink)
  (test* "directory-fold :follow-link? #f"
         '("test.out/test.d/test10.o" "test.out/test.d/test11.o"
           "test.out/test1.o"
           "test.out/test2.o" "test.out/test3.o"
           "test.out/test6.o" "test.out/test7.o")
         (reverse 
          (directory-fold "test.out"
                          (lambda (path result)
                            (if (= (file-size path) 100)
                                (cons path result)
                                result))
                          '()
                          :follow-link? #f)))
  ;; this tests dangling symlink
  (sys-symlink "foo" "test.out/test.dangling")
  (test* "directory-fold :follow-link? #t; dangling link"
         '("test.out/test.d/test10.o" "test.out/test.d/test11.o"
           "test.out/test.dangling" "test.out/test1.o"
           "test.out/test2.d/test10.o" "test.out/test2.d/test11.o"
           "test.out/test2.o" "test.out/test3.o" "test.out/test4.o"
           "test.out/test5.o" "test.out/test6.o" "test.out/test7.o")
         (sort (directory-fold "test.out" cons '())))
  (sys-unlink "test.out/test.dangling")
  )



(test* "directory-fold :lister"
       '("test.out/test.d/test10.o" "test.out/test.d/test11.o" "test.out/test1.o")
       (reverse
        (directory-fold "test.out" cons '()
                        :follow-link? #f
                        :lister (lambda (dir knil)
                                  (receive (dirs files)
                                      (directory-list2 dir :add-path? #t :children? #t :follow-link? #f)
                                    (append dirs
                                            (filter (cut string-contains <> "test1")
                                                    files))))))
       )

(test* "build-path" "" (build-path ""))
(test* "build-path" "." (build-path "."))
(test* "build-path" "/" (build-path "/"))
(test* "build-path" "a/b/c" (build-path "a" "b" "c"))
(test* "build-path" "a/b/c" (build-path "a/" "b/" "c"))
(test* "build-path" "/a/b/c" (build-path "/" "a/b" "c"))
(test* "build-path" "./a/b/c" (build-path "." "a/b" "c"))
(test* "build-path" *test-error* (build-path "." "/a/b"))
(test* "build-path" "foo" (build-path "" "foo"))
(test* "build-path" "foo/bar" (build-path "" "foo" "" "bar"))
(test* "build-path" "foo" (build-path "" "foo" ""))

(test* "resolve-path" "/" (resolve-path "/"))
(test* "resolve-path" "." (resolve-path "."))
(test* "resolve-path" "test.out" (resolve-path "test.out"))
(when (symbol-bound? 'sys-symlink)
  (test* "resolve-path" "test.out/test1.o"
         (resolve-path "test.out/test6.o"))
  (test* "resolve-path" "test.out/test1.o"
         (resolve-path "test.out/test7.o"))
  (test* "resolve-path" "test.out/test1.o"
         (resolve-path "test.out/test2.d/test11.o"))
  (test* "resolve-path" "test.out/test1.o"
         (resolve-path "test.out/test2.d/../test.d/test11.o"))
  (test* "resolve-path" "test.out/test1.o"
         (resolve-path "test.out/test.d/../test2.d/test11.o"))
  )

(test* "decompose-path" '("/a/b/c" "d" "e")
       (receive r (decompose-path "/a/b/c/d.e") r))
(test* "decompose-path" '("." "d" "e")
       (receive r (decompose-path "d.e") r))
(test* "decompose-path" '("." "d" "")
       (receive r (decompose-path "d.") r))
(test* "decompose-path" '("." "d" #f)
       (receive r (decompose-path "d") r))
(test* "decompose-path" '("/a.b" "c" #f)
       (receive r (decompose-path "/a.b/c") r))
(test* "decompose-path" '("/a.b" #f #f)
       (receive r (decompose-path "/a.b/") r))
(test* "decompose-path" '("/a.b" "c.c" "d")
       (receive r (decompose-path "/a.b/c.c.d") r))
(test* "decompose-path" '("/a.b" ".d" #f)
       (receive r (decompose-path "/a.b/.d") r))

(test* "path-extension" "c" (path-extension "/a.b/c.d/e.c"))
(test* "path-extension" "" (path-extension "/a.b/c.d/e.c."))
(test* "path-extension" #f (path-extension "/a.b/c.d/.e"))
(test* "path-extension" #f (path-extension "/a.b/c.d/e"))

(test* "path-sans-extension" "/a.b/c.d/e"
       (path-sans-extension "/a.b/c.d/e.c"))
(test* "path-sans-extension" "/a.b/c.d/e.c"
       (path-sans-extension "/a.b/c.d/e.c."))
(test* "path-sans-extension" "/a.b/c.d/.e"
       (path-sans-extension "/a.b/c.d/.e"))
(test* "path-sans-extension" "/a.b/c.d/e"
       (path-sans-extension "/a.b/c.d/e"))

(test* "path-swap-extension" "/a.b/c.d/e.o"
       (path-swap-extension "/a.b/c.d/e.c" "o"))
(test* "path-swap-extension" "/a.b/c.d/e.c.o"
       (path-swap-extension "/a.b/c.d/e.c." "o"))
(test* "path-swap-extension" "/a.b/c.d/.e.o"
       (path-swap-extension "/a.b/c.d/.e" "o"))
(test* "path-swap-extension" "/a.b/c.d/e.o"
       (path-swap-extension "/a.b/c.d/e" "o"))
(test* "path-swap-extension" "/a.b/c.d/e"
       (path-swap-extension "/a.b/c.d/e.c" #f))

(test* "file-type" #f
       (file-type "nonexistent/file"))
(test* "file-type" '(directory directory regular)
       (map file-type
            '("test.out/test.d" "test.out/test2.d" "test.out/test1.o")))
(when (symbol-bound? 'sys-symlink)
  (test* "file-type :follow-link? #f" '(directory symlink regular)
         (map (cut file-type <> :follow-link? #f)
              '("test.out/test.d" "test.out/test2.d" "test.out/test1.o")))
  )

(test* "file-eq?" #t
       (file-eq? "test.out/test1.o" "test.out/test1.o"))
(when (symbol-bound? 'sys-symlink)
  (test* "file-eq? (symlink)" #f
         (file-eq? "test.out/test1.o" "test.out/test7.o")))
(test* "file-eq?" #f
       (file-eq? "test.out/test1.o" "test.out/test2.o"))
(test* "file-eqv?" #t
       (file-eqv? "test.out/test1.o" "test.out/test1.o"))
(when (symbol-bound? 'sys-symlink)
  (test* "file-eqv? (symlink)" #t
         (file-eqv? "test.out/test1.o" "test.out/test7.o"))
  (test* "file-eqv? (symlink)" #t
         (file-eqv? "test.out/test1.o" "test.out/test.d/test11.o")))
(test* "file-eqv?" #f
       (file-eqv? "test.out/test1.o" "test.out/test2.o"))
(test* "file-equal?" #t      
       (file-equal? "test.out/test1.o" "test.out/test1.o"))
(test* "file-equal?" #t      
       (file-equal? "test.out/test1.o" "test.out/test2.o"))
(test* "file-equal?" #f
       (file-equal? "test.out/test1.o" "test.out/test4.o"))
(test* "file-equal?" #t
       (file-equal? "test.out/test4.o" "test.out/test5.o"))

(test* "touch-file" #t
       (and (not (file-exists? "test.out/touched"))
            (begin (touch-file "test.out/touched")
                   (file-exists? "test.out/touched"))))

(test* "copy-file (normal)" #t
       (and (copy-file "test.out/test5.o" "test.out/test.copy")
            (not (file-eq? "test.out/test5.o" "test.out/test.copy"))
            (file-equal? "test.out/test5.o" "test.out/test.copy")))

(test* "copy-file (:if-exists :error)" *test-error*
       (copy-file "test.out/test5.o" "test.out/test.copy"))

(test* "copy-file (:if-exists #f)" #f
       (copy-file "test.out/test5.o" "test.out/test.copy" :if-exists #f))

(test* "copy-file (:if-exists :supersede)" #t
       (and (copy-file "test.out/test1.o" "test.out/test.copy"
                       :if-exists :supersede)
            (file-equal? "test.out/test1.o" "test.out/test.copy")
            (not (file-exists? "test.out/test.copy.orig"))))

(test* "copy-file (:if-exists :backup)" #t
       (and (copy-file "test.out/test5.o" "test.out/test.copy"
                       :if-exists :backup)
            (file-equal? "test.out/test5.o" "test.out/test.copy")
            (file-equal? "test.out/test1.o" "test.out/test.copy.orig")))

(test* "copy-file (:if-exists :backup)" #t
       (and (copy-file "test.out/test1.o" "test.out/test.copy"
                       :if-exists :backup :backup-suffix "~")
            (file-equal? "test.out/test1.o" "test.out/test.copy")
            (file-equal? "test.out/test5.o" "test.out/test.copy~")))

(sys-unlink "test.out/test.copy")
(sys-unlink "test.out/test.copy~")
(sys-unlink "test.out/test.copy.orig")

(test* "copy-file (normal, safe)" #t
       (and (copy-file "test.out/test5.o" "test.out/test.copy" :safe #t)
            (not (file-eq? "test.out/test5.o" "test.out/test.copy"))
            (file-equal? "test.out/test5.o" "test.out/test.copy")))

(test* "copy-file (:if-exists :error, safe)" *test-error*
       (copy-file "test.out/test5.o" "test.out/test.copy" :safe #t))

(test* "copy-file (:if-exists #f, safe)" #f
       (copy-file "test.out/test5.o" "test.out/test.copy" :if-exists #f :safe #t))

(test* "copy-file (:if-exists :supersede, safe)" #t
       (and (copy-file "test.out/test1.o" "test.out/test.copy"
                       :if-exists :supersede :safe #t)
            (file-equal? "test.out/test1.o" "test.out/test.copy")
            (not (file-exists? "test.out/test.copy.orig"))))

(test* "copy-file (:if-exists :backup, safe)" #t
       (and (copy-file "test.out/test5.o" "test.out/test.copy"
                       :if-exists :backup :safe #t)
            (file-equal? "test.out/test5.o" "test.out/test.copy")
            (file-equal? "test.out/test1.o" "test.out/test.copy.orig")))

(test* "copy-file (:if-exists :backup, safe)" #t
       (and (copy-file "test.out/test1.o" "test.out/test.copy"
                       :if-exists :backup :backup-suffix "~" :safe #t)
            (file-equal? "test.out/test1.o" "test.out/test.copy")
            (file-equal? "test.out/test5.o" "test.out/test.copy~")))

(test* "copy-file (same file)" *test-error*
       (copy-file "test.out/test.copy" "test.out/test.copy"
                  :if-exists :supersede))

(test* "move-file (normal)" #t
       (and (move-file "test.out/test.copy" "test.out/test.move")
            (not (file-exists? "test.out/test.copy"))
            (file-equal? "test.out/test1.o" "test.out/test.move")))

(test* "move-file (:if-exists :error)" *test-error*
       (move-file "test.out/test5.o" "test.out/test.move"))

(test* "move-file (:if-exists :supersede)" #t
       (and (move-file "test.out/test5.o" "test.out/test.move"
                       :if-exists :supersede)
            (not (file-exists? "test.out/test5.o"))
            (not (file-equal? "test.out/test1.o" "test.out/test.move"))))

(test* "move-file (:if-exists :backup)" #t
       (and (move-file "test.out/test1.o" "test.out/test.move"
                       :if-exists :backup)
            (not (file-exists? "test.out/test1.o"))
            (file-equal? "test.out/test2.o" "test.out/test.move")
            (file-equal? "test.out/test4.o" "test.out/test.move.orig")))

(test* "move-file (same file)" *test-error*
       (move-file "test.out/test.move" "test.out/test.move"
                  :if-exists :supersede))

(test* "remove-directory*" #f
       (begin
         (remove-directory* "test.out")
         (file-exists? "test.out")))

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
