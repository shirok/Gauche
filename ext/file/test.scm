;;
;; testing file.util
;;

(use gauche.test)
(use srfi-1)
(use srfi-13)
(use gauche.uvector)

(test-start "file.util")
(use file.util)
(test-module 'file.util)

;; shorthand of normalizing pathname.  this doesn't do anything on
;; unix, but on Windows the separator in PATHNAME is replaced.
(define (n pathname . pathnames)
  (if (null? pathnames)
    (sys-normalize-pathname pathname)
    (map sys-normalize-pathname (cons pathname pathnames))))

;; mingw doesn't have fully compatible permissions as unix.
;; this procedure compensates it.  
(define (P perm)
  (cond-expand
   [gauche.os.windows (let1 p (ash perm -6)
                        (logior p (ash p 3) (ash p 6)))]
   [else perm]))

;;=====================================================================
(test-section "pathname utils")

(test* "build-path" "" (build-path ""))
(test* "build-path" "." (build-path "."))
(test* "build-path" (n "/") (build-path "/"))
(test* "build-path" (n "a/b/c") (build-path "a" "b" "c"))
(test* "build-path" (n "a/b/c") (build-path "a/" "b/" "c"))
(test* "build-path" (n "/a/b/c") (build-path "/" "a/b" "c"))
(test* "build-path" (n "./a/b/c") (build-path "." "a/b" "c"))
(test* "build-path" (test-error) (build-path "." "/a/b"))
(test* "build-path" (n "foo") (build-path "" "foo"))
(test* "build-path" (n "foo/bar") (build-path "" "foo" "" "bar"))
(test* "build-path" (n "foo") (build-path "" "foo" ""))

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
(test* "path-sans-extension" "a" (path-sans-extension "a.c"))
(test* "path-sans-extension" "a" (path-sans-extension "a."))
(test* "path-sans-extension" "a" (path-sans-extension "a"))
(test* "path-sans-extension" ".a" (path-sans-extension ".a"))
(test* "path-sans-extension" ".a" (path-sans-extension ".a.c"))

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
(test* "path-swap-extension" "a.o" (path-swap-extension "a.c" "o"))
(test* "path-swap-extension" "a.o" (path-swap-extension "a." "o"))
(test* "path-swap-extension" "a.o" (path-swap-extension "a" "o"))
(test* "path-swap-extension" ".a.o" (path-swap-extension ".a" "o"))
(test* "path-swap-extension" ".a.o" (path-swap-extension ".a.c" "o"))


;;=====================================================================
(test-section "directories")

(test "current-directory" (list (n "/") (n "/") #t #t)
      (lambda ()
        (let* ((cur   (sys-getcwd))
               (root  (begin (current-directory "/")
                             (sys-getcwd)))
               (root* (current-directory))
               (cur*  (begin (current-directory cur)
                             (sys-getcwd)))
               (cur** (current-directory)))
          (list root root* (string=? cur cur*) (string=? cur* cur**)))))

;; rm -rf
(define (cmd-rmrf dir)
  ;; shorthand of normalizing pathname.  this doesn't do anything on
  ;; unix, but on Windows the separator in PATHNAME is replaced.
  (define (n pathname) (sys-normalize-pathname pathname))

  (cond-expand
   [gauche.os.windows
    (sys-system #`"rmdir /q /s ,(n dir) > NUL 2>&1")
    (sys-system #`"del /q ,(n dir) > NUL 2>&1")]
   [else
    (sys-system #`"rm -rf ,dir > /dev/null")]))

;; prepare test data set
(define *test-tree*
  `(test.out
    ((test1.o ,(make-string 100 #\o))
     (test2.o ,(make-string 100 #\o))
     (test3.o ,(make-string 100 #\i))
     (test4.o ,(make-string 20000 #\i))
     (test5.o ,(make-string 20000 #\i))
     ,@(cond-expand
        [gauche.sys.symlink
         '((test6.o :symlink "test1.o")
           (test7.o :symlink "test6.o")
           (test2.d :symlink "test.d"))]
        [else
         `((test6.o ,(make-string 100 #\o))
           (test7.o ,(make-string 100 #\o))
           (test2.d :mode 777
		    ((test10.o ,(make-string 100 #\o))
		     (test11.o ,(make-string 100 #\o)))))])
     (test.d ((test10.o ,(make-string 100 #\o))
              ,(cond-expand
                [gauche.sys.symlink
                 '(test11.o :symlink "../test1.o")]
                [else
                 `(test11.o ,(make-string 100 #\o))])))
     )))

(cmd-rmrf "test.out")

(test* "create-directory-tree" #t
       (begin (create-directory-tree "." *test-tree*)
              (and (file-is-directory? "test.out")
                   (file-is-regular? "test.out/test1.o")
                   (file-is-regular? "test.out/test2.o")
                   (file-is-regular? "test.out/test3.o")
                   (file-is-regular? "test.out/test4.o")
                   (file-is-regular? "test.out/test5.o")
                   (file-is-directory? "test.out/test.d")
                   (file-is-regular? "test.out/test.d/test10.o"))))

(test* "check-directory-tree" #t
       (check-directory-tree "." *test-tree*))
           
;; (sys-system "rm -rf test.out test2.out")
;; (sys-system "mkdir test.out")
;; (with-output-to-file "test.out/test1.o" (cut display (make-string 100 #\o)))
;; (with-output-to-file "test.out/test2.o" (cut display (make-string 100 #\o)))
;; (with-output-to-file "test.out/test3.o" (cut display (make-string 100 #\i)))
;; (with-output-to-file "test.out/test4.o" (cut display (make-string 20000 #\i)))
;; (with-output-to-file "test.out/test5.o" (cut display (make-string 20000 #\i)))

;; (cond-expand
;;  [gauche.sys.symlink
;;   (sys-symlink "test1.o" "test.out/test6.o")
;;   (sys-symlink "test6.o" "test.out/test7.o")
;;   (sys-symlink "test.d" "test.out/test2.d")]
;;  [else
;;   (with-output-to-file "test.out/test6.o" (cut newline))
;;   (with-output-to-file "test.out/test7.o" (cut newline))
;;   (sys-mkdir "test.out/test2.d" #o777)])

;; (sys-system "mkdir test.out/test.d")
;; (with-output-to-file "test.out/test.d/test10.o"
;;   (cut display (make-string 100 #\o)))

;; (cond-expand
;;  [gauche.sys.symlink
;;   (sys-symlink "../test1.o" "test.out/test.d/test11.o")
;;   (test* "file-is-symlink?" #t
;;          (file-is-symlink? "test.out/test.d/test11.o"))]
;;  [else
;;   (with-output-to-file "test.out/test.d/test11.o" (cut newline))])

(test* "directory-list"
       '("." ".." "test.d" "test1.o" "test2.d" "test2.o"
         "test3.o" "test4.o" "test5.o" "test6.o" "test7.o" )
       (directory-list "test.out"))

(test* "directory-list :children?"
       '("test.d" "test1.o" "test2.d" "test2.o"
         "test3.o" "test4.o" "test5.o" "test6.o" "test7.o" )
       (directory-list "test.out" :children? #t))

(test* "directory-list :add-path?"
       (n "test.out/." "test.out/.." "test.out/test.d" "test.out/test1.o"
          "test.out/test2.d" "test.out/test2.o"  "test.out/test3.o"
          "test.out/test4.o" "test.out/test5.o" "test.out/test6.o"
          "test.out/test7.o" )
       (directory-list "test.out/" :add-path? #t))

(test* "directory-list :filter"
       (n "test.out/test1.o"
          "test.out/test2.o"  "test.out/test3.o" "test.out/test4.o"
          "test.out/test5.o" "test.out/test6.o" "test.out/test7.o" )
       (directory-list "test.out" :add-path? #t
                       :filter (lambda (p) (string-suffix? "o" p))))

(test* "directory-list :filter"
       (n "test.out/test1.o"
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
       `(,(n "test.out/." "test.out/.." "test.out/test.d" "test.out/test2.d")
         ,(n "test.out/test1.o" "test.out/test2.o"  "test.out/test3.o"
             "test.out/test4.o" "test.out/test5.o" "test.out/test6.o"
             "test.out/test7.o"))
       (receive x (directory-list2 "test.out" :add-path? #t) x))

(test* "directory-list2 :children"
       `(,(n "test.out/test.d" "test.out/test2.d")
         ,(n "test.out/test1.o" "test.out/test2.o"  "test.out/test3.o"
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

(cond-expand
 [gauche.sys.symlink
  (test* "directory-list2 :follow-link? #f"
         '(("test.d")
           ("test1.o" "test2.d" "test2.o" "test3.o" "test4.o"
            "test5.o" "test6.o" "test7.o" ))
         (receive x (directory-list2 "test.out" :follow-link? #f :children? #t)
           x)
         )]
 [else])

(test* "directory-fold"
       (n "test.out/test.d/test10.o" "test.out/test.d/test11.o"
          "test.out/test1.o"
          "test.out/test2.d/test10.o" "test.out/test2.d/test11.o"
          "test.out/test2.o" "test.out/test3.o"
          "test.out/test6.o" "test.out/test7.o")
       (reverse
        (directory-fold "test.out"
                        (lambda (path result)
                          (if (= (file-size path) 100)
                              (cons path result)
                              result))
                        '()))
       )

(test* "directory-fold"
       (n "test.out"
          "test.out/test.d"
          "test.out/test.d/test10.o" "test.out/test.d/test11.o"
          "test.out/test1.o"
          "test.out/test2.d"
          "test.out/test2.d/test10.o"
          "test.out/test2.d/test11.o"
          "test.out/test2.o" "test.out/test3.o"
          "test.out/test4.o" "test.out/test5.o"
          "test.out/test6.o" "test.out/test7.o")
       (reverse
        (directory-fold "test.out" cons '()
                        :lister (lambda (path seed)
                                  (values
                                   (directory-list path
                                                   :add-path? #t
                                                   :children? #t)
                                   (cons path seed)))))
       )

(cond-expand
 [gauche.sys.symlink
  (test* "directory-fold :follow-link? #f"
         (n "test.out/test.d/test10.o" "test.out/test.d/test11.o"
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
         (n "test.out/test.d/test10.o" "test.out/test.d/test11.o"
            "test.out/test.dangling" "test.out/test1.o"
            "test.out/test2.d/test10.o" "test.out/test2.d/test11.o"
            "test.out/test2.o" "test.out/test3.o" "test.out/test4.o"
            "test.out/test5.o" "test.out/test6.o" "test.out/test7.o")
         (sort (directory-fold "test.out" cons '())))
  (sys-unlink "test.out/test.dangling")]
 [else])

(test* "directory-fold :lister"
       (cond-expand
        [gauche.sys.symlink
         (n "test.out/test.d/test10.o" "test.out/test.d/test11.o"
            "test.out/test1.o")]
        [else
         (n "test.out/test.d/test10.o" "test.out/test.d/test11.o"
            "test.out/test2.d/test10.o" "test.out/test2.d/test11.o"
            "test.out/test1.o")])
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

(cmd-rmrf "test.out")

;;=====================================================================
(test-section "file utils")

(create-directory-tree "." *test-tree*)

;; resolve-path should be here for it uses test.out hierarchy created above.
'(test* "resolve-path" "/" (resolve-path "/"))
(test* "resolve-path" "." (resolve-path "."))
(test* "resolve-path" "test.out" (resolve-path "test.out"))
(cond-expand
 [gauche.sys.symlink
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
  ]
 [else])

(test* "file-type" #f
       (file-type "nonexistent/file"))
(test* "file-type" '(directory directory regular)
       (map file-type
            '("test.out/test.d" "test.out/test2.d" "test.out/test1.o")))
(cond-expand
 [gauche.sys.symlink
  (test* "file-type :follow-link? #f" '(directory symlink regular)
         (map (cut file-type <> :follow-link? #f)
              '("test.out/test.d" "test.out/test2.d" "test.out/test1.o")))]
 [else])

(test* "file-eq?" #t
       (file-eq? "test.out/test1.o" "test.out/test1.o"))
(cond-expand
 [gauche.sys.symlink
  (test* "file-eq? (symlink)" #f
         (file-eq? "test.out/test1.o" "test.out/test7.o"))]
 [else])
(test* "file-eq?" #f
       (file-eq? "test.out/test1.o" "test.out/test2.o"))
(test* "file-eqv?" #t
       (file-eqv? "test.out/test1.o" "test.out/test1.o"))
(cond-expand
 [gauche.sys.symlink
  (test* "file-eqv? (symlink)" #t
         (file-eqv? "test.out/test1.o" "test.out/test7.o"))
  (test* "file-eqv? (symlink)" #t
         (file-eqv? "test.out/test1.o" "test.out/test.d/test11.o"))]
 [else])
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

(test* "copy-file (normal; permission check)"
       (number->string (logand #o666 (lognot (sys-umask))) 8)
       (number->string (file-perm "test.out/test.copy") 8))

(test* "copy-file (:if-exists :error)" (test-error)
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

(cond-expand
 [gauche.sys.symlink
  (test* "copy-file (:follow-link? #f)" 'symlink
         (begin
           (copy-file "test.out/test6.o" "test.out/test.copy" :follow-link? #f)
           (sys-system "ls -l test.out")
           (file-type "test.out/test.copy" :follow-link? #f)))
  (test* "copy-file (:follow-link? #f :if-exists :backup)"
         '(symlink symlink)
         (begin
           (copy-file "test.out/test6.o" "test.out/test.copy"
                      :follow-link? #f :if-exists :backup :backup-suffix "~")
           (list (file-type "test.out/test.copy" :follow-link? #f)
                 (file-type "test.out/test.copy~" :follow-link? #f))))
  (sys-unlink "test.out/test.copy")
  (sys-unlink "test.out/test.copy~")]
 [else])

(test* "copy-file (normal, safe)" #t
       (and (copy-file "test.out/test5.o" "test.out/test.copy" :safe #t)
            (not (file-eq? "test.out/test5.o" "test.out/test.copy"))
            (file-equal? "test.out/test5.o" "test.out/test.copy")))

(test* "copy-file (:if-exists :error, safe)" (test-error)
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

(test* "copy-file (same file)" (test-error)
       (copy-file "test.out/test.copy" "test.out/test.copy"
                  :if-exists :supersede))

(sys-unlink "test.out/test.copy")

(test* "copy-file (keep-mode)" (P #o642)
       (begin (sys-chmod "test.out/test1.o" #o642)
              (copy-file "test.out/test1.o" "test.out/test.copy"
                         :keep-mode #t)
              (file-perm "test.out/test.copy")))

(sys-unlink "test.out/test.copy")

(test* "copy-file (keep-mode w/safe)" (P #o624)
       (begin (sys-chmod "test.out/test1.o" #o624)
              (copy-file "test.out/test1.o" "test.out/test.copy"
                         :keep-mode #t)
              (file-perm "test.out/test.copy")))

(sys-chmod "test.out/test1.o" #o644)
(sys-chmod "test.out/test.copy" #o644)

(test* "move-file (normal)" #t
       (and (move-file "test.out/test.copy" "test.out/test.move")
            (not (file-exists? "test.out/test.copy"))
            (file-equal? "test.out/test1.o" "test.out/test.move")))

(test* "move-file (:if-exists :error)" (test-error)
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

(test* "move-file (same file)" (test-error)
       (move-file "test.out/test.move" "test.out/test.move"
                  :if-exists :supersede))

(let ()
  (define (listdir d)
    (map (cut regexp-replace #/^test2?\.out[\/\\]/ <> "")
         (directory-fold d cons '())))
    
  (test* "copy-directory*" (listdir "test.out")
         (begin
           (copy-directory* "test.out" "test2.out")
           (listdir "test2.out"))
         (cut lset= string=? <> <>))

  (test* "remove-directory*" '(#f #f)
         (begin
           (remove-directory* "test.out")
           (remove-directory* "test2.out")
           (list (file-exists? "test.out") (file-exists? "test2.out"))))

  ;; check dangling link behavior
  (cond-expand
   [gauche.sys.symlink
    (let ()
      (define (tester desc fn expect . opts)
        (test* #`"dangling symlink - ,desc" expect
               (begin
                 (sys-symlink "no such file" "test.out")
                 (apply fn "test.out" "test2.out" opts)
                 (and (file-is-symlink? "test2.out")
                      (string=? (sys-readlink "test.out")
                                (sys-readlink "test2.out")))))
        (remove-files "test.out" "test2.out"))

      (tester "copy-file" copy-file (test-error))
      (tester "copy-file" copy-file #t :follow-link? #f)
      (tester "copy-directory*" copy-directory* #t)
      (tester "copy-directory*" copy-directory* (test-error) :follow-link? #t)
      )]
   [else])
  )

;;
;; file->*
;;

(test* "file->string, if-does-not-exist #f" #f
       (file->string "test.out" :if-does-not-exist #f))
(test* "file->string, if-does-not-exist :error" (test-error)
       (file->string "test.out" :if-does-not-exist :error))

(with-output-to-file "test.out"
  (cut display "humuhumu\nnukunuku\napua`a\n\n"))

(test* "file->string" "humuhumu\nnukunuku\napua`a\n\n"
       (file->string "test.out"))
(test* "file->string-list" '("humuhumu" "nukunuku" "apua`a" "")
       (file->string-list "test.out"))

(remove-files "test.out" "test2.out")

(test-end)
