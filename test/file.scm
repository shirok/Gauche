;;
;; testing gauche.fileutil and file.* modules
;;

(use gauche.test)
(use gauche.generator)
(test-start "file utilities")
(use srfi-1)
(use srfi-13)

;; shorthand of normalizing pathname.  this doesn't do anything on
;; unix, but on Windows the separator in PATHNAME is replaced.
(define (n . pathnames) (map sys-normalize-pathname pathnames))

;;------------------------------------------------------------------
(test-section "built-in gauche.fileutil")

(define (md p) (sys-mkdir p #o777))
(define (mf p) (with-output-to-file p (cut display "z")))
(define (rmrf p) (sys-system #"rm -rf ~p"))

(and glob (test-module 'gauche.fileutil)) ;; trigger autoload

(rmrf "tmp1.o")

(define (file-pred-tests path expected)
  (test* #"file-exists? (~path)" (car expected) (file-exists? path))
  (test* #"file-is-regular? (~path)" (cadr expected) (file-is-regular? path))
  (test* #"file-is-directory? (~path)" (caddr expected) (file-is-directory? path)))

(file-pred-tests "tmp1.o" '(#f #f #f))

(with-output-to-file "tmp1.o" (cut display "Z"))

(file-pred-tests "tmp1.o" '(#t #t #f))

(sys-unlink "tmp1.o")
(sys-mkdir "tmp1.o" #o777)

(file-pred-tests "tmp1.o" '(#t #f #t))

(sys-rmdir "tmp1.o")

;;
;; glob test.
;; Note: on Windows the file/directory name can't end with a period.
;; 
(let ()
  (md "tmp1.o")
  (md "tmp1.o/a")
  (mf "tmp1.o/a/b")
  (md "tmp1.o/a/cc")
  (mf "tmp1.o/a/cc/a")
  (mf "tmp1.o/a/.d")
  (md "tmp1.o/.a")
  (mf "tmp1.o/.a/b")
  (md "tmp1.o/.a/.d")
  (md "tmp1.o/aa")
  (mf "tmp1.o/aa/b")
  (mf "tmp1.o/aa/.d")
  (mf "tmp1.o/a.a")
  (mf "tmp1.o/a.b")
  (mf "tmp1.o/a.a.a")

  ;; literal
  (test* "glob a.a" (n "tmp1.o/a.a")
         (glob "tmp1.o/a.a")
         (pa$ lset= equal?))

  ;; nomatch
  (test* "glob z" '()
         (glob "tmp1.o/z")
         (pa$ lset= equal?))

  ;; wildcard
  (test* "glob *" (n "tmp1.o/a" "tmp1.o/aa" "tmp1.o/a.a"
                     "tmp1.o/a.b" "tmp1.o/a.a.a")
         (glob "tmp1.o/*")
         (pa$ lset= equal?))

  (test* "glob a.*" (n "tmp1.o/a.a" "tmp1.o/a.b" "tmp1.o/a.a.a")
         (glob "tmp1.o/a.*")
         (pa$ lset= equal?))

  (test* "glob .*" (n "tmp1.o/.a" "tmp1.o/." "tmp1.o/..")
         (glob "tmp1.o/.*")
         (pa$ lset= equal?))

  (test* "glob ?" (n "tmp1.o/a")
         (glob "tmp1.o/?")
         (pa$ lset= equal?))

  (test* "glob *?" (n "tmp1.o/a" "tmp1.o/aa" "tmp1.o/a.a"
                      "tmp1.o/a.b" "tmp1.o/a.a.a")
         (glob "tmp1.o/*?")
         (pa$ lset= equal?))

  (test* "glob ??" (n "tmp1.o/aa")
         (glob "tmp1.o/??")
         (pa$ lset= equal?))

  (test* "glob *.*" (n "tmp1.o/a.a" "tmp1.o/a.b" "tmp1.o/a.a.a")
         (glob "tmp1.o/*.*")
         (pa$ lset= equal?))

  (test* "glob */*" (n "tmp1.o/a/b" "tmp1.o/a/cc" "tmp1.o/aa/b")
         (glob "tmp1.o/*/*")
         (pa$ lset= equal?))

  (test* "glob */?" (n "tmp1.o/a/b" "tmp1.o/aa/b")
         (glob "tmp1.o/*/?")
         (pa$ lset= equal?))

  (test* "glob *  (chdir)" (n "a" "aa" "a.a" "a.b" "a.a.a")
         (begin (sys-chdir "tmp1.o") (begin0 (glob "*") (sys-chdir "..")))
         (pa$ lset= equal?))

  (test* "glob */" (n "tmp1.o/a/" "tmp1.o/aa/")
         (glob "tmp1.o/*/")
         (pa$ lset= equal?))

  ;; **
  (test* "glob tmp1.o/**/?" (n "tmp1.o/a" "tmp1.o/a/b" "tmp1.o/a/cc/a"
                               "tmp1.o/aa/b")
         (glob "tmp1.o/**/?")
         (pa$ lset= equal?))

  ;; multi
  (test* "glob * .* (multi)" (n "tmp1.o/." "tmp1.o/.." "tmp1.o/.a" "tmp1.o/a"
                                "tmp1.o/aa" "tmp1.o/a.a" "tmp1.o/a.b"
                                "tmp1.o/a.a.a")
         (glob '("tmp1.o/*" "tmp1.o/.*"))
         (pa$ lset= equal?))

  ;; braces
  (test* "glob {a,aa}/{b,cc}" (n "tmp1.o/a/b" "tmp1.o/a/cc" "tmp1.o/aa/b")
         (glob '("tmp1.o/{a,aa}/{b,cc}"))
         (pa$ lset= equal?))
  (test* "glob {a{,a,.{a,b}}}" (n "tmp1.o/a" "tmp1.o/aa"
                                 "tmp1.o/a.a" "tmp1.o/a.b")
         (glob '("tmp1.o/{a{,a,.{a,b}}}"))
         (pa$ lset= equal?))
  (test* "glob {a/*,aa/*}" (n "tmp1.o/a/b" "tmp1.o/a/cc" "tmp1.o/aa/b")
         (glob '("tmp1.o/{a/*,aa/*}"))
         (pa$ lset= equal?))
  (test* "glob {,?/}*" (n "tmp1.o/a/b" "tmp1.o/a/cc" "tmp1.o/a" "tmp1.o/aa"
                          "tmp1.o/a.a" "tmp1.o/a.b" "tmp1.o/a.a.a")
         (glob '("tmp1.o/{,?/}*"))
         (pa$ lset= equal?))
  (test* "glob {,.}*" (n "tmp1.o/a" "tmp1.o/aa" "tmp1.o/.a" "tmp1.o/."
                         "tmp1.o/.." "tmp1.o/a.a" "tmp1.o/a.b" "tmp1.o/a.a.a")
         (glob '("tmp1.o/{,.}*"))
         (pa$ lset= equal?))

  ;; charset
  (test* "glob a.[ab]" (n "tmp1.o/a.a" "tmp1.o/a.b")
         (glob "tmp1.o/a.[ab]")
         (pa$ lset= equal?))
  (test* "glob a.[[:alpha:]]" (n "tmp1.o/a.a" "tmp1.o/a.b")
         (glob "tmp1.o/a.[[:alpha:]]")
         (pa$ lset= equal?))
  (test* "glob *.[[:alpha:]]" (n "tmp1.o/a.a" "tmp1.o/a.b" "tmp1.o/a.a.a")
         (glob "tmp1.o/*.[[:alpha:]]")
         (pa$ lset= equal?))
  (test* "glob *.[![:alpha:]]" '()
         (glob "tmp1.o/*.[![:alpha:]]")
         (pa$ lset= equal?))
  (test* "glob *.[^[:alpha:]]" '()
         (glob "tmp1.o/*.[^[:alpha:]]")
         (pa$ lset= equal?))
  (test* "glob *.[^A-Z]" (n "tmp1.o/a.a" "tmp1.o/a.b" "tmp1.o/a.a.a")
         (glob "tmp1.o/*.[^A-Z]")
         (pa$ lset= equal?))

  ;; specifying current/root dir
  (test* "glob w/alt root dir"  (n "tmp1.o/a.a" "tmp1.o/a.b" "tmp1.o/a.a.a")
          (glob "/*.*" :folder (make-glob-fs-fold :root-path "tmp1.o"))
          (pa$ lset= equal?))
  (test* "glob w/alt current dir"  (n "tmp1.o/a.a" "tmp1.o/a.b" "tmp1.o/a.a.a")
          (glob "*.*" :folder (make-glob-fs-fold :current-path "tmp1.o"))
          (pa$ lset= equal?))

  (rmrf "tmp1.o")
  )

;;------------------------------------------------------------------
(test-section "file.filter")
(use file.filter)
(test-module 'file.filter)

(rmrf "tmp1.o")
(rmrf "tmp2.o")
(with-output-to-file "tmp1.o"
  (cut display "aaa bbb ccc ddd\neee fff ggg hhh\n"))

(test* "file.filter tmp1.o -> string"
       "AAA BBB CCC DDDEEE FFF GGG HHH"
       (with-output-to-string
         (cut file-filter (^[in out]
                            (generator-for-each
                             (^[line] (display (string-upcase line) out))
                             (cut read-line in)))
              :input "tmp1.o")))

(test* "file.filter string -> tmp2.o"
       "AAA BBB CCC DDDEEE FFF GGG HHH"
       (begin
         (with-input-from-string "aaa bbb ccc ddd\neee fff ggg hhh\n"
           (cut file-filter (^[in out]
                              (generator-for-each
                               (^[line] (display (string-upcase line) out))
                               (cut read-line in)))
                :output "tmp2.o"))
         (call-with-input-file "tmp2.o" port->string)))

(sys-unlink "tmp2.o")

(test* "file.filter cleanup" #f
       (with-error-handler (^e (file-exists? "tmp2.o"))
         (cut with-input-from-string "zzz"
              (cut file-filter (^[in out] (error "yyy"))
                   :output "tmp2.o"))))

(sys-unlink "tmp2.o")

(test* "file.filter cleanup" #t
       (with-error-handler (^e (file-exists? "tmp2.o"))
         (cut with-input-from-string "zzz"
              (cut file-filter (^[in out] (error "yyy"))
                   :output "tmp2.o"
                   :keep-output? #t))))

(sys-unlink "tmp2.o")

(let ()
  (define (t tempfile)
    (test* "file.filter temporary"
           '(#f "AAA BBB CCC DDDEEE FFF GGG HHH")
           (let* ([r1
                   (with-input-from-string "aaa bbb ccc ddd\neee fff ggg hhh\n"
                     (cut file-filter
                          (^[in out]
                            (do-generator [line (cut read-line in)]
                               (display (string-upcase line) out))
                            (file-exists? "tmp2.o"))
                          :output "tmp2.o"
                          :temporary-file tempfile))]
                  [r2 (call-with-input-file "tmp2.o" port->string)])
             (sys-unlink "tmp2.o")
             (list r1 r2))))
  (t "foo")
  (t #t))

(sys-unlink "tmp1.o")
(sys-unlink "tmp2.o")

(test* "file.filter rename-hook" #t
       (begin
         (with-output-to-file "tmp1.o" (cut display "foo"))
         (let1 i1 (~ (sys-stat "tmp1.o")'ino)
           (file-filter (^[in out] (copy-port in out))
                        :input (open-input-string "foo")
                        :output "tmp1.o"
                        :temporary-file #t
                        :leave-unchanged #t)
           (eqv? i1 (~ (sys-stat "tmp1.o")'ino)))))

(sys-unlink "tmp1.o")

(let ()
  (define (f2s f) (call-with-input-file f
                    (^i (call-with-output-string (^o (copy-port i o))))))
  (define (t reader expect-result expect-output)
    (test* (format "file-filter-fold") (list expect-result expect-output)
           (let1 r (file-filter-fold
                    (^[elt seed out] (write elt out) (+ seed 1))
                    0
                    :input "test1.o"
                    :output "test2.o"
                    :reader reader)
             (list r (f2s "test2.o"))))
    (test* (format "file-filter-map") (list expect-result expect-output)
           (let1 r (file-filter-map
                    (^[elt out] (write elt out) #t)
                    :input "test1.o"
                    :output "test2.o"
                    :reader reader)
             (list (length r) (f2s "test2.o"))))
    (test* (format "file-filter-for-each") (list expect-result expect-output)
           (let1 r 0
             (file-filter-for-each
              (^[elt out] (write elt out) (inc! r))
              :input "test1.o"
              :output "test2.o"
              :reader reader)
             (list r (f2s "test2.o")))))
  
  (with-output-to-file "test1.o" (cut display "abc\n(def\nghi)"))
  (unwind-protect
      (begin
        (t read-line 3 "\"abc\"\"(def\"\"ghi)\"")
        (t read-char 13 "#\\a#\\b#\\c#\\newline#\\(#\\d#\\e#\\f#\\newline#\\g#\\h#\\i#\\)")
        (t read 2 "abc(def ghi)"))
    (sys-unlink "test1.o")
    (sys-unlink "test2.o")))

(test* "file-filter-fold example"
       "  1: abc\n  3: def\n"
       (unwind-protect
           (begin
             (with-output-to-file "test1.o"
               (^[]
                 (print "abc")
                 (print "123")
                 (print "def")))
             (file-filter-fold (^[line nc out]
                                 (when (#/[a-z]/ line)
                                   (format out "~3d: ~a\n" nc line))
                                 (+ nc 1))
                               1 :input "test1.o" :output "test2.o")
             (call-with-input-file "test2.o" 
               (^i (call-with-output-string (^o (copy-port i o))))))
         (sys-unlink "test1.o")
         (sys-unlink "test2.o")))

(test-end)
