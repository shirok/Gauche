;;
;; test for port I/O functions
;;

(use gauche.test)

(test-start "io")

;;-------------------------------------------------------------------
(test-section "file i/o")

(sys-system "rm -f tmp2.o")

(test "open-input-file" #t
      (lambda () (with-error-handler
                  (lambda (e) #t)
                  (lambda () (open-input-file "tmp2.o")))))

(test "open-input-file :if-does-not-exist #f" #f
      (lambda () (open-input-file "tmp2.o" :if-does-not-exist #f)))

(test "open-output-file :if-does-not-exist :error" #t
      (lambda ()
        (with-error-handler
         (lambda (e) #t)
         (lambda () (open-output-file "tmp2.o" :if-does-not-exist :error)))))

(test "open-output-file :if-does-not-exit #f" #f
      (lambda ()
        (open-output-file "tmp2.o" :if-does-not-exist #f)))

(test "open-output-file" #t
      (lambda ()
        (let* ((p (open-output-file "tmp2.o"))
               (r (output-port? p)))
          (display "abcde" p)
          (close-output-port p)
          r)))

(test "open-input-file" 'abcde
      (lambda ()
        (let* ((p (open-input-file "tmp2.o"))
               (s (read p)))
          (close-input-port p)
          s)))

(test "open-output-file :if-exists :error" #t
      (lambda ()
        (with-error-handler
         (lambda (e) #t)
         (lambda ()
           (open-output-file "tmp2.o" :if-exists :error)))))

(test "open-output-file :if-exists :supersede" 'cdefg
      (lambda ()
        (let ((o (open-output-file "tmp2.o")))
          (display "cdefg" o)
          (close-output-port o)
          (let* ((i (open-input-file "tmp2.o"))
                 (s (read i)))
            (close-input-port i)
            s))))

(test "open-output-file :if-exists :append" 'cdefghij
      (lambda ()
        (let ((o (open-output-file "tmp2.o" :if-exists :append)))
          (display "hij" o)
          (close-output-port o)
          (let* ((i (open-input-file "tmp2.o"))
                 (s (read i)))
            (close-input-port i)
            s))))

(test "open-output-file :if-exists :append" 'cdefghijklm
      (lambda ()
        (let1 o (open-output-file "tmp2.o"
                                  :if-exists :append
                                  :if-does-not-exist :error)
          (display "klm" o)
          (close-output-port o)
          (let* ((i (open-input-file "tmp2.o"))
                 (s (read i)))
            (close-input-port i)
            s))))

(test "open-output-file :if-exists :supersede" 'nopqr
      (lambda ()
        (let1 o (open-output-file "tmp2.o"
                                  :if-exists :supersede
                                  :if-does-not-exist #f)
          (display "nopqr" o)
          (close-output-port o)
          (let* ((i (open-input-file "tmp2.o"))
                 (s (read i)))
            (close-input-port i)
            s))))

(sys-system "rm -f tmp2.o")

(test "call-with-input-file :if-does-not-exist #f" '(#f #f)
      (lambda ()
        (call-with-input-file "tmp2.o" (lambda (p) (list p p))
                              :if-does-not-exist #f)))

(test "with-input-from-file :if-does-not-exist #f" #f
      (lambda ()
        (with-input-from-file "tmp2.o" (lambda () 5)
                              :if-does-not-exist #f)))

(call-with-output-file "tmp2.o" (lambda (p) (display "stu" p)))

(test "call-with-output-file :if-exists #f" 'stu
      (lambda ()
        (call-with-output-file "tmp2.o" (lambda (p)
                                          (and p (display "vwx" p)))
                              :if-exists #f)
        (call-with-input-file "tmp2.o" read)))

(test "with-output-to-file :if-exists #f" 'stu
      (lambda ()
        (or (with-output-to-file "tmp2.o"
              (lambda () (display "yz" p) 4)
              :if-exists #f)
            (call-with-input-file "tmp2.o" read))))

;;-------------------------------------------------------------------
(test-section "input ports")

(sys-unlink "tmp1.o")
(with-output-to-file "tmp1.o" (lambda () (display "")))
(test "read-char (EOF)" #t
      (lambda () (eof-object? (call-with-input-file "tmp1.o" read-char))))
(test "read-byte (EOF)" #t
      (lambda () (eof-object? (call-with-input-file "tmp1.o" read-byte))))
(test "read-line (EOF)" #t
      (lambda () (eof-object? (call-with-input-file "tmp1.o" read-line))))
(test "read-block (EOF)" #t
      (lambda ()
        (eof-object? (call-with-input-file "tmp1.o"
                       (cut read-block 10 <>)))))

(with-output-to-file "tmp1.o" (lambda () (display "ab")))
(test "read-char (a)" #\a
      (lambda () (call-with-input-file "tmp1.o" read-char)))
(test "read-byte (a)" 97
      (lambda () (call-with-input-file "tmp1.o" read-byte)))
(test "read-byte (ungotten)" 97
      (lambda ()
        (call-with-input-file "tmp1.o"
          (lambda (p) (peek-char p) (read-byte p)))))
(test "read-line (a)" "ab"
      (lambda () (call-with-input-file "tmp1.o" read-line)))
(test "read-byte (ungotten)" 97
      (lambda ()
        (call-with-input-file "tmp1.o"
          (lambda (p) (peek-char p) (read-byte p)))))
(test "read-block (a)" #*"ab"
      (lambda ()
        (call-with-input-file "tmp1.o" (cut read-block 10 <>))))
(test "read-block (ungotten)" #*"ab"
      (lambda ()
        (call-with-input-file "tmp1.o"
          (lambda (p) (peek-char p) (read-block 10 p)))))

(with-output-to-file "tmp1.o" (lambda () (display "\n")))
(test "read-line (LF)" ""
      (lambda () (call-with-input-file "tmp1.o" read-line)))
(test "read-line (LF, ungotten)" ""
      (lambda ()
        (call-with-input-file "tmp1.o"
          (lambda (p) (peek-char p) (read-line p)))))
(with-output-to-file "tmp1.o" (lambda () (display "\r")))
(test "read-line (CR)" ""
      (lambda () (call-with-input-file "tmp1.o" read-line)))
(test "read-line (CR, ungotten)" ""
      (lambda ()
        (call-with-input-file "tmp1.o"
          (lambda (p) (peek-char p) (read-line p)))))
(with-output-to-file "tmp1.o" (lambda () (display "\n\n")))
(test "read-line (LF)" '("" "" #t)
      (lambda ()
        (call-with-input-file "tmp1.o"
          (lambda (_)
             (let* ((c1 (peek-char _))
                    (l1 (read-line _))
                    (c2 (peek-char _))
                    (l2 (read-line _))
                    (c2 (peek-char _))
                    (l3 (read-line _)))
               (list l1 l2 (eof-object? l3)))))))
(with-output-to-file "tmp1.o" (lambda () (display "\r\r\n")))
(test "read-line (CR, CRLF)" '("" "" #t)
      (lambda ()
        (call-with-input-file "tmp1.o"
          (lambda (_)
            (let* ((c1 (peek-char _))
                   (l1 (read-line _))
                   (c2 (peek-char _))
                   (l2 (read-line _))
                   (c2 (peek-char _))
                   (l3 (read-line _)))
              (list l1 l2 (eof-object? l3)))))))
(with-output-to-file "tmp1.o" (lambda () (display "a\r\nb\nc")))
(test "read-line (mix)" '("a" "b" "c" #t)
      (lambda ()
        (call-with-input-file "tmp1.o"
          (lambda (_)
            (let* ((c1 (peek-char _))
                   (l1 (read-line _))
                   (c2 (peek-char _))
                   (l2 (read-line _))
                   (c2 (peek-char _))
                   (l3 (read-line _))
                   (c3 (peek-char _)))
              (list l1 l2 l3 (eof-object? c3)))))))

(with-output-to-file "tmp1.o"
  (lambda ()
    (display "a b c \"d e\" f g\n(0 1 2\n3 4 5)\n")))

(test "port->string" "a b c \"d e\" f g\n(0 1 2\n3 4 5)\n"
      (lambda ()
        (call-with-input-file "tmp1.o" port->string)))
(test "port->list" '(a b c "d e" f g (0 1 2 3 4 5))
      (lambda ()
        (call-with-input-file "tmp1.o" (lambda (p) (port->list read p)))))
(test "port->list" '("a b c \"d e\" f g" "(0 1 2" "3 4 5)")
      (lambda ()
        (call-with-input-file "tmp1.o" (lambda (p) (port->list read-line p)))))
(test "port->string-list" '("a b c \"d e\" f g" "(0 1 2" "3 4 5)")
      (lambda ()
        (call-with-input-file "tmp1.o" port->string-list)))
(test "port->sexp-list" '(a b c "d e" f g (0 1 2 3 4 5))
      (lambda ()
        (call-with-input-file "tmp1.o" port->sexp-list)))

(test "port-fold" '((0 1 2 3 4 5) g f "d e" c b a)
      (lambda ()
        (with-input-from-file "tmp1.o"
          (lambda () (port-fold cons '() read)))))
(test "port-fold" '("3 4 5)" "(0 1 2" "a b c \"d e\" f g")
      (lambda ()
        (with-input-from-file "tmp1.o"
          (lambda () (port-fold cons '() read-line)))))
(test "port-fold-right" '(a b c "d e" f g (0 1 2 3 4 5))
      (lambda ()
        (with-input-from-file "tmp1.o"
          (lambda () (port-fold-right cons '() read)))))

(test "port-map" '(a b c "d e" f g (0 1 2 3 4 5))
      (lambda ()
        (with-input-from-file "tmp1.o"
          (lambda () (port-map (lambda (x) x) read)))))

;;-------------------------------------------------------------------
(test-section "seeking")

(define (seek-tester1 p)
  (display (read-block 5 p))
  (let ((p0 (port-tell p)))
    (port-seek p -3 SEEK_CUR)
    (display (read-block 5 p))
    (port-seek p p0)
    (display (read p))
    (port-seek p 0 SEEK_SET)
    (display (read-block 3 p))
    (port-seek p -3 SEEK_END)
    (display (read p))))

(test "seek (istr)" "abcdecdefgfghijabchij"
      (lambda ()
        (with-output-to-string
          (lambda ()
            (call-with-input-string "abcdefghij" seek-tester1)))))
(test "seek (istr, boundary)" #\a
      (lambda ()
        (call-with-input-string "abcdefghij"
          (lambda (p)
            (read-char p)
            (port-seek p -1 SEEK_CUR)
            (read-char p)))))
(test "seek (istr, boundary)" #t
      (lambda ()
        (call-with-input-string "abcdefghij"
          (lambda (p)
            (read-char p)
            (port-seek p 10)
            (eof-object? (read-char p))))))
(test "seek (istr, out of range)" #f
      (lambda ()
        (call-with-input-string "abcdefghij"
          (lambda (p)
            (read-char p)
            (port-seek p 10 SEEK_CUR)))))
(test "seek (istr, out of range)" #f
      (lambda ()
        (call-with-input-string "abcdefghij"
          (lambda (p)
            (read-char p)
            (port-seek p -2)))))

(test "seek (ifile)" "abcdecdefgfghijabchij"
      (lambda ()
        (sys-unlink "test.o")
        (with-output-to-file "test.o" (lambda () (display "abcdefghij")))
        (with-output-to-string
          (lambda ()
            (call-with-input-file "test.o" seek-tester1)))))

(test "seek (ofile)" "--//efg**j++"
      (lambda ()
        (call-with-output-file "test.o"
          (lambda (p)
            (port-seek p 0)
            (display "--" p)
            (let ((p0 (port-tell p)))
              (port-seek p 0 SEEK_END)
              (display "++" p)
              (port-seek p -5 SEEK_CUR)
              (display "**" p)
              (port-seek p p0)
              (display "//" p)))
          :if-exists :overwrite)
        (call-with-input-file "test.o" port->string)))

(test "seek (ifile, large)"
      "0000050055019999050100027500"
      (lambda ()
        (sys-unlink "test.o")
        (with-output-to-file "test.o"
          (lambda () (dotimes (n 10000) (format #t "~4,'0d" n))))
        (with-output-to-string
          (lambda ()
            (call-with-input-file "test.o"
              (lambda (p)
                (display (read-block 4 p))
                (port-seek p 2000)
                (display (read-block 4 p))
                (let ((p0 (port-tell p)))
                  (port-seek p 20000 SEEK_CUR)
                  (display (read-block 4 p))
                  (port-seek p -4 SEEK_END)
                  (display (read-block 4 p))
                  (port-seek p p0)
                  (display (read-block 4 p))
                  (port-seek p -2000 SEEK_CUR)
                  (display (read-block 4 p))
                  (port-seek p -10000 SEEK_END)
                  (display (read-block 4 p))
                  )))))))

(test "seek (ofile, large)"
      "*0-0*/-0999+"
      (lambda ()
        (call-with-output-file "test.o"
          (lambda (p)
            (display "*" p)
            (port-seek p 20000)
            (display "*" p)
            (let ((p0 (port-tell p)))
              (port-seek p -19999 SEEK_CUR)
              (display "-" p)
              (port-seek p -19998 SEEK_END)
              (display "-" p)
              (port-seek p 19996 SEEK_CUR)
              (display "+" p)
              (port-seek p p0)
              (display "/" p)))
          :if-exists :overwrite)
        (with-output-to-string
          (lambda ()
            (call-with-input-file "test.o"
              (lambda (p)
                (display (read-block 4 p))
                (port-seek p 20000)
                (display (read-block 4 p))
                (port-seek p 39996)
                (display (read-block 4 p))))))
        ))

(sys-unlink "test.o")

;;-------------------------------------------------------------------
(test-section "format")

(test "format ~s" "\"abc\""
      (lambda () (format #f "~s" "abc")))
(test "format ~s" "\"abc\"     "
      (lambda () (format #f "~10s" "abc")))
(test "format ~s" "     \"abc\""
      (lambda () (format #f "~10@s" "abc")))
(test "format ~s" "\"abc\"      "
      (lambda () (format #f "~10,3s" "abc")))
(test "format ~s" "      \"abc\""
      (lambda () (format #f "~10,3@s" "abc")))
(test "format ~s" "\"abc\" "
      (lambda () (format #f "~,,1s" "abc")))
(test "format ~s" " \"abc\""
      (lambda () (format #f "~,,1@s" "abc")))
(test "format ~s" "\"abc\"*****"
      (lambda () (format #f "~10,,,'*s" "abc")))
(test "format ~s" "*****\"abc\""
      (lambda () (format #f "~10,,,'*@s" "abc")))

(test "format ~s" "\"abc\"*****"
      (lambda () (format #f "~10,,,'*,15s" "abc")))
(test "format ~s" "*****\"abc\""
      (lambda () (format #f "~10,,,'*,15@s" "abc")))
(test "format ~s" "(\"abc\" \"def\" \"g"
      (lambda () (format #f "~10,,,'*,15s" '("abc" "def" "ghi" "jkl"))))
(test "format ~s" "(\"abc\" \"def\" \"g"
      (lambda () (format #f "~10,,,'*,15@s" '("abc" "def" "ghi" "jkl"))))
(test "format ~s" "(\"abc\" \"def ..."
      (lambda () (format #f "~10,,,'*,15:s" '("abc" "def" "ghi" "jkl"))))
(test "format ~s" "(\"abc\" \"def ..."
      (lambda () (format #f "~10,,,'*,15@:s" '("abc" "def" "ghi" "jkl"))))

(test "format ~a" "abc"
      (lambda () (format #f "~a" "abc")))
(test "format ~a" "abc       "
      (lambda () (format #f "~10a" "abc")))
(test "format ~a" "       abc"
      (lambda () (format #f "~10@a" "abc")))
(test "format ~a" "abc         "
      (lambda () (format #f "~10,3a" "abc")))
(test "format ~a" "         abc"
      (lambda () (format #f "~10,3@a" "abc")))
(test "format ~a" "abc "
      (lambda () (format #f "~,,1a" "abc")))
(test "format ~a" " abc"
      (lambda () (format #f "~,,1@a" "abc")))
(test "format ~a" "abc*******"
      (lambda () (format #f "~10,,,'*a" "abc")))
(test "format ~a" "*******abc"
      (lambda () (format #f "~10,,,'*@a" "abc")))

(test "format ~a" "(abc def ghi jk"
      (lambda () (format #f "~10,,,'*,15a" '("abc" "def" "ghi" "jkl"))))
(test "format ~a" "(abc def ghi jk"
      (lambda () (format #f "~10,,,'*,15@a" '("abc" "def" "ghi" "jkl"))))
(test "format ~a" "(abc def gh ..."
      (lambda () (format #f "~10,,,'*,15:a" '("abc" "def" "ghi" "jkl"))))
(test "format ~a" "(abc def gh ..."
      (lambda () (format #f "~10,,,'*,15@:a" '("abc" "def" "ghi" "jkl"))))

(test "format ~d" "12345"       (lambda () (format #f "~d" 12345)))
(test "format ~d" "-12345"      (lambda () (format #f "~d" -12345)))
(test "format ~d" "+12345"      (lambda () (format #f "~@d" 12345)))
(test "format ~d" "-12345"      (lambda () (format #f "~@d" -12345)))
(test "format ~d" "     12345"  (lambda () (format #f "~10d" 12345)))
(test "format ~d" "    -12345"  (lambda () (format #f "~10d" -12345)))
(test "format ~d" "    +12345"  (lambda () (format #f "~10@d" 12345)))
(test "format ~d" "    -12345"  (lambda () (format #f "~10@d" -12345)))
(test "format ~d" "0000012345"  (lambda () (format #f "~10,'0d" 12345)))
(test "format ~d" "0000-12345"  (lambda () (format #f "~10,'0d" -12345)))

(test "format ~:d" "1"  (lambda () (format #f "~:d" 1)))
(test "format ~:d" "-1"  (lambda () (format #f "~:d" -1)))
(test "format ~:d" "12"  (lambda () (format #f "~:d" 12)))
(test "format ~:d" "-12"  (lambda () (format #f "~:d" -12)))
(test "format ~:d" "123"  (lambda () (format #f "~:d" 123)))
(test "format ~:d" "-123"  (lambda () (format #f "~:d" -123)))
(test "format ~:d" "+123"  (lambda () (format #f "~:@d" 123)))
(test "format ~:d" "1,234"  (lambda () (format #f "~:d" 1234)))
(test "format ~:d" "-1,234"  (lambda () (format #f "~:d" -1234)))
(test "format ~:d" "+1,234"  (lambda () (format #f "~:@d" 1234)))
(test "format ~:d" "12,345"  (lambda () (format #f "~:d" 12345)))
(test "format ~:d" "-12,345"  (lambda () (format #f "~:d" -12345)))
(test "format ~:d" "123,456,789"  (lambda () (format #f "~:d" 123456789)))
(test "format ~:d" "-123,456,789"  (lambda () (format #f "~:d" -123456789)))
(test "format ~:d" "123.456.789"  (lambda () (format #f "~,,'.:d" 123456789)))
(test "format ~:d" "-123.456.789" (lambda () (format #f "~,,'.:d" -123456789)))
(test "format ~:d" "1.2345.6789"  (lambda () (format #f "~,,'.,4:d" 123456789)))
(test "format ~:d" "-1.2345.6789" (lambda () (format #f "~,,'.,4:d" -123456789)))
(test "format ~:d" "    12,345"  (lambda () (format #f "~10:d" 12345)))
(test "format ~:d" "   -12,345"  (lambda () (format #f "~10:d" -12345)))
(test "format ~:d" "   +12,345"  (lambda () (format #f "~10:@d" 12345)))

(test "format ~b" "10101"       (lambda () (format #f "~b" 21)))
(test "format ~b" "-10101"      (lambda () (format #f "~b" -21)))
(test "format ~b" "+10101"      (lambda () (format #f "~@b" 21)))
(test "format ~b" "-10101"      (lambda () (format #f "~@b" -21)))
(test "format ~b" "     10101"  (lambda () (format #f "~10b" 21)))
(test "format ~b" "    -10101"  (lambda () (format #f "~10b" -21)))
(test "format ~b" "    +10101"  (lambda () (format #f "~10@b" 21)))
(test "format ~b" "    -10101"  (lambda () (format #f "~10@b" -21)))
(test "format ~b" "0000010101"  (lambda () (format #f "~10,'0b" 21)))
(test "format ~b" "0000-10101"  (lambda () (format #f "~10,'0b" -21)))

(test "format ~b" "101"         (lambda () (format #f "~,,' ,4:b" 5)))
(test "format ~b" "101 0101"    (lambda () (format #f "~,,' ,4:b" 85)))

(test "format ~o" "12345"       (lambda () (format #f "~o" 5349)))
(test "format ~o" "-12345"      (lambda () (format #f "~o" -5349)))
(test "format ~o" "+12345"      (lambda () (format #f "~@o" 5349)))
(test "format ~o" "-12345"      (lambda () (format #f "~@o" -5349)))
(test "format ~o" "     12345"  (lambda () (format #f "~10o" 5349)))
(test "format ~o" "    -12345"  (lambda () (format #f "~10o" -5349)))
(test "format ~o" "    +12345"  (lambda () (format #f "~10@o" 5349)))
(test "format ~o" "    -12345"  (lambda () (format #f "~10@o" -5349)))
(test "format ~o" "0000012345"  (lambda () (format #f "~10,'0o" 5349)))
(test "format ~o" "0000-12345"  (lambda () (format #f "~10,'0o" -5349)))

(test "format ~x" "12345"       (lambda () (format #f "~x" 74565)))
(test "format ~x" "-12345"      (lambda () (format #f "~x" -74565)))
(test "format ~x" "+12345"      (lambda () (format #f "~@x" 74565)))
(test "format ~x" "-12345"      (lambda () (format #f "~@x" -74565)))
(test "format ~x" "     12345"  (lambda () (format #f "~10x" 74565)))
(test "format ~x" "    -12345"  (lambda () (format #f "~10x" -74565)))
(test "format ~x" "    +12345"  (lambda () (format #f "~10@x" 74565)))
(test "format ~x" "    -12345"  (lambda () (format #f "~10@x" -74565)))
(test "format ~x" "0000012345"  (lambda () (format #f "~10,'0x" 74565)))
(test "format ~x" "0000-12345"  (lambda () (format #f "~10,'0x" -74565)))

(test "format v param" "     12345"
      (lambda () (format #f "~vd" 10 12345)))
(test "format v param" "0000012345"
      (lambda () (format #f "~v,vd" 10 #\0 12345)))

(test "format ~*" "1 2 4 5"
      (lambda () (format #f "~a ~a ~*~a ~a" 1 2 3 4 5)))
(test "format ~*" "1 5"
      (lambda () (format #f "~a ~3*~a" 1 2 3 4 5)))
(test "format ~:*" "1 2 2 3 4 5"
      (lambda () (format #f "~a ~a ~:*~a ~a ~a ~a" 1 2 3 4 5)))
(test "format ~:*" "1 2 3 1 2 3 4 5"
      (lambda () (format #f "~a ~a ~a ~3:*~a ~a ~a ~a ~a" 1 2 3 4 5)))
(test "format ~:*" "1 2 3 1"
      (lambda () (format #f "~a ~a ~a ~3:*~a" 1 2 3 4 5)))
(test "format ~@*" "1 2 5"
      (lambda () (format #f "~a ~a ~4@*~a" 1 2 3 4 5)))

;;-------------------------------------------------------------------
(test-section "nested multi-line comments")

(test "#|...|#" '(foo bar baz)
      (lambda ()
        (read-from-string "(foo bar #||# baz)")))
(test "#|...|#" '(foo bar baz)
      (lambda ()
        (read-from-string "(foo bar #| oof rab |# baz)")))
(test "#|...|#" '(foo bar baz)
      (lambda ()
        (read-from-string "(foo bar #| oof rab) |# baz)")))
(test "#|...|# (multiline)" '(foo bar baz)
      (lambda ()
        (read-from-string "(foo bar #| oof \nrab) |# baz)")))
(test "#|...|# (multiline)" '(foo bar baz)
      (lambda ()
        (read-from-string "(foo bar #| oof \nrab)\n|# baz)")))
(test "#|...|# (nested)" '(foo bar baz)
      (lambda ()
        (read-from-string "(foo bar #| oof #|\nrab)|#\n|# baz)")))
(test "#|...|# (nested)" '(foo bar baz)
      (lambda ()
        (read-from-string "(foo bar #|#|\nrab)|#|# baz)")))
(test "#|...|# (intertwined with string)"
      '(foo bar this is outside of comment "hence this is in a string")
      (lambda ()
        (read-from-string
         "(foo bar #| grok
 \"the following bar-and-sharp terminates the comment |# 
   this is outside of comment
 \"hence this is in a string\")
 ")))
(test "#|...|# (intertwined with string)"
      '(foo bar
        "#| this is a string, not a comment"
        |# and this is not a comment terminator but an escaped symbol|)
      (lambda ()
        (read-from-string
         "(foo bar 
 \"#| this is a string, not a comment\"
 |# and this is not a comment terminator but an escaped symbol|)")))

(test-end)

