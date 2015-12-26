;;
;; test for port I/O functions
;;

(use gauche.test)

(test-start "io")

;;-------------------------------------------------------------------
(test-section "file i/o")

(sys-system "rm -rf tmp2.o")

(test* "open-input-file" (test-error <system-error>)
       (open-input-file "tmp2.o"))

(test* "open-input-file :if-does-not-exist #f" #f
       (open-input-file "tmp2.o" :if-does-not-exist #f))

(test* "open-output-file :if-does-not-exist :error" (test-error)
       (open-output-file "tmp2.o" :if-does-not-exist :error))

(test* "open-output-file :if-does-not-exit #f" #f
       (open-output-file "tmp2.o" :if-does-not-exist #f))

(test* "open-output-file" #t
       (let* ([p (open-output-file "tmp2.o")]
              [r (output-port? p)])
         (display "abcde" p)
         (close-output-port p)
         r))

(test* "open-input-file" 'abcde
       (let* ([p (open-input-file "tmp2.o")]
              [s (read p)])
         (close-input-port p)
         s))

(test* "open-output-file :if-exists :error" (test-error)
       (open-output-file "tmp2.o" :if-exists :error))

(test* "open-output-file :if-exists :supersede" 'cdefg
       (let1 o (open-output-file "tmp2.o")
         (display "cdefg" o)
         (close-output-port o)
         (let* ([i (open-input-file "tmp2.o")]
                [s (read i)])
           (close-input-port i)
           s)))

(test* "open-output-file :if-exists :append" 'cdefghij
       (let1 o (open-output-file "tmp2.o" :if-exists :append)
         (display "hij" o)
         (close-output-port o)
         (let* ([i (open-input-file "tmp2.o")]
                [s (read i)])
           (close-input-port i)
           s)))

(test* "open-output-file :if-exists :append" 'cdefghijklm
       (let1 o (open-output-file "tmp2.o"
                                 :if-exists :append
                                 :if-does-not-exist :error)
         (display "klm" o)
         (close-output-port o)
         (let* ([i (open-input-file "tmp2.o")]
                [s (read i)])
           (close-input-port i)
           s)))

(test* "open-output-file :if-exists :supersede" 'nopqr
       (let1 o (open-output-file "tmp2.o"
                                 :if-exists :supersede
                                 :if-does-not-exist #f)
         (display "nopqr" o)
         (close-output-port o)
         (let* ([i (open-input-file "tmp2.o")]
                [s (read i)])
           (close-input-port i)
           s)))

(sys-system "rm -f tmp2.o")

(test* "call-with-input-file :if-does-not-exist #f" '(#f #f)
       (call-with-input-file "tmp2.o" (^p (list p p))
                             :if-does-not-exist #f))

(test* "with-input-from-file :if-does-not-exist #f" #f
       (with-input-from-file "tmp2.o" (^() 5)
                             :if-does-not-exist #f))

(call-with-output-file "tmp2.o" (^p (display "stu" p)))

(test* "call-with-output-file :if-exists #f" 'stu
       (begin
         (call-with-output-file "tmp2.o" (^p (and p (display "vwx" p)))
                                :if-exists #f)
         (call-with-input-file "tmp2.o" read)))

(test* "with-output-to-file :if-exists #f" 'stu
       (or (with-output-to-file "tmp2.o"
             (^() (display "yz" p) 4)
             :if-exists #f)
           (call-with-input-file "tmp2.o" read)))

;;-------------------------------------------------------------------
(test-section "port-attributes")

(let ([p (open-output-file "tmp.o" :if-exists :supersede)])
  (test* "port-attribute-ref (nonexistent)" (test-error)
         (port-attribute-ref p 'my-attribute))
  (test* "port-attribute-ref (fallback)" 'none
         (port-attribute-ref p 'my-attribute 'none))
  (test* "port-attribute-set! (autocreate)" 'ok
         (begin (port-attribute-set! p 'my-attribute 'ok)
                (port-attribute-ref p 'my-attribute)))
  (test* "port-attribute-create! (existent)" (test-error)
         (port-attribute-create! p 'my-attribute))
  (test* "port-attribute-create! (procedural)" 1
         (let1 val 0
           (define (get port :optional fallback) (inc! val) val)
           (define (set port newval) (set! val newval))
           (port-attribute-create! p 'my-attribute2 get set)
           (port-attribute-ref p 'my-attribute2)))
  (test* "port-attribute (procedural)" 101
         (begin
           (port-attribute-set! p 'my-attribute2 100)
           (port-attribute-ref p 'my-attribute2)))
  (test* "port-attribute again (procedural)" 102
         (port-attribute-ref p 'my-attribute2))
  (test* "port-attribute-create! (procedural, read-only)" 11
         (let1 val 10
           (define (get port :optional fallback) (inc! val) val)
           (port-attribute-create! p 'my-attribute3 get)
           (port-attribute-ref p 'my-attribute3)))
  (test* "port-attribute-set! (procedural, read-only)" (test-error)
         (port-attribute-set! p 'my-attribute3 20))
  (test* "port-attributes" '((my-attribute . ok)
                             (my-attribute2 . 103)
                             (my-attribute3 . 12))
         (alist-delete 'reader-lexical-mode
                       (sort (port-attributes p)
                             string<?
                             ($ symbol->string $ car $))))
  (test* "port-attribute-delete!" '((my-attribute3 . 13))
         (begin
           (port-attribute-delete! p 'my-attribute)
           (port-attribute-delete! p 'my-attribute2)
           (alist-delete 'reader-lexical-mode (port-attributes p))))
  )

;;-------------------------------------------------------------------
(test-section "port-fd-dup!")

(cond-expand
 (gauche.os.windows #f)
 (else
  (test* "port-fd-dup!" '("foo" "bar")
         (let* ([p1 (open-output-file "tmp1.o")]
                [p2 (open-output-file "tmp2.o")])
           (display "foo\n" p1)
           (port-fd-dup! p1 p2)
           (display "bar\n" p1)
           (close-output-port p1)
           (close-output-port p2)
           (list (call-with-input-file "tmp1.o" read-line)
                 (call-with-input-file "tmp2.o" read-line))))

  (test* "port-fd-dup!" '("foo" "bar")
         (let* ([p1 (open-input-file "tmp1.o")]
                [p2 (open-input-file "tmp2.o")]
                [s1 (read-line p1)])
           (port-fd-dup! p1 p2)
           (list s1 (read-line p1))))

  (test* "port-fd-dup!" (test-error)
         (let* ([p1 (open-output-file "tmp1.o")]
                [p2 (open-input-file "tmp2.o")])
           (guard (e (else
                      (close-output-port p1)
                      (close-input-port p2)
                      (raise e)))
             (port-fd-dup! p1 p2))))

  (test* "port-fd-dup!" (test-error)
         (let1 p1 (open-input-file "tmp2.o")
           (guard (e (else
                      (close-input-port p1)
                      (raise e)))
             (port-fd-dup! (open-input-string "") p1))))
  )) ; !gauche.os.windows

;;-------------------------------------------------------------------
(test-section "input ports")

(sys-unlink "tmp1.o")
(with-output-to-file "tmp1.o" (cut display ""))
(test* "read-char (EOF)" #t
       (eof-object? (call-with-input-file "tmp1.o" read-char)))
(test* "read-byte (EOF)" #t
       (eof-object? (call-with-input-file "tmp1.o" read-byte)))
(test* "read-line (EOF)" #t
       (eof-object? (call-with-input-file "tmp1.o" read-line)))
(test* "read-block (EOF)" #t
       (eof-object? (call-with-input-file "tmp1.o"
                      (cut read-block 10 <>))))

(with-output-to-file "tmp1.o" (cut display "ab"))
(test* "read-char (a)" #\a
       (call-with-input-file "tmp1.o" read-char))
(test* "read-byte (a)" 97
       (call-with-input-file "tmp1.o" read-byte))
(test* "read-byte (ungotten)" 97
       (call-with-input-file "tmp1.o"
         (^p (peek-char p) (read-byte p))))
(test* "read-line (a)" "ab"
       (call-with-input-file "tmp1.o" read-line))
(test* "read-byte (ungotten)" 97
       (call-with-input-file "tmp1.o"
         (^p (peek-char p) (read-byte p))))
(test* "peek-byte (a)" '(97 97)
       (call-with-input-file "tmp1.o"
         (^p (let1 a (peek-byte p) (list a (read-byte p))))))
(test* "peek-byte (ungotten)" '(97 97)
       (call-with-input-file "tmp1.o"
         (^p
           (peek-char p) (let1 a (peek-byte p) (list a (read-byte p))))))
(test* "peek-byte and read-char" #\a
       (call-with-input-file "tmp1.o"
         (^p (peek-byte p) (read-char p))))
(test* "peek-byte and peek-char" #\a
       (call-with-input-file "tmp1.o"
         (^p (peek-byte p) (peek-char p))))
(test* "read-block (a)" #*"ab"
       (call-with-input-file "tmp1.o" (cut read-block 10 <>)))
(test* "read-block (ungotten)" #*"ab"
       (call-with-input-file "tmp1.o"
         (^p (peek-char p) (read-block 10 p))))

(with-output-to-file "tmp1.o" (cut display "\n"))
(test* "read-line (LF)" ""
       (call-with-input-file "tmp1.o" read-line))
(test* "read-line (LF, ungotten)" ""
       (call-with-input-file "tmp1.o"
         (^p (peek-char p) (read-line p))))
(with-output-to-file "tmp1.o" (cut display "\r"))
(test* "read-line (CR)" ""
       (call-with-input-file "tmp1.o" read-line))
(test* "read-line (CR, ungotten)" ""
       (call-with-input-file "tmp1.o"
         (^p (peek-char p) (read-line p))))
(with-output-to-file "tmp1.o" (cut display "\n\n"))
(test* "read-line (LF)" '("" "" #t)
       (call-with-input-file "tmp1.o"
         (^_ (let* ([c1 (peek-char _)]
                    [l1 (read-line _)]
                    [c2 (peek-char _)]
                    [l2 (read-line _)]
                    [c2 (peek-char _)]
                    [l3 (read-line _)])
               (list l1 l2 (eof-object? l3))))))
(with-output-to-file "tmp1.o" (cut display "\r\r\n"))
(test* "read-line (CR, CRLF)" '("" "" #t)
       (call-with-input-file "tmp1.o"
         (^_ (let* ([c1 (peek-char _)]
                    [l1 (read-line _)]
                    [c2 (peek-char _)]
                    [l2 (read-line _)]
                    [c2 (peek-char _)]
                    [l3 (read-line _)])
               (list l1 l2 (eof-object? l3))))))
(with-output-to-file "tmp1.o" (cut display "a\r\nb\nc"))
(test* "read-line (mix)" '("a" "b" "c" #t)
       (call-with-input-file "tmp1.o"
         (^_ (let* ([c1 (peek-char _)]
                    [l1 (read-line _)]
                    [c2 (peek-char _)]
                    [l2 (read-line _)]
                    [c2 (peek-char _)]
                    [l3 (read-line _)]
                    [c3 (peek-char _)])
               (list l1 l2 l3 (eof-object? c3))))))

(with-output-to-file "tmp1.o"
  (cut for-each write-byte '(#x80 #xff #x80 #xff #x80 #x0d #x0a #x0d #x0a)))
(test* "read-line (bad sequence)" '(5 0)
       (call-with-input-file "tmp1.o"
         (^_ (let* ([s1 (read-line _ #t)]
                    [s2 (read-line _ #t)]
                    [s3 (read-line _ #t)])
               (and (eof-object? s3)
                    (list (string-size s1) (string-size s2)))))))

(with-output-to-file "tmp1.o"
  (cut display "a b c \"d e\" f g\n(0 1 2\n3 4 5)\n"))

(test* "port->string" "a b c \"d e\" f g\n(0 1 2\n3 4 5)\n"
       (call-with-input-file "tmp1.o" port->string))
(test* "port->list" '(a b c "d e" f g (0 1 2 3 4 5))
       (call-with-input-file "tmp1.o" (cut port->list read <>)))
(test* "port->list" '("a b c \"d e\" f g" "(0 1 2" "3 4 5)")
       (call-with-input-file "tmp1.o" (cut port->list read-line <>)))
(test* "port->string-list" '("a b c \"d e\" f g" "(0 1 2" "3 4 5)")
       (call-with-input-file "tmp1.o" port->string-list))
(test* "port->sexp-list" '(a b c "d e" f g (0 1 2 3 4 5))
       (call-with-input-file "tmp1.o" port->sexp-list))

(test* "port-fold" '((0 1 2 3 4 5) g f "d e" c b a)
       (with-input-from-file "tmp1.o"
         (cut port-fold cons '() read)))
(test* "port-fold" '("3 4 5)" "(0 1 2" "a b c \"d e\" f g")
       (with-input-from-file "tmp1.o"
         (cut port-fold cons '() read-line)))
(test* "port-fold, side-effecting" '(#\c 3 #\b 2 #\a 1)
       (with-input-from-string "abc"
         (cut port-fold (^[x s] `(,x ,(port-tell (current-input-port)) ,@s))
              '() read-char)))
(test* "port-fold-right" '(a b c "d e" f g (0 1 2 3 4 5))
       (with-input-from-file "tmp1.o"
         (cut port-fold-right cons '() read)))

(test* "port-map" '(a b c "d e" f g (0 1 2 3 4 5))
       (with-input-from-file "tmp1.o"
         (cut port-map identity read)))

(test* "port-map, side-effecting" '((#\a 1) (#\b 2) (#\c 3))
       (with-input-from-string "abc"
         (cut port-map (^x `(,x ,(port-tell (current-input-port)))) read-char)))

;;-------------------------------------------------------------------
(test-section "with-ports")

(test* "with-input-from-port" '(#\b #\d #\c #\a)
       (let ([x (open-input-string "ab")]
             [y (open-input-string "cd")]
             [r '()]
             [restart #f])
         (if (call/cc 
              (^[escape]
                (with-input-from-port x
                  (^()
                    (push! r (read-char))
                    (with-input-from-port y
                      (^()
                        (push! r (read-char))
                        (call/cc (^k (set! restart k) (escape #t)))
                        (push! r (read-char))))
                    (push! r (read-char))))
                #f))
           (restart #f)
           r)))

(for-each
 (lambda (with cur name)
   (test* "with-,|name|-to-port" '("ad" "bc")
          (let ([x (open-output-string)]
                [y (open-output-string)]
                [restart #f])
            (if (call/cc 
                 (^[escape]
                   (with x
                     (^()
                       (write-char #\a (cur))
                       (with y
                         (^()
                           (write-char #\b (cur))
                           (call/cc (^k (set! restart k) (escape #t)))
                           (write-char #\c (cur))))
                       (write-char #\d (cur))))
                   #f))
              (restart #f)
              (list (get-output-string x) (get-output-string y))))))
 `(,with-output-to-port ,with-error-to-port)
 `(,current-output-port ,current-error-port)
 '("output" "error"))

(test* "with-ports 1" '("a" "b")
       (let ([o0 (open-output-string)]
             [o1 (open-output-string)])
         (with-ports (open-input-string "abcd") o0 o1
           (^()
             (write-char (read-char))
             (write-char (read-char) (current-error-port))))
         (list (get-output-string o0) (get-output-string o1))))
(test* "with-ports 2" '("B" "A")
       (let ([o0 (open-output-string)]
             [o1 (open-output-string)])
         (with-ports (open-input-string "abcd") o0 o0
           (^()
             (with-ports (open-input-string "ABCD") o1 #f
               (^()
                 (write-char (read-char))
                 (write-char (read-char) (current-error-port))))))
         (list (get-output-string o0) (get-output-string o1))))
(test* "with-ports 3" '("A" "B")
       (let ([o0 (open-output-string)]
             [o1 (open-output-string)])
         (with-ports (open-input-string "abcd") o0 o0
           (^()
             (with-ports (open-input-string "ABCD") #f o1
               (^()
                 (write-char (read-char))
                 (write-char (read-char) (current-error-port))))))
         (list (get-output-string o0) (get-output-string o1))))
(test* "with-ports 4" '("" "ab")
       (let ([o0 (open-output-string)]
             [o1 (open-output-string)])
         (with-ports (open-input-string "abcd") o0 o0
           (^()
             (with-ports #f o1 o1
               (^()
                 (write-char (read-char))
                 (write-char (read-char) (current-error-port))))))
         (list (get-output-string o0) (get-output-string o1))))

;;-------------------------------------------------------------------
(test-section "seeking")

(define (seek-tester1 p)
  (display (read-block 5 p))
  (let1 p0 (port-tell p)
    (port-seek p -3 SEEK_CUR)
    (display (read-block 5 p))
    (port-seek p p0)
    (display (read p))
    (port-seek p 0 SEEK_SET)
    (display (read-block 3 p))
    (port-seek p -3 SEEK_END)
    (display (read p))))

(test* "seek (istr)" "abcdecdefgfghijabchij"
       (with-output-to-string
         (cut call-with-input-string "abcdefghij" seek-tester1)))
(test* "seek (istr, boundary)" #\a
       (call-with-input-string "abcdefghij"
         (^p
           (read-char p)
           (port-seek p -1 SEEK_CUR)
           (read-char p))))
(test* "seek (istr, boundary)" #t
       (call-with-input-string "abcdefghij"
         (^p
           (read-char p)
           (port-seek p 10)
           (eof-object? (read-char p)))))
(test* "seek (istr, out of range)" #f
       (call-with-input-string "abcdefghij"
         (^p
           (read-char p)
           (port-seek p 10 SEEK_CUR))))
(test* "seek (istr, out of range)" #f
       (call-with-input-string "abcdefghij"
         (^p
           (read-char p)
           (port-seek p -2))))
;; ungetc and seek interaction; pointed out by Alex Shinn
(test* "seek (istr, with peek-char)" '("hello" "hello")
       (letrec ([read-zstring
                 (^p (let loop ([ls '()])
                       (let1 c (peek-char p)
                         (if (or (eof-object? c) (eqv? c #\null))
                           (list->string (reverse ls))
                           (begin (read-char p) (loop (cons c ls)))))))])
         (call-with-input-string "hello\0world"
           (^p (let* ([first (read-zstring p)]
                      [dummy (port-seek p 0)]
                      [second (read-zstring p)])
                 (list first second))))))
(test* "seek (istr, with peek-char)" '(#\b #\b)
       (with-input-from-string "abc"
         (^()
           (read-char)
           (let1 c1 (peek-char)
             (port-seek (current-input-port) 0 SEEK_CUR)
             (list c1 (peek-char))))))
(test* "seek (istr, with peek-byte)" '(#x61 #x62)
       (with-input-from-string (rlet1 s (make-byte-string 2 #x61)
                                 (string-byte-set! s 1 #x62))
         (^()
           (let1 b1 (peek-byte)
             (port-seek (current-input-port) 1)
             (list b1 (peek-byte))))))

;; NB: in the following four test, each ifile-ofile test is a pair
;;     (the ofile test depends on the previous state by ifile).  do not
;;     separate them.
(test* "seek (ifile)" "abcdecdefgfghijabchij"
       (begin
         (sys-unlink "test.o")
         (with-output-to-file "test.o" (cut display "abcdefghij"))
         (with-output-to-string
           (cut call-with-input-file "test.o" seek-tester1))))

(test* "seek (ofile)" "--//efg**j++"
       (begin
         (call-with-output-file "test.o"
           (^p
             (port-seek p 0)
             (display "--" p)
             (let1 p0 (port-tell p)
               (port-seek p 0 SEEK_END)
               (display "++" p)
               (port-seek p -5 SEEK_CUR)
               (display "**" p)
               (port-seek p p0)
               (display "//" p)))
           :if-exists :overwrite)
         (call-with-input-file "test.o" port->string)))

(test* "seek (ifile, large)"
       "0000050055019999050100027500"
       (begin
         (sys-unlink "test.o")
         (with-output-to-file "test.o"
           (^() (dotimes (n 10000) (format #t "~4,'0d" n))))
         (with-output-to-string
           (^()
             (call-with-input-file "test.o"
               (^p
                 (display (read-block 4 p))
                 (port-seek p 2000)
                 (display (read-block 4 p))
                 (let1 p0 (port-tell p)
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

(test* "seek (ofile, large)"
       "*0-0*/-0999+"
       (begin
         (call-with-output-file "test.o"
           (^p
             (display "*" p)
             (port-seek p 20000)
             (display "*" p)
             (let1 p0 (port-tell p)
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
           (^()
             (call-with-input-file "test.o"
               (^p
                 (display (read-block 4 p))
                 (port-seek p 20000)
                 (display (read-block 4 p))
                 (port-seek p 39996)
                 (display (read-block 4 p))))))
         ))

(test* "seek (ifile, with peek-char)" '("hello" "hello")
       (letrec ([read-zstring
                 (^p
                   (let loop ((ls '()))
                     (let1 c (peek-char p)
                       (if (or (eof-object? c) (eqv? c #\null))
                         (list->string (reverse ls))
                         (begin (read-char p) (loop (cons c ls)))))))])
         (begin
           (sys-unlink "test.o")
           (with-output-to-file "test.o"
             (cut display "hello\0world"))
           (call-with-input-file "test.o"
             (^p
               (let* ([first (read-zstring p)]
                      [dummy (port-seek p 0)]
                      [second (read-zstring p)])
                 (list first second)))))))

(test* "seek (ifile, with peek-byte)" '(#x61 #x62)
       (begin
         (sys-unlink "test.o")
         (with-output-to-file "test.o"
           (cut display (rlet1 s (make-byte-string 2 #x61)
                          (string-byte-set! s 1 #x62))))
         (call-with-input-file "test.o"
           (^p
             (let1 b1 (peek-byte p)
               (port-seek p 1)
               (list b1 (peek-byte p)))))))

(test* "seek (with appending output)" '(50 100)
       (begin
         (sys-unlink "test.o")
         (with-output-to-file "test.o"
           (cut display (make-string 50 #\a)))
         (call-with-output-file "test.o"
           (^p (let1 a (port-tell p)
                 (display (make-string 50 #\b) p)
                 (list a (port-tell p))))
           :if-exists :append)))

(sys-unlink "test.o")

;;-------------------------------------------------------------------
(test-section "format")

(test* "format ~s" "\"abc\""         (format #f "~s" "abc"))
(test* "format ~s" "\"abc\"     "    (format #f "~10s" "abc"))
(test* "format ~s" "     \"abc\""    (format #f "~10@s" "abc"))
(test* "format ~s" "\"abc\"      "   (format #f "~10,3s" "abc"))
(test* "format ~s" "      \"abc\""   (format #f "~10,3@s" "abc"))
(test* "format ~s" "\"abc\" "        (format #f "~,,1s" "abc"))
(test* "format ~s" " \"abc\""        (format #f "~,,1@s" "abc"))
(test* "format ~s" "\"abc\"*****"    (format #f "~10,,,'*s" "abc"))
(test* "format ~s" "*****\"abc\""    (format #f "~10,,,'*@s" "abc"))

(test* "format ~s" "\"abc\"*****"    (format #f "~10,,,'*,15s" "abc"))
(test* "format ~s" "*****\"abc\""    (format #f "~10,,,'*,15@s" "abc"))
(test* "format ~s" "(\"abc\" \"def\" \"g"
       (format #f "~10,,,'*,15s" '("abc" "def" "ghi" "jkl")))
(test* "format ~s" "(\"abc\" \"def\" \"g"
       (format #f "~10,,,'*,15@s" '("abc" "def" "ghi" "jkl")))
(test* "format ~s" "(\"abc\" \"def ..."
       (format #f "~10,,,'*,15:s" '("abc" "def" "ghi" "jkl")))
(test* "format ~s" "(\"abc\" \"def ..."
       (format #f "~10,,,'*,15@:s" '("abc" "def" "ghi" "jkl")))

(test* "format ~a" "abc"           (format #f "~a" "abc"))
(test* "format ~a" "abc       "    (format #f "~10a" "abc"))
(test* "format ~a" "       abc"    (format #f "~10@a" "abc"))
(test* "format ~a" "abc         "  (format #f "~10,3a" "abc"))
(test* "format ~a" "         abc"  (format #f "~10,3@a" "abc"))
(test* "format ~a" "abc "          (format #f "~,,1a" "abc"))
(test* "format ~a" " abc"          (format #f "~,,1@a" "abc"))
(test* "format ~a" "abc*******"    (format #f "~10,,,'*a" "abc"))
(test* "format ~a" "*******abc"    (format #f "~10,,,'*@a" "abc"))

(test* "format ~a" "(abc def ghi jk"
       (format #f "~10,,,'*,15a" '("abc" "def" "ghi" "jkl")))
(test* "format ~a" "(abc def ghi jk"
       (format #f "~10,,,'*,15@a" '("abc" "def" "ghi" "jkl")))
(test* "format ~a" "(abc def gh ..."
       (format #f "~10,,,'*,15:a" '("abc" "def" "ghi" "jkl")))
(test* "format ~a" "(abc def gh ..."
       (format #f "~10,,,'*,15@:a" '("abc" "def" "ghi" "jkl")))

(test* "format ~d" "12345"       (format #f "~d" 12345))
(test* "format ~d" "-12345"      (format #f "~d" -12345))
(test* "format ~d" "+12345"      (format #f "~@d" 12345))
(test* "format ~d" "-12345"      (format #f "~@d" -12345))
(test* "format ~d" "     12345"  (format #f "~10d" 12345))
(test* "format ~d" "    -12345"  (format #f "~10d" -12345))
(test* "format ~d" "    +12345"  (format #f "~10@d" 12345))
(test* "format ~d" "    -12345"  (format #f "~10@d" -12345))
(test* "format ~d" "0000012345"  (format #f "~10,'0d" 12345))
(test* "format ~d" "0000-12345"  (format #f "~10,'0d" -12345))

(test* "format ~:d" "1"  (format #f "~:d" 1))
(test* "format ~:d" "-1"  (format #f "~:d" -1))
(test* "format ~:d" "12"  (format #f "~:d" 12))
(test* "format ~:d" "-12"  (format #f "~:d" -12))
(test* "format ~:d" "123"  (format #f "~:d" 123))
(test* "format ~:d" "-123"  (format #f "~:d" -123))
(test* "format ~:d" "+123"  (format #f "~:@d" 123))
(test* "format ~:d" "1,234"  (format #f "~:d" 1234))
(test* "format ~:d" "-1,234"  (format #f "~:d" -1234))
(test* "format ~:d" "+1,234"  (format #f "~:@d" 1234))
(test* "format ~:d" "12,345"  (format #f "~:d" 12345))
(test* "format ~:d" "-12,345"  (format #f "~:d" -12345))
(test* "format ~:d" "123,456,789"  (format #f "~:d" 123456789))
(test* "format ~:d" "-123,456,789"  (format #f "~:d" -123456789))
(test* "format ~:d" "123.456.789"  (format #f "~,,'.:d" 123456789))
(test* "format ~:d" "-123.456.789" (format #f "~,,'.:d" -123456789))
(test* "format ~:d" "1.2345.6789"  (format #f "~,,'.,4:d" 123456789))
(test* "format ~:d" "-1.2345.6789" (format #f "~,,'.,4:d" -123456789))
(test* "format ~:d" "    12,345"  (format #f "~10:d" 12345))
(test* "format ~:d" "   -12,345"  (format #f "~10:d" -12345))
(test* "format ~:d" "   +12,345"  (format #f "~10:@d" 12345))

(test* "format ~b" "10101"       (format #f "~b" 21))
(test* "format ~b" "-10101"      (format #f "~b" -21))
(test* "format ~b" "+10101"      (format #f "~@b" 21))
(test* "format ~b" "-10101"      (format #f "~@b" -21))
(test* "format ~b" "     10101"  (format #f "~10b" 21))
(test* "format ~b" "    -10101"  (format #f "~10b" -21))
(test* "format ~b" "    +10101"  (format #f "~10@b" 21))
(test* "format ~b" "    -10101"  (format #f "~10@b" -21))
(test* "format ~b" "0000010101"  (format #f "~10,'0b" 21))
(test* "format ~b" "0000-10101"  (format #f "~10,'0b" -21))

(test* "format ~b" "101"         (format #f "~,,' ,4:b" 5))
(test* "format ~b" "101 0101"    (format #f "~,,' ,4:b" 85))

(test* "format ~o" "12345"       (format #f "~o" 5349))
(test* "format ~o" "-12345"      (format #f "~o" -5349))
(test* "format ~o" "+12345"      (format #f "~@o" 5349))
(test* "format ~o" "-12345"      (format #f "~@o" -5349))
(test* "format ~o" "     12345"  (format #f "~10o" 5349))
(test* "format ~o" "    -12345"  (format #f "~10o" -5349))
(test* "format ~o" "    +12345"  (format #f "~10@o" 5349))
(test* "format ~o" "    -12345"  (format #f "~10@o" -5349))
(test* "format ~o" "0000012345"  (format #f "~10,'0o" 5349))
(test* "format ~o" "0000-12345"  (format #f "~10,'0o" -5349))

(test* "format ~x" "12345"       (format #f "~x" 74565))
(test* "format ~x" "-12345"      (format #f "~x" -74565))
(test* "format ~x" "+12345"      (format #f "~@x" 74565))
(test* "format ~x" "-12345"      (format #f "~@x" -74565))
(test* "format ~x" "     12345"  (format #f "~10x" 74565))
(test* "format ~x" "    -12345"  (format #f "~10x" -74565))
(test* "format ~x" "    +12345"  (format #f "~10@x" 74565))
(test* "format ~x" "    -12345"  (format #f "~10@x" -74565))
(test* "format ~x" "0000012345"  (format #f "~10,'0x" 74565))
(test* "format ~x" "0000-12345"  (format #f "~10,'0x" -74565))

(test* "format ~nr" "wud0up"  (format "~36r" 1985913745))
(test* "format ~nr" "    wud0up"  (format "~36,10r" 1985913745))
(test* "format ~nr" "****wud0up"  (format "~36,10,'*r" 1985913745))
(test* "format ~nr" "***+wud0up"  (format "~36,10,'*@r" 1985913745))
(test* "format ~nR" "WUD0UP"  (format "~36R" 1985913745))

(test* "format v param" "     12345"
       (format #f "~vd" 10 12345))
(test* "format v param" "0000012345"
       (format #f "~v,vd" 10 #\0 12345))

(test* "format ~*" "1 2 4 5"
       (format #f "~a ~a ~*~a ~a" 1 2 3 4 5))
(test* "format ~*" "1 5"
       (format #f "~a ~3*~a" 1 2 3 4 5))
(test* "format ~:*" "1 2 2 3 4 5"
       (format #f "~a ~a ~:*~a ~a ~a ~a" 1 2 3 4 5))
(test* "format ~:*" "1 2 3 1 2 3 4 5"
       (format #f "~a ~a ~a ~3:*~a ~a ~a ~a ~a" 1 2 3 4 5))
(test* "format ~:*" "1 2 3 1"
       (format #f "~a ~a ~a ~3:*~a" 1 2 3 4 5))
(test* "format ~@*" "1 2 5"
       (format #f "~a ~a ~4@*~a" 1 2 3 4 5))

(test* "format incomplete tilde sequence" (test-error)
       (format #f "~"))
(test* "format incomplete tilde sequence" (test-error)
       (format #f "~123"))

(test* "format skip to the end" "||"
       (format #f "|~*|" 1))

;; regression check for format/ss
(test* "format/ss" "z  " (format/ss "~v,a" 3 'z))

;;-------------------------------------------------------------------
(test-section "some corner cases in list reader")

(define (dot-reader-tester str expect)
  (test* (format "dot ~a" str) expect (read-from-string str)))

(dot-reader-tester "(().())"  '(()))
(dot-reader-tester "([].[])"  '(()))
(dot-reader-tester "(x .,y)"  '(x unquote y))
(dot-reader-tester "(x .,@y)" '(x unquote-splicing y))
(dot-reader-tester "(().)"    (test-error <read-error>))
(dot-reader-tester "((). .)"  (test-error <read-error>))


;;-------------------------------------------------------------------
(test-section "nested multi-line comments")

(test* "#|...|#" '(foo bar baz)
       (read-from-string "(foo bar #||# baz)"))
(test* "#|...|#" '(foo bar baz)
       (read-from-string "(foo bar #| oof rab |# baz)"))
(test* "#|...|#" '(foo bar baz)
       (read-from-string "(foo bar #| oof rab) |# baz)"))
(test* "#|...|# (multiline)" '(foo bar baz)
       (read-from-string "(foo bar #| oof \nrab) |# baz)"))
(test* "#|...|# (multiline)" '(foo bar baz)
       (read-from-string "(foo bar #| oof \nrab)\n|# baz)"))
(test* "#|...|# (nested)" '(foo bar baz)
       (read-from-string "(foo bar #| oof #|\nrab)|#\n|# baz)"))
(test* "#|...|# (nested)" '(foo bar baz)
       (read-from-string "(foo bar #|#|\nrab)|#|# baz)"))
(test* "#|...|# (intertwined with string)"
       '(foo bar this is outside of comment "hence this is in a string")
       (read-from-string
        "(foo bar #| grok
 \"the following bar-and-sharp terminates the comment |# 
   this is outside of comment
 \"hence this is in a string\")
 "))
(test* "#|...|# (intertwined with string)"
       '(foo bar
             "#| this is a string, not a comment"
             |# and this is not a comment terminator but an escaped symbol|)
       (read-from-string
        "(foo bar 
 \"#| this is a string, not a comment\"
 |# and this is not a comment terminator but an escaped symbol|)"))

;;-------------------------------------------------------------------
(test-section "#;-style commend (a la Chez)")

(test* "#;123 456" 456
       (read-from-string "#;123 456"))
(test* "#; (123 456) 789" 789
       (read-from-string "#; (123 456) 789"))
(test* "#;#;(123 456) 789 1" 1
       (read-from-string "#;#;(123 456) 789 1"))

(define *counter* 0)

(define-reader-ctor 'countup
  (^() (inc! *counter*) #t))

(test* "S-expression comment and read-time constructor" 2
       (begin (list #,(countup) #;#,(countup) #,(countup))
              *counter*))

;;-------------------------------------------------------------------
(test-section "port->* basic")

;; testing port->string etc.

(define (%test-port->* name proc data writer)
  (test* (format "~a ~s" name data) data
         (begin
           (sys-unlink "tmp2.o")
           (with-output-to-file "tmp2.o" (cut writer data))
           (call-with-input-file "tmp2.o" proc))))
(define-syntax test-port->*
  (syntax-rules ()
    [(_ proc data writer) (%test-port->* 'proc proc data writer)]))

(test-port->* port->string "" display)
(test-port->* port->string "abc" display)
(test-port->* port->string "abc\ndef\n" display)
(test-port->* (.$ string-complete->incomplete port->string)
              #*"\x00\x80\xc0\xd0\xff\xfe\xef" display)

(test-port->* port->string-list '("abc") (cut for-each print <>))
(test-port->* port->string-list '("abc" "def") (cut for-each print <>))
(test-port->* (.$ (cut map string-complete->incomplete <>) port->string-list)
              '(#*"\x00\x80\xc0\xd0\xff\xfe\xef" #*"abc")
              (cut for-each (^z (display z) (newline)) <>))

(test-port->* port->sexp-list '(abc) (cut for-each print <>))

;;-------------------------------------------------------------------
(test-section "coding-aware-port basic")

;; Testing source port _without_ any conversion.  Basically, these
;; tests just checks up the boundary condition of source-port prefetching
;; routine.
;; The actual conversion is tested in ext/charconv.

(define (with-coding-aware-port input proc)
  (let* ([src  (open-input-string input)]
         [wrap (open-coding-aware-port src)])
    (proc src wrap)))

(test* "ownership" #t
       (with-coding-aware-port
        "abc"
        (^[src wrap] (close-input-port wrap) (port-closed? src))))

(test* "read from empty port" '(#t #t #t #t #t)
       (map (^p (with-coding-aware-port "" p))
            (list (^[src wrap] (eof-object? (read-char wrap)))
                  (^[src wrap] (eof-object? (read-byte wrap)))
                  (^[src wrap] (eof-object? (peek-char wrap)))
                  (^[src wrap] (eof-object? (peek-byte wrap)))
                  (^[src wrap] (eof-object? (read-line wrap))))))

(let ([tdata '("abc" "abc\n" "\nabc" "abc\ndef\n"
               "abc\ndef\nghi" "abc\ndef\nghi\n"
               "abc\r\ndef\r\nghi" "abc\r\ndef\r\nghi\r\n"
               "abc\rdef\rghi" "abc\rdef\rghi\r"
               "abc\ndef\nghi\njkl" "abc\ndef\nghi\njkl\n")])
  (test* "read from simple contents"
         tdata
         (map (^i (with-coding-aware-port
                   i
                   (^[src wrap]
                     (let loop ((ch (read-char wrap))
                                (r  '()))
                       (if (eof-object? ch)
                         (list->string (reverse r))
                         (loop (read-char wrap) (cons ch r)))))))
              tdata)))

(let ([tdata '("coding: abcde\naa"
               ";coding:\nabcdef\naa"
               ";coding:\r\nabcdef\r\naa"
               ";coding:\rabcdef\raa"
               "coding: coding: coding:; abcde\naa"
               "coding: coding: coding:; abcde\r\naa"
               ";; co\nding: foobar\naa"
               ";; co\r\nding: foobar\r\naa"
               ";; co\rding: foobar\raa"
               ";; coding:\n;; foobar\naa"
               ";; coding : foobar\naa"
               ";; coding : foobar\r\naa"
               ";; coding : foobar\raa"
               "\n\n;; coding: foobar\naa"
               "\r\n\r\n;; coding: foobar\r\naa"
               "\r\r;; coding: foobar\raa"
               "\n;;    codincodincoding:\naa")])
  (test* "to confuse DFA"
         tdata
         (map (^i (with-coding-aware-port
                   i
                   (^[src wrap]
                     (let loop ((ch (read-char wrap))
                                (r  '()))
                       (if (eof-object? ch)
                         (list->string (reverse r))
                         (loop (read-char wrap) (cons ch r)))))))
              tdata)))

(test-end)

