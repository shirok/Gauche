;;
;; testing termios
;;

(use gauche.test)

(test-start "vport")
(use gauche.vport)
(test-module 'gauche.vport)

;;-----------------------------------------------------------
(test-section "virtual-input-port")

(test* "vanilla" (make-list 8 #t)
       (let ((p (make <virtual-input-port>)))
         (list (eof-object? (read-byte p))
               (eof-object? (read-char p))
               (eof-object? (read-line p))
               (eof-object? (read-block 10 p))
               (byte-ready? p)
               (char-ready? p)
               (eof-object? (peek-byte p))
               (eof-object? (peek-char p)))))

(test* "getc" '(#\a #\a #\a 97 97 97 #*"aaaaaaaaaa")
       (let* ((p (make <virtual-input-port> :getc (lambda () #\a)))
              (c0 (read-char p))
              (c1 (read-char p))
              (c2 (read-char p))
              (b0 (read-byte p))
              (b1 (read-byte p))
              (b2 (read-byte p))
              (s  (read-block 10 p)))
         (list c0 c1 c2 b0 b1 b2 s)))

(test* "getb" '(#\a #\a #\a 97 97 97 #*"aaaaaaaaaa")
       (let* ((p (make <virtual-input-port> :getb (lambda () 97)))
              (c0 (read-char p))
              (c1 (read-char p))
              (c2 (read-char p))
              (b0 (read-byte p))
              (b1 (read-byte p))
              (b2 (read-byte p))
              (s  (read-block 10 p)))
         (list c0 c1 c2 b0 b1 b2 s)))

(test* "peekc/peekb & getc" '(#\a #\a 98 #\b)
       (let* ((x '(#\a #\b #\c))
              (p (make <virtual-input-port>
                   :getc (lambda ()
                           (and (pair? x) (pop! x)))))
              (c0 (peek-char p))
              (c1 (read-char p))
              (c2 (peek-byte p))
              (c3 (read-char p))
              )
         (list c0 c1 c2 c3)))

(test* "peekc/peekb & getb" '(97 #\a #\b 98)
       (let* ((x '(97 98 99))
              (p (make <virtual-input-port>
                   :getb (lambda ()
                           (and (pair? x) (pop! x)))))
              (c0 (peek-byte p))
              (c1 (read-char p))
              (c2 (peek-char p))
              (c3 (read-byte p)))
         (list c0 c1 c2 c3)))

(test* "getc -> read-line" "abcd"
       (let* ((x '(#\a #\b #\c #\d))
              (p (make <virtual-input-port>
                   :getc (lambda ()
                           (and (pair? x) (pop! x))))))
         (read-line p)))

(test* "getc -> read-line" '("abcd" "efg")
       (let* ((x '(#\a #\b #\c #\d #\newline #\e #\f #\g #\newline))
              (p (make <virtual-input-port>
                   :getc (lambda ()
                           (and (pair? x) (pop! x))))))
         (port->string-list p)))
         
(test* "getb -> read-line" '("abcd" "efg")
       (let* ((x '(97 98 99 100 10 101 102 103 10))
              (p (make <virtual-input-port>
                   :getb (lambda ()
                           (and (pair? x) (pop! x))))))
         (port->string-list p)))

(test* "getc -> read-block" #*"abcd\ne"
       (let* ((x '(#\a #\b #\c #\d #\newline #\e #\f #\g #\newline))
              (p (make <virtual-input-port>
                   :getc (lambda ()
                           (and (pair? x) (pop! x))))))
         (read-block 6 p)))

(test* "getc -> read-block" #*"abcd\nefg\n"
       (let* ((x '(#\a #\b #\c #\d #\newline #\e #\f #\g #\newline))
              (p (make <virtual-input-port>
                   :getc (lambda ()
                           (and (pair? x) (pop! x))))))
         (read-block 200 p)))

(test* "getb -> read-block" #*"abcd\ne"
       (let* ((x '(97 98 99 100 10 101 102 103 10))
              (p (make <virtual-input-port>
                   :getb (lambda ()
                           (and (pair? x) (pop! x))))))
         (read-block 6 p)))

(test* "getb -> read-block" #*"abcd\nefg\n"
       (let* ((x '(97 98 99 100 10 101 102 103 10))
              (p (make <virtual-input-port>
                   :getb (lambda ()
                           (and (pair? x) (pop! x))))))
         (read-block 200 p)))

(test* "gets -> read-block" #*"this is fr"
       (let* ((buf "this is from gets")
              (p (make <virtual-input-port>
                   :getc (lambda () #f)
                   :getb (lambda () #f)
                   :gets (lambda (length)
                           (if (< length (string-size buf))
                             (substring buf 0 length)
                             buf)))))
         (read-block 10 p)))

;;-----------------------------------------------------------
(test-section "virtual-output-port")

(test* "putc" "abcdef"
       (call-with-output-string
         (lambda (o)
           (let* ((p (make <virtual-output-port>
                       :putc (lambda (c) (write-char c o)))))
             (write-char #\a p)
             (display "bcd" p)
             (write-byte 101 p)
             (write-char #\f p)
             (get-output-string o)))))

(test* "putb" "abcdef"
       (call-with-output-string
         (lambda (o)
           (let* ((p (make <virtual-output-port>
                       :putb (lambda (b) (write-byte b o)))))
             (write-char #\a p)
             (display "bcd" p)
             (write-byte 101 p)
             (write-char #\f p)
             (get-output-string o)))))

(test* "puts" "bcdxyz"
       (call-with-output-string
         (lambda (o)
           (let* ((p (make <virtual-output-port>
                       :putb (lambda (b) #f)
                       :putc (lambda (c) #f)
                       :puts (lambda (s) (display s o)))))
             (write-char #\a p)
             (display "bcd" p)
             (write-byte 101 p)
             (write-char #\f p)
             (display "xyz" p)
             (get-output-string o)))))

(test-end)
