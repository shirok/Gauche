;;;
;;; Test foreign function interface
;;;

(use gauche.test)

(test-start "gauche.ffi")

;; bail out if we aren't configured to build ffi
#;(unless (file-exists? (string-append "ffi" (gauche-dso-suffix)))
  (test-end)
  (exit 0))

(load "./ffi")
(import gauche.ffi)
(test-module 'gauche.ffi)

(test* "size-of-int8" #t (number? (size-of-int8)))
(test* "size-of-uint8" #t (number? (size-of-uint8)))
(test* "size-of-int16" #t (number? (size-of-int16)))
(test* "size-of-uint16" #t (number? (size-of-uint16)))
(test* "size-of-int32" #t (number? (size-of-int32)))
(test* "size-of-uint32" #t (number? (size-of-uint32)))
(test* "size-of-int64" #t (number? (size-of-int64)))
(test* "size-of-uint64" #t (number? (size-of-uint64)))
(test* "size-of-char" #t (number? (size-of-char)))
(test* "size-of-unsigned-char" #t (number? (size-of-unsigned-char)))
(test* "size-of-short" #t (number? (size-of-short)))
(test* "size-of-unsigned-short" #t (number? (size-of-unsigned-short)))
(test* "size-of-int" #t (number? (size-of-int)))
(test* "size-of-unsigned-int" #t (number? (size-of-unsigned-int)))
(test* "size-of-long" #t (number? (size-of-long)))
(test* "size-of-unsigned-long" #t (number? (size-of-unsigned-long)))
(test* "size-of-float" #t (number? (size-of-float)))
(test* "size-of-double" #t (number? (size-of-double)))
(test* "size-of-string" #t (number? (size-of-string)))
(test* "size-of-pointer" #t (number? (size-of-pointer)))

(test* "align-of-int8" #t (number? (align-of-int8)))
(test* "align-of-uint8" #t (number? (align-of-uint8)))
(test* "align-of-int16" #t (number? (align-of-int16)))
(test* "align-of-uint16" #t (number? (align-of-uint16)))
(test* "align-of-int32" #t (number? (align-of-int32)))
(test* "align-of-uint32" #t (number? (align-of-uint32)))
(test* "align-of-int64" #t (number? (align-of-int64)))
(test* "align-of-uint64" #t (number? (align-of-uint64)))
(test* "align-of-char" #t (number? (align-of-char)))
(test* "align-of-unsigned-char" #t (number? (align-of-unsigned-char)))
(test* "align-of-short" #t (number? (align-of-short)))
(test* "align-of-unsigned-short" #t (number? (align-of-unsigned-short)))
(test* "align-of-int" #t (number? (align-of-int)))
(test* "align-of-unsigned-int" #t (number? (align-of-unsigned-int)))
(test* "align-of-long" #t (number? (align-of-long)))
(test* "align-of-unsigned-long" #t (number? (align-of-unsigned-long)))
(test* "align-of-float" #t (number? (align-of-float)))
(test* "align-of-double" #t (number? (align-of-double)))
(test* "align-of-string" #t (number? (align-of-string)))
(test* "align-of-pointer" #t (number? (align-of-pointer)))

(define so (open-shared-library "./test/f"))

(display "HERE: ")
(write so)
(newline)

(define libcurl (open-shared-library "libcurl"))

(display "HERE: ")
(write libcurl)
(newline)


;(test* "open-shared-library" #t (and so))

#|
(test* "zlib-version" #t (string? (zlib-version)))

(test-section "inflate port")

;;------------------------------------------------------------------
(test-section "checksum procedures")

(test* "crc32 (string)" 0 (crc32 ""))
(test* "crc32 (string)" 2666930069 (crc32 "foobar"))
(test* "crc32 (string)" 4010574376 (crc32 "abc" 8563))
(test* "crc32 (u8vector)" 0 (crc32 #u8()))
(test* "crc32 (u8vector)" 2666930069 (crc32 (string->u8vector "foobar")))
(test* "crc32 (u8vector)" 4010574376 (crc32 (string->u8vector "abc") 8563))
(test* "crc32 (error)" (test-error) (crc32 'foo))

(test* "adler32 (string)" 1 (adler32 ""))
(test* "adler32 (string)" 145425018 (adler32 "foobar"))
(test* "adler32 (string)" 1721967257 (adler32 "abc" 8563))
(test* "adler32 (u8vector)" 1 (adler32 #u8()))
(test* "adler32 (u8vector)" 145425018 (adler32 (string->u8vector "foobar")))
(test* "adler32 (string)" 1721967257 (adler32 (string->u8vector "abc") 8563))
(test* "adler32 (error)" (test-error) (adler32 'foo))

;;------------------------------------------------------------------
(test-section "constant values")

(test* "Z_NO_COMPRESSION"       0 Z_NO_COMPRESSION)
(test* "Z_BEST_SPEED"           1 Z_BEST_SPEED)
(test* "Z_BEST_COMPRESSION"     9 Z_BEST_COMPRESSION)
(test* "Z_DEFAULT_COMPRESSION" -1 Z_DEFAULT_COMPRESSION)
(test* "Z_FILTERED"             1 Z_FILTERED)
(test* "Z_HUFFMAN_ONLY"         2 Z_HUFFMAN_ONLY)
(test* "Z_RLE"                  3 Z_RLE)
(test* "Z_FIXED"                4 Z_FIXED)
(test* "Z_DEFAULT_STRATEGY"     0 Z_DEFAULT_STRATEGY)
(test* "Z_BINARY"               0 Z_BINARY)
(test* "Z_TEXT"                 1 Z_TEXT)
(test* "Z_ASCII"                1 Z_ASCII)
(test* "Z_UNKNOWN"              2 Z_UNKNOWN)

;;------------------------------------------------------------------
(test-section "zlib condition type")

(test* "condition-type? <zlib-error>" #t
       (condition-type? <zlib-error>))
(test* "condition-type? <zlib-need-dict-error>" #t
       (condition-type? <zlib-need-dict-error>))
(test* "condition-type? <zlib-stream-error>" #t
       (condition-type? <zlib-stream-error>))
(test* "condition-type? <zlib-data-error>" #t
       (condition-type? <zlib-data-error>))
(test* "condition-type? <zlib-memory-error>" #t
       (condition-type? <zlib-memory-error>))
(test* "condition-type? <zlib-version-error>" #t
       (condition-type? <zlib-version-error>))

(test* "class-precedence-list <zlib-error>"
       '(<zlib-error> <error> <message-condition> <serious-condition> <condition> <top>)
       (map class-name (class-precedence-list <zlib-error>)))

;;------------------------------------------------------------------
(test-section "deflate port")

(test* "<deflating-port>" <deflating-port>
       (class-of (open-deflating-port (open-output-string))))
(test* "<deflating-port>'s CPL" '(<deflating-port> <port> <top>)
       (map class-name
            (class-precedence-list
             (class-of (open-deflating-port (open-output-string))))))

(test* "open-deflating-port" #t
       (port? (open-deflating-port (open-output-string))))

(test* "open-deflating-port" (test-error)
       (open-deflating-port (standard-input-port)))

(test* "open-deflating-port :compression-level 9" #t
       (port? (open-deflating-port (open-output-string) :compression-level 9)))
(test* "open-deflating-port :compression-level 10" 'OK
       (guard (e ((<zlib-stream-error> e) 'OK)
                 (else 'error))
         (open-deflating-port (open-output-string) :compression-level 10)
         'error))

(test* "open-deflating-port :buffer-size 512" #t
       (port? (open-deflating-port (open-output-string) :buffer-size 512)))

(test* "open-deflating-port :window-bits 15" #t
       (port? (open-deflating-port (open-output-string) :window-bits 15)))
(test* "open-deflating-port :window-bits -1" 'OK
       (guard (e ((<zlib-stream-error> e) 'OK))
         (open-deflating-port (open-output-string) :window-bits -1)))

(test* "open-deflating-port :memory-level 1" #t
       (port? (open-deflating-port (open-output-string) :memory-level 1)))
(test* "open-deflating-port :memory-level -1" 'OK
       (guard (e ((<zlib-stream-error> e) 'OK))
         (open-deflating-port (open-output-string) :memory-level -1)))

(test* "open-deflating-port :strategy Z_HUFFMAN_ONLY" #t
       (port? (open-deflating-port (open-output-string) :strategy Z_HUFFMAN_ONLY)))
(test* "open-deflating-port :strategy 100" 'OK
       (guard (e ((<zlib-stream-error> e) 'OK))
         (open-deflating-port (open-output-string) :strategy 100)))

(test* "open-deflating-port :dictionary \"abc\"" #t
       (port? (open-deflating-port (open-output-string) :dictionary "abc")))
(test* "open-deflating-port :dictionary '()" (test-error)
       (open-deflating-port (open-output-string) :dictionary '()))

(test* "zstream-dictionary-adler32" #f
       (zstream-dictionary-adler32
        (open-deflating-port (open-output-string))))
(test* "zstream-dictionary-adler32" (adler32 "abc")
       (zstream-dictionary-adler32
        (open-deflating-port (open-output-string) :dictionary "abc")))

(test* "deflate-string" #*"x\x9c\x03\0\0\0\0\x01"
       (deflate-string ""))

(test* "deflate-string" #*"x\x9cK\xcb\xcfOJ,\x02\0\x08\xab\x02z"
       (deflate-string "foobar"))

(test* "deflate-string" #*"x\x9c\xed\xc1\x81\0\0\0\0\xc3 \xed\xe1\x9f\xe0\x06U\x01\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\x1c\x03%eI{"
       (deflate-string (make-string 16537 #\x)))

(test* "owner? keyword" #f
       (let1 p (open-output-string)
         (close-output-port (open-deflating-port p))
         (port-closed? p)))

(test* "owner? keyword" #t
       (let1 p (open-output-string)
         (close-output-port (open-deflating-port p :owner? #t))
         (port-closed? p)))

(test* "port-file-number" #f
       (port-file-number (open-deflating-port (open-output-string))))

(test* "port-file-number" 'OK
       (call-with-output-file "test.o"
         (lambda (in)
           (let1 dp (open-deflating-port in)
             (begin0 (and (number? (port-file-number dp)) 'OK)
                     (close-output-port dp))))))
(sys-unlink "test.o")

(test* "port-name" "[deflating (output string port)]"
       (port-name (open-deflating-port (open-output-string))))

(test* "zstream-total-in" 3
       (let1 p (open-deflating-port (open-output-string))
         (display "foo" p)
         (close-output-port p)
         (zstream-total-in p)))

(test* "zstream-total-out" 8
       (let1 p (open-deflating-port (open-output-string))
         (close-output-port p)
         (zstream-total-out p)))

(test* "zstream-params-set!" #*"x\x01\x01\x06\0\xf9\xfffoobar\x08\xab\x02z"
       (call-with-output-string
         (lambda (out)
           (let1 out2 (open-deflating-port out)
             (zstream-params-set! out2
               :compression-level 0 :strategy Z_DEFAULT_STRATEGY)
             (display "foobar" out2)
             (close-output-port out2)))))

(test* "deflating-port-full-flush" #*"x\x9cJLJ\x06\0\0\0\xff\xffKIM\x03\0\x08\x1e\x02V"
       (call-with-output-string
         (lambda (p1)
           (let1 p2 (open-deflating-port p1)
             (display "abc" p2)
             (deflating-port-full-flush p2)
             (display "def" p2)
             (close-output-port p2)))))

(test* "zstream-adler32" 1
       (zstream-adler32 (open-deflating-port (open-output-string))))

(test* "zstream-data-type" #t
       (let1 p (open-deflating-port (open-output-string))
         (close-output-port p)
         (not (not (memq (zstream-data-type p) `(,Z_BINARY ,Z_UNKNOWN))))))

(test* "zstream-data-type" Z_TEXT
       (let1 p (open-deflating-port (open-output-string))
         (display "foo" p)
         (close-output-port p)
         (zstream-data-type p)))

(test* "zstream-data-type" Z_BINARY
       (let1 p (open-deflating-port (open-output-string))
         (display #*"\0" p)
         (close-output-port p)
         (zstream-data-type p)))

;;------------------------------------------------------------------
(test-section "inflate port")

(test* "<inflating-port>" <inflating-port>
       (class-of (open-inflating-port (open-input-string ""))))
(test* "<inflating-port>'s CPL" '(<inflating-port> <port> <top>)
       (map class-name
            (class-precedence-list
             (class-of (open-inflating-port (open-input-string ""))))))

(test* "open-inflating-port" #t
       (port? (open-inflating-port (open-input-string ""))))

(test* "open-inflating-port" (test-error)
       (open-inflating-port (open-output-string)))

(test* "open-inflating-port" #t
       (port? (open-inflating-port (open-input-string "") :buffer-size 512)))

(test* "open-inflating-port :window-bits 8" #t
       (port? (open-inflating-port (open-input-string "") :window-bits 8)))
(test* "open-inflating-port :window-bits -1" 'OK
       (guard (e ((<zlib-stream-error> e) 'OK))
         (open-inflating-port (open-input-string "") :window-bits -1)))

(test* "open-inflating-port :dictionary \"abc\"" "abc"
       (let1 s (call-with-output-string
                 (lambda (p)
                   (let1 p2 (open-deflating-port p :dictionary "abc")
                     (display "abc" p2)
                     (close-output-port p2))))
         (port->string
          (open-inflating-port (open-input-string s) :dictionary "abc"))))

(test* "inflate-string" ""
       (inflate-string (deflate-string "")))

(test* "inflate-string" "foobar"
       (inflate-string (deflate-string "foobar")))

(test* "EOF" (read-from-string "")
       (let1 p (open-inflating-port
                (open-input-string (deflate-string "foobar")))
         (port->string p)
         (read-char p)))

(test* "zstream-total-in" 14
       (let1 p (open-inflating-port (open-input-string #*"x\x9cK\xcb\xcfOJ,\x02\0\x08\xab\x02z"))
         (port->string p)
         (zstream-total-in p)))

(test* "zstream-total-out" 6
       (let1 p (open-inflating-port (open-input-string #*"x\x9cK\xcb\xcfOJ,\x02\0\x08\xab\x02z"))
         (port->string p)
         (zstream-total-out p)))

(test* "read gzip'ed stream" "foobar"
       (let ((str #*"\x1f\x8b\x08\0:c\xc3C\x02\x03K\xcb\xcfOJ,\x02\0\x95\x1f\xf6\x9e\x06\0\0\0"))
         (port->string (open-inflating-port (open-input-string str)
                                          :window-bits (+ 15 16)))))

(test* "gzip-encode-string / gzip-decode-string" "foobar"
       (gzip-decode-string
        (gzip-encode-string "foobar" :compression-level 9)))

(test* "owner? keyword" #t
       (let1 p (open-input-string (deflate-string "foo"))
         (close-input-port (open-inflating-port p :owner? #t))
         (port-closed? p)))

(test* "owner? keyword" #f
       (let1 p (open-input-string (deflate-string "foo"))
         (close-input-port (open-inflating-port p))
         (port-closed? p)))

(test* "read/write" #t
       (let* ((s1 (make-string 16537 #\X))
              (s2 (inflate-string (deflate-string s1))))
         (equal? s1 s2)))

(test* "port-name" "[inflating (input string port)]"
       (port-name (open-inflating-port (open-input-string ""))))

(test* "port-file-number" #f
       (port-file-number (open-inflating-port (open-input-string ""))))

(call-with-output-file "test.o"
  (lambda (o)
    (close-output-port (open-deflating-port o))))

(test* "port-file-number" 'OK
       (call-with-input-file "test.o"
         (lambda (in)
           (let1 p (open-inflating-port in)
             (when (number? (port-file-number p))
               'OK)))))

(sys-unlink "test.o")

(test* "broken data" 'OK
       (guard (e ((and (<zlib-data-error> e)
                       (<io-read-error> e))
                  'OK))
         (port->string (open-inflating-port (open-input-string "abc")))
         'error))

(test* "need-dict-error" 'OK
       (guard (e ((and (<zlib-need-dict-error> e)
                       (<io-read-error> e))
                  'OK))
         (port->string
          (open-inflating-port
           (open-input-string
            (deflate-string "abcdefg" :dictionary "abc"))))))

(test* "inflate-sync" '("abc" 9 "abc")
       (let ((in (open-inflating-port
                  (open-input-string
                   #*"x\x9cJLJ\x06\0\0\0\xff\0JLJ\x06\0\0\0\xff\xffJLJ\x06\0\0\0\xff\xff")))
             (out (open-output-string)))
         (guard (e ((and (<io-read-error> e)
                         (<zlib-data-error> e))
                    (let1 v (inflate-sync in)
                      (list (get-output-string out)
                            v
                            (port->string in)))))
           (let loop ()
             (write-char (read-char in) out)
             (loop))
           'error)))

(test* "inflate-sync" '(#f #t)
       (let* ((in (open-inflating-port
                   (open-input-string
                    (deflate-string "abc"))))
              (v (inflate-sync in)))
         (list v (eof-object? (read-char in)))))

|#
(test-end)
