;;
;; test for md5 module
;;

(use gauche.test)
(test-start "md5")

(add-load-path ".")
(load "md5")
(import rfc.md5)
(use util.digest)

(define (hexify string)
  (with-string-io string
    (lambda ()
      (let loop ((b (read-byte)))
	(unless (eof-object? b)
	  (format #t "~2,'0x" b)
	  (loop (read-byte)))))))

(for-each
 (lambda (string hexdigest)
   (test "md5-digest-string" hexdigest
	 (lambda () (hexify (md5-digest-string string))))
   (test "digest-string" hexdigest
 	 (lambda () (hexify (digest-string <md5> string)))))
 '(""
   "a"
   "abc"
   "message digest"
   "abcdefghijklmnopqrstuvwxyz"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
   "12345678901234567890123456789012345678901234567890123456789012345678901234567890")
 '("d41d8cd98f00b204e9800998ecf8427e"
   "0cc175b9c0f1b6a831c399e269772661"
   "900150983cd24fb0d6963f7d28e17f72"
   "f96b697d7cb7938d525a2f31aaf161d0"
   "c3fcd3d76192e4007dfb496cca67e13b"
   "d174ab98d277d9f5a5611c2c9f419d9f"
   "57edf4a22be3c955ac49da2e2107b67a"))

(test-end)
