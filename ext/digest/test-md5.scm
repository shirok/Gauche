;;
;; test for md5 module
;;

(test-section "md5")

(use rfc.md5)
(test-module 'rfc.md5)

(for-each
 (^[args]
   (test* "md5-digest-string" (car args)
          (digest-hexify (apply md5-digest-string (cdr args))))
   (test* "digest-string" (car args)
          (digest-hexify (apply digest-string <md5> (cdr args)))))
 '(("d41d8cd98f00b204e9800998ecf8427e" "")
   ("0cc175b9c0f1b6a831c399e269772661" "a")
   ("900150983cd24fb0d6963f7d28e17f72" "abc")
   ("f96b697d7cb7938d525a2f31aaf161d0" "message digest")
   ("c3fcd3d76192e4007dfb496cca67e13b" "abcdefghijklmnopqrstuvwxyz")
   ("d174ab98d277d9f5a5611c2c9f419d9f" "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789")
   ("57edf4a22be3c955ac49da2e2107b67a" "12345678901234567890123456789012345678901234567890123456789012345678901234567890")))

