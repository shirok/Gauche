;;
;; test for sha1 module
;;

(use gauche.test)
(test-start "sha1")

(if (member "." *load-path*) ;; trick to allow in-place test
  (load "sha1")
  (load "rfc/sha1"))
(import rfc.sha1)
(test-module 'rfc.sha1)

(for-each
 (lambda (args)
   (test "sha1-digest-string" (car args)
	 (lambda () (digest-hexify (apply sha1-digest-string (cdr args)))))
   (test "digest-string" (car args)
 	 (lambda () (digest-hexify (apply digest-string <sha1> (cdr args))))))
 `(("a9993e364706816aba3e25717850c26c9cd0d89d" "abc")
   ("84983e441c3bd26ebaae4aa1f95129e5e54670f1" "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
   ("34aa973cd4c4daa4f61eeb2bdbad27316534016f"
    ,(make-string 1000000 #\a))
   ("dea356a2cddd90c7a7ecedc5ebb563934f460452"
    ,(with-output-to-string (lambda () (dotimes (n 10) (display "0123456701234567012345670123456701234567012345670123456701234567")))))))

(test-end)
