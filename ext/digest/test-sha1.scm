;;
;; test for sha1 module
;;

(use gauche.test)
(test-start "sha1")

(add-load-path ".")
(load "sha1")
(import rfc.sha1)
(use util.digest)

(define (hexify string)
  (with-string-io string
    (lambda ()
      (port-for-each (lambda (x) (format #t "~2,'0x" x)) read-byte))))

(for-each
 (lambda (args)
   (test "sha1-digest-string" (car args)
	 (lambda () (hexify (apply sha1-digest-string (cdr args)))))
   (test "digest-string" (car args)
 	 (lambda () (hexify (apply digest-string <sha1> (cdr args))))))
 '(("a9993e364706816aba3e25717850c26c9cd0d89d" "abc")
   ("84983e441c3bd26ebaae4aa1f95129e5e54670f1" "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")))

(test-end)
