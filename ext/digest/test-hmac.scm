;;
;; test for hmac module
;;

(use gauche.test)
(test-start "hmac")

;; kludge to run "in-place" test
(add-load-path ".")
(add-load-path "../uvector")

(load "md5")
(import rfc.md5)
(load "uvector")
(import gauche.uvector)
(use rfc.hmac)


(define (hexify string)
  (with-string-io string
    (lambda ()
      (let loop ((b (read-byte)))
	(unless (eof-object? b)
	  (format #t "~2,'0x" b)
	  (loop (read-byte)))))))

(define hmac (make <hmac>
	           :key (make-byte-string 16 #x0b)
	           :hasher <md5>))
(hmac-update hmac "Hi There")
(test "hmac-final" "9294727a3638bb1c13f48ef8158bfc9d"
      (lambda () (hexify (hmac-final hmac))))

(test "hmac-digest-string" "9294727a3638bb1c13f48ef8158bfc9d"
      (lambda ()
	(hexify (hmac-digest-string "Hi There"
				    :key (make-byte-string 16 #x0b)
				    :hasher <md5>))))

(test "hmac-digest-string" "750c783e6ab0b503eaa86e310a5db738"
      (lambda ()
	(hexify (hmac-digest-string "what do ya want for nothing?"
				    :key "Jefe"
				    :hasher <md5>))))

(test "hmac-digest-string" "56be34521d144c88dbb8c733f0e8b3f6"
      (lambda ()
	(hexify (hmac-digest-string (make-byte-string 50 #xdd)
				    :key (make-byte-string 16 #xaa)
				    :hasher <md5>))))

(test "hmac-digest-string" "6b1ab7fe4bd7bf8f0b62e6ce61b9d0cd"
      (lambda ()
	(hexify (hmac-digest-string "Test Using Larger Than Block-Size Key - Hash Key First"
				    :key (make-byte-string 80 #xaa)
				    :hasher <md5>))))

(test-end)
