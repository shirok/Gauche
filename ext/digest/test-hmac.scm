;;
;; test for hmac module
;;

(use gauche.test)
(test-start "hmac")

;; kludge for "in-place" test
(add-load-path ".")
(load "md5")
(import rfc.md5)
(load "sha1")
(import rfc.sha1)

(add-load-path "../uvector")
(load "uvector")
(import gauche.uvector)

(use rfc.hmac)

(define (hexify string)
  (with-string-io string
    (lambda ()
      (port-for-each (lambda (x) (format #t "~2,'0x" x)) read-byte))))

(define hmac (make <hmac>
	           :key (make-byte-string 16 #x0b)
	           :hasher <md5>))
(hmac-update! hmac "Hi ")
(hmac-update! hmac "There")
(test "hmac-final!" "9294727a3638bb1c13f48ef8158bfc9d"
      (lambda () (hexify (hmac-final! hmac))))

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

(test "hmac-digest-string" "b617318655057264e28bc0b6fb378c8ef146be00"
      (lambda ()
	(hexify (hmac-digest-string "Hi There"
				    :key (make-byte-string 20 #x0b)
				    :hasher <sha1>))))

(test "hmac-digest-string" "aa4ae5e15272d00e95705637ce8a3b55ed402112"
      (lambda ()
	(hexify (hmac-digest-string "Test Using Larger Than Block-Size Key - Hash Key First"
				    :key (make-byte-string 80 #xaa)
				    :hasher <sha1>))))

(test-end)
