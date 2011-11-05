;;
;; test for hmac module
;;

(test-section "hmac")

(use gauche.uvector)
(use rfc.md5)
(use rfc.sha1)

(use rfc.hmac)
(test-module 'rfc.hmac)

(define hmac (make <hmac>
	           :key (make-byte-string 16 #x0b)
	           :hasher <md5>))
(hmac-update! hmac "Hi ")
(hmac-update! hmac "There")
(test* "hmac-final!" "9294727a3638bb1c13f48ef8158bfc9d"
       (digest-hexify (hmac-final! hmac)))

(test* "hmac-digest-string" "9294727a3638bb1c13f48ef8158bfc9d"
       (digest-hexify (hmac-digest-string "Hi There"
                                          :key (make-byte-string 16 #x0b)
                                          :hasher <md5>)))

(test* "hmac-digest-string" "750c783e6ab0b503eaa86e310a5db738"
       (digest-hexify (hmac-digest-string "what do ya want for nothing?"
                                          :key "Jefe"
                                          :hasher <md5>)))

(test* "hmac-digest-string" "56be34521d144c88dbb8c733f0e8b3f6"
       (digest-hexify (hmac-digest-string (make-byte-string 50 #xdd)
                                          :key (make-byte-string 16 #xaa)
                                          :hasher <md5>)))

(test* "hmac-digest-string" "6b1ab7fe4bd7bf8f0b62e6ce61b9d0cd"
       (digest-hexify (hmac-digest-string "Test Using Larger Than Block-Size Key - Hash Key First"
                                          :key (make-byte-string 80 #xaa)
                                          :hasher <md5>)))

(test* "hmac-digest-string" "b617318655057264e28bc0b6fb378c8ef146be00"
       (digest-hexify (hmac-digest-string "Hi There"
                                          :key (make-byte-string 20 #x0b)
                                          :hasher <sha1>)))

(test* "hmac-digest-string" "aa4ae5e15272d00e95705637ce8a3b55ed402112"
       (digest-hexify (hmac-digest-string "Test Using Larger Than Block-Size Key - Hash Key First"
                                          :key (make-byte-string 80 #xaa)
                                          :hasher <sha1>)))
