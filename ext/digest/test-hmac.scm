;;
;; test for hmac module
;;

(test-section "hmac")

(use gauche.uvector)
(use rfc.md5)
(use rfc.sha)

(use rfc.hmac)
(test-module 'rfc.hmac)

(define hmac (make <hmac>
                   :key (make-byte-string 16 #x0b)
                   :hasher <md5>))
(hmac-update! hmac "Hi ")
(hmac-update! hmac "There")
(test* "hmac-final!" "9294727a3638bb1c13f48ef8158bfc9d"
       (digest-hexify (hmac-final! hmac)))

(define *hmac-test-keys*
  '("01234567890123456789"
    "0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij"
    "0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij0123456789abcdefghij"))
(define *hmac-test-message* "Hi, There")

#|
The following test data can be obtained by the following snippet:

(use gauche.process)

(begin
  (with-output-to-file "tmp.o" (^[] (display *hmac-test-message*)))
  (print "(define *hmac-test-results* `(")
  (dolist [class '(md5 sha1 sha224 sha256 sha384 sha512)]
    (print "(,<" class "> ")
    (dolist [key *hmac-test-keys*]
      (write (regexp-replace
              #/^[^=]*=\s*/
              (process-output->string
               `(openssl dgst ,#"-~class" -hmac ~key "tmp.o"))
              ""))
      (print))
    (print ")"))
  (print "))")
  (sys-unlink "tmp.o"))
|#

(define *hmac-test-results* `(
(,<md5> 
"32fad29f907cdf38becd0746dbfb5c8c"
"c93e25f979c2698af603be3cdc3bde61"
"1ba67a2a0394f3179ea8dc5dfd80c182"
)
(,<sha1> 
"7a24d5f70eb816abf08ddbff3839810dab31f899"
"f80dae80d19cf4557397e4752a676065f03a9c7e"
"6ef42ff9c975dc8e454ab8204f2f7b72cfda03fd"
)
(,<sha224> 
"349641db8a8cebb0792f922324dcbc9ffbd889b782f7b9d4f48fc3e0"
"df2f9f9085a92393d8559f34127c8d5828e0fa1805a0d28aeddbf3ba"
"7d32f9d373095e6a62d077d84673e2077e1afa2c88c08181c85b9ee9"
)
(,<sha256> 
"c54b248f72bd92455f866cd77b171a3dc74f6b2c1b1f19a51375f723506c1f04"
"68ff224de29d79ca6bbc273777de97ecf8ee57d1ac9360d365887dc6ab693218"
"e8da3ca91a29a996bf3bb1b140c5d55a3bda661ac54fae0ceca63b74a4254896"
)
(,<sha384> 
"8cf00284dd899f69e9a173dc142e0f53e2c8629a523ae8b16be5415c3f236379cb5bfc512df29caea4b28cf27d8a34e6"
"3765c9af8409f6fbc00a25560c0ea611bf2b78623a29c2c53f538c402fa2bc3fa0554a8b9ee0246be6dd775688c6531f"
"e83a53cff39e6274cf3bc271da01f3010e4cbd4ac4caf2c5d0e04072b503f8b3b9f0730027e31fa1f058c6a588273a65"
)
(,<sha512> 
"9324878a0be9ca269a6949ce9c21703443afc2fb440c4b5d3d5da2419fffe69dad7e005df799cff29c7506f1e43d77a4fe759a0261ffb675c9e44cdaf79b3659"
"0f6fad711fedc681106dce5f38be92229d344ce7757ff77a269df5ef100566238cc7b4220ef21c835caf5613e0400a840a7b33315c013158d25b56f4fbecd4e3"
"ba9327d25e066a5d9d486cc53c054a24da2f1848f45168c20f773e339c8c8b8a66e13e1fe8772ca5007f3857e42c06e996e2e02cdbf6b4f31b69df3d51ec470d"
)
))

(dolist [tests *hmac-test-results*]
  (let ([class (car tests)]
        [hashes (cdr tests)]
        [keys *hmac-test-keys*])
    (for-each
     (^[hash key]
       (test* #"hmac-digest-string ~(class-name class) keylen=~(string-length key)"
              hash
              (digest-hexify (hmac-digest-string *hmac-test-message*
                                                 :key key
                                                 :hasher class))))
     hashes
     keys)))
