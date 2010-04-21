(use gauche.test)
(use srfi-13)
(test-start "bcrypt")

(use crypt.bcrypt)
(test-module 'crypt.bcrypt)

;; Test data is taken from wrapper.c
(define (test-hashpw hash pw)
  (test* "bcrypt-hashpw" hash (bcrypt-hashpw pw hash)))

(test-hashpw "$2a$05$CCCCCCCCCCCCCCCCCCCCC.E5YPO9kmyuRGyh0XouQYb4YMJKvyOeW"
             "U*U")
(test-hashpw "$2a$05$CCCCCCCCCCCCCCCCCCCCC.VGOzA784oUp/Z0DY336zx7pLYAy0lwK"
             "U*U*")
(test-hashpw "$2a$05$XXXXXXXXXXXXXXXXXXXXXOAcXxm9kjPGEMsLznoKqmqw7tc8WCx4a"
             "U*U*U")
(test-hashpw "$2a$05$abcdefghijklmnopqrstuu5s2v8.iXieOjg/.AySBTTZIIVFJeBui"
             "0123456789abcdefghijklmnopqrstuvwxyz\
              ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

(test* "bcrypt-gensalt" "$2a$12$"
       (string-take (bcrypt-gensalt :count 12) 7))

(test-end)
