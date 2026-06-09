;;;
;;; Test libuuid
;;;

(use gauche.test)
(use gauche.native-type)

(test-start "ffi-libuuid")
(use ffi-libuuid)
(test-module 'ffi-libuuid)

(test* "uuid-is-null" 1
       (uuid-is-null (uvector->uuid_t/shared (make-u8vector 16 0))))

(let ([uuid-s "14625c67-fc54-427c-9421-26e7c9223f2a"]
      [uuid-u '#u8(20 98 92 103 252 84 66 124 148 33 38 231 201 34 63 42)]
      [ubuf (make-u8vector 16)]
      [sbuf (make-u8vector UUID_STR_LEN)])
  (test* "uuid-parse" (list 0 uuid-u)
         (let* ([h (uvector->native-handle ubuf uuid_t)]
                [r (uuid-parse uuid-s h)])
           (list r (uuid_t->u8vector h))))
  (test* "uuid-unparse" (string-append uuid-s "\x0;")
         (let* ([h (uvector->native-handle uuid-u uuid_t)]
                [b (uvector->native-handle sbuf (native-type 'uint8_t*))])
           (uuid-unparse h b)
           (u8vector->string sbuf)))
  )

;; If you don't want `gosh' to exit with nonzero status even if
;; the test fails, pass #f to :exit-on-failure.
(test-end :exit-on-failure #t)
