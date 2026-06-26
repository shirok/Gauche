;;;
;;; Test libuuid
;;;

(use gauche.test)
(use gauche.native-type)
(use gauche.uvector)

(test-start "ffi-libuuid")
(use ffi-libuuid)
(test-module 'ffi-libuuid)

(test* "uuid-is-null" 1
       (uuid-is-null (uvector->uuid_t/shared (make-u8vector 16 0))))

(let ([uuid-s "14625c67-fc54-427c-9421-26e7c9223f2a"]
      [uuid-u '#u8(20 98 92 103 252 84 66 124 148 33 38 231 201 34 63 42)])
  (test* "uuid-parse" (list 0 uuid-u)
         (let* ([h (uvector->native-handle #f uuid_t)]
                [r (uuid-parse uuid-s h)])
           (list r (uuid_t->u8vector h))))
  (test* "uuid-unparse" (string-append uuid-s "\x0;")
         (let* ([h (uvector->native-handle uuid-u uuid_t)]
                [b (uvector->native-handle #f (native-type `(.array char (,UUID_STR_LEN))))])
           (uuid-unparse h b)
           (u8vector->string (native-handle-owner b))))

  (let* ([s (string-append "uuid={" uuid-s "}")]
         [start-index (string-size "uuid={")]
         [end-index (+ start-index (string-size uuid-s))]
         [buf (string->u8vector s)]
         [start-handle (uvector->native-handle buf (native-type '(const char*))
                                               start-index)]
         [end-handle  (uvector->native-handle buf (native-type '(const char*))
                                              end-index)])
    (test* "uuid-parse-range" (list 0 uuid-u)
           (let* ([m (make-empty-uuid)]
                  [r (uuid-parse-range start-handle end-handle m)])
             (list r (uuid_t->u8vector m)))))

  (test* "uuic-copy" uuid-u
         (let1 r (make-empty-uuid)
           (uuid-copy r (uvector->uuid_t/shared uuid-u))
           (uuid_t->u8vector r)))

  (let ([uuid-u= (uvector->uuid_t/shared uuid-u)]
        [uuid-u< (let1 z (u8vector-copy uuid-u)
                   (u8vector-set! z 15 41)
                   (uvector->uuid_t/shared z))]
        [uuid-u> (let1 z (u8vector-copy uuid-u)
                   (u8vector-set! z 15 43)
                   (uvector->uuid_t/shared z))])
    (define (cmp a b)
      (let1 r (uuid-compare a b)
        (cond [(= r 0) 0]
              [(< r 0) -1]
              [(> r 0) 1])))
    (test* "uuid-compare" 0 (cmp uuid-u= uuid-u=))
    (test* "uuid-compare" -1 (cmp uuid-u< uuid-u=))
    (test* "uuid-compare" 1 (cmp uuid-u= uuid-u<))
    (test* "uuid-compare" 1 (cmp uuid-u> uuid-u<))
    )

  (let ()
    (define (t-type expected-type generator)
      (test* "uuid-type" expected-type
         (let1 r (make-empty-uuid)
           (generator r)
           (uuid-type r))))

    (t-type UUID_TYPE_DCE_RANDOM uuid-generate-random)
    (t-type UUID_TYPE_DCE_TIME uuid-generate-time)
    (t-type UUID_TYPE_DCE_TIME uuid-generate-time-safe)
    )
  )

(test-end :exit-on-failure #t)
