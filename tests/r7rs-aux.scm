;;
;; Additional R7RS tests that hasn't covered by other tests
;; NB: Most R7RS-large libraries are tested via its srfi counterparts.
;;
;; This is called after ext/* are tested.
;;

(use gauche.test)
(test-start "r7rs-aux")

(test-section "scheme.bytevector")

;; scheme.bytevector exports a few conflicting symbols, so we isolate it.
(define-module bytevector-test
  (use gauche.test)
  (use scheme.bytevector)
  (test-module 'scheme.bytevector)

  (let1 lis '(big big-endian little little-endian arm-little-endian)
    (test* "endianness" lis
           (map (^x (eval `(endianness ,x) (current-module))) lis))
    (test* "endianness" (test-error)
           (eval '(endianness wha) (current-module)))
    (test* "native-endianness" #t
           (boolean (memq (native-endianness) lis))))

  (test* "bytevector-[su8]-ref/set!" '#u8(5 255 0 3 4 5)
         (rlet1 v (u8-list->bytevector '(0 1 2 3 4 5))
           (bytevector-u8-set! v 0 (bytevector-u8-ref v 5))
           (bytevector-s8-set! v 1 -1)
           (bytevector-s8-set! v 2 (+ (bytevector-s8-ref v 1) 1))))
  (test* "bytevector->u8-list" '(0 255 1 254 2 253)
         (bytevector->u8-list '#u8(0 255 1 254 2 253)))

  ;; https://github.com/shirok/Gauche/issues/1290
  (let ([uv '#u8(#x01 #x02 #x03 #x04 #x05)])
    (test* "bytevector-uint-ref big" #x0102030405
           (bytevector-uint-ref uv 0 (endianness big) 5))
    (test* "bytevector-uint-ref little" #x0504030201
           (bytevector-uint-ref uv 0 (endianness little) 5)))
  (let ([uv '#u8(#xff #xfe #xfd #xfc #xfb)])
    (test* "bytevector-sint-ref big" #x-0001020305
           (bytevector-sint-ref uv 0 (endianness big) 5))
    (test* "bytevector-sint-ref little" #x-0403020101
           (bytevector-sint-ref uv 0 (endianness little) 5)))
  )

(test-end)
