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
  )

(test-end)

