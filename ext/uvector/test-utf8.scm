;; -*- coding: utf-8 -*-
;; Auxiliary test for string <-> uvector conversion  (UTF-8)

(test "mb string->u8vector" '#u8(#xe3 #x81 #x82 #xe3 #x81 #x84 #xe3 #x81 #x86)
      (lambda ()
        (string->u8vector "あいう")))
(test "mb string->u8vector (start)" '#u8(#xe3 #x81 #x84 #xe3 #x81 #x86)
      (lambda ()
        (string->u8vector "あいう" 1)))
(test "mb string->u8vector (start,end)" '#u8(#xe3 #x81 #x82 #xe3 #x81 #x84)
      (lambda ()
        (string->u8vector "あいう" 0 2)))
(test "mb string->u8vector (start,end)" 'error
      (lambda ()
        (with-error-handler
            (^e 'error)
          (lambda ()
            (string->u8vector "あいう" 0 4)))))

(test "mb u8vector->string" "あいう"
      (lambda ()
        (u8vector->string '#u8(#xe3 #x81 #x82 #xe3 #x81 #x84 #xe3 #x81 #x86))))
(test "mb u8vector->string (incomplete)" #*"\xe3\x81\x82\xe3"
      (lambda ()
        (u8vector->string '#u8(#xe3 #x81 #x82 #xe3 #x81 #x84 #xe3 #x81 #x86) 0 4)))

(test "mb string->u32vector" '#u32(#x3042 #x41 #x3044 #x42)
      (lambda ()
        (string->u32vector "あAいB")))
(test "mb string->u32vector (start)" '#u32(#x41 #x3044 #x42)
      (lambda ()
        (string->u32vector "あAいB" 1)))
(test "mb string->u32vector (start, end)" '#u32(#x3042 #x41 #x3044)
      (lambda ()
        (string->u32vector "あAいB" 0 3)))

(test "mb u32vector->string" "あAいB"
      (lambda ()
        (u32vector->string '#u32(#x3042 #x41 #x3044 #x42))))
(test "mb u32vector->string (start, end)" "Aい"
      (lambda ()
        (u32vector->string '#u32(#x3042 #x41 #x3044 #x42) 1 3)))
