;; -*- coding: shift_jis -*-
;; Auxiliary test for string <-> uvector conversion  (SJIS)

(test "mb string->u8vector" '#u8(#x82 #xa0 #x82 #xa2 #x82 #xa4)
      (lambda ()
        (string->u8vector "あいう")))
(test "mb string->u8vector (start)" '#u8(#x82 #xa2 #x82 #xa4)
      (lambda ()
        (string->u8vector "あいう" 1)))
(test "mb string->u8vector (start,end)" '#u8(#x82 #xa0 #x82 #xa2)
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
        (u8vector->string '#u8(#x82 #xa0 #x82 #xa2 #x82 #xa4))))
(test "mb u8vector->string (incomplete)" #*"\x82\xa0\x82"
      (lambda ()
        (u8vector->string '#u8(#x82 #xa0 #x82 #xa2 #x82 #xa4) 0 3)))

(test "mb string->u32vector" '#u32(#x82a0 #x41 #x82a2 #x42)
      (lambda ()
        (string->u32vector "あAいB")))
(test "mb string->u32vector (start)" '#u32(#x41 #x82a2 #x42)
      (lambda ()
        (string->u32vector "あAいB" 1)))
(test "mb string->u32vector (start, end)" '#u32(#x82a0 #x41 #x82a2)
      (lambda ()
        (string->u32vector "あAいB" 0 3)))

(test "mb u32vector->string" "あAいB"
      (lambda ()
        (u32vector->string '#u32(#x82a0 #x41 #x82a2 #x42))))
(test "mb u32vector->string (start, end)" "Aい"
      (lambda ()
        (u32vector->string '#u32(#x82a0 #x41 #x82a2 #x42) 1 3)))


