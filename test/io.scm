;;
;; test for string related functions
;;

(use gauche.test)

(test-start "io")

;;-------------------------------------------------------------------
(test-section "format")

(test "format ~s" "\"abc\""
      (lambda () (format #f "~s" "abc")))
(test "format ~s" "\"abc\"     "
      (lambda () (format #f "~10s" "abc")))
(test "format ~s" "     \"abc\""
      (lambda () (format #f "~10@s" "abc")))
(test "format ~s" "\"abc\"      "
      (lambda () (format #f "~10,3s" "abc")))
(test "format ~s" "      \"abc\""
      (lambda () (format #f "~10,3@s" "abc")))
(test "format ~s" "\"abc\" "
      (lambda () (format #f "~,,1s" "abc")))
(test "format ~s" " \"abc\""
      (lambda () (format #f "~,,1@s" "abc")))
(test "format ~s" "\"abc\"*****"
      (lambda () (format #f "~10,,,'*s" "abc")))
(test "format ~s" "*****\"abc\""
      (lambda () (format #f "~10,,,'*@s" "abc")))

(test "format ~a" "abc"
      (lambda () (format #f "~a" "abc")))
(test "format ~a" "abc       "
      (lambda () (format #f "~10a" "abc")))
(test "format ~a" "       abc"
      (lambda () (format #f "~10@a" "abc")))
(test "format ~a" "abc         "
      (lambda () (format #f "~10,3a" "abc")))
(test "format ~a" "         abc"
      (lambda () (format #f "~10,3@a" "abc")))
(test "format ~a" "abc "
      (lambda () (format #f "~,,1a" "abc")))
(test "format ~a" " abc"
      (lambda () (format #f "~,,1@a" "abc")))
(test "format ~a" "abc*******"
      (lambda () (format #f "~10,,,'*a" "abc")))
(test "format ~a" "*******abc"
      (lambda () (format #f "~10,,,'*@a" "abc")))

(test "format ~d" "12345"       (lambda () (format #f "~d" 12345)))
(test "format ~d" "-12345"      (lambda () (format #f "~d" -12345)))
(test "format ~d" "+12345"      (lambda () (format #f "~@d" 12345)))
(test "format ~d" "-12345"      (lambda () (format #f "~@d" -12345)))
(test "format ~d" "     12345"  (lambda () (format #f "~10d" 12345)))
(test "format ~d" "    -12345"  (lambda () (format #f "~10d" -12345)))
(test "format ~d" "    +12345"  (lambda () (format #f "~10@d" 12345)))
(test "format ~d" "    -12345"  (lambda () (format #f "~10@d" -12345)))
(test "format ~d" "0000012345"  (lambda () (format #f "~10,'0d" 12345)))
(test "format ~d" "0000-12345"  (lambda () (format #f "~10,'0d" -12345)))

(test "format ~:d" "1"  (lambda () (format #f "~:d" 1)))
(test "format ~:d" "-1"  (lambda () (format #f "~:d" -1)))
(test "format ~:d" "12"  (lambda () (format #f "~:d" 12)))
(test "format ~:d" "-12"  (lambda () (format #f "~:d" -12)))
(test "format ~:d" "123"  (lambda () (format #f "~:d" 123)))
(test "format ~:d" "-123"  (lambda () (format #f "~:d" -123)))
(test "format ~:d" "+123"  (lambda () (format #f "~:@d" 123)))
(test "format ~:d" "1,234"  (lambda () (format #f "~:d" 1234)))
(test "format ~:d" "-1,234"  (lambda () (format #f "~:d" -1234)))
(test "format ~:d" "+1,234"  (lambda () (format #f "~:@d" 1234)))
(test "format ~:d" "12,345"  (lambda () (format #f "~:d" 12345)))
(test "format ~:d" "-12,345"  (lambda () (format #f "~:d" -12345)))
(test "format ~:d" "123,456,789"  (lambda () (format #f "~:d" 123456789)))
(test "format ~:d" "-123,456,789"  (lambda () (format #f "~:d" -123456789)))
(test "format ~:d" "123.456.789"  (lambda () (format #f "~,,'.:d" 123456789)))
(test "format ~:d" "-123.456.789" (lambda () (format #f "~,,'.:d" -123456789)))
(test "format ~:d" "1.2345.6789"  (lambda () (format #f "~,,'.,4:d" 123456789)))
(test "format ~:d" "-1.2345.6789" (lambda () (format #f "~,,'.,4:d" -123456789)))
(test "format ~:d" "    12,345"  (lambda () (format #f "~10:d" 12345)))
(test "format ~:d" "   -12,345"  (lambda () (format #f "~10:d" -12345)))
(test "format ~:d" "   +12,345"  (lambda () (format #f "~10:@d" 12345)))

(test "format ~b" "10101"       (lambda () (format #f "~b" 21)))
(test "format ~b" "-10101"      (lambda () (format #f "~b" -21)))
(test "format ~b" "+10101"      (lambda () (format #f "~@b" 21)))
(test "format ~b" "-10101"      (lambda () (format #f "~@b" -21)))
(test "format ~b" "     10101"  (lambda () (format #f "~10b" 21)))
(test "format ~b" "    -10101"  (lambda () (format #f "~10b" -21)))
(test "format ~b" "    +10101"  (lambda () (format #f "~10@b" 21)))
(test "format ~b" "    -10101"  (lambda () (format #f "~10@b" -21)))
(test "format ~b" "0000010101"  (lambda () (format #f "~10,'0b" 21)))
(test "format ~b" "0000-10101"  (lambda () (format #f "~10,'0b" -21)))

(test "format ~b" "101"         (lambda () (format #f "~,,' ,4:b" 5)))
(test "format ~b" "101 0101"    (lambda () (format #f "~,,' ,4:b" 85)))

(test "format ~o" "12345"       (lambda () (format #f "~o" 5349)))
(test "format ~o" "-12345"      (lambda () (format #f "~o" -5349)))
(test "format ~o" "+12345"      (lambda () (format #f "~@o" 5349)))
(test "format ~o" "-12345"      (lambda () (format #f "~@o" -5349)))
(test "format ~o" "     12345"  (lambda () (format #f "~10o" 5349)))
(test "format ~o" "    -12345"  (lambda () (format #f "~10o" -5349)))
(test "format ~o" "    +12345"  (lambda () (format #f "~10@o" 5349)))
(test "format ~o" "    -12345"  (lambda () (format #f "~10@o" -5349)))
(test "format ~o" "0000012345"  (lambda () (format #f "~10,'0o" 5349)))
(test "format ~o" "0000-12345"  (lambda () (format #f "~10,'0o" -5349)))

(test "format ~x" "12345"       (lambda () (format #f "~x" 74565)))
(test "format ~x" "-12345"      (lambda () (format #f "~x" -74565)))
(test "format ~x" "+12345"      (lambda () (format #f "~@x" 74565)))
(test "format ~x" "-12345"      (lambda () (format #f "~@x" -74565)))
(test "format ~x" "     12345"  (lambda () (format #f "~10x" 74565)))
(test "format ~x" "    -12345"  (lambda () (format #f "~10x" -74565)))
(test "format ~x" "    +12345"  (lambda () (format #f "~10@x" 74565)))
(test "format ~x" "    -12345"  (lambda () (format #f "~10@x" -74565)))
(test "format ~x" "0000012345"  (lambda () (format #f "~10,'0x" 74565)))
(test "format ~x" "0000-12345"  (lambda () (format #f "~10,'0x" -74565)))

(test "format v param" "     12345"
      (lambda () (format #f "~vd" 10 12345)))
(test "format v param" "0000012345"
      (lambda () (format #f "~v,vd" 10 #\0 12345)))

(test-end)

