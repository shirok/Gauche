;;
;; test numeric system implementation
;;

(use gauche.test)

(define *word-bits* 32)  ;; # of bits used for long 

(define *small-int-bits* (- *word-bits* 3))  ;; # of bits used to represent
                                             ;; small integer constant
                                             ;; (except sign bit)
(define *inum-bits* (- *word-bits* 13)) ;; # of bits used for immediate
                                        ;; integer constant in the compiled
                                        ;; code (except sign bit)

(define (exp2 pow)
  (do ((i 0 (+ i 1))
       (m 1 (+ m m)))
      ((>= i pow) m)))

(test-start "numbers")

;;==================================================================
;; Integers
;;

;;------------------------------------------------------------------
(test-section "integer addition & reader")

(define (i-tester x)
  (list x (+ x -1 x) (+ x x) (- x) (- (+ x -1 x)) (- 0 x x) (- 0 x x 1)))

(test "around 2^28"
      '(268435456 536870911 536870912
        -268435456 -536870911 -536870912 -536870913)
      (lambda () (i-tester (exp2 28))))
      
(test "around 2^31"
      '(2147483648 4294967295 4294967296
        -2147483648 -4294967295 -4294967296 -4294967297)
      (lambda () (i-tester (exp2 31))))

(test "around 2^60"
      '(1152921504606846976 2305843009213693951 2305843009213693952
        -1152921504606846976 -2305843009213693951 -2305843009213693952
        -2305843009213693953)
      (lambda () (i-tester (exp2 60))))

(test "around 2^63"
      '(9223372036854775808 18446744073709551615 18446744073709551616
        -9223372036854775808 -18446744073709551615 -18446744073709551616
        -18446744073709551617)
      (lambda () (i-tester (exp2 63))))

(test "around 2^127"
      '(170141183460469231731687303715884105728
        340282366920938463463374607431768211455
        340282366920938463463374607431768211456
        -170141183460469231731687303715884105728
        -340282366920938463463374607431768211455
        -340282366920938463463374607431768211456
        -340282366920938463463374607431768211457)
      (lambda () (i-tester (exp2 127))))

(define x #xffffffff00000000ffffffff00000000)
(define xx (- x))
(define y #x00000002000000000000000200000000)
(define yy (- y))
(define z #x00000000000000010000000000000001)
(test "bignum + bignum" #x100000001000000010000000100000000
      (lambda () (+ x y)))
(test "bignum + -bignum" #xfffffffd00000000fffffffd00000000
      (lambda () (+ x yy)))
(test "bignum - bignum" #xfffffffefffffffffffffffeffffffff
      (lambda () (- x z)))
(test "bignum - bignum" x
      (lambda () (- (+ x y) y)))
(test "-bignum + bignum" (- #xfffffffd00000000fffffffd00000000)
      (lambda () (+ xx y)))
(test "-bignum + -bignum" (- #x100000001000000010000000100000000)
      (lambda () (+ xx yy)))
(test "-bignum - bignum" (- #x100000001000000010000000100000000)
      (lambda () (- xx y)))
(test "-bignum - -bignum" (- #xfffffffd00000000fffffffd00000000)
      (lambda () (- xx yy)))
      
;;------------------------------------------------------------------
(test-section "integer writer syntax")

(define (i-tester2 x)
  (map number->string (i-tester x)))

(test "around 2^28"
      '("268435456" "536870911" "536870912"
        "-268435456" "-536870911" "-536870912" "-536870913")
      (lambda () (i-tester2 (exp2 28))))
      
(test "around 2^31"
      '("2147483648" "4294967295" "4294967296"
        "-2147483648" "-4294967295" "-4294967296" "-4294967297")
      (lambda () (i-tester2 (exp2 31))))

(test "around 2^60"
      '("1152921504606846976" "2305843009213693951" "2305843009213693952"
        "-1152921504606846976" "-2305843009213693951" "-2305843009213693952"
        "-2305843009213693953")
      (lambda () (i-tester2 (exp2 60))))

(test "around 2^63"
      '("9223372036854775808" "18446744073709551615" "18446744073709551616"
        "-9223372036854775808" "-18446744073709551615" "-18446744073709551616"
        "-18446744073709551617")
      (lambda () (i-tester2 (exp2 63))))

(test "around 2^127"
      '("170141183460469231731687303715884105728"
        "340282366920938463463374607431768211455"
        "340282366920938463463374607431768211456"
        "-170141183460469231731687303715884105728"
        "-340282366920938463463374607431768211455"
        "-340282366920938463463374607431768211456"
        "-340282366920938463463374607431768211457")
      (lambda () (i-tester2 (exp2 127))))


;;------------------------------------------------------------------
(test-section "small integer literals")

;; small literal integer x (-2^19 <= x < 2^19 on 32bit architecture)
;; in binary addition/subtraction is compiled in special instructuions,
;; NUMADDI and NUMSUBI.

(define x 2)
(test "NUMADDI" 5 (lambda () (+ 3 x)))
(test "NUMADDI" 5 (lambda () (+ x 3)))
(test "NUMADDI" 1 (lambda () (+ -1 x)))
(test "NUMADDI" 1 (lambda () (+ x -1)))
(test "NUMSUBI" 1 (lambda () (- 3 x)))
(test "NUMSUBI" -1 (lambda () (- x 3)))
(test "NUMSUBI" -5 (lambda () (- -3 x)))
(test "NUMSUBI" 5 (lambda () (- x -3)))
(define x 2.0)
(test "NUMADDI" 5.0 (lambda () (+ 3 x)))
(test "NUMADDI" 5.0 (lambda () (+ x 3)))
(test "NUMADDI" 1.0 (lambda () (+ -1 x)))
(test "NUMADDI" 1.0 (lambda () (+ x -1)))
(test "NUMSUBI" 1.0 (lambda () (- 3 x)))
(test "NUMSUBI" -1.0 (lambda () (- x 3)))
(test "NUMSUBI" -5.0 (lambda () (- -3 x)))
(test "NUMSUBI" 5.0 (lambda () (- x -3)))
(define x #x100000000)
(test "NUMADDI" #x100000003 (lambda () (+ 3 x)))
(test "NUMADDI" #x100000003 (lambda () (+ x 3)))
(test "NUMADDI" #xffffffff (lambda () (+ -1 x)))
(test "NUMADDI" #xffffffff (lambda () (+ x -1)))
(test "NUMSUBI" (- #xfffffffd) (lambda () (- 3 x)))
(test "NUMSUBI" #xfffffffd (lambda () (- x 3)))
(test "NUMSUBI" (- #x100000003) (lambda () (- -3 x)))
(test "NUMSUBI" #x100000003 (lambda () (- x -3)))

;;------------------------------------------------------------------
(test-section "integer multiplication")

(test "fix*fix->big[1]" 727836879  (lambda () (* 41943 17353)))
(test "fix*fix->big[1]" -727836879  (lambda () (* -41943 17353)))
(test "fix*fix->big[1]" -727836879  (lambda () (* 41943 -17353)))
(test "fix*fix->big[1]" 727836879  (lambda () (* -41943 -17353)))
(test "fix*fix->big[1]" 3663846879 (lambda () (* 41943 87353)))
(test "fix*fix->big[1]" -3663846879 (lambda () (* -41943 87353)))
(test "fix*fix->big[1]" -3663846879 (lambda () (* 41943 -87353)))
(test "fix*fix->big[1]" 3663846879 (lambda () (* -41943 -87353)))
(test "fix*fix->big[2]" 4294967296 (lambda () (* 65536 65536)))
(test "fix*fix->big[2]" -4294967296 (lambda () (* -65536 65536)))
(test "fix*fix->big[2]" -4294967296 (lambda () (* 65536 -65536)))
(test "fix*fix->big[2]" 4294967296 (lambda () (* 65536 65536)))
(test "fix*fix->big[2]" 366384949959 (lambda () (* 4194303 87353)))
(test "fix*fix->big[2]" -366384949959 (lambda () (* -4194303 87353)))
(test "fix*fix->big[2]" -366384949959 (lambda () (* 4194303 -87353)))
(test "fix*fix->big[2]" 366384949959 (lambda () (* -4194303 -87353)))
(test "fix*big[1]->big[1]" 3378812463 (lambda () (* 3 1126270821)))
(test "fix*big[1]->big[1]" -3378812463 (lambda () (* -3 1126270821)))
(test "fix*big[1]->big[1]" -3378812463 (lambda () (* 3 -1126270821)))
(test "fix*big[1]->big[1]" 3378812463 (lambda () (* -3 -1126270821)))
(test "fix*big[1]->big[2]" 368276265762816 (lambda () (* 85746 4294967296)))
(test "fix*big[1]->big[2]" -368276265762816 (lambda () (* -85746 4294967296)))
(test "fix*big[1]->big[2]" -368276265762816 (lambda () (* 85746 -4294967296)))
(test "fix*big[1]->big[2]" 368276265762816 (lambda () (* -85746 -4294967296)))
(test "big[1]*fix->big[1]" 3378812463 (lambda () (* 1126270821 3)))
(test "big[1]*fix->big[1]" -3378812463 (lambda () (* 1126270821 -3)))
(test "big[1]*fix->big[1]" -3378812463 (lambda () (* -1126270821 3)))
(test "big[1]*fix->big[1]" 3378812463 (lambda () (* -1126270821 -3)))
(test "big[1]*fix->big[2]" 368276265762816 (lambda () (* 4294967296 85746)))
(test "big[1]*fix->big[2]" -368276265762816 (lambda () (* 4294967296 -85746)))
(test "big[1]*fix->big[2]" -368276265762816 (lambda () (* -4294967296 85746)))
(test "big[1]*fix->big[2]" 368276265762816 (lambda () (* -4294967296 -85746)))

(test "big[1]*big[1]->big[2]" 1345585795375391817
      (lambda () (* 1194726677 1126270821)))
(test "big[1]*big[1]->big[2]" -1345585795375391817
      (lambda () (* -1194726677 1126270821)))
(test "big[1]*big[1]->big[2]" -1345585795375391817
      (lambda () (* 1194726677 -1126270821)))
(test "big[1]*big[1]->big[2]" 1345585795375391817
      (lambda () (* -1194726677 -1126270821)))

;;------------------------------------------------------------------
(test-section "integer division")

(test "big[1]/fix->fix" 17353 (lambda () (/ 727836879 41943)))
(test "big[1]/fix->fix" -17353 (lambda () (/ -727836879 41943)))
(test "big[1]/fix->fix" -17353 (lambda () (/ 727836879 -41943)))
(test "big[1]/fix->fix" 17353 (lambda () (/ -727836879 -41943)))
(test "big[1]/fix->fix" 41943 (lambda () (/ 3663846879 87353)))
(test "big[1]/fix->fix" -41943 (lambda () (/ -3663846879 87353)))
(test "big[1]/fix->fix" -41943 (lambda () (/ 3663846879 -87353)))
(test "big[1]/fix->fix" 41943 (lambda () (/ -3663846879 -87353)))

(test-end)
