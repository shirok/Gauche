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

;; test for reader's overflow detection code
(test "peculiarity around 2^32"
      (* 477226729 10) (lambda () 4772267290))

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

(define (m-result x) (list x (- x) (- x) x))
(define (m-tester x y)
  (list (* x y) (* (- x) y) (* x (- y)) (* (- x) (- y))))

(test "fix*fix->big[1]" (m-result 727836879)
      (lambda () (m-tester 41943 17353)))
(test "fix*fix->big[1]" (m-result 3663846879)
      (lambda () (m-tester 41943 87353)))
(test "fix*fix->big[2]" (m-result 4294967296)
      (lambda () (m-tester 65536 65536)))
(test "fix*fix->big[2]" (m-result 366384949959)
      (lambda () (m-tester 4194303 87353)))
(test "fix*big[1]->big[1]" (m-result 3378812463)
      (lambda () (m-tester 3 1126270821)))
(test "fix*big[1]->big[2]" (m-result 368276265762816)
      (lambda () (m-tester 85746 4294967296)))
(test "big[1]*fix->big[1]" (m-result 3378812463)
      (lambda () (m-tester 1126270821 3)))
(test "big[1]*fix->big[2]" (m-result 368276265762816)
      (lambda () (m-tester 4294967296 85746)))
(test "big[2]*fix->big[2]" (m-result 12312849128741)
      (lambda () (m-tester 535341266467 23)))
(test "big[1]*big[1]->big[2]" (m-result 1345585795375391817)
      (lambda () (m-tester 1194726677 1126270821)))

;;------------------------------------------------------------------
(test-section "division")

(define (almost=? x y)
  (define (flonum=? x y)
    (let ((ax (abs x)) (ay (abs y)))
      (< (abs (- x y)) (* (max ax ay) 0.0000000000001))))
  (and (flonum=? (car x) (car y))
       (flonum=? (cadr x) (cadr y))
       (flonum=? (caddr x) (caddr y))
       (flonum=? (cadddr x) (cadddr y))
       (eq? (list-ref x 4) (list-ref y 4))))

(define (d-result x exact?) (list x (- x) (- x) x exact?))
(define (d-tester x y)
  (list (/ x y) (/ (- x) y) (/ x (- y)) (/ (- x) (- y))
        (exact? (/ x y))))

;; these uses BignumDivSI -> bignum_sdiv
(test "big[1]/fix->fix" (d-result 17353 #t) 
      (lambda () (d-tester 727836879 41943)))
(test "big[1]/fix->fix" (d-result 136582.040690235 #f)
      (lambda () (d-tester 3735928559 27353))
      almost=?)
(test "big[2]/fix->big[1]" (d-result 535341266467 #t)
      (lambda () (d-tester 12312849128741 23)))
(test "big[2]/fix->big[2]" (d-result 12312849128741 #t)
      (lambda () (d-tester 12312849128741 1)))

;; these uses BignumDivSI -> bignum_gdiv
(test "big[1]/fix->fix" (d-result 41943 #t)
      (lambda () (d-tester 3663846879 87353)))
(test "big[2]/fix->fix" (d-result 19088743.0196145 #f)
      (lambda () (d-tester 705986470884353 36984440))
      almost=?)
(test "big[2]/fix->fix" (d-result 92894912.9263878 #f)
      (lambda () (d-tester 12312849128741 132546))
      almost=?)
(test "big[2]/fix->big[1]" (d-result 2582762030.11968 #f)
      (lambda () (d-tester 425897458766735 164900))
      almost=?)

;; inexact division
(test "exact/inexact -> inexact" (d-result 3.25 #f)
      (lambda () (d-tester 13 4.0)))
(test "inexact/exact -> inexact" (d-result 3.25 #f)
      (lambda () (d-tester 13.0 4)))
(test "inexact/inexact -> inexact" (d-result 3.25 #f)
      (lambda () (d-tester 13.0 4.0)))

;;------------------------------------------------------------------
(test-section "quotient")

(define (q-result x exact?) (list x (- x) (- x) x exact?))
(define (q-tester x y)
  (list (quotient x y) (quotient (- x) y)
        (quotient x (- y)) (quotient (- x) (- y))
        (exact? (quotient x y))))

;; these uses BignumDivSI -> bignum_sdiv
(test "big[1]/fix->fix" (q-result 17353 #t) 
      (lambda () (q-tester 727836879 41943)))
(test "big[1]/fix->fix" (q-result 136582 #t)
      (lambda () (q-tester 3735928559 27353)))
(test "big[2]/fix->big[1]" (q-result 535341266467 #t)
      (lambda () (q-tester 12312849128741 23)))
(test "big[2]/fix->big[2]" (q-result 12312849128741 #t)
      (lambda () (q-tester 12312849128741 1)))

;; these uses BignumDivSI -> bignum_gdiv
(test "big[1]/fix->fix" (q-result 41943 #t)
      (lambda () (q-tester 3663846879 87353)))
(test "big[2]/fix->fix" (q-result 19088743 #t)
      (lambda () (q-tester 705986470884353 36984440)))
(test "big[2]/fix->fix" (q-result 92894912 #t)
      (lambda () (q-tester 12312849128741 132546)))
(test "big[2]/fix->big[1]" (q-result 2582762030 #t)
      (lambda () (q-tester 425897458766735 164900)))

;; these uses BignumDivRem
(test "big[1]/big[1]->fix" (q-result 2 #t)
      (lambda () (q-tester 4020957098 1952679221)))
(test "big[1]/big[1] -> fix" (q-result 0 #t)
      (lambda () (q-tester 1952679221 4020957098)))
;; this tests loop in estimation phase
(test "big[3]/big[2] -> big[1]" (q-result #xffff0001 #t)
      (lambda () (q-tester #x10000000000000000 #x10000ffff)))
;; this test goes through a rare case handling code ("add back") in
;; the algorithm.
(test "big[3]/big[2] -> fix" (q-result #xeffe #t)
      (lambda () (q-tester #x7800000000000000 #x80008889ffff)))

;; inexact quotient
(test "exact/inexact -> inexact" (q-result 3.0 #f)
      (lambda () (q-tester 13 4.0)))
(test "inexact/exact -> inexact" (q-result 3.0 #f)
      (lambda () (q-tester 13.0 4)))
(test "inexact/inexact -> inexact" (q-result 3.0 #f)
      (lambda () (q-tester 13.0 4.0)))
(test "exact/inexact -> inexact" (q-result 17353.0 #f)
      (lambda () (q-tester 727836879 41943.0)))
(test "inexact/exact -> inexact" (q-result 17353.0 #f)
      (lambda () (q-tester 727836879.0 41943)))
(test "inexact/inexact -> inexact" (q-result 17353.0 #f)
      (lambda () (q-tester 727836879.0 41943.0)))

;;------------------------------------------------------------------
(test-section "remainder")

(define (r-result x exact?) (list x (- x) x (- x) exact?))
(define (r-tester x y)
  (list (remainder x y) (remainder (- x) y)
        (remainder x (- y)) (remainder (- x) (- y))
        (exact? (remainder x y))))

;; small int
(test "fix rem fix -> fix" (r-result 1 #t)
      (lambda () (r-tester 13 4)))
(test "fix rem fix -> fix" (r-result 1234 #t)
      (lambda () (r-tester 1234 87935)))
(test "fix rem big[1] -> fix" (r-result 12345 #t)
      (lambda () (r-tester 12345 3735928559)))

;; these uses BignumDivSI -> bignum_sdiv
(test "big[1] rem fix -> fix" (r-result 0 #t)
      (lambda () (r-tester 727836879 41943)))
(test "big[1] rem fix -> fix" (r-result 1113 #t)
      (lambda () (r-tester 3735928559 27353)))
(test "big[2] rem fix -> fix" (r-result 15 #t)
      (lambda () (r-tester 12312849128756 23)))
(test "big[2] rem fix -> fix" (r-result 0 #t)
      (lambda () (r-tester 12312849128756 1)))

;; these uses BignumDivSI -> bignum_gdiv
(test "big[1] rem fix -> fix" (r-result 0 #t)
      (lambda () (r-tester 3663846879 87353)))
(test "big[2] rem fix -> fix" (r-result 725433 #t)
      (lambda () (r-tester 705986470884353 36984440)))
(test "big[2] rem fix -> fix" (r-result 122789 #t)
      (lambda () (r-tester 12312849128741 132546)))
(test "big[2] rem fix -> fix" (r-result 19735 #t)
      (lambda () (r-tester 425897458766735 164900)))

;; these uses BignumDivRem
(test "big[1] rem big[1] -> fix" (r-result 115598656 #t)
      (lambda () (r-tester 4020957098 1952679221)))
(test "big[1] rem big[1] -> fix" (r-result 1952679221 #t)
      (lambda () (r-tester 1952679221 4020957098)))
;; this tests loop in estimation phase
(test "big[3] rem big[2] -> big[1]" (r-result #xfffe0001 #t)
      (lambda () (r-tester #x10000000000000000 #x10000ffff)))
;; this tests "add back" code
(test "big[3] rem big[2] -> big[2]" (r-result #x7fffb114effe #t)
      (lambda () (r-tester #x7800000000000000 #x80008889ffff)))

;; inexact remainder
(test "exact rem inexact -> inexact" (r-result 1.0 #f)
      (lambda () (r-tester 13 4.0)))
(test "inexact rem exact -> inexact" (r-result 1.0 #f)
      (lambda () (r-tester 13.0 4)))
(test "inexact rem inexact -> inexact" (r-result 1.0 #f)
      (lambda () (r-tester 13.0 4.0)))
(test "exact rem inexact -> inexact" (r-result 1113.0 #f)
      (lambda () (r-tester 3735928559 27353.0)))
(test "inexact rem exact -> inexact" (r-result 1113.0 #f)
      (lambda () (r-tester 3735928559.0 27353)))
(test "inexact rem inexact -> inexact" (r-result 1113.0 #f)
      (lambda () (r-tester 3735928559.0 27353.0)))

;;------------------------------------------------------------------
(test-section "modulo")

(define (m-result a b exact?) (list a b (- b) (- a) exact?))
(define (m-tester x y)
  (list (modulo x y) (modulo (- x) y)
        (modulo x (- y)) (modulo (- x) (- y))
        (exact? (modulo x y))))

;; small int
(test "fix mod fix -> fix" (m-result 1 3 #t)
      (lambda () (m-tester 13 4)))
(test "fix mod fix -> fix" (m-result 1234 86701 #t)
      (lambda () (m-tester 1234 87935)))
(test "fix mod big[1] -> fix/big" (m-result 12345 3735916214 #t)
      (lambda () (m-tester 12345 3735928559)))

;; these uses BignumDivSI -> bignum_sdiv
(test "big[1] mod fix -> fix" (m-result 0 0 #t)
      (lambda () (m-tester 727836879 41943)))
(test "big[1] mod fix -> fix" (m-result 1113 26240 #t)
      (lambda () (m-tester 3735928559 27353)))
(test "big[2] mod fix -> fix" (m-result 15 8 #t)
      (lambda () (m-tester 12312849128756 23)))
(test "big[2] mod fix -> fix" (m-result 0 0 #t)
      (lambda () (m-tester 12312849128756 1)))

;; these uses BignumDivSI -> bignum_gdiv
(test "big[1] mod fix -> fix" (m-result 0 0 #t)
      (lambda () (m-tester 3663846879 87353)))
(test "big[2] mod fix -> fix" (m-result 725433 36259007 #t)
      (lambda () (m-tester 705986470884353 36984440)))
(test "big[2] mod fix -> fix" (m-result 122789 9757 #t)
      (lambda () (m-tester 12312849128741 132546)))
(test "big[2] mod fix -> fix" (m-result 19735 145165 #t)
      (lambda () (m-tester 425897458766735 164900)))

;; these uses BignumDivRem
(test "big[1] mod big[1] -> fix" (m-result 115598656 1837080565 #t)
      (lambda () (m-tester 4020957098 1952679221)))
(test "big[1] mod big[1] -> fix" (m-result 1952679221 2068277877 #t)
      (lambda () (m-tester 1952679221 4020957098)))
;; this tests loop in estimation phase
(test "big[3] mod big[2] -> big[1]" (m-result #xfffe0001 #x2fffe #t)
      (lambda () (m-tester #x10000000000000000 #x10000ffff)))
;; this tests "add back" code
(test "big[3] mod big[2] -> big[2]" (m-result #x7fffb114effe #xd7751001 #t)
      (lambda () (m-tester #x7800000000000000 #x80008889ffff)))

;; inexact modulo
(test "exact mod inexact -> inexact" (m-result 1.0 3.0 #f)
      (lambda () (m-tester 13 4.0)))
(test "inexact mod exact -> inexact" (m-result 1.0 3.0 #f)
      (lambda () (m-tester 13.0 4)))
(test "inexact mod inexact -> inexact" (m-result 1.0 3.0 #f)
      (lambda () (m-tester 13.0 4.0)))
(test "exact mod inexact -> inexact" (m-result 1113.0 26240.0 #f)
      (lambda () (m-tester 3735928559 27353.0)))
(test "inexact mod exact -> inexact" (m-result 1113.0 26240.0 #f)
      (lambda () (m-tester 3735928559.0 27353)))
(test "inexact mod inexact -> inexact" (m-result 1113.0 26240.0 #f)
      (lambda () (m-tester 3735928559.0 27353.0)))

(test-end)
