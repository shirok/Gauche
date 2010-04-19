;;
;; test numeric system implementation
;;

(use gauche.test)

(define (exp2 pow)
  (do ((i 0 (+ i 1))
       (m 1 (+ m m)))
      ((>= i pow) m)))

(define (fermat n)                      ;Fermat's number
  (+ (expt 2 (expt 2 n)) 1))

(test-start "numbers")

;;==================================================================
;; Reader/writer
;;

;;------------------------------------------------------------------
(test-section "integer addition & reader")

(define (i-tester x)
  (list x (+ x -1 x) (+ x x) (- x) (- (+ x -1 x)) (- 0 x x) (- 0 x x 1)))

(test* "around 2^28"
       '(268435456 536870911 536870912
         -268435456 -536870911 -536870912 -536870913)
       (i-tester (exp2 28)))

(test* "around 2^31"
       '(2147483648 4294967295 4294967296
         -2147483648 -4294967295 -4294967296 -4294967297)
       (i-tester (exp2 31)))

(test* "around 2^60"
       '(1152921504606846976 2305843009213693951 2305843009213693952
         -1152921504606846976 -2305843009213693951 -2305843009213693952
         -2305843009213693953)
       (i-tester (exp2 60)))

(test* "around 2^63"
       '(9223372036854775808 18446744073709551615 18446744073709551616
         -9223372036854775808 -18446744073709551615 -18446744073709551616
         -18446744073709551617)
       (i-tester (exp2 63)))

(test* "around 2^127"
       '(170141183460469231731687303715884105728
         340282366920938463463374607431768211455
         340282366920938463463374607431768211456
         -170141183460469231731687303715884105728
         -340282366920938463463374607431768211455
         -340282366920938463463374607431768211456
         -340282366920938463463374607431768211457)
       (i-tester (exp2 127)))

;; test for reader's overflow detection code
(test* "peculiarity around 2^32"
      (* 477226729 10) 4772267290)

(test* "radix" '(43605 342391 718048024785
                 123456789 123456789987654321
                 1193046 3735928559 3735928559)
       (list #b1010101001010101
             #o1234567
             #o12345677654321
             #d123456789
             #d123456789987654321
             #x123456
             #xdeadbeef
             #xDeadBeef))

(test* "exactness" #t (exact? #e10))
(test* "exactness" #t (exact? #e10.0))
(test* "exactness" #t (exact? #e10e10))
(test* "exactness" #t (exact? #e12.34))
(test* "inexactness" #f (exact? #i10))
(test* "inexactness" #f (exact? #i10.0))
(test* "inexactness" #f (exact? #i12.34))

(test* "exactness & radix" '(#t 3735928559 #t 3735928559)
       (list (exact? #e#xdeadbeef)
             #e#xdeadbeef
             (exact? #x#edeadbeef)
             #x#edeadbeef))
(test* "inexactness & radix" '(#f 3735928559.0 #f 3735928559.0)
       (list (exact? #i#xdeadbeef)
             #i#xdeadbeef
             (exact? #x#ideadbeef)
             #x#ideadbeef))

(test* "invalid exactness/radix spec" #f
       (or (string->number "#e")
           (string->number "#i")
           (string->number "#e#i3")
           (string->number "#i#e5")
           (string->number "#x#o13")
           (string->number "#e#b#i00101")))

(define (radix-tester radix)
  (list
   (let loop ((digits 0)
              (input "1")
              (value 1))
     (cond ((> digits 64) #t)
           ((eqv? (string->number input radix) value)
            (loop (+ digits 1) (string-append input "0") (* value radix)))
           (else #f)))
   (let loop ((digits 0)
              (input (string (integer->digit (- radix 1) radix)))
              (value (- radix 1)))
     (cond ((> digits 64) #t)
           ((eqv? (string->number input radix) value)
            (loop (+ digits 1)
                  (string-append input (string (integer->digit (- radix 1) radix)))
                  (+ (* value radix) (- radix 1))))
           (else #f)))))

(test* "base-2 reader" '(#t #t) (radix-tester 2))
(test* "base-3 reader" '(#t #t) (radix-tester 3))
(test* "base-4 reader" '(#t #t) (radix-tester 4))
(test* "base-5 reader" '(#t #t) (radix-tester 5))
(test* "base-6 reader" '(#t #t) (radix-tester 6))
(test* "base-7 reader" '(#t #t) (radix-tester 7))
(test* "base-8 reader" '(#t #t) (radix-tester 8))
(test* "base-9 reader" '(#t #t) (radix-tester 9))
(test* "base-10 reader" '(#t #t) (radix-tester 10))
(test* "base-11 reader" '(#t #t) (radix-tester 11))
(test* "base-12 reader" '(#t #t) (radix-tester 12))
(test* "base-13 reader" '(#t #t) (radix-tester 13))
(test* "base-14 reader" '(#t #t) (radix-tester 14))
(test* "base-15 reader" '(#t #t) (radix-tester 15))
(test* "base-16 reader" '(#t #t) (radix-tester 16))
(test* "base-17 reader" '(#t #t) (radix-tester 17))
(test* "base-18 reader" '(#t #t) (radix-tester 18))
(test* "base-19 reader" '(#t #t) (radix-tester 19))
(test* "base-20 reader" '(#t #t) (radix-tester 20))
(test* "base-21 reader" '(#t #t) (radix-tester 21))
(test* "base-22 reader" '(#t #t) (radix-tester 22))
(test* "base-23 reader" '(#t #t) (radix-tester 23))
(test* "base-24 reader" '(#t #t) (radix-tester 24))
(test* "base-25 reader" '(#t #t) (radix-tester 25))
(test* "base-26 reader" '(#t #t) (radix-tester 26))
(test* "base-27 reader" '(#t #t) (radix-tester 27))
(test* "base-28 reader" '(#t #t) (radix-tester 28))
(test* "base-29 reader" '(#t #t) (radix-tester 29))
(test* "base-30 reader" '(#t #t) (radix-tester 30))
(test* "base-31 reader" '(#t #t) (radix-tester 31))
(test* "base-32 reader" '(#t #t) (radix-tester 32))
(test* "base-33 reader" '(#t #t) (radix-tester 33))
(test* "base-34 reader" '(#t #t) (radix-tester 34))
(test* "base-35 reader" '(#t #t) (radix-tester 35))
(test* "base-36 reader" '(#t #t) (radix-tester 36))

;;------------------------------------------------------------------
(test-section "rational reader")

(define (rational-test v)
  (if (number? v) (list v (exact? v)) v))

(test* "rational reader" '(1234 #t) (rational-test '1234/1))
(test* "rational reader" '(-1234 #t) (rational-test '-1234/1))
(test* "rational reader" '(1234 #t) (rational-test '+1234/1))
(test* "rational reader" '|1234/-1| (rational-test '1234/-1))
(test* "rational reader" '(1234 #t) (rational-test '2468/2))
(test* "rational reader" '(1/2 #t) (rational-test '1/2))
(test* "rational reader" '(-1/2 #t) (rational-test '-1/2))
(test* "rational reader" '(1/2 #t) (rational-test '+1/2))
(test* "rational reader" '(1/2 #t) (rational-test '751/1502))

(test* "rational reader" '(1 #t)
       (rational-test (string->number "3/03")))
(test* "rational reader" '(+inf.0 #f)
       (rational-test (string->number "3/0")))
(test* "rational reader" '(-inf.0 #f)
       (rational-test (string->number "-3/0")))
(test* "rational reader" #f
       (rational-test (string->number "3/3/4")))
(test* "rational reader" #f
       (rational-test (string->number "1/2.")))
(test* "rational reader" #f
       (rational-test (string->number "1.3/2")))

(test* "rational reader" (test-error)
       (rational-test (read-from-string "#e3/0")))
(test* "rational reader" (test-error)
       (rational-test (read-from-string "#e-3/0")))

(test* "rational reader w/#e" '(1234 #t)
       (rational-test '#e1234/1))
(test* "rational reader w/#e" '(-1234 #t)
       (rational-test '#e-1234/1))
(test* "rational reader w/#e" '(32/7 #t)
       (rational-test '#e32/7))
(test* "rational reader w/#e" '(-32/7 #t)
       (rational-test '#e-32/7))
(test* "rational reader w/#i" '(1234.0 #f)
       (rational-test '#i1234/1))
(test* "rational reader w/#i" '(-1234.0 #f)
       (rational-test '#i-1234/1))
(test* "rational reader w/#i" '(-0.125 #f)
       (rational-test '#i-4/32))

(test* "rational reader w/radix" '(15 #t)
       (rational-test '#e#xff/11))
(test* "rational reader w/radix" '(56 #t)
       (rational-test '#o770/11))
(test* "rational reader w/radix" '(15.0 #f)
       (rational-test '#x#iff/11))

(test* "rational reader edge case" #t (symbol? (read-from-string "/1")))
(test* "rational reader edge case" #t (symbol? (read-from-string "-/1")))
(test* "rational reader edge case" #t (symbol? (read-from-string "+/1")))

;;------------------------------------------------------------------
(test-section "flonum reader")

(define (flonum-test v)
  (if (number? v) (list v (inexact? v)) v))

(test* "flonum reader" '(3.14 #t)  (flonum-test 3.14))
(test* "flonum reader" '(0.14 #t)  (flonum-test 0.14))
(test* "flonum reader" '(0.14 #t)  (flonum-test .14))
(test* "flonum reader" '(3.0  #t)  (flonum-test 3.))
(test* "flonum reader" '(-3.14 #t)  (flonum-test -3.14))
(test* "flonum reader" '(-0.14 #t)  (flonum-test -0.14))
(test* "flonum reader" '(-0.14 #t)  (flonum-test -.14))
(test* "flonum reader" '(-3.0  #t)  (flonum-test -3.))
(test* "flonum reader" '(3.14 #t)  (flonum-test +3.14))
(test* "flonum reader" '(0.14 #t)  (flonum-test +0.14))
(test* "flonum reader" '(0.14 #t)  (flonum-test +.14))
(test* "flonum reader" '(3.0  #t)  (flonum-test +3.))
(test* "flonum reader" '(0.0  #t)  (flonum-test .0))
(test* "flonum reader" '(0.0  #t)  (flonum-test 0.))
(test* "flonum reader" #f (string->number "."))
(test* "flonum reader" #f (string->number "-."))
(test* "flonum reader" #f (string->number "+."))

(test* "flonum reader (exp)" '(314.0 #t) (flonum-test 3.14e2))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test .314e3))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test 314e0))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test 314e-0))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test 3140000e-4))
(test* "flonum reader (exp)" '(-314.0 #t) (flonum-test -3.14e2))
(test* "flonum reader (exp)" '(-314.0 #t) (flonum-test -.314e3))
(test* "flonum reader (exp)" '(-314.0 #t) (flonum-test -314e0))
(test* "flonum reader (exp)" '(-314.0 #t) (flonum-test -314.e-0))
(test* "flonum reader (exp)" '(-314.0 #t) (flonum-test -3140000e-4))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test +3.14e2))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test +.314e3))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test +314.e0))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test +314e-0))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test +3140000.000e-4))

(test* "flonum reader (exp)" '(314.0 #t) (flonum-test .314E3))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test .314s3))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test .314S3))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test .314l3))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test .314L3))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test .314f3))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test .314F3))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test .314d3))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test .314D3))

(test* "flonum reader (minimum denormalized number 5.0e-324)" #t
       (let1 x (expt 2.0 -1074)
         (= x (string->number (number->string x)))))
(test* "flonum reader (minimum denormalized number -5.0e-324)" #t
       (let1 x (- (expt 2.0 -1074))
         (= x (string->number (number->string x)))))
       

(test* "padding" '(10.0 #t) (flonum-test '1#))
(test* "padding" '(10.0 #t) (flonum-test '1#.))
(test* "padding" '(10.0 #t) (flonum-test '1#.#))
(test* "padding" '(100.0 #t) (flonum-test '10#.#))
(test* "padding" '(100.0 #t) (flonum-test '1##.#))
(test* "padding" '(100.0 #t) (flonum-test '100.0#))
(test* "padding" '(1.0 #t) (flonum-test '1.#))

(test* "padding" '|1#1| (flonum-test '1#1))
(test* "padding" '|1##1| (flonum-test '1##1))
(test* "padding" '|1#.1| (flonum-test '1#.1))
(test* "padding" '|1.#1| (flonum-test '1.#1))

(test* "padding" '|.#| (flonum-test '.#))
(test* "padding" '(0.0 #t) (flonum-test '0.#))
(test* "padding" '(0.0 #t) (flonum-test '.0#))
(test* "padding" '(0.0 #t) (flonum-test '0#))
(test* "padding" '(0.0 #t) (flonum-test '0#.#))
(test* "padding" '|0#.0| (flonum-test '0#.0))

(test* "padding" '(1000.0 #t) (flonum-test '1#e2))
(test* "padding" '(1000.0 #t) (flonum-test '1##e1))
(test* "padding" '(1000.0 #t) (flonum-test '1#.##e2))
(test* "padding" '(0.0 #t) (flonum-test '0.#e2))
(test* "padding" '(0.0 #t) (flonum-test '.0#e2))
(test* "padding" '|.##e2| (flonum-test '.##e2))

(test* "padding (exactness)" '(100 #f) (flonum-test '#e1##))
(test* "padding (exactness)" '(120 #f) (flonum-test '#e12#))
(test* "padding (exactness)" '(120 #f) (flonum-test '#e12#.#))
(test* "padding (exactness)" '(100.0 #t) (flonum-test '#i1##))
(test* "padding (exactness)" '(120.0 #t) (flonum-test '#i12#))
(test* "padding (exactness)" '(120.0 #t) (flonum-test '#i12#.#))

(test* "exponent out-of-range 1" '(+inf.0 #t) (flonum-test '1e309))
(test* "exponent out-of-range 2" '(+inf.0 #t) (flonum-test '1e10000))
(test* "exponent out-of-range 3" '(+inf.0 #t) (flonum-test '1e1000000000000000000000000000000000000000000000000000000000000000))
(test* "exponent out-of-range 4" '(-inf.0 #t) (flonum-test '-1e309))
(test* "exponent out-of-range 5" '(-inf.0 #t) (flonum-test '-1e10000))
(test* "exponent out-of-range 6" '(-inf.0 #t) (flonum-test '-1e1000000000000000000000000000000000000000000000000000000000000000))
(test* "exponent out-of-range 7" '(0.0 #t) (flonum-test '1e-324))
(test* "exponent out-of-range 8" '(0.0 #t) (flonum-test '1e-1000))
(test* "exponent out-of-range 9" '(0.0 #t) (flonum-test '1e-1000000000000000000000000000000000000000000000000000000000000000000))

(test* "no integral part" 0.5 (read-from-string ".5"))
(test* "no integral part" -0.5 (read-from-string "-.5"))
(test* "no integral part" 0.5 (read-from-string "+.5"))

;;------------------------------------------------------------------
(test-section "exact fractional number")

(test* "exact fractonal number" 12345
       (string->number "#e1.2345e4"))
(test* "exact fractonal number" 123450000000000
       (string->number "#e1.2345e14"))
(test* "exact fractonal number" 12345/100
       (string->number "#e1.2345e2"))
(test* "exact fractonal number" 12345/1000000
       (string->number "#e1.2345e-2"))
(test* "exact fractonal number" -12345
       (string->number "#e-1.2345e4"))
(test* "exact fractonal number" -123450000000000
       (string->number "#e-1.2345e14"))
(test* "exact fractonal number" -12345/100
       (string->number "#e-1.2345e2"))
(test* "exact fractonal number" -12345/1000000
       (string->number "#e-1.2345e-2"))

(test* "exact fractonal number" (%expt 10 296)
       (string->number "#e0.0001e300"))
(test* "exact fractonal number" (- (%expt 10 296))
       (string->number "#e-0.0001e300"))

(test* "exact fractonal number" (test-error)
       (read-from-string "#e1e330"))
(test* "exact fractonal number" (test-error)
       (read-from-string "#e1e-330"))


;;------------------------------------------------------------------
(test-section "complex reader")

(define (decompose-complex z)
  (cond ((real? z) z)
        ((complex? z)
         (list (real-part z) (imag-part z)))
        (else z)))

(test* "complex reader" '(1.0 1.0) (decompose-complex '1+i))
(test* "complex reader" '(1.0 1.0) (decompose-complex '1+1i))
(test* "complex reader" '(1.0 -1.0) (decompose-complex '1-i))
(test* "complex reader" '(1.0 -1.0) (decompose-complex '1-1i))
(test* "complex reader" '(1.0 1.0) (decompose-complex '1.0+1i))
(test* "complex reader" '(1.0 1.0) (decompose-complex '1.0+1.0i))
(test* "complex reader" '(1e-5 1.0) (decompose-complex '1e-5+1i))
(test* "complex reader" '(1e+5 1.0) (decompose-complex '1e+5+1i))
(test* "complex reader" '(1.0 1e-5) (decompose-complex '1+1e-5i))
(test* "complex reader" '(1.0 1e+5) (decompose-complex '1+1e+5i))
(test* "complex reader" '(0.1 1e+4) (decompose-complex '0.1+0.1e+5i))
(test* "complex reader" '(0.0 1.0) (decompose-complex '+i))
(test* "complex reader" '(0.0 -1.0) (decompose-complex '-i))
(test* "complex reader" '(0.0 1.0) (decompose-complex '+1i))
(test* "complex reader" '(0.0 -1.0) (decompose-complex '-1i))
(test* "complex reader" '(0.0 1.0) (decompose-complex '+1.i))
(test* "complex reader" '(0.0 -1.0) (decompose-complex '-1.i))
(test* "complex reader" '(0.0 1.0) (decompose-complex '+1.0i))
(test* "complex reader" '(0.0 -1.0) (decompose-complex '-1.0i))
(test* "complex reader" 1.0 (decompose-complex '1+0.0i))
(test* "complex reader" 1.0 (decompose-complex '1+.0i))
(test* "complex reader" 1.0 (decompose-complex '1+0.i))
(test* "complex reader" 1.0 (decompose-complex '1+0.0e-43i))
(test* "complex reader" 100.0 (decompose-complex '1e2+0.0e-43i))

(test* "complex reader" 'i (decompose-complex 'i))
(test* "complex reader" #f (decompose-complex (string->number ".i")))
(test* "complex reader" #f (decompose-complex (string->number "+.i")))
(test* "complex reader" #f (decompose-complex (string->number "-.i")))
(test* "complex reader" '33i (decompose-complex '33i))
(test* "complex reader" 'i+1 (decompose-complex 'i+1))
(test* "complex reader" '|++i| (decompose-complex '++i))
(test* "complex reader" '|--i| (decompose-complex '--i))

(test* "complex reader" '(0.5 0.5) (decompose-complex 1/2+1/2i))
(test* "complex reader" '(0.0 0.5) (decompose-complex 0+1/2i))
(test* "complex reader" '(0.0 -0.5) (decompose-complex -1/2i))
(test* "complex reader" 1/2 (decompose-complex 1/2-0/2i))
(test* "complex reader" '(0.5 -inf.0) (decompose-complex (string->number "1/2-1/0i")))

(test* "complex reader (polar)" (make-polar 1.0 1.0) 1.0@1.0)
(test* "complex reader (polar)" (make-polar 1.0 -1.0) 1.0@-1.0)
(test* "complex reader (polar)" (make-polar 1.0 1.0) 1.0@+1.0)
(test* "complex reader (polar)" (make-polar -7.0 -3.0) -7@-3.0)
(test* "complex reader (polar)" (make-polar 3.5 -3.0) 7/2@-3.0)
(test* "complex reader (polar)" #f (string->number "7/2@-3.14i"))

;;------------------------------------------------------------------
(test-section "integer writer syntax")

(define (i-tester2 x)
  (map number->string (i-tester x)))

(test* "around 2^28"
      '("268435456" "536870911" "536870912"
        "-268435456" "-536870911" "-536870912" "-536870913")
      (i-tester2 (exp2 28)))
      
(test* "around 2^31"
      '("2147483648" "4294967295" "4294967296"
        "-2147483648" "-4294967295" "-4294967296" "-4294967297")
      (i-tester2 (exp2 31)))

(test* "around 2^60"
      '("1152921504606846976" "2305843009213693951" "2305843009213693952"
        "-1152921504606846976" "-2305843009213693951" "-2305843009213693952"
        "-2305843009213693953")
      (i-tester2 (exp2 60)))

(test* "around 2^63"
      '("9223372036854775808" "18446744073709551615" "18446744073709551616"
        "-9223372036854775808" "-18446744073709551615" "-18446744073709551616"
        "-18446744073709551617")
      (i-tester2 (exp2 63)))

(test* "around 2^127"
      '("170141183460469231731687303715884105728"
        "340282366920938463463374607431768211455"
        "340282366920938463463374607431768211456"
        "-170141183460469231731687303715884105728"
        "-340282366920938463463374607431768211455"
        "-340282366920938463463374607431768211456"
        "-340282366920938463463374607431768211457")
      (i-tester2 (exp2 127)))

;;==================================================================
;; Conversions
;;

;; We first test expt, for we need to use it to test exact<->inexact
;; conversion stuff.
(test-section "expt")

(test* "exact expt" 1 (expt 5 0))
(test* "exact expt" 9765625 (expt 5 10))
(test* "exact expt" 1220703125 (expt 5 13))
(test* "exact expt" 94039548065783000637498922977779654225493244541767001720700136502273380756378173828125 (expt 5 123))
(test* "exact expt" 1/94039548065783000637498922977779654225493244541767001720700136502273380756378173828125 (expt 5 -123))
(test* "exact expt" 1 (expt -5 0))
(test* "exact expt" 9765625 (expt -5 10))
(test* "exact expt" -1220703125 (expt -5 13))
(test* "exact expt" -94039548065783000637498922977779654225493244541767001720700136502273380756378173828125 (expt -5 123))
(test* "exact expt" -1/94039548065783000637498922977779654225493244541767001720700136502273380756378173828125 (expt -5 -123))
(test* "exact expt" 1 (expt 1 720000))
(test* "exact expt" 1 (expt -1 720000))
(test* "exact expt" -1 (expt -1 720001))

(test* "exact expt (ratinoal)" 8589934592/5559060566555523
       (expt 2/3 33))
(test* "exact expt (rational)" -8589934592/5559060566555523
       (expt -2/3 33))
(test* "exact expt (ratinoal)" 5559060566555523/8589934592
       (expt 2/3 -33))

(test* "expt (coercion to inexact)" 1.4142135623730951
       (expt 2 1/2)
       (lambda (x y) (nearly=? 10e7 x y))) ;; NB: pa$ will be tested later

(test-section "exact<->inexact")

(for-each
 (lambda (e&i)
   (let ((e (car e&i))
         (i (cdr e&i)))
     (test* (format "exact->inexact ~s" i) i (exact->inexact e))
     (test* (format "exact->inexact ~s" (- i)) (- i) (exact->inexact (- e)))
     (test* (format "inexact->exact ~s" e) e (inexact->exact i))
     (test* (format "inexact->exact ~s" (- e)) (- e) (inexact->exact (- i)))
     ))
 `((0  . 0.0)
   (1  . 1.0)
   (-1 . -1.0)
   (,(%expt 2 52) . ,(%expt 2.0 52))
   (,(%expt 2 53) . ,(%expt 2.0 53))
   (,(%expt 2 54) . ,(%expt 2.0 54))
   ))

;; Rounding bignum to flonum, edge cases.
;; Test patterns:
;;
;;   <------53bits------->
;;a) 100000000...000000000100000....0000       round down (r0)
;;b) 100000000...000000000100000....0001       round up (r1)
;;c) 100000000...000000001100000....0000       round up (r2)
;;d) 100000000...000000001011111....1111       round down (r1)
;;e) 111111111...111111111100000....0000       round up, carry over (* r0 2)
;;f) 101111111...111111111100000....0000       round up, no carry over (r3)
;;            <--32bits-->
;;g) 100..0000111.....1111100000....0000       round up; boundary on ILP32 (r4)

(let loop ((n 0)
           (a (+ (expt 2 53) 1))
           (c (+ (expt 2 53) 3))
           (e (- (expt 2 54) 1))
           (f (+ (expt 2 53) (expt 2 52) -1))
           (g (+ (expt 2 53) (expt 2 33) -1))
           (r0 (expt 2.0 53))
           (r1 (+ (expt 2.0 53) 2.0))
           (r2 (+ (expt 2.0 53) 4.0))
           (r3 (+ (expt 2.0 53) (expt 2.0 52)))
           (r4 (+ (expt 2.0 53) (expt 2.0 33))))
  (when (< n 32)
    (test* (format "exact->inexact, pattern a: round down (~d)" n)
           r0 (exact->inexact a))
    (test* (format "exact->inexact, pattern b: round up   (~d)" n)
           r1 (exact->inexact (+ a 1)))
    (test* (format "exact->inexact, pattern c: round up   (~d)" n)
           r2 (exact->inexact c))
    (test* (format "exact->inexact, pattern d: round down (~d)" n)
           r1 (exact->inexact (- c 1)))
    (test* (format "exact->inexact, pattern e: round up   (~d)" n)
           (* r0 2.0) (exact->inexact e))
    (test* (format "exact->inexact, pattern f: round up   (~d)" n)
           r3 (exact->inexact f))
    (test* (format "exact->inexact, pattern g: round up   (~d)" n)
           r4 (exact->inexact g))
    (loop (+ n 1) (ash a 1) (ash c 1) (ash e 1) (ash f 1) (ash g 1)
          (* r0 2.0) (* r1 2.0) (* r2 2.0) (* r3 2.0) (* r4 2.0))))

(test* "expt (ratnum with large denom and numer) with inexact conversion 1"
       (expt 8/9 342.0)
       (exact->inexact (expt 8/9 342))
       (lambda (x y) (nearly=? 10e12 x y)))

(test* "expt (ratnum with large denom and numer) with inexact conversion 2"
       (expt -8/9 343.0)
       (exact->inexact (expt -8/9 343))
       (lambda (x y) (nearly=? 10e12 x y)))

;; The following few tests covers RATNUM paths in Scm_GetDouble
(test* "expt (ratnum with large denom and numer) with inexact conversion 3"
       1.0e-308 (exact->inexact (/ (expt 10 20) (expt 10 328))))
(test* "expt (ratnum with large denom and numer) with inexact conversion 4"
       0.0 (exact->inexact (/ (expt 10 20) (expt 10 329))))
(test* "expt (ratnum with large denom and numer) with inexact conversion 5"
       1.0e308 (exact->inexact (/ (expt 10 328) (expt 10 20))))
(test* "expt (ratnum with large denom and numer) with inexact conversion 6"
       +inf.0 (exact->inexact (/ (expt 10 329) (expt 10 20))))
(test* "expt (ratnum with large denom and numer) with inexact conversion 7"
       -inf.0 (exact->inexact (/ (expt -10 329) (expt 10 20))))

;;==================================================================
;; Predicates
;;

(test-section "predicates")

(test* "integer?" #t (integer? 0))
(test* "integer?" #t (integer? 85736847562938475634534245))
(test* "integer?" #f (integer? 85736.534245))
(test* "integer?" #f (integer? 3.14))
(test* "integer?" #f (integer? 3+4i))
(test* "integer?" #t (integer? 3+0i))
(test* "integer?" #f (integer? #f))

(test* "rational?" #t (rational? 0))
(test* "rational?" #t (rational? 85736847562938475634534245))
(test* "rational?" #t (rational? 1/2))
(test* "rational?" #t (rational? 85736.534245))
(test* "rational?" #t (rational? 3.14))
(test* "rational?" #f (rational? 3+4i))
(test* "rational?" #t (rational? 3+0i))
(test* "rational?" #f (rational? #f))

(test* "real?" #t (real? 0))
(test* "real?" #t (real? 85736847562938475634534245))
(test* "real?" #t (real? 857368.4756293847))
(test* "real?" #t (real? 3+0i))
(test* "real?" #f (real? 3+4i))
(test* "real?" #f (real? +4.3i))
(test* "real?" #f (real? '()))

(test* "complex?" #t (complex? 0))
(test* "complex?" #t (complex? 85736847562938475634534245))
(test* "complex?" #t (complex? 857368.4756293847))
(test* "complex?" #t (complex? 3+0i))
(test* "complex?" #t (complex? 3+4i))
(test* "complex?" #t (complex? +4.3i))
(test* "complex?" #f (complex? '()))

(test* "number?" #t (number? 0))
(test* "number?" #t (number? 85736847562938475634534245))
(test* "number?" #t (number? 857368.4756293847))
(test* "number?" #t (number? 3+0i))
(test* "number?" #t (number? 3+4i))
(test* "number?" #t (number? +4.3i))
(test* "number?" #f (number? '()))

(test* "exact?" #t (exact? 1))
(test* "exact?" #t (exact? 4304953480349304983049304953804))
(test* "exact?" #t (exact? 430495348034930/4983049304953804))
(test* "exact?" #f (exact? 1.0))
(test* "exact?" #f (exact? 4304953480349304983.049304953804))
(test* "exact?" #f (exact? 1.0+0i))
(test* "exact?" #f (exact? 1.0+5i))
(test* "inexact?" #f (inexact? 1))
(test* "inexact?" #f (inexact? 4304953480349304983049304953804))
(test* "inexact?" #f (inexact? 430495348034930/4983049304953804))
(test* "inexact?" #t (inexact? 1.0))
(test* "inexact?" #t (inexact? 4304953480349304983.049304953804))
(test* "inexact?" #t (inexact? 1.0+0i))
(test* "inexact?" #t (inexact? 1.0+5i))

(test* "odd?" #t (odd? 1))
(test* "odd?" #f (odd? 2))
(test* "even?" #f (even? 1))
(test* "even?" #t (even? 2))
(test* "odd?" #t (odd? 1.0))
(test* "odd?" #f (odd? 2.0))
(test* "even?" #f (even? 1.0))
(test* "even?" #t (even? 2.0))
(test* "odd?" #t (odd? 10000000000000000000000000000000000001))
(test* "odd?" #f (odd? 10000000000000000000000000000000000002))
(test* "even?" #f (even? 10000000000000000000000000000000000001))
(test* "even?" #t (even? 10000000000000000000000000000000000002))

(test* "zero?" #t (zero? 0))
(test* "zero?" #t (zero? 0.0))
(test* "zero?" #t (zero? (- 10 10.0)))
(test* "zero?" #t (zero? 0+0i))
(test* "zero?" #f (zero? 1.0))
(test* "zero?" #f (zero? +5i))
(test* "positive?" #t (positive? 1))
(test* "positive?" #f (positive? -1))
(test* "positive?" #t (positive? 1/7))
(test* "positive?" #f (positive? -1/7))
(test* "positive?" #t (positive? 3.1416))
(test* "positive?" #f (positive? -3.1416))
(test* "positive?" #t (positive? 134539485343498539458394))
(test* "positive?" #f (positive? -134539485343498539458394))
(test* "negative?" #f (negative? 1))
(test* "negative?" #t (negative? -1))
(test* "negative?" #f (negative? 1/7))
(test* "negative?" #t (negative? -1/7))
(test* "negative?" #f (negative? 3.1416))
(test* "negative?" #t (negative? -3.1416))
(test* "negative?" #f (negative? 134539485343498539458394))
(test* "negative?" #t (negative? -134539485343498539458394))

(let1 tester
    (lambda (name proc result)
      (test* name (test-error) (proc #t))
      (test* name result
             (list (proc 1) (proc +inf.0) (proc -inf.0) (proc +nan.0))))
  (tester "finite?"   finite?   `(#t #f #f #f))
  (tester "infinite?" infinite? `(#f #t #t #f))
  (tester "nan?"      nan?      `(#f #f #f #t))
  )


(test* "eqv?" #t (eqv? 20 20))
(test* "eqv?" #t (eqv? 20.0 20.00000))
(test* "eqv?" #f (eqv? 4/5 0.8))
(test* "eqv?" #t (eqv? (exact->inexact 4/5) 0.8))
;(test* "eqv?" #f (eqv? 4/5 (inexact->exact 0.8)))
(test* "eqv?" #t (eqv? 20 (inexact->exact 20.0)))
(test* "eqv?" #f (eqv? 20 20.0))

;; numeric comparison involving nan.  we should test both 
;; inlined case and applied case
(define-macro (test-nan-cmp op)
  `(begin
     (test* (format "NaN ~a (inlined)" ',op) '(#f #f #f)
            (list (,op +nan.0 +nan.0) (,op +nan.0 0) (,op 0 +nan.0)))
     (test* (format "NaN ~a (applied)" ',op) '(#f #f #f)
            (list (apply ,op '(+nan.0 +nan.0))
                  (apply ,op '(+nan.0 0))
                  (apply ,op '(0 +nan.0))))))
(test-nan-cmp =)
(test-nan-cmp <)
(test-nan-cmp <=)
(test-nan-cmp >)
(test-nan-cmp >=)

;; the following tests combine instructions for comparison.
(let ((zz #f))
  (set! zz 3.14)  ;; prevent the compiler from optimizing constants

  (test* "NUMEQF" '(#t #t #f #f)
         (list (= 3.14 zz) (= zz 3.14) (= 3.15 zz) (= zz 3.15)))
  (test* "NLTF" '(#f #f #f #t #t #f)
         (list (< 3.14 zz) (< zz 3.14)
               (< 3.15 zz) (< zz 3.15)
               (< 3.13 zz) (< zz 3.13)))
  (test* "NLEF" '(#t #t #f #t #t #f)
         (list (<= 3.14 zz) (<= zz 3.14)
               (<= 3.15 zz) (<= zz 3.15)
               (<= 3.13 zz) (<= zz 3.13)))
  (test* "NGTF" '(#f #f #t #f #f #t)
         (list (> 3.14 zz) (> zz 3.14)
               (> 3.15 zz) (> zz 3.15)
               (> 3.13 zz) (> zz 3.13)))
  (test* "NGEF" '(#t #t #t #f #f #t)
         (list (>= 3.14 zz) (>= zz 3.14)
               (>= 3.15 zz) (>= zz 3.15)
               (>= 3.13 zz) (>= zz 3.13)))
  )

;; Go through number comparison routines.
;; assumes a >= b, a > 0, b > 0
;; we use apply to prevent inlining.
(define (numcmp-test msg eq a b) 
  (let ((pp (list a b))
        (pm (list a (- b)))
        (mp (list (- a) b))
        (mm (list (- a) (- b))))
    (define (test4 op opname rev results)
      (for-each (lambda (result comb args)
                  (test* #`",|msg| ,(if rev 'rev \"\") ,opname(,comb)" result
                         (apply op (if rev (reverse args) args))))
                results '(++ +- -+ --) (list pp pm mp mm)))
    (test4 =  '=  #f (list eq #f #f eq))
    (test4 =  '=  #t (list eq #f #f eq))
    (test4 >= '>= #f (list #t #t #f eq))
    (test4 >= '>= #t (list eq #f #t #t))
    (test4 >  '>  #f (list (not eq) #t #f #f))
    (test4 >  '>  #t (list #f #f #t (not eq)))
    (test4 <= '<= #f (list eq #f #t #t))
    (test4 <= '<= #t (list #t #t #f eq))
    (test4 <  '<  #f (list #f #f #t (not eq)))
    (test4 <  '<  #t (list (not eq) #t #f #f))
    ))

(numcmp-test "fixnum vs fixnum eq" #t 156 156)
(numcmp-test "fixnum vs fixnum ne" #f 878252 73224)
(numcmp-test "bignum vs fixnum ne" #f (expt 3 50) 9982425)
(numcmp-test "bignum vs bignum eq" #t (expt 3 50) (expt 3 50))
(numcmp-test "bignum vs bignum ne" #f (expt 3 50) (expt 3 49))
(numcmp-test "flonum vs fixnum eq" #t 314.0 314)
(numcmp-test "flonum vs fixnum ne" #f 3140.0 314)
(numcmp-test "flonum vs bignum eq" #t (expt 2.0 64) (expt 2 64))
(numcmp-test "flonum vs bignum ne" #f (expt 2.0 64) (expt 2 63))
(numcmp-test "ratnum vs fixnum ne" #f 13/2 6)
(numcmp-test "ratnum vs ratnum eq" #t 3/5 3/5)
(numcmp-test "ratnum vs ratnum 1 ne" #f 3/5 4/7)
(numcmp-test "ratnum vs ratnum 2 ne" #f 4/5 3/7)
(numcmp-test "ratnum vs ratnum 3 ne" #f 4/7 2/5)
(numcmp-test "ratnum vs ratnum 4 ne" #f 4/7 3/7)
(numcmp-test "ratnum vs flonum eq" #t 3/8 0.375)
(numcmp-test "ratnum vs flonum ne" #f 8/9 0.6)
(numcmp-test "ratnum vs bignum ne" #f (/ (+ (expt 2 64) 1) 2) (expt 2 63))

;; This is from the bug report from Bill Schottsteadt.  Before 0.8.10
;; this yielded #t because of the precision loss in fixnum vs ratnum
;; comparison.

(test* "fixnum/ratnum comparison" #f
       (= -98781233389595723930250385525631360344437602649022271391716773162526352115087074898920261954897888235939429993829738630297052776667061779065100945771127020439712527398509771853491319737304616607041615012797134365574007368603232768089410097730646360760856052946465578073788924743642391638455649511108051053789425902013657106523269224045822294981391380222050223141347787674321888089837786284947870569165079491411110074602544203383038299901291952931113248943344436935596614205784436844912243069019367149526328612664067719765890897558075277707055756274228634652905751880612235340874976952880431555921814590049070979276358637989837532124647692152520447680373275200239544449293834424643702763974403094033892112967196087310232853165951285609426599617479356206218697586025251765476179158153123631158173662488102357611674821528467825910806391548770908013608889792001203039243914696463472490444573930050190716726220002151679336252008777326482398042427845860796285369622627679324605214987983884122808994422164327311297556122943400093231935477754959547620500784989043704825777186301417894825200797719289692636286337716705491307686644214213732116277102140558505945554566856673724837541141206267647285222293953181717113434757149921850120377706206012113994795124049471433490016083401216757825264766474891405185591236321448744678896448941259668731597494947127423662646933419809756274038044752395708014998820826196523041220918922611359697502638594907608648168849193813197790291360087857093790119162389573209640804111261616771827989939551840471235079945175327536638365874717775169210186608268924244639016270610098894971732892267642318266405837012482726627199088381027028630711279130575230815976484191675172279903609489448225149181063260231957171204855841611039996959582465138269247794842445177715476581512709861409446684911276158067098438009067149531119008707418601627426255891/2063950098473886055933596136103014753954685977787179797499441692283103642150668140884348149132839387663291870239435604463778573480782766958396423322880804442523056530013282118705429274303746421980903580754656364533869319744640130831962767797772323836293079599182477171562218297208495122660799328579852852969560730744211066545295945803939271680397511478811389399527913043145952054883289558914237172406636283114284363301999238526952309439259354223729114988806937903509692118585280437646676248013406270664905997291670857985754768850507766359973207600149782819306010561088246502918148146264806947375101624011387317921439210509902170092173796154464078297852707797984007992277904626058467143192149921546030028316990855470478894515952884526783686210401408859364838148201339959570732480920969000913791571631154267939054105878236201498477027265774680071188764947522112650857013491135901945605796776829525789886482760578142306057177990048751864852763036720112071475134369179525117161001517868525821398753039187062869247457336940152614866298628205010037695017885878296140891234142925514925051385440766473260338168038302226808098439763889250948602137806546736025439919604390464712793474019469457135856879584745805794574609707742445431851999335443724488636749987837445626810087003490329257105472274738811579817454656532496370562155449815456374456838912258383282154811001588175608617475540639254689723629881619252699580383612847920348111900440075645703960104081690968807839189109040568288972353424306876947127635585164905071821419089229871978994388197349499565628906992171901547121903117815637249359328193980583892566359962066242217169190169986105579733710057404319381685578470983838597020624234209884597110721892707818651210378187525863009879314177842634871978427592746452643603586344401223449546482306838947819060455178762434166799996220143825677025686435609179225302671777326568324855229172912876656233006785717920665743720753617646617017219230313226844735567400507490772935145894670445831971526014183234960075574401616682479457962912905141754252265169682318523572680657053374002911007741991220001444440319448034755483178790032581428679303588017268970 0))

;;==================================================================
;; Fixnum stuff
;;

(test* "fixnum? fixnum" #t (fixnum? 0))
(test* "fixnum? ratnum" #f (fixnum? 1/2))
(test* "fixnum? bignum" #f (fixnum? (expt 2 256)))
(test* "fixnum? flonum" #f (fixnum? 3.14))
(test* "fixnum? compnum" #f (fixnum? 1+3i))

(test* "fixnum? greatest"    #t (fixnum? (greatest-fixnum)))
(test* "fixnum? greatest+1"  #f (fixnum? (+ (greatest-fixnum) 1)))
(test* "fixnum? least"       #t (fixnum? (least-fixnum)))
(test* "fixnum? least-1"     #f (fixnum? (- (least-fixnum) 1)))

(test* "greatest fixnum & width" (greatest-fixnum)
       (- (ash 1 (fixnum-width)) 1))
(test* "least fixnum & width" (least-fixnum)
       (- (ash 1 (fixnum-width))))

;;==================================================================
;; Arithmetics
;;

;;------------------------------------------------------------------
(test-section "integer addition")

(define x #xffffffff00000000ffffffff00000000)
(define xx (- x))
(define y #x00000002000000000000000200000000)
(define yy (- y))
(define z #x00000000000000010000000000000001)
(test* "bignum + bignum" #x100000001000000010000000100000000
      (+ x y))
(test* "bignum + -bignum" #xfffffffd00000000fffffffd00000000
      (+ x yy))
(test* "bignum - bignum" #xfffffffefffffffffffffffeffffffff
      (- x z))
(test* "bignum - bignum" x
      (- (+ x y) y))
(test* "-bignum + bignum" #x-fffffffd00000000fffffffd00000000
      (+ xx y))
(test* "-bignum + -bignum" #x-100000001000000010000000100000000
      (+ xx yy))
(test* "-bignum - bignum" #x-100000001000000010000000100000000
      (- xx y))
(test* "-bignum - -bignum" #x-fffffffd00000000fffffffd00000000
      (- xx yy))

;; This test a possible shortcut in Scm_Add etc.  We use apply
;; to avoid operators from being inlined.
(test* "0 + bignum" (list x x)
       (list (apply + (list 0 x)) (apply + (list x 0))))
(test* "0 - bignum" (list (- x) x)
       (list (apply - (list 0 x)) (apply - (list x 0))))
(test* "0 * bignum" (list 0 0)
       (list (apply * (list 0 x)) (apply * (list x 0))))
(test* "1 * bignum" (list x x)
       (list (apply * (list 1 x)) (apply * (list x 1))))
(test* "bignum / 1" x
       (apply / (list x 1)))

;;------------------------------------------------------------------
(test-section "small immediate integer constants")

;; pushing small literal integer on the stack may be done
;; by combined instruction PUSHI.  These test if it works.

(define (foo a b c d e) (list a b c d e))

;; 2^19-1
(test* "PUSHI" '(0 524287 524288 -524287 -524288)
              (foo 0 524287 524288 -524287 -524288))
;; 2^51-1
(test* "PUSHI" '(0 2251799813685247 2251799813685248
                  -2251799813685247 -2251799813685248 )
              (foo 0 2251799813685247 2251799813685248
             -2251799813685247 -2251799813685248))

;;------------------------------------------------------------------
(test-section "small immediate integer additions")

;; small literal integer x (-2^19 <= x < 2^19 on 32bit architecture)
;; in binary addition/subtraction is compiled in special instructuions,
;; NUMADDI and NUMSUBI.

(define x 2)
(test* "NUMADDI" 5 (+ 3 x))
(test* "NUMADDI" 5 (+ x 3))
(test* "NUMADDI" 1 (+ -1 x))
(test* "NUMADDI" 1 (+ x -1))
(test* "NUMSUBI" 1 (- 3 x))
(test* "NUMSUBI" -1 (- x 3))
(test* "NUMSUBI" -5 (- -3 x))
(test* "NUMSUBI" 5 (- x -3))
(define x 2.0)
(test* "NUMADDI" 5.0 (+ 3 x))
(test* "NUMADDI" 5.0 (+ x 3))
(test* "NUMADDI" 1.0 (+ -1 x))
(test* "NUMADDI" 1.0 (+ x -1))
(test* "NUMSUBI" 1.0 (- 3 x))
(test* "NUMSUBI" -1.0 (- x 3))
(test* "NUMSUBI" -5.0 (- -3 x))
(test* "NUMSUBI" 5.0 (- x -3))
(define x #x100000000)
(test* "NUMADDI" #x100000003 (+ 3 x))
(test* "NUMADDI" #x100000003 (+ x 3))
(test* "NUMADDI" #xffffffff (+ -1 x))
(test* "NUMADDI" #xffffffff (+ x -1))
(test* "NUMSUBI" #x-fffffffd (- 3 x))
(test* "NUMSUBI" #xfffffffd (- x 3))
(test* "NUMSUBI" #x-100000003 (- -3 x))
(test* "NUMSUBI" #x100000003 (- x -3))
(define x 33/7)
(test* "NUMADDI" 54/7 (+ 3 x))
(test* "NUMADDI" 54/7 (+ x 3))
(test* "NUMADDI" 26/7 (+ -1 x))
(test* "NUMADDI" 26/7 (+ x -1))
(test* "NUMADDI" -12/7 (- 3 x))
(test* "NUMADDI" 12/7 (- x 3))
(test* "NUMADDI" -54/7 (- -3 x))
(test* "NUMADDI" 54/7 (- x -3))

(test* "NUMADDI" 30 (+ 10 (if #t 20 25)))
(test* "NUMADDI" 30 (+ (if #t 20 25) 10))
(test* "NUMADDI" 35 (+ 10 (if #f 20 25)))
(test* "NUMADDI" 35 (+ (if #f 20 25) 10))
(test* "NUMADDI" 30 (let ((x #t)) (+ 10 (if x 20 25))))
(test* "NUMADDI" 30 (let ((x #t)) (+ (if x 20 25) 10)))
(test* "NUMADDI" 35 (let ((x #f)) (+ 10 (if x 20 25))))
(test* "NUMADDI" 35 (let ((x #f)) (+ (if x 20 25) 10)))
(test* "NUMADDI" 21 (+ 10 (do ((x 0 (+ x 1))) ((> x 10) x))))
(test* "NUMADDI" 21 (+ (do ((x 0 (+ x 1))) ((> x 10) x)) 10))
(test* "NUMSUBI" -10 (- 10 (if #t 20 25)))
(test* "NUMSUBI" 10 (- (if #t 20 25) 10))
(test* "NUMSUBI" -15 (- 10 (if #f 20 25)))
(test* "NUMSUBI" 15 (- (if #f 20 25) 10))
(test* "NUMSUBI" -10 (let ((x #t)) (- 10 (if x 20 25))))
(test* "NUMSUBI" 10 (let ((x #t)) (- (if x 20 25) 10)))
(test* "NUMSUBI" -15 (let ((x #f)) (- 10 (if x 20 25))))
(test* "NUMSUBI" 15 (let ((x #f)) (- (if x 20 25) 10)))
(test* "NUMSUBI" -1 (- 10 (do ((x 0 (+ x 1))) ((> x 10) x))))
(test* "NUMSUBI" 1 (- (do ((x 0 (+ x 1))) ((> x 10) x)) 10))

;;------------------------------------------------------------------
(test-section "immediate flonum integer arith")

;; tests special instructions for immediate flonum integer arithmetic


(define x 2.0)
(test* "NUMADDF" 5.0 (+ 3 x))
(test* "NUMADDF" 5.0 (+ x 3))
(test* "NUMADDF" 1.0 (+ -1 x))
(test* "NUMADDF" 1.0 (+ x -1))
(test* "NUMADDF" 2.0+1.0i (+ +i x))
(test* "NUMADDF" 2.0+1.0i (+ x +i))

(test* "NUMSUBF" 1.0 (- 3 x))
(test* "NUMSUBF" -1.0 (- x 3))
(test* "NUMSUBF" -5.0 (- -3 x))
(test* "NUMSUBF" 5.0 (- x -3))
(test* "NUMSUBF" -2.0+1.0i (- +i x))
(test* "NUMSUBF" 2.0-1.0i (- x +i))

(test* "NUMMULF" 4.0 (* x 2))
(test* "NUMMULF" 4.0 (* 2 x))
(test* "NUMMULF" 3.0 (* x 1.5))
(test* "NUMMULF" 3.0 (* 1.5 x))
(test* "NUMMULF" 0+2.0i (* x +i))
(test* "NUMMULF" 0+2.0i (* +i x))

(test* "NUMDIVF" 0.5 (/ x 4))
(test* "NUMDIVF" 2.0 (/ 4 x))
(test* "NUMDIVF" 0.5 (/ x 4.0))
(test* "NUMDIVF" 2.0 (/ 4.0 x))
(test* "NUMDIVF" 0.0-0.5i (/ x +4i))
(test* "NUMDIVF" 0.0+2.0i (/ +4i x))

;;------------------------------------------------------------------
(test-section "rational number addition")

(test* "ratnum +" 482/247 (+ 11/13 21/19))
(test* "ratnum -" -64/247 (- 11/13 21/19))

;; tests possible shortcut in Scm_Add etc.
(test* "ratnum + 0" (list 11/13 11/13)
       (list (apply + '(0 11/13)) (apply + '(11/13 0))))
(test* "ratnum - 0" (list -11/13 11/13)
       (list (apply - '(0 11/13)) (apply - '(11/13 0))))
(test* "ratnum * 0" (list 0 0)
       (list (apply * '(0 11/13)) (apply * '(11/13 0))))
(test* "ratnum * 1" (list 11/13 11/13)
       (list (apply * '(1 11/13)) (apply * '(11/13 1))))
(test* "ratnum / 1" 11/13
       (apply / '(11/13 1)))
 
;;------------------------------------------------------------------
(test-section "promotions in addition")

(define-syntax +-tester
  (syntax-rules ()
    ((_ (+ . args))
     (let ((inline (+ . args))
           (other  (apply + 'args)))
       (and (= inline other)
            (list inline (exact? inline)))))))

(test* "+" '(0 #t) (+-tester (+)))
(test* "+" '(1 #t) (+-tester (+ 1)))
(test* "+" '(3 #t) (+-tester (+ 1 2)))
(test* "+" '(6 #t) (+-tester (+ 1 2 3)))
(test* "+" '(1 #t) (+-tester (+ 1/6 1/3 1/2)))
(test* "+" '(1.0 #f) (+-tester (+ 1.0)))
(test* "+" '(3.0 #f) (+-tester (+ 1.0 2)))
(test* "+" '(3.0 #f) (+-tester (+ 1 2.0)))
(test* "+" '(6.0 #f) (+-tester (+ 1 2 3.0)))
(test* "+" '(1.0 #f) (+-tester (+ 1/6 1/3 0.5)))
(test* "+" '(1+i #f) (+-tester (+ 1 +i)))
(test* "+" '(3+i #f) (+-tester (+ 1 2 +i)))
(test* "+" '(3+i #f) (+-tester (+ +i 1 2)))
(test* "+" '(3+i #f) (+-tester (+ 1.0 2 +i)))
(test* "+" '(3+i #f) (+-tester (+ +i 1.0 2)))
(test* "+" '(4294967298.0 #f) (+-tester (+ 4294967297 1.0)))
(test* "+" '(4294967299.0 #f) (+-tester (+ 4294967297 1 1.0)))
(test* "+" '(4294967298.0-i #f) (+-tester (+ 4294967297 1.0 -i)))
(test* "+" '(4294967298.0-i #f) (+-tester (+ -i 4294967297 1.0)))
(test* "+" '(4294967298.0-i #f) (+-tester (+ 1.0 4294967297 -i)))

;;------------------------------------------------------------------
(test-section "integer multiplication")

(define (m-result x) (list x (- x) (- x) x x (- x) (- x) x))
(define (m-tester x y)
  (list (* x y) (* (- x) y) (* x (- y)) (* (- x) (- y))
        (apply * (list x y)) (apply * (list (- x) y))
        (apply * (list x (- y))) (apply * (list (- x) (- y)))))

(test* "fix*fix->big[1]" (m-result 727836879)
      (m-tester 41943 17353))
(test* "fix*fix->big[1]" (m-result 3663846879)
      (m-tester 41943 87353))
(test* "fix*fix->big[2]" (m-result 4294967296)
      (m-tester 65536 65536))
(test* "fix*fix->big[2]" (m-result 366384949959)
      (m-tester 4194303 87353))
(test* "fix*big[1]->big[1]" (m-result 3378812463)
      (m-tester 3 1126270821))
(test* "fix*big[1]->big[2]" (m-result 368276265762816)
      (m-tester 85746 4294967296))
(test* "big[1]*fix->big[1]" (m-result 3378812463)
      (m-tester 1126270821 3))
(test* "big[1]*fix->big[2]" (m-result 368276265762816)
      (m-tester 4294967296 85746))
(test* "big[2]*fix->big[2]" (m-result 12312849128741)
      (m-tester 535341266467 23))
(test* "big[1]*big[1]->big[2]" (m-result 1345585795375391817)
      (m-tester 1194726677 1126270821))

;; Large number multiplication test using Fermat's number
;; The decomposition of Fermat's number is taken from
;;   http://www.dd.iij4u.or.jp/~okuyamak/Information/Fermat.html
(test* "fermat(7)" (fermat 7)
      (* 59649589127497217 5704689200685129054721))
(test* "fermat(8)" (fermat 8)
              (* 1238926361552897
           93461639715357977769163558199606896584051237541638188580280321))
(test* "fermat(9)" (fermat 9)
              (* 2424833
           7455602825647884208337395736200454918783366342657
           741640062627530801524787141901937474059940781097519023905821316144415759504705008092818711693940737))
(test* "fermat(10)" (fermat 10)
              (* 45592577
           6487031809
           4659775785220018543264560743076778192897
           130439874405488189727484768796509903946608530841611892186895295776832416251471863574140227977573104895898783928842923844831149032913798729088601617946094119449010595906710130531906171018354491609619193912488538116080712299672322806217820753127014424577
           ))
(test* "fermat(11)" (fermat 11)
              (* 319489
           974849
           167988556341760475137
           3560841906445833920513
           173462447179147555430258970864309778377421844723664084649347019061363579192879108857591038330408837177983810868451546421940712978306134189864280826014542758708589243873685563973118948869399158545506611147420216132557017260564139394366945793220968665108959685482705388072645828554151936401912464931182546092879815733057795573358504982279280090942872567591518912118622751714319229788100979251036035496917279912663527358783236647193154777091427745377038294584918917590325110939381322486044298573971650711059244462177542540706913047034664643603491382441723306598834177
           ))

;;------------------------------------------------------------------
(test-section "multiplication short cuts")

;; these test shortcut in Scm_Mul
;; note the difference of 0 and 0.0
(let1 big 100000000000000000000
  (test* "bignum * 0"  0 (apply * `(,big 0)) eqv?)
  (test* "0 * bignum"  0 (apply * `(0 ,big)) eqv?)
  (test* "bignum * 1"  big (apply * `(,big 1)) eqv?)
  (test* "1 * bignum"  big (apply * `(1 ,big)) eqv?)

  (test* "bignum * 0.0"  0.0 (apply * `(,big 0.0)) eqv?)
  (test* "0.0 * bignum"  0.0 (apply * `(0.0 ,big)) eqv?)
  (test* "bignum * 1.0"  1.0e20 (apply * `(,big 1.0)) eqv?)
  (test* "1.0 * bignum"  1.0e20 (apply * `(1.0 ,big)) eqv?)
  )

(test* "ratnum * 0"  0 (apply * '(1/2 0)) eqv?)
(test* "0 * ratnum"  0 (apply * '(0 1/2)) eqv?)
(test* "ratnum * 1"  1/2 (apply * '(1/2 1)) eqv?)
(test* "1 * ratnum"  1/2 (apply * '(1 1/2)) eqv?)

(test* "ratnum * 0.0"  0.0 (apply * '(1/2 0.0)) eqv?)
(test* "0.0 * ratnum"  0.0 (apply * '(0.0 1/2)) eqv?)
(test* "ratnum * 1.0"  0.5 (apply * '(1/2 1.0)) eqv?)
(test* "1.0 * ratnum"  0.5 (apply * '(1.0 1/2)) eqv?)

(test* "flonum * 0"  0 (apply * '(3.0 0)) eqv?)
(test* "0 * flonum"  0 (apply * '(0 3.0)) eqv?)
(test* "flonum * 1"  3.0 (apply * '(3.0 1)) eqv?)
(test* "1 * flonum"  3.0 (apply * '(1 3.0)) eqv?)

(test* "flonum * 0.0"  0.0 (apply * '(3.0 0.0)) eqv?)
(test* "0.0 * flonum"  0.0 (apply * '(0.0 3.0)) eqv?)
(test* "flonum * 1.0"  3.0 (apply * '(3.0 1.0)) eqv?)
(test* "1.0 * flonum"  3.0 (apply * '(1.0 3.0)) eqv?)

(test* "compnum * 0" 0 (* 0 +i) eqv?)
(test* "0 * compnum" 0 (* +i 0) eqv?)
(test* "compnum * 1" +i (* 1 +i) eqv?)
(test* "1 * compnum" +i (* +i 1) eqv?)

(test* "compnum * 0.0" 0.0 (* 0.0 +i) eqv?)
(test* "0.0 * compnum" 0.0 (* +i 0.0) eqv?)
(test* "compnum * 1.0" +i (* 1.0 +i) eqv?)
(test* "1.0 * compnum" +i (* +i 1.0) eqv?)

;;------------------------------------------------------------------
(test-section "division")

(test* "exact division" 3/20 (/ 3 4 5))
(test* "exact division" 1/2  (/ 9223372036854775808 18446744073709551616))
(test* "exact division" 4692297364841/7
       (/ 28153784189046 42))
(test* "exact division" 7/4692297364841
       (/ 42 28153784189046))
(test* "exact division" -7/4692297364841
       (/ 42 -28153784189046))
(test* "exact division" 7/4692297364841
       (/ -42 -28153784189046))
(test* "exact reciprocal" 1/3 (/ 3))
(test* "exact reciprocal" -1/3 (/ -3))
(test* "exact reciprocal" 5/6 (/ 6/5))
(test* "exact reciprocal" -5/6 (/ -6/5))
(test* "exact reciprocal" 7/4692297364841 (/ 4692297364841/7))

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

;; inexact division
(test* "exact/inexact -> inexact" (d-result 3.25 #f)
      (d-tester 13 4.0))
(test* "exact/inexact -> inexact" (d-result 1.625 #f)
      (d-tester 13/2 4.0))
(test* "inexact/exact -> inexact" (d-result 3.25 #f)
      (d-tester 13.0 4))
(test* "inexact/exact -> inexact" (d-result 9.75 #f)
      (d-tester 13.0 4/3))
(test* "inexact/inexact -> inexact" (d-result 3.25 #f)
      (d-tester 13.0 4.0))

;; complex division
(test* "complex division" 0.0
       (let ((a 3)
             (b 4+3i)
             (c 7.3))
         (- (/ a b c)
            (/ (/ a b) c))))

;;------------------------------------------------------------------
(test-section "quotient")

(define (q-result x exact?) (list x (- x) (- x) x exact?))
(define (q-tester x y)
  (list (quotient x y) (quotient (- x) y)
        (quotient x (- y)) (quotient (- x) (- y))
        (exact? (quotient x y))))

;; these uses BignumDivSI -> bignum_sdiv
(test* "big[1]/fix->fix" (q-result 17353 #t) 
      (q-tester 727836879 41943))
(test* "big[1]/fix->fix" (q-result 136582 #t)
      (q-tester 3735928559 27353))
(test* "big[2]/fix->big[1]" (q-result 535341266467 #t)
      (q-tester 12312849128741 23))
(test* "big[2]/fix->big[2]" (q-result 12312849128741 #t)
      (q-tester 12312849128741 1))

;; these uses BignumDivSI -> bignum_gdiv
(test* "big[1]/fix->fix" (q-result 41943 #t)
      (q-tester 3663846879 87353))
(test* "big[2]/fix->fix" (q-result 19088743 #t)
      (q-tester 705986470884353 36984440))
(test* "big[2]/fix->fix" (q-result 92894912 #t)
      (q-tester 12312849128741 132546))
(test* "big[2]/fix->big[1]" (q-result 2582762030 #t)
      (q-tester 425897458766735 164900))

;; these uses BignumDivRem
(test* "big[1]/big[1]->fix" (q-result 2 #t)
      (q-tester 4020957098 1952679221))
(test* "big[1]/big[1] -> fix" (q-result 0 #t)
      (q-tester 1952679221 4020957098))
;; this tests loop in estimation phase
(test* "big[3]/big[2] -> big[1]" (q-result #xffff0001 #t)
      (q-tester #x10000000000000000 #x10000ffff))
;; this test goes through a rare case handling code ("add back") in
;; the algorithm.
(test* "big[3]/big[2] -> fix" (q-result #xeffe #t)
      (q-tester #x7800000000000000 #x80008889ffff))

;; inexact quotient
(test* "exact/inexact -> inexact" (q-result 3.0 #f)
      (q-tester 13 4.0))
(test* "inexact/exact -> inexact" (q-result 3.0 #f)
      (q-tester 13.0 4))
(test* "inexact/inexact -> inexact" (q-result 3.0 #f)
      (q-tester 13.0 4.0))
(test* "exact/inexact -> inexact" (q-result 17353.0 #f)
      (q-tester 727836879 41943.0))
(test* "inexact/exact -> inexact" (q-result 17353.0 #f)
      (q-tester 727836879.0 41943))
(test* "inexact/inexact -> inexact" (q-result 17353.0 #f)
      (q-tester 727836879.0 41943.0))

;; Test by fermat numbers
(test* "fermat(7)" 59649589127497217
      (quotient (fermat 7) 5704689200685129054721))
(test* "fermat(8)" 1238926361552897
              (quotient (fermat 8) 93461639715357977769163558199606896584051237541638188580280321))
(test* "fermat(9)" 2424833
              (quotient (quotient (fermat 9) 7455602825647884208337395736200454918783366342657)
                  741640062627530801524787141901937474059940781097519023905821316144415759504705008092818711693940737))
(test* "fermat(10)" 4659775785220018543264560743076778192897
              (quotient (quotient (quotient (fermat 10)
                                      130439874405488189727484768796509903946608530841611892186895295776832416251471863574140227977573104895898783928842923844831149032913798729088601617946094119449010595906710130531906171018354491609619193912488538116080712299672322806217820753127014424577)
                            6487031809)
                  45592577))
(test* "fermat(11)" 3560841906445833920513
              (quotient (quotient (quotient (quotient (fermat 11)
                                                167988556341760475137)
                                      173462447179147555430258970864309778377421844723664084649347019061363579192879108857591038330408837177983810868451546421940712978306134189864280826014542758708589243873685563973118948869399158545506611147420216132557017260564139394366945793220968665108959685482705388072645828554151936401912464931182546092879815733057795573358504982279280090942872567591518912118622751714319229788100979251036035496917279912663527358783236647193154777091427745377038294584918917590325110939381322486044298573971650711059244462177542540706913047034664643603491382441723306598834177
                                      )
                            974849)
                  319489))

;;------------------------------------------------------------------
(test-section "remainder")

(define (r-result x exact?) (list x (- x) x (- x) exact?))
(define (r-tester x y)
  (list (remainder x y) (remainder (- x) y)
        (remainder x (- y)) (remainder (- x) (- y))
        (exact? (remainder x y))))

;; small int
(test* "fix rem fix -> fix" (r-result 1 #t)
      (r-tester 13 4))
(test* "fix rem fix -> fix" (r-result 1234 #t)
      (r-tester 1234 87935))
(test* "fix rem big[1] -> fix" (r-result 12345 #t)
      (r-tester 12345 3735928559))

;; these uses BignumDivSI -> bignum_sdiv
(test* "big[1] rem fix -> fix" (r-result 0 #t)
      (r-tester 727836879 41943))
(test* "big[1] rem fix -> fix" (r-result 1113 #t)
      (r-tester 3735928559 27353))
(test* "big[2] rem fix -> fix" (r-result 15 #t)
      (r-tester 12312849128756 23))
(test* "big[2] rem fix -> fix" (r-result 0 #t)
      (r-tester 12312849128756 1))

;; these uses BignumDivSI -> bignum_gdiv
(test* "big[1] rem fix -> fix" (r-result 0 #t)
      (r-tester 3663846879 87353))
(test* "big[2] rem fix -> fix" (r-result 725433 #t)
      (r-tester 705986470884353 36984440))
(test* "big[2] rem fix -> fix" (r-result 122789 #t)
      (r-tester 12312849128741 132546))
(test* "big[2] rem fix -> fix" (r-result 19735 #t)
      (r-tester 425897458766735 164900))

;; these uses BignumDivRem
(test* "big[1] rem big[1] -> fix" (r-result 115598656 #t)
      (r-tester 4020957098 1952679221))
(test* "big[1] rem big[1] -> fix" (r-result 1952679221 #t)
      (r-tester 1952679221 4020957098))
;; this tests loop in estimation phase
(test* "big[3] rem big[2] -> big[1]" (r-result #xfffe0001 #t)
      (r-tester #x10000000000000000 #x10000ffff))
;; this tests "add back" code
(test* "big[3] rem big[2] -> big[2]" (r-result #x7fffb114effe #t)
      (r-tester #x7800000000000000 #x80008889ffff))

;; inexact remainder
(test* "exact rem inexact -> inexact" (r-result 1.0 #f)
      (r-tester 13 4.0))
(test* "inexact rem exact -> inexact" (r-result 1.0 #f)
      (r-tester 13.0 4))
(test* "inexact rem inexact -> inexact" (r-result 1.0 #f)
      (r-tester 13.0 4.0))
(test* "exact rem inexact -> inexact" (r-result 1113.0 #f)
      (r-tester 3735928559 27353.0))
(test* "inexact rem exact -> inexact" (r-result 1113.0 #f)
      (r-tester 3735928559.0 27353))
(test* "inexact rem inexact -> inexact" (r-result 1113.0 #f)
      (r-tester 3735928559.0 27353.0))

;;------------------------------------------------------------------
(test-section "modulo")

(define (m-result a b exact?) (list a b (- b) (- a) exact?))
(define (m-tester x y)
  (list (modulo x y) (modulo (- x) y)
        (modulo x (- y)) (modulo (- x) (- y))
        (exact? (modulo x y))))

;; small int
(test* "fix mod fix -> fix" (m-result 1 3 #t)
      (m-tester 13 4))
(test* "fix mod fix -> fix" (m-result 1234 86701 #t)
      (m-tester 1234 87935))
(test* "fix mod big[1] -> fix/big" (m-result 12345 3735916214 #t)
      (m-tester 12345 3735928559))

;; these uses BignumDivSI -> bignum_sdiv
(test* "big[1] mod fix -> fix" (m-result 0 0 #t)
      (m-tester 727836879 41943))
(test* "big[1] mod fix -> fix" (m-result 1113 26240 #t)
      (m-tester 3735928559 27353))
(test* "big[2] mod fix -> fix" (m-result 15 8 #t)
      (m-tester 12312849128756 23))
(test* "big[2] mod fix -> fix" (m-result 0 0 #t)
      (m-tester 12312849128756 1))

;; these uses BignumDivSI -> bignum_gdiv
(test* "big[1] mod fix -> fix" (m-result 0 0 #t)
      (m-tester 3663846879 87353))
(test* "big[2] mod fix -> fix" (m-result 725433 36259007 #t)
      (m-tester 705986470884353 36984440))
(test* "big[2] mod fix -> fix" (m-result 122789 9757 #t)
      (m-tester 12312849128741 132546))
(test* "big[2] mod fix -> fix" (m-result 19735 145165 #t)
      (m-tester 425897458766735 164900))

;; these uses BignumDivRem
(test* "big[1] mod big[1] -> fix" (m-result 115598656 1837080565 #t)
      (m-tester 4020957098 1952679221))
(test* "big[1] mod big[1] -> fix" (m-result 1952679221 2068277877 #t)
      (m-tester 1952679221 4020957098))
;; this tests loop in estimation phase
(test* "big[3] mod big[2] -> big[1]" (m-result #xfffe0001 #x2fffe #t)
      (m-tester #x10000000000000000 #x10000ffff))
;; this tests "add back" code
(test* "big[3] mod big[2] -> big[2]" (m-result #x7fffb114effe #xd7751001 #t)
      (m-tester #x7800000000000000 #x80008889ffff))

;; inexact modulo
(test* "exact mod inexact -> inexact" (m-result 1.0 3.0 #f)
      (m-tester 13 4.0))
(test* "inexact mod exact -> inexact" (m-result 1.0 3.0 #f)
      (m-tester 13.0 4))
(test* "inexact mod inexact -> inexact" (m-result 1.0 3.0 #f)
      (m-tester 13.0 4.0))
(test* "exact mod inexact -> inexact" (m-result 1113.0 26240.0 #f)
      (m-tester 3735928559 27353.0))
(test* "inexact mod exact -> inexact" (m-result 1113.0 26240.0 #f)
      (m-tester 3735928559.0 27353))
(test* "inexact mod inexact -> inexact" (m-result 1113.0 26240.0 #f)
      (m-tester 3735928559.0 27353.0))

;; test by mersenne prime? - code by 'hipster'

(define (mersenne-prime? p)
  (let ((m (- (expt 2 p) 1)))
    (do ((i 3 (+ i 1))
         (s 4 (modulo (- (* s s) 2) m)))
        ((= i (+ p 1)) (= s 0)))))

(test* "mersenne prime"
       '(#t #t #t #t #t #t #t #t #t #t #t #t #t #t)
       (map mersenne-prime? '(3 5 7 13 17 19 31 61 89 107 127 521 607 1279)))


;;------------------------------------------------------------------
(test-section "rounding")

(define (round-tester value exactness cei flo tru rou)
  (test* (string-append "rounding " (number->string value))
         (list exactness cei flo tru rou)
         (let ((c (ceiling value))
               (f (floor value))
               (t (truncate value))
               (r (round value)))
           (list (and (exact? c) (exact? f) (exact? t) (exact? r))
                 c f t r))))

(round-tester 0  #t 0 0 0 0)
(round-tester 3  #t 3 3 3 3)
(round-tester -3 #t -3 -3 -3 -3)
(round-tester (expt 2 99) #t (expt 2 99) (expt 2 99) (expt 2 99) (expt 2 99))
(round-tester (- (expt 2 99)) #t
              (- (expt 2 99)) (- (expt 2 99)) (- (expt 2 99)) (- (expt 2 99)))

(round-tester 9/4  #t 3 2 2 2)
(round-tester -9/4 #t -2 -3 -2 -2)
(round-tester 34985495387484938453495/17 #t
              2057970316910878732559
              2057970316910878732558
              2057970316910878732558
              2057970316910878732559)
(round-tester -34985495387484938453495/17 #t
              -2057970316910878732558
              -2057970316910878732559
              -2057970316910878732558
              -2057970316910878732559)

(round-tester 35565/2 #t 17783 17782 17782 17782)
(round-tester -35565/2 #t -17782 -17783 -17782 -17782)
(round-tester 35567/2 #t 17784 17783 17783 17784)
(round-tester -35567/2 #t -17783 -17784 -17783 -17784)

(test* "round->exact" 3 (round->exact 3.4) =)
(test* "round->exact" 4 (round->exact 3.5) =)
(test* "floor->exact" 3 (floor->exact 3.4) =)
(test* "floor->exact" -4 (floor->exact -3.5) =)
(test* "ceiling->exact" 4 (ceiling->exact 3.4) =)
(test* "ceiling->exact" -3 (ceiling->exact -3.5) =)
(test* "truncate->exact" 3 (truncate->exact 3.4) =)
(test* "truncate->exact" -3 (truncate->exact -3.5) =)

;;------------------------------------------------------------------
(test-section "clamping")

(test* "clamp (1)"   1   (clamp 1) eqv?)
(test* "clamp (1 #f)" 1  (clamp 1 #f) eqv?)
(test* "clamp (1 #f #f)" 1  (clamp 1 #f #f) eqv?)
(test* "clamp (1.0)"   1.0   (clamp 1.0) eqv?)
(test* "clamp (1.0 #f)" 1.0  (clamp 1.0 #f) eqv?)
(test* "clamp (1.0 #f #f)" 1.0  (clamp 1.0 #f #f) eqv?)

(test* "clamp (1 0)" 1   (clamp 1 0) eqv?)
(test* "clamp (1 0 #f)" 1 (clamp 1 0 #f) eqv?)
(test* "clamp (1 0 2)" 1 (clamp 1 0 2) eqv?)
(test* "clamp (1 5/4)" 5/4 (clamp 1 5/4) eqv?)
(test* "clamp (1 5/4 #f)" 5/4 (clamp 1 5/4 #f) eqv?)
(test* "clamp (1 #f 5/4)" 1 (clamp 1 #f 5/4) eqv?)
(test* "clamp (1 0 3/4)" 3/4 (clamp 1 0 3/4) eqv?)
(test* "clamp (1 #f 3/4)" 3/4 (clamp 1 #f 3/4) eqv?)

(test* "clamp (1.0 0)" 1.0   (clamp 1.0 0) eqv?)
(test* "clamp (1.0 0 #f)" 1.0 (clamp 1.0 0 #f) eqv?)
(test* "clamp (1.0 0 2)" 1.0 (clamp 1.0 0 2) eqv?)
(test* "clamp (1.0 5/4)" 1.25 (clamp 1.0 5/4) eqv?)
(test* "clamp (1.0 5/4 #f)" 1.25 (clamp 1.0 5/4 #f) eqv?)
(test* "clamp (1.0 #f 5/4)" 1.0 (clamp 1.0 #f 5/4) eqv?)
(test* "clamp (1.0 0 3/4)" 0.75 (clamp 1.0 0 3/4) eqv?)
(test* "clamp (1.0 #f 3/4)" 0.75 (clamp 1.0 #f 3/4) eqv?)

(test* "clamp (1 0.0)" 1.0   (clamp 1 0.0) eqv?)
(test* "clamp (1 0.0 #f)" 1.0 (clamp 1 0.0 #f) eqv?)
(test* "clamp (1 0.0 2)" 1.0 (clamp 1 0.0 2) eqv?)
(test* "clamp (1 0 2.0)" 1.0 (clamp 1 0 2.0) eqv?)
(test* "clamp (1 1.25)" 1.25 (clamp 1 1.25) eqv?)
(test* "clamp (1 #f 1.25)" 1.0 (clamp 1 #f 1.25) eqv?)
(test* "clamp (1 1.25 #f)" 1.25 (clamp 1 1.25 #f) eqv?)
(test* "clamp (1 0.0 3/4)" 0.75 (clamp 1 0.0 3/4) eqv?)
(test* "clamp (1 0 0.75)" 0.75 (clamp 1 0 0.75) eqv?)

(test* "clamp (1 -inf.0 +inf.0)" 1.0 (clamp 1 -inf.0 +inf.0) eqv?)

;;------------------------------------------------------------------
(test-section "logical operations")

(test* "ash (fixnum)" #x408000           ;fixnum
      (ash #x81 15))
(test* "ash (fixnum)" #x81
      (ash #x408000 -15))
(test* "ash (fixnum)" #x01
      (ash #x408000 -22))
(test* "ash (fixnum)" 0
      (ash #x408000 -23))
(test* "ash (fixnum)" 0
      (ash #x408000 -24))
(test* "ash (fixnum)" 0
      (ash #x408000 -100))
(test* "ash (fixnum)" #x81
      (ash #x81 0))
(test* "ash (neg. fixnum)" #x-408000  ;negative fixnum
      (ash #x-81 15))
(test* "ash (neg. fixnum)" #x-81      ;nagative fixnum
      (ash #x-408000 -15))
(test* "ash (fixnum)" -2
      (ash #x-408000 -22))
(test* "ash (fixnum)" -1
      (ash #x-408000 -23))
(test* "ash (fixnum)" -1
      (ash #x-408000 -24))
(test* "ash (fixnum)" -1
      (ash #x-408000 -100))
(test* "ash (fixnum)" #x-408000
      (ash #x-408000 0))

(test* "ash (fixnum->bignum)" #x81000000
      (ash #x81 24))
(test* "ash (fixnum->bignum)" #x4080000000
      (ash #x81 31))
(test* "ash (fixnum->bignum)" #x8100000000
      (ash #x81 32))
(test* "ash (fixnum->bignum)" #x8100000000000000
      (ash #x81 56))
(test* "ash (fixnum->bignum)" #x408000000000000000
      (ash #x81 63))
(test* "ash (fixnum->bignum)" #x810000000000000000
      (ash #x81 64))
(test* "ash (neg.fixnum->bignum)" #x-81000000
      (ash #x-81 24))
(test* "ash (neg.fixnum->bignum)" #x-4080000000
      (ash #x-81 31))
(test* "ash (neg.fixnum->bignum)" #x-8100000000
      (ash #x-81 32))
(test* "ash (neg.fixnum->bignum)" #x-8100000000000000
      (ash #x-81 56))
(test* "ash (neg.fixnum->bignum)" #x-408000000000000000
      (ash #x-81 63))
(test* "ash (neg.fixnum->bignum)" #x-810000000000000000
      (ash #x-81 64))

(test* "ash (bignum->fixnum)" #x81
      (ash  #x81000000 -24))
(test* "ash (bignum->fixnum)" #x40
      (ash  #x81000000 -25))
(test* "ash (bignum->fixnum)" 1
      (ash  #x81000000 -31))
(test* "ash (bignum->fixnum)" 0
      (ash  #x81000000 -32))
(test* "ash (bignum->fixnum)" 0
      (ash  #x81000000 -100))
(test* "ash (bignum->fixnum)" #x81
      (ash #x4080000000 -31))
(test* "ash (bignum->fixnum)" #x81
      (ash #x8100000000 -32))
(test* "ash (bignum->fixnum)" #x40
      (ash #x8100000000 -33))
(test* "ash (bignum->fixnum)" 1
      (ash #x8100000000 -39))
(test* "ash (bignum->fixnum)" 0
      (ash #x8100000000 -40))
(test* "ash (bignum->fixnum)" 0
      (ash #x8100000000 -100))
(test* "ash (bignum->fixnum)" #x81
      (ash #x8100000000000000 -56))
(test* "ash (bignum->fixnum)" #x81
      (ash #x408000000000000000 -63))
(test* "ash (bignum->fixnum)" #x40
      (ash #x408000000000000000 -64))
(test* "ash (bignum->fixnum)" #x20
      (ash #x408000000000000000 -65))
(test* "ash (bignum->fixnum)" 1
      (ash #x408000000000000000 -70))
(test* "ash (bignum->fixnum)" 0
      (ash #x408000000000000000 -71))
(test* "ash (bignum->fixnum)" 0
      (ash #x408000000000000000 -100))

(test* "ash (neg.bignum->fixnum)" #x-81
      (ash #x-81000000 -24))
(test* "ash (neg.bignum->fixnum)" #x-41
      (ash #x-81000000 -25))
(test* "ash (neg.bignum->fixnum)" #x-21
      (ash #x-81000000 -26))
(test* "ash (neg.bignum->fixnum)" -2
      (ash #x-81000000 -31))
(test* "ash (neg.bignum->fixnum)" -1
      (ash #x-81000000 -32))
(test* "ash (neg.bignum->fixnum)" -1
      (ash #x-81000000 -33))
(test* "ash (neg.bignum->fixnum)" -1
      (ash #x-81000000 -100))
(test* "ash (neg.bignum->fixnum)" #x-81
      (ash #x-4080000000 -31))
(test* "ash (neg.bignum->fixnum)" #x-41
      (ash #x-4080000000 -32))
(test* "ash (neg.bignum->fixnum)" #x-21
      (ash #x-4080000000 -33))
(test* "ash (neg.bignum->fixnum)" -2
      (ash #x-4080000000 -38))
(test* "ash (neg.bignum->fixnum)" -1
      (ash #x-4080000000 -39))
(test* "ash (neg.bignum->fixnum)" -1
      (ash #x-4080000000 -100))
(test* "ash (neg.bignum->fixnum)" #x-81
      (ash #x-408000000000000000 -63))
(test* "ash (neg.bignum->fixnum)" #x-41
      (ash #x-408000000000000000 -64))
(test* "ash (neg.bignum->fixnum)" #x-21
      (ash #x-408000000000000000 -65))
(test* "ash (neg.bignum->fixnum)" -2
      (ash #x-408000000000000000 -70))
(test* "ash (neg.bignum->fixnum)" -1
      (ash #x-408000000000000000 -71))
(test* "ash (neg.bignum->fixnum)" -1
      (ash #x-408000000000000000 -72))

(test* "ash (bignum->bignum)" #x12345678123456780
      (ash #x1234567812345678 4))
(test* "ash (bignum->bignum)" #x1234567812345678000000000000000
      (ash #x1234567812345678 60))
(test* "ash (bignum->bignum)" #x12345678123456780000000000000000
      (ash #x1234567812345678 64))
(test* "ash (bignum->bignum)" #x123456781234567
      (ash #x1234567812345678 -4))
(test* "ash (bignum->bignum)" #x12345678
      (ash #x1234567812345678 -32))
(test* "ash (neg.bignum->bignum)" #x-123456781234568
      (ash #x-1234567812345678 -4))
(test* "ash (bignum->bignum)" #x-12345679
      (ash #x-1234567812345678 -32))

(test* "lognot (fixnum)" -1 (lognot 0))
(test* "lognot (fixnum)" 0 (lognot -1))
(test* "lognot (fixnum)" -65536 (lognot 65535))
(test* "lognot (fixnum)" 65535 (lognot -65536))
(test* "lognot (bignum)" #x-1000000000000000001
      (lognot #x1000000000000000000))
(test* "lognot (bignum)" #x1000000000000000000
      (lognot #x-1000000000000000001))

(test* "logand (+fix & 0)" 0
      (logand #x123456 0))
(test* "logand (+big & 0)" 0
      (logand #x1234567812345678 0))
(test* "logand (+fix & -1)" #x123456
      (logand #x123456 -1))
(test* "logand (+big & -1)" #x1234567812345678
      (logand #x1234567812345678 -1))
(test* "logand (+fix & +fix)" #x2244
      (logand #xaa55 #x6666))
(test* "logand (+fix & +big)" #x2244
      (logand #xaa55 #x6666666666))
(test* "logand (+big & +fix)" #x4422
      (logand #xaa55aa55aa #x6666))
(test* "logand (+big & +big)" #x2244224422
      (logand #xaa55aa55aa #x6666666666))
(test* "logand (+big & +big)" #x103454301aaccaa
      (logand #x123456789abcdef #xfedcba987654321fedcba987654321fedcba))
(test* "logand (+big & +big)" #x400000
      (logand #xaa55ea55aa #x55aa55aa55))
(test* "logand (+fix & -fix)" #x8810
      (logand #xaa55 #x-6666))
(test* "logand (+fix & -big)" #x8810
      (logand #xaa55 #x-6666666666))
(test* "logand (+big & -fix)" #xaa55aa118a
      (logand #xaa55aa55aa #x-6666))
(test* "logand (+big & -big)" #x881188118a
      (logand #xaa55aa55aa #x-6666666666))
(test* "logand (+big & -big)" #x20002488010146
      (logand #x123456789abcdef #x-fedcba987654321fedcba987654321fedcba))
(test* "logand (-fix & +fix)" #x4422
      (logand #x-aa55 #x6666))
(test* "logand (-fix & +big)" #x6666664422
      (logand #x-aa55 #x6666666666))
(test* "logand (-big & +fix)" #x2246
      (logand #x-aa55aa55aa #x6666))
(test* "logand (-big & +big)" #x4422442246
      (logand #x-aa55aa55aa #x6666666666))
(test* "logand (-big & +big)" #xfedcba987654321fedcba884200020541010
      (logand #x-123456789abcdef #xfedcba987654321fedcba987654321fedcba))
(test* "logand (-fix & -fix)" #x-ee76
      (logand #x-aa55 #x-6666))
(test* "logand (-fix & -big)" #x-666666ee76
      (logand #x-aa55 #x-6666666666))
(test* "logand (-big & -fix)" #x-aa55aa77ee
      (logand #x-aa55aa55aa #x-6666))
(test* "logand (-big & -big)" #x-ee77ee77ee
      (logand #x-aa55aa55aa #x-6666666666))
(test* "logand (-big & -big)" #x-fedcba987654321fedcba9a76567a9ffde00
      (logand #x-123456789abcdef #x-fedcba987654321fedcba987654321fedcba))

(test* "logior (+fix | 0)" #x123456
      (logior #x123456 0))
(test* "logior (+big | 0)" #x1234567812345678
      (logior #x1234567812345678 0))
(test* "logior (+fix | -1)" -1
      (logior #x123456 -1))
(test* "logior (+big | -1)" -1
      (logior #x1234567812345678 -1))
(test* "logior (+fix | +fix)" #xee77
      (logior #xaa55 #x6666))
(test* "logior (+fix | +big)" #x666666ee77
      (logior #xaa55 #x6666666666))
(test* "logior (+big | +fix)" #xaa55aa77ee
      (logior #xaa55aa55aa #x6666))
(test* "logior (+big | +big)" #xee77ee77ee
      (logior #xaa55aa55aa #x6666666666))
(test* "logior (+big | +big)" #xfedcba987654321fedcba9a76567a9ffddff
      (logior #x123456789abcdef #xfedcba987654321fedcba987654321fedcba))
(test* "logior (+fix | -fix)" #x-4421
      (logior #xaa55 #x-6666))
(test* "logior (+fix | -big)" #x-6666664421
      (logior #xaa55 #x-6666666666))
(test* "logior (+big | -fix)" #x-2246
      (logior #xaa55aa55aa #x-6666))
(test* "logior (+big | -big)" #x-4422442246
      (logior #xaa55aa55aa #x-6666666666))
(test* "logior (+big | -big)" #x-fedcba987654321fedcba884200020541011
      (logior #x123456789abcdef #x-fedcba987654321fedcba987654321fedcba))
(test* "logior (-fix | +fix)" #x-8811
      (logior #x-aa55 #x6666))
(test* "logior (-fix | +big)" #x-8811
      (logior #x-aa55 #x6666666666))
(test* "logior (-big | +fix)" #x-aa55aa118a
      (logior #x-aa55aa55aa #x6666))
(test* "logior (-big | +big)" #x-881188118a
      (logior #x-aa55aa55aa #x6666666666))
(test* "logior (-big | +big)" #x-20002488010145
      (logior #x-123456789abcdef #xfedcba987654321fedcba987654321fedcba))
(test* "logior (-fix | -fix)" #x-2245
      (logior #x-aa55 #x-6666))
(test* "logior (-fix | -big)" #x-2245
      (logior #x-aa55 #x-6666666666))
(test* "logior (-big | -fix)" #x-4422
      (logior #x-aa55aa55aa #x-6666))
(test* "logior (-big | -big)" #x-2244224422
      (logior #x-aa55aa55aa #x-6666666666))
(test* "logior (-big | -big)" #x-103454301aacca9
      (logior #x-123456789abcdef #x-fedcba987654321fedcba987654321fedcba))

(test* "logtest" #t
      (logtest #xfeedbabe #x10000000))
(test* "logtest" #f
      (logtest #xfeedbabe #x01100101))

(let loop ((a 1)   ; 1, 10, 100, ...
           (b 1)   ; 1, 11, 111, ...
           (c 2)   ; 10, 101, 1001, ...
           (n 1))  ; counter
  (when (< n 69)
    (test* (format "logcount (positive, 100...) ~a" n) 1 (logcount a))
    (test* (format "logcount (positive, 111...) ~a" n) n (logcount b))
    (test* (format "logcount (negative, 100...) ~a" n) (- n 1) (logcount (- a)))
    (test* (format "logcount (negative, 100..1) ~a" n) 1 (logcount (- c)))
    (loop (+ b 1) (+ b b 1) (+ b b 3) (+ n 1))))

(test* "logbit?" '(#f #t #t #f #t #f #f)
              (map (lambda (i) (logbit? i #b10110)) '(0 1 2 3 4 5 6)))
(test* "logbit?" '(#f #t #f #t #f #t #t)
              (map (lambda (i) (logbit? i #b-10110)) '(0 1 2 3 4 5 6)))

(test* "copy-bit" #b11010110
      (copy-bit 4 #b11000110 #t))
(test* "copy-bit" #b11000110
      (copy-bit 4 #b11000110 #f))
(test* "copy-bit" #b10000110
      (copy-bit 6 #b11000110 #f))

(test* "bit-field" #b1010
      (bit-field #b1101101010 0 4))
(test* "bit-field" #b10110
      (bit-field #b1101101010 4 9))

(test* "copy-bit-field" #b1101100000
      (copy-bit-field #b1101101010 0 4 0))
(test* "copy-bit-field" #b1101101111
      (copy-bit-field #b1101101010 0 4 -1))
(test* "copy-bit-field" #b1111111111101010
      (copy-bit-field #b1101101010 5 16 -1))

(test* "integer-length" 8
      (integer-length #b10101010))
(test* "integer-length" 4
      (integer-length #b1111))

;;------------------------------------------------------------------
(test-section "inexact arithmetics")

(test* "+. (0)" 0.0 (+.))
(test* "+. (1)" 1.0 (+. 1))
(test* "+. (1big)" 1.0e20 (+. 100000000000000000000))
(test* "+. (1rat)" 1.5 (+. 3/2))
(test* "+. (1cmp)" 1.0+i (+. 1+i))
(test* "+. (2)" 1.0 (+. 0 1))
(test* "+. (2big)" 1.0e20 (+. 1 100000000000000000000))
(test* "+. (2rat)" 1.5 (+. 1 1/2))
(test* "+. (many)" 15.0 (+. 1 2 3 4 5))

(test* "-. (1)" -1.0 (-. 1))
(test* "-. (1big)" -1.0e20 (-. 100000000000000000000))
(test* "-. (1rat)" -1.5 (-. 3/2))
(test* "-. (1cmp)" -1.0-i (-. 1+i))
(test* "-. (2)" -1.0 (-. 0 1))
(test* "-. (2big)" -1.0e20 (-. 1 100000000000000000000))
(test* "-. (2rat)" 0.5 (-. 1 1/2))
(test* "-. (many)" -13.0 (-. 1 2 3 4 5))

(test* "*. (0)" 1.0 (*.))
(test* "*. (1)" 1.0 (*. 1))
(test* "*. (1big)" 1.0e20 (*. 100000000000000000000))
(test* "*. (1rat)" 1.5 (*. 3/2))
(test* "*. (1cmp)" 1.0+i (*. 1+i))
(test* "*. (2)"  0.0 (*. 0 1))
(test* "*. (2big)" 1.0e20 (*. 1 100000000000000000000))
(test* "*. (2rat)" 0.5 (*. 1 1/2))
(test* "*. (many)" 120.0 (*. 1 2 3 4 5))

(test* "/. (1)" 1.0 (/. 1))
(test* "/. (1big)" 1.0e-20 (/. 100000000000000000000))
(test* "/. (1rat)" 0.6666666666666666 (/. 3/2))
(test* "/. (1cmp)" 0.5-0.5i (/. 1+i))
(test* "/. (2)"  0.0 (/. 0 1))
(test* "/. (2big)" 1.0e-20 (/. 1 100000000000000000000))
(test* "/. (2rat)" 2.0 (/. 1 1/2))
(test* "/. (many)" 0.1 (/. 1 2 5))

;;------------------------------------------------------------------
(test-section "ffx optimization")

;; This code is provided by naoya_t to reproduce the FFX bug
;; existed until r6714.   The bug was that the ARGP words of
;; in-stack continuations were not scanned when flonum register
;; bank was cleared.  This code exhibits the case by putting
;; the result of (sqrt 2) as an unfinished argument, then calling
;; inverse-erf which caused flushing flonum regs (see "NG" line).

(use math.const)
(let ()
  (define *epsilon* 1e-12)
  
  ;;
  ;; normal quantile function (probit function)
  ;;
  (define (probit p)
    (define (probit>0 p)
      (* (inverse-erf (- (* p 2) 1)) (sqrt 2))) ;; OK
    (if (< p 0)
      (- 1 (probit>0 (- p)))
      (probit>0 p) ))
  
  (define (probit p)
    (define (probit>0 p)
      (* (sqrt 2) (inverse-erf (- (* p 2) 1)))) ;; NG
    (if (< p 0)
      (- 1 (probit>0 (- p)))
      (probit>0 p) ))
  
  ;;
  ;; inverse error function (erf-1)
  ;;
  (define (inverse-erf z)
    (define (calc-next-ck k c)
      (let loop ((m 0) (sum 0) (ca c) (cz (reverse c)))
        (if (= m k) sum
            (loop (+ m 1)
                  (+ sum (/. (* (car ca) (car cz)) (+ m 1) (+ m m 1)))
                  (cdr ca) (cdr cz)))))
    (define (calc-cks k)
      (let loop ((i 0) (cks '(1)))
        (if (= i k) cks
            (loop (+ i 1) (cons (calc-next-ck (+ i 1) cks) cks)))))
    (define (calc-ck k) (car (calc-cks k)))
    
    (define (inverse-erf>0 z)
      (let1 r (* pi z z 1/4) ; (pi*z^2)/4
        (let loop ((k 0) (cks '(1)) (sum 0) (a 1))
          (let1 delta (* a (/ (car cks) (+ k k 1)))
            (if (< delta (* sum *epsilon*))
              (* 1/2 z (sqrt pi) sum)
              (loop (+ k 1)
                    (cons (calc-next-ck (+ k 1) cks) cks)
                    (+ sum delta)
                    (* a r)))))))
    
    (cond [(< z 0) (- (inverse-erf>0 (- z)))]
          [(= z 0) 0]
          [else (inverse-erf>0 z)]) )

  (define ~= (lambda (x y) (< (abs (- x y)) 1e-7)))
  ;;
  ;; TEST
  ;;
  (test-section "ffx optimization")
  (test* "probit(0.025)" -1.959964 (probit 0.025) ~=)
  (test* "probit(0.975)" 1.959964 (probit 0.975) ~=)
  )

;;------------------------------------------------------------------
(test-section "arithmetic operation overload")

;; NB: these tests requires the object system working.

;; These code are only for tests, and do not suggest the real use of
;; arithmetic operation override.  For practical use, it is important
;; to define those operations consistently.  Note that Gauche's compiler
;; may reorder or change operations based on the assumption of the
;; normal definition of those arithmetic operations.

(define-method object-+ ((a <string>) b) #`",|a|+,|b|")
(define-method object-+ (a (b <string>)) #`",|a|+,|b|")
(define-method object-- ((a <string>) b) #`",|a|-,|b|")
(define-method object-- (a (b <string>)) #`",|a|-,|b|")
(define-method object-* ((a <string>) b) #`",|a|*,|b|")
(define-method object-* (a (b <string>)) #`",|a|*,|b|")
(define-method object-/ ((a <string>) b) #`",|a|/,|b|")
(define-method object-/ (a (b <string>)) #`",|a|/,|b|")

(define-method object-- ((a <string>)) #`"-,|a|")
(define-method object-/ ((a <string>)) #`"/,|a|")

(test* "object-+" "a+b" (+ "a" "b"))
(test* "object-+" "a+b" (+ "a" 'b))
(test* "object-+" "a+b" (+ 'a "b"))
(test* "object-+" "3+a" (+ 3 "a"))
;; NB: this becomes "3+a" instead of "a+3", because of compiler optimization.
;; DO NOT COUNT ON THIS BEHAVIOR IN THE REAL CODE.   Might be changed in
;; the future release.
(test* "object-+" "3+a" (+ "a" 3))

(test* "object--" "a-b" (- "a" "b"))
(test* "object--" "a-b" (- "a" 'b))
(test* "object--" "a-b" (- 'a "b"))
(test* "object--" "3-a" (- 3 "a"))
;; NB: this becomes "-3+a" instead of "a-3", because of compiler optimization
;; DO NOT COUNT ON THIS BEHAVIOR IN THE REAL CODE.   Might be changed in
;; the future release.
(test* "object--" "-3+a" (- "a" 3))

(test* "object--" "-a"  (- "a"))

(test* "object-*" "a*b" (* "a" "b"))
(test* "object-*" "a*b" (* "a" 'b))
(test* "object-*" "a*b" (* 'a "b"))
(test* "object-*" "3*a" (* 3 "a"))
(test* "object-*" "a*3" (* "a" 3))

(test* "object-/" "a/b" (/ "a" "b"))
(test* "object-/" "a/b" (/ "a" 'b))
(test* "object-/" "a/b" (/ 'a "b"))
(test* "object-/" "3/a" (/ 3 "a"))
(test* "object-/" "a/3" (/ "a" 3))

(test* "object-/" "/a"  (/ "a"))

(test-end)
