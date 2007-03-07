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
(test* "rational reader" '(#i1/0 #f)
       (rational-test (string->number "3/0")))
(test* "rational reader" '(#i-1/0 #f)
       (rational-test (string->number "-3/0")))
(test* "rational reader" #f
       (rational-test (string->number "3/3/4")))
(test* "rational reader" #f
       (rational-test (string->number "1/2.")))
(test* "rational reader" #f
       (rational-test (string->number "1.3/2")))

(test* "rational reader" *test-error*
       (rational-test (read-from-string "#e3/0")))
(test* "rational reader" *test-error*
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

(test* "exponent out-of-range 1" '(#i1/0 #t) (flonum-test '1e309))
(test* "exponent out-of-range 2" '(#i1/0 #t) (flonum-test '1e10000))
(test* "exponent out-of-range 3" '(#i1/0 #t) (flonum-test '1e1000000000000000000000000000000000000000000000000000000000000000))
(test* "exponent out-of-range 4" '(#i-1/0 #t) (flonum-test '-1e309))
(test* "exponent out-of-range 5" '(#i-1/0 #t) (flonum-test '-1e10000))
(test* "exponent out-of-range 6" '(#i-1/0 #t) (flonum-test '-1e1000000000000000000000000000000000000000000000000000000000000000))
(test* "exponent out-of-range 7" '(0.0 #t) (flonum-test '1e-324))
(test* "exponent out-of-range 8" '(0.0 #t) (flonum-test '1e-1000))
(test* "exponent out-of-range 9" '(0.0 #t) (flonum-test '1e-1000000000000000000000000000000000000000000000000000000000000000000))

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

(test* "exact fractonal number" (expt 10 296)
       (string->number "#e0.0001e300"))
(test* "exact fractonal number" (- (expt 10 296))
       (string->number "#e-0.0001e300"))

(test* "exact fractonal number" *test-error*
       (read-from-stirng "#e1e330"))
(test* "exact fractonal number" *test-error*
       (read-from-stirng "#e1e-330"))


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

(test* "complex reader" '(0.5 0.5) (decompose-complex 1/2+1/2i))
(test* "complex reader" '(0.0 0.5) (decompose-complex 0+1/2i))
(test* "complex reader" '(0.0 -0.5) (decompose-complex -1/2i))
(test* "complex reader" 1/2 (decompose-complex 1/2-0/2i))
(test* "complex reader" '(0.5 -1/0) (decompose-complex (string->number "1/2-1/0i")))

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

(test* "eqv?" #t (eqv? 20 20))
(test* "eqv?" #t (eqv? 20.0 20.00000))
(test* "eqv?" #f (eqv? 4/5 0.8))
(test* "eqv?" #t (eqv? (exact->inexact 4/5) 0.8))
;(test* "eqv?" #f (eqv? 4/5 (inexact->exact 0.8)))
(test* "eqv?" #t (eqv? 20 (inexact->exact 20.0)))
(test* "eqv?" #f (eqv? 20 20.0))

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

(test* "logcount" 4
      (logcount #b10101010))
(test* "logcount" 13
      (logcount #b00010010001101000101011001111000))
(test* "logcount" 4
      (logcount #b-10101010))

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
