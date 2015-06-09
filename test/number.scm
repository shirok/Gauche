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

(test* "Gauche extended format" #x123456789
       (string->number "#x1_2345_6789"))
(test* "Gauche extended format" #x-123456789
       (string->number "#x-123_456_789"))
(test* "Gauche extended format" #f
       (string->number "123_456_789"))

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
(test* "rational reader" #f
       (rational-test (string->number "3/0")))
(test* "rational reader" #f
       (rational-test (string->number "-3/0")))
(test* "rational reader" #f
       (rational-test (string->number "0/0")))
(test* "rational reader" '(+inf.0 #f)
       (rational-test (string->number "#i3/0")))
(test* "rational reader" '(-inf.0 #f)
       (rational-test (string->number "#i-3/0")))
(test* "rational reader" '(+nan.0 #f)
       (rational-test (string->number "#i0/0")))
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

(define (flonum-test s)
  (let1 v (read-from-string s)
    (if (number? v) (list v (inexact? v)) v)))

(test* "flonum reader" '(3.14 #t)  (flonum-test "3.14"))
(test* "flonum reader" '(0.14 #t)  (flonum-test "0.14"))
(test* "flonum reader" '(0.14 #t)  (flonum-test ".14"))
(test* "flonum reader" '(3.0  #t)  (flonum-test "3."))
(test* "flonum reader" '(-3.14 #t)  (flonum-test "-3.14"))
(test* "flonum reader" '(-0.14 #t)  (flonum-test "-0.14"))
(test* "flonum reader" '(-0.14 #t)  (flonum-test "-.14"))
(test* "flonum reader" '(-3.0  #t)  (flonum-test "-3."))
(test* "flonum reader" '(3.14 #t)  (flonum-test "+3.14"))
(test* "flonum reader" '(0.14 #t)  (flonum-test "+0.14"))
(test* "flonum reader" '(0.14 #t)  (flonum-test "+.14"))
(test* "flonum reader" '(3.0  #t)  (flonum-test "+3."))
(test* "flonum reader" '(0.0  #t)  (flonum-test ".0"))
(test* "flonum reader" '(0.0  #t)  (flonum-test "0."))
(test* "flonum reader" #f (string->number "."))
(test* "flonum reader" #f (string->number "-."))
(test* "flonum reader" #f (string->number "+."))

(test* "flonum reader (exp)" '(314.0 #t) (flonum-test "3.14e2"))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test ".314e3"))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test "314e0"))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test "314e-0"))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test "3140000e-4"))
(test* "flonum reader (exp)" '(-314.0 #t) (flonum-test "-3.14e2"))
(test* "flonum reader (exp)" '(-314.0 #t) (flonum-test "-.314e3"))
(test* "flonum reader (exp)" '(-314.0 #t) (flonum-test "-314e0"))
(test* "flonum reader (exp)" '(-314.0 #t) (flonum-test "-314.e-0"))
(test* "flonum reader (exp)" '(-314.0 #t) (flonum-test "-3140000e-4"))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test "+3.14e2"))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test "+.314e3"))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test "+314.e0"))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test "+314e-0"))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test "+3140000.000e-4"))

(test* "flonum reader (exp)" '(314.0 #t) (flonum-test ".314E3"))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test ".314s3"))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test ".314S3"))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test ".314l3"))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test ".314L3"))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test ".314f3"))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test ".314F3"))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test ".314d3"))
(test* "flonum reader (exp)" '(314.0 #t) (flonum-test ".314D3"))

(test* "flonum reader (minimum denormalized number 5.0e-324)" #t
       (let1 x (expt 2.0 -1074)
         (= x (string->number (number->string x)))))
(test* "flonum reader (minimum denormalized number -5.0e-324)" #t
       (let1 x (- (expt 2.0 -1074))
         (= x (string->number (number->string x)))))

(test* "flonum reader lots of digits" 1.0
       (read-from-string
        "1.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001"))
(test* "flonum reader lots of digits" 1.0e308
       (read-from-string
        "1.000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000001e308"))

;; This hanged in 0.9.1.  See Jens Thiele's message in gauche-devel
;; in Feb. 2011.
(test* "flonum reader (minimum normalized number)" #t
       (= (expt 2.0 (- 52 1074))
          (string->number "2.2250738585072012e-308")))
       
;; We used to allow 1#1 to be read as a symbol.  As of 0.9.4, it is an error.
(test* "padding" '(10.0 #t) (flonum-test "1#"))
(test* "padding" '(10.0 #t) (flonum-test "1#."))
(test* "padding" '(10.0 #t) (flonum-test "1#.#"))
(test* "padding" '(100.0 #t) (flonum-test "10#.#"))
(test* "padding" '(100.0 #t) (flonum-test "1##.#"))
(test* "padding" '(100.0 #t) (flonum-test "100.0#"))
(test* "padding" '(1.0 #t) (flonum-test "1.#"))

(test* "padding" (test-error) (flonum-test "1#1"))
(test* "padding" (test-error) (flonum-test "1##1"))
(test* "padding" (test-error) (flonum-test "1#.1"))
(test* "padding" (test-error) (flonum-test "1.#1"))

(test* "padding" (test-error) (flonum-test ".#"))
(test* "padding" '(0.0 #t) (flonum-test "0.#"))
(test* "padding" '(0.0 #t) (flonum-test ".0#"))
(test* "padding" '(0.0 #t) (flonum-test "0#"))
(test* "padding" '(0.0 #t) (flonum-test "0#.#"))
(test* "padding" (test-error) (flonum-test "0#.0"))

(test* "padding" '(1000.0 #t) (flonum-test "1#e2"))
(test* "padding" '(1000.0 #t) (flonum-test "1##e1"))
(test* "padding" '(1000.0 #t) (flonum-test "1#.##e2"))
(test* "padding" '(0.0 #t) (flonum-test "0.#e2"))
(test* "padding" '(0.0 #t) (flonum-test ".0#e2"))
(test* "padding" (test-error) (flonum-test ".##e2"))

(test* "padding (exactness)" '(100 #f) (flonum-test "#e1##"))
(test* "padding (exactness)" '(120 #f) (flonum-test "#e12#"))
(test* "padding (exactness)" '(120 #f) (flonum-test "#e12#.#"))
(test* "padding (exactness)" '(100.0 #t) (flonum-test "#i1##"))
(test* "padding (exactness)" '(120.0 #t) (flonum-test "#i12#"))
(test* "padding (exactness)" '(120.0 #t) (flonum-test "#i12#.#"))

(test* "exponent out-of-range 1" '(+inf.0 #t) (flonum-test "1e309"))
(test* "exponent out-of-range 2" '(+inf.0 #t) (flonum-test "1e10000"))
(test* "exponent out-of-range 3" '(+inf.0 #t) (flonum-test "1e1000000000000000000000000000000000000000000000000000000000000000"))
(test* "exponent out-of-range 4" '(-inf.0 #t) (flonum-test "-1e309"))
(test* "exponent out-of-range 5" '(-inf.0 #t) (flonum-test "-1e10000"))
(test* "exponent out-of-range 6" '(-inf.0 #t) (flonum-test "-1e1000000000000000000000000000000000000000000000000000000000000000"))
(test* "exponent out-of-range 7" '(0.0 #t) (flonum-test "1e-324"))
(test* "exponent out-of-range 8" '(0.0 #t) (flonum-test "1e-1000"))
(test* "exponent out-of-range 9" '(0.0 #t) (flonum-test "1e-1000000000000000000000000000000000000000000000000000000000000000000"))

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
(test* "complex reader" #f (decompose-complex (string->number "1/2-1/0i")))

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

(let ()
  (define (exact-expt-tester x y)
    (let1 x^y (expt x y)
      (test* "exact expt (non-integral power)" x (expt x^y (/ y)))
      (test* "exact expt (non-integral power)" (* x x) (expt x^y (/ 2 y)))
      (test* "exact expt (non-integral power, inexact fallback)"
             (expt (+ x^y 1.0) (/ y)) (expt (+ x^y 1) (/ y)))
      ))

  (exact-expt-tester 3 7)
  (exact-expt-tester 5 3)
  (exact-expt-tester 13 17)
  (exact-expt-tester 101 103)
  (exact-expt-tester 11/13 23)
  )
             
;; expt-mod
(define (test-expt-mod base mod)
  ;; NB: we haven't tested iota.
  (let1 es (do ([e 1 (+ e 3)] [r '() (cons e r)])
               [(> e 100) (reverse r)])
    (test* (format "expt-mod(~a, e, ~a)" base mod)
           (map (^e (modulo (expt base e) mod)) es)
           (map (^e (expt-mod base e mod)) es))))

(test-expt-mod 35 41)
(test-expt-mod 15841875 319999357)
(test-expt-mod 915151975010144550184898988758 1775619891701751758948583493979350)
(test-expt-mod -324574950475018750175057087501 100184859387038471089598349534598)
(test-expt-mod 324574950475018750175057087501 -100184859387038471089598349534598)

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
   (,(%expt 2 52) . ,(%expt 2.0 52))
   (,(%expt 2 53) . ,(%expt 2.0 53))
   (,(%expt 2 54) . ,(%expt 2.0 54))

   (1/2 . 0.5)
   (3/4 . 0.75)

   (1/3 . 0.3333333333333333)
   ))

;; Boundary conditions for inexact->exact
;; Since inexact->exact returns a simplest rational within the flonum precision,
;; the roundtrip of exact -> inexact -> exact isn't necessary kept, but
;; inexact -> exact -> inexact is.
(let ([one-plus-delta 1.0000000000000002]
      [one-minus-half-delta 0.9999999999999999])
  (define (t what orig expect)
    (test* (format "inexact->exact->inexact roundtrip ~s" what)
           expect (exact->inexact (inexact->exact orig))))
  (t "1+d" one-plus-delta one-plus-delta)
  (t "1-d" one-minus-half-delta one-minus-half-delta))

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
       1.0e-310 (exact->inexact (/ (expt 10 20) (expt 10 330))))
(test* "expt (ratnum with large denom and numer) with inexact conversion 5"
       1.0e308 (exact->inexact (/ (expt 10 328) (expt 10 20))))
(test* "expt (ratnum with large denom and numer) with inexact conversion 6"
       +inf.0 (exact->inexact (/ (expt 10 329) (expt 10 20))))
(test* "expt (ratnum with large denom and numer) with inexact conversion 7"
       -inf.0 (exact->inexact (/ (expt -10 329) (expt 10 20))))
(test* "expt (ratnum with large denom and numer) with inexact conversion 8"
       -inf.0 (exact->inexact (/ (expt 10 329) (- (expt 10 20)))))
(test* "expt (ratnum with large denom and numer) with inexact conversion 9"
       +inf.0 (exact->inexact (/ (expt -10 329) (- (expt 10 20)))))
;; denormalized range
(let ()
  (define data '(5.0e-324   ; minimum positive denormalized flonum
                 -5.0e-324
                 1.0e-323
                 1.5e-323
                 2.0e-323
                 1.0e-322
                 1.04e-322
                 1.1e-322))
  (dolist [d data]
    (test* #"inexact conversion in subnormal range ~d" d
           (inexact (exact d)))))
;; close to inifinity (but not quite)
(test* "ratnum -> flonum, close to infinity 1" 1.0e308
       (inexact (/ (+ (expt 10 309) 1) 10)))
(test* "ratnum -> flonum, close to infinity 2" 1.0e308
       (inexact (/ (+ (expt 10 310) 1) 100)))

;; this exhibits a bug fixed on 9/12/2013.
(test* "real->rational" '(1/3 2/3)
       (list (real->rational 3/10 1/10 1/10)
             (real->rational 24/35 4/35 4/35)))

(test* "rationalize (edge cases)" '(#t #t #t +inf.0 -inf.0 0.0)
       (list (nan? (rationalize +nan.0 0))
             (nan? (rationalize 0 +nan.0))
             (nan? (rationalize +inf.0 +inf.0))
             (rationalize +inf.0 0)
             (rationalize -inf.0 0)
             (rationalize 1234 +inf.0)))
(test* "rationalize (integers)" '(1 2 0 -1 -2 0)
       (list (rationalize 1 1/2)
             (rationalize 5 3)
             (rationalize 1 3)
             (rationalize -1 1/2)
             (rationalize -5 3)
             (rationalize -1 3)))
(test* "rationalize (exactness)" '(#t #f #f #f)
       (list (exact? (rationalize 1/2 1/3))
             (exact? (rationalize 0.5 1/3))
             (exact? (rationalize 1/2 0.1))
             (exact? (rationalize 0.5 0.1))))

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
(test* "integer?" #f (integer? +inf.0))
(test* "integer?" #f (integer? -inf.0))
(test* "integer?" #f (integer? +nan.0))

(test* "rational?" #t (rational? 0))
(test* "rational?" #t (rational? 85736847562938475634534245))
(test* "rational?" #t (rational? 1/2))
(test* "rational?" #t (rational? 85736.534245))
(test* "rational?" #t (rational? 3.14))
(test* "rational?" #f (rational? 3+4i))
(test* "rational?" #t (rational? 3+0i))
(test* "rational?" #f (rational? #f))
(test* "rational?" #f (rational? +inf.0))
(test* "rational?" #f (rational? -inf.0))
(test* "rational?" #f (rational? +nan.0))

(test* "real?" #t (real? 0))
(test* "real?" #t (real? 85736847562938475634534245))
(test* "real?" #t (real? 857368.4756293847))
(test* "real?" #t (real? 3+0i))
(test* "real?" #f (real? 3+4i))
(test* "real?" #f (real? +4.3i))
(test* "real?" #f (real? '()))
(test* "real?" #t (real? +inf.0))
(test* "real?" #t (real? -inf.0))
(test* "real?" #t (real? +nan.0))

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
                  (test* #"~msg ~(if rev 'rev \"\") ~opname(~comb)" result
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

;; This tests variable number of arguments.  The current stub code accepts
;; up to 4 args in stack and the rest by list, so we want to test the
;; boundary case.
(define (numcmp-multiarg-test lis eq lt le gt ge)
  (test* #"=~lis" eq (apply = lis))
  (test* #"<~lis" lt (apply < lis))
  (test* #"<=~lis" le (apply <= lis))
  (test* #">~lis" gt (apply > lis))
  (test* #">=~lis" ge (apply >= lis)))

;;                                      =  <  <= >  >=
(numcmp-multiarg-test '(1 2 3 4)        #f #t #t #f #f)
(numcmp-multiarg-test '(1 2 3 3)        #f #f #t #f #f)
(numcmp-multiarg-test '(1 2 3 2)        #f #f #f #f #f)
(numcmp-multiarg-test '(1 2 3 4 5)      #f #t #t #f #f)
(numcmp-multiarg-test '(1 2 3 4 4)      #f #f #t #f #f)
(numcmp-multiarg-test '(1 2 3 4 3)      #f #f #f #f #f)
(numcmp-multiarg-test '(1 2 3 4 5 6)    #f #t #t #f #f)
(numcmp-multiarg-test '(1 2 3 4 5 5)    #f #f #t #f #f)
(numcmp-multiarg-test '(1 2 3 4 5 4)    #f #f #f #f #f)
(numcmp-multiarg-test '(4 3 2 1)        #f #f #f #t #t)
(numcmp-multiarg-test '(4 3 2 2)        #f #f #f #f #t)
(numcmp-multiarg-test '(4 3 2 3)        #f #f #f #f #f)
(numcmp-multiarg-test '(5 4 3 2 1)      #f #f #f #t #t)
(numcmp-multiarg-test '(5 4 3 2 2)      #f #f #f #f #t)
(numcmp-multiarg-test '(5 4 3 2 3)      #f #f #f #f #f)
(numcmp-multiarg-test '(6 5 4 3 2 1)    #f #f #f #t #t)
(numcmp-multiarg-test '(6 5 4 3 2 2)    #f #f #f #f #t)
(numcmp-multiarg-test '(6 5 4 3 2 3)    #f #f #f #f #f)
(numcmp-multiarg-test '(1 1 1 1 1)      #t #f #t #f #t)
(numcmp-multiarg-test '(1 1 1 1 2)      #f #f #t #f #f)
(numcmp-multiarg-test '(1 1 1 1 0)      #f #f #f #f #t)
(numcmp-multiarg-test '(1 1 1 1 1 1)    #t #f #t #f #t)
(numcmp-multiarg-test '(1 1 1 1 1 2)    #f #f #t #f #f)
(numcmp-multiarg-test '(1 1 1 1 1 0)    #f #f #f #f #t)

;; This is from the bug report from Bill Schottsteadt.  Before 0.8.10
;; this yielded #t because of the precision loss in fixnum vs ratnum
;; comparison.

(test* "fixnum/ratnum comparison" #f
       (= -98781233389595723930250385525631360344437602649022271391716773162526352115087074898920261954897888235939429993829738630297052776667061779065100945771127020439712527398509771853491319737304616607041615012797134365574007368603232768089410097730646360760856052946465578073788924743642391638455649511108051053789425902013657106523269224045822294981391380222050223141347787674321888089837786284947870569165079491411110074602544203383038299901291952931113248943344436935596614205784436844912243069019367149526328612664067719765890897558075277707055756274228634652905751880612235340874976952880431555921814590049070979276358637989837532124647692152520447680373275200239544449293834424643702763974403094033892112967196087310232853165951285609426599617479356206218697586025251765476179158153123631158173662488102357611674821528467825910806391548770908013608889792001203039243914696463472490444573930050190716726220002151679336252008777326482398042427845860796285369622627679324605214987983884122808994422164327311297556122943400093231935477754959547620500784989043704825777186301417894825200797719289692636286337716705491307686644214213732116277102140558505945554566856673724837541141206267647285222293953181717113434757149921850120377706206012113994795124049471433490016083401216757825264766474891405185591236321448744678896448941259668731597494947127423662646933419809756274038044752395708014998820826196523041220918922611359697502638594907608648168849193813197790291360087857093790119162389573209640804111261616771827989939551840471235079945175327536638365874717775169210186608268924244639016270610098894971732892267642318266405837012482726627199088381027028630711279130575230815976484191675172279903609489448225149181063260231957171204855841611039996959582465138269247794842445177715476581512709861409446684911276158067098438009067149531119008707418601627426255891/2063950098473886055933596136103014753954685977787179797499441692283103642150668140884348149132839387663291870239435604463778573480782766958396423322880804442523056530013282118705429274303746421980903580754656364533869319744640130831962767797772323836293079599182477171562218297208495122660799328579852852969560730744211066545295945803939271680397511478811389399527913043145952054883289558914237172406636283114284363301999238526952309439259354223729114988806937903509692118585280437646676248013406270664905997291670857985754768850507766359973207600149782819306010561088246502918148146264806947375101624011387317921439210509902170092173796154464078297852707797984007992277904626058467143192149921546030028316990855470478894515952884526783686210401408859364838148201339959570732480920969000913791571631154267939054105878236201498477027265774680071188764947522112650857013491135901945605796776829525789886482760578142306057177990048751864852763036720112071475134369179525117161001517868525821398753039187062869247457336940152614866298628205010037695017885878296140891234142925514925051385440766473260338168038302226808098439763889250948602137806546736025439919604390464712793474019469457135856879584745805794574609707742445431851999335443724488636749987837445626810087003490329257105472274738811579817454656532496370562155449815456374456838912258383282154811001588175608617475540639254689723629881619252699580383612847920348111900440075645703960104081690968807839189109040568288972353424306876947127635585164905071821419089229871978994388197349499565628906992171901547121903117815637249359328193980583892566359962066242217169190169986105579733710057404319381685578470983838597020624234209884597110721892707818651210378187525863009879314177842634871978427592746452643603586344401223449546482306838947819060455178762434166799996220143825677025686435609179225302671777326568324855229172912876656233006785717920665743720753617646617017219230313226844735567400507490772935145894670445831971526014183234960075574401616682479457962912905141754252265169682318523572680657053374002911007741991220001444440319448034755483178790032581428679303588017268970 0))

(let ()
  (define (test-minmax mi ma data)
    (test* (format "min, max ~s" data)
           (list mi ma)
           (list (apply min data) (apply max data))))
  (test-minmax 0 10 '(3 10 2 0 5))
  (test-minmax -1/3 99/5 '(2 6 99/5 0 -1/6 -1/3))
  (test-minmax -10.0 10.0 '(3 10 2.0 -10 5))
  (test-minmax -inf.0 +inf.0 '(5 -inf.0 2 +inf.0 1))
  (test-minmax +nan.0 +nan.0 '(5 -inf.0 +nan.0 +inf.0 1))
  (test-minmax +nan.0 +nan.0 '(+nan.0 -inf.0 3 +inf.0 1))
  )

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
       (- (ash 1 (- (fixnum-width) 1)) 1))
(test* "least fixnum & width" (least-fixnum)
       (- (ash 1 (- (fixnum-width) 1))))

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

;; avoid inlining
(define (divide . args)
  (apply / args))

(define (divide. . args)
  (apply /. args))

(test* "division by zero" (test-error) (divide 0))
(test* "division by zero" (test-error) (divide 0 0))
(test* "division by zero" (test-error) (divide 3 0))
(test* "division by zero" (test-error) (divide 1/2 0))
(test* "division by zero" +inf.0 (divide 0.0))
(test* "division by zero" #t (nan? (divide 0.0 0)))
(test* "division by zero" #t (nan? (divide 0 0.0)))
(test* "division by zero" #t (nan? (divide 0.0 0.0)))
(test* "division by zero" +inf.0 (divide 0.5 0))

(test* "division by zero" +inf.0 (divide. 0))
(test* "division by zero" #t (nan? (divide. 0 0)))
(test* "division by zero" +inf.0 (divide. 3 0))
(test* "division by zero" +inf.0 (divide. 1/2 0))
(test* "division by zero" #t (nan? (divide. 0.0 0)))
(test* "division by zero" #t (nan? (divide. 0 0.0)))
(test* "division by zero" #t (nan? (divide. 0.0 0.0)))
(test* "division by zero" +inf.0 (divide. 0.5 0))

(test* "division by zero" +inf.0+inf.0i (/ 1+2i 0.0))
(test* "division by zero" +inf.0-inf.0i (/ 1-2i 0.0))
(test* "division by zero" -inf.0+inf.0i (/ -1+2i 0.0))
(test* "division by zero" -inf.0-inf.0i (/ -1-2i 0.0))

(test* "division by zero" #t
       (let ((r (/ 0+1i 0)))
         (and (nan? (real-part r))
              (= (imag-part r) +inf.0))))

(test* "division by zero" #t
       (let ((r (/ 0+1i 0.0)))
         (and (nan? (real-part r))
              (= (imag-part r) +inf.0))))

;; See if we don't fold exact divide-by-zero case.  If compile blindly
;; fold constant division, the following causes compile-time error
;; rather than runtime error.
(let ()
  (define (recip x) (or x (/ 0)))
  (define (two x) (or x (/ 2 0)))
  (define (three x) (or x (/ 2 4 0)))
  (define (four x) (or x (/ 2 0 4 3)))

  (define (tests exp arg)
    (test* "div-by-zero constant folding 1" exp (recip arg))
    (test* "div-by-zero constant folding 2" exp (two arg))
    (test* "div-by-zero constant folding 3" exp (three arg))
    (test* "div-by-zero constant folding 4" exp (four arg)))

  (tests 10 10)
  (tests (test-error) #f)
  )

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
(test-section "modulo and remainder")

;; invariance:
;; suppose (modulo x y) = m
;;         (remainder x y) = r
;;         (quotient x y) = q
;; then
;;         (sign r) = (sign x)
;;         (sign m) = (sign y)
;;         (+ (* q y) r) = x
;;         (+ (* q y) m) = x       if (sign (* x y)) >= 0,
;;                         (+ x y) if (sign (* x y)) < 0
;;         (exact? m) = (and (exact? x) (exact? y))
;;         (exact? r) = (and (exact? x) (exact? y))

(define (mr-tester msg x y)
  (define (sign a) (cond [(< a 0) -1] [(= a 0) 0] [else 1]))
  (define (mr-tester-1 x y)
    ;; We don't use quotient&remainder, for we want to test remainder-only path.
    (let1 adjust-exactness (if (and (exact? x) (exact? y)) exact inexact)
      (test* (format "modulo and remainder (~a) ~s ~s" msg x y)
             `(:rem (,(sign x)
                     ,(adjust-exactness x)
                     ,(and (exact? x) (exact? y)))
               :mod (,(sign y)
                     ,(adjust-exactness (if (>= (sign (* x y)) 0) x (+ x y)))
                     ,(and (exact? x) (exact? y))))
             (let ([q (quotient x y)]
                   [m (modulo x y)]
                   [r (remainder x y)]
                   [e? (and (exact? x) (exact? y))])
               `(:rem (,(sign r) ,(+ (* q y) r) ,(exact? r))
                      :mod (,(sign m) ,(+ (* q y) m) ,(exact? m)))))))
  (mr-tester-1 x y)
  (mr-tester-1 (- x) y)
  (mr-tester-1 x (- y))
  (mr-tester-1 (- x) (- y)))

(mr-tester "fix op fix -> fix" 13 4)
(mr-tester "fix op fix -> fix" 1234 87935)
(mr-tester "fix op big -> fix" 12345 3735928559) ;32bit
(mr-tester "fix op big -> fix" 8478574387345 #x7fffffffffffffff) ;64bit

;; These go through Scm_BignumRemSI
(mr-tester "big op fix -> fix" #x7f245637 41943) ;32bit
(mr-tester "big op fix -> fix" #x7f787486ff73cacb 41943) ;64bit
(mr-tester "big2 op big1 -> big1" #x9aa9bbcb #x50053343) ; 32bit
(mr-tester "big2 op big1 -> big1" #x9aa9bbcb10013303 #x50053343cafebabe) ; 64bit

;; These go through BignumDivRem
(mr-tester "big op big -> big"
           #x78ab76d8aa7787a78963556174babdccade44e54e543232ab
           #xabcbdbdbcbdabefbebfbebbbababba)

;; this tests loop in estimation phase (32bit)
(mr-tester "big[3] rem big[2] -> big[1]" #x10000000000000000 #x10000ffff)
;; this tests "add back" code (32bit)
(mr-tester "big[3] rem big[2] -> big[2]" #x7800000000000000 #x80008889ffff)

;; Inexact
(mr-tester "exact rem inexact -> inexact" 13 4.0)
(mr-tester "inexact rem exact -> inexact" 13.0 4)
(mr-tester "inexact rem inexact -> inexact" 13.0 4.0)
(mr-tester "exact rem inexact -> inexact" 3735928559 27353.0)
(mr-tester "inexact rem exact -> inexact" 3735928559.0 27353)
(mr-tester "inexact rem inexact -> inexact" 3735928559.0 27353.0)

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
(test-section "quotient&remainder")

(let ()
  (define (check x y)
    (test* (format "quotient&remainder ~s ~s" x y)
           (list (quotient x y) (remainder x y))
           (receive (q r) (quotient&remainder x y) (list q r))))
  (define (do-quadrants p)
    (lambda (x y) (p x y) (p (- x) y) (p x (- y)) (p (- x) (- y))))
  (define do-exactness
    (let1 p (do-quadrants check)
      (lambda (x y) (p x y) (p (inexact x) y) (p x (inexact y)))))

  (do-exactness 3 2)
  (do-exactness 7 3)
  (do-exactness 7 9)
  )

;;------------------------------------------------------------------
(test-section "div and mod")

(let ()
  (define (do-quadrants proc)
    (lambda (x y =)
      (proc x y =)
      (proc (- x) y =)
      (proc x (- y) =)
      (proc (- x) (- y) =)))

  (define (test-div x y =)
    (test* (format "~a div ~a" x y) '(#t #t)
           (receive (d m) (div-and-mod x y)
             (let1 z (+ (* d y) m)
               (list (or (= x z) z)
                     (or (and (<= 0 m) (< m (abs y))) m))))))

  (define (test-div0 x y =)
    (test* (format "~a div0 ~a" x y) '(#t #t)
           (receive (d m) (div0-and-mod0 x y)
             (let1 z (+ (* d y) m)
               (list (or (= x z) z)
                     (or (and (<= (- (abs y)) (* m 2))
                              (< (* m 2) (abs y)))
                         m))))))

  ((do-quadrants test-div) 3 2 =)
  ((do-quadrants test-div) 3.0 2 (lambda (a b) (nearly=? 1e-10 a b)))
  ((do-quadrants test-div) 123 10 =)
  ((do-quadrants test-div) 123.0 10.0 (lambda (a b) (nearly=? 1e-10 a b)))
  ((do-quadrants test-div) 123/7 10/7 =)
  ((do-quadrants test-div) 123/7 5 =)
  ((do-quadrants test-div) 123 5/7 =)
  ((do-quadrants test-div) 130.75 10.5 =)

  ((do-quadrants test-div0) 123 10 =)
  ((do-quadrants test-div0) 129 10 =)
  ((do-quadrants test-div0) 123.0 10.0 (lambda (a b) (nearly=? 1e-10 a b)))
  ((do-quadrants test-div0) 129.0 10.0 (lambda (a b) (nearly=? 1e-10 a b)))
  ((do-quadrants test-div0) 123/7 10/7 =)
  ((do-quadrants test-div0) 129/7 10/7 =)
  ((do-quadrants test-div0) 121/7 5 =)
  ((do-quadrants test-div0) 124/7 5 =)
  ((do-quadrants test-div0) 121 5/7 =)
  ((do-quadrants test-div0) 124 5/7 =)
  ((do-quadrants test-div0) 130.75 10.5 =)
  ((do-quadrants test-div0) 129.75 10.5 =)
  )


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

;; covers
(define bitwise-tester-x 0)
(define bitwise-tester-y 0)

(define-macro (ash-tester msg expect x y)
  `(begin
     (set! bitwise-tester-x ,x)
     (set! bitwise-tester-y ,y)
     (test* ,(format "ash (~a) compile-time constant, inlined, generic1, generic2" msg)
            (list ,expect ,expect ,expect ,expect)
            (list (ash ,x ,y)
                  (ash bitwise-tester-x ,y)
                  (ash ,x bitwise-tester-y)
                  (ash bitwise-tester-x bitwise-tester-y)))))

(ash-tester "fixnum" #x408000 #x81 15)
(ash-tester "fixnum" #x81 #x408000 -15)
(ash-tester "fixnum" #x01 #x408000 -22)
(ash-tester "fixnum" 0 #x408000 -23)
(ash-tester "fixnum" 0 #x408000 -24)
(ash-tester "fixnum" 0 #x408000 -100)
(ash-tester "fixnum" #x81 #x81 0)
(ash-tester "neg. fixnum" #x-408000 #x-81 15)
(ash-tester "neg. fixnum" #x-81 #x-408000 -15)
(ash-tester "fixnum" -2 #x-408000 -22)
(ash-tester "fixnum" -1 #x-408000 -23)
(ash-tester "fixnum" -1 #x-408000 -24)
(ash-tester "fixnum" -1 #x-408000 -100)
(ash-tester "fixnum" #x-408000 #x-408000 0)

(ash-tester "fixnum->bignum" #x81000000 #x81 24)
(ash-tester "fixnum->bignum" #x4080000000 #x81 31)
(ash-tester "fixnum->bignum" #x8100000000 #x81 32)
(ash-tester "fixnum->bignum" #x8100000000000000 #x81 56)
(ash-tester "fixnum->bignum" #x408000000000000000 #x81 63)
(ash-tester "fixnum->bignum" #x810000000000000000 #x81 64)
(ash-tester "neg.fixnum->bignum" #x-81000000 #x-81 24)
(ash-tester "neg.fixnum->bignum" #x-4080000000 #x-81 31)
(ash-tester "neg.fixnum->bignum" #x-8100000000 #x-81 32)
(ash-tester "neg.fixnum->bignum" #x-8100000000000000 #x-81 56)
(ash-tester "neg.fixnum->bignum" #x-408000000000000000 #x-81 63)
(ash-tester "neg.fixnum->bignum" #x-810000000000000000 #x-81 64)

(ash-tester "bignum->fixnum" #x81  #x81000000 -24)
(ash-tester "bignum->fixnum" #x40  #x81000000 -25)
(ash-tester "bignum->fixnum" 1  #x81000000 -31)
(ash-tester "bignum->fixnum" 0  #x81000000 -32)
(ash-tester "bignum->fixnum" 0  #x81000000 -100)
(ash-tester "bignum->fixnum" #x81 #x4080000000 -31)
(ash-tester "bignum->fixnum" #x81 #x8100000000 -32)
(ash-tester "bignum->fixnum" #x40 #x8100000000 -33)
(ash-tester "bignum->fixnum" 1 #x8100000000 -39)
(ash-tester "bignum->fixnum" 0 #x8100000000 -40)
(ash-tester "bignum->fixnum" 0 #x8100000000 -100)
(ash-tester "bignum->fixnum" #x81 #x8100000000000000 -56)
(ash-tester "bignum->fixnum" #x81 #x408000000000000000 -63)
(ash-tester "bignum->fixnum" #x40 #x408000000000000000 -64)
(ash-tester "bignum->fixnum" #x20 #x408000000000000000 -65)
(ash-tester "bignum->fixnum" 1 #x408000000000000000 -70)
(ash-tester "bignum->fixnum" 0 #x408000000000000000 -71)
(ash-tester "bignum->fixnum" 0 #x408000000000000000 -100)

(ash-tester "neg.bignum->fixnum" #x-81 #x-81000000 -24)
(ash-tester "neg.bignum->fixnum" #x-41 #x-81000000 -25)
(ash-tester "neg.bignum->fixnum" #x-21 #x-81000000 -26)
(ash-tester "neg.bignum->fixnum" -2 #x-81000000 -31)
(ash-tester "neg.bignum->fixnum" -1 #x-81000000 -32)
(ash-tester "neg.bignum->fixnum" -1 #x-81000000 -33)
(ash-tester "neg.bignum->fixnum" -1 #x-81000000 -100)
(ash-tester "neg.bignum->fixnum" #x-81 #x-4080000000 -31)
(ash-tester "neg.bignum->fixnum" #x-41 #x-4080000000 -32)
(ash-tester "neg.bignum->fixnum" #x-21 #x-4080000000 -33)
(ash-tester "neg.bignum->fixnum" -2 #x-4080000000 -38)
(ash-tester "neg.bignum->fixnum" -1 #x-4080000000 -39)
(ash-tester "neg.bignum->fixnum" -1 #x-4080000000 -100)
(ash-tester "neg.bignum->fixnum" #x-81 #x-408000000000000000 -63)
(ash-tester "neg.bignum->fixnum" #x-41 #x-408000000000000000 -64)
(ash-tester "neg.bignum->fixnum" #x-21 #x-408000000000000000 -65)
(ash-tester "neg.bignum->fixnum" -2 #x-408000000000000000 -70)
(ash-tester "neg.bignum->fixnum" -1 #x-408000000000000000 -71)
(ash-tester "neg.bignum->fixnum" -1 #x-408000000000000000 -72)

(ash-tester "bignum->bignum" #x12345678123456780 #x1234567812345678 4)
(ash-tester "bignum->bignum" #x1234567812345678000000000000000 #x1234567812345678 60)
(ash-tester "bignum->bignum" #x12345678123456780000000000000000 #x1234567812345678 64)
(ash-tester "bignum->bignum" #x123456781234567 #x1234567812345678 -4)
(ash-tester "bignum->bignum" #x12345678 #x1234567812345678 -32)
(ash-tester "neg.bignum->bignum" #x-123456781234568 #x-1234567812345678 -4)
(ash-tester "bignum->bignum" #x-12345679 #x-1234567812345678 -32)

(test* "lognot (fixnum)" -1 (lognot 0))
(test* "lognot (fixnum)" 0 (lognot -1))
(test* "lognot (fixnum)" -65536 (lognot 65535))
(test* "lognot (fixnum)" 65535 (lognot -65536))
(test* "lognot (bignum)" #x-1000000000000000001
      (lognot #x1000000000000000000))
(test* "lognot (bignum)" #x1000000000000000000
      (lognot #x-1000000000000000001))

(define-macro (logop-tester op msg expect x y)
  `(begin
     (set! bitwise-tester-x ,x)
     (set! bitwise-tester-y ,y)
     (test* (format "~a (~a)" ',op ,msg)
            (list ,expect ,expect ,expect ,expect ,expect ,expect)
            (list (,op ,x ,y)
                  (,op bitwise-tester-x ,y)
                  (,op ,x bitwise-tester-y)
                  (,op bitwise-tester-x bitwise-tester-y)
                  (,op bitwise-tester-x ,y bitwise-tester-x)
                  (,op ,x bitwise-tester-x ,y bitwise-tester-y)))))

(logop-tester logand "+fix & 0" 0 #x123456 0)
(logop-tester logand "+big & 0" 0 #x1234567812345678 0)
(logop-tester logand "+fix & -1" #x123456 #x123456 -1)
(logop-tester logand "+big & -1" #x1234567812345678 #x1234567812345678 -1)
(logop-tester logand "+fix & +fix" #x2244 #xaa55 #x6666)
(logop-tester logand "+fix & +big" #x2244 #xaa55 #x6666666666)
(logop-tester logand "+big & +fix" #x4422 #xaa55aa55aa #x6666)
(logop-tester logand "+big & +big" #x2244224422 #xaa55aa55aa #x6666666666)
(logop-tester logand "+big & +big" #x103454301aaccaa #x123456789abcdef #xfedcba987654321fedcba987654321fedcba)
(logop-tester logand "+big & +big" #x400000 #xaa55ea55aa #x55aa55aa55)
(logop-tester logand "+fix & -fix" #x8810 #xaa55 #x-6666)
(logop-tester logand "+fix & -big" #x8810 #xaa55 #x-6666666666)
(logop-tester logand "+big & -fix" #xaa55aa118a #xaa55aa55aa #x-6666)
(logop-tester logand "+big & -big" #x881188118a #xaa55aa55aa #x-6666666666)
(logop-tester logand "+big & -big" #x20002488010146 #x123456789abcdef #x-fedcba987654321fedcba987654321fedcba)
(logop-tester logand "-fix & +fix" #x4422 #x-aa55 #x6666)
(logop-tester logand "-fix & +big" #x6666664422 #x-aa55 #x6666666666)
(logop-tester logand "-big & +fix" #x2246 #x-aa55aa55aa #x6666)
(logop-tester logand "-big & +big" #x4422442246 #x-aa55aa55aa #x6666666666)
(logop-tester logand "-big & +big" #xfedcba987654321fedcba884200020541010 #x-123456789abcdef #xfedcba987654321fedcba987654321fedcba)
(logop-tester logand "-fix & -fix" #x-ee76 #x-aa55 #x-6666)
(logop-tester logand "-fix & -big" #x-666666ee76 #x-aa55 #x-6666666666)
(logop-tester logand "-big & -fix" #x-aa55aa77ee #x-aa55aa55aa #x-6666)
(logop-tester logand "-big & -big" #x-ee77ee77ee #x-aa55aa55aa #x-6666666666)
(logop-tester logand "-big & -big" #x-fedcba987654321fedcba9a76567a9ffde00 #x-123456789abcdef #x-fedcba987654321fedcba987654321fedcba)

(logop-tester logior "+fix | 0" #x123456 #x123456 0)
(logop-tester logior "+big | 0" #x1234567812345678 #x1234567812345678 0)
(logop-tester logior "+fix | -1" -1 #x123456 -1)
(logop-tester logior "+big | -1" -1 #x1234567812345678 -1)
(logop-tester logior "+fix | +fix" #xee77 #xaa55 #x6666)
(logop-tester logior "+fix | +big" #x666666ee77 #xaa55 #x6666666666)
(logop-tester logior "+big | +fix" #xaa55aa77ee #xaa55aa55aa #x6666)
(logop-tester logior "+big | +big" #xee77ee77ee #xaa55aa55aa #x6666666666)
(logop-tester logior "+big | +big" #xfedcba987654321fedcba9a76567a9ffddff #x123456789abcdef #xfedcba987654321fedcba987654321fedcba)
(logop-tester logior "+fix | -fix" #x-4421 #xaa55 #x-6666)
(logop-tester logior "+fix | -big" #x-6666664421 #xaa55 #x-6666666666)
(logop-tester logior "+big | -fix" #x-2246 #xaa55aa55aa #x-6666)
(logop-tester logior "+big | -big" #x-4422442246 #xaa55aa55aa #x-6666666666)
(logop-tester logior "+big | -big" #x-fedcba987654321fedcba884200020541011 #x123456789abcdef #x-fedcba987654321fedcba987654321fedcba)
(logop-tester logior "-fix | +fix" #x-8811 #x-aa55 #x6666)
(logop-tester logior "-fix | +big" #x-8811 #x-aa55 #x6666666666)
(logop-tester logior "-big | +fix" #x-aa55aa118a #x-aa55aa55aa #x6666)
(logop-tester logior "-big | +big" #x-881188118a #x-aa55aa55aa #x6666666666)
(logop-tester logior "-big | +big" #x-20002488010145 #x-123456789abcdef #xfedcba987654321fedcba987654321fedcba)
(logop-tester logior "-fix | -fix" #x-2245 #x-aa55 #x-6666)
(logop-tester logior "-fix | -big" #x-2245 #x-aa55 #x-6666666666)
(logop-tester logior "-big | -fix" #x-4422 #x-aa55aa55aa #x-6666)
(logop-tester logior "-big | -big" #x-2244224422 #x-aa55aa55aa #x-6666666666)
(logop-tester logior "-big | -big" #x-103454301aacca9 #x-123456789abcdef #x-fedcba987654321fedcba987654321fedcba)

;; regression test for incorrect check till 0.9.1
(test* "lognot (error)" (test-error) (lognot 1/2))
(test* "logand (error)" (test-error) (logand 3 1/2))
(test* "logior (error)" (test-error) (logior 3 1/2))
(test* "logxor (error)" (test-error) (logxor 3 1/2))

;; zero and one-argument bitops a la srfi-60
(test* "logand (0arg)" -1 (logand))
(test* "logand (1arg)" 1 (logand 1))
(test* "logand (1arg)" (test-error) (logand 3.14))
(test* "logior (0arg)" 0 (logior))
(test* "logior (1arg)" 1 (logior 1))
(test* "logior (1arg)" (test-error) (logior 3.14))
(test* "logxor (0arg)" 0 (logxor))
(test* "logxor (1arg)" 1 (logxor 1))
(test* "logxor (1arg)" (test-error) (logxor 3.14))

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
              (map (^i (logbit? i #b10110)) '(0 1 2 3 4 5 6)))
(test* "logbit?" '(#f #t #f #t #f #t #t)
              (map (^i (logbit? i #b-10110)) '(0 1 2 3 4 5 6)))

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
      (copy-bit-field #b1101101010 0 0 4))
(test* "copy-bit-field" #b1101101111
      (copy-bit-field #b1101101010 -1 0 4))
(test* "copy-bit-field" #b1111111111101010
      (copy-bit-field #b1101101010 -1 5 16))

(test* "integer-length" 8 (integer-length #b10101010))
(test* "integer-length" 4 (integer-length #b1111))
(test* "integer-length" 0 (integer-length 0))
(test* "integer-length" 0 (integer-length -1))
(test* "integer-length" 1 (integer-length 1))
(test* "integer-length" 1 (integer-length -2))
(test* "integer-length" 29 (integer-length (- (expt 2 29) 1)))
(test* "integer-length" 30 (integer-length (expt 2 29)))
(test* "integer-length" 61 (integer-length (- (expt 2 61) 1)))
(test* "integer-length" 62 (integer-length (expt 2 61)))
(test* "integer-length" 29 (integer-length (- (expt 2 29))))
(test* "integer-length" 30 (integer-length (- (- (expt 2 29)) 1)))
(test* "integer-length" 61 (integer-length (- (expt 2 61))))
(test* "integer-length" 62 (integer-length (- (- (expt 2 61)) 1)))
(test* "integer-length" 1025 (integer-length (expt 2 1024)))

(let1 2s-exponent-factor-tests `((0 0) (-1 0) (1 0) (2 1) (-2 1)
                                 (1048576 20) (-1048576 20)
                                 (,(expt 2 100) 100)
                                 (,(- (expt 2 100)) 100)
                                 (,(* 7 (expt 2 50)) 50))
  (test* "twos-exponent-factor" 2s-exponent-factor-tests
         (map (lambda [t] (list (car t) (twos-exponent-factor (car t))))
              2s-exponent-factor-tests)))

(let1 2s-exponent-tests `(0 1 -1 2 -2 4 8 65535 65536
                          131072 ,(* 3 131072)
                          ,(expt 2 80) ,(- (expt 2 80))
                          ,(* 3 (expt 2 80)))
  (define (dumb-test n k)
    (cond [(<= n 0) #f]
          [(= n 1) k]
          [(odd? n) #f]
          [else (dumb-test (/ n 2) (+ k 1))]))
  (test* "twos-exponent"
         (map (lambda [k] (cons k (dumb-test k 0))) 2s-exponent-tests)
         (map (lambda [k] (cons k (twos-exponent k))) 2s-exponent-tests)))

;;------------------------------------------------------------------
(test-section "inexact arithmetics")

;; +. etc are inlined, so we want to test both inlined case and
;; explicitly called case.
(define-syntax inexact-arith-test
  (syntax-rules ()
    [(_ msg exp (op . args))
     (begin
       (test* (string-append msg " inlined") exp (op . args))
       (test* (string-append msg " applied") exp (apply op (list . args))))]))

(inexact-arith-test "+. (0)" 0.0 (+.))
(inexact-arith-test "+. (1)" 1.0 (+. 1))
(inexact-arith-test "+. (1big)" 1.0e20 (+. 100000000000000000000))
(inexact-arith-test "+. (1rat)" 1.5 (+. 3/2))
(inexact-arith-test "+. (1cmp)" 1.0+i (+. 1+i))
(inexact-arith-test "+. (2)" 1.0 (+. 0 1))
(inexact-arith-test "+. (2big)" 1.0e20 (+. 1 100000000000000000000))
(inexact-arith-test "+. (2rat)" 1.5 (+. 1 1/2))
(inexact-arith-test "+. (many)" 15.0 (+. 1 2 3 4 5))

(inexact-arith-test "-. (1)" -1.0 (-. 1))
(inexact-arith-test "-. (1big)" -1.0e20 (-. 100000000000000000000))
(inexact-arith-test "-. (1rat)" -1.5 (-. 3/2))
(inexact-arith-test "-. (1cmp)" -1.0-i (-. 1+i))
(inexact-arith-test "-. (2)" -1.0 (-. 0 1))
(inexact-arith-test "-. (2big)" -1.0e20 (-. 1 100000000000000000000))
(inexact-arith-test "-. (2rat)" 0.5 (-. 1 1/2))
(inexact-arith-test "-. (many)" -13.0 (-. 1 2 3 4 5))

(inexact-arith-test "*. (0)" 1.0 (*.))
(inexact-arith-test "*. (1)" 1.0 (*. 1))
(inexact-arith-test "*. (1big)" 1.0e20 (*. 100000000000000000000))
(inexact-arith-test "*. (1rat)" 1.5 (*. 3/2))
(inexact-arith-test "*. (1cmp)" 1.0+i (*. 1+i))
(inexact-arith-test "*. (2)"  0.0 (*. 0 1))
(inexact-arith-test "*. (2big)" 1.0e20 (*. 1 100000000000000000000))
(inexact-arith-test "*. (2rat)" 0.5 (*. 1 1/2))
(inexact-arith-test "*. (many)" 120.0 (*. 1 2 3 4 5))

(inexact-arith-test "/. (1)" 1.0 (/. 1))
(inexact-arith-test "/. (1big)" 1.0e-20 (/. 100000000000000000000))
(inexact-arith-test "/. (1rat)" 0.6666666666666666 (/. 3/2))
(inexact-arith-test "/. (1cmp)" 0.5-0.5i (/. 1+i))
(inexact-arith-test "/. (2)"  0.0 (/. 0 1))
(inexact-arith-test "/. (2big)" 1.0e-20 (/. 1 100000000000000000000))
(inexact-arith-test "/. (2rat)" 2.0 (/. 1 1/2))
(inexact-arith-test "/. (2rat1)" 0.5 (/. 1/2 1))
(inexact-arith-test "/. (2rat2)" 2.0 (/. 1/2 1/4))
(inexact-arith-test "/. (many)" 0.1 (/. 1 2 5))

;; The following takes a special path to avoid overflow.
(inexact-arith-test "/. fixnum bignum" 0.0 (/. (expt 10 400)))
(inexact-arith-test "/. bignum fixnum" +inf.0 (/. (expt 10 400) 1))
(inexact-arith-test "/. bignum fixnum" -inf.0 (/. (expt 10 400) -1))
(inexact-arith-test "/. bignum bignum" 10.0 (/. (expt 10 401) (expt 10 400)))

;;------------------------------------------------------------------
(test-section "sqrt")

(define (integer-sqrt-tester k)
  (test* (format "exact-integer-sqrt ~a" k) '(#t #t #t)
         (receive (s r) (exact-integer-sqrt k)
           (list (>= r 0)
                 (= k (+ (* s s) r))
                 (< k (* (+ s 1) (+ s 1)))))))

(integer-sqrt-tester 0)
(integer-sqrt-tester 1)
(integer-sqrt-tester 2)
(integer-sqrt-tester 3)
(integer-sqrt-tester 4)
(integer-sqrt-tester 10)
(integer-sqrt-tester (expt 2 32))
(integer-sqrt-tester (- (expt 2 52) 1))
(integer-sqrt-tester (expt 2 52))
(integer-sqrt-tester (+ (expt 2 52) 1))
(integer-sqrt-tester 9007199136250224)
(integer-sqrt-tester 9007199136250226)
(integer-sqrt-tester (- (expt 2 53) 1))
(integer-sqrt-tester (expt 2 53))
(integer-sqrt-tester (+ (expt 2 53) 1))
(integer-sqrt-tester 9999999999999999999999999999999999999999999999999999)
(integer-sqrt-tester (+ (expt 10 400) 3141592653589)) ; double range overflow

(test* "exact-integer-sqrt -1" (test-error) (exact-integer-sqrt -1))
(test* "exact-integer-sqrt 1.0" (test-error) (exact-integer-sqrt 1.0))
(test* "exact-integer-sqrt 1/4" (test-error) (exact-integer-sqrt 1/4))

;; try to cover various paths in sqrt of exact numbers
(test* "sqrt, exact" 0 (sqrt 0) eqv?)
(test* "sqrt, exact" 4 (sqrt 16) eqv?)
(test* "sqrt, exact" (expt 2 64) (sqrt (expt 2 128)) eqv?)

(test* "sqrt, inexact" 4.0 (sqrt 16.0) eqv?)
(test* "sqrt, inexact" +4.0i (sqrt -16.0) eqv?)
(test* "sqrt, inexact" (%sqrt (- (expt 2 64) 1))
       (sqrt (- (expt 2 64) 1)) eqv?)

(test* "sqrt, exact" 1/4 (sqrt 1/16) eqv?)
(test* "sqrt, exact" (/ 1 (expt 2 64)) (sqrt (/ 1 (expt 2 128))) eqv?)
(test* "sqrt, exact" (/ (expt 2 64) 3) (sqrt (/ (expt 2 128) 9)) eqv?)
(test* "sqrt, exact" (/ (expt 2 64) (expt 3 30))
       (sqrt (/ (expt 2 128) (expt 3 60))) eqv?)

(test* "sqrt, inexact" 0.25 (sqrt (exact->inexact 1/16)) eqv?)
(test* "sqrt, inexact" (%sqrt (/ (- (expt 2 64) 1) (expt 3 30)))
       (sqrt (/ (- (expt 2 64) 1) (expt 3 30))) eqv?)

;;------------------------------------------------------------------
(test-section "posix math functions")

(test* "fmod" 0.25 (fmod 5.25 1) (^[x y] (nearly=? 1e-6 x y)))
(test* "fmod" 2.3  (fmod 8.3 3)  (^[x y] (nearly=? 1e-6 x y)))
(test* "fmod" 8.3  (fmod 8.3 33) (^[x y] (nearly=? 1e-6 x y)))

(test* "frexp" '(0.785 2)
       (values->list (frexp 3.14))
       (^[x y] (and (nearly=? 1e-6 (car x) (car y))
                    (nearly=? 1e-6 (cadr x) (cadr y)))))

(test* "ldexp" 3.14 (ldexp 0.785 2) (^[x y] (nearly=? 1e-6 x y)))

(test* "modf" '(0.14 3.0)
       (values->list (modf 3.14))
       (^[x y] (and (nearly=? 1e-6 (car x) (car y))
                    (nearly=? 1e-6 (cadr x) (cadr y)))))

;; This is to check alternative gamma implementation assuming we can use
;; system's tgamma and lgamma.
'(let ()
  (define (test-gamma name fn0 fn1)
    (test* #"alt-~name" #f
           (any (^[x] (let* ([y0 (fn0 x)]
                             [y1 (fn1 x)]
                             [e  (/ (abs (- y0 y1)) y0)])
                        (and (> e 1e-6)
                             (format "Error too big (~s) at x=~s (~a=~s alt-~a=~s"
                                     e x name y0 name y1))))
                (map (cut expt 10 <>) (iota 150 -5 0.05)))))
  (test-gamma "gamma"
              (with-module gauche.internal %gamma)
              (with-module gauche.internal %alt-gamma))
  (test-gamma "lgamma"
              (with-module gauche.internal %lgamma)
              (with-module gauche.internal %alt-lgamma))
  )

;; log on huge number - naive use of Scm_GetDouble overflows
(let-syntax ([log-tester
              (syntax-rules ()
                [(_ input)
                 (let1 factor (expt 2 (integer-length input))
                   (test* (write-to-string '(log input))
                          (+ (log factor) (log (/ input factor)))
                          (log input)))])])
  (log-tester (expt 2 2048))
  (log-tester (- (expt 2 2048)))
  (log-tester (+ (expt 3 2048) (expt 3 2047)))
  (log-tester (- (expt 7 7715)))
  )

;;------------------------------------------------------------------
(test-section "sinpi, cospi, tanpi")

(let ()
  (define (check trig trig-pi)
    (let loop ([x -4])
      (if (> x 4)
        #f
        (let ([t0 (trig (* x 3.141592653589793))]
              [t1 (trig-pi x)])
          (if (or (and (> (abs t0) 1e15)
                       (> (abs t1) 1e15))
                  (< (abs (- t0 t1)) 1e-10))
            (loop (+ x 1/16))
            `(((,trig (* pi ,x)) ,t0)
              ((,trig-pi ,x) ,t1)))))))
  (test* "sin vs sinpi" #f (check %sin %sinpi))
  (test* "cos vs cospi" #f (check %cos %cospi))
  (test* "tan vs tanpi" #f (check %tan %tanpi)))

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

(define-method object-+ ((a <string>) b) #"~|a|+~|b|")
(define-method object-+ (a (b <string>)) #"~|a|+~|b|")
(define-method object-- ((a <string>) b) #"~|a|-~|b|")
(define-method object-- (a (b <string>)) #"~|a|-~|b|")
(define-method object-* ((a <string>) b) #"~|a|*~|b|")
(define-method object-* (a (b <string>)) #"~|a|*~|b|")
(define-method object-/ ((a <string>) b) #"~|a|/~|b|")
(define-method object-/ (a (b <string>)) #"~|a|/~|b|")

(define-method object-- ((a <string>)) #"-~|a|")
(define-method object-/ ((a <string>)) #"/~|a|")

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
