;;
;; test for srfi-4 module
;;

(use gauche.test)
(use gauche.parameter)
(use srfi-1)

(test-start "uniform vector and array")
(use gauche.uvector)
(test-module 'gauche.uvector)

;;-------------------------------------------------------------------
(test-section "reader syntax")

(test* "#s8()" #t (s8vector? '#s8(0 1 2 3 4)))
(test* "#s8()" #f (s8vector? '#(0 1 2 3 4)))
(test* "#u8()" #t (u8vector? '#u8(0 1 2 3 4)))
(test* "#u8()" #f (u8vector? '#(0 1 2 3 4)))
(test* "#s16()" #t (s16vector? '#s16(0 1 2 3 4)))
(test* "#s16()" #f (s16vector? '#(0 1 2 3 4)))
(test* "#u16()" #t (u16vector? '#u16(0 1 2 3 4)))
(test* "#u16()" #f (u16vector? '#(0 1 2 3 4)))
(test* "#s32()" #t (s32vector? '#s32(0 1 2 3 4)))
(test* "#s32()" #f (s32vector? '#(0 1 2 3 4)))
(test* "#u32()" #t (u32vector? '#u32(0 1 2 3 4)))
(test* "#u32()" #f (u32vector? '#(0 1 2 3 4)))
(test* "#s64()" #t (s64vector? '#s64(0 1 2 3 4)))
(test* "#s64()" #f (s64vector? '#(0 1 2 3 4)))
(test* "#u64()" #t (u64vector? '#u64(0 1 2 3 4)))
(test* "#u64()" #f (u64vector? '#(0 1 2 3 4)))
(test* "#f16()" #t (f16vector? '#f16(0.0 1.0 2.0 3.0 4.0)))
(test* "#f16()" #f (f16vector? '#(0.0 1.0 2.0 3.0 4.0)))
(test* "#f32()" #t (f32vector? '#f32(0.0 1.0 2.0 3.0 4.0)))
(test* "#f32()" #f (f32vector? '#(0.0 1.0 2.0 3.0 4.0)))
(test* "#f64()" #t (f64vector? '#f64(0.0 1.0 2.0 3.0 4.0)))
(test* "#f64()" #f (f64vector? '#(0.0 1.0 2.0 3.0 4.0)))

;;-------------------------------------------------------------------
(test-section "writer syntax")

(test* "#s8()" "#s8(0 1 2 3 4)"
       (with-output-to-string
         (^[] (write (apply s8vector (iota 5))))))
(test* "#u8()" "#u8(0 1 2 3 4)"
       (with-output-to-string
         (^[] (write (apply u8vector (iota 5))))))
(test* "#s16()" "#s16(0 1 2 3 4)"
       (with-output-to-string
         (^[] (write (apply s16vector (iota 5))))))
(test* "#u16()" "#u16(0 1 2 3 4)"
       (with-output-to-string
         (^[] (write (apply u16vector (iota 5))))))
(test* "#s32()" "#s32(0 1 2 3 4)"
       (with-output-to-string
         (^[] (write (apply s32vector (iota 5))))))
(test* "#u32()" "#u32(0 1 2 3 4)"
       (with-output-to-string
         (^[] (write (apply u32vector (iota 5))))))
(test* "#s64()" "#s64(0 1 2 3 4)"
       (with-output-to-string
         (^[] (write (apply s64vector (iota 5))))))
(test* "#u64()" "#u64(0 1 2 3 4)"
       (with-output-to-string
         (^[] (write (apply u64vector (iota 5))))))
(test* "#f16()" "#f16()"
       (with-output-to-string
         (^[] (write (f16vector)))))
(test* "#f32()" "#f32()"
       (with-output-to-string
         (^[] (write (f32vector)))))
(test* "#f64()" "#f64()"
       (with-output-to-string
         (^[] (write (f64vector)))))

;;-------------------------------------------------------------------
(test-section "constructors")

(define (uvmaketester class specific inits)
  ;; zero length
  (test* (format "make ~a 0" (class-name class))
         (make-uvector class 0)
         (specific 0))
  ;; some content
  (dolist [init inits]
    (test* (format "make ~a 10 ~a" (class-name class) init)
           (make-uvector class 10 init)
           (specific 10 init))))

(uvmaketester <s8vector> make-s8vector '(0 10 -4))
(uvmaketester <u8vector> make-u8vector '(0 4 255))
(uvmaketester <s16vector> make-s16vector '(0 -32768 32767))
(uvmaketester <u16vector> make-u16vector '(0 65535))
(uvmaketester <s32vector> make-s32vector '(0))
(uvmaketester <u32vector> make-u32vector '(0))
(uvmaketester <s64vector> make-s64vector '(0))
(uvmaketester <u64vector> make-u64vector '(0))
(uvmaketester <f16vector> make-f16vector '(0 1.0 -1.0)) 
(uvmaketester <f32vector> make-f32vector '(0 1.0 -1.0)) 
(uvmaketester <f64vector> make-f64vector '(0 1.0 -1.0)) 

;;-------------------------------------------------------------------
(test-section "ref and set")

(define (uvrefset-tester make ref set numlist expvec)
  (let ([vec (make (length numlist))]
        [seq (iota (length numlist))])
    (for-each (^[n i] (set vec i n)) numlist seq)
    (and (equal? expvec vec)
         (equal? numlist (map (^i (ref vec i)) seq)))))

(test* "s8vector-ref|set!" #t
       (uvrefset-tester make-s8vector s8vector-ref s8vector-set!
                        '(0 -1 1 -128 127)
                        '#s8(0 -1 1 -128 127)))
(test* "u8vector-ref|set!" #t
       (uvrefset-tester make-u8vector u8vector-ref u8vector-set!
                        '(0 1 2 3 255)
                        '#u8(0 1 2 3 255)))
(test* "s16vector-ref|set!" #t
       (uvrefset-tester make-s16vector s16vector-ref s16vector-set!
                        '(0 -1 1 -32768 32767)
                        '#s16(0 -1 1 -32768 32767)))
(test* "u16vector-ref|set!" #t
       (uvrefset-tester make-u16vector u16vector-ref u16vector-set!
                        '(0 1 2 3 65535)
                        '#u16(0 1 2 3 65535)))
(test* "s32vector-ref|set!" #t
       (uvrefset-tester make-s32vector s32vector-ref s32vector-set!
                        '(0 -1 1 #x-80000000 #x7fffffff)
                        '#s32(0 -1 1 #x-80000000 #x7fffffff)))
(test* "u32vector-ref|set!" #t
       (uvrefset-tester make-u32vector u32vector-ref u32vector-set!
                        '(0 1 2 #xffffffff)
                        '#u32(0 1 2 #xffffffff)))
(test* "s64vector-ref|set!" #t
       (uvrefset-tester make-s64vector s64vector-ref s64vector-set!
                        '(0 -1 1 #x-8000000000000000 #x7fffffffffffffff)
                        '#s64(0 -1 1 #x-8000000000000000 #x7fffffffffffffff)))
(test* "u64vector-ref|set!" #t
       (uvrefset-tester make-u64vector u64vector-ref u64vector-set!
                        '(0 1 2 #xffffffffffffffff)
                        '#u64(0 1 2 #xffffffffffffffff)))
(test* "f16vector-ref|set!" #t
       (uvrefset-tester make-f16vector f16vector-ref f16vector-set!
                        '(0.0 -1.0 1.0)
                        '#f16(0.0 -1.0 1.0)))
(test* "f32vector-ref|set!" #t
       (uvrefset-tester make-f32vector f32vector-ref f32vector-set!
                        '(0.0 -1.0 1.0)
                        '#f32(0.0 -1.0 1.0)))
(test* "f64vector-ref|set!" #t
       (uvrefset-tester make-f64vector f64vector-ref f64vector-set!
                        '(0.0 -1.0 1.0)
                        '#f64(0.0 -1.0 1.0)))

(define (uvset-clamp-tester make ref set value)
  (let1 v (make 1)
    (list (with-error-handler (^e 'error)
            (^[] (set v 0 value) (ref v 0)))
          (with-error-handler (^e 'error)
            (^[] (set v 0 value 'low) (ref v 0)))
          (with-error-handler (^e 'error)
            (^[] (set v 0 value 'high) (ref v 0)))
          (with-error-handler (^e 'error)
            (^[] (set v 0 value 'both) (ref v 0))))))

(test* "s8vector-set! clamp" '(error -128 error -128)
       (uvset-clamp-tester make-s8vector s8vector-ref s8vector-set! -129))
(test* "s8vector-set! clamp" '(error error 127 127)
       (uvset-clamp-tester make-s8vector s8vector-ref s8vector-set! 128))

(test* "u8vector-set! clamp" '(error 0 error 0)
       (uvset-clamp-tester make-u8vector u8vector-ref u8vector-set! -1))
(test* "u8vector-set! clamp" '(error error 255 255)
       (uvset-clamp-tester make-u8vector u8vector-ref u8vector-set! 256))

(test* "s16vector-set! clamp" '(error -32768 error -32768)
       (uvset-clamp-tester make-s16vector s16vector-ref s16vector-set! -32769))
(test* "s16vector-set! clamp" '(error error 32767 32767)
       (uvset-clamp-tester make-s16vector s16vector-ref s16vector-set! 32768))

(test* "u16vector-set! clamp" '(error 0 error 0)
       (uvset-clamp-tester make-u16vector u16vector-ref u16vector-set! -1))
(test* "u16vector-set! clamp" '(error error 65535 65535)
       (uvset-clamp-tester make-u16vector u16vector-ref u16vector-set! 65536))

(test* "s32vector-set! clamp" '(error -2147483648 error -2147483648)
       (uvset-clamp-tester make-s32vector s32vector-ref s32vector-set! -2147483649))
(test* "s32vector-set! clamp" '(error error 2147483647 2147483647)
       (uvset-clamp-tester make-s32vector s32vector-ref s32vector-set! 2147483648))

(test* "u32vector-set! clamp" '(error 0 error 0)
       (uvset-clamp-tester make-u32vector u32vector-ref u32vector-set! -1))
(test* "u32vector-set! clamp" '(error error 4294967295 4294967295)
       (uvset-clamp-tester make-u32vector u32vector-ref u32vector-set! 4294967296))

(test* "s64vector-set! clamp" '(error -9223372036854775808 error -9223372036854775808)
       (uvset-clamp-tester make-s64vector s64vector-ref s64vector-set! -9223372036854775809))
(test* "s64vector-set! clamp" '(error error 9223372036854775807 9223372036854775807)
       (uvset-clamp-tester make-s64vector s64vector-ref s64vector-set! 9223372036854775808))

(test* "u64vector-set! clamp" '(error 0 error 0)
       (uvset-clamp-tester make-u64vector u64vector-ref u64vector-set! -1))
(test* "u64vector-set! clamp" '(error error 18446744073709551615 18446744073709551615)
       (uvset-clamp-tester make-u64vector u64vector-ref u64vector-set! 18446744073709551616))

;;-------------------------------------------------------------------
(test-section "conversions")

(define (uvconv-tester ->list list-> ->vec vec-> uvec nums)
  (let* ([lis (->list uvec)]
         [uv2 (list-> lis)]
         [vec (->vec  uvec)]
         [uv3 (vec->  vec)])
    (and (equal? lis nums)
         (equal? uv2 uvec)
         (equal? vec (list->vector nums))
         (equal? uv3 uvec))))

(test* "s8vector conversion" #t
       (uvconv-tester s8vector->list list->s8vector
                      s8vector->vector vector->s8vector
                      '#s8(0 -1 1 -128 127) '(0 -1 1 -128 127)))
(test* "u8vector conversion" #t
       (uvconv-tester u8vector->list list->u8vector
                      u8vector->vector vector->u8vector
                      '#u8(0 1 254 255) '(0 1 254 255)))
(test* "s16vector conversion" #t
       (uvconv-tester s16vector->list list->s16vector
                      s16vector->vector vector->s16vector
                      '#s16(0 -1 1 -32768 32767) '(0 -1 1 -32768 32767)))
(test* "u16vector conversion" #t
       (uvconv-tester u16vector->list list->u16vector
                      u16vector->vector vector->u16vector
                      '#u16(0 1 65534 65535) '(0 1 65534 65535)))
(test* "s32vector conversion" #t
       (uvconv-tester s32vector->list list->s32vector
                      s32vector->vector vector->s32vector
                      '#s32(0 -1 1 #x-80000000 #x7fffffff)
                      '(0 -1 1 #x-80000000 #x7fffffff)))
(test* "u32vector conversion" #t
       (uvconv-tester u32vector->list list->u32vector
                      u32vector->vector vector->u32vector
                      '#u32(0 1 #xfffffffe #xffffffff)
                      '(0 1 #xfffffffe #xffffffff)))
(test* "s64vector conversion" #t
       (uvconv-tester s64vector->list list->s64vector
                      s64vector->vector vector->s64vector
                      '#s64(0 -1 1 #x-8000000000000000 #x7fffffffffffffff)
                      '(0 -1 1 #x-8000000000000000 #x7fffffffffffffff)))
(test* "u64vector conversion" #t
       (uvconv-tester u64vector->list list->u64vector
                      u64vector->vector vector->u64vector
                      '#u64(0 1 #xffffffffffffffff)
                      '(0 1 #xffffffffffffffff)))
(test* "f16vector conversion" #t
       (uvconv-tester f16vector->list list->f16vector
                      f16vector->vector vector->f16vector
                      '#f16(0.0 -1.0 1.0)
                      '(0.0 -1.0 1.0)))
(test* "f32vector conversion" #t
       (uvconv-tester f32vector->list list->f32vector
                      f32vector->vector vector->f32vector
                      '#f32(0.0 -1.0 1.0)
                      '(0.0 -1.0 1.0)))
(test* "f64vector conversion" #t
       (uvconv-tester f64vector->list list->f64vector
                      f64vector->vector vector->f64vector
                      '#f64(0.0 -1.0 1.0)
                      '(0.0 -1.0 1.0)))

;;-------------------------------------------------------------------
(test-section "comparison")

(define (uvcomp-tester list-> samples)
  (dolist [x samples]
    (dolist [y samples]
      (test* #"equal? ~(list-> x) ~(list-> y)"
             (and (= (length x) (length y))
                  (every = x y))
             (equal? (list-> x) (list-> y))))))

(for-each (cut uvcomp-tester <> '(() (0) (1) (0 0) (0 1)))
          (list list->s8vector list->u8vector
                list->s16vector list->u16vector
                list->s32vector list->u32vector
                list->s64vector list->u64vector))

(for-each (cut uvcomp-tester <> '(() (0) (1) (0 0) (0 1)
                                  (+inf.0) (-inf.0) (+nan.0)
                                  (0 +inf.0) (0 -inf.0) (0 +nan.0)))
          (list list->f16vector
                list->f32vector
                list->f64vector))

;;-------------------------------------------------------------------
(test-section "copying and filling")

(define (uvcopy-tester copy copy! fill! ->list list-> uvec filler)
  (let* ([c0 (list-> (->list uvec))]
         [c1 (copy uvec)]
         [c2 (uvector-copy uvec)])
    (and (equal? c1 uvec)
         (equal? c2 uvec)
         (begin (fill! c1 filler)
                (and (equal? c0 uvec)
                     (every (^n (= n filler))  (->list c1))
                     (begin (copy! c1 uvec)
                            (equal? c1 c0)))))))

(test* "s8vector copy|fill!" #t
       (uvcopy-tester s8vector-copy s8vector-copy! s8vector-fill!
                      s8vector->list list->s8vector
                      '#s8(0 -1 1 -128 127) -128))
(test* "u8vector copy|fill!" #t
       (uvcopy-tester u8vector-copy u8vector-copy! u8vector-fill!
                      u8vector->list list->u8vector
                      '#u8(0 1 255) 255))
(test* "s16vector copy|fill!" #t
       (uvcopy-tester s16vector-copy s16vector-copy! s16vector-fill!
                      s16vector->list list->s16vector
                      '#s16(0 -1 1 -32768 32767) -32768))
(test* "u16vector copy|fill!" #t
       (uvcopy-tester u16vector-copy u16vector-copy! u16vector-fill!
                      u16vector->list list->u16vector
                      '#u16(0 1 65535) 32768))
(test* "s32vector copy|fill!" #t
       (uvcopy-tester s32vector-copy s32vector-copy! s32vector-fill!
                      s32vector->list list->s32vector
                      '#s32(0 -1 1 #x-80000000 #x7fffffff) #x7fffffff))
(test* "u32vector copy|fill!" #t
       (uvcopy-tester u32vector-copy u32vector-copy! u32vector-fill!
                      u32vector->list list->u32vector
                      '#u32(0 1 #xffffffff) #x80000000))
(test* "s64vector copy|fill!" #t
       (uvcopy-tester s64vector-copy s64vector-copy! s64vector-fill!
                      s64vector->list list->s64vector
                      '#s64(0 -1 1 #x-8000000000000000 #x7fffffffffffffff)
                      #x7fffffffffffffff))
(test* "u64vector copy|fill!" #t
       (uvcopy-tester u64vector-copy u64vector-copy! u64vector-fill!
                      u64vector->list list->u64vector
                      '#u64(0 1 #xffffffffffffffff) #x8000000000000000))

(test* "f16vector copy|fill!" #t
       (uvcopy-tester f16vector-copy f16vector-copy! f16vector-fill!
                      f16vector->list list->f16vector
                      '#f16(0 -1.0 1.0) 1.0))
(test* "f32vector copy|fill!" #t
       (uvcopy-tester f32vector-copy f32vector-copy! f32vector-fill!
                      f32vector->list list->f32vector
                      '#f32(0 -1.0 1.0) 1.0))
(test* "f64vector copy|fill!" #t
       (uvcopy-tester f64vector-copy f64vector-copy! f64vector-fill!
                      f64vector->list list->f64vector
                      '#f64(0 -1.0 1.0) 1.0e64))

(define (uvcopy-startend-test msg make copy fill)
  (test* msg (list (make 1 2 3) (make 1 2) (make 0 9 9 3))
         (let1 v (make 0 1 2 3)
           (list (copy v 1)
                 (copy v 1 3)
                 (fill v 9 1 3)))))

(uvcopy-startend-test "uvcopy-startend s8vector"
                      s8vector s8vector-copy s8vector-fill!)
(uvcopy-startend-test "uvcopy-startend u8vector"
                      u8vector u8vector-copy u8vector-fill!)
(uvcopy-startend-test "uvcopy-startend s16vector"
                      s16vector s16vector-copy s16vector-fill!)
(uvcopy-startend-test "uvcopy-startend u16vector"
                      u16vector u16vector-copy u16vector-fill!)
(uvcopy-startend-test "uvcopy-startend s32vector"
                      s32vector s32vector-copy s32vector-fill!)
(uvcopy-startend-test "uvcopy-startend u32vector"
                      u32vector u32vector-copy u32vector-fill!)
(uvcopy-startend-test "uvcopy-startend s64vector"
                      s64vector s64vector-copy s64vector-fill!)
(uvcopy-startend-test "uvcopy-startend u64vector"
                      u64vector u64vector-copy u64vector-fill!)

(define (uvcopy!-newapi-test msg make copy!)
  (test* #"~msg /tstart" (make 0 7 8 9)
         (copy! (make 0 1 2 3) 1 (make 7 8 9 10)))
  (test* #"~msg /tstart(over)" (make 0 1 2 3)
         (copy! (make 0 1 2 3) 4 (make 7 8 9 10)))
  (test* #"~msg /tstart,sstart" (make 0 9 10 3)
         (copy! (make 0 1 2 3) 1 (make 7 8 9 10) 2))
  (test* #"~msg /tstart,sstart,send" (make 0 1 2 9)
         (copy! (make 0 1 2 3) 3 (make 7 8 9 10) 2 3))
  )

(uvcopy!-newapi-test "s8vector-copy! newapi" s8vector s8vector-copy!)
(uvcopy!-newapi-test "u8vector-copy! newapi" u8vector u8vector-copy!)
(uvcopy!-newapi-test "s16vector-copy! newapi" s16vector s16vector-copy!)
(uvcopy!-newapi-test "u16vector-copy! newapi" u16vector u16vector-copy!)
(uvcopy!-newapi-test "s32vector-copy! newapi" s32vector s32vector-copy!)
(uvcopy!-newapi-test "u32vector-copy! newapi" u32vector u32vector-copy!)
(uvcopy!-newapi-test "s64vector-copy! newapi" s64vector s64vector-copy!)
(uvcopy!-newapi-test "u64vector-copy! newapi" u64vector u64vector-copy!)
(uvcopy!-newapi-test "f16vector-copy! newapi" f16vector f16vector-copy!)
(uvcopy!-newapi-test "f32vector-copy! newapi" f32vector f32vector-copy!)
(uvcopy!-newapi-test "f64vector-copy! newapi" f64vector f64vector-copy!)

(test* "uvector-copy! (generic)" '#u16(0 0 1 2 65535 0)
       (rlet1 v (make-u16vector 6 0)
         (uvector-copy! v 2 '#s16(1 2 -1))))
(test* "uvector-copy! (generic, start)" '#u16(0 0 1 2 65535 0)
       (rlet1 v (make-u16vector 6 0)
         (uvector-copy! v 2 '#s16(0 1 2 -1) 1)))
(test* "uvector-copy! (generic, start+end)" '#u16(0 0 1 2 65535 0)
       (rlet1 v (make-u16vector 6 0)
         (uvector-copy! v 2 '#s16(0 1 2 -1 -2) 1 4)))
(test* "uvector-copy! (generic, different size)" '#u16(0 0 257 0)
       (rlet1 v (make-u16vector 4 0)
         (uvector-copy! v 2 '#u8(1 1))))

(define (uv-multicopy!-test msg make ctor copy!)
  (test* #"~msg generic" (ctor 0 1 2 3 0 1 2 3 0 1 2)
         (rlet1 dst (make 11 0)
           (copy! dst 1 4 (ctor 1 2 3))))
  (test* #"~msg ssize" (ctor 0 1 2 3 0 0 4 5 6 0 0 7)
         (rlet1 dst (make 12 0)
           (copy! dst 1 5 (ctor 1 2 3 4 5 6 7 8 9) 0 3)))
  (test* #"~msg sstart, ssize" (ctor 0 5 6 7 0 0 8 9 0 0 0 0)
         (rlet1 dst (make 12 0)
           (copy! dst 1 5 (ctor 1 2 3 4 5 6 7 8 9) 4 3)))
  (test* #"~msg ssize, sstride" (ctor 1 2 3 0 2 3 4 0 3 4 5 0)
         (rlet1 dst (make 12 0)
           (copy! dst 0 4 (ctor 1 2 3 4 5 6 7 8 9) 0 3 1)))
  (test* #"~msg count" (ctor 1 2 3 0 2 3 4 0 0 0 0 0)
         (rlet1 dst (make 12 0)
           (copy! dst 0 4 (ctor 1 2 3 4 5 6 7 8 9) 0 3 1 2)))
  (test* #"~msg single item" (ctor 1 0 1 0 1 0 1 0 1 0)
         (rlet1 dst (make 10 0)
           (copy! dst 0 2 (ctor 1))))
  )

(uv-multicopy!-test "s8vector-multi-copy!" make-s8vector s8vector s8vector-multi-copy!)
(uv-multicopy!-test "u8vector-multi-copy!" make-u8vector u8vector u8vector-multi-copy!)
(uv-multicopy!-test "s16vector-multi-copy!" make-s16vector s16vector s16vector-multi-copy!)
(uv-multicopy!-test "u16vector-multi-copy!" make-u16vector u16vector u16vector-multi-copy!)
(uv-multicopy!-test "s32vector-multi-copy!" make-s32vector s32vector s32vector-multi-copy!)
(uv-multicopy!-test "u32vector-multi-copy!" make-u32vector u32vector u32vector-multi-copy!)
(uv-multicopy!-test "s64vector-multi-copy!" make-s64vector s64vector s64vector-multi-copy!)
(uv-multicopy!-test "u64vector-multi-copy!" make-u64vector u64vector u64vector-multi-copy!)
(uv-multicopy!-test "f16vector-multi-copy!" make-f16vector f16vector f16vector-multi-copy!)
(uv-multicopy!-test "f32vector-multi-copy!" make-f32vector f32vector f32vector-multi-copy!)
(uv-multicopy!-test "f64vector-multi-copy!" make-f64vector f64vector f64vector-multi-copy!)

(define (uv-append-test msg ctor append)
  (test* #"~msg base" (ctor) (append))
  (test* #"~msg unit" (ctor 1 2 3 ) (append (ctor 1 2 3)))
  (test* #"~msg" (ctor 1 2 3 4 5 6 7 8)
         (append (ctor 1 2 3) (ctor) (ctor 4 5) (ctor 6 7 8))))

(uv-append-test "s8vector-append" s8vector s8vector-append)
(uv-append-test "u8vector-append" u8vector u8vector-append)
(uv-append-test "s16vector-append" s16vector s16vector-append)
(uv-append-test "u16vector-append" u16vector u16vector-append)
(uv-append-test "s32vector-append" s32vector s32vector-append)
(uv-append-test "u32vector-append" u32vector u32vector-append)
(uv-append-test "s64vector-append" s64vector s64vector-append)
(uv-append-test "u64vector-append" u64vector u64vector-append)
(uv-append-test "f16vector-append" f16vector f16vector-append)
(uv-append-test "f32vector-append" f32vector f32vector-append)
(uv-append-test "f64vector-append" f64vector f64vector-append)

;;-------------------------------------------------------------------
(test-section "swapping bytes")

(test* "swapb s16"
       '#s16(#x0123 #x4567 #x7654 #x3210)
       (s16vector-swap-bytes '#s16(#x2301 #x6745 #x5476 #x1032)))
(test* "swapb u16"
       '#u16(#x0123 #x4567 #x7654 #x3210)
       (u16vector-swap-bytes '#u16(#x2301 #x6745 #x5476 #x1032)))
(test* "swapb s32"
       '#s32(#x01234567 #x76543210)
       (s32vector-swap-bytes '#s32(#x67452301 #x10325476)))
(test* "swapb u32"
       '#u32(#x01234567 #x76543210)
       (u32vector-swap-bytes '#u32(#x67452301 #x10325476)))
(test* "swapb s64"
       '#s64(#x0123456789abcdef #x-123456789abcdf0)
       (s64vector-swap-bytes
        '#s64(#x-1032547698badcff #x1032547698badcfe)))
(test* "swapb u64"
       '#u64(#x0123456789abcdef #xfedcba9876543210)
       (u64vector-swap-bytes
        '#u64(#xefcdab8967452301 #x1032547698badcfe)))

;;-------------------------------------------------------------------
(test-section "collection interface")

(use gauche.collection)
(use gauche.sequence)

(define (num-equal? l1 l2)
  (cond [(pair? l1)
         (if (pair? l2)
             (and (num-equal? (car l1) (car l2))
                  (num-equal? (cdr l1) (cdr l2)))
             #f)]
        [(vector? l1)
         (if (vector? l2)
             (with-iterator (l1 end1 next1)
               (with-iterator (l2 end2 next2)
                 (let loop ()
                   (if (end1)
                       (if (end2) #t #f)
                       (and (num-equal? (next1) (next2))
                            (loop)))))))]
        [(number? l1)
         (if (number? l2)
             (= l1 l2)
             #f)]
        [else (equal? l1 l2)]))

(define (collection-tester class vec)
  (and (=      (fold + 0 vec) 10)
       (num-equal? (map identity vec) '(1 2 3 4))
       (=      (find (^e (= e 2)) vec) 2)
       (equal? (coerce-to class '(1 2 3 4)) vec)
       (=      (ref vec 2) 3)
       (num-equal? (begin (set! (ref vec 1) 0)
                          (coerce-to <list> vec))
                   '(1 0 3 4))
       (num-equal? (coerce-to <vector> (subseq vec 1 3)) '#(0 3))))

(test* "s8vector collection interface" #t
       (collection-tester <s8vector> (s8vector 1 2 3 4)))
(test* "u8vector collection interface" #t
       (collection-tester <u8vector> (u8vector 1 2 3 4)))
(test* "s16vector collection interface" #t
       (collection-tester <s16vector> (s16vector 1 2 3 4)))
(test* "u16vector collection interface" #t
       (collection-tester <u16vector> (u16vector 1 2 3 4)))
(test* "s32vector collection interface" #t
       (collection-tester <s32vector> (s32vector 1 2 3 4)))
(test* "u32vector collection interface" #t
       (collection-tester <u32vector> (u32vector 1 2 3 4)))
(test* "s64vector collection interface" #t
       (collection-tester <s64vector> (s64vector 1 2 3 4)))
(test* "u64vector collection interface" #t
       (collection-tester <u64vector> (u64vector 1 2 3 4)))
(test* "f16vector collection interface" #t
       (collection-tester <f16vector> (f16vector 1 2 3 4)))
(test* "f32vector collection interface" #t
       (collection-tester <f32vector> (f32vector 1 2 3 4)))
(test* "f64vector collection interface" #t
       (collection-tester <f64vector> (f64vector 1 2 3 4)))

;;-------------------------------------------------------------------
(test-section "arithmetic operations")

;; there are too many combinations to write down by hand.
;;
;; for each opertaion in add, sub, mul
;;   for each testdata such that:
;;     - vector vs vector normal
;;     - vector vs vector underflow
;;     - vector vs vector overflow
;;     - vector vs smallint normal
;;     - vector vs smallint underflow
;;     - vector vs smallint overflow
;;     - vector vs bignum normal
;;     - vector vs bignum underflow
;;     - vector vs bignum overflow
;;

(define *bounds*
  `((s8   ,(- (expt 2  7)) ,(- (expt 2  7) 1))
    (u8   0                ,(- (expt 2  8) 1))
    (s16  ,(- (expt 2 15)) ,(- (expt 2 15) 1))
    (u16  0                ,(- (expt 2 16) 1))
    (s32  ,(- (expt 2 31)) ,(- (expt 2 31) 1))
    (u32  0                ,(- (expt 2 32) 1))
    (s64  ,(- (expt 2 63)) ,(- (expt 2 63) 1))
    (u64  0                ,(- (expt 2 64) 1))
    ))

(define (tag->min tag) (cadr  (assq tag *bounds*)))
(define (tag->max tag) (caddr (assq tag *bounds*)))

(define-macro (arith-test-generate tag)
  `(arith-test ',tag ,(tag->min tag) ,(tag->max tag)
               ,(string->symbol #"~|tag|vector")
               ,(string->symbol #"~|tag|vector-add")
               ,(string->symbol #"~|tag|vector-sub")
               ,(string->symbol #"~|tag|vector-mul")))

(define (arith-test tag min max make add sub mul)
  (define v0 (make 0 1 2 3))
  (define v1 (make 4 5 6 7))
  (define v2 (make (- max 2) (- max 2) (- max 2) (- max 2)))
  (define v3 (make (+ min 2) (+ min 2) (+ min 2) (+ min 2)))
  (define v0+v1 (make 4 6 8 10))
  (define v1-v0 (make 4 4 4 4))
  (define v0*v1 (make 0 5 12 21))
  (define v0*2  (make 0 2 4 6))
  (define v0+v2 (make (- max 2) (- max 1) max max))
  (define v0*v2 (make 0 (- max 2) max max))
  (define v0-min-1 (make min min (+ min 1) (+ min 2)))
  (define v3-v0    (make (+ min 2) (+ min 1) min min))
  (define v0*v3 (make 0 (+ min 2) min min))
  (define vmin (make min min min min))
  (define vmax (make max max max max))
  (define big32  #xffffffff)
  (define big64  #xffffffffffffffff)

  (define (gen-tester op v0 v1)
    (define (safe-test clamp-flag)
      (with-error-handler (^e 'error)
        (^[] (op v0 v1 clamp-flag))))
    (^[] (list (safe-test #f)
               (safe-test 'high)
               (safe-test 'low)
               (safe-test 'both))))

  (define (result-normal v) (list v v v v))
  (define (result-hi-ok  v) (list 'error v 'error v))
  (define (result-lo-ok  v) (list 'error 'error v v))

  ;; Add
  (test (format #f "~avector-add (v+v)" tag) (result-normal v0+v1)
        (gen-tester add v0 v1))
  (test (format #f "~avector-add (v+v)" tag) (result-hi-ok v0+v2)
        (gen-tester add v0 v2))
  (test (format #f "~avector-add (v+l)" tag) (result-normal v0+v1)
        (gen-tester add v0 (coerce-to <list> v1)))
  (test (format #f "~avector-add (v+vv)" tag) (result-normal v0+v1)
        (gen-tester add v0 (coerce-to <vector> v1)))
  (test (format #f "~avector-add (v+s)" tag) (result-normal v1)
        (gen-tester add v0 4))
  (test (format #f "~avector-add (v+s)" tag) (result-hi-ok v0+v2)
        (gen-tester add v0 (- max 2)))
  (test (format #f "~avector-add (v+s)" tag) (result-lo-ok v0-min-1)
        (gen-tester add v0 (- min 1)))
  (test (format #f "~avector-add (v+b)" tag)
        (case tag
          [(s64 u64)
           (result-normal (make big32 (+ big32 1) (+ big32 2) (+ big32 3)))]
          [else (result-hi-ok vmax)])
        (gen-tester add v0 big32))
  (test (format #f "~avector-add (v+b)" tag)
        (case tag
          [(s64)
           (result-normal (make (- big32) (- 1 big32) (- 2 big32) (- 3 big32)))]
          [else (result-lo-ok vmin)])
        (gen-tester add v0 (- big32)))
  (test (format #f "~avector-add (v+b)" tag) (result-hi-ok vmax)
        (gen-tester add v0 big64))
  (test (format #f "~avector-add (v+b)" tag) (result-lo-ok vmin)
        (gen-tester add v0 (- big64)))

  ;; Sub
  (test (format #f "~avector-sub (v-v)" tag) (result-normal v1-v0)
        (gen-tester sub v1 v0))
  (test (format #f "~avector-sub (v-v)" tag) (result-lo-ok  v3-v0)
        (gen-tester sub v3 v0))
  (test (format #f "~avector-sub (v-l)" tag) (result-normal v1-v0)
        (gen-tester sub v1 (coerce-to <list> v0)))
  (test (format #f "~avector-sub (v-vv)" tag) (result-normal v1-v0)
        (gen-tester sub v1 (coerce-to <vector> v0)))
  (test (format #f "~avector-sub (v-s)" tag) (result-normal v0)
        (gen-tester sub v1 4))
  (test (format #f "~avector-sub (v-s)" tag) (result-lo-ok v0-min-1)
        (gen-tester sub v0 (- (- min 1))))
  (test (format #f "~avector-sub (v-b)" tag)
        (case tag
          [(s64) (result-normal (make (- big32) (- 1 big32) (- 2 big32) (- 3 big32)))]
          [else  (result-lo-ok vmin)])
        (gen-tester sub v0 big32))
  (test (format #f "~avector-sub (v-b)" tag)
        (case tag
          [(s64 u64)
           (result-normal (make big32 (+ big32 1) (+ big32 2) (+ big32 3)))]
          [else (result-hi-ok vmax)])
        (gen-tester sub v0 (- big32)))
  (test (format #f "~avector-sub (v-b)" tag) (result-lo-ok vmin)
        (gen-tester sub v0 big64))
  (test (format #f "~avector-sub (v-b)" tag) (result-hi-ok vmax)
        (gen-tester sub v0 (- big64)))

  ;; Mul
  (test (format #f "~avector-mul (v*v)" tag) (result-normal v0*v1)
        (gen-tester mul v0 v1))
  (test (format #f "~avector-mul (v*v)" tag) (result-hi-ok v0*v2)
        (gen-tester mul v0 v2))
  (test (format #f "~avector-mul (v*l)" tag) (result-normal v0*v1)
        (gen-tester mul v0 (coerce-to <list> v1)))
  (test (format #f "~avector-mul (v*vv)" tag) (result-normal v0*v1)
        (gen-tester mul v0 (coerce-to <vector> v1)))
  (unless (memq tag '(u8 u16 u32 u64))
    (test (format #f "~avector-mul (v*v)" tag) (result-lo-ok v0*v3)
          (gen-tester mul v0 v3)))
  (test (format #f "~avector-mul (v*s)" tag) (result-normal v0*2)
        (gen-tester mul v0 2))
  (test (format #f "~avector-mul (v*s)" tag)
        (if (memq tag '(s8 s16 s32 s64))
          (result-normal (make 0 -2 -4 -6))
          (result-lo-ok  vmin))
        (gen-tester mul v0 -2))
  (test (format #f "~avector-mul (v*s)" tag)
        (result-hi-ok (make 0 (- max 1) max max))
        (gen-tester mul v0 (- max 1)))
  (test (format #f "~avector-mul (v*s)" tag)
        (case tag
          [(s8 s16 s32 s64) (result-lo-ok (make 0 (- (- max 1)) min min))]
          [else (result-lo-ok  vmin)])
        (gen-tester mul v0 (- (- max 1))))
  (test (format #f "~avector-mul (v*b)" tag)
        (case tag
          [(s64 u64) (result-normal (make 0 big32 (* big32 2) (* big32 3)))]
          [else (result-hi-ok  (make 0 max max max))])
        (gen-tester mul v0 big32))
  (test (format #f "~avector-mul (v*b)" tag)
        (case tag
          [(s64)
           (result-normal (make 0 (- big32) (- (* big32 2))  (- (* big32 3))))]
          [else (result-lo-ok  (make 0 min min min))])
        (gen-tester mul v0 (- big32)))
  )

(arith-test-generate s8)
(arith-test-generate u8)
(arith-test-generate s16)
(arith-test-generate u16)
(arith-test-generate s32)
(arith-test-generate u32)
(arith-test-generate s64)
(arith-test-generate u64)

;; flonum vectors; no clamping, so it's a bit simple
(define-macro (flonum-arith-test-generate tag)
  `(flonum-arith-test ',tag
                      ,(string->symbol #"~|tag|vector")
                      ,(string->symbol #"~|tag|vector-add")
                      ,(string->symbol #"~|tag|vector-sub")
                      ,(string->symbol #"~|tag|vector-mul")
                      ,(string->symbol #"~|tag|vector-div")
                      ))

(define (flonum-arith-test tag make add sub mul div)
  (test* (format #f "~svector-add (v+v)" tag)
         (make 4.0 6.0 8.0 10.0)
         (add (make 0.0 1.0 2.0 3.0) (make 4.0 5.0 6.0 7.0)))
  (test* (format #f "~svector-add (v+s)" tag)
         (make 4.0 5.0 6.0 7.0)
         (add (make 0.0 1.0 2.0 3.0) 4.0))
  (test* (format #f "~svector-sub (v-v)" tag)
         (make -4.0 -4.0 -4.0 -4.0)
         (sub (make 0.0 1.0 2.0 3.0) (make 4.0 5.0 6.0 7.0)))
  (test* (format #f "~svector-sub (v-s)" tag)
         (make -4.0 -3.0 -2.0 -1.0)
         (sub (make 0.0 1.0 2.0 3.0) 4.0))
  (test* (format #f "~svector-mul (v*v)" tag)
         (make 0.0 5.0 12.0 21.0)
         (mul (make 0.0 1.0 2.0 3.0) (make 4.0 5.0 6.0 7.0)))
  (test* (format #f "~svector-mul (v*s)" tag)
         (make 0.0 5.0 10.0 15.0)
         (mul (make 0.0 1.0 2.0 3.0) 5.0))
  (test* (format #f "~svector-div (v/v)" tag)
         (make 0.0 0.5 0.5 0.375)
         (div (make 0.0 1.0 2.0 3.0) (make 1.0 2.0 4.0 8.0)))
  (test* (format #f "~svector-div (v/v)" tag)
         (make 0.0 0.5 1.0 1.5)
         (div (make 0.0 1.0 2.0 3.0) 2.0))
  )

(flonum-arith-test-generate f16)
(flonum-arith-test-generate f32)
(flonum-arith-test-generate f64)

;;-------------------------------------------------------------------
(test-section "bitwise operations")

(define (bit-test tag v0 v1 s0 s1 ->list list-> and ior xor)
  (define (tests opname op logop)
    (test* (format #f "~svector-~s ~s ~s" tag opname v0 v1)
           (list-> (map logop (->list v0) (->list v1)))
           (op v0 v1))
    (test* (format #f "~svector-~s ~s ~s" tag opname v0 s0)
           (list-> (map (pa$ logop s0) (->list v0)))
           (op v0 s0))
    (test* (format #f "~svector-~s ~s ~s" tag opname v0 s1)
           (list-> (map (pa$ logop s1) (->list v0)))
           (op v0 s1)))
  (tests 'and and logand)
  (tests 'ior ior logior)
  (tests 'xor xor logxor))

(define-macro (bit-test-generate tag v0 v1 s0 s1)
  `(bit-test ',tag ',v0 ',v1 ,s0 ,s1
             ,(string->symbol #"~|tag|vector->list")
             ,(string->symbol #"list->~|tag|vector")
             ,(string->symbol #"~|tag|vector-and")
             ,(string->symbol #"~|tag|vector-ior")
             ,(string->symbol #"~|tag|vector-xor")))

(bit-test-generate s8
                   #s8(#x0f #x70 #x-0f #x-70)
                   #s8(#x55 #x2a #x-55 #x-2a)
                   #x55
                   #x-55)
(bit-test-generate u8
                   #u8(#x0f #x70 #xf0 #xcc)
                   #u8(#x55 #xaa #x5a #xa5)
                   #x55
                   #xaa)
(bit-test-generate s16
                   #s16(#x0fff #x7070 #x-0fff #x-7070)
                   #s16(#x3c3c #x-43c3 #x43c3 #x-3c3c)
                   #x55aa
                   #x-55aa)
(bit-test-generate u16
                   #u16(#x0fff #x7070 #xff00 #xc0c0)
                   #u16(#x3c3c #xc3c3 #x55aa #xaa55)
                   #x55aa
                   #x9696)
(bit-test-generate s32
                   #s32(#x0fffffff #x70707070 #x-0fffffff #x-70707070)
                   #s32(#x3c3c3c3c #x-43c3c3c3 #x43c3c3c3 #x-3c3c3c3c)
                   #x55aa55aa
                   #x-55aa55aa)
(bit-test-generate u32
                   #u32(#x0fffffff #x70707070 #xff00ff00 #xc0c0c0c0)
                   #u32(#x3c3c3c3c #xc3c3c3c3 #x55aa55aa #xaa55aa55)
                   #x55aa55aa
                   #x96966969)
(bit-test-generate s64
                   #s64(#x0fffffffffffffff #x7070707007070707
                                           #x-0fffffffffffffff #x-7070707007070707)
                   #s64(#x3c3c3c3cc3c3c3c3 #x-43c3c3c33c3c3c3c
                                           #x43c3c3c3c3c3c3c3 #x-3c3c3c3c3c3c3c3c)
                   #x55aa55aa55aa55aa
                   #x-55aa55aa55aa55aa)
(bit-test-generate u64
                   #u64(#x0fffffffffffffff #x70707070f0f0f0f0
                                           #xff00ff00ff00ff00 #xc0c0c0c003030303)
                   #u64(#x3c3c3c3c3c3c3c3c #xc3c3c3c3c3c3c3c3
                                           #x55aa55aa55aa55aa #xaa55aa55aa55aa55)
                   #x55aa55aa5a5a5a5a
                   #x9696696988778877)

;;-------------------------------------------------------------------
(test-section "dot product")

(define (dotprod-test tag v0 v1 dot)
  (let1 result (fold (^[e0 e1 sum] (+ sum (* e0 e1)))
                     0
                     (coerce-to <list> v0)
                     (coerce-to <list> v1))
    (test* (format #f "~svector-dot(~s, ~s)" tag v0 v1)
           result
           (dot v0 v1))
    (test* (format #f "~svector-dot(~s, ~s)" tag v0 (coerce-to <list> v1))
           result
           (dot v0 (coerce-to <list> v1)))
    (test* (format #f "~svector-dot(~s, ~s)" tag v0 (coerce-to <vector> v1))
           result
           (dot v0 (coerce-to <vector> v1)))
    ))

(define-macro (dotprod-test-generate tag v0 v1)
  `(dotprod-test ',tag ',v0 ',v1
                 ,(string->symbol #"~|tag|vector-dot")))

(dotprod-test-generate s8 #s8() #s8())
(dotprod-test-generate s8 #s8(0 1 2 3) #s8(4 5 6 7))
(dotprod-test-generate s8 #s8(0 -1 2 -3) #s8(-4 5 -6 7))
(dotprod-test-generate s8 #s8(127 127 127 127 127) #s8(127 127 127 127 127))
(dotprod-test-generate s8
                       #s8(-128 -128 -128 -128 -128)
                       #s8(127 127 127 127 127))
(dotprod-test-generate u8 #u8(0 1 2 3) #u8(4 5 6 7))
(dotprod-test-generate u8 #u8(255 255 255 255 255) #u8(255 255 255 255 255))

(dotprod-test-generate s16 #s16(0 1 2 3) #s16(4 5 6 7))
(dotprod-test-generate s16 #s16(0 -1 2 -3) #s16(-4 5 -6 7))
(dotprod-test-generate s16 #s16(16384 16384 16384 16384 16384)
                       #s16(16384 16384 16384 16384 16384))
(dotprod-test-generate s16 #s16(16384 -16384 16384 -16384 16384)
                       #s16(16384 -16384 16384 -16384 16384))
(dotprod-test-generate s16 #s16(32767 32767 32767 32767 32767)
                       #s16(32767 32767 32767 32767 32767))
(dotprod-test-generate s16 #s16(32767 1 2 3 4)
                       #s16(32767 1 2 3 4))
(dotprod-test-generate s16 #s16(1 2 3 4 32767)
                       #s16(1 2 3 4 32767))
(dotprod-test-generate s16 #s16(32767 -32767 32767 -32767 32767)
                       #s16(32767 32767 32767 32767 32767))
(dotprod-test-generate s16 #s16(-32768 -32768 -32768 -32768 -32768)
                       #s16(32767 32767 32767 32767 32767))
(dotprod-test-generate u16 #u16(0 1 2 3) #u16(4 5 6 7))
(dotprod-test-generate u16 #u16(16384 16384 16384 16384 16384)
                       #u16(16384 16384 16384 16384 16384))
(dotprod-test-generate u16 #u16(65535 65535 65535 65535 65535)
                       #u16(65535 65535 65535 65535 65535))
(dotprod-test-generate u16 #u16(32767 1 2 3 4)
                       #u16(32767 1 2 3 4))
(dotprod-test-generate u16 #u16(1 2 3 4 32767)
                       #u16(1 2 3 4 32767))

(dotprod-test-generate s32 #s32(0 1 2 3) #s32(4 5 6 7))
(dotprod-test-generate s32 #s32(0 -1 2 -3) #s32(-4 5 -6 7))
(dotprod-test-generate s32 #s32(16384 16384 16384 16384 16384)
                       #s32(16384 16384 16384 16384 16384))
(dotprod-test-generate s32 #s32(16384 -16384 16384 -16384 16384)
                       #s32(16384 -16384 16384 -16384 16384))
(dotprod-test-generate s32 #s32(32767 32767 32767 32767 32767)
                       #s32(32767 32767 32767 32767 32767))
(dotprod-test-generate s32 #s32(214748367 214748367 214748367 214748367 214748367)
                       #s32(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-generate s32 #s32(214748367 1 2 3 4)
                       #s32(214748367 1 2 3 4))
(dotprod-test-generate s32 #s32(1 2 3 4 214748367)
                       #s32(1 2 3 4 214748367))
(dotprod-test-generate s32 #s32(32767 -32767 32767 -32767 32767)
                       #s32(32767 32767 32767 32767 32767))
(dotprod-test-generate s32 #s32(214748367 -214748367 214748367 -214748367 214748367)
                       #s32(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-generate s32 #s32(-214748368 -214748368 -214748368 -214748368 -214748368)
                       #s32(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-generate u32 #u32(0 1 2 3) #u32(4 5 6 7))
(dotprod-test-generate u32 #u32(16384 16384 16384 16384 16384)
                       #u32(16384 16384 16384 16384 16384))
(dotprod-test-generate u32 #u32(4294967295 4294967295 4294967295 4294967295 4294967295)
                       #u32(4294967295 4294967295 4294967295 4294967295 4294967295))
(dotprod-test-generate u32 #u32(4294967295 1 2 3 4)
                       #u32(4294967295 1 2 3 4))
(dotprod-test-generate u32 #u32(1 2 3 4 4294967295)
                       #u32(1 2 3 4 4294967295))

(dotprod-test-generate s64 #s64(0 1 2 3) #s64(4 5 6 7))
(dotprod-test-generate s64 #s64(0 -1 2 -3) #s64(-4 5 -6 7))
(dotprod-test-generate s64 #s64(16384 16384 16384 16384 16384)
                       #s64(16384 16384 16384 16384 16384))
(dotprod-test-generate s64 #s64(16384 -16384 16384 -16384 16384)
                       #s64(16384 -16384 16384 -16384 16384))
(dotprod-test-generate s64 #s64(32767 32767 32767 32767 32767)
                       #s64(32767 32767 32767 32767 32767))
(dotprod-test-generate s64 #s64(214748367 214748367 214748367 214748367 214748367)
                       #s64(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-generate s64 #s64(9223372036854775807 1 2 3 4)
                       #s64(9223372036854775807 1 2 3 4))
(dotprod-test-generate s64 #s64(1 2 3 4 9223372036854775807 1)
                       #s64(1 2 3 4 9223372036854775807 1))
(dotprod-test-generate s64 #s64(32767 -32767 32767 -32767 32767)
                       #s64(32767 -32767 32767 -32767 32767))
(dotprod-test-generate s64 #s64(214748367 -214748367 214748367 -214748367 214748367)
                       #s64(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-generate s64 #s64(-214748368 -214748368 -214748368 -214748368 -214748368)
                       #s64(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-generate s64 #s64(9223372036854775807 -9223372036854775807 9223372036854775807 -9223372036854775807 9223372036854775807)
                       #s64(9223372036854775807 9223372036854775807 9223372036854775807 9223372036854775807 9223372036854775807))
(dotprod-test-generate s64 #s64(-9223372036854775808 -9223372036854775808 -9223372036854775808 -9223372036854775808 -9223372036854775808)
                       #s64(9223372036854775807 9223372036854775807 9223372036854775807 9223372036854775807 9223372036854775807))
(dotprod-test-generate u64 #u64(0 1 2 3) #u64(4 5 6 7))
(dotprod-test-generate u64 #u64(16384 16384 16384 16384 16384)
                       #u64(16384 16384 16384 16384 16384))
(dotprod-test-generate u64 #u64(18446744073709551615 18446744073709551615 18446744073709551615 18446744073709551615 18446744073709551615)
                       #u64(18446744073709551615 18446744073709551615 18446744073709551615 18446744073709551615 18446744073709551615))
(dotprod-test-generate u64 #u64(18446744073709551615 1 2 3 4)
                       #u64(18446744073709551615 1 2 3 4))
(dotprod-test-generate u64 #u64(1 2 3 4 18446744073709551615)
                       #u64(1 2 3 4 18446744073709551615))

(dotprod-test-generate f16 #f16(0 1 2 3) #f16(4 5 6 7))
(dotprod-test-generate f16 #f16(0 -1 2 -3) #f16(-4 5 -6 7))
(dotprod-test-generate f16 #f16(16384 16384 16384 16384 16384)
                       #f16(16384 16384 16384 16384 16384))
(dotprod-test-generate f16 #f16(16384 -16384 16384 -16384 16384)
                       #f16(16384 -16384 16384 -16384 16384))
(dotprod-test-generate f16 #f16(32767 32767 32767 32767 32767)
                       #f16(32767 32767 32767 32767 32767))
(dotprod-test-generate f16 #f16(214748367 214748367 214748367 214748367 214748367)
                       #f16(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-generate f16 #f16(9223372036854775807 1 2 3 4)
                       #f16(9223372036854775807 1 2 3 4))
(dotprod-test-generate f16 #f16(1 2 3 4 9223372036854775807 1)
                       #f16(1 2 3 4 9223372036854775807 1))
(dotprod-test-generate f16 #f16(32767 -32767 32767 -32767 32767)
                       #f16(32767 -32767 32767 -32767 32767))

(dotprod-test-generate f32 #f32(0 1 2 3) #f32(4 5 6 7))
(dotprod-test-generate f32 #f32(0 -1 2 -3) #f32(-4 5 -6 7))
(dotprod-test-generate f32 #f32(16384 16384 16384 16384 16384)
                       #f32(16384 16384 16384 16384 16384))
(dotprod-test-generate f32 #f32(16384 -16384 16384 -16384 16384)
                       #f32(16384 -16384 16384 -16384 16384))
(dotprod-test-generate f32 #f32(32767 32767 32767 32767 32767)
                       #f32(32767 32767 32767 32767 32767))
(dotprod-test-generate f32 #f32(214748367 214748367 214748367 214748367 214748367)
                       #f32(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-generate f32 #f32(9223372036854775807 1 2 3 4)
                       #f32(9223372036854775807 1 2 3 4))
(dotprod-test-generate f32 #f32(1 2 3 4 9223372036854775807 1)
                       #f32(1 2 3 4 9223372036854775807 1))
(dotprod-test-generate f32 #f32(32767 -32767 32767 -32767 32767)
                       #f32(32767 -32767 32767 -32767 32767))

(dotprod-test-generate f64 #f64(0 1 2 3) #f64(4 5 6 7))
(dotprod-test-generate f64 #f64(0 -1 2 -3) #f64(-4 5 -6 7))
(dotprod-test-generate f64 #f64(16384 16384 16384 16384 16384)
                       #f64(16384 16384 16384 16384 16384))
(dotprod-test-generate f64 #f64(16384 -16384 16384 -16384 16384)
                       #f64(16384 -16384 16384 -16384 16384))
(dotprod-test-generate f64 #f64(32767 32767 32767 32767 32767)
                       #f64(32767 32767 32767 32767 32767))
(dotprod-test-generate f64 #f64(214748367 214748367 214748367 214748367 214748367)
                       #f64(214748367 214748367 214748367 214748367 214748367))
(dotprod-test-generate f64 #f64(9223372036854775807 1 2 3 4)
                       #f64(9223372036854775807 1 2 3 4))
(dotprod-test-generate f64 #f64(1 2 3 4 9223372036854775807 1)
                       #f64(1 2 3 4 9223372036854775807 1))
(dotprod-test-generate f64 #f64(32767 -32767 32767 -32767 32767)
                       #f64(32767 -32767 32767 -32767 32767))

;;-------------------------------------------------------------------
(test-section "range-check")

(define-macro (range-test-generate tag v min max result)
  `(test (format #f "~svector-range-check" ',tag) ,result
         (^[]
           (,(string->symbol #"~|tag|vector-range-check") ',v ',min ',max)))
  )

(range-test-generate s8 #s8(-4 -2 0 2 4) #f #f #f)
(range-test-generate s8 #s8(-4 -2 0 2 4) -4 4 #f)
(range-test-generate s8 #s8(-4 -2 0 2 4) -4 3 4)
(range-test-generate s8 #s8(-4 -2 0 2 4) -4 0 3)
(range-test-generate s8 #s8(-4 -2 0 2 4) -3 4 0)
(range-test-generate s8 #s8(-4 -2 0 2 4) 0 4 0)
(range-test-generate s8 #s8(-4 -2 0 2 4) #x-ffffffffffffffff #f #f)
(range-test-generate s8 #s8(-4 -2 0 2 4) #f #xffffffffffffffff  #f)
(range-test-generate s8 #s8(-4 -2 0 2 4) #xffffffffffffffff #f  0)
(range-test-generate s8 #s8(-4 -2 0 2 4) #f #x-ffffffffffffffff 0)
(range-test-generate s8 #s8(-4 -2 0 2 4) #s8(-4 -3 -2 -1 0) #s8(0 1 2 3 4) #f)
(range-test-generate s8 #s8(-4 -2 0 2 4) #s8(-4 -1 -2 -1 0) #s8(0 1 2 3 4) 1)
(range-test-generate s8 #s8(-4 -2 0 2 4) #s8(-4 -3 -2 -1 0) #s8(0 1 -1 3 4) 2)

(range-test-generate u8 #u8(0 2 4 6 9) #f #f #f)
(range-test-generate u8 #u8(0 2 4 6 9) -4 9 #f)
(range-test-generate u8 #u8(0 2 4 6 9) -4 8 4)
(range-test-generate u8 #u8(0 2 4 6 9) -4 4 3)
(range-test-generate u8 #u8(0 2 4 6 9) 3 4 0)
(range-test-generate u8 #u8(0 2 4 6 9) #x-ffffffffffffffff #f #f)
(range-test-generate u8 #u8(0 2 4 6 9) #f #xffffffffffffffff  #f)
(range-test-generate u8 #u8(0 2 4 6 9) #xffffffffffffffff #f  0)
(range-test-generate u8 #u8(0 2 4 6 9) #f #x-ffffffffffffffff 1)
(range-test-generate u8 #u8(0 2 4 6 9) #u8(0 1 2 3 4) #u8(5 6 7 8 9) #f)
(range-test-generate u8 #u8(0 2 4 6 9) #u8(0 1 5 3 4) #u8(5 6 7 8 9) 2)
(range-test-generate u8 #u8(0 2 4 6 9) #u8(0 1 2 3 4) #u8(5 6 7 5 6) 3)

(range-test-generate s16 #s16(-4 -2 0 2 4) #f #f #f)
(range-test-generate s16 #s16(-4 -2 0 2 4) -4 4 #f)
(range-test-generate s16 #s16(-4 -2 0 2 4) -4 3 4)
(range-test-generate s16 #s16(-4 -2 0 2 4) -4 0 3)
(range-test-generate s16 #s16(-4 -2 0 2 4) -3 4 0)
(range-test-generate s16 #s16(-4 -2 0 2 4) 0 4 0)
(range-test-generate s16 #s16(-4 -2 0 2 4) #x-ffffffffffffffff #f #f)
(range-test-generate s16 #s16(-4 -2 0 2 4) #f #xffffffffffffffff  #f)
(range-test-generate s16 #s16(-4 -2 0 2 4) #xffffffffffffffff #f  0)
(range-test-generate s16 #s16(-4 -2 0 2 4) #f #x-ffffffffffffffff 0)
(range-test-generate s16 #s16(-4 -2 0 2 4) #s16(-4 -3 -2 -1 0) #s16(0 1 2 3 4) #f)
(range-test-generate s16 #s16(-4 -2 0 2 4) #s16(-4 -1 -2 -1 0) #s16(0 1 2 3 4) 1)
(range-test-generate s16 #s16(-4 -2 0 2 4) #s16(-4 -3 -2 -1 0) #s16(0 1 -1 3 4) 2)

(range-test-generate u16 #u16(0 2 4 6 9) #f #f #f)
(range-test-generate u16 #u16(0 2 4 6 9) -4 9 #f)
(range-test-generate u16 #u16(0 2 4 6 9) -4 8 4)
(range-test-generate u16 #u16(0 2 4 6 9) -4 4 3)
(range-test-generate u16 #u16(0 2 4 6 9) 3 4 0)
(range-test-generate u16 #u16(0 2 4 6 9) #x-ffffffffffffffff #f #f)
(range-test-generate u16 #u16(0 2 4 6 9) #f #xffffffffffffffff  #f)
(range-test-generate u16 #u16(0 2 4 6 9) #xffffffffffffffff #f  0)
(range-test-generate u16 #u16(0 2 4 6 9) #f #x-ffffffffffffffff 1)
(range-test-generate u16 #u16(0 2 4 6 9) #u16(0 1 2 3 4) #u16(5 6 7 8 9) #f)
(range-test-generate u16 #u16(0 2 4 6 9) #u16(0 1 5 3 4) #u16(5 6 7 8 9) 2)
(range-test-generate u16 #u16(0 2 4 6 9) #u16(0 1 2 3 4) #u16(5 6 7 5 6) 3)

(range-test-generate s32 #s32(-4 -2 0 2 4) #f #f #f)
(range-test-generate s32 #s32(-4 -2 0 2 4) -4 4 #f)
(range-test-generate s32 #s32(-4 -2 0 2 4) -4 3 4)
(range-test-generate s32 #s32(-4 -2 0 2 4) -4 0 3)
(range-test-generate s32 #s32(-4 -2 0 2 4) -3 4 0)
(range-test-generate s32 #s32(-4 -2 0 2 4) 0 4 0)
(range-test-generate s32 #s32(-4 -2 0 2 4) #x-ffffffffffffffff #f #f)
(range-test-generate s32 #s32(-4 -2 0 2 4) #f #xffffffffffffffff  #f)
(range-test-generate s32 #s32(-4 -2 0 2 4) #xffffffffffffffff #f  0)
(range-test-generate s32 #s32(-4 -2 0 2 4) #f #x-ffffffffffffffff 0)
(range-test-generate s32 #s32(-4 -2 0 2 4) #s32(-4 -3 -2 -1 0) #s32(0 1 2 3 4) #f)
(range-test-generate s32 #s32(-4 -2 0 2 4) #s32(-4 -1 -2 -1 0) #s32(0 1 2 3 4) 1)
(range-test-generate s32 #s32(-4 -2 0 2 4) #s32(-4 -3 -2 -1 0) #s32(0 1 -1 3 4) 2)

(range-test-generate u32 #u32(0 2 4 6 9) #f #f #f)
(range-test-generate u32 #u32(0 2 4 6 9) -4 9 #f)
(range-test-generate u32 #u32(0 2 4 6 9) -4 8 4)
(range-test-generate u32 #u32(0 2 4 6 9) -4 4 3)
(range-test-generate u32 #u32(0 2 4 6 9) 3 4 0)
(range-test-generate u32 #u32(0 2 4 6 9) #x-ffffffffffffffff #f #f)
(range-test-generate u32 #u32(0 2 4 6 9) #f #xffffffffffffffff  #f)
(range-test-generate u32 #u32(0 2 4 6 9) #xffffffffffffffff #f  0)
(range-test-generate u32 #u32(0 2 4 6 9) #f #x-ffffffffffffffff 1)
(range-test-generate u32 #u32(0 2 4 6 9) #u32(0 1 2 3 4) #u32(5 6 7 8 9) #f)
(range-test-generate u32 #u32(0 2 4 6 9) #u32(0 1 5 3 4) #u32(5 6 7 8 9) 2)
(range-test-generate u32 #u32(0 2 4 6 9) #u32(0 1 2 3 4) #u32(5 6 7 5 6) 3)

(range-test-generate s64 #s64(-4 -2 0 2 4) #f #f #f)
(range-test-generate s64 #s64(-4 -2 0 2 4) -4 4 #f)
(range-test-generate s64 #s64(-4 -2 0 2 4) -4 3 4)
(range-test-generate s64 #s64(-4 -2 0 2 4) -4 0 3)
(range-test-generate s64 #s64(-4 -2 0 2 4) -3 4 0)
(range-test-generate s64 #s64(-4 -2 0 2 4) 0 4 0)
(range-test-generate s64 #s64(-4 -2 0 2 4) #x-ffffffffffffffff #f #f)
(range-test-generate s64 #s64(-4 -2 0 2 4) #f #xffffffffffffffff  #f)
(range-test-generate s64 #s64(-4 -2 0 2 4) #xffffffffffffffff #f  0)
(range-test-generate s64 #s64(-4 -2 0 2 4) #f #x-ffffffffffffffff 0)
(range-test-generate s64 #s64(-4 -2 0 2 4) #s64(-4 -3 -2 -1 0) #s64(0 1 2 3 4) #f)
(range-test-generate s64 #s64(-4 -2 0 2 4) #s64(-4 -1 -2 -1 0) #s64(0 1 2 3 4) 1)
(range-test-generate s64 #s64(-4 -2 0 2 4) #s64(-4 -3 -2 -1 0) #s64(0 1 -1 3 4) 2)

(range-test-generate u64 #u64(0 2 4 6 9) #f #f #f)
(range-test-generate u64 #u64(0 2 4 6 9) -4 9 #f)
(range-test-generate u64 #u64(0 2 4 6 9) -4 8 4)
(range-test-generate u64 #u64(0 2 4 6 9) -4 4 3)
(range-test-generate u64 #u64(0 2 4 6 9) 3 4 0)
(range-test-generate u64 #u64(0 2 4 6 9) #x-ffffffffffffffff #f #f)
(range-test-generate u64 #u64(0 2 4 6 9) #f #xffffffffffffffff  #f)
(range-test-generate u64 #u64(0 2 4 6 9) #xffffffffffffffff #f  0)
(range-test-generate u64 #u64(0 2 4 6 9) #f #x-ffffffffffffffff 1)
(range-test-generate u64 #u64(0 2 4 6 9) #u64(0 1 2 3 4) #u64(5 6 7 8 9) #f)
(range-test-generate u64 #u64(0 2 4 6 9) #u64(0 1 5 3 4) #u64(5 6 7 8 9) 2)
(range-test-generate u64 #u64(0 2 4 6 9) #u64(0 1 2 3 4) #u64(5 6 7 5 6) 3)

(range-test-generate f32 #f32(-4.0 -2.0 0.0 2.0 4.0) #f #f #f)
(range-test-generate f32 #f32(-4.0 -2.0 0.0 2.0 4.0) -4.0 4.0 #f)
(range-test-generate f32 #f32(-4.0 -2.0 0.0 2.0 4.0) -4.0 3.0 4)
(range-test-generate f32 #f32(-4.0 -2.0 0.0 2.0 4.0) -4.0 0.0 3)
(range-test-generate f32 #f32(-4.0 -2.0 0.0 2.0 4.0) -3.0 4.0 0)
(range-test-generate f32 #f32(-4.0 -2.0 0.0 2.0 4.0) 0.0 4.0 0)
(range-test-generate f32 #f32(-4.0 -2.0 0.0 2.0 4.0) #f32(-4.0 -3.0 -2.0 -1.0 0.0) #f32(0.0 1.0 2.0 3.0 4.0) #f)
(range-test-generate f32 #f32(-4.0 -2.0 0.0 2.0 4.0) #f32(-4.0 -1.0 -2.0 -1.0 0.0) #f32(0.0 1.0 2.0 3.0 4.0) 1)
(range-test-generate f32 #f32(-4.0 -2.0 0.0 2.0 4.0) #f32(-4.0 -3.0 -2.0 -1.0 0.0) #f32(0.0 1.0 -1.0 3.0 4.0) 2)

(range-test-generate f64 #f64(-4.0 -2.0 0.0 2.0 4.0) #f #f #f)
(range-test-generate f64 #f64(-4.0 -2.0 0.0 2.0 4.0) -4.0 4.0 #f)
(range-test-generate f64 #f64(-4.0 -2.0 0.0 2.0 4.0) -4.0 3.0 4)
(range-test-generate f64 #f64(-4.0 -2.0 0.0 2.0 4.0) -4.0 0.0 3)
(range-test-generate f64 #f64(-4.0 -2.0 0.0 2.0 4.0) -3.0 4.0 0)
(range-test-generate f64 #f64(-4.0 -2.0 0.0 2.0 4.0) 0.0 4.0 0)
(range-test-generate f64 #f64(-4.0 -2.0 0.0 2.0 4.0) #f64(-4.0 -3.0 -2.0 -1.0 0.0) #f64(0.0 1.0 2.0 3.0 4.0) #f)
(range-test-generate f64 #f64(-4.0 -2.0 0.0 2.0 4.0) #f64(-4.0 -1.0 -2.0 -1.0 0.0) #f64(0.0 1.0 2.0 3.0 4.0) 1)
(range-test-generate f64 #f64(-4.0 -2.0 0.0 2.0 4.0) #f64(-4.0 -3.0 -2.0 -1.0 0.0) #f64(0.0 1.0 -1.0 3.0 4.0) 2)

;;-------------------------------------------------------------------
(test-section "clamp")

(define (clamp-test tag class tagvector? tagvector-ref tagvector-length
                    clamp v minv maxv)
  (define (clamp-min index v)
    (cond [(tagvector? minv)
           (max v (tagvector-ref minv index) (tag->min tag))]
          [(not minv) (max v (tag->min tag))]
          [else (max v minv (tag->min tag))]))
  (define (clamp-max index v)
    (cond [(tagvector? maxv)
           (min v (tagvector-ref maxv index) (tag->max tag))]
          [(not maxv) (min v (tag->max tag))]
          [else (min v maxv (tag->max tag))]))
  (let1 result (map-to class
                       (^i (clamp-min i (clamp-max i (tagvector-ref v i))))
                       (iota (tagvector-length v)))
    (test* (format #f "~svector-clamp" tag)
           result
           (clamp v minv maxv))
    (when (or (tagvector? minv) (tagvector? maxv))
      (test* (format #f "~svector-clamp (list)" tag)
             result
             (clamp v
                    (if (tagvector? minv)
                        (coerce-to <list> minv)
                        minv)
                    (if (tagvector? maxv)
                        (coerce-to <list> maxv)
                        maxv)))
      (test* (format #f "~svector-clamp (vector)" tag)
             result
             (clamp v
                    (if (tagvector? minv)
                        (coerce-to <vector> minv)
                        minv)
                    (if (tagvector? maxv)
                        (coerce-to <vector> maxv)
                        maxv)))
      )
    ))

(define-macro (clamp-test-generate tag v minv maxv)
  `(clamp-test ',tag ,(string->symbol #"<~|tag|vector>")
               ,(string->symbol #"~|tag|vector?")
               ,(string->symbol #"~|tag|vector-ref")
               ,(string->symbol #"~|tag|vector-length")
               ,(string->symbol #"~|tag|vector-clamp")
               ',v ',minv ',maxv))

(clamp-test-generate s8 #s8(0 -127 -4 4 127) #f #f)
(clamp-test-generate s8 #s8(0 -127 -4 4 127) -8 #f)
(clamp-test-generate s8 #s8(0 -127 -4 4 127) 0 #f)
(clamp-test-generate s8 #s8(0 -127 -4 4 127) 8 #f)
(clamp-test-generate s8 #s8(0 -127 -4 4 127) #f -8)
(clamp-test-generate s8 #s8(0 -127 -4 4 127) #f 0)
(clamp-test-generate s8 #s8(0 -127 -4 4 127) #f 8)
(clamp-test-generate s8 #s8(0 -127 -4 4 127) -1 1)
(clamp-test-generate s8 #s8(0 -127 -4 4 127)
                     #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-generate s8 #s8(0 -127 -4 4 127)
                     0 #xffffffffffffffffffffffffffffff)
(clamp-test-generate s8 #s8(0 -127 -4 4 127)
                     #s8(-3 -4 -6 -8 -19) #s8(3 7 9 2 4))

(clamp-test-generate u8 #u8(127 0 4 200 255) #f #f)
(clamp-test-generate u8 #u8(127 0 4 200 255) -4 #f)
(clamp-test-generate u8 #u8(127 0 4 200 255) 0 #f)
(clamp-test-generate u8 #u8(127 0 4 200 255) 199 #f)
(clamp-test-generate u8 #u8(127 0 4 200 255) #f -4)
(clamp-test-generate u8 #u8(127 0 4 200 255) #f 0)
(clamp-test-generate u8 #u8(127 0 4 200 255) #f 199)
(clamp-test-generate u8 #u8(127 0 4 200 255)
                     #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-generate u8 #u8(127 0 4 200 255)
                     0 #xffffffffffffffffffffffffffffff)
(clamp-test-generate u8 #u8(127 0 4 200 255)
                     #u8(3 3 3 3 3) #u8(199 199 199 199 199))

(clamp-test-generate s16 #s16(0 -127 -4 4 127) #f #f)
(clamp-test-generate s16 #s16(0 -127 -4 4 127) -8 #f)
(clamp-test-generate s16 #s16(0 -127 -4 4 127) 0 #f)
(clamp-test-generate s16 #s16(0 -127 -4 4 127) 8 #f)
(clamp-test-generate s16 #s16(0 -127 -4 4 127) #f -8)
(clamp-test-generate s16 #s16(0 -127 -4 4 127) #f 0)
(clamp-test-generate s16 #s16(0 -127 -4 4 127) #f 8)
(clamp-test-generate s16 #s16(0 -127 -4 4 127) -1 1)
(clamp-test-generate s16 #s16(0 -127 -4 4 127)
                     #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-generate s16 #s16(0 -127 -4 4 127)
                     0 #xffffffffffffffffffffffffffffff)
(clamp-test-generate s16 #s16(0 -127 -4 4 127)
                     #s16(-3 -4 -6 -8 -19) #s16(3 7 9 2 4))

(clamp-test-generate u16 #u16(127 0 4 200 255) #f #f)
(clamp-test-generate u16 #u16(127 0 4 200 255) -4 #f)
(clamp-test-generate u16 #u16(127 0 4 200 255) 0 #f)
(clamp-test-generate u16 #u16(127 0 4 200 255) 199 #f)
(clamp-test-generate u16 #u16(127 0 4 200 255) #f -4)
(clamp-test-generate u16 #u16(127 0 4 200 255) #f 0)
(clamp-test-generate u16 #u16(127 0 4 200 255) #f 199)
(clamp-test-generate u16 #u16(127 0 4 200 255)
                     #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-generate u16 #u16(127 0 4 200 255)
                     0 #xffffffffffffffffffffffffffffff)
(clamp-test-generate u16 #u16(127 0 4 200 255)
                     #u16(3 3 3 3 3) #u16(199 199 199 199 199))

(clamp-test-generate s32 #s32(0 -127 -4 4 127) #f #f)
(clamp-test-generate s32 #s32(0 -127 -4 4 127) -8 #f)
(clamp-test-generate s32 #s32(0 -127 -4 4 127) 0 #f)
(clamp-test-generate s32 #s32(0 -127 -4 4 127) 8 #f)
(clamp-test-generate s32 #s32(0 -127 -4 4 127) #f -8)
(clamp-test-generate s32 #s32(0 -127 -4 4 127) #f 0)
(clamp-test-generate s32 #s32(0 -127 -4 4 127) #f 8)
(clamp-test-generate s32 #s32(0 -127 -4 4 127) -1 1)
(clamp-test-generate s32 #s32(0 -127 -4 4 127)
                     #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-generate s32 #s32(0 -127 -4 4 127)
                     0 #xffffffffffffffffffffffffffffff)
(clamp-test-generate s32 #s32(0 -127 -4 4 127)
                     #s32(-3 -4 -6 -8 -19) #s32(3 7 9 2 4))

(clamp-test-generate u32 #u32(127 0 4 200 255) #f #f)
(clamp-test-generate u32 #u32(127 0 4 200 255) -4 #f)
(clamp-test-generate u32 #u32(127 0 4 200 255) 0 #f)
(clamp-test-generate u32 #u32(127 0 4 200 255) 199 #f)
(clamp-test-generate u32 #u32(127 0 4 200 255) #f -4)
(clamp-test-generate u32 #u32(127 0 4 200 255) #f 0)
(clamp-test-generate u32 #u32(127 0 4 200 255) #f 199)
(clamp-test-generate u32 #u32(127 0 4 200 255)
                     #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-generate u32 #u32(127 0 4 200 255)
                     0 #xffffffffffffffffffffffffffffff)
(clamp-test-generate u32 #u32(127 0 4 200 255)
                     #u32(3 3 3 3 3) #u32(199 199 199 199 199))

(clamp-test-generate s64 #s64(0 -127 -4 4 127) #f #f)
(clamp-test-generate s64 #s64(0 -127 -4 4 127) -8 #f)
(clamp-test-generate s64 #s64(0 -127 -4 4 127) 0 #f)
(clamp-test-generate s64 #s64(0 -127 -4 4 127) 8 #f)
(clamp-test-generate s64 #s64(0 -127 -4 4 127) #f -8)
(clamp-test-generate s64 #s64(0 -127 -4 4 127) #f 0)
(clamp-test-generate s64 #s64(0 -127 -4 4 127) #f 8)
(clamp-test-generate s64 #s64(0 -127 -4 4 127) -1 1)
(clamp-test-generate s64 #s64(0 -127 -4 4 127)
                     #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-generate s64 #s64(0 -127 -4 4 127)
                     0 #xffffffffffffffffffffffffffffff)
(clamp-test-generate s64 #s64(0 -127 -4 4 127)
                     #s64(-3 -4 -6 -8 -19) #s64(3 7 9 2 4))

(clamp-test-generate u64 #u64(127 0 4 200 255) #f #f)
(clamp-test-generate u64 #u64(127 0 4 200 255) -4 #f)
(clamp-test-generate u64 #u64(127 0 4 200 255) 0 #f)
(clamp-test-generate u64 #u64(127 0 4 200 255) 199 #f)
(clamp-test-generate u64 #u64(127 0 4 200 255) #f -4)
(clamp-test-generate u64 #u64(127 0 4 200 255) #f 0)
(clamp-test-generate u64 #u64(127 0 4 200 255) #f 199)
(clamp-test-generate u64 #u64(127 0 4 200 255)
                     #x-ffffffffffffffffffffffffffffff 0)
(clamp-test-generate u64 #u64(127 0 4 200 255)
                     0 #xffffffffffffffffffffffffffffff)
(clamp-test-generate u64 #u64(127 0 4 200 255)
                     #u64(3 3 3 3 3) #u64(199 199 199 199 199))

;;-------------------------------------------------------------------
(test-section "block i/o")

(sys-unlink "test.o")

(let* ([data1 '#u8(0 1 2 3 255 254 253 252 4 5 6 7 251 250 249 248)]
       [data2 '#u8(1 0 3 2 254 255 252 253 5 4 7 6 250 251 248 249)]
       [data3 '#u8(3 2 1 0 252 253 254 255 7 6 5 4 248 249 250 251)]
       [data4 '#u8(252 253 254 255 3 2 1 0 248 249 250 251 7 6 5 4)]

       [data5 '#u8(3 0 1 3 3 1 0 3 131 0 1 131 131 1 0 131)]
       )

  ;; Invaliance:
  ;; w = (uvector-alias T u8v)  <--> w = read-block! T + write-block u8v
  (define (test-default-endian data)
    (^[T maker size]
      (test* (format "native endian ~a" (class-name T))
             (uvector-alias T data)
             (rlet1 buf (maker size)
               (call-with-output-file "test.o" (cut write-block data <>))
               (call-with-input-file "test.o" (cut read-block! buf <>))))))

  (define (test-reverse-endian T maker size)
    (let* ([rev-endian (case (native-endian)
                         [(big-endian) 'little-endian]
                         [(little-endian arm-little-endian) 'big-endian])]
           [rev-vec (cond
                     [(memq T (list <u8vector> <s8vector>)) data1]
                     [(memq T (list <u16vector> <s16vector>)) data2]
                     [(memq T (list <u32vector> <s32vector>)) data3]
                     [(memq T (list <u64vector> <s64vector>)) data4])])
      (test* (format "reading ~a ~a" rev-endian (class-name T))
             (uvector-alias T rev-vec)
             (rlet1 buf (maker size)
               (call-with-output-file "test.o" (cut write-block data1 <>))
               (call-with-input-file "test.o"
                 (cut read-block! buf <> 0 -1 rev-endian))))
      (test* (format "reading ~a ~a, parameter" rev-endian (class-name T))
             (uvector-alias T rev-vec)
             (rlet1 buf (maker size)
               (call-with-output-file "test.o" (cut write-block data1 <>))
               (parameterize ([default-endian rev-endian])
                 (call-with-input-file "test.o" (cut read-block! buf <>)))))
      (test* (format "writing ~a ~a" rev-endian (class-name T))
             (uvector-alias T data1)
             (rlet1 buf (maker size)
               (call-with-output-file "test.o"
                 (cut write-block (uvector-alias T rev-vec) <> 0 -1 rev-endian))
               (call-with-input-file "test.o" (cut read-block! buf <>))))
      (test* (format "writing ~a ~a, parameter" rev-endian (class-name T))
             (uvector-alias T data1)
             (rlet1 buf (maker size)
               (parameterize ([default-endian rev-endian])
                 (call-with-output-file "test.o"
                   (cut write-block (uvector-alias T rev-vec) <>)))
               (call-with-input-file "test.o" (cut read-block! buf <>))))))

  (define (run-across fn)
    (let1 s (u8vector-length data1)
      (fn <u8vector> make-u8vector s)
      (fn <s8vector> make-s8vector s)
      (fn <u16vector> make-u16vector (/ s 2))
      (fn <s16vector> make-s16vector (/ s 2))
      (fn <u32vector> make-u32vector (/ s 4))
      (fn <s32vector> make-s32vector (/ s 4))
      (fn <u64vector> make-u64vector (/ s 8))
      (fn <s64vector> make-s64vector (/ s 8))
      ))
  (define (run-across-f fn)
    (let1 s (u8vector-length data5)
      (fn <f16vector> make-f16vector (/ s 2))
      (fn <f32vector> make-f32vector (/ s 4))
      (fn <f64vector> make-f64vector (/ s 8))
      ))

  (run-across (test-default-endian data1))
  (run-across-f (test-default-endian data5))
  (run-across test-reverse-endian)
  )

;;-------------------------------------------------------------------
(test-section "string <-> uvector")

(test* "string->u8vector" '#u8(64 65 66 67 68)
       (string->u8vector "@ABCD"))
(test* "string->u8vector (start)" '#u8(66 67 68)
       (string->u8vector "@ABCD" 2))
(test* "string->u8vector (start, end)" '#u8(65 66 67)
       (string->u8vector "@ABCD" 1 4))
(test* "string->u8vector (start, end)" '#u8(64 65 66 67 68)
       (string->u8vector "@ABCD" 0 5))
(test* "string->u8vector (OOB)" (test-error)
       (string->u8vector "abcde" 2 6))

(test* "string->u8vector (immutable)" '(#t #t)
       (let ([a (string->u8vector "@ABCD" 0 -1 #t)]
             [b (string->u8vector "@ABCD")])
         (list (uvector-immutable? a)
               (equal? a b))))

(test* "string->u8vector!" '#u8(64 65 66 67 68)
       (let1 v (u8vector 0 1 2 3 4)
         (string->u8vector! v 0 "@ABCD")))
(test* "string->u8vector!" '#u8(64 65 66 67 68)
       (let1 v (u8vector 0 1 2 3 4)
         (string->u8vector! v 0 "@ABCDEFGHIJKLMNOPQRTSUVWXYZ")))
(test* "string->u8vector!" '#u8(64 65 66 3 4)
       (let1 v (u8vector 0 1 2 3 4)
         (string->u8vector! v 0 "@AB")))
(test* "string->u8vector!" '#u8(0 64 65 66 4)
       (let1 v (u8vector 0 1 2 3 4)
         (string->u8vector! v 1 "@AB")))
(test* "string->u8vector!" '#u8(0 1 2 3 65)
       (let1 v (u8vector 0 1 2 3 4)
         (string->u8vector! v 4 "@ABCDE" 1)))
(test* "string->u8vector!" '#u8(0 1 2 3 4)
       (let1 v (u8vector 0 1 2 3 4)
         (string->u8vector! v 8 "@ABCDE" 1)))
(test* "string->u8vector!" '#u8(0 1 2 3 4)
       (let1 v (u8vector 0 1 2 3 4)
         (string->u8vector! v -1 "@ABCDE" 1)))

(test* "u8vector->string" "@ABCD"
       (u8vector->string '#u8(64 65 66 67 68)))
(test* "u8vector->string (start)" "ABCD"
       (u8vector->string '#u8(64 65 66 67 68) 1))
(test* "u8vector->string (start, end)" "BC"
       (u8vector->string '#u8(64 65 66 67 68) 2 4))
(test* "u8vector->string (start, end)" "@ABCD"
       (u8vector->string '#u8(64 65 66 67 68) 0 5))
(test* "u8vector->string (OOB)" (test-error)
       (u8vector->string '#u8(64 65 66 67 68) 0 8))

(test* "string->s8vector" '#s8(64 65 66 67 68)
       (string->s8vector "@ABCD"))
(test* "string->s8vector (start)" '#s8(66 67 68)
       (string->s8vector "@ABCD" 2))
(test* "string->s8vector (start, end)" '#s8(65 66 67)
       (string->s8vector "@ABCD" 1 4))
(test* "string->s8vector (start, end)" '#s8(64 65 66 67 68)
       (string->s8vector "@ABCD" 0 5))
(test* "string->s8vector (OOB)" (test-error)
       (string->s8vector "abcde" 2 6))

(test* "string->s8vector!" '#s8(64 65 66 67 68)
       (let1 v (s8vector 0 1 2 3 4)
         (string->s8vector! v 0 "@ABCD")))
(test* "string->s8vector!" '#s8(64 65 66 67 68)
       (let1 v (s8vector 0 1 2 3 4)
         (string->s8vector! v 0 "@ABCDEFGHIJKLMNOPQRTSUVWXYZ")))
(test* "string->s8vector!" '#s8(64 65 66 3 4)
       (let1 v (s8vector 0 1 2 3 4)
         (string->s8vector! v 0 "@AB")))
(test* "string->s8vector!" '#s8(0 64 65 66 4)
       (let1 v (s8vector 0 1 2 3 4)
         (string->s8vector! v 1 "@AB")))
(test* "string->s8vector!" '#s8(0 1 2 3 65)
       (let1 v (s8vector 0 1 2 3 4)
         (string->s8vector! v 4 "@ABCDE" 1)))
(test* "string->s8vector!" '#s8(0 1 2 3 4)
       (let1 v (s8vector 0 1 2 3 4)
         (string->s8vector! v 8 "@ABCDE" 1)))
(test* "string->s8vector!" '#s8(0 1 2 3 4)
       (let1 v (s8vector 0 1 2 3 4)
         (string->s8vector! v -1 "@ABCDE" 1)))

(test* "s8vector->string" "@ABCD"
       (s8vector->string '#s8(64 65 66 67 68)))
(test* "s8vector->string (start)" "ABCD"
       (s8vector->string '#s8(64 65 66 67 68) 1))
(test* "s8vector->string (start, end)" "BC"
       (s8vector->string '#s8(64 65 66 67 68) 2 4))
(test* "s8vector->string (start, end)" "@ABCD"
       (s8vector->string '#s8(64 65 66 67 68) 0 5))
(test* "s8vector->string (OOB)" (test-error)
       (s8vector->string '#s8(64 65 66 67 68) 0 8))

(test* "string->u32vector" '#u32(64 65 66 67 68)
       (string->u32vector "@ABCD"))
(test* "string->u32vector (start)" '#u32(66 67 68)
       (string->u32vector "@ABCD" 2))
(test* "string->u32vector (start, end)" '#u32(65 66 67)
       (string->u32vector "@ABCD" 1 4))
(test* "string->u32vector (start, end)" '#u32(64 65 66 67 68)
       (string->u32vector "@ABCD" 0 5))
(test* "string->u32vector (OOB)" (test-error)
       (string->u32vector "abcde" 2 6))

(test* "string->u32vector!" '#u32(64 65 66 67 68 1 1 1)
       (string->u32vector! (make-u32vector 8 1) 0 "@ABCD"))
(test* "string->u32vector!" '#u32(1 1 1 1 64 65 66 67)
       (string->u32vector! (make-u32vector 8 1) 4 "@ABCD"))
(test* "string->u32vector!" '#u32(1 1 1 66 67 1 1 1)
       (string->u32vector! (make-u32vector 8 1) 3 "@ABCD" 2 4))
       

(test* "u32vector->string" "@ABCD"
       (u32vector->string '#u32(64 65 66 67 68)))
(test* "u32vector->string (start)" "ABCD"
       (u32vector->string '#u32(64 65 66 67 68) 1))
(test* "u32vector->string (start, end)" "BC"
       (u32vector->string '#u32(64 65 66 67 68) 2 4))
(test* "u32vector->string (start, end)" "@ABCD"
       (u32vector->string '#u32(64 65 66 67 68) 0 5))
(test* "u32vector->string (OOB)" (test-error)
       (u32vector->string '#u32(64 65 66 67 68) 0 8))

(test* "string->s32vector" '#s32(64 65 66 67 68)
       (string->s32vector "@ABCD"))
(test* "string->s32vector (start)" '#s32(66 67 68)
       (string->s32vector "@ABCD" 2))
(test* "string->s32vector (start, end)" '#s32(65 66 67)
       (string->s32vector "@ABCD" 1 4))
(test* "string->s32vector (start, end)" '#s32(64 65 66 67 68)
       (string->s32vector "@ABCD" 0 5))
(test* "string->s32vector (OOB)" (test-error)
       (string->s32vector "abcde" 2 6))

(test* "s32vector->string" "@ABCD"
       (s32vector->string '#s32(64 65 66 67 68)))
(test* "s32vector->string (start)" "ABCD"
       (s32vector->string '#s32(64 65 66 67 68) 1))
(test* "s32vector->string (start, end)" "BC"
       (s32vector->string '#s32(64 65 66 67 68) 2 4))
(test* "s32vector->string (start, end)" "@ABCD"
       (s32vector->string '#s32(64 65 66 67 68) 0 5))
(test* "s32vector->string (OOB)" (test-error)
       (s32vector->string '#s32(64 65 66 67 68) 0 8))

;; test for multibyte chars
(cond-expand
 [gauche.ces.eucjp (include "test-eucjp")]
 [gauche.ces.utf8  (include "test-utf8")]
 [gauche.ces.sjis  (include "test-sjis")]
 [else])

;;-------------------------------------------------------------------
(test-section "uvector alias")

(test* "alias u8 u8" #u8(0 1 2 3)
       (let* ([src (u8vector 0 1 2 0)]
              [dst (uvector-alias <u8vector> src)])
         (u8vector-set! src 3 3)
         dst))
(test* "alias u8 u8 (range)" #u8(1 2)
       (let* ([src (u8vector 0 1 0 1)]
              [dst (uvector-alias <u8vector> src 1 3)])
         (u8vector-set! src 2 2)
         dst))
(test* "alias s8 u8" #s8(1 -1)
       (let* ([src (u8vector 0 0 0 0)]
              [dst (uvector-alias <s8vector> src 1 3)])
         (s8vector-set! dst 0 1)
         (u8vector-set! src 2 255)
         dst))
;; the following test cases avoid endian complexity
(test* "alias s8 u16" '(#s8(#x11 #x11 #x22 #x22) #x1111)
       (let* ([src (u16vector 0 0 0)]
              [dst (uvector-alias <s8vector> src 1)])
         (u16vector-set! src 2 #x2222)
         (s8vector-set! dst 0 #x11)
         (s8vector-set! dst 1 #x11)
         (list dst (u16vector-ref src 1))))
(test* "alias u32 u8" #u32(#xaaaaaaaa #xbbbbbbbb)
       (let* ([src (make-u8vector 8 #xaa)]
              [ali (uvector-alias <u8vector> src 4)]
              [dst (uvector-alias <u32vector> src)])
         (u8vector-fill! ali #xbb)
         dst))

;; tricky, but should work on IEEE754 compiant floats
(test* "alias u32 f32" #u32(0)
       (uvector-alias <u32vector> #f32(0.0)))
(test* "alias u32 f64" #u32(0 0)
       (uvector-alias <u32vector> #f64(0.0)))
(test* "alias f64 u32" #f64(0.0)
       (uvector-alias <f64vector> #u32(1 1 0 0) 2))
(test* "alias u64 f64" #u64(0)
       (uvector-alias <u64vector> #f64(0.0)))

(test* "alias turn-around" #f32(1.5)
       (uvector-alias <f32vector>
                      (uvector-alias <u8vector> #f32(1.0 1.5 2.0))
                      4 8))

;; test alignment check
(test* "alias u32 u8 (alignment violation)" (test-error)
       (let* ([src (make-u8vector 9)]
              [dst (uvector-alias <u32vector>)])
         dst))
(test* "alias u32 u8 (alignment violation)" (test-error)
       (let* ([src (make-u8vector 32)]
              [dst (uvector-alias <u32vector> 2)])
         dst))
(test* "alias u32 u8 (alignment violation)" (test-error)
       (let* ([src (make-u8vector 32)]
              [dst (uvector-alias <u32vector> 4 5)])
         dst))

;; test if immutable property propagates
(test* "immutability violation" (test-error)
       (let* ([src '#u8(0 1 2 3)]
              [dst (uvector-alias <u8vector> src)])
         (u8vector-set! dst 0 1)))

;;-------------------------------------------------------------------
; (use gauche.array)
(test-section "gauche.array")
(use gauche.array)
(test-module 'gauche.array)

(test-section "simple array op")

(test* "shape" #t
       (and (shape)
            (shape -1 -1)
            (shape -1 0)
            (shape -1 1)
            (shape 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8)
            #t))

(test* "shape" (test-error) (shape 1))
(test* "shape" (test-error) (shape 1 2 3))
(test* "shape" (test-error) (shape 3 1))

(test* "make-array" #t
       (and (make-array (shape))
            (make-array (shape) *)
            (make-array (shape -1 -1))
            (make-array (shape -1 -1) *)
            (make-array (shape -1 1))
            (make-array (shape 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8 1 2 3 4) *)
            #t))

(test* "array" #t
       (and (array (shape) *)
            (array (shape -1 -1))
            (array (shape -1 1) * *)
            (array (shape 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7 8) *)
            #t))

(test-section "array-rank")
(test* "array-rank (shape)" 2
       (array-rank (shape)))
(test* "array-rank (shape)" 2
       (array-rank (shape -1 -1)))
(test* "array-rank (shape)" 2
       (array-rank (shape 1 2 3 4 5 6 7 8)))

(test* "array-rank (make-array)" 0
       (array-rank (make-array (shape))))
(test* "array-rank (make-array)" 1
       (array-rank (make-array (shape -1 -1))))
(test* "array-rank (make-array)" 1
       (array-rank (make-array (shape -1 1))))
(test* "array-rank (make-array)" 4
       (array-rank (make-array (shape 1 2 3 4 5 6 7 8))))

(test* "array-rank (array)" 0
       (array-rank (array (shape) *)))
(test* "array-rank (array)" 1
       (array-rank (array (shape -1 -1))))
(test* "array-rank (array)" 1
       (array-rank (array (shape -1 1) * *)))
(test* "array-rank (array)" 4
       (array-rank (array (shape 1 2 3 4 5 6 7 8) *)))

(test-section "array-start and array-end")
(test* "array-start (shape)" 0
       (array-start (shape -1 -1) 0))
(test* "array-start (shape)" 0
       (array-start (shape -1 -1) 1))
(test* "array-start (shape)" 0
       (array-start (shape -1 1) 0))
(test* "array-start (shape)" 0
       (array-start (shape -1 1) 1))
(test* "array-start (shape)" 0
       (array-start (shape 1 2 3 4 5 6 7 8) 0))
(test* "array-start (shape)" 0
       (array-start (shape 1 2 3 4 5 6 7 8) 1))

(test* "array-end (shape)" 1
       (array-end (shape -1 -1) 0))
(test* "array-end (shape)" 2
       (array-end (shape -1 -1) 1))
(test* "array-end (shape)" 1
       (array-end (shape -1 1) 0))
(test* "array-end (shape)" 2
       (array-end (shape -1 1) 1))
(test* "array-end (shape)" 4
       (array-end (shape 1 2 3 4 5 6 7 8) 0))
(test* "array-end (shape)" 2
       (array-end (shape 1 2 3 4 5 6 7 8) 1))

(test* "array-start (make-array)" -1
       (array-start (make-array (shape -1 -1)) 0))
(test* "array-start (make-array)" -1
       (array-start (make-array (shape -1 1)) 0))
(test* "array-start (make-array)" '(1 3 5 7)
       (map (pa$ array-start (make-array (shape 1 2 3 4 5 6 7 8)))
            '(0 1 2 3)))

(test* "array-end (make-array)" -1
       (array-end (make-array (shape -1 -1)) 0))
(test* "array-end (make-array)" 1
       (array-end (make-array (shape -1 1)) 0))
(test* "array-end (make-array)" '(2 4 6 8)
       (map (pa$ array-end (make-array (shape 1 2 3 4 5 6 7 8)))
            '(0 1 2 3)))

(test* "array-start (array)" -1
       (array-start (array (shape -1 -1)) 0))
(test* "array-start (array)" -1
       (array-start (array (shape -1 1) * *) 0))
(test* "array-start (array)" '(1 3 5 7)
       (map (pa$ array-start (array (shape 1 2 3 4 5 6 7 8) *))
            '(0 1 2 3)))

(test* "array-end (array)" -1
       (array-end (array (shape -1 -1)) 0))
(test* "array-end (array)" 1
       (array-end (array (shape -1 1) * *) 0))
(test* "array-end (array)" '(2 4 6 8)
       (map (pa$ array-end (array (shape 1 2 3 4 5 6 7 8) *))
            '(0 1 2 3)))

(test-section "array-ref")
(for-each
 (^[ls]
   (let1 a (car ls)
     (for-each
      (^t (test* (format "array-ref ~S:" (cdr t)) (car t)
                 (apply array-ref a (cdr t))))
      (cdr ls))))
 '((#,(<array> () a) (a))
   (#,(<array> (-1 1) a b) (a -1) (b 0))
   (#,(<array> (1 2 3 4 5 6 7 8) a) (a 1 3 5 7))
   (#,(<array> (1 3 4 6 7 9 10 12) a b c d e f g h i j k l m n o p)
    (a 1 4 7 10) (b 1 4 7 11) (i 2 4 7 10) (n 2 5 7 11))
   (#,(<array> () a) (a #()))
   (#,(<array> (-1 1) a b) (a #(-1)) (b #(0)))
   (#,(<array> (1 2 3 4 5 6 7 8) a) (a #(1 3 5 7)))
   (#,(<array> (1 3 4 6 7 9 10 12) a b c d e f g h i j k l m n o p)
    (a #(1 4 7 10)) (b #(1 4 7 11)) (i #(2 4 7 10)) (n #(2 5 7 11)))
   (#,(<array> () a) (a #,(<array> (0 0))))
   (#,(<array> (-1 1) a b) (a #,(<array> (0 1) -1)) (b #,(<array> (0 1) 0)))
   (#,(<array> (1 2 3 4 5 6 7 8) a) (a #,(<array> (0 4) 1 3 5 7)))
   (#,(<array> (1 3 4 6 7 9 10 12) a b c d e f g h i j k l m n o p)
    (a #,(<array> (0 4) 1 4 7 10)) (b #,(<array> (0 4) 1 4 7 11))
    (i #,(<array> (0 4) 2 4 7 10)) (n #,(<array> (0 4) 2 5 7 11)))
   (#,(<u8array> () 2) (2))
   (#,(<u8array> (-1 1) 2 3) (2 -1) (3 0))
   (#,(<u8array> (1 2 3 4 5 6 7 8) 2) (2 1 3 5 7))
   (#,(<u8array> (1 3 4 6 7 9 10 12) 2 3 5 7 11 13 17 23 29 31 37 39 41 43 47 51)
    (2 1 4 7 10) (3 1 4 7 11) (29 2 4 7 10) (43 2 5 7 11))
   ))

(test-section "array-set!")
(for-each
 (^[ls]
   (let1 a (car ls)
     (for-each
      (^t (test* (format "array-set! ~S:" (cdr t)) (car t)
                 (begin (apply array-set! a (append (cdr t) (list (car t))))
                        (apply array-ref a (cdr t)))))
      (cdr ls))))
 '((#,(<array> () a) (x))
   (#,(<array> (-1 1) a b) (y -1) (z 0))
   (#,(<array> (1 2 3 4 5 6 7 8) a) (x 1 3 5 7))
   (#,(<array> (1 3 4 6 7 9 10 12) a b c d e f g h i j k l m n o p)
    (w 1 4 7 10) (x 1 4 7 11) (y 2 4 7 10) (z 2 5 7 11))
   (#,(<array> () a) (x #()))
   (#,(<array> (-1 1) a b) (x #(-1)) (y #(0)))
   (#,(<array> (1 2 3 4 5 6 7 8) a) (x #(1 3 5 7)))
   (#,(<array> (1 3 4 6 7 9 10 12) a b c d e f g h i j k l m n o p)
    (w #(1 4 7 10)) (x #(1 4 7 11)) (y #(2 4 7 10)) (z #(2 5 7 11)))
   (#,(<array> () a) (x #,(<array> (0 0))))
   (#,(<array> (-1 1) a b) (x #,(<array> (0 1) -1)) (y #,(<array> (0 1) 0)))
   (#,(<array> (1 2 3 4 5 6 7 8) a) (x #,(<array> (0 4) 1 3 5 7)))
   (#,(<array> (1 3 4 6 7 9 10 12) a b c d e f g h i j k l m n o p)
    (w #,(<array> (0 4) 1 4 7 10)) (x #,(<array> (0 4) 1 4 7 11))
    (y #,(<array> (0 4) 2 4 7 10)) (z #,(<array> (0 4) 2 5 7 11)))
   (#,(<u8array> () 2) (102))
   (#,(<u8array> (-1 1) 2 3) (102 -1) (103 0))
   (#,(<u8array> (1 2 3 4 5 6 7 8) 2) (102 1 3 5 7))
   (#,(<u8array> (1 3 4 6 7 9 10 12) 2 3 5 7 11 13 17 23 29 31 37 39 41 43 47 51)
    (102 1 4 7 10) (103 1 4 7 11) (129 2 4 7 10) (143 2 5 7 11))
   ))

(test* "array-valid-index? (list)" #t
       (array-valid-index? (make-array (shape) 'o)))
(test* "array-valid-index? (list)" #t
       (array-valid-index? (make-array (shape -1 1) 'o) -1))
(test* "array-valid-index? (list)" #t
       (array-valid-index? (make-array (shape 1 2 3 4 5 6 7 8) 'o) 1 3 5 7))
(test* "array-valid-index? (list)" #f
       (array-valid-index? (make-array (shape) 'o) 0))
(test* "array-valid-index? (list)" #f
       (array-valid-index? (make-array (shape -1 1) 'o) -2))
(test* "array-valid-index? (list)" #f
       (array-valid-index? (make-array (shape 1 2 3 4 5 6 7 8) 'o) 1 3 5))

(test* "array-valid-index? (vector)" #t
       (array-valid-index? (make-array (shape) 'o) #()))
(test* "array-valid-index? (vector)" #t
       (array-valid-index? (make-array (shape -1 1) 'o) #(0)))
(test* "array-valid-index? (vector)" #t
       (array-valid-index? (make-array (shape 1 2 3 4 5 6 7 8) 'o) #(1 3 5 7)))
(test* "array-valid-index? (vector)" #f
       (array-valid-index? (make-array (shape) 'o) #(0)))
(test* "array-valid-index? (vector)" #f
       (array-valid-index? (make-array (shape -1 1) 'o) #(1)))
(test* "array-valid-index? (vector)" #f
       (array-valid-index? (make-array (shape 1 2 3 4 5 6 7 8) 'o) #(1 3 5 7 9)))

(test* "array-valid-index? (array)" #t
       (array-valid-index? (make-array (shape) 'o) #,(<array> (0 0))))
(test* "array-valid-index? (array)" #t
       (array-valid-index? (make-array (shape -1 1) 'o) #,(<array> (0 1) 0)))
(test* "array-valid-index? (array)" #t
       (array-valid-index? (make-array (shape 1 2 3 4 5 6 7 8) 'o) #,(<array> (0 4) 1 3 5 7)))
(test* "array-valid-index? (array)" #f
       (array-valid-index? (make-array (shape) 'o) #,(<array> (0 1) 0)))
(test* "array-valid-index? (array)" #f
       (array-valid-index? (make-array (shape -1 1) 'o) #,(<array> (0 1) 1)))
(test* "array-valid-index? (array)" #f
      (array-valid-index? (make-array (shape 1 2 3 4 5 6 7 8) 'o) #,(<array> (0 4) 1 4 5 7)))

;;; Share and change:
;;;
;;;  org     brk     swp            box
;;;
;;;   0 1     1 2     5 6
;;; 6 a b   2 a b   3 d c   0 2 4 6 8: e
;;; 7 c d   3 e f   4 f e
;;; 8 e f

(test-section "shared change")

(let* ([org (array (shape 6 9 0 2) 'a 'b 'c 'd 'e 'f)]
       [brk (share-array org
                         (shape 2 4 1 3)
                         (^[r k] (values
                                  (+ 6 (* 2 (- r 2)))
                                  (- k 1))))]
       [swp (share-array org
                         (shape 3 5 5 7)
                         (^[r k] (values
                                  (+ 7 (- r 3))
                                  (- 1 (- k 5)))))]
       [box (share-array swp
                         (shape 0 1 2 3 4 5 6 7 8 9)
                         (^ _ (values 4 6)))]
       [org-contents (^[] (list (array-ref org 6 0) (array-ref org 6 1)
                                (array-ref org 7 0) (array-ref org 7 1)
                                (array-ref org 8 0) (array-ref org 8 1)))]
       [brk-contents (^[] (list (array-ref brk 2 1) (array-ref brk 2 2)
                                (array-ref brk 3 1) (array-ref brk 3 2)))]
       [swp-contents (^[] (list (array-ref swp 3 5) (array-ref swp 3 6)
                                (array-ref swp 4 5) (array-ref swp 4 6)))]
       [box-contents (^[] (list (array-ref box 0 2 4 6 8)))])
  (test "org-contents" '(a b c d e f) org-contents)
  (test "brk-contents" '(a b e f) brk-contents)
  (test "swp-contents" '(d c f e) swp-contents)
  (test "box-contents" '(e) box-contents)
  (begin (array-set! org 6 0 'x) #t)
  (test "org-contents" '(x b c d e f) org-contents)
  (test "brk-contents" '(x b e f) brk-contents)
  (test "swp-contents" '(d c f e) swp-contents)
  (test "box-contents" '(e) box-contents)
  (begin (array-set! brk 3 1 'y) #t)
  (test "org-contents" '(x b c d y f) org-contents)
  (test "brk-contents" '(x b y f) brk-contents)
  (test "swk-contents" '(d c f y) swp-contents)
  (test "box-contents" '(y) box-contents)
  (begin (array-set! swp 4 5 'z) #t)
  (test "org-contents" '(x b c d y z) org-contents)
  (test "brk-contents" '(x b y z) brk-contents)
  (test "swp-contents" '(d c z y) swp-contents)
  (test "box-contents" '(y) box-contents)
  (begin (array-set! box 0 2 4 6 8 'e) #t)
  (test "org-contents" '(x b c d e z) org-contents)
  (test "brk-contents" '(x b e z) brk-contents)
  (test "swp-contents" '(d c z e) swp-contents)
  (test "box-contents" '(e) box-contents)
  )

;;; Check that arrays copy the shape specification

(test-section "array-set! of shape")

(let1 shp (shape 10 12)
  (let ([arr (make-array shp)]
        [ars (array shp * *)]
        [art (share-array (make-array shp) shp identity)])
    (array-set! shp 0 0 '?)
    (array-set! shp 0 1 '!)
    (test* "modifying array shape"
           '(2 0 1 0 2 ? ! 1 10 12 1 10 12 1 10 12)
           (list (array-rank shp)
                 (array-start shp 0)
                 (array-end shp 0)
                 (array-start shp 1)
                 (array-end shp 1)
                 (array-ref shp 0 0)
                 (array-ref shp 0 1)
                 (array-rank arr)
                 (array-start arr 0)
                 (array-end arr 0)
                 (array-rank ars)
                 (array-start ars 0)
                 (array-end ars 0)
                 (array-rank art)
                 (array-start art 0)
                 (array-end art 0)))
    ))
;;; Check that index arrays work even when they share
;;;
;;; arr       ixn
;;;   5  6      0 1
;;; 4 nw ne   0 4 6
;;; 5 sw se   1 5 4

(test-section "array access with sharing index array")
(let ([arr (array (shape 4 6 5 7) 'nw 'ne 'sw 'se)]
      [ixn (array (shape 0 2 0 2) 4 6 5 4)])
  (let ([col0 (share-array ixn
                           (shape 0 2)
                           (^k (values k 0)))]
        [row0 (share-array ixn
                           (shape 0 2)
                           (^k (values 0 k)))]
        [wor1 (share-array ixn
                           (shape 0 2)
                           (^k (values 1 (- 1 k))))]
        [cod (share-array ixn
                          (shape 0 2)
                          (^k (case k
                                [(0) (values 1 0)]
                                [(1) (values 0 1)])))]
        [box (share-array ixn
                          (shape 0 2)
                          (^k (values 1 0)))])
    (test* "array-ref before change"
           '(nw ne nw se sw)
           (list (array-ref arr col0)
                 (array-ref arr row0)
                 (array-ref arr wor1)
                 (array-ref arr cod)
                 (array-ref arr box)))
    (array-set! arr col0 'ul)
    (array-set! arr row0 'ur)
    (array-set! arr cod 'lr)
    (array-set! arr box 'll)
    (test* "array-ref after change"
           '(ul ur ll lr)
           (list (array-ref arr 4 5)
                 (array-ref arr 4 6)
                 (array-ref arr 5 5)
                 (array-ref arr 5 6)))
    (array-set! arr wor1 'xx)
    (test* "array-ref after change" 'xx
           (array-ref arr 4 5))
    ))

;;; Check that shape arrays work even when they share
;;;
;;; arr             shp       shq       shr       shs
;;;    1  2  3  4      0  1      0  1      0  1      0  1
;;; 1 10 12 16 20   0 10 12   0 12 20   0 10 10   0 12 12
;;; 2 10 11 12 13   1 10 11   1 11 13   1 11 12   1 12 12
;;;                                     2 12 16
;;;                                     3 13 20

(test-section "sharing shape array")
(let1 arr (array (shape 1 3 1 5) 10 12 16 20 10 11 12 13)
  (let ([shp (share-array arr
                          (shape 0 2 0 2)
                          (^[r k] (values (+ r 1) (+ k 1))))]
        [shq (share-array arr
                          (shape 0 2 0 2)
                          (^[r k] (values (+ r 1) (* 2 (+ 1 k)))))]
        [shr (share-array arr
                          (shape 0 4 0 2)
                          (^[r k] (values (- 2 k) (+ r 1))))]
        [shs (share-array arr
                          (shape 0 2 0 2)
                          (^[r k] (values 2 3)))])
    (test* "using make-array shp"
           '(2 10 12 10 11)
           (let ((arr-p (make-array shp)))
             (list (array-rank arr-p)
                   (array-start arr-p 0)
                   (array-end arr-p 0)
                   (array-start arr-p 1)
                   (array-end arr-p 1))))
    (test* "using array shq"
           '(2 12 20 11 13)
           (let1 arr-q (array shq * * * *  * * * *  * * * *  * * * *)
             (list (array-rank arr-q)
                   (array-start arr-q 0)
                   (array-end arr-q 0)
                   (array-start arr-q 1)
                   (array-end arr-q 1))))
    (test* "using share-array"
           '(4 10 10 11 12 12 16 13 20)
           (let1 arr-r (share-array (array (shape) *)
                                    shr
                                    (^ _ (values)))
             (list (array-rank arr-r)
                   (array-start arr-r 0)
                   (array-end arr-r 0)
                   (array-start arr-r 1)
                   (array-end arr-r 1)
                   (array-start arr-r 2)
                   (array-end arr-r 2)
                   (array-start arr-r 3)
                   (array-end arr-r 3))))
    (test* "using make-array shs"
           '(2 12 12 12 12)
           (let1 arr-s (make-array shs)
             (list (array-rank arr-s)
                   (array-start arr-s 0)
                   (array-end arr-s 0)
                   (array-start arr-s 1)
                   (array-end arr-s 1))))
    ))

(test-section "sharing with sharing subshape")
(let ([super (array (shape 4 7 4 7)
                    1 * *
                    * 2 *
                    * * 3)]
      [subshape (share-array (array (shape 0 2 0 3)
                                    * 4 *
                                    * 7 *)
                             (shape 0 1 0 2)
                             (^[r k] (values k 1)))])
  (let1 sub (share-array super subshape (^k (values k k)))
    (test* "subshape check" #t
           (equal? subshape (shape 4 7)))
    (test* "sharing subshape" '(2 0 1 0 2 4 7)
           (list (array-rank subshape)
                 (array-start subshape 0)
                 (array-end subshape 0)
                 (array-start subshape 1)
                 (array-end subshape 1)
                 (array-ref subshape 0 0)
                 (array-ref subshape 0 1)))
    (test* "sub check" #t
           (equal? sub (array (shape 4 7) 1 2 3)))
    (test* "sharing with sharing subshape" '(1 4 7 1 2 3)
           (list (array-rank sub)
                 (array-start sub 0)
                 (array-end sub 0)
                 (array-ref sub 4)
                 (array-ref sub 5)
                 (array-ref sub 6)))
    ))

(test-section "subarray")
(let ([super (array (shape 0 3 0 3)
                    1 * *
                    * 2 *
                    * * 3)]
      [subshape (shape 1 3 1 3)])
  (let1 sub (subarray super subshape)
    (test* "sub check" #t
           (equal? sub (array (shape 0 2 0 2) 2 * * 3)))
    (test* "sharing with sharing subshape" (list 2 0 2 2 * * 3)
           (list (array-rank sub)
                 (array-start sub 0)
                 (array-end sub 0)
                 (array-ref sub 0 0)
                 (array-ref sub 0 1)
                 (array-ref sub 1 0)
                 (array-ref sub 1 1)))
    ))

;;----------------------------------------------------------------
(test-section "array-iteration")

(test* "array-for-each-index (list)" '((0 0) (0 1) (1 0) (1 1))
  (let ([ar (make-array (shape 0 2 0 2))]
        [ls '()])
    (array-for-each-index ar (^[a b] (push! ls (list a b))))
    (reverse ls)))

(test* "array-for-each-index (vector)" '(#(0 0) #(0 1) #(1 0) #(1 1))
  (let ([ar (make-array (shape 0 2 0 2))]
        [ls '()]
        [vec (make-vector 2)])
    (array-for-each-index ar (^v (push! ls (vector-copy v))) vec)
    (reverse ls)))

(test* "array-for-each-index (array)" '(#(0 0) #(0 1) #(1 0) #(1 1))
  (let ([ar (make-array (shape 0 2 0 2))]
        [ls '()]
        [ind (make-array (shape 0 2))])
    (array-for-each-index ar (^a (push! ls (array->vector a))) ind)
    (reverse ls)))

(test* "array-for-each-index-by-dimension (list)" '(0 1)
  (let ([ar (make-array (shape 0 2 5 7))]
        [ls '()])
    (array-for-each-index-by-dimension ar '(0) (^[a b] (push! ls a)))
    (reverse ls)))

(test* "array-for-each-index-by-dimension (vector)" '(#(3 5) #(3 6) #(3 7))
  (let ([ar (make-array (shape 0 2 5 8))]
        [ls '()])
    (array-for-each-index-by-dimension
     ar '(1) (^v (push! ls (vector-copy v))) (vector 3 4))
    (reverse ls)))

(test* "array-for-each-index-by-dimension (array)" '(#(5) #(6) #(7))
  (let ([ar (make-array (shape 5 8 0 2))]
        [ls '()])
    (array-for-each-index-by-dimension
     ar '(0) (^a (push! ls (array->vector a))) (make-array (shape 0 1)))
    (reverse ls)))

(test* "shape-for-each (list)" '((0 0) (0 1) (1 0) (1 1))
  (let ([sh (shape 0 2 0 2)]
        [ls '()])
    (shape-for-each sh (^[a b] (push! ls (list a b))))
    (reverse ls)))

(test* "shape-for-each (vector)" '(#(0 0) #(0 1) #(1 0) #(1 1))
  (let ([sh (shape 0 2 0 2)]
        [ls '()]
        [vec (make-vector 2)])
    (shape-for-each sh (^v (push! ls (vector-copy v))) vec)
    (reverse ls)))

(test* "shape-for-each (array)" '(#(0 0) #(0 1) #(1 0) #(1 1))
  (let ([sh (shape 0 2 0 2)]
        [ls '()]
        [ind (make-array (shape 0 2))])
    (shape-for-each sh (^a (push! ls (array->vector a))) ind)
    (reverse ls)))

(test* "tabulate-array (list)" #,(<array> (1 3 1 3) 11 12 21 22)
  (tabulate-array (shape 1 3 1 3) (^[a b] (+ (* a 10) b))))

(test* "tabulate-array (vector)" #,(<array> (2 4 2 4) 12 31 17 36)
  (let ([vec (make-vector 2)]
        [square (^x (* x x))]
        [cube (^x (* x x x))])
    (tabulate-array (shape 2 4 2 4) (^v (+ (square (ref v 0))
                                           (cube (ref v 1))))
                    vec)))

(test* "array-retabulate! (list)" #,(<array> (0 2 0 2) 1.0 0.5 0.25 0.125)
  (rlet1 ar #,(<array> (0 2 0 2) 1 2 4 8)
    (array-retabulate! ar (^[i j] (/ 1.0 (array-ref ar i j))))))

(test* "array-retabulate! (vector)" #,(<array> (0 2 0 2) 1.0 0.5 0.25 0.125)
  (let ([ar #,(<array> (0 2 0 2) 1 2 4 8)]
        [vec (make-vector 2)])
    (array-retabulate! ar (^v (/ 1.0 (array-ref ar v))) vec)
    ar))

(test* "array-retabulate! (uniform)" #,(<f64array> (0 2 0 2) 1.0 0.5 0.25 0.125)
  (let ([ar #,(<f64array> (0 2 0 2) 1 2 4 8)]
        [vec (make-vector 2)])
    (array-retabulate! ar (^v (/ 1.0 (array-ref ar v))) vec)
    ar))

(test* "array-map-1" #,(<array> (0 2 0 2) 1 4 9 16)
  (array-map (^x (* x x)) #,(<array> (0 2 0 2) 1 2 3 4)))

(test* "array-map-2" #,(<array> (0 2 0 2) 11 22 33 44)
  (array-map (^[a b] (+ a b))
             #,(<array> (0 2 0 2) 1 2 3 4)
             #,(<array> (0 2 0 2) 10 20 30 40)))

(test* "array-map-3" #,(<array> (0 2 0 2) 11.1 22.2 33.3 44.4)
  (array-map (lambda (a b c) (+ a b c))
             #,(<array> (0 2 0 2) 1 2 3 4)
             #,(<array> (0 2 0 2) 10 20 30 40)
             #,(<array> (0 2 0 2) .1 .2 .3 .4)))

(test* "array-map-4" #,(<array> (0 2 0 2) 111.0 22.2 3.33 .444)
  (array-map (lambda (a b c d) (* d (+ a b c)))
             #,(<array> (0 2 0 2) 1 2 3 4)
             #,(<array> (0 2 0 2) 10 20 30 40)
             #,(<array> (0 2 0 2) .1 .2 .3 .4)
             #,(<array> (0 2 0 2) 10.0 1.0 0.1 0.01)))

(test* "array-map-uniform" #,(<array> (0 2 0 2) 111.0 22.2 3.33 .444)
  (array-map (lambda (a b c d) (/ (truncate (* (* d (+ a b c)) 10000)) 10000))
             #,(<u8array> (0 2 0 2) 1 2 3 4)
             #,(<s32array> (0 2 0 2) 10 20 30 40)
             #,(<f32array> (0 2 0 2) .1 .2 .3 .4)
             #,(<f64array> (0 2 0 2) 10.0 1.0 0.1 0.01)))

(test* "array-every-1" #t
  (array-every even? #,(<array> (0 0))))
(test* "array-every-2" #t
  (array-every even? #,(<array> () 2)))
(test* "array-every-3" #t
  (array-every even? #,(<array> (0 2 0 2) 2 4 6 8)))
(test* "array-every-4" #f
  (array-every even? #,(<array> () 3)))
(test* "array-every-5" #f
  (array-every even? #,(<array> (0 2 0 2) 2 4 3 8)))

(test* "array-any-1" #t
  (array-any even? #,(<array> () 2)))
(test* "array-any-2" #t
  (array-any even? #,(<array> (0 2 0 2) 1 2 3 8)))
(test* "array-any-3" #f
  (array-any even? #,(<array> (0 0))))
(test* "array-any-4" #f
  (array-any even? #,(<array> () 3)))
(test* "array-any-5" #f
  (array-any even? #,(<array> (0 2 0 2) 1 3 5 7)))

(test-section "matrices")

;; zero-base the shape
(define (array-normalize a)
  (define (every-other pred ls)
    (if (null? ls) #t (and (pred (car ls)) (every-other pred (cddr ls)))))
  (if (every-other zero? (array->list (array-shape a)))
    a
    (subarray a (array-shape a))))

(define (approx-equal? x y . opt)
  (< (abs (- x y)) (get-optional opt 0.0000001)))

(define (array-approx-equal? a b)
  (or (eq? a b) (array-equal? a b approx-equal?)))

(let1 i 0
  (for-each
   (^t (let-optionals* t (ar (inv #f) (det 0))
         (test* (format "array-inverse-~D" (inc! i)) inv
                (array-inverse ar)
                array-approx-equal?)
         (when inv
           (test* (format "array-inverse-~D" (inc! i)) (array-normalize ar)
                  (array-inverse inv)
                  array-approx-equal?))
         (test* (format "determinant-~D" (inc! i)) det
                (determinant ar)
                approx-equal?)))
   '((#,(<array> (0 2 0 2) 1 2 3 4)
      #,(<array> (0 2 0 2) -2.0 1.0 1.5 -0.5)
      -2)
     (#,(<array> (1 3 1 3) 1 2 3 4)
      #,(<array> (0 2 0 2) -2.0 1.0 1.5 -0.5)
      -2)
     (#,(<array> (3 5 7 9) 1 2 3 4)
      #,(<array> (0 2 0 2) -2.0 1.0 1.5 -0.5)
      -2)
     (#,(<array> (0 3 0 3) 1 5 2 1 1 7 0 -3 4)
      #,(<array> (0 3 0 3) -25 26 -33 4 -4 5 3 -3 4)
      -1)
     (#,(<array> (0 3 0 3) 2 0 1 1 1 0 3 2 1)
      #,(<array> (0 3 0 3) 1 2 -1 -1 -1 1 -1 -4 2)
      1)
     (#,(<array> (0 3 0 3) 1 -1 3 2 1 2 -2 -2 1)
      #,(<array> (0 3 0 3) 1 -1 -1 -1.2 1.4 0.8 -0.4 0.8 0.6)
      5)
     (#,(<array> (0 2 0 2) 1 2 3 6))
     )))

(let ((i 0))
  (for-each
   (^t (let-optionals* t (a b c)
         (test* (format "array-mul-~D" (inc! i)) c
                (array-mul a b)
                array-approx-equal?)))
   '((#,(<array> (0 2 0 2) 1 2 3 4)
      #,(<array> (0 2 0 2) 4 3 2 1)
      #,(<array> (0 2 0 2) 8 5 20 13))
     (#,(<array> (1 3 1 3) 1 1 1 1)
      #,(<array> (2 4 2 4) 2 2 2 2)
      #,(<array> (0 2 0 2) 4 4 4 4))
     (#,(<u8array> (3 5 7 9) 3 1 4 1)
      #,(<u8array> (55 57 17 19) 2 7 8 1)
      #,(<u8array> (0 2 0 2) 14 22 16 29))
     (#,(<s16array> (3 4 1 9) 1 -2 3 -4 5 -6 7 -8)
      #,(<s16array> (3 11 5 6) 1 -2 3 -4 5 -6 7 -8)
      #,(<s16array> (0 1 0 1) 204))
     )))


;;-------------------------------------------------------------------
;; NB: copy-port uses read-block! and write-block for block copy,
;;     so we test it here.
(test-section "copy-port")

(define s (make-string 10000 #\z))

(test* "copy-port (default)" #t
       (equal? s (call-with-string-io s (^[in out] (copy-port in out)))))

(test* "copy-port (unit byte)" #t
       (equal? s (call-with-string-io s (^[in out]
                                          (copy-port in out :unit 'byte)))))

(test* "copy-port (unit char)" #t
       (equal? s (call-with-string-io s (^[in out]
                                          (copy-port in out :unit 'char)))))

(test* "copy-port (unit 10)" #t
       (equal? s (call-with-string-io s (^[in out]
                                          (copy-port in out :unit 10)))))

(test* "copy-port (unit 100000)" #t
       (equal? s (call-with-string-io s (^[in out]
                                          (copy-port in out :unit 100000)))))

(test-end)
